import { async_killable, make_thread, thread_kill,
         run_in_thread, async_unkillable, with_resource } from "../task";
import { make_stream } from "../stream";
import { open, close, DEFAULT_MODE } from "./fs";
import * as $fs from "node:fs";


export const read_file = (path) =>
  (push) =>
    with_resource(open(path, "r", DEFAULT_MODE),
      (fd) =>
        // TODO is it okay for make_read_stream to be impure ?
        read_from_stream(make_read_stream(fd), push);
      close);

export const write_file = (path, input) =>
  with_resource(open(path, "wx", DEFAULT_MODE),
    (fd) =>
      // TODO is it okay for make_write_stream to be impure ?
      write_to_stream(input, make_write_stream(fd));
    close);


export const make_read_stream = (fd) =>
  $fs.createReadStream(null, {
    "encoding": "utf8",
    "fd": fd,
    "autoClose": false
  });

export const make_write_stream = (fd) =>
  $fs.createWriteStream(null, {
    "encoding": "utf8",
    "fd": fd,
    "autoClose": false
  });


export const read_from_stream = (input, push) =>
  async_killable((success, error) => {
    let finished = false;

    const cleanup = () => {
      if (finished) {
        throw new Error("invalid cleanup");

      } else {
        finished = true;

        // TODO is this correct ?
        input["destroy"]();
        thread_kill(thread);

        input["removeListener"]("end", onEnd);
        input["removeListener"]("error", onError);
        input["removeListener"]("readable", onReadable);
      }
    };

    const onEnd = () => {
      cleanup();
      success(null);
    };

    const onError = (e) => {
      cleanup();
      error(e);
    };

    const onReadable = () => {
      // TODO remove this later
      if (finished) {
        throw new Error("HI!");
      }

      // TODO should this set a byte size for `read` ?
      // TODO is this a good byte size ?
      const chunk = input["read"]();
      if (chunk !== null) {
        // TODO is it possible for a "readable" event to trigger even if `chunk` is not `null` ?
        run_in_thread(thread, push(chunk));
      }
    };

    input["setEncoding"]("utf8");

    input["on"]("end", onEnd);
    input["on"]("error", onError);
    input["on"]("readable", onReadable);

    const thread = make_thread(onReadable, onError);

    onReadable();

    return cleanup;
  });


export const write_to_stream = (input, output) =>
  async_killable((success, error) => {
    let cleaned = false;

    // TODO should this end the output ?
    const cleanup = () => {
      if (cleaned) {
        throw new Error("invalid cleanup");

      } else {
        cleaned = true;
        _success = null;

        // TODO is this correct ?
        output["destroy"]();
        thread_kill(thread);

        output["removeListener"]("finish", onFinish);
        output["removeListener"]("error", onError);
        output["removeListener"]("drain", onDrain);
      }
    };

    const onFinish = () => {
      cleanup();
      success(null);
    };

    const onComplete = () => {
      // We don't cleanup, because that's handled by `onFinish`
      // TODO what if `onDrain` gets called before `onFinish` ?
      output["end"]();
    };

    const onError = (e) => {
      cleanup();
      error(e);
    };


    let _success = null;

    const onDrain = () => {
      // TODO remove this later
      if (cleaned) {
        throw new Error("HI!!!!");
      }

      const x = _success;
      _success = null;
      x(null);
    };

    // TODO this doesn't work
    //output["setDefaultEncoding"]("utf8");

    output["on"]("finish", onFinish);
    output["on"]("error", onError);
    output["on"]("drain", onDrain);

    const thread = make_thread(onComplete, onError);

    run_in_thread(thread, input((value) =>
      async_killable((success, error) => {
        // Continue writing
        if (output["write"](value, "utf8")) {
          success(null);

        // Wait for drain event
        // TODO verify that _success is null ?
        } else {
          _success = success;
        }

        // TODO is this correct ?
        return () => {
          cleanup();
        };
      })));

    return cleanup;
  });
