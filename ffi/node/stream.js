import { _null } from "../types";


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


export const read = (input, push) =>
  async_killable((success, error) => {
    let finished = false;

    const thread = make_thread();

    const cleanup = () => {
      if (finished) {
        throw new Error("Invalid cleanup");

      } else {
        finished = true;

        // TODO is this correct ?
        input["destroy"]();
        kill_thread(thread);

        input["removeListener"]("end", onEnd);
        input["removeListener"]("error", onError);
        input["removeListener"]("readable", onReadable);
      }
    };

    const onEnd = () => {
      cleanup();
      success(_null);
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
        run(push(chunk), thread, onReadable, onError);
      }
    };

    input["setEncoding"]("utf8");

    input["on"]("end", onEnd);
    input["on"]("error", onError);
    input["on"]("readable", onReadable);

    onReadable();

    return cleanup;
  });


export const write = (output, pull) =>
  async_killable((success, error) => {
    let cleaned = false;

    const thread = make_thread();

    // TODO should this end the output ?
    const cleanup = () => {
      if (cleaned) {
        throw new Error("Invalid cleanup");

      } else {
        cleaned = true;

        // TODO is this correct ?
        output["destroy"]();
        kill_thread(thread);

        output["removeListener"]("finish", onFinish);
        output["removeListener"]("error", onError);
        output["removeListener"]("drain", onDrain);
      }
    };

    const onFinish = () => {
      cleanup();
      success(_null);
    };

    const onSuccess = (value) => {
      if (value.$ === 0) {
        // We don't cleanup, because that's handled by `onFinish`
        // TODO what if `onDrain` gets called before `onFinish` ?
        output["end"]();

      } else {
        if (output["write"](value.a, "utf8")) {
          onDrain();
        }
      }
    };

    const onError = (e) => {
      cleanup();
      error(e);
    };

    const onDrain = () => {
      // TODO remove this later
      if (cleaned) {
        throw new Error("HI!!!!");
      }

      run(pull, thread, onSuccess, onError);
    };

    // TODO this doesn't work
    //output["setDefaultEncoding"]("utf8");

    output["on"]("finish", onFinish);
    output["on"]("error", onError);
    output["on"]("drain", onDrain);

    onDrain();

    return cleanup;
  });
