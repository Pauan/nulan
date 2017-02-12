import { chain, reply, with_resource } from "../task";
import { random_characters, ALPHANUMERIC } from "./random";
import { fs_mkdir } from "./fs";
import * as $path from "node:path";
import * as $os from "node:os";

const _tmpdir = $os.tmpdir();


export const temporary_path =
  chain(random_characters(16, ALPHANUMERIC), (x) =>
    reply($path.join($tmpdir, "tmp-" + x)));

export const make_temporary_directory =
  chain(temporary_path, (path) =>
    chain(fs_mkdir(path, 0o700), (success) =>
      (success
        ? reply(path)
        : make_temporary_directory)));

export const with_temporary_directory = (use) =>
  with_resource(make_temporary_directory, use, remove_directory);
