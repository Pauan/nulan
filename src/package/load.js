import * as $fs from "../../util/fs";
import * as $path from "path";
import * as $path_util from "../../util/path";
import * as $dependency from "./dependency";
import * as $version from "./version";
import * as $git from "../../util/git";


const get_tags = async (repo, version) => {
  const tags = await $git.get_tags(repo);

  const parsed = tags["map"]((x) =>
    ({ tag: x, parsed: $version.parse(x) }));

  const filtered = parsed["filter"]((x) =>
    x.parsed !== null && $version.matches(version, x.parsed));

  return filtered["sort"]((a, b) => $version.order(a.parsed, b.parsed));
};


const get_repository = async (a) => {
  await $fs.mkdir($path.join(".nulan", "cache"));
  await $fs.mkdir($path.join(".nulan", "cache", "git"));

  const path = $path.join(".nulan", "cache", "git", $path_util.encode($dependency.url(a)));

  return await $git.clone($dependency.url(a), path);
};


const clone_repository = async (repo, a) => {
  await $fs.mkdir($path.join(".nulan", "dependencies"));

  const path = $path.join(".nulan", "dependencies", $path_util.encode($dependency.serialize(a)));

  return await $git.clone_local(repo["path"](), path);
};


export const load_package = async (a) => {
  await $fs.mkdir(".nulan");

  const repo = await get_repository(a);

  const version = $dependency.version(a);

  const tags = await get_tags(repo, version);

  if (tags["length"] === 0) {
    // TODO line and column numbers
    throw new Error("Could not find tag that matches " + $version.serialize(version));

  } else {
    const last = tags[tags["length"] - 1];

    console.log(last);

    const new_repo = await clone_repository(repo, $dependency.change_version(a, last.parsed));

    await $git.checkout_tag(new_repo, last.tag);
  }
};


import { version } from "./version";

console.log("STARTING");

load_package($dependency.github("Pauan/Immutable", version(3, 0))).catch((e) => {
  console.log(e);
});
