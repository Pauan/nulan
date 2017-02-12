import * as $fs from "../../util/fs";
import * as $path from "path";
import * as $path_util from "../../util/path";
import * as $dependency from "./dependency";
import * as $version from "./version";
import * as $git from "../../util/git";


const get_tags = async (repo) => {
  const tags = await $git.get_tags(repo);

  const parsed = tags["map"]((x) =>
    ({ tag: x, parsed: $version.parse(x) }));

  return parsed["filter"]((x) => x.parsed !== null);
};


const get_highest_version = (tags) => {
  const a = tags["sort"]((a, b) => $version.order(a.parsed, b.parsed));

  return a[a["length"] - 1];
};


const get_highest_compatible_version = (tags, version) => {
  const filtered = tags["filter"]((x) =>
    $version.matches(version, x.parsed));

  if (filtered["length"] === 0) {
    // TODO line and column numbers
    throw new Error("Could not find tag that matches " + $version.serialize(version));

  } else {
    return get_highest_version(filtered);
  }
};


const get_repository = async (a) => {
  const path = $path.join(".nulan", "cache", "git", $path_util.encode($dependency.url(a)));

  return await $git.clone($dependency.url(a), path);
};


const clone_repository = async (repo, a) => {
  const path = $path.join(".nulan", "dependencies", $path_util.encode($dependency.serialize(a)));

  return await $git.clone_local(repo, path);
};


// TODO if the dependency already exists, do nothing ?
export const load_package = async (a) => {
  await $fs.mkdir(".nulan");
  await $fs.mkdir($path.join(".nulan", "cache"));
  await $fs.mkdir($path.join(".nulan", "cache", "git"));
  await $fs.mkdir($path.join(".nulan", "dependencies"));

  const repo = await get_repository(a);

  // TODO is this correct? is this slow? does this fetch new tags?
  await $git.fetch(repo);

  const tags = await get_tags(repo);

  const version = $dependency.version(a);

  const highest = get_highest_version(tags);

  const last = get_highest_compatible_version(tags, version);

  if ($version.is_higher($dependency.version(a), highest.parsed)) {
    // TODO line/column info
    console["warn"]("Warning:\n  Package was specified as:\n    " +
                      $dependency.serialize(a) + "\n\n  But the highest version is:\n    " +
                      $dependency.serialize($dependency.change_version(a, highest.parsed)) + "\n");
  }

  const new_dependency = $dependency.change_version(a, last.parsed);

  console["info"]("Loading package " + $dependency.serialize(new_dependency) + "\n");

  const new_repo = await clone_repository(repo, new_dependency);

  // TODO does this need to fetch if the new repository already exists ?
  await $git.checkout_tag(new_repo, last.tag);

  return $git.get_path(new_repo);
};


import { version } from "./version";

console.log("STARTING");

load_package($dependency.github("Pauan/Immutable", "v6.5.0")).catch((e) => {
  console.log(e);
}).then((x) => {
  console.log(x);
});
