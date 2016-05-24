import * as $version from "./version";
import * as $string from "../../util/string";


const GIT = 0;
const GITHUB = 1;

export const git = (a, b) =>
  ({ $: GIT, a, b });

export const github = (a, b) =>
  ({ $: GITHUB, a, b });

export const url = (a) => {
  switch (a.$) {
  case GIT:
    return a.a;
  case GITHUB:
    return "https://github.com/" + a.a + ".git"
  }
};

export const version = (a) => {
  return a.b;
};

export const change_version = (a, b) =>
  ({ $: a.$, a: a.a, b: b });

export const serialize = (a) => {
  switch (a.$) {
  case GIT:
    return "(git " + $string.serialize(a.a) + " " + $version.serialize(a.b) + ")";
  case GITHUB:
    return "(github " + $string.serialize(a.a) + " " + $version.serialize(a.b) + ")";
  }
};
