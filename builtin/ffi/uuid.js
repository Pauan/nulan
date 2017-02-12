import { crash, error_from } from "./crash";


// https://en.wikipedia.org/wiki/Universally_unique_identifier
// https://tools.ietf.org/html/rfc4122
// https://www.ietf.org/rfc/rfc4122.txt
const uuid_regexp = /^[0-9a-f]{8}\-[0-9a-f]{4}\-[12345][0-9a-f]{3}\-[89ab][0-9a-f]{3}\-[0-9a-f]{12}$/;

export const uuid = (s) => {
  if (uuid_regexp["test"](s)) {
    return s;

  } else {
    return crash(error_from("expected a lowercase UUID but got: " + s));
  }
};
