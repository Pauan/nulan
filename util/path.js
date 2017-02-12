// TODO move this elsewhere
const pad = (a) =>
  (a["length"] < 2
    ? "0" + a
    : a);

// TODO does this correctly handle Unicode ?
const percentEncode = (a) =>
  "%" + pad(a["charCodeAt"](0)["toString"](16)["toUpperCase"]());

// TODO does this correctly handle Unicode ?
const percentDecode = (a) =>
  String["fromCharCode"](parseInt(a["slice"](1), 16));

// TODO does this correctly handle Unicode ?
// This handles ext4 and NTFS
export const encode = (a) =>
  (a === "."
    ? "%2E"
    : (a === ".."
        ? "%2E%2E"
        : a["replace"](/[\u0000\/\\\:\*\?\u0022\<\>\|\%]/g, percentEncode)));

// TODO does this correctly handle Unicode ?
export const decode = (a) =>
  a["replace"](/\%[0-9a-fA-F]{2}/g, percentDecode);

// TODO what about Unicode ?
export const is_valid = (a) =>
  a !== "." && a !== ".." && /^[^\u0000\/\\\:\*\?\u0022\<\>\|]+$/["test"](a);
