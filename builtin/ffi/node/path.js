import * as $path from "node:path";


// TODO better version of this ?
export const parent = (x) =>
  $path.dirname(x);

// TODO better version of this ?
export const filename = (x) =>
  $path.basename(x);

// TODO better version of this ?
export const extension = (x) =>
  $path.extname(x);

// TODO better version of this ?
export const concat = (x, y) =>
  $path.join(x, y);
