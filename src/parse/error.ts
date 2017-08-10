import { Loc } from "../util/loc";
import { NulanError } from "../util/error";


export function errorMissing(loc: Loc, message: string): NulanError {
  return new NulanError(loc, "Missing " + message);
}
