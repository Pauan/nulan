import { Loc } from "./loc";
import { NulanError } from "./error";


export function expectError(fn: () => void, loc: Loc, message: string): void {
  // TODO fix this type error
  expect(fn).toThrow(<any>new NulanError(loc, message));
}
