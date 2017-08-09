import { EOL } from "./node";


test("EOL", () => {
  expect(EOL).toMatch(/^\n|\r\n$/);
});
