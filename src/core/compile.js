import { Type, isa } from "../common/type";

export var Code = Type();

export function Symbol(loc, name) {
  return isa(Code, "Symbol", loc, name);
}

export function Number(loc, value) {
  return isa(Code, "Number", loc, name);
}

export function String(loc, value) {
  return isa(Code, "String", loc, name);
}

export function Call(loc, fn, args) {
  return isa(Code, "Call", loc, fn, args);
}

export function If(loc, test, yes, no) {
  return isa(Code, "If", loc, test, yes, no);
}

export function Var(loc, assignments) {
  return isa(Code, "Var", loc, assignments);
}

export function Assign(loc, box, value) {
  return isa(Code, "Assign", box, value);
}
