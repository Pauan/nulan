// TODO inline this
export const bitNot = (a) => ~a;

// TODO inline this
export const bitAnd = (a, b) => a & b;

// TODO inline this
export const bitOr = (a, b) => a | b;

// TODO inline this
export const bitXor = (a, b) => a ^ b;

// TODO should the second argument be a Uint32 ?
// TODO inline this
export const bitLeftShift = (a, b) => a << b;

// TODO should the second argument be a Uint32 ?
// TODO inline this
export const bitRightShift = (a, b) => a >> b;

// TODO inline this
export const add = (a, b) => (a + b) | 0;

// TODO inline this
export const subtract = (a, b) => (a - b) | 0;

// Truncates its result
// TODO inline this
export const divide = (a, b) => (a / b) | 0;

// TODO is it faster to wrap this in a lambda or not ?
// TODO inline this
export const multiply = Math.imul;

// TODO is it faster to wrap this in a lambda or not ?
// TODO inline this
// TODO should this use | 0 ?
// TODO is this correct ?
// Will always be between 0-32
export const bitLeadingZeros = (a) => Math.clz32(a);
