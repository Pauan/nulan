// TODO inline this
// TODO is this correct ?
export const bitNot = (a) => (~a) >>> 0;

// TODO inline this
// TODO is this correct ?
export const bitAnd = (a, b) => (a & b) >>> 0;

// TODO inline this
// TODO is this correct ?
export const bitOr = (a, b) => (a | b) >>> 0;

// TODO inline this
// TODO is this correct ?
export const bitXor = (a, b) => (a ^ b) >>> 0;

// TODO inline this
// TODO is this correct ?
export const bitLeftShift = (a, b) => (a << b) >>> 0;

// TODO inline this
// TODO is this the same as (a >> b) >>> 0 ?
export const bitRightShift = (a, b) => a >>> b;

// TODO inline this
export const add = (a, b) => (a + b) >>> 0;

// TODO inline this
export const subtract = (a, b) => (a - b) >>> 0;

// Truncates its result
// TODO inline this
export const divide = (a, b) => (a / b) >>> 0;

// TODO inline this
// TODO is this correct ?
export const multiply = (a, b) => Math.imul(a, b) >>> 0;

// TODO is it faster to wrap this in a lambda or not ?
// TODO inline this
// TODO should this use >>> 0 ?
// Will always be between 0-32
export const bitLeadingZeros = (a) => Math.clz32(a);
