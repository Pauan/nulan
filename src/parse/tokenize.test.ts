import { tokenize } from "./tokenize";
import { NulanError } from "../util/error";
import { loc as l, position as p } from "../util/loc";
import { symbol, integer, string } from "./ast";


const f = "test.nul";


test("symbol", () => {
	expect(tokenize("foo", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3)))
	]);

	expect(tokenize("foo bar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 0, 4), p(7, 0, 7)))
	]);

	expect(tokenize("foo.bar()[]{}@&~", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol(".", l(f, p(3, 0, 3), p(4, 0, 4))),
		symbol("bar", l(f, p(4, 0, 4), p(7, 0, 7))),
		symbol("(", l(f, p(7, 0, 7), p(8, 0, 8))),
		symbol(")", l(f, p(8, 0, 8), p(9, 0, 9))),
		symbol("[", l(f, p(9, 0, 9), p(10, 0, 10))),
		symbol("]", l(f, p(10, 0, 10), p(11, 0, 11))),
		symbol("{", l(f, p(11, 0, 11), p(12, 0, 12))),
		symbol("}", l(f, p(12, 0, 12), p(13, 0, 13))),
		symbol("@", l(f, p(13, 0, 13), p(14, 0, 14))),
		symbol("&", l(f, p(14, 0, 14), p(15, 0, 15))),
		symbol("~", l(f, p(15, 0, 15), p(16, 0, 16))),
	]);
});


test("integer", () => {
	expect(tokenize("10", f)).toEqual([
		integer("10", l(f, p(0, 0, 0), p(2, 0, 2)))
	]);

	expect(tokenize("10.20", f)).toEqual([
		integer("10", l(f, p(0, 0, 0), p(2, 0, 2))),
		symbol(".", l(f, p(2, 0, 2), p(3, 0, 3))),
		integer("20", l(f, p(3, 0, 3), p(5, 0, 5)))
	]);
});


test("string", () => {
	expect(tokenize("\"foo\"", f)).toEqual([
		string("foo", l(f, p(0, 0, 0), p(5, 0, 5)))
	]);

	expect(tokenize("\"foo bar\"", f)).toEqual([
		string("foo bar", l(f, p(0, 0, 0), p(9, 0, 9)))
	]);

	expect(tokenize("\"foo\\\"\\\\\\t\\n\\rbar\"", f)).toEqual([
		string("foo\"\\\t\n\rbar", l(f, p(0, 0, 0), p(18, 0, 18)))
	]);

	expect(tokenize("  \"foo\\\n   bar\"", f)).toEqual([
		string("foobar", l(f, p(2, 0, 2), p(15, 1, 7)))
	]);

	expect(tokenize("  \"foo\\\r   bar\"", f)).toEqual([
		string("foobar", l(f, p(2, 0, 2), p(15, 1, 7)))
	]);

	expect(tokenize("  \"foo\\\n\r   bar\"", f)).toEqual([
		string("foobar", l(f, p(2, 0, 2), p(16, 1, 7)))
	]);

	expect(tokenize("  \"foo\\\r\n   bar\"", f)).toEqual([
		string("foobar", l(f, p(2, 0, 2), p(16, 1, 7)))
	]);

	expect(() => tokenize("\"foo\\\t\"", f)).toThrow("Invalid tab (U+0009)");

	expect(() => tokenize("\"foo\\", f)).toThrow("Missing ending \"");
	expect(() => tokenize("\"foo\\1\"", f)).toThrow("Invalid \\1, it must be one of the following: \\<NEWLINE> \\\" \\\\ \\n \\r \\t \\u");

	expect(() => tokenize(" \"foo", f)).toThrow("Missing ending \"");
	expect(() => tokenize(" \"foo     ", f)).toThrow("Missing ending \"");
	expect(() => tokenize(" \"foo\n", f)).toThrow("Missing ending \"");
	expect(() => tokenize(" \"foo\n ", f)).toThrow("Missing ending \"");

	expect(() => tokenize(" \"foo\n\"", f)).toThrow("There must be at least 2 spaces, but there are 0");
	expect(() => tokenize(" \"foo\n \"", f)).toThrow("There must be at least 2 spaces, but there is 1");

	expect(() => tokenize(" \"foo\n  bar   \n  \"", f)).toThrow("Spaces are not allowed at the end of the line, but there are 3");
	expect(() => tokenize(" \"foo\n      \n\"", f)).toThrow("Spaces are not allowed at the end of the line, but there are 6");
	expect(() => tokenize("\"foo\n \n bar\"", f)).toThrow("Spaces are not allowed at the end of the line, but there is 1");
	expect(() => tokenize("\"foo\n   \n bar\"", f)).toThrow("Spaces are not allowed at the end of the line, but there are 3");

	expect(() => tokenize("\"foo\nbar", f)).toThrow("There must be at least 1 space, but there are 0");
	expect(() => tokenize(" \"foo\nbar", f)).toThrow("There must be at least 2 spaces, but there are 0");
	expect(() => tokenize(" \"foo\n bar", f)).toThrow("There must be at least 2 spaces, but there is 1");
	expect(() => tokenize("  \"foo\n  bar", f)).toThrow("There must be at least 3 spaces, but there are 2");
	expect(() => tokenize("  \"foo\r  bar", f)).toThrow("There must be at least 3 spaces, but there are 2");
	expect(() => tokenize("  \"foo\n\r  bar", f)).toThrow("There must be at least 3 spaces, but there are 2");
	expect(() => tokenize("  \"foo\r\n  bar", f)).toThrow("There must be at least 3 spaces, but there are 2");

	expect(tokenize(" \"foo\n  \"", f)).toEqual([
		string("foo\n", l(f, p(1, 0, 1), p(9, 1, 3)))
	]);

	expect(tokenize(" \"foo\n        \"", f)).toEqual([
		string("foo\n      ", l(f, p(1, 0, 1), p(15, 1, 9)))
	]);

	expect(tokenize("\"foo\n bar\"", f)).toEqual([
		string("foo\nbar", l(f, p(0, 0, 0), p(10, 1, 5)))
	]);

	expect(tokenize("  \"foo\n   bar\"", f)).toEqual([
		string("foo\nbar", l(f, p(2, 0, 2), p(14, 1, 7)))
	]);

	expect(tokenize("  \"foo\n      bar\"", f)).toEqual([
		string("foo\n   bar", l(f, p(2, 0, 2), p(17, 1, 10)))
	]);

	expect(tokenize("  \"foo\n\n\n\n      bar\"", f)).toEqual([
		string("foo\n\n\n\n   bar", l(f, p(2, 0, 2), p(20, 4, 10)))
	]);


	expect(() => tokenize("\"foo\\u", f)).toThrow("Missing starting [");
	expect(() => tokenize("\"foo\\u[", f)).toThrow("Missing ending ]");
	expect(() => tokenize("\"foo\\u[0", f)).toThrow("Missing ending ]");
	expect(() => tokenize("\"foo\\u[0]", f)).toThrow("Missing ending \"");
	expect(() => tokenize("\"foo\\u[0 ", f)).toThrow("Missing ending ]");
	expect(() => tokenize("\"foo\\u[0 0", f)).toThrow("Missing ending ]");
	expect(() => tokenize("\"foo\\u[0 0]", f)).toThrow("Missing ending \"");

	expect(() => tokenize("\"foo\\u\"", f)).toThrow("Invalid \", it must be [");
	expect(() => tokenize("\"foo\\u[A\"", f)).toThrow("Invalid \", it must be one of the following: <SPACE> ] 0 1 2 3 4 5 6 7 8 9 A B C D E F");
	expect(() => tokenize("\"foo\\u[A\n]", f)).toThrow("Invalid <NEWLINE>, it must be one of the following: <SPACE> ] 0 1 2 3 4 5 6 7 8 9 A B C D E F");
	expect(() => tokenize("\"foo\\u[\n]", f)).toThrow("Invalid <NEWLINE>, it must be one of the following: 0 1 2 3 4 5 6 7 8 9 A B C D E F");
	expect(() => tokenize("\"foo\\u[a]", f)).toThrow("Invalid a, it must be one of the following: 0 1 2 3 4 5 6 7 8 9 A B C D E F");

	expect(() => tokenize("\"foo\\u[ A]", f)).toThrow("There cannot be a space after [");
	expect(() => tokenize("\"foo\\u[A ]", f)).toThrow("There cannot be a space before ]");
	expect(() => tokenize("\"foo\\u[A  A]", f)).toThrow("There must be exactly 1 space, but there are 2");
	expect(() => tokenize("\"foo\\u[A     A]", f)).toThrow("There must be exactly 1 space, but there are 5");

	// TODO better error message ?
	expect(() => tokenize("\"foo\\u[FFFFFF]", f)).toThrow("Invalid code point 16777215");

	expect(tokenize("\"foo\\u[0]\"", f)).toEqual([
		string("foo\u0000", l(f, p(0, 0, 0), p(10, 0, 10)))
	]);

	expect(tokenize("\"foo\\u[0 0 0 0]\"", f)).toEqual([
		string("foo\u0000\u0000\u0000\u0000", l(f, p(0, 0, 0), p(16, 0, 16)))
	]);

	expect(tokenize("\"foo\\u[0 9 5E 23 41]\"", f)).toEqual([
		string("foo\u0000\t^#A", l(f, p(0, 0, 0), p(21, 0, 21)))
	]);

	expect(tokenize("\"foo\\u[0000000]\"", f)).toEqual([
		string("foo\u0000", l(f, p(0, 0, 0), p(16, 0, 16)))
	]);

	expect(tokenize("\"foo\\u[10FFFF]\"", f)).toEqual([
		string("foo\udbff\udfff", l(f, p(0, 0, 0), p(15, 0, 15)))
	]);
});


test("newline", () => {
	expect(tokenize("foo\nbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 1, 0), p(7, 1, 3)))
	]);

	expect(tokenize("foo\rbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 1, 0), p(7, 1, 3)))
	]);

	expect(tokenize("foo\n\rbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(5, 1, 0), p(8, 1, 3)))
	]);

	expect(tokenize("foo\r\nbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(5, 1, 0), p(8, 1, 3)))
	]);

	expect(() => tokenize("foo\n  \n", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");
	expect(() => tokenize("foo\r  \r", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");
	expect(() => tokenize("foo\n\r  \n\r", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");
	expect(() => tokenize("foo\r\n  \r\n", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");

	expect(() => tokenize("foo\n  ", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");
	expect(() => tokenize("foo\n  \nbar", f)).toThrow("Spaces are not allowed at the end of the line, but there are 2");
});


test("line comment", () => {
	expect(tokenize("foo#", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3)))
	]);

	expect(tokenize("foo\nbar#qux\ncorge", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 1, 0), p(7, 1, 3))),
		symbol("corge", l(f, p(12, 2, 0), p(17, 2, 5)))
	]);

	expect(tokenize("foo\rbar#qux\rcorge", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 1, 0), p(7, 1, 3))),
		symbol("corge", l(f, p(12, 2, 0), p(17, 2, 5)))
	]);

	expect(tokenize("foo\n\rbar#qux\n\rcorge", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(5, 1, 0), p(8, 1, 3))),
		symbol("corge", l(f, p(14, 2, 0), p(19, 2, 5)))
	]);

	expect(tokenize("foo\r\nbar#qux\r\ncorge", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(5, 1, 0), p(8, 1, 3))),
		symbol("corge", l(f, p(14, 2, 0), p(19, 2, 5)))
	]);

	expect(tokenize("foo#    a\nbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(10, 1, 0), p(13, 1, 3)))
	]);

	expect(() => tokenize("#foo\t", f)).toThrow("Invalid tab (U+0009)");

	expect(() => tokenize("#foo     ", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#foo     \n", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
});


test("block comment", () => {
	expect(tokenize("foo#/bar/qux/#qux", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("qux", l(f, p(14, 0, 14), p(17, 0, 17)))
	]);

	expect(tokenize("foo#/bar qux/#qux", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("qux", l(f, p(14, 0, 14), p(17, 0, 17)))
	]);


	expect(tokenize("foo#/bar/#qux", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("qux", l(f, p(10, 0, 10), p(13, 0, 13)))
	]);

	expect(() => tokenize("foo#/barqux", f)).toThrow("Missing ending /#");


	expect(tokenize("foo#/1#/2/#3/#bar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(14, 0, 14), p(17, 0, 17)))
	]);

	expect(() => tokenize("foo#/1#/2/#3bar", f)).toThrow("Missing ending /#");
	expect(() => tokenize("foo#/1#/23/#bar", f)).toThrow("Missing ending /#");


	expect(tokenize("foo\n#/\n1\n#/\n2\n/#3\n/#\nbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(21, 7, 0), p(24, 7, 3)))
	]);

	expect(tokenize("foo\r#/\r1\r#/\r2\r/#3\r/#\rbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(21, 7, 0), p(24, 7, 3)))
	]);

	expect(tokenize("foo\n\r#/\n\r1\n\r#/\n\r2\n\r/#3\n\r/#\n\rbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(28, 7, 0), p(31, 7, 3)))
	]);

	expect(tokenize("foo\r\n#/\r\n1\r\n#/\r\n2\r\n/#3\r\n/#\r\nbar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(28, 7, 0), p(31, 7, 3)))
	]);


	expect(() => tokenize("#/foo\t/#", f)).toThrow("Invalid tab (U+0009)");

	expect(() => tokenize("#/foo     ", f)).toThrow("Missing ending /#");
	expect(() => tokenize("#/foo#", f)).toThrow("Missing ending /#");
	expect(() => tokenize("#/foo/", f)).toThrow("Missing ending /#");

	expect(() => tokenize("#/foo/# ", f)).toThrow("Spaces are not allowed at the end of the line, but there is 1");
	expect(() => tokenize("#/foo/# \n", f)).toThrow("Spaces are not allowed at the end of the line, but there is 1");
	expect(() => tokenize("#/foo \n/#", f)).toThrow("Spaces are not allowed at the end of the line, but there is 1");
	expect(() => tokenize("#/foo     \n/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#/foo     \r/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#/foo     \n\r/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#/foo     \r\n/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#/foo#     \n/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
	expect(() => tokenize("#/foo/     \n/#", f)).toThrow("Spaces are not allowed at the end of the line, but there are 5");
});
