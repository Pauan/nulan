import { expectError } from "../util/test";
import { tokenize } from "./tokenize";
import { NulanError } from "../util/error";
import { loc as l, position as p } from "../util/loc";
import { symbol, integer, string, tag } from "./ast";


const f = "test.nul";


function testInfixWhitespace(s: string): void {
	expect(tokenize("a" + s + "b", f)).toEqual([
		symbol("a", l(f, p(0, 0, 0), p(1, 0, 1))),
		symbol(s, l(f, p(1, 0, 1), p(2, 0, 2))),
		symbol("b", l(f, p(2, 0, 2), p(3, 0, 3)))
	]);

	expectError(() => tokenize("a" + s, f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + " b", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + "\n", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + "\r", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + "\n\r", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + "\r\n", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize("a" + s + "\t", f),
		l(f, p(2, 0, 2), p(3, 0, 3)),
		"Invalid tab (U+0009)");

	expectError(() => tokenize("a" + s + "#", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the right of .");

	expectError(() => tokenize(s + "b", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize(" " + s + "b", f),
		l(f, p(1, 0, 1), p(2, 0, 2)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("\n" + s + "b", f),
		l(f, p(1, 1, 0), p(2, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("\r" + s + "b", f),
		l(f, p(1, 1, 0), p(2, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("\n\r" + s + "b", f),
		l(f, p(2, 1, 0), p(3, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("\r\n" + s + "b", f),
		l(f, p(2, 1, 0), p(3, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("111\n\r" + s + "b", f),
		l(f, p(5, 1, 0), p(6, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("111\r\n" + s + "b", f),
		l(f, p(5, 1, 0), p(6, 1, 1)),
		"There cannot be whitespace to the left of .");

	expectError(() => tokenize("\t" + s + "b", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"Invalid tab (U+0009)");

	expectError(() => tokenize("#//#" + s + "b", f),
		l(f, p(4, 0, 4), p(5, 0, 5)),
		"There cannot be whitespace to the left of .");
}


test("infix", () => {
	testInfixWhitespace(".");
});


test("symbol", () => {
	expect(tokenize("foo", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3)))
	]);

	expect(tokenize("foo bar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(4, 0, 4), p(7, 0, 7)))
	]);

	expect(tokenize("foo.bar|()[]{}@&~", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol(".", l(f, p(3, 0, 3), p(4, 0, 4))),
		symbol("bar", l(f, p(4, 0, 4), p(7, 0, 7))),
		symbol("|", l(f, p(7, 0, 7), p(8, 0, 8))),
		symbol("(", l(f, p(8, 0, 8), p(9, 0, 9))),
		symbol(")", l(f, p(9, 0, 9), p(10, 0, 10))),
		symbol("[", l(f, p(10, 0, 10), p(11, 0, 11))),
		symbol("]", l(f, p(11, 0, 11), p(12, 0, 12))),
		symbol("{", l(f, p(12, 0, 12), p(13, 0, 13))),
		symbol("}", l(f, p(13, 0, 13), p(14, 0, 14))),
		symbol("@", l(f, p(14, 0, 14), p(15, 0, 15))),
		symbol("&", l(f, p(15, 0, 15), p(16, 0, 16))),
		symbol("~", l(f, p(16, 0, 16), p(17, 0, 17))),
	]);

	expect(tokenize("foo!?-barABC0123_$<<=", f)).toEqual([
		symbol("foo!?-barABC0123_$<<=", l(f, p(0, 0, 0), p(21, 0, 21)))
	]);
});


test("tag", () => {
	expect(tokenize("*foo", f)).toEqual([
		tag("foo", l(f, p(0, 0, 0), p(4, 0, 4)))
	]);

	expect(tokenize("*foo!?-barABC0123_$<<=", f)).toEqual([
		tag("foo!?-barABC0123_$<<=", l(f, p(0, 0, 0), p(22, 0, 22)))
	]);

	expect(tokenize("*foo!?-bar*ABC0123_$<<=", f)).toEqual([
		tag("foo!?-bar", l(f, p(0, 0, 0), p(10, 0, 10))),
		tag("ABC0123_$<<=", l(f, p(10, 0, 10), p(23, 0, 23)))
	]);

	expect(tokenize("uh*foo!?-bar(ABC0123_$<<=)", f)).toEqual([
		symbol("uh", l(f, p(0, 0, 0), p(2, 0, 2))),
		tag("foo!?-bar", l(f, p(2, 0, 2), p(12, 0, 12))),
		symbol("(", l(f, p(12, 0, 12), p(13, 0, 13))),
		symbol("ABC0123_$<<=", l(f, p(13, 0, 13), p(25, 0, 25))),
		symbol(")", l(f, p(25, 0, 25), p(26, 0, 26)))
	]);

	expectError(() => tokenize("*", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"Tag cannot be empty");

	expectError(() => tokenize("**foo", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"Tag cannot be empty");

	expectError(() => tokenize("* foo", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"Tag cannot be empty");

	expectError(() => tokenize("*(", f),
		l(f, p(0, 0, 0), p(1, 0, 1)),
		"Tag cannot be empty");
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

	expect(tokenize("\"foo\\\"\\\\\\t\\n\\s\\rbar\"", f)).toEqual([
		string("foo\"\\\t\n \rbar", l(f, p(0, 0, 0), p(20, 0, 20)))
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

	expectError(() => tokenize("\"foo\\\t\"", f),
		l(f, p(5, 0, 5), p(6, 0, 6)),
		"Invalid tab (U+0009)");

	expectError(() => tokenize("\"foo\\", f),
		l(f, p(0, 0, 0), p(5, 0, 5)),
		"Missing ending \"");

	expectError(() => tokenize("\"foo\\1\"", f),
		l(f, p(4, 0, 4), p(6, 0, 6)),
		"Invalid \\1, it must be one of the following: \\<NEWLINE> \\\" \\\\ \\n \\r \\s \\t \\u");

	expectError(() => tokenize(" \"foo", f),
		l(f, p(1, 0, 1), p(5, 0, 5)),
		"Missing ending \"");

	expectError(() => tokenize(" \"foo     ", f),
		l(f, p(1, 0, 1), p(10, 0, 10)),
		"Missing ending \"");

	expectError(() => tokenize(" \"foo\n", f),
		l(f, p(1, 0, 1), p(6, 1, 0)),
		"Missing ending \"");

	expectError(() => tokenize(" \"foo\n ", f),
		l(f, p(1, 0, 1), p(7, 1, 1)),
		"Missing ending \"");

	expectError(() => tokenize(" \"foo\n\"", f),
		l(f, p(6, 1, 0), p(6, 1, 0)),
		"There must be at least 2 spaces, but there are 0");

	expectError(() => tokenize(" \"foo\n \"", f),
		l(f, p(6, 1, 0), p(7, 1, 1)),
		"There must be at least 2 spaces, but there is 1");

	expectError(() => tokenize(" \"foo\n  bar   \n  \"", f),
		l(f, p(11, 1, 5), p(14, 1, 8)),
		"Spaces are not allowed at the end of the line, but there are 3");

	expectError(() => tokenize(" \"foo\n      \n\"", f),
		l(f, p(6, 1, 0), p(12, 1, 6)),
		"Spaces are not allowed at the end of the line, but there are 6");

	expectError(() => tokenize("\"foo\n \n bar\"", f),
		l(f, p(5, 1, 0), p(6, 1, 1)),
		"Spaces are not allowed at the end of the line, but there is 1");

	expectError(() => tokenize("\"foo\n   \n bar\"", f),
		l(f, p(5, 1, 0), p(8, 1, 3)),
		"Spaces are not allowed at the end of the line, but there are 3");

	expectError(() => tokenize("\"foo\nbar", f),
		l(f, p(5, 1, 0), p(5, 1, 0)),
		"There must be at least 1 space, but there are 0");

	expectError(() => tokenize(" \"foo\nbar", f),
		l(f, p(6, 1, 0), p(6, 1, 0)),
		"There must be at least 2 spaces, but there are 0");

	expectError(() => tokenize(" \"foo\n bar", f),
		l(f, p(6, 1, 0), p(7, 1, 1)),
		"There must be at least 2 spaces, but there is 1");

	expectError(() => tokenize("  \"foo\n  bar", f),
		l(f, p(7, 1, 0), p(9, 1, 2)),
		"There must be at least 3 spaces, but there are 2");

	expectError(() => tokenize("  \"foo\r  bar", f),
		l(f, p(7, 1, 0), p(9, 1, 2)),
		"There must be at least 3 spaces, but there are 2");

	expectError(() => tokenize("  \"foo\n\r  bar", f),
		l(f, p(8, 1, 0), p(10, 1, 2)),
		"There must be at least 3 spaces, but there are 2");

	expectError(() => tokenize("  \"foo\r\n  bar", f),
		l(f, p(8, 1, 0), p(10, 1, 2)),
		"There must be at least 3 spaces, but there are 2");

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


	expectError(() => tokenize("\"foo\\u", f),
		l(f, p(4, 0, 4), p(6, 0, 6)),
		"Missing starting [");

	expectError(() => tokenize("\"foo\\u[", f),
		l(f, p(4, 0, 4), p(7, 0, 7)),
		"Missing ending ]");

	expectError(() => tokenize("\"foo\\u[0", f),
		l(f, p(4, 0, 4), p(8, 0, 8)),
		"Missing ending ]");

	expectError(() => tokenize("\"foo\\u[0]", f),
		l(f, p(0, 0, 0), p(9, 0, 9)),
		"Missing ending \"");

	expectError(() => tokenize("\"foo\\u[0 ", f),
		l(f, p(4, 0, 4), p(9, 0, 9)),
		"Missing ending ]");

	expectError(() => tokenize("\"foo\\u[0 0", f),
		l(f, p(4, 0, 4), p(10, 0, 10)),
		"Missing ending ]");

	expectError(() => tokenize("\"foo\\u[0 0]", f),
		l(f, p(0, 0, 0), p(11, 0, 11)),
		"Missing ending \"");

	expectError(() => tokenize("\"foo\\u\"", f),
		l(f, p(6, 0, 6), p(7, 0, 7)),
		"Invalid \", it must be [");

	expectError(() => tokenize("\"foo\\u[A\"", f),
		l(f, p(8, 0, 8), p(9, 0, 9)),
		"Invalid \", it must be one of the following: <SPACE> ] 0 1 2 3 4 5 6 7 8 9 A B C D E F");

	expectError(() => tokenize("\"foo\\u[A\n]", f),
		l(f, p(8, 0, 8), p(9, 1, 0)),
		"Invalid <NEWLINE>, it must be one of the following: <SPACE> ] 0 1 2 3 4 5 6 7 8 9 A B C D E F");

	expectError(() => tokenize("\"foo\\u[\n]", f),
		l(f, p(7, 0, 7), p(8, 1, 0)),
		"Invalid <NEWLINE>, it must be one of the following: 0 1 2 3 4 5 6 7 8 9 A B C D E F");

	expectError(() => tokenize("\"foo\\u[a]", f),
		l(f, p(7, 0, 7), p(8, 0, 8)),
		"Invalid a, it must be one of the following: 0 1 2 3 4 5 6 7 8 9 A B C D E F");

	expectError(() => tokenize("\"foo\\u[ A]", f),
		l(f, p(7, 0, 7), p(8, 0, 8)),
		"There cannot be a space after [");

	expectError(() => tokenize("\"foo\\u[A ]", f),
		l(f, p(8, 0, 8), p(9, 0, 9)),
		"There cannot be a space before ]");

	expectError(() => tokenize("\"foo\\u[A  A]", f),
		l(f, p(8, 0, 8), p(10, 0, 10)),
		"There must be exactly 1 space, but there are 2");

	expectError(() => tokenize("\"foo\\u[A     A]", f),
		l(f, p(8, 0, 8), p(13, 0, 13)),
		"There must be exactly 1 space, but there are 5");

	expectError(() => tokenize("\"foo\\u[FFFFFF]", f),
		l(f, p(7, 0, 7), p(13, 0, 13)),
		"Invalid code point FFFFFF");


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

	expect(tokenize("\"foo\\u[10FFFF]1AF\"", f)).toEqual([
		string("foo\udbff\udfff1AF", l(f, p(0, 0, 0), p(18, 0, 18)))
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

	expectError(() => tokenize("foo\n  \n", f),
		l(f, p(4, 1, 0), p(6, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");

	expectError(() => tokenize("foo\r  \r", f),
		l(f, p(4, 1, 0), p(6, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");

	expectError(() => tokenize("foo\n\r  \n\r", f),
		l(f, p(5, 1, 0), p(7, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");

	expectError(() => tokenize("foo\r\n  \r\n", f),
		l(f, p(5, 1, 0), p(7, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");

	expectError(() => tokenize("foo\n  ", f),
		l(f, p(4, 1, 0), p(6, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");

	expectError(() => tokenize("foo\n  \nbar", f),
		l(f, p(4, 1, 0), p(6, 1, 2)),
		"Spaces are not allowed at the end of the line, but there are 2");
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

	expectError(() => tokenize("#foo\t", f),
		l(f, p(4, 0, 4), p(5, 0, 5)),
		"Invalid tab (U+0009)");

	expectError(() => tokenize("#foo     ", f),
		l(f, p(4, 0, 4), p(9, 0, 9)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#foo     \n", f),
		l(f, p(4, 0, 4), p(9, 0, 9)),
		"Spaces are not allowed at the end of the line, but there are 5");
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

	expectError(() => tokenize("foo#/barqux", f),
		l(f, p(3, 0, 3), p(11, 0, 11)),
		"Missing ending /#");


	expect(tokenize("foo#/1#/2/#3/#bar", f)).toEqual([
		symbol("foo", l(f, p(0, 0, 0), p(3, 0, 3))),
		symbol("bar", l(f, p(14, 0, 14), p(17, 0, 17)))
	]);

	expectError(() => tokenize("foo#/1#/2/#3bar", f),
		l(f, p(3, 0, 3), p(15, 0, 15)),
		"Missing ending /#");

	expectError(() => tokenize("foo#/1#/23bar", f),
		l(f, p(6, 0, 6), p(13, 0, 13)),
		"Missing ending /#");

	expectError(() => tokenize("foo#/1#/23/#bar", f),
		l(f, p(3, 0, 3), p(15, 0, 15)),
		"Missing ending /#");


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


	expectError(() => tokenize("#/foo\t/#", f),
		l(f, p(5, 0, 5), p(6, 0, 6)),
		"Invalid tab (U+0009)");

	expectError(() => tokenize("#/foo     ", f),
		l(f, p(0, 0, 0), p(10, 0, 10)),
		"Missing ending /#");

	expectError(() => tokenize("#/foo#", f),
		l(f, p(0, 0, 0), p(6, 0, 6)),
		"Missing ending /#");

	expectError(() => tokenize("#/foo/", f),
		l(f, p(0, 0, 0), p(6, 0, 6)),
		"Missing ending /#");

	expectError(() => tokenize("#/foo/# ", f),
		l(f, p(7, 0, 7), p(8, 0, 8)),
		"Spaces are not allowed at the end of the line, but there is 1");

	expectError(() => tokenize("#/foo/# \n", f),
		l(f, p(7, 0, 7), p(8, 0, 8)),
		"Spaces are not allowed at the end of the line, but there is 1");

	expectError(() => tokenize("#/foo \n/#", f),
		l(f, p(5, 0, 5), p(6, 0, 6)),
		"Spaces are not allowed at the end of the line, but there is 1");

	expectError(() => tokenize("#/foo     \n/#", f),
		l(f, p(5, 0, 5), p(10, 0, 10)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#/foo     \r/#", f),
		l(f, p(5, 0, 5), p(10, 0, 10)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#/foo     \n\r/#", f),
		l(f, p(5, 0, 5), p(10, 0, 10)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#/foo     \r\n/#", f),
		l(f, p(5, 0, 5), p(10, 0, 10)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#/foo#     \n/#", f),
		l(f, p(6, 0, 6), p(11, 0, 11)),
		"Spaces are not allowed at the end of the line, but there are 5");

	expectError(() => tokenize("#/foo/     \n/#", f),
		l(f, p(6, 0, 6), p(11, 0, 11)),
		"Spaces are not allowed at the end of the line, but there are 5");
});
