const keywords = [
  "abstract",
  "arguments",
  "boolean",
  "break",
  "byte",
  "case",
  "catch",
  "char",
  "class",
  "comment",
  "const",
  "continue",
  "debugger",
  "default",
  "delete",
  "do",
  "double",
  "else",
  "enum",
  "eval",
  "export",
  "extends",
  "false",
  "final",
  "finally",
  "float",
  "for",
  "function",
  "function*",
  "goto",
  "if",
  "implements",
  "import",
  "in",
  "instanceOf",
  "int",
  "interface",
  "label",
  "long",
  "module",
  "native",
  "new",
  "null",
  "package",
  "private",
  "protected",
  "public",
  "return",
  "short",
  "static",
  "super",
  "switch",
  "synchronized",
  "this",
  "throw",
  "throws",
  "transient",
  "true",
  "try",
  "typeof",
  "var",
  "void",
  "while",
  "with",
  "yield"
];

export const mangle_name = (value) => {
  if (keywords.indexOf(value) === -1) {
    return value.replace(/^[0-9]|[^0-9a-zA-Z$]/g, (s) => {
      return "_" + s.charCodeAt(0).toString(16) + "_";
    });

  } else {
    return "__" + value;
  }
};