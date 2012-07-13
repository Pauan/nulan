from nu_types import *

def from_hex(args):
  return int("".join(args), 16)

def to_hex(x):
  return "{:0=4X}".format(x)


char_white = " \n"
char_num   = "0123456789"
char_lower = "abcdefghijklmnopqrstuvwxyz"
char_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
char_sym   = char_num + char_lower + char_upper + "$%!?&" + "+-*/<>="


class IterBuffer(object):
  def __init__(self, f):
    self._next  = iter(f).next
    self._empty = False
    try:
      self._current = self._next()
    except StopIteration:
      self._empty = True

  def __iter__(self):
    return self

  @property
  def current(self):
    if self._empty:
      raise StopIteration
    else:
      return self._current

  def next(self):
    old = self.current
    try:
      self._current = self._next()
    except StopIteration:
      self._empty = True
    return old

class CharBuffer(IterBuffer):
  def __init__(self, x):
    IterBuffer.__init__(self, x)
    self.line   = 1
    self.column = 0
    self.seen   = []

  def next(self):
    x = IterBuffer.next(self)
    #x = self._next()
    if x == "\n":
      self.line  += 1
      self.column = 0
      self.seen   = []
    else:
      self.column += 1
      self.seen.append(x)
    return x

class IOBuffer(CharBuffer):
  def __init__(self, f, name=None, isatty=False):
    if isinstance(f, file):
      def i(f):
        while 1:
          x = f.read(1)
          if x == "":
            return
          else:
            yield x
      self.filename = name or f.name
      self.isatty = isatty or f.isatty()
      CharBuffer.__init__(self, i(f))
    else:
      self.filename = name
      self.isatty   = isatty
      CharBuffer.__init__(self, f)


class Token(object):
  bind = 0
  def __init__(self, s):
    self.line   = s.line
    self.column = s.column
  def __str__(self):
    return "Token: {}".format(self.__class__.__name__)

class InfixToken(Token):
  pass

class EndToken(Token):
  pass

class LiteralToken(Token):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return str(self.value)
  def nud(self, s):
    return self.value

def nud_pre(pre, bind=None):
  def f(self, s):
    b = self.bind if bind is None else bind
    return w_Seq(pre, w_Seq(parse(s, b), w_nil))
  return f

def led_pre(pre, bind=None):
  def f(self, left, s):
    b = self.bind if bind is None else bind
    return w_Seq(pre, w_Seq(left, w_Seq(parse(s, b), w_nil)))
  return f

def parse_until(end_token):
  def decorator(fn):
    def wrapped(self, s):
      result = []
      while 1:
        c = s.current
        if isinstance(c, end_token):
          break
        else:
          expr = parse(s)
          if expr == w_arrow:
            result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
          else:
            result.append(expr)
      s.next()
      return fn(list_to_seq(result), s)
    return wrapped
  return decorator

def unwrap_if(typ):
  infix = [w_mul, w_div, w_add, w_sub, w_lt, w_gt, w_lte, w_gte, w_is]
  def decorator(fn):
    def wrapped(x, s):
      if x == w_nil:
        return x
      elif x.rest == w_nil:
        try:
          if x.first.first in infix:
            return fn(x.first, s)
        except AttributeError:
          pass
      return fn(x, s)
    return wrapped
  return decorator


class EOFToken(EndToken):
  def nud(self, s):
    return w_eof

class DedentToken(EndToken):
  def __str__(self):
    return "<DE>"
  def nud(self, s):
    return parse(s)

class EndRoundToken(EndToken):
  def __str__(self):
    return ")"

class EndSquareToken(EndToken):
  def __str__(self):
    return "]"

class EndCurlyToken(EndToken):
  def __str__(self):
    return "}"


class SpliceToken(Token):
  def __str__(self):
    return "@"
  def nud(self, s):
    return w_Seq(w_splice, w_Seq(parse(s), w_nil))


class IndentToken(Token):
  def __str__(self):
    return "<IN>"
  @parse_until(DedentToken)
  def nud(x, s):
    if x == w_nil:
      #if isinstance(s.current, EndToken):
      #  return x
      #else:
      return parse(s)
    elif x.rest == w_nil:
      return x.first
    else:
      return x

class RoundToken(Token):
  def __str__(self):
    return "("
  @parse_until(EndRoundToken)
  @unwrap_if(InfixToken)
  def nud(x, s):
    return x

class SquareToken(Token):
  def __str__(self):
    return "["
  @parse_until(EndSquareToken)
  @unwrap_if(InfixToken)
  def nud(x, s):
    return w_Seq(w_seq, x)

class CurlyToken(Token):
  def __str__(self):
    return "{"
  @parse_until(EndCurlyToken)
  def nud(x, s):
    return w_Seq(w_dict, x)


class MulToken(InfixToken):
  bind = 40
  led  = led_pre(w_mul)

class DivToken(InfixToken):
  bind = 40
  led  = led_pre(w_div)

class AddToken(InfixToken):
  bind = 30
  nud  = nud_pre(w_add, 50)
  led  = led_pre(w_add)

class SubToken(InfixToken):
  bind = 30
  nud  = nud_pre(w_sub, 50)
  led  = led_pre(w_sub)

class LtToken(InfixToken):
  bind = 20
  led  = led_pre(w_lt)

class LteToken(InfixToken):
  bind = 20
  led  = led_pre(w_lte)

class GtToken(InfixToken):
  bind = 20
  led  = led_pre(w_gt)

class GteToken(InfixToken):
  bind = 20
  led  = led_pre(w_gte)

class EqualToken(InfixToken):
  bind = 10
  led  = led_pre(w_is)


## Pratt's Top Down Operator Precedence Parser.
## Yes, the entire parsing function really is this tiny.
## What'cha gonna do about it?
def parse(s, bind=0):
  left = s.next().nud(s)
  while bind < s.current.bind:
    left = s.next().led(left, s)
  return left


def parse_string(fn):
  def wrapped(q, s):
    result = []
    info = {
      "at_start" : True,
      "indent"   : 0,
      "column"   : s.column,
      "quote"    : q,
    }
    try:
      s.next()
      while 1:
        c = s.current
        if c == q:
          s.next()
          return w_Seq(w_str, list_to_seq(result))
        else:
          fn(result, s, info)
    except StopIteration:
      raise w_Thrown(w_SyntaxError("missing ending {} quote".format(q), s))
  return wrapped

@parse_string
def parse_double_quotes(result, s, info):
  c = s.current
  if (c == " " and
      info["at_start"] and
      info["indent"] < info["column"]):
    info["indent"] += 1
    s.next()
  else:
    info["at_start"] = False
    s.next()
    if c == "@":
      result.append(parse(tokenize(s, indent=False, one=True)))
    else:
      if c == "\n":
        info["at_start"] = True
        info["indent"]   = 0
      if c == "\\":
        c = s.next()
        if c == info["quote"] or c in "\\@":
          c = w_Char(c)
        elif c == "n":
          c = w_Char("\n")
          c.tostring = "\\n"
        elif c == "t":
          c = w_Char("\t")
          c.tostring = "\\t"
        elif c == "u":
          def f(x):
            if x in "0123456789ABCDEF":
              return x
            else:
              raise w_Thrown(w_SyntaxError("{} is not valid hexadecimal".format(x), s))
          h = "".join(f(s.next()) for x in range(4))
          c = w_Char(unichr(from_hex(h)))
          c.tostring = "\\u{}".format(h)
        else:
          raise w_Thrown(w_SyntaxError("unknown escape sequence {}".format(c), s))
      else:
        c = w_Char(c)
      c.line   = s.line
      c.column = s.column
      result.append(c)

@parse_string
def parse_back_quotes(result, s, info):
  result.append(w_Char(s.next(), line=s.line, column=s.column))

##  #\|[\s\S]*?\|#
##  #.*
def parse_comment(s):
  if s.current == "|":
    try:
      while 1:
        s.next()
        c = s.current
        if c == "#":
          s.next()
          if s.current == "|":
            parse_comment(s)
        elif c == "|":
          s.next()
          if s.current == "#":
            s.next()
            break
    except StopIteration:
      raise w_Thrown(w_SyntaxError("missing ending |# block", s))
  else:
    while s.next() != "\n":
      pass

##  \d+\.?\d+
def parse_number(s):
  result = []
  line   = s.line
  column = s.column
  dot    = False ## Has the dot been seen yet?
  try:
    while 1:
      c = s.current
      if c in char_num:
        result.append(s.next())
      elif c == "." and not dot:
        dot = True
        result.append(s.next())
      else:
        break
  except StopIteration:
    pass
  return w_Number("".join(result), line=line, column=column)

##  [a-zA-Z0-9\-$%!?]+
def parse_symbol(s):
  result = []
  line   = s.line
  column = s.column
  try:
    while s.current in char_sym:
      result.append(s.next())
  except StopIteration:
    pass
  return w_Sym("".join(result), line=line, column=column)

def find_indent(s):
  indent = 0
  while s.current == " ":
    indent += 1
    s.next()
  return indent

def tokenize(s, indent=True, one=False):
  def f(s, one, stop):
    braces     = []
    indents    = []
    #colons     = []
    #semicolons = []
    on_end     = None
    #indent_seen = True
    if indent:
      new    = find_indent(s)
      on_end = IndentToken
      stop   = False
      braces.append((IndentToken, DedentToken))
      yield IndentToken(s)
      indents.append(new)
    c = s.current
    try:
      while 1:
        if c == "#":
          s.next()
          parse_comment(s)
        elif c == " ":
          s.next()
        elif c == "\n":
          s.next()
          if s.current == "\n" and s.isatty:
            raise StopIteration
          elif indent:
            #if braces[-1][0] != IndentToken:
            #  continue

              #raise w_Thrown(w_SyntaxError("missing parentheses {}".format(b), s))
            #while colons and colons[-1][0] == IndentToken:
            #  yield colons.pop()[1](s)
            #while braces and braces[-1][0] == ":":
            #  yield braces.pop()[1](s)
            while braces:
              b = braces[-1]
              if b[0] == ":" and b[2] == IndentToken:
                yield braces.pop()[1](s)
              else:
                break
            new = find_indent(s)
            #print on_end, new, indents
            if on_end == IndentToken:
              indents.pop()
              yield DedentToken(s)
            if indents:
              if new <= indents[-1]:
                #while braces and braces[-1][0] == ";":
                #  yield braces.pop()[1](s)
                while braces:
                  b = braces[-1]
                  if b[0] == ";" and b[2] == IndentToken:
                    yield braces.pop()[1](s)
                  else:
                    break
                #while semicolons and semicolons[-1][0] == IndentToken:
                #  yield semicolons.pop()[1](s)
                while indents and new <= indents[-1]:
                  indents.pop()
                  #if on_end and on_end != IndentToken:
                  #  s.next()
                  #  print s.line, s.column, s.seen, s.current
                  #  raise w_Thrown(w_SyntaxError(on_end, s))
                  b = braces[-1][0]
                  if b != IndentToken:
                    raise w_Thrown(w_SyntaxError("missing parentheses {}".format(b), s))
                  else:
                    braces.pop()
                  yield DedentToken(s)
            #if on_end == IndentToken or (indents and new >= indents[-1]):
            #on_end = "expected an expression after indentation"
            #indent_seen = True
            on_end = IndentToken
            stop   = False
            braces.append((IndentToken, DedentToken))
            yield IndentToken(s)
            indents.append(new)
        elif c == "<":
          s.next()
          try:
            if s.current == "=":
              if not braces:
                raise w_Thrown(w_SyntaxError("<= must occur inside parentheses", s))
              on_end = "expected an expression after <="
              yield LteToken(s)
              s.next()
            else:
              raise StopIteration
          except StopIteration:
            if not braces:
              raise w_Thrown(w_SyntaxError("< must occur inside parentheses", s))
            on_end = "expected an expression after <"
            yield LtToken(s)
        elif c == "=":
          s.next()
          try:
            if s.current == ">":
              if not braces:
                raise w_Thrown(w_SyntaxError("=> must occur inside parentheses", s))
              on_end = "expected an expression after =>"
              yield GteToken(s)
              s.next()
            elif s.current == "=":
              s.next()
              raise w_Thrown(w_SyntaxError("== is invalid (you probably meant to use =)", s))
            else:
              raise StopIteration
          except StopIteration:
            if not braces:
              raise w_Thrown(w_SyntaxError("= must occur inside parentheses", s))
            on_end = "expected an expression after ="
            yield EqualToken(s)
        elif c == ">":
          s.next()
          try:
            if s.current == "=":
              s.next()
              raise w_Thrown(w_SyntaxError(">= is invalid (you probably meant to use =>)", s))
            else:
              raise StopIteration
          except StopIteration:
            if not braces:
              raise w_Thrown(w_SyntaxError("> must occur inside parentheses", s))
            on_end = "expected an expression after >"
            yield GtToken(s)
        elif c == "-":
          s.next()
          try:
            if s.current == ">":
              if not braces:
                raise w_Thrown(w_SyntaxError("-> must occur inside parentheses", s))
              yield LiteralToken(w_arrow)
              s.next()
              #braces.append((IndentToken, DedentToken))
              on_end = "expected an expression after ->"
              #semicolons.append((IndentToken, DedentToken))
              braces.append((";", DedentToken, IndentToken))
              yield IndentToken(s)
              #colon_seen = True
              #if indent:
              #  stop           = False
              #  semicolon_seen = True
              #  yield IndentToken(s)
              #  indents.append(s.column)
            else:
              raise StopIteration
          except StopIteration:
            if not braces:
              raise w_Thrown(w_SyntaxError("- must occur inside parentheses", s))
            on_end = "expected an expression after -"
            yield SubToken(s)
        elif c == "\"":
          on_end = None
          yield LiteralToken(parse_double_quotes(c, s))
          if stop:
            raise StopIteration
        elif c == "`":
          on_end = None
          yield LiteralToken(parse_back_quotes(c, s))
          if stop:
            raise StopIteration
        elif c in char_num:
          on_end = None
          yield LiteralToken(parse_number(s))
          if stop:
            raise StopIteration
                                        # TODO a little hacky
        elif c in char_sym and not c in "+*/":
          on_end = None
          yield LiteralToken(parse_symbol(s))
          if stop:
            raise StopIteration
        else:
          s.next()
          if c == "~":
            on_end = None
            yield LiteralToken(w_tilde)
            if stop:
              raise StopIteration
          elif c == "+":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after +"
            yield AddToken(s)
          elif c == "*":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after *"
            yield MulToken(s)
          elif c == "/":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after /"
            yield DivToken(s)
          elif c == "(":
            on_end = None
            stop   = False
            #while colons and colons[-1][0] == RoundToken:
            #  yield colons.pop()[1](s)
            while braces:
              b = braces[-1]
              if b[0] == ":" and b[2] == RoundToken:
                yield braces.pop()[1](s)
              else:
                break
            #while braces and braces[-1][0] == ":":
            #  yield braces.pop()[1](s)
            braces.append((RoundToken, EndRoundToken))
            yield RoundToken(s)
          elif c == "[":
            on_end = None
            stop   = False
            #while colons and colons[-1][0] == SquareToken:
            #  yield colons.pop()[1](s)
            while braces:
              b = braces[-1]
              if b[0] == ":" and b[2] == SquareToken:
                yield braces.pop()[1](s)
              else:
                break
            #while braces and braces[-1][0] == ":":
            #  yield braces.pop()[1](s)
            braces.append((SquareToken, EndSquareToken))
            yield SquareToken(s)
          elif c == "{":
            on_end = None
            stop   = False
            #while colons and colons[-1][0] == CurlyToken:
            #  yield colons.pop()[1](s)
            while braces:
              b = braces[-1]
              if b[0] == ":" and b[2] == CurlyToken:
                yield braces.pop()[1](s)
              else:
                break
            #while braces and braces[-1][0] == ":":
            #  yield braces.pop()[1](s)
            braces.append((CurlyToken, EndCurlyToken))
            yield CurlyToken(s)
          elif c == ")":
            if on_end:
              raise w_Thrown(w_SyntaxError(on_end, s))
            old = braces[-1]
            if not (old[0] == ":" or old[0] == ";" or old[0] == RoundToken):
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            #while colons and colons[-1][0] == b[0]:
            #  yield colons.pop()[1](s)
            #while semicolons and semicolons[-1][0] == b[0]:
            #  yield semicolons.pop()[1](s)
            #while braces:
            #  b = braces[-1][0]
            #  if b == ":" or b == ";":
            #    yield braces.pop()[1](s)
            #  else:
            #    break
            while braces:
              b = braces[-1]
              if (b[0] == ":" or b[0] == ";") and b[2] == old[0]:
                yield braces.pop()[1](s)
              else:
                break
            yield EndRoundToken(s)
            if one:
              raise StopIteration
          elif c == "]":
            if on_end:
              raise w_Thrown(w_SyntaxError(on_end, s))
            old = braces[-1]
            if not (old[0] == ":" or old[0] == ";" or old[0] == SquareToken):
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            #while colons and colons[-1][0] == b[0]:
            #  yield colons.pop()[1](s)
            #while semicolons and semicolons[-1][0] == b[0]:
            #  yield semicolons.pop()[1](s)
            #while braces:
            #  b = braces[-1][0]
            #  if b == ":" or b == ";":
            #    yield braces.pop()[1](s)
            #  else:
            #    break
            while braces:
              b = braces[-1]
              if (b[0] == ":" or b[0] == ";") and b[2] == old[0]:
                yield braces.pop()[1](s)
              else:
                break
            yield EndSquareToken(s)
            if one:
              raise StopIteration
          elif c == "}":
            if on_end:
              raise w_Thrown(w_SyntaxError(on_end, s))
            old = braces[-1]
            if not (old[0] == ":" or old[0] == ";" or old[0] == CurlyToken):
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            #while colons and colons[-1][0] == b[0]:
            #  yield colons.pop()[1](s)
            #while semicolons and semicolons[-1][0] == b[0]:
            #  yield semicolons.pop()[1](s)
            #while braces:
            #  b = braces[-1][0]
            #  if b == ":" or b == ";":
            #    yield braces.pop()[1](s)
            #  else:
            #    break
            while braces:
              b = braces[-1]
              if (b[0] == ":" or b[0] == ";") and b[2] == old[0]:
                yield braces.pop()[1](s)
              else:
                break
            yield EndCurlyToken(s)
            if one:
              raise StopIteration
          elif c == ":":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after :"
            old = braces[-1]
            if old[0] == IndentToken:
              braces.append((":", EndRoundToken, old[0]))
              #colons.append((IndentToken, EndRoundToken))
              yield RoundToken(s)
            else:
              braces.append((":", old[1], old[0]))
              #colons.append((b[0], b[1]))
              yield old[0](s)
          elif c == ";":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after ;"
            old = braces[-1]
            while braces:
              b = braces[-1]
              if b[0] == ";" and b[2] == old[0]:
                yield braces.pop()[1](s)
              else:
                break
            #while semicolons and semicolons[-1][0] == b[0]:
            #  yield semicolons.pop()[1](s)
            if old[0] == IndentToken:
              braces.append((";", EndRoundToken, old[0]))
              #semicolons.append((IndentToken, EndRoundToken))
              yield RoundToken(s)
            else:
              braces.append((";", old[1], old[0]))
              #semicolons.append((b[0], b[1]))
              yield old[0](s)
          elif c == "@":
            if not braces:
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            on_end = "expected an expression after @"
            yield SpliceToken(s)
          else:
            raise w_Thrown(w_SyntaxError("invalid character {}".format(c), s))
        c = s.current
    except StopIteration:
      if on_end and on_end != IndentToken:
        raise w_Thrown(w_SyntaxError(on_end, s))
      while braces:
        b = braces[-1][0]
        # TODO
        if b == ":" or b == ";":
          yield braces.pop()[1](s)
        elif b != IndentToken:
          #yield braces.pop()[1](s)
          #print braces[-1]
          raise w_Thrown(w_SyntaxError("missing parentheses {!s}".format(braces[-1][1]), s))
        else:
          if indent and indents:
            yield DedentToken(s)
            indents.pop()
          braces.pop()
      ## TODO verify this works correctly with funky paren mismatches
      #while colons:
      #  yield colons.pop()[1](s)
      #while semicolons:
      #  yield semicolons.pop()[1](s)
      #if indent:
      #  while indents:
      #    indents.pop()
      #    yield DedentToken(s)
      yield EOFToken(s)
  return IterBuffer(f(s, one, one))


def read1(s, eof=w_eof):
  try:
    return parse(tokenize(IOBuffer(s)))
  except StopIteration:
    return eof

def read_all(s):
  s = tokenize(IOBuffer(s))
  try:
    while 1:
      yield parse(s)
  except w_Thrown as e:
    sys.stderr.write("{}\n".format(e))
    return
  except StopIteration:
    return
  except Exception as e:
    print e

def read_file(name):
  f = open(name, "r")
  try:
    for x in read_all(f):
      yield x
  except StopIteration:
    f.close()
