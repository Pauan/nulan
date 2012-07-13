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
      raise EOFError
      #self._empty = True

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
      #if x != " " or self.seen:
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
    self.line     = s.line
    self.column   = s.column
    self.seen     = list(s.seen) # Copies the list
    self.filename = s.filename
  def __str__(self):
    try:
      return str(self.tostr)
    except AttributeError:
      return "Token: {}".format(self.__class__.__name__)

class LiteralToken(Token):
  def __init__(self, value):
    self.value = value
    self.tostr = value
  def nud(self, s):
    return self.value

class StartToken(Token):
  pass

class EndToken(Token):
  pass

class PrefixToken(Token):
  pass

class InfixToken(PrefixToken):
  pass

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
        elif isinstance(c, ArrowToken):
          s.next()
          #print result
          result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
        else:
          #expr = parse(s)
          #if expr == w_arrow:
          #  result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
          #else:
          result.append(parse(s)) #expr
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


class EOFToken(Token):
  def __init__(self):
    pass
  def nud(self, s):
    #raise EOFError
    return w_eof


class DedentToken(EndToken):
  tostr = "deindentation"
  #def nud(self, s):
  #  return parse(s)
  def match(self):
    return IndentToken

class EndRoundToken(EndToken):
  tostr = ")"
  def match(self):
    return RoundToken

class EndSquareToken(EndToken):
  tostr = "]"
  def match(self):
    return SquareToken

class EndCurlyToken(EndToken):
  tostr = "}"
  def match(self):
    return CurlyToken


class IndentToken(StartToken):
  tostr = "indentation"
  @parse_until(DedentToken)
  def nud(x, s):
    if x == w_nil:
      return parse(s)
    elif x.rest == w_nil:
      return x.first
    else:
      return x
  def match(self):
    return DedentToken

class RoundToken(StartToken):
  tostr = "("
  @parse_until(EndRoundToken)
  @unwrap_if(InfixToken)
  def nud(x, s):
    return x
  def match(self):
    return EndRoundToken

class SquareToken(StartToken):
  tostr = "["
  @parse_until(EndSquareToken)
  @unwrap_if(InfixToken)
  def nud(x, s):
    return w_Seq(w_seq, x)
  def match(self):
    return EndSquareToken

class CurlyToken(StartToken):
  tostr = "{"
  @parse_until(EndCurlyToken)
  def nud(x, s):
    return w_Seq(w_dict, x)
  def match(self):
    return EndCurlyToken


class ArrowToken(PrefixToken):
  tostr = "->"

class ColonToken(PrefixToken):
  tostr = ":"

class SemicolonToken(PrefixToken):
  tostr = ";"

class SpliceToken(PrefixToken):
  tostr = "@"
  def nud(self, s):
    return w_Seq(w_splice, w_Seq(parse(s), w_nil))


class MulToken(InfixToken):
  tostr = "*"
  bind  = 40
  led   = led_pre(w_mul)

class DivToken(InfixToken):
  tostr = "/"
  bind  = 40
  led   = led_pre(w_div)

class AddToken(InfixToken):
  tostr = "+"
  bind  = 30
  nud   = nud_pre(w_add, 50)
  led   = led_pre(w_add)

class SubToken(InfixToken):
  tostr = "-"
  bind  = 30
  nud   = nud_pre(w_sub, 50)
  led   = led_pre(w_sub)

class LtToken(InfixToken):
  tostr = "<"
  bind  = 20
  led   = led_pre(w_lt)

class LteToken(InfixToken):
  tostr = "<="
  bind  = 20
  led   = led_pre(w_lte)

class GtToken(InfixToken):
  tostr = ">"
  bind  = 20
  led   = led_pre(w_gt)

class GteToken(InfixToken):
  tostr = "=>"
  bind  = 20
  led   = led_pre(w_gte)

class EqualToken(InfixToken):
  tostr = "="
  bind  = 10
  led   = led_pre(w_is)


## Pratt's Top Down Operator Precedence Parser.
## Yes, the entire parsing function really is this tiny.
## What'cha gonna do about it?
def parse(s, bind=0):
  left = s.next().nud(s)
  while bind < s.current.bind:
    left = s.next().led(left, s)
  return left


def tokenize_string(fn):
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

@tokenize_string
def tokenize_double_quotes(result, s, info):
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

@tokenize_string
def tokenize_back_quotes(result, s, info):
  result.append(w_Char(s.next(), line=s.line, column=s.column))

##  #\|[\s\S]*?\|#
##  #.*
def tokenize_comment(s):
  if s.current == "|":
    try:
      while 1:
        s.next()
        c = s.current
        if c == "#":
          s.next()
          if s.current == "|":
            tokenize_comment(s)
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
def tokenize_number(s):
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
def tokenize_symbol(s):
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

def tokenize1(s):
  indents = []
  braces  = 0
  new = find_indent(s)
  yield IndentToken(s)
  indents.append(new)
  try:
    while 1:
      c = s.current
      if c == "#":
        s.next()
        tokenize_comment(s)
      elif c == " ":
        s.next()
      elif c == "\n":
        s.next()
        if s.current == "\n" and s.isatty:
          raise StopIteration
          #raise EOFError
        else:
          if braces == 0:
            new = find_indent(s)
            while indents and new <= indents[-1]:
              indents.pop()
              yield DedentToken(s)
            yield IndentToken(s)
            indents.append(new)
      elif c == "<":
        s.next()
        try:
          if s.current == "=":
            s.next()
            yield LteToken(s)
          else:
            raise StopIteration
        except StopIteration:
          yield LtToken(s)
      elif c == "=":
        s.next()
        try:
          if s.current == ">":
            yield GteToken(s)
            s.next()
          elif s.current == "=":
            s.next()
            raise w_Thrown(w_SyntaxError("== is invalid (you probably meant to use =)", s))
          else:
            raise StopIteration
        except StopIteration:
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
          yield GtToken(s)
      elif c == "-":
        s.next()
        try:
          if s.current == ">":
            s.next()
            yield ArrowToken(s)
          else:
            raise StopIteration
        except StopIteration:
          yield SubToken(s)
      elif c == "+":
        s.next()
        yield AddToken(s)
      elif c == "*":
        s.next()
        yield MulToken(s)
      elif c == "/":
        s.next()
        yield DivToken(s)
      elif c == "\"":
        yield LiteralToken(tokenize_double_quotes(c, s))
      elif c == "`":
        yield LiteralToken(tokenize_back_quotes(c, s))
      elif c in char_num:
        yield LiteralToken(tokenize_number(s))
      elif c in char_sym:
        yield LiteralToken(tokenize_symbol(s))
      else:
        s.next()
        if c == "~":
          yield LiteralToken(w_tilde)
        elif c == "(":
          stop = False
          braces += 1
          yield RoundToken(s)
        elif c == "[":
          stop = False
          braces += 1
          yield SquareToken(s)
        elif c == "{":
          stop = False
          braces += 1
          yield CurlyToken(s)
        elif c == ")":
          braces -= 1
          yield EndRoundToken(s)
        elif c == "]":
          braces -= 1
          yield EndSquareToken(s)
        elif c == "}":
          braces -= 1
          yield EndCurlyToken(s)
        elif c == ":":
          yield ColonToken(s)
        elif c == ";":
          yield SemicolonToken(s)
        elif c == "@":
          yield SpliceToken(s)
        else:
          raise w_Thrown(w_SyntaxError("invalid character {}".format(c), s))
  except StopIteration:
    while indents:
      indents.pop()
      yield DedentToken(s)
    yield EOFToken()

def tokenize(s, indent=True, one=False):
  def f(s, stop, one):
    braces = []
    c = s.next()
    while 1:
      if isinstance(c, EOFToken):
        yield c
        return
      elif not indent and isinstance(c, (IndentToken, DedentToken)):
        pass
      # Is the indent token immediately followed by a dedent token?
      elif isinstance(c, IndentToken) and isinstance(s.current, DedentToken):
        s.next()
      elif isinstance(c, StartToken):
        #new = s.next()
        #if isinstance(c, IndentToken) and isinstance(new, DedentToken):
        #  pass
        #else:
        #  ...
        #  c = new
        #  continue
        # TODO pretty damn hacky in general
        try:
          b = braces[-1]
          if isinstance(c, type(b["start"])):
            b, colons = b["end"], b["colons"]
            while colons and colons[-1] == ":":
              if colons.pop() != "->" and isinstance(b, DedentToken):
                yield EndRoundToken(c)
              else:
                yield b
        except (KeyError, IndexError):
          pass
        stop = False
        braces.append({ "start": c, "end": c.match()(c) })
        yield c
      elif isinstance(c, EndToken):
        if braces:
          b = braces.pop()
          try:
            colons = b["colons"]
            while colons:
              if colons.pop() != "->" and isinstance(b["end"], DedentToken):
                yield EndRoundToken(c)
              else:
                yield b["end"]
          except KeyError:
            pass
          b = b["end"]
          if not isinstance(b, type(c)):
            if isinstance(b, DedentToken):
              b = c.match().tostr
              raise w_Thrown(w_SyntaxError("missing starting {} parenthesis".format(b), c))
            elif isinstance(c, DedentToken):
              b = b.tostr
              raise w_Thrown(w_SyntaxError("missing ending {} parenthesis".format(b), c))
            else:
              raise w_Thrown(w_SyntaxError("mismatched parentheses", c))
        else:
          #continue
          b = c.match().tostr
          raise w_Thrown(w_SyntaxError("missing starting {} parenthesis".format(b), c))
        yield c
        if one and not braces:
          yield EOFToken()
          return
      elif isinstance(c, PrefixToken):
        if not braces:
          raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), c))
        if isinstance(s.current, (EndToken, EOFToken)):
          raise w_Thrown(w_SyntaxError("expected an expression after {}".format(c), s.current))
        if isinstance(c, ArrowToken):
          yield c
          #braces.append({ "start": IndentToken(c),
          #                "end": DedentToken(c),
          #                "colons": ["->"] })
          #yield IndentToken(c)
          if not isinstance(s.current, StartToken):
            b = braces[-1]
            try:
              b["colon_first"]
            except KeyError:
              b["colon_first"] = "->"
            try:
              colons = b["colons"]
            except KeyError:
              colons = b["colons"] = []
            yield b["start"]
            colons.append("->")
        elif isinstance(c, ColonToken):
          b = braces[-1]
          try:
            if b["colon_first"] == ";":
              raise w_Thrown(w_SyntaxError("can't use : after ;", c))
          except KeyError:
            b["colon_first"] = ":"
          try:
            colons = b["colons"]
          except KeyError:
            colons = b["colons"] = []
          if isinstance(b["start"], IndentToken):
            yield RoundToken(c)
          else:
            yield b["start"]
          colons.append(":")
        elif isinstance(c, SemicolonToken):
          b = braces[-1]
          try:
            if b["colon_first"] == ";":
              raise w_Thrown(w_SyntaxError("can't use ; after ;", c))
          except KeyError:
            b["colon_first"] = ";"
          try:
            colons = b["colons"]
            x = colons.pop()
            if x != "->" and isinstance(b["end"], DedentToken):
              yield EndRoundToken(c)
            else:
              yield b["end"]
          except KeyError:
            colons = b["colons"] = []
          if isinstance(b["start"], IndentToken):
            yield RoundToken(c)
          else:
            yield b["start"]
          colons.append(";")
        else:
          yield c
      else:
        yield c
        if stop and isinstance(c, LiteralToken):
          yield EOFToken()
          return
      c = s.next()
  return IterBuffer(f(IterBuffer(tokenize1(s)), one, one))

#@token_inside_parens
#@token_prefix(":")
#def f(self, ret, when):
#  start = self.start
#  end   = self.end
#
#  ret(start)()
#  when([";", start, end], ret(end))
#
#@token_inside_parens
#@token_prefix(";")
#def f(self, ret, when):
#  start = self.start
#  end   = self.end
#
#  ret(start)()
#  if self.seen(":"):
#    when([";", end], ret(")"))
#  else:
#    when([":", ";"], self.error("can't use : or ; after ;"))
#    when([end],      ret(end))


def read1(s):
  #try:
  return parse(tokenize(IOBuffer(s)))
  #except StopIteration:
  #  raise EOFError
    #return eof

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

#with open("nu.nu", "r") as f:
#  list(tokenize(IOBuffer(f)))
