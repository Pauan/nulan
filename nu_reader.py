from nu_types import *

def from_hex(args):
  return int("".join(args), 16)

def to_hex(x):
  return "{:0=4X}".format(x)

char_white = " \n"
char_num   = "0123456789"
char_lower = "abcdefghijklmnopqrstuvwxyz"
char_upper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
char_sym   = char_num + char_lower + char_upper + "-$%!?"

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



def transform_colons(xs, fn, line, column, last=None):
  def f(x, y):
    if x == w_nil:
      x = fn(y, line, column)
      return last(x) if last else x
    else:
      return fn(y.join(w_Seq(x, w_nil)), line, column)
  return reduce(f, (list_to_seq(x) for x in reversed(xs)), w_nil)


class Token(object):
  bind = 0
  def __init__(self, s):
    self.line   = s.line
    self.column = s.column
  def __str__(self):
    return "Token: {}".format(self.__class__.__name__)

class EndToken(Token):
  pass

class LiteralToken(Token):
  def __init__(self, value):
    self.value = value
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

def parse_until(end_token, colon=None):
  def decorator(fn):
    def wrapped(self, s):
      def f(self, fn, s, on_colon=None):
        result = []
        while 1:
          c = s.current
          if isinstance(c, end_token):
            break
          #elif isinstance(c, EndToken):
          #  raise w_Thrown(w_SyntaxError("missing ending {}".format(bracket), s))
          elif isinstance(c, BarToken):
            s.next()
            x = f(self, lambda x, s: x, s, on_colon=fn)
            print x, x.rest
            result += x
            return w_Seq(w_apply, fn(list_to_seq(result), s))
          elif isinstance(c, ColonToken):
            s.next()
            result.append(f(self, fn, s, on_colon=colon))
          elif isinstance(c, SemicolonToken):
            break
          else:
            expr = parse(s)
            if expr == w_arrow:
              result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
            else:
              result.append(expr)
        if on_colon:
          return on_colon(list_to_seq(result), s)
        else:
          return fn(list_to_seq(result), s)
      x = f(self, fn, s)
      s.next()
      return x
    return wrapped
  return decorator

def unwrap_if(*pre):
  def decorator(fn):
    def wrapped(x, s):
      if x == w_nil:
        return x
      elif x.rest == w_nil:
        try:
          if x.first.first in pre:
            return fn(x.first, s)
        except AttributeError:
          pass
      return fn(x, s)
    return wrapped
  return decorator


class EOFToken(EndToken):
  pass

class DedentToken(EndToken):
  pass

class EndRoundToken(EndToken):
  pass

class EndSquareToken(EndToken):
  pass

#class BarToken(Token):
#  pass

#class ColonToken(Token):
#  pass

#class SemicolonToken(Token):
#  pass


class IndentToken(Token):
  @parse_until(DedentToken, colon=lambda x, s: x) #, "deindentation"
  def nud(x, s):
    if x == w_nil:
      return parse(s)
    elif x.rest == w_nil:
      return x.first
    else:
      return x

class RoundToken(Token):
  @parse_until(EndRoundToken) #, ") bracket"
  @unwrap_if(w_add, w_sub, w_mul, w_div)
  def nud(x, s):
    #print repr(x)
    return x

class SquareToken(Token):
  @parse_until(EndSquareToken) #, "] bracket"
  @unwrap_if(w_add, w_sub, w_mul, w_div)
  def nud(x, s):
    return w_Seq(w_seq, x)


class MulToken(Token):
  bind = 30
  led  = led_pre(w_mul)

class DivToken(Token):
  bind = 30
  led  = led_pre(w_div)

class AddToken(Token):
  bind = 20
  nud  = nud_pre(w_add, 40)
  led  = led_pre(w_add)

class SubToken(Token):
  bind = 20
  nud  = nud_pre(w_sub, 40)
  led  = led_pre(w_sub)

class LtToken(Token):
  bind = 10
  led  = led_pre(w_lt)

class LteToken(Token):
  bind = 10
  led  = led_pre(w_lte)

class GtToken(Token):
  bind = 10
  led  = led_pre(w_gt)

class GteToken(Token):
  bind = 10
  led  = led_pre(w_gte)


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
  s.next()
  if s.current == "|":
    try:
      while 1:
        s.next()
        c = s.current
        if c == "#":
          s.next()
          if s.current == "|":
            s.next()
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
    braces  = []
    indents = []
    colon_seen    = False
    if indent:
      new = find_indent(s)
      stop           = False
      colon_seen     = False
      yield IndentToken(s)
      indents.append(new)
    try:
      while 1:
        c = s.current
        if c == "#":
          parse_comment(s)
        elif c == " ":
          s.next()
        elif c == "\n":
          s.next()
          if s.current == "\n" and s.isatty:
            raise StopIteration
          elif indent:
            new = find_indent(s)
            if new > indents[-1]:
              stop           = False
              colon_seen     = False
              yield IndentToken(s)
              indents.append(new)
            else:
              while indents:
                if indents.pop() <= new:
                  yield DedentToken(s)
                else:
                  break
              stop           = False
              colon_seen     = False
              yield IndentToken(s)
              indents.append(new)
        elif c == "<":
          c = s.next()
          if c == "=":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("<= must occur inside parentheses", s))
            yield LteToken(s)
            s.next()
          else:
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("< must occur inside parentheses", s))
            yield LtToken(s)
        elif c == ">":
          c = s.next()
          if c == "=":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError(">= must occur inside parentheses", s))
            yield GteToken(s)
            s.next()
          else:
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("> must occur inside parentheses", s))
            yield GtToken(s)
        elif c == "-":
          c = s.next()
          if c == ">":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("-> must occur inside parentheses", s))
            yield LiteralToken(w_arrow)
            s.next()
            if indent:
              stop          = False
              colon_seen    = False
              yield IndentToken(s)
              indents.append(s.column)
          else:
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("- must occur inside parentheses", s))
            yield SubToken(s)
        elif c == "\"":
          yield LiteralToken(parse_double_quotes(c, s))
          if stop:
            raise StopIteration
          #yield DoubleQuoteToken(s)
          #for x in parse_double_quotes(c, s):
          #  yield LiteralToken(x)
          #yield DoubleQuoteToken(s)
          #continue
        elif c == "`":
          yield LiteralToken(parse_back_quotes(c, s))
          if stop:
            raise StopIteration
          #yield BackQuoteToken(s)
          #for x in parse_back_quotes(c, s):
          #  yield LiteralToken(x)
          #yield BackQuoteToken(s)
          #continue
        elif c in char_num:
          yield LiteralToken(parse_number(s))
          if stop:
            raise StopIteration
        elif c in char_sym:
          yield LiteralToken(parse_symbol(s))
          if stop:
            raise StopIteration
        else:
          s.next()
          if c == "~":
            yield w_tilde
            if stop:
              raise StopIteration
          elif c == "+":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            yield AddToken(s)
          elif c == "*":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            yield MulToken(s)
          elif c == "/":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            yield DivToken(s)
          elif c == "(":
            stop          = False
            colon_seen    = False
            braces.append(")")
            yield RoundToken(s)
          elif c == "[":
            stop          = False
            colon_seen    = False
            braces.append("]")
            yield SquareToken(s)
          elif c == "{":
            stop          = False
            colon_seen    = False
            braces.append("}")
            yield CurlyToken(s)
          elif c == ")":
            b = braces[-1]
            if b != c:
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            yield EndRoundToken(s)
            if one:
              raise StopIteration
          elif c == "]":
            b = braces[-1]
            if b != c:
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            yield EndSquareToken(s)
            if one:
              raise StopIteration
          elif c == "}":
            b = braces[-1]
            if b != c:
              raise w_Thrown(w_SyntaxError("mismatched parentheses {}".format(c), s))
            else:
              braces.pop()
            yield EndCurlyToken(s)
            if one:
              raise StopIteration
          elif c == ":":
            colon_seen = True
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            yield ColonToken(s)
          elif c == ";":
            if not colon_seen:
              raise w_Thrown(w_SyntaxError("no matching :", s))
            yield SemicolonToken(s)
          elif c == "|":
            if not (braces or indents):
              raise w_Thrown(w_SyntaxError("{} must occur inside parentheses".format(c), s))
            yield BarToken(s)
          else:
            raise w_Thrown(w_SyntaxError("invalid character {}".format(c), s))
    except StopIteration:
      if braces:
        raise w_Thrown(w_SyntaxError("missing parentheses {}".format(braces[-1]), s))
      if indent:
        while indents:
          indents.pop()
          yield DedentToken(s)
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

def read_file(name):
  f = open(name, "r")
  try:
    for x in read_all(f):
      yield x
  except StopIteration:
    f.close()
