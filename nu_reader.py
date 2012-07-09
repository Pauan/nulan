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

class CharBuffer(object):
  def __init__(self, x):
    self._next  = iter(x).next
    self.line   = 1
    self.column = 0
    self.seen   = []

  def next(self):
    x = self._next()
    if x == "\n":
      self.line  += 1
      self.column = 0
      self.seen   = []
    else:
      self.column += 1
      self.seen.append(x)
    return x
  def __iter__(self):
    return self

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
      CharBuffer.__init__(self, i(f))
      self.filename = name or f.name
      self.isatty = isatty or f.isatty()
    else:
      CharBuffer.__init__(self, f)
      self.filename = name
      self.isatty   = isatty

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

def transform_colons(xs, fn, line, column, last=None):
  def f(x, y):
    if x == w_nil:
      x = fn(y, line, column)
      return last(x) if last else x
    else:
      return fn(y.join(w_Seq(x, w_nil)), line, column)
  return reduce(f, (list_to_seq(x) for x in reversed(xs)), w_nil)

def parse_inside_parens(info, s,
                        bar=False,
                        priority=0,
                        colon_seen=False):
  result       = []
  colons       = []
  # TODO: correct line/column
  line         = s.line
  column       = s.column
  comment_seen = False
  try:
    while 1:
      c = s.peek()
      if info.stop(c, line, column, info, s):
        if info.on_fail:
          #if colon_seen:
          #  result.append(transform_colons(colons, info.cons, line, column))
          #  colon_seen = False
          info.on_fail(info, s, result, colons, bar, priority, colon_seen)
          break
        else:
          break
      elif c == "|":
        s.read()
        x = parse_inside_parens(info, s,
                                bar=True,
                                priority=priority,
                                colon_seen=colon_seen)
        if colon_seen:
          colons[-1].append(x)
          result.append(transform_colons(colons, info.cons, line, column,
                                         last=lambda x: w_Seq(w_apply, x)))
          return info.cons(list_to_seq(result), line, column)
        else:
          result.append(x)
          return w_Seq(w_apply, info.cons(list_to_seq(result), line, column))
      elif c == ":":
        s.read()
        if result and bar:
          raise w_SyntaxError("illegal use of |", s)
        colon_seen = True
        colons.append([])
      elif c == ";":
        s.read()
        if colon_seen:
          result.append(transform_colons(colons, info.cons, line, column))
          colons = []
          colon_seen = False
        else:
          raise w_SyntaxError("no matching :", s)
  #    elif c == "+":
  #      s.read()
  #      if priority > 10:
  #        #x = parse_inside_parens(stop, fn, s, priority=10)
  #        print priority, result, x
  #      else:
  #        x = parse_inside_parens(stop, fn, s, priority=10)
  #        #print priority, result, x
  #        result = [w_add, result[-1], x]
  #        #print result
      elif c == "-":
        s.read()
        c = s.peek()
        if c == ">":
          if info.arrow:
            s.read()
            #result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
            #try:
            #  result.append(read_inside_top(indent, seen, lambda: False, origfn, s)) # + ";"
            #except StopIteration:
            #  pass
          else:
            s.read()
            raise w_SyntaxError("cannot use arrow syntax inside lists", s)
        else:
          raise w_SyntaxError("invalid character {}".format(c), s)
  #    elif c in "*":
  #      s.read()
  #      if priority > 20:
  #        pass
  #      else:
  #        x = parse_inside_parens(stop, fn, s, priority=20)
  #        #print priority, result, x
  #        result = [w_mul, result[-1], x]
  #        #print result
      elif c in char_white:
        s.read()
      ## You can't have more than one item in `result` when `bar` is true
      elif result and bar:
        s.read()
        raise w_SyntaxError("illegal use of |", s)
      else:
        x = read1(s)
        if x is None:
          comment_seen = True
        else:
          if colon_seen:
            try:
              colons[-1].append(x)
            except IndexError:
              result.append(x)
          else:
            result.append(x)
  except StopIteration:
    if info.end:
      raise w_SyntaxError("missing ending {} brackets".format(info.end), s)
  if colon_seen:
    result.append(transform_colons(colons, info.cons, line, column))
    #colon_seen = False
  result = list_to_seq(result)
  if result == w_nil:
    if bar:
      s.read()
      raise w_SyntaxError("expected an expression after |", s)
    elif info.ignore_comments:
      if comment_seen:
        return parse_inside_parens(info, s,
                                   bar,
                                   priority,
                                   colon_seen)
      else:
        raise StopIteration
    else:
      return result
  elif bar or (result.rest == w_nil and info.unwrap):
    return result.first
  else:
    return info.cons(result, line, column)

def parse_recursive_until(sym):
  def decorator(fn):
    def wrapped(s):
      s.read()
      x = parse_inside_parens(Parser(cons=fn, end=sym), s)
      s.read()
      return x
    return wrapped
  return decorator

def parse_string(sym):
  def decorator(fn):
    def wrapped(s):
      q = s.read()
      info = {
        "line": s.line,
        "column": s.column,
        "start_quote": q,
      }
      result = []
      try:
        while s.peek() != q:
          fn(result, s, info)
        s.read()
      except StopIteration:
        raise w_SyntaxError("missing ending {} quote".format(q), s)
      x = w_Seq(sym, list_to_seq(result))
      x.line   = info["line"]
      x.column = info["column"]
      return x
    return wrapped
  return decorator

# Flag used when we reach the end of the input
stop_parsing = False

def read_inside_top(indent, seen, fn, origfn, s):
  global stop_parsing
  result         = []
  column         = s.column
  line           = s.line
  comment_seen   = False # Has a comment been seen?
  semicolon_seen = False # Has a semicolon been seen?
  try:
    #c = s.peek()
    #if c == ":":
    #  s.read()
    #  x = read_inside_top(chars, s.column, origfn, origfn, s)
    #  print x
    #if c == ";":
    #  s.read()
    #  while s.peek() in " ":
    #    s.read()
    #  if s.peek() == "\n":
    #    s.read()
    #    print "HIYA"
    #    #x = read_inside_top(chars, indent, origfn, origfn, s)
    #    return
    #  else:
    #    x = read_inside_top(chars, s.column, origfn, origfn, s)
    #    #print x.join(w_Seq(read_inside_top(chars, indent, origfn, origfn, s), w_nil))
    #    return x
    while s.line == line:
      c = s.peek()
      if c == ":":
        s.read()
        #seen = {"colon": True}
        #r = []
        #while not stop_parsing and s.indent > indent and fn():
        #  r.append(read_inside_top(s.indent, seen, origfn, origfn, s))
        #result.append(list_to_seq(r))
        ##return list_to_seq(result)
        #raise StopIteration
        result.append(read_inside_top(indent, {"colon": True}, origfn, origfn, s))
      elif c == ";":
        s.read()
        try:
          seen["colon"]
        except KeyError:
          raise w_SyntaxError("no matching :", s)
        raise StopIteration
          #return list_to_seq(result)
        #s.read()
        #semicolon_seen = True
        #raise StopIteration
      elif c == "|":
        s.read()
        while 1:
          while s.peek() in char_white:
            s.read()
          if s.peek() == ":":
            s.read()
            x = read_inside_top(s.column, seen, origfn, origfn, s)
          else:
            x = read1(s)
          if x is None:
            comment_seen = True
          else:
            result.append(x)
            result = list_to_seq(result)
            try:
              while s.peek() == " ":
                s.read()
              c = s.peek()
              if c in ";\n":
                s.read()
                raise StopIteration
              else:
                s.read()
                if c == "-" and s.peek() == ">":
                  s.read()
                  result = [w_arrow, w_Seq(w_apply, w_Seq(w_seq, result))]
                  try:
                    result.append(read_inside_top(indent, seen, lambda: False, origfn, s))
                  except StopIteration:
                    pass
                else:
                  raise w_SyntaxError("illegal use of |", s)
            except StopIteration:
              return w_Seq(w_apply, result)
            break
      elif c == "-":
        s.read()
        if s.peek() == ">":
          s.read()
          result = [w_arrow, w_Seq(w_seq, list_to_seq(result))]
          try:
            result.append(read_inside_top(indent, seen, lambda: False, origfn, s)) # + ";"
          except StopIteration:
            pass
        else:
          # TODO: not sure how this will interact with infix math -
          result.append(parse_symbol([c], line, column, s))
      elif c in char_white:
        s.read()
        if s.isatty and c == "\n" and s.peek() == "\n":
          stop_parsing = True
          raise StopIteration
      else:
        x = read1(s)
        if x is None:
          comment_seen = True
        else:
          result.append(x)
    while s.peek() in char_white:
      s.read()
    while not stop_parsing and s.indent > indent and fn():
      result.append(read_inside_top(s.indent, seen, origfn, origfn, s))
  except StopIteration:
    pass
  result = list_to_seq(result)
  if result == w_nil:
    if comment_seen:
      return read_inside_top(s.indent, seen, origfn, origfn, s)
    else:
      raise StopIteration
  elif result.rest == w_nil:
    return result.first
  else:
    return result

def read_top(s):
  #global stop_parsing
  #stop_parsing = False
  while s.peek() in char_white:
    s.read()
  #line   = s.line
  #indent = s.indent
  #fn = lambda: True
  #return read_inside_top(s.indent, {}, fn, fn, s)
  def f(c, line, column, info, s):
    return s.line != info.line# and s.indent <= indent

  def on_fail(info, s, result, colons, bar, priority, colon_seen):
    while s.peek() in char_white:
      s.read()
    if s.indent > info.indent:
      p = Parser(stop=f,
                 on_fail=on_fail,
                 arrow=True,
                 unwrap=True,
                 ignore_comments=True,
                 indent=s.indent,
                 line=s.line)
      #while s.indent > indent: #s.line == line or
      if colon_seen:
        colons[-1].append(parse_inside_parens(p, s))
      else:
        result.append(parse_inside_parens(p, s))

  p = Parser(stop=f,
             on_fail=on_fail,
             arrow=True,
             unwrap=True,
             ignore_comments=True,
             indent=s.indent,
             line=s.line)
  return parse_inside_parens(p, s)


@parse_recursive_until(")")
def parse_round_bracket(x, line, column):
  x.line   = line
  x.column = column
  return x

@parse_recursive_until("]")
def parse_square_bracket(x, line, column):
  x = w_Seq(w_seq, x)
  x.line   = line
  x.column = column
  return x

@parse_string(w_string)
def parse_single_string(result, s, info):
  c = s.read()
  if c == "@":
    while 1:
      x = read1(s)
      if x is not None:
        result.append(x)
        break
  elif c == "|":
    while 1:
      x = read1(s)
      if x is not None:
        result.append(x)
        break
  elif c == " " and s._at_start and s.indent < info["column"]:
    pass
  else:
    if c == "\\":
      c = s.peek()
      if c == info["start_quote"] or c in "\\@":
        c = s.read()
        c = w_Char(c)
      elif c == "n":
        s.read()
        c = w_Char("\n")
        c.tostring = "\\n"
      elif c == "t":
        s.read()
        c = w_Char("\t")
        c.tostring = "\\t"
      elif c == "u":
        s.read()
        h = []
        for i in range(4):
          x = s.read()
          h.append(x)
          if not x in "0123456789ABCDEF":
            raise w_SyntaxError("{} is not valid hexadecimal".format(x), s)
        h = "".join(h)
        c = w_Char(unichr(from_hex(h)))
        c.tostring = "\\u{}".format(h)
      else:
        s.read()
        raise w_SyntaxError("unknown escape sequence {}".format(c), s)
    else:
      c = w_Char(c)
    c.line   = s.line
    c.column = s.column
    result.append(c)

@parse_string(w_string)
def parse_raw_string(result, s, info):
  c = w_Char(s.read())
  c.line   = s.line
  c.column = s.column
  result.append(c)

##  #\|[\s\S]*?\|#
##  #.*
def parse_comment(s):
  if s.peek() == "|":
    s.read()
    try:
      while 1:
        c = s.read()
        d = s.peek()
        if c == "#" and d == "|":
          parse_comment(s)
        elif c == "|" and d == "#":
          break
      s.read()
    except StopIteration:
      raise w_SyntaxError("missing ending |# block", s)
  else:
    try:
      while s.read() != "\n":
        pass
    except StopIteration:
      pass
  #return read_top(s)

##  [a-zA-Z0-9\-$%!?]+
def parse_symbol(result, line, column, s):
  try:
    while s.peek() in char_sym:
      result.append(s.read())
  except StopIteration:
    pass
  x = w_Symbol("".join(result))
  x.line   = line
  x.column = column
  return x

##  \d+\.?\d+
def parse_num_or_symbol(s):
  result = []
  column = s.column
  line   = s.line
  dot    = False ## Has the dot been seen yet?
  try:
    while 1:
      c = s.peek()
      if c in char_num:
        result.append(s.read())
      elif c == "." and not dot:
        dot = True
        result.append(s.read())
      elif c in char_sym:
        return parse_symbol(result, line, column, s)
      else:
        break
  except StopIteration:
    pass
  return w_Number("".join(result))

def read1(s):
  c = s.peek()
  if c == "(":
    return parse_round_bracket(s)
  elif c == "[":
    return parse_square_bracket(s)
  elif c == "\"":
    return parse_single_string(s)
  elif c == "`":
    return parse_raw_string(s)
  elif c == "#":
    s.read()
    return parse_comment(s)
  elif c == "~":
    s.read()
    return w_tilde
  elif c in char_white:
    s.read()
    return read1(s)
  elif c == "-" or c not in char_sym:
    s.read()
    raise w_SyntaxError("invalid character {}".format(c), s)
  else:
    return parse_num_or_symbol(s)


class Token(object):
  bind = 0
  def __init__(self, s):
    self.line   = s.line
    self.column = s.column
  def __str__(self):
    return "Token: {}".format(self.__class__.__name__)

class LiteralToken(Token):
  def __init__(self, value):
    self.value = value
  def nud(self, s):
    return self.value

def nud_pre(pre, bind=None):
  def f(self, s):
    if bind is None:
      bind = self.bind
    return w_Seq(pre, w_Seq(parse(s, bind), w_nil))
  return f

def led_pre(pre, bind=None):
  def f(self, left, s):
    if bind is None:
      bind = self.bind
    return w_Seq(pre, w_Seq(left, w_Seq(parse(s, bind), w_nil)))
  return f


class ArrowToken(Token):
  def nud(self, s):
    print parse(s)
    pass
  def led(self, left, s):
    print left, parse(s)
    pass

class RoundToken(Token):
  def nud(self, s):
    expr = parse(s)
    c = s.next()
    print expr, c
    return expr

class EndRoundToken(Token):
  pass

class IndentToken(Token):
  def nud(self, s):
    result = []
    try:
      while not isinstance(s.current, DedentToken):
        expr = parse(s)
        print expr, s.current
        result.append(expr)
      s.next()
    except StopIteration:
      raise SyntaxError("missing ending dedent")
    #c = s.next()
    #print expr, c
    #if not isinstance(c, DedentToken):
    #  raise SyntaxError("missing ending dedent")
    #  #raise w_Thrown(w_SyntaxError("missing ending deindentation", s))
    #else:
    print result
    return list_to_seq(result)

class DedentToken(Token):
  pass
  #def nud(self, s):
  #  return w_nil
  #pass
  #bind = 9001
  #def led(self, left, s):
  #  return left

class WhitespaceToken(Token):
  pass
  #bind = 2
  #def led(self, left, s):
  #  x = parse(s, 1)
  #  print left, x
  #  return w_Seq(left, x)

class EOFToken(Token):
  pass

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


## Pratt's Top Down Operator Precedence Parser
def parse(s, bind=0):
  #if isinstance(t, Token):
  left = s.next().nud(s)
  #else:
  #  left = t
  p = s.current
  print "PARSE2", left, p, bind, p.bind
  while bind < p.bind:
    #current = t = s.next()
    #if isinstance(t, Token):
    left = s.next().led(left, s)
  return left
    #else:
    #  return left

##  #\|[\s\S]*?\|#
##  #.*
def parse_comment(s):
  c = s.next()
  if c == "|":
    try:
      while 1:
        c = s.next()
        if c == "#":
          c = s.next()
          if c == "|":
            parse_comment(s)
        elif c == "|":
          c = s.next()
          if c == "#":
            break
    except StopIteration:
      raise w_Thrown(w_SyntaxError("missing ending |# block", s))
  else:
    while c != "\n":
      c = s.next()
  return c

##  \d+\.?\d+
def parse_number(c, s):
  result = []
  line   = s.line
  column = s.column
  dot    = False ## Has the dot been seen yet?
  try:
    while 1:
      if c in char_num:
        result.append(c)
      elif c == "." and not dot:
        dot = True
        result.append(c)
      else:
        break
      c = s.next()
  except StopIteration:
    c = None
  return c, w_Number("".join(result), line=line, column=column)

##  [a-zA-Z0-9\-$%!?]+
def parse_symbol(c, s):
  result = []
  line   = s.line
  column = s.column
  try:
    while c in char_sym:
      result.append(c)
      c = s.next()
  except StopIteration:
    c = None
  return c, w_Symbol("".join(result), line=line, column=column)

def find_indent(s):
  indent = 0
  c = s.next()
  while c == " ":
    indent += 1
    c = s.next()
  return c, indent

def tokenize(s):
  indents = []

  c, new = find_indent(s)
  yield IndentToken(s)
  indents.append(new)

  try:
    while 1:
      if c == "#":
        c = parse_comment(s)
        continue
      elif c == "~":
        yield w_tilde
      elif c == " ":
        #yield WhitespaceToken(s)
        pass
      elif c == "\n":
        c, new = find_indent(s)
        if c == "\n" and s.isatty:
          return
        elif new > indents[-1]:
          yield IndentToken(s)
          indents.append(new)
        else:
          while indents.pop() <= new:
            yield DedentToken(s)
        # Uses the existing value of c
        continue
      elif c == "<":
        o = c
        c = s.next()
        if c == "=":
          yield LteToken(s)
        else:
          yield LtToken(s)
          # Uses the existing value of c
          continue
      elif c == ">":
        o = c
        c = s.next()
        if c == "=":
          yield GteToken(s)
        else:
          yield GtToken(s)
          # Uses the existing value of c
          continue
      elif c == "-":
        o = c
        c = s.next()
        if c == ">":
          yield ArrowToken(s)
        else:
          yield SubToken(s)
          # Uses the existing value of c
          continue
      elif c == "+":
        yield AddToken(s)
      elif c == "*":
        yield MulToken(s)
      elif c == "/":
        yield DivToken(s)
      elif c == "(":
        yield RoundToken(s)
      elif c == ")":
        yield EndRoundToken(s)
      elif c == "[":
        yield SquareToken(s)
      elif c == "]":
        yield EndSquareToken(s)
      elif c == "{":
        yield CurlyToken(s)
      elif c == "}":
        yield EndCurlyToken(s)
      elif c == "\"":
        yield DoubleQuoteToken(s)
      elif c == "`":
        yield BackQuoteToken(s)
      elif c == ":":
        yield ColonToken(s)
      elif c == ";":
        yield SemicolonToken(s)
      elif c in char_num:
        c, x = parse_number(c, s)
        yield LiteralToken(x)
        if c is None:
          raise StopIteration
        else:
          continue
      elif c in char_sym:
        c, x = parse_symbol(c, s)
        yield LiteralToken(x)
        if c is None:
          raise StopIteration
        else:
          continue
      else:
        raise w_Thrown(w_SyntaxError("invalid character {}".format(c), s))
      c = s.next()
  except StopIteration:
    while indents:
      indents.pop()
      yield DedentToken(s)
    #yield EOFToken(s)

def read_raw(s):
  yield parse(IterBuffer(tokenize(s)))

def read(s, eof=w_eof):
  try:
    return read_raw(IOBuffer(s))
  except StopIteration:
    return eof

def read_all(s):
  s = IOBuffer(s)
  try:
    for x in read_raw(s):
      yield x
  except w_Thrown as e:
    sys.stderr.write("{}\n".format(e))
    return
  except StopIteration:
    return

def read_file(name):
  with open(name, "r") as f:
    return read_all(f)
