from nu_types import *

#def from_hex1(x):
#  try:
#    return [ "0", "1", "2", "3",
#             "4", "5", "6", "7",
#             "8", "9", "A", "B",
#             "C", "D", "E", "F" ].index(x)
#  except ValueError:
#    raise SyntaxError("{} is not valid hexadecimal".format(x))

def from_hex(args):
  return int("".join(args), 16)
  #return sum(from_hex1(x) * (16 ** i) for i, x in enumerate(reversed(args)))

def to_hex(x):
  return "{:0=4X}".format(x)


class W_SyntaxError(SyntaxError):
  def __init__(self, message, s):
    SyntaxError.__init__(self, message,
      (s.filename, s.line, s.column, None if s.seen.isspace() else s.seen))
  def __str__(self):
    x = "{}: {!s} (line {}, column {})".format(type(self).__name__,
                                               self.msg,
                                               self.lineno,
                                               self.offset)
    if self.text:
      x += ":\n  {}\n {}{}".format(self.text, " " * self.offset, "^")
    return x

W_SyntaxError.__name__ = "SyntaxError"


char_white  = " \n"
char_num    = "0123456789"
char_lower  = "abcdefghijklmnopqrstuvwxyz"
char_upper  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
char_sym    = char_num + char_lower + char_upper + "-$%!?"# + "+/*" # TODO: unsure about using + / *

## Unique values for the : ; | syntaxes
#w_colon     = {}
#w_semicolon = {}
#w_bar       = {}

def parse_inside_parens(chars, fn, s):
  result = []
  # TODO: correct line/column
  line   = s.line
  column = s.column
  c      = s.peek()
  while not c in chars:
    if c == ":":
      s.read()
      result.append(parse_inside_parens(chars + ";", fn, s))
      #result.append(fn(parse_inside_parens(chars + ";", fn, s), line, column))
    elif c == ";":
      s.read()
      result.append(parse_inside_parens(chars, fn, s))
      #result.append(fn(parse_inside_parens(chars, fn, s), line, column))
    elif c == "|":
      s.read()
      #line   = s.line
      #column = s.column
      result.append(read1(s))
      result = list_to_cons(result)
      #x = read1(s)
      while s.peek() in char_white:
        s.read()
      c = s.peek()
      if c in chars:
        return W_Cons(w_apply, fn(result, line, column)) #.join(x)
      else:
        #c = s.read()
        #print c
        #print s.peek()
        s.read()
        if c == "-" and s.peek() == ">":
          s.read()
          result = [w_arrow, W_Cons(w_apply, W_Cons(w_list, result))]
          #list_to_cons(result).join(x)
          #y = parse_inside_parens(chars, fn, s)
          #if y.cdr == w_nil:
          #  result.append(y.car)
          #else:
          #  result.append(fn(y, line, column))
          #result = list_to_cons(result)
        else:
          raise W_SyntaxError("illegal use of |", s)
      ## TODO: proper line and column
      #return result #fn(result, line, column)
    elif c == "-":
      s.read()
      if s.peek() == ">":
        s.read()
        result = [w_arrow, W_Cons(w_list, list_to_cons(result))]
      else:
        # TODO: not sure how this will interact with infix math -
        result.append(parse_symbol([c], line, column, s))
    elif c in char_white:
      s.read()
    else:
      result.append(read1(s))
    c = s.peek()
  return fn(list_to_cons(result), line, column)

def parse_recursive_until(sym):
  def decorator(fn):
    def wrapped(s):
      line   = s.line
      column = s.column
      s.read()
      try:
        #return
        x = parse_inside_parens(sym, fn, s)
        s.read()
        return x
        #if x:
        #  return fn(x, line, column)
      except StopIteration:
        raise W_SyntaxError("missing ending {} brackets".format(sym), s)
      #return fn(list_to_cons(result), line, column)
    return wrapped
  return decorator

#def parse_column(sym, wrap):
#  def wrapped(c, s):
#    x = read_top(s.column, s.read(), s)
#    if isinstance(x, W_Cons):
#      x.car = W_Symbol(sym)
#      if wrap:
#        x.cdr = W_Cons(W_Cons(W_Symbol("&round-brackets"), x.cdr), w_nil)
#    else:
#      x = W_Cons(W_Symbol(sym), W_Cons(x, w_nil))
#    return x
#  return wrapped

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
        raise W_SyntaxError("missing ending {} quote".format(q), s)
      x = W_Cons(sym, list_to_cons(result))
      x.line   = info["line"]
      x.column = info["column"]
      return x
    return wrapped
  return decorator

#def parse_indent(sym, unwrap=True):
#  def decorator(fn):
#    def wrapped(s, indent=None):
#      if indent is None:
#        indent = s.indent
#      ## TODO: code duplication
#      if s._at_start:
#        while s.peek() == " ":
#          s.read()
#      column = s.column
#      line   = s.line
#      left   = None
#      body   = []
#      ## TODO: ugly hack
#      infix  = False ## Is infix?
#      try:
#        while s.line == line:
#          try:
#            c = s.peek()
#            x = read1(s)
#            #print x
#            body.append(x)
#          ## TODO: ugly hack
#          except Infixer as e:
#            infix = W_Symbol(e.name)
#            left  = list_to_cons(body)
#            body  = []
#            #column = s.column
#            #line   = s.line
#            #print s.column, indent, s.indent
#            #while fn(indent, column, s):
#            #  x = read_top(s.peek(), s)
#            #  print repr(x)
#            #  body.append(x)
#            #body = [list_to_cons(body)]
#          #print s.peek()
#          if s.peek() == "\n":
#            s.read()
#            break
#          elif infix:
#            if c == "(" or c == "[":
#              print s.peek()
#            else:
#              body = [read_top(s, indent=column + 1)]
#        ## TODO: code duplication
#        if s._at_start:
#          while s.peek() == " ":
#            s.read()
#        while fn(indent, column, s):
#          x = read_top(s)
#          #print repr(x)
#          body.append(x)
#        #while s.indent > indent:
#        #  body.append(read_top(s.indent, s.read(), s))
#      except StopIteration:
#        pass
#      l = len(body)
#      if l == 0:
#        raise StopIteration
#      elif l == 1 and unwrap and not infix:
#        return body[0]
#      else:
#        body = list_to_cons(body)
#        if infix:
#          #print repr(body.cdr)
#          #body = W_Cons(W_Cons(W_Symbol("&round-brackets"), body), w_nil)
#          return W_Cons(infix, W_Cons(left, body))
#        else:
#          return W_Cons(W_Symbol(sym), body)
#    return wrapped
#  return decorator

#parse_round_bracket  = parse_recursive_until("&round-brackets",  ")")
#parse_square_bracket = parse_recursive_until("&square-brackets", "]")
#parse_colon          = parse_recursive_until("&colon", False, ")", "]", "\\", "\n")
#parse_bar            = parse_recursive_until("&bar", False, ")", "]", "\\")
#parse_arrow          = parse_recursive_until("&arrow",     False, ")", "]", "\\", "\n")
#parse_big_arrow      = parse_recursive_until("&big-arrow", False, ")", "]", "\\", "\n")
#parse_backslash      = parse_recursive_until("&backslash", False, "\n")

#parse_colon          = parse_column("&colon", False)
#parse_arrow          = parse_column("&arrow", True)

#def parse_backslash(c, s):
#  #s.read()
#  x = read_top(s.indent, s.peek(), s)
#  x.car = W_Symbol("&backslash")
#  return x
#  #try:
#  #  x = read_top(s.indent, c, s)
#  #  print x
#  #  return x
#  #except StopIteration:
#  #  return 90001

def read_inside_top(unwrap, chars, indent, fn, s):
  result = []
  column = s.column
  line   = s.line
  try:
    while s.line == line:
      c = s.peek()
      if c in chars:
        break
      elif c == ":":
        s.read()
        result.append(read_inside_top(False, chars + ";", s.column, fn, s))
      elif c == ";":
        s.read()
        column = s.column
        result.append(read_inside_top(False, chars, indent, lambda: s.indent < column, s))
      elif c == "|":
        s.read()
        #x = read1(s)
        result.append(read1(s))
        result = list_to_cons(result)
        try:
          while s.peek() == " ":
            s.read()
          c = s.peek()
          if c in chars + "\n":
            raise StopIteration
          else:
            s.read()
            if c == "-" and s.peek() == ">":
              s.read()
              result = [w_arrow, W_Cons(w_apply, W_Cons(w_list, result))]
              # list_to_cons(result).join(x)
              try:
                result.append(read_inside_top(True, chars, indent, lambda: False, s))
              except StopIteration:
                pass
            else:
              raise W_SyntaxError("illegal use of |", s)
        except StopIteration:
          return W_Cons(w_apply, result) #list_to_cons(result).join(x)
      elif c == "-":
        s.read()
        if s.peek() == ">":
          s.read()
          result = [w_arrow, W_Cons(w_list, list_to_cons(result))]
          try:
            result.append(read_inside_top(True, chars, indent, lambda: False, s))
          except StopIteration:
            pass
        else:
          # TODO: not sure how this will interact with infix math -
          result.append(parse_symbol([c], line, column, s))
      elif c in char_white:
        s.read()
      else:
        result.append(read1(s))
    while s.peek() == " ":
      s.read()
    if s.peek() == "\n":
      s.read()
    while s.peek() == " ":
      s.read()
    while s.indent > indent and fn():
      result.append(read_inside_top(True, chars, s.indent, fn, s))
  except StopIteration:
    pass
  result = list_to_cons(result)
  if result == w_nil:
    raise StopIteration
  elif result.cdr == w_nil and unwrap:
    return result.car
  else:
    return result

def read_top(s):
  while s.peek() in char_white:
    s.read()
  return read_inside_top(True, "", s.indent, lambda: True, s)


@parse_recursive_until(")")
def parse_round_bracket(x, line, column):
  x.line   = line
  x.column = column
  return x

@parse_recursive_until("]")
def parse_square_bracket(result, line, column):
  x = W_Cons(w_list, result)
  x.line   = line
  x.column = column
  return x

#def parse_bar(s):
#  #s.read()
#  return W_Cons(W_Symbol("&bar"), W_Cons(read1(s), w_nil))

@parse_string(w_string)
def parse_single_string(result, s, info):
  c = s.read()
  if c == "@":
    #s.read()
    result.append(read1(s))
  elif c == " " and s._at_start and s.indent < info["column"]:
    pass
  else:
    if c == "\\":
      c = s.peek()
      if c == info["start_quote"] or c in "\\@":
        c = s.read()
        c = W_Char(c)
      elif c == "n":
        s.read()
        c = W_Char("\n")
        c.tostring = "\\n"
      elif c == "t":
        s.read()
        c = W_Char("\t")
        c.tostring = "\\t"
      elif c == "u":
        s.read()
        h = []
        for i in range(4):
          x = s.read()
          h.append(x)
          if not x in "0123456789ABCDEF":
            raise W_SyntaxError("{} is not valid hexadecimal".format(x), s)
        h = "".join(h)
        c = W_Char(unichr(from_hex(h)))
        c.tostring = "\\u{}".format(h)
      else:
        s.read()
        #result = "'{}\\{} ...'".format("".join(x.value for x in result), c)
        raise W_SyntaxError("unknown escape sequence {}".format(c), s) #result
    else:
      c = W_Char(c)
    c.line   = s.line
    c.column = s.column
    result.append(c)

@parse_string(w_string)
def parse_raw_string(result, s, info):
  c = W_Char(s.read())
  c.line   = s.line
  c.column = s.column
  result.append(c)

#@parse_indent("&round-brackets", unwrap=True)
#def read_top(indent, column, s):
#  return s.indent > indent

#@parse_indent("&backslash", unwrap=False)
#def parse_backslash(indent, column, s):
#  return s.indent < column and s.indent > indent

#@parse_indent("&colon", unwrap=False)
#def parse_colon(indent, column, s):
#  return s.indent > column

## TODO: ugly hack
#class Infixer(Exception):
#  def __init__(self, name):
#    self.name = name

#@parse_indent("&arrow", unwrap=False, multi=True)
#def parse_arrow(indent, column, s):
#def parse_arrow(s):
#  ## TODO: ugly hack
#  raise Infixer("&arrow")
#  #return s.indent > indent #s.indent > column

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
      raise W_SyntaxError("missing ending |# block", s)
  else:
    try:
      while s.read() != "\n":
        pass
    except StopIteration:
      pass
  return read1(s)

##  [a-zA-Z0-9\-$%!?]+
def parse_symbol(result, line, column, s):
  try:
    while s.peek() in char_sym:
      result.append(s.read())
  except StopIteration:
    pass
  x = W_Symbol("".join(result))
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
  return W_Number("".join(result))

def read1(s):
  c = s.peek()
  if c == "(":
    return parse_round_bracket(s)
  elif c == "[":
    return parse_square_bracket(s)
  #elif c == ":":
  #  s.read()
  #  return w_colon
  #elif c == ";":
  #  s.read()
  #  return w_semicolon
  #elif c == "|":
  #  s.read()
  #  return w_bar
  #elif c == "-" and s.peek() == ">":
  #  #s.read()
  #  return parse_arrow(s.read(), s)
  #elif c == "=" and s.peek() == ">":
  #  #s.read()
  #  return parse_big_arrow(s.read(), s)
  elif c == "'":
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
  elif c in char_sym:
    return parse_num_or_symbol(s)
  else:
    s.read()
    raise W_SyntaxError("invalid character {}".format(c), s)

def read(s, eof):
  try:
    #return read1(s)
    return read_top(s)
  except StopIteration:
    return eof

def readstring(s, eof):
  return read(W_Stream(iter(s).next), eof)
