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
  for x in args:
    if not x in "0123456789ABCDEF":
      raise SyntaxError("{} is not valid hexadecimal".format(x))
  return int("".join(args), 16)
  #return sum(from_hex1(x) * (16 ** i) for i, x in enumerate(reversed(args)))

def to_hex(x):
  return "{:0=4X}".format(x)


char_white  = " \n\t"
char_num    = "0123456789"
char_lower  = "abcdefghijklmnopqrstuvwxyz"
char_upper  = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
char_sym    = char_num + char_lower + char_upper + "=->$%!?" + "+/*" # TODO: unsure about using + / *
char_number = char_sym + "."

w_colon     = {}
w_semicolon = {}
w_bar       = {}

def parse_inside_parens(chars, fn, s):
  result = []
  # TODO: correct line/column
  line   = s.line
  column = s.column
  c      = s.peek()
  while not c in chars:
    if c == ":":
      s.read()
      result.append(fn(parse_inside_parens(chars + ";", fn, s), line, column))
    elif c == ";":
      s.read()
      result.append(fn(parse_inside_parens(chars, fn, s), line, column))
    elif c == "|":
      s.read()
      #line   = s.line
      #column = s.column
      x = read1(s)
      if s.peek() in chars:
        return list_to_cons(result).join(x)
      else:
        if read1(s) == W_Symbol("->"):
          result = [w_arrow, fn(list_to_cons(result).join(x), line, column)]
          #y = parse_inside_parens(chars, fn, s)
          #if y.cdr == w_nil:
          #  result.append(y.car)
          #else:
          #  result.append(fn(y, line, column))
          #result = list_to_cons(result)
        else:
          raise SyntaxError("illegal use of |")
      ## TODO: proper line and column
      #return result #fn(result, line, column)
    elif c in char_white:
      s.read()
    else:
      x = read1(s)
      if x == W_Symbol("->"):
        result = [w_arrow, fn(list_to_cons(result), line, column)]
        #print s.peek()
        #x = parse_inside_parens(chars, fn, s)
        #if x.cdr == w_nil:
        #  result.append(x.car)
        #else:
        #  result.append(fn(x, line, column))
      else:
        result.append(x)
    c = s.peek()
  return list_to_cons(result)

def parse_recursive_until(sym):
  def decorator(fn):
    def wrapped(s):
      line   = s.line
      column = s.column
      s.read()
      try:
        x = parse_inside_parens(sym, fn, s)
        s.read()
        if x:
          return fn(x, line, column)
      except StopIteration:
        raise SyntaxError("missing ending {} brackets".format(sym))
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
        raise SyntaxError("missing ending {} quote".format(q))
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

def read_top(s):
  return read1(s)

@parse_recursive_until(")")
def parse_round_bracket(x, line, column):
  x.line   = line
  x.column = column
  return x

@parse_recursive_until("]")
def parse_square_bracket(result, line, column):
  x = W_Cons(w_square_brackets, result)
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
        h = s.read() + s.read() + s.read() + s.read()
        c = W_Char(unichr(from_hex(h)))
        c.tostring = "\\u{}".format(h)
      else:
        raise SyntaxError("unknown escape sequence {}".format(c))
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

def parse_comment(s):
  if s.read() == "|":
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
      raise SyntaxError("missing ending |# block")
  else:
    try:
      while s.read() != "\n":
        pass
    except StopIteration:
      pass
  return read1(s)

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

## \d+\.?\d+
def parse_num_or_symbol(s):
  result = []
  column = s.column
  line   = s.line
  dot    = False ## Has the dot been seen yet?
  first  = True  ## First iteration?
  try:
    while 1:
      c = s.peek()
      if c in char_num:
        first = False
        result.append(s.read())
      elif not dot and not first and c == ".":
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
  elif c == ":":
    s.read()
    return w_colon
  elif c == ";":
    s.read()
    return w_semicolon
  elif c == "|":
    s.read()
    return w_bar
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
  elif c in char_number:
    return parse_num_or_symbol(s)
  else:
    raise SyntaxError("invalid character {}".format(c))

def read(s, eof):
  s = W_Stream(s)
  try:
    #return read1(s)
    return read_top(s)
  except StopIteration:
    return eof
