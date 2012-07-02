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

# TODO: lots of overlap with read_inside_top
def parse_inside_parens(chars, fn, s):
  result     = []
  # TODO: correct line/column
  line       = s.line
  column     = s.column
  c          = s.peek()
  colon_seen = False # Has the colon been seen?
  while not c in chars:
    if c == ":":
      colon_seen = True
      s.read()
      result.append(parse_inside_parens(chars, fn, s)) # + ";"
    elif c == ";":
      if not colon_seen:
        break
      s.read()
      result.append(parse_inside_parens(chars, fn, s)) # + ";"
    elif c == "|":
      s.read()
      while 1:
        while s.peek() in char_white:
          s.read()
        if s.peek() == ":":
          s.read()
          x = parse_inside_parens(chars, fn, s)
        else:
          x = read1(s)
        if x is not None:
          result.append(x)
          result = list_to_cons(result)
          while s.peek() in char_white:
            s.read()
          c = s.peek()
          if c in chars + ";":
            return w_Cons(w_apply, fn(result, line, column)) #.join(x)
          else:
            s.read()
            if c == "-" and s.peek() == ">":
              s.read()
              result = [w_arrow, w_Cons(w_apply, w_Cons(w_list, result))]
            else:
              raise w_SyntaxError("illegal use of |", s)
          break
    elif c == "-":
      s.read()
      if s.peek() == ">":
        s.read()
        result = [w_arrow, w_Cons(w_list, list_to_cons(result))]
      else:
        # TODO: not sure how this will interact with infix math -
        result.append(parse_symbol([c], line, column, s))
    elif c in char_white:
      s.read()
    else:
      x = read1(s)
      if x is not None:
        result.append(x)
    c = s.peek()
  return fn(list_to_cons(result), line, column)

def parse_recursive_until(sym):
  def decorator(fn):
    def wrapped(s):
      line   = s.line
      column = s.column
      s.read()
      try:
        x = parse_inside_parens(sym, fn, s)
        s.read()
        return x
      except StopIteration:
        raise w_SyntaxError("missing ending {} brackets".format(sym), s)
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
      x = w_Cons(sym, list_to_cons(result))
      x.line   = info["line"]
      x.column = info["column"]
      return x
    return wrapped
  return decorator

# Flag used when we reach the end of the input
stop_parsing = False

def read_inside_top(unwrap, chars, indent, fn, origfn, s):
  global stop_parsing
  result       = []
  column       = s.column
  line         = s.line
  comment_seen = False # Has a comment been seen?
  colon_seen   = False # Has the colon been seen?
  try:
    while s.line == line:
      c = s.peek()
      if c in chars:
        break
      elif c == ":":
        colon_seen = True
        s.read()
        result.append(read_inside_top(False, chars + ";", s.column, origfn, origfn, s))
      elif c == ";":
        s.read()
        column = s.column
        result.append(read_inside_top(True, chars + ";", indent, lambda: s.indent < column, origfn, s))
        #if not colon_seen:
        #  break
      elif c == "|":
        s.read()
        while 1:
          while s.peek() in char_white:
            s.read()
          if s.peek() == ":":
            s.read()
            x = read_inside_top(False, chars, s.column, origfn, origfn, s) # + ";"
          else:
            x = read1(s)
          if x is None:
            comment_seen = True
          else:
            result.append(x)
            result = list_to_cons(result)
            try:
              while s.peek() == " ":
                s.read()
              c = s.peek()
              if c in chars + ";\n": # TODO: not sure about using ; in here
                raise StopIteration
              else:
                s.read()
                if c == "-" and s.peek() == ">":
                  s.read()
                  result = [w_arrow, w_Cons(w_apply, w_Cons(w_list, result))]
                  try:
                    result.append(read_inside_top(True, chars, indent, lambda: False, origfn, s)) # + ";"
                  except StopIteration:
                    pass
                else:
                  raise w_SyntaxError("illegal use of |", s)
            except StopIteration:
              return w_Cons(w_apply, result)
            break
      elif c == "-":
        s.read()
        if s.peek() == ">":
          s.read()
          result = [w_arrow, w_Cons(w_list, list_to_cons(result))]
          try:
            result.append(read_inside_top(True, chars, indent, lambda: False, origfn, s)) # + ";"
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
    #while s.peek() == " ":
    #  s.read()
    #if s.peek() == "\n":
    #  s.read()
    #while s.peek() == " ":
    #  s.read()
    while s.peek() in char_white:
      s.read()
    while not stop_parsing and s.indent > indent and fn():
      result.append(read_inside_top(True, chars, s.indent, origfn, origfn, s))
  except StopIteration:
    pass
  result = list_to_cons(result)
  if result == w_nil:
    if comment_seen:
      return read_inside_top(True, chars, s.indent, origfn, origfn, s)
    else:
      raise StopIteration
  elif result.cdr == w_nil and unwrap:
    return result.car
  else:
    return result

def read_top(s):
  global stop_parsing
  stop_parsing = False
  while s.peek() in char_white:
    s.read()
  fn = lambda: True
  return read_inside_top(True, "", s.indent, fn, fn, s)


@parse_recursive_until(")")
def parse_round_bracket(x, line, column):
  x.line   = line
  x.column = column
  return x

@parse_recursive_until("]")
def parse_square_bracket(result, line, column):
  x = w_Cons(w_list, result)
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
    raise w_SyntaxError("invalid character {}".format(c), s)

def read(s, eof):
  try:
    return read_top(s)
  except StopIteration:
    return eof

def readstring(s, eof):
  return read(w_Stream(iter(s).next), eof)
