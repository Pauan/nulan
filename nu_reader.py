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

def parse_inside_parens(chars, fn, s):
  result = []
  # TODO: correct line/column
  line   = s.line
  column = s.column
  c      = s.peek()
  while not c in chars:
    if c == "|":
      s.read()
      while 1:
        x = read1(s)
        if x is not None:
          result.append(x)
          result = list_to_seq(result)
          while s.peek() in char_white:
            s.read()
          c = s.peek()
          if c in chars:
            return w_Seq(w_apply, fn(result, line, column))
          else:
            raise w_SyntaxError("illegal use of |", s)
    elif c in char_white:
      s.read()
    else:
      x = read1(s)
      if x is not None:
        result.append(x)
    c = s.peek()
  return fn(list_to_seq(result), line, column)

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
      x = w_Seq(sym, list_to_seq(result))
      x.line   = info["line"]
      x.column = info["column"]
      return x
    return wrapped
  return decorator

# Flag used when we reach the end of the input
stop_parsing = False

def read_inside_top(chars, indent, fn, origfn, s):
  global stop_parsing
  result         = []
  column         = s.column
  line           = s.line
  comment_seen   = False # Has a comment been seen?
  colon_seen     = False # Has a colon been seen?
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
      if c in chars:
        break
      elif c == ":":
        colon_seen = True
        s.read()
        while s.peek() in " ":
          s.read()
        if s.peek() == "\n":
          s.read()
          x = read_inside_top(chars + ";", indent, origfn, origfn, s)
          result.append(x)
        else:
          # I have noooo clue how the hell this works, but it does
          while s.line == line:
            x = read_inside_top(chars, s.column, origfn, origfn, s)
            #print x, semicolon_seen, result
            result.append(x)
            #x = read_inside_top(chars, 9001, origfn, origfn, s)
            #result.append(x)
      # I have noooo clue how the hell this works, but it does
      elif c == ";":
        s.read()
        if colon_seen:
          pass
        else:
          raise w_SyntaxError("no matching :", s)
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
            x = read_inside_top(chars, s.column, origfn, origfn, s)
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
              if c == "\n":
                raise StopIteration
              else:
                s.read()
                if c == "-" and s.peek() == ">":
                  s.read()
                  result = [w_arrow, w_Seq(w_apply, w_Seq(w_seq, result))]
                  try:
                    result.append(read_inside_top(chars, indent, lambda: False, origfn, s))
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
            result.append(read_inside_top(chars, indent, lambda: False, origfn, s)) # + ";"
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
      result.append(read_inside_top(chars, s.indent, origfn, origfn, s))
  except StopIteration:
    pass
  result = list_to_seq(result)
  if result == w_nil:
    if comment_seen:
      return read_inside_top(chars, s.indent, origfn, origfn, s)
    else:
      raise StopIteration
  elif result.rest == w_nil:
    return result.first
  else:
    return result

def read_top(s):
  global stop_parsing
  stop_parsing = False
  while s.peek() in char_white:
    s.read()
  fn = lambda: True
  return read_inside_top("", s.indent, fn, fn, s)


@parse_recursive_until(")")
def parse_round_bracket(x, line, column):
  x.line   = line
  x.column = column
  return x

@parse_recursive_until("]")
def parse_square_bracket(result, line, column):
  x = w_Seq(w_seq, result)
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

def parse_greater_than(result, indent, s):
  while 1:
    while s.peek() != "\n":
      result.append(s.read())
    result.append(s.read())
    while s.peek() in char_white:
      result.append(s.read())
    if not s.indent > indent:
      break

#[":"            | R] ->
#[";"            | R] ->
#["->"           | R] ->
#[(is? " " "\n") | R] ->
#[X              | R] ->


def read_indent1(result, indent, spaces, s):
  try:
    while 1:
      c = s.peek()
      if c == ":":
        s.read()
        column = s.column
        while s.peek() in " ":
          result.append(s.read())
        if s.peek() == "\n":
          print "FOO", spaces
        else:
          #spaces += 1
          result.append("\n")
          result.append(" " * (spaces + 1))
          read_indent1(result, column, spaces + 1, s)
          read_indent1(result, indent, spaces, s)
          #parse_greater_than(result, column, s)
          #while s.peek() != "\n":
          #  result.append(s.read())
          #while s.indent > column:
          #  result.append(s.read())
          #print "".join(result)
      elif c == ";":
        s.read()
        column = s.column
        while s.peek() in " ":
          result.append(s.read())
        if s.peek() == "\n":
          print "FOO", spaces
        else:
          result.append("\n")
          result.append(" " * spaces)
          read_indent1(result, column, spaces, s)
      elif c == "-":
        result.append(s.read())
        #if s.peek() == ">":
        #  result.append(s.read())
        #  result.append("\n")
        #  result.append(" " * s.indent + 1)
        #  #while s.peek() != "\n":
        #  #  result.append(s.read())
        #  result.append("\n")
        #  result.append(" " * s.indent + 1)

        #result.append(s.read())
        #while s.peek() in char_white:
        #  s.read()
      elif c == "\n":
        result.append(s.read())
        while s.peek() in " ":
          s.read()
        print s.indent, indent
        if s.indent > indent:
          result.append(" " * (spaces + 1))
          #result.append(s.read())
          read_indent1(result, s.indent, s.indent, s)
          #if indent_add:
          #  indent += indent_add
        else:
          #result.append(s.read())
          break
          #indent_add = None
        #temp = indent = s.indent
        #
      else:
        result.append(s.read())
  except StopIteration:
    pass
  return result

def read_indent(s):
  while s.peek() in char_white:
    s.read()
  result = []
  print "".join(read_indent1(result, s.indent, s.indent, s))

def read(s, eof):
  try:
    return read_top(s)
  except StopIteration:
    return eof

def readstring(s, eof):
  return read(w_Stream(iter(s).next), eof)
