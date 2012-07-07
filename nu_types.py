##############################################################################
#  Helpers
##############################################################################
def eval_(env, x):
  # TODO
  if isinstance(x, (w_Symbol, w_Uniq)):
    return env[x]
  elif isinstance(x, w_Seq):
    return eval_(env, x.first)(env, x.rest)
  else:
    return x

def list_to_seq(x):
  result = w_nil
  for x in reversed(x):
    result = w_Seq(x, result)
  return result

def seq(*args, **kwargs):
  try:
    top = r = w_Seq(args[0], w_nil)
    for x in args[1:]:
      r.rest = w_Seq(w_Symbol(x), w_nil)
      r = r.rest
    try:
      r.rest = w_Symbol(kwargs["rest"])
    except KeyError:
      pass
  except IndexError:
    try:
      return w_Symbol(kwargs["rest"])
    except KeyError:
      return w_nil
  return top

def join_string(args):
  x = args
  result = []
  while isinstance(x, w_Seq):
    if isinstance(x.first, w_Seq):
      result += join_string(x.first)
    elif isinstance(x.first, w_Char):
      result.append(x.first)
    else:
      raise TypeError("cannot coerce {} in {} to a string".format(x.first, args))
    x = x.rest
  return list_to_seq(result)

def pattern_match(base, pattern, args):
  def rec(p, a):
    if isinstance(p, w_Seq):
      if p.first == w_apply:
        p = p.rest
        if p.first == w_seq:
          p = p.rest
          while isinstance(p.rest, w_Seq):
            if not isinstance(a, w_Seq):
              raise w_PatternFail(p, a)
            r = rec(p.first, a.first)
            #if r == w_false:
            #  return r
            p = p.rest
            a = a.rest
          return rec(p.first, a)
        else:
          raise Exception(p)
      elif p.first == w_seq:
        p = p.rest
      while isinstance(p, w_Seq):
        if not isinstance(a, w_Seq):
          raise w_PatternFail(p, a)
        r = rec(p.first, a.first)
        #if r == w_false:
        #  return r
        p = p.rest
        a = a.rest
      return rec(p, a)
    # TODO
    elif isinstance(p, (w_Symbol, w_Uniq)):
      base(p, a)
    elif p == w_tilde:
      return
    else:
      if p != a:
        raise w_PatternFail(p, a)
        #return w_false
  return rec(pattern, args)


def nu_vau(name, *args, **kwargs):
  def decorator(fn):
    fn.__name__ = name
    return w_Builtin(fn, *args, **kwargs)
  return decorator

def nu_lambda(name, *args, **kwargs):
  def decorator(fn):
    fn.__name__ = name
    return w_Wrapped(w_Builtin(fn, *args, **kwargs))
  return decorator

##############################################################################
#  Exceptions
##############################################################################
class w_BaseError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return "error: {}".format(self.message)

class w_SyntaxError(w_BaseError, SyntaxError):
  def __init__(self, message, s, line=None, column=None, text=None, filename=None):
    self.msg      = message
    self.lineno   = line     or s.line
    self.offset   = column   or s.column
    self.text     = text     or (None if s.seen.isspace() else s.seen)
    self.filename = filename or s.filename
  def __str__(self):
    x = "error: {!s} (line {}, column {})".format(self.msg,
                                                  self.lineno,
                                                  self.offset)
    if self.text:
      x += ":\n  {}\n {}{}".format(self.text, " " * self.offset, "^")
    return x

class w_VariableError(w_BaseError):
  def __init__(self, name):
    self.name = name
  def __str__(self):
    return "error: {} is undefined".format(self.name)

class w_TypeError(w_BaseError):
  def __init__(self, expected, got):
    self.expected = expected
    self.got      = got
  def __str__(self):
    return "error: expected something of type {} but instead got {}".format(self.expected, self.got)

class w_PatternFail(w_BaseError):
  def __init__(self, pat, args):
    self.pattern   = pat
    self.arguments = args
  def __str__(self):
    return "error: pattern {} failed to match {}".format(self.pattern, self.arguments)

##############################################################################
#  Types
##############################################################################
class w_Stream(object):
  def __init__(self, next, name=""):
    self._next      = next #iter(iterable)
    self._empty     = False
    self._at_start  = True
    self.filename   = name
    self.old_indent = 0
    self.indent     = 0
    self.line       = 1
    self.column     = 0
    self.seen       = ""
    self.isatty     = False
    try:
      self._peeked = self._next()
    except StopIteration:
      self._empty = True

  def peek(self):
    if self._empty:
      raise StopIteration
    else:
      return self._peeked

  def read(self):
    if self._empty:
      raise StopIteration
    else:
      old = self._peeked
      if old == "\n":
        self._at_start  = True
        self.old_indent = self.indent
        self.indent     = 0
        self.line      += 1
        self.column     = 0
        self.seen       = ""
      else:
        if old in " \t":
          if self._at_start:
            self.indent += 1
        else:
          self._at_start = False
        self.column += 1
        self.seen += old
      try:
        self._peeked = self._next()
      except StopIteration:
        self._empty = True
      return old

  def empty(self):
    return self._empty

  next = read
  def __iter__(self):
    return self

class w_InputStream(w_Stream):
  def __init__(self, f):
    def wrapped():
      x = f.read(1)
      if x == "":
        raise StopIteration
      else:
        return x
    w_Stream.__init__(self, wrapped, name=f.name)
    self.isatty = f.isatty()

## Generic stuff for chars, symbols, numbers, etc.
class w_Base(object):
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return str(self.value)
  def __repr__(self):
    return repr(self.value)
  def __eq__(self, x):
    return self.value == x
  def __ne__(self, x):
    return not self == x
  def __hash__(self):
    return hash(self.value)
  def __radd__(self, x):
    return self.__add__(x)
  def pretty(self):
    return str(self)

class w_Number(w_Base):
  def __init__(self, value):
    self.value = float(value)
  def __str__(self):
    return "{:g}".format(self.value)
  def __repr__(self):
    return "(&number {})".format(str(self))
  def __add__(self, x):
    return self.value + x

class w_Char(w_Base):
  def __str__(self):
    try:
      return self.tostring
    except AttributeError:
      ## Prints " \ and @ with a preceding \
      if self.value in "\"\\@":
        return "\\{}".format(self.value)
      else:
        return self.value
  def __repr__(self):
    if self.value == "\n":
      x = "\\n"
    elif self.value == "\t":
      x = "\\t"
    else:
      x = str(self)
    return "(&char {})".format(x)
  #def __eq__(self, x):
  #         # self.value == x
  #  return (w_Base.__eq__(self, x) or
  #          (isinstance(x, w_Seq) and
  #           x.rest == w_nil and
  #           x.first == self))

class w_Symbol(w_Base):
  def __repr__(self):
    return "(&symbol {})".format(self.value)

class w_Uniq(object):
  counter = 1
  def __init__(self, name):
    self.counter = w_Uniq.counter
    self.name    = name
    w_Uniq.counter += 1
  def __repr__(self):
    if self.name:
      return self.name
    else:
      return "(&uniq {})".format(self.counter)
  def pretty(self):
    return str(self)

class w_Nil(object):
  def __repr__(self):
    return "[]"
  def __len__(self):
    return 0
  def __iter__(self):
    return self
  def next(self):
    raise StopIteration
  def pretty(self):
    return str(self)
  def join(self, y):
    return y
  def mappair(self, f):
    return self

w_nil = w_Nil()

class w_Seq(w_Nil):
  def __init__(self, first, rest):
    self.first = first
    self.rest  = rest

  def __iter__(self):
    item = self
    while 1:
      if isinstance(item, w_Seq):
        yield item.first
        item = item.rest
      elif item == w_nil:
        return
      else:
        raise TypeError("can't iterate on a dotted seq")

  def print_with(self, f):
    x        = self
    x.line   = None
    x.column = None
    char_all = True ## Are all the elements chars?
    result   = []
    while 1:
      ## It's a proper seq: print it as a seq or a string
      if x == w_nil:
        ## It's a seq of characters: print it as a string
        if char_all:
          ## This uses str because chars have different printing in strings
          return "\"{}\"".format("".join(str(x) for x in result))
        ## Print it as a seq
        else:
          return "[{}]".format(" ".join(f(x) for x in result))
      if isinstance(x, w_Seq):
        if char_all and not isinstance(x.first, w_Char):
          char_all = False
        result.append(x.first)
        x = x.rest
      ## It's an improper seq: print it as a seq with a bar
      else:
        return "[{} | {}]".format(" ".join(f(x) for x in result), f(x))

  def pretty(self):
    x        = self
    char_all = True ## Are all the elements chars?
    result   = []
    braces   = "()"

    if x.first == w_seq:
      braces = "[]"
      x = x.rest
      if x == w_nil:
        return "[]"
    elif x.first == w_string:
      braces = "[]"
      x = x.rest
    elif x.first == w_arrow:
      #x = x.rest
      #x = x.first.rest.join(w_Seq(w_Symbol("->"), x.rest))
      x.first = w_Symbol("$fn")
    elif x.first == w_apply:
      x = x.rest
      if x.first == w_seq:
        braces = "[]"
        x = x.rest
        if x.rest == w_nil:
          return x.first.pretty()

    while 1:
      ## It's a proper seq: print it as a seq or a string
      if x == w_nil:
        ## It's a seq of characters: print it as a string
        if char_all:
          ## This uses str because chars have different printing in strings
          return "\"{}\"".format("".join(x.pretty() for x in result))
        ## Print it as a seq
        else:
          return "{}{}{}".format(braces[0], " ".join(x.pretty() for x in result), braces[1])
      if isinstance(x, w_Seq):
        if x.rest == w_nil and self.first == w_apply:
          return "{}{} | {}{}".format(braces[0],
                                      " ".join(x.pretty() for x in result),
                                      x.first.pretty(),
                                      braces[1])
        else:
          if char_all and not isinstance(x.first, w_Char):
            char_all = False
          result.append(x.first)
          x = x.rest
      ## It's an improper seq: print it as a seq with a bar
      else:
        return "{}{} | {}{}".format(braces[0], " ".join(x.pretty() for x in result), x.pretty(), braces[1])

  def __repr__(self):
    return self.print_with(repr)

  def __str__(self):
    return self.print_with(str)

#  ## This is just for pretty printing
#  def __str__(self):
#    x        = self
#    x.line   = None
#    x.column = None
#    char_all = True ## Are all the elements chars?
#    result   = []
#    braces   = "[]"

#    if (x.first == w_seq or
#        x.first == w_string):
#      #braces = "[]"
#      x = x.rest
#    elif x.first == w_arrow:
#      x = x.rest
#      x = x.first.rest.join(w_Seq(w_Symbol("->"), x.rest))
#    elif x.first == w_apply:
#      x = x.rest
#      if x.first == w_seq:
#        #braces = "[]"
#        x = x.rest

#    while 1:
#      ## It's a proper seq: print it as a seq or a string
#      if x == w_nil:
#        ## It's a seq of characters: print it as a string
#        if char_all:
#          ## This uses str because chars have different printing in strings
#          return "'{}'".format("".join(str(x) for x in result))
#        ## Print it as a seq
#        else:
#          return "{0[0]}{1}{0[1]}".format(braces, " ".join(str(x) for x in result))
#      if isinstance(x, w_Seq):
#        if x.rest == w_nil and self.first == w_apply:
#          return "{0[0]}{1} | {2}{0[1]}".format(braces,
#                                                " ".join(str(x) for x in result),
#                                                str(x.first))
#        else:
#          if char_all and not isinstance(x.first, w_Char):
#            char_all = False
#          result.append(x.first)
#          x = x.rest
#      ## It's an improper seq: print it as a seq with a bar
#      else:
#        return "{0[0]}{1} | {2}{0[1]}".format(braces,
#                                              " ".join(str(x) for x in result),
#                                              str(x))

  # TODO: code duplication with join
  def mappair(self, f):
    x = self
    top = r = w_Seq(f(x.first), w_nil)
    x = x.rest
    while isinstance(x, w_Seq):
      r.rest = w_Seq(f(x.first), w_nil)
      r = r.rest
      x = x.rest
    if x != w_nil:
      # TODO: not sure if needed
      r.rest = f(x)
    return top

  def join(self, y):
    x = self
    top = r = w_Seq(x.first, w_nil)
    x = x.rest
    while isinstance(x, w_Seq):
      r.rest = w_Seq(x.first, w_nil)
      r = r.rest
      x = x.rest
    if x != w_nil:
      raise TypeError("cannot call join on improper seq {}".format(repr(self)))
    r.rest = y
    return top

  def tostring(self):
    x = self
    result = []
    while 1:
      if x == w_nil:
        break
      elif isinstance(x, w_Seq) and isinstance(x.first, w_Char):
        result.append(x.first.value)
        x = x.rest
      else:
        raise TypeError("cannot convert {} to a string".format(repr(self)))
    return "".join(result)

class w_Env(object):
  def __init__(self, parent):
    self.variables = {}
    self.parent = parent
  def __getitem__(self, name):
    try:
      return self.variables[name]
    except KeyError:
      return self.parent[name]
  def __setitem__(self, name, value):
    self.variables[name] = value

class w_TopEnv(w_Env):
  def __init__(self, variables):
    self.variables = variables
  def __getitem__(self, name):
    try:
      return self.variables[name]
    except KeyError:
      raise w_VariableError(name)

class w_Vau(object):
  def __init__(self, closure, env, args, body):
    self.closure = closure
    self.args    = args
    self.body    = body
    self.env     = env

  def __repr__(self):
    try:
      return "(&vau {})".format(self.__name__)
    except AttributeError:
      return "(&vau)"

  def pretty(self):
    return str(self)

  def __call__(self, env, args):
    #print env.variables
    #print
    # TODO: maybe figure out a way to not need to create and
    #       destroy a new environment every time the vau is called
    inner = w_Env(self.closure)

    if self.env != w_tilde:
      inner[self.env] = env

    seen = {}
    def base(p, a):
      if p in seen:
        if inner[p] != a:
          raise w_PatternFail(p, a)
      else:
        seen[p] = True
        inner[p] = a

    pattern_match(base, self.args, args)
    #if m == w_false:
    #  return m

    x = self.body
    last = w_false
    while x != w_nil:
      last = eval_(inner, x.first)
      x = x.rest
    return last

class w_Builtin(w_Vau):
  def __init__(self, f, *args, **kwargs):
    self.args    = seq(*args, **kwargs)
    self.wrapped = f
    self.__name__ = f.__name__
  def __call__(self, env, args):
    def base(p, a):
      pass
    pattern_match(base, self.args, args)
    return self.wrapped(env, args)

class w_Wrapped(object):
  def __init__(self, f):
    self.wrapped = f
  def __repr__(self):
    try:
      return "(&fn {})".format(self.wrapped.__name__)
    except AttributeError:
      return "(&fn)"
  def pretty(self):
    return str(self)
  def __call__(self, env, args):
    return self.wrapped(env, args.mappair(lambda x: eval_(env, x)))

##############################################################################
#  Syntax
##############################################################################
@nu_vau("$fn", "Args", rest="Body")
def w_arrow(env, args, body):
  return w_Wrapped(w_Vau(env, w_tilde, args, body))

#@nu_vau("&apply")
#def w_apply(env, args):
#  x = args
#  if x.rest == w_nil:
#    print x.first
#    return eval_(env, x.first)
#  else:
#    while x.rest.rest != w_nil:
#      x = x.rest
#    x.rest = x.rest.first #eval_(env, )
#    return eval_(env, args)

@nu_lambda("apply", "F", rest="Args")
def w_apply(env, f, rest):
  if rest == w_nil:
    return eval_(env, f)
  else:
    x = rest
    while x.rest.rest != w_nil:
      x = x.rest
    x.rest = x.rest.first
    # TODO: code duplication with unwrap
    #if isinstance(args.first, w_Wrapped):
    #  args.first = args.first.wrapped
    #else:
    #  args.first = w_false
    f = w_unwrap(env, w_Seq(f, w_nil))
    #print args
    return eval_(env, w_Seq(f, args))

@nu_lambda("seq", rest="Args")
def w_seq(env, args):
  return args

@nu_lambda("str", rest="Args")
def w_string(env, args):
  return join_string(args)

@nu_lambda("&tilde")
def w_tilde(env):
  return w_true

##############################################################################
#  Primitives
##############################################################################
#(x, y)           = destructure(args, "X", "Y")
#(e, args, *body) = destructure(args, "Env", "Args", rest="Body")

# Constants
w_true  = w_Symbol("%t")
w_false = w_Symbol("%f")
w_eof   = w_Uniq("%eof")

# Non-referentially transparent
@nu_vau("$assign!", "Name", "X")
def w_assignd(env, x, y):
  y = eval_(env, y)
  env[x] = y
  return y

@nu_lambda("pr!", rest="Args")
def w_pr(env, args):
  x = args
  while isinstance(x, w_Seq):
    try:
      print x.first.tostring(),
    except (TypeError, AttributeError):
      print x.first,
    x = x.rest
  return args.first

@nu_lambda("write!", rest="Args")
def w_write(env, args):
  x = args
  while isinstance(x, w_Seq):
    print x.first,
    x = x.rest
  return args.first

# Predicates
@nu_lambda("fn?", "X")
def w_fnq(env, x):
  if isinstance(x, w_Wrapped):
    return x
  else:
    return w_false

@nu_lambda("is?", rest="Args")
def w_is(env, args):
  while 1:
    if args.rest == w_nil:
      return w_true
    elif args.first != args.rest.first:
      return w_false
    args = args.rest

@nu_lambda("vau?", "X")
def w_vauq(env, x):
  if isinstance(x, w_Vau):
    return x
  else:
    return w_false

# Vaus
@nu_vau("$assign", "Name", "X")
def w_assign(env, x, y):
  try:
    env[x]
  except KeyError:
    y = eval_(env, y)
    env[x] = y
    return y
  raise w_MutationError(x)

@nu_vau("$if", rest="Args")
def w_if(env, args):
  try:
    while eval_(env, args.first) == w_false:
      args = args.rest.rest
    if args.rest != w_nil:
      args = args.rest
    return eval_(env, args.first)
  except AttributeError:
    return w_false

@nu_vau("$vau", "Env", "Args", rest="Body")
def w_vau(env, e, args, body):
  return w_Vau(env, e, args, body)

# Fns
@nu_lambda("add", rest="Args")
def w_add(env, args):
  #try:
  return w_Number(sum(args))
  #except (TypeError, AttributeError):
  #  return w_false

@nu_lambda("error", rest="Args")
def w_error(env, args):
  raise w_BaseError(join_string(args).tostring())

@nu_lambda("eval", rest="Args")
def w_eval(env, args):
  if args.rest == w_nil:
    return eval_(eval_(env, w_Symbol("%Env")), args.first)
  elif args.rest.rest == w_nil:
    return eval_(eval_(env, args.first), args.rest.first)
  else:
    raise w_PatternFail(seq("Env", "X"), args)

@nu_lambda("uniq")
def w_uniq(env):
  return w_Uniq()

@nu_lambda("unwrap", "X")
def w_unwrap(env, x):
  if isinstance(x, w_Wrapped):
    return x.wrapped
  else:
    raise w_TypeError("fn", x)

@nu_lambda("wrap", "X")
def w_wrap(env, x):
  return w_Wrapped(x)

top_env = w_TopEnv({
  # Syntax
  "$fn"      : w_arrow,
  "apply"    : w_apply,
  "seq"      : w_seq,
  "str"      : w_string,

  # Constants
  "%t"       : w_true,
  "%f"       : w_false,
  "%eof"     : w_eof,

  # Non-referentially transparent
  "$assign!" : w_assignd,
  "pr!"      : w_pr,
  "write!"   : w_write,

  # Predicates
  "fn?"      : w_fnq,
  "is?"      : w_is,
  "vau?"     : w_vauq,

  # Vaus
  "$assign"  : w_assign,
  "$if"      : w_if,
  "$vau"     : w_vau,

  # Fns
  "add"      : w_add,
  "error"    : w_error,
  "eval"     : w_eval,
  "uniq"     : w_uniq,
  "unwrap"   : w_unwrap,
  "wrap"     : w_wrap,
})
