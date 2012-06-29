##############################################################################
#  Helpers
##############################################################################
def eval_(env, x):
  if isinstance(x, w_Symbol):
    return env[x]
  elif isinstance(x, w_Cons):
    return eval_(env, x.car)(env, x.cdr)
  else:
    return x

def list_to_cons(x):
  result = w_nil
  for x in reversed(x):
    result = w_Cons(x, result)
  return result

def join_string(args):
  x = args
  result = []
  while isinstance(x, w_Cons):
    if isinstance(x.car, w_Cons):
      result += join_string(x.car)
    elif isinstance(x.car, w_Char):
      result.append(x.car)
    else:
      raise TypeError("cannot coerce {} in {} to a string".format(x.car, args))
    x = x.cdr
  return list_to_cons(result)

def pattern_match(closure, pattern, args):
  seen = {}
  def rec(env, p, a):
    if isinstance(p, w_Cons):
      print repr(p)
      if p.car == w_apply:
        p = p.cdr
        if p.car == w_list:
          p = p.cdr
          while isinstance(p.cdr, w_Cons):
            if not isinstance(a, w_Cons):
              return w_false
            r = rec(env, p.car, a.car)
            if r == w_false:
              return r
            p = p.cdr
            a = a.cdr
          return rec(env, p.car, a)
        else:
          raise Exception("foo")
      elif p.car == w_list:
        p = p.cdr
        while isinstance(p, w_Cons):
          if not isinstance(a, w_Cons):
            return w_false
          r = rec(env, p.car, a.car)
          if r == w_false:
            return r
          p = p.cdr
          a = a.cdr
        return rec(env, p, a)
      else:
        raise Exception("foo")
    elif isinstance(p, w_Symbol):
      if p in seen:
        if env[p] != a:
          return w_false
      else:
        seen[p] = True
        env[p] = a
    elif p == w_tilde:
      return
    else:
      if p != a:
        return w_false
  return rec(closure, pattern, args)

def nu_vau(name):
  def decorator(fn):
    fn.__name__ = name
    return w_Builtin(fn)
  return decorator

def nu_lambda(name):
  def decorator(fn):
    fn.__name__ = name
    return w_Wrapped(w_Builtin(fn))
  return decorator

##############################################################################
#  Exceptions
##############################################################################
class w_BaseError(Exception):
  def __init__(self, message):
    self.message = message
  def __str__(self):
    return "error: {}".format(self.message)

class w_TypeError(w_BaseError):
  pass

class w_SyntaxError(w_BaseError, SyntaxError):
  def __init__(self, message, s, line=None, column=None, text=None, filename=None):
    self.message  = message
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

##############################################################################
#  Types
##############################################################################
class w_Stream:
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

## Generic stuff for chars, symbols, numbers, etc.
class w_Base:
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
      ## Prints ' \ and @ with a preceding \
      if self.value in "'\\@":
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
  #          (isinstance(x, w_Cons) and
  #           x.cdr == w_nil and
  #           x.car == self))

class w_Symbol(w_Base):
  def __repr__(self):
    return "(&symbol {})".format(self.value)

class w_Uniq:
  counter = 1
  def __init__(self):
    self.counter = w_Uniq.counter
    w_Uniq.counter += 1
  def __repr__(self):
    return "(&uniq {})".format(self.counter)

class w_Nil:
  def __repr__(self):
    return "[]"
  def join(self, y):
    return y
  def map(self, f):
    return self

w_nil = w_Nil()

class w_Cons(w_Nil):
  def __init__(self, car, cdr):
    self.car = car
    self.cdr = cdr

  def __iter__(self):
    item = self
    while 1:
      if isinstance(item, w_Cons):
        yield item.car
        item = item.cdr
      elif item == w_nil:
        return
      else:
        raise TypeError("can't iterate on a dotted list")

  def __repr__(self):
    x        = self
    x.line   = None
    x.column = None
    char_all = True ## Are all the elements chars?
    result   = []
    while 1:
      ## It's a proper list: print it as a list or a string
      if x == w_nil:
        ## It's a list of characters: print it as a string
        if char_all:
          ## This uses str because chars have different printing in strings
          return "'{}'".format("".join(str(x) for x in result))
        ## Print it as a list
        else:
          return "[{}]".format(" ".join(repr(x) for x in result))
      if isinstance(x, w_Cons):
        if char_all and not isinstance(x.car, w_Char):
          char_all = False
        result.append(x.car)
        x = x.cdr
      ## It's an improper list: print it as a list with a bar
      else:
        return "[{} | {}]".format(" ".join(repr(x) for x in result), repr(x))

  ## This is just for pretty printing
  def __str__(self):
    x        = self
    x.line   = None
    x.column = None
    char_all = True ## Are all the elements chars?
    result   = []
    braces   = "()"

    if (x.car == w_list or
        x.car == w_string):
      braces = "[]"
      x = x.cdr
    elif x.car == w_arrow:
      x = x.cdr.car.join(w_Cons(w_Symbol("->"), x.cdr.cdr))
      #print repr(x)
    #elif x.car == w_apply:
    #  x = x.cdr

    while 1:
      ## It's a proper list: print it as a list or a string
      if x == w_nil:
        ## It's a list of characters: print it as a string
        if char_all:
          ## This uses str because chars have different printing in strings
          return "'{}'".format("".join(str(x) for x in result))
        ## Print it as a list
        else:
          return "{0[0]}{1}{0[1]}".format(braces, " ".join(str(x) for x in result))
      if isinstance(x, w_Cons):
        if char_all and not isinstance(x.car, w_Char):
          char_all = False
        result.append(x.car)
        x = x.cdr
      ## It's an improper list: print it as a list with a bar
      else:
        return "{0[0]}{1} | {2}{0[1]}".format(braces,
                                              " ".join(str(x) for x in result),
                                              str(x))

  # TODO: code duplication with join
  def map(self, f):
    x = self
    top = r = w_Cons(f(x.car), w_nil)
    x = x.cdr
    while isinstance(x, w_Cons):
      r.cdr = w_Cons(f(x.car), w_nil)
      r = r.cdr
      x = x.cdr
    r.cdr = x #f(x)
    return top

  def join(self, y):
    x = self
    top = r = w_Cons(x.car, w_nil)
    x = x.cdr
    while isinstance(x, w_Cons):
      r.cdr = w_Cons(x.car, w_nil)
      r = r.cdr
      x = x.cdr
    if x != w_nil:
      raise TypeError("cannot call join on improper list {}".format(repr(self)))
    r.cdr = y
    return top

  def tostring(self):
    x = self
    result = []
    while 1:
      if x == w_nil:
        break
      elif isinstance(x, w_Cons) and isinstance(x.car, w_Char):
        result.append(str(x.car))
        x = x.cdr
      else:
        raise TypeError("cannot convert {} to a string".format(repr(self)))
    return "".join(result)

class w_Env:
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

class w_GlobalEnv(w_Env):
  def __init__(self, variables):
    self.variables = variables
  def __getitem__(self, name):
    try:
      return self.variables[name]
    except KeyError:
      raise w_VariableError(name)

class w_Vau:
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

  def __call__(self, env, args):
    # TODO: maybe figure out a way to not need to create and
    #       destroy a new environment every time the vau is called
    inner = w_Env(self.closure)

    if self.env != w_tilde:
      inner[self.env] = env

    m = pattern_match(inner, self.args, args)
    if m == w_false:
      return m

    x = self.body
    last = w_false
    while x != w_nil:
      last = eval_(inner, x.car)
      x = x.cdr
    return last

class w_Builtin(w_Vau):
  def __init__(self, f):
    self.value = f
    self.__name__ = f.__name__
  def __call__(self, env, args):
    return self.value(env, args)

class w_Wrapped:
  def __init__(self, f):
    self.value = f
  def __repr__(self):
    try:
      return "(&fn {})".format(self.value.__name__)
    except AttributeError:
      return "(&fn)"
  def __call__(self, env, args):
    return self.value(env, args.map(lambda x: eval_(env, x)))

##############################################################################
#  Syntax
##############################################################################
@nu_vau("&arrow")
def w_arrow(env, args):
  return w_Wrapped(w_Vau(env, w_tilde, args.car, args.cdr))

@nu_vau("&apply")
def w_apply(env, args):
  x = args
  if x.cdr == w_nil:
    return eval_(env, x.car)
  else:
    while x.cdr.cdr != w_nil:
      x = x.cdr
    x.cdr = eval_(env, x.cdr.car)
    return eval_(env, args)

@nu_lambda("&list")
def w_list(env, args):
  return args

@nu_lambda("&string")
def w_string(env, args):
  return join_string(args)

@nu_lambda("&tilde")
def w_tilde(env, args):
  return w_true

##############################################################################
#  Primitives
##############################################################################
w_true  = w_Symbol("%t")
w_false = w_Symbol("%f")

@nu_vau("$if")
def w_if(env, args):
  try:
    while eval_(env, args.car) == w_false:
      args = args.cdr.cdr
    if args.cdr != w_nil:
      args = args.cdr
    return eval_(env, args.car)
  except AttributeError:
    return w_false

@nu_vau("$set!")
def w_set(env, args):
  if args.cdr.cdr != w_nil:
    return w_false
  else:
    x = eval_(env, args.cdr.car)
    env[args.car] = x
    return x

@nu_vau("$vau")
def w_vau(env, args):
  return w_Vau(env, args.car, args.cdr.car, args.cdr.cdr)

@nu_lambda("add")
def w_add(env, args):
  try:
    return w_Number(sum(args))
  except TypeError:
    return w_false

@nu_lambda("error")
def w_error(env, args):
  raise w_BaseError(join_string(args).tostring())

@nu_lambda("eval")
def w_eval(env, args):
  if args.cdr.cdr != w_nil:
    return w_false
  else:
    return eval_(env, args.cdr.car)

@nu_lambda("fn?")
def w_fnq(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    if isinstance(args.car, w_Wrapped):
      return args.car
    else:
      return w_false

@nu_lambda("null?")
def w_nullq(env, args):
  if args.cdr != w_nil:
    return w_false
  elif args.car == w_nil:
    return args.car
  else:
    return w_false

@nu_lambda("uniq")
def w_uniq(env, args):
  if args != w_nil:
    return w_false
  else:
    return w_Uniq()

@nu_lambda("unwrap")
def w_unwrap(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    if isinstance(args.car, w_Wrapped):
      return args.car.value
    else:
      return w_false

@nu_lambda("vau?")
def w_vauq(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    if isinstance(args.car, w_Vau):
      return args.car
    else:
      return w_false

@nu_lambda("wrap")
def w_wrap(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    return w_Wrapped(args.car)

glob = w_GlobalEnv({
  "%t"     : w_true,
  "%f"     : w_false,
  "$if"    : w_if,
  "$set!"  : w_set,
  "$vau"   : w_vau,
  "add"    : w_add,
  "error"  : w_error,
  "eval"   : w_eval,
  "fn?"    : w_fnq,
  "null?"  : w_nullq,
  "uniq"   : w_uniq,
  "unwrap" : w_unwrap,
  "vau?"   : w_vauq,
  "wrap"   : w_wrap,
})
