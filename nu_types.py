##############################################################################
#  Helpers
##############################################################################
def eval_(env, x):
  if isinstance(x, W_Symbol):
    return env[x]
  elif isinstance(x, W_Cons):
    return eval_(env, x.car)(env, x.cdr)
  else:
    return x

def list_to_cons(x):
  result = w_nil
  for x in reversed(x):
    result = W_Cons(x, result)
  return result

def pattern_match(closure, pattern, args):
  seen = {}
  def rec(env, p, a):
    if isinstance(p, W_Cons):
      if p.car == w_list:
        p = p.cdr
        while isinstance(p, W_Cons):
          if not isinstance(a, W_Cons):
            return w_false
          r = rec(env, p.car, a.car)
          if r == w_false:
            return r
          p = p.cdr
          a = a.cdr
        return rec(env, p, a)
      else:
        return w_false
    elif isinstance(p, W_Symbol):
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
    return W_Builtin(fn)
  return decorator

def nu_lambda(name):
  def decorator(fn):
    fn.__name__ = name
    return W_WrappedBuiltin(W_Builtin(fn))
  return decorator

##############################################################################
#  Types
##############################################################################
class W_Stream:
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

#class W_Char:
#  interned = {}
#  def __init__(self, value):
#    self.value = value
#  def __repr__(self):
#    if self.value in "\"\\@":
#      return "\\{}".format(self.value)
#    elif self.value == "\n":
#      return "\\n"
#    elif self.value == "\t":
#      return "\\t"
#    else:
#      return repr(self.value)
#
#def char(x):
#  if not x in W_Char.interned:
#    W_Char.interned[x] = W_Char(x)
#  return W_Char.interned[x]

## Generic stuff for symbols and chars
class W_Base:
  def __init__(self, value):
    self.value = value
  def __str__(self):
    return str(self.value)
  def __repr__(self):
    #self.line = None
    #self.column = None
    #return "{} at {}x{}".format(self.value, self.line, self.column)
    return repr(self.value)
  def __eq__(self, x):
    return self.value == x
  def __ne__(self, x):
    return not self == x
  def __hash__(self):
    return hash(self.value)
  def __radd__(self, x):
    return self.__add__(x)

class W_Number(W_Base):
  def __init__(self, value):
    self.value = float(value)
  def __str__(self):
    return "{:g}".format(self.value)
  def __repr__(self):
    return "(&number {})".format(str(self))
  def __add__(self, x):
    return self.value + x

class W_Char(W_Base):
  def __str__(self):
    try:
      return self.tostring
    except AttributeError:
      ## Prints ' \ and @ with a preceding \
      if self.value in "'\\@":
        return "\\{}".format(self.value)
      else:
        return self.value
      #x = ord(self.value)
      #if 31 < x < 127:
      #  return self.value
      #else:
      #  return "\\u{}".format(to_hex(x))
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
  #  return (W_Base.__eq__(self, x) or
  #          (isinstance(x, W_Cons) and
  #           x.cdr == w_nil and
  #           x.car == self))

class W_Symbol(W_Base):
  def __repr__(self):
    return "(&symbol {})".format(self.value)

#class W_Keyword:
#  interned = {}
#  def __init__(self, value):
#    self.value = value
#
#def keyword(x):
#  if not x in W_Keyword.interned:
#    W_Keyword.interned[x] = W_Keyword(x)
#  return W_Keyword.interned[x]

class W_Nil:
  def __repr__(self):
    return "[]"
  def join(self, y):
    return y

w_nil = W_Nil()

class W_Cons:
  def __init__(self, car, cdr):
    self.car = car
    self.cdr = cdr

  def map(self, f):
    x = self
    # TODO: code duplication
    top = r = W_Cons(f(x.car), w_nil)
    x = x.cdr
    while isinstance(x, W_Cons):
      r.cdr = W_Cons(f(x.car), w_nil)
      r = r.cdr
      x = x.cdr
    #r.cdr = f(x)
    return top

  def join(self, y):
    x = self
    top = r = W_Cons(x.car, w_nil)
    x = x.cdr
    while isinstance(x, W_Cons):
      r.cdr = W_Cons(x.car, w_nil)
      r = r.cdr
      x = x.cdr
    if x != w_nil:
      raise TypeError("cannot call join on improper list")
    r.cdr = y
    return top

  def __iter__(self):
    item = self
    while 1:
      if isinstance(item, W_Cons):
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
      if isinstance(x, W_Cons):
        if char_all and not isinstance(x.car, W_Char):
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
      x = x.cdr.car.join(W_Cons(W_Symbol("->"), x.cdr.cdr))
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
      if isinstance(x, W_Cons):
        if char_all and not isinstance(x.car, W_Char):
          char_all = False
        result.append(x.car)
        x = x.cdr
      ## It's an improper list: print it as a list with a bar
      else:
        return "{0[0]}{1} | {2}{0[1]}".format(braces,
                                              " ".join(str(x) for x in result),
                                              str(x))

class W_Env:
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

class W_Vau:
  def __init__(self, closure, env, args, body):
    self.closure = closure
    self.args    = args
    self.body    = body
    self.env     = env

  def __repr__(self):
    return "(&vau)"

  def __call__(self, env, args):
    # TODO: maybe figure out a way to not need to create and
    #       destroy a new environment every time the vau is called
    inner = W_Env(self.closure)

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

#  def pattern_match(closure, pattern, args):
#    seen = {}
#    s = W_Symbol("&square-brackets")
#    x = pattern
#    while isinstance(x, W_Cons):
#      if x.car == s
#        c = x.cdr.car
#        r = x.cdr.cdr.car
#        if isinstance(c, W_Cons) and c.car == s:
#          self.pattern_match(closure, x.car, args.car)
#        elif isinstance(c, W_Symbol) and c.value not in seen:
#          closure[c] = args.car
#        elif c != args.car:
#          return w_false
#        args = args.cdr
#      else
#        raise Exception("non-cons in pattern: {}".format(x))
#      x = x.cdr

class W_Wrapped(W_Vau):
  def __init__(self, f):
    self.value = f
  def __repr__(self):
    return "(&fn)"
  def __call__(self, env, args):
    return self.value(env, args.map(lambda x: eval_(env, x)))

class W_Builtin(W_Vau):
  def __init__(self, f):
    self.value = f
  def __repr__(self):
    return "(&vau {})".format(self.value.__name__)
  def __call__(self, env, args):
    return self.value(env, args)

class W_WrappedBuiltin(W_Builtin):
  def __init__(self, f):
    self.value = f
  def __repr__(self):
    return "(&fn {})".format(self.value.value.__name__)
  def __call__(self, env, args):
    return self.value(env, args.map(lambda x: eval_(env, x)))
    #return self.value(env, [eval_(env, x) for x in args])

##############################################################################
#  Syntax
##############################################################################
@nu_vau("&arrow")
def w_arrow(env, args):
  return W_Wrapped(W_Vau(env, w_tilde, args.car, args.cdr))

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

def join_all(x):
  return x

@nu_lambda("&string")
def w_string(env, args):
  return join_all(args)

@nu_lambda("&tilde")
def w_tilde(env, args):
  return w_true

##############################################################################
#  Primitives
##############################################################################
w_true  = W_Symbol("%t")
w_false = W_Symbol("%f")

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
  return W_Vau(env, args.car, args.cdr.car, args.cdr.cdr)

@nu_lambda("add")
def w_add(env, args):
  return W_Number(sum(args))

@nu_lambda("eval")
def w_eval(env, args):
  if args.cdr.cdr != w_nil:
    return w_false
  else:
    return eval_(env, args.cdr.car)

@nu_lambda("unwrap")
def w_unwrap(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    if isinstance(args.car, (W_Wrapped, W_WrappedBuiltin)):
      return args.car.value
    else:
      return w_false

@nu_lambda("wrap")
def w_wrap(env, args):
  if args.cdr != w_nil:
    return w_false
  else:
    return W_Wrapped(car.args)

glob = {
  "%t"     : w_true,
  "%f"     : w_false,
  "$set!"  : w_set,
  "$vau"   : w_vau,
  "add"    : w_add,
  "eval"   : w_eval,
  "unwrap" : w_unwrap,
  "wrap"   : w_wrap,
}
