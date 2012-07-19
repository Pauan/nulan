class Tree(object):
  pass

class BTree(Tree):
  def __repr__(self):
    if self.l or self.r:
      return "({} {} {})".format(self.l, self.key, self.r)
    else:
      return str(self.value)
  def get(self, key):
    x = self
    while 1:
      if key == x.key:
        return x.value
      elif key < x.key:
        x = x.l
      else:
        x = x.r
  #def fold(self, f):
  #  pass
  #def __iter__(self):
  #  if l is not None:
  #    inorder(self.l)
  #  if l is None and r is None:
  #    return node.value
  #  if r is not None:
  #    inorder(self.r)

class AATree(BTree):
  def __init__(self, key, value=None, l=None, r=None):
    self.l     = l
    self.r     = r
    self.key   = key
    self.value = value
    self.level = 1
    #if l is None and r is None:
    #  self.level = 1
    #else:
    #  self.level = l.level + 1
    #self.rebalance()
  #def rebalance(self):
  #  if self.level != 1:
  #    if self.l.level == self.level:
  #      l = self.l
  #      self.l = l.l
  #      self.r = None
  #
  #    print self.level, self.l.level, self.r.level
  #def __repr__(self):
  #  if self.l or self.r:
  #    return "({} {} {})".format(self.l, self.level, self.r)
  #  else:
  #    return str(self.value)
  def set(self, key, value):
    x = ret = self
    while 1:
      if key == x.key:
        ret = AATree(key, value)
      elif key < x.key:
        x = x.l
      else:
        x = x.r


#T is leaf and L is leaf
#
#L - T
#     \
#      R


def list_to_tree(xs, cons=AATree):
  xs = enumerate(xs)
  y = cons(*xs.next())
  try:
    while 1:
      y = y.set(*xs.next())
      #y = cons(i, x, l=y, r=cons(i, x))
      print y
  except StopIteration:
    return y

def list_to_tree(xs, cons=AATree):
  l = len(xs)
  i = l


  y = cons(*xs.next())
  try:
    while 1:
      #cons(i / 2, xs[i / 2], )
      y = y.set(*xs.next())
      #y = cons(i, x, l=y, r=cons(i, x))
      print y
  except StopIteration:
    return y


# [10, 20, 30, 40, 50, 60, 70, 80, 90]

# len(xs) / 2

#    30
#  20  40
#10      50

#    40
#  20  50
#10  30


#     3
#  1     5
# 0 2   4 6
#
# order: 0 1 2 3 4 5 6
# depth: 1 2 1 3 1 2 1
# count: 1 3 1 7 1 3 1

class Nil(object):
  count = 0
  depth = 0

  def __repr__(self):
    return "[]"
  def __iter__(self):
    return self
  def next(self):
    raise StopIteration

nil = Nil()
nil.l = nil
nil.r = nil


#def haskey(self, key):
#  return False
#def getkey(self, key, default=w_false):
#  return default
#def setkey(self, key, value):
#  return Dictionary(nil, nil, key, value)


# AVL Tree
class Tree(object):
  def __init__(self, l, r):
    self.l     = l
    self.r     = r
    self.depth = max(l.depth, r.depth) + 1

  def __iter__(self):
    for x in self.l:
      yield x
    yield self
    for x in self.r:
      yield x

  def __reversed__(self):
    for x in self.r:
      yield x
    yield self
    for x in self.l:
      yield x

# AVL Tree + sorted by index
class Ordered(Tree):
  def __init__(self, l, r, value):
    Tree.__init__(self, l, r)
    self.value = value
    self.count = l.count + r.count + 1

  def __iter__(self):
    return (x.value for x in Tree.__iter__(self))

  def __reversed__(self):
    return (x.value for x in Tree.__reversed__(self))

  def __str__(self):
    return "[{}]".format("".join(str(x) for x in self))

  def __repr__(self):
    return "(%seq {})".format("".join(repr(x) for x in self))

  def transfer(self, y):
    self.value = y.value
    return self

  def assign(self, value):
    self.value = value
    return self

#  def hasind(self, ind):
#    if key == self.key:
#      return True
#    elif key < self.key:
#      return self.l.haskey(key)
#    else:
#      return self.r.haskey(key)

#  def getind(self, ind, default=w_false):
#    if self.count == ind:
#      return self.value
#    elif self.l.count < ind:
#      return self.l.getind(ind - self.l.count, default=default)
#    else:
#      return self.r.getind(ind - self.r.count, default=default)

#  def setind(self, ind, value):
#    if self is nil:
#      return List(nil, nil, value)
#    elif self.count == ind:
#    elif self.l.count < ind:
#    else:


# Dictionary where the values are the keys
class Set(Ordered):
  def __init__(self, l, r, value):
    Ordered.__init__(self, l, r, value)
    self.key = value

  def __repr__(self):
    result = []
    for x in self:
      result.append(repr(x.key))
    return "(&set {})".format("".join(result))

  def haskey(self, key):
    if key == self.key:
      return True
    elif key < self.key:
      return self.l.haskey(key)
    else:
      return self.r.haskey(key)

  def getkey(self, key, default=w_false):
    return self.haskey(key) or default

  def setkey(self, key, value=None):
    if self is nil:
      return Set(nil, nil, key)
    elif key == self.key:
      return self
    elif key < self.key:
      return Set(self.l.setkey(key, value),
                 self.r,
                 self.key)
    else:
      return Set(self.l,
                 self.r.setkey(key, value),
                 self.key)


class Dictionary(Set):
  def __init__(self, l, r, key, value):
    Set.__init__(self, l, r, key)
    self.value = value

  def __repr__(self):
    result = []
    for k, v in self:
      result.append(repr(k))
      result.append(repr(v))
    return "(dict {})".format(" ".join(result))

  def __iter__(self):


  def __repr__(self):
    if self.l is nil:
      if self.r is nil:
        return "({}={})".format(self.key, self.value)
      else:
        return "({}={} {})".format(self.key, self.value, self.r)
    elif self.r is nil:
      return "({} {}={})".format(self.l, self.key, self.value)
    else:
      return "({} {}={} {})".format(self.l, self.key, self.value, self.r)

  def getkey(self, key, default=w_false):
    if key == self.key:
      return self.value
    elif key < self.key:
      return self.l.getkey(key, default=default)
    else:
      return self.r.getkey(key, default=default)

  def setkey(self, key, value):
    if key == self.key:
      return Dictionary(self.l, self.r, key, value)
    elif key < self.key:
      return Dictionary(self.l.setkey(key, value),
                        self.r,
                        self.key,
                        self.value)
    else:
      return Dictionary(self.l,
                        self.r.setkey(key, value),
                        self.key,
                        self.value)

#def size(tree):
#  if tree is nil:
#    return 0
#  else:
#    return size(tree.l) + size(tree.r) + 1


#(0.5 < α < 1)

#height(tree) <= log (1 / α) (NodeCount)


# Balanced tree construction
def bcons(cons, l, r, value, trans):
  if l.depth > r.depth + 1:
    if l.l.depth > l.r.depth:
      return trans(cons(l.l,
                        value(cons(l.r, r))),
                   l)
    else:
      return trans(cons(trans(cons(l.l,   l.r.l),
                              l),
                        value(cons(l.r.r, r))),
                   l.r)
  elif r.depth > l.depth + 1:
    if r.r.depth > r.l.depth:
      return trans(cons(value(cons(l, r.l)),
                        r.r),
                   r)
    else:
      return trans(cons(value(cons(l,     r.l.l)),
                        trans(cons(r.l.r, r.r),
                              r)),
                   r.l)
  else:
    return value(cons(l, r))


def getind(tree, ind):
  c = tree.l.count
  if ind == c:
    return tree
  elif ind < c:  # left branch
    return getind(tree.l, ind)
  else:          # right branch
    return getind(tree.r, ind - c)

def setind(cons, tree, ind, value):
  c = tree.l.count
  if tree is nil:
    return cons(nil, nil).assign(value)
  elif ind == c:
    return cons(tree.l, tree.r).assign(value)
  elif ind < c:  # left branch
    return bcons(cons,
                 setind(cons, tree.l, ind, value),
                 tree.r,
                 value=lambda x: x.assign(value),
                 trans=lambda x, y: x.transfer(y))
  else:          # right branch
    return bcons(cons,
                 tree.l,
                 setind(cons, tree.r, ind - c, value),
                 value=lambda x: x.assign(value),
                 trans=lambda x, y: x.transfer(y))


# O(n log2(n))
def iter_to_tree(xs, cons=Ordered):
  xs = iter(xs)
  y  = nil
  try:
    while 1:
      y = setind(cons, y, y.count, xs.next())
      print y
  except StopIteration:
    return y

# O(n)
def list_to_tree(xs):
  pass
