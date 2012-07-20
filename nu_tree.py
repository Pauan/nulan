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

#str(iter_to_tree([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))

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

  def map(self):
    return self

  def __iter__(self):
    for x in self.l:
      yield x
    yield self.map()
    for x in self.r:
      yield x

  def __reversed__(self):
    for x in self.r:
      yield x
    yield self.map()
    for x in self.l:
      yield x

  def __repr__(self):
    return "({!r} {!r})".format(self.l, self.r)


# Tree with O(1) length and lookup by index
class Ordered(Tree):
  def __init__(self, l, r):
    Tree.__init__(self, l, r)
    #self.value = value
    self.count = l.count + r.count + 1

  def __str__(self):
    return "[{}]".format(" ".join(str(x) for x in self))

  def __repr__(self):
    return "(%seq {})".format(" ".join(repr(x) for x in self))

  #def __str__(self):
  #  return "({} {}={} {})".format(str(self.l), self.value, self.count, str(self.r))

  def map(self):
    return self.value

  def transfer(self, y):
    self.value = y.value
    return self

  def assign(self, value):
    self.value = value
    return self


# Dictionary where the values are the keys
class Set(Ordered):
  def __init__(self, l, r):
    Ordered.__init__(self, l, r)
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

  #def getkey(self, key, default=w_false):
  #  return self.haskey(key) or default

  def setkey(self, key, value=None):
    if self == nil:
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
    pass

  def __repr__(self):
    if self.l == nil:
      if self.r == nil:
        return "({}={})".format(self.key, self.value)
      else:
        return "({}={} {})".format(self.key, self.value, self.r)
    elif self.r == nil:
      return "({} {}={})".format(self.l, self.key, self.value)
    else:
      return "({} {}={} {})".format(self.l, self.key, self.value, self.r)

  #def getkey(self, key, default=w_false):
  #  if key == self.key:
  #    return self.value
  #  elif key < self.key:
  #    return self.l.getkey(key, default=default)
  #  else:
  #    return self.r.getkey(key, default=default)

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
def bcons(cons, l, r, tree):
  if l.depth > r.depth + 1:
    if l.l.depth > l.r.depth:
      return cons(l.l,
                  cons(l.r, r).transfer(tree)).transfer(l)
    else:
      return cons(cons(l.l,   l.r.l).transfer(l)
                  cons(l.r.r, r    ).transfer(tree)).transfer(l.r)
  elif r.depth > l.depth + 1:
    if r.r.depth > r.l.depth:
      return cons(cons(l, r.l).transfer(tree),
                  r.r).transfer(r)
    else:
      return cons(cons(l,     r.l.l).transfer(tree),
                  cons(r.l.r, r.r  ).transfer(r)).transfer(r.l)
  else:
    return cons(l, r).transfer(tree)

# Not a general merge; assumes [all of a] <= [all of b]
def simple_merge(cons, l, r):
  if l == nil:
    return r
  elif r == nil:
    return l
  elif l.depth < b.depth:
    return bcons(cons, r
                 simple_merge(l, r.l),
                 r.r)
  else:
    return bcons(cons, l,
                 l.l,
                 simple_merge(l.r, r))


def ind_get(tree, ind, default=9001):
  c = tree.l.count
  #print tree, ind, c
  if tree == nil:
    return default
  elif ind == c:
    return tree.map()
  elif ind < c:  # left branch
    return ind_get(tree.l, ind, default)
  #elif ind == tree.r.count:
  #  return tree.map()
  else:          # right branch
    #print "RIGHT", tree, ind, tree.r.count
    return ind_get(tree.r, ind - c - 1, default)

def ind_ins(cons, tree, ind, value):
  c = tree.l.count
  #print c, ind
  if tree == nil:
    return cons(nil, nil).assign(value)
  elif ind == c:
    return cons(tree.l, tree.r).assign(value)
  elif ind < c:  # left branch
    return bcons(cons, tree
                 ind_ins(cons, tree.l, ind, value),
                 tree.r)
  else:          # right branch
    return bcons(cons, tree
                 tree.l,
                 ind_ins(cons, tree.r, ind - c - 1, value))

def ind_rem(cons, tree, ind, default=9001):
  c = tree.l.count
  if tree == nil:
    return default
  elif ind == c:
    return simple_merge(cons, tree.l, tree.r)
  elif ind < c:  # left branch
    return bcons(cons, tree
                 ind_rem(cons, tree.l, ind, value),
                 tree.r)
  else:          # right branch
    return bcons(cons, tree
                 tree.l,
                 ind_rem(cons, tree.r, ind - c - 1, value))


# O(n log2(n))
def iter_to_tree(xs, cons):
  xs = iter(xs)
  y  = nil
  try:
    while 1:
      y = ind_ins(cons, y, y.count, xs.next())
  except StopIteration:
    return y

def iter_to_list(xs):
  return iter_to_tree(xs, cons=Ordered)

def iter_to_dict(xs):
  return iter_to_tree(xs, cons=Dictionary)

# O(n)
def list_to_list(xs):
  pass
