# AVL Tree algorithm courtesy of waterhouse (http://www.arclanguage.org/item?id=14181)

class Nil(object):
  count = 0
  depth = 0

  def __repr__(self):
    return "[]"
  def __iter__(self):
    return self
  def __reversed__(self):
    return self
  def next(self):
    raise StopIteration

nil = Nil()
nil.l = nil
nil.r = nil


class Tree(object):
  def __init__(self, l, r):
    self.l     = l
    self.r     = r
    self.depth = max(l.depth, r.depth) + 1

  def ret(self):
    return self

  def __iter__(self):
    for x in self.l:
      yield x
    yield self.ret()
    for x in self.r:
      yield x

  def __reversed__(self):
    for x in reversed(self.r):
      yield x
    yield self.ret()
    for x in reversed(self.l):
      yield x


# O(1)       element count
# O(log2(n)) lookup by index
class Ordered(Tree):
  def __init__(self, l, r):
    Tree.__init__(self, l, r)
    self.count = l.count + r.count + 1

  def __len__(self):
    return self.count

  def __repr__(self):
    return "(%seq {})".format(" ".join(repr(x) for x in self))

  def __str__(self):
    return "[{}]".format(" ".join(str(x) for x in self))

  def ret(self):
    return self.value

  def transfer(self, y):
    self.value = y.value
    return self


# O(log2(n)) lookup by key
class Dictionary(Ordered):
  def __repr__(self):
    return "(dict {})".format(" ".join("{!r} {!r}".format(*x) for x in self))

  def __str__(self):
    return "{{{}}}".format(" ".join("{} {}".format(*x) for x in self))

  def ret(self):
    return self.key, self.value

  def transfer(self, y):
    self.key   = y.key
    self.value = y.value
    return self

  def key_ins(self, key, value):
    self.key   = key
    self.value = value
    return self


# O(log2(n)) lookup by value
class Set(Dictionary):
  def __repr__(self):
    return "(&set {})".format(" ".join(repr(x) for x in self))

  __str__ = __repr__

  def ret(self):
    return self.key

  def transfer(self, y):
    self.key   = y.key
    self.value = y.key
    return self

  def key_ins(self, key, value):
    self.key   = key
    self.value = key
    return self



# Balanced tree construction
def bcons(cons, tree, l, r):
  if l.depth > r.depth + 1:
    if l.l.depth > l.r.depth:
      return cons(l.l,
                  cons(l.r, r).transfer(tree)).transfer(l)
    else:
      return cons(cons(l.l,   l.r.l).transfer(l),
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
  elif l.depth < r.depth:
    return bcons(cons, r,
                 simple_merge(cons, l, r.l),
                 r.r)
  else:
    return bcons(cons, l,
                 l.l,
                 simple_merge(cons, l.r, r))



def ind_get(tree, ind, default=9001):
  c = tree.l.count
  if tree == nil:
    return default
  elif ind == c:
    return tree.value
  elif ind < c:  # left branch
    return ind_get(tree.l, ind, default)
  else:          # right branch
    return ind_get(tree.r, ind - c - 1, default)

def ind_ins(cons, tree, ind, value):
  if not -1 <= ind <= tree.count:
    raise IndexError(ind)
  c = tree.l.count
  if tree == nil:
    x = cons(nil, nil)
    x.value = value
    return x
  elif ind == c:
    x = cons(tree.l, tree.r)
    x.value = value
    return x
  elif ind < c:  # left branch
    return bcons(cons, tree,
                 ind_ins(cons, tree.l, ind, value),
                 tree.r)
  else:          # right branch
    return bcons(cons, tree,
                 tree.l,
                 ind_ins(cons, tree.r, ind - c - 1, value))

# TODO provide a default that's returned if deletion fails?
def ind_rem(cons, tree, ind):
  if not -1 < ind < tree.count:
    raise IndexError(ind)
  c = tree.l.count
  if ind == c:
    return simple_merge(cons, tree.l, tree.r)
  elif ind < c:  # left branch
    return bcons(cons, tree,
                 ind_rem(cons, tree.l, ind),
                 tree.r)
  else:          # right branch
    return bcons(cons, tree,
                 tree.l,
                 ind_rem(cons, tree.r, ind - c - 1))


def key_has(tree, key):
  if tree == nil:
    # TODO
    return False
  elif key == tree.key:
    # TODO
    return True
  elif key < tree.key:
    return key_has(tree.l, key)
  else:
    return key_has(tree.r, key)

def key_get(tree, key, default=9001):
  if tree == nil:
    return default
  elif key == tree.key:
    return tree.value
  elif key < tree.key:
    return key_get(tree.l, key, default)
  else:
    return key_get(tree.r, key, default)

def key_ins(cons, tree, key, value):
  if tree == nil:
    return cons(nil, nil).key_ins(key, value)
  elif key == tree.key:
    return cons(tree.l, tree.r).key_ins(key, value)
  elif key < tree.key:
    return bcons(cons, tree,
                 key_ins(cons, tree.l, key, value),
                 tree.r)
  else:
    return bcons(cons, tree,
                 tree.l,
                 key_ins(cons, tree.r, key, value))

# TODO provide a default that's returned if deletion fails?
def key_rem(cons, tree, key):
  if tree == nil:
    raise KeyError(key)
  elif key == tree.key:
    return simple_merge(cons, tree.l, tree.r)
  elif key < tree.key:
    return bcons(cons, tree,
                 key_rem(cons, tree.l, key),
                 tree.r)
  else:
    return bcons(cons, tree,
                 tree.l,
                 key_rem(cons, tree.r, key))



# O(n log2(n))
def iter_to_list(xs, cons=Ordered):
  y = nil
  for k in xs:
    y = ind_ins(cons, y, y.count, k)
  return y

# O(n log2(n))
def iter_to_set(xs, cons=Set):
  y = nil
  for k in xs:
    y = key_ins(cons, y, k, k)
  return y

# O(n log2(n))
def dict_to_dict(xs, cons=Dictionary):
  y = nil
  for k in xs:
    y = key_ins(cons, y, k, xs[k])
  return y

# O(n)
def list_to_list(xs):
  pass
