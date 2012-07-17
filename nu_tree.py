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

class Nil(Tree):
  def __init__(self):
    self.level = 0
  def __repr__(self):
    return "[]"

nil = Nil()
nil.l = nil
nil.r = nil

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


#    3
#  1   4
# 0   2
# cons()


# 0

# 0
#  1
#   2

#

# 0
#  1
#   2


#    3
#     4

#  1
# 0

#   2
#  1
# 0

#     4
#    3
#   2
#  1
# 0

#Tree(3, 40, l=Tree(1, 20, l=Tree(0, 10), r=Tree(2, 30)), r=Tree(4, 50))


#x = Tree(0)
#x = Tree(1, 20, l=x, r=Tree(2, 30))
