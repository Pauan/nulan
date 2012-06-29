import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *

eof = {}

def repl():
  try:
    print repr(eval_(glob, readstring(raw_input("=> "), eof)))
  except EOFError:
    return
  except w_BaseError as e:
    print e
  repl()

#with open("nu.nu") as f:
#  s = w_Stream(lambda: f.read(1), "nu.nu")
#  while read(s, eof) != eof:
#    pass


if __name__ == "__main__":
  #import doctest
  #doctest.testfile("tests/nu_reader.py")
  repl()
