import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *
#from nu_types import W_Symbol, W_Cons, w_true, w_false

eof = {}

def repl():
  try:
    print repr(eval_(glob, read(raw_input("=> "), eof)))
  except EOFError:
    return
  #except Exception as e:
  #                  # TODO: is there a better way to print the type of the error?
  #  print "{}: {}".format(type(e).__name__, e)
  repl()


if __name__ == "__main__":
  #import doctest
  #doctest.testfile("tests/nu_reader.py")
  repl()
