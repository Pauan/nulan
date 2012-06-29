import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *
#from nu_types import w_Symbol, w_Cons, w_true, w_false

eof = {}

def repl():
  try:
    print repr(eval_(glob, readstring(raw_input("=> "), eof)))
  except EOFError:
    return
  except (w_SyntaxError, w_BaseError) as e:
    print e
#  except Exception as e:
#                    # TODO: is there a better way to print the type of the error?
#    print "{}: {}".format(type(e).__name__, e)
  repl()

#with open("nu.nu") as f:
#  s = w_Stream(lambda: f.read(1), "nu.nu")
#  while read(s, eof) != eof:
#    pass


if __name__ == "__main__":
  #import doctest
  #doctest.testfile("tests/nu_reader.py")
  repl()
