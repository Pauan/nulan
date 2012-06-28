import sys

sys.dont_write_bytecode = True

from nu_reader import *
#from nu_types import W_Symbol, W_Cons, w_true, w_false

glob = {
  "%t"   : w_true,
  "%f"   : w_false,
  "&add" : nu_lambda(lambda env, args: sum(args)),
}


eof = {}

def eval_(env, x):
  if isinstance(x, W_Symbol):
    return env[x]
  elif isinstance(x, W_Cons):
    eval_(env, x.car)(env, x.cdr)
  else:
    return x

def repl():
  try:
    print eval_(glob, read(raw_input("> "), eof))
  except EOFError:
    print
    return
  except Exception as e:
                    # TODO: is there a better way to print the type of the error?
    print "{}: {}".format(type(e).__name__, e)
  repl()


if __name__ == "__main__":
  import doctest
  doctest.testfile("tests/nu_reader.py")
  #repl()
