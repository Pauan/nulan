import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *

eof = {}

def get_input(prompt="=> ", eof=eof):
  sys.stdout.write(prompt)
  f = w_InputStream(sys.stdin)
  x = read(f, eof)
  if x == eof:
    print
    raise EOFError
  else:
    return x

def repl():
  try:
    print repr(eval_(glob, get_input()))
    #print repr(eval_(glob, readstring(raw_input("=> "), eof)))
  except EOFError:
    return
  except w_BaseError as e:
    print e
  except KeyboardInterrupt:
    print
  repl()

def load_file_in(env, name):
  with open(name, "r") as f:
    f = w_InputStream(f)
    while 1:
      try:
        x = read(f, eof)
        if x == eof:
          break
        else:
          try:
            eval_(env, x)
          except w_BaseError as e:
            print x
            raise
      except w_BaseError as e:
        print e
        break

load_file_in(glob, "nu.nu")

if __name__ == "__main__":
  #import doctest
  #doctest.testfile("tests/nu_reader.py")
  repl()
