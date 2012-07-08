import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *

def get_input(prompt="=> ", eof=w_eof):
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
    print repr(eval_(top_env, get_input()))
    #print repr(eval_(top_env, readstring(raw_input("=> "), eof)))
  except EOFError:
    return
  except w_BaseError as e:
    print e
  except KeyboardInterrupt:
    print
  repl()

#def load_file_in(env, name):
#  with open(name, "r") as f:
#    f = w_InputStream(f)
#    while 1:
#      try:
#        x = read(f, eof)
#        if x == eof:
#          break
#        else:
#          try:
#            eval_(env, x)
#          except w_BaseError as e:
#            print x
#            raise
#      except w_BaseError as e:
#        print e
#        break

def load_file_in(env, name):
  for x in read_file(name):
    try:
      eval_(env, x)
    except w_BaseError as e:
      print x
      print e

#load_file_in(top_env, "nu.nu")

if __name__ == "__main__":
  import doctest
  doctest.testfile("tests/nu_reader.py")
  #doctest.testfile("tests/TEMP.py")
  #repl()
