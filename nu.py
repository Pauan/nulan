import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *

#reader = read(sys.stdin)
reader = None

def repl(prompt="=> "):
  try:
    sys.stdout.write(prompt)
    print repr(eval_(top_env, reader.next()))
  except w_Thrown as e:
    sys.stderr.write("{}\n".format(e))
  except KeyboardInterrupt:
    print
  except StopIteration:
    print
    return
  repl(prompt)

def load_file_in(env, name):
  for x in read_file(name):
    try:
      eval_(env, x)
    except w_Thrown as e:
      sys.stderr.write("{}\n".format(x))
      sys.stderr.write("{}\n".format(e))

#load_file_in(top_env, "nu.nu")

if __name__ == "__main__":
  import doctest
  #doctest.testfile("tests/nu_reader.py")
  #doctest.testfile("tests/TEMP.py")
  #repl()
