import sys
import readline

sys.dont_write_bytecode = True

from nu_reader import *

reader = read_all(sys.stdin)

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
  def test_read(x):
    try:
      print read1(x).pretty()
    except w_Thrown as e:
      print e
#      print list(nu_reader.tokenize(nu_reader.IOBuffer(x)))

  def test_write(x):
    try:
      print repr(read1(x))
    except w_Thrown as e:
      print e
#      print list(nu_reader.tokenize(nu_reader.IOBuffer(x)))

  def test_write_all(x):
    try:
      o = sys.stderr
      sys.stderr = sys.stdout
      for x in read_all(x):
        print repr(x)
      sys.stderr = o
    except w_Thrown as e:
      print e

  test_globs = {
    "read": test_read,
    "write": test_write,
    "write_all": test_write_all,
  }

  import doctest
  #doctest.testfile("tests/nu_reader.py")
  doctest.testfile("tests/nu_reader (abbreviations).py", globs=test_globs)
  #doctest.testfile("tests/TEMP.py")
  #repl()
