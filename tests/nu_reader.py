>>> import nu_reader

>>> def read(x):
...   print nu_reader.read(x, 9001)

>>> def write(x):
...   print repr(nu_reader.read(x, 9001))

##############################################################################
#  Strings
##############################################################################
>>> read(r"'foo\bar\n\qux'")
Traceback (most recent call last):
  ...
SyntaxError: unknown escape sequence b

>>> read(r"'foo\\\@\tbar'")
'foo\\\@\tbar'

>>> read(r"'foo\uA00Gbar'")
Traceback (most recent call last):
  ...
SyntaxError: G is not valid hexadecimal

>>> read(r"'foo\ua00fbar'")
Traceback (most recent call last):
  ...
SyntaxError: a is not valid hexadecimal

>>> read(r"'foo\u0000bar'")
'foo\u0000bar'

>>> read(r"'foo\uA00Fbar'")
'foo\uA00Fbar'

>>> read("'fooÀbar'")
'fooÀbar'

>>> read(r"'foo\u00C0bar'")
'foo\u00C0bar'


>>> read(r"'foo\u0009bar'")
'foo\u0009bar'

>>> read("'foo	bar'")
'foo  bar'

>>> read(r"'foo\tbar'")
'foo\tbar'

>>> read(r"'foo\u000Abar'")
'foo\u000Abar'

>>> read(r'''
...       'foo
...        bar'
...       ''')
'foo
 bar'

>>> read(r"'foo\nbar'")
'foo\nbar'


>>> write(r"'foo@bar\nqux'")
[(&fn &string) (&char f) (&char o) (&char o) (&symbol bar) (&char \n) (&char q) (&char u) (&char x)]

>>> write("'foo@barqux'")
[(&fn &string) (&char f) (&char o) (&char o) (&symbol barqux)]

>>> write("'foo@'@'bar''qux'")
[(&fn &string) (&char f) (&char o) (&char o) [(&fn &string) [(&fn &string) (&char b) (&char a) (&char r)]] (&char q) (&char u) (&char x)]

>>> write("'foo@(id bar)qux'")
[(&fn &string) (&char f) (&char o) (&char o) [(&symbol id) (&symbol bar)] (&char q) (&char u) (&char x)]

>>> read(r'''
...       'foo\t
...        bar'
...       ''')
'foo\t
 bar'

>>> read(r'''
...       'foo
...          bar
...            qux'
...       ''')
'foo
   bar
     qux'

##############################################################################
#  Raw Strings
##############################################################################
>>> read(r'''
...       `fo@o\\bar\\n\qux`
...       ''')
'fo\@o\\\\bar\\\\n\\qux'

>>> read(r'''
...       `fo@o\\bar`n;qux`
...       ''')
Traceback (most recent call last):
  ...
SyntaxError: missing ending ` quote

>>> read(r'''
...       `fo@o\\bar`
...       n`\qux`
...       ''')
'fo\@o\\\\bar'

>>> read(r'''
...       `foo
...          bar
...            qux`
...       ''')
'foo
         bar
           qux'

##############################################################################
#  Comments
##############################################################################
>>> read(r"#|#|foo|#bar|#qux")
qux

>>> read(r"#||#qux")
qux

>>> read(r'''
...       #|
...         #|
...           foo
...         |#
...       bar
...       |#qux
...       ''')
qux

>>> read(r'''
...       #|
...         #|
...           foo
...         |#
...       bar
...       |#
...       ''')
9001

>>> read(r'''
...       #foobar
...       qux
...       ''')
qux

>>> read(r'''
...       #
...       qux
...       ''')
qux

>>> read("#foobar")
9001

##############################################################################
#  Symbols
##############################################################################
>>> write("foo")
(&symbol foo)

>>> write("foo bar")
[(&symbol foo) (&symbol bar)]

>>> write("foo:bar")
[(&symbol foo) [(&symbol bar)]]

>>> write(r"foo;bar")
[(&symbol foo) [(&symbol bar)]]

>>> write("foo1")
(&symbol foo1)

>>> write("100foo")
(&symbol 100foo)

##############################################################################
#  Ignore
##############################################################################
>>> write("~")
(&fn &tilde)

>>> write("foo~bar")
[(&symbol foo) (&fn &tilde) (&symbol bar)]

##############################################################################
#  Numbers
##############################################################################
>>> write("100")
(&number 100)

>>> write("100")
(&number 100)

>>> write("100.50")
(&number 100.5)

>>> write(".100")
Traceback (most recent call last):
  ...
SyntaxError: invalid character .

>>> write("100.50.70")
Traceback (most recent call last):
  ...
SyntaxError: invalid character .

##############################################################################
#  Infix Math
##############################################################################
>>> write("+100")
[(&fn &add) (&number 100)]

>>> write("-100")
[(&fn &sub) (&number 100)]


>>> write("+100.50")
[(&fn &add) (&number 100.5)]

>>> write("-100.50")
[(&fn &sub) (&number 100.5)]


>>> write("100 + 200")
[(&fn &add) (&number 100) (&number 200)]

>>> write("100 - 200")
[(&fn &sub) (&number 100) (&number 200)]

>>> write("100 * 200")
[(&fn &mul) (&number 100) (&number 200)]

>>> write("100 / 200")
[(&fn &div) (&number 100) (&number 200)]


>>> write("100 + 200 * 300")
[(&fn &add) (&number 100) [(&fn &mul) (&number 200) (&number 300)]]

>>> write("(100 + 200) * 300")
[(&fn &mul) (&number 300) [(&fn &add) (&number 100) (&number 200)]]

>>> write("100 + 200 - 300")
[(&fn &sub) [(&fn &add) (&number 100) (&number 200)] (&number 300)]

>>> write("100 + (200 - 300)")
[(&fn &add) (&number 100) [(&fn &sub) (&number 200) (&number 300)]]

>>> write("100 * 200 / 300")
[(&fn &div) [(&fn &mul) (&number 100) (&number 200)] (&number 300)]

>>> write("100 * (200 / 300)")
[(&fn &mul) (&number 100) [(&fn &div) (&number 200) (&number 300)]]

##############################################################################
#  Cons
##############################################################################
>>> write("|foo")
[(&vau &apply) (&symbol foo)]

>>> write("(|foo)")
[(&vau &apply) (&symbol foo)]

>>> write("[|foo]")
[(&vau &apply) (&fn &list) (&symbol foo)]


>>> write("|foo    ")
[(&vau &apply) (&symbol foo)]

>>> write("(|foo    )")
[(&vau &apply) (&symbol foo)]

>>> write("[|foo    ]")
[(&vau &apply) (&fn &list) (&symbol foo)]


>>> write("|foo bar qux")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("(|foo bar qux)")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("[|foo bar qux]")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("(foo bar | qux)")
[(&vau &apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("foo bar | qux")
[(&vau &apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("[foo bar | qux]")
[(&vau &apply) (&fn &list) (&symbol foo) (&symbol bar) (&symbol qux)]


>>> write("Test | Args: Orig | Args")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("(Test | Args: Orig | Args)")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("[Test | Args: Orig | Args]")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("Test | Args; Orig | Args")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("(Test | Args; Orig | Args)")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("[Test | Args; Orig | Args]")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("foo: Test | Args; Orig | Args")
[(&symbol foo) [(&vau &apply) (&symbol Test) (&symbol Args)] [(&vau &apply) (&symbol Orig) (&symbol Args)]]

>>> write("(foo: Test | Args; Orig | Args)")
[(&symbol foo) [(&vau &apply) (&symbol Test) (&symbol Args)] [(&vau &apply) (&symbol Orig) (&symbol Args)]]

>>> write("[foo: Test | Args; Orig | Args]")
[(&fn &list) (&symbol foo) [(&vau &apply) (&fn &list) (&symbol Test) (&symbol Args)] [(&vau &apply) (&fn &list) (&symbol Orig) (&symbol Args)]]


>>> write("X | (join R Y)")
[(&vau &apply) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]

>>> write("(X | (join R Y))")
[(&vau &apply) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]

>>> write("[X | (join R Y)]")
[(&vau &apply) (&fn &list) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]


>>> write("X | [join R Y]")
[(&vau &apply) (&symbol X) [(&fn &list) (&symbol join) (&symbol R) (&symbol Y)]]

>>> write("(X | [join R Y])")
[(&vau &apply) (&symbol X) [(&fn &list) (&symbol join) (&symbol R) (&symbol Y)]]

>>> write("[X | [join R Y]]")
[(&vau &apply) (&fn &list) (&symbol X) [(&fn &list) (&symbol join) (&symbol R) (&symbol Y)]]

##############################################################################
#  Colon
##############################################################################
>>> write(
... r'''
... foo: bar
... qux
... ''')
[(&symbol foo) [(&symbol bar)]]


>>> write("(foo: bar: qux)")
[(&symbol foo) [(&symbol bar) [(&symbol qux)]]]

>>> write("(foo: bar | qux)")
[(&symbol foo) [(&vau &apply) (&symbol bar) (&symbol qux)]]

>>> write("(foo: bar; qux)")
[(&symbol foo) [(&symbol bar)] [(&symbol qux)]]


>>> write("[foo: bar: qux]")
[(&fn &list) (&symbol foo) [(&fn &list) (&symbol bar) [(&fn &list) (&symbol qux)]]]

>>> write("[foo: bar | qux]")
[(&fn &list) (&symbol foo) [(&vau &apply) (&fn &list) (&symbol bar) (&symbol qux)]]

>>> write("[foo: bar; qux]")
[(&fn &list) (&symbol foo) [(&fn &list) (&symbol bar)] [(&fn &list) (&symbol qux)]]


>>> write(
... r'''
... (foo:
...   bar:
...     qux)
... ''')
[(&symbol foo) [(&symbol bar) [(&symbol qux)]]]


>>> write(
... r'''
... $let: F: $fn Args: foo
...            bar
...            qux
...   corge
... ''')
[(&symbol $let) [(&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol foo)] (&symbol bar) (&symbol qux)]] (&symbol corge)]


>>> write(
... r'''
... foo bar;qux ->
...  $let:Orig:eval Name
...       Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar) [(&vau &arrow) [(&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)]] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]]

>>> write(
... r'''
... foo bar;qux ->
...  $let:Orig:eval Name
...      Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar) [(&vau &arrow) [(&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)]] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]]

>>> write(
... r'''
... foo bar;qux ->
...  $let;Orig:eval Name
...      Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar) [(&vau &arrow) [(&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]]]


>>> write(
... r'''
... any: zip Fns Args; [X Y] ->
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]

>>> write(
... r'''
... any (zip Fns Args); [X Y] ->
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]

>>> write(
... r'''
... any; [X Y] -> zip: Fns Args
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&symbol zip) [(&symbol Fns) (&symbol Args)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]

[(&symbol any) [(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&symbol zip) [(&symbol Fns) (&symbol Args)] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]]


>>> write(": X -> : $let | R; Y")
[[(&vau &arrow) [(&symbol X)] [(&vau &apply) (&symbol $let) (&symbol R)]] [(&symbol Y)]]

>>> write("((X -> : $let | R) Y)")
[[(&vau &arrow) [(&symbol X)] [(&vau &apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("[[X -> : $let | R] Y]")
[(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]


>>> write(": $fn: X; $let | R; Y")
[[(&symbol $fn) [(&symbol X)] [(&vau &apply) (&symbol $let) (&symbol R)]] (&symbol Y)]u

>>> write("(($fn (X): $let | R) Y)")
[[(&symbol $fn) [(&symbol X)] [(&vau &apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("[[$fn [X]: $let | R] Y]")
[(&fn &list) [(&fn &list) (&symbol $fn) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]

##############################################################################
#  Semicolon
##############################################################################
>>> write(
... r'''
... foo; bar
... qux
... ''')
[(&symbol foo) [(&symbol bar)]]


>>> write(
... r'''
... $def! foo; X ->
...  bar
... ''')
[(&symbol $def!) (&symbol foo) [(&vau &arrow) [(&symbol X)] (&symbol bar)]]

>>> write(
... r'''
... $def! foo; X ->
...  bar
...   qux
... ''')
[(&symbol $def!) (&symbol foo) [(&vau &arrow) [(&symbol X)] [(&symbol bar) (&symbol qux)]]]

>>> write(
... r'''
... $def! foo; X ->
...  bar
...   qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) [(&vau &arrow) [(&symbol X)] [(&symbol bar) (&symbol qux)] (&symbol corge)]]


>>> write(
... r'''
... $def!
...  foo
...  bar
...  qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo
...  bar
...  qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo bar
...  qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo bar qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo bar qux corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo;
...  bar
...  qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) (&symbol qux) (&symbol corge)]]

>>> write(
... r'''
... $def! foo;bar;
...  qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) [(&symbol qux) (&symbol corge)]]]

>>> write(
... r'''
... $def! foo;bar;qux;
...  corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) [(&symbol qux) [(&symbol corge)]]]]

>>> write(
... r'''
... $def! foo;bar;qux;corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) [(&symbol qux) [(&symbol corge)]]]]


>>> write(
... r'''
... $def! foo;bar
...           qux
...           corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar)] (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! ; foo bar
...   qux
...   corge
... ''')
[(&symbol $def!) [(&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]]

>>> write(
... r'''
... $def! foo bar
...           qux
...           corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo;bar
...          qux
...           corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) (&symbol qux)] (&symbol corge)]

>>> write(
... r'''
... $def! foo;bar qux;corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) (&symbol qux) [(&symbol corge)]]]

>>> write(
... r'''
... $def! foo
...  bar qux
...   corge
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol bar) (&symbol qux) (&symbol corge)]]


>>> write(
... r'''
... $let F: $fn Args: $if-error: Test | Args; Orig Args
...  eval foo
... ''')
[(&symbol $let) (&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol $if-error) [(&vau &apply) (&symbol Test) (&symbol Args)]]] [(&symbol Orig) (&symbol Args) [(&symbol eval) (&symbol foo)]]]

>>> write(
... r'''
... $let F: $fn Args: $if-error: Test | Args; Orig | Args
...  eval foo
... ''')
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |u

##############################################################################
#  Arrow
##############################################################################
>>> write("foo->bar")
[(&vau arrow) [(&symbol foo)] (&symbol bar)]

>>> write("foo -> foo")
[(&vau &arrow) [(&symbol foo)] (&symbol foo)]

>>> write("(foo -> foo)")
[(&vau &arrow) [(&symbol foo)] (&symbol foo)]

>>> write("[foo -> foo]")
[(&fn &list) (&vau &arrow) [(&fn &list) (&symbol foo)] (&symbol foo)]


>>> write("foo | bar -> foo")
[(&vau &arrow) [(&vau &apply) (&symbol foo) (&symbol bar)] (&symbol foo)]

>>> write("(foo | bar -> foo)")
[(&vau &arrow) [(&vau &apply) (&symbol foo) (&symbol bar)] (&symbol foo)]

>>> write("[foo | bar -> foo]")
[(&fn &list) (&vau &arrow) [(&vau &apply) (&fn &list) (&symbol foo) (&symbol bar)] (&symbol foo)]


>>> write("|foo -> foo")
[(&vau &arrow) [(&vau &apply) (&symbol foo)] (&symbol foo)]

>>> write("(|foo -> foo)")
[(&vau &arrow) [(&vau &apply) (&symbol foo)] (&symbol foo)]

>>> write("[|foo -> foo]")
[(&fn &list) (&vau &arrow) [(&vau &apply) (&fn &list) (&symbol foo)] (&symbol foo)]


>>> write("(foo -1 foo)")
u


>>> write(
... r'''
... x y ->
...  foo
...  bar
... ''')
[(&vau &arrow) [(&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write(
... r'''
... x y -> foo
...        bar
... ''')
[(&vau &arrow) [(&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write("(x y -> foo bar)")
[(&vau &arrow) [(&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write("[x y -> foo bar]")
[(&fn &list) (&vau &arrow) [(&fn &list) (&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]


>>> write("x y -> foo bar")
[(&vau &arrow) [(&symbol x) (&symbol y)] [(&symbol foo) (&symbol bar)]]

>>> write("(x y -> (foo bar))")
[(&vau &arrow) [(&symbol x) (&symbol y)] [(&symbol foo) (&symbol bar)]]

>>> write("[x y -> [foo bar]]")
[(&fn &list) (&vau &arrow) [(&fn &list) (&symbol x) (&symbol y)] [(&fn &list) (&symbol foo) (&symbol bar)]]


>>> write("|foo -> foo bar")
[(&vau &arrow) [(&vau &apply) (&symbol foo)] [(&symbol foo) (&symbol bar)]]

>>> write("(|foo -> (foo bar))")
[(&vau &arrow) [(&vau &apply) (&symbol foo)] [(&symbol foo) (&symbol bar)]]

>>> write("[|foo -> [foo bar]]")
[(&fn &list) (&vau &arrow) [(&vau &apply) (&fn &list) (&symbol foo)] [(&fn &list) (&symbol foo) (&symbol bar)]]


>>> write("|foo bar -> foo bar")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("(|foo bar -> foo bar)")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |

>>> write("[|foo bar -> foo bar]")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("(X Y -> Y)")
[(&vau &arrow) [(&symbol X) (&symbol Y)] (&symbol Y)]

>>> write(
... r'''
... (X Y ->
... (-> X)
...   (-> Y))
... ''')
[(&vau &arrow) [(&symbol X) (&symbol Y)] [(&vau &arrow) [] (&symbol X)] [(&vau &arrow) [] (&symbol Y)]]

>>> write("(X Y -> -> X -> Y)")
[(&vau &arrow) [(&vau &arrow) [(&vau &arrow) [(&symbol X) (&symbol Y)]] (&symbol X)] (&symbol Y)]

>>> write(
... r'''
... (X Y ->
...   -> X
...   -> Y)
... ''')
[(&vau &arrow) [(&vau &arrow) [(&vau &arrow) [(&symbol X) (&symbol Y)]] (&symbol X)] (&symbol Y)]


>>> write(
... r'''
... (: [X Y] -> [[X -> [$let | R]] Y]; X)
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]] [(&symbol X)]]


>>> write(
... r'''
... (([X Y] -> [[X -> [$let | R]] Y])
...   X)
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]

>>> write(
... r'''
... [X Y] -> [[X -> [$let | R]] Y]
...  X
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [X Y] -> [[X -> [$let | R]] Y]
... X
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]]

>>> write(
... r'''
... :[X Y] -> [[X -> [$let | R]] Y]
...  X
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]

>>> write(
... r'''
... :[X Y] -> [[X -> [$let | R]] Y]
...   X
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [[[X Y] -> [[X -> [$let | R]] Y]]
...   X]
... ''')
[(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) [(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]


>>> write(
... r'''
... ([X Y] -> [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... ([X Y] ->
...   [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [X Y] ->
...  [[X -> [$let | R]] Y]
...  X
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [[X Y] -> [[X -> [$let | R]] Y] X]
... ''')
[(&fn &list) (&vau &arrow) [(&fn &list) [(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&vau &apply) (&fn &list) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

##############################################################################
#  Whitespace indentation
##############################################################################
>>> read(
... r'''
... foo
...  bar
...  qux
...  corge
... ''')
(foo bar qux corge)

>>> read(
... r'''
... foo
...  bar
...  qux
...   corge
... ''')
(foo bar (qux corge))

>>> read(
... r'''
... foo
...  bar
...   qux
...    corge
... ''')
(foo (bar (qux corge)))

>>> read(
... r'''
... foo bar
...  qux
...  corge
... ''')
(foo bar qux corge)

>>> read(
... r'''
... foo bar
...  qux
...  corge
... ''')
(foo bar qux corge)

>>> read(
... r'''
... foo bar
...  qux
...   corge
... ''')
(foo bar (qux corge))

# TODO: should this throw an error?
>>> read(
... r'''
... foo bar
...   qux
...  corge
... ''')
(foo bar qux corge)

##############################################################################
#  Code Snippets
##############################################################################
>>> write(
... r'''
... $def! type; $fn Fns
...  $fn Args
...   any: zip Fns Args; [X Y] ->
...    $if: not: X Y
...     error 'type check failed on argument @Y'
... ''')
[(&symbol $def!) (&symbol type) [(&symbol $fn) (&symbol Fns) [(&symbol $fn) (&symbol Args) [(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) [(&fn &string) (&char t) (&char y) (&char p) (&char e) (&char  ) (&char c) (&char h) (&char e) (&char c) (&char k) (&char  ) (&char f) (&char a) (&char i) (&char l) (&char e) (&char d) (&char  ) (&char o) (&char n) (&char  ) (&char a) (&char r) (&char g) (&char u) (&char m) (&char e) (&char n) (&char t) (&char  ) (&symbol Y)]]]]]]]]


>>> write(
... r'''
... $def! foo: type list? any? ; X -> X
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol type) (&symbol list?) (&symbol any?)] [(&vau &arrow) [(&symbol X)] (&symbol X)]]


>>> write(
... r'''
... $def! foo: X -> X
... ''')
[(&symbol $def!) (&symbol foo) [(&vau &arrow) [(&symbol X)] (&symbol X)]]


>>> write(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let; Orig:  eval Name
...         Test:  eval Test
...         F:     $fn Args: $if-error: Test | Args
...                            Orig | Args
...     eval [$def! Name F | Fns]
... ''')
u


>>> write(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let: Orig: eval Name
...         Test: eval Test
...     $let F: $fn Args: $if-error (Test | Args): Orig | Args
...       eval [$def! Name F | Fns]
... ''')
[(&symbol $defvau!) (&symbol $def-if!) [(&vau &arrow) [(&symbol Name) (&symbol Test) | (&symbol Fns)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)] [(&symbol Test) [(&symbol eval) (&symbol Test)]]] [(&symbol $let) (&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol $if-error) [(&symbol Test) | (&symbol Args)] [(&symbol Orig) | (&symbol Args)]]] [(&symbol eval) [(&fn &list) (&symbol $def!) (&symbol Name) (&symbol F) | (&symbol Fns)]]]]]]


>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream
...            -> nil? X
...            -> car X
...            -> $set-in! Env X: cdr X
... ''')
[(&symbol $def-if!) (&symbol stream) [(&symbol type) (&symbol list?)] [(&vau &arrow) [(&symbol X)] [(&symbol $let) (&symbol Env) [(&symbol current-env)] [(&symbol make-stream) [(&vau &arrow) [] [(&symbol nil?) (&symbol X)]] [(&vau &arrow) [] [(&symbol car) (&symbol X)]] [(&vau &arrow) [] [(&symbol $set-in!) (&symbol Env) (&symbol X) [(&symbol cdr) (&symbol X)]]]]]]]


>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream; -> nil? X
...                       -> car X
...                       -> $set-in! Env X: cdr X
... ''')
[(&symbol $def-if!) (&symbol stream) [(&symbol type) (&symbol list?)] [(&vau &arrow) [(&symbol X)] [(&symbol $let) (&symbol Env) [(&symbol current-env)] [(&symbol make-stream) [(&vau &arrow) [] [(&symbol nil?) (&symbol X)]] [(&vau &arrow) [] [(&symbol car) (&symbol X)]] [(&vau &arrow) [] [(&symbol $set-in!) (&symbol Env) (&symbol X) [(&symbol cdr) (&symbol X)]]]]]]]


>>> write(
... r'''
... $def! join: type list? any?
...   [X | R] Y -> [X | (join R Y)]
...   [X]     Y -> [X | Y]
... ''')
[(&symbol $def!) (&symbol join) [(&symbol type) (&symbol list?) (&symbol any?)] [(&vau &arrow) [[(&vau &apply) (&fn &list) (&symbol X) (&symbol R)] (&symbol Y)] [(&vau &apply) (&fn &list) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]] [(&vau &arrow) [[(&fn &list) (&symbol X)] (&symbol Y)] [(&vau &apply) (&fn &list) (&symbol X) (&symbol Y)]]]


>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau &arrow) [[(&vau &apply) (&fn &list) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] (&symbol each) (&symbol R) (&symbol F)]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) (each R F)
... ''')
[(&symbol $def!) (&symbol each) [(&vau &arrow) [[(&vau &apply) (&fn &list) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X): each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau &arrow) [[(&vau &apply) (&fn &list) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> : F X ; each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau &arrow) [[(&vau &apply) (&fn &list) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]
