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
...       `fo@o\\bar`n\qux`
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

>>> write("foo->bar")
(&symbol foo->bar)

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

>>> write("100.50.70")
[(&number 100.5) (&symbol .70)]

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
>>> write("(|foo)")
(&symbol foo)

>>> write("(|foo bar qux)")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("|foo")
(&symbol foo)

>>> write("|foo bar qux")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |u


>>> write("[|foo]")
[(&fn &list) | (&symbol foo)]

>>> write("[|foo bar qux]")
Traceback (most recent call last):
  ...
SyntaxError: illegal use of |


>>> write("(foo bar | qux)")
[(&symbol foo) (&symbol bar) | (&symbol qux)]

>>> write("foo bar | qux")
[(&symbol foo) (&symbol bar) | (&symbol qux)]

>>> write("[foo bar | qux]")
[(&fn &list) (&symbol foo) (&symbol bar) | (&symbol qux)]

##############################################################################
#  Colon
##############################################################################
>>> write("(foo: bar: qux)")
[(&symbol foo) [(&symbol bar) [(&symbol qux)]]]

>>> write("(foo: bar | qux)")
[(&symbol foo) [(&symbol bar) | (&symbol qux)]]

>>> write("(foo: bar; qux)")
[(&symbol foo) [(&symbol bar)] [(&symbol qux)]]


>>> write("[foo: bar: qux]")
[(&fn &list) (&symbol foo) [(&fn &list) (&symbol bar) [(&fn &list) (&symbol qux)]]]

>>> write("[foo: bar | qux]")
[(&fn &list) (&symbol foo) [(&fn &list) (&symbol bar) | (&symbol qux)]]

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
... foo bar; qux ->
...   $let: Orig: eval Name
...         Test: eval Test
...     $let F: corge
...       eval F
... ''')
u


>>> write(
... r'''
... any: zip Fns Args; [X Y] ->
...   $if: not: X Y
...     error foo
... ''')
u

>>> write(
... r'''
... any (zip Fns Args) ; [X Y] ->
...   $if: not: X Y
...     error foo
... ''')
u

>>> write(
... r'''
... any; [X Y] -> zip: Fns Args
...   $if: not: X Y
...     error foo
... ''')
u
(any (zip Fns Args ([X Y] ->)
  ($if (not (X Y))
    (error foo))))

##############################################################################
#  Backslash
##############################################################################
>>> write(
... r'''
... $def! foo; X ->
...   bar
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&arrow') [(&symbol 'X')] (&symbol 'bar')]]

>>> write(
... r'''
... $def! foo; X ->
...   bar
...     qux
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol 'bar') (&symbol 'qux')]]]

>>> write(
... r'''
... $def! foo; X ->
...   bar
...     qux
...   corge
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol 'bar') (&symbol 'qux')] (&symbol 'corge')]]


>>> write(
... r'''
... $def! foo; foo
...   bar
...   qux
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&backslash') (&symbol 'foo') (&symbol 'bar') (&symbol 'qux')]]

>>> write(
... r'''
... $def! foo; foo
...            bar
...            qux
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&backslash') (&symbol 'foo')] (&symbol 'bar') (&symbol 'qux')]

##############################################################################
#  Arrow
##############################################################################
>>> write("foo -> foo")
[(&vau &arrow) [(&symbol foo)] (&symbol foo)]u

>>> write("(foo -> foo)")
[(&vau &arrow) [(&symbol foo)] (&symbol foo)]

>>> write("[foo -> foo]")
[(&fn &list) (&vau &arrow) [(&fn &list) (&symbol foo)] (&symbol foo)]


>>> write("foo | bar -> foo")
[(&vau &arrow) [(&symbol foo) | (&symbol bar)] (&symbol foo)]u

>>> write("(foo | bar -> foo)")
[(&vau &arrow) [(&symbol foo) | (&symbol bar)] (&symbol foo)]

>>> write("[foo | bar -> foo]")
[(&fn &list) (&vau &arrow) [(&fn &list) (&symbol foo) | (&symbol bar)] (&symbol foo)]


>>> write("|foo -> foo")
[(&vau &arrow) (&symbol foo) (&symbol foo)]u

>>> write("(|foo -> foo)")
[(&vau &arrow) (&symbol foo) (&symbol foo)]

>>> write("[|foo -> foo]")
[(&fn &list) (&vau &arrow) [(&fn &list) | (&symbol foo)] (&symbol foo)]


>>> write(
... r'''
... x y ->
...   foo
...   bar
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
[(&vau &arrow) (&symbol foo) [(&symbol foo) (&symbol bar)]]

>>> write("(|foo -> (foo bar))")
[(&vau &arrow) (&symbol foo) [(&symbol foo) (&symbol bar)]]

>>> write("[|foo -> [foo bar]]")
[(&fn &list) (&vau &arrow) [(&fn &list) | (&symbol foo)] [(&fn &list) (&symbol foo) (&symbol bar)]]


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


>>> write(
... r'''
... (X Y -> Y)
... ''')
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
... (: [X Y] -> [[X -> [$let | R]] Y] ; X)
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)]] [(&symbol X)]]


>>> write(
... r'''
... (([X Y] -> [[X -> [$let | R]] Y])
...   X)
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)]] (&symbol X)]

>>> write(
... r'''
... [X Y] -> [[X -> [$let | R]] Y]
...   X
... ''')
[[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)]] [(&symbol X)]]

>>> write(
... r'''
... [[[X Y] -> [[X -> [$let | R]] Y]]
...   X]
... ''')
[(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) [(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)]] (&symbol X)]


>>> write(
... r'''
... ([X Y] -> [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... ([X Y] ->
...   [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [X Y] ->
...   [[X -> [$let | R]] Y]
...   X
... ''')
[(&vau &arrow) [[(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [[X Y] -> [[X -> [$let | R]] Y] X]
... ''')
[(&fn &list) (&vau &arrow) [(&fn &list) [(&fn &list) (&symbol X) (&symbol Y)]] [(&fn &list) [(&fn &list) (&vau &arrow) [(&fn &list) (&symbol X)] [(&fn &list) (&symbol $let) | (&symbol R)]] (&symbol Y)] (&symbol X)]

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
...    corge
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
>>> read(
... r'''
... $def! type; $fn Fns
...   $fn Args
...     any: zip Fns Args; [X Y] ->
...       $if: not: X Y
...         error 'type check failed on argument @Y'
... ''')
($def! type
  ($fn Fns
    ($fn Args
      (any (zip Fns Args)
        ([X Y] ->
          ($if (not (X Y))
            (error 'type check failed on argument @Y')))))))


>>> read(
... r'''
... $def! foo: type list? any? ; X -> X
... ''')
($def! foo (type list? any?)
  (X -> X))

>>> write(
... r'''
... $def! foo: type list? any? ; X -> X
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol type) (&symbol list?) (&symbol any?)] [(&vau &arrow) [(&symbol X)] (&symbol X)]]


>>> write(
... r'''
... $def! foo: X -> X
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'foo') [(&symbol '&arrow') [(&symbol 'X')] (&symbol 'X')]]


>>> read(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let: Orig: eval Name
...         Test: eval Test
...     $let F: $fn Args: $if-error: Test | Args ; Orig | Args
...       eval [$def! Name F | Fns]
... ''')
($defvau! $def-if!
  (Name Test | Fns ->
    ($let (Orig (eval Name)
           Test (eval Test))
      ($let F ($fn Args ($if-error (Test | Args)
                          (Orig | Args)))
        (eval [$def! Name F | Fns])))))

>>> write(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let: Orig: eval Name
...         Test: eval Test
...     $let F: $fn Args: $if-error: Test | Args ; Orig | Args
...       eval [$def! Name F | Fns]
... ''')
u


>>> write(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let; Orig:  eval Name
...         Test:  eval Test
...         F:     $fn Args: $if-error: Test | Args
...                            Orig | Args
...     eval [$def! Name F | Fns]
... ''')
[(&symbol '&round-brackets') (&symbol '$defvau!') (&symbol '$def-if!') [(&symbol '&arrow') [(&symbol 'Name') (&symbol 'Test') [(&symbol '&bar') (&symbol 'Fns')]] [(&symbol '&round-brackets') (&symbol '$lets') [(&symbol '&backslash') (&symbol 'Orig') [(&symbol '&colon') (&symbol 'eval') (&symbol 'Name')]] [(&symbol '&round-brackets') (&symbol 'Test') [(&symbol '&colon') (&symbol 'eval') (&symbol 'Test')]] [(&symbol '&round-brackets') (&symbol 'F') [(&symbol '&colon') (&symbol '$fn') (&symbol 'Args') [(&symbol '&colon') (&symbol '$if-error') [(&symbol '&colon') (&symbol 'Test') [(&symbol '&bar') (&symbol 'Args')]] [(&symbol '&round-brackets') (&symbol 'Orig') [(&symbol '&bar') (&symbol 'Args')]]]]] [(&symbol '&round-brackets') (&symbol 'eval') [(&symbol '&square-brackets') (&symbol '$def!') (&symbol 'Name') (&symbol 'F') [(&symbol '&bar') (&symbol 'Fns')]]]]]]


>>> read(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let: Orig: eval Name
...         Test: eval Test
...     $let F: $fn Args: $if-error (Test | Args) : Orig | Args
...       eval [$def! Name F | Fns]
... ''')
($defvau! $def-if!
  (Name Test | Fns ->
    ($let (Orig (eval Name)
           Test (eval Test))
      ($let F ($fn Args ($if-error (Test | Args) (Orig | Args)))
        (eval [$def! Name F | Fns])))))

>>> write(
... r'''
... $defvau! $def-if! ; Name Test | Fns ->
...   $let (Orig: eval Name
...         Test: eval Test)
...     $let F: $fn Args: $if-error (Test | Args) : Orig | Args
...       eval [$def! Name F | Fns]
... ''')
[&round-brackets $defvau! $def-if! [&arrow [Name Test [&bar Fns]] [&round-brackets $let [&round-brackets Orig [&colon eval Name] Test [&colon eval Test]] [&round-brackets $let F [&colon $fn Args [&colon $if-error [&round-brackets Test [&bar Args]] [&colon Orig [&bar Args]]]] [&round-brackets eval [&square-brackets $def! Name F [&bar Fns]]]]]]]


>>> read(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream
...            -> nil? X
...            -> car X
...            -> $set-in! Env X: cdr X
... ''')
($def-if! stream (type list?)
  (X -> ($let Env (current-env)
          (make-stream
            (-> (nil? X))
            (-> (car X))
            (-> ($set-in! Env X (cdr X)))))))

>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream
...            -> nil? X
...            -> car X
...            -> $set-in! Env X: cdr X
... ''')
[(&symbol '&round-brackets') (&symbol '$def-if!') (&symbol 'stream') [(&symbol '&colon') (&symbol 'type') (&symbol 'list?')] [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol '$let') (&symbol 'Env') [(&symbol '&colon') (&symbol 'current-env')] [(&symbol '&round-brackets') (&symbol 'make-stream') [(&symbol '&arrow') [] [(&symbol '&round-brackets') (&symbol 'nil?') (&symbol 'X')]] [(&symbol '&arrow') [] [(&symbol '&round-brackets') (&symbol 'car') (&symbol 'X')]] [(&symbol '&arrow') [] [(&symbol '&round-brackets') (&symbol '$set-in!') (&symbol 'Env') (&symbol 'X') [(&symbol '&colon') (&symbol 'cdr') (&symbol 'X')]]]]]]]


>>> read(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream; X -> nil? X
...                       X -> car X
...                       X -> $set-in! Env X: cdr X
... ''')
($def-if! stream (type list?)
  (X -> ($let Env (current-env)
          (make-stream
            (-> (nil? X))
            (-> (car X))
            (-> ($set-in! Env X (cdr X)))))))

>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream; X -> nil? X
...                       X -> car X
...                       X -> $set-in! Env X: cdr X
... ''')
[(&symbol '&round-brackets') (&symbol '$def-if!') (&symbol 'stream') [(&symbol '&colon') (&symbol 'type') (&symbol 'list?')] [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol '$let') (&symbol 'Env') [(&symbol '&colon') (&symbol 'current-env')] [(&symbol '&round-brackets') (&symbol 'make-stream') [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol 'nil?') (&symbol 'X')]] [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol 'car') (&symbol 'X')]] [(&symbol '&arrow') [(&symbol 'X')] [(&symbol '&round-brackets') (&symbol '$set-in!') (&symbol 'Env') (&symbol 'X') [(&symbol '&colon') (&symbol 'cdr') (&symbol 'X')]]]]]]]


>>> read(
... r'''
... $def! join: type list? any?
...   [X | R] Y -> [X | (pair R Y)]
...   [X]     Y -> [X | Y]
... ''')
($def! join (type list? any?)
  ([X | R] Y -> [X | (pair R Y)])
  ([X] Y -> [X | Y]))

>>> write(
... r'''
... $def! join: type list? any?
...   [X | R] Y -> [X | (pair R Y)]
...   [X]     Y -> [X | Y]
... ''')
[(&symbol '&round-brackets') (&symbol '$def!') (&symbol 'join') [(&symbol '&colon') (&symbol 'type') (&symbol 'list?') (&symbol 'any?')] [(&symbol '&arrow') [[(&symbol '&square-brackets') (&symbol 'X') [(&symbol '&bar') (&symbol 'R')]] (&symbol 'Y')] [(&symbol '&square-brackets') (&symbol 'X') [(&symbol '&bar') [(&symbol '&round-brackets') (&symbol 'pair') (&symbol 'R') (&symbol 'Y')]]]] [(&symbol '&arrow') [[(&symbol '&square-brackets') (&symbol 'X')] (&symbol 'Y')] [(&symbol '&square-brackets') (&symbol 'X') [(&symbol '&bar') (&symbol 'Y')]]]]


>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) (each R F)
... ''')
u

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) : each R F
... ''')
u
