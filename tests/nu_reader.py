>>> import nu_reader
>>> import nu_types

>>> def read(x):
...   print nu_reader.readstring(x, 9001)

>>> def write(x):
...   print repr(nu_reader.readstring(x, 9001))

>>> def write_all(x):
...   result = []
...   x = nu_types.w_Stream(iter(x).next)
...   while 1:
...     y = nu_reader.read(x, 9001)
...     if y == 9001:
...       break
...     else:
...       result.append(y)
...   for x in result:
...     print repr(x)

>>> def error(f, x):
...   try:
...     return f(x)
...   except nu_types.w_BaseError as e:
...     print e

##############################################################################
#  nu.nu
##############################################################################
>>> for x in nu_reader.read_file("nu.nu"): print x.pretty()
($assign $let ($vau Env [X Y | R] (eval Env [[$fn [seq X] | R] Y])))
($assign $quote ($vau (&fn &tilde) [X] X))
($assign $or ($vau Env [X | R] ($let X (eval Env X) ($if X X (eval Env [$or | R])))))
($assign any? ($fn [[X | R] F] ($or (F X) (any? R F))))
($assign case ($fn [X | Fns] (any? Fns ($fn [F] (F | X)))))
($assign case-vau ($vau Env Fns ($let Args (uniq) (eval Env [$vau ($quote %Env) Args [case Args | Fns]]))))
($assign case-fn ($fn Fns ($fn Args (case Args | Fns))))
($assign $defvau ($vau Env [Name | Fns] (eval Env [$assign Name [case-vau | Fns]])))
($assign $def ($vau Env [Name | Fns] (eval Env [$assign Name [case-fn | Fns]])))
($def get-current-env (wrap ($vau Env [] Env)))
($def make-env ($fn [Env] (eval Env [[$vau (&fn &tilde) (&fn &tilde) [get-current-env]]])))
($def make-base-env)
($def $use ($vau Env Args (each Args ($fn [X] ($let New (make-base-env) ($hook Env get-if-unbound ($fn [K] (eval New K))) (load-file-in New (find-file X)) (eval Env [$def (strip-extension (basename X)) New]))))))
($def eachf ($fn [[X | R] F] (F X) (each R F)) ($fn [(&fn &tilde) (&fn &tilde)] %f))
($def not ($fn [X] ($if X %f %t)))
($defvau do ($fn Args (eval [[$fn [] | Args]])))
($defvau $and ($fn [X] (eval X)) ($fn [X | R] ($if (eval X) (eval [$and | R]))))
($def all? ($fn [[X] F] (F X)) ($fn [[X | R] F] ($and (F X) (all? R F))))
($def none? ($fn [X F] (all? X ($fn [X] (not (F X))))))
($def fn-fn ($fn [F] ($fn Fns ($fn Args (F Fns ($fn [X] (X | Args)))))))
($def fn-not (fn-fn none?))
($def fn-and (fn-fn all?))
($def fn-or (fn-fn any?))
($def sum ($fn [[X | R] F] (sum1 X R F)) ($fn [I X F] (sum1 I X F)))
($def sum1 ($fn [I [X | R] F] (sum1 (F I X) R F)) ($fn [I (&fn &tilde) (&fn &tilde)] I))
($def sumr ($fn [[X | R] F] (F X (sumr R F))) ($fn [X (&fn &tilde)] X) ($fn [X I F] (sumr1 X I F)))
($def sumr1 ($fn [[X | R] I F] (F X (sumr R I F))) ($fn [(&fn &tilde) I (&fn &tilde)] I))
($def compose ($fn Fns (sum Fns ($fn [X Y] ($fn Args (X (Y | Args)))))))
($def each ($fn [X F] (sumr X [] ($fn [X Y] [(F X) | Y]))))
($def rev ($fn [X] (sum [] X ($fn [Y X] [X | Y]))))
($def keep ($fn [X F] (sumr X [] ($fn [X Y] ($if (F X) [X | Y] Y)))))
($def rem ($fn [X F] (keep X (fn-not F))))
($def empty? ($fn [[]] %t))
($def first ($fn [[X | (&fn &tilde)]] X))
($def rest ($fn [[(&fn &tilde) | R]] R))
($def zip ($fn Args ($if (any? Args empty?) [] [(each Args first) | (zip | (each Args rest))])))
($def join ($fn [[X | R] Y] [X | (join R Y)]) ($fn [[X] Y] [X | Y]))
($def joinr ($fn [X Y] (join X [Y])))
($def ref ($fn [[K V | R] K] V) ($fn [[(&fn &tilde) (&fn &tilde) | R] K] (ref R K)))
($def iso? ($fn [X X] %t) ($fn [[X | R1] [Y | R2]] ($and (iso? X Y) (iso? R1 R2))))
($def id ($fn [X] X))
($def copy ($fn [X] (each X id)))
($def prn! ($fn Args (pr! | Args) (pr! "\n")))
($def writen! ($fn Args (write! | Args) (pr! "\n")))
($defvau $lets ($fn [[X]] (eval X)) ($fn [[[X Y] | R]] (eval [$let X Y [$lets | R]])))
($defvau $if-error ($fn [[X Y | R]] ($let U (uniq) (eval [$on-error X [$fn [seq [error (&fn &tilde)]] Y] | (joinr R [$fn [seq U] U])]))))
($defvau $def-if! ($fn [Name Test | Fns] ($lets (Orig (eval Name)) (Test (eval Test)) (F ($fn Args ($if-error (Test | Args) (Orig | Args)))) (eval [$assign! Name [case-fn F | Fns]]))))


##############################################################################
#  Strings
##############################################################################
>>> error(read, r'"foo\bar\n\qux"')
error: unknown escape sequence b (line 1, column 6):
  'foo\b
       ^

>>> read(r"'foo\\\@\tbar'")
'foo\\\@\tbar'

>>> error(read, r"'foo\uA00Gbar'")
error: G is not valid hexadecimal (line 1, column 10):
  'foo\uA00G
           ^

>>> error(read, r"'foo\ua00fbar'")
error: a is not valid hexadecimal (line 1, column 7):
  'foo\ua
        ^

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
[(&fn str) (&char f) (&char o) (&char o) (&symbol bar) (&char \n) (&char q) (&char u) (&char x)]

>>> write("'foo@barqux'")
[(&fn str) (&char f) (&char o) (&char o) (&symbol barqux)]

>>> write("'foo@'@'bar''qux'")
[(&fn str) (&char f) (&char o) (&char o) [(&fn str) [(&fn str) (&char b) (&char a) (&char r)]] (&char q) (&char u) (&char x)]

>>> write("'foo@(id bar)qux'")
[(&fn str) (&char f) (&char o) (&char o) [(&symbol id) (&symbol bar)] (&char q) (&char u) (&char x)]

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

>>> error(write_all,
...       r'''
...       `fo@o\\bar`n;qux`
...       ''')
error: missing ending ` quote (line 3, column 6)

>>> error(read,
...       r'''
...       `fo@o\\bar`n;qux`
...       ''')
['fo\@o\\\\bar' n]

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

>>> write(r'''
... #foo
... bar qux corge
... ''')
[(&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(r'''
... #|foo
... |#bar qux corge
... ''')
[(&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(r'''
...       #|
...         #|
...           foo
...         |#
...       bar
...       |#qux
...       ''')
(&symbol qux)

>>> read(r'''
...       #|
...         #|
...           foo
...         |#
...       bar
...       |#
...       ''')
9001

>>> write(r'''
...       #foobar
...       qux
...       ''')
(&symbol qux)

>>> write(r'''
...       #
...       qux
...       ''')
(&symbol qux)

>>> read("#foobar")
9001


>>> write("'foo@#||#qux'")
[(&fn str) (&char f) (&char o) (&char o) (&symbol qux)]


>>> write("foo bar #||# qux")
[(&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("(foo bar #||# qux)")
[(&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("[foo bar #||# qux]")
[(&fn seq) (&symbol foo) (&symbol bar) (&symbol qux)]


>>> write("foo bar | #||# qux")
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("(foo bar | #||# qux)")
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("[foo bar | #||# qux]")
[(&fn apply) (&fn seq) (&symbol foo) (&symbol bar) (&symbol qux)]

##############################################################################
#  Symbols
##############################################################################
>>> write("foo")
(&symbol foo)

>>> write("foo bar")
[(&symbol foo) (&symbol bar)]

>>> write("foo:bar")
[(&symbol foo) (&symbol bar)]

>>> write_all(r"foo;bar")
(&symbol foo)
(&symbol bar)

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

>>> error(write, ".100")
error: invalid character . (line 1, column 1):
  .
  ^

>>> error(write, "100.50.70")
error: invalid character . (line 1, column 7):
  100.50.
        ^

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
#  Bar
##############################################################################
>>> write("|foo")
[(&fn apply) (&symbol foo)]

>>> write("(|foo)")
[(&fn apply) (&symbol foo)]

>>> write("[|foo]")
[(&fn apply) (&fn seq) (&symbol foo)]


>>> write("|foo    ")
[(&fn apply) (&symbol foo)]

>>> write("(|foo    )")
[(&fn apply) (&symbol foo)]

>>> write("[|foo    ]")
[(&fn apply) (&fn seq) (&symbol foo)]


>>> error(write, "|foo bar qux")
error: illegal use of | (line 1, column 6):
  |foo b
       ^

>>> error(write, "(|foo bar qux)")
error: illegal use of | (line 1, column 7):
  (|foo b
        ^

>>> error(write, "[|foo bar qux]")
error: illegal use of | (line 1, column 7):
  [|foo b
        ^


>>> error(write, r'''
... foo bar |
... qux
... ''')
error: expected an expression after | (line 2, column 9):
  foo bar |
           ^u

>>> error(write, r'''
... (foo bar |)
... qux
... ''')
error: expected an expression after | (line 2, column 9):
  (foo bar |
            ^u

>>> error(write, r'''
... [foo bar |]
... qux
... ''')
error: expected an expression after | (line 2, column 9):
  (foo bar |
            ^u


>>> write(r'''
... foo bar |
...   qux
... ''')
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]u

>>> write(r'''
... (foo bar |
...   qux)
... ''')
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]u

>>> error(write, r'''
... [foo bar |
...   qux]
... ''')
[(&fn apply) (&fn seq) (&symbol foo) (&symbol bar) (&symbol qux)]u


>>> write("(foo bar | qux)")
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("foo bar | qux")
[(&fn apply) (&symbol foo) (&symbol bar) (&symbol qux)]

>>> write("[foo bar | qux]")
[(&fn apply) (&fn seq) (&symbol foo) (&symbol bar) (&symbol qux)]


>>> error(write, "Test | Args: Orig | Args")
error: illegal use of | (line 1, column 12):
  Test | Args:
             ^

>>> error(write, "(Test | Args: Orig | Args)")
error: illegal use of | (line 1, column 13):
  (Test | Args:
              ^

>>> error(write, "[Test | Args: Orig | Args]")
error: illegal use of | (line 1, column 13):
  [Test | Args:
              ^


>>> write_all("Test | Args; Orig | Args")
[(&fn apply) (&symbol Test) (&symbol Args)]
[(&fn apply) (&symbol Orig) (&symbol Args)]u

>>> error(write, "(Test | Args; Orig | Args)")
[[(&fn apply) (&symbol Test) (&symbol Args)] [(&fn apply) (&symbol Orig) (&symbol Args)]]

>>> error(write, "[Test | Args; Orig | Args]")
[(&fn seq) [(&fn apply) (&fn seq) (&symbol Test) (&symbol Args)] [(&fn apply) (&fn seq) (&symbol Orig) (&symbol Args)]]


>>> write("foo: Test | Args; Orig | Args")
[(&symbol foo) [(&fn apply) (&symbol Test) (&symbol Args)] [(&fn apply) (&symbol Orig) (&symbol Args)]]

>>> error(write, "(foo: Test | Args; Orig | Args)")
error: invalid character : (line 1, column 5):
  (foo:
      ^

>>> error(write, "[foo: Test | Args; Orig | Args]")
error: invalid character : (line 1, column 5):
  [foo:
      ^


>>> write("X | (join R Y)")
[(&fn apply) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]

>>> write("(X | (join R Y))")
[(&fn apply) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]

>>> write("[X | (join R Y)]")
[(&fn apply) (&fn seq) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]


>>> write("X | [join R Y]")
[(&fn apply) (&symbol X) [(&fn seq) (&symbol join) (&symbol R) (&symbol Y)]]

>>> write("(X | [join R Y])")
[(&fn apply) (&symbol X) [(&fn seq) (&symbol join) (&symbol R) (&symbol Y)]]

>>> write("[X | [join R Y]]")
[(&fn apply) (&fn seq) (&symbol X) [(&fn seq) (&symbol join) (&symbol R) (&symbol Y)]]

##############################################################################
#  Colon
##############################################################################
>>> write("zip | : map Args cdr")
[(&fn apply) (&symbol zip) [(&symbol map) (&symbol Args) (&symbol cdr)]]

>>> error(write, "(zip | : map Args cdr)")
error: invalid character : (line 1, column 8):
  (zip | :
         ^

>>> error(write, "[zip | : map Args cdr]")
error: invalid character : (line 1, column 8):
  [zip | :
         ^


>>> write("zip | : map Args: cdr")
[(&fn apply) (&symbol zip) [(&symbol map) (&symbol Args) (&symbol cdr)]]

>>> error(write, "(zip | : map Args: cdr)")
error: invalid character : (line 1, column 8):
  (zip | :
         ^

>>> error(write, "[zip | : map Args: cdr]")
error: invalid character : (line 1, column 8):
  [zip | :
         ^


>>> error(write, "zip | : map Args cdr; bar")
error: illegal use of | (line 1, column 21):
  zip | : map Args cdr;
                      ^

>>> error(write, "(zip | : map Args cdr; bar)")
error: invalid character : (line 1, column 8):
  (zip | :
         ^

>>> error(write, "[zip | : map Args cdr; bar]")
error: invalid character : (line 1, column 8):
  [zip | :
         ^


>>> write_all(
... r'''
... foo: bar
... qux
... ''')
[(&symbol foo) (&symbol bar)]
(&symbol qux)


>>> write("foo: bar: qux")
[(&symbol foo) [(&symbol bar) (&symbol qux)]]

>>> error(write, "(foo: bar: qux)")
error: invalid character : (line 1, column 5):
  (foo:
      ^

>>> error(write, "[foo: bar: qux]")
error: invalid character : (line 1, column 5):
  [foo:
      ^


>>> write("foo: bar | qux")
[(&symbol foo) [(&fn apply) (&symbol bar) (&symbol qux)]]

>>> error(write, "(foo: bar | qux)")
error: invalid character : (line 1, column 5):
  (foo:
      ^

>>> error(write, "[foo: bar | qux]")
error: invalid character : (line 1, column 5):
  [foo:
      ^


>>> write("foo: bar; qux")
[(&symbol foo) (&symbol bar) (&symbol qux)]

>>> error(write, "(foo: bar; qux)")
error: invalid character : (line 1, column 5):
  (foo:
      ^

>>> error(write, "[foo: bar; qux]")
error: invalid character : (line 1, column 5):
  [foo:
      ^


>>> write("foo: bar; qux; corge")
[(&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> error(write, "(foo: bar; qux; corge)")
error: invalid character : (line 1, column 5):
  (foo:
      ^

>>> error(write, "[foo: bar; qux; corge]")
error: invalid character : (line 1, column 5):
  [foo:
      ^


>>> error(write,
... r'''
... (foo:
...   bar:
...     qux)
... ''')
error: invalid character : (line 2, column 5):
  (foo:
      ^


>>> write(
... r'''
... $let: F: $fn Args: foo
...            bar
...            qux
...   corge
... ''')
[(&symbol $let) [(&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]]]]

>>> write(
... r'''
... $let: F: $fn Args: foo;
...            bar
...            qux
...   corge
... ''')
[(&symbol $let) [(&symbol F) [(&symbol $fn) (&symbol Args) (&symbol foo)]] (&symbol bar) (&symbol qux) (&symbol corge)]

>>> error(write,
... r'''
... $let: F: $fn Args: foo;
...            bar
...            qux;
...   corge
... ''')
error: no matching : (line 4, column 15):
  qux;
     ^

>>> error(write,
... r'''
... $let: F: $fn Args (foo)
...            bar
...            qux;
...   corge
... ''')
error: no matching : (line 4, column 15):
  qux;
     ^


>>> write_all(
... r'''
... foo bar;qux ->
...  $let:Orig:eval Name
...       Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar)]
[(&vau $fn) [(&fn seq) (&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)]] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]

>>> write_all(
... r'''
... foo bar;qux ->
...  $let:Orig:eval Name
...      Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar)]
[(&vau $fn) [(&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)]] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]

>>> write_all(
... r'''
... foo bar;qux ->
...  $let;Orig:eval Name
...      Test:eval Test
...   $let F:corge
...    eval F
... ''')
[(&symbol foo) (&symbol bar)]
[(&vau $fn) [(&symbol qux)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)] [(&symbol Test) [(&symbol eval) (&symbol Test)]] [(&symbol $let) (&symbol F) [(&symbol corge)] [(&symbol eval) (&symbol F)]]]]]


>>> write(
... r'''
... any: zip Fns Args; [X Y] ->
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]

>>> write_all(
... r'''
... any (zip Fns Args); [X Y] ->
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]

>>> write_all(
... r'''
... any; [X Y] -> zip: Fns Args
...  $if: not: X Y
...   error foo
... ''')
[(&symbol any) [(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&symbol zip) [(&symbol Fns) (&symbol Args)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) (&symbol foo)]]]]


>>> write(": X -> : $let | R; Y")
[[(&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("((X -> : $let | R) Y)")
[[(&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("[[X -> : $let | R] Y]")
[(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]


>>> write(": $fn: X; $let | R; Y")
[[(&symbol $fn) [(&symbol X)] [(&fn apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("(($fn (X): $let | R) Y)")
[[(&symbol $fn) [(&symbol X)] [(&fn apply) (&symbol $let) (&symbol R)]] (&symbol Y)]

>>> write("[[$fn [X]: $let | R] Y]")
[(&fn seq) [(&fn seq) (&symbol $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]

##############################################################################
#  Semicolon
##############################################################################
>>> write("[$set! Name: $vau: $quote %Env; Args; case Args | Fns]")
[(&fn seq) (&symbol $set!) (&symbol Name) [(&fn seq) (&symbol $vau) [(&fn seq) (&symbol $quote) (&symbol %Env)] (&symbol Args) [(&fn apply) (&fn seq) (&symbol case) (&symbol Args) (&symbol Fns)]]]


>>> write(
... r'''
... $def! list? $or: cons? X; null? X
... ''')
[(&symbol $def!) (&symbol list?) (&symbol $or) [(&symbol cons?) (&symbol X)] [(&symbol null?) (&symbol X)]]

>>> write(
... r'''
... $def! list? : X -> $or: cons? X; null? X
... ''')
[(&symbol $def!) (&symbol list?) [(&vau $fn) [(&fn seq) (&symbol X)] [(&symbol $or) [(&symbol cons?) (&symbol X)] [(&symbol null?) (&symbol X)]]]]

>>> write(
... r'''
... ($def! list? : X -> $or: cons? X; null? X)
... ''')
[(&symbol $def!) (&symbol list?) [(&vau $fn) [(&fn seq) (&symbol X)] (&symbol $or) [(&symbol cons?) (&symbol X)] [(&symbol null?) (&symbol X)]]]

>>> write(
... r'''
... ($def! list? : X -> ($or: cons? X; null? X))
... ''')
[(&symbol $def!) (&symbol list?) [(&vau $fn) [(&fn seq) (&symbol X)] [(&symbol $or) [(&symbol cons?) (&symbol X)] [(&symbol null?) (&symbol X)]]]]


>>> write(
... r'''
... foo; bar
... qux
... ''')
[(&symbol foo) (&symbol bar)]


>>> write(
... r'''
... $def! foo; X ->
...  bar
... ''')
[(&symbol $def!) (&symbol foo) [(&vau $fn) [(&fn seq) (&symbol X)] (&symbol bar)]]

>>> write(
... r'''
... $def! foo; X ->
...  bar
...   qux
... ''')
[(&symbol $def!) (&symbol foo) [(&vau $fn) [(&fn seq) (&symbol X)] [(&symbol bar) (&symbol qux)]]]

>>> write(
... r'''
... $def! foo; X ->
...  bar
...   qux
...  corge
... ''')
[(&symbol $def!) (&symbol foo) [(&vau $fn) [(&fn seq) (&symbol X)] [(&symbol bar) (&symbol qux)] (&symbol corge)]]


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
[(&symbol $def!) (&symbol foo) (&symbol bar) [(&symbol qux) (&symbol corge)]]

>>> write(
... r'''
... $def! foo;bar;qux;
...  corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

>>> write(
... r'''
... $def! foo;bar;qux;corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]


>>> write(
... r'''
... $def! foo;bar
...           qux
...           corge
... ''')
[(&symbol $def!) (&symbol foo) (&symbol bar) (&symbol qux) (&symbol corge)]

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
[(&symbol $def!) (&symbol foo) [(&symbol bar) (&symbol qux)] (&symbol corge)]

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
[(&symbol $let) (&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol $if-error) [(&fn apply) (&symbol Test) (&symbol Args)]]] [(&symbol Orig) (&symbol Args) [(&symbol eval) (&symbol foo)]]]

>>> write(
... r'''
... $let F: $fn Args: $if-error: Test | Args; Orig | Args
...  eval foo
... ''')
error:

>>> write(
... r'''
... $let F: $fn Args: $if-error: Test | Args
...  Orig | Args
...   eval foo
... ''')
error:

[(&symbol $let) (&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol $if-error) [(&fn apply) (&symbol Test) (&symbol Args)]]] [(&fn apply) (&symbol Orig) (&symbol Args)] [(&symbol eval) (&symbol foo)]]

##############################################################################
#  Arrow
##############################################################################
>>> error(write, "foo->bar")
error: invalid character > (line 1, column 5):
  foo->
      ^

>>> write("foo -> foo")
[(&vau $fn) [(&fn seq) (&symbol foo)] (&symbol foo)]

>>> write("(foo -> foo)")
[(&vau $fn) [(&fn seq) (&symbol foo)] (&symbol foo)]

>>> write("[foo -> foo]")
[(&fn seq) (&vau $fn) [(&fn seq) (&symbol foo)] (&symbol foo)]


>>> write("foo | bar -> foo")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo) (&symbol bar)] (&symbol foo)]

>>> write("(foo | bar -> foo)")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo) (&symbol bar)] (&symbol foo)]

>>> write("[foo | bar -> foo]")
[(&fn seq) (&vau $fn) [(&fn apply) (&fn seq) (&symbol foo) (&symbol bar)] (&symbol foo)]


>>> write("|foo -> foo")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] (&symbol foo)]

>>> write("(|foo -> foo)")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] (&symbol foo)]

>>> write("[|foo -> foo]")
[(&fn seq) (&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] (&symbol foo)]


>>> write("(foo -a foo)")
[(&symbol foo) (&symbol -a) (&symbol foo)]


>>> write(
... r'''
... x y ->
...  foo
...  bar
... ''')
[(&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write(
... r'''
... x y -> foo
...        bar
... ''')
[(&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write("(x y -> foo bar)")
[(&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]

>>> write("[x y -> foo bar]")
[(&fn seq) (&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] (&symbol foo) (&symbol bar)]


>>> write("x y -> foo bar")
[(&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] [(&symbol foo) (&symbol bar)]]

>>> write("(x y -> (foo bar))")
[(&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] [(&symbol foo) (&symbol bar)]]

>>> write("[x y -> [foo bar]]")
[(&fn seq) (&vau $fn) [(&fn seq) (&symbol x) (&symbol y)] [(&fn seq) (&symbol foo) (&symbol bar)]]


>>> write("|foo -> foo bar")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] [(&symbol foo) (&symbol bar)]]

>>> write("(|foo -> (foo bar))")
[(&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] [(&symbol foo) (&symbol bar)]]

>>> write("[|foo -> [foo bar]]")
[(&fn seq) (&vau $fn) [(&fn apply) (&fn seq) (&symbol foo)] [(&fn seq) (&symbol foo) (&symbol bar)]]


>>> error(write, "|foo bar -> foo bar")
error: illegal use of | (line 1, column 6):
  |foo b
       ^

>>> error(write, "(|foo bar -> foo bar)")
error: illegal use of | (line 1, column 7):
  (|foo b
        ^

>>> error(write, "[|foo bar -> foo bar]")
error: illegal use of | (line 1, column 7):
  [|foo b
        ^


>>> write("(X Y -> Y)")
[(&vau $fn) [(&fn seq) (&symbol X) (&symbol Y)] (&symbol Y)]

>>> write(
... r'''
... (X Y ->
... (-> X)
...   (-> Y))
... ''')
[(&vau $fn) [(&fn seq) (&symbol X) (&symbol Y)] [(&vau $fn) [(&fn seq)] (&symbol X)] [(&vau $fn) [(&fn seq)] (&symbol Y)]]

>>> write("(X Y -> -> X -> Y)")
[(&vau $fn) [(&fn seq) (&vau $fn) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X) (&symbol Y)]] (&symbol X)] (&symbol Y)]

>>> write(
... r'''
... (X Y ->
...   -> X
...   -> Y)
... ''')
[(&vau $fn) [(&fn seq) (&vau $fn) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X) (&symbol Y)]] (&symbol X)] (&symbol Y)]


>>> write(
... r'''
... (: [X Y] -> [[X -> [$let | R]] Y]; X)
... ''')
[[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]] [(&symbol X)]]


>>> write(
... r'''
... (([X Y] -> [[X -> [$let | R]] Y])
...   X)
... ''')
[[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]

>>> write(
... r'''
... [X Y] -> [[X -> [$let | R]] Y]
...  X
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [X Y] -> [[X -> [$let | R]] Y]
... X
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]]

>>> write(
... r'''
... :[X Y] -> [[X -> [$let | R]] Y]
...  X
... ''')
[[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]

>>> write(
... r'''
... :[X Y] -> [[X -> [$let | R]] Y]
...   X
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [[[X Y] -> [[X -> [$let | R]] Y]]
...   X]
... ''')
[(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]


>>> write(
... r'''
... ([X Y] -> [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... ([X Y] ->
...   [[X -> [$let | R]] Y]
...   X)
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [X Y] ->
...  [[X -> [$let | R]] Y]
...  X
... ''')
[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]

>>> write(
... r'''
... [[X Y] -> [[X -> [$let | R]] Y] X]
... ''')
[(&fn seq) (&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)] (&symbol X)]


>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] (&symbol each) (&symbol R) (&symbol F)]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X) (each R F)
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X): each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> (F X); each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> : F X; each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   [X | R] F -> F X; each R F
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   ([X | R] F -> (F X); each R F)
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

>>> write(
... r'''
... $def! each
...   ([X | R] F -> : F X; each R F)
... ''')
[(&symbol $def!) (&symbol each) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol F) (&symbol X)] [(&symbol each) (&symbol R) (&symbol F)]]]

##############################################################################
#  Whitespace indentation
##############################################################################
>>> write_all(r'''
... $pref; foo bar; qux yes
...   nou
... ''')
(&symbol $pref)
[(&symbol foo) (&symbol bar)]
[(&symbol qux) (&symbol yes) (&symbol nou)]

>>> write_all(r'''
... $pref; foo bar: qux yes
...   nou
... ''')
(&symbol $pref)
[(&symbol foo) (&symbol bar) [(&symbol qux) (&symbol yes)] (&symbol nou)]

>>> write_all(r'''
... $pref: foo bar; qux yes
...   nou
... ''')
[(&symbol $pref) [(&symbol foo) (&symbol bar)] [(&symbol qux) (&symbol yes)] (&symbol nou)]

>>> write_all(r'''
... $pref: foo bar: qux yes
...   nou
... ''')
[(&symbol $pref) [(&symbol foo) (&symbol bar) [(&symbol qux) (&symbol yes)]] (&symbol nou)]

>>> write_all(r'''
... $pref: foo bar: qux yes:
...   nou
... ''')
[(&symbol $pref) [(&symbol foo) (&symbol bar) [(&symbol qux) (&symbol yes) (&symbol nou)]]]

>>> write_all(r'''
... $pref: foo bar: qux yes:
...   nou
...    ohso
... ''')
[(&symbol $pref) [(&symbol foo) (&symbol bar) [(&symbol qux) (&symbol yes) [(&symbol nou) (&symbol ohso)]]]]

>>> write_all(r'''
... $pref; foo bar; qux yes;
...   nou
... ''')
(&symbol $pref)
[(&symbol foo) (&symbol bar)]
[(&symbol qux) (&symbol yes)]
(&symbol nou)

>>> write_all(r'''
... $pref: foo bar
...        qux yes
...   nou
... ''')
[(&symbol $pref) [(&symbol foo) (&symbol bar)] [(&symbol qux) (&symbol yes)] (&symbol nou)]

>>> write_all(r'''
... $pref; foo bar
...        qux yes
...   nou
... ''')
(&symbol $pref)
[(&symbol foo) (&symbol bar)]
[(&symbol qux) (&symbol yes) (&symbol nou)]

>>> write_all(r'''
... $pref: corge; foo bar
...               qux yes
...   nou
... ''')
[(&symbol $pref) (&symbol corge) [(&symbol foo) (&symbol bar)] [(&symbol qux) (&symbol yes)] (&symbol nou)]


>>> write_all(r'''
... each Args: X -> foo bar
...                 qux corge
...                 yes nou
... ''')
[(&symbol $each) (&symbol Args) [(&vau $fn) [(&fn &list) (&symbol X)] [(&symbol foo) (&symbol bar)] [(&symbol qux) (&symbol corge)] [(&symbol yes) (&symbol nou)]]]

>>> write_all(r'''
... each Args: X -> foo bar
...   qux corge
...   yes nou
... ''')
[(&symbol $each) (&symbol Args) [(&vau $fn) [(&fn &list) (&symbol X)] [(&symbol foo) (&symbol bar)]] [(&symbol qux) (&symbol corge)] [(&symbol yes) (&symbol nou)]]

>>> write_all(r'''
... each Args: X ->
...   foo bar
...   qux corge
...   yes nou
... ''')
[(&symbol $each) (&symbol Args) [(&vau $fn) [(&fn &list) (&symbol X)] [(&symbol foo) (&symbol bar)] [(&symbol qux) (&symbol corge)] [(&symbol yes) (&symbol nou)]]]


>>> write(r'''
...                             # TODO
... $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol any) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol $or) [(&symbol F) (&symbol X)] [(&symbol any) (&symbol R) (&symbol F)]]]]

>>> write(r'''
... $set! $or; $vau Env [X | R]
...   $let: X: eval Env X
...     $if X X: eval Env [$or | R]
...
...                             # TODO
... $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol $or) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol $let) [(&symbol X) [(&symbol eval) (&symbol Env) (&symbol X)]] [(&symbol $if) (&symbol X) (&symbol X) [(&symbol eval) (&symbol Env) [(&fn apply) (&fn seq) (&symbol $or) (&symbol R)]]]]]]

>>> write(r'''
... $set! $or; $vau Env [X | R]
...   $let: X: eval Env X
...     $if X X: eval Env [$or | R]
...                             # TODO
... $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol $or) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol $let) [(&symbol X) [(&symbol eval) (&symbol Env) (&symbol X)]] [(&symbol $if) (&symbol X) (&symbol X) [(&symbol eval) (&symbol Env) [(&fn apply) (&fn seq) (&symbol $or) (&symbol R)]]]]]]


>>> write(r'''
... $set! $or; $vau Env [X | R]
...   $let: X: eval Env X
...     $if X X: eval Env [$or | R]
...                             'foo'
... $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol $or) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol $let) [(&symbol X) [(&symbol eval) (&symbol Env) (&symbol X)]] [(&symbol $if) (&symbol X) (&symbol X) [(&symbol eval) (&symbol Env) [(&fn apply) (&fn seq) (&symbol $or) (&symbol R)] [(&fn str) (&char f) (&char o) (&char o)]]]]]]


>>> write(r'''
... $set! $or; $vau Env [X | R]
...   $let: X: eval Env X
...     $if X X: eval Env [$or | R]
...                             # TODO
...   $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol $or) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol $let) [(&symbol X) [(&symbol eval) (&symbol Env) (&symbol X)]] [(&symbol $if) (&symbol X) (&symbol X) [(&symbol eval) (&symbol Env) [(&fn apply) (&fn seq) (&symbol $or) (&symbol R)]]]] [(&symbol $set!) (&symbol any) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol $or) [(&symbol F) (&symbol X)] [(&symbol any) (&symbol R) (&symbol F)]]]]]]

>>> write(r'''
... $set! $or; $vau Env [X | R]
...   $let: X: eval Env X
...     $if X X: eval Env [$or | R]
...
...                             # TODO
...   $set! any: [X | R] F -> $or (F X): any R F
... ''')
[(&symbol $set!) (&symbol $or) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol $let) [(&symbol X) [(&symbol eval) (&symbol Env) (&symbol X)]] [(&symbol $if) (&symbol X) (&symbol X) [(&symbol eval) (&symbol Env) [(&fn apply) (&fn seq) (&symbol $or) (&symbol R)]]]] [(&symbol $set!) (&symbol any) [(&vau $fn) [(&fn seq) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol F)] [(&symbol $or) [(&symbol F) (&symbol X)] [(&symbol any) (&symbol R) (&symbol F)]]]]]]


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
[(&symbol $def!) (&symbol type) [(&symbol $fn) (&symbol Fns) [(&symbol $fn) (&symbol Args) [(&symbol any) [(&symbol zip) (&symbol Fns) (&symbol Args)] [(&vau $fn) [[(&fn seq) (&symbol X) (&symbol Y)]] [(&symbol $if) [(&symbol not) [(&symbol X) (&symbol Y)]] [(&symbol error) [(&fn str) (&char t) (&char y) (&char p) (&char e) (&char  ) (&char c) (&char h) (&char e) (&char c) (&char k) (&char  ) (&char f) (&char a) (&char i) (&char l) (&char e) (&char d) (&char  ) (&char o) (&char n) (&char  ) (&char a) (&char r) (&char g) (&char u) (&char m) (&char e) (&char n) (&char t) (&char  ) (&symbol Y)]]]]]]]]


>>> write(
... r'''
... $def! foo: type list? any? ; X -> X
... ''')
[(&symbol $def!) (&symbol foo) [(&symbol type) (&symbol list?) (&symbol any?)] [(&vau $fn) [(&fn seq) (&symbol X)] (&symbol X)]]


>>> write(
... r'''
... $def! foo: X -> X
... ''')
[(&symbol $def!) (&symbol foo) [(&vau $fn) [(&fn seq) (&symbol X)] (&symbol X)]]


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
[(&symbol $defvau!) (&symbol $def-if!) [(&vau $fn) [(&symbol Name) (&symbol Test) | (&symbol Fns)] [(&symbol $let) [(&symbol Orig) [(&symbol eval) (&symbol Name)] [(&symbol Test) [(&symbol eval) (&symbol Test)]]] [(&symbol $let) (&symbol F) [(&symbol $fn) (&symbol Args) [(&symbol $if-error) [(&symbol Test) | (&symbol Args)] [(&symbol Orig) | (&symbol Args)]]] [(&symbol eval) [(&fn seq) (&symbol $def!) (&symbol Name) (&symbol F) | (&symbol Fns)]]]]]]


>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream
...            -> nil? X
...            -> car X
...            -> $set-in! Env X: cdr X
... ''')
[(&symbol $def-if!) (&symbol stream) [(&symbol type) (&symbol list?)] [(&vau $fn) [(&symbol X)] [(&symbol $let) (&symbol Env) [(&symbol current-env)] [(&symbol make-stream) [(&vau $fn) [] [(&symbol nil?) (&symbol X)]] [(&vau $fn) [] [(&symbol car) (&symbol X)]] [(&vau $fn) [] [(&symbol $set-in!) (&symbol Env) (&symbol X) [(&symbol cdr) (&symbol X)]]]]]]]


>>> write(
... r'''
... $def-if! stream: type list?
...   X -> $let Env: current-env
...          make-stream; -> nil? X
...                       -> car X
...                       -> $set-in! Env X: cdr X
... ''')
[(&symbol $def-if!) (&symbol stream) [(&symbol type) (&symbol list?)] [(&vau $fn) [(&symbol X)] [(&symbol $let) (&symbol Env) [(&symbol current-env)] [(&symbol make-stream) [(&vau $fn) [] [(&symbol nil?) (&symbol X)]] [(&vau $fn) [] [(&symbol car) (&symbol X)]] [(&vau $fn) [] [(&symbol $set-in!) (&symbol Env) (&symbol X) [(&symbol cdr) (&symbol X)]]]]]]]


>>> write(
... r'''
... $def! join: type list? any?
...   [X | R] Y -> [X | (join R Y)]
...   [X]     Y -> [X | Y]
... ''')
[(&symbol $def!) (&symbol join) [(&symbol type) (&symbol list?) (&symbol any?)] [(&vau $fn) [[(&fn apply) (&fn seq) (&symbol X) (&symbol R)] (&symbol Y)] [(&fn apply) (&fn seq) (&symbol X) [(&symbol join) (&symbol R) (&symbol Y)]]] [(&vau $fn) [[(&fn seq) (&symbol X)] (&symbol Y)] [(&fn apply) (&fn seq) (&symbol X) (&symbol Y)]]]


>>> write(
... r'''
... $set! $let; $vau Env [X | R]
...   eval Env: $if: null? R
...               X
...               # Equivalent to ($let: [X Y] X; ...)
...               :[X Y] -> [[X -> : $let | R] Y]
...                X
... ''')
[(&symbol $set!) (&symbol $let) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol X) (&symbol R)] [(&symbol eval) (&symbol Env) [(&symbol $if) [(&symbol null?) (&symbol R)] (&symbol X) [[(&vau $fn) [(&fn seq) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]] (&symbol Y)]] (&symbol X)]]]]]


>>> write(
... r'''
... $set! $def! ; $vau Env [Name | Fns]
...   $let: Args: uniq
...     eval Env [$set! Name: $fn Args: case Args | Fns]
... ''')
[(&symbol $set!) (&symbol $def!) [(&symbol $vau) (&symbol Env) [(&fn apply) (&fn seq) (&symbol Name) (&symbol Fns)] [(&symbol $let) [(&symbol Args) [(&symbol uniq)]] [(&symbol eval) (&symbol Env) [(&fn seq) (&symbol $set!) (&symbol Name) [(&fn seq) (&symbol $fn) (&symbol Args) [(&fn apply) (&fn seq) (&symbol case) (&symbol Args) (&symbol Fns)]]]]]]]


>>> write(
... r'''
... $set! $let; $vau Env [X | R]
...   eval Env: $if: null? R
...               X
...               # Equivalent to ($let: [X Y] X; ...)
...               :[X Y] -> [[X -> : $let | R] Y]
...                X
... ''')
[(&symbol $set!) (&symbol $let) [(&symbol $vau) (&symbol Env) [(&vau &apply
) (&fn seq) (&symbol X) (&symbol R)] [(&symbol eval) (&symbol Env) [(&symbol
$if) [(&symbol null?) (&symbol R)] (&symbol X) [[(&vau $fn) [(&fn seq) [(&
fn &list) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&vau $fn) [(&
fn &list) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let) (&symbol R)]]
(&symbol Y)]] (&symbol X)]]]]]

>>> write(
... r'''
... $set! $let; $vau Env [X | R]
...   eval Env: $if: null? R
...               X
...               # Equivalent to ($let: [X Y] X; ...)
...               prn :[X Y] -> [[X -> : $let | R] Y]
...                    X
... ''')
[(&symbol $set!) (&symbol $let) [(&symbol $vau) (&symbol Env) [(&vau &apply
) (&fn seq) (&symbol X) (&symbol R)] [(&symbol eval) (&symbol Env) [(&symbol
$if) [(&symbol null?) (&symbol R)] (&symbol X) [(&symbol prn) [(&vau $fn) [(
&fn &list) [(&fn seq) (&symbol X) (&symbol Y)]] [(&fn seq) [(&fn seq) (&v
au &arrow) [(&fn seq) (&symbol X)] [(&fn apply) (&fn seq) (&symbol $let)
(&symbol R)]] (&symbol Y)]] (&symbol X)]]]]]

>>> write(
... r'''
... $set! $let; $vau Env [X | R]
...   eval Env: $if: null? R
...               X
...               # Equivalent to ($let: [X Y] X; ...)
...               ([X Y] -> [[X -> : $let | R] Y]
...                X)
... ''')
u

>>> write(
... r'''
... $set! $let; $vau Env [X | R]
...   eval Env: $if: null? R
...               X
...               # Equivalent to ($let: [X Y] X; ...)
...               (([X Y] -> [[X -> : $let | R] Y])
...                X)
... ''')
u
