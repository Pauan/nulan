##############################################################################
#  Core
##############################################################################

$assign $let; $vau Env [X Y @R]
  eval Env [[$fn [seq X] @R] Y]

$assign $quote: $vau ~ [X] X

$assign $or; $vau Env [X @R]
  $let X: eval Env X
    $if X X: eval Env [$or @R]

$assign any?: [X @R] F -> $or: F X; any? R F

$assign case: X @Fns -> any? Fns: F -> F @X

# TODO is it necessary for this to be a vau rather than an fn?
$assign case-vau; $vau Env Fns
  $let Args: uniq
    eval Env [$vau ($quote %Env) Args: case Args @Fns]

$assign case-fn; @Fns ->
  @Args -> case Args @Fns

$assign $defvau; $vau Env [Name @Fns]
  eval Env [$assign Name: case-vau @Fns]

$assign $def; $vau Env [Name @Fns]
  eval Env [$assign Name: case-fn @Fns]

#|
($assign foo; bar
  (qux 1)
  (corge 2))

($let foo: uniq
  (bar 1))

($or: foo 1; bar 2)

($if: not: X Y
  (type-error "type check failed on argument @Y"))

($assign foo; $or: F X: any? R F)
($assign foo ($or (F X (any? R F))))

($assign foo: $or; F X; any? R F)
($assign foo ($or (F X) (any? R F)))

($assign foo: $or: F X; any? R F)
($assign foo ($or (F X) (any? R F)))
|#

##############################################################################
#  $use
##############################################################################

$def get-current-env: wrap: $vau Env [] Env

$def make-env: Env -> eval Env [[$vau ~ ~ : get-current-env]]

$def make-base-env
  $let Top: get-current-env
    -> make-env Top


$def $use; $vau Env Args
  each Args; X ->
    $let New: make-base-env
      $hook Env get-if-unbound
        K -> eval New K
      load-file-in New: find-file X
      eval Env [$def (strip-extension: basename X) New]

##############################################################################
#  Other utilities
##############################################################################

# Could be implemented in terms of each, but it would be inefficient
$def eachf
  [X @R] F -> F X; each R F
  ~      ~ -> %f

$def not: X -> $if X %f %t

#|
$def type; @Fns ->
  $if: seq? Fns
    @Args -> any?: zip Fns Args; [X Y] ->
               $if: not: X Y
                 type-error "type check failed on argument @Y"
    @Args -> any? Args; X ->
               $if: not: Fns X
                 type-error "type check failed on argument @Y"
|#

$defvau do: @Args -> eval [[$fn [] @Args]]

$defvau $and
  X    -> eval X
  X @R -> $if: eval X
            eval [$and @R]

$def all?
  [X]    F -> F X
  [X @R] F -> $and: F X; all? R F

$def none?
  X F -> all? X: X -> not: F X


# Logical functions on functions
$def fn-fn
  F -> @Fns -> @Args -> F Fns: X -> X @Args

$def fn-not: fn-fn none?
$def fn-and: fn-fn all?
$def fn-or:  fn-fn any?

# TODO dotted list support for sum/sumr

# Called foldl/reduce in other languages.
#
# Takes 2 to 3 arguments:
#
#   => sum [1 2 3] seq
#   [[1 2] 3]
#
#   => sum 0 [1 2 3] seq
#   [[[0 1] 2] 3]
#
$def sum
  [X @R] F   -> sum1 X R F
  I      X F -> sum1 I X F

$def sum1
  I [X @R] F -> sum1 (F I X) R F
  I ~      ~ -> I

#|
0 [1 2 3] F
sum1 (F 0 1) [2 3] F
sum1 (F (F 0 1) 2) [3] F
sum1 (F (F (F 0 1) 2) 3) F

[1 2 3] 0 F
(F 1 (sumr [2 3] 0 F))
(F 1 (F 2 (sumr [3] 0 F)))
(F 1 (F 2 (F 3 0)))
|#


"foobar@(X \n) foo"

# Called foldr/rreduce in other languages.
#
# Takes 2 to 3 arguments:
#
#   => sumr [1 2 3] seq
#   [1 [2 3]]
#
#   => sumr [1 2 3] 0 seq
#   [1 [2 [3 0]]]
#
$def sumr
  [X @R] F   -> F X: sumr R F
  X      ~   -> X
  X      I F -> sumr1 X I F

$def sumr1
  [X @R] I F -> F X: sumr R I F
  ~      I ~ -> I

#|
[|Foo |Bar]

"foo |bar"
|#


# Whee, function composition implemented with sum
$def compose
  @Fns -> sum Fns: X Y -> @Args -> X: Y @Args


# Called map in other languages.
#|
# It's easier to just define it explicitly, rather than using cons + sumr.
$def each
  [X @R] F -> [(F X) @(each R F)]
  X       ~ -> X
|#
$def each; X F ->
  sumr X []: X Y -> [(F X) @Y]

$def rev; X ->
  sum [] X: Y X -> [X @Y]

#|

# mapreduce
$def sumeach
  I []      ~ ~ -> I
  I [X]     F ~ -> (F X)
  I [@L @R] F G -> G: sumeach I L F G
                      sumeach I R F G

# map
$def each
  X F -> sumeach [] X F tree

$def id: X -> X
#$def copy: X -> each X id

# reduce
$def sum
  I X F -> sumeach I X id F


$def first
  []      -> []
  [X]     -> X
  [@L @R] -> first L

$def rest
  []      -> []
  [X]     -> []
  [@L @R] -> [@(rest L) @R]

$def each
  []      ~ -> []
  [X]     F -> F X
  [@L @R] F -> [@(each L F) @(each R F)]

|#

#|
$def foldr
  F I X -> sum I X: K X -> K: F X

$def sum1
  I [X @R] F -> sum1 (F I X) R F
  I ~      ~ -> I

(def foldl (init x f)
  (if (acons x)
      (foldl (f init (car x)) (cdr x) f)
      init))

(def foldr (x init f)
  (foldl init x (fn (x y) (f x y)))
  ;((foldl idfn x (fn (k x) (compose k (f x)))) init)
  )
|#
#|
$def cons: X Y -> [X @Y]
|#


#|
$def sum
  [X Y @R] F -> sum1 [(F X Y) @R] F
  X        ~ -> X
|#

#|
$def keep
  [X @R] F -> $if: F X
                [X @(keep R F)]
                keep R F
  X      ~ -> X
|#

$def keep; X F ->
  sumr X []; X Y ->
    $if: F X
      [X @Y]
      Y

#|
(def keep (x f)
  (rreduce (fn (x y) (prn x " " y) (if (f x) (cons x y) y)) x))

(keep '(1 2 3 4 5) (fn (x) (> x 3)))
|#

$def rem
  X F -> keep X: fn-not F


$def empty?: []     -> %t
$def first:  [X @~] -> X
$def rest:   [~ @R] -> R

# => (zip [a 1] [b 2] [c 3])
#    [[a b c] [1 2 3]]
$def zip; @Args ->
  $if: any? Args empty?
    []
    [(each Args first) @(zip @(each Args rest))]


# Takes two seqs and returns a single seq.
#
# Linear in time to the length of the first argument.
# This is because it simply conses it directly onto the second argument.
# This is okay because Nulan doesn't have seq mutation.
$def join
  [X @R] Y -> [X @(join R Y)]
  [X]    Y -> [X @Y]

# TODO could use a better name than joinr
$def joinr
  X Y -> join X [Y]


$def ref
  [K V @R] K -> V
  [~ ~ @R] K -> ref R K


# TODO maybe make it take rest args?
$def iso?
  X       X       -> %t
  [X @R1] [Y @R2] -> $and: iso? X Y; iso? R1 R2


$def prn!; @Args ->
  pr! @Args
  pr! "\n"

$def writen!; @Args ->
  write! @Args
  pr! "\n"

#|
$def pair
  [X Y @R] I -> [[X Y] @(pair R I)]
  [X]      I -> [[X I]]
  X          -> pair X []
|#

$defvau $lets
  [X]        -> eval X
  [[X Y] @R] -> eval [$let X Y: $lets @R]

$defvau $if-error; [X Y @R] ->
  $let U: uniq
    eval [$on-error X
           [fn [seq: error ~] Y]
           @(joinr R [$fn [seq U] U])]

$defvau $def-if!; Name Test @Fns ->
  $lets: Orig:  eval Name
         Test:  eval Test
         F;     $fn Args
                  $if-error: Test @Args
                    Orig @Args
    eval [$assign! Name: case-fn F @Fns]

#|
$on-error: Test @Args
  (error ~) -> Orig @Args
  X         -> X
|#

#|
X -> foo bar
  qux corge

$fn [X]: foo bar
  qux corge

($fn [X]
  (foo bar)
  (qux corge))


{X + {Y * Z}}
/X is? Y/
X `is?` []


#$def list?: X -> $or; cons? X; null? X;


#|
# => ($let X)
#    X
# => ($let (X Y) ...)
#    (($fn [X] ($let ...)) Y)
$assign $let; $vau Env [X Y | R]
  #eval Env
  #  $if: null? R
  #    X
  #    # Equivalent to ($let [X Y] X ...)
  #    ;[X Y] -> [[$fn [X] [$let | R]] Y]
  #     X
  eval Env [[X -> | R] Y]
|#


#|
$assign $let; $vau Env [X Y | R]
  [X]         -> eval Env X
  [[X Y] | R] -> eval [[$fn [seq X] [$let | R]] Y]
  eval [[$fn [seq X] [$let | R]] Y]
|#


($defvau $def-if!
  (Name Test | Fns ->
    ($let
      (Orig  (eval Name))
      (Test  (eval Test))
      F
      ($fn Args
        ($if-error: Test | Args
           (~ -> Orig | Args)
           (X -> X)))
      (eval [$def Name F | Fns]))))

($defvau $def-if!
  ($fn [Name Test | Fns]
    ($let
      (Orig
        (eval Name))
      (Test
        (eval Test))
      (F
        ($fn Args
          ($if-error (Test | Args)
            (~ -> Orig | Args)
            (X -> X))))
      (eval [$def Name F | Fns]))))

($defvau $def-if!
  ($fn [Name Test | Fns]
    ($let (Orig  (eval Name))
          (Test  (eval Test))
          (F     ($fn Args
                   ($if-error (Test | Args)
                     (~ -> Orig | Args)
                     (X -> X))))
      (eval [$def Name F | Fns]))))
|#
