##############################################################################
#  Core
##############################################################################
# Equivalent to:
#   $set! $set!; $vau Env [X Y] ...
(($vau Env []
   (set-ref! Env
     (replace-at Env (($vau ~ [X] X) $set!)
       ($fn []
         ($vau Env [X Y]
           (call-cc
             ($fn [C]
               (set-ref! Env
                 (replace-at Env X
                   ($fn []
                     (eval Env Y))
                   ($fn [X]
                     ($if (ref? X)
                       (C (set-ref! X (eval Env Y)))
                       (eval Env Y)))))))))
       ($fn [])))))

#|
$set! $set!; $vau Env [X Y]
  $catch; set-ref! Env
    replace-at Env X
      $fn []
        eval Env Y
      $fn [X]
        $if: ref? X
          throw: set-ref! X: eval Env Y
          eval Env Y
|#


$set! $let; $vau Env [X Y @R]
  eval Env [[$fn [seq X] @R] Y]

$set! $quote: $vau ~ [X] X

#|
$set! $do; $vau Env Body
  eval Env [[$fn [] @Body]]

$set! %t; $vau Env [X Y]
  eval Env X

$set! %f; $vau Env [X Y]
  eval Env Y

$set! $if; $vau Env [X Y @R]
  eval Env [X Y: $do @R]
|#

$set! $or; $vau Env [X @R]
  $let X: eval Env X
    $if X X: eval Env [$or @R]

$set! any?: [X @R] F -> $or: F X; any? R F

$set! case: X @Fns -> any? Fns: F -> F @X

# TODO is it necessary for this to be a vau rather than an fn?
$set! case-vau; $vau Env Fns
  $let Args: uniq
    eval Env [$vau ($quote %Env) Args: case Args @Fns]

$set! case-fn; @Fns ->
  @Args -> case Args @Fns

$set! $defvau!; $vau Env [Name @Fns]
  eval Env [$set! Name: case-vau @Fns]

$set! $def!; $vau Env [Name @Fns]
  eval Env [$set! Name: case-fn @Fns]

#|
($set! foo; bar
  (qux 1)
  (corge 2))

($let foo: uniq
  (bar 1))

($or: foo 1; bar 2)

($if: not: X Y
  (type-error "type check failed on argument @Y"))

($set! foo; $or: F X: any? R F)
($set! foo ($or (F X (any? R F))))

($set! foo: $or; F X; any? R F)
($set! foo ($or (F X) (any? R F)))

($set! foo: $or: F X; any? R F)
($set! foo ($or (F X) (any? R F)))
|#

##############################################################################
#  $use!
##############################################################################
$defvau! $ref!
  X Y -> eval [$set! X: ref Y]
  X   -> eval [$ref! X %f]

$def! get-current-env: wrap: $vau Env [] Env

#$def! make-env: Env -> eval Env [[$vau ~ ~ : get-current-env]]

#|
$ref! make-base-env

$let Top: get-current-env
  $def! make-base-env
    -> make-env Top
|#
#$ref! make-base-env

#$def! make-base-env: -> (get-current-env)

#$let X 5
#  $let Y: get-current-env
#    eval Y: $quote X
#5
#|
$defvau! $use!; @Args ->
  each Args; X ->
    $let New: make-base-env
      $hook %Env get-if-unbound
        K -> eval New K
      load-file-in New: find-file X
      eval %Env [$set! (strip-extension: basename X) New]
|#

##############################################################################
#  Other utilities
##############################################################################
$defvau! $do: @Body -> eval [[$fn [] @Body]]

# Could be implemented in terms of each, but it would be inefficient
$def! eachf
  [X @R] F -> F X; each R F
  ~      ~ -> %f

$def! not: X -> $if X %f %t

#|
$def! type; @Fns ->
  $if: seq? Fns
    @Args -> any?: zip Fns Args; [X Y] ->
               $if: not: X Y
                 type-error "type check failed on argument @Y"
    @Args -> any? Args; X ->
               $if: not: Fns X
                 type-error "type check failed on argument @Y"
|#

$defvau! do: @Args -> eval [[$fn [] @Args]]

$defvau! $and
  X    -> eval X
  X @R -> $if: eval X
            eval [$and @R]

$def! all?
  [X]    F -> F X
  [X @R] F -> $and: F X; all? R F

$def! none?
  X F -> all? X: X -> not: F X


# Logical functions on functions
$def! fn-fn
  F -> @Fns -> @Args -> F Fns: X -> X @Args

$def! fn-not: fn-fn none?
$def! fn-and: fn-fn all?
$def! fn-or:  fn-fn any?

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
$def! sum
  [X @R] F   -> sum1 X R F
  I      X F -> sum1 I X F

$def! sum1
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


#"foobar@(X \n) foo"

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
$def! sumr
  [X @R] F   -> F X: sumr R F
  X      ~   -> X
  X      I F -> sumr1 X I F

$def! sumr1
  [X @R] I F -> F X: sumr R I F
  ~      I ~ -> I

#|
[|Foo |Bar]

"foo |bar"
|#


#|
$def! map
  [X @R] F -> $lazy [(F X) @(map R F)]
  X      ~ -> X


=> map [1 2 3 4 5] id
$lazy [(id 1) @(map [2 3 4 5] id)]
$lazy [(id 2) @(map [3 4 5] id)]
$lazy [(id 3) @(map [4 5] id)]
$lazy [(id 4) @(map [5] id)]
$lazy [(id 4) @(map [] id)]
[]


$lazy [(id 1) @$lazy [(id 2) @$lazy [(id 3) @$lazy [(id 4) @$lazy [(id 4) @[]]]]]]


$fn [] [(id 1) @(map [2 3 4 5] id)]
$fn [] [(id 2) @(map [3 4 5] id)]
$fn [] [(id 3) @(map [4 5] id)]
$fn [] [(id 4) @(map [5] id)]
$fn [] [(id 4) @(map [] id)]
[]
|#


# Whee, function composition implemented with sum
$def! compose
  @Fns -> sum Fns: X Y -> @Args -> X: Y @Args


# Called map in other languages.
#|
# It's easier to just define it explicitly, rather than using cons + sumr.
$def! each
  [X @R] F -> [(F X) @(each R F)]
  X      ~ -> X
|#
$def! each; X F ->
  sumr X []: X Y -> [(F X) @Y]

$def! rev; X ->
  sum [] X: Y X -> [X @Y]

#|

# mapreduce
$def! sumeach
  I []      ~ ~ -> I
  I [X]     F ~ -> (F X)
  I [@L @R] F G -> G: sumeach I L F G
                      sumeach I R F G

# map
$def! each
  X F -> sumeach [] X F tree

$def! id: X -> X
#$def! copy: X -> each X id

# reduce
$def! sum
  I X F -> sumeach I X id F


$def! first
  []      -> []
  [X]     -> X
  [@L @R] -> first L

$def! rest
  []      -> []
  [X]     -> []
  [@L @R] -> [@(rest L) @R]

$def! each
  []      ~ -> []
  [X]     F -> F X
  [@L @R] F -> [@(each L F) @(each R F)]

|#

#|
$def! foldr
  F I X -> sum I X: K X -> K: F X

$def! sum1
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
$def! cons: X Y -> [X @Y]
|#


#|
$def! sum
  [X Y @R] F -> sum1 [(F X Y) @R] F
  X        ~ -> X
|#

#|
$def! keep
  [X @R] F -> $if: F X
                [X @(keep R F)]
                keep R F
  X      ~ -> X
|#

$def! keep; X F ->
  sumr X []; X Y ->
    $if: F X
      [X @Y]
      Y

#|
(def keep (x f)
  (rreduce (fn (x y) (prn x " " y) (if (f x) (cons x y) y)) x))

(keep '(1 2 3 4 5) (fn (x) (> x 3)))
|#

$def! rem
  X F -> keep X: fn-not F


$def! empty?: []     -> %t
$def! first:  [X @~] -> X
$def! rest:   [~ @R] -> R

# => (zip [a 1] [b 2] [c 3])
#    [[a b c] [1 2 3]]
$def! zip; @Args ->
  $if: any? Args empty?
    []
    [(each Args first) @(zip @(each Args rest))]

#|
(def zip args
  (if (some no args)
      nil
      (cons (map car args) (zip (map cdr args)))))
|#


# Takes two seqs and returns a single seq.
#
# Linear in time to the length of the first argument.
# This is because it simply conses it directly onto the second argument.
# This is okay because Nulan doesn't have seq mutation.
$def! join
  [X @R] Y -> [X @(join R Y)]
  [X]    Y -> [X @Y]

# TODO could use a better name than joinr
$def! joinr
  X Y -> join X [Y]


$def! ref
  [K V @R] K -> V
  [~ ~ @R] K -> ref R K


# TODO maybe make it take rest args?
$def! iso?
  X       X       -> %t
  [X @R1] [Y @R2] -> $and: iso? X Y; iso? R1 R2


$def! prn!; @Args ->
  pr! @Args
  pr! "\n"

$def! writen!; @Args ->
  write! @Args
  pr! "\n"

#|
$def! pair
  [X Y @R] I -> [[X Y] @(pair R I)]
  [X]      I -> [[X I]]
  X          -> pair X []
|#

$defvau! $lets
  [X]        -> eval X
  [[X Y] @R] -> eval [$let X Y: $lets @R]

$defvau! $if-error; [X Y @R] ->
  $let U: uniq
    eval [$on-error X
           [$fn [seq: error ~] Y]
           @(joinr R [$fn [seq U] U])]

#|
$set! reference (ref 12)
$set! (ref twelve) reference


$set! reference: (ref 12)
$set! *twelve reference
$set! (ref twelve) reference
$set! reference
|#

#|
$defvau! $def-if!; Name Test @Fns ->
  $lets: Orig:  eval Name
         Test:  eval Test
         F;     $fn Args
                  $if-error: Test @Args
                    Orig @Args
    eval [$set! Name: case-fn F @Fns]
|#

# Lazily evaluates the expression X with call-by-need semantics:
#
# The expression won't be evaluated until it's actually needed, and if
# it's evaluated more than once, it'll reuse the previous value
$defvau! $lazy; X ->
  $let Saved: ref
    call-on-eval; ->
      $if *Saved
        *Saved
        $set! *Saved: eval X

# Dynamically rebinds a ref while within the $letref's body:
#
#   => $set! *foo 1
#   1
#
#   => $letref foo 5
#        *foo
#   5
#
#   => *foo
#   1
$defvau! $letref; X Y @Body ->
  $lets: Old:  ref
         X:    eval X
         Y:    eval Y
    eval [dynamic-wind
           ($fn [] ($set! *Old *X)
                   ($set! *X Y))
           [$fn [] @Body]
           ($fn [] ($set! *X *Old))]


# Evaluates the expression X
#
# When the expression exits (either normally or due to a
# continuation/exception), expression Y will be evaluated
#
# This is used for cleanup code, e.g. closing a file after
# performing some operations on it
$defvau! $after; X Y ->
  $lets: Seen:  ref %f
         F;     $fn []
                  $if *Seen
                    throw: error "cannot re-enter an $after with a continuation"
                    $set! *Seen %t
    eval [dynamic-wind
           F
           [$fn [] X]
           [$fn [] Y]]

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


#$def! list?: X -> $or; cons? X; null? X;


#|
# => ($let X)
#    X
# => ($let (X Y) ...)
#    (($fn [X] ($let ...)) Y)
$set! $let; $vau Env [X Y | R]
  #eval Env
  #  $if: null? R
  #    X
  #    # Equivalent to ($let [X Y] X ...)
  #    ;[X Y] -> [[$fn [X] [$let | R]] Y]
  #     X
  eval Env [[X -> | R] Y]
|#


#|
$set! $let; $vau Env [X Y | R]
  [X]         -> eval Env X
  [[X Y] | R] -> eval [[$fn [seq X] [$let | R]] Y]
  eval [[$fn [seq X] [$let | R]] Y]
|#


($defvau! $def-if!
  (Name Test | Fns ->
    ($let
      (Orig  (eval Name))
      (Test  (eval Test))
      F
      ($fn Args
        ($if-error: Test | Args
           (~ -> Orig | Args)
           (X -> X)))
      (eval [$def! Name F | Fns]))))

($defvau! $def-if!
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
      (eval [$def! Name F | Fns]))))

($defvau! $def-if!
  ($fn [Name Test | Fns]
    ($let (Orig  (eval Name))
          (Test  (eval Test))
          (F     ($fn Args
                   ($if-error (Test | Args)
                     (~ -> Orig | Args)
                     (X -> X))))
      (eval [$def! Name F | Fns]))))
|#
