##############################################################################
#  Core
##############################################################################

$set! $quote: $vau ~ [X] X

$set! $fn; $vau Env [Args | Body]
  wrap: eval Env [$vau ~ Args | Body]

$set! null? : X -> is X []

# => ($let X)
#    X
# => ($let (X Y) ...)
#    ((X -> ($let ...)) Y)
$set! $let; $vau Env [X | R]
  eval Env: $if: null? R
              X
              # Equivalent to ($let: [X Y] X; ...)
              :[X Y] -> [[X -> : $let | R] Y]
               X

$set! $or; $vau Env [X | R]
  $let: X: eval Env X
    $if X X: eval Env [$or | R]

$set! any: [X | R] F -> $or: F X; any R F

$set! case: X | Fns -> any Fns: F -> F | X

$set! $def! ; $vau Env [Name | Fns]
  $let: Args: uniq
    eval Env [$set! Name: $fn Args: case Args | Fns]

$def! not: X -> $if X %f %t

# TODO: type needs to work with varargs like (type | list?)
$def! type; $fn Fns
  $fn Args
    any: zip Fns Args; [X Y] ->
      $if: not: X Y
        error 'type check failed on argument @Y'

$def! list? : X -> $or: cons? X; null? X

# TODO: map needs to type check on any list, including dotted
$def! map: type list? fn?
  [X | R] F -> [(F X) | (map R F)]
  X       ~ -> X

$set! $defvau! ; $vau Env [Name | Fns]
  $let: Args: uniq
    eval Env [$set! Name: $vau ($quote %Env) Args: case Args | Fns]

##############################################################################
#  $use
##############################################################################

$def! each: type list? fn?
  [X | R] F -> F X; each R F

$set! get-current-env: wrap: $vau Env [] Env

$def! make-env
  Env -> eval Env [[$vau ~ ~ [get-current-env]]]

$def! make-base-env


$defvau! $use
  | Args -> each Args; X ->
              $let: New: make-base-env
                $hook New %env-get-if-unbound
                  K -> eval K New
                load-file-in New: find-file X
                eval [$set! (strip-extension:basename X) New]

##############################################################################
#  Other utilities
##############################################################################

$defvau! $and
  [X]     -> eval X
  [X | R] -> $if: eval X; eval [$and | R]

$def! all: type list? fn?
  [X | R] F -> $and: F X; all R F

$def! none: type list? fn?
  X F -> all X: X -> not: F X


# Logical functions on functions
$def! fnfn: type fn?
  F -> | Fns -> | Args -> F Fns: X -> X | Args

$def notfn: type | fn? ; fnfn none
$def andfn: type | fn? ; fnfn all
$def orfn:  type | fn? ; fnfn any


$def! fold:  type list? fn?
  [X Y | R] F -> fold [(F X Y) | R] F
  X         ~ -> X

$def! foldr: type list? fn?
  [X Y | R] F -> F X: F Y: foldr R F
  X         ~ -> X


$def! keep: type list? fn?
  [X | R] F -> $if: F X
                 [X | (keep R F)]
                 keep R F
  X       ~ -> X

$def! rem: type list? fn?
  X F -> keep X: notfn F


$def! list: | R -> [| R]
$def! car: [X | R] -> X
$def! cdr: [X | R] -> R

# => (zip [a 1] [b 2] [c 3])
#    [[a b c] [1 2 3]]
$def! zip: type | list?
  | Args -> $if: some Args null?
              []
              [(map Args car) | (zip | (map Args cdr))]


$def! join: type list? ~
  [X | R] Y -> [X | (join R Y)]
  [X]     Y -> [X | Y]


$def! ref: type list? ~
  [K V | R] K -> V
  [~ ~ | R] K -> ref R K


# TODO: maybe make it take rest args?
$def! iso
  X        X        -> %t
  [X | R1] [Y | R2] -> $and: iso X Y; iso R1 R2


$def! id:   X -> X
$def! copy: X -> map X id

#|
$def! pair: type list?
  [X Y | R] I -> [[X Y] | (pair R I)]
  [X]       I -> [[X I]]
  X           -> pair X []
|#

$defvau! $def-if! ; Name Test | Fns ->
  $let; Orig:  eval Name
        Test:  eval Test
        F:     $fn Args: $if-error: Test | Args
                           ~ -> Orig | Args
                           X -> X
    eval [$def! Name F | Fns]
