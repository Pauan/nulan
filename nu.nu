##############################################################################
#  Core
##############################################################################

#$set! $fn: $vau Env [Args | Body]:
#  wrap: eval Env [$vau ~ Args | Body]

$set! $let: $vau Env [X Y | R]:
  eval Env [[$fn [seq X] | R] Y]

$set! $quote: $vau ~ [X] X

$set! $or: $vau Env [X | R]:
  $let X: eval Env X
    $if X X: eval Env [$or | R]

$set! any [X | R] F -> $or: F X; any R F

$set! case: X | Fns -> any Fns: F -> F | X

$set! $defvau! : $vau Env [Name | Fns]:
  $let Args (uniq)
    eval Env [$set! Name
               [$vau ($quote %Env) Args
                 [case Args | Fns]]]

$defvau! $def! : Name | Fns ->
  $let Args (uniq)
    eval [$set! Name
           [$fn Args
             [case Args | Fns]]]

##############################################################################
#  $use
##############################################################################

$def! each: type seq? fn?
  [X | R] F -> F X; each R F

$set! get-current-env: wrap: $vau Env [] Env

$def! make-env: Env -> eval Env [[$vau ~ ~ [get-current-env]]]

$def! make-base-env


$defvau! $use: | Args ->
  each Args: X ->
    $let New (make-base-env)
      $hook New env-get-if-unbound
        K -> eval K New
      load-file-in New: find-file X
      eval [$set! (strip-extension (basename X)) New]

##############################################################################
#  Other utilities
##############################################################################

$def! not: X -> $if X %f %t

$def! type: | Fns ->
  $if: seq? Fns
    | Args -> any: zip Fns Args; [X Y] ->
                $if: not: X Y
                  error "type check failed on argument @Y"
    | Args -> any Args: X ->
                $if: not: Fns X
                  error "type check failed on argument @Y"

# TODO: map needs to type check on any seq, including dotted
$def! map: type seq? fn?
  [X | R] F -> [(F X) | (map R F)]
  X       ~ -> X

$defvau! do: | Args -> eval [[$fn [] | Args]]

$defvau! $and
  [X]     -> eval X
  [X | R] -> $if: eval X; eval [$and | R]

$def! all: type seq? fn?
  [X | R] F -> $and: F X; all R F

$def! none: type seq? fn?
  X F -> all X: X -> not: F X


# Logical functions on functions
$def! fnfn: type fn?
  F -> | Fns -> | Args -> F Fns: X -> X | Args

$def! notfn: type | fn? ; fnfn none
$def! andfn: type | fn? ; fnfn all
$def! orfn:  type | fn? ; fnfn any


$def! fold:  type seq? fn?
  [X Y | R] F -> fold [(F X Y) | R] F
  X         ~ -> X

$def! foldr: type seq? fn?
  [X Y | R] F -> F X: F Y: foldr R F
  X         ~ -> X


$def! keep: type seq? fn?
  [X | R] F -> $if: F X
                 [X | (keep R F)]
                 keep R F
  X       ~ -> X

$def! rem: type seq? fn?
  X F -> keep X: notfn F


$def! empty? : []      -> %t
$def! first  : [X | ~] -> X
$def! rest   : [~ | R] -> R

# => (zip [a 1] [b 2] [c 3])
#    [[a b c] [1 2 3]]
$def! zip: type | seq?
  | Args -> $if: some Args empty?
              []
              [(map Args first) | (zip | (map Args rest))]


$def! join: type seq? ~
  [X | R] Y -> [X | (join R Y)]
  [X]     Y -> [X | Y]

# TODO: could use a better name than joinr
$def! joinr: type seq? ~
  X Y -> join X [Y]


$def! ref: type seq? ~
  [K V | R] K -> V
  [~ ~ | R] K -> ref R K


# TODO: maybe make it take rest args?
$def! iso?
  X        X        -> %t
  [X | R1] [Y | R2] -> $and: iso? X Y; iso? R1 R2


$def! id:   X -> X
$def! copy: X -> map X id


$def! prn! : | Args ->
  pr! | Args
  pr! "\n"

$def! writen! : | Args ->
  write! | Args
  pr! "\n"

#|
$def! pair: type seq?
  [X Y | R] I -> [[X Y] | (pair R I)]
  [X]       I -> [[X I]]
  X           -> pair X []
|#

$defvau! $lets
  [X]         -> eval X
  [[X Y] | R] -> eval [$let X Y [$lets | R]]

$defvau! $if-error: [X Y | R] ->
  $let U (uniq)
    eval [$on-error X
           [$fn [seq [error ~]] Y] | (joinr R [$fn [seq U] U])]

$defvau! $def-if! : Name Test | Fns ->
  $lets: Orig:  eval Name
         Test:  eval Test
         F:     $fn Args
                  $if-error: Test | Args
                    Orig | Args
    eval [$def! Name F | Fns]

$on-error: Test | Args
  (error ~) -> Orig | Args
  X         -> X


#|
X -> foo bar
  qux corge

$fn [X]: foo bar;
  qux corge

($fn [X]
  (foo bar)
  (qux corge))


{X + {Y * Z}}
/X is? Y/
X `is?` []


#$def! list? : X -> $or: cons? X; null? X


#|
# => ($let X)
#    X
# => ($let (X Y) ...)
#    (($fn [X] ($let ...)) Y)
$set! $let: $vau Env [X Y | R]:
  #eval Env
  #  $if: null? R
  #    X
  #    # Equivalent to ($let [X Y] X ...)
  #    :[X Y] -> [[$fn [X] [$let | R]] Y]
  #     X
  eval Env [[X -> | R] Y]
|#


#|
$set! $let: $vau Env [X Y | R]:
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
