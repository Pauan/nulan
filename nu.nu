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

$set! case-fn; @Fns ->
  @Args -> case Args @Fns

$set! $case-vau; $vau Env [E @Fns]
  $let Args: uniq
    eval Env [$vau E Args
               [$let ($quote eval) [case-fn
                                     [Args] [eval E Args]
                                     Args   [eval @Args]]
                 [case Args @Fns]]]

$set! $defv!; $vau Env [Name E @Fns]
  eval Env [$def! Name: $case-vau E @Fns]

$defv! $defn Env; @Fns ->
  eval [$def! Name: case-fn @Fns]

#$set! $defn!; $vau Env [Name @Fns]
#  eval Env [$def! Name: case-fn @Fns]

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
#|
$defv! $ref!
  X Y -> eval [$set! X: ref Y]
  X   -> eval [$ref! X %f]
|#

$defn! get-current-env: wrap: $vau Env [] Env

$defn! make-env; Env ->
  eval Env [[$vau ~ ~ : get-current-env]]

#$defn! make-env: X -> wrapped-var: X

#make-env

$defn! listify: X -> $if: seq? X; X [X]

$defv! $include! Env; X @Body ->
  $let New: make-env Env
    eval New [$do @Body]
    each: listify X; X ->
      eval [$set! X (eval New [$get X])]
      #set! Env X: eval New [$get X]
      # (New) X

$defv! $exclude! Env; X @Body ->
  $let X (each: listify X; X ->
           [X (eval [$get X])])
           #((Env) X)
    eval [$do @Body]
    each X: [L R] -> eval [$set! L R]
                     #set! Env L R


#$defn! make-env: Env -> var: Env

#|
$ref! make-base-env

$let Top: get-current-env
  $defn! make-base-env
    -> make-env Top
|#
#$ref! make-base-env

#$defn! make-base-env: -> (get-current-env)

$defv! $until Env; X Y ->
  $let X2: eval X
    $if: is? X2 Y
      []
      $lazy [X2 @(eval [$until X Y])]

$defv! $while Env; X ->
  eval [$until X %f]


$defn! path; @Args ->
  sum Args; X Y ->
    $let X: expand-path X
      $if: is? (first Y) "/"
             Y
           is? (last X) "/"
             "@X@Y "
           "@X/ @Y  "

$defn! abspath: @Args -> path cwd @Args

#|
(def path args
  (reduce (fn (x y)
            (let x (expand-path x)
              (if (is (y 0) #\/)
                    y
                  (is (x (- (len x) 1)) #\/)
                    (string x y)
                  (string x "/" y))))
          args))

$defn! path; casefn
  "/@X " @R -> "/@X/@(path R) "
  "/@X " @R -> "/@X@(path R) "
  X      @R -> $let Y: expand-path X
                 $if: is? X Y
                   "@Y@(path R) "
                   path Y R
              $ifis? X (expand-path X) Y
                "@X@(path R) "
                path X R
              $ifis? (eval X) Y
                []
                $lazy []
|#

# TODO make it a primitive
$defn! file-exists?; X ->
  $catch: read-file X: ~ -> X
    (%not-found ~) -> %f

$var! file-paths [cwd load-dir]

$defn! find-file; X ->
  any file-paths; Y ->
    $let P: abspath X Y
      $or: file-exists? P
           file-exists? "@P.nu "

$defn! load-file; X ->
  read-file X; I -> $while: parse I

$defn! load-file-in; Env X ->
  each: load-file X; X -> eval Env X

$defv! $use! Env; @Args ->
  each Args; X ->
    load-file-in Env: find-file X
    (Env)

#$let X 5
#  $let Y: get-current-env
#    eval Y: $quote X
#5
#|
$defv! $use! Env; @Args ->
  each Args; X ->
    $let New: make-base-env
      $hook Env get-if-unbound
        K -> eval New K
      load-file-in New: find-file X
      eval [$set! (strip-extension: basename X) New]
|#

##############################################################################
#  Other utilities
##############################################################################
$defv! $do Env: @Body -> eval [[$fn [] @Body]]

# Could be implemented in terms of each, but it would be inefficient
$defn! eachf
  [X @R] F -> F X; eachf R F
  ~      ~ -> %f

$defn! not: X -> $if X %f %t

#|
$defn! type; @Fns ->
  $if: seq? Fns
    @Args -> any?: zip Fns Args; [X Y] ->
               $if: not: X Y
                 type-error "type check failed on argument @Y"
    @Args -> any? Args; X ->
               $if: not: Fns X
                 type-error "type check failed on argument @Y"
|#

$defv! $and Env
  X    -> eval X
  X @R -> $if: eval X
            eval [$and @R]

$defn! all?
  [X]    F -> F X
  [X @R] F -> $and: F X; all? R F

$defn! none?; X F ->
  all? X: X -> not: F X


# Logical functions on functions
$defn! fn-fn; F ->
  @Fns -> @Args -> F Fns: X -> X @Args

$defn! fn-not: fn-fn none?
$defn! fn-and: fn-fn all?
$defn! fn-or:  fn-fn any?

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
$defn! sum
  [X @R] F   -> sum1 X R F
  I      X F -> sum1 I X F

$defn! sum1
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
$defn! sumr
  [X @R] F   -> F X: sumr R F
  X      ~   -> X
  X      I F -> sumr1 X I F

$defn! sumr1
  [X @R] I F -> F X: sumr R I F
  ~      I ~ -> I

#|
[|Foo |Bar]

"foo |bar"
|#


#|
$defn! map
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
$defn! compose; @Fns ->
  sum Fns; X Y ->
    @Args -> X: Y @Args

#|
(def compose fns
  (reduce (fn (x y) (fn args (x (apply y args)))) fns))

(def foldl (f a bs)
  (.a (foldr (fn (b g) [g:f _ b]) idfn bs)))

(def foldl (f a bs)
  ((foldr (fn (b g) (fn (x) (g (f x b)))) idfn bs) a))
|#


$defn! inter
  [X @R] I -> [X I @(inter R)]
  X      ~ -> X


# Called map in other languages.
#|
# It's easier to just define it explicitly, rather than using cons + sumr.
$defn! each
  [X @R] F -> [(F X) @(each R F)]
  X      ~ -> X
|#
$defn! each; X F ->
  sumr X []: X Y -> [(F X) @Y]

$defn! rev; X ->
  sum [] X: Y X -> [X @Y]

#|

# mapreduce
$defn! sumeach
  I []      ~ ~ -> I
  I [X]     F ~ -> (F X)
  I [@L @R] F G -> G: sumeach I L F G
                      sumeach I R F G

# map
$defn! each
  X F -> sumeach [] X F tree

$defn! id: X -> X
#$defn! copy: X -> each X id

# reduce
$defn! sum; I X F ->
  sumeach I X id F


$defn! first
  []      -> []
  [X]     -> X
  [@L @R] -> first L

$defn! rest
  []      -> []
  [X]     -> []
  [@L @R] -> [@(rest L) @R]

$defn! each
  []      ~ -> []
  [X]     F -> F X
  [@L @R] F -> [@(each L F) @(each R F)]

|#

#|
$defn! foldr; F I X ->
  sum I X: K X -> K: F X

$defn! sum1
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
$defn! cons: X Y -> [X @Y]
|#


#|
$defn! sum
  [X Y @R] F -> sum1 [(F X Y) @R] F
  X        ~ -> X
|#

#|
$defn! keep
  [X @R] F -> $if: F X
                [X @(keep R F)]
                keep R F
  X      ~ -> X
|#

$defn! keep; X F ->
  sumr X []; X Y ->
    $if: F X
      [X @Y]
      Y

#|
(def keep (x f)
  (rreduce (fn (x y) (prn x " " y) (if (f x) (cons x y) y)) x))

(keep '(1 2 3 4 5) (fn (x) (> x 3)))
|#

$defn! rem: X F -> keep X: fn-not F


$defn! empty?: []     -> %t
$defn! first:  [X @~] -> X
$defn! rest:   [~ @R] -> R

# => (zip [a 1] [b 2] [c 3])
#    [[a b c] [1 2 3]]
$defn! zip; @Args ->
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
$defn! join
  [X @R] Y -> [X @(join R Y)]
  [X]    Y -> [X @Y]

# TODO could use a better name than joinr
$defn! joinr: X Y -> join X [Y]

#|
$defn! ref
  [K V @R] K -> V
  [~ ~ @R] K -> ref R K
|#

#|
# TODO maybe make it take rest args?
$defn! iso?
  X       X       -> %t
  [X @R1] [Y @R2] -> $and: iso? X Y; iso? R1 R2
|#

$defn! prn!; @Args ->
  pr! @Args
  pr! "\n"

$defn! writen!; @Args ->
  write! @Args
  pr! "\n"

#|
$defn! pair
  [X Y @R] I -> [[X Y] @(pair R I)]
  [X]      I -> [[X I]]
  X          -> pair X []
|#

$defv! $lets Env
  [X]        -> eval X
  [[X Y] @R] -> eval [$let X Y: $lets @R]


$defn! with1
  Env [X]        -> eval Env X
  Env [[X Y] @R] -> set! Env X Y; with1 Env R

$defv! $with Env; @Args ->
  with1 (make-env Env) Args


$defv! $if-error Env; X Y @R ->
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
$defv! $def-if!; Name Test @Fns ->
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
$defv! $lazy Env; X ->
  $lets: U:      uniq
         Saved:  var U
    call-on-eval; ->
      $if: is? Saved U
        $set-var! Saved: eval X
        Saved


$defv! $get Env: X -> (Env) X

$defv! $get-var Env; X ->
  $let X: var: eval [$get X] #(Env) X
    $if: var? X
      X
      throw: error "expected var but got @X " #TODO

#$def! $set-var!; vau Env [X Y]
#  eval Env [set-var! [$get-var X] Y]

#|
$def! $set!: vau Env [X Y]:


$var! bar
$set! bar: var

$predef! bar
  $def! foo ...
  $def! bar ...

$set! bar: var
$def! foo ...
$def! bar ...
$set! bar bar
|#

# Dynamically rebinds a var while within the $let-var's body:
#
#   => $var! foo 1
#   1
#
#   => $let-var foo 5
#        foo
#   5
#
#   => foo
#   1
$defv! $let-var Env; X Y @Body ->
  $lets: Old:  var
         X:    eval [$get-var X]
         Y:    eval Y
    eval [dynamic-wind
           ($fn [] ($set-var! Old X)
                   ($set-var! X Y))
           [$fn [] @Body]
           ($fn [] ($set-var! X Old))]


# Evaluates the expression X
#
# When the expression exits (either normally or due to a
# continuation/exception), expression Y will be evaluated
#
# This is used for cleanup code, e.g. closing a file after
# performing some operations on it
$defv! $after Env; X Y ->
  $let Seen: var %f
    eval [dynamic-wind
           ($fn []
             ($if Seen
               (throw: error "cannot re-enter an $after")
               ($set! Seen %t)))
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


($defv! $def-if!
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

($defv! $def-if!
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

($defv! $def-if!
  ($fn [Name Test | Fns]
    ($let (Orig  (eval Name))
          (Test  (eval Test))
          (F     ($fn Args
                   ($if-error (Test | Args)
                     (~ -> Orig | Args)
                     (X -> X))))
      (eval [$def! Name F | Fns]))))
|#
