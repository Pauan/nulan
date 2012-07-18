    M? *  Mutate var if it exists
    M?    Never mutate var

    W? A  Always
    W? E  If var exists
    W?    If var doesn't exist

    C? N  Create new name
    C? V  Create new variable
    C?    Throw an error

    R? *  Second argument is recursive
    R?    Second argument isn't recursive

##############################################################################

       Global  M? W? C? R?
               *  A  N  *
               *  A  N
               *  A  V  *
               *  A  V
               *  E  N  *
               *  E  N
               *  E  V  *
               *  E  V
        $def!  *     N  *  # generic assignment
       $defn!  *     N  *  # recursive functions
       $defv!  *     N  *  # recursive vaus
               *     N
        $var!  *     V  *  # recursive mutable functions
               *     V
               *        *
    $set-var!  *           # variable mutation
       $setr!     A  N  *  # recursive binding
        $set!     A  N     # binding
                  A  V  *
                  A  V
                  E  N  *
                  E  N
                  E  V  *
                  E  V
                     N  *
                     N
                     V  *
                     V

##############################################################################

        Local  M? W? C? R?
               *  A  N  *
               *  A  N
               *  A  V  *
               *  A  V
               *  E  N  *
               *  E  N
               *  E  V  *
               *  E  V
               *     N  *
               *     N
               *     V  *
               *     V
               *        *
     $let-var  *
        $letr     A  N  *
         $let     A  N
                  A  V  *
                  A  V
                  E  N  *
                  E  N
                  E  V  *
                  E  V
                     N  *
                     N
                     V  *
                     V
