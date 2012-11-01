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
               *     N  *
               *     N
               *     V  *
               *     V
               *        *
        $set!  *           # variable mutation
        $defn     A  N  *  # recursive binding
      $defvau     A  N  *  # recursive vau binding
         $def     A  N     # binding
                  A  V  *
        $def!     A  V     # implicit variable creation
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
        $let!  *           # dynamic variable
        $letn     A  N  *  # local recursive binding
         $let     A  N     # local binding
        $lets     A  N     # nested $let
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
