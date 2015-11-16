(TYPE Token
  (*symbol String)
  (*string String)
  (*number Number)
  (*integer Integer))

(ALIAS State =
  { input  :: (Output Char)
  | output :: (Input Token) })

(FUNCTION
  (tokenize-whitespace :: (-> Char State (Task Void)))
  (tokenize-whitespace c state)
    void)

(FUNCTION
  (tokenize-symbol1 :: (-> Char State (Task Void)))
  (tokenize-symbol1 c state)
    (push! state.output (*symbol (cast c))))

(VAR specials =
  (DICT (CHAR " ")  = tokenize-whitespace
      | (CHAR "\n") = tokenize-whitespace
      | (CHAR "(")  = tokenize-symbol1
      | (CHAR ")")  = tokenize-symbol1
      | (CHAR "{")  = tokenize-symbol1
      | (CHAR "}")  = tokenize-symbol1
      | (CHAR "[")  = tokenize-symbol1
      | (CHAR "]")  = tokenize-symbol1
      | (CHAR "@")  = tokenize-symbol1))

(FUNCTION
  (tokenize-symbol-end :: (-> String State (Task Void)))
  (tokenize-symbol-end str state)
    (MATCH (cast-maybe str)
      *none
        (MATCH (cast-maybe str)
          *none
            (push! state.output (*symbol str))
          (*some x)
            (push! state.output (*number x)))
      (*some x)
        (push! state.output (*integer x))))

(FUNCTION
  (tokenize-symbol :: (-> Char State (Task Void)))
  (tokenize-symbol c state)
    (LOOP str = (cast c)
      (DO-MATCH (peek state.input)
        *none
          (tokenize-symbol-end str state)
        (*some c2)
          (MATCH (get specials c2)
            *none
              (DO _ = (pull! state.input)
                  (loop (push str c2)))
            (*some _)
              (tokenize-symbol-end str state)))))

(FUNCTION
  (tokenize1 :: (-> State (Task Void)))
  (tokenize1 state)
    (LOOP
      (DO-MATCH (pull! state.input)
        *none
          void
        (*some c)
          (MATCH (get specials c)
            *none
              (DO (tokenize-symbol c state)
                  (loop))
            (*some f)
              (DO (f c state)
                  (loop))))))

(FUNCTION
  (tokenize :: (-> (Stream Char) (Stream Token)))
  (tokenize input)
    (make-stream -> output
      (with-stream input -> input
        (tokenize1 { input | output }))))
