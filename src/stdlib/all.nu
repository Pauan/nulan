($mac def -> name f
  `(var name = f))

(type Something)

(type Nothing)

(def maybe -> x def
  (match x
    (isa Something value)
      value
    (isa Nothing)
      def))

(def self -> x x)

(def do -> @_ _ x x)

; TODO this might not be needed
(mac def-async -> name `(-> @args body)
  `(def name -> ,@args (async body)))
