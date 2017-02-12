(DYNAMIC a
  $flatten :: (-> (a (a b)) (a b)))

(DYNAMIC ($flatten a)
  $flatten :: (-> (a (a b)) (a b)))

(DYNAMIC ($flatten a) ::
  (-> (a (a b)) (a b)))

(DYNAMIC ($flatten a) :: (-> (a (a b)) (a b)))

(DYNAMIC ($flatten a)
  (-> (a (a b)) (a b)))

(GENERIC (flatten a)
  (-> (a (a b)) (a b)))

(GENERIC ($flatten a)
  (-> (a (a b)) (a b)))

(PROTOCOL ($flatten a)
  flatten :: (-> (a (a b)) (a b)))


(PROVIDE
  flatten)

(PROVIDE
  $flatten)

(PROVIDE ($flatten a)
  flatten)


(PROVIDE
  flatten <= foo)

(PROVIDE
  $flatten <= foo)

(PROVIDE ($flatten a)
  flatten)
