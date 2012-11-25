@>>>
  > (var foo 10)
  > var foo;
    foo = 10
  > 10

@>>>
  > (var foo 20)
  > var foo2;
    foo2 = 20
  > 20

@>>>
  > (var foo2 30)
  > var foo22;
    foo22 = 30
  > 30

@>>>
  > foo
  > foo2
  > 20

@>>>
  > foo2
  > foo22
  > 30


@>>>
  > (var foo 10)
  > var foo3;
    foo3 = 10
  > 10

@>>>
  > (del foo)
  > foo3
  > 10

@>>>
  > (var foo 20)
  > var foo4;
    foo4 = 20
  > 20


@>>>c
  > (var + 5)
  > var _43_2;
    _43_2 = 5

@>>>c
  > (var ++yoobar% 5)
  > var _43__43_yoobar_37_;
    _43__43_yoobar_37_ = 5

@>>>c
  > (var 0yoobar 5)
  > var _48_yoobar;
    _48_yoobar = 5

@>>>c
  > (var _yoobar 5)
  > var __yoobar;
    __yoobar = 5


@>>>
  > (var + (fn))
  > var _43_3;
    _43_3 = function () {}
  > function () {}

@>>>
  > (+ 1 2)
  > _43_3(1, 2)
  > undefined
