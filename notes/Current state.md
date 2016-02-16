<div style='background-color: #0f171f; color: #7ea9d7;'><pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>F</span>oo a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #b54a4a;'>C</span>olor</pre>
<pre style='margin: 0px; position: relative;'>  *red</pre>
<pre style='margin: 0px; position: relative;'>  *orange</pre>
<pre style='margin: 0px; position: relative;'>  *yellow</pre>
<pre style='margin: 0px; position: relative;'>  *green</pre>
<pre style='margin: 0px; position: relative;'>  *blue</pre>
<pre style='margin: 0px; position: relative;'>  *indigo</pre>
<pre style='margin: 0px; position: relative;'>  *violet<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #b54a4a;'>RGB</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>*rgba <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>L</span>ist a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  *empty</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>*value a <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>L</span>ist a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #b54a4a;'>F</span>oo</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REQUIRE</span> <span style='color: #4d769f;'>(</span>$foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>*foo a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROTOCOL</span> <span style='color: #4d769f;'>(</span>$transform a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  transform :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #4d769f;'>(</span>a b<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> b c<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span>a c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>CONSTANT</span></pre>
<pre style='margin: 0px; position: relative;'>  after :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REQUIRE</span> <span style='color: #4d769f;'>(</span>$transform a<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span>$flatten a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>             <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #4d769f;'>(</span>a b<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> b <span style='color: #4d769f;'>(</span>a c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span>a c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  after <span style='color: #b54a4a;'><=</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a fn</pre>
<pre style='margin: 0px; position: relative;'>             <span style='color: #4d769f;'>(</span>flatten <span style='color: #4d769f;'>(</span>transform a fn<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> after :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REQUIRE</span> <span style='color: #4d769f;'>(</span>$transform a<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span>$flatten a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>                     <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #4d769f;'>(</span>a b<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> b <span style='color: #4d769f;'>(</span>a c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>(</span>a c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>after a fn<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>flatten <span style='color: #4d769f;'>(</span>transform a fn<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IMPORT</span> <span style='color: #4d769f;'>(</span>github <span style='color: #4d769f;'>{</span> name <span style='color: #b54a4a;'><=</span> <span style='color: #eac06c;'>"nulan/flatten"</span></pre>
<pre style='margin: 0px; position: relative;'>                  file <span style='color: #b54a4a;'><=</span> <span style='color: #eac06c;'>"src/flatten.nul"</span></pre>
<pre style='margin: 0px; position: relative;'>                  version <span style='color: #b54a4a;'><=</span> <span style='color: #4d769f;'>(</span>major 2<span style='color: #4d769f;'>)</span> <span style='color: #4d769f;'>}</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  $flatten</pre>
<pre style='margin: 0px; position: relative;'>  flatten<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IMPORT</span> <span style='color: #4d769f;'>(</span>file <span style='color: #eac06c;'>"foo/bar"</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> d</pre>
<pre style='margin: 0px; position: relative;'>  b</pre>
<pre style='margin: 0px; position: relative;'>  c</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #b54a4a;'>M</span>aybe</pre>
<pre style='margin: 0px; position: relative;'>  *some</pre>
<pre style='margin: 0px; position: relative;'>  *none</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROVIDE</span> <span style='color: #4d769f;'>(</span>$flatten <span style='color: #b54a4a;'>M</span>aybe<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    flatten<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROVIDE</span> <span style='color: #4d769f;'>(</span>$transform <span style='color: #b54a4a;'>M</span>aybe<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    transform <span style='color: #b54a4a;'><=</span> map<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROVIDE</span> <span style='color: #4d769f;'>(</span>$yield <span style='color: #b54a4a;'>M</span>aybe<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    yield<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>EXPORT</span></pre>
<pre style='margin: 0px; position: relative;'>  d <span style='color: #b54a4a;'><=</span> a</pre>
<pre style='margin: 0px; position: relative;'>  b</pre>
<pre style='margin: 0px; position: relative;'>  c</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #b54a4a;'>M</span>aybe</pre>
<pre style='margin: 0px; position: relative;'>  *some</pre>
<pre style='margin: 0px; position: relative;'>  *none<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IF</span> test</pre>
<pre style='margin: 0px; position: relative;'>  then</pre>
<pre style='margin: 0px; position: relative;'>  else<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>EXPORT-CONSTANT</span></pre>
<pre style='margin: 0px; position: relative;'>  foo <span style='color: #b54a4a;'><=</span> *foo<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IMPORT-BUILTINS</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IMPORT</span> <span style='color: #4d769f;'>(</span>nulan <span style='color: #eac06c;'>"unsafe/ffi"</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #b54a4a;'>UNSAFE-FFI-IMPORT</span></pre>
<pre style='margin: 0px; position: relative;'>  javascript<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-FFI-IMPORT</span> <span style='color: #4d769f;'>{</span> target <span style='color: #b54a4a;'><=</span> javascript</pre>
<pre style='margin: 0px; position: relative;'>                     file <span style='color: #b54a4a;'><=</span> <span style='color: #eac06c;'>"foo/bar"</span> <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'>  a :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  b :: <span style='color: #b54a4a;'>I</span>nteger</pre>
<pre style='margin: 0px; position: relative;'>  c :: <span style='color: #b54a4a;'>I</span>nteger</pre>
<pre style='margin: 0px; position: relative;'>  d :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>F</span>oo <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> foo<| :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a <span style='color: #b54a4a;'>F</span>oo<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>foo<| a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    ...<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> <|foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>F</span>oo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><|foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    ...<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>CONSTANT</span></pre>
<pre style='margin: 0px; position: relative;'>  foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span> <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  foo <span style='color: #b54a4a;'><=</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a <span style='color: #4d769f;'>(</span>a 1 2<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>LOCAL</span></pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>+ a 2<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>LOCAL</span></pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> 2</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>+ a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>LOOP</span> loop</pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> 2</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>loop a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span> <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>a 1 2<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>T</span>ext <span style='color: #b54a4a;'>T</span>ext<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> bar :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>bar 1<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    2</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>bar a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>+ <span style='color: #4d769f;'>(</span>bar 1<span style='color: #4d769f;'>)</span> a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REWRITE-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>QUX</span> ~@a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    &<span style='color: #4d769f;'>(</span>+ ~@a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MUTUALLY-RECURSIVE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> even? :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>B</span>oolean<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>even? 0<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      true</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>even? a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      <span style='color: #4d769f;'>(</span>odd? <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span> a 1<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> odd? :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>B</span>oolean<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>odd? 0<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      false</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>odd? a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      <span style='color: #4d769f;'>(</span>even? <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span> a 1<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MUTUALLY-RECURSIVE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROTOCOL</span> <span style='color: #4d769f;'>(</span>$foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REQUIRE</span> <span style='color: #4d769f;'>(</span>$bar a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>             <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PROTOCOL</span> <span style='color: #4d769f;'>(</span>$bar a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    bar :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REQUIRE</span> <span style='color: #4d769f;'>(</span>$foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>             <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MUTUALLY-RECURSIVE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #b54a4a;'>F</span>oo</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>*foo <span style='color: #b54a4a;'>B</span>ar<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TYPE</span> <span style='color: #b54a4a;'>B</span>ar</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>*bar <span style='color: #b54a4a;'>F</span>oo<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MUTUALLY-RECURSIVE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REWRITE-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FOO</span> ~n <span style='color: #b54a4a;'><=</span> ~v<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      &<span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>BAR</span> ~n ~v<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FOO</span> ~v<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      &<span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>BAR</span> ~v<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REWRITE-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>BAR</span> ~a ~@b<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MATCH</span> a</pre>
<pre style='margin: 0px; position: relative;'>        &~n <span style='color: #b54a4a;'><=</span> ~v</pre>
<pre style='margin: 0px; position: relative;'>          &<span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>QUX</span> ~n ~v ~@b<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>        v</pre>
<pre style='margin: 0px; position: relative;'>          &<span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>QUX</span> 1 ~v ~@b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span>foo <span style='color: #b54a4a;'>-</span>> a b <span style='color: #4d769f;'>(</span>+ a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span>foo <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a b <span style='color: #4d769f;'>(</span>+ a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #b54a4a;'>FOO</span></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FOO</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FOO</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #4d769f;'>(</span><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FOO</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MUTUALLY-RECURSIVE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REWRITE-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSTREAM</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>STREAM</span> ~a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      a</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSTREAM</span> ~a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      &<span style='color: #4d769f;'>(</span>unstream ~a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>REWRITE-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>STREAM</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSTREAM</span> ~a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      a</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>STREAM</span> ~a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      &<span style='color: #4d769f;'>(</span>stream ~a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FLATTEN-TRANSFORM</span></pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> a</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> b</pre>
<pre style='margin: 0px; position: relative;'>  c<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FLATTEN-TRANSFORM</span></pre>
<pre style='margin: 0px; position: relative;'>  x <span style='color: #b54a4a;'><=</span> <span style='color: #4d769f;'>(</span>read<span style='color: #b54a4a;'>-</span>file <span style='color: #eac06c;'>"foo"</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>log x<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>write<span style='color: #b54a4a;'>-</span>file <span style='color: #eac06c;'>"bar"</span> x<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>yield null<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>TRANSFORM</span></pre>
<pre style='margin: 0px; position: relative;'>  a <span style='color: #b54a4a;'><=</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> 2</pre>
<pre style='margin: 0px; position: relative;'>  c <span style='color: #b54a4a;'><=</span> 3</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>+ a b c<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MATCHES</span> <span style='color: #4d769f;'>[</span> a b c <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>[</span> 1 2 3 <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'>    1</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>[</span> 1 2 a <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'>    2</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>[</span> 1 a b <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'>    3</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>[</span> a b c <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'>    4<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>[</span> 1 2 3 <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>[</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  2</pre>
<pre style='margin: 0px; position: relative;'>  3 <span style='color: #4d769f;'>]</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>{</span> a b <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>{</span> a <span style='color: #b54a4a;'><=</span> 1 b <span style='color: #b54a4a;'><=</span> 2 <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>{</span> a <span style='color: #b54a4a;'><=</span> 1</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> 2 <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>{</span> @a</pre>
<pre style='margin: 0px; position: relative;'>  b <span style='color: #b54a4a;'><=</span> 2</pre>
<pre style='margin: 0px; position: relative;'>  c <span style='color: #b54a4a;'><=</span> 3 <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MATCH</span> a</pre>
<pre style='margin: 0px; position: relative;'>  _</pre>
<pre style='margin: 0px; position: relative;'>    1</pre>
<pre style='margin: 0px; position: relative;'>  a</pre>
<pre style='margin: 0px; position: relative;'>    2</pre>
<pre style='margin: 0px; position: relative;'>  1</pre>
<pre style='margin: 0px; position: relative;'>    3</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #eac06c;'>"foo"</span></pre>
<pre style='margin: 0px; position: relative;'>    4</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>{</span> a b c <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'>    5</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>{</span> a <span style='color: #b54a4a;'><=</span> b c <span style='color: #b54a4a;'><=</span> d <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>{</span> b <span style='color: #b54a4a;'><=</span> a d <span style='color: #b54a4a;'><=</span> c <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>*foo 1<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    6<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #b54a4a;'>U</span>nsure</pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>::</pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>PRAGMA</span> <span style='color: #4d769f;'>{</span> phase <span style='color: #b54a4a;'><=</span> run<span style='color: #b54a4a;'>-</span>time</pre>
<pre style='margin: 0px; position: relative;'>          target <span style='color: #b54a4a;'><=</span> javascript <span style='color: #4d769f;'>}</span></pre>
<pre style='margin: 0px; position: relative;'>  foo<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>METADATA</span> impure inline<span style='color: #b54a4a;'>-</span>function synchronous</pre>
<pre style='margin: 0px; position: relative;'>  foo<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>INLINE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> foo bar<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MATCH</span> a</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> view a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    9<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>MATCH</span> a</pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>LOCAL</span></pre>
<pre style='margin: 0px; position: relative;'>    a <span style='color: #b54a4a;'><=</span> a</pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>equal? a 1<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    9<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'># foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>POLYMORPH</span></pre>
<pre style='margin: 0px; position: relative;'>#          <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>#          <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>T</span>ext <span style='color: #b54a4a;'>T</span>ext<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>POLYMORPHIC</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> foo :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>T</span>ext <span style='color: #b54a4a;'>T</span>ext<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>foo a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>      a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>FUNCTION</span> bar :: <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>POLYMORPH</span></pre>
<pre style='margin: 0px; position: relative;'>                   <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>I</span>nteger <span style='color: #b54a4a;'>I</span>nteger<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>                   <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> <span style='color: #b54a4a;'>T</span>ext <span style='color: #b54a4a;'>T</span>ext<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>bar a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>foo a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-DEFAULT-PROVIDE</span> <span style='color: #4d769f;'>(</span>$transform <span style='color: #b54a4a;'>M</span>aybe<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  transform <span style='color: #b54a4a;'><=</span> map<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-DEFAULT-PROVIDE</span> <span style='color: #4d769f;'>(</span>$transform <span style='color: #b54a4a;'>M</span>aybe<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  transform <span style='color: #b54a4a;'><=</span> <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>-</span>> a a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>IMPORT</span> <span style='color: #4d769f;'>(</span>nulan <span style='color: #eac06c;'>"unsafe"</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>after a b<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>flatten <span style='color: #4d769f;'>(</span>transform a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>reduce<span style='color: #b54a4a;'>-</span>left <span style='color: #4d769f;'>[</span><span style='color: #4d769f;'>]</span> a <span style='color: #b54a4a;'>-</span>> b c <span style='color: #4d769f;'>(</span>push b d<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span>reduce<span style='color: #b54a4a;'>-</span>left <span style='color: #4d769f;'>[</span><span style='color: #4d769f;'>]</span> a <span style='color: #b54a4a;'>-</span>> b c <span style='color: #4d769f;'>(</span>unsafe<span style='color: #b54a4a;'>-</span>push! b d<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>unstream <span style='color: #4d769f;'>(</span>stream a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>stream <span style='color: #4d769f;'>(</span>unstream a<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    a<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre>
<pre style='margin: 0px; position: relative;'><span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>UNSAFE-OPTIMIZATION-RULE</span></pre>
<pre style='margin: 0px; position: relative;'>  <span style='color: #4d769f;'>(</span>add a b<span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'>    <span style='color: #4d769f;'>(</span><span style='color: #b54a4a;'>ADD</span> a b<span style='color: #4d769f;'>)</span><span style='color: #4d769f;'>)</span></pre>
<pre style='margin: 0px; position: relative;'></pre></div>