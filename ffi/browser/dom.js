import { make_thread_run, sync } from "../task";
import { _null } from "../types";


const set_attribute_observe = (x, attr) => {
  // TODO error handling and cancellation
  make_thread_run(attr.a(attr.c, (maybe) =>
    sync(() => {
      switch (maybe.$) {
      // *none
      case 0:
        x["removeAttribute"](attr.b);
        return _null;

      // *some
      case 1:
        x["setAttribute"](attr.b, maybe.a);
        return _null;
      }
    })));
};

const set_attribute_class_observe = (x, attr) => {
  // TODO error handling and cancellation
  make_thread_run(attr.a(attr.b, (a) =>
    sync(() => {
      x["setAttribute"]("class", a["join"](" "));
      return _null;
    })));
};

const set_attribute = (x, attr) => {
  switch (attr.$) {
  // *attribute-text
  case 0:
    x["setAttribute"](attr.a, attr.b);
    break;

  // *attribute-class
  case 1:
    x["setAttribute"]("class", attr.a["join"](" "));
    break;

  // *attribute-observe
  case 2:
    set_attribute_observe(x, attr);
    break;

  // *attribute-class-observe
  case 3:
    set_attribute_class_observe(x, attr);
    break;
  }
};

// TODO duplicate attribute checks ?
const set_attributes = (x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    set_attribute(x, a[i]);
  }
};


const set_children_list = (x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    x["appendChild"](html(a[i]));
  }
};

const set_children_observe = (x, a) => {
  // TODO error handling and cancellation
  make_thread_run(a.a(a.b, (a) =>
    sync(() => {
      // TODO cleanup the running observers of the children
      x["innerHTML"] = "";

      for (let i = 0; i < a["length"]; ++i) {
        x["appendChild"](html(a[i]));
      }

      return _null;
    })));
};

const set_children = (x, a) => {
  switch (a.$) {
  // *children-list
  case 0:
    set_children_list(x, a.a);
    break;

  // *children-observe
  case 1:
    set_children_observe(x, a);
    break;
  }
};


const html_parent = (a) => {
  const x = document["createElement"](a.a);
  set_attributes(x, a.b);
  set_children(x, a.c);
  return x;
};

const html_void = (a) => {
  const x = document["createElement"](a.a);
  set_attributes(x, a.b);
  return x;
};

const html_observe = (a) => {
  const x = document["createTextNode"]("");

  // TODO error handling and cancellation
  make_thread_run(a.a(a.b, (text) =>
    sync(() => {
      // TODO is this correct ?
      x["textContent"] = text;
      return _null;
    })));

  return x;
};

const html = (a) => {
  switch (a.$) {
  // *parent
  case 0:
    return html_parent(a);

  // *void
  case 1:
    return html_void(a);

  // *text
  case 2:
    return document["createTextNode"](a.a);

  // *observe
  case 3:
    return html_observe(a);
  }
};


const render = (parent, a) =>
  sync(() => {
    parent["appendChild"](html(a));
    return _null;
  });


const bar = "__style1__";
const qux = "__style2__";
const corge = "__style3__";

const observe = (a, b) =>
  b(a);

const always = (a) => a;

make_thread_run(render(document.body, {
  $: 0,
  a: "div",
  b: [
    { $: 0, a: "foo", b: "bar" },
    { $: 2, a: observe, b: "src", c: always({ $: 1, a: "nou" }) },
    { $: 1, a: [ bar, qux, corge ] }
  ],
  c: {
    $: 0,
    a: [
      { $: 2, a: "Hi" }
    ]
  }
}));
