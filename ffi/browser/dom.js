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

const set_children = (x, a) => {
  switch (a.$) {
  // *children-list
  case 0:
    // TODO
    for (let i = 0; i < a.a["length"]; ++i) {
      x["appendChild"](html(a.a[i]));
    }
    break;

  // *children-observe
  case 1:
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
    break;
  }
};

const html = (a) => {
  switch (a.$) {
  // *parent
  case 0:
    const x1 = document["createElement"](a.a);
    set_attributes(x1, a.b);
    set_children(x1, a.c);
    return x1;

  // *void
  case 1:
    const x2 = document["createElement"](a.a);
    set_attributes(x2, a.b);
    return x2;

  // *text
  case 2:
    return document["createTextNode"](a.a);

  // *observe
  case 3:
    const x3 = document["createTextNode"]("");

    // TODO error handling and cancellation
    make_thread_run(a.a(a.b, (text) =>
      sync(() => {
        // TODO is this correct ?
        x3["textContent"] = text;
        return _null;
      })));

    return x3;
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
