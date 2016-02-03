import { make_thread_pool, kill_thread_pool, run_in_thread_pool, sync,
         async_killable } from "../task";
import { _null } from "../types";


const set_attribute_event = (pool, x, attr) => {
  x["addEventListener"](attr.a, (e) => {
    run_in_thread_pool(pool, attr.b(e));
  }, true);
};

const set_attribute_observe = (pool, x, attr) => {
  run_in_thread_pool(pool, attr.a(attr.c, (maybe) =>
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

const set_attribute_class_observe = (pool, x, attr) => {
  run_in_thread_pool(pool, attr.a(attr.b, (a) =>
    sync(() => {
      x["setAttribute"]("class", a["join"](" "));
      return _null;
    })));
};

const set_attribute_style_observe = (pool, x, attr) => {
  run_in_thread_pool(pool, attr.a(attr.c, (maybe) =>
    sync(() => {
      switch (maybe.$) {
      // *none
      case 0:
        x["style"]["removeProperty"](attr.b);
        return _null;

      // *some
      case 1:
        x["style"]["setProperty"](attr.b, maybe.a, "");
        return _null;
      }
    })));
};

const set_attribute = (pool, x, attr) => {
  switch (attr.$) {
  // *attribute-text
  case 0:
    x["setAttribute"](attr.a, attr.b);
    break;

  // *attribute-class
  case 1:
    x["setAttribute"]("class", attr.a["join"](" "));
    break;

  // *attribute-style-text
  case 2:
    x["style"]["setProperty"](attr.a, attr.b, "");
    break;

  // *attribute-event
  case 3:
    set_attribute_event(pool, x, attr);
    break;

  // *attribute-observe
  case 4:
    set_attribute_observe(pool, x, attr);
    break;

  // *attribute-class-observe
  case 5:
    set_attribute_class_observe(pool, x, attr);
    break;

  // *attribute-style-observe
  case 6:
    set_attribute_style_observe(pool, x, attr);
    break;
  }
};

// TODO duplicate attribute checks ?
const set_attributes = (pool, x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    set_attribute(pool, x, a[i]);
  }
};


const set_children_list = (pool, x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    x["appendChild"](html(pool, a[i]));
  }
};


const push_children = (children, error, x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    // TODO kill all the thread pools when it errors ?
    const _pool = make_thread_pool(error);

    children["push"](_pool);

    x["appendChild"](html(_pool, a[i]));
  }
};

// TODO test this
const insert_before = (children, error, x, index, a) => {
  // TODO kill all the thread pools when it errors ?
  const _pool = make_thread_pool(error);

  // TODO test this
  if (index === children["length"]) {
    children["push"](_pool);

    x["appendChild"](html(_pool, a));

  } else {
    children["splice"](index, 0, _pool);

    x["insertBefore"](html(_pool, a), x["childNodes"][index]);
  }
};

// TODO test this
const replace_child = (children, error, x, index, a) => {
  // TODO kill all the thread pools when it errors ?
  const _pool = make_thread_pool(error);

  kill_thread_pool(children[index]);
  children[index] = _pool;

  x["replaceChild"](html(_pool, a), x["childNodes"][index]);
};

// TODO test this
const remove_child = (children, x, index) => {
  kill_thread_pool(children[index]);

  children["splice"](index, 1);

  x["removeChild"](x["childNodes"][index]);
};

// TODO test this
const set_children_observe_list = (pool, x, observe, a) => {
  // TODO hacky
  run_in_thread_pool(pool, async_killable((success, error) => {
    const children = [];

    // TODO should this remove the children from the DOM ?
    // TODO prevent double kill
    const kill = () => {
      for (let i = 0; i < children["length"]; ++i) {
        kill_thread_pool(children[i]);
      }
    };

    run_in_thread_pool(pool, observe(a, (a) =>
      sync(() => {
        switch (a.$) {
        // *set
        case 0:
          if (children["length"] !== 0) {
            kill();
            children["length"] = 0;
            // TODO is there a faster way to clear the children ?
            x["innerHTML"] = "";
          }

          push_children(children, error, x, a.a);
          break;

        // *insert
        case 1:
          insert_before(children, error, x, a.a, a.b);
          break;

        // *update
        case 2:
          replace_child(children, error, x, a.a, a.b);
          break;

        // *remove
        case 3:
          remove_child(children, x, a.a);
          break;
        }

        return _null;
      })));

    return kill;
  }));
};


const html_parent_list = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  set_children_list(pool, x, a.c);
  return x;
};

const html_parent_observe_list = (pool, a) => {
  const x = document["createElement"](a.b);
  set_attributes(pool, x, a.c);
  set_children_observe_list(pool, x, a.a, a.d);
  return x;
};

const html_void = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  return x;
};

const html_observe = (pool, a) => {
  const x = document["createTextNode"]("");

  run_in_thread_pool(pool, a.a(a.b, (text) =>
    sync(() => {
      // http://jsperf.com/textnode-performance
      x["data"] = text;
      return _null;
    })));

  return x;
};

const html = (pool, a) => {
  switch (a.$) {
  // *parent-list
  case 0:
    return html_parent_list(pool, a);

  // *void
  case 1:
    return html_void(pool, a);

  // *text
  case 2:
    return document["createTextNode"](a.a);

  // *parent-observe-list
  case 3:
    return html_parent_observe_list(pool, a);

  // *observe
  case 4:
    return html_observe(pool, a);
  }
};


const _render = (parent, a) =>
  async_killable((success, error) => {
    // TODO remove from the DOM when an error happens ?
    const pool = make_thread_pool(error);

    const x = html(pool, a);

    parent["appendChild"](x);

    return () => {
      kill_thread_pool(pool);
      // TODO test this
      parent["removeChild"](x);
    };
  });

export const render = (a) =>
  _render(document["body"], a);
