import { make_thread_pool, kill_thread_pool, run_in_thread_pool, sync,
         async_killable } from "../task";
import { _null } from "../types";


const root_style = document["createElement"]("style");
root_style["type"] = "text/css";
document["head"]["appendChild"](root_style);

// TODO test if this works in all browsers
const root_sheet = root_style["sheet"];
const css_rules = root_sheet["cssRules"];

const insert_rule = (rule) => {
  // TODO test if this works in all browsers
  const index = root_sheet["insertRule"](rule + " {}", css_rules["length"]);

  // TODO test if this works in all browsers
  return css_rules[index];
};


// TODO remove the stylesheet when it is errored or killed ?
export const stylesheet = (name, rules) =>
  async_killable((success, error) => {
    const pool = make_thread_pool(error);

    const rule = insert_rule(name);

    set_attribute_styles(pool, rule["style"], rules);

    return () => {
      kill_thread_pool(pool);
    };
  });


/*let class_id = 0;

export const make_class = (rules) => {
  const class_name = "__" + (++class_id);

  stylesheet("." + class_name, rules);

  return class_name;
};*/


// TODO check that the property and value is valid
// TODO vendor prefixes
const set_style = (pool, style, a) => {
  style[a.a] = a.b;
};

// TODO check that the property and value is valid
// TODO vendor prefixes
const set_style_observe = (pool, style, a) => {
  run_in_thread_pool(pool, a.a(a.c, (maybe) =>
    sync(() => {
      switch (maybe.$) {
      // *none
      case 0:
        // TODO set it to "" instead ?
        style["removeProperty"](attr.b);
        return _null;

      // *some
      case 1:
        style[attr.b] = maybe.a;
        return _null;
      }
    })));
};


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

const set_attribute_classes_observe = (pool, x, attr) => {
  run_in_thread_pool(pool, attr.a(attr.b, (a) =>
    sync(() => {
      x["className"] = a["join"](" ");
      return _null;
    })));
};

// TODO duplicate style checks ?
const set_attribute_styles = (pool, style, styles) => {
  for (let i = 0; i < styles["length"]; ++i) {
    const a = styles[i];

    switch (a.$) {
    // *style
    case 0:
      set_style(pool, style, a);
      break;
    // *style-observe
    case 1:
      set_style_observe(pool, style, a);
      break;
    }
  }
};

const set_attribute = (pool, x, attr) => {
  switch (attr.$) {
  // *attribute-text
  case 0:
    x["setAttribute"](attr.a, attr.b);
    break;

  // *attribute-classes
  case 1:
    x["className"] = attr.a["join"](" ");
    break;

  // *attribute-event
  case 2:
    set_attribute_event(pool, x, attr);
    break;

  // *attribute-styles
  case 3:
    set_attribute_styles(pool, x["style"], attr.a);
    break;

  // *attribute-text-observe
  case 4:
    set_attribute_observe(pool, x, attr);
    break;

  // *attribute-classes-observe
  case 5:
    set_attribute_classes_observe(pool, x, attr);
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

const html_child = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  return x;
};

const html_text_observe = (pool, a) => {
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

  // *child
  case 1:
    return html_child(pool, a);

  // *text
  case 2:
    return document["createTextNode"](a.a);

  // *parent-observe-list
  case 3:
    return html_parent_observe_list(pool, a);

  // *text-observe
  case 4:
    return html_text_observe(pool, a);
  }
};


export const render = (parent, a) =>
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

export const root = document["body"];
