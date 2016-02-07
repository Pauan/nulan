import { make_thread_pool, kill_thread_pool, run_in_thread_pool, sync,
         async_killable } from "../task";
import { _null } from "../types";
import { crash } from "../../util/error";


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


const escape_class = (s) => {
  if (s["length"] === 0) {
    crash(new Error("Class cannot be empty"));
  }

  const out = [];

  // TODO test the performance of this
  switch (s[0]) {
  case "0":
  case "1":
  case "2":
  case "3":
  case "4":
  case "5":
  case "6":
  case "7":
  case "8":
  case "9":
    out["push"]("\\00003");
    break;
  }

  const length = s["length"];

  for (let i = 0; i < length; ++i) {
    const c = s[i];

    // TODO test the performance of this
    switch (c) {
    case "!":
    case "\"":
    case "#":
    case "$":
    case "%":
    case "&":
    case "'":
    case "(":
    case ")":
    case "*":
    case "+":
    case ",":
    case "-":
    case ".":
    case "/":
    case ":":
    case ";":
    case "<":
    case "=":
    case ">":
    case "?":
    case "@":
    case "[":
    case "\\":
    case "]":
    case "^":
    case "`":
    case "{":
    case "|":
    case "}":
    case "~":
    case " ":
    case "_":
      out["push"]("\\");
      out["push"](c);
      break;

    case "\t":
    case "\n":
    case "\v":
    case "\f":
    case "\r":
      out["push"]("\\");
      out["push"](c["charCodeAt"](0)["toString"](16));
      out["push"](" ");
      break;

    default:
      out["push"](c);
      break;
    }
  }

  return out["join"]("");
};

export const stylesheet_class = (name, rules) =>
  stylesheet("." + escape_class(name), rules);


/*let class_id = 0;

export const make_class = (rules) => {
  const class_name = "__" + (++class_id);

  stylesheet("." + class_name, rules);

  return class_name;
};*/


const each = (pool, stream, f) => {
  run_in_thread_pool(pool, stream(_null, (a, b) =>
    sync(() => {
      f(b);
      return a;
    })));
};


// TODO check that the property and value is valid
// TODO vendor prefixes
const set_style = (style, name, value) => {
  style[name] = value;
};

const set_style_stream = (pool, style, attr) => {
  each(pool, attr.b, (maybe) => {
    switch (maybe.$) {
    // *none
    case 0:
      // TODO set it to "" instead ?
      style["removeProperty"](attr.a);
      break;

    // *some
    case 1:
      set_style(style, attr.a, maybe.a);
      break;
    }
  });
};


const set_attribute_event = (pool, x, name, f) => {
  x["addEventListener"](name, (e) => {
    run_in_thread_pool(pool, f(e));
  }, true);
};

const set_on_click = (pool, x, f) => {
  x["addEventListener"]("click", (e) => {
    run_in_thread_pool(pool, f({}));
  }, true);
};

const set_on_hover = (pool, x, f) => {
  // TODO check for descendents
  x["addEventListener"]("mouseover", (e) => {
    run_in_thread_pool(pool, f({
      a: true
    }));
  }, true);

  // TODO check for descendents
  x["addEventListener"]("mouseout", (e) => {
    run_in_thread_pool(pool, f({
      a: false
    }));
  }, true);
};

// TODO test this
const set_on_hold = (pool, x, f) => {
  const mouseup = (e) => {
    removeEventListener("mouseup", mouseup, true);

    run_in_thread_pool(pool, f({
      a: false
    }));
  };

  x["addEventListener"]("mousedown", (e) => {
    // TODO use the blur event as well ?
    addEventListener("mouseup", mouseup, true);

    run_in_thread_pool(pool, f({
      a: true
    }));
  }, true);
};

const set_event = (pool, x, attr) => {
  switch (attr.$) {
  // *attribute-event
  case 0:
    set_attribute_event(pool, x, attr.a, attr.b);
    break;

  // *on-click
  case 1:
    set_on_click(pool, x, attr.a);
    break;

  // *on-hover
  case 2:
    set_on_hover(pool, x, attr.a);
    break;

  // *on-hold
  case 3:
    set_on_hold(pool, x, attr.a);
    break;
  }
};

const set_attribute_events = (pool, x, a) => {
  for (let i = 0; i < a["length"]; ++i) {
    set_event(pool, x, a[i]);
  }
};


const set_attribute_stream = (pool, x, attr) => {
  each(pool, attr.b, (maybe) => {
    switch (maybe.$) {
    // *none
    case 0:
      x["removeAttribute"](attr.a);
      break;

    // *some
    case 1:
      x["setAttribute"](attr.a, maybe.a);
      break;
    }
  });
};

const set_attribute_class_stream = (pool, x, attr) => {
  each(pool, attr.b, (a) => {
    // *false
    if (a === 0) {
      x["classList"]["remove"](attr.a);

    // *true
    } else {
      x["classList"]["add"](attr.a);
    }
  });
};

// TODO duplicate style checks ?
const set_attribute_styles = (pool, style, styles) => {
  for (let i = 0; i < styles["length"]; ++i) {
    const a = styles[i];

    switch (a.$) {
    // *style
    case 0:
      set_style(style, a.a, a.b);
      break;
    // *style-stream
    case 1:
      set_style_stream(pool, style, a);
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

  // *attribute-class
  case 1:
    x["classList"]["add"](attr.a);
    break;

  // *attribute-class-stream
  case 2:
    set_attribute_class_stream(pool, x, attr);
    break;

  // *attribute-events
  case 3:
    set_attribute_events(pool, x, attr.a);
    break;

  // *attribute-styles
  case 4:
    set_attribute_styles(pool, x["style"], attr.a);
    break;

  // *attribute-text-stream
  case 5:
    set_attribute_stream(pool, x, attr);
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
const set_children_stream = (pool, x, a) => {
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

    each(pool, a, (a) => {
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
    });

    return kill;
  }));
};


const html_parent_list = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  set_children_list(pool, x, a.c);
  return x;
};

const html_parent_stream = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  set_children_stream(pool, x, a.c);
  return x;
};

const html_child = (pool, a) => {
  const x = document["createElement"](a.a);
  set_attributes(pool, x, a.b);
  return x;
};

const html_text_stream = (pool, a) => {
  const x = document["createTextNode"]("");

  each(pool, a.a, (text) => {
    // http://jsperf.com/textnode-performance
    x["data"] = text;
  });

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

  // *text-stream
  case 3:
    return html_text_stream(pool, a);

  // *parent-stream
  case 4:
    return html_parent_stream(pool, a);
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
