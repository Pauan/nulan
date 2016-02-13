import { make_thread_pool, kill_thread_pool, run_in_thread_pool,
         async_killable } from "../task";
import { crash } from "../../util/error";


const kill_all = (running) => {
  const length = running["length"];

  for (let i = 0; i < length; ++i) {
    running[i]();
  }
};

const each = (running, observe, f) => {
  running["push"](observe(f));
};


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
    const running = [];

    const rule = insert_rule(name);

    set_attribute_styles(running, rule["style"], rules);

    return () => {
      kill_all(running);
    };
  });


const escape_class = (s) => {
  if (s["length"] === 0) {
    crash(new Error("class cannot be empty"));
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


// TODO check that the property and value is valid
// TODO vendor prefixes
const set_style = (style, name, value) => {
  style[name] = value;
};

const set_changing_style = (running, style, attr) => {
  each(running, attr.b, (maybe) => {
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


const make_event_pool = (running) => {
  // TODO what about if it errors ?
  // TODO is this needed ?
  const pool = make_thread_pool(crash);

  // TODO remove the event listener ?
  running["push"](() => {
    kill_thread_pool(pool);
  });

  return pool;
};

// TODO test this
const set_attribute_event = (running, x, name, f) => {
  const pool = make_event_pool(running);

  x["addEventListener"](name, (e) => {
    run_in_thread_pool(pool, f(e));
  }, true);
};

const set_on_click = (running, x, f) => {
  set_attribute_event(running, x, "click", (e) =>
    f({}));
};

const set_on_hover = (running, x, f) => {
  const pool = make_event_pool(running);

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
const set_on_hold = (running, x, f) => {
  const pool = make_event_pool(running);

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

const set_event = (running, x, attr) => {
  switch (attr.$) {
  // *attribute-event
  case 0:
    set_attribute_event(running, x, attr.a, attr.b);
    break;

  // *on-click
  case 1:
    set_on_click(running, x, attr.a);
    break;

  // *on-hover
  case 2:
    set_on_hover(running, x, attr.a);
    break;

  // *on-hold
  case 3:
    set_on_hold(running, x, attr.a);
    break;
  }
};

const set_attribute_events = (running, x, a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    set_event(running, x, a[i]);
  }
};


const set_changing_attribute = (running, x, attr) => {
  each(running, attr.b, (maybe) => {
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

const set_attribute_changing_class = (running, x, attr) => {
  each(running, attr.b, (a) => {
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
const set_attribute_styles = (running, style, styles) => {
  const length = styles["length"];

  for (let i = 0; i < length; ++i) {
    const a = styles[i];

    switch (a.$) {
    // *style
    case 0:
      set_style(style, a.a, a.b);
      break;
    // *changing-style
    case 1:
      set_changing_style(running, style, a);
      break;
    }
  }
};


const set_attribute = (running, x, attr) => {
  switch (attr.$) {
  // *attribute-text
  case 0:
    x["setAttribute"](attr.a, attr.b);
    break;

  // *attribute-class
  case 1:
    x["classList"]["add"](attr.a);
    break;

  // *attribute-changing-class
  case 2:
    set_attribute_changing_class(running, x, attr);
    break;

  // *attribute-events
  case 3:
    set_attribute_events(running, x, attr.a);
    break;

  // *attribute-styles
  case 4:
    set_attribute_styles(running, x["style"], attr.a);
    break;

  // *attribute-changing-text
  case 5:
    set_changing_attribute(running, x, attr);
    break;
  }
};

// TODO duplicate attribute checks ?
const set_attributes = (running, x, a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    set_attribute(running, x, a[i]);
  }
};


const set_children_list = (running, x, a) => {
  const length = a["length"];

  // TODO is it faster or slower to use a document fragment ?
  const fragment = document["createDocumentFragment"]();

  for (let i = 0; i < length; ++i) {
    fragment["appendChild"](html(running, a[i]));
  }

  x["appendChild"](fragment);
};


const push_children = (x, a) => {
  const length = a["length"];

  // TODO test this
  const children = new Array(length);

  // TODO is it faster or slower to use a document fragment ?
  const fragment = document["createDocumentFragment"]();

  for (let i = 0; i < length; ++i) {
    const running = [];

    children[i] = running;

    fragment["appendChild"](html(running, a[i]));
  }

  x["appendChild"](fragment);

  return children;
};

// TODO test this
const insert_before = (children, x, index, a) => {
  const running = [];

  // TODO test this
  if (index === children["length"]) {
    children["push"](running);

    x["appendChild"](html(running, a));

  } else {
    children["splice"](index, 0, running);

    x["insertBefore"](html(running, a), x["childNodes"][index]);
  }
};

// TODO test this
const replace_child = (children, x, index, a) => {
  const running = [];

  kill_all(children[index]);
  children[index] = running;

  x["replaceChild"](html(running, a), x["childNodes"][index]);
};

// TODO test this
const remove_child = (children, x, index) => {
  kill_all(children[index]);

  children["splice"](index, 1);

  x["removeChild"](x["childNodes"][index]);
};

// TODO test this
const set_changing_children = (running, x, a) => {
  let children = null;

  // TODO should this remove the children from the DOM ?
  // TODO prevent double kill
  const kill = () => {
    const length = children["length"];

    for (let i = 0; i < length; ++i) {
      kill_all(children[i]);
    }
  };

  each(running, a, (a) => {
    switch (a.$) {
    // *set
    case 0:
      if (children !== null) {
        kill();
        // TODO is there a faster way to clear the children ?
        x["innerHTML"] = "";
      }

      children = push_children(x, a.a);
      break;

    // *insert
    case 1:
      insert_before(children, x, a.a, a.b);
      break;

    // *update
    case 2:
      replace_child(children, x, a.a, a.b);
      break;

    // *remove
    case 3:
      remove_child(children, x, a.a);
      break;
    }
  });

  // TODO test this
  running["push"](kill);
};


const html_parent_list = (running, a) => {
  const x = document["createElement"](a.a);
  set_attributes(running, x, a.b);
  set_children_list(running, x, a.c);
  return x;
};

const html_changing_parent = (running, a) => {
  const x = document["createElement"](a.a);
  set_attributes(running, x, a.b);
  set_changing_children(running, x, a.c);
  return x;
};

const html_child = (running, a) => {
  const x = document["createElement"](a.a);
  set_attributes(running, x, a.b);
  return x;
};

const html_changing_text = (running, a) => {
  const x = document["createTextNode"]("");

  each(running, a.a, (text) => {
    // http://jsperf.com/textnode-performance
    x["data"] = text;
  });

  return x;
};

const html = (running, a) => {
  switch (a.$) {
  // *parent-list
  case 0:
    return html_parent_list(running, a);

  // *child
  case 1:
    return html_child(running, a);

  // *text
  case 2:
    return document["createTextNode"](a.a);

  // *changing-text
  case 3:
    return html_changing_text(running, a);

  // *changing-parent
  case 4:
    return html_changing_parent(running, a);
  }
};


export const render = (parent, a) =>
  async_killable((success, error) => {
    const running = [];

    const x = html(running, a);

    parent["appendChild"](x);

    return () => {
      kill_all(running);
      // TODO test this
      parent["removeChild"](x);
    };
  });

export const root = document["body"];
