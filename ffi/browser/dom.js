import { make_thread_pool, kill_thread_pool, run_in_thread_pool,
         async_killable } from "../task";
import { crash } from "../crash";


const kill_all = (running) => {
  const length = running["length"];

  for (let i = 0; i < length; ++i) {
    running[i]();
  }
};

const each = (running, observe, f) =>
  running["push"](observe(f));


let root_style = null;

const get_root_style = () => {
  if (root_style === null) {
    root_style = document["createElement"]("style");
    root_style["type"] = "text/css";
    document["head"]["appendChild"](root_style);
  }

  return root_style;
};

const insert_rule = (rule) => {
  // TODO test if this works in all browsers
  const root_sheet = get_root_style()["sheet"];
  const css_rules = root_sheet["cssRules"];

  // TODO test if this works in all browsers
  const index = root_sheet["insertRule"](rule + " {}", css_rules["length"]);

  // TODO test if this works in all browsers
  return css_rules[index];
};


// TODO remove the stylesheet when it is errored or killed ?
// TODO check for duplicate stylesheets
export const stylesheet = (name, rules) =>
  async_killable((success, error) => {
    const running = [];

    const rule = insert_rule(name);

    // TODO check for duplicate styles
    set_attributes(running, rule["style"], rules);

    return () =>
      kill_all(running);
  });


// TODO remove the stylesheet when it is errored or killed ?
// TODO check for duplicate keyframes
export const keyframes = (name, rules) =>
  async_killable((success, error) => {
    const running = [];

    // TODO escape the name ?
    const rule = insert_rule("@keyframes " + name);

    set_attributes(running, rule, rules);

    return () =>
      kill_all(running);
  });


const get_transform = (style) => {
  // TODO handle this better
  // TODO verify that style["transform"] is ""
  if (style.__transforms__ == null) {
    // TODO is it faster to use an object ?
    style.__transforms__ = [];
  }

  return style.__transforms__;
};

// TODO test this
const special_styles_transform_change = (style, name, value, visible) => {
  const transforms = get_transform(style);

  const length = transforms["length"];

  const out = [];

  let seen = false;

  for (let i = 0; i < length; ++i) {
    const x = transforms[i];

    if (x.a === name) {
      seen = true;
      x.b = value;
      x.c = visible;
    }

    if (x.c) {
      out["push"](x.a + "(" + x.b + ")");
    }
  }

  if (!seen) {
    transforms["push"]({ a: name, b: value, c: visible });

    if (visible) {
      out["push"](name + "(" + value + ")");
    }
  }

  style["transform"] = out["join"](" ");
};

const special_styles_transform_set = (style, name, value) =>
  special_styles_transform_change(style, name, value, true);

const special_styles_transform_remove = (style, name) =>
  special_styles_transform_change(style, name, "", false);

const special_styles_remove = {
  "translateX": special_styles_transform_remove,
  "translateY": special_styles_transform_remove,
  "translateZ": special_styles_transform_remove,
  "scaleX": special_styles_transform_remove,
  "scaleY": special_styles_transform_remove,
  "scaleZ": special_styles_transform_remove,
  "rotate": special_styles_transform_remove,
  "rotateX": special_styles_transform_remove,
  "rotateY": special_styles_transform_remove,
  "rotateZ": special_styles_transform_remove,
  "skewX": special_styles_transform_remove,
  "skewY": special_styles_transform_remove,
  "perspective": special_styles_transform_remove
};

const special_styles_set = {
  "translateX": special_styles_transform_set,
  "translateY": special_styles_transform_set,
  "translateZ": special_styles_transform_set,
  "scaleX": special_styles_transform_set,
  "scaleY": special_styles_transform_set,
  "scaleZ": special_styles_transform_set,
  "rotate": special_styles_transform_set,
  "rotateX": special_styles_transform_set,
  "rotateY": special_styles_transform_set,
  "rotateZ": special_styles_transform_set,
  "skewX": special_styles_transform_set,
  "skewY": special_styles_transform_set,
  "perspective": special_styles_transform_set
};

// TODO test this
const remove_style = (style, name) => {
  const x = special_styles_remove[name];

  if (x == null) {
    // TODO set it to "" instead ?
    return style["removeProperty"](name);

  } else {
    return x(style, name);
  }
};

// TODO check that the property and value is valid
// TODO vendor prefixes
// TODO can this be made faster ?
// TODO test this
const set_style = (style, name, value) => {
  const x = special_styles_set[name];

  if (x == null) {
    // TODO is it faster to use this or setProperty ?
    style[name] = value;

  } else {
    return x(style, name, value);
  }
};


// TODO check for duplicate frames
const set_frame = (running, keyframes, attr) => {
  const css_rules = keyframes["cssRules"];

  keyframes["appendRule"](attr.b + "% {}");

  const rule = css_rules[css_rules["length"] - 1];

  return set_attributes(running, rule["style"], attr.c);
};

export const frame = (b, c) =>
  ({ a: set_frame, b, c });


const style_style = (running, style, attr) =>
  set_style(style, attr.b, attr.c);

export const style = (b, c) =>
  ({ a: style_style, b, c });


const style_changing_style = (running, style, attr) =>
  each(running, attr.c, (maybe) => {
    switch (maybe.$) {
    // *none
    case 0:
      return remove_style(style, attr.b);

    // *some
    default:
      return set_style(style, attr.b, maybe.a);
    }
  });

export const changing_style = (b, c) =>
  ({ a: style_changing_style, b, c });


const make_event_pool = (running) => {
  // TODO what about if it errors ?
  // TODO is this needed ?
  const pool = make_thread_pool(crash);

  // TODO remove the event listener ?
  running["push"](() =>
    kill_thread_pool(pool));

  return pool;
};


// TODO test this
const event_event = (running, x, event) => {
  const f = event.c;

  const pool = make_event_pool(running);

  return x["addEventListener"](event.b, (e) =>
    run_in_thread_pool(pool, f({ a: x })), true);
};

export const event = (b, c) =>
  ({ a: event_event, b, c });


const event_on_create = (running, x, event) => {
  const f = event.b;

  // TODO does this need a thread pool or can it use a single thread ?
  const pool = make_event_pool(running);

  return run_in_thread_pool(pool, f({ a: x }));
};

// TODO is this a good idea ?
export const on_create = (b) =>
  ({ a: event_on_create, b });


// TODO test this
const event_on_left_click = (running, x, event) => {
  const f = event.b;

  const pool = make_event_pool(running);

  return x["addEventListener"]("click", (e) => {
    // TODO test this
    if (e["button"] === 0) {
      // TODO preventDefault ?
      return run_in_thread_pool(pool, f({ a: x }));
    }
  }, true);
};

export const on_left_click = (b) =>
  ({ a: event_on_left_click, b });


// TODO test this
const event_on_mouse_hover = (running, x, event) => {
  const f = event.b;
  const pool = make_event_pool(running);

    // TODO check for descendents
  x["addEventListener"]("mouseover", (e) =>
    run_in_thread_pool(pool, f({
      a: x,
      b: true
    })), true);

  // TODO check for descendents
  return x["addEventListener"]("mouseout", (e) =>
    run_in_thread_pool(pool, f({
      a: x,
      b: false
    })), true);
};

export const on_mouse_hover = (b) =>
  ({ a: event_on_mouse_hover, b });


// TODO test this
const event_on_mouse_hold = (running, x, event) => {
  const f = event.b;
  const pool = make_event_pool(running);

  const mouseup = (e) => {
    removeEventListener("mouseup", mouseup, true);

    return run_in_thread_pool(pool, f({
      a: x,
      b: false
    }));
  };

  return x["addEventListener"]("mousedown", (e) => {
    // TODO use the blur event as well ?
    addEventListener("mouseup", mouseup, true);

    return run_in_thread_pool(pool, f({
      a: x,
      b: true
    }));
  }, true);
};

export const on_mouse_hold = (b) =>
  ({ a: event_on_mouse_hold, b });


// TODO if the DOM isn't dirty, should this return immediately ?
export const get_position = (a) =>
  async_killable((success, error) => {
    // TODO use separate read/write queues ?
    const id = requestAnimationFrame(() => {
      const x = a["getBoundingClientRect"]();

      return success({
        a: x["left"],
        b: x["top"],
        c: x["width"],
        d: x["height"]
      });
    });

    return () =>
      cancelAnimationFrame(id);
  });


// TODO duplicate attr check
const attribute_attr = (running, x, attr) =>
  x["setAttribute"](attr.b, attr.c);

export const attr = (b, c) =>
  ({ a: attribute_attr, b, c });


// TODO duplicate class check
// http://jsperf.com/classname-vs-classlist-multiple-classes/2
const attribute_classes = (running, x, attr) => {
  const a = x["className"];
  const b = attr.b["join"](" ");

  if (a === "") {
    x["className"] = b;
  } else {
    x["className"] = a + " " + b;
  }
};

export const classes = (b) =>
  ({ a: attribute_classes, b });


// TODO duplicate class check
const attribute_changing_class = (running, x, attr) =>
  each(running, attr.c, (a) => {
    // *false
    if (a === 0) {
      return x["classList"]["remove"](attr.b);

    // *true
    } else {
      return x["classList"]["add"](attr.b);
    }
  });

export const changing_class = (b, c) =>
  ({ a: attribute_changing_class, b, c });


// TODO duplicate event check
const attribute_events = (running, x, attr) =>
  set_attributes(running, x, attr.b);

export const events = (b) =>
  ({ a: attribute_events, b });


// TODO duplicate attr check
const attribute_changing_attr = (running, x, attr) =>
  each(running, attr.c, (maybe) => {
    switch (maybe.$) {
    // *none
    case 0:
      return x["removeAttribute"](attr.b);

    // *some
    default:
      return x["setAttribute"](attr.b, maybe.a);
    }
  });

export const changing_attr = (b, c) =>
  ({ a: attribute_changing_attr, b, c });


// TODO duplicate style check
const attribute_styles = (running, x, attr) =>
  set_attributes(running, x["style"], attr.b);

export const styles = (b) =>
  ({ a: attribute_styles, b });


// TODO duplicate attribute checks ?
const set_attributes = (running, x, a) => {
  const length = a["length"];

  for (let i = 0; i < length; ++i) {
    const b = a[i];
    b.a(running, x, b);
  }
};


const set_children_list = (running, x, a) => {
  const length = a["length"];

  if (length !== 0) {
    // TODO is it faster or slower to use a document fragment ?
    const fragment = document["createDocumentFragment"]();

    for (let i = 0; i < length; ++i) {
      const b = a[i];
      fragment["appendChild"](b.a(running, b));
    }

    return x["appendChild"](fragment);
  }
};


const push_children = (x, a) => {
  const length = a["length"];

  // TODO test this
  const children = new Array(length);

  if (length !== 0) {
    // TODO is it faster or slower to use a document fragment ?
    const fragment = document["createDocumentFragment"]();

    for (let i = 0; i < length; ++i) {
      const running = [];

      children[i] = running;

      const b = a[i];
      fragment["appendChild"](b.a(running, b));
    }

    x["appendChild"](fragment);
  }

  return children;
};

// TODO test this
const insert_before = (children, x, index, a) => {
  const running = [];

  // TODO test this
  if (index === children["length"]) {
    children["push"](running);

    return x["appendChild"](a.a(running, a));

  } else {
    children["splice"](index, 0, running);

    return x["insertBefore"](a.a(running, a), x["childNodes"][index]);
  }
};

// TODO test this
const replace_child = (children, x, index, a) => {
  const running = [];

  kill_all(children[index]);
  children[index] = running;

  return x["replaceChild"](a.a(running, a), x["childNodes"][index]);
};

// TODO test this
const remove_child = (children, x, index) => {
  kill_all(children[index]);

  children["splice"](index, 1);

  return x["removeChild"](x["childNodes"][index]);
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

  // TODO test this
  running["push"](kill);

  return each(running, a, (a) => {
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
      return insert_before(children, x, a.a, a.b);

    // *update
    case 2:
      return replace_child(children, x, a.a, a.b);

    // *remove
    default:
      return remove_child(children, x, a.a);
    }
  });
};


const easing_step = (a) => {
  switch (a) {
  // *step-start
  case 0:
    return "start";
  // *step-end
  default:
    return "end";
  }
};

const easing = (a) => {
  switch (a.$) {
  // *linear
  case 0:
    return "linear";
  // *ease
  case 1:
    return "ease";
  // *ease-in
  case 2:
    return "ease-in";
  // *ease-in-out
  case 3:
    return "ease-in-out";
  // *ease-out
  case 4:
    return "ease-out";
  // *steps
  case 5:
    return "steps(" + a.a + "," + easing_step(a.b) + ")";
  // *cubic-bezier
  default:
    return "cubic-bezier(" + a.a + "," + a.b + "," + a.c + "," + a.d + ")";
  }
};

const get_animations = (animations) => {
  const output = {
    a: [],
    b: [],
    c: [],
    d: []
  };

  const length = animations["length"];

  for (let i = 0; i < length; ++i) {
    const x = animations[i];

    // The record is inlined into the data constructor
    const state = x.b;

    // TODO escape the name ?
    // TODO use an animation-fill of `both` ?
    const s = (x.a + " " +
               state.a + "ms " +
               easing(state.b) + " " +
               (state.$ === 0
                 ? "normal"
                 : "reverse"));

    switch (x.$) {
    // *animate-set
    case 0:
      output.a["push"](s);
      break;
    // *animate-insert
    case 1:
      output.b["push"](s);
      break;
    // *animate-update
    case 2:
      output.c["push"](s);
      break;
    // *animate-remove
    default:
      output.d["push"](s);
      break;
    }
  }

  return output;
};

const animated_state = (parent, a) => {
  const running = [];

  const child = a.a(running, a);

  // TODO allow for multiple types of animations to play simultaneously (e.g. insert + update)
  const state = {
    a: child,   // element
    b: running, // running
    c: 0,       // pending_animations
    d: false    // pending_removal
  };

  // TODO use feature testing to detect whether animations are supported or not ?
  child["addEventListener"]("animationend", (e) => {
    if (e["target"] === child) {
      --state.c;

      // TODO test this
      if (state.c === 0) {
        if (state.d) {
          parent["removeChild"](child);
        }

        return remove_style(child["style"], "animation");
      }
    }
  }, true);

  return state;
};

// TODO test this
const set_animation = (state, child, animations) => {
  const length = animations["length"];

  // TODO is this correct ?
  if (length !== 0) {
    state.c = length;

    return set_style(child["style"], "animation", animations["join"](","));
  }
};

const animated_push_children = (animations, parent, a) => {
  const length = a["length"];

  // TODO test this
  const children = new Array(length);

  if (length !== 0) {
    // TODO is it faster or slower to use a document fragment ?
    const fragment = document["createDocumentFragment"]();

    for (let i = 0; i < length; ++i) {
      const state = animated_state(parent, a[i]);
      const child = state.a;

      set_animation(state, child, animations);

      children[i] = state;

      fragment["appendChild"](child);
    }

    parent["appendChild"](fragment);
  }

  return children;
};

// TODO test this
const animated_insert_before = (animations, children, x, index, a) => {
  const state = animated_state(x, a);
  const child = state.a;

  set_animation(state, child, animations);

  // TODO test this
  // TODO use insertAfter ?
  if (index === children["length"]) {
    children["push"](state);

    return x["appendChild"](child);

  } else {
    const old = children[index];

    children["splice"](index, 0, state);

    return x["insertBefore"](child, old.a);
  }
};

// TODO test this
const animated_replace_child = (animations, children, x, index, a) => {
  const old = children[index];

  kill_all(old.b);

  const state = animated_state(x, a);
  const child = state.a;

  set_animation(state, child, animations);

  children[index] = state;

  return x["replaceChild"](child, old.a);
};

// TODO test this
const animated_remove_child = (animations, children, x, index) => {
  const old = children[index];
  const child = old.a;

  kill_all(old.b);

  children["splice"](index, 1);

  // TODO is this correct ?
  // TODO test this
  if (animations["length"] === 0) {
    return x["removeChild"](child);

  } else {
    old.d = true;
    return set_animation(old, child, animations);
  }
};

// TODO test this
const set_animated_children = (running, x, a, b) => {
  let children = null;

  const animations = get_animations(a);

  // TODO should this remove the children from the DOM ?
  // TODO prevent double kill
  const kill = () => {
    const length = children["length"];

    for (let i = 0; i < length; ++i) {
      kill_all(children[i].b);
    }
  };

  // TODO test this
  running["push"](kill);

  return each(running, b, (a) => {
    switch (a.$) {
    // *set
    case 0:
      if (children !== null) {
        kill();
        // TODO is there a faster way to clear the children ?
        x["innerHTML"] = "";
      }

      children = animated_push_children(animations.a, x, a.a);
      break;

    // *insert
    case 1:
      return animated_insert_before(animations.b, children, x, a.a, a.b);

    // *update
    case 2:
      return animated_replace_child(animations.c, children, x, a.a, a.b);

    // *remove
    default:
      return animated_remove_child(animations.d, children, x, a.a);
    }
  });
};


const html_animated_parent = (running, a) => {
  const x = document["createElement"](a.b);
  set_attributes(running, x, a.d);
  set_animated_children(running, x, a.c, a.e);
  return x;
};

export const animated_parent = (b, c, d, e) =>
  ({ a: html_animated_parent, b, c, d, e });


const html_parent = (running, a) => {
  const x = document["createElement"](a.b);
  set_attributes(running, x, a.c);
  set_children_list(running, x, a.d);
  return x;
};

export const parent = (b, c, d) =>
  ({ a: html_parent, b, c, d });


const html_child = (running, a) => {
  const x = document["createElement"](a.b);
  set_attributes(running, x, a.c);
  return x;
};

export const child = (b, c) =>
  ({ a: html_child, b, c });


const html_changing_parent = (running, a) => {
  const x = document["createElement"](a.b);
  set_attributes(running, x, a.c);
  set_changing_children(running, x, a.d);
  return x;
};

export const changing_parent = (b, c, d) =>
  ({ a: html_changing_parent, b, c, d });


const html_text = (running, a) =>
  document["createTextNode"](a.b);

export const text = (b) =>
  ({ a: html_text, b });


const html_changing_text = (running, a) => {
  const x = document["createTextNode"]("");

  each(running, a.b, (text) => {
    // http://jsperf.com/textnode-performance
    x["data"] = text;
  });

  return x;
};

export const changing_text = (b) =>
  ({ a: html_changing_text, b });


export const render = (parent, a) =>
  async_killable((success, error) => {
    const running = [];

    const x = a.a(running, a);

    parent["appendChild"](x);

    return () => {
      kill_all(running);
      // TODO test this
      return parent["removeChild"](x);
    };
  });

export const root = document["body"];
