import { blocking } from "./blocking-task";
import * as $list from "./list";
import * as $unsafe_list from "./unsafe/list";
import { trigger } from "./mutable";

export { get, mutable as mutable_list } from "./mutable";


export const set = (mutable, a) =>
  blocking(() => {
    // TODO does this affect the semantics ?
    // TODO test this
    if (mutable.a !== a) {
      mutable.a = a;

      trigger(mutable.b, {
        $: 0,
        a: a
      });
    }

    return null;
  });


export const push = (mutable, a) =>
  blocking(() => {
    // TODO slightly hacky
    const index = mutable.a["length"];

    mutable.a = $list.push(mutable.a, a);

    trigger(mutable.b, {
      $: 1,
      a: index,
      b: a
    });

    return null;
  });


export const insert = (mutable, index1, a) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"] + 1);

    // TODO slightly inefficient
    mutable.a = $list.insert(mutable.a, index2, a);

    trigger(mutable.b, {
      $: 1,
      a: index2,
      b: a
    });

    return null;
  });


export const update = (mutable, index1, a) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    // TODO slightly inefficient
    const b = $list.update(mutable.a, index2, a);

    // TODO test this
    // TODO does this affect the semantics ?
    if (mutable.a !== b) {
      mutable.a = b;

      trigger(mutable.b, {
        $: 2,
        a: index2,
        b: a
      });
    }

    return null;
  });


export const remove = (mutable, index1) =>
  blocking(() => {
    // TODO very slightly hacky
    // TODO what about errors ?
    const index2 = $list.get_index(index1, mutable.a["length"]);

    // TODO slightly inefficient
    mutable.a = $list.remove(mutable.a, index2);

    trigger(mutable.b, {
      $: 3,
      a: index2
    });

    return null;
  });


export const changing_list_from = (mutable) =>
  (push) => {
    push({
      $: 0,
      a: mutable.a
    });

    mutable.b["push"](push);

    return () =>
      $unsafe_list.remove_element(mutable.b, push);
  };


export const changing_from = (mutable) =>
  (push) => {
    const on_change = () =>
      push(mutable.a);

    on_change();

    mutable.b["push"](on_change);

    return () =>
      $unsafe_list.remove_element(mutable.b, on_change);
  };
