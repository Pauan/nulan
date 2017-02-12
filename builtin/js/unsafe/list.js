import { crash, error_from } from "../crash";


export const unsafe_push = (array, x) => {
  array.push(x);
  return array;
};


export const unsafe_remove_element = (array, x) => {
  const i = a.indexOf(x);

  if (i === -1) {
    return crash(error_from("Cannot remove: element not found"));

  } else {
    return a.splice(i, 1);
  }
};
