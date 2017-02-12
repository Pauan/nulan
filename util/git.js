import $nodegit from "nodegit";


const _clone = async (url, path, opts) => {
  try {
    return await $nodegit["Clone"](url, path, opts);

  // TODO better error checking
  } catch (e) {
    return await $nodegit["Repository"]["open"](path);
  }
};


export const clone = (url, path) =>
  _clone(url, path);


export const clone_local = (from, to) =>
  // TODO is this correct ?
  _clone(from["workdir"](), to, {
    "local": $nodegit["Clone"]["LOCAL"]["LOCAL"]
  });


export const get_path = (repo) =>
  repo["workdir"]();


export const get_tags = (repo) =>
  $nodegit["Tag"]["list"](repo);


export const fetch = (repo) =>
  // TODO is this correct ?
  repo["fetch"]("origin");


// TODO this is hacky...
export const checkout_tag = async (repo, name) => {
  const tag = await repo["getTagByName"](name);

  const commit = await $nodegit["Commit"]["lookup"](repo, tag["targetId"]());

  const tree = await commit["getTree"]();

  await $nodegit["Checkout"]["tree"](repo, tree, {
    // TODO is this correct ?
    "checkoutStrategy": $nodegit["Checkout"]["STRATEGY"]["SAFE"] |
                        $nodegit["Checkout"]["STRATEGY"]["RECREATE_MISSING"]
  });

  // TODO is this needed ?
  await repo["setHeadDetached"](tag["targetId"]());
};
