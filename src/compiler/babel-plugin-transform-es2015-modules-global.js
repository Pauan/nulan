const loadFile = (state, file) => {
  if (!(file in state.modules)) {
    state.modules[file] = state.opts.loadFile(file);
  }

  return state.modules[file];
};


const getPrefix = (parent, state, name) =>
  state.opts.importedVariable(name, loadFile(state, parent.source.value));


export default ({ types: $t }) => {
  return {
    visitor: {
      "BindingIdentifier|ReferencedIdentifier": (path, state) => {
        const name = path.node.name;
        const binding = path.scope.getBinding(name);

        // TODO is this correct ?
        if (binding != null) {
          if (binding.kind === "module") {
            const _path = binding.path;

            const parent = _path.parent;

            if ($t.isImportSpecifier(_path)) {
              path.node.name = getPrefix(parent, state, _path.node.imported.name);

            } else if ($t.isImportDefaultSpecifier(_path)) {
              path.node.name = getPrefix(parent, state, "default");
            }

          } else {
            path.node.name = state.opts.localVariable(name);
          }
        }
      },

      MemberExpression: {
        enter: (path, state) => {
          const object = path.node.object;
          const property = path.node.property;

          if ($t.isIdentifier(object)) {
            const name = object.name;

            const binding = path.scope.getBinding(name);

            if (binding != null &&
                binding.kind === "module" &&
                $t.isImportNamespaceSpecifier(binding.path)) {

              const parent = binding.path.parent;

              // TODO check node.computed ?
              if ($t.isStringLiteral(property)) {
                // TODO source maps ?
                path.replaceWith($t.identifier(getPrefix(parent, state, property.value)));

              // TODO test this
              } else if (!path.node.computed && $t.isIdentifier(property)) {
                // TODO source maps ?
                path.replaceWith($t.identifier(getPrefix(parent, state, property.name)));

              } else {
                const file = loadFile(state, parent.source.value);

                object.name = state.opts.namespaceFrom(file);
              }
            }
          }
        }
      },

      ImportDeclaration: {
        exit: (path, state) => {
          path.remove();
        }
      },

      ExportAllDeclaration: {
        enter: (path, state) => {
          const file = loadFile(state, path.node.source.value);

          state.opts.exportAllFrom(file);
        },
        exit: (path, state) => {
          path.remove();
        }
      },

      ExportDefaultDeclaration: {
        enter: (path, state) => {
          state.opts.export("default", "default");
        },
        exit: (path, state) => {
          // TODO source maps ?
          // TODO use something other than var ?
          path.replaceWith($t.variableDeclaration("var", [
            $t.variableDeclarator(
              $t.identifier(state.opts.localVariable("default")),
              path.node.declaration
            )
          ]));
        }
      },

      ExportSpecifier: {
        enter: (path, state) => {
          const source = path.parent.source;
          const local = path.node.local.name;
          const exported = path.node.exported.name;

          if (source === null) {
            state.opts.export(local, exported);

          } else {
            const file = loadFile(state, source.value);

            state.opts.exportFrom(file, local, exported);
          }
        }
      },

      // TODO handle pattern matches ?
      VariableDeclarator: {
        enter: (path, state) => {
          const parent = path.parentPath.parent;

          // TODO is there a better way of doing this ?
          if ($t.isExportNamedDeclaration(parent)) {
            const name = path.node.id.name;

            state.opts.export(name, name);
          }
        }
      },

      ExportNamedDeclaration: {
        exit: (path, state) => {
          if (path.node.declaration === null) {
            path.remove();

          } else {
            path.replaceWith(path.node.declaration);
          }
        }
      },

      Program: {
        enter: (path, state) => {
          state.modules = {};
        },
        exit: (path, state) => {
          const exports = state.opts.getAllExports();

          const keys = [];

          // TODO make this more efficient ?
          for (let name in exports) {
            keys.push($t.objectMethod("get", $t.identifier(name), [], $t.blockStatement([
              $t.returnStatement($t.identifier(exports[name]))
            ])));
          }

          // TODO source maps ?
          const top = $t.variableDeclaration("var", [
            $t.variableDeclarator(
              $t.identifier(state.opts.namespace),
              $t.callExpression($t.memberExpression($t.identifier("Object"), $t.identifier("freeze")), [
                $t.objectExpression(keys)
              ])
            )
          ]);

          path.pushContainer("body", top);
        }
      }
    }
  };
};
