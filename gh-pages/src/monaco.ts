/// <reference path="https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/monaco.d.ts" />
import * as monacoTypes from "https://esm.sh/monaco-editor@0.52.2";
// Somehow monaco-editor from esm.sh is buggy, so we use jsdelivr instead
import * as monacoImpl from "https://cdn.jsdelivr.net/npm/monaco-editor@0.52.2/+esm";
// Worker scripts from esm.sh can be invoked as functions
import editorWorker from "https://esm.sh/monaco-editor@0.52.2/esm/vs/editor/editor.worker?worker";
import jsonWorker from "https://esm.sh/monaco-editor@0.52.2/esm/vs/language/json/json.worker?worker";
import cssWorker from "https://esm.sh/monaco-editor@0.52.2/esm/vs/language/css/css.worker?worker";
import htmlWorker from "https://esm.sh/monaco-editor@0.52.2/esm/vs/language/html/html.worker?worker";
import tsWorker from "https://esm.sh/monaco-editor@0.52.2/esm/vs/language/typescript/ts.worker?worker";

Reflect.set(globalThis, "MonacoEnvironment", {
  getWorker(_: unknown, label: string) {
    if (label === "json") {
      // @ts-ignore js import
      return new jsonWorker();
    }
    if (label === "css" || label === "scss" || label === "less") {
      // @ts-ignore js import
      return new cssWorker();
    }
    if (label === "html" || label === "handlebars" || label === "razor") {
      // @ts-ignore js import
      return new htmlWorker();
    }
    if (label === "typescript" || label === "javascript") {
      // @ts-ignore js import
      return new tsWorker();
    }
    // @ts-ignore js import
    return new editorWorker();
  },
});

export default monacoImpl as unknown as typeof monacoTypes;
