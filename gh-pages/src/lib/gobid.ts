import { init, runWasix, Directory } from "npm:@wasmer/sdk";

type Path = string;
type GoSourceCode = string;

export async function gobid(
  inputs: Record<Path, GoSourceCode>,
): Promise<string> {
  if (Object.entries(inputs).length > 1) {
    return "one file at a time";
  }

  await init();

  const module = await WebAssembly.compileStreaming(fetch(import.meta.resolve("../build/main.wasm")));

  const instance = await runWasix(module, {
    program: "gobid",
    args: ["-"], // read from stdin
  });

  const [[_, source]] = Object.entries(inputs);

  const stdin = instance.stdin.getWriter();
  await stdin.write(new TextEncoder().encode(source));
  await stdin.close();

  const result = await instance.wait();
  if (!result.ok) {
    return result.stderr;
  }

  return result.stdout;
}
