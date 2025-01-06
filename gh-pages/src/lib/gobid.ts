import { createFS } from "./go-fs.ts";
import { process } from "./go-process.ts";
import { Go } from "./wasm_exec@1.23.4.ts";

type Path = string;
type GoSourceCode = string;

export async function gobid(
  inputs: Record<Path, GoSourceCode>,
): Promise<string> {
  const fs = await createFS();

  const go = new Go(fs, process);

  for (const [path, source] of Object.entries(inputs)) {
    fs.writeFileSync(path, source);
    go.argv.push(path);
  }

  const source = await WebAssembly.instantiateStreaming(
    fetch(import.meta.resolve("../build/main.wasm")),
    go.importObject,
  );

  await go.run(source.instance);

  const { stdout, stderr } = fs.finalize();

  if (stderr) {
    return stderr;
  }

  return stdout;
}
