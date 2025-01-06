import {
  Errno,
  ErrnoError,
  File,
  FileSystem,
  flagToString,
  InMemory,
} from "npm:@zenfs/core";
import * as constants from "npm:@zenfs/core/emulation/constants.js";
import {
  GoFileSystemCallback,
  GoFileSystemError,
  GoFileSystemFFI,
  GoFileSystemFileStats,
} from "./wasm_exec@1.23.4.ts";

class GoFileSystemAdapter implements GoFileSystemFFI {
  #fs: FileSystem;
  #fdMap = new Map<number, File>();
  #nextFd = 100;

  #stdout: File;
  #stderr: File;

  constructor(fs: FileSystem) {
    this.#fs = fs;
    fs.mkdirSync("/dev", 0o660);
    fs.createFileSync("/dev/stdout", "w", 0o660);
    fs.createFileSync("/dev/stderr", "w", 0o660);
    this.#stdout = fs.openFileSync("/dev/stdout", "w");
    this.#stderr = fs.openFileSync("/dev/stderr", "w");
  }

  get backend() {
    return this.#fs;
  }

  get constants() {
    return constants;
  }

  finalize(): { stdout: string; stderr: string } {
    this.#stdout.closeSync();
    this.#stderr.closeSync();
    return {
      stdout: this.readFileSync("/dev/stdout"),
      stderr: this.readFileSync("/dev/stderr"),
    };
  }

  writeFileSync(path: string, content: string): void {
    const file = this.#fs.createFileSync(path, "w", 0o660);
    const data = new TextEncoder().encode(content);
    file.writeSync(data, 0, data.length);
    file.syncSync();
    file.closeSync();
  }

  readFileSync(path: string): string {
    const file = this.#fs.openFileSync(path, "r");
    const stat = file.statSync();
    const buffer = new Uint8Array(stat.size);
    file.readSync(buffer);
    file.closeSync();
    return new TextDecoder("utf8").decode(buffer);
  }

  #fdToFile(fd: number): File {
    if (fd == 1) {
      return this.#stdout;
    }
    if (fd == 2) {
      return this.#stderr;
    }
    const file = this.#fdMap.get(fd);
    if (!file) {
      throw new ErrnoError(Errno.EBADF);
    }
    return file;
  }

  writeSync(fd: number, buf: Uint8Array): number {
    return this.#fdToFile(fd).writeSync(buf, 0, buf.length);
  }

  write(
    fd: number,
    buf: Uint8Array,
    offset: number,
    length: number,
    position: number | null,
    callback: GoFileSystemCallback<number>,
  ) {
    handle(
      (async () => {
        const file = this.#fdToFile(fd);
        const written = await file.write(
          buf,
          offset,
          length,
          position ?? undefined,
        );
        return written;
      })(),
      callback,
    );
  }

  open(
    path: string,
    flags: number,
    _mode: number, // TODO?
    callback: GoFileSystemCallback<number>,
  ) {
    handle(
      (async () => {
        const file = await this.#fs.openFile(path, flagToString(flags));
        const fd = this.#nextFd++;
        this.#fdMap.set(fd, file);
        return fd;
      })(),
      callback,
    );
  }

  fstat(
    fd: number,
    callback: GoFileSystemCallback<GoFileSystemFileStats>,
  ) {
    handle(
      this.#fdToFile(fd).stat(),
      callback,
    );
  }

  read(
    fd: number,
    buffer: Uint8Array,
    offset: number,
    length: number,
    position: number,
    callback: (
      err: GoFileSystemError | null,
      bytesRead: number | undefined,
    ) => void,
  ) {
    handle(
      this.#fdToFile(fd)
        .read(buffer, offset, length, position).then((result) => {
          return result.bytesRead;
        }),
      callback,
    );
  }

  close(fd: number, callback: GoFileSystemCallback<void>) {
    handle(
      (async () => {
        const file = this.#fdMap.get(fd);
        if (!file) {
          throw new ErrnoError(Errno.EBADF);
        }
        await file.close();
        this.#fdMap.delete(fd);
      })(),
      callback,
    );
  }
}

export async function createFS(): Promise<GoFileSystemAdapter> {
  const fs = InMemory.create({});
  await fs.ready();
  return new GoFileSystemAdapter(fs);
}

async function handle<T>(
  promise: Promise<T>,
  cb: (error: GoFileSystemError | null, value: T | undefined) => void,
): Promise<void> {
  try {
    const value = await promise;
    cb(null, value);
  } catch (error) {
    if (error instanceof ErrnoError) {
      cb(error, undefined);
    } else {
      console.error("Unexpected error:", error);
      cb(new ErrnoError(Errno.EPROTO, String(error)), undefined);
    }
  }
}
