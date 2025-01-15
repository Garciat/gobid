// TODO: go back to regular npm:@zenfs/core import
// https://github.com/esm-dev/esm.sh/pull/990
import {
  Errno,
  ErrnoError,
  File,
  FileSystem,
  FileSystemMetadata,
  flagToString,
  normalizeTime,
  Store,
} from "https://esm.sh/@zenfs/core@1.8.0/dist/index";
import { StoreFS } from "https://esm.sh/@zenfs/core@1.8.0/dist/backends/index.js";
import { InMemoryStore } from "https://esm.sh/@zenfs/core@1.8.0/dist/backends/memory.js";
import * as constants from "https://esm.sh/@zenfs/core@1.8.0/dist/vfs/constants";
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
    fs.mkdirSync("/dev", 0o660, { uid: 0, gid: 0 });
    fs.createFileSync("/dev/stdout", "w", 0o660, { uid: 0, gid: 0 });
    fs.createFileSync("/dev/stderr", "w", 0o660, { uid: 0, gid: 0 });
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
    const file = this.#fs.createFileSync(path, "w", 0o660, { uid: 0, gid: 0 });
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
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        const written = await file.write(
          buf,
          offset,
          length,
          position ?? undefined,
        );
        return written;
      },
    );
  }

  open(
    path: string,
    flags: number,
    _mode: number, // TODO?
    callback: GoFileSystemCallback<number>,
  ) {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, flagToString(flags));
        const fd = this.#nextFd++;
        this.#fdMap.set(fd, file);
        return fd;
      },
    );
  }

  fstat(
    fd: number,
    callback: GoFileSystemCallback<GoFileSystemFileStats>,
  ) {
    handle(
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        const stat = await file.stat();
        return stat;
      },
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
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        const result = await file.read(buffer, offset, length, position);
        return result.bytesRead;
      },
    );
  }

  close(fd: number, callback: GoFileSystemCallback<void>) {
    handle(
      callback,
      async () => {
        const file = this.#fdMap.get(fd);
        if (!file) {
          throw new ErrnoError(Errno.EBADF);
        }
        try {
          await file.close();
        } finally {
          this.#fdMap.delete(fd);
        }
      },
    );
  }

  chmod(
    path: string,
    mode: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, "r+");
        await file.chmod(mode);
      },
    );
  }

  chown(
    path: string,
    uid: number,
    gid: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, "r+");
        await file.chown(uid, gid);
      },
    );
  }

  fchmod(fd: number, mode: number, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        await file.chmod(mode);
      },
    );
  }

  fchown(
    fd: number,
    uid: number,
    gid: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        await file.chown(uid, gid);
      },
    );
  }

  fsync(fd: number, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        await file.sync();
      },
    );
  }

  ftruncate(
    fd: number,
    length: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = this.#fdToFile(fd);
        await file.truncate(length);
      },
    );
  }

  lchown(
    path: string,
    uid: number,
    gid: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, "r+");
        await file.chown(uid, gid);
      },
    );
  }

  link(path: string, link: string, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        await this.#fs.link(path, link);
      },
    );
  }

  lstat(
    path: string,
    callback: GoFileSystemCallback<GoFileSystemFileStats>,
  ): void {
    handle(
      callback,
      async () => {
        const stat = await this.#fs.stat(path);
        return stat;
      },
    );
  }

  mkdir(
    path: string,
    perm: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        await this.#fs.mkdir(path, perm, { uid: 0, gid: 0 });
      },
    );
  }

  readdir(path: string, callback: GoFileSystemCallback<string[]>): void {
    handle(
      callback,
      async () => {
        const entries = await this.#fs.readdir(path);
        return entries;
      },
    );
  }

  readlink(_path: string, callback: GoFileSystemCallback<string>): void {
    callback(new ErrnoError(Errno.ENOSYS, "not implemented"));
  }

  rename(from: string, to: string, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        await this.#fs.rename(from, to);
      },
    );
  }

  rmdir(path: string, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        await this.#fs.rmdir(path);
      },
    );
  }

  stat(
    path: string,
    callback: GoFileSystemCallback<GoFileSystemFileStats>,
  ): void {
    handle(
      callback,
      async () => {
        const stat = await this.#fs.stat(path);
        return stat;
      },
    );
  }

  symlink(
    path: string,
    link: string,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        await this.#fs.link(path, link);
      },
    );
  }

  truncate(
    path: string,
    length: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, "r+");
        await file.truncate(length);
      },
    );
  }

  unlink(path: string, callback: GoFileSystemCallback<void>): void {
    handle(
      callback,
      async () => {
        await this.#fs.unlink(path);
      },
    );
  }

  utimes(
    path: string,
    atime: number,
    mtime: number,
    callback: GoFileSystemCallback<void>,
  ): void {
    handle(
      callback,
      async () => {
        const file = await this.#fs.openFile(path, "r+");
        await file.utimes(normalizeTime(atime), normalizeTime(mtime));
      },
    );
  }
}

class CustomStoreFS extends StoreFS {
  constructor(store: Store) {
    super(store);
  }

  override metadata(): FileSystemMetadata {
    return {
      ...super.metadata(),
      noResizableBuffers: true, // causes errors in Safari
    };
  }
}

export async function createFS(): Promise<GoFileSystemAdapter> {
  const fs = new CustomStoreFS(new InMemoryStore());
  await fs.ready();
  return new GoFileSystemAdapter(fs);
}

async function handle<T>(
  cb: (error: GoFileSystemError | null, value: T | undefined) => void,
  task: () => Promise<T>,
): Promise<void> {
  try {
    const value = await task();
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
