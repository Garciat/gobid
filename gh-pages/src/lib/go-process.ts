import { enosys, GoProcessFFI } from "./wasm_exec@1.23.4.ts";

export const process: GoProcessFFI = {
  getuid() {
    return -1;
  },
  getgid() {
    return -1;
  },
  geteuid() {
    return -1;
  },
  getegid() {
    return -1;
  },
  getgroups() {
    throw enosys();
  },
  pid: -1,
  ppid: -1,
  umask() {
    throw enosys();
  },
  cwd() {
    throw enosys();
  },
  chdir() {
    throw enosys();
  },
};
