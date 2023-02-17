# rustdump

A tool for analyzing assembly code. \
It demangles rust symbols for both the current and v0 mangling scheme. \
There is a roadmap for this project but currently i'm very busy with university.

### Features yet to be implemented

Whenever I have time this year i'll try implementing most of these. \
If you're feeling like it, submit a pull request and i'll have a look at it.

- [ ] Port GUI to wgpu + winit
- [ ] Assembly instruction byte patching
- [ ] Hex binary viewer
- [ ] Some form of debugger front-end
  - [ ] [GDB](https://www.sourceware.org/gdb) bindings + support
  - [ ] [LLDB](https://lldb.llvm.org) bindings + support
  - [ ] [WinDbg](https://windbg.org) (maybe?) / own implementation (depends)
- [ ] X86-64 support
- [ ] AArch64/AArch32 support
