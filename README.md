<h1 align="center">
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./assets/logo_dark.png">
    <img height="150px" src="./assets/logo_light.png">
  </picture>
  <picture>
    <source media="(prefers-color-scheme: dark)" srcset="./assets/logo_text_dark.svg">
    <img height="150px" src="./assets/logo_text_light.svg">
   </picture>
</h1>

<h4 align="center">Ever wanted to inspect every <i>bite</i> of your binary.</h4>

## Showcase

![Assembly listing](./assets/screenshot.png)

## Features yet to be implemented

Whenever I have time this year I'll try implementing most of these. \
If you're feeling like it, submit a pull request and I'll have a look at it.

- [x] Port GUI to wgpu + winit
- [ ] Header with buttons and options
- [x] Assembly listing exploration
- [ ] Interactive terminal
- [ ] Assembly instruction byte patching
- [ ] Hex binary viewer
- [ ] Some form of debugger front-end
  - [ ] [GDB](https://www.sourceware.org/gdb) bindings + support
  - [ ] [LLDB](https://lldb.llvm.org) bindings + support
  - [ ] [WinDbg](https://windbg.org) (maybe?) / own implementation (depends)
- [x] X86-64 support
- [ ] AArch64/AArch32 support
- [x] Riscv64gc/Riscv32gc support
- [x] MIPS-V support
- [x] Demangling support for most targets
  - [x] MSVC
  - [x] Itanium
  - [x] Rust
- [ ] Assembly listing lifting
  - [ ] Resolving addresses
  - [ ] Creating labels for relative jumps
