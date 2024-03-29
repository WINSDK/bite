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

Here is an example of the assembly listing viewing.

![Assembly listing](./assets/screenshot.png)

The ability of viewing source code for debugging purposes. 

![Source Code](./assets/source_code.png)

## Features yet to be implemented

Whenever I have time this year I'll try implementing most of these. \
If you're feeling like it, submit a pull request and I'll have a look at it.

- [x] Port GUI to wgpu + winit
- [x] Header with buttons and options
- [x] Assembly listing exploration
- [x] Interactive terminal
- [ ] Assembly instruction byte patching
- [x] Hex binary viewer
- [ ] Debugging front-end's
  - [ ] [GDB](https://www.sourceware.org/gdb)
  - [ ] [LLDB](https://lldb.llvm.org)
  - [ ] [WinDbg](https://windbg.org)
- [ ] Debugging back-end
  - [ ] Initial MacOS support
  - [ ] Initial Windows support
  - [x] Initial Linux support
- [x] X86-64 support
- [ ] AArch64/AArch32 support
- [x] Riscv64gc/Riscv32gc support
- [x] MIPS-V support
- [x] Demangling support for most targets
  - [x] MSVC
  - [x] Itanium
  - [x] Rust
- [ ] Assembly listing lifting
  - [x] Resolving addresses
  - [ ] Creating labels for relative jumps
