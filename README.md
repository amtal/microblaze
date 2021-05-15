# μBlaze Architecture Plugin for Binary Ninja

This is a plugin for [Binary Ninja](https://binary.ninja/) that adds MicroBlaze architecture support.

[MicroBlaze](https://en.wikipedia.org/wiki/MicroBlaze) is a configurable [soft processor](https://en.wikipedia.org/wiki/Soft_microprocessor) core from Xilinx going all the way back to their Spartan-II series of [FPGA](https://en.wikipedia.org/wiki/Field-programmable_gate_array)s. It can be found in a variety of roles within larger FPGA designs: from bare-bones microcontroller, to full Linux application processor, to early-boot [embedded controller](https://xilinx-wiki.atlassian.net/wiki/spaces/A/pages/18841724/PMU+Firmware).

![Example Disassembly and HLIL](https://raw.githubusercontent.com/amtal/microblaze/master/img/header.gif)

## Features

This plugin works on Linux binaries:

* Relocations† for working Triage Summary imports!
* Syscall arguments!‡
* That one cursed ELF `e_machine` value that's no longer used!

This plugin supports bare-metal firmware:

* Bus transfer and MSR intrinsics!
* Intrinsics for privileged operations!
* 64-bit instruction≠ extensions!

Minor, unimportant quality of life things:

* Nice disassembly of relative branches and 32-bit immediates!
* Delay slots are properly lifted!
* Disassemblesキ all configuration options in [UG984 (v2020.2)](https://www.xilinx.com/support/documentation/sw_manuals/xilinx2020_2/ug984-vivado-microblaze-ref.pdf) even the weird ones!

## Usage

All ELF files should Just Work™ but otherwise:

- Likely use `ublaze32be` architecture for older designs.
- Likely use `ublaze32le` architecture for newer Zynq designs.
- The `linux-ublaze32xx` default platforms aren't meaningfully different from `arch.standalone_platform`, you don't need to override them.
- If HLIL looks broken, check source to see if that configuration option has been properly implemented yet.

If you want to cite this plugin please use:

<pre>
  @online{ublaze-arch-plugin,
    title     = {{\mu}Blaze Architecture Plugin for Binary Ninja},
    url       = {https://github.com/amtal/microblaze},
    doi       = {10.5281/zenodo.4749824},
    author    = {amtal},
    year      = 2021,
  }
</pre>

## Caveats

*† relocations not well-tested, probably buggy*

*‡ minimum viable product, BYOSyscall typelib/headers*

*≠ literally just the added 64-from-32 instructions, but 64-bit architecture variant should be trivial if anyone needs it now*

*キbut definitely does not lift correctly, if someone has a clever cross-config validation plan lmk*
