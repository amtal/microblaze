#from . import bench_hook
from . import arch, reloc, abi
import binaryninja as bn

arch.MicroBlaze32LE.register()
arch.MicroBlaze32EB.register()

elf = bn.BinaryViewType['ELF']

for suffix in ['32le', '32eb']:
    a = bn.Architecture[f'ublaze{suffix}']

    # calling conventions
    cc = abi.ABI(arch=a, name='abi')
    a.register_calling_convention(cc)
    a.standalone_platform.default_calling_convention = cc

    linux = ({'32le':abi.LinuxLEPlatform,
              '32eb':abi.LinuxEBPlatform}[suffix])(arch=a)
    linux.register_calling_convention(cc)
    linux.default_calling_convention = cc
    cc_sys = abi.LinuxSyscall(arch=a, name='linux-syscall')
    linux.register_calling_convention(cc_sys)
    linux.system_call_convention = cc_sys
    linux.register('linux')
    elf.register_default_platform(a, a.standalone_platform)
    elf.register_platform(3, a, linux)  # correct
    elf.register_platform(0, a, linux)  # sometimes?


    # loaders and relocations
    # 189 should be official for BE/LE, 47787 is a legacy?
    # https://sourceware.org/legacy-ml/binutils/2009-08/msg00127.html
    # likely to see more LE 189 and BE 47787 though?
    #elf_arch = {'32le':189, '32eb':47787}[suffix]
    elf.register_arch(189,   a.endianness, a)
    elf.register_arch(47787, a.endianness, a)
    a.set_view_type_constant("ELF", "R_COPY", reloc.R_MICROBLAZE._COPY)
    a.set_view_type_constant("ELF", "R_JUMP_SLOT", reloc.R_MICROBLAZE._JUMP_SLOT)
    reloc.MicroBlazeELFRelocationHandler.register(a, "ELF")
