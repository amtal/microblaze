import binaryninja as bn

class ABI(bn.CallingConvention):  # Table 3-2 of UG081
    """MicroBlaze Application Binary Interface defines one calling convention.

    See Table 3-2 in Chapter 3 of UG081 for details. GCC supports it and is
    probably still the only compiler you'll encounter.

    Table 4-2 of UG984 is more recent.
    """
    # r0 constant generator
    # r2 read-only small data area anchor
    int_return_reg = 'r3'
    high_int_return_reg = 'r4'
    int_arg_regs = [f'r{n}' for n in range(5,11)]
    # r11-r12 temporaries
    # r13 read-write small data area anchor
    ###caller_saved_regs = [f'r{n}' for n in range(3,13)]
    # r14 interrupt return addr
    # r16 debugger trap return addr
    # r17 exception return addr
    # r18 assembler/compiler temporaries
    calee_saved_regs = [f'r{n}' for n in range(19,32) 
                                if n != 20]  # PIC code only
    global_pointer_reg = 'r20'  # already covered by Architecture?

    stack_adjusted_on_return = True  # usually in delay slot

class LinuxSyscall(bn.CallingConvention):
    """
    
    https://elixir.bootlin.com/linux/latest/source/arch/microblaze/kernel/entry.S#L290
    """
    int_arg_regs = 'r12  r5 r6 r7 r8 r9 r10'.split()
    int_return_reg = 'r3'
    eligible_for_heuristics = False

class LinuxLEPlatform(bn.Platform):
    name = 'linux-ublaze32le'

class LinuxEBPlatform(bn.Platform):
    name = 'linux-ublaze32eb'
