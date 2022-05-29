import functools
import binaryninja as bn
from . import microblaze

# TODO ship pattern libs for gcc builtins
# e.g. udivsi3.S
# https://gcc.gnu.org/onlinedocs/gccint/Integer-library-routines.html
# https://github.com/Xilinx/gcc/blob/xlnx/gcc-4_8-branch/libgcc/config/microblaze/udivsi3.S

class MicroBlaze32LE(bn.Architecture):
    """Variant for chips with AXI and hard-IP ARM cores."""
    name = "ublaze32le"
    endianness = bn.Endianness.LittleEndian
    address_size = 4
    default_int_size = 4
    instr_alignment = 4
    max_instr_length = 12  # 32-bit immediate via IMM prefix + delay slot
    opcode_display_length = 5  # the "8 byte wide" instructions are p. boring
    regs = {name:bn.RegisterInfo(name, 4) 
            for name in [f'r{n}' for n in range(32)] + []}
    stack_pointer = 'r1'
    link_reg = 'r15'
    global_regs = ['r20']  # GOT in GCC

    flags = ['c']
    flag_roles = {'c': bn.FlagRole.CarryFlagRole,}
    flag_write_types = ['carry']
    flags_written_by_flag_write_type = {'carry':['c'],}

    system_regs = [
        # LWX/SWX reservation marker
        'reserved',
        # all this for MTS/MFS instructions...
        'msr', 'fsr', 'slr', 'shr', 'pid', 'zpr', 'tlbx', 'tlblo', 'tlbhi',
        'tlbsx', 'ear', 'esr', 'btr', 'edr'] + [f'pvr{n}' for n in range(12)]
    regs.update({name:bn.RegisterInfo(name, 4) for name in system_regs})

    class NameAndType:  # NameAndType in python??
        def __init__(self, name, type):
            self.name = name
            self.type = type
            self.handle = type.handle
            self.confidence = 123

    intrinsics = {
            'btc_clear':bn.IntrinsicInfo([], []),
            'mbar_i_side':bn.IntrinsicInfo([], []),
            'mbar_d_side':bn.IntrinsicInfo([], []),
            'hibernate':bn.IntrinsicInfo([], []),
            'sleep':bn.IntrinsicInfo([], []),
            'suspend':bn.IntrinsicInfo([], []),
            '__lzcnt32':bn.IntrinsicInfo(
                [bn.Type.int(4, False)], 
                [bn.Type.int(4, False)],
            ),
            'pcmpbf':bn.IntrinsicInfo(  # lazy lift, real one is a series
                [bn.Type.int(4, False)], # of byte comparisons + branches
                [bn.Type.int(4, False), bn.Type.int(4, False)],
            ),
            'ntoh':bn.IntrinsicInfo(    # this'll break dataflow if it shows
                [bn.Type.int(4, False)], # up a lot, easy but tedious fix in
                [bn.Type.int(4, False)], # `class Mem(Op)` if you need itk
            ),
            'move_to_spr':bn.IntrinsicInfo(
                [], 
                [bn.Type.int(4, False), bn.Type.int(4, False)],
            ),
            'move_from_spr':bn.IntrinsicInfo(
                [bn.Type.int(4, False)], 
                [bn.Type.int(4, False)],
            ),
            # macros names in UG647 don't map well and just emit asm anyway
            'get_fsl':bn.IntrinsicInfo(
                [bn.Type.int(4, False)], [
                    NameAndType('iface', bn.Type.int(4, False)),
                    NameAndType('block', bn.Type.int(1, False)),
                    NameAndType('ctl',   bn.Type.int(1, False)),
                    NameAndType('test',  bn.Type.int(1, False)),
                    NameAndType('atomic', bn.Type.int(1, False)),
                    NameAndType('exc',   bn.Type.int(1, False)),
                ],
            ),
            'put_fsl':bn.IntrinsicInfo([], [
                    NameAndType('iface', bn.Type.int(4, False)),
                    bn.Type.int(4, False),
                    NameAndType('block', bn.Type.int(1, False)),
                    NameAndType('ctl',   bn.Type.int(1, False)),
                    NameAndType('test',  bn.Type.int(1, False)),
                    NameAndType('atomic', bn.Type.int(1, False)),
                ],
            ),
            'icache_clear':bn.IntrinsicInfo([], [
                bn.Type.int(4, False), bn.Type.int(4, False)
            ]),
    }
    intrinsics['dcache_clear'] = intrinsics['icache_clear']
    intrinsics['dcache_flush'] = intrinsics['icache_clear']

    @functools.lru_cache(maxsize=None)
    def get_instruction_info(self, data, addr):
        for i in fuse_ops(data, addr, self.endianness):
            return i.info()

    def get_instruction_low_level_il(self, data, addr, il):
        for i in (ops := fuse_ops(data, addr, self.endianness)):
            i.arch = self  # pass-through for IL label/register numbers
            if not i.delay:
                i.lift(il)
            else:
            # ghetto first-pass at delay slots
                if not (j := next(ops, None)):
                    il.append(il.undefined())  # something went wrong
                    return len(i)
                i.lift(il, delay_slot=j.lift)
                return len(i) + len(j)
            return len(i)

    def get_instruction_text(self, data, addr):
        for i in fuse_ops(data, addr, self.endianness):
            return i.text(), len(i)


# naming leaves room for 64-bit impl, don't think QEMU/GDB/GAS/GCC support's
# there yet.... GCC supports addressing via LWEA/SWEA/LBUEA/SBEA macros:
#       https://github.com/search?q=org%3AXilinx+lbuea&type=code
class MicroBlaze32EB(MicroBlaze32LE):
    """Variant for older designs based on CoreConnect PLB."""
    name = "ublaze32eb"
    endianness = bn.Endianness.BigEndian


class FusedLength: byte_length = 8
import copy
def fuse_ops(data, addr, order):
    # (x:ub.ImmPrefix, y:(b:ub.Imm16)) => (y.b:=Imm32)
    # (x:ub.ImmPrefix, y:_) => (x, y)
    # (x:_) => (x)
    order = [0, 4][order == bn.Endianness.LittleEndian]
    ops = microblaze.Op.iter_bytes(data, byteswap=order, addr=addr)
    for x in ops:
        if type(x) != microblaze.ImmPrefix:
            yield x
        else:
            if not (y:= next(ops, None)):
                return  # not enough data to disambiguate
            if isinstance(y.b, microblaze.Imm16):
                y = copy.copy(y)  # ???
                y.b = microblaze.Imm32(
                    i=(x.b.i << 16) | (y.b.i & 0xFFFF),
                    fused_from=type(y.b)
                )
                # length is treated as 8 to fool assembler algorithm, but .addr
                # is still the non-IMM instruction's addr so lifter/branch
                # predictor should still be correct
                y.__bitspec_match__ = FusedLength
                assert len(y) == 8
            else: # probably garbage code, show wreckage
                yield x  # would be nice to tag it as Unresolved IMM prefix?
            yield y

