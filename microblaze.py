from dataclasses import dataclass
import binaryninja as bn
from . import bitspec

class Operand:
    __slots__ = "i"
    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        return [tok(ty.TextToken, str(self))]

@bitspec.dataclass('-:16 i:s16')
class Imm16(Operand):  # only inherit on IMM fusion candidates
    i:int

    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        return [tok(ty.IntegerToken, hex(self.i), self.i)]

    def r(self, il, addr=None, sz=4):  # uh, do we need a size param? FIXME
        return il.const(sz, self.i)    # everything gets sext to 32bit?

@bitspec.dataclass('-:8 i:s24')
class Imm24(Imm16): pass

@bitspec.dataclass('-:16 i:s16')
class Rel16(Imm16):
    """Imm16 operand statically known to be relative address pointer.

    A few options for displaying these:

    - Just a signed offset from addr: completely unreadable.
    - Destination address: it matches hand-written assembly, since the 
        GNU Assembler accepts absolute labels on non-A I-instructions.
        Kind of confusing if your image base is wrong or you're staring
        at garbage marked as code.
    - Destination - addr: verbose, but allows navigation in assembly view
        without confusing degenerate cases.
    """
    def text(self, addr):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        ea = (self.i + addr) & 0xFFFFffff
        return [
            tok(ty.PossibleAddressToken, hex(ea), ea),
            tok(ty.TextToken, ' - '),  # do not break il.Operand count
            tok(ty.PossibleAddressToken, hex(addr), addr),
        ]
    def r(self, il, addr=None, sz=4): 
        return il.const_pointer(sz, (self.i+addr) & 0xFFFFffff)

@bitspec.dataclass('-:16 i:s16')
class Abs16(Imm16):
    """Imm16 operand statically known to be absolute address pointer.

    It's tempting to apply these to `addik r., r0, ...` since they're often
    used to generate an address without needing an IMM prefix. However, unlike
    branch targets, annotations from IL analysis are shown beside them.
    """
    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        ea = self.i & 0xFFFFffff
        return [tok(ty.PossibleAddressToken, hex(ea), ea)]
    def r(self, il, addr=None, sz=2): 
        return il.const_pointer(sz, self.i)


@dataclass
class Imm32(Imm16): 
    """Result of fusing IMM "prefix" instruction."""
    fused_from:object  # operand input
    def __len__(self): return 4
    def text(self, addr=None): return self.fused_from.text(self, addr=addr)
    def r(self, il, addr=None): return self.fused_from.r(self, il, addr=addr, sz=4)


@bitspec.dataclass('-:27 i:5')
class Imm5(Operand):  # barrel shifter only, not fused with IMM
    i:int

    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        return [tok(ty.IntegerToken, hex(self.i), self.i)]

    def r(self, il, addr=None, sz=2): 
        return il.const(sz, self.i)


@dataclass
class Register:
    __slots__ = 'n'
    n: int

    def text(self, addr=None): 
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        return [tok(ty.RegisterToken, f'r{self.n}')]

    def r(self, il, addr=None): 
        return il.reg(4, f'r{self.n}') if self.n else il.const(4, 0)
    def w(self, il, val): 
        #assert self.n != 0  # nvm, r0 writes are used as NOPs
        il.append(il.set_reg(4, f'r{self.n}', val))

@bitspec.bitspec('...... n:5 -:5 -:5 .:11')
class Rd(Register): pass
@bitspec.bitspec('...... -:5 n:5 -:5 .:11')
class Ra(Register): pass
@bitspec.bitspec('...... -:5 -:5 n:5 .:11')
class Rb(Register): pass


@bitspec.dataclass('0x80000000', op='NOP') # or r0, r0, r0
@bitspec.dataclass('0x10000000', op='NOP') # addk r0, r0, r0
class Op:
    op:str
    d:Operand = None
    a:Operand = None
    b:Operand = None
    delay:int = 0       # delay slot
    long:int = 0        # 64-bit variant
    privileged:int = 0  # if C_USE_MMU >= 1 and not explicitly allowed

    def info(self):
        ii = bn.InstructionInfo()
        if self.delay:
            ii.branch_delay = True
        ii.length = len(self)
        return ii

    def text(self):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType

        name = self.op.lower()
        if self.long:
            # No convenient test corpus for 64-bit code. 
            # Just going to ghetto-rig support until one's found.
            if name.startswith('bs'):
                name = name[:2] + 'l' + name[2:]
            elif name.startswith('f'):
                name = 'd' + name[1:]
            elif len(name) >= 4:
                name = name[:4] + 'l' + name[4:]
            else:  # slightly wrong FIXME
                name = name[:3] + 'l' + name[3:]
        acc = [tok(ty.InstructionToken, name)]

        if self.d or self.a or self.b: 
            # for il.operand(n,..), operand
            # 0 will be instruction text
            # 1 is d-slot
            # 2 is a-slot
            # 3 is b or imm-slot
            acc.append(tok(ty.OperandSeparatorToken, ' '))
            if self.d:
                acc += self.d.text(addr=self.addr)
            self.d and (self.a or self.b) and acc.append(tok(ty.OperandSeparatorToken, ', '))
            if self.a:
                acc += self.a.text(addr=self.addr)
            self.a and self.b and acc.append(tok(ty.OperandSeparatorToken, ', '))
            if self.b: 
                acc += self.b.text(addr=self.addr)
        return acc

    def lift(self, il):
        if self.op == 'NOP':
            il.append(il.nop())
        else:
            il.append(il.unimplemented())

# MicroBlaze Processor Reference Guide, UG081 (v9.0)
# Table 1-6: MicroBlaze Instruction Set Summary
#
# 64-bit variant also needs 64-bit regs lol (I don't wanna test these)
#
# options? C_DATA_SIZE, C_ENDIANNESS, C_USE_BARREL, C_USE_DIV, C_USE_HW_MUL
# C_USE_REORDER_INSTR, C_USE_FPU
# MBV version code?
# 
# GCC support is via macros in mb_interface.h
# https://github.com/Xilinx/embeddedsw/blob/master/lib/bsp/standalone/src/microblaze/mb_interface.h
# so, binutils supports but gcc don't
# there's also QEMU
# https://github.com/Xilinx/qemu/blob/master/target/microblaze/insns.decode

#
# TODO multiple inheritance for "optional" instructions? then a limited number
# of top-level ISA config options, with the right instructions hanging off
# could be handy for quick triage of how well-lifted an image is
#
#
# any lift-time instruction fusion optimizations from here?
# https://raw.githubusercontent.com/gcc-mirror/gcc/master/gcc/config/microblaze/microblaze.md
# any pipeline hazard info worth including from above?
# might be interesting to emit tags if found, prob. an external pass though
#
# could use vv as isa testsuite, it's minimal but >nothing
# https://github.com/Xilinx/binutils/tree/xlnx/binutils-2_23-branch/gas/testsuite/gas/microblaze
@bitspec.dataclass('000000 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='ADD')
@bitspec.dataclass('000001 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='RSUB')
@bitspec.dataclass('000010 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='ADDC')
@bitspec.dataclass('000011 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='RSUBC')
@bitspec.dataclass('000100 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='ADDK')
@bitspec.dataclass('000101 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='RSUBK')
@bitspec.dataclass('000110 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='ADDKC')
@bitspec.dataclass('000111 -:15 00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='RSUBKC')
class Arith(Op):
    def lift(self, il):
        if self.long:
            return super().lift(il)
        a, b = self.a.r(il), self.b.r(il)
        if 'ADD' in self.op:
            op = il.add_carry if self.op[-1] == 'C' else il.add
        elif 'SUB' in self.op:
            op = il.sub_borrow if self.op[-1] == 'C' else il.sub
        elif 'AND' in self.op:
            op = il.and_expr
            if 'ANDN' in self.op:
                b = il.not_expr(4, b)
        elif 'XOR' in self.op:
            op = il.xor_expr
        elif 'OR' in self.op:
            op = il.or_expr

        if 'SUB' not in self.op:
            args = {'size':4, 'a':a, 'b':b}  # same order as asm
        else:
            args = {'size':4, 'a':b, 'b':a}  # reverse for RSUB
        if 'K' not in self.op:  # Keep previous carry value
            args['flags'] = 'carry'
        if self.op[-1] == 'C':  # carry-out
            args['carry'] = il.flag('c')
        self.d.w(il, il.operand(1, op(**args)))


# TODO validate vs gdb sim vvv
#      https://github.com/Xilinx/binutils-gdb/blob/xlnx/master-rebase/sim/microblaze/microblaze.isa#L72
#      it's pretty detailed, no 64bit extensions but w/e
#      oh hey, QEMU supports microblaze p. well
# https://github.com/Xilinx/qemu/tree/master/target/microblaze
#       still no 64bit?

@bitspec.dataclass('000101 -:15 00 long:1 0000 0001', d=Rd, a=Ra, b=Rb, op='CMP')
@bitspec.dataclass('000101 -:15 00      0 0000 0011', d=Rd, a=Ra, b=Rb, op='CMPU')
class Cmp(Op):
    def lift(self, il):
        if self.long:
            return super().lift(il)
        a, b = self.a.r(il), self.b.r(il)
        sign = {
            'CMPU':il.compare_unsigned_less_than,
            'CMP':il.compare_signed_less_than,
        }[self.op](4, b, a)
        sign = il.shift_left(4, sign, il.const(1, 31))
        val = il.and_expr(4, il.sub(4, b, a), il.const(4, 0x7FFFffff))
        self.d.w(il, il.or_expr(4, sign, val))


@bitspec.dataclass('001000 -:26', d=Rd, a=Ra, b=Imm16, op='ADDI')
@bitspec.dataclass('001001 -:26', d=Rd, a=Ra, b=Imm16, op='RSUBI')
@bitspec.dataclass('001010 -:26', d=Rd, a=Ra, b=Imm16, op='ADDIC')
@bitspec.dataclass('001011 -:26', d=Rd, a=Ra, b=Imm16, op='RSUBIC')
@bitspec.dataclass('001100 -:26', d=Rd, a=Ra, b=Imm16, op='ADDIK')
@bitspec.dataclass('001101 -:26', d=Rd, a=Ra, b=Imm16, op='RSUBIK')
@bitspec.dataclass('001110 -:26', d=Rd, a=Ra, b=Imm16, op='ADDIKC')
@bitspec.dataclass('001111 -:26', d=Rd, a=Ra, b=Imm16, op='RSUBIKC')
class ImmArith(Arith): pass

@bitspec.dataclass('010000 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='MUL')
@bitspec.dataclass('010000 -:15 00000000001', d=Rd, a=Ra, b=Rb, op='MULH')
@bitspec.dataclass('010000 -:15 00000000011', d=Rd, a=Ra, b=Rb, op='MULHU')
@bitspec.dataclass('010000 -:15 00000000010', d=Rd, a=Ra, b=Rb, op='MULHSU')
@bitspec.dataclass('011000 -:26',             d=Rd, a=Ra, b=Imm16, op='MULI')
class Mul(Op):
    def lift(self, il):
        a, b = self.a.r(il), self.b.r(il)
        if self.op in ['MUL', 'MULI']:
            val = il.mult(8, a, b)
        elif self.op == 'MULH':
            val = il.mult(8, il.sign_extend(8, a), il.sign_extend(8, b))
            val = il.logical_shift_right(8, val, il.const(1, 32))
        elif self.op == 'MULHU':
            val = il.mult(8, il.zero_extend(8, a), il.zero_extend(8, b))
            val = il.arith_shift_right(8, val, il.const(1, 32))
        elif self.op == 'MULHSU':
            val = il.mult(8, il.sign_extend(8, a), il.zero_extend(8, b))
            val = il.arith_shift_right(8, val, il.const(1, 32))
        self.d.w(il, il.low_part(4, val))


# BSRL isn't in the summary table, detailed summary of "bs" covers it though
@bitspec.dataclass('010001 -:15       00 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='BSRL')
@bitspec.dataclass('010001 -:15       01 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='BSRA')
@bitspec.dataclass('010001 -:15       10 long:1 0000 0000', d=Rd, a=Ra, b=Rb, op='BSLL')
@bitspec.dataclass('011001 -:10 00000 00 long:1 000. ....', d=Rd, a=Ra, b=Imm5, op='BSRLI')
@bitspec.dataclass('011001 -:10 00000 01 long:1 000. ....', d=Rd, a=Ra, b=Imm5, op='BSRAI')
@bitspec.dataclass('011001 -:10 00000 10 long:1 000. ....', d=Rd, a=Ra, b=Imm5, op='BSLLI')
class BarrelShift(Op):
    def lift(self, il):
        if self.long:
            return super().lift(il)
        self.d.w(il, {
            'BSRL':il.logical_shift_right,
            'BSRA':il.arith_shift_right,
            'BSLL':il.shift_left,
        }[self.op[:4]](4, self.a.r(il), self.b.r(il)))


@bitspec.dataclass('-:16 01 ... w:5 . s:5', width=False)
@bitspec.dataclass('-:16 10 ... w:5 . s:5', width=True)
class ImmField:
    w:int; s:int; width:bool

    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        if self.width:
            width = self.w - self.s + 1
        else:
            width = self.w
        return [
            tok(ty.IntegerToken, str(width), width),
            tok(ty.OperandSeparatorToken, ', '),
            tok(ty.IntegerToken, str(self.s), self.s),
        ]

@bitspec.dataclass('011001 -:10 01 long:1 00 -:5 0 -:5', d=Rd, a=Ra, b=ImmField, op='BSEFI')
@bitspec.dataclass('011001 -:10 10 long:1 00 -:5 0 -:5', d=Rd, a=Ra, b=ImmField, op='BSIFI')
class BarrelShiftField(Op):
    pass

@bitspec.dataclass('010010 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='IDIV')
@bitspec.dataclass('010010 -:15 00000000010', d=Rd, a=Ra, b=Rb, op='IDIVU')
class Div(Op):
    def lift(self, il):
        op = il.div_signed if self.op == 'IDIV' else il.div_unsigned
        self.d.w(il, op(4, self.a.r(il), self.b.r(il)))


@bitspec.dataclass('-:16 -:5 . n:1 c:1 t:1 a:1 e:1 . ....')
class NTAEDyn:
    n:int; c:int; t:int; a:int; e:int

@bitspec.dataclass('-:16 . n:1 c:1 t:1 a:1 e:1 .. .... fsl_x:4')
class NTAE(NTAEDyn):
    fsl_x:int

@bitspec.dataclass('010011 -:5 00000 -:5  0 .....  0 0000', d=Rd, b=Rb, flags=NTAEDyn, op='GETD')
@bitspec.dataclass('010011 00000 -:5 -:5  1 ....  00 0000', a=Ra, b=Rb, flags=NTAEDyn, op='PUTD')
@bitspec.dataclass('011011 -:5 00000      0 ..... 00 0000....', d=Rd, flags=NTAE, op='GET')
@bitspec.dataclass('011011 00000 -:5      1 ....0 00 0000....', a=Ra, flags=NTAE, op='PUT')
class FSLBus(Op): 
    flags:Operand = None
    c:int = -1  # data-control bitflag
    privileged:int = 1

    def text(self):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        
        name = ''
        if self.flags.t:
            name += 't'  # PUT doesn't have reg arg.
        if self.flags.n:
            name += 'n'
        if self.flags.e and 'GET' in self.op:
            name += 'e'
        if self.flags.a:
            name += 'a'
        if self.flags.c:
            name += 'c'
        acc = [tok(ty.InstructionToken, name + self.op.lower())]
        acc.append(tok(ty.OperandSeparatorToken, ' '))
        if self.d:
                acc += self.d.text()
                if self.a or self.b:
                    acc.append(tok(ty.OperandSeparatorToken, ', '))
        if self.a and not self.flags.t:
                acc += self.a.text()
                if self.b:
                    acc.append(tok(ty.OperandSeparatorToken, ', '))
        if self.b:
                acc += self.b.text()
        return acc

    def lift(self, il):
        iname = 'get_fsl' if 'GET' in self.op else 'put_fsl'
        flags = [il.const(1, f) for f in [
            self.flags.n, self.flags.c, self.flags.t, self.flags.a
        ]]
        if 'GET' in self.op:
            flags += [il.const(1, self.flags.e)]
        iface = il.const(4, self.flags.fsl_x) if 'TD' not in self.op else self.b.r(il)
        if 'GET' in self.op:
            il.append(il.intrinsic(
                [self.arch.regs[f'r{self.d.n}']],
                iname,
                [iface] + flags,
            ))
        else:
            il.append(il.intrinsic(
                [],
                iname,
                [iface, self.a.r(il)] + flags,
            ))


@bitspec.dataclass('010110 -:15 long:1 0000000000', d=Rd, a=Ra, b=Rb, op='FADD')
@bitspec.dataclass('010110 -:15 long:1 0010000000', d=Rd, a=Ra, b=Rb, op='FRSUB')
@bitspec.dataclass('010110 -:15 long:1 0100000000', d=Rd, a=Ra, b=Rb, op='FMUL')
@bitspec.dataclass('010110 -:15 long:1 0110000000', d=Rd, a=Ra, b=Rb, op='FDIV')
@bitspec.dataclass('010110 -:15 long:1 1000000000', d=Rd, a=Ra, b=Rb, op='FCMP.UN')
@bitspec.dataclass('010110 -:15 long:1 1000010000', d=Rd, a=Ra, b=Rb, op='FCMP.LT')
@bitspec.dataclass('010110 -:15 long:1 1000100000', d=Rd, a=Ra, b=Rb, op='FCMP.EQ')
@bitspec.dataclass('010110 -:15 long:1 1000110000', d=Rd, a=Ra, b=Rb, op='FCMP.LE')
@bitspec.dataclass('010110 -:15 long:1 1001000000', d=Rd, a=Ra, b=Rb, op='FCMP.GT')
@bitspec.dataclass('010110 -:15 long:1 1001010000', d=Rd, a=Ra, b=Rb, op='FCMP.NE')
@bitspec.dataclass('010110 -:15 long:1 1001100000', d=Rd, a=Ra, b=Rb, op='FCMP.GE')
@bitspec.dataclass('010110 -:10 00000 0      1010000000', d=Rd, a=Ra, op='FLT')
@bitspec.dataclass('010110 -:10 00000 1      1010000000', d=Rd, a=Ra, op='DBL')
@bitspec.dataclass('010110 -:10 00000 0      1100000000', d=Rd, a=Ra, op='FINT')
@bitspec.dataclass('010110 -:10 00000 1      1100000000', d=Rd, a=Ra, op='DLONG')
@bitspec.dataclass('010110 -:10 00000 long:1 1110000000', d=Rd, a=Ra, op='FSQRT')
class Floats(Op):
    def lift(self, il):
        a = self.a.r(il)
        size = {'F':4, 'D':8}[self.op[0]]
        if self.op in ['FLT', 'DBL']:
            val = il.int_to_float(size, a)
        elif self.op in ['FINT', 'DLONG']:
            val = il.float_to_int(size, a)
        else:
            val = il.undefined()
        self.d.w(il, val)

@bitspec.dataclass('011010 -:5 00000', d=Rd, a=Imm16, op='ADDLI')
@bitspec.dataclass('011010 -:5 00001', d=Rd, a=Imm16, op='RSUBLI')
@bitspec.dataclass('011010 -:5 00010', d=Rd, a=Imm16, op='ADDLIC')
@bitspec.dataclass('011010 -:5 00011', d=Rd, a=Imm16, op='RSUBLIC')
@bitspec.dataclass('011010 -:5 00100', d=Rd, a=Imm16, op='ADDLIK')
@bitspec.dataclass('011010 -:5 00101', d=Rd, a=Imm16, op='RSUBLIK')
@bitspec.dataclass('011010 -:5 00110', d=Rd, a=Imm16, op='ADDLIKC')
@bitspec.dataclass('011010 -:5 00111', d=Rd, a=Imm16, op='RSUBLIKC')
@bitspec.dataclass('011010 -:5 10000', d=Rd, a=Imm16, op='ORLI')
@bitspec.dataclass('011010 -:5 10001', d=Rd, a=Imm16, op='ANDLI')
@bitspec.dataclass('011010 -:5 10010', d=Rd, a=Imm16, op='XORLI')
@bitspec.dataclass('011010 -:5 10011', d=Rd, a=Imm16, op='ANDNLI')
class Arith64(Op): pass


@bitspec.dataclass('100000 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='OR')
@bitspec.dataclass('100001 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='AND')
@bitspec.dataclass('100010 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='XOR')
@bitspec.dataclass('100011 -:15 00000000000', d=Rd, a=Ra, b=Rb, op='ANDN')
class BitArith(Arith): pass

@bitspec.dataclass('100000 -:15 10000000000', d=Rd, a=Ra, b=Rb, op='PCMPBF')
class PatCmpBytes(Op):
    def lift(self, il):
        a, b = self.a.r(il), self.b.r(il)
        self.d.w(il, il.unimplemented())

@bitspec.dataclass('100010 -:15 10000000000', d=Rd, a=Ra, b=Rb, op='PCMPEQ')
@bitspec.dataclass('100011 -:15 10000000000', d=Rd, a=Ra, b=Rb, op='PCMPNE')
class PatCmp(Op):
    def lift(self, il):
        op = il.compare_equal if self.op == 'PCMPEQ' else il.compare_not_equal
        self.d.w(il, op(4, self.a.r(il), self.b.r(il)))


@bitspec.dataclass('100100 -:10 0000000000000001', d=Rd, a=Ra, op='SRA')
@bitspec.dataclass('100100 -:10 0000000000100001', d=Rd, a=Ra, op='SRC')
@bitspec.dataclass('100100 -:10 0000000001000001', d=Rd, a=Ra, op='SRL')
@bitspec.dataclass('100100 -:10 0000000001100000', d=Rd, a=Ra, op='SEXT8')
@bitspec.dataclass('100100 -:10 0000000001100001', d=Rd, a=Ra, op='SEXT16')
@bitspec.dataclass('100100 -:10 0000000001100010', d=Rd, a=Ra, op='SEXT32')
class Bit(Op):
    def lift(self, il):
        a = self.a.r(il)
        if self.op == 'SEXT8':
            val = il.zero_extend(4, il.low_part(1, a))
        elif self.op == 'SEXT16':
            val = il.zero_extend(4, il.low_part(2, a))
        elif self.op == 'SEXT32':
            val = il.undefined()  # need ublaze64xx arch
        else:
            one = il.const(1, 1)
            if self.op == 'SRL':
                val = il.shift_left(4, a, one, flags='carry')
            elif self.op == 'SRA':
                val = il.arith_shift_right(4, a, one, flags='carry')
            elif self.op == 'SRC':
                val = il.rotate_right_carry(4, a, one, il.flag('c'), 
                                                      flags='carry')
        self.d.w(il, val)

@bitspec.dataclass('100100 -:10 0000000011100000', d=Rd, a=Ra, op='CLZ')
class CountLeadingZeros(Op):
    def lift(self, il):
        il.append(il.intrinsic(
            [self.arch.regs[f'r{self.d.n}']],
            '__lzcnt32',
            [self.a.r(il)],
        ))

@bitspec.dataclass('100100 -:10 0000000111100000', d=Rd, a=Ra, op='SWAPB')
@bitspec.dataclass('100100 -:10 0000000111100010', d=Rd, a=Ra, op='SWAPH')
class EndianSwap(Op):
    pass


@bitspec.dataclass('100100 00000 -:10 00001101000', a=Ra, b=Rb, op='WIC')
@bitspec.dataclass('100100 00000 -:10 00001100100', a=Ra, b=Rb, op='WDC')
@bitspec.dataclass('100100 00000 -:10 00001110100', a=Ra, b=Rb, op='WDC.FLUSH')
@bitspec.dataclass('100100 00000 -:10 00001100110', a=Ra, b=Rb, op='WDC.CLEAR')
@bitspec.dataclass('100100 00000 -:10 00001110110', a=Ra, b=Rb, op='WDC.CLEAR.EA')
class Cache(Op):  # table encoding missing a few 0s, detailed one is fine
    privileged:int = 1

    def lift(self, il):
        op = {
            'WIC':'icache_clear',
            'WDC':'dcache_clear',
            'WDC.C':'dcache_clear',
            'WDC.F':'dcache_flush',
        }[self.op[:5]]
        il.append(il.intrinsic(
            [],
            op,
            [self.a.r(il), self.b.r(il)],
        ))

MTS_REG = {  # lists in MicroBlaze Processor Reference Guide are partial...
    1:'msr', 7:'fsr', 0x0800:'slr', 0x0802:'shr',
    0x1000:'pid', 0x1001:'zpr',
    0x1002:'tlbx', 0x1003:'tlblo', 0x1004:'tlbhi', 0x1005:'tlbsx',
}
MFS_REG = {
    0:'pc', 1:'msr', 3:'ear', 5:'esr', 7:'fsr', 0xB:'btr', 0xD:'edr',
    0x1000:'pid', 0x1001:'zpr', 
    0x1002:'tlbx', 0x1003:'tlblo', 0x1004:'tlbhi',
}
MFS_REG.update({0x2000 + n:f'pvr{n}' for n in range(12)})

@bitspec.dataclass('-:16 11 reg:14', lut=MTS_REG)
@bitspec.dataclass('-:16 10 reg:14', lut=MFS_REG)
class SpecialReg(Operand):
    reg:int
    lut:dict
    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        if reg := self.lut.get(self.reg, None):
            return [tok(ty.RegisterToken, reg)]
        else:
            return [tok(ty.IntegerToken, hex(self.reg), self.reg)]
    def r(self, il, addr): 
        reg = self.lut.get(self.reg, None)
        if reg == 'pc':
            return il.const(4, addr)
        else:
            return il.reg(4, reg) if reg else il.undefined()
    def w(self, il, val): 
        reg = self.lut.get(self.reg, None)
        il.append(il.set_reg(4, reg, val) if reg else il.undefined())

@bitspec.dataclass('100101 00000 -:5 11 -:14', d=SpecialReg, a=Ra, op='MTS')
@bitspec.dataclass('100101 01000 -:5 11 -:14', d=SpecialReg, a=Ra, op='MTSE')
@bitspec.dataclass('100101 -:5 00000 10 -:14', d=Rd, a=SpecialReg, op='MFS')
@bitspec.dataclass('100101 -:5 01000 10 -:14', d=Rd, a=SpecialReg, op='MFSE')
class MoveSpecialReg(Op):
    def lift(self, il):
        if ((type(self.d) == SpecialReg and self.d.reg in self.d.lut) or 
            (type(self.a) == SpecialReg and self.a.reg in self.a.lut)):

            self.d.w(il, self.a.r(il, self.addr))
        else:
            if self.op.startswith('MTS'):
                il.append(il.intrinsic([],
                    'move_to_spr', 
                    [il.const(4, self.d.reg), self.a.r(il)]
                ))
            else:
                il.append(il.intrinsic(
                    [self.arch.regs[f'r{self.d.n}']],
                    'move_from_spr', 
                    [il.const(4, self.d.reg)]
                ))



@bitspec.dataclass('100101 -:5 100010 mask:15', d=Rd, op='MSRCLR')
@bitspec.dataclass('100101 -:5 100000 mask:15', d=Rd, op='MSRSET')
# summary table shows Ra field ^^^^^ as 0 but detail shows 16
# summary table shows imm14, detail shows imm15...
class MSR(Op):  
    mask:int = -1
    privileged:int = 1
    def text(self, addr=None):
        tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
        return [
            tok(ty.InstructionToken, self.op.lower()),
            tok(ty.OperandSeparatorToken, ' '),
        ] + self.d.text(addr=addr) + [
            tok(ty.OperandSeparatorToken, ', '),
            tok(ty.IntegerToken, hex(self.mask), self.mask),
        ]

    def lift(self, il):
        msr = il.reg(4, 'msr')
        self.d.w(il, msr)
        if self.op.endswith('CLR'):  # not bothering lifting to carry flag
            msr = il.and_expr(4, msr, il.const(4, 0xFFFF8000 | (self.mask ^ 0x7Fff)))
        else:
            msr = il.or_expr(4, msr, il.const(4, self.mask))
        il.append(il.set_reg(4, 'msr', msr))


@bitspec.dataclass('100110 00000 00000 ..... 00000000000', b=Rb, op='BR')
@bitspec.dataclass('100110 00000 10000 ..... 00000000000', b=Rb, op='BRD', delay=1)
@bitspec.dataclass('100110 ..... 10100 ..... 00000000000', d=Rd, b=Rb, op='BRLD', delay=1)
@bitspec.dataclass('100110 00000 01000 ..... 00000000000', b=Rb, op='BRA')
@bitspec.dataclass('100110 00000 11000 ..... 00000000000', b=Rb, op='BRAD', delay=1)
@bitspec.dataclass('100110 ..... 11100 ..... 00000000000', d=Rd, b=Rb, op='BRALD', delay=1)
@bitspec.dataclass('100110 ..... 01100 ..... 00000000000', d=Rd, b=Rb, op='BRK', privileged=1)
class UnconditionalIndirect(Op):
    def info(self):
        ii = super().info()
        if self.op != 'BRK':
            if 'I' in self.op:
                t_dst = self.b.i if 'A' in self.op else self.addr + self.b.i
                if 'LID' in self.op and self.d.n == 15:
                    ii.add_branch(bn.BranchType.CallDestination, t_dst)
                else:
                    ii.add_branch(bn.BranchType.UnconditionalBranch, t_dst)
            else:
                if 'LD' in self.op and self.d.n == 15:
                    ii.add_branch(bn.BranchType.CallDestination)
                else:
                    ii.add_branch(bn.BranchType.UnresolvedBranch)
        else:
            ii.add_branch(bn.BranchType.ExceptionBranch)
        return ii

    def lift_t_dst(self, il):
        if self.op.startswith('BRA') or self.op.startswith('BRK'):
            return self.b.r(il)  # BRK is also absolute
        else:
            return il.add(4, il.const(4, self.addr), self.b.r(il))

    def lift(self, il, delay_slot=None):
        if self.op.endswith('LD') or self.op.endswith('LID'):
            if self.d.n == 15:
                if delay_slot:
                    delay_slot(il)
                il.append(il.call(self.lift_t_dst(il)))
                return
            self.d.w(il, il.const_pointer(4, self.addr))

        if 'I' in self.op:
            if self.op.startswith('BRA') or self.op.startswith('BRK'):
                dst = self.b.i 
            else:
                dst = (self.addr + self.b.i) & 0xFFFFffff
            t = il.get_label_for_address(self.arch, dst)
            if t:
                if delay_slot:
                    delay_slot(il)
                il.append(il.goto(t))
            else:
                t = bn.LowLevelILLabel()
                il.mark_label(t)
                if delay_slot:
                    delay_slot(il)
                il.append(il.jump(self.lift_t_dst(il)))
        else:
            if delay_slot:
                delay_slot(il)
            il.append(il.jump(self.lift_t_dst(il)))


@bitspec.dataclass('100111 00000        .:10 00000000000', a=Ra, b=Rb, op='BEQ')
@bitspec.dataclass('100111 00001        .:10 00000000000', a=Ra, b=Rb, op='BNE')
@bitspec.dataclass('100111 00010        .:10 00000000000', a=Ra, b=Rb, op='BLT')
@bitspec.dataclass('100111 00011        .:10 00000000000', a=Ra, b=Rb, op='BLE')
@bitspec.dataclass('100111 00100        .:10 00000000000', a=Ra, b=Rb, op='BGT')
@bitspec.dataclass('100111 00101        .:10 00000000000', a=Ra, b=Rb, op='BGE')
@bitspec.dataclass('100111 10000        .:10 00000000000', a=Ra, b=Rb, op='BEQD', delay=1)
@bitspec.dataclass('100111 10001        .:10 00000000000', a=Ra, b=Rb, op='BNED', delay=1)
@bitspec.dataclass('100111 10010        .:10 00000000000', a=Ra, b=Rb, op='BLTD', delay=1)
@bitspec.dataclass('100111 10011        .:10 00000000000', a=Ra, b=Rb, op='BLED', delay=1)
@bitspec.dataclass('100111 10100        .:10 00000000000', a=Ra, b=Rb, op='BGTD', delay=1)
@bitspec.dataclass('100111 10101        .:10 00000000000', a=Ra, b=Rb, op='BGED', delay=1)
class ConditionalIndirect(UnconditionalIndirect):
    def lift(self, il, delay_slot=None):
        cond = {
            'EQ':il.compare_equal,
            'NE':il.compare_not_equal,
            'LT':il.compare_unsigned_less_than,
            'LE':il.compare_unsigned_less_equal,
            'GT':il.compare_unsigned_greater_than,
            'GE':il.compare_unsigned_greater_equal,
        }[self.op[1:3]]
        if 'I' in self.op:
            t_dst = self.addr + self.b.i
            t = il.get_label_for_address(self.arch, t_dst)
            t_dst = il.const_pointer(4, t_dst)
        else:
            t_dst = self.b.r(il)
            t = None

        f_dst = self.addr + 4 + self.delay * 0  # not skipped
        f = il.get_label_for_address(self.arch, f_dst)
        f_dst = il.const_pointer(4, f_dst)

        p = cond(4, self.a.r(il), il.const(4, 0))
        if t and f:
            if delay_slot:
                delay_slot(il)
            il.append(il.if_expr(p, t, f))
            return
        if t:
            f = bn.LowLevelILLabel()
            il.append(il.if_expr(p, t, f))
            il.mark_label(f)
            if delay_slot:
                delay_slot(il)
            il.append(il.jump(f_dst))
            return
        if f:
            t = bn.LowLevelILLabel()
            il.append(il.if_expr(p, t, f))
            il.mark_label(t)
            if delay_slot:
                delay_slot(il)
            il.append(il.jump(t_dst))
            return
        t, f = bn.LowLevelILLabel(), bn.LowLevelILLabel()
        il.append(il.if_expr(p, t, f))
        il.mark_label(t)
        if delay_slot:
            delay_slot(il)
        il.append(il.jump(t_dst))
        il.mark_label(f)
        il.append(il.jump(f_dst))


@bitspec.dataclass('101000 -:10 -:16', d=Rd, a=Ra, b=Imm16, op='ORI')
@bitspec.dataclass('101001 -:10 -:16', d=Rd, a=Ra, b=Imm16, op='ANDI')
@bitspec.dataclass('101010 -:10 -:16', d=Rd, a=Ra, b=Imm16, op='XORI')
@bitspec.dataclass('101011 -:10 -:16', d=Rd, a=Ra, b=Imm16, op='ANDNI')
class BitImm(Arith): pass

@bitspec.dataclass('101100 00000 00000', b=Imm16, op='IMM')
class ImmPrefix(Op): pass

@bitspec.dataclass('101100 10',          b=Imm24, op='IMML')
class ImmLPrefix(Op): pass

@bitspec.dataclass('101101 10000 -:5 -:16', a=Ra, b=Imm16, op='RTSD', delay=1)
@bitspec.dataclass('101101 10001 -:5 -:16', a=Ra, b=Imm16, op='RTID', delay=1)
@bitspec.dataclass('101101 10010 -:5 -:16', a=Ra, b=Imm16, op='RTBD', delay=1)
@bitspec.dataclass('101101 10100 -:5 -:16', a=Ra, b=Imm16, op='RTED', delay=1)
class Returns(Op):
    delay = 1
    def info(self):
        ii = super().info()
        ii.add_branch(bn.BranchType.FunctionReturn)
        return ii

    def lift(self, il, delay_slot):
        if delay_slot:  # mandatory, but maybe we're lifting partial snippet
            delay_slot(il)
        il.append(il.ret(
            il.add(4, self.a.r(il), self.b.r(il))
        ))

@bitspec.dataclass('101110 imm:5 00010 0000000000000100', op='MBAR')
@bitspec.dataclass('101110 01000 00010 0000000000000100', op='HIBERNATE')
@bitspec.dataclass('101110 10000 00010 0000000000000100', op='SLEEP')
@bitspec.dataclass('101110 11000 00010 0000000000000100', op='SUSPEND')
class MemBarrier(Op):
    imm:int=-1

    def text(self):
        if self.imm != -1:
            tok, ty = bn.InstructionTextToken, bn.InstructionTextTokenType
            return [
                tok(ty.InstructionToken, name),
                tok(ty.OperandSeparatorToken, ' '),
                tok(ty.IntegerToken, hex(self.imm), self.imm),
            ]
        else:
            return super().text()

    def lift(self, il):
        if self.imm != -1:
            if self.imm in [0,2]:
                # also cleared when MBAR follows branch, but not worth effort
                # of checking for
                il.append(il.intrinsic([], 'btc_clear', []))
            if self.imm & 1:
                il.append(il.intrinsic([], 'mbar_i_side', []))
            if self.imm & 2:
                il.append(il.intrinsic([], 'mbar_d_side', []))


@bitspec.dataclass('101110 00000 00000 -:16',       b=Rel16, op='BRI')
@bitspec.dataclass('101110 00000 10000 -:16',       b=Rel16, op='BRID', delay=1)
@bitspec.dataclass('101110 ..... 10100 -:16', d=Rd, b=Rel16, op='BRLID', delay=1)
@bitspec.dataclass('101110 00000 01000 -:16',       b=Abs16, op='BRAI')
@bitspec.dataclass('101110 00000 11000 -:16',       b=Abs16, op='BRAID', delay=1)
@bitspec.dataclass('101110 ..... 11100 -:16', d=Rd, b=Abs16, op='BRAID', delay=1)
@bitspec.dataclass('101110 ..... 01100 -:16', d=Rd, b=Abs16, op='BRKI')
class UnconditionalImmediate(UnconditionalIndirect):
    def lift_t_dst(self, il):
        return self.b.r(il, self.addr)

@bitspec.dataclass('101110 00000 00000 0000 0000 0000 0100',       b=Rel16, op='BRI')
class SynchronizingBranch(UnconditionalImmediate):
    # odd duck BRI 4 synchronizing branch, invalidates Branch Target Cache
    def lift(self, il):
        if self.b.i == 4:  # check there's no 32bit imm prefix
            il.append(il.intrinsic([], 'btc_clear', []))
        else:
            super().lift(il)

C_BASE_VECTORS = 0x00000000  # might be elsewhere in some instantiations
assert C_BASE_VECTORS == 0   # if so, below decodings will be wrong
@bitspec.dataclass('101110 ..... 01100 0x0008', d=Rd, b=Abs16(0x08), op='BRKI')
@bitspec.dataclass('101110 ..... 01100 0x0018', d=Rd, b=Abs16(0x18), op='BRKI')
class MaybeSyscall(Op):
    def info(self):
        ii = super().info()
        ii.add_branch(bn.BranchType.SystemCall)
        return ii

    def lift(self, il):
        il.append(il.system_call())

@bitspec.dataclass('101111 00000 ..... -:16', a=Ra, b=Rel16, op='BEQI')
@bitspec.dataclass('101111 00001 ..... -:16', a=Ra, b=Rel16, op='BNEI')
@bitspec.dataclass('101111 00010 ..... -:16', a=Ra, b=Rel16, op='BLTI')
@bitspec.dataclass('101111 00011 ..... -:16', a=Ra, b=Rel16, op='BLEI')
@bitspec.dataclass('101111 00100 ..... -:16', a=Ra, b=Rel16, op='BGTI')
@bitspec.dataclass('101111 00101 ..... -:16', a=Ra, b=Rel16, op='BGEI')
@bitspec.dataclass('101111 10000 ..... -:16', a=Ra, b=Rel16, op='BEQID', delay=1)
@bitspec.dataclass('101111 10001 ..... -:16', a=Ra, b=Rel16, op='BNEID', delay=1)
@bitspec.dataclass('101111 10010 ..... -:16', a=Ra, b=Rel16, op='BLTID', delay=1)
@bitspec.dataclass('101111 10011 ..... -:16', a=Ra, b=Rel16, op='BLEID', delay=1)
@bitspec.dataclass('101111 10100 ..... -:16', a=Ra, b=Rel16, op='BGTID', delay=1)
@bitspec.dataclass('101111 10101 ..... -:16', a=Ra, b=Rel16, op='BGEID', delay=1)
class ConditionalImmediate(ConditionalIndirect):
    def info(self):
        f_dst = self.addr + 4 + self.delay * 0  # not skipped
        t_dst = self.addr + self.b.i
        ii = Op.info(self)  # make sure no redundant branches get added
        ii.add_branch(bn.BranchType.FalseBranch, f_dst)
        ii.add_branch(bn.BranchType.TrueBranch, t_dst)
        return ii

@bitspec.dataclass('110000 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=1, op='LBU')
@bitspec.dataclass('110000 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=1, op='LBUR', reverse=1)
@bitspec.dataclass('110000 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=1, op='LBUEA', ea=1, privileged=1)
@bitspec.dataclass('110001 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=2, op='LHU')
@bitspec.dataclass('110001 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=2, op='LHUR', reverse=1)
@bitspec.dataclass('110001 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=2, op='LHUEA', ea=1, privileged=1)
@bitspec.dataclass('110010 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=4, op='LW')
@bitspec.dataclass('110010 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=4, op='LWR', reverse=1)
@bitspec.dataclass('110010 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=4, op='LWEA', ea=1, privileged=1)
@bitspec.dataclass('110010 -:15 10000000000', d=Rd, a=Ra, b=Rb, sz=4, op='LWX')
@bitspec.dataclass('110010 -:15 00100000000', d=Rd, a=Ra, b=Rb, sz=8, op='LL')
@bitspec.dataclass('110010 -:15 01100000000', d=Rd, a=Ra, b=Rb, sz=8, op='LLR', reverse=1)
@bitspec.dataclass('110100 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=1, op='SB')
@bitspec.dataclass('110100 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=1, op='SBR', reverse=1)
@bitspec.dataclass('110100 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=1, op='SBEA', ea=1, privileged=1)
@bitspec.dataclass('110101 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=2, op='SH')
@bitspec.dataclass('110101 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=2, op='SHR', reverse=1)
@bitspec.dataclass('110101 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=2, op='SHEA', ea=1, privileged=1)
@bitspec.dataclass('110110 -:15 00000000000', d=Rd, a=Ra, b=Rb, sz=4, op='SW')
@bitspec.dataclass('110110 -:15 01000000000', d=Rd, a=Ra, b=Rb, sz=4, op='SWR', reverse=1)
@bitspec.dataclass('110110 -:15 00010000000', d=Rd, a=Ra, b=Rb, sz=4, op='SWEA', ea=1, privileged=1)
@bitspec.dataclass('110110 -:15 10000000000', d=Rd, a=Ra, b=Rb, sz=4, op='SWX')
@bitspec.dataclass('110110 -:15 00100000000', d=Rd, a=Ra, b=Rb, sz=8, op='SL')
@bitspec.dataclass('110110 -:15 01100000000', d=Rd, a=Ra, b=Rb, sz=8, op='SLR', reverse=1)
@bitspec.dataclass('111000        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=1, op='LBUI')
@bitspec.dataclass('111000 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=1, op='LBUI')
@bitspec.dataclass('111001        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=2, op='LHUI')
@bitspec.dataclass('111001 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=2, op='LHUI')
@bitspec.dataclass('111010        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=4, op='LWI')
@bitspec.dataclass('111010 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=4, op='LWI')
@bitspec.dataclass('111011        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=8, op='LLI')
@bitspec.dataclass('111100        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=1, op='SBI')
@bitspec.dataclass('111100 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=1, op='SBI')
@bitspec.dataclass('111101        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=2, op='SHI')
@bitspec.dataclass('111101 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=2, op='SHI')
@bitspec.dataclass('111110        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=4, op='SWI')
@bitspec.dataclass('111110 ..... 00000 -:16', d=Rd, a=Ra(0), b=Abs16, sz=4, op='SWI')
@bitspec.dataclass('111111        -:10 -:16', d=Rd, a=Ra,    b=Imm16, sz=8, op='SLI')
class Mem(Op):
    sz:int = -1
    reverse:bool = False
    ea:int = 0

    # all 8-byte lifts are incorrect, fix in ublaze64
    def lift(self, il):
        if not self.ea:
            addr = il.add(4, il.operand(2, self.a.r(il)), il.operand(3, self.b.r(il)))
        else:
            addr = il.or_expr(8,  # won't actually work until ublaze64xx
                il.operand(2, il.shift_left(8, self.a.r(il), il.const(1, 32))), 
                il.operand(3, self.b.r(il))
            )
        if self.op[0] == 'S':
            val = il.operand(1, self.d.r(il))
            if self.reverse:
                val = ntoh_dword(il, self.arch, val, self.sz)
            if self.op[-1] == 'X':  # mark LWX reservation
                il.append(il.set_reg(4, 'reserved', addr))
            il.append(il.store(self.sz, addr, val))
        elif self.op[0] == 'L': 
            val = il.operand(1, il.load(self.sz, addr))
            if self.sz != 4:
                val = il.zero_extend(4, val)
            if self.reverse:
                val = ntoh_dword(il, self.arch, val, self.sz)
            if self.op[-1] == 'X':  # check LWX reservation
                il.append(il.set_flag('c', 
                    il.compare_not_equal(4,
                        addr,
                        il.reg(4, 'reserved'),
                    )
                )) # if flag's set, exclusive transaction got stomped
                   # might not be clear enough in HLIL with just sysreg,
                   # switch to intrinsics if so
            self.d.w(il, val)


def ntoh_dword(il, arch, val, size=4):
    # If these *aren't* rare on your target, you'll need to write a proper
    # lift since the intrinsic will break VSA in dataflows it's inserted into.
    if size != 4:
        return il.unimplemented()
    il.append(il.intrinsic(
        [arch.regs['temp']],
        'ntoh',
        [val],
    ))
    return il.reg(4, 'temp')
