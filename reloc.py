import enum
import ctypes as ct
from typing import Optional, List
import binaryninja as bn
from binaryninja import core

BN_AUTOCOERCE_EXTERN_PTR = 0xfffffffd  # binaryninjacore.h
BN_NOCOERCE_EXTERN_PTR = 0xfffffffe

class CustomRelocationHandler:
    """Link or load time pointer fixups.

    Generally speaking, relocations are used for modifying bytes at a given
    address to point to some symbol. These symbols usually don't exist in the
    binary and rather exist in some other module, which Binary Ninja doesn't
    know about. To solve this issue we first record all the symbols referenced
    by this binary and at the point of BinaryView finalization we assign these
    symbols addresses in a fake segment which we create at the end of the
    binary.
    
    See ELF/COFF examples in C++ ARM/x86/MIPS architectures for Binary Ninja
    implementation details. See architecture-specific documentation or source
    code for your file format for relocation behavior.
    """
    _instance = None

    def __init__(self):
        """
        
        .. warning:: Do not instnatiate directly. Use register(...) instead.
        """
        context=None  # TODO worth plumbing this through? why? GIL singleton anyway
        self._cb = core.BNCustomRelocationHandler()
        self._cb.context = context
        self._cb.freeObject = self._cb.freeObject.__class__(self._free_object)

        self._cb.getRelocationInfo = self._cb.getRelocationInfo.__class__(self._get_relocation_info)
        self._cb.applyRelocation = self._cb.applyRelocation.__class__(self._apply_relocation)
        self._cb.getOperandForExternalRelocation = self._cb.getOperandForExternalRelocation.__class__(
            self._get_operand_for_external_relocation
        )

        self.handle = core.BNCreateRelocationHandler(self._cb)


    def _get_relocation_info(self, _ctxt, view, arch, result, result_count):
        try:
            view = bn.binaryview.BinaryView(
                file_metadata=bn.filemetadata.FileMetadata(handle=core.BNGetFileForView(view)),
                handle=core.BNNewViewReference(view)
            )
            arch = bn.CoreArchitecture._from_cache(handle=arch)
            results = [result[i] for i in range(result_count)]
            return self.get_relocation_info(view, arch, results)
        except OSError:
            log.log_error(traceback.format_exc())
            return False


    def _apply_relocation(self, _ctxt, view, arch, reloc, dest, size):
        try:
            view = bn.binaryview.BinaryView(
                file_metadata=bn.filemetadata.FileMetadata(handle=core.BNGetFileForView(view)),
                handle=core.BNNewViewReference(view)
            )
            arch = bn.CoreArchitecture._from_cache(handle=arch)
            # TODO dest, size
            result = self.apply_relocation(view, arch, reloc, dest, size)
            return result if type(result) == bool else False
        except OSError:
            log.log_error(traceback.format_exc())
            return False


    def _get_operand_for_external_relocation(self, _ctxt, data, addr, length, il, reloc):
        try:
            buf = ct.create_string_buffer(length[0])
            ct.memmove(buf, data, length[0])
            il = bn.lowlevelil.LowLevelILFunction(self, core.BNNewLowLevelILFunctionReference(il))
            # TODO reloc obj wrapper
            reloc = core.BNNewRelocationReference(reloc)
            result = self.get_operand_for_external_relocation(buf.raw, addr, il, reloc)
            return result if type(result) == int else BN_AUTOCOERCE_EXTERN_PTR
        except OSError:
            log.log_error(traceback.format_exc())
            return BN_AUTOCOERCE_EXTERN_PTR


    def _free_object(self, _ctxt):
        pass


    def __del__(self):
        core.BNFreeRelocationHandler(self.handle)


    @classmethod
    def register(cls, arch:bn.Architecture, view_name:str):
        """Extend a binary view that looks for relocations with a new arch.

        view_name: one of `[t.name for t in list(BinaryViewType)]`, but
                   most likely "ELF"/"Mach-O"/"PE".
        """
        if cls._instance is None:
            cls._instance = cls()
        core.BNArchitectureRegisterRelocationHandler(arch.handle, view_name, cls._instance.handle)


    def get_relocation_info(self, view, arch, results:List[core.BNRelocationInfo]) -> bool:
        """aaaa build reloc info sometimes, it'll get lazily applied l8r

        (I guess actual fix-up is lazy because not all segments may be loaded
            by the time this is hit?)
        """
        assert type(view) == bn.BinaryView
        #bn.log_info(f'{self}.get_relocation_info(bv, {arch}, {results})')
        return False


    def apply_relocation(self, view, arch, reloc:core.BNRelocation, dest, size) -> bool:
        """lazily called to fix-up previously-emitted relocations
        """
        assert type(view) == bn.BinaryView
        #bn.log_info(f'{self}.apply_relocation(bv, {arch}, {reloc}, {dest}, {size})')
        return core.BNRelocationHandlerDefaultApplyRelocation(
            self.handle, view.handle, arch.handle, 
            core.BNNewRelocationReference(reloc), 
            dest, size
        )


    def get_operand_for_external_relocation(self, data, addr, il, reloc:core.BNRelocation) -> Optional[int]:
        """Get operand index to display the relocation annotation next to.

        GetOperandForExternalRelocation is called to attempt to determine which
        LLIL operand contains the pointer. It helps analysis to know which
        constants are in fact pointers.
        """
        #bn.log_info(f'{self}.get_operand_for_external_relocation({data}, {hex(addr)}, {il}, {reloc})')
        return BN_AUTOCOERCE_EXTERN_PTR


class R_MICROBLAZE(enum.IntEnum):
    """
    Table 4-7 of UG984 (v2020.1) June 3, 2020 is bad. Instead see glibc:
        https://github.com/Xilinx/glibc/blob/xlnx/glibc-2_19-branch/elf/elf.h#L3103
        https://github.com/Xilinx/glibc/blob/xlnx/glibc-2_19-branch/ports/sysdeps/microblaze/dl-machine.h#L227
    """
    _NONE           = 0
    _32             = 1
    _32_PCREL       = 2
    _64_PCREL       = 3
    _32_PCREL_LO    = 4
    _64             = 5
    _32_LO          = 6
    _SRO32          = 7
    _SRW32          = 8
    _64_NONE        = 9
    _32_SYM_OP_SYM  = 10
    _GNU_VTINHERIT  = 11
    _GNU_VTENTRY    = 12
    _GOTPC_64       = 13
    _GOT_64         = 14
    _PLT_64         = 15
    _REL            = 16
    _JUMP_SLOT      = 17
    _GLOB_DAT       = 18
    _GOTOFF_64      = 19
    _GOTOFF_32      = 20
    _COPY           = 21
    _TLS            = 22
    _TLSGD          = 23
    _TLSLD          = 24
    _TLSDTPMOD32    = 25
    _TLSDTPREL32    = 26
    _TLSDTPREL64    = 27
    _TLSGOTTPREL32  = 28
    _TLSTPREL32     = 29


import struct
class MicroBlazeELFRelocationHandler(CustomRelocationHandler):
    def get_relocation_info(self, view, arch, results):
        for reloc in results:
            reloc.type = bn.RelocationType.StandardRelocationType
            if reloc.nativeType == R_MICROBLAZE._REL:
                reloc.pcRelative = False;
                reloc.baseRelative = True;
                reloc.hasSign = False;
                reloc.size = 4;
                reloc.truncateSize = 4;
                reloc.implicitAddend = False;
            elif reloc.nativeType in [R_MICROBLAZE._GLOB_DAT, 
                                      R_MICROBLAZE._JUMP_SLOT, 
                                      R_MICROBLAZE._32]:
                if reloc.nativeType == R_MICROBLAZE._GLOB_DAT:
                    reloc.type = bn.RelocationType.ELFGlobalRelocationType
                elif reloc.nativeType == R_MICROBLAZE._JUMP_SLOT:
                    reloc.type = bn.RelocationType.ELFJumpSlotRelocationType
                reloc.pcRelative = False
                reloc.baseRelative = False
                if reloc.nativeType == R_MICROBLAZE._32:
                    reloc.hasSign = False
                reloc.size = 4
                reloc.truncateSize = 4
            elif reloc.nativeType == R_MICROBLAZE._COPY:
                reloc.type = bn.RelocationType.ELFCopyRelocationType
                reloc.pcRelative = false
                reloc.baseRelative = False
                reloc.size = 4
                reloc.truncateSize = 4
            elif reloc.nativeType == R_MICROBLAZE._NONE:
                reloc.type = bn.RelocationType.IgnoredRelocation
            else:
                reloc.type = bn.RelocationType.UnhandledRelocation
                bn.log_warn('Unimplemented ELF relocation type: %d' % (R_MICROBLAZE(reloc.nativeType),))
        #if len(results):
        #    bn.log_info(f'{self}.get_relocation_info(bv, {arch}, |{len(results)}|) / {reloc.nativeType}')
        return True

    def apply_relocation(self, view, arch, reloc:core.BNRelocation, dest, size):
        ret = super().apply_relocation(view, arch, reloc, dest, size)
        if arch.endianness == bn.Endianness.BigEndian and size>=4:  # lol
            dest[0],dest[1],dest[2],dest[3] = dest[3],dest[2],dest[1],dest[0]
        return ret
