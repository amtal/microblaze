import binaryninja 
import cProfile 
profile = cProfile.Profile()
import os.path
import atexit
def dump():
    """atexit not reliable, just do

    >>> from microblaze import bench_hook; bench_hook.dump()
    """
    dst = os.path.join(os.path.dirname(__file__), 'bench', 'profile.stats')
    profile.dump_stats(dst)
atexit.register(dump)

def decorate(profile, fun):
    def profile_closure(*args, **kwargs):
        profile.enable()
        try:
            return fun(*args, **kwargs)
        finally:
            profile.disable()
    return profile_closure

def hook_arch(cls):
    cls.get_instruction_info = decorate(profile, cls.get_instruction_info)
    cls.get_instruction_text = decorate(profile, cls.get_instruction_text)
    cls.get_instruction_low_level_il = decorate(profile, cls.get_instruction_low_level_il)
    binaryninja.log_warn("Performance impact from bench_hook.py profiling, check if it's still needed.")
    

from . import arch
hook_arch(arch.MicroBlaze32LE)
hook_arch(arch.MicroBlaze32EB)
