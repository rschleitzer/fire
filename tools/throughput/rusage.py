# rusage.py <pid>: what the kernel has counted of a process -- instructions retired, of
# them on the performance cores, and its processor time, of it on the performance cores
# (macOS: proc_pid_rusage, flavor 6; the same user may read it without privileges).
# Elsewhere "- - - -". An instruction counts the same on either kind of core; a second of
# processor time does not, which is what this is for.
import ctypes, sys
try:
    lib = ctypes.CDLL('/usr/lib/libproc.dylib')
    class Usage(ctypes.Structure):
        _fields_ = [('uuid', ctypes.c_uint8 * 16), ('f', ctypes.c_uint64 * 64)]
    u = Usage()
    if lib.proc_pid_rusage(ctypes.c_int(int(sys.argv[1])), ctypes.c_int(6), ctypes.byref(u)) != 0:
        raise OSError()
    f = u.f
    # rusage_info_v6: user 0, system 1, instructions 29, user and system on P cores 36, 37,
    # instructions on P cores 38 (times in ticks of the absolute clock)
    print(f[29], f[38], f[0] + f[1], f[36] + f[37])
except Exception:
    print('- - - -')
