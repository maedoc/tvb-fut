import numpy as np
from futhark_ffi import Futhark
import os
os.system('futhark c --library test_fft_modrecur.fut')
os.system('build_futhark_ffi test_fft_modrecur')
import _test_fft_modrecur
fft = Futhark(_test_fft_modrecur)

N = 128
z = np.random.randn(128).astype(np.complex64)
Z = np.fft.fft(z)
z_ = np.c_[z.real, z.imag].flat[:]
Z2_ = fft.from_futhark(fft.fft128(z_))
Z2 = np.zeros_like(Z)
Z2.real[:] = Z2_[0::2]
Z2.imag[:] = Z2_[1::2]

np.testing.assert_allclose(Z, Z2)
