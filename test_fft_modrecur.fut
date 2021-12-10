import "lib/github.com/maedoc/tvb-fut/fft-modrecur"


-- mkfft2n fft128 causes OpenCL drivers to crash
-- so we use an array version instead
module fft256 = mk_fft2na fft128

-- ==
-- input { 128i64 }
-- input { 768i64 }
entry main (M:i64) =
        let x = tabulate_2d M fft256.Nr (\i j -> (fft256.R.i64 i) fft256.R.+ (fft256.R.i64 j)) :> [M][fft256.Nr]fft256.R.t
        let y = map (\xi -> [fft128.ar2t (xi[:256]:>[fft128.Nr]fft128.R.t), fft128.ar2t (xi[256:]:>[fft128.Nr]fft128.R.t)]) x
        in map fft256.apply y

-- ==
-- input { 128i64 }
-- input { 768i64 }
entry main128 (M:i64) =
        let x = tabulate_2d M fft128.Nr (\i j -> (fft128.R.i64 i) fft128.R.+ (fft128.R.i64 j)) :> [M][fft128.Nr]fft128.R.t
        let y = map fft128.ar2t x
        in map fft128.apply y

-- about half speed of genfut.py version, but it's doing full complex fft
-- so the input (128x256) is twice as large ~2MB vs ~1MB.

-- would be work doing an rfft variant with real input data

-- worse, tho is that it appears too large to function on NVIDIA OpenCL (3MB .c file!)
