-- plain radix2 w/ f32

type c32 = (f32, f32)

let cadd ((a,b):c32) ((c,d):c32): c32 = (a + c, b + d)
let csub ((a,b):c32) ((c,d):c32): c32 = (a - c, b - d)
let cmul ((a,b):c32) ((c,d):c32): c32 = ((a * c - b * d, b * c + a * d))
let Cadd as bs = map2 cadd as bs
let Csub as bs = map2 csub as bs
let Cmul as bs = map2 cmul as bs

module type fftl2n = {
    val N: i64
    val fft [n]: [n]c32 -> [n]c32
}

module fftl2 = {
    let N = 2i64
    let fft [n] (x:[n]c32): [n]c32 = [cadd x[0] x[1], csub x[0] x[1]] :> [n]c32
}

module mk_fftl2n (F: fftl2n) = {
    let N = 2 * F.N

    let W = iota N
        |> map (\k -> f32.pi*(f32.i64 k)/(f32.i64 N))
        |> map (\x -> (f32.cos x, -(f32.sin x)))

    let fft [n] (x:[n]c32): [n]c32 =
        let n2 = n / 2
        let E = F.fft x[0::2] :> [n2]c32
        let O = F.fft x[1::2] :> [n2]c32
        let L = Cadd E (Cmul W[:n2] O)
        let R = Csub E (Cmul W[:n2] O)
        in (L ++ R) :> [n]c32
}

module fft128 = mk_fftl2n (mk_fftl2n (mk_fftl2n (mk_fftl2n (mk_fftl2n (mk_fftl2n fftl2)))))
