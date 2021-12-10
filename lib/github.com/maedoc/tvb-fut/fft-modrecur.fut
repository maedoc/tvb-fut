-- construct higher radix FFTs via module system

module type fft2_t = {
    module R: real
    -- type of half of transform i.e. (R.t,R.t) for N=2
    type t
    -- length of transform and number of reals involved
    val N: i64
    val Nr: i64
    -- twiddle factors for stage N used by apply at stage 2*N
    val W: (t, t)
    -- ops on stage N used by apply at stage 2*N
    val t0: t
    val +: t -> t -> t
    val -: t -> t -> t
    val *: t -> t -> t
    -- apply N point transform
    val apply: (t, t) -> (t, t)
    -- conversion between t & array of time points
    val a2t: [N](R.t,R.t) -> (t, t)
    val t2a: (t, t) -> [N](R.t,R.t)
    -- conversion between t & array of reals
    val ar2t: [Nr]R.t -> (t, t)
    val t2ar: (t, t) -> [Nr]R.t
}

-- base case with two point transform
module mk_fft2 (R: real) = {
    module R = R
    type t = (R.t, R.t)
    let t0: t = (R.i32 0, R.i32 0)
    let N = 2i64
    let Nr = 4i64
    let W = ((R.i64 1,R.i64 0), (R.i64 (1-2),R.i64 0))
    let (+) (ar,ai) (br,bi) = (ar R.+ br, ai R.+ bi)
    let (-) (ar,ai) (br,bi) = (ar R.- br, ai R.- bi)
    -- complex multiplication
    let (*) (a,b) (c,d) = R.((a * c - b * d, b * c + a * d))
    let apply ((a, b): (t, t)): (t, t) = (a + b, a - b)
    let ar2t (ab:[Nr]R.t): (t, t) = ((ab[0], ab[1]),(ab[2], ab[3]))
    let t2ar ((a,b),(c,d)): [Nr]R.t = [a,b,c,d] :> [Nr]R.t
    let a2t (ab:[N](R.t,R.t)): (t, t) = (ab[0], ab[1])
    let t2a (a,b): [N](R.t,R.t) = [a,b] :> [N](R.t,R.t)
}

-- compute 2*N transform given module computing N point transform
module mk_fft2n (F: fft2_t) = {
    module R = F.R
    type t = (F.t, F.t)
    type w = (R.t, R.t)
    let t0 = (F.t0, F.t0)
    let N = 2 * F.N
    let Nr = 2 * F.Nr
    let (+) (ar,ai) (br,bi) = (ar F.+ br, ai F.+ bi)
    let (-) (ar,ai) (br,bi) = (ar F.- br, ai F.- bi)
    -- elementwise, not using complex formula
    let (*) (ar,ai) (br,bi) = (ar F.* br, ai F.* bi)
    let ar2t (x:[Nr]R.t) = (F.ar2t (x[:F.Nr] :> [F.Nr]R.t), F.ar2t (x[F.Nr:] :> [F.Nr]R.t))
    let t2ar (a,b): [Nr]R.t = ((F.t2ar a) ++ (F.t2ar b)) :> [Nr]R.t
    let a2t (ab:[N](R.t,R.t)) = (F.a2t (ab[:F.N] :> [F.N]w), F.a2t (ab[F.N:] :> [F.N]w))
    let t2a (a,b) = ((F.t2a a) ++ (F.t2a b)) :> [N]w

    -- cos(2*pi*k/N), -sin(2*pi*k/N)
    let ws = iota N
            |> map (\k -> R.((i64 2)*pi*(i64 k)/(i64 N)))
            |> map (\x -> (R.cos x, R.neg (R.sin x)))
    let W = a2t ws

    let apply (a, b) = 
        -- split a & b into even and odd time time points
        let ab: [N]w = t2a (a, b)
        let e: t = F.a2t (ab[0::2]:>[F.N]w)
        let o: t = F.a2t (ab[1::2]:>[F.N]w)
        -- do subtransforms on even and odd
        let E: t = F.apply e
        let O: t = F.apply o
        let L: t = E + W.0 * O
        let R: t = E - W.0 * O
        in (L, R)
}

-- compute 2*N transform given module computing N point transform w/ arrays
module mk_fft2na (F: fft2_t) = {
    module R = F.R
    type t = [2]F.t
    type w = (R.t, R.t)
    let t0 = [F.t0, F.t0]
    let N = 2 * F.N
    let Nr = 2 * F.Nr
    let (+) as bs = [as[0] F.+ bs[0], as[1] F.+ bs[1]]
    let (-) as bs = [as[0] F.- bs[0], as[1] F.- bs[1]]
    let (*) as bs = [as[0] F.* bs[0], as[1] F.* bs[1]]

    -- cos(2*pi*k/N), -sin(2*pi*k/N)
    let ws = iota N
            |> map (\k -> R.((i64 2)*pi*(i64 k)/(i64 N)))
            |> map (\x -> (R.cos x, R.neg (R.sin x)))

    let split ((a,b):(F.t,F.t)): t = [a,b]
    let W: t = split(F.a2t (ws[:N/2]:>[F.N]w))

    let apply ab: [4]F.t = 
        -- split a & b into even and odd time time points
        let ab: [N]w = ((F.t2a ab[0]) ++ (F.t2a ab[1])) :> [N]w
        let e: (F.t,F.t) = F.a2t (ab[0::2]:>[F.N]w)
        let o: (F.t,F.t) = F.a2t (ab[1::2]:>[F.N]w)
        -- do subtransforms on even and odd
        let E: t = split (F.apply e)
        let O: t = split (F.apply o)
        let L: t = E + W * O
        let R: t = E - W * O
        in ((L ++ R) :> [4]F.t)
}

module fft2 = mk_fft2 f32
module fft4 = mk_fft2n fft2
module fft8 = mk_fft2n fft4
module fft16 = mk_fft2n fft8
module fft32 = mk_fft2n fft16
module fft64 = mk_fft2n fft32
module fft128 = mk_fft2n fft64
