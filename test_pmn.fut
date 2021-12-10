
let len_q (N:i64): i64 = (N + 1) * (N + 2) // 2

let gen_ml (N:i64): [](i64,i64) =
    loop ml = [(0,0)] for i < ((len_q N)-1) do
        let (m,l) = ml[i]
        let nl = if N == l then m + 1 else l + 1
        let nm = if N == l then m + 1 else m
        in ml ++ [(nm,nl)]

entry gen_ml_arr (N:i64): [][]i64 =
    let (m,l) = gen_ml N |> unzip in [m,l]

entry all_amm (N:i64): []f32 = iota N 
    |> map (\i -> f32.i64 (i + 1))
    |> map (\k -> (2*k+1)/(2*k))
    |> ([1f32]++)
    |> scan (*) 1f32
    |> map (\e -> f32.sqrt(e/(4*f32.pi)))

entry amn (m:f32) (n:f32): f32 = f32.sqrt((4*n*n - 1)/(n*n - m*m))

entry bmn (m:f32) (n:f32): f32 =
    let l = (2*n + 1)/(2*n - 3)
    let r = ((n - 1)*(n - 1) - m*m)/(n*n - m*m)
    in (-f32.sqrt(l * r))

entry lat_grid (nlat:i64): []f32 = iota nlat
    |> map f32.i64 
    |> map (\x -> f32.cos (x/(f32.i64 nlat)*f32.pi))


-- traverse Pmn for given m and lmax
let Pmn1 (m:i64) (n:i64) (amm:f32) (cx:f32): f32 =
    let m' = f32.i64 m
    -- P^m_m
    let p0 = amm*(1 - cx*cx)**(m'/2)*(-1)**m'
    -- P^m_(m + 1)
    let p1 = (amn m' (m' + 1))*cx*p0
    -- P^m_n -> P^m_n+1 -> P^m_n+2
    let p2 n p1 p0 = (amn m' n)*cx*p1 + (bmn m' n)*p0
    -- P^m_n
    let (pn, _) = match (n-m)
        case 0 -> (p0, 0f32)
        case 1 -> (p1, p0)
        case _ -> loop (p1,p0) 
            for i < (n-m-1) do (p2 (m'+2+f32.i64 i) p1 p0, p1)
    in pn

entry Pmn [nlat] (m:i64) (n:i64) (amm:f32) (cx:[nlat]f32): [nlat]f32 =
    map (Pmn1 m n amm) cx

entry Lmx [nlat] (m:i64) (np1:i64) (amm:f32) (cx:[nlat]f32) (x:[nlat]f32): [np1]f32 =
    let n = np1 - 1
    let X = tabulate np1 (\i -> 0f32)
    let m' = f32.i64 m
    let Sx p = map2 (*) p x |> reduce (+) 0f32
    -- P^m_m
    let p0 = map (\cx -> amm*(1 - cx*cx)**(m'/2)*(-1)**m') cx
    let X[m] = Sx p0
    -- P^m_(m + 1)
    let p1 = map2 (\cx p0 -> (amn m' (m' + 1))*cx*p0) cx p0
    let X[m + 1] = Sx p1
    -- P^m_n -> P^m_n+1 -> P^m_n+2
    let p2 n p1 p0 = map3 (\cx p1 p0 -> (amn m' n)*cx*p1 + (bmn m' n)*p0) cx p1 p0
    -- P^m_n
    let (X, pn, _) = match (n-m)
        case 0 -> (X, p0, p0)
        case 1 -> (X, p1, p0)
        case _ -> loop (X,p1,p0) 
            for i < (n-m-1) do 
                let pi = p2 (m'+2+f32.i64 i) p1 p0
                let X[m+2+i] = Sx pi
                in (X, pi, p1)
    in X

entry Lmx2 [nlat] (m:i64) (np1:i64) (amm:f32) (amn:[]f32) (bmn:[]f32) (cx:[nlat]f32) (x:[nlat]f32): [np1]f32 =
    let n = np1 - 1
    let X = tabulate np1 (\i -> 0f32)
    let m' = f32.i64 m
    let Sx p = map2 (*) p x |> reduce (+) 0f32
    -- P^m_m
    let p0 = map (\cx -> amm*(1 - cx*cx)**(m'/2)*(-1)**m') cx
    let X[m] = Sx p0
    -- P^m_(m + 1)
    let p1 = map2 (\cx p0 -> amn[m+1]*cx*p0) cx p0
    let X[m + 1] = Sx p1
    -- P^m_n -> P^m_n+1 -> P^m_n+2
    let p2 n p1 p0 = map3 (\cx p1 p0 -> amn[n]*cx*p1 + bmn[n]*p0) cx p1 p0
    -- P^m_n
    let (X, pn, _) = match (n-m)
        case 0 -> (X, p0, p0)
        case 1 -> (X, p1, p0)
        case _ -> loop (X,p1,p0) 
            for i < (n-m-1) do 
                let pi = p2 (m+2+i) p1 p0
                let X[m+2+i] = Sx pi
                in (X, pi, p1)
    in X

-- == 
-- input { 6i64 50i64 128i64 1 }
-- input { 48i64 50i64 128i64 1 }
-- input { 6i64 50i64 128i64 2 }
-- input { 48i64 50i64 128i64 2 }
entry main (nxfm:i64) (lmax:i64) (nlat:i64) (variant:i32) =
    -- lmax > nlat
    let amm = all_amm lmax
    let np1 = lmax + 1
    let amn' = tabulate_2d np1 np1 (\m n -> amn (f32.i64 m) (f32.i64 n))
    let bmn' = tabulate_2d np1 np1 (\m n -> bmn (f32.i64 m) (f32.i64 n))
    let x = tabulate_3d nxfm lmax nlat (\i j k -> (f32.i64 i)+(f32.i64 j)+(f32.i64 k))
    let cx = lat_grid nlat
    in match variant
        case 1 -> map (\x' -> map2 (\m x -> Lmx m np1 amm[m] cx x) (iota lmax) x') x
        case 2 -> map (\x' -> map2 (\m x -> Lmx2 m np1 amm[m] amn'[m] bmn'[m] cx x) (iota lmax) x') x