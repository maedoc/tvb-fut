import "lib/github.com/maedoc/tvb-fut/spatepi"
import "lib/github.com/maedoc/tvb-fut/util"

-- ==
-- input { 1024i64 0 }
-- input { 8192i64 0 }
-- input { 16384i64 0 }
-- input { 32768i64 0 }
-- input { 66000i64 0 }
-- input { 400000i64 0 }
entry main (n:i64) (grad:i32): [n]f32 = 
    let t = 1i64
    let nodes = tabulate n (\i -> (-1.4624261f32+((f32.i64 i)*1e-3), -9.6934490f32+((f32.i64 i)*1e-3),  2.9502959f32+((f32.i64 i)*1e-3), -1.1118182f32+((f32.i64 i)*1e-3),
 -9.5610595e-20+((f32.i64 i)*1e-3), -4.3872780e-01+((f32.i64 i)*1e-3)))
    let lcs = tabulate n (\i -> (0.1f32+((f32.i64 i)*1e-3), 0.1f32+((f32.i64 i)*1e-3), 0.1f32+((f32.i64 i)*1e-3)))
    let cs = tabulate n (\i -> 0.1f32+((f32.i64 i) * 1e-3))
    let x0s = tabulate n (\i -> -1.8f32+((f32.i64 i) * 1e-3))
    let dfun = spatepi.dfun 0.169 0.318 0.0318 1.0 3.1 0.45 0.17 1.0 2857.0 1.0
    let h = 0.1f32
    let eu (a,b,c,d,e,f) (da,db,dc,dd,de,df) = (a+h*da,b+h*db,c+h*dc,d+h*dd,e+h*de,f+h*df)
    let f (n, c, l, x) = loop (n) for i < t do (eu n (dfun n c l x))
    in
    map4 (\n c l x -> f (n,c,l,x)) nodes cs lcs x0s |> unzip6 |> (.0)
