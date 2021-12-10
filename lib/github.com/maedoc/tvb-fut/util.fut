-- | A collection of utilities.

let unzip6 [n] 'a 'b 'c 'd 'e 'f (xs: [n](a,b,c,d,e,f)): ([n]a, [n]b, [n]c, [n]d, [n]e, [n]f) =
  let (as, bs, cs, ds, efs) = unzip5 (map (\(a,b,c,d,e,f) -> (a,b,c,d,(e,f))) xs)
  let (es, fs) = unzip efs
  in (as, bs, cs, ds, es, fs)

let npsign (x:f32): f32 =
    if x < 0f32 then -1f32 else if x > 0f32 then 1f32 else 0f32