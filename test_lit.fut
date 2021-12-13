-- Construct a table
let e = tabulate_2d 64 64 (\i j -> (f32.i64 (i * j)) / (f32.i64 (64 * 64)))
-- > :covert :img e

-- Now show a value
let f = iota 5
-- > f
