-- | Some useful statistics

let std [n] (xs:[n]f32): f32 =
    let mu = (reduce (+) 0f32 xs) / (f32.i64 n)
    let n_var = map (\x -> (x - mu)*(x - mu)) xs |> reduce (+) 0f32
    in f32.sqrt (n_var / (f32.i64 n))


-- Welford online variance
module welford = {

type state = {count: i64, mean: f32, m2: f32}

type result = {mean: f32, var: f32, std: f32}

let init (y:f32): state = {count=1, mean=y, m2=0f32}

let update ({count,mean,m2}:state) (y:f32): state =
  let count = count + 1
  let delta = y - mean
  let mean = mean + delta / (f32.i64 count)
  let delta2 = y - mean
  let m2 = m2 + delta * delta2
  in {count, mean, m2}

let finalize ({count,mean,m2}:state): result =
  let var = m2 / (f32.i64 count)
  let std = f32.sqrt var
  in {mean, var, std}

}
