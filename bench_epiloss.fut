import "lib/github.com/maedoc/tvb-fut/epileptor"
import "lib/github.com/maedoc/tvb-fut/stats"

type node = epileptor.node

entry node_eval (y:[6]f32) (c_pop1:f32) (c_pop2:f32): [6]f32 =
  let y' = (y[0],y[1],y[2],y[3],y[4],y[5])
  let (d1,d2,d3,d4,d5,d6) = epileptor.node y' c_pop1 c_pop2
  in [d1,d2,d3,d4,d5,d6]

let init = (-1.5f32, -11.3f32, 3.0f32, -0.82f32, 0.04f32, -0.15f32)

let run [n] (nt:i64) (dt:f32) (init:[n]node) (w:[n][n]f32): [][n]node = 
    let y = tabulate nt (\_ -> init)
    in loop (y) for i < (nt - 1) do
        let y[i+1] = epileptor.heun dt (copy y[i]) w
        in y

let lfp ((x1,_,_,x2,_,_):node): f32 = x2 - x1

let run_online_std [n] (nt:i64) (dt:f32) (init:[n]node) (w:[n][n]f32): [n]f32 =
    let v = map (\node -> lfp node |> welford.init) init
    let (_, v) = loop (y, v) = (init, v)
        for i < (nt - 1) do
            let y = epileptor.heun dt y w
            let v = map2 (\v y -> welford.update v (lfp y)) v y
            in (y, v)
    in map (\v -> (welford.finalize v).std) v

let run_z [n] (nt:i64) (dt:f32) (z:[n]f32) (w:[n][n]f32): [][n]node = 
    let (x1,y1,_,x2,y2,g) = init
    let y0 = map (\z -> (x1,y1,z,x2,y2,g)) z
    in run nt dt y0 w

let run_z_online [n] (nt:i64) (dt:f32) (z:[n]f32) (w:[n][n]f32): []f32 = 
    let (x1,y1,_,x2,y2,g) = init
    let y0 = map (\z -> (x1,y1,z,x2,y2,g)) z
    in run_online_std nt dt y0 w

entry run_z_array [n] (nt:i64) (dt:f32) (z:[n]f32) (w:[n][n]f32): [nt][n][6]f32 =
  let y = run_z nt dt z w
  let to_array (yi:[n]node): [n][6]f32 = map (\(a,b,c,d,e,f) -> [a,b,c,d,e,f]) yi
  in map to_array y

let forward [nt] [n] (y:[nt][n]node) =
    let lfp: [n][nt]f32 = map (map (\(x1,_,_,x2,_,_) -> x2 - x1)) y |> transpose
    in map std lfp

entry forward_array [nt] [n] (y:[nt][n][6]f32): [n]f32 =
    let lfp: [n][nt]f32 = map (map (\x -> x[3] - x[0])) y |> transpose
    in map std lfp

let sse [n] (x:[n]f32) (y:[n]f32): f32 =
    map2 (\x y -> (x - y)*(x - y)) x y |> reduce (+) 0f32

entry loss [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): f32 =
    run_z nt dt zh w |> forward |> sse z

entry loss_online [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): f32 =
    run_z_online nt dt zh w |> sse z

entry vjp_loss_online [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): [n]f32 =
  vjp (loss_online nt dt w z) zh 1f32

entry jvp_loss_online [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): [n]f32 =
  let one i = jvp (loss_online nt dt w z) zh (tabulate n (\j -> if i==j then 1f32 else 0f32))
  in map one (iota n)

entry vjp_loss [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): [n]f32 =
  vjp (loss nt dt w z) zh 1f32

entry jvp_loss [n] (nt:i64) (dt:f32) (w:[n][n]f32) (z:[n]f32) (zh:[n]f32): [n]f32 =
  let one i = jvp (loss nt dt w z) zh (tabulate n (\j -> if i==j then 1f32 else 0f32))
  in map one (iota n)
-- because of the type of jvp we need to write the overall loss differently to
-- push jvp further down.


-- ==
-- random input { 75i64 [76]f32 [76]f32 [76][76]f32 0i64 }
-- random input { 75i64 [76]f32 [76]f32 [76][76]f32 1i64 }
-- random input { 75i64 [76]f32 [76]f32 [76][76]f32 2i64 }
-- random input { 75i64 [76]f32 [76]f32 [76][76]f32 3i64 }
-- random input { 75i64 [76]f32 [76]f32 [76][76]f32 4i64 }
-- random input { 750i64 [76]f32 [76]f32 [76][76]f32 0i64 }
-- random input { 750i64 [76]f32 [76]f32 [76][76]f32 1i64 }
-- random input { 750i64 [76]f32 [76]f32 [76][76]f32 2i64 }
-- random input { 750i64 [76]f32 [76]f32 [76][76]f32 3i64 }
-- random input { 750i64 [76]f32 [76]f32 [76][76]f32 4i64 }
let main [n] (nt:i64) (z:[n]f32) (zh:[n]f32) (w:[n][n]f32) (mode:i64): [n]f32 =
  let dt: f32 = 0.01 in
  match mode
  case 0i64 -> loss nt dt w z zh |> replicate n
  case 1i64 -> vjp_loss nt dt w z zh
  case 2i64 -> jvp_loss nt dt w z zh
  case 3i64 -> vjp_loss_online nt dt w z zh
  case 4i64 -> jvp_loss_online nt dt w z zh
  case _ -> replicate n 0f32
