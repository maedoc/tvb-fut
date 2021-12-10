-- | Epileptor implementation.
--
-- This implements dfun for the standard Epileptor.

import "util"

module epileptor = {

    let a = 1.0f32
    let b = 3.0f32
    let c = 1.0f32
    let d = 5.0f32
    let r = 0.00015f32
    let s = 4.0f32
    let Iext = 3.1f32
    let slope = 0.0f32
    let Iext2 = 0.45f32
    let tau = 10.0f32
    let aa = 6.0f32
    let bb = 2.0f32
    let Kvf = 0.0f32
    let Kf = 0.00f32
    let Ks = 0.00f32
    let tt = 1.0f32
    let modification = 0.0f32
    let x0 = -2f32

  -- not sure what's going on with the indentation here
  type node = (f32,f32,f32, f32,f32,f32)

    let init: node = (-1.5f32, -11.3f32, 3.0f32, -0.82f32, 0.04f32, -0.15f32)



    let coupling (w:[][]f32) (x:[]f32): []f32 =
      let per_node wi xi = map2 (\wij xj -> wij * (xj - xi)) wi x |> reduce (+) 0f32
      in map2 per_node w x

    let node (y:node) (c_pop1:f32) (c_pop2:f32): node =
      let (x1,y1,z,x2,y2,g) = y
      let dx1' = if x1 < 0
                 then ((-a) * x1*x1) + b * x1
                 else (slope - x2) + 0.6f32 * (z - 4f32)*(z - 4f32)
      let dx1 = tt * (y1 - z + Iext + Kvf * c_pop1 + dx1' * x1)
      let dy1 = tt * (c - d * x1*x1 - y1)
      let dz = if z < 0
               then -0.1f32 * z*z*z*z*z*z*z
               else 0f32
      let h = if modification>0
              then x0 + 3f32/(1f32 + f32.exp(-(x1 + 0.5f32)/0.1f32))
              else 4f32 * (x1 - x0) + dz
      let dz = tt * (r * (h - z + Ks * c_pop1))
      let dx2 = tt * (-y2 + x2 - x2*x2*x2 + Iext2 + bb * g - 0.3f32 * (z - 3.5) + Kf * c_pop2)
      let dy2 = if x2 < -0.25f32 then 0f32 else aa * (x2 + 0.25f32)
      let dy2 = tt * ((-y2 + dy2) / tau)
      let dg = tt * (-0.01f32 * (g - 0.1 * x1))
      in (dx1, dy1, dz, dx2, dy2, dg)

    let network (y:[]node) (w:[][]f32): []node =
      let (x1, y1, z, x2, y2, g) = unzip6 y
      let c_pop1 = coupling w x1
      let c_pop2 = coupling w x2
      in map3 node y c_pop1 c_pop2

    let heun (dt:f32) (y:[]node) (w:[][]f32): []node =
      let half_dt:f32 = dt / 2
      let dy1 = network y w
      let yi = map2 (\(x1, y1, z, x2, y2, g) (dx1, dy1, dz, dx2, dy2, dg) ->
                       (x1 + dt * dx1, y1 + dt * dy1, z + dt * dz, x2 + dt * dx2, y2 + dt * dy2, g + dt * dg))
                    y dy1
      let dy2 = network yi w
      in map3 (\(x1, y1, z, x2, y2, g) (dx1, dy1, dz, dx2, dy2, dg) (ex1, ey1, ez, ex2, ey2, eg) ->
                 (x1 + half_dt * (dx1 + ex1),
                  y1 + half_dt * (dy1 + ey1),
                  z + half_dt * (dz + ez),
                  x2 + half_dt * (dx2 + ex2),
                  y2 + half_dt * (dy2 + ey2),
        g + half_dt * (dg + eg)))
        y dy1 dy2
}
