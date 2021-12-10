-- | Spatialized Epileptor implementation.
--
-- This implements dfun for the spatial Epileptor.

import "util"

module spatepi = {
        
    type node = (f32,f32,f32, f32,f32,f32)
    type lc = (f32,f32,f32)

    let dfun
                (gamma11:f32) (gamma22:f32)
                (gamma12:f32) (gamma_glob:f32)
                (Iext:f32) (Iext2:f32) (tt:f32)
                (py0:f32) (tau0:f32) (tau2:f32)
                (y:node) (c:f32) (lc:lc) (x0:f32)
            : node =

        let loc11 = gamma11 * lc.0
        let loc22 = gamma22 * lc.1
        let loc12 = gamma12 * lc.2
        let c_pop = gamma_glob * c
        let (y0,y1,y2,y3,y4,y5) = y

        -- -- pop 1
        let y02 = y0 * y0
        let dy0 = if y0 < 0 
            then (y0 * y02 - 3.0f32 * y02)
            else (y3 - 0.6f32 * (y2 - 4.0f32) * (y2 - 4.0f32)) * y0
        
        let dy0 = tt * (y1 - dy0 - y2 + Iext + loc11 + c_pop)
        let dy1 = tt * (py0 - 5 * y02 - y1)

        -- -- energy
        let y22 = y2 * y2
        let y23 = y22 * y2
        let dy2 = if y2 < 0 then (-0.1f32 * y22 * y22 * y23) else 0f32
        let dy2 = tt * (1.0f32 / tau0 * (4.0f32 * (y0 - x0) - y2 + dy2))

        -- pop 2
        let y33 = y3 * y3 * y3
        let dy3 = tt * (-y4 + y3 - y33 + Iext2 + 2.0f32 * y5 - 0.3f32 * (y2 - 3.5f32) + loc22)
        let dy4 = if (y3 < (-0.25f32)) then 0f32 else 6.0f32 * (y3 + 0.25f32)
        let dy4 = tt * ((-y4 + dy4) / tau2)

        -- filter
        let dy5 = tt * (-0.01f32 * y5 + 0.003f32 * y0 + 0.01f32 * loc12)

        in (dy0, dy1, dy2, dy3, dy4, dy5)


    let lcfun (theta11:f32) (theta22:f32) (theta12:f32)
                    (y:node): lc =
        let (y0,y1,y2,y3,y4,y5) = y
        in (
            0.5f32 * ((npsign (y0 - theta11)) + 1f32),
            0.5f32 * ((npsign (y3 - theta11)) + 1f32),
            0.5f32 * ((npsign (y0 - theta12)) + 1f32)
        )
}