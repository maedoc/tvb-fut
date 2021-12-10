import numpy as np
from sympy import exp, pi, I, Symbol, cse, re, im, Indexed
from sympy.printing.c import ccode
import time
import sys

N, = [int(_) for _ in sys.argv[1:]]
tic = time.time()

def ft(x):
    N = len(x)
    if N == 2:
        a, b = x
        return a + b, a - b
    else:
        E, O = ft(x[::2]), ft(x[1::2])
        Ws = np.array([exp(-2*pi*I*k/N) for k in range(N//2)])
        return np.r_[E + Ws*O, E - Ws*O]

# build input sequence symbolically as array elements y[k]
y = np.array([Symbol(f'y[{k}]') for k in range(N)])

# apply FFT symbolically
Y = ft(y).tolist()
assert len(Y) == N

# for ccode to work, we use just floats, not complex numbers
Yr, Yi = [re(_) for _ in Y], [im(_) for _ in Y]

# cse improves speed
aux, Yri = cse(Yr + Yi)
Yr, Yi = Yri[:len(Y)], Yri[len(Y):]

print('''let M_PI = f32.pi
let M_SQRT2 = f32.sqrt(2)
let M_SQRT1_2 = 1/f32.sqrt(2)''')

print('let fft [n] (y:[n](f32,f32)): [n](f32,f32) =')
print('    let re (a,_) = a')
print('    let im (_,b) = b')

def myccode(ex):
    src, = [l for l in ccode(ex).split('\n') if not l.startswith('//')]
    for f in 'sin cos exp sqrt pi'.split(' '):
        src = src.replace(f, 'f32.'+f)
    return src

for l, r in aux:
    print('    let', l, '=', myccode(r))

print('    in [')
for i, (yr, yi) in enumerate(zip(Yr, Yi)):
    print('        (', myccode(yr), ',', myccode(yi), ')',
          ',' if i < (len(Yr)-1) else '')
print('    ] :> [n](f32,f32)')