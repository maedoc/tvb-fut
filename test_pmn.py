import numpy as np
from scipy.special import sph_harm
import shtns
import os
os.system('futhark c --library test_pmn.fut')
os.system('build_futhark_ffi test_pmn')
from futhark_ffi import Futhark
import _test_pmn
pmn = Futhark(_test_pmn)


# cf https://arxiv.org/abs/1202.6522

def gen_ml(N):
    return np.array([[m, l] for m in range(N+1) for l in range(N+1) if m<=l])

def all_amm(N):
    amm = []
    for m in range(N+1):
        els = []
        for k in range(1,m+1):
            els.append((2*k+1)/(2*k))
        el = np.prod(els)/(4*np.pi)
        amm.append(np.sqrt(el))
    amm = np.array(amm)
    return amm

def amn(m, n):
    nom = 4*n*n - 1
    den = n*n - m*m
    return np.sqrt(nom/den)

def bmn(m, n):
    l = (2*n + 1)/(2*n - 3)
    r = ((n - 1)*(n - 1) - m*m)/(n*n - m*m)
    return - np.sqrt(l*r)

def lat_grid(nlat):
    x = np.r_[:nlat] / nlat * np.pi
    cx = np.cos(x)
    return cx

check_Pmn = lambda p, m, n, cx: \
    np.testing.assert_allclose(p, 
        sph_harm(m, n, 0, np.arccos(cx)).real, rtol=1e-5, atol=1e-6)

# TODO separate algo from checks?
def test_Pmn_recursion(N=10, nlat=32, check=True):
    "check all generated P^m_n(x) coefficients"
    amm = all_amm(N)
    cx = lat_grid(nlat)
    ml = set([(m_, l_) for (m_, l_) in gen_ml(N)])

    for m in range(N+1):

        n = m
        ml.remove((m,n))
        p0 = amm[m]*(1 - cx*cx)**(m/2)*(-1)**m # eq 13
        if check: check_Pmn(p0, m, m, cx)
        if n == N:
            break

        n += 1
        ml.remove((m,n))
        p1 = amn(m, n) * cx * p0 # eq 14
        if check: check_Pmn(p1, m, n, cx)
        if n == N:
            continue

        n += 1
        ml.remove((m,n))
        p2 = amn(m, n)*cx*p1 + bmn(m, n)*p0 # eq 15
        if check: check_Pmn(p2, m, n, cx)
        if n == N:
            continue

        while n < N:
            p0, p1 = p1, p2
            n += 1
            ml.remove((m,n))
            p2 = amn(m, n)*cx*p1 + bmn(m, n)*p0 # eq 15
            if check: check_Pmn(p2, m, n, cx)

    assert(len(ml) == 0)

def test_ml():
    for N in range(8,32):
        sht = shtns.sht(N)
        # sht.set_grid(nlat, nlat*2)
        ml2 = gen_ml(N)
        ml3 = np.c_[sht.m, sht.l]
        np.testing.assert_allclose(ml2, ml3)
        mf, lf = pmn.from_futhark(pmn.gen_ml_arr(N))
        np.testing.assert_allclose(ml2, np.c_[mf, lf])       

def test_amm():
    N = 20
    np_amm = all_amm(N)
    ft_amm = pmn.from_futhark(pmn.all_amm(N))
    np.testing.assert_allclose(ft_amm, np_amm, rtol=1e-6, atol=1e-7)

def test_amn():
    for m, l in gen_ml(10):
        np.testing.assert_allclose(
            pmn.amn(m,l),
            amn(m,l))

def test_bmn():
    for m, l in gen_ml(10):
        np.testing.assert_allclose(
            pmn.bmn(m,l),
            bmn(m,l))

def test_lat_grid():
    nlat = 64
    np.testing.assert_allclose(
        pmn.from_futhark(pmn.lat_grid(nlat)) + 1,
        lat_grid(nlat) + 1,
        rtol=1e-6, atol=1e-6
    )

def test_Pm_rec_ft(N=10, nlat=32):
    amm = all_amm(N)
    cx = lat_grid(nlat)
    for m, l in gen_ml(N):
        p = pmn.from_futhark(pmn.Pmn(m, l, amm[m], cx))
        check_Pmn(p, m, l, cx)

if __name__ == '__main__':
    # test_Pmn_recursion(N=90, nlat=32, check=False)
    test_ml()
    test_amm()
    test_amn()
    test_bmn()
    test_lat_grid()
    test_Pmn_recursion()
    test_Pm_rec_ft()
    print('all done')

