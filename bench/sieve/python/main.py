import sys
sys.setrecursionlimit(100000)

def count_from (n):
    return (n, lambda : count_from(n+1))

def unfold(st):
    hd,stt=st
    return (hd, stt())

def sift(n,st):
    hd,tl = unfold(st)
    if hd % n == 0:
        return sift(n,tl)
    else:
        return (hd, lambda : sift (n,tl))

def sieve(st):
    hd,tl = unfold(st)
    return (hd, lambda : sieve (sift(hd,tl)))

def get(st,i):
    hd,tl = unfold(st)
    while i != 0:
        hd,tl = unfold(tl)
        i-=1
    return hd

primes = sieve (count_from(2))

print(get(primes,int(sys.argv[1])))
