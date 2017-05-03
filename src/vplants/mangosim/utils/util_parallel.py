from multiprocessing import Process, Pipe
import itertools

def spawn(f):
    def fun(pipe,x):
        pipe.send(f(x))
        pipe.close()
    return fun

def parmap(f,X):
    pipe=[Pipe() for x in X]
    proc=[Process(target=spawn(f),args=(c,x)) for x,(p,c) in itertools.izip(X,pipe)]
    [p.start() for p in proc]
    [p.join() for p in proc]
    return [p.recv() for (p,c) in pipe]

def nparmap(f,X,n = 7):
    from math import ceil
    res = []
    nbdata = len(X)
    nbsteps =  int(ceil(float(nbdata) / n))
    for j in xrange(nbsteps):
        res += parmap(f,X[j*n:min(nbdata,(j+1)*n)])
    return res

def nparmap(f,X,n = None):
    if n is None:
        import multiprocessing as mp
        n = mp.cpu_count()-1
    ids = range(n)
    pipes = [Pipe() for x in xrange(n)]
    procs = [Process(target=spawn(f),args=(c,x)) for x,(p,c) in itertools.izip(X,pipes)]
    [p.start() for p in procs]
    results = [None for i in xrange(len(X))]

    nbdata = len(X)
    timeout = 0.1

    i = n
    nbproc = n
    while nbproc > 0:
        j = 0
        while j < nbproc:
            p = procs[j]
            p.join(timeout)
            if not p.is_alive():
                results[ids[j]] = pipes[j][0].recv()
                ids.pop(j)
                pipes.pop(j)
                procs.pop(j)
                if i < nbdata:
                    pp,c = Pipe()
                    pipes.append((pp,c))
                    proc = Process(target=spawn(f),args=(c,X[i]))
                    proc.start()
                    procs.append(proc)
                    ids.append(i)
                    i += 1
                else:
                    nbproc = len(procs)
            else:
                j += 1
    return results
