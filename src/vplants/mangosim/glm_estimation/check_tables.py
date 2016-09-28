from os.path import join
import pandas as pd
share_dir = '../../../../share/'
data_dir = join(share_dir,'glm_output_proba2/cogshall/selected_glm')

from vplants.mangosim.tools import isiterable
from vplants.mangosim.state import *


def total_proba_nb(probs, nbs):
    totnb = 0
    totprob = 0
    for p, n in zip(probs, nbs):
        totprob += p*n
        totnb += n
    totprob /= totnb
    return totprob, totnb


def simulate_binomial(probs, nbs, nbiter = 10000):
    from numpy.random import binomial
    from numpy import mean, std
    if not isiterable(probs):
        #values =  [sum(binomial(1, probs, nbs)) for i in xrange(nbiter)]
        #totnb = nbs
        probs = [probs]
        nbs = [nbs]
    #else:
    values = [sum([sum(binomial(1, prob, nb)) for prob, nb in zip(probs,nbs)]) for i in xrange(nbiter)]
    totnb = sum(nbs)
    return mean(values)/totnb, std(values)/totnb

def fms(mv):
    return '%.5f +/- %5f' % mv

def check_proba_table(table, etype = eBinomial, nbiter = 100000):
    probs = table['probability'].values
    nbs = table['number'].values
    totprob, totnb = total_proba_nb(probs, nbs)
    print 'Total proba and number : %.5f ; %i' % (totprob, totnb)
    if etype == eBinomial:
        print 'Simulation     :', fms(simulate_binomial(totprob, totnb, nbiter))
        print 'Simulation w f :', fms(simulate_binomial(probs, nbs, nbiter))


def process(name, etype = eBinomial):
    print '*****', name
    data = pd.read_csv(join(data_dir, name+'.csv'))
    print 'Factors :', data.columns.values[:-2]
    check_proba_table(data)
    print


def main():
    process('vegetative_burst_within_04')
    process('vegetative_burst_within_05')

if __name__ == '__main__':
    main()
