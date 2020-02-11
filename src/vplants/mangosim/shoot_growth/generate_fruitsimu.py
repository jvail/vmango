from __future__ import print_function
from builtins import str
from builtins import range
from os.path import join, basename, dirname
import glob

lsysfile      = 'mango_mtg_replay.lpy'
inputdir      = 'fruitmodeloutput'
pattern = join(inputdir, 'fruitmodel-{}-*-seed-{}-cycle-{}-fdist-{}', 'fruitstructure.csv')


def generate(seed = 0, tree = 0, fdist = 4, regenerate = False):
    from openalea.lpy import Lsystem
    import os
    rep = glob.glob(pattern.format(tree, seed, '*' , fdist))
    if len(rep) == 2 and not regenerate: 
        print('Do not regenerate', seed, tree, fdist)
        return True
    l = Lsystem(lsysfile,{'RESOLUTION' : 1, 'daystep' : 30, 'TIMEBAR' : False, 'LEAFY' : True, 'WITH_INFLO' : True, 'EXPORT_TO_MTG' : False, 'TREE' : tree, 'SEED' : seed, 'FRUITBRANCHSIZE' : fdist})
    if l.tree_load:
        print('Generate', seed, tree, fdist)
        try:
            lstring = l.derive()
            return True
        except:
            return False
    else :
        print('Not loaded tree', tree)
        return False


def generate_range(nb = 10, fdist = 4, regenerate = False):
    result = {}
    for tree in [1,3,4]:
        res3tree = []
        res4tree = []
        for seed in range(0,nb):
            if generate(seed, tree, fdist, regenerate):
                tname, r3, r4 = retrieve_simu_result(seed, tree, fdist)
                res3tree.append(r3)
                res4tree.append(r4)
        if len(res3tree) > 0:
            result[str(tree)+'-'+tname] = (res3tree, res4tree)
    return result

def retrieve_simu_result(seed, tree, fdist):
    import pandas

    def get_values(tables):
        totalresult = tables[tables['Filename']=='TOTAL']
        NbInflos = totalresult.NbInflos.values[0]
        NbFruits = totalresult.NbFruits.values[0]
        NbLeaves = totalresult.NbLeaf.values[0]
        TotalMassFruit = totalresult.TotalMassFruit.values[0]
        return NbInflos, NbFruits, NbLeaves, TotalMassFruit

    def get_tree_name(path):
        p = basename(dirname(path))
        p = p[13:]
        return p[:p.index('-')]

    rep3 = glob.glob(pattern.format(tree, seed, 3, fdist))
    assert len(rep3) == 1
    tablevalues3 = pandas.read_csv(rep3[0],sep='\t')
    res3 = get_values(tablevalues3)

    rep4 = glob.glob(pattern.format(tree, seed, 4, fdist))
    assert len(rep4) == 1
    tablevalues4 = pandas.read_csv(rep4[0],sep='\t')
    res4 = get_values(tablevalues4)

    return get_tree_name(rep3[0]), res3, res4

def process_result(result,i):
    def meansd(val, i):
        import numpy as np
        values = [v[i] for v in val]
        m,std =  np.mean(values), np.std(values)
        return str(m)+'+/-'+str(std)

    fname = join(inputdir, 'result-fdist-'+str(i)+'.csv')
    stream = file(fname,'w')
    stream.write('Tree\tCycle\tNbInflos\tNbFruits\tNbLeaves\tTotalMassFruit\n')
    for tree, res in list(result.items()):
        res3, res4 = res
        stream.write(tree+'\t3\t'+meansd(res3,0)+'\t'+meansd(res3,1)+'\t'+meansd(res3,2)+'\t'+meansd(res3,3)+'\n')
        stream.write(tree+'\t4\t'+meansd(res4,0)+'\t'+meansd(res4,1)+'\t'+meansd(res4,2)+'\t'+meansd(res3,3)+'\n')
    stream.close()


if __name__ == '__main__':
    for i in range(1,6):
        process_result(generate_range(100,i),i)
