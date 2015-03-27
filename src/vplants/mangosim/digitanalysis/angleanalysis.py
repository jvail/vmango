from os.path import join, exists
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = join("share",'digitized_mango_mtg'))

def get_mtg_filename(name):
	return join(share_dir,name)

def get_ucs(m):
    return [e for e in m.vertices(scale=2) if m.class_name(e) == 'U']

def ucfirstpos(uc, m):
    s = [e for e in m.components(uc) if m.class_name(e) == 'S' and m.property('XX').has_key(e)]
    if len(s) == 0: return None
    return (m.property('XX')[s[0]],m.property('YY')[s[0]],m.property('ZZ')[s[0]])

def ucinsertionpos(uc, m):
    firstseg = m.component_roots(uc)[0]
    parentseg = m.parent(firstseg)
    while not parentseg is None and not m.property('XX').has_key(parentseg):
        parentseg = m.parent(parentseg)
    if parentseg is None: 
        parentseg = m.complex_at_scale(uc,1)
    return (m.property('XX')[parentseg],m.property('YY')[parentseg],m.property('ZZ')[parentseg])

from openalea.plantgl.all import Vector3, angle
from math import degrees

def ucdir(uc, m):
    ucfpos = ucfirstpos(uc, m)
    if ucfpos is None: return None
    ucins = ucinsertionpos(uc,m)
    return Vector3(ucfpos) - ucins

def ucinsertionangle(uc,m):
    ucldir = ucdir(uc,m) 
    ucpdir = ucdir(m.parent(uc),m)
    if ucldir is None or ucpdir is None: return None 
    return degrees(angle(ucldir,ucpdir))



import numpy as np
import matplotlib.pyplot as plt

def plot_histo(angles):
    allanglevalues = filter(lambda x : not x is None, angles.values())
    hist, bins = np.histogram(allanglevalues, bins=50)
    width = 0.7 * (bins[1] - bins[0])
    center = (bins[:-1] + bins[1:]) / 2
    plt.bar(center, hist, align='center', width=width)
    plt.show()

if __name__ == '__main__':
    from openalea.mtg import *

    m = MTG(get_mtg_filename("a19.mtg"))
    #m = MTG("jf31.mtg")
    ucs = get_ucs(m)
    #print ucfirstpos(ucs[0],m)
    allfirstpos = dict([(uc,ucfirstpos(uc,m)) for uc in ucs])
    allinsertpos = dict([(uc,ucinsertionpos(uc,m)) for uc in ucs])
    allangles = dict([(uc,ucinsertionangle(uc,m)) for uc in ucs])
    allangles = dict([(uc,a) for uc,a in allangles.items() if not a is None])
    # plot_histo(allangles)
    ucorder =  dict([(uc,m.order(uc)) for uc in ucs])
    maxorder = max(ucorder.values())
    data = [[allangles[uc] for uc in ucs if ucorder[uc] == o if allangles.has_key(uc)] for o in xrange(0,maxorder+1)]
    plt.hist(data, 20, histtype='bar', stacked=True,label = ['Order '+str(o) for o in xrange(0,maxorder+1)])
    plt.legend()
    plt.show()



