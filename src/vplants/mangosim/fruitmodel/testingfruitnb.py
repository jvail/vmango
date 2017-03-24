#c = [(vid,nb) for vid, nb in mtg.property('nb_thin_fr').items() if get_tree_name(mtg,get_tree_of_gu(mtg,vid)) == 'B10']

def get_parent(lstring, fid):
    pid = lstring.parent(fid)
    while lstring[pid].name != 'Inflorescence':
        pid = lstring.parent(pid)
    return pid

from vplants.mangosim.doralice_mtg.mtg_import import get_mtg
import vplants.mangosim.doralice_mtg.mtg_manipulation as mm 
mtg = get_mtg()

def identify_fruits(lstring):

    tree = lstring[4][0].mtgid
    fruitids = [i for i,l in enumerate(lstring) if l.name == 'Fruit']
    #ipf = ignorePredefined()
    #parentids = [lstring.parent(fid,ipf) for fid in fruitids]

    parentids = [get_parent(lstring,fid) for fid in fruitids]

    parentmtgids = [lstring[pid][0].mtgid for pid in parentids]

    mtgparentid = [vid for vid, nb in mtg.property('nb_fr').items() if mm.get_tree_of_gu(mtg,vid) == tree and nb > 0 and mm.get_unit_cycle(mtg,vid) == 3]
    #print 'Lstring:',len(parentmtgids), parentmtgids
    #print 'MTG:',len(mtgparentid), mtgparentid
    #print set(mtgparentid).symmetric_difference(set(parentmtgids))

    from collections import Counter
    c = Counter(parentmtgids).items()
    c.sort()
    print 'Lstring:',len(c),':',c
    mtgparentid = [(vid,nb) for vid, nb in mtg.property('nb_fr').items() if mm.get_tree_of_gu(mtg,vid) == tree and nb > 0 and mm.get_unit_cycle(mtg,vid) == 3]
    mtgparentid.sort()
    print 'MTG:',len(mtgparentid),':',mtgparentid
    l = list(set(mtgparentid).symmetric_difference(set(c)))
    l.sort()
    print 'Diff:', l
    print 'Diff1:',list(set(mtgparentid).difference(set(c)))
    l2 = list(set(mtgparentid).intersection(set(c)))
    l2.sort()
    print 'Common:', l2


def find_unit(lstring, mtgid):
  for i,l in enumerate(lstring):
    if l.name in ['GU','Inflorescence']:
      if l[0].mtgid == mtgid:
        return i