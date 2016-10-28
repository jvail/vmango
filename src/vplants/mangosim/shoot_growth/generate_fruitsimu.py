lsysfile      = 'mango_mtg_replay.lpy'

def generate(seed = 0, tree = 0):
    from openalea.lpy import Lsystem
    import os
    print 'Scene generator launched'
    l = Lsystem(lsysfile,{'RESOLUTION' : 1, 'daystep' : 30, 'TIMEBAR' : False, 'LEAFY' : True, 'WITH_INFLO' : True, 'EXPORT_TO_MTG' : False, 'SEED' : seed})

    if not os.path.exists(workingrep) : os.makedirs(workingrep)
    lstring = l.derive()


def generate_range():
    for seed in range(0,10):
        for tree in range(5):
            generate(seed, tree)

if __name__ == '__main__':
    generate_range()