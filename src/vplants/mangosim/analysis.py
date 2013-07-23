#from vplants.mangotree import mango
share_dir = '../../../share/'

Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }
def todate(st) :
    from datetime import date
    m,y = st.split('.')
    return date(2000+int(y), Month[m], 1)
    

def load_mtg(name='mango_mtg.pkl'):
    import cPickle as pickle
    if True : # (share_dir+name).isfile():
        pkl_file = open(share_dir+name, 'rb')
        g = pickle.load(pkl_file)
        pkl_file.close()
    return g

g = load_mtg()

def get_mproperty(propname, ucs):
    prop = g.property(propname)
    return dict([( i, prop[i] ) for i in ucs if i in prop])

# Les ucs de l'arbre 1
tree = 1
def get_delta_flush_histo(trees = 1, year = 4):
    if type(trees) == int: trees = [trees]
    res = {}
    for tree in trees:
        ucs1 = g.components_at_scale(tree,scale=4) #take all uc in the tree
        #
        mlabel = dict([( i, g.label(i) ) for i in ucs1])
        assert len(mlabel) == len(ucs1)    # to verify if we have the ucs 
        myear = get_mproperty('year',ucs1) # get year of the ucs
        assert len(myear) > 0
        # ucs_y3 = [i for i in ucs1 if myear.get(i) == 3]
        ucs_y4 = [i for i in ucs1 if myear.get(i) == year and mlabel.get(i) == 'U']
        assert len(ucs_y4) > 0
        #
        dby4 = get_mproperty('date_burst', ucs_y4)
        assert len(dby4) > 0
        dby4 = dict([( i, todate(v) ) for i,v in dby4.items()])    
        #
        ucs_y4_ordern = [i for i in ucs_y4 if g.parent(i) in ucs_y4]
        delta_flush_y4 = [int(0.5+(dby4[i]-dby4[g.parent(i)]).days/30.) for i in ucs_y4_ordern]    
        lres = [(i,delta_flush_y4.count(i)) for i in set(delta_flush_y4)]
        for i,v in lres:
            res[i] = v+res.get(i,0)
    return res, sum(i*v for i,v in res.items())/float(sum(v for i,v in res.items()))

def get_delta_flush_histo_between_year(trees = 1, inflo_condition = None):
    if type(trees) == int: trees = [trees]
    res = {}
    year = 5
    for tree in trees:
        ucs1 = g.components_at_scale(tree,scale=4)
        #
        mlabel = dict([( i, g.label(i) ) for i in ucs1])
        assert len(mlabel) == len(ucs1)
        myear = get_mproperty('year',ucs1)
        assert len(myear) > 0
        # ucs_y3 = [i for i in ucs1 if myear.get(i) == 3]
        ucs_y5 = [i for i in ucs1 if myear.get(i) == 5 and mlabel.get(i) == 'U']
        #
        has_inflo = lambda i : 'F' in [g.label(j) for j in g.children(i)]
        has_not_inflo = lambda i : not has_inflo(i)
        no_cond = lambda i : True
        #
        if inflo_condition == 0: inflo_cond = has_not_inflo
        elif inflo_condition == 1: inflo_cond = has_inflo
        else : inflo_cond = no_cond
        #
        ucs_y4 = [i for i in ucs1 if myear.get(i) == 4 and mlabel.get(i) == 'U' and inflo_cond(i)]
        #
        dby = get_mproperty('date_burst', ucs1)
        dby = dict([( i, todate(v) ) for i,v in dby.items()])    
        #
        ucs_first_y5 = [i for i in ucs_y5 if g.parent(i) in ucs_y4]
        delta_first_flush_y5 = [int(0.5+(dby[i]-dby[g.parent(i)]).days/30.) for i in ucs_first_y5]    
        lres = [(i,delta_first_flush_y5.count(i)) for i in set(delta_first_flush_y5)]
        for i,v in lres:
            res[i] = v+res.get(i,0)
    return res, sum(i*v for i,v in res.items())/float(sum(v for i,v in res.items()))

    
def get_delta_flush_histo_per_month(trees = 1, year = 4):
    if type(trees) == int: trees = [trees]
    res = {}
    for tree in trees:
        ucs1 = g.components_at_scale(tree,scale=4)
        #
        mlabel = dict([( i, g.label(i) ) for i in ucs1])
        assert len(mlabel) == len(ucs1)
        myear = get_mproperty('year',ucs1)
        assert len(myear) > 0
        # 
        ucs_y4 = [i for i in ucs1 if myear.get(i) == year and mlabel.get(i) == 'U']
        assert len(ucs_y4) > 0
        #
        dby4 = get_mproperty('date_burst', ucs_y4)
        assert len(dby4) > 0
        dby4 = dict([( i, todate(v) ) for i,v in dby4.items()])    
        #
        ucs_y4_ordern = [i for i in ucs_y4 if g.parent(i) in ucs_y4 and dby4[g.parent(i)] > date(1999+year,7,1)]
        for i in ucs_y4_ordern:
            delta = int(0.5+(dby4[i]-dby4[g.parent(i)]).days/30.)
            parent_month = dby4[g.parent(i)].month
            if parent_month == 5: print i
            if parent_month not in res:
                res[parent_month] = {}
            res[parent_month][delta] = res[parent_month].get(delta,0)+1
    return res #, sum(i*v for i,v in res.items())/float(sum(v for i,v in res.items()))


def get_flush_histo_per_month(trees = 1, year = 4):
    if type(trees) == int: trees = [trees]
    res = {}
    for tree in trees:
        ucs1 = g.components_at_scale(tree,scale=4)
        #
        mlabel = dict([( i, g.label(i) ) for i in ucs1])
        assert len(mlabel) == len(ucs1)
        myear = get_mproperty('year',ucs1)
        assert len(myear) > 0
        # 
        ucs_y4 = [i for i in ucs1 if myear.get(i) == year and mlabel.get(i) == 'U']
        assert len(ucs_y4) > 0
        #
        dby4 = get_mproperty('date_burst', ucs_y4)
        assert len(dby4) > 0
        #dby4 = dict([( i, todate(v) ) for i,v in dby4.items()])    
        #
        for i in ucs_y4:
            month = dby4[i]
            res[month] = res.get(month,0)+1
    res = res.items()
    res.sort(lambda x,y: cmp(todate(x[0]),todate(y[0]))) 
    return res #, sum(i*v for i,v in res.items())/float(sum(v for i,v in res.items()))


    
cogshall_trees = [i for i, v in g.property('var').items() if v == 'cogshall']
fruit_loaded_prop = g.property('fr_load') 
cogshall_notloaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'NC']
cogshall_loaded_trees = [i for i in cogshall_trees if fruit_loaded_prop[i] == 'C']
