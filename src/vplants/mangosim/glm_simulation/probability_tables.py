from vplants.mangosim.tools import share_dir
from vplants.mangosim.state import *
from vplants.mangosim.util_date import *


def get_repository(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    from os.path import join
    return join(share_dir, 'glm_output_proba', variety, 'complete_glm' if estimationtype == eCompleteGlm else 'selected_glm', treename)


vegetative_proba = ['vegetative_burst','burst_date_children','has_lateral_gu_children','nb_lateral_gu_children']
vegetative_proba_family = [eBinomial, eVglm, eBinomial, ePoisson]
flowering_proba  = ['flowering','nb_inflorescences','flowering_week']
flowering_proba_family  = [eBinomial, ePoisson, eVglm ]

within_extension = {3 : None, 4 : 'within_04', 5 : 'within_05'}
between_etension = {3 : 'between_03to0405', 4 : 'between_04to05', 5 : None }

allfactors = ['Tree_Fruit_Load', 'Burst_Date', 'Position_A', 'Position_Ancestor_A', 'Nature_Ancestor_V', 'Nature_V']


class ProbaTable:
    def __init__(self, name, family, fname = None):
        self.name = name
        self.family = family
        if fname: self.readvalues(fname)

    def readvalues(self, fname):
        import pandas
        tablevalues = pandas.read_csv(fname)
        self.setvalues(tablevalues)

    def setvalues(self, tablevalues, family = None):
        """
            tablevalues should be a pandas dataframe
        """
        if family: self.family = family

        self.factors = [ name for name in list(tablevalues.columns) if name in allfactors]
        extrafactors = [ name for name in list(tablevalues.columns) if name not in allfactors]
        if self.family == eVglm:
            self.answers = list(extrafactors)

        subset_table_factor = tablevalues[self.factors]
        subset_table_probas = tablevalues[extrafactors]
        self.values = {}
        for ind in xrange(len(table_prob)):
            factorv = tuple(subset_table_var.iloc[ind])
            self.values[factorv] = list(subset_table_prob.iloc[ind])

    def get_proba_value(self, args):
        if len(args) != len(self.factors): raise ValueError('Invalid number of factors.',args, self.factors)
        factoractualvalue = tuple([args[f] for f in self.factors])
        return self.values[factoractualvalue]

    def realization(self, **args):
        from numpy import cumsum
        from numpy.random import binomial, poisson, uniform
        probavalue = self.get_proba_value(args)
        if self.family == eBinomial:
            return bool( binomial(1,probavalue) )
        elif self.family == ePoisson:
            return int( poisson(proba,1) )
        elif self.family == eVglm:
            cumsum_probs = list( cumsum(probavalue) )
            unif_realization = float( uniform(0,1,1) )
            cumsum_probs[-1] = 1
            i = 0
            while unif_realization >= cumsum_probs[i] : i += 1
            return self.answers[i]



def read_proba_tables(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    from os.path import exists, join
    probafilepath = get_repository(variety, treename, estimationtype)
    assert exists(probafilepath)
    probacycle = {}
    for cycle in range(3,6):
        proba_within, proba_between = {}, {}
        if within_extension[cycle]:
            ext = within_extension[cycle]
            for zip(prop,family) in zip(vegetative_proba+flowering_proba,vegetative_proba_family+flowering_proba_family):
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if exists(propfile):
                    proba_within[prop] = ProbaTable(prop,family,propfile)
                else:
                    import warnings
                    warnings.warn("Table '%s' for variety '%s', tree '%s' does not exist." % (prop, variety, treename))
        if between_etension[cycle]:
            ext = between_etension[cycle]
            for prop, family in zip(vegetative_proba,vegetative_proba_family):
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if exists(propfile):
                    proba_within[prop] = ProbaTable(prop,family,propfile)
                else:
                    import warnings
                    warnings.warn("Table '%s' for variety '%s', tree '%s' does not exist." % (prop, variety, treename))
        probacycle[cycle] = (proba_within, proba_between)
    return probacycle

global_proba_tables = {}
current_proba_table = None
    
def get_proba_tables(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    global global_proba_tables
    if not (variety, treename, estimationtype) in global_proba_tables:
        global_proba_tables[(variety, treename, estimationtype)] = read_proba_tables(variety, treename, estimationtype)
    return global_proba_tables[(variety, treename, estimationtype)]

def use_proba_table(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    global current_proba_table
    current_proba_table = get_proba_tables(variety, treename, estimationtype)

def use_proba_table_from(treename,  estimationbase, estimationtype = eCompleteGlm)
    import vplants.mangosim.doralice_mtg.mtg_manipulation  as mm
    probnames = treename
    if estimationbase == eFamilyBased:
        Tree_Fruit_Load = mm.load_state(treeid)
        probnames = 'loaded' if Tree_Fruit_Load == eLoaded else 'notloaded'
    elif estimationbase == eVarietyBased:
        probnames = 'alltrees'
    use_proba_table(mm.get_variety(mm.get_tree_from_name(treename)), probnames, estimationtype)

class UnitDev:
    def __init__(self, Burst_Date, 
                       Position_A, 
                       Nature_V = None, 
                       Position_Ancestor_A = None,
                       Nature_Ancestor_V = None, 
                       Tree_Fruit_Load  = eLoaded):
        self.burst_date = Burst_Date
        self.cycle = get_cycle(Burst_Date)

        self.params = dict(Burst_Date = Burst_Date.month,
                           Position_A = Position_A, 
                           Position_Ancestor_A = Position_Ancestor_A, 
                           Nature_Ancestor_V   = Nature_Ancestor_V,
                           Tree_Fruit_Load     = Tree_Fruit_Load)
        self.paramsdelayed = dict(Burst_Date = Burst_Date.month,
                                  Position_A = Position_A,
                                  Nature_V   = Nature_V,
                                  Tree_Fruit_Load = Tree_Fruit_Load)

        self.proba_tables = current_proba_table[self.cycle]

        def get_realization(self, name, cycle = eWithinCycle):
            p = self.params if eWithinCycle else self.paramsdelayed
            return self.proba_tables[0 if eWithinCycle else 1][name].realization(**p)

        def vegetative_burst(self, cycle = eWithinCycle):
            return self.get_realization('vegetative_burst',cycle)

        def burst_date_children(self, cycle = eWithinCycle):
            burst_index = int(self.get_realization('burst_date_children',cycle)
            cycle_delay, burst_month = divmod(burst_index,100)
            burst_year = self.burst_date.year + cycle_delay
            if burst_month < 6:
                burst_year += 1

            assert burst_year > self.burst_date.year or burst_month > self.burst_date.month
            return (burst_year, burst_month)

        def has_lateral_gu_children(self, cycle = eWithinCycle):
            return self.get_realization('has_lateral_gu_children',cycle)

        def nb_lateral_gu_children(self, cycle = eWithinCycle):
            return self.get_realization('nb_lateral_gu_children',cycle)+1

        def flowering(self):
            return self.get_realization('flowering')

        def nb_inflorescences(self):
            return self.get_realization('nb_inflorescences')+1

        def flowering_date(self):
            from random import randint
            from datetime import timedelta
            try:
                fweek = self.get_realization('flowering_week')
            except KeyError,e:
                fweek = 0      
            period_beg, period_end = bloom_weeks[self.cycle][fweek]
            return period_beg + timedelta(days=randint(0,(period_end-period_beg).days))

        def process(self):
            nb_gu_children, nb_inflorescences = 0,0
            date_children_burst, date_inflo_bloom = None, None
            if self.cycle > 3 and self.vegetative_burst():
                nb_gu_children = 1 
                if self.has_lateral_gu_children():
                    nb_gu_children += self.nb_lateral_gu_children()
                date_children_burst = self.burst_date_children()
            else:
                if self.cycle > 3 and self.flowering():
                    nb_inflorescences = self.nb_inflorescences()
                    date_inflo_bloom  = self.flowering_date()
                    self.paramsdelayed['Nature_V'] = eInflorescence
                else:
                    self.paramsdelayed['Nature_V'] = eVegetative

                if self.cycle < 5 and self.vegetative_burst(eLaterCycle):
                    nb_gu_children = 1 
                    if self.has_lateral_gu_children(eLaterCycle):
                        nb_gu_children += self.nb_lateral_gu_children(eLaterCycle)
                    date_children_burst = self.burst_date_children(eLaterCycle)
            return nb_gu_children, nb_inflorescences, date_children_burst, date_inflo_bloom
                











