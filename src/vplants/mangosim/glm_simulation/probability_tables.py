from vplants.mangosim.tools import share_dir
from vplants.mangosim.state import *
from vplants.mangosim.util_date import *


def get_repository(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    from os.path import join
    return join(share_dir, 'glm_output_proba', variety, 'complete_glm' if estimationtype == eCompleteGlm else 'selected_glm', treename)


vegetative_proba = ['vegetative_burst','burst_date_children','has_apical_gu_child','has_lateral_gu_children','nb_lateral_gu_children']
vegetative_proba_family = [eBinomial, eVglm, eBinomial, eBinomial, ePoisson]
flowering_proba  = ['flowering','nb_inflorescences','flowering_week']
flowering_proba_family  = [eBinomial, ePoisson, eVglm ]

within_extension = {3 : None, 4 : 'within_04', 5 : 'within_05'}
between_extension = {3 : 'between_03to0405', 4 : 'between_04to05', 5 : None }

allfactors = ['Tree_Fruit_Load', 'Burst_Date', 'Position_A', 'Position_Ancestor_A', 'Nature_Ancestor_V', 'Nature_V']
factorsvalues = [range(0,2), range(1,13), [eApical,eLateral], [eApical,eLateral], [eVegetative, eInflorescence], [eVegetative, eInflorescence]]
factorsvalues = dict(zip(allfactors,factorsvalues))

class ProbaTable:
    def __init__(self, name, family, fname = None):
        self.name = name
        self.family = family
        if fname: self.readvalues(fname)

    def readvalues(self, fname):
        import pandas
        self.fname = fname
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
        for ind in xrange(len(tablevalues)):
            factorv = tuple(subset_table_factor.iloc[ind])
            self.values[factorv] = list(subset_table_probas.iloc[ind])

    def get_proba_value(self, args):
        if len(args) < len(self.factors): 
            raise ValueError('Invalid number of factors.',args, self.factors)
        for arg, value in args.items():
            if not value in factorsvalues[arg]: 
                raise ValueError('Invalid value for factor of test',value,arg,self.name)
        try:
            factoractualvalue = tuple([args[f] for f in self.factors])
        except KeyError, e:
            print self.factors, args, self.type, self.name
            raise e
        return self.values[factoractualvalue]

    def realization(self, **args):
        from numpy import cumsum
        from numpy.random import binomial, poisson, uniform
        probavalue = self.get_proba_value(args)
        if self.family == eBinomial:
            return bool( binomial(1,probavalue) )
        elif self.family == ePoisson:
            return int( poisson(probavalue,1) )
        elif self.family == eVglm:
            cumsum_probs = list( cumsum(probavalue) )
            unif_realization = float( uniform(0,1,1) )
            cumsum_probs[-1] = 1
            i = 0
            while unif_realization >= cumsum_probs[i] : i += 1
            return self.answers[i]
    def check(self):
        if self.type == eWithinCycle:
            for f in self.factors:
                if not f in ['Tree_Fruit_Load', 'Burst_Date', 'Position_A', 'Position_Ancestor_A', 'Nature_Ancestor_V']:
                    raise ValueError('Invalid factor for Within cycle proba',f)
        elif self.type == eLaterCycle:
            for f in self.factors:
                if not f in ['Tree_Fruit_Load', 'Burst_Date', 'Position_A', 'Nature_V']:
                    raise ValueError('Invalid factor for Within cycle proba',f)


def read_proba_tables(variety = 'cogshall', treename = 'all_trees', estimationtype = eCompleteGlm):
    from os.path import exists, join
    probafilepath = get_repository(variety, treename, estimationtype)
    if not exists(probafilepath): raise ValueError("Proba path repository does not exist", probafilepath)
    probacycle = {}
    for cycle in range(3,6):
        proba_within, proba_between = {}, {}
        if within_extension[cycle]:
            ext = within_extension[cycle]
            for prop,family in zip(vegetative_proba+flowering_proba,vegetative_proba_family+flowering_proba_family):
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if exists(propfile):
                    p = ProbaTable(prop,family,propfile)
                    p.type = eWithinCycle
                    p.cycle = cycle
                    p.estimation = (variety, treename, estimationtype)
                    proba_within[prop] = p
                else:
                    import warnings
                    warnings.warn("Table '%s' for variety '%s', tree '%s' does not exist." % (prop, variety, treename))
        if between_extension[cycle]:
            ext = between_extension[cycle]
            for prop, family in zip(vegetative_proba,vegetative_proba_family):
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if exists(propfile):
                    p = ProbaTable(prop,family,propfile)
                    p.type = eLaterCycle
                    p.cycle = cycle
                    p.estimation = (variety, treename, estimationtype)
                    proba_between[prop] = p
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

def use_proba_table_from(treename,  estimationbase, estimationtype = eCompleteGlm):
    import vplants.mangosim.doralice_mtg.mtg_manipulation  as mm
    probnames = treename
    if estimationbase == eManagementTypeBased:
        Tree_Fruit_Load = mm.load_state(treeid)
        probnames = 'loaded' if Tree_Fruit_Load == eLoaded else 'notloaded'
    elif estimationbase == eVarietyBased:
        probnames = 'all_trees'
    use_proba_table(mm.get_variety(mm.get_tree_from_name(treename)), probnames, estimationtype)

def iterprobatables():
    for k, ps in global_proba_tables.items():
        for cycle, pbs in ps.items():
            wpbs, lpbs =  pbs
            for pbname, pb in wpbs.items():
                yield pb
            for pbname, pb in lpbs.items():
                yield pb

def set_seed(value):
    from numpy.random import seed
    seed(value)
    from random import seed
    seed(value)


current_unitdev = None

class UnitDev:
    def __init__(self, Burst_Date, 
                       Position_A, 
                       Nature_V = None, 
                       Position_Ancestor_A = None,
                       Nature_Ancestor_V = None, 
                       Tree_Fruit_Load  = eLoaded):
        self.burst_date = Burst_Date
        self.cycle = get_cycle(Burst_Date)
        self.trace = False

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
        global current_unitdev
        current_unitdev = self

    def get_realization(self, name, cycle = eWithinCycle):
        if self.trace: print 'Test',self.cycle,'Within' if cycle == eWithinCycle else 'Delayed',name
        p = (self.params if cycle == eWithinCycle else self.paramsdelayed)
        return self.proba_tables[0 if cycle == eWithinCycle else 1][name].realization(**p)

    def vegetative_burst(self, cycle = eWithinCycle):
        try:
            return self.get_realization('vegetative_burst',cycle)
        except KeyError, ie: # some month may not be fulfilled.
            return False

    def burst_date_children(self, cycle = eWithinCycle):
        burst_index = int(self.get_realization('burst_date_children',cycle))
        cycle_delay, burst_month = divmod(burst_index,100)
        pyear = self.burst_date.year
        if self.burst_date.month < 6: pyear -= 1
        burst_year = pyear + cycle_delay
        if burst_month < 6:  burst_year += 1

        if  not (burst_year > self.burst_date.year or burst_month >= self.burst_date.month):
            raise ValueError('Children burst date is before parent burst',(burst_year, burst_month), self.burst_date )
        if (burst_year == self.burst_date.year and burst_month == self.burst_date.month):
            print 'Warning: Children GUs are borned in the same month than their parent GU'
        return (burst_year, burst_month)

    def has_apical_gu_child(self, cycle = eWithinCycle):
        return self.get_realization('has_apical_gu_child',cycle)

    def has_lateral_gu_children(self, cycle = eWithinCycle):
        return self.get_realization('has_lateral_gu_children',cycle)

    def nb_lateral_gu_children(self, cycle = eWithinCycle):
        return self.get_realization('nb_lateral_gu_children',cycle)+1

    def flowering(self):
        try:
            return self.get_realization('flowering')
        except KeyError, ie:
            return False

    def nb_inflorescences(self):
        return self.get_realization('nb_inflorescences')+1

    def flowering_date(self):
        from random import randint
        from datetime import timedelta
        try:
            fweek = int(self.get_realization('flowering_week'))
        except KeyError,e:
            fweek = 0      
        period_beg, period_end = bloom_weeks[self.cycle][fweek]
        return period_beg + timedelta(days=randint(0,(period_end-period_beg).days))

    def process(self):
        apical_child, nb_lateral_gu_children, nb_inflorescences = False, 0, 0
        date_children_burst, date_inflo_bloom = None, None
        if self.cycle > 3 and self.vegetative_burst():
            apical_child = self.has_apical_gu_child()
            nb_lateral_gu_children = 0 
            if self.has_lateral_gu_children():
                nb_lateral_gu_children += self.nb_lateral_gu_children()
            date_children_burst = self.burst_date_children()
        else:
            if self.cycle > 3 and self.flowering():
                nb_inflorescences = self.nb_inflorescences()
                date_inflo_bloom  = self.flowering_date()
                self.paramsdelayed['Nature_V'] = eInflorescence
            else:
                self.paramsdelayed['Nature_V'] = eVegetative

            if self.cycle < 5 and self.vegetative_burst(eLaterCycle):
                apical_child = self.has_apical_gu_child(eLaterCycle)
                nb_lateral_gu_children = 0
                if self.has_lateral_gu_children(eLaterCycle):
                    nb_lateral_gu_children += self.nb_lateral_gu_children(eLaterCycle)
                date_children_burst = self.burst_date_children(eLaterCycle)
        return apical_child, nb_lateral_gu_children, nb_inflorescences, date_children_burst, date_inflo_bloom
            










