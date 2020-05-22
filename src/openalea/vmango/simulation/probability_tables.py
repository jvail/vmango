from __future__ import division, print_function, unicode_literals

from builtins import map, object, range, zip
from importlib import reload

from openalea.vmango.devlaw_description import *
from openalea.vmango.constants import *
from openalea.vmango.utilities.util_date import *
from openalea.vmango.utilities.util_path import *
from past.utils import old_div

import openalea.vmango.utilities.util_path as up; reload(up)

def has_concatenated_values(array):
    for v in array:
        if type(v) == str and '-' in v:
            return True
    return False

def deconcatenate_values(array):
    val = [list(map(int,f.split('-'))) if type(f) == str and '-' in f else [int(f)] for f in array]
    index = []
    i = 0
    for v in val:
        for j in range(len(v)):
            index.append(i)
        i += 1
    val = sum(val,[])
    return val, index

class IncompatibleValues(Exception):
    pass


month_order = list(range(6,13))+list(range(6))
month_position = dict([(m,i+1) for i,m in enumerate(month_order)])

def target_monthes(parentmonth):
    return set(month_order[month_position[parentmonth]:])

class ProbaTable(object):
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
        import itertools
        if family: self.family = family

        self.factors = [ name for name in list(tablevalues.columns) if name in allfactors]
        extrafactors = [ name for name in list(tablevalues.columns) if name not in allfactors]
        if self.family == eMultinomial:
            self.answers = list(extrafactors)
            if has_concatenated_values(self.answers):
                self.answers, valindex = deconcatenate_values(self.answers)
            else:
                valindex = None
        else:
            if 'number' in extrafactors and self.family != eGaussian: extrafactors.remove('number')
            lextrafactors = list(extrafactors)
            lextrafactors.remove('probability')
            if self.family == eGaussian:
                lextrafactors.remove('stderror')
                lextrafactors.remove('number')
            if len(lextrafactors) != 0 :
                raise ValueError('Unrecognized factors', self.name, self.fname, lextrafactors)

        subset_table_factor = tablevalues[self.factors]
        subset_table_probas = tablevalues[extrafactors]
        self.values = {}
        self.factorsvalues = dict([(f,set()) for f in self.factors])
        for ind in range(len(tablevalues)):
            factorv = tuple(subset_table_factor.iloc[ind])
            probvalue = list(subset_table_probas.iloc[ind])
            if self.family == eMultinomial and not valindex is None:
                probvalue = [probvalue[i] for i in valindex]
            probvalue = [v if v > 0.01 else 0 for v in probvalue]


            tosplit = False
            for f in factorv:
                if type(f) == str :
                    tosplit = True
                    break
            if not tosplit:
                self.values[factorv] = probvalue
                for fv,f in zip(factorv, self.factors):
                    self.factorsvalues[f].add(fv)
            else:
                factorvl = [list(map(int,f.split('-'))) if type(f) == str and '-' in f else [int(f)] for f in factorv]
                for factorvi in itertools.product(*factorvl):
                    self.values[factorvi] = probvalue
                    for fv,f in zip(factorvi, self.factors):
                        self.factorsvalues[f].add(fv)


    def get_proba_value(self, args):
        if len(args) < len(self.factors):
            raise ValueError('Invalid number of factors.',args, self.factors)
        for arg, value in list(args.items()):
            if arg in self.factorsvalues and not value in self.factorsvalues[arg]:
                #print arg, value
                if arg in ['Nature_F','Nature_Ancestor_F'] and value == eFruiting:
                    if eFlowering in self.factorsvalues[arg]:
                        args[arg] = eFlowering
                        continue
                raise KeyError('Invalid value for factor of test',value,arg,self.name)
        try:
            factoractualvalue = tuple([args[f] for f in self.factors])
        except KeyError as e:
            #print self.factors, args, self.type, self.name
            raise e
        try:
            return self.values[factoractualvalue]
        except KeyError as e:
            # print self.factors, factoractualvalue, self.type, self.name
            raise KeyError(self.factors, e.args)

    def realization(self, **args):
        from numpy import cumsum
        from numpy.random import binomial, poisson, uniform, normal
        from math import sqrt
        probavalue = self.get_proba_value(args)
        if self.family == eBinomial:
            return bool( binomial(1,probavalue[0]) )
        elif self.family == ePoisson:
            return int( poisson(probavalue[0],1) )
        elif self.family == eGaussian:
            return float(normal(probavalue[0], probavalue[1]*sqrt(0 if probavalue[2] <= 0 else probavalue[2]), 1))
        elif self.family == eMultinomial:
            check_compat = True
            if check_compat:
                compatvalues = self.check_value_compatibility(args)
                if len(compatvalues) == 0:
                    raise IncompatibleValues(self.name, args['Burst_Month'], self.answers)
                probavalue = [probavalue[i] for i in compatvalues]
                answers = [self.answers[i] for i in compatvalues]
            else:
                answers = self.answers
            cumsum_probs = list( cumsum(probavalue) )
            unif_realization = float( uniform(0,1,1) )
            cumsum_probs[-1] = 1
            i = 0
            while unif_realization >= cumsum_probs[i] : i += 1
            return answers[i]

    def check(self):
        if self.type == eWithinCycle:
            withinfactors = list(allfactors)
            withinfactors.remove('Nature_F')
            for f in self.factors:
                if not f in withinfactors:
                    raise ValueError('Invalid factor for Within cycle proba',f)
        elif self.type == eLaterCycle:
            betweenfactors = list(allfactors)
            betweenfactors.remove('Position_Ancestor_A')
            betweenfactors.remove('Nature_Ancestor_F')
            for f in self.factors:
                if not f in betweenfactors:
                    raise ValueError('Invalid factor for Within cycle proba',f)

    def check_value_compatibility(self, args):
        assert self.family == eMultinomial
        if self.type == eWithinCycle and 'burst_date' in self.name:
            targetanswers = target_monthes(args['Burst_Month'])
            return [i for i,a in enumerate(self.answers) if int(a) in targetanswers]
        else:
            return list(range(len(self.answers)))

    def check_cycle(self, cycle):
        if 'Cycle' in self.factors:
            cyclepos = self.factors.index('Cycle')
            nvalues = {}
            for key, val in list(self.values.items()):
                if key[cyclepos] == self.cycle:
                    nkey = list(key)
                    del nkey[cyclepos]
                    nkey = tuple(nkey)
                    nvalues[nkey] = val
            self.values = nvalues
            del self.factors[cyclepos]
            del self.factorsvalues['Cycle']

def read_proba_tables(variety = 'cogshall', estimationtype = eCompleteGlm, restriction = None):
    from os.path import exists, join
    probafilepath = get_probability_repository(variety, estimationtype, restriction)
    if not exists(probafilepath): raise ValueError("Proba path repository does not exist", probafilepath)
    probacycle = {}
    replacement = {'gu_flowering_week_within_04' : 'gu_flowering_week_within_05', 'gu_harvest_week_within_04' : 'gu_harvest_week_within_05' }
    for cycle in range(3,6):
        proba_within, proba_between = {}, {}
        if within_extension[cycle]:
            ext = within_extension[cycle]
            for prop,family in zip( gu_vegetative_proba+gu_vegetative_proba_within+gu_flowering_proba+gu_fruiting_proba+mi_vegetative_proba+mi_vegetative_proba_within,
                                    gu_vegetative_proba_family+gu_vegetative_proba_within_family+gu_flowering_proba_family+gu_fruiting_proba_family+mi_vegetative_proba_family+mi_vegetative_proba_within_family):
                if prop.startswith('mi_'): ext = 'within_0405'
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if not exists(propfile) and prop+'_'+ext in replacement:
                    propfile = join(probafilepath,replacement[prop+'_'+ext]+'.csv')
                if not exists(propfile):
                    import warnings
                    warnings.warn("Table '%s' for variety '%s' does not exist." % (prop+'_'+ext, variety))
                else:
                    p = ProbaTable(prop,family,propfile)
                    p.type = eWithinCycle
                    p.cycle = cycle
                    p.check_cycle(cycle)
                    p.estimation = (variety, estimationtype)
                    p.check()
                    proba_within[prop] = p
        if between_extension[cycle]:
            ext = between_extension[cycle]
            for prop, family in zip(gu_vegetative_proba+gu_vegetative_proba_between+gu_mixedinflo_proba,
                                    gu_vegetative_proba_family+gu_vegetative_proba_between_family+gu_mixedinflo_proba_family):
                propfile = join(probafilepath,prop+'_'+ext+'.csv')
                if exists(propfile):
                    p = ProbaTable(prop,family,propfile)
                    p.type = eLaterCycle
                    p.cycle = cycle
                    p.check_cycle(cycle)
                    p.estimation = (variety, estimationtype)
                    p.check()
                    proba_between[prop] = p
                else:
                    import warnings
                    warnings.warn("Table '%s' for variety '%s' does not exist." % (prop+'_'+ext, variety))
        probacycle[cycle] = (proba_within, proba_between)
    return probacycle


def make_repetition(proba_table, maxcycle = 10):
    probacycle = proba_table.copy()
    probacycle[5] = (probacycle[5][0],probacycle[4][1])
    for cycle in range(6,min(6,maxcycle+1)):
        probacycle[cycle] = probacycle[5]
    return probacycle

global_proba_tables = {}
current_proba_table = None

def get_proba_tables(variety = 'cogshall', estimationtype = eSelectedGlm, restriction = None):
    global global_proba_tables
    tableid = (variety, estimationtype, restriction)
    if not tableid in global_proba_tables:
        global_proba_tables[tableid] = read_proba_tables(variety, estimationtype, restriction)
    return global_proba_tables[tableid]

def use_proba_table(variety = 'cogshall', estimationtype = eSelectedGlm, restriction = None, repeatlastprobas = False):
    global current_proba_table
    current_proba_table = get_proba_tables(variety, estimationtype, restriction)
    if repeatlastprobas:
        current_proba_table = make_repetition(current_proba_table)

def iterprobatables():
    for k, ps in list(global_proba_tables.items()):
        for cycle, pbs in list(ps.items()):
            wpbs, lpbs =  pbs
            for pbname, pb in list(wpbs.items()):
                yield pb
            for pbname, pb in list(lpbs.items()):
                yield pb

def check_proba_tables():
    def check_which_values(pb, factor = 'Burst_Month'):
        if factor in pb.factors:
            idx = pb.factors.index(factor)
            res = set()
            for key, prob in list(pb.values.items()):
                if prob[0] > 0.01:
                    res.add(key[idx])
        else:
            res = factorsvalues[factor]
            res = set(res)
        return res

    def check_values_incompatibility(pb, factor, values):
        if not factor in pb.factors:
            return []
        else:
            res = set(values)
            idx = pb.factors.index(factor)
            for key, prob in list(pb.values.items()):
                res.discard(key[idx])
            return list(res)

    def check_consistency(tables, probanames):
        pbinit = tables[probanames[0]]
        for factor in pbinit.factors:
            refvalues = check_which_values(pbinit, factor)
            #print 'Values of',factor,'in',repr(probanames[0]),':',refvalues
            for pbname in probanames[1:]:
                if pbname in tables:
                    incompatvalues = check_values_incompatibility(tables[pbname], factor, refvalues)
                    if len(incompatvalues):
                        print('Incompatible values coverage of ',repr(factor),'between',repr(probanames[0]),'and', repr(pbname), ':',incompatvalues)
                else:
                    print('Cannot find', repr(pbname))

    for cycle, pbs in list(current_proba_table.items()):
        print('Examining cycle',cycle)
        wpbs, lpbs =  pbs
        if len(wpbs) > 0:
            check_consistency(wpbs, gu_vegetative_proba+gu_vegetative_proba_within+[gu_flowering_proba[0],gu_fruiting_proba[0]])
            check_consistency(wpbs, gu_flowering_proba+[gu_fruiting_proba[0]])
            check_consistency(wpbs, gu_fruiting_proba)
            check_consistency(wpbs, mi_vegetative_proba+mi_vegetative_proba_within)
        if len(lpbs) > 0:
            check_consistency(lpbs, gu_vegetative_proba+gu_vegetative_proba_between)
            check_consistency(lpbs, gu_mixedinflo_proba)



def set_seed(value):
    from numpy.random import seed
    seed(value)
    from random import seed
    seed(value)

current_unitdev = None


def appendmonthdelta(intialdate, monthdelta):
        burst_year = intialdate.year
        burst_month = intialdate.month + monthdelta
        if burst_month  > 12:
            dyear = (burst_month-1) // 12
            burst_year += dyear
            burst_month -=  dyear*12
            if not(1 <= burst_month <= 12):
                raise ValueError('Invalid month',burst_month, monthdelta, self.burst_date.month)
        return burst_year, burst_month

def appenddeltaindex(burst_date, burst_index):
    cycle_delay, burst_month = divmod(burst_index,100)
    pyear = burst_date.year
    if burst_date.month < 6: pyear -= 1
    burst_year = pyear + cycle_delay
    if burst_month < 6:  burst_year += 1
    return burst_year, burst_month


def find_closest_values(value, existingvalues, keyordering = None):
    if value in existingvalues: return value
    existingvalues.append(value)
    existingvalues.sort(key = keyordering)
    valueindex = existingvalues.index(value)
    if valueindex == 0 : return existingvalues[1]
    elif valueindex == (len(existingvalues) -1): return existingvalues[-2]
    else: return (existingvalues[valueindex-1],existingvalues[valueindex+1])

class UnitDev(object):
    def __init__(self, Burst_Date,
                       Position_A,
                       Nature_F = None,
                       Position_Ancestor_A = None,
                       Nature_Ancestor_F = None,
                       UnitType = eGU,
                       # Tree_Fruit_Load  = eLoaded,
                       WithinDelayMethod = eDeltaPoissonForWithin,
                       verbose = True):
        self.burst_date = Burst_Date
        self.cycle = get_vegetative_cycle(Burst_Date)
        self.trace = False
        self.withindelaymethod = WithinDelayMethod
        self.unittype = UnitType

        self.params = dict(Burst_Month = Burst_Date.month,
                           Position_A = Position_A,
                           Position_Ancestor_A = Position_Ancestor_A,
                           Nature_Ancestor_F   = Nature_Ancestor_F, # eVegetative if Nature_Ancestor_F == eVegetative else eFlowering,
                           #Tree_Fruit_Load     = Tree_Fruit_Load
                           Cycle = self.cycle
                           )

        self.paramsdelayed = dict(Burst_Month = Burst_Date.month,
                                  Position_A = Position_A,
                                  Nature_F   = Nature_F,
                                  Cycle = self.cycle
                                  #Tree_Fruit_Load = Tree_Fruit_Load
                                  )

        self.proba_tables = current_proba_table.get(self.cycle)
        self.verbose = verbose
        global current_unitdev
        current_unitdev = self

    def log(self, cycle, processname, msg):
        if self.verbose:
            print(self.cycle,'Within' if cycle == eWithinCycle else 'Delayed','>',processname,':', msg)

    def get_name(self, name):
        return ('gu_' if self.unittype == eGU else 'mi_') + name

    def has_table(self, cycle = eWithinCycle):
        return not self.proba_tables is None and len(self.proba_tables[0 if cycle == eWithinCycle else 1]) > 0

    def get_table(self, name, cycle = eWithinCycle):
        name = self.get_name(name)
        return self.proba_tables[0 if cycle == eWithinCycle else 1][name]

    def get_realization(self, name, cycle = eWithinCycle):
        if self.trace: print('Test',self.cycle,'Within' if cycle == eWithinCycle else 'Delayed',name)
        p = (self.params if cycle == eWithinCycle else self.paramsdelayed)
        #print 'current_proba_table[',self.cycle,'][', 0 if cycle == eWithinCycle else 1,"]['"+self.get_name(name)+"']"
        proba_table = self.get_table(name, cycle)
        try:
            return proba_table.realization(**p)
        except KeyError as ie:
            self.log(cycle, name, repr(ie))
            raise ie


    def vegetative_burst(self, cycle = eWithinCycle):
        if cycle == eWithinCycle and self.burst_date.month == vegetative_cycle_end(self.cycle).month:
            self.log(cycle, 'vegetative_burst', 'Too late to decide')
            return False
        try:
            return self.get_realization('vegetative_burst',cycle)
        except KeyError as ie: # some month may not be fulfilled.
            return False

    def find_closest_factor_values(self, name, cycle = eWithinCycle, factorname = 'Burst_Month'):
        import numpy as np
        proba_table = self.get_table(name, cycle)
        refvalue = self.params[factorname]
        keyordering = {'Burst_Month' : lambda v : MonthOrder.index(v)}
        if factorname in proba_table.factors:
            idx = proba_table.factors.index(factorname)
            existingvalues = list(np.unique([k[idx] for k in list(proba_table.values.keys())]))
            return find_closest_values(refvalue, existingvalues, keyordering=keyordering.get(factorname))
        else:
            return refvalue

    def get_realization_from_closestvalues(self, name, cycle = eWithinCycle, factorname = 'Burst_Month'):
        try:
            return self.get_realization(name, cycle = cycle)
        except KeyError as ke:
            mth = self.find_closest_factor_values(name, cycle=cycle, factorname=factorname)
            p = (self.params if cycle == eWithinCycle else self.paramsdelayed)
            cm = p[factorname]
            if type(mth) == tuple:
                mth1, mth2 = mth
                p[factorname] = mth1
                res1 = self.get_realization(name, cycle)
                p[factorname] = mth2
                res2 = self.get_realization(name, cycle)
                res = old_div((res1+res2),2)
            else:
                p[factorname] = mth
                res = self.get_realization(name, cycle)
            p[factorname] = cm
            return res

    def burst_delta_date_gu_children(self, cycle = eWithinCycle):
        return int(self.get_realization_from_closestvalues('burst_delta_date_gu_children', cycle))

    def burst_delta_date_gu_children_poisson(self, cycle = eWithinCycle):
        return self.get_realization_from_closestvalues('burst_delta_date_gu_children_poisson', cycle)+1

    def gu_burst_date_children(self, cycle = eWithinCycle):
        if cycle == eLaterCycle or self.withindelaymethod == eMonthMultinomialForWithin:
            try:
                burst_index = int(self.get_realization('burst_date_gu_children',cycle))
                burst_year, burst_month = appenddeltaindex(self.burst_date, burst_index)
            except KeyError as ie: # some month may not be fulfilled.
                if cycle == eLaterCycle:
                    burst_index = int(self.get_realization_from_closestvalues('burst_date_gu_children', cycle))
                    burst_year, burst_month = appenddeltaindex(self.burst_date, burst_index)
                else:
                    burst_delta = self.burst_delta_date_gu_children_poisson(cycle)
                    burst_year, burst_month = appendmonthdelta(self.burst_date, burst_delta)
        else:
            if self.withindelaymethod == eDeltaMultinomialForWithin:
                burst_delta = self.burst_delta_date_gu_children(cycle)
            else:
                burst_delta = self.burst_delta_date_gu_children_poisson(cycle)
            burst_year, burst_month = appendmonthdelta(self.burst_date, burst_delta)


        #if  not (burst_year > self.burst_date.year or burst_month >= self.burst_date.month):
        #    self.log(cycle, 'burst_date_gu_children', 'Invalid children date. Earlier than parent')
        #    return self.gu_burst_date_children(cycle)
        #    #raise ValueError('Children burst date is before parent burst',(burst_year, burst_month), self.burst_date )
        #if (burst_year == self.burst_date.year and burst_month == self.burst_date.month):
        #    self.log(cycle, 'burst_date_gu_children', 'Invalid children date. Same than parent')
        #    return self.gu_burst_date_children(cycle)
        #    #print 'Warning: Children GUs are borned in the same month than their parent GU'
        if cycle == eWithinCycle:
            endcycle = vegetative_cycle_end(self.cycle)
            if (endcycle < date(year=burst_year,month=burst_month,day=15)):
                self.log(cycle, 'burst_date_gu_children', 'Invalid children date. Outside cycle')
                # print 'Warning within cycle children borned outside cycle'
                return self.gu_burst_date_children(cycle)
        return (burst_year, burst_month)

    def mi_burst_date_children(self):
        burst_index = int(self.get_realization('burst_date_gu_children', eWithinCycle))
        burst_year, burst_month = appenddeltaindex(self.burst_date, burst_index)
        return (burst_year, burst_month)

    def has_apical_gu_child(self, cycle = eWithinCycle):
        try:
            return self.get_realization('has_apical_gu_child',cycle)
        except KeyError as ie:
            return True

    def has_lateral_gu_children(self, cycle = eWithinCycle):
        try:
            return self.get_realization('has_lateral_gu_children',cycle)
        except KeyError as ie:
            return False

    def nb_lateral_gu_children(self, cycle = eWithinCycle):
        try:
            return self.get_realization('nb_lateral_gu_children',cycle)+1
        except KeyError as ie:
            return 1

    def nb_gu_children(self, cycle = eWithinCycle):

        try:
            return self.get_realization('nb_gu_children',cycle)+1
        except KeyError as ie:
            return 1

    def flowering(self):
        try:
            return self.get_realization('flowering')
        except KeyError as ie:
            return False

    def nb_inflorescences(self):
        val = 0
        while not (1 <= val <=5):
            val = int(self.get_realization_from_closestvalues('nb_inflorescences'))+1
        return val

    def flowering_date(self):
        from random import randint
        from datetime import timedelta
        fweek = 5
        try:
            fweek = int(self.get_realization('flowering_week'))
        except KeyError as e:
            fweek = randint(0,max(get_bloom_weeks(self.cycle).keys()))
        period_beg, period_end = get_bloom_weeks(self.cycle)[fweek]
        return period_beg + timedelta(days=randint(0,(period_end-period_beg).days)), fweek

    def fruiting(self):
        try:
            return self.get_realization('fruiting')
        except KeyError as ie:
            return False

    def nb_fruits(self):
        try:
            return int(self.get_realization('nb_fruits'))+1
        except KeyError:
            return int(self.get_realization_from_closestvalues('nb_fruits', factorname = 'Nb_Inflorescences'))+1

    def fruit_weight(self):
        try:
            return int(self.get_realization('fruit_weight'))
        except KeyError:
            return int(self.get_realization_from_closestvalues('fruit_weight'))

    def harvest_date(self):
        from random import randint
        from datetime import timedelta
        try:
            fweek = int(self.get_realization('harvest_week'))
        except KeyError as e:
            fweek = randint(0,max(get_harvest_weeks(self.cycle).keys()))
        period_beg, period_end = get_harvest_weeks(self.cycle)[fweek]
        return period_beg + timedelta(days=randint(0,(period_end-period_beg).days)), fweek

    def has_mixedinflo_child(self):
        try:
            return self.get_realization('mixedinflo_burst')
        except KeyError as ie:
            return False

    def is_mixedinflo_apical(self):
        try:
            return self.get_realization('has_apical_mi_children')
        except KeyError as ie:
            return True

    def mixedinflo_burstdate(self):
        burst_index = int(self.get_realization('burst_date_mi_children', eBetweenCycle))
        burst_year, burst_month = appenddeltaindex(self.burst_date, burst_index)
        return (burst_year, burst_month)

    def process(self):
        apical_child, nb_lateral_gu_children = False, 0
        has_mi_child, mi_burst_date = None, None
        nb_inflorescences, nb_fruits, fruit_weight =  0, 0, 0
        date_children_burst, date_inflo_bloom, harvest_date = None, None, None

        if self.has_table():
            veg_burst = self.vegetative_burst()
            if veg_burst:
                # Within steps.
                try:
                    if self.unittype == eGU:
                        date_children_burst = self.gu_burst_date_children()
                    else:
                        date_children_burst = self.mi_burst_date_children()
                except IncompatibleValues as iv:
                    self.log(eWithinCycle, 'burst_date_children', 'Incompatible children month burst value compared to parent burst month.')
                    veg_burst = False

                if veg_burst:
                    apical_child = self.has_apical_gu_child()

                    #nb_children  = self.nb_gu_children()
                    #nb_lateral_gu_children = nb_children - apical_child

                    nb_lateral_gu_children = 0
                    if not apical_child or self.has_lateral_gu_children():
                        nb_lateral_gu_children += self.nb_lateral_gu_children()
        else :
            veg_burst = False


        if not veg_burst and self.unittype == eGU:
            # Between steps
            if self.has_table() and self.has_mixedinflo_child():
                has_mi_child = eApical if self.is_mixedinflo_apical() else eLateral
                mi_burst_date = self.mixedinflo_burstdate()

            if self.has_table() and self.flowering():
                self.paramsdelayed['Nature_F'] = eFlowering
                nb_inflorescences = self.nb_inflorescences()
                date_inflo_bloom, fweek  = self.flowering_date()
                self.params['Flowering_Week'] = fweek
                self.params['Nb_Inflorescences'] = nb_inflorescences
                if self.fruiting():
                    nb_fruits    = self.nb_fruits()
                    fruit_weight = self.fruit_weight()
                    harvest_date, hweek = self.harvest_date()
            else:
                self.paramsdelayed['Nature_F'] = eVegetative

            if self.has_table(eLaterCycle) and self.vegetative_burst(eLaterCycle):

                apical_child = self.has_apical_gu_child(eLaterCycle) if has_mi_child != eApical else False
                self.paramsdelayed['Has_Apical_GU_Child'] = apical_child

                #nb_children  = self.nb_gu_children(eLaterCycle)
                #nb_lateral_gu_children = nb_children - apical_child

                nb_lateral_gu_children = 0
                if not apical_child or self.has_lateral_gu_children(eLaterCycle):
                    nb_lateral_gu_children += self.nb_lateral_gu_children(eLaterCycle)

                date_children_burst = self.gu_burst_date_children(eLaterCycle)

        return apical_child, nb_lateral_gu_children, has_mi_child, nb_inflorescences, nb_fruits, fruit_weight, date_children_burst, date_inflo_bloom, mi_burst_date, harvest_date
