from __future__ import absolute_import
from builtins import range
from builtins import object
from openalea.lpy import *
import openalea.vmango.preparation.doralice_mtg.mtg_manipulation  as mm
import openalea.vmango.utilities.util_date as ud
from datetime import datetime, date
from random import randint
from importlib import reload


class ArchiBuilder(object):
    def __init__(self, gumanager, inflomanager, verbose = True):
        self.gumanager = gumanager
        self.inflomanager = inflomanager
        self.verbose = verbose


class MTGArchiBuilder (ArchiBuilder):
    def __init__(self, mtg, gumanager, inflomanager, verbose = True):
        ArchiBuilder.__init__(self, gumanager, inflomanager, verbose)
        self.mtg = mtg

    def init(self, rootid, current_date):
        self.nbdescendants = self.estimate_nb_descendants(rootid, current_date)

        trunkparam = self.gu_parameters_from_mtg(rootid, self.nbdescendants[rootid])
        self.generate_from_mtg(trunkparam, current_date)

    def starteach(self, current_date):
        self.current_date = current_date

    def endeach(self, current_date):
        pass

    def has_to_be_developped(self, gu, cdate):
        bdate = mm.get_burst_date(self.mtg,gu)
        if bdate is None: return True
        if type(cdate) == datetime: cdate = cdate.date()
        return bdate < cdate

    def estimate_nb_descendants(self, rootid, target_date):
        from openalea.mtg.traversal import post_order2
        nbdescendants = {}

        for gu in post_order2(self.mtg, rootid):
            if self.has_to_be_developped(gu, target_date):
                nbdescendants[gu] = sum([nbdescendants[cgu] for cgu in mm.vegetative_children(self.mtg,gu) if self.has_to_be_developped(cgu, target_date)])+1
        return nbdescendants

    def generate_from_mtg(self, apexparam, current_date):

        GU = ModuleClass.get('GU')
        InflorescenceBud = ModuleClass.get('InflorescenceBud')
        A = ModuleClass.get('A')

        phyllotaxy = self.gumanager.phyllotaxy 
        branching_angle = self.gumanager.branching_angle

        # GU is composed of leaves and internodes
        guparam = self.gumanager.set_dimensions(apexparam, current_date)
        current = apexparam.mtgid
        
        nsproduce( [ GU( guparam ) ] )
        
        is_base = (guparam.burst_date is None)
        is_root =  self.mtg.parent(current) == None
        
        #guparam.leafy = not is_root
        
        if is_root:    
            guparam.final_length_gu *= 2
        
        elif self.mtg.parent(self.mtg.parent(current)) == None:
            guparam.final_length_gu *= 1.5
        
        
        children = mm.vegetative_children(self.mtg, current)
        apical_child = [c for c in children if mm.is_apical(self.mtg, c)]
        lat_children = [c for c in children if mm.is_lateral(self.mtg,c)]
        
        inflo = mm.inflorescence_children(self.mtg, current)
        nb_inflorescences = len(inflo)
        nb_lat_children = len(lat_children)
        nb_lat_inflo = nb_inflorescences if len(apical_child) > 0 else max(0,nb_inflorescences-1)

        nb_proc_lat = 0
        has_apical_gu = len(apical_child) > 0
        
        for ch in lat_children:
            nsproduce( [ SB(), RollL(phyllotaxy*nb_proc_lat), Down(branching_angle) ] )
            nb_proc_lat += 1
            if self.has_to_be_developped(ch, current_date): 
                self.generate_from_mtg( self.gu_parameters_from_mtg(ch, self.nbdescendants.get(ch,1)), current_date )
                # nproduce B( generate_parameters_GU(ch, nbdescendants[ch]) )
            else:  
                nsproduce( [ A( self.gu_parameters_from_mtg(ch) ) ] )
            nsproduce( [ EB() ] )
        if len(inflo) > 0 :
            for i in range(0 if has_apical_gu else 1,nb_inflorescences):
                p = self.inflo_parameters_from_mtg(inflo[i], guparam)
                self.inflomanager.set_dimensions(p, current_date)
                nsproduce( [ SB(), RollL(phyllotaxy*nb_proc_lat), Down(branching_angle), InflorescenceBud(p), EB() ] )
                nb_proc_lat += 1
              
        if has_apical_gu:
            ch = apical_child[0]
            if  self.has_to_be_developped(ch, current_date):  
                self.generate_from_mtg( self.gu_parameters_from_mtg(ch, self.nbdescendants.get(ch,1)), current_date )
                # nproduce B( generate_parameters_GU(ch, nbdescendants[ch]))
            else:   
                nsproduce( [ A( self.gu_parameters_from_mtg(ch) ) ] )
        
        elif nb_inflorescences > 0:
            p = self.inflo_parameters_from_mtg(inflo[0], guparam)
            self.inflomanager.set_dimensions(p, current_date)
            nsproduce( [ InflorescenceBud(p) ] )

    #def generate_parameters_GU(current, length, burst_date, nbdescendants):
    def gu_parameters_from_mtg(self, gu, nbdescendants = 1):
       parent = mm.get_parent(self.mtg, gu)
       ancestor = mm.get_ancestor_of_previous_cycle(self.mtg, gu)
       p = ParameterSet(mtgid            = gu, 
                        nature           = mm.get_nature_gu(self.mtg,gu),
                        nature_parent    = mm.get_nature_gu(self.mtg,parent),
                        nature_ancestor  = mm.get_nature_gu(self.mtg,ancestor), 
                        position         = mm.get_position_gu(self.mtg,gu), 
                        position_parent  = mm.get_position_gu(self.mtg,parent),
                        position_ancestor  = mm.get_position_gu(self.mtg,ancestor), 
                        nbdescendants    = nbdescendants)
       if mm.is_gu_mixed_inflorescence(self.mtg, gu):
           p.mixed_inflo = True
       p.set(burst_date = ud.todatetime(mm.get_burst_date(self.mtg,gu))) 
       return p


    def inflo_parameters_from_mtg(self, inflo, guparam):
        bloom_date = mm.get_bloom_dates(self.mtg,inflo)        
        inflo_cycle = mm.get_unit_cycle(inflo)
        if inflo_cycle is None:
            inflo_cycle = get_vegetative_cycle(guparam.burst_date) if guparam.burst_date else 3
        if not bloom_date: 
            period_beg, period_end = get_bloom_weeks(inflo_cycle)[5][0], get_bloom_weeks(inflo_cycle)[7][0]
            bloom_date = period_beg + timedelta(days=randint(0,(period_end-period_beg).days))
            #bloom_date = date(2000+inflo_cycle, 9, randint(1,31)) # We bloom the inflo in august
        elif type(bloom_date) is list: 
            bloom_date = bloom_date[0]
        
        nb_fruits = mm.get_nb_fruits(self.mtg,inflo)
        fruit_weight = mm.get_fruits_weight(self.mtg,inflo)
        fruit_maturity_date = mm.get_fruits_harvest_date(self.mtg,inflo)
        if fruit_maturity_date is None:
            fruit_maturity_date = date(2000+inflo_cycle, 12, 15) + timedelta(days=randint(1,31))

        #if fruit_weight and nb_fruits > 0 :
        #    fruit_weight /= nb_fruits

        p = ParameterSet( mtgid=inflo,
                             
                          bloom_date = bloom_date,

                          nb_fruits = nb_fruits)
        if nb_fruits > 0:
            p.set(fruits_weight = fruit_weight,
                  fruits_maturity_date = fruit_maturity_date)
        return p

from . import probability_tables as pt ; reload(pt)
from .probability_tables import *


class GLMArchiBuilder(MTGArchiBuilder):
    def __init__(self, mtg, gumanager, inflomanager, verbose = True):
        MTGArchiBuilder.__init__(self, mtg, gumanager, inflomanager, verbose)


    def init(self, rootid, current_date, estimationtype = eSelectedGlm, factorrestriction = None, repeatlastprobas = False):
        MTGArchiBuilder.init(self, rootid, current_date)
        use_proba_table(estimationtype = estimationtype, restriction = factorrestriction, repeatlastprobas=repeatlastprobas)
        assert not pt.current_proba_table is None

    def starteach(self, current_date):
        self.current_date  = current_date
        self.current_cycle = get_vegetative_cycle(current_date)

    def generate_from_glm(self, param, current_date):        
        if  param.burst_date <= current_date :
            
            GU = ModuleClass.get('GU')
            InflorescenceBud = ModuleClass.get('InflorescenceBud')
            A = ModuleClass.get('A')

            dev = UnitDev(UnitType = eGU if not param.hasattr('mixed_inflo') else eMixedInflorescence,
                          Burst_Date = param.burst_date, 
                          Position_A = param.position,
                          Position_Ancestor_A = param.position_ancestor,
                          Nature_Ancestor_F   = eVegetative if param.nature_ancestor == eVegetative else eFlowering, #param.nature_ancestor, #
                          #Tree_Fruit_Load     = Tree_Fruit_Load,
                          WithinDelayMethod   = eMonthMultinomialForWithin, # eMonthMultinomialForWithin if param.nature_ancestor != eVegetative else eDeltaPoissonForWithin,
                          verbose = self.verbose)
            apical_child, nb_lat_children, mi_child, nb_inflorescences, nb_fruits, mean_fruit_weight, date_children_burst, date_inflo_bloom, mi_burst_date, harvest_date = dev.process()

            phyllotaxy      = self.gumanager.phyllotaxy
            branching_angle = self.gumanager.branching_angle
            
            param = param.copy()
            cycle = get_vegetative_cycle(param.burst_date)
            param.set(nature = eFlowering if nb_inflorescences > 0 else eVegetative,
                      nbdescendants = 1, 
                      cycle = cycle)
            guparam = self.gumanager.set_dimensions(param, current_date)
            nsproduce( [ RollL(phyllotaxy), GU(guparam) ] )
            
            nb_lat_inflo = nb_inflorescences if apical_child else max(0,nb_inflorescences-1)
            nb_fruiting_lat_inflo = min(nb_lat_inflo, max(0, nb_fruits - int(not apical_child)))
            anglebetweenchildren = phyllotaxy
            if not date_children_burst is None:
                date_children_burst = random_date_in_month(*date_children_burst)
                #date(year=date_children_burst[0], month=date_children_burst[1], day=randint(1,lastday_of_month(date_children_burst[0],date_children_burst[1])))
                #if date_children_burst > vegetative_cycle_end(5) and self.verbose:
                #    print('Invalid date of burst %s (after last date %s) of children with parent borned in %s (in cycle %i)' %(date_children_burst, vegetative_cycle_end(5),p.burst_date,current_cycle))
                cyclechange = (get_vegetative_cycle(date_children_burst) != get_vegetative_cycle(param.burst_date))
                baseparameter = ParameterSet(burst_date        = date_children_burst, 
                                             position_parent   = param.position,
                                             nature_parent     = param.nature,
                                             nature_ancestor   = param.nature_ancestor   if not cyclechange else param.nature, 
                                             position_ancestor = param.position_ancestor if not cyclechange else param.position)
                if nb_lat_children > 0:
                    latparam = baseparameter.copy()
                    latparam.set(position = eLateral)
                    for i in range(nb_lat_children):
                        p = latparam.copy()
                        nsproduce( [ RollL(phyllotaxy), SB(), Down(branching_angle), A(p), EB() ] )
                  
            if nb_lat_inflo > 0:
                for i in range(nb_lat_inflo):
                    inflo_nb_fruits = (1 if i < nb_fruiting_lat_inflo else 0)
                    p = ParameterSet(gu_burst_date = param.burst_date,
                                     bloom_date=date_inflo_bloom, 
                                     cycle =  cycle, 
                                     nb_fruits = inflo_nb_fruits)
                    if inflo_nb_fruits > 0:
                        p.set(fruits_maturity_date = harvest_date,
                              fruits_weight = mean_fruit_weight *  inflo_nb_fruits)
                    self.inflomanager.set_dimensions(p, current_date)
                    nsproduce( [ RollL(phyllotaxy), SB(), Down(branching_angle), InflorescenceBud(p), EB() ] )
            
            if mi_child:
                date_mi_child_burst = random_date_in_month(*mi_burst_date) #date(year=mi_burst_date[0], month=mi_burst_date[1], day=15)
                cyclechange = (get_vegetative_cycle(date_mi_child_burst) != get_vegetative_cycle(param.burst_date))
                p = ParameterSet(burst_date        = date_mi_child_burst, 
                                 position_parent   = param.position,
                                 nature_parent     = param.nature,
                                 nature_ancestor   = param.nature_ancestor   if not cyclechange else param.nature, 
                                 position_ancestor = param.position_ancestor if not cyclechange else param.position,
                                 position = eApical,
                                 mixed_inflo = True)
                nsproduce( [ A(p) ] )                
            elif apical_child:
                assert not date_children_burst is None
                p = baseparameter
                p.set(position = eApical)
                nsproduce( [ A(p) ] )
            elif nb_inflorescences > 0:
                inflo_nb_fruits = nb_fruits - nb_fruiting_lat_inflo
                p = ParameterSet(gu_burst_date = param.burst_date,
                                 bloom_date=date_inflo_bloom, 
                                 cycle =  cycle, 
                                 nb_fruits = inflo_nb_fruits)
                if inflo_nb_fruits > 0:
                    p.set(fruits_maturity_date = harvest_date,
                          fruits_weight = mean_fruit_weight *  inflo_nb_fruits)
                self.inflomanager.set_dimensions(p, current_date)
                nsproduce( [ InflorescenceBud(p) ] )
