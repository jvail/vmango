from openalea.lpy import *
import vplants.mangosim.doralice_mtg.mtg_manipulation  as mm
import vplants.mangosim.util_date as ud
from datetime import datetime, date
from random import randint


class ArchiBuilder:
    def __init__(self, gumanager, inflomanager):
        self.gumanager = gumanager
        self.inflomanager = inflomanager


class MTGArchiBuilder (ArchiBuilder):
    def __init__(self, mtg, gumanager, inflomanager):
        ArchiBuilder.__init__(self, gumanager, inflomanager)
        self.mtg = mtg

    def init(self, rootid, current_date):
        self.nbdescendants = self.estimate_nb_descendants(rootid, current_date)

        trunkparam = self.gu_parameters_from_mtg(rootid, self.nbdescendants[rootid])
        self.generate_from_mtg(trunkparam, current_date)

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
        
        guparam.leafy = not is_root
        
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
        nb_lat_elements = nb_lat_children + nb_lat_inflo

        nb_proc_lat = 0
        has_apical_gu = len(apical_child) > 0
        
        for ch in lat_children:
            nsproduce( [ SB(), RollL(phyllotaxy*nb_proc_lat), Down(branching_angle) ] )
            nb_proc_lat += 1
            if self.has_to_be_developped(ch, current_date): 
                self.generate_from_mtg( self.gu_parameters_from_mtg(ch, self.nbdescendants[ch]), current_date )
                # nproduce B( generate_parameters_GU(ch, nbdescendants[ch]) )
            else:  
                nsproduce( [ A( self.gu_parameters_from_mtg(ch) ) ] )
            nsproduce( [ EB() ] )
        if len(inflo) > 0 :
            for i in xrange(0 if has_apical_gu else 1,nb_inflorescences):
                p = self.inflo_parameters_from_mtg(inflo[i], guparam)
                self.inflomanager.set_dimensions(p, current_date)
                nsproduce( [ SB(), RollL(phyllotaxy*nb_proc_lat), Down(branching_angle), InflorescenceBud(p), EB() ] )
                nb_proc_lat += 1
              
        if has_apical_gu:
            ch = apical_child[0]
            if  self.has_to_be_developped(ch, current_date):  
                self.generate_from_mtg( self.gu_parameters_from_mtg(ch, self.nbdescendants[ch]), current_date )
                # nproduce B( generate_parameters_GU(ch, nbdescendants[ch]))
            else:   
                nsproduce( [ A( self.gu_parameters_from_mtg(ch) ) ] )
        
        elif nb_inflorescences > 0:
            p = self.inflo_parameters_from_mtg(inflo[0], guparam)
            self.inflomanager.set_dimensions(p, current_date)
            nsproduce( [ InflorescenceBud(p) ] )

    #def generate_parameters_GU(current, length, burst_date, nbdescendants):
    def gu_parameters_from_mtg(self, gu, nbdescendants = 1):
       parent = mm.get_parent(self.mtg,gu)
       p = ParameterSet(mtgid            = gu, 
                        nature           = mm.get_nature_gu(self.mtg,gu),
                        nature_parent    = mm.get_nature_gu(self.mtg,parent),
                        position         = mm.get_position_gu(self.mtg,gu), 
                        position_parent  = mm.get_position_gu(self.mtg,parent),
                        nbdescendants    = nbdescendants)

       p.set(burst_date = ud.todatetime(mm.get_burst_date(self.mtg,gu))) 
       return p


    def inflo_parameters_from_mtg(self, inflo, guparam):
        bloom_date = mm.get_bloom_dates(self.mtg,inflo)        
        if not bloom_date: 
            inflo_cycle = mm.get_unit_cycle(inflo)
            if inflo_cycle is None:
                inflo_cycle = get_cycle(guparam.burst_date) if guparam.burst_date else 3
            bloom_date = date(2000+inflo_cycle, 8, randint(1,31)) # We bloom the inflo in august
        elif type(bloom_date) is list: 
            bloom_date = bloom_date[0]
        
        nb_fruits = mm.get_nb_fruits(self.mtg,inflo)
        fruits_weight = mm.get_fruits_weight(self.mtg,inflo)

        return ParameterSet( mtgid=inflo,
                             
                             bloom_date = bloom_date,

                             nb_fruits = nb_fruits,
                             fruits_weight=fruits_weight,
                             
                             fruiting = False)

from probability_tables import *


class GLMArchiBuilder(MTGArchiBuilder):
    def __init__(self, mtg, gumanager, inflomanager):
        MTGArchiBuilder.__init__(self, gumanager, inflomanager)


    def init(self, rootid, current_date):
        MTGArchiBuilder.init(self, rootid, current_date)

        use_proba_table_from(treenames[treeselection], estimationbase, estimationtype, factorrestriction)
set_seed(SEED)


    def generate_from_glm(self, parentparam, current_date):        
        if  parentparam.burst_date <= current_date :
            
            GU = ModuleClass.get('GU')
            InflorescenceBud = ModuleClass.get('InflorescenceBud')
            A = ModuleClass.get('A')

            dev = UnitDev(Burst_Date = parentparam.burst_date, 
                          Position_A = parentparam.position,
                          Position_Ancestor_A = parentparam.position_ancestor,
                          Nature_Ancestor_F   = parentparam.nature_ancestor, #eVegetative if parentparam.nature_ancestor == eVegetative else eFlowering,
                          Tree_Fruit_Load     = Tree_Fruit_Load,
                          WithinDelayMethod   = WithinDelayMethod)

            apical_child, nb_lat_children, nb_inflorescences, nb_fruits, date_children_burst, date_inflo_bloom = dev.process()

            phyllotaxy      = self.gumanager.phyllotaxy
            branching_angle = self.gumanager.branching_angle
            
            p = parentparam.copy()
            p.set(nature = eFlowering if nb_inflorescences > 0 else eVegetative,
                  nbdescendants = 1, radius = estimate_radius(1),
                  cycle = current_cycle)
            nsproduce( [ RollL(phyllotaxy), GU(p) ] )
            
            nb_lat_inflo = nb_inflorescences if apical_child else max(0,nb_inflorescences-1)
            nb_lat_elements = nb_lat_children + nb_lat_inflo
            nb_fruiting_lat_inflo = min(nb_lat_inflo, max(0, nb_fruits - int(not apical_child)))
            if nb_lat_elements > 0:
                anglebetweenchildren = phyllotaxy
            if not date_children_burst is None:
                date_children_burst = date(year=date_children_burst[0], month=date_children_burst[1], day=15)
                if date_children_burst > cycle_end(5):
                    import warnings
                    warnings.warn('Invalid date of burst %s (after last date %s) of children with parent borned in %s (in cycle %i)' %(date_children_burst, cycle_end(5),p.burst_date,current_cycle))
                cyclechange = (get_cycle(date_children_burst) != get_cycle(p.burst_date))
                baseparameter = ParameterSet(burst_date=date_children_burst, 
                                         nature_ancestor=p.nature_ancestor if not cyclechange else p.nature, 
                                         position_ancestor=p.position_ancestor if not cyclechange else p.position)
                if nb_lat_children > 0:
                    latparam = baseparameter.copy()
                    latparam.set(position = eLateral)
                    for i in xrange(nb_lat_children):
                        nsproduce( [ RollL(anglebetweenchildren), SB(), Down(branching_angle), A(latparam.copy()), EB() ] )
                  
            if nb_inflorescences > 0:
                for i in xrange(nb_lat_inflo):
                    nsproduce( [ RollL(anglebetweenchildren), SB(), Down(branching_angle), Inflorescence(ParameterSet(bloom_date=date_inflo_bloom, cycle =  get_cycle(p.burst_date), nb_fruits = (1 if i < nb_fruiting_lat_inflo else 0))), EB() ] )
            
            if apical_child:
                p = baseparameter
                p.set(position = eApical)
                nsproduce( [ A(p) ] )
            elif nb_inflorescences > 0:
                nsproduce( [ Inflorescence(ParameterSet(bloom_date=date_inflo_bloom, cycle =  get_cycle(p.burst_date), nb_fruits = nb_fruits - nb_fruiting_lat_inflo)) ] )
