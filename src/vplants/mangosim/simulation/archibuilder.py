from openalea.lpy import *
import vplants.mangosim.doralice_mtg.mtg_manipulation  as mm
import vplants.mangosim.util_date as ud
from datetime import datetime, date
from random import randint

class MTGArchiBuilder:
    def __init__(self, mtg, gumanager, inflomanager):
        self.mtg = mtg
        self.gumanager = gumanager
        self.inflomanager = inflomanager

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
        lat_angle = self.gumanager.phyllotaxy 
        
        nb_proc_lat = 0
        has_apical_gu = len(apical_child) > 0
        
        for ch in lat_children:
            nsproduce( [ SB(), RollL(lat_angle*nb_proc_lat), Down(60) ] )
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
                nsproduce( [ SB(), RollL(lat_angle*nb_proc_lat), Down(60), InflorescenceBud(p), EB() ] )
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
