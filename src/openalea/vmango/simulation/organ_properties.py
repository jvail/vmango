from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from builtins import object, range, str, zip
from importlib import reload
from math import exp
from random import *

import numpy as np
from openalea.lpy import *
from openalea.plantgl.all import NurbsCurve2D, Point3Array, QuantisedFunction
from openalea.vmango.constants import *
from openalea.vmango.simulation.temperature import *
from openalea.vmango.simulation.temperature import get_temperature
from openalea.vmango.simulation.thermaltime import *
from openalea.vmango.utilities.util_date import *
from past.utils import old_div


def get_realisation(mean, sd, minval, maxval, rfunc = gauss):
    val = rfunc(mean, sd)
    while (val < minval) or (val > maxval):
        val = rfunc(mean, sd)
    return val

class OrganManager(object):
    def __init__(self, **kwargs):
        self.phyllotaxy = 144  # +randint(-2,2)          # mango phyllotaxie
        pass

class GUManager (OrganManager):
    def __init__(self,  **kwargs):
      OrganManager.__init__(self, **kwargs)

      # Initializing variables
      self.t_ip_gu_mean = 79.40 # should be 178.8/2.
      self.t_ip_gu_sd   = 10.75
      self.t_ip_leaf    = 182.04/2.                           # Inflexion point of leaf growth curve

      self.base_temperature_gu    = 9.2                # Base temperature of GUs
      self.base_temperature_leaf  = 10.73              # Base temperature of Leafs

      self.final_diameter = 0.2                           # diameter of GUs (at the end of growth)

      # Phenological stages definition
      self.pheno_base_temp   = [13.37766, 13.37766, 13.37766, 9.784431] #,0]   # stages DEF_G_H
      self.pheno_stade_temp  = [38.50,    47.61,    47.39,    316.376] #,999]   # temperatures of GU's stage change
      #self.pheno_change_temp = np.cumsum(self.pheno_stade_temp)

      self.base_color  = 14

      self.pheno_color = list(range(self.base_color, self.base_color+5))# [7,10,11,14,13]                         # color for each stage (GU)
      self.pheno_angle_values = [0,90,165,60,60]                        # angle between leaf and internode for each stage
      self.nb_stades_pheno = len(self.pheno_stade_temp)              # number of phenological stages

      self.pheno_angle = QuantisedFunction(NurbsCurve2D(Point3Array(list(enumerate(self.pheno_angle_values)),1), degree = 1 ) )

      self.pheno_stadename =    [ 'ABCD', 'E', 'F', 'G', 'H' ]

      # Colors definition
      self.mixed_inflo_color = self.pheno_color[-1]

      self.oldwood_color = 5
      self.stem_color = 6
      self.petiolecolor = 6
      self.textures_colorid = 10

      # Gaussian distribution parameters

      # GUs length
      # Depends on Position. 3 Distributions are found. Apical Gu on Apical parent, Apical Gu on Lateral parent and Lateral GU.
      # Mean and Std dev of length distribution.
      self.gu_length_distrib = { (eApical, eApical)   : ( 18.14 , 4.14 ) ,
                                 (eApical, eLateral)  : ( 13.79 , 4.03 ) ,
                                 (eLateral, eApical)  : ( 12.59 , 3.38 ) ,
                                 (eLateral, eLateral) : ( 12.59 , 3.38 ) }

      # Number of leafs
      # Depends on Position and GU's length
      # Ratio and intercept for the relation with leaf nb and length.
      self.leaf_nb_distrib = { eApical  : ( 0.59, 5.5),
                               eLateral : ( 0.62, 0.36) }

      # Leaves length
      # Depends on Position
      # Mean and Std dev of length distribution.
      self.leaf_length_distrib = { eApical  : ( 17.06 , 2.7) ,
                                   eLateral : ( 14.87 , 2.7) }

      #
      self.leaflength = QuantisedFunction(NurbsCurve2D( Point3Array([(0, 1, 1),(0.00149779, 1.00072, 1),(1, 0.995671, 1),(1, 0.400121, 1)])))

      # Leaf dimensions
      self.leaf_area_length_ratio   = 2.3594
      self.leaf_width_length_ratio  = 0.24

      self.radius_exponent = 0.443 # 0.5
      self.radius_coefficient = 0.415 # 0.3

      self.branching_angle = 60

      self.max_leafy_diameter = 1.65

      self.__dict__.update(kwargs)

    def set_parameters(self, leafaxis, leafsection, leafwidth, leafwidthgrowth, petioleCurve, resolution):
          # Graphic Parameters
          self.resolution = resolution
          if resolution == 2:
              self.LeafWidthRes  = 10
              self.LeafLengthRes = 20
              self.InternodeRes  = 1
              self.PetioleRes    = 15
          else:
              self.LeafWidthRes  = 5
              self.LeafLengthRes = 5
              self.InternodeRes  = 1
              self.PetioleRes    = 3

          leafsection.stride = self.LeafWidthRes
          from .leafgeometry import SymbolManager

          self.leafwidth = leafwidth
          self.leafwidthgrowth = leafwidthgrowth
          self.petioleCurve = petioleCurve
          self.leafSymbol = SymbolManager(leafaxis,[0,0.1,0.5,0.6,0.7,0.8], 3, leafsection, 1., 1./self.LeafLengthRes, self.leaf_width_length_ratio, leafwidth)

    def retrieve_parameters(self, namespace):
        from .leafgeometry import retrieveCurves
        curves = retrieveCurves(namespace)
        self.set_parameters(curves, namespace['leafsection'], namespace['leafwidth'], namespace['leafwidthgrowth'], namespace['petioleCurve'], namespace['RESOLUTION'])

    def step(self, daystep):
        pass

    def internode_length_distribution(self, nb_internodes, gu_length):
      """ Internode length distribution """

      lengths = [exp(-2.64 * i / float(nb_internodes-1)) for i in range(nb_internodes)]
      scaling = old_div(gu_length, sum(lengths))
      return [l*scaling for l in lengths]

    def internode_length(self, i, nb_internodes, gu_length):
        sl = -2.64
        e = exp()
        du = 1./nb_internodes
        k = old_div((1-pow(e,du)),(1-e))
        return gu_length * exp(-sl*i*du)

    def length_before_first_leaf(self, position, final_length_gu):
      #length of space before the first leaf
      if position == eApical:
        from numpy.random import gamma
        # LEPF = gauss(2.63,1.72)
        LEPF = get_realisation(2.007, 0.763, 0, 8, gamma)

      else: # Lateral case
        #length of space before the first leaf depend of GU's length
        LEPF = final_length_gu * 0.38 + 0.88
      return LEPF

    def estimate_radius(self, nbdescendants):
        return self.radius_coefficient*pow(nbdescendants, self.radius_exponent)


    def set_dimensions(self, params, current_date, temperature_variability = False):
      # We define parameters for simulation

      position = params.position
      position_parent = params.position_parent

      mean, sd = self.gu_length_distrib[(position,position_parent)]
      final_length_gu = get_realisation(mean, sd, 5, 25)

      ratio, intercept = self.leaf_nb_distrib[position]
      nb_internodes = max(int(round(intercept + ratio*final_length_gu)),1)

      mean, sd = self.leaf_length_distrib[position]
      final_length_leaves = [get_realisation(mean, sd, 5, 34) * self.leaflength(i/float(nb_internodes-1)) for i in range(nb_internodes)]

      LEPF = self.length_before_first_leaf(position, final_length_gu)
      final_length_internodes = [LEPF] + self.internode_length_distribution(nb_internodes-1, final_length_gu)

      t_ip= gauss(self.t_ip_gu_mean,self.t_ip_gu_sd)

      params = params.copy()

      radius = self.estimate_radius(params.nbdescendants)

      params.set(radius = radius,
                 length_gu               =  0,

                 final_length_gu         = final_length_gu,
                 final_length_leaves     = final_length_leaves,
                 final_length_internodes = final_length_internodes,

                 nb_internodes    = nb_internodes,

                 t_ip             = t_ip,

                 leafy = radius < self.max_leafy_diameter)

      if params.burst_date:
          delta_base_temp  = gauss(0,3) if temperature_variability else 0
          gu_growth_tts    = ThermalTimeAccumulator(self.base_temperature_gu - delta_base_temp)
          leaf_growth_tts  = ThermalTimeAccumulator(self.base_temperature_leaf - delta_base_temp)
          pheno_base_temp  = self.pheno_base_temp if not temperature_variability else [t - delta_base_temp  for t in self.pheno_base_temp]
          gu_pheno_tts     = MultiPhaseThermalTimeAccumulator(pheno_base_temp, self.pheno_stade_temp)

          for day in date_xrange(params.burst_date, current_date+timedelta(days=1)):
            daytemp = get_temperature(day)
            for tts in [gu_growth_tts, leaf_growth_tts, gu_pheno_tts]:
                tts.accumulate(daytemp)

          params.set(gu_growth_tts    = gu_growth_tts,
                     leaf_growth_tts  = leaf_growth_tts,
                     gu_pheno_tts     = gu_pheno_tts)

          params.length_gu = self.gu_growth_function(gu_growth_tts.ttsum, final_length_gu, t_ip = t_ip)
          params.basestructure = False

      else:
          params.basestructure = True
      return params


    # Function who return an organ (Leaf, Internode or GU) length
    def gu_growth_function(self, T,  FinalSize, t_ip = None):
        """ Sigmoid function used to compute growth of organs """
        # relationship between final size and maximum growth rate for GUs
        # maxGR = 0.0111513*FinalSize
        # B = FinalSize/(4*maxGR)
        B = 22.41891079963771 # 1. / 0.0446052
        return old_div(FinalSize,(1+exp(old_div(-(T-t_ip),B))))        # sigmoid equation

    def leaf_growth_function(self, T,  FinalSize):
        maxGR = -0.0188725+0.0147985*FinalSize
        B = old_div(FinalSize,(4*maxGR))
        return old_div(FinalSize,(1+exp(old_div(-(T-self.t_ip_leaf),B))))

    def gu_growth(self, params, daystep, current_temperatures):
        if not params.basestructure:
            if params.gu_pheno_tts.stage < 4 :
              # Update of parameters
              for tts in [params.gu_growth_tts, params.leaf_growth_tts, params.gu_pheno_tts]:
                  if daystep > 1:
                    for ctemperature in current_temperatures:
                        tts.accumulate(ctemperature)
                  else:
                    tts.accumulate(current_temperatures[-1], daystep)

              params.length_gu = self.gu_growth_function(params.gu_growth_tts.ttsum, params.final_length_gu, t_ip = params.t_ip)  # GU's length calculation
          #p.diam   = growth_function(eGU, p.gu_growth_tts.ttsum, final_diamI, t_ip = p.t_ip)     # GU's diameter calculation

        if params.radius >= self.max_leafy_diameter: params.leafy = False

    def init_plot(self):
        execContext().turtle.setSurface('finalleaf', self.leafSymbol())

    def plot(self, p, textured = True, leafy = True, distinct_mi = False):
        def fLeaf(position, radius, size):
            #nsproduce([ SetColor(13), Down(self.pheno_angle(4)), surface('finalleaf', size)])
            #return
            nsproduce([ EndGC(),Elasticity(0.),Down(90),f(radius), Up(90), SetColor(self.petiolecolor),SetWidth(self.leafwidth(0)*self.leafwidthgrowth(1))])
            # petiole
            nsproduce([ Down(self.pheno_angle(4)) ])
            Petiole((1.1-position)* size/4.,self.leafwidth(0))
            nsproduce([ RollToVert() ])
            if distinct_mi and p.hasattr('mixed_inflo'):
                nsproduce([ SetColor(self.mixed_inflo_color) ])
            elif textured:
                nsproduce([ TextureBaseColor(self.pheno_color[-1]), SetColor(self.textures_colorid+1), TextureVScale(1) ])
                #nsproduce([ SetColor(30), TextureVScale(1) ])
            else:
                nsproduce([ SetColor(self.pheno_color[-1]) ])
            nsproduce([ surface('finalleaf', size) ])
            #nsproduce([ PglShape(self.leafSymbol(), size) ])

        def gLeaf(position, radius, final_length, growth_ratio, pheno_stage, pheno_rank, ttsum):
            petioleradius  = self.leafwidth(0)*self.leafwidthgrowth(min(growth_ratio,1))
            nsproduce([ EndGC(), Elasticity(0.), Down(90),f(radius), Up(90) ])
            # Angle depends of phenological stage and advancement
            nsproduce([ Down(self.pheno_angle(pheno_stage+pheno_rank)),SetWidth(petioleradius) ])
            # Color depends of phenological stage and advancement
            pheno_color = self.pheno_color
            if False:
                if pheno_rank > 0:
                    nsproduce([ InterpolateTextureBaseColors(pheno_color[pheno_stage],pheno_color[pheno_stage+1],pheno_rank) ])
                else:
                    nsproduce([ TextureBaseColor(pheno_color[pheno_stage]) ])
                nsproduce([ TextureVScale(1), SetColor(self.textures_colorid+1) ])
            else:
                if pheno_rank > 0:
                    nsproduce([ InterpolateColors(pheno_color[pheno_stage],pheno_color[pheno_stage+1],pheno_rank) ])
                else:
                    nsproduce([ SetColor(pheno_color[pheno_stage]) ])

            # petiole
            petiolelength = max(0.01,(1-position)* final_length/4.)
            Petiole(petiolelength, petioleradius)
            nsproduce([ RollToVert() ])

            clength = final_length * growth_ratio
            nsproduce([ PglShape(self.leafSymbol(pheno_stage+pheno_rank), clength) ])

        def Petiole(length,radius):
            if self.resolution > 0:
                nsproduce([ SetWidth(radius), SetGuide(self.petioleCurve,length), nF(length, old_div(length,self.PetioleRes),radius) ])
            else:
                nsproduce([ SetWidth(radius),SetGuide(self.petioleCurve,length), F(length) ])

        radius = p.radius
        phyllotaxy = self.phyllotaxy
        if p.leafy :
          if (not p.basestructure and p.gu_pheno_tts.stage < 4):
            if p.length_gu < 1e-3: return
            finalleaf = False
            pheno_stage       = p.gu_pheno_tts.stage
            pheno_rank        = p.gu_pheno_tts.rank_in_stage()
            gu_growth_ratio   = old_div(p.length_gu, p.final_length_gu)
            leaf_ttsum        = p.leaf_growth_tts.ttsum
            leaf_growth_ratio = old_div(self.leaf_growth_function(leaf_ttsum, p.final_length_leaves[0]),p.final_length_leaves[0])
            nsproduce([EndGC(), SetColor(self.stem_color), StartGC()])
          else:
            gu_growth_ratio   = 1
            finalleaf = True
            if textured:
                nsproduce([SetColor(self.textures_colorid+2),TextureVScale(0.02)])
            else:
                nsproduce([SetColor(self.oldwood_color)])
          i = 0
          posnorm = 1./float(p.nb_internodes-1)
          nsproduce([SetWidth(radius)])
          for ilength, flength in zip(p.final_length_internodes , p.final_length_leaves):
            nsproduce([F(ilength*gu_growth_ratio, radius), RollL(phyllotaxy)])
            if leafy:
              nsproduce([ SB() ])
              if finalleaf :   fLeaf(i*posnorm, radius, flength)
              else :           gLeaf(i*posnorm, radius, flength, leaf_growth_ratio, pheno_stage, pheno_rank, leaf_ttsum)
              nsproduce([ EB() ])
            i += 1
          if p.nbdescendants ==1 : nsproduce([Sphere(radius)])
        else:
          if textured:
            nsproduce([SetColor(self.textures_colorid+2),TextureVScale(0.02)])
          else:
            nsproduce([SetColor(self.oldwood_color)])
          nsproduce([SetWidth(radius)]+[F(l) for l in p.final_length_internodes]+[RollL(phyllotaxy*len(p.final_length_internodes))])
          if not textured: nsproduce([Sphere(radius)])





class InfloManager (OrganManager):

    def __init__(self, fruitmanager = None, **kwargs):
        OrganManager.__init__(self, **kwargs)

        self.t_ip_inflo   = 346.03/2.                          # Inflexion point of inflorescence growth curve
        self.base_temperature = 11.12              # Base temperature of inflorescences

        self.pheno_base_temp   = [11.10, 8.67,   15.11, 16, 16]  # base temperature for each phenological stage of inflorescence
        self.pheno_stade_temp  = [70.56, 133.32, 230.42, 352.72, 500]


        self.base_color_inflo   = 20
        self.pheno_color_inflo  =  [0,0,1,1,2,3]  # color for each stage (inflorescence)
        self.pheno_color_inflo  = [c+self.base_color_inflo for c in self.pheno_color_inflo ]
        self.base_color_flower  = 25
        self.pheno_color_flower =  list(range(self.base_color_flower,self.base_color_flower+6)) # [45,46,47,48,49,50]
        self.fruitcolor         = 32

        self.pheno_stadename = [ 'ABCD', 'E', 'F', 'G', 'S' ]
        self.bloompos = self.pheno_stadename.index('F')

        pheno_change_temp = np.cumsum (self.pheno_stade_temp)     # temperatures of inflorescence stage change
        self.mean_bloom_cum_temp    = pheno_change_temp[self.bloompos-1]+self.pheno_stade_temp[self.bloompos]/2.
        self.full_bloom_cum_temp    = pheno_change_temp[self.bloompos]

        # Inflorescences length
        # Mean and Std dev of length distribution.
        self.length_distrib       =  (23.15833, 6.767254)

        self.nbaxes_length_ratio = (0.9, 10.02)

        self.fruitmanager = fruitmanager

        self.__dict__.update(kwargs)

    def set_parameters(self, resolution, bract_axis, bract_section, bract_width):
        # Graphic Parameters
        self.resolution = resolution
        self.bract_axis = bract_axis
        self.bract_section = bract_section
        self.bract_width = bract_width
        self.bract_section.stride = 5

    def retrieve_parameters(self, namespace):
        self.set_parameters(namespace['RESOLUTION'],namespace['bract_axis'],namespace['bract_section'],namespace['bract_width'])

    def set_dimensions(self, params, current_date):
        final_length_inflo = get_realisation(self.length_distrib[0], self.length_distrib[1], 5, 44)
        nb_axes = int(self.nbaxes_length_ratio[0]*final_length_inflo + self.nbaxes_length_ratio[1])

        growth_tts   = ThermalTimeAccumulator(self.base_temperature)
        pheno_tts    = MultiPhaseThermalTimeAccumulator(self.pheno_base_temp, self.pheno_stade_temp, 0)

        # burst date should be computed from bloom date and pheno_tts reverse timing
        if params.hasattr('burst_date') and params.hasattr('bloom_date'):
            burst_date = params.burst_date
            bloom_date = params.bloom_date
            lpheno_tts    = MultiPhaseThermalTimeAccumulator(self.pheno_base_temp, self.pheno_stade_temp, self.mean_bloom_cum_temp)
            fullbloom_date = todatetime(lpheno_tts.find_date_of_accumulation(self.full_bloom_cum_temp, bloom_date, get_temperature))
        elif params.hasattr('burst_date'):
            fullbloom_date = todatetime(pheno_tts.find_date_of_accumulation(self.full_bloom_cum_temp, params.burst_date, get_temperature))
            bloom_date = todatetime(pheno_tts.find_date_of_accumulation(self.mean_bloom_cum_temp, params.burst_date, get_temperature))
            burst_date = params.burst_date
        elif params.hasattr('bloom_date'):
            pheno_tts  = MultiPhaseThermalTimeAccumulator(self.pheno_base_temp, self.pheno_stade_temp, self.mean_bloom_cum_temp)
            burst_date = todatetime(pheno_tts.reverse_from_finaldate(0, params.bloom_date, get_temperature))
            bloom_date = params.bloom_date
            fullbloom_date = todatetime(pheno_tts.find_date_of_accumulation(self.full_bloom_cum_temp, burst_date, get_temperature))

        for day in date_xrange(burst_date, current_date+timedelta(days=1)):
            daytemp = get_temperature(day)
            for tts in [growth_tts, pheno_tts]:
               tts.accumulate(daytemp)

        params.set(burst_date = burst_date,
                   bloom_date = bloom_date,
                   fullbloom_date = fullbloom_date,
                   cycle = get_flowering_cycle(fullbloom_date),

                   final_length = final_length_inflo,
                   length = 0.01,

                   nb_axes = nb_axes,

                   growth_tts = growth_tts,
                   pheno_tts  = pheno_tts,

                   fruiting = False)

        if self.fruitmanager :
            self.fruitmanager.init_fruiting_start_date(params, current_date)

        return params

    def second_order_length(self, param, mainlength, upos):
        final_second_order = param.final_length * 0.687 - 3.97
        ratio = old_div(final_second_order, param.final_length)
        return (mainlength * ratio)*(1-upos)

    # Function who return an organ (Leaf, Internode or GU) length
    def growth_function(self, T,  FinalSize):
        """ Sigmoid function used to compute growth of organs """
        B = 50.853318687577556 # 1. / 0.0196644
        return old_div(FinalSize,(1+exp(old_div(-(T-self.t_ip_inflo),B))))

    def pre_step_growth(self, p, daystep, current_temperatures):
        if daystep > 1:
            for ctemperature in current_temperatures:
                p.growth_tts.accumulate(ctemperature)
                p.pheno_tts.accumulate(ctemperature)
        else:
            p.growth_tts.accumulate(current_temperatures[-1], daystep)
            p.pheno_tts.accumulate(current_temperatures[-1], daystep)


        if p.pheno_tts.stage < 4 :
            p.length = self.growth_function(p.growth_tts.ttsum, p.final_length)

    def post_step_growth(self, p, current_date):
        Fruit = ModuleClass.get('Fruit')
        if p.fruiting == False :
            if p.nb_fruits > 0 and current_date >= p.fullbloom_date:
                p.fruiting = True
                for i in range(p.nb_fruits):
                    nsproduce([SB(),RollToVert(),Left((-1**i)*(old_div(120,p.nb_fruits))*i),Fruit(self.fruitmanager.set_dimensions(p, current_date)),EB()])

    def init_growth(self, p, current_date):
        for day in date_xrange(p.burst_date, current_date+timedelta(days=1)):
            daytemp = get_temperature(day)
            for tts in [p.growth_tts, p.pheno_tts]:
                tts.accumulate(daytemp)
        p.length = self.growth_function(p.growth_tts.ttsum, p.final_length)

    def init_plot(self):
        pass

    def radius(self, n_pheno):
        return 0.05+0.30*(old_div(n_pheno,5))

    def plot(self, p, current_date):
        import random
        rstate = random.getstate()

        def Inflorescence(param):
           def myratio(x):
              from math import exp
              return old_div((log(x+1)),(log(2)))

           length = param.length
           if length <= 1e-3: return

           NbAxe2 = param.nb_axes
           internode_length = old_div(length,NbAxe2)
           growth_ttsum = param.growth_tts.ttsum

           pheno_color_inflo = self.pheno_color_inflo
           pheno_color_flower = self.pheno_color_flower

           pheno_stage = param.pheno_tts.stage
           pheno_rank  = param.pheno_tts.rank_in_stage()
           n_pheno = pheno_stage + pheno_rank

           internode_radius = self.radius(n_pheno)

           colinterfunc = lambda x, coef : 1./(1+exp(-(x-(0.8-0.6*coef))/0.05))

           #nsproduce ([ SB(), StartScreenProjection(), SetColor(20), MoveTo(-0.8,0.8), Label(str((inflomanager.pheno_stadename[pheno_stage], n_pheno))), EB() ])

           nsproduce ([ EndGC(), Tropism(0,0,-1)])
           if ( pheno_stage >= 4):
              if current_date >= fruiting_cycle_end(param.cycle):
                return

              elasticity = 0.01
              if param.nb_fruits > 0 :
                 elasticity = 0.04
                 if current_date <= p.fruits_maturity_date :
                    elasticity += 0.04 * (n_pheno-4)
              nsproduce([ Elasticity(elasticity) ])
              if ( pheno_stage == 4):
                nsproduce([ InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , pheno_rank) ])
              else:
                nsproduce([SetColor(pheno_color_inflo[5])])
              nsproduce([SetWidth(internode_radius),nF(length,internode_length),RollR(param.get('totalphyloangles',0))])
           else:
              nsproduce([f(0.3)])
              if ( pheno_stage >= 3 ):
                targetelasticity = 0.005 if param.nb_fruits == 0 else 0.035
                elasticity = 0.005 + targetelasticity*pheno_rank
              else:
                elasticity = 0.005 * (n_pheno / 3.)
              nsproduce([ Elasticity(elasticity)])
              if self.resolution >= 1 :
                  nsproduce([ SetWidth(internode_radius)])
                  if self.resolution <= 1 : nsproduce ([ SectionResolution(5) ])

                  if not hasattr(param,'phyloangles'):
                     param.phyloangles = [ ((60 + randint(0,30)) if (i < NbAxe2*0.136 or i > NbAxe2*0.555) else 220) for i in range(NbAxe2) ]
                     param.totalphyloangles = sum(param.phyloangles)
                     param.activeaxes = [True for i in range(NbAxe2)]
                     param.nbactiveaxes = NbAxe2
                     param.flowersinflo = [[] for i in range(NbAxe2)]
                  elif pheno_stage == 3 and pheno_rank > 0.5:
                     nbactiveaxe = NbAxe2 - int(2*(pheno_rank-0.5)*NbAxe2)
                     while nbactiveaxe < param.nbactiveaxes:
                       toremove = randint(0,param.nbactiveaxes)
                       nbiter = 0
                       for i,v in enumerate(param.activeaxes):
                         if v:
                           nbiter += 1
                           if nbiter == toremove:
                             toremove = i
                             break
                       param.activeaxes[toremove] = False
                       param.nbactiveaxes -= 1
                  if pheno_stage < 1:
                    bract_level = max(0,(2*(pheno_rank-0.5)))
                  for k in range(0,NbAxe2):
                    pos = k/float(NbAxe2)
                    # On choisit la couleur en fonction du stade et de son avancement
                    cpos = pos if pheno_stage > 2 else (1-pos)
                    iradius = internode_radius * (2-pos)/2.
                    nsproduce([InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , colinterfunc(pheno_rank, cpos))])

                    nsproduce([F(internode_length,iradius),RollR(param.phyloangles[k])])
                    if param.activeaxes[k]:
                      axelength = self.second_order_length(param, length, pos)
                      nbwhorl = 6 + int(8 * (1-pos))
                      nsproduce([SB(),Down(90),f(iradius),Up(90),Down(5+40*(1 if pheno_stage >= 1 else pheno_rank)),Tropism(0,0,1),Elasticity(0.01)])
                      if pheno_stage < 1 and pos > bract_level:
                        bract_length = length*0.1+axelength*1.5
                        nsproduce([SB(),Down(0),StartGC(),Sweep(self.bract_axis,self.bract_section, bract_length,bract_length/10., bract_length/5., self.bract_width),EB()])
                      Pedicel(pos, length, axelength, nbwhorl, pheno_stage, pheno_rank, param.flowersinflo[k])
                      nsproduce([EB()])
              else:
                  nsproduce([InterpolateColors(pheno_color_flower[pheno_stage],pheno_color_flower[pheno_stage+1],pheno_rank),IncWidth(old_div(length,10)),F(length,0.1)])
           # produce ]


        def Pedicel(pos, inflolength, length, nbwhorl, pheno_stage, pheno_rank, flower_info):
              if length <= 1e-3: return
              flowering_index = 0
              npheno = pheno_stage+ pheno_rank
              nbflower = 3

              intlength = length/float(nbwhorl)
              latintlength = old_div(length, 20)
              intradius = old_div(self.radius(npheno),2)
              firstwhorl = 4

              ppos = 1 - pos

              pheno_color_inflo = self.pheno_color_inflo
              pheno_color_flower = self.pheno_color_flower

              # We use a logistic function
              sininterpolation = lambda x, coef : 1./(1+exp(-(x-(0.8-0.6*coef))/0.05))

              if self.resolution == 1 :
                  nsproduce([InterpolateColors(pheno_color_flower[pheno_stage],pheno_color_flower[pheno_stage+1],pheno_rank),IncWidth(old_div(length,10)),F(length,0.1)])
                  return

              nsproduce([SetWidth(intradius)])


              if flower_info == []:
                flower_info += [None for i in range(firstwhorl)]+[(randint(nbflower-1,nbflower+1), randint(-20,20), uniform(-0.1,0.1), uniform(0,1) < 0.2) for i in range(nbwhorl-firstwhorl-1)]+[(1,0,0, 1)]

              flowerradius = 0.05 + sininterpolation(npheno/3.,0.2) * 0.45

              for k in range(0,nbwhorl):
                  pos = k/float(nbwhorl)
                  iradius = intradius * (2-pos)/2.

                  kpos = 1 - (k-firstwhorl)/float(nbwhorl -firstwhorl -1)

                  # On choisi la couleur en fonction du stade et de son avancement
                  nsproduce([InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , sininterpolation(pheno_rank, 1 - ppos -ppos*kpos))])
                  #if pos < 0.7:
                  #  nproduce InterpolateColors(13, pheno_color_inflo[pheno_stage+1], npheno * kpos)
                  nsproduce([F(intlength, iradius)])

                  info = flower_info[k]

                  if not info is None:

                    nbflower, angdiv, deltapheno, withfruit = info
                    floangle = 360. / nbflower

                    if  pheno_stage <= 1:
                       colinterpolation = pheno_rank
                    else:
                      # A flowering probability that takes into account pheno,
                      # pos on primary axis and pos on secondary axis
                      #flowering_index = max(0,0.2 + (npheno-2) - (((age * pos) + (length * k/float(nbwhorl -1.))) / age))
                      colinterpolation = sininterpolation(pheno_rank,old_div((ppos+kpos),2))


                    for d in range (nbflower):
                        nsproduce([SB(),RollR(floangle*d + k*2 +angdiv),Down(90),f(iradius),Up(90),Down(75 if nbflower > 1 else 0),SetWidth(min(flowerradius, old_div(intradius,2))),F(latintlength * kpos )])
                        if not colinterpolation: colinterpolation = flowering_index
                        nsproduce([InterpolateColors(pheno_color_flower[pheno_stage], pheno_color_flower[pheno_stage+1], colinterpolation)])
                        Flower(iradius, flowerradius, pheno_stage, pheno_rank, withfruit)
                        nsproduce([EB()])
                  nsproduce([Sphere()])


        def Flower(iradius, radius, pheno_stage, pheno_rank, withfruit):
          if pheno_stage >= 2:
            nbpetal = 4
            petalangle = old_div(360, nbpetal)
            incl = 90 # 10 + 50 * (pheno_stage+pheno_rank -1)/2.
            nsproduce([Elasticity(0)])
            for i in range(nbpetal):
              nsproduce([SB(),RollL(i*petalangle),Down(incl)])
              for i in range(3) :
                nsproduce([Down(old_div(-incl,5)),Quad(old_div(radius,3), radius*0.7*(i+1)/3.)])
              nsproduce([EB()])
            if withfruit and pheno_stage > 2:
              sininterpolation = lambda x : 1./(1+exp(-(x-(0.2))/0.05))

              nsproduce([SetColor(self.fruitcolor),f(iradius),Sphere(old_div(radius,8)+(old_div(radius,2))*sininterpolation(pheno_rank))]) ; return
              pass
          else:
            # nproduce f(radius)
            nsproduce([Sphere(radius)]) ; return

        Inflorescence(p)
        random.setstate(rstate)

class FruitManager (OrganManager):

    def __init__(self, **kwargs):
        OrganManager.__init__(self, **kwargs)

        self.inflo_flush_start = {}

        self.pheno_base_temp   = [15.11]  # base temperature for each phenological stage of inflorescence
        self.pheno_stade_temp  = [352.72]
        # First phase correspond to cell multiplication then fruit maturation

        self.pheno_colors = [32,33]

        self.__dict__.update(kwargs)

    def set_parameters(self, profile, resolution, modelenabled, branchsize, outputenabled, outputname, parallelfruitmodel):
        # Graphic Parameters
        from openalea.plantgl.all import Revolution
        self.resolution = resolution
        self.profile = profile
        self.profile.ctrlPointList = list(reversed(self.profile.ctrlPointList))

        # Model parameters
        self.modelenabled = modelenabled
        self.branchsize = branchsize
        self.outputenabled = outputenabled
        self.outputname = outputname
        self.parallelfruitmodel = parallelfruitmodel
        self.Revolution = Revolution(profile, 8 if resolution < 2 else 30)
        #print 'Fruit Model Enabled :', modelenabled


    def generate_initial_masses(self):
        from random import normalvariate
        MS_Init = 0.97 * normalvariate(mu=13.9,sigma=4.1) + 0.03 * normalvariate(mu=29.2,sigma=0.66)
        MS_Init = max(0,MS_Init)
        MF_Init = 23.647 * (MS_Init ** 0.6182)
        return MF_Init, MS_Init

    def retrieve_parameters(self, namespace):
        self.set_parameters(namespace['fruitprofile'], namespace['RESOLUTION'], namespace['FRUIT_MODEL'], namespace['FRUITBRANCHSIZE'], namespace['FRUITMODEL_OUTPUT'], str(namespace['TREE'])+'-'+namespace['treename']+'-seed-'+str(namespace['SEED']),namespace['PARALLELFRUITMODEL'])

    def set_dimensions(self, infloparam, current_date):
        if self.modelenabled:
            return ParameterSet(inflo_fullbloom_date=infloparam.fullbloom_date,
                                growth=infloparam.fruits_growth,
                                maturity_date=infloparam.fruits_maturity_date,
                                weight = old_div(infloparam.fruits_weight, infloparam.nb_fruits),
                                growth_stage_date=infloparam.fruits_growth_stage_date,
                                initial_weight=infloparam.fruits_initial_weight)
        else:
            mass, drymass = self.generate_initial_masses()
            fruit_growth_tts   = ThermalTimeAccumulator(self.pheno_base_temp[0])
            growth_stage_date = todatetime(fruit_growth_tts.find_date_of_accumulation(self.pheno_stade_temp[0], infloparam.fullbloom_date))
            return ParameterSet(inflo_fullbloom_date=infloparam.fullbloom_date,
                                maturity_date=infloparam.fruits_maturity_date,
                                weight = old_div(infloparam.fruits_weight, infloparam.nb_fruits),
                                growth_stage_date=growth_stage_date,
                                initial_weight=mass)

    def applymodel(self, lstring, current_date):
        if self.modelenabled :
            import openalea.vmango.simulation.fruitmodel.fruitmodel as fm ; reload(fm)
            from openalea.vmango.simulation.fruitmodel.fruitmodel import applymodel
            from openalea.vmango.utilities.util_lstring2mtg import export_to_mtg_light
            cycle = get_flowering_cycle(current_date)
            #print('Fruit model evaluation', current_date, 'for cycle', cycle)
            lmtg = export_to_mtg_light(lstring, cycle) # , lscene)
            applymodel(lmtg, cycle, self.branchsize, self.outputenabled, self.outputname, self.parallelfruitmodel)
            #from openalea.vmango.utilities.util_tools import dump_obj
            #dump_obj(lmtg,'../simulation/fruitmodel/structure-cycle'+str(cycle)+'.pkl')
        else:
            pass
            #print 'No Fruit model evaluation'
        #self.reset_fruiting_start_date()

    def init_fruiting_start_date(self, infloparam, current_date):
        if infloparam.nb_fruits > 0:
            candidate = infloparam.fullbloom_date - timedelta(days=1)
            lcycle = candidate.year
            self.inflo_flush_start.setdefault(lcycle,None)
            if self.inflo_flush_start[lcycle] is None or candidate < self.inflo_flush_start[lcycle]:
                self.inflo_flush_start[lcycle] = candidate
                #print 'Set inflo_flush_start ',lcycle,' to', candidate, current_date
            if self.modelenabled and self.inflo_flush_start[lcycle] < current_date:
                raise ValueError('Reduce timestep. Cannot process the flush', self.inflo_flush_start[lcycle], current_date, infloparam.gu_burst_date)
            #elif infloparam.burst_date > self.inflo_flush_start:
            #    raise ValueError('The flush will not process all inflos', self.inflo_flush_start, infloparam.burst_date)

    def has_fruiting_in_period(self, begin_period, end_period):
        for year in range(begin_period.year, end_period.year+1):
            if year in self.inflo_flush_start and begin_period < self.get_fruiting_start_date(year) <= end_period:
                return True
        return False

    def get_first_fruiting_start_date(self, begin_period, end_period):
        for year in range(begin_period.year, end_period.year+1):
            if year in self.inflo_flush_start:
                fdate = self.get_fruiting_start_date(year)
                if begin_period < fdate <= end_period:
                    return fdate
        return None

    def is_fruiting_start_date(self, date):
        return self.inflo_flush_start.get(date.year) == date


    def get_fruiting_start_date(self, year):
        return self.inflo_flush_start[year]

    def init_plot(self):
        pass

    def fruit_dimensions(self, weight):
        sizefactor = 1
        ep = (0.98*pow(weight,0.3398))*sizefactor
        larg = (1.25*pow(weight,0.3203))*sizefactor
        long_ = (2.23*pow(weight,0.2896))*sizefactor
        return ep, larg, long_

    def plot(self, fruitparam, current_date):
        first_date = fruitparam.inflo_fullbloom_date
        stage = 1
        if first_date < current_date <= fruitparam.maturity_date+timedelta(days=30):
            if current_date < fruitparam.growth_stage_date:
                stage = 1
                weight = fruitparam.initial_weight
                growthindex = (current_date - first_date).days/float((fruitparam.growth_stage_date-first_date).days)
                #weight *= growthindex
                #sininterpolation = lambda x : 1./(1+exp(-(x-(0.5))/0.15))
                dims = self.fruit_dimensions(weight)
                mn = np.mean(dims)
                mn *= growthindex
                ratios = [old_div(d,mn) for d in dims]
                cratios = [1+(r-1)*growthindex for r in ratios]
                ep, larg, int_ = [mn*r for r in cratios]
            elif current_date >= fruitparam.maturity_date:
                stage = 3
                weight = fruitparam.weight
                ep, larg, int_ = self.fruit_dimensions(weight)
            else:
                stage = 2
                if fruitparam.hasattr('growth'):
                    weight = fruitparam.growth[current_date][0]
                    ep, larg, int_ = self.fruit_dimensions(weight)
                else:
                    growthindex = (current_date - fruitparam.growth_stage_date).days/float((fruitparam.maturity_date - fruitparam.growth_stage_date).days)
                    weight = fruitparam.initial_weight + (fruitparam.weight-fruitparam.initial_weight)* growthindex
                    ep, larg, int_ = self.fruit_dimensions(weight)

            if fruitparam.growth_stage_date == first_date: print(fruitparam)
            phenoindex = (current_date - first_date).days/float((fruitparam.growth_stage_date-first_date).days)
            from openalea.plantgl.all import Scaled, Revolution
            nsproduce([SB(),RollToVert(),F(5*min(1,phenoindex)),RollToHorizontal()])
            if  current_date > fruitparam.maturity_date:
                nsproduce([MoveTo(None,None,old_div(ep,2)),])
            else:
                nsproduce([Down(90)])
            if stage == 1:
                nsproduce([SetColor(self.pheno_colors[0])])
            elif stage == 2:
                colphenoindex = (current_date - fruitparam.growth_stage_date).days/float((fruitparam.maturity_date-fruitparam.growth_stage_date).days)
                nsproduce([InterpolateColors(self.pheno_colors[0],self.pheno_colors[1],colphenoindex)])
            elif stage == 3:
                nsproduce([SetColor(self.pheno_colors[1])])
            nsproduce([PglShape(Scaled(ep,larg,int_, self.Revolution)),EB()])
