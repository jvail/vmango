from vplants.mangosim.temperature import *
from random import *
from vplants.mangosim.state import *
from openalea.plantgl.all import QuantisedFunction, NurbsCurve2D, Point3Array
import numpy as np
from vplants.mangosim.temperature import get_temperature
from vplants.mangosim.thermaltime import *
from vplants.mangosim.util_date import *
from math import exp
from openalea.lpy import *


def get_realisation(mean, sd, minval, maxval, rfunc = gauss):
    val = rfunc(mean, sd)
    while (val < minval) or (val > maxval):
        val = rfunc(mean, sd)
    return val

class OrganManager:
    def __init__(self, **kwargs):
        self.phyllotaxy = 144  # +randint(-2,2)          # mango phyllotaxie
        pass

class GUManager (OrganManager):
    def __init__(self,  **kwargs):
      OrganManager.__init__(self, **kwargs)
      
      # Initializing variables
      self.t_ip_gu_mean = 79.40
      self.t_ip_gu_sd   = 10.75
      self.t_ip_leaf    = 182.04/2.                           # Inflexion point of leaf growth curve
      
      self.base_temperature_gu    = 9.2                # Base temperature of GUs
      self.base_temperature_leaf  = 10.73              # Base temperature of Leafs
      
      self.final_diameter = 0.2                           # diameter of GUs (at the end of growth)
      
      # Phenological stages definition
      self.pheno_base_temp   = [13.37766, 13.37766, 13.37766, 9.784431] #,0]   # stages DEF_G_H
      self.pheno_stade_temp  = [38.50,    47.61,    47.39,    316.376] #,999]   # temperatures of GU's stage change
      self.pheno_change_temp = np.cumsum(self.pheno_stade_temp)
      self.pheno_color = [7,10,11,14,13]                         # color for each stage (GU)
      self.pheno_angle_values = [0,90,165,60,60]                        # angle between leaf and internode for each stage
      self.nb_stades_pheno = len(self.pheno_change_temp)              # number of phenological stages
      
      self.pheno_angle = QuantisedFunction(NurbsCurve2D(Point3Array(list(enumerate(self.pheno_angle_values)),1), degree = 1 ) )
      
      self.pheno_stadename =    {0: 'ABCD', 1 : 'E', 2 : 'F', 3 : 'G', 4: 'H' }
      

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

      self.radius_exponent = 0.45
      self.radius_coefficient = 0.3

      self.branching_angle = 60 

      self.__dict__.update(kwargs)

    def set_parameters(self, leafaxis, leafsection, leafdiam, leafwidth, petioleCurve, resolution):
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
          from leafgeometry import SymbolManager

          self.leafdiam = leafdiam
          self.leafwidth = leafwidth
          self.petioleCurve = petioleCurve
          self.leafSymbol = SymbolManager(leafaxis,[0,0.1,0.5,0.6,0.7,0.8], 3, leafsection, 1., 1./self.LeafLengthRes, self.leaf_width_length_ratio, leafdiam)

    def retrieve_parameters(self, namespace):
        from leafgeometry import retrieveCurves
        curves = retrieveCurves(namespace)
        self.set_parameters(curves, namespace['leafsection'], namespace['leafdiam'], namespace['leafwidth'], namespace['petioleCurve'], namespace['RESOLUTION'])

    def step(self, daystep):
        pass
    
    def internode_length_distribution(self, nb_internodes, gu_length):
      """ Internode length distribution """
      lengths = [exp(-2.64 * i / float(nb_internodes-1)) for i in xrange(nb_internodes)]
      scaling = gu_length / sum(lengths)
      return [l*scaling for l in lengths]
    
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
      final_length_leaves = [get_realisation(mean, sd, 5, 34) * self.leaflength(i/float(nb_internodes-1)) for i in xrange(nb_internodes)]
      
      LEPF = self.length_before_first_leaf(position, final_length_gu)
      final_length_internodes = [LEPF] + self.internode_length_distribution(nb_internodes-1, final_length_gu)
      
      t_ip= gauss(self.t_ip_gu_mean,self.t_ip_gu_sd)
      
      params = params.copy()
      params.set(radius = self.estimate_radius(params.nbdescendants), 
                 length_gu               =  0,
                 
                 final_length_gu         = final_length_gu,
                 final_length_leaves     = final_length_leaves,
                 final_length_internodes = final_length_internodes,
                 
                 nb_internodes    = nb_internodes,
                 
                 t_ip             = t_ip)
      
      if params.burst_date:          
          delta_base_temp  = gauss(0,3) if temperature_variability else 0
          gu_growth_tts    = ThermalTimeAccumulator(self.base_temperature_gu - delta_base_temp)
          leaf_growth_tts  = ThermalTimeAccumulator(self.base_temperature_leaf - delta_base_temp)
          pheno_base_temp  = self.pheno_base_temp if not temperature_variability else [t - delta_base_temp  for t in self.pheno_base_temp]
          gu_pheno_tts     = MultiPhaseThermalTimeAccumulator(pheno_base_temp, self.pheno_change_temp)
          
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
        return FinalSize/(1+exp(-(T-t_ip)/B))        # sigmoid equation
        
    def leaf_growth_function(self, T,  FinalSize):
        maxGR = -0.0188725+0.0147985*FinalSize
        B = FinalSize/(4*maxGR)
        return FinalSize/(1+exp(-(T-self.t_ip_leaf)/B))
        
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

    def init_plot(self):
        execContext().turtle.setSurface('finalleaf', self.leafSymbol())

    def plot(self, p, textured = True, leafy = True):

        def fLeaf(position, size):
              #nsproduce([ SetColor(13), Down(self.pheno_angle(4)), surface('finalleaf', size)])
              #return
              nsproduce([ EndGC(),Elasticity(0.),Down(90), SetColor(1),F(0.001,self.leafdiam(0)*self.leafwidth(1)), Up(90)])  
              # petiole
              nsproduce([ Down(self.pheno_angle(4)) ])
              Petiole((1.1-position)* size/4.,self.leafdiam(0))
              nsproduce([ RollToVert() ])
              if textured:
                nsproduce([ TextureBaseColor(13), SetColor(31), TextureVScale(1) ])
              else:
                nsproduce([ SetColor(13) ])
              nsproduce([ surface('finalleaf', size) ])
              #nsproduce([ PglShape(self.leafSymbol(), size) ])

        def gLeaf(position, final_length, growth_ratio, pheno_stage, pheno_rank, ttsum):
              petioleradius  = self.leafdiam(0)*self.leafwidth(min(growth_ratio,1))
              nsproduce([ EndGC(), Elasticity(0.), Down(90), SetColor(1), F(0.001, petioleradius), Up(90) ]) 
              # Angle depends of phenological stage and advancement
              nsproduce([ Down(self.pheno_angle(pheno_stage+pheno_rank)) ])
              # petiole
              petiolelength = max(0.01,(1-position)* final_length/4.)
              Petiole(petiolelength, petioleradius)
              nsproduce([ RollToVert() ])
              # Color depends of phenological stage and advancement
              pheno_color = self.pheno_color
              if textured:
                if pheno_rank > 0:
                  nsproduce([ InterpolateTextureBaseColors(pheno_color[pheno_stage],pheno_color[pheno_stage+1],pheno_rank) ])
                else: 
                  nsproduce([ TextureBaseColor(pheno_color[pheno_stage]) ])
                nsproduce([ TextureVScale(1), SetColor(31) ])
              else:
                if pheno_rank > 0:
                  nsproduce([ InterpolateColors(pheno_color[pheno_stage],pheno_color[pheno_stage+1],pheno_rank) ])
                else: 
                  nsproduce([ SetColor(pheno_color[pheno_stage]) ])
                
              clength = final_length * growth_ratio
              nsproduce([ PglShape(self.leafSymbol(pheno_stage+pheno_rank), clength) ])

        def Petiole(length,radius):
              if self.resolution > 0:
                nsproduce([ SetGuide(self.petioleCurve,length), nF(length, length/self.PetioleRes,radius) ])
              else:
                nsproduce([ SetGuide(self.petioleCurve,length), F(length) ])


        radius = p.radius
        phyllotaxy = self.phyllotaxy
        if p.leafy :
          if (not p.basestructure and p.gu_pheno_tts.stage < 4):
            if p.length_gu < 1e-3: return
            finalleaf = False
            pheno_stage       = p.gu_pheno_tts.stage
            pheno_rank        = p.gu_pheno_tts.rank_in_stage()
            gu_growth_ratio   = p.length_gu / p.final_length_gu
            leaf_ttsum        = p.leaf_growth_tts.ttsum
            leaf_growth_ratio = self.leaf_growth_function(leaf_ttsum, p.final_length_leaves[0])/p.final_length_leaves[0]
            nsproduce([EndGC(), SetColor(1), StartGC()])
          else: 
            gu_growth_ratio   = 1
            finalleaf = True
          i = 0
          posnorm = 1./float(p.nb_internodes-1)
          nsproduce([SetWidth(radius)])
          for ilength, flength in zip(p.final_length_internodes , p.final_length_leaves):  
            nsproduce([F(ilength*gu_growth_ratio, radius), RollL(phyllotaxy)]) 
            if leafy:
              nsproduce([ SB() ])
              if finalleaf :   fLeaf(i*posnorm, flength)
              else :           gLeaf(i*posnorm, flength, leaf_growth_ratio, pheno_stage, pheno_rank, leaf_ttsum)
              nsproduce([ EB() ])
            i += 1
        else:
          if textured:
            nsproduce([SetColor(32),TextureVScale(0.02)])
          else:
            nsproduce([SetColor(21)])
          nsproduce([SetWidth(radius),nF(p.final_length_gu, p.final_length_gu/self.InternodeRes , radius)])





class InfloManager (OrganManager):

    def __init__(self, **kwargs):
      OrganManager.__init__(self, **kwargs)

      self.t_ip_inflo   = 346.03/2.                          # Inflexion point of inflorescence growth curve
      self.base_temperature = 11.12              # Base temperature of inflorescences

      self.pheno_base_temp   = [11.10, 5.38,   8.67,   15.11, 16]  # base temperature for each phenological stage of inflorescence
      self.pheno_stade_temp  = [70.56, 133.32, 230.42, 352.72, 500]
      
      self.pheno_change_temp = np.cumsum (self.pheno_stade_temp)     # temperatures of inflorescence stage change

      self.pheno_color_inflo  =  [13, 13, 16, 16, 23, 20]                  # color for each stage (inflorescence)
      self.pheno_color_flower = [13,2,7,17,10,20]

      self.pheno_stadename = {0: 'ABCD', 1 : 'E', 2 : 'F', 3 : 'PF', 4 :'G', 5: 'S' }

      # Inflorescences length
      # Mean and Std dev of length distribution.       
      self.length_distrib       =  (23.15833, 6.767254)

      self.nbaxes_length_ratio = 1.19

      self.__dict__.update(kwargs)

    def set_parameters(self, resolution):
          # Graphic Parameters
          self.resolution = resolution

    def retrieve_parameters(self, namespace):
        self.set_parameters(namespace['RESOLUTION'])

    def set_dimensions(self, params, current_date):
      final_length_inflo = get_realisation(self.length_distrib[0], self.length_distrib[1], 5, 44)
      nb_axes = int(self.nbaxes_length_ratio*final_length_inflo)
      
      growth_tts   = ThermalTimeAccumulator(self.base_temperature)
      pheno_tts    = MultiPhaseThermalTimeAccumulator(self.pheno_base_temp, self.pheno_change_temp, 0)
      
      # burst date should be computed from bloom date and pheno_tts reverse timing
      if params.hasattr('burst_date') and params.hasattr('bloom_date'):
        burst_date = params.burst_date
        bloom_date = params.bloom_date      
      elif params.hasattr('burst_date'):
        bloom_date = todatetime(pheno_tts.find_date_of_accumulation(350, params.burst_date, get_temperature))
        burst_date = params.burst_date
      elif params.hasattr('bloom_date'):
        pheno_tts  = MultiPhaseThermalTimeAccumulator(self.pheno_base_temp, self.pheno_change_temp, 350)
        burst_date = todatetime(pheno_tts.reverse_from_finaldate(0, params.bloom_date, get_temperature))
        bloom_date = params.bloom_date
      fruiting_date = todatetime(growth_tts.find_date_of_accumulation(800, burst_date, get_temperature)) if params.nb_fruits > 0 else None
      # burst_date = bloom_date - timedelta(days=28)

      for day in date_xrange(burst_date, current_date+timedelta(days=1)):
          daytemp = get_temperature(day)
          for tts in [growth_tts, pheno_tts]:
               tts.accumulate(daytemp)  
      
      params.set(burst_date = burst_date,
                 bloom_date = bloom_date,
                 fruiting_date = fruiting_date,
                 
                 final_length = final_length_inflo,
                 length = 0.01,

                 nb_axes = nb_axes,
                 
                 growth_tts = growth_tts,
                 pheno_tts  = pheno_tts)
      
      return params

    def second_order_length(self, param, mainlength, upos):
        final_second_order = param.final_length * 0.687 - 3.97
        ratio = final_second_order / param.final_length
        return (mainlength * ratio)*(1-upos)

    # Function who return an organ (Leaf, Internode or GU) length 
    def growth_function(self, T,  FinalSize):
        """ Sigmoid function used to compute growth of organs """
        B = 50.853318687577556 # 1. / 0.0196644
        return FinalSize/(1+exp(-(T-self.t_ip_inflo)/B))

    def step_growth(self, p, daystep, current_temperatures):
        if daystep > 1:
            for ctemperature in current_temperatures:
                p.growth_tts.accumulate(ctemperature)
                p.pheno_tts.accumulate(ctemperature)
        else:
            p.growth_tts.accumulate(current_temperatures[-1], daystep)
            p.pheno_tts.accumulate(current_temperatures[-1], daystep)
        
      
        if p.pheno_tts.stage < 4 :
            p.length = self.growth_function(p.growth_tts.ttsum, p.final_length)

    def init_growth(self, p, current_date):
        for day in date_xrange(p.burst_date, current_date+timedelta(days=1)):
            daytemp = get_temperature(day)
            for tts in [p.growth_tts, p.pheno_tts]:
                tts.accumulate(daytemp)
        p.length = self.growth_function(p.growth_tts.ttsum, p.final_length)

    def init_plot(self):
        pass


    def plot(self, p, current_date):

        def Inflorescence(param):  
           def myratio(x):
              from math import exp
              return (log(x+1))/(log(2))
              
           length = param.length
           if length <= 1e-3: return
           
           NbAxe2 = param.nb_axes
           internode_length = length/NbAxe2
           internode_radius = internode_length / 5.
           growth_ttsum = param.growth_tts.ttsum
           
           pheno_color_inflo = self.pheno_color_inflo
           pheno_color_flower = self.pheno_color_flower
           
           pheno_stage = param.pheno_tts.stage
           pheno_rank  = param.pheno_tts.rank_in_stage()
           n_pheno = pheno_stage + pheno_rank
          
           colinterfunc = lambda x, coef : 1./(1+exp(-(x-(0.8-0.6*coef))/0.05))
           
           #nsproduce ([ SB(), StartScreenProjection(), SetColor(20), MoveTo(-0.8,0.8), Label(str((inflomanager.pheno_stadename[pheno_stage], n_pheno))), EB() ])
           
           nsproduce ([ EndGC(), Tropism(0,0,-1) ])
           if ( pheno_stage >= 4):
              elasticity = 0.01
              if param.nb_fruits > 0 and param.hasattr('fruit_maturity_date') and current_date <= param.fruit_maturity_date:
                elasticity += 0.02 * (n_pheno-4.)
              nsproduce([ Elasticity(elasticity) ])
              if ( pheno_stage == 4):  
                nsproduce([ InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , pheno_rank) ])
              else:
                nsproduce([SetColor(pheno_color_inflo[5])])
              nsproduce([IncWidth(internode_length/5.),nF(length,internode_length)])
           else:
              nsproduce([ Elasticity(0.01 * (n_pheno / 4.) )])
              if self.resolution >= 1 : 
                  nsproduce([ SetWidth(internode_radius)]) 
                  if self.resolution <= 1 : nsproduce ([ SectionResolution(5) ])
                  
                  if not hasattr(param,'phyloangles'):
                     param.phyloangles = [ ((60 + randint(0,30)) if (i < 8 or i > 19) else 220) for i in xrange(NbAxe2) ]
                     param.activeaxes = [True for i in xrange(NbAxe2)]
                     param.nbactiveaxes = NbAxe2
                     param.flowersinflo = [[] for i in xrange(NbAxe2)]
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
                  for k in xrange(0,NbAxe2):
                    pos = k/float(NbAxe2)
                    # On choisit la couleur en fonction du stade et de son avancement
                    cpos = pos if pheno_stage > 2 else (1-pos)
                    nsproduce([InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , colinterfunc(pheno_rank, cpos))])
                     
                    nsproduce([F(internode_length),RollR(param.phyloangles[k])])
                    if param.activeaxes[k]:
                      axelength = self.second_order_length(param, length, pos)
                      nbwhorl = 6 + int(8 * (1-pos))
                      nsproduce([SB(),Left(5+40*(1 if pheno_stage >= 1 else pheno_rank)),Tropism(0,0,1),Elasticity(0.01)]),
                      Pedicel(pos, length, axelength, nbwhorl, pheno_stage, pheno_rank, param.flowersinflo[k])
                      nsproduce([EB()]) 
              else:
                  nsproduce([InterpolateColors(pheno_color_flower[pheno_stage],pheno_color_flower[pheno_stage+1],pheno_rank),IncWidth(length/10),F(length,0.1)])
           # produce ]


        def Pedicel(pos, inflolength, length, nbwhorl, pheno_stage, pheno_rank, flower_info):
              if length <= 1e-3: return
              flowering_index = 0
              npheno = pheno_stage+ pheno_rank
              nbflower = 3
              
              intlength = length/float(nbwhorl)
              latintlength = length / 20
              intradius = intlength / 10
              firstwhorl = 4
              
              ppos = 1 - pos
              
              pheno_color_inflo = self.pheno_color_inflo
              pheno_color_flower = self.pheno_color_flower
              
              # We use a logistic function
              sininterpolation = lambda x, coef : 1./(1+exp(-(x-(0.8-0.6*coef))/0.05))
              
              nsproduce([SetWidth(intradius)])
              
              if flower_info == []:
                flower_info += [None for i in xrange(firstwhorl)]+[(randint(nbflower-1,nbflower+1), randint(-20,20), uniform(-0.1,0.1), uniform(0,1) < 0.2) for i in xrange(nbwhorl-firstwhorl-1)]+[(1,0,0, 1)]
                
              flowerradius = 0.1 + sininterpolation(npheno/3.,0.2) * 0.4
              
              for k in xrange(0,nbwhorl):
                  kpos = 1 - (k-firstwhorl)/float(nbwhorl -firstwhorl -1)
                  
                  # On choisi la couleur en fonction du stade et de son avancement
                  nsproduce([InterpolateColors(pheno_color_inflo[pheno_stage], pheno_color_inflo[pheno_stage+1] , sininterpolation(pheno_rank, 1 - ppos -ppos*kpos))]) 
                  #if pos < 0.7: 
                  #  nproduce InterpolateColors(13, pheno_color_inflo[pheno_stage+1], npheno * kpos) 
                  nsproduce([F(intlength)])
                  
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
                      colinterpolation = sininterpolation(pheno_rank,(ppos+kpos)/2) 
                      
                    
                    for d in xrange (nbflower):
                        nsproduce([SB(),RollR(floangle*d + k*2 +angdiv),Down(75 if nbflower > 1 else 0),F(latintlength * kpos, intradius/2 )])
                        if not colinterpolation: colinterpolation = flowering_index
                        nsproduce([InterpolateColors(pheno_color_flower[pheno_stage], pheno_color_flower[pheno_stage+1], colinterpolation)]) 
                        Flower(flowerradius, pheno_stage, pheno_rank, withfruit)
                        nsproduce([EB()]) 


        def Flower(radius, pheno_stage, pheno_rank, withfruit):
          if pheno_stage >= 2:
            nbpetal = 4 
            petalangle = 360 / nbpetal
            incl = 90 # 10 + 50 * (pheno_stage+pheno_rank -1)/2.
            nsproduce([Elasticity(0)]) 
            for i in xrange(nbpetal):
              nsproduce([SB(),RollL(i*petalangle),Down(incl)])  
              for i in xrange(3) :
                nsproduce([Down(-incl/5),Quad(radius/3, radius*0.7*(i+1)/3.)])  
              nsproduce([EB()]) 
            if withfruit and pheno_stage > 2:
              sininterpolation = lambda x : 1./(1+exp(-(x-(0.5))/0.05))
              
              nsproduce([SetColor(13),f(radius/2),Sphere(radius/8+(radius/2)*sininterpolation(pheno_rank))]) ; return
              pass
          else:
            # nproduce f(radius) 
            nsproduce([Sphere(radius)]) ; return

        Inflorescence(p)


class FruitManager (OrganManager):

    def __init__(self, **kwargs):
        OrganManager.__init__(self, **kwargs)

        self.inflo_flush_start = None

        self.__dict__.update(kwargs)

    def set_parameters(self, profile, resolution, modelenabled, branchsize, outputenabled, outputname):
        # Graphic Parameters
        self.resolution = resolution
        self.profile = profile

        # Model parameters
        self.modelenabled = modelenabled
        self.branchsize = branchsize
        self.outputenabled = outputenabled
        self.outputname = outputname
        print 'Fruit Model Enabled :', modelenabled

    def retrieve_parameters(self, namespace):
        self.set_parameters(namespace['fruitprofile'], namespace['RESOLUTION'], namespace['FRUIT_MODEL'], namespace['FRUITBRANCHSIZE'], namespace['FRUITMODEL_OUTPUT'], str(namespace['TREE'])+'-'+namespace['treename']+'-seed-'+str(namespace['SEED']))
    
    def set_dimensions(self, infloparam, params, current_date):
        if not self.modelenabled:
            ParameterSet(growth=p.fruit_growth, 
                         maturity_date=p.fruit_maturity_date,
                         appearance_date=p.fruit_appearance_date,
                         weight_min=p.fruit_weight_min)
    def applymodel(self, lstring, lscene):
        if self.modelenabled :
            import vplants.mangosim.fruitmodel.fruitmodel as fm ; reload(fm)
            from vplants.mangosim.fruitmodel.fruitmodel import applymodel
            from vplants.mangosim.util_lstring2mtg import export_to_mtg_light
            print 'Fruit model evaluation'
            lmtg = export_to_mtg_light(lstring, None) # , lscene)
            applymodel(lmtg, get_flowering_cycle(self.inflo_flush_start), self.branchsize, self.outputenabled, self.outputname)
        else:
            print 'No Fruit model evaluation'
        self.reset_fruiting_start_date()

    def init_fruiting_start_date(self, burst_date):
        if self.inflo_flush_start is None: 
            self.inflo_flush_start = burst_date

    def is_fruiting_started(self, current_date):
        return self.inflo_flush_start and current_date >= self.get_fruiting_start_date()

    def get_fruiting_start_date(self):
        return self.inflo_flush_start + timedelta(days=50)

    def reset_fruiting_start_date(self):
        self.inflo_flush_start = None

    def init_plot(self):
        pass

    def plot(self, infloparam, fruitparam, current_date):
        first_date = infloparam.fruiting_date
        if first_date < current_date <= fruitparam.maturity_date:
            from openalea.plantgl.all import Scaled, Revolution
            if current_date < fruitparam.appearance_date:
                MF = fruitparam.growth[fruitparam.appearance_date][0] 
                MF *= (current_date - first_date).days/float((fruitparam.appearance_date-first_date).days)
            elif current_date >= fruitparam.maturity_date:
                MF = fruitparam.growth[fruitparam.maturity_date][0]
            else:
                MF = fruitparam.growth[current_date][0]
            sizefactor = 0.06
            ep = (9.8*pow(MF,0.3398))*sizefactor
            larg = (12.5*pow(MF,0.3203))*sizefactor
            long = (22.3*pow(MF,0.2896))*sizefactor   # facteur 0.06 choisit arbitrairement`
            phenoindex = (current_date - first_date).days/float((fruitparam.maturity_date-first_date).days)
            nsproduce([SB(),InterpolateColors(2,6,phenoindex),PglShape(Scaled(ep,larg,long, Revolution(self.profile, 8 if self.resolution < 2 else 30))),EB()])   
