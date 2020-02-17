from __future__ import division
from __future__ import print_function
from __future__ import unicode_literals

from builtins import zip
from past.utils import old_div
from openalea.plantgl.all import Viewer,eStatic,eAnimatedScene, eAnimatedPrimitives
from openalea.fractalysis.light.directLight import directionalInterception
from openalea.fractalysis.light.sunDome import getSkyTurtleSize, skyTurtle, getSkyTurtleAt, getSkyTurtleDir, weights, getDirectLight, plotDirect
#from openalea.fractalysis.light.sunPositions import positionSoleil
import datetime
import pandas as pd

# rappel: exemple pour afficher deux colonnes:
# Radiation_use.ix[:,["Rayonnement""Rayonnement diffus"]]

Meteo_data = None # crée une variable globale
#lightfile = 'rayostpierre2002.csv'
DIF = 0.2233 # coef pour passer de RG au rayonnement diffus
DIRN = 0.8896  # coef pour passer de RG au rayonnement direct

def Initialize_Meteo_data(lightfile = 'rayostpierre2002.csv'):
    # recuperer infos des fichiers env lumineux
    # day: format "jj/mm/yyyy"
    global Meteo_data
    from openalea.vmango.utilities.util_path import share_dir, join
    from pandas import read_csv
    import pandas as pd
    Radiation = read_csv(join(share_dir, 'environment', lightfile), delimiter=';')
    Radiation["Date"] = pd.to_datetime(Radiation["Date"])
    Radiation["DATE"] = pd.to_datetime(Radiation["DATE"]) # permet de lire les dates
    #Y = Radiation["Date"].dt.year
    Meteo_data = Radiation[Radiation.Rayonnement != 0] # enlève toutes les heures où le rayonnement est nul

    
def get_day_total_light(day):
    return Meteo_data[Meteo_data.DATE == day] # sélectionne le tableau pour un jour donné

def get_day_meteo(day):
    day_radiation = get_day_total_light(day) 
    day_radiation.loc[:,"Rayonnement diffus"] =  day_radiation["Rayonnement"] * DIF
    day_radiation.loc[:,"Rayonnement direct"] =  day_radiation["Rayonnement"] * DIRN
    sum_diffuse_radiation_per_day = sum(day_radiation["Rayonnement diffus"]) # totalité du rayonnement diffus dans une journée
    return list(zip(day_radiation.HEURE, day_radiation["Rayonnement direct"])), sum_diffuse_radiation_per_day

def get_day_diffuse_light(day):
    day_radiation = get_day_total_light(day) 
    day_radiation.loc[:,"Rayonnement diffus"] =  day_radiation["Rayonnement"] * DIF
    return sum(day_radiation["Rayonnement diffus"]) # totalité du rayonnement diffus dans une journée

def get_day_direct_light(day):
    day_radiation = get_day_total_light(day) 
    day_radiation.loc[:,"Rayonnement direct"] =  day_radiation["Rayonnement"] * DIRN
    return list(zip(day_radiation.HEURE, day_radiation["Rayonnement direct"]))

def convert_jourJUL(day):
    # transforme la date en jours julien (nième jour de l'année)
    import datetime
    date = datetime.datetime.strptime(day, "%d/%m/%Y").date() # convertit la date en format date
    d = date.day # récupère le jour de la date
    m = date.month
    y = date.year
    if ((y%4==0 and y%100!=0) or y%400==0):  # bissextile?
        return (0,31,60,91,121,152,182,213,244,274,305,335,366)[m-1] + d
    else:
        return (0,31,59,90,120,151,181,212,243,273,304,334,365)[m-1] + d

# dissocier lumiere directe vs diffuse
                # voir modèle fruit de Mathieu et relations de Maxwell
def get_diffus_light(mtg, scene, day):
    # pour chacune des directions du ciel:
    weights = [w for a,e,w in skyTurtle()] # extrait les valeurs de weigths dans skyTurtle
    sumweigths = sum(weights)
    nskyturtle = [(a,e,old_div(w,sumweigths)) for a,e,w in skyTurtle()] # recrée un tableau skyTurtle avec les nouvelles valeurs de weights
    diffus = get_day_diffuse_light(day) # calcul le rayonnement diffus pour un jour donné
    Interception_diffus = directionalInterception(scene, nskyturtle) # donne une surface par élément qui reçoit la lumière (ou une quantité d'énergie) 
    Diffus_light = dict([(uc_id, interception * diffus) for uc_id, interception in list(Interception_diffus.items())]) # crée un nouveau dictionnaire en multipliant la surface par le rayonnement diffus pour chaque élément de l'arbre
    return Diffus_light         

def get_direct_light(mtg, day):
    directlight = get_day_direct_light(day)    
    direct_w = [w for h, w in directlight]  # le poids est égal au rayonnement direct                                                                                                # somme des surfaces percues par feuille avec coef lié a l'importance de chaque angle.
    print(len(direct_w))
    start = min([h for h,par in directlight]) # première heure avec du rayonnement
    stop = max([h for h,par in directlight])
    print(start, stop)
    # calculer course du soleil
    jourJul = convert_jourJUL(day)
    longitude = 55.50
    decalGMT = +4
    decalSun = 0

    import openalea.fractalysis.light.sunPositions as sp

    seq = sp.Sequence()
    print(seq.heureTSV(jourJul, start, decalSun, decalGMT, longitude))
    print(seq.heureTSV(jourJul, stop, decalSun, decalGMT, longitude))
    ddl = getDirectLight( latitude = -21.32, longitude = 55.50, jourJul = convert_jourJUL(day), startH = start , stopH = stop, step= 60, decalSun = decalSun, decalGMT = +4) # donne les positions du soleil
    print(len(ddl))
    print(ddl)
    #ndir =[(ddl[i][0],ddl[i][1],direct_w[i]) for i in xrange(len(ddl))]
    #print ndir

#def photosynthesis(mtg, scene, day, lightfile = 'rayostpierre2002.csv'):
    
      
    
    
    
    
    #Interception_direct = directionalInterception(scene, ddl)
    #print len(Interception_direct)
    #print len(ddl)
    
    #return Direction_direct_light
    #Direct_light = dict([(uc_id, surface_direct_light * directlight) for uc_id, surface_direct_light in Surface_direct_light.items()])
    #print Direct_light
    # faire directionalInterception(ndir)
    
# Saint-Pierre, Réunion: lat = -21.32  long = 55.50
    # calculer interception pour chacune des feuilles pour chaque heure

    # multiplier par la loi de coef photosynthèse (dependant du niveau d'interception)


if __name__ == '__main__':
    from openalea.vmango.utilities.util_tools import load_obj
    from openalea.plantgl.all import Scene, Viewer
    #mtg = load_obj('fruitstructure.pkl','../shoot_growth')
    mtg = None
    #sc = Scene('../shoot_growth/fruitstructure.bgeom')
    Initialize_Meteo_data(lightfile = 'rayostpierre2002.csv') 
    sunrf, skyrf = get_day_meteo(day =  "02/09/2002")
    dt = convert_jourJUL(day = "02/09/2002")
    #fs = photosynthesis(mtg,sc, day =  "02/09/2002")
    #ddl = getDirectLight( latitude = -21.32, longitude = 55.50, jourJul = convert_jourJUL("02/09/2002"), startH = 7 , stopH = 18, step=30, decalSun = 1, decalGMT = +4)
    #print Direction_direct_light
    #print sunrf
    #print min([h for h,par in sunrf]), max([h for h,par in sunrf])
    essai = get_direct_light(mtg, day = "02/09/2002")
