
from vplants.mangosim.tools import *
from vplants.mangosim.util_date import *


def load_mtg(name = 'mango_mtg.pkl'):
  g = load_obj(name,share_dir)
  return g

def normalize_property(g):
  """ 
    Fonction from Pierre Fernique, see him to understand
  """
  if "flowering" in g.property_names():
    raise IOError('`flowering` property has already been computed !')
  #warnings.warn('`flowering` property will be added to the mangotree but `date_flo` property will be removed !')
  
  olppropnb_fr = g.property('nb_fr')
  newpropnb_fr = dict([(vid, int(v)) for vid,v in olppropnb_fr.items() if len(v) > 0])
  g.properties()['nb_fr'] = newpropnb_fr

  old = g.property('date_flo')
  g.add_property('flowering')
  new = g.property('flowering')
  for i in old:
    dates = []
    mdate = old[i].split("+")
    if len(mdate) > 1:
      for j in mdate:
        d = j.split("/")
        if int(d[2]) < 2000:
          d[2] = 2000 + int(d[2])
        else: d[2] = int(d[2])
        dates.append(date(d[2], int(d[1]), int(d[0])))
    else:
      mdate = old[i].split('/')
      if len(mdate) == 3:
        dates.append(date(int(mdate[2]), int(mdate[1]), int(mdate[0])))
      else:
        mdate = old[i].split("-")
        if len(mdate) == 3:
          d = int(mdate[0])
          y = 2000+int(mdate[2])
          # monthdict = { "mai" : 5, "juin" : 6, "juil" : 7,  "aout" : 8, "sept" : 9,  "oct" : 10, "nov" : 11, "dec" : 12}
          try:
                m = Month[mdate[1]]
          except:
                raise ValueError('date_flo` property is no more valid for this function')
          dates.append(date(y, m, d))
    if len(dates) > 0:
      new[i] = dates

  date_burst = g.property('date_burst')
  if type(date_burst.itervalues().next()) != str:
        print type(date_burst.itervalues().next())
        raise ValueError('`date_burst` property has already been transformed !')
  date_burst_new = dict([(k,date_from_string(v)) for k,v in date_burst.items()])
  g.property('date_burst').update(date_burst_new)


doralice_mtg = None
def get_mtg():
    global doralice_mtg
    if doralice_mtg is None:
        doralice_mtg = load_mtg()
        normalize_property(doralice_mtg)
    return doralice_mtg


if __name__ == '__main__':
    mtg = get_mtg()
