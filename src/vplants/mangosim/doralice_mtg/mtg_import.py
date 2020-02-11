
from vplants.mangosim.tools import *
from vplants.mangosim.util_path import *
from vplants.mangosim.util_date import *


def load_mtg(name = 'mango_mtg.pkl'):
  g = load_obj(name,share_dir)
  return g

def normalize_property(g):
    """
    A number of property are stored in string format.
    We convert them in adequate format here.
    """
    def convert_prop_to(mtg, name, mtype = int):
        oldproperty = mtg.property(name)
        if type(list(oldproperty.values())[0]) != mtype:
            newproperty = dict([(vid, mtype(v)) for vid,v in list(oldproperty.items()) if len(v) > 0 and v != 'NA'])
            mtg.properties()[name] = newproperty

    def convert_prop_to_date(mtg, name, refdate = date(1900,1,1)):
        oldproperty = mtg.property(name)
        if type(list(oldproperty.values())[0]) != date:
            newproperty = dict([(vid, refdate + timedelta(days=int(v))) for vid,v in list(oldproperty.items()) if type(v) is str and len(v) > 0 and v != 'NA'])
            newproperty.update(dict([(vid, refdate + timedelta(days=int(v))) for vid,v in list(oldproperty.items()) if type(v) is date]))
            mtg.properties()[name] = newproperty

    def convert_prop_to_monthdate(mtg, name, refyear = 2000):
        oldproperty = mtg.property(name)
        if type(list(oldproperty.values())[0]) != date:
            newproperty = dict([(vid, date_from_string(v)) for vid,v in list(oldproperty.items()) if len(v) > 0])
            mtg.properties()[name] = newproperty

    def convert_prop_to_datelist(mtg, name):
        oldproperty = mtg.property(name)
        newproperty = {}
        for vid, mdates in list(oldproperty.items()):
            if len(mdates) > 0:
                dates = []
                mdates = mdates.split("+")
                for mdate in mdates:
                    sep = '/' if '/' in mdate else '-'
                    sdate = mdate.split(sep)
                    assert len(sdate) == 3
                    day = int(sdate[0])
                    year = int(sdate[2])
                    if year < 2000:  year += 2000
                    month = sdate[1]
                    if month in Month:
                        month = Month[month]
                    else : 
                        month = int(month)
                    dates.append(date(year, month, day))
                if len(dates) > 0:
                    newproperty[vid] = dates
        mtg.properties()[name] = newproperty

  
    convert_prop_to(g,'nb_fr')
    convert_prop_to(g,'nb_inflo_t')
    convert_prop_to(g,'nb_inflo_l')
    convert_prop_to(g,'nb_thin_fr')
    convert_prop_to(g,'wgt_fr', float)
    convert_prop_to(g,'wgt_thin_fr', float)
    convert_prop_to(g,'length', float)
    convert_prop_to(g,'mean_diam', float)

    # date_e_harv is a date define as a number of day after 1 january of 1900
    convert_prop_to_date(g, 'date_e_harv')
    convert_prop_to_date(g, 'date_b_harv')

    # date_burst is define as a 'month.year' string
    convert_prop_to_monthdate(g, 'date_burst')
    convert_prop_to_monthdate(g, 'dead')

    # Converting date of date_flo
    if "flowering" in g.property_names():
      raise IOError('`flowering` property has already been computed !')

    g.properties()['flowering'] = g.properties()['date_flo']
    del g.properties()['date_flo']
    convert_prop_to_datelist(g, 'flowering')



doralice_mtg = None
def get_mtg():
    global doralice_mtg
    if doralice_mtg is None:
        mtg = load_mtg()
        normalize_property(mtg)
        doralice_mtg = mtg
    return doralice_mtg


if __name__ == '__main__':
    mtg = get_mtg()
