from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")
from os.path import join, exists

def load_obj(filename, dirname = '.'):
  import cPickle as pickle
  gfname = join(dirname,filename)
  if exists(gfname ):
    pkl_file = open(gfname,'rb')
    obj = pickle.load(pkl_file)
    pkl_file.close()
    return obj
  else:
    raise ValueError(gfname)

def dump_obj(obj,filename, dirname = '.'):
  import cPickle as pickle
  gfname = join(dirname,filename)
  pkl_file = open(gfname,'wb')
  pickle.dump(obj,pkl_file)
  pkl_file.close()

def load_mtg(name = 'mango_mtg.pkl'):
  g = load_obj(name,share_dir)
  return g

  


Month = {'janv' : 1, 'fev' : 2, 'mars' : 3,
         'avril' : 4, 'mai' : 5, 'juin' : 6,
         'juil' : 7, 'aout' : 8, 'sept' : 9,
         'oct' : 10, 'nov' : 11, 'dec' : 12 }

date_weeks = {
    0 : ('05-07-01','05-08-07'),
    1 : ('05-08-08','05-08-14'),
    2 : ('05-08-15','05-08-21'),
    3 : ('05-08-22','05-08-28'),
    4 : ('05-08-29','05-09-04'),
    5 : ('05-09-05','05-09-11'),
    6 : ('05-09-12','05-09-18'),
    7 : ('05-09-19','05-09-25'),
    8 : ('05-09-26','05-10-02'),
    9 : ('05-10-03','05-10-09'),
    10 : ('05-10-10','05-10-16'),
    11 : ('05-10-17','05-10-23'),
    12 : ('05-10-24','05-10-30')
}


beg_end_period = {'E' : (7,8,9,10), 'I' : (11,12,1,2), 'L' : (3,4,5,6)}

# for "get_data_for_glm"

from datetime import datetime
date_weeks_04 = {
0 : (datetime(2004,7,1),datetime(2004,8,7)),
1 : (datetime(2004,8,8),datetime(2004,8,14)),
2 : (datetime(2004,8,15),datetime(2004,8,21)),
3 : (datetime(2004,8,22),datetime(2004,8,28)),
4 : (datetime(2004,8,29),datetime(2004,9,4)),
5 : (datetime(2004,9,5),datetime(2004,9,11)),
6 : (datetime(2004,9,12),datetime(2004,9,18)),
7 : (datetime(2004,9,19),datetime(2004,9,25)),
8 : (datetime(2004,9,26),datetime(2004,10,2)),
9 : (datetime(2004,10,3),datetime(2004,10,9)),
10 : (datetime(2004,10,10),datetime(2004,10,16)),
11 : (datetime(2004,10,17),datetime(2004,10,23)),
12 : (datetime(2004,10,24),datetime(2004,10,30))  }

date_weeks_05 = {
0 : (datetime(2005,7,1),datetime(2005,8,7)),
1 : (datetime(2005,8,8),datetime(2005,8,14)),
2 : (datetime(2005,8,15),datetime(2005,8,21)),
3 : (datetime(2005,8,22),datetime(2005,8,28)),
4 : (datetime(2005,8,29),datetime(2005,9,4)),
5 : (datetime(2005,9,5),datetime(2005,9,11)),
6 : (datetime(2005,9,12),datetime(2005,9,18)),
7 : (datetime(2005,9,19),datetime(2005,9,25)),
8 : (datetime(2005,9,26),datetime(2005,10,2)),
9 : (datetime(2005,10,3),datetime(2005,10,9)),
10 : (datetime(2005,10,10),datetime(2005,10,16)),
11 : (datetime(2005,10,17),datetime(2005,10,23)),
12 : (datetime(2005,10,24),datetime(2005,10,30))  }
date_weeks = {4 : date_weeks_04, 5 : date_weeks_05}






