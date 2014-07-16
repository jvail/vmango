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



