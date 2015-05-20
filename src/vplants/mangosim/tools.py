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
  pickle.dump(obj,pkl_file, pickle.HIGHEST_PROTOCOL)
  pkl_file.close()



