# Name of the modules to export
modules_to_export = ['Tree', 'GU','Inflorescence','Fruit']
scales = [1,2,2,2]


def export_to_mtg(lstring, lscene):
  from openalea.mtg.io import axialtree2mtg
  # Name of the modules to export
  params_to_export = [['treename','method','estimation','loading','variety','within_delay_method','seed'],['burst_date','cycle'],['bloom_date','cycle'],['inflo_bloom_date','cycle']]
  # Convert lstring into mtg
  mtg = axialtree2mtg(lstring,
                      scale = dict(zip(modules_to_export,scales)),
                      scene = None,
                      parameters = dict(zip(modules_to_export,params_to_export)))
  return mtg

def export_to_mtg_light(lstring, lscene):
  from openalea.mtg.io import axialtree2mtg
  params_to_export = [['treename','variety'],['burst_date','cycle'],['bloom_date','cycle','nb_fruits'],['inflo_bloom_date','cycle']]
  params_to_export = [['p']+p for p in params_to_export]
  # Convert lstring into mtg
  mtg = axialtree2mtg(lstring,
                      scale = dict(zip(modules_to_export,scales)),
                      scene = lscene,
                      parameters = dict(zip(modules_to_export,params_to_export)))
  return mtg
