# Name of the modules to export
modules_to_export = ['Tree', 'GU','Inflorescence','Fruit']
scales = [1,2,2,2]


def export_to_mtg(lstring):
  from openalea.mtg.io import axialtree2mtg
  # Name of the modules to export
  tree_parameters = ['treename','estimation','loading','variety','seed']
  for m in lstring:
    if m.name == 'Tree':
        tree_parameters = list(m[0].parameter_names())
        break

  params_to_export = [tree_parameters,
                      ['burst_date','cycle','mtgid','nature','mixed_inflo','final_length_gu','length','radius','final_length_leaves','nb_internodes'],
                      ['burst_date','bloom_date','fullbloom_date','cycle','final_length','length','fruiting','nb_axes','nb_fruits','fruits_weight','fruits_maturity_date','leaffruit_ratio'],
                      ['inflo_fullbloom_date','cycle','growth', 'maturity_date', 'growth_stage_date', 'initial_weight','weight']]

  # Convert lstring into mtg
  mtg = axialtree2mtg(lstring,
                      scale = dict(zip(modules_to_export,scales)),
                      scene = None,
                      parameters = dict(zip(modules_to_export,params_to_export)))
  assert not mtg is None
  return mtg

def export_to_mtg_light(lstring, cycle):
  from openalea.mtg.io import axialtree2mtg
  lmodules_to_export = modules_to_export+['InflorescenceBud']
  lscales = scales+[2]
  params_to_export = [['treename','variety'],
                      ['burst_date','cycle'],
                      ['fullbloom_date','bloom_date','cycle','nb_fruits'],
                      ['inflo_fullbloom_date','cycle'],
                      ['fullbloom_date','bloom_date','cycle','nb_fruits']]
  params_to_export = [['p']+p for p in params_to_export]
  parameters = dict(zip(lmodules_to_export,params_to_export))
  # Convert lstring into mtg
  mtg = axialtree2mtg(lstring,
                      scale = dict(zip(lmodules_to_export,lscales)),
                      scene = None,
                      parameters = parameters)
  labels =  mtg.property('label')
  toupdate  = [(v,'Inflorescence') for v,n in labels.items() if n == 'InflorescenceBud' and mtg.property('cycle')[v] == cycle]
  labels.update(toupdate)
  toremove =   [v for v,n in labels.items() if n == 'InflorescenceBud']
  for vid in toremove:
     mtg.remove_vertex(vid)
     for pname in parameters['InflorescenceBud']:
        if mtg.property(pname).has_key(vid):
            del mtg.property(pname)[vid]
  return mtg
