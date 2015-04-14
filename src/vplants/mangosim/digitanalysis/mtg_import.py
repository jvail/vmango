from openalea.mtg import MTG

def get_filename(name):
    from os.path import join, exists
    from openalea.deploy.shared_data import shared_data
    import vplants.mangosim
    share_dir = shared_data(vplants.mangosim, share_path = join("share",'digitized_mango_mtg'))
    return join(share_dir,name)

def getMTG(name):
	return MTG(get_filename(name))