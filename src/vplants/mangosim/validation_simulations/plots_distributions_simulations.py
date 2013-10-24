from vplants.statistic.core.data.marginal.scalar import *
from openalea.deploy.shared_data import shared_data
import vplants.mangosim
share_dir = shared_data(vplants.mangosim, share_path = "share")

from vplants.mangostat import properties, calendar, xlabels_date
from datetime import datetime
properties.run()

def get_limit_IC(list_to_sort,percentille = 5.0):
  """ """
  a = np.sort(list_to_sort)
  list_sort = list( a)
  length_list = len(list_to_sort)
  min_p = percentille/200.
  max_p = 1 - min_p
  min_ind , max_ind , median_ind = int(length_list*min_p) , int(length_list*max_p), length_list/2 
  return (list_sort[min_ind],list_sort[max_ind],list_sort[median_ind])

from os.path import join

def choice_data_for_ploting(model, name_tree, path_choice, nb_distribution, cycle):
    """
    Parameters : 
        model : an integer, 
            if 0 : model null is wanted
            if 1 : model glm is wanted
            if 2 : graphic model is wanted
        name_tree : a string, it is the name of the tree
        path_choice : a string,
            if "by_all_trees" : model was fitted by all trees data
            if "by_feature_loaded_tree" : model was fitted by all trees loaded or not loaded data
            if "by_tree" : model was fitted by the tree data
            if "nothinned_nomixture" : graphic model for loaded tree and no mixture distribution
        nb_distribution : an integer,
            - if 0 : "total_number_ucs_tree_cycle"
            - if 2 : "nb_ucs_tree_cycle_in_extremity"
            - if 4 : "nb_ucs_tree_cycle_in_extremity_apical_position"
            - if 6 : "terminal_apical_rate_tree_cycle"
            - if 8 : "nb_uc_giving_inflorescence_tree_cycle"
            - if 10 : "flowering_rate_tree_cycle"
            - if 14 : "nb_axe_tree_cycle"
            - if -10 : "nb_inflo_per_uc_tree_cycle" 
            - if -6 : "nb_children_per_uc_tree_cycle"
            - if -4 : "nb_ucs_per_axe_tree_cycle"
            - if -2 : "monthly_date_ucs_tree_cycle"
        cycle: an integer, 
            if 0 : we take the cycle 4 
            if 1 : we take the cycle 5
    Return a tuple of data : (data_simulate, [data_empirical])
        data_empirical corresponds to a list of data(value or dict) for tree loaded or not loaded (depends on data_simulate)
    """
    if model==0 : # if model null is wanted 
        path_model = join( "simulation_mangotree","simulations_mangotree_model_null" )
    elif model==1 : # if model glm is wanted
        path_model = join( "simulation_mangotree","simulations_mangotree_glm","glm_complet" )
    else : # graphic model is wanted
        path_model = join( "simulation_mangotree","simulations_mangotree_graphic_model" )
        path_choice = "nothinned_nomixture"
    #name_tree = "B10"
    #path_choice = "by_feature_loaded_tree"
    final_path = join( share_dir, path_model, path_choice, "info_simulate_"+ name_tree +"_mtg.pkl")
    if nb_distribution == 0:
        name_file_empirical = "total_number_ucs_tree_cycle"
    elif nb_distribution == 2:
        name_file_empirical = "nb_ucs_tree_cycle_in_extremity"
    elif nb_distribution == 4:
        name_file_empirical = "nb_ucs_tree_cycle_in_extremity_apical_position"
    elif nb_distribution == 6:
        name_file_empirical = "terminal_apical_rate_tree_cycle"
    elif nb_distribution == 8:
        name_file_empirical = "nb_uc_giving_inflorescence_tree_cycle"
    elif nb_distribution == 10:
        name_file_empirical = "flowering_rate_tree_cycle"
    elif nb_distribution == 12:
        name_file_empirical = "nb_axe_tree_cycle"
    elif nb_distribution == -10:
        name_file_empirical = "nb_inflo_per_uc_tree_cycle"
    elif nb_distribution == -6:
        name_file_empirical = "nb_children_per_uc_tree_cycle"
    elif nb_distribution == -4:
        name_file_empirical = "nb_ucs_per_axe_tree_cycle"
    elif nb_distribution == -2:
        name_file_empirical = "monthly_date_ucs_tree_cycle"
    else : print "Pb : number given is not a possibility, choice another number"
    simulate = load(final_path)[nb_distribution]
    empirical_data = {}
    if name_tree in ["B12","B10","F2"] :
        empirical_data["B12"] = load( join(share_dir, "info_mtg_doralice", name_file_empirical+".pkl" ) )[0+cycle]
        empirical_data["B10"] = load( join( share_dir, "info_mtg_doralice",name_file_empirical+".pkl" ) )[2+cycle]
        empirical_data["F2"] = load( join( share_dir, "info_mtg_doralice",name_file_empirical+".pkl") )[4+cycle]
    else : 
        empirical_data["B14"] = load( join(share_dir, "info_mtg_doralice", name_file_empirical+".pkl" ) )[6+cycle]
        empirical_data["F6"] = load( join( share_dir, "info_mtg_doralice",name_file_empirical+".pkl") )[8+cycle]
    return (simulate, empirical_data)


def plot_data_list(simulate_data, dict_empirical_data, xlab = "", ylab = "", title=""):
    """
    Parameters : 
        simulate_data : list of values 
        dict_empirical_data : list of values of trees loaded or not loaded
    """
    hs = Histogram(Scalar.Integer)
    for i in simulate_data:
        hs[i] += 1
    hs.plot(bar=dict(ec='none'))
    plt.xlabel(xlab) #"Number of UC for a cycle")
    plt.ylabel(ylab) #"Count")
    plt.title(title) #"Distribution of UC's number in cycle 4 ")
    #plt.xlim([250,850])
    #plt.ylim([0,10])
    colors = ["r", "m", "y"]
    for name,val, j in zip(dict_empirical_data.keys(), dict_empirical_data.values(), xrange(len(dict_empirical_data)) ):
        plt.axvline(val , color = colors[j], linestyle = "dashed", linewidth=2, label = name)
    limit_IC  = get_limit_IC(simulate_data)
    plt.axvline(limit_IC[0], color = 'g', linestyle = "dashed", linewidth=3, label ="IC 95 %" )
    plt.axvline(limit_IC[1], color = 'g', linestyle = "dashed", linewidth=3)
    plt.axvline(limit_IC[2], color = 'chartreuse', linestyle = "dashed", linewidth=3 ,label = "Median")
    plt.legend(loc=0)
    plt.show()

# # total_number_ucs_tree_cycle : 
# data_sim, data_emp = choice_data_for_ploting(2,"B12","nothinned_nomixture",0,0)
# plot_data_list(data_sim, data_emp, xlab = "Number of GU for a cycle", ylab = "Count", title="Distribution of GU's number in cycle 4")
# # nb_inflo_tree_cycle :
# data_sim_per_GU, data_emp_per_GU = choice_data_for_ploting(0,"B12","by_all_tree",-10,0)
# d = {key : [nb for nb in data_emp_per_GU[key].keys() for j in range( data_emp_per_GU[key][nb]) ] for key in data_emp_per_GU.keys()}
# data_sim = sum(data_sim_per_GU)
# data_emp = { key : sum(data_emp_per_GU[key]) for key in data_emp_per_GU.keys()}
# plot_data_list(data_sim, data_emp, xlab = "Number of inflorescences for a cycle", ylab = "Count", title="Distribution of inflorescences's number in cycle 4")



def plot_data_rate(simulate_data, dict_empirical, emp_tree_name="", xlab="", ylab="Count", title=""):
    """
    Parameters : 
        simulate_data : list of values 
        dict_empirical_data : list of values of trees loaded or not loaded
        emp_tree_name : string
    """
    hs = Histogram(Scalar.Real)
    for i in simulate_data:
        hs[i] += 1
    fig = plt.figure()
    axes = hs.plot(axes=fig.add_subplot(2,1,1), norm=True)
    axes.set_xlim(0,1)
    plt.title(title)
    if emp_tree_name!="":
        plt.axvline(dict_empirical[emp_tree_name] , color = 'r', linestyle = "dashed", linewidth=2, label = emp_tree_name)
    else :
        colors = ['r', 'm', 'y']
        for name, j in zip(dict_empirical.keys(), range(len(dict_empirical))):
            plt.axvline(dict_empirical[name] , color = colors[j], linestyle = "dashed", linewidth=2, label = name)
    plt.xlabel(xlab)
    plt.ylabel(ylab)
    plt.legend(loc=0)
    axesb = hs.plot("boxplot", axes=fig.add_subplot(3,1,3), vert=False)
    axesb.set_xticks(axes.get_xticks())
    axesb.xaxis.set_visible(False)
    if emp_tree_name!="":
        plt.axvline(dict_empirical[emp_tree_name] , color = 'r', linestyle = "dashed", linewidth=2)
    else:
        for name, j in zip(dict_empirical.keys(), xrange(len(dict_empirical))):
            plt.axvline(dict_empirical[name] , color = colors[j], linestyle = "dashed", linewidth=2)
    plt.show()

# # terminal_apical_rate_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(2,"F2","",6,0)
# plot_data_rate(data_sim, data_emp, emp_tree_name="", xlab = "Rate of terminal apical GUs", ylab = "Count", title="Distribution of terminal apical GU rate in cycle 4")
# # flowering_rate_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(2,"F2","",10,0)
# plot_data_rate(data_sim, data_emp, emp_tree_name="", xlab = "Flowering rate", ylab = "Count", title="Distribution of flowering rate in cycle 4")


def plot_data_dict(simulate_data, dict_empirical_data, emp_tree_name="", xlab = "", ylab = "", title = ""):
    """
    Parameters : 
        simulate_data : list of dict
        dict_empirical_data : list of dict of trees loaded or not loaded
        emp_tree_name : string
            if data are estimated from just a tree, you could show only these empiric data of the tree
    """
    # plot simulate_data
    keys = set()
    hs = list()
    for i in simulate_data:
        hs.append(Histogram(Scalar.Integer))
        for j, k in i.iteritems():
            hs[-1][j] += k
            keys.add(j)
    keys = list(keys)
    axes = plt.subplot(1,1,1)
    for i in keys:
        ths = Histogram(Scalar.Real)
        for j in hs:
            #ths[j[i]*he.sum()/float(j.sum())] += 1
            ths[j[i]/float(j.sum())] += 1
        ths.plot("boxplot", axes=axes, pos=i, vert=True)
    axes.xaxis.set_visible(True)
    #plot empirical_data
    if emp_tree_name!="":
        he = Histogram(Scalar.Integer, weighted=True)
        s = float(len(dict_empirical_data[emp_tree_name]))
        for j in dict_empirical_data[emp_tree_name]:
            he[j] += 1./s
        colors = ["r", "m", "y"]
        axes = he.plot(fmt='o-', color='r', axes=axes, plot=dict(label=emp_tree_name))
    else :
        he = []
        for i in dict_empirical_data.values():
            he.append(Histogram(Scalar.Integer, weighted=True))
            s = float(len(i))
            for j in i:
                he[-1][j] += 1./s
        colors = ["r", "m", "y"]
        for i, name, j in zip(he, dict_empirical_data.keys(), xrange(len(dict_empirical_data)) ):
            axes = i.plot(fmt='o-', color=colors[j], axes=axes, plot=dict(label=name))
    axes.set_xlabel(xlab) 
    axes.set_ylabel(ylab) 
    axes.set_title(title) 
    axes.set_xlim(min(keys)-0.3, max(keys)+0.2)
    axes.set_xticks(keys)
    plt.legend(loc=0)
    plt.show()

# # nb_children_per_uc_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(2,"F2","",-6,0)
# plot_data_dict(data_sim, data_emp, emp_tree_name="", xlab = "Number of children per GU", ylab = "Probability", title="Distribution of children's number per GU in cycle 4")
# # nb_ucs_per_axe_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(1,"F2","by_all_trees",-4,0)
# plot_data_dict(data_sim, data_emp, emp_tree_name="", xlab = "Number of children per ancestor GU", ylab = "Probability", title="Distribution of children's number per anceestor GU in cycle 4")
# # nb_inflo_per_uc_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(2,"B12","",-10,0)
# d = {key : [nb for nb in data_emp[key].keys() for j in range( data_emp[key][nb]) ] for key in data_emp.keys()}
# plot_data_dict(data_sim, d, emp_tree_name="", xlab = "Number of inflorescences per GU", ylab = "Probability", title="Distribution of inflorescences's number per GU in cycle 4")


def plot_data_monthly(simulate_data, dict_empirical_data, cycle, name_tree= "", title = ""):
    """
    Parameters : 
        simulate_data : list of dict 
        dict_empirical_data : list of dict of trees loaded or not loaded
        cycle : integer
        name_tree : string
            if data are estimated from just a tree, you could show only these empiric data of the tree
        title : string
    """
    # plot simulate_data
    axes = plt.subplot(1,1,1)
    keys = set()
    hs = list()
    for i in simulate_data:
        hs.append(Histogram(Scalar.Factor))
        for j, k in i.iteritems():
            hs[-1][j+"-01"] += k
            keys.add(j+"-01")
    keys = list(keys)
    #axes = plt.subplot(1,1,1)
    # plot empirical_data
    cal = calendar()
    if name_tree != "":
        empirical_data = dict_empirical_data[name_tree]
        he = Histogram(Scalar.Factor)
        for j in cal:
            he[j.strftime("%y-%m-%d")] = 0
        for k , l in empirical_data.iteritems():
            he[k+"-01"] += l
        for ii in keys:
            ths = Histogram(Scalar.Real)
            for jj in hs:
                ths[jj[ii]] += 1
            d = ii.split("-")
            d = datetime(2000+int(d[0]), int(d[1]), 1)
            ths.plot("boxplot", axes=axes, pos=cal.index(d), widths=10, vert=True)
        axes = he.plot("density", fmt="|", color="r", axes=axes, bar=dict(ec='none', width=3, label=name_tree, alpha=0.6) )
    else :
        he = []
        index = 1
        for values_dict in dict_empirical_data.values():
            he.append(Histogram(Scalar.Factor))
            for d in cal:
                he[-1][d.strftime("%y-%m-%d")] = 0
            for k , l in values_dict.iteritems():
                he[-1][k+"-0"+str(index*3)] += l
            index += 1
        for ii in keys:
            ths = Histogram(Scalar.Real)
            for jj in hs:
                ths[jj[ii]] += 1
            d = ii.split("-")
            d = datetime(2000+int(d[0]), int(d[1]), 1)
            ths.plot("boxplot", axes=axes, pos=cal.index(d), widths=10, vert=True)
        colors = ["r", "m", "y"]
        alphas = [0.6,0.7,1]
        for h, c, name in zip(he,xrange(len(he)) , dict_empirical_data.keys()) :
            axes = h.plot("density", fmt="|", color=colors[c], axes=axes, bar=dict(ec='none',width=3,label=name, alpha=alphas[c]))
    axes.xaxis.set_visible(True)
    xlabels_date(axes)
    axes.set_title(title)
    axes.set_ylabel("Count")
    axes.set_xlim(cal.index(datetime(2000+cycle-1, 06, 01)), cal.index(datetime(2000+cycle, 07, 01)))
    plt.legend()
    plt.show()

# # monthly_date_ucs_tree_cycle :
# data_sim, data_emp = choice_data_for_ploting(2,"F2","",-2,0)
# plot_data_monthly(data_sim, data_emp, 4, name_tree= "", title = "Distribution of GU's number per month in the cycle 4")


