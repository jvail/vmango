from vplants.mangosim.state import *
from vplants.mangosim.utils.util_plot import *
from vplants.mangosim.util_path import common_options

restrictions = [None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction, eBurstDateOnlyRestriction, ePositionAOnlyRestriction, ePositionAncestorAOnlyRestriction, eNatureFOnlyRestriction]
restrictionLabels = ['With All Factors', 'Without Mother Burst Date', 'Without Mother Position','Without Ancestor Position', 'Without Ancestor Fate', 'With No Factor', 'With only Mother Burst Date', 'Without only Mother Position','With only Ancestor Position', 'With only Ancestor Fate']

restrictionLabels = dict(zip(restrictions,restrictionLabels))


optionvalueLabeling = {'GLM_RESTRICTION' : restrictionLabels}
optionnameLabeling = {'GLM_RESTRICTION' : 'Factors', 'FRUIT_MODEL' : 'Fruit Growth', 'FRUITBRANCHSIZE' : 'Fruiting Branch Size'}

DEFAULT_OPTION = {'GLM_TYPE' : eInteractionGlm, 'GLM_RESTRICTION' : None, 'FRUIT_MODEL' : False}

def build_legends(legends, legendtags):
    if legendtags:
        if len(legendtags) > 1:
            commons = set(common_options(legendtags))
        else:
            commons = set(common_options([DEFAULT_OPTION]+legendtags))
        properoptions = [set(p).difference(commons) for p in legendtags]
        proper0 = properoptions[0]
        similars = False
        if len(proper0) == 1:
            similars = True
            for p in properoptions :
                if p != proper0:
                    similars = False
                    break
            proper0 = proper0.pop()

        plegends = []
        for p, pnames in zip(legendtags, properoptions):
            if similars :
                pvalue = p[proper0]
                pvalue = optionvalueLabeling.get(proper0, {}).get(pvalue, str(pvalue))
                plegends.append(pvalue)
            else:
                llegends = []
                for pname in pnames:
                    pvalue = p[pname]
                    pvalue = optionvalueLabeling.get(pname, {}).get(pvalue, str(pvalue))
                    pname = optionnameLabeling.get(pname, pname)
                    llegends.append(pname+' : '+pvalue)
                plegends.append(', '.join(llegends))

        if legends:
            legends = [plegend+ (':' if len(plegend) > 0 else '')+leg  for leg, plegend in zip(legends,plegends)]
        else:
            legends = plegends
    return legends


def plot_histo(keys, allvalues, _title = None, reference = None, legendtag = None, linestyle = '-', titlelocation = 2, histocurve = False, xlabelrotation = 0):
    import matplotlib.pyplot as plt
    import numpy as np
    fig, ax = plt.subplots(figsize=(10,8))
    nbplot = len(allvalues)
    nbx = len(allvalues[0])
    width = 1

    ind = np.arange(0,nbx*width,width)+width
    #if reference : ax.bar(ind-width/4., reference, width/2., color='r' )
 
    # 'k--'
    if reference:
        color = (0.5,0.0,0.5,1)
        if linestyle != 'o':
            x,y = smoothcurve(ind, reference)
            plt.plot( x, y, linestyle, color=color,  label = 'Reference', linewidth= 3)
            plt.plot( ind, reference, 'o', color=color,  linewidth= 3)
        else:
            plt.plot( ind, reference, 'o', color=color, label = 'Reference', linewidth= 3)

    
    legend = str(len(allvalues))+' Simulations'
    legend = build_legends([legend], [legendtag])[0]

    meanvalues = [np.mean([v[i] for v in allvalues]) for i in xrange(nbx)]
    if histocurve:
        x,y = smoothcurve(ind, meanvalues)
        plt.plot(x, y, linestyle, color='k', label = legend, linewidth= 2)
    else:
        print ind
        print meanvalues
        ax.bar(ind, meanvalues, label = legend)

    bpdata = [[v[i] for v in allvalues] for i in range(nbx)]
    ax.boxplot(bpdata, widths=0.5)
    ax.set_xticks(ind)

    #plt.ylim(0,1500)

    ax.set_xticklabels(keys, rotation=xlabelrotation)
    #ax.set_xticklabels(keys)
    if _title: ax.set_title(_title)
    #fig.set_size_inches(1600,800)
    ax.legend(loc=titlelocation)
    return fig, ax
    #plt.show()


def plot_histos(keys, allvaluesset, _title = None, reference = None, legends = None, legendtags = None, titlelocation = 2, xlabelrotation = 0, normalize = False):
    import matplotlib.pyplot as plt
    import matplotlib.patches as mpatches

    import numpy as np
    fig, ax = plt.subplots(figsize=(15,5))
    nbplot = len(allvaluesset)
    nbx = len(allvaluesset[0][0])
    uwidth = 1
    width = (nbplot+1) * uwidth * 1.2
    
    listcolors = ['r','y','b','c','m','g','k']
    colors = lambda x: listcolors[x]

    legends = build_legends(legends, legendtags)

    indg = np.arange(0,nbx*width,width)+width

    legend_patches = []
    if reference:
        if normalize: reference = normalize_histo(reference)
        refhandle, = plt.plot(indg, reference, 'o', color=(0.5,0.5,0.5,1),  label = 'Reference', linewidth= 3)
        legend_patches.append(refhandle)
    
    if normalize:
        nallvaluesset = []
        for valueset in allvaluesset:
            nallvaluesset.append([normalize_histo(val) for val in valueset])
    else:
        nallvaluesset = allvaluesset


    for i,allvalues in enumerate(nallvaluesset):
        ind = indg+ (i+0.5 - nbplot/2)*uwidth
        bpdata = [[v[j] for v in allvalues] for j in range(nbx)]
        bp = ax.boxplot(bpdata, widths=uwidth, positions = ind)
        #print bp.keys()
        plt.setp(bp['boxes'], color=colors(i))
        plt.setp(bp['medians'], color=colors(i))
        legend_patches.append(mpatches.Patch(color=colors(i), label=legends[i]))
    
    #plt.legend()

    plt.xlim(0, width*(nbx+1))

    ax.set_xticks(indg)
    ax.set_xticklabels(keys, rotation=xlabelrotation)
    if _title: ax.set_title(_title)
    #fig.set_size_inches(1600,800)
    ax.legend(handles=legend_patches, loc=titlelocation)
    return fig, ax
    #plt.show()

def normalize_histo(histo):
    sh1 = float(sum(histo))
    return [v1/sh1 for v1 in histo]


def plot_histos_means(keys, allvalues, _title = None,  reference = None, legends = None, legendtags = None, linewidth = 1, linestyle = '-o', titlelocation = 1, xlabelrotation = 0, normalize = True):
    import matplotlib.pyplot as plt
    import numpy as np

    legends = list(reversed(build_legends(legends, legendtags)))

    #fig, ax = plt.subplots()
    fig = plt.figure(figsize=(10,8))
    ax = fig.add_subplot(111)
    nbplot = len(allvalues)
    nbx = len(allvalues[0])
    width = 1
    if nbplot > 7:
        _colors = plt.get_cmap('jet',nbplot)
        colors = lambda x: _colors(nbplot-1-x)
    else:
        listcolors = list(reversed(['r','y','b','c','m','k']))
        colors = lambda x: listcolors[x]
    ind = np.arange(0,nbx*width,width)
    for i,values in enumerate(reversed(allvalues)):
        if normalize : values = normalize_histo(values)
        x,y = smoothcurve(ind, values)
        ax.plot(x, y, '-', color=colors(i), linewidth = linewidth, label = '' if legends is None else 'GLM '+legends[i] )
        ax.plot(ind, values, 'o', color=colors(i), linewidth = linewidth)
    if reference:
        if normalize : reference = normalize_histo(reference)
        x,y = smoothcurve(ind, reference)
        plt.plot(x,y, linestyle = '-', color=(0.5,0.5,0.5,1), label = 'Reference', linewidth= linewidth+1)
        plt.plot(ind,reference, 'o', color=(0.5,0.5,0.5,1), linewidth= linewidth+1)
    ax.set_xticks(ind)

    ax.set_xticklabels(keys, rotation=xlabelrotation)
    if _title: ax.set_title(_title)
    if legends : ax.legend(loc=titlelocation)
    #plt.show()
    return fig, ax


