from __future__ import division

from builtins import range, str, zip

from openalea.vmango.constants import *
from openalea.vmango.utilities.util_tools import isiterable
from openalea.vmango.utilities.util_path import common_options
#from openalea.vmango.utils.util_plot import *
from past.utils import old_div

restrictions = [None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction, eBurstDateOnlyRestriction, ePositionAOnlyRestriction, ePositionAncestorAOnlyRestriction, eNatureFOnlyRestriction]
restrictionLabels = ['With All Factors', 'Without Mother Burst Date', 'Without Mother Position','Without Ancestor Position', 'Without Ancestor Fate', 'With No Factor', 'With only Mother Burst Date', 'Without only Mother Position','With only Ancestor Position', 'With only Ancestor Fate']

restrictionLabels = dict(list(zip(restrictions,restrictionLabels)))


optionvalueLabeling = {'GLM_RESTRICTION' : restrictionLabels}
optionnameLabeling = {'GLM_RESTRICTION' : 'Factors', 'FRUIT_MODEL' : 'Fruit Growth', 'FRUITBRANCHSIZE' : 'Fruiting Branch Size'}

DEFAULT_OPTION = {'GLM_TYPE' : eInteractionGlm, 'GLM_RESTRICTION' : None, 'FRUIT_MODEL' : False}

def build_legends(legendoptions, legendtags):
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

        if legendoptions:
            legends = [plegend+ (':' if len(plegend) > 0 else '')+leg  for leg, plegend in zip(legendoptions,plegends)]
        else:
            legends = plegends
    else:
        legends = legendoptions
    return legends


def plot_histo(keys, allvalues, _title = None, reference = None, legendtag = None, linestyle = '-', titlelocation = 2, histocurve = False, xlabelrotation = 0, figsize=(10,8)):
    import matplotlib.pyplot as plt
    import numpy as np
    fig, ax = plt.subplots(figsize=figsize)
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

    meanvalues = [np.mean([v[i] for v in allvalues]) for i in range(nbx)]
    if histocurve:
        x,y = smoothcurve(ind, meanvalues)
        plt.plot(x, y, linestyle, color='k', label = legend, linewidth= 2)
    else:
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


def plot_histos(keys, allvaluesset, _title = None, reference = None, legends = None, legendoptions = None, legendtags = None, titlelocation = 2, xlabelrotation = 0, normalize = False, figsize=(15,5)):
    import matplotlib.pyplot as plt
    import matplotlib.patches as mpatches

    import numpy as np
    fig, ax = plt.subplots(figsize=figsize, dpi=100)
    nbplot = len(allvaluesset)
    nbx = len(allvaluesset[0][0])
    uwidth = 1
    decal = 0.5
    width = (nbplot) * uwidth + decal
    
    listcolors = ['r','y','b','c','m','g','plum','k']
    colors = lambda x: listcolors[x]

    if not legends:
        legends = build_legends(legendoptions, legendtags)

    centerdecal = (nbplot * uwidth)/2.
    indg = np.arange(0,nbx*width,width) + centerdecal + decal

    legend_patches = []
    if reference:
        if normalize: reference = normalize_histo(reference)
        for indgi, refi in zip(indg,reference):
            if isiterable(refi):
                refi = np.mean(refi)
            refhandle, = plt.plot([indgi-centerdecal,indgi+centerdecal], [refi,refi], '-', color=(0.5,0.5,0.5,1),  label = 'Measured', linewidth= 3)
        legend_patches.append(refhandle)
    
    if normalize:
        nallvaluesset = []
        for valueset in allvaluesset:
            nallvaluesset.append([normalize_histo(val) for val in valueset])
    else:
        nallvaluesset = allvaluesset


    for i,allvalues in enumerate(nallvaluesset):
        ind = indg+ (i+0.5 - nbplot/2.)*uwidth
        bpdata = [[v[j] for v in allvalues] for j in range(nbx)]
        bp = ax.boxplot(bpdata, widths=0.9*uwidth, positions = ind)
        colori = colors(i)
        # print bp.keys()
        linewith = 1.5
        plt.setp(bp['boxes'], color=colori, linewidth=linewith)
        plt.setp(bp['medians'], color=colori, linewidth=linewith)
        plt.setp(bp['whiskers'], color=colori, linewidth=linewith)
        plt.setp(bp['caps'], color=colori, linewidth=linewith)
        plt.setp(bp['fliers'], color=colori, linewidth=linewith)
        legend_patches.append(mpatches.Patch(facecolor=colori, edgecolor='k',label=legends[i]))
    
    #plt.legend()

    plt.xlim(0, width*nbx + decal)

    ax.set_xticks(indg)
    ax.set_xticklabels(keys, rotation=xlabelrotation)
    if _title: ax.set_title(_title)
    #fig.set_size_inches(1600,800)

    ax.legend(handles=legend_patches, loc=titlelocation)
    return fig, ax
    #plt.show()

def normalize_histo(histo):
    sh1 = float(sum(histo))
    return [old_div(v1,sh1) for v1 in histo]


def plot_histos_means(keys, allvalues, _title = None,  reference = None, legends = None, legendtags = None, linewidth = 1, linestyle = '-o', titlelocation = 1, xlabelrotation = 0, normalize = True, figsize=(10,8)):
    import matplotlib.pyplot as plt
    import numpy as np

    legends = list(reversed(build_legends(legends, legendtags)))

    #fig, ax = plt.subplots()
    fig = plt.figure(figsize=figsize)
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
        plt.plot(x,y, linestyle = '-', color=(0.5,0.5,0.5,1), label = 'Measured', linewidth= linewidth+1)
        plt.plot(ind,reference, 'o', color=(0.5,0.5,0.5,1), linewidth= linewidth+1)
    ax.set_xticks(ind)

    ax.set_xticklabels(keys, rotation=xlabelrotation)
    if _title: ax.set_title(_title)
    if legends : ax.legend(loc=titlelocation)
    #plt.show()
    return fig, ax
