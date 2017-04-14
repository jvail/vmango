from vplants.mangosim.state import *

restrictions = [None, eBurstDateRestriction, ePositionARestriction, ePositionAncestorARestriction, eNatureFRestriction, eAllRestriction]
restrictionLabels = ['All Factors', 'Without Burst Date', 'Without Position','Without Ancestor Position', 'Without Ancestor Fate', 'With No Factor']

restrictionLabels = dict(zip(restrictions,restrictionLabels))

def plot_histo(keys, allvalues, _title = None, reference = None, legendtag = None, linestyle = '-', titlelocation = 2):
    import matplotlib.pyplot as plt
    import numpy as np
    fig, ax = plt.subplots(figsize=(10,8))
    nbplot = len(allvalues)
    nbx = len(allvalues[0])
    width = 1

    ind = np.arange(0,nbx*width,width)+width
    #if reference : ax.bar(ind-width/4., reference, width/2., color='r' )
    bpdata = [[v[i] for v in allvalues] for i in range(nbx)]
    ax.boxplot(bpdata, widths=0.5)
    ax.set_xticks(ind)

    # 'k--'
    if reference:
        plt.plot(ind, reference, linestyle, color=(0.5,0.5,0.5,1),  label = 'Reference', linewidth= 3)
    legend = str(len(allvalues))+' Simulations'
    if legendtag:
        g,r = legendtag
        legend += ' '+ restrictionLabels[r]
    plt.plot(ind, [np.mean([v[i] for v in allvalues]) for i in xrange(nbx)], linestyle, color='k', label = legend, linewidth= 2)
    #plt.ylim(0,1500)

    ax.set_xticklabels(keys, rotation=90)
    if _title: ax.set_title(_title)
    #fig.set_size_inches(1600,800)
    ax.legend(loc=titlelocation)
    return fig, ax
    #plt.show()


def plot_histos(keys, allvaluesset, _title = None, reference = None, legends = None, legendtags = None, titlelocation = 2):
    import matplotlib.pyplot as plt
    import matplotlib.patches as mpatches

    import numpy as np
    fig, ax = plt.subplots(figsize=(15,5))
    nbplot = len(allvaluesset)
    nbx = len(allvaluesset[0][0])
    uwidth = 1
    width = (nbplot+1) * uwidth * 1.2
    if legendtags:
        if legends:
            legends = [restrictionLabels[lt[1]]+ ':'+leg  for leg, lt in zip(legends,legendtags)]
        else:
            legends = [restrictionLabels[r] for g,r in legendtags]
    
    listcolors = list(reversed(['r','y','b','c','m','k']))
    colors = lambda x: listcolors[x]

    indg = np.arange(0,nbx*width,width)+width

    legend_patches = []
    if reference:
        refhandle, = plt.plot(indg, reference, 'o', color=(0.5,0.5,0.5,1),  label = 'Reference', linewidth= 3)
        legend_patches.append(refhandle)
    
    for i,allvalues in enumerate(allvaluesset):
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
    ax.set_xticklabels(keys, rotation=90)
    if _title: ax.set_title(_title)
    #fig.set_size_inches(1600,800)
    ax.legend(handles=legend_patches, loc=titlelocation)
    return fig, ax
    #plt.show()

def plot_histos_means(keys, allvalues, _title = None,  reference = None, legends = None, legendtags = None, linewidth = 1, linestyle = '-o', titlelocation = 1):
    import matplotlib.pyplot as plt
    import numpy as np
    if legendtags:
        if legends:
            legends = [restrictionLabels[lt[1]]+ ':'+leg  for leg, lt in zip(legends,legendtags)]
        else:
            legends = [restrictionLabels[r] for g,r in legendtags]
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
    for i,values in enumerate(allvalues):
        ax.plot(ind, values, linestyle, color=colors(i), linewidth = linewidth, label = '' if legends is None else 'GLM '+legends[i] )
    if reference:
        plt.plot(ind, reference, linestyle , color=(0.5,0.5,0.5,1), label = 'Reference', linewidth= linewidth+1)
    ax.set_xticks(ind)

    ax.set_xticklabels(keys, rotation=90)
    if _title: ax.set_title(_title)
    if legends : ax.legend(loc=titlelocation)
    #plt.show()
    return fig, ax


