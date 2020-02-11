from __future__ import print_function
from past.builtins import cmp
from builtins import zip
from builtins import map
from builtins import str
from builtins import range
import pandas as pd
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.collections import PolyCollection
from matplotlib.colors import colorConverter
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt
import numpy as np
import itertools

from os.path import *
import os, sys

from vplants.mangosim.devlaw_description import *

share_dir = '../../../../share/'
proba_dir = join(share_dir,'glm_output_proba/cogshall/')
#data_dir = join(share_dir,'glm_output_proba/cogshall/allfactors/interaction_glm')
#tmprep = 'interactionreport'

proba_factors = 'allfactors'
if len(sys.argv) > 1:
    for argv in sys.argv[1:]:
        if not argv.startswith('-'):
            proba_factors = argv
            print(proba_factors)
            break 

data_dir = join(proba_dir,proba_factors,'interaction_glm')
if not os.path.exists(data_dir):
    l = os.listdir(proba_dir)
    l = [d for d in l if isdir(join(proba_dir,d))]
    print('Invalid value :', repr(proba_factors))
    print('Valid values :',', '.join(map(repr,l)))
    raise ValueError(data_dir)

tmprep = 'report_'+proba_factors

periodtags = [ 'within_04', 'within_05'] #,'within_0405',]  
periodtags2 = ["between_03to0405", "between_04to05"]
periodtags3 = [ 'within_0405',]  
name = 'vegetative_burst_within_04' # 'burst_date_children_within_05' 
names1 = ['Vegetative_Burst',  #'Nb_GU_Children',
         "Has_Apical_GU_Child", "Has_Lateral_GU_Children", 'Nb_Lateral_GU_Children', 
         'Burst_Date_GU_Children'] 
#names1 += ['Burst_Delta_Date_GU_Children', 'Burst_Delta_Date_GU_Children_Poisson']
#names1 += ['Burst_Date_GU_Children_0', 'Burst_Date_GU_Children_1', 'Burst_Date_GU_Children_2']
names2 = ['Flowering', 'Nb_Inflorescences', 'Flowering_Week', 'Flowering_Week_Poisson']
names2 += ['Fruiting', 'Nb_Fruits', 'Fruit_Weight','Harvest_Week','Harvest_Week_Poisson']
names3 = [n.replace('GU','MI').replace('Vegetative','MixedInflo') for n in names1]



names = names1+names2

knowfactors = list(allfactors)


def get_factor_values(factor, data = None):
    if not data is None:
        val = data[factor].unique()
    else:
        val = factorsvalues[factor]
    return val

def get_factors_values(factors, data = None):
    return [get_factor_values(f, data) for f in factors]

monthnamemap = { 1 : 'Jan', 2 : 'Feb', 3 : 'Mar', 4 : 'Apr', 5 : 'May', 6 : 'Jun' , 7 : 'Jul', 8 : 'Aug', 9 : 'Sep' , 10 : 'Oct', 11 : 'Nov', 12 : 'Dec'}

date_factors = ['Burst_Month', 'Burst_Date_GU_Children', 'Flowering_Week']
#knowfactors += date_factors[1:]

def nb_date_factor(factors):
    return sum([f in date_factors for f in factors])

def has_date_factor(factors):
    return nb_date_factor(factors) > 0

def get_date_factors(factors):
    return [f for f in factors if f in date_factors]


def convertmonth(mvalue):
    if type(mvalue) == str:
        if '-' in mvalue: # multiple value
            return '-'.join(map(convertmonth,mvalue.split('-')))
    res = monthnamemap.get(int(mvalue), mvalue)
    return res

import vplants.mangosim.utils.util_report as ur ; reload(ur)


def sort_burst_date(data, date_factor):
    if date_factor == 'Flowering_Week': return data
    if data[date_factor].dtype.name != 'category':
        gmonthids = list(range(6,13))+list(range(6))
        monthids = np.unique(data[date_factor])
        if  monthids.dtype == object:
            def mcmp(a, b) : return cmp(gmonthids.index(int(a.split('-')[0])), gmonthids.index(int(b.split('-')[0])))
            monthids = list(monthids)
            monthids.sort(cmp = mcmp) 
            #print 'monthids :', monthids
        else:
            monthids = gmonthids
        data[date_factor] = data[date_factor].astype("category", categories=monthids)

    factors = get_factors(data)
    data = data.sort_values(factors)
    data = data.reset_index()
    del data['index']
    return data

def get_factors(data):
    lknowfactors = set(knowfactors)
    factors = []
    for factor in data.columns:
        if factor in lknowfactors:
            factors.append(factor)
    return factors

def ismultivariate(data):
    return not ('probability' in data.columns)

def multivariate_values(data):
    factors = get_factors(data)
    nbfactors = len(factors)
    return data.columns.values[nbfactors:]    

def decompose_dataframe(data):
    factors = get_factors(data)
    nbfactors = len(factors)
    criteria = list(factors)

    assert has_date_factor(criteria)

    if has_date_factor(criteria): 
        date_factor = get_date_factors(criteria)[0]
        criteria.remove(date_factor)
        data = sort_burst_date(data, date_factor)
    nbdata = len(data)
    criteria = list(reversed(criteria))

    criteriavalues = list(itertools.product(*[get_factor_values(c, data) for c in criteria]))
    for ic, cvalues in enumerate(criteriavalues):
        
        mdata = data
        label = []
        for factor, cvalue in zip(criteria,cvalues):
            label.append((factor,cvalue))
            mdata = mdata.loc[mdata[factor] == cvalue]
        
        yield label, mdata

def simplify_factor_name(name):
    nparts = name.split('_')
    return '_'.join([n[:2] for n in nparts[:min(len(nparts),2)]])


gsummary = None

def build_factor_summary(names, simplified = True):
    print(knowfactors)
    global gsummary
    summary = []
    for name in names:
        fname = join(data_dir, name+'.csv')
        if os.path.exists(fname):
            dataprob = pd.read_csv(fname)
            summary.append([name]+['X' if factor in dataprob.columns else '' for factor in knowfactors])

    summary = pd.DataFrame(summary, columns=['Variables']+(knowfactors if not simplified else list(map(simplify_factor_name,knowfactors))))
    gsummary = summary
    for v in  (knowfactors if not simplified else list(map(simplify_factor_name,knowfactors))):
        if sum(summary[v] == 'X') == 0:
            del summary[v]
    return summary



def barplot_from_date_matrix(data, name = name, valuename = 'Probability'):

    def cc(arg): return colorConverter.to_rgba(arg, alpha=0.6)
    colors = [cc('r'), cc('g'), cc('b'), cc('y')]

    factors = get_factors(data)
    nbfactors = len(factors)
    
    nbdata = len(data)

    assert nb_date_factor(factors) == 1
    date_factor = get_date_factors(factors)[0]

    fig = plt.figure()
    ax = fig.gca(projection='3d')
    for i, v in enumerate(data.iloc[:,0]):
        rowdata = data.iloc[nbdata-i-1,nbfactors:]
        ax.bar(list(range(len(rowdata))), rowdata, zs=i, zdir='y', color = colors[ i % len(colors)])

    def mxlabel(idx, labels):
        if idx % 1 == 0:    return labels.get(int(idx),'')
        else:               return ''

    def mylabel(idx, labels):
        if idx % 1 == 0:    return labels.get(nbdata-1-int(idx),'')
        else:               return ''

    if 'burst_date' in name:
        ax.set_xlabel('Month', labelpad = 10)
        xlabels = dict([(i,convertmonth(v)) for i,v in enumerate(data.columns.values[nbfactors:])])
    elif 'week' in name:
        ax.set_xlabel('Week', labelpad = 10)
        xlabels = dict([(i,v) for i,v in enumerate(data.columns.values[nbfactors:])])
    else:
        xlabels = dict([(i,v) for i,v in enumerate(data.columns.values[nbfactors:])])
    ax.set_xticklabels( [mxlabel(x,xlabels) for x in ax.get_xticks()] , ha ='center', rotation = 20 )


    ylabels = dict([(i,convertmonth(v)) for i,v in enumerate(data.values[:,factors.index(date_factor)])])
    ax.set_ylabel(date_factor, labelpad = 10)
    ax.set_yticklabels([mylabel(y,ylabels) for y in ax.get_yticks()] , ha ='center' )

    ax.set_zlabel(valuename)

    ax.view_init(elev=40,azim=-75)
    #plt.title(name)



def heat_map(data, name = name):
    factors = get_factors(data)
    nbfactors = len(factors)
    date_factor = get_date_factors(factors)[0]

    fig = plt.figure()
    ax = fig.gca()

    #ax.imshow(data.values[:,1:])    
    view = ax.matshow(data.values[:,nbfactors:].astype(float))  
    fig.colorbar(view)
    ax.set_ylabel(name, labelpad = 10)
    if 'burst_date' in name:
        ax.set_xticklabels( [0]+[convertmonth(v) for v in data.columns[nbfactors:]] )
    else:
        ax.set_xticklabels( [0]+[v for v in data.columns[nbfactors:]] )        
    ax.set_yticklabels( [0]+[convertmonth(v) for v in data.values[:,factors.index(date_factor)]] )

factor_translate = {'Burst_Month' : 'Mother Burst Month'}
value_factor_translate = {'Burst_Month' : convertmonth }

variable_translate = {'gu_burst_date_gu_children_within_04' : 'Daughter Burst Month - Cycle 4',
                      'gu_burst_date_gu_children_within_05' : 'Daughter Burst Month - Cycle 5'}

def curves_of_date(data, name = name, type = 'probability', check = False):
    assert type in ['probability', 'number', 'probabilityset', 'numberset']

    
    factors = get_factors(data)
    nbfactors = len(factors)
    criteria = list(factors)
    plotset = ('set' in type)

    if nb_date_factor(factors) > 1:
        raise ValueError('Invalid number of date factor', name, get_date_factors(factors))

    if nb_date_factor(factors) == 0 :
        if check : raise ValueError('No date factor', factors, data)
        if not plotset: 
            return False
        date_factor = None
    else:
        if plotset:
            date_factor = None
        else:
            date_factor = get_date_factors(factors)[0]
            criteria.remove(date_factor)

    fig = plt.figure()
    ax = fig.gca()

    if not date_factor is None:
        dates = sum([str(d).split('-') for d in data[date_factor].unique()],[])
        nbdates = len(dates)
        nbrepetitions = [len(str(d).split('-')) for d in data[date_factor].unique()]

    else:
        dates = sum([str(d).split('-') for d in data.columns[nbfactors:]],[])
        nbdates = len(dates)
        nbrepetitions = [len(str(d).split('-')) for d in data.columns[nbfactors:]]

    def repet(dataline):
        assert len(dataline) == len(nbrepetitions)
        return sum([[v for i in range(nb)] for v,nb in zip(dataline, nbrepetitions)],[])



    criteriavalues = list(itertools.product(*[get_factor_values(c, data) for c in criteria]))
    for cvalues in criteriavalues:
        mdata = data
        label = []
        for factor, cvalue in zip(criteria,cvalues):
            if factor == 'Burst_Month': 
                lcvalue = convertmonth(cvalue)
            else:
                lcvalue = str(cvalue)
            label.append(factor_translate.get(factor,factor)+'='+lcvalue)
            mdata = mdata.loc[mdata[factor] == cvalue]

        if plotset:
            assert len(mdata) == 1
            ax.plot(list(range(nbdates)),repet(mdata.values[0,nbfactors:]), label=','.join(label),linewidth=2)
        else:
            ax.plot(list(range(nbdates)),repet(mdata[type]), label=','.join(label),linewidth=2)

    #if plotset:
    #    plt.xticks(range(nbdates), dates) #, [monthnamemap[int(v)] for v in data.columns[nbfactors:]] )
    #else:
    print(name)
    if date_factor != 'Flowering_Week' and not 'flowering_week' in name:
        xdates = [convertmonth(v) for v in dates]
    else:
        xdates = dates
    plt.xticks(list(range(nbdates)), xdates )
    if not plotset:
        ax.set_xlabel(date_factor)
    elif name in variable_translate:
        ax.set_xlabel(variable_translate[name])        

    import numpy as np

    if type == 'probability':
        ax.set_ylabel('Probabilities')
        maxdata = np.amax(data[type])
        plt.ylim(0,max(1.03,maxdata * 1.05))
    elif type == 'probabilityset':
        ax.set_ylabel('Probabilities')        
        maxdata = np.amax(data.values[:,nbfactors:])
        plt.ylim(0,max(1.03,maxdata * 1.05))
    else:
        ax.set_ylabel('Number of Elements')

    #plt.title(name)

    return True

def savefig(fname):
    #fontP = FontProperties()
    #fontP.set_size('small')
    legend = plt.legend(loc = 8, bbox_to_anchor=(0.5,1.05))#, prop = fontP)
    if legend:
        plt.savefig(fname,  bbox_extra_artists=(legend,), bbox_inches='tight')
    else:
        plt.savefig(fname,  bbox_inches='tight')
    plt.close()
    #print 'Save', repr(fname)
  

def exists_files(fname, timestamp = None):
    from glob import glob
    existingfiles = glob(fname.replace('%s','*'))
    if len(existingfiles) > 0:
        if timestamp is None: return True
        return timestamp < os.stat(existingfiles[0]).st_mtime
    else:
        return False


class MyReportGenerator(ur.TexReportGenerator):
    def __init__(self, projectrep, fname):
        self.projectrep = projectrep
        if not exists(projectrep): os.makedirs(projectrep)
        ur.TexReportGenerator.__init__(self, join(projectrep,fname), "Report on mango development processes")

    def make_section(self, name):
        secname = name.replace('mixedinfo','from mixedinflo')
        secname = ' '.join(map(str.capitalize,secname.split('_')))
        self.add_section(secname)

    def make_subsection(self, name):
        secname = ' '.join(map(str.capitalize,name.split('_')))
        self.add_subsection(secname)

    def write_factors(self, data):
        factors = ' , '.join(get_factors(data))
        factors = factors.replace('_','\\_')
        self.write("Factors: "+factors+'\n\n')

    def add_dev_date_section_prev(self, name, dataprob, datanb, imgproba, imgnb):
        self.make_section(name)
        self.write_factors(dataprob)
        self.add_subsection('Probabilities')
        for im1, im2, caption in imgproba:
            self.add_figure([im1,im2], 'Proba.:'+caption) 
        for label, mdataprob in decompose_dataframe(dataprob):
            self.add_table(mdataprob)
        self.add_subsection('Number of elements')
        for im1, im2, caption in imgnb:
            self.add_figure([im1,im2], 'Nb. of elem.:'+caption)
        for label, mdatanb in decompose_dataframe(datanb):
            self.add_table(mdatanb)

    def add_dev_date_section(self, name, dataprob, datanb, imgproba, imgnb, withheader = True):
        if withheader:  
            self.make_section(name)
            self.write_factors(dataprob)

        iterimgprob = iter(imgproba)
        iterimgnb = iter(imgnb)
        iterprob = decompose_dataframe(dataprob)
        iternb = decompose_dataframe(datanb)

        while True:
            try:
                improb1, improb2, improb3, caption1 =  next(iterimgprob)
            except StopIteration as e:
                break
            imnb1, imnb2, imnb3, caption2 =  next(iterimgnb)
            label1, mdataprob =  next(iterprob)     
            label2, mdatanb =  next(iternb)
            if caption1:
                self.add_subsection(caption1)
            else :
                self.add_subsection('Data')
            if not improb3 is None:
                self.add_figure([improb3,imnb3], 'Curve Representation')
            self.add_figure([improb1,improb2], 'Probabilities')
            self.add_figure([imnb1,imnb2], 'Number of elements')
            self.add_table(mdataprob, 'Probabilities')
            self.add_table(mdatanb,   'Number of elements')
            self.clear_page()

        # for im1, im2, caption in imgproba:
        # self.add_subsection('Probabilities')
        #     self.add_figure([im1,im2], 'Proba.:'+caption) 
        # for label, mdataprob in decompose_dataframe(dataprob):
        #     self.add_table(mdataprob)
        # self.add_subsection('Number of elements')
        # for im1, im2, caption in imgnb:
        #     self.add_figure([im1,im2], 'Nb. of elem.:'+caption)
        # for label, mdatanb in decompose_dataframe(datanb):
        #     self.add_table(mdatanb)

    def add_dev_proba_section(self, name, data, imgs, otherdata = None):
        self.make_section(name)
        self.write_factors(data)
        self.add_subsection('Probabilities')
        if len(imgs) > 0:
            self.add_figure(imgs) #,'Probabilities plot')
        datas = [data]
        if not otherdata is None: datas.append(otherdata)
        for data in datas:
            if len(data) > 40:
                factors = get_factors(data)
                print(factors)
                values = data[factors[0]].unique()
                for value in values:
                    data1 = data.loc[data[factors[0]] == value]
                    data1= data1.sort_values(factors[1:])
                    self.add_table(data1,'Probabilities and number of elements')
            else:
                self.add_table(data,'Probabilities and number of elements')

    def add_summary(self, summary = None):
        if summary : 
            datasummary, glmsummary = summary.split('[1] "******** GLM  *************"\n')
            glmsumlines = glmsummary.splitlines()
            init = 2
            if '[1] "Initial' in glmsumlines[1] : init+=1
            del glmsumlines[init:init+4];
            glmsummary = '\n'.join(glmsumlines)
            # self.add_subsection('Data Summary')
            # self.write('{\\small\n')
            # self.write('\\begin{verbatim}\n')
            # self.write(datasummary[datasummary.find('\n')+1:])
            # self.write('\n\\end{verbatim}\n')
            # self.write('}\n')
            self.add_subsection('GLM Summary')
            self.write('{\\small\n')
            self.write('\\begin{verbatim}\n')
            linemaxsize = 110
            for glmsumline in glmsumlines:
                while len(glmsumline) > linemaxsize:
                    self.write(glmsumline[:linemaxsize]+'\n')
                    glmsumline = glmsumline[linemaxsize:]
                self.write(glmsumline+'\n')
            #self.write(glmsummary)
            self.write('\n\\end{verbatim}\n')
            self.write('}\n')

    def make_dev_date_section(self, name, dataprob, regenerateall = False):
        fname = join(data_dir, name+'.csv')
        if dataprob is None:
            dataprob = pd.read_csv(fname)
        timestamp = os.stat(fname).st_mtime
        datanb = pd.read_csv(join(data_dir, name+'_nbelements.csv'))

        def gen_img(data, type = 'prob', title = 'Probability'):            
            img = []
            barplotfname = join(self.projectrep,name+'_'+type+'_bar_%s.png')
            heatmapfname = join(self.projectrep, name+'_'+type+'_heatmap_%s.png')
            datecurvefname = join(self.projectrep, name+'_'+type+'_datecurves_%s.png')
            i = 0
            for label, mdata in decompose_dataframe(data):
                labelstr = str(i).zfill(3) #('_'.join([l+'='+str(v) for l,v in label]))
                lbarplotfname = barplotfname % labelstr
                if not exists_files(lbarplotfname, timestamp) or regenerateall:
                    barplot_from_date_matrix(mdata, name, title)
                    savefig( lbarplotfname )
                lheatmapfname = heatmapfname % labelstr
                if not exists_files(lheatmapfname, timestamp) or regenerateall:
                    heat_map(mdata, name)
                    savefig( lheatmapfname )
                ldatecurvefname = datecurvefname % labelstr
                if not exists_files(ldatecurvefname, timestamp) or regenerateall:
                    hasfig = curves_of_date(mdata, name, 'probabilityset' if type == 'prob' else 'numberset', True)
                    if not hasfig is None: savefig( ldatecurvefname )
                    else : lbarplotfname = None
                img.append(( os.path.basename(lbarplotfname),
                             os.path.basename(lheatmapfname), 
                             os.path.basename(ldatecurvefname) if lbarplotfname else lbarplotfname, 
                             (' , '.join([l.replace('_','\\_')+'='+str(v) for l,v in label])) ))
                i += 1

            return img
        
        factors = get_factors(dataprob)
        nbfactors = len(factors)
        matrixplot = ('Burst_Month' in dataprob.columns) and nbfactors >= 2

        imgs = []
        probcurvefname = join(self.projectrep,name+'_prob.png')
        if not exists_files(probcurvefname, timestamp) or regenerateall:
            curves_of_date(dataprob, name, 'probabilityset')
            savefig(probcurvefname)
        imgs.append(os.path.basename(probcurvefname))

        nbcurvefname = join(self.projectrep,name+'_nbelem.png')
        if not exists_files(nbcurvefname, timestamp) or regenerateall:
            curves_of_date(datanb, name, 'numberset')
            savefig(nbcurvefname)
        imgs.append(os.path.basename(nbcurvefname))
        self.add_dev_proba_section(name, dataprob, imgs, datanb)

        self.clear_page()

        if matrixplot:
            imgproba = gen_img(dataprob)
            imgnb = gen_img(datanb, 'nbelem', 'Number of Elements')

            self.add_dev_date_section(name, dataprob, datanb, imgproba, imgnb, False)

        #self.clear_page()
        summary_file = join(data_dir, name+'_summary.txt')
        if (os.path.exists(summary_file)):
            summary = file(summary_file,'r').read()
            self.add_summary(summary)
        self.clear_page()

    def make_dev_proba_section(self, name, dataprob = None, regenerateall = False):
        fname = join(data_dir, name+'.csv')
        if dataprob is None:
            dataprob = pd.read_csv(fname)
        timestamp = os.stat(fname).st_mtime

        if has_date_factor(dataprob.columns): 
            date_factor = get_date_factors(dataprob.columns)[0]
            dataprob = sort_burst_date(dataprob, date_factor)

        factors = get_factors(dataprob)
        nbfactors = len(factors)

        #print 'make_dev_proba_section'
        imgs = []
        probcurvefname = join(self.projectrep,name+'_prob.png')
        if not exists_files(probcurvefname, timestamp) or regenerateall :
            if curves_of_date(dataprob, name, 'probability'):
                savefig(probcurvefname)
                imgs.append(os.path.basename(probcurvefname))
        else:
            imgs.append(os.path.basename(probcurvefname))

        nbcurvefname = join(self.projectrep,name+'_nbelem.png')
        if not exists_files(nbcurvefname, timestamp) or regenerateall :
            if curves_of_date(dataprob, name, 'number'):
                savefig(nbcurvefname)
                imgs.append(os.path.basename(nbcurvefname))
        else:
            imgs.append(os.path.basename(nbcurvefname))

        self.add_dev_proba_section(name, dataprob, imgs)
        #if len(imgs) :self.clear_page()

        summary_file = join(data_dir, name+'_summary.txt')
        if (os.path.exists(summary_file)):
            summary = file(summary_file,'r').read()
            self.add_summary(summary)
        self.clear_page()

    def make_dev_section(self, name, regenerateall = False):
        fname = join(data_dir, name+'.csv')
        if os.path.exists(fname):
            dataprob = pd.read_csv(fname)
            if ismultivariate(dataprob) :
                self.make_dev_date_section(name, dataprob, regenerateall)
            else:
                self.make_dev_proba_section(name, dataprob, regenerateall)
        else:
            print("File '"+fname+"' does not exist.")
            self.make_section(name)
            self.write('No data\n\n')


    def make_factor_summary_section(self, names):
        summary = build_factor_summary(names)
        self.add_section('Factors Summary')
        self.write('{\\small\n')
        self.add_table(summary)
        self.write('}\n')
        for factor in knowfactors:
            self.write(simplify_factor_name(factor).replace('_','\\_')+' : '+factor.replace('_','\\_')+'\n\n')
        self.clear_page()

    def make_intro(self, periodtags):
        self.add_section('Data Summary')
        for tag in periodtags:            
            fname = join(data_dir, pardir, 'info_table_'+tag+'.txt')
            if os.path.exists(fname):
                summary = file(fname,'r').read()
                self.make_subsection(tag)
                self.write('{\\footnotesize\n')
                self.write('\\begin{verbatim}\n')
                self.write(summary)
                self.write('\n\\end{verbatim}\n')
                self.write('}\n')
            else:
                print('Cannot find',repr(fname))

    def make_sections(self,names, regenerateall= False):
        self.make_factor_summary_section(names)

        for name in names:
            print('process', name)
            self.make_dev_section(name, regenerateall = regenerateall)

    def make_sections_parallel(self,names, regenerateall= False):
        self.make_factor_summary_section(names)
        import multiprocessing
        num_cores = multiprocessing.cpu_count()
        pool = multiprocessing.Pool(num_cores)

        def makesection(regenerateall= False):
            def msection(name):
                return self.make_dev_section(name, regenerateall = regenerateall)
            return msection
        msect = makesection(regenerateall = regenerateall)

        pool.map(msect, names)


    def clear_tmpfiles(self):
        assert self.texstream is None
        if exists(self.projectrep):
            import shutil
            shutil.rmtree(self.projectrep)        

def process(regenerateall = False, fastcompilation = None):
    if regenerateall and exists(tmprep):
        import shutil
        shutil.rmtree(tmprep)

    name = 'mango_development_report.tex'
    if proba_factors != 'allfactors' :
        name = 'mango_development_report_'+proba_factors+'.tex'
    texstream = MyReportGenerator(tmprep, name)

    texstream.make_intro(periodtags+periodtags2)

    mnames = []
    'Within GU : veg and rep'
    mnames += list(itertools.product(periodtags,['gu_'+n.lower() for n in names]))
    'Within MI : veg'
    mnames += list(itertools.product(periodtags3,['mi_'+n.lower() for n in names1]))
    'Between GU : veg'
    mnames += list(itertools.product(periodtags2,['gu_'+n.lower() for n in names1]))
    'Between GU : mi'
    mnames += list(itertools.product(periodtags2,['gu_'+n.lower() for n in names3]))
    mnames = [n+'_'+y for y,n in mnames]

    texstream.make_sections(mnames, regenerateall)

    texstream.close()
    #texstream.viewtex()
    texstream.compile(fastcompilation)
    texstream.view()
    return texstream


if __name__ == '__main__':
    # data = pd.read_csv(join(data_dir, name+'.csv'))
    # #curves_of_date(data, name)
    # #plt.show()
    # #plt.close()
    # curves_of_date(data, name, 'number')
    # plt.show()
    # plt.close()

    texstream = process('-f' in sys.argv)