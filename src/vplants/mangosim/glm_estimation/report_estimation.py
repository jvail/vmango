import pandas as pd
from mpl_toolkits.mplot3d import Axes3D
from matplotlib.collections import PolyCollection
from matplotlib.colors import colorConverter
from matplotlib.font_manager import FontProperties
import matplotlib.pyplot as plt
import numpy as np
import itertools

from os.path import *
import os

from vplants.mangosim.devlaw_description import *

share_dir = '../../../../share/'
data_dir = join(share_dir,'glm_output_proba2/cogshall/selected_glm')
periodtags = [ 'within_04', 'within_05','within_0405',]  
periodtags2 = ["04to05", "03to0405"]
name = 'vegetative_burst_within_04' # 'burst_date_children_within_05' 
names1 = ['Vegetative_Burst',  
         "Has_Apical_GU_Child", "Has_Lateral_GU_Children", 'Nb_Lateral_GU_Children', 
         'Burst_Date_Children'] 
names2 = ['Burst_Delta_Date_Children', 'Burst_Delta_Date_Children_Poisson', 
         'Flowering', 'Nb_Inflorescences', 'Flowering_Week', 'Fruiting', 'Nb_Fruits'
         ]
names = names1+names2

tmprep = 'tmp'
knowfactors = list(allfactors)
knowfactors.remove('Tree_Fruit_Load')
knowfactors.append("Cycle")
factorsvalues['Cycle'] = [4,5]

def get_factor_values(factor):
    return factorsvalues[factor]

def get_factors_values(factors):
    return [get_factor_values(f) for f in factors]

monthnamemap = { 1 : 'Jan', 2 : 'Feb', 3 : 'Mar', 4 : 'Apr', 5 : 'May', 6 : 'Jun' , 7 : 'Jul', 8 : 'Aug', 9 : 'Sep' , 10 : 'Oct', 11 : 'Nov', 12 : 'Dec'}

import util_report as ur ; reload(ur)

def sort_burst_date(data):
    if data.Burst_Month.dtype.name != 'category':
        monthids = range(6,13)+range(6)
        data.Burst_Month = data.Burst_Month.astype("category", categories=monthids)

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
    if 'Burst_Month' in criteria: 
        criteria.remove('Burst_Month')
        data = sort_burst_date(data)
    nbdata = len(data)

    criteriavalues = list(itertools.product(*[get_factor_values(c) for c in criteria]))
    for ic, cvalues in enumerate(criteriavalues):
        
        mdata = data
        label = []
        for factor, cvalue in zip(criteria,cvalues):
            label.append((factor,cvalue))
            mdata = mdata.loc[mdata[factor] == cvalue]
        
        yield label, mdata

def simplify_factor_name(name):
    return '_'.join([n[:3] for n in name.split('_')])


gsummary = None

def build_factor_summary(names, simplified = True):
    global gsummary
    summary = []
    for name in names:
        fname = join(data_dir, name+'.csv')
        if os.path.exists(fname):
            dataprob = pd.read_csv(fname)
            summary.append([name]+['X' if factor in dataprob.columns else '' for factor in knowfactors])

    summary = pd.DataFrame(summary, columns=['Variables']+(knowfactors if not simplified else map(simplify_factor_name,knowfactors)))
    gsummary = summary
    for v in  (knowfactors if not simplified else map(simplify_factor_name,knowfactors)):
        if sum(summary[v] == 'X') == 0:
            del summary[v]
    return summary

def barplot_from_date_matrix(data, name = name, valuename = 'Probability'):

    def cc(arg): return colorConverter.to_rgba(arg, alpha=0.6)
    colors = [cc('r'), cc('g'), cc('b'), cc('y')]

    factors = get_factors(data)
    nbfactors = len(factors)
    
    nbdata = len(data)

    assert 'Burst_Month' in factors 


    fig = plt.figure()
    ax = fig.gca(projection='3d')

    for i, v in enumerate(data.iloc[:,0]):
        rowdata = data.iloc[i,nbfactors:]
        ax.bar(range(len(rowdata)), rowdata, zs=nbdata-i, zdir='y', color = colors[ i % len(colors)])

    if 'burst_date' in name:
        ax.set_xlabel('Month')
        ax.set_xticklabels( [monthnamemap.get(int(v),str(v)) for v in data.columns.values[nbfactors:]] )
    elif 'week' in name:
        ax.set_xlabel('Week')
        ax.set_xticklabels( data.columns.values[nbfactors:] )
    else:
        ax.set_xticklabels( data.columns.values[nbfactors:] )


    ax.set_ylabel('Burst_Month')
    ax.set_yticklabels( reversed([monthnamemap[int(v)] for v in data.values[:,0]]) )
    ax.set_zlabel(valuename)
    ax.view_init(elev=40,azim=-75)
    plt.title(name)



def heat_map(data, name = name):
    factors = get_factors(data)
    nbfactors = len(factors)

    fig = plt.figure()
    ax = fig.gca()

    #ax.imshow(data.values[:,1:])    
    view = ax.matshow(data.values[:,nbfactors:].astype(float))  
    fig.colorbar(view)
    plt.title(name)
    ax.set_xticklabels( [0]+[monthnamemap.get(int(v),str(v)) for v in data.columns[nbfactors:]] )
    ax.set_yticklabels( [0]+[monthnamemap[int(v)] for v in data.values[:,0]] )


def curves_of_date(data, name = name, type = 'probability'):
    assert type in ['probability', 'number', 'probabilityset', 'numberset']

    
    factors = get_factors(data)
    nbfactors = len(factors)
    criteria = list(factors)
    plotset = ('set' in type)

    if 'Burst_Month' in criteria:
        criteria.remove('Burst_Month')
    else:
        if not plotset: return False

    fig = plt.figure()
    ax = fig.gca()

    criteriavalues = list(itertools.product(*[get_factor_values(c) for c in criteria]))
    for cvalues in criteriavalues:
        mdata = data
        label = []
        for factor, cvalue in zip(criteria,cvalues):
            label.append(factor+'=='+str(cvalue))
            mdata = mdata.loc[mdata[factor] == cvalue]

        if plotset:
            assert len(mdata) == 1
            ax.plot(range(len(data.columns[nbfactors:])),mdata.values[0,nbfactors:], label=','.join(label))
        else:
            ax.plot(range(len(mdata['Burst_Month'])),mdata[type], label=','.join(label))

    if plotset:
        plt.xticks(range(len(data.columns[nbfactors:])), data.columns[nbfactors:]) #, [monthnamemap[int(v)] for v in data.columns[nbfactors:]] )
    else:
        plt.xticks(range(len(mdata['Burst_Month'])), [monthnamemap[int(v)] for v in mdata['Burst_Month']] )

    import numpy as np

    ax.set_xlabel('Burst Date')
    if type == 'probability':
        ax.set_ylabel('Probabilities')
        maxdata = np.amax(data[type])
        plt.ylim(0,max(1.1,maxdata * 1.1))
    elif type == 'probabilityset':
        ax.set_ylabel('Probabilities')        
    else:
        ax.set_ylabel('Number of Elements')

    plt.title(name)

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
    print 'Save', repr(fname)
  

def exists_files(fname):
    from glob import glob
    return len(glob(fname.replace('%s','*'))) > 0

class MyReportGenerator(ur.TexReportGenerator):
    def __init__(self, projectrep, fname):
        self.projectrep = projectrep
        if not exists(projectrep): os.makedirs(projectrep)
        ur.TexReportGenerator.__init__(self, join(projectrep,fname), "Report on mango development processes")

    def make_section(self, name):
        secname = ' '.join(map(str.capitalize,name.split('_')))
        self.add_section(secname)

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

    def add_dev_date_section(self, name, dataprob, datanb, imgproba, imgnb):
        self.make_section(name)
        self.write_factors(dataprob)

        iterimgprob = iter(imgproba)
        iterimgnb = iter(imgnb)
        iterprob = decompose_dataframe(dataprob)
        iternb = decompose_dataframe(datanb)

        while True:
            try:
                improb1, improb2, caption1 =  iterimgprob.next()
            except StopIteration, e:
                break
            imnb1, imnb2, caption2 =  iterimgnb.next()
            label1, mdataprob =  iterprob.next()     
            label2, mdatanb =  iternb.next()
            if caption1:
                self.add_subsection(caption1)
            else :
                self.add_subsection('Data')
            self.add_figure([improb1,improb2], 'Probabilities')
            self.add_figure([improb1,improb2], 'Number of elements')
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
            gs = glmsummary.splitlines()
            del gs[1];del gs[1];del gs[1];del gs[1]
            glmsummary = '\n'.join(gs)
            # self.add_subsection('Data Summary')
            # self.write('{\\small\n')
            # self.write('\\begin{verbatim}\n')
            # self.write(datasummary[datasummary.find('\n')+1:])
            # self.write('\n\\end{verbatim}\n')
            # self.write('}\n')
            self.add_subsection('GLM Summary')
            self.write('{\\small\n')
            self.write('\\begin{verbatim}\n')
            self.write(glmsummary)
            self.write('\n\\end{verbatim}\n')
            self.write('}\n')

    def make_dev_date_section(self, name, dataprob, regenerateall = False):
        if dataprob is None:
            dataprob = pd.read_csv(join(data_dir, name+'.csv'))
        datanb = pd.read_csv(join(data_dir, name+'_nbelements.csv'))
        #if 'Burst_Month' in dataprob.columns:
        #    dataprob = sort_burst_date(dataprob)
        #    datanb = sort_burst_date(datanb)

        def gen_img(data, type = 'prob', title = 'Probability'):            
            img = []
            barplotfname = join(self.projectrep,name+'_'+type+'_bar_%s.png')
            heatmapfname = join(self.projectrep, name+'_'+type+'_heatmap_%s.png')
            i = 0
            for label, mdata in decompose_dataframe(data):
                labelstr = str(i).zfill(3) #('_'.join([l+'='+str(v) for l,v in label]))
                lbarplotfname = barplotfname % labelstr
                if not exists_files(lbarplotfname) or regenerateall:
                    barplot_from_date_matrix(mdata, name, title)
                    savefig( lbarplotfname )
                lheatmapfname = heatmapfname % labelstr
                if not exists_files(lheatmapfname) or regenerateall:
                    heat_map(mdata, name)
                    savefig( lheatmapfname )
                img.append(( os.path.basename(lbarplotfname),
                             os.path.basename(lheatmapfname), 
                             (' , '.join([l.replace('_','\\_')+'='+str(v) for l,v in label])) ))
                i += 1

            return img
        
        matrixplot = 'Burst_Month' in dataprob.columns

        if matrixplot:
            imgproba = gen_img(dataprob)
            imgnb = gen_img(datanb, 'nbelem', 'Number of Elements')

            self.add_dev_date_section(name, dataprob, datanb, imgproba, imgnb)
        else:
            imgs = []
            probcurvefname = join(self.projectrep,name+'_prob.png')
            if not exists_files(probcurvefname) or regenerateall:
                curves_of_date(dataprob, name, 'probabilityset')
                savefig(probcurvefname)
            imgs.append(os.path.basename(probcurvefname))

            nbcurvefname = join(self.projectrep,name+'_nbelem.png')
            if not exists_files(nbcurvefname) or regenerateall:
                curves_of_date(datanb, name, 'numberset')
                savefig(nbcurvefname)
            imgs.append(os.path.basename(nbcurvefname))
            self.add_dev_proba_section(name, dataprob, imgs, datanb)

        #self.clear_page()

        summary = file(join(data_dir, name+'_summary.txt'),'r').read()
        self.add_summary(summary)
        self.clear_page()

    def make_dev_proba_section(self, name, dataprob = None, regenerateall = False):
        if dataprob is None:
            dataprob = pd.read_csv(join(data_dir, name+'.csv'))
        if 'Burst_Month' in dataprob.columns:
            dataprob = sort_burst_date(dataprob)

        factors = get_factors(dataprob)
        nbfactors = len(factors)

        print 'make_dev_proba_section'
        imgs = []
        probcurvefname = join(self.projectrep,name+'_prob.png')
        if not exists_files(probcurvefname) or regenerateall:
            if curves_of_date(dataprob, name, 'probability'):
                savefig(probcurvefname)
                imgs.append(os.path.basename(probcurvefname))
        else:
            imgs.append(os.path.basename(probcurvefname))

        nbcurvefname = join(self.projectrep,name+'_nbelem.png')
        if not exists_files(nbcurvefname) or regenerateall:
            if curves_of_date(dataprob, name, 'number'):
                savefig(nbcurvefname)
                imgs.append(os.path.basename(nbcurvefname))
        else:
            imgs.append(os.path.basename(nbcurvefname))

        self.add_dev_proba_section(name, dataprob, imgs)
        #if len(imgs) :self.clear_page()

        summary = file(join(data_dir, name+'_summary.txt'),'r').read()
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

    def make_sections(self,names, regenerateall= False):
        self.make_factor_summary_section(names)
        for name in names:
            print 'process', name
            self.make_dev_section(name, regenerateall = regenerateall)

    def clear_tmpfiles(self):
        assert self.texstream is None
        if exists(self.projectrep):
            import shutil
            shutil.rmtree(self.projectrep)        

def process(regenerateall = False):
    if regenerateall and exists(tmprep):
        import shutil
        shutil.rmtree(tmprep)

    texstream = MyReportGenerator(tmprep, 'mango_development_report.tex')

    mnames = []
    mnames += list(itertools.product(periodtags,[n.lower() for n in names]))
    mnames += list(itertools.product(periodtags2,[n.lower() for n in names1]))
    mnames = [n+'_'+y for y,n in mnames]

    texstream.make_sections(mnames, regenerateall)

    texstream.close()
    #texstream.viewtex()
    texstream.compile()
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

    texstream = process(False)