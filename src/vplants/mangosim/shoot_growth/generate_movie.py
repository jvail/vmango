
workingrep    = 'images'
lsysfile      = 'mango_mtg_replay.lpy'
tempbgeomfile = 'temp_scene.bgeom'
bgeomfile     = 'scene_{}.bgeom'
povfile       = 'scene_{}.pov'
mainpovfile   = 'image_{}.pov'
imgfile       = 'image_{}.png'
stepfile      = 'simu_nbsteps.txt'
imageresolution = 1920, 1080
povcomdline   = 'povray -I{} -O{} +FN +W{} +H{} +A -D'

from multiprocessing import cpu_count
import os
from os.path import join



def _generate(params):
    p, nbpovprocess = params
    os.system('python generate_movie.py --process '+str(p)+' '+str(nbpovprocess))

def generate_movie(nbpovprocess):
    import time
    init = time.time()
    from multiprocessing import Pool

    paramvalueslist = [(i,nbpovprocess) for i in range(-1,nbpovprocess)]
    #print paramvalueslist
    # _generate(paramvalueslist[2])
    pool = Pool(processes=nbpovprocess+1)
    pool.map(_generate,paramvalueslist)
    os.remove(stepfile)
    print 'Computed in ',time.time() - init,'sec ...'



def generate_bgeom():
    from openalea.lpy import Lsystem
    import os
    print 'Scene generator launched'
    l = Lsystem(lsysfile,{'RESOLUTION' : 2, 'daystep' : 1, 'TIMEBAR' : False})
    nbsteps =  l.derivationLength
    open(stepfile,'w').write(str(nbsteps))
    if not os.path.exists(workingrep) : os.makedirs(workingrep)
    for step in xrange(nbsteps):
        if step == 0: lstring = l.derive(1)
        else: lstring = l.derive(lstring,step,1)
        lscene = l.sceneInterpretation(lstring)
        fname = join(workingrep, bgeomfile.format(str(step).zfill(4)))
        lscene.save(tempbgeomfile)
        os.rename(tempbgeomfile,fname)
        print "Scene",step,"generated ..."

def wait_for_file(fname, timeout = 60):
    import time
    c = time.time()
    while not os.path.exists(fname) and (time.time() - c) < timeout:
        time.sleep(.05)
    if not os.path.exists(fname) : raise ValueError(fname)
    time.sleep(.05)



mainpovtemplate = '''
#ifndef (__camera_definition__)
#declare __camera_definition__ = true;

camera {
   perspective
    location <335,-20,105>
    direction <-1,-0,-0>
    up <0,0,1>
    right <0,%i/%i,0>
    rotate <0,8,38>
}

light_source {
     <120,  90,500>
    color rgb <1,1,1>
}



light_source {
     <379.656,291.526,52.1352>
    color rgb 0.1
}



background { color rgb 1 }

plane { <0,0,1> 0 
        clipped_by { plane {<-1,0,0> 200     rotate <0,8,38>}}
        texture {
            pigment {
               color rgb 1
            }
            finish {
              ambient 0.4
              diffuse 1
              specular 0
            }

       }
}

#end // __camera_definition__

#include "%s"
'''


def generate_pov(i=0,nbpovprocess=1):
    """ generate ith images modulo nbpovprocess """
    print 'Image generator ',i,' launched'
    from openalea.plantgl.all import Scene, Tesselator, PovFilePrinter
    wait_for_file(stepfile)
    nbsteps = int(open(stepfile,'r').read())
    step = i
    os.chdir(workingrep)
    while step < nbsteps:
        bgeomfname = bgeomfile.format(str(step).zfill(4))
        wait_for_file(bgeomfname)
        lscene = Scene(bgeomfname)
        for sh in lscene:
            sh.setComputedName()
        steppovfile = povfile.format(str(step).zfill(4))
        stepimgfile = imgfile.format(str(step).zfill(4))
        lscene.save(steppovfile)
        #tess = Tesselator()
        #printer = PovFilePrinter(steppovfile,tess)
        #lscene.apply(printer)
        if os.path.exists(steppovfile):
            mpovtext = mainpovtemplate % (imageresolution[0],imageresolution[1],steppovfile)
            mpovfile = mainpovfile.format(step)
            file(mpovfile,'w').write(mpovtext)
            cmd = povcomdline.format(mpovfile, stepimgfile, imageresolution[0], imageresolution[1])
            print
            print i,'>>>',cmd
            print 'Image ',step,'computed ...'
            os.system(cmd)
            if os.path.exists(stepimgfile):
                os.remove(mpovfile)
                os.remove(steppovfile)
                os.remove(bgeomfname)
            else:
                print 'Error with image', step
        else: print 'Error with image', step
        step += nbpovprocess


def main():
    import sys
    processflag = '--process'
    numjobflag  =  '-j'
    if processflag  in sys.argv:
        pfi = sys.argv.index(processflag)
        numproc = int(sys.argv[pfi+1])
        nbpovprocess = int(sys.argv[pfi+2])
        if numproc == -1: generate_bgeom()
        else : generate_pov(numproc,nbpovprocess)
    elif numjobflag in sys.argv:
        njf = sys.argv.index(numjobflag)
        if len(sys.argv) > njf+1:
            nbpovprocess = int(sys.argv[njf+1])
        else:
            nbpovprocess = cpu_count() - 1
        generate_movie(nbpovprocess)
    elif '--bgeom' in sys.argv:
        generate_bgeom()
    elif len(sys.argv) > 1:
        raise ValueError(sys.argv[1:])
    else:
        generate_bgeom()
        generate_pov()

if __name__ == '__main__':
    main()

