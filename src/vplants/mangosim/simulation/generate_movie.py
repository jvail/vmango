from __future__ import print_function

from builtins import str
from builtins import map
from builtins import range
workingrep    = 'images2'
lsysfile      = 'mango_simulation.lpy'
tempbgeomfile = 'temp_scene.bgeom'
bgeomfile     = 'scene_{}.bgeom'
povfile       = 'scene_{}.pov'
mainpovfile   = 'image_{}.pov'
imgfile       = 'image_{}.png'
stepfile      = 'simu_nbsteps.txt'
#imageresolution = 1920, 1080
imageresolution = 1280, 720
povcomdline   = 'povray -I{} -O{} +FN +W{} +H{} +A -D -GA +GF'

from multiprocessing import cpu_count
import os
from os.path import join, basename



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
    #os.remove(stepfile)
    print('Computed in ',time.time() - init,'sec ...')



def generate_bgeom(step = None, endstep = None):
    from openalea.lpy import Lsystem
    import os
    print('Scene generator launched')
    l = Lsystem(lsysfile,{'SEED' : 0, 'TREE' : 0, 'RESOLUTION' : 2, 'TIMESTEP' : 1, 'TIMEBAR' : False, 
                          'LEAFY' : True, 'WITH_INFLO' : True, 'TEXTURE' : True, 'GENERALIZEDCYLINDER' : True, 
                          'WITH_GLM' : True, 'FRUIT_MODEL' : False, 'GLM_RESTRICTION' : None, '_GLM_TYPE' : 3,
                          'EXPORT_TO_MTG' : False})

    if step is None:
        firststep = 0
        endstep =  l.derivationLength
    else:
        firststep = step
        if endstep is None:
            endstep =  l.derivationLength
        else:
            assert endstep < l.derivationLength

    open(stepfile,'w').write(str(endstep))
    if not os.path.exists(workingrep) : os.makedirs(workingrep)
    for step in range(firststep, endstep):
        if step == firststep: lstring = l.derive(firststep+1)
        else: lstring = l.derive(lstring,step,1)
        lscene = l.sceneInterpretation(lstring)
        fname = join(workingrep, bgeomfile.format(str(step).zfill(4)))
        lscene.save(tempbgeomfile)
        os.rename(tempbgeomfile,fname)
        print("Scene",step,"generated ...")

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
    jitter
}



light_source {
     <379.656,291.526,52.1352>
    color rgb 0.1
    jitter
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

union {
    #include "%s"
    rotate <0,0,%i>
}
'''


def generate_pov(i=0,nbpovprocess=1, maxstep = None):
    """ generate ith images modulo nbpovprocess """
    print('Image generator ',i,' launched')
    from openalea.plantgl.all import Scene, Tesselator, PovFilePrinter
    print('Look at',stepfile)
    wait_for_file(stepfile)
    if maxstep is None:
        maxstep = int(open(stepfile,'r').read())
    step = i
    os.chdir(workingrep)
    while step < maxstep:
        bgeomfname = bgeomfile.format(str(step).zfill(4))
        steppovfile = povfile.format(str(step).zfill(4))
        stepimgfile = imgfile.format(str(step).zfill(4))
        if os.path.exists(stepimgfile):
            print('Image ',step,'already computed ...')
            step += nbpovprocess
            continue
        wait_for_file(bgeomfname)
        lscene = Scene(bgeomfname)
        for sh in lscene:
            sh.setComputedName()
        lscene.save(steppovfile)
        #tess = Tesselator()
        #printer = PovFilePrinter(steppovfile,tess)
        #lscene.apply(printer)
        if os.path.exists(steppovfile):
            mpovtext = mainpovtemplate % (imageresolution[0],imageresolution[1],steppovfile, step)
            mpovfile = mainpovfile.format(str(step).zfill(4))
            file(mpovfile,'w').write(mpovtext)
            cmd = povcomdline.format(mpovfile, stepimgfile, imageresolution[0], imageresolution[1])
            print()
            print(i,'>>>',cmd)
            print('Image ',step,'computed ...')
            os.system(cmd)
            if os.path.exists(stepimgfile):
                os.remove(mpovfile)
                os.remove(steppovfile)
                os.remove(bgeomfname)
            else:
                print('Error with image', step)
        else: print('Error with image', step)
        step += nbpovprocess


def main():
    import sys
    processflag =  '--process'
    numjobflag  =  '-j'
    stepflag    =  '--step'
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
            nbpovprocess = 3 # cpu_count() - 1
        generate_movie(nbpovprocess=nbpovprocess)
    elif '--bgeom' in sys.argv: 
        bgi = sys.argv.index('--bgeom')
        beg = None
        if len(sys.argv) > bgi+1:
            beg = int(sys.argv[bgi+1])
        else:
            import glob
            bgeomfiles = glob.glob(join(workingrep,bgeomfile.format('*')))
            if len(bgeomfiles) > 0:
                bgeomfiles= list(map(os.path.basename, bgeomfiles))
                bgeomids = list(map(int,[os.path.splitext(f)[0][bgeomfile.index('{'):] for f in bgeomfiles]))
                beg = max(bgeomids)
        generate_bgeom(beg)
    elif stepflag in sys.argv:
        pfi = sys.argv.index(stepflag)
        numstep = int(sys.argv[pfi+1])
        generate_bgeom(numstep,numstep+1)
        generate_pov(numstep,1,numstep+1)
    elif len(sys.argv) > 1:
        raise ValueError(sys.argv[1:])
    else:
        generate_bgeom()
        generate_pov(0,1)

if __name__ == '__main__':
    main()

