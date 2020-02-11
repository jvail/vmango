from __future__ import division
from __future__ import print_function
from past.utils import old_div
from openalea.mtg import MTG
import openalea.mtg.plantframe as opf; reload(opf) 
from openalea.mtg.plantframe.plantframe import PlantFrame
from openalea.mtg.plantframe.dresser import DressingData
from mtg_import import getMTG, get_filename
from openalea.plantgl.all import *

def pos_prop(mtg):
    xx,yy,zz = mtg.property("XX"),mtg.property("YY"),mtg.property("ZZ")
    return dict ([(node,(x,yy[node],-zz[node])) for node,x in list(xx.items())])

from math import radians, degrees

def orientation_prop(mtg):
    aa,bb,cc = mtg.property("AA"),mtg.property("BB"),mtg.property("CC")
    return dict ([(node,(radians(a),radians(bb[node]),radians(cc[node]))) for node,a in list(aa.items())])


matrixmethod = True

def plot(mtg, mtgname, leaves = True):
    posproperty = pos_prop(mtg)
    orientations = orientation_prop(mtg)

    div10 = lambda x : abs(x/10.) if x else x
    minus = lambda x : -x if x else x

    botdia = lambda x: div10(mtg.property('BottomDiameter').get(x))
    topdia = lambda x: div10(mtg.property('TopDiameter').get(x))

    if mtgname in ['b7','g5'] :
        zz = lambda x: mtg.property('ZZ').get(x)
    else:
        zz = lambda x: minus(mtg.property('ZZ').get(x))


    pf = PlantFrame(mtg, BottomDiameter=botdia, 
                    TopDiameter=topdia, 
                    ZZ = zz, 
                    origin=posproperty[mtg.roots(1)[0]])
    
    if mtgname in ['a21'] :
        pf.algo_diameter(0,5) # simple pipe mode with strange exponent

    if mtgname in ['jf31'] :
        pf.algo_diameter(0,2.5) # simple pipe mode with strange exponent

    elif mtgname in ['b7','f21'] :
        pf.algo_diameter(1,4)

    diameters = pf.compute_diameter()
    pf.points = dict([(node,Vector3(pos)-pf.origin) for node,pos in list(pf.points.items())])
    #pf.points = dict([(node,Vector3(pos)) for node,pos in pf.points.items()])

    leafdiam = QuantisedFunction(NurbsCurve2D(Point3Array([Vector3(0,0.0846264,1),Vector3(0.239002,1.00091,1),Vector3(0.485529,0.991241,1),Vector3(0.718616,1.00718,1),Vector3(0.877539,0.231273,1),Vector3(1,0.00332359,1)])))
    leafpath = NurbsCurve2D(Point3Array([(-0.5, 0, 1),(-0.145022, -0.0735931, 1),(0.0844156, -0.212121, 1),(0.123377, -0.497835, 1)]))    
    leafsection = NurbsCurve2D(Point3Array([Vector3(-0.508209,0.16873,1),Vector3(-0.515031,0.138195,1),Vector3(-0.198373,-0.0924227,1),Vector3(-0.00298323,0.188761,1),Vector3(0.0897461,-0.106293,1),Vector3(0.555704,0.0979703,1),Vector3(0.545047,0.12817,1)]))


    heights = dict([(v,mtg.Height(v)) for v in mtg.vertices(scale=mtg.max_scale())])
    maxh = float(max(heights.values()))

    leavelength = [l for n,l in list(mtg.property('Long').items()) if 'F' in mtg.label(n)]
    if len(leavelength) > 0:
        meanleaflength = old_div(sum(leavelength),(len(leavelength)*10.))
    else:
        meanleaflength = 20

    def plantframe_visitor(g, v, turtle):
        radius = diameters.get(v)/2.
        pt = pf.points.get(v)


        if pt:
            turtle.setId(v)
            if 'F' in g.label(v):
                if not leaves: return
                try:
                    l = mtg.property('Long')[v]/10.
                except:
                    l = meanleaflength

                orient = orientations[v]

                turtle.push()
                turtle.setColor(2).setWidth(radius)
                turtle.move(pt)
                if matrixmethod:
                    m = Matrix3.eulerRotationZYX((orient[0],-orient[1],orient[2]))
                    h, u =  m * (1,0,0), m * (0,0,1)
                    turtle.setHead(h,u)
                else:
                    turtle.setHead((0,0,1),(1,0,0)).rollR(180+degrees(orient[2])).down(90-degrees(orient[1])).right(degrees(orient[0]))
                turtle.startGC().sweep(leafpath,leafsection,l,l/10.,3,leafdiam)
                turtle.pop()
            else:
                if pt.z < 0 : print(v, 'lineTo', pt, radius)
                turtle.interpolateColors(1,2,old_div(heights[v],maxh))
                turtle.lineTo(pt, radius)
    pf.plot(origins=[(0,0,0)],visitor=plantframe_visitor,gc=False)
    return pf


def process_mtg(mtgname, leaves = True):
    mtg = getMTG(mtgname+'.mtg')
    pf = plot(mtg,mtgname,leaves)

if __name__ == '__main__':
    import sys
    if len(sys.argv) >= 2:
        mtgname = sys.argv[1]
    else:
        mtgname = 'a19'
    if len(sys.argv) >= 3:
        leaf = eval(sys.argv[2])
    else : leaf = True
    print('Process MTG',repr(mtgname))
    mtg = getMTG(mtgname+'.mtg')
    pf = plot(mtg,mtgname,leaf)
