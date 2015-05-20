
def retrieveCurves(globals):
  from openalea.plantgl.all import BezierCurve2D
  # Determine the set of curve representing axis at different time. 
  # Look for object in global namespace name axisX 
  curves = [(n,v) for n,v in globals.items() if 'axis' in n and type(v) == BezierCurve2D ]
  
  # sort curves according to their names
  for n,v in curves: v.name = n
  curves = [v for n,v in curves]
  curves.sort(lambda x,y : cmp(x.name,y.name))
  return curves


def ProfileInterpolation(curves, knotlist = None, degree = 3, resolution = 10):
    from openalea.plantgl.all import Point4Matrix, NurbsPatch, NurbsCurve2D, BezierCurve2D
    nbcurves = len(curves)
    if knotlist is None: knotlist = [i/float(nbcurves-1) for i in xrange(nbcurves)]
    k = [knotlist[0] for i in xrange(degree-1)]+knotlist+[knotlist[-1] for i in xrange(degree-1)]
    pts = [[(i.x,i.y,0,1) for i in c.ctrlPointList] for c in curves]
    ppts = Point4Matrix(pts)
    p = NurbsPatch(ctrlPointList=ppts,udegree=degree,vdegree=3)
    def getSectionAt(t): 
      section = p.getIsoUSectionAt(t)
      res = NurbsCurve2D([(i.x,i.y,i.w) for i in section.ctrlPointList], section.knotList,section.degree)
      res.stride = resolution
      return res
    p.getAt = getSectionAt
    return p

def sweepSymbol(path, section, length, dlength, radius = 1, radiusvariation = None):
  from openalea.plantgl.all import PglTurtle
  t = PglTurtle()
  t.start()
  return t.startGC().sweep(path, section, length, dlength, radius, radiusvariation).stopGC().getScene()[0].geometry

