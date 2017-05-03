import numpy as np
from scipy.interpolate import *
    
def smoothcurve(x,y):    
    fy = [y[0],y[0]]+sum([[yi,yi,yi] for yi in y[1:-1]],[])+[y[-1],y[-1]]
    fy = [y[0]]+[(fy[i-1]+fy[i]*2+fy[i+1])/4. for i in xrange(1,len(fy)-1)]+[y[-1]]
    c = 1/3.
    fx = [x[0],x[0]+(x[1]-x[0])*c]+sum([[x[i]+(x[i-1]-x[i])*c,x[i],x[i]+(x[i+1]-x[i])*c] for i in range(1,len(x)-1)],[])+[x[-1]+(x[-2]-x[-1])*c,x[-1]]
    xnew = np.linspace(min(x),max(x),300)
    #tck = splrep(fx, fy, s=0)
    #smoothy = splev(xnew, tck, der=0)
    smoothy = spline(fx,fy,xnew,3)
    return xnew, smoothy
