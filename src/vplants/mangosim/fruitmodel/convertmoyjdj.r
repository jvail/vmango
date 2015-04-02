## fonction pour convertir temperatures moyennes journalieres en ddj
convertmoyjdj <- function(Tu, Tl, Tmax, Tmin)
{
	provi <- (Tmax + Tmin)/2
	if (provi >= Tl)
		return (provi - Tl)
	else
		return (0)
}

