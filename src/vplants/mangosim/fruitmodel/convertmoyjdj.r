## fonction pour convertir températures moyennes journalières en ddj
convertmoyjdj <- function(Tu, Tl, Tmax, Tmin)
{
	provi <- (Tmax + Tmin)/2
	if (provi >= Tl)
		provi - Tl
	else
		0
}