#tempstpierre2002 <- read.table("tempstpierre2002.csv", header=T, sep=";")
#tempstpierre2002$Date <- as.Date(tempstpierre2002$Date, "%d/%m/%y")
#head(tempstpierre2002)
#  "Avis : class discarded from column ?Date?"
#rayostpierre2002 <- read.table("rayostpierre2002.csv", header=T, sep=";")
#rayostpierre2002$DATE <- as.Date(rayostpierre2002$DATE, "%d/%m/%y")
#head(rayostpierre2002)
#k1runquant <- read.table("k1runquant.txt", header=T, sep="\t")

#source("convertmoyjdj.r")
#source("modele-complet-arbreB2-paramML.r")

fruitmodel <- function(nbfruits, dateflowering, leafareaperfruit, lightenvironment)
{
    mangsucreauB202(nbfruits)
    return (resultfincroiss)
}

fruitgrowth <- function(fruitdata)
{
    return (fruitdata)
}

empty_fruit_model <- function(nbfruits, dateflowering, nbleaf)
{
    print('run R model of fruit growth')
}
