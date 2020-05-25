args <- commandArgs(trailingOnly = T)
localdir = args[1]
setwd(localdir)
source('fruit_model_main.r', keep.source=TRUE)

dfDigits <- function(x, digits = 2) {
    ## x is a data.frame
    for (col in colnames(x)[sapply(x, class) == 'numeric'])
        x[,col] <- format(round(x[,col], digits = digits), nsmall = digits)
    x
}

bloom_date = args[2]
nb_fruits = as.double(args[3])
nb_leaves = as.double(args[4])
verbose = as.logical(args[5])
DM_fruit_0 = as.double(args[6])
light_envir_sample = as.double(args[7])
weather_fname = args[8]

res = fruitmodel(
    bloom_date,
    nb_fruits,
    nb_leaves,
    light_envir_sample,
    DM_fruit_0,
    DM_fruit_ini = NaN,
    sim_date_ini = NaN,
    weather_fname,
    cultivar_fname = "cogshall",
    dd_thresh = NaN,
    stop_sim_ddcum = NaN,
    verbose
)
# write.csv(dfDigits(res, 8), paste(localdir,"/tmp/r.csv",sep=''), row.names=FALSE, quote=FALSE)
write.csv(res, paste(localdir,"/tmp/r.csv",sep=''), na='nan', row.names=FALSE, quote=FALSE)
