## Check for supporting packages
cran.pkgs <- c("magrittr", "devtools", "ggplot2",
              "xtable", "reshape", "tools", "tidyr", "stringr",
              "MuMIn", "lme4", "RLRsim", "vegan", "ecodist", 
              "bipartite", "RColorBrewer", "gplots", "plyr"
              )
gh.pkgs <- c("ROpenSci/drake", "r-lib/styler")
gh.pkgs.names <- do.call(rbind, 
                         strsplit(gh.pkgs, 
                                  split = "/"))[, 2]
## install packages that are not installed
## CRAN
if (any(!(cran.pkgs %in% installed.packages()[, 1]))){
    sapply(cran.pkgs[which(!(cran.pkgs %in% 
                             installed.packages()[, 1]))], 
           install.packages, 
           dependencies = TRUE, 
           repos = 'http://cran.us.r-project.org')
}
## github 
if (any(!(gh.pkgs.names %in% installed.packages()[, 1]))){
    sapply(gh.pkgs[which(!(gh.pkgs.names %in% 
                           installed.packages()[, 1]))], 
           devtools::install_github,
           dependencies = TRUE)
}
## Load libraries
sapply(c(cran.pkgs, gh.pkgs.names), 
       library, quietly = TRUE, character.only = TRUE)

## ENM Packages (copied from helpers.R)
packs <- c("gdistance", "fossil" , "igraph", "rgbif","mapproj","mapdata","sp", "reader", "dplyr",
           "maptools","dismo","rJava","rgdal", "rgeos", "raster", "reshape2", "ggplot2","FedData")

lapply(packs[!(packs %in% installed.packages()[,'Package'])],install.packages)
all(unlist(lapply(packs, require, character.only = TRUE,quietly=TRUE)))
