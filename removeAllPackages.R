rm(list = ls())

# create a list of all installed packages
ip <- as.data.frame(installed.packages())
#write.csv(ip, file = "packages.csv")
#head(ip)
# if you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, grepl("Documents", ip$LibPath))
# we don't want to remove base or recommended packages either\
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
# determine the library where the packages are installed
path.lib <- unique(ip$LibPath)
# create a vector with all the names of the packages you want to remove
pkgs.to.remove <- as.character(ip[,1])
head(pkgs.to.remove)
# remove the packages
sapply(pkgs.to.remove, remove.packages, lib = path.lib)

ip <- as.data.frame(installed.packages())
ip <- subset(ip, grepl("Documents", ip$LibPath))
ip <- ip[!(ip[,"Priority"] %in% c("base", "recommended")),]
path.lib <- unique(ip$LibPath)
pkgs.to.remove <- as.character(ip[,1])
sapply(pkgs.to.remove, remove.packages, lib = path.lib)

installed.packages("dplyr")

packages <- c("dplyr")

check <- if(all(packages %in% installed.packages(lib.loc=.libPaths()))){
  msg <- "Ok!"
}else{
  msg <- "Some packages seem to be missing!"
}

check
sessionInfo()
install.packages("dplyr")
