if (!("remotes" %in% installed.packages()))
  install.packages("remotes", dep = TRUE)

library(remotes)

install.packages("datastructures")

# Github packages
install_github("jakobbossek/salesperson")
install_github("jakobbossek/re")
install_github("jakobbossek/tspgen")
