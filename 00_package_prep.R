if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("weinbergerlab/ExcessILI")

packages <- c("tidyverse", "MMWRweek", "cdcfluview", "parallel",
              "lubridate", "reshape2", "pbapply", "HDInterval",
              "cdcfluview", "abind")

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), repos = "http://cran.us.r-project.org")  
}


# to install rjags, you must first install jags
# in terminal, simply type "brew install jags"
# or via installer for mac or windows here
# https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/

# and then the following lines will work
install.packages("rjags")
