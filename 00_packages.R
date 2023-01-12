# CRAN packages
ls <- c("tidyverse", "lidR", "sf", "sfheaders", "future", "terra", 
  "data.table", "RCSF", "BiocManager", "devtools")

new_packages <- ls[!(ls %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)

# Install the FAIBBase package from the BC Government for calculating tree volumes
if(!"FAIBBase" %in% installed.packages()[, "Package"]) {
  devtools::install_github("bcgov/FAIBBase")
}

# I found a bug in the lidR package and told the developer about it, and he
# immediately fixed it. Make sure lidR package is up to date
if(packageVersion("lidR") < "4.0.2") {
  devtools::install_github("r-lidar/lidR")
}

# Install my custom lidR function
devtools::install_github("mcoghill/lidR.li2012enhancement")
