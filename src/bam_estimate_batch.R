# bam_estimate.R
# Mark Hagemann
# 10/25/2017
# Making bam estimates from reachdf cross-section widths. 
# Modified from work in notebooks 20171024 and 20171025.
# Overhauled and split up (see bam_prep.R) on 1/9/2018
# This script uses the batchtools package, which enables the computation on HPC. 
# However, it will also work (more slowly) on a local machine. 

dataloc <- "cache" # modify for HPC usage

library(batchtools)

load(sprintf("%s/bamdatas.RData", dataloc))
load(sprintf("%s/bampriors.RData", dataloc))


reg <- makeRegistry(sprintf("%s/reg", dataloc), make.default = FALSE, 
                       packages = "bamr", seed = 5382)


batchMap(bam_estimate, bamdata = bamdatas, bampriors = bampriors, 
         more.args = list(variant = "amhg", iter = 2000, 
                          chains = 3, meas_error = TRUE, reparam = TRUE), 
         reg = reg)



submitJobs(reg = reg, resources = list(walltime = 120))

# May need to resubmit some jobs

resub <- function(reg) {
  jstat <- getJobStatus(reg = reg)
  notdone <- which(is.na(jstat$done))
  print(paste0("resubmitting: ", paste(notdone, collapse = ", ")))
  resetJobs(notdone, reg = reg)
  submitJobs(notdone, reg = reg)
}



