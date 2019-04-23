# bam_estimate.R
# Mark Hagemann
# 10/25/2017
# Making bam estimates from reachdf cross-section widths. 
# Modified from work in notebooks 20171024 and 20171025.
# Overhauled and split up (see bam_prep.R) on 1/9/2018
# This script uses the batchtools package, which enables the computation on HPC. 
# However, it will also work (more slowly) on a local machine. 

idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')

width_list = c('LS','ST','PL','LS-ST','Comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet

dataloc <- path.expand("~/Planet") # modify for HPC usage
rename = function(x){
  new_name = paste0(reachID,'_',width_input,'_',x)
  file.rename(file.path(sprintf("%s/reg/results", dataloc),x),file.path(dataloc,new_name))
}

library(batchtools)
library(tidyverse)

for(reachID in idlist[7]){
  for(width_input in width_list[5]){
    load(paste0(dataloc,'/',reachID,'_',width_input,'_bamdatas.RData'))
    load(paste0(dataloc,'/',reachID,'_',width_input,'_bampriors.RData'))
    #load(sprintf("%s/bamdatas.RData", dataloc))
    #load(sprintf("%s/bampriors.RData", dataloc))
    
    
    reg <- makeRegistry(sprintf("%s/reg", dataloc), make.default = FALSE, conf.file = "~/.batchtools.conf.R",
                        work.dir = "/home/df43a/Planet", packages = "bamr", seed = 5382)
    
    
    batchMap(bam_estimate, bamdata = bamdatas, bampriors = bampriors, 
             more.args = list(variant = "amhg", iter = 2000, 
                              chains = 3, meas_error = TRUE, reparam = TRUE), 
             reg = reg)
    
    
    submitJobs(reg = reg, resources = list(walltime = 120))
    
    i = 1
    while(length(list.files( sprintf("%s/reg/results", dataloc)))<500){
      Sys.sleep(10)      #wait for loading all files
      if(i==1){
        print('loading...')         
      }
      i = i+1
    }
    
    list.files(sprintf("%s/reg/results", dataloc))%>%
      map(rename)
    
    
    unlink(sprintf("%s/reg", dataloc), recursive=TRUE)
    
    k = 1
    while(dir.exists(file.path(sprintf("%s/reg", dataloc)))){
      Sys.sleep(10)     #wait for deleting 'reg'
      unlink(sprintf("%s/reg", dataloc), recursive=TRUE)
      if(k==1){
        print('deleting...')
      }
      k = k+1
    }
    
    
    # May need to resubmit some jobs
  }
}
resub <- function(reg) {
  jstat <- getJobStatus(reg = reg)
  notdone <- which(is.na(jstat$done))
  print(paste0("resubmitting: ", paste(notdone, collapse = ", ")))
  resetJobs(notdone, reg = reg)
  submitJobs(notdone, reg = reg)
}



