library('ProjectTemplate')
setwd('D:/RiverWidth/Arcrwl/Planet')
load.project()
require(bamr)
require(rstudioapi)
library(ggplot2)
require(rstan)
library(readr)
library(tidyverse)
library(boot)
discharge_folder='cache/bam_discharge'
dataloc <- "cache"

idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')

priorID = 1   #1: USGS, 2:NECP, 3:PR
rcID = 0

width_list = c('LS','ST','PL','LS-ST','Comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet
width_input = width_list[1]
reachID = idlist[1]
#xs_samp=c(1:20)

for(reachID in idlist){
  for(width_input in width_list){
    source('src/bam_prior_batch.R')
    if(width_input=='LS-ST'){
      source('src/bam_prep_LS_ST_combined_keepall_batch.R')
    }else if(width_input=='Comb'){
      source('src/bam_prep_combined_keepall_batch.R')
    }else{
      source('src/bam_prep_batch.R')
    }
    source('munge/rename.R')
    
    list.files(dataloc,pattern="*.RData")%>%
      map(rename)
    
    #
    #source('src/bam_estimate_batch.R')
    #source('src/bam_rename_batch.R')
    
    #unlink('cache/reg', recursive=TRUE)
  }
}

