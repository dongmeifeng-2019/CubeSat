library('ProjectTemplate')
setwd('D:/RiverWidth/Arcrwl/Planet')
load.project()
require(bamr)
require(rstudioapi)
library(ggplot2)
require(rstan)
library(readr)
library(tidyverse)
discharge_folder='cache/bam_discharge'

idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')

priorID = 2  #1: USGS, 2:NECP, 3:MERRA-2, 4:ERA5
rcID = 0  #weather choose rating curve b

width_list = c('LS','ST','PL','LS-ST','comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet, comb: 3 datasets combined
width_input = width_list[1]
reachID = idlist[2]
xs_samp=20
ii = 1
kk = 1

for(reachID in idlist){
  for(width_input in width_list){
    source('src/bam_prior.R')
    if(width_input=='LS-ST'){
      source('src/bam_prep_LS_ST_combined_keepall.R')
    }else if(width_input=='comb'){
      source('src/bam_prep_combined_keepall.R')
    }else{
      source('src/bam_prep.R')
    }
    source('src/bam_estimate.R')
    source('src/bam_rename.R')
    kk = kk + 1
    #unlink('cache/reg', recursive=TRUE)
  }
  ii = ii + 1
}

