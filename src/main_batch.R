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

priorID = 1   #1: USGS, 2:NECP, 3:PR

width_list = c('LS','ST','PL','Comb')  # LS: Landsat, ST: Sentinel-2, PL: Planet
width_input = width_list[3]
reachID = idlist[1]
xs_samp=c(1:20)

for(reachID in idlist){
  for(width_input in width_list[4]){
    source('src/bam_prior_batch.R')
    #source('src/bam_prep_batch.R')
    source('src/bam_prep_combined_batch.R')
    source('src/bam_estimate_batch.R')
    source('src/bam_rename_batch.R')
    
    unlink('cache/reg', recursive=TRUE)
  }
}

