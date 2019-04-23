
bamestimates = bam_estimate(bamdata = bamdatas, bampriors = bampriors,variant = "amhg",iter = 2000, 
                            chains = 3, meas_error = TRUE, reparam = TRUE)
bam_output_format = bam_hydrograph(bamestimates)





