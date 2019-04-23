
if(rcID){
  file_q = paste0(discharge_folder, '/',reachID,'_',width_input,'_','20_RatingCurve_b','.rds')
  saveRDS(bam_output_format,file_q)
}else{
  file_q = paste0(discharge_folder, '/',reachID,'_',width_input,'_','20_BAM_b','.rds')
  saveRDS(bam_output_format,file_q)
  file_date = paste0(discharge_folder, '/',reachID,'_',width_input,'_dates.rds')
  saveRDS(bamdates,file_date)
}



