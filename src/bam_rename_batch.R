dataloc <- "cache/reg/results"

rename = function(x){
  new_name = paste0(reachID,'_',width_input,'_',x)
  file.rename(file.path(dataloc,x),file.path(discharge_folder,new_name))
}
list.files(dataloc)%>%
  map(rename)

dataloc <- "cache"

list.files(dataloc,pattern="*.RData")%>%
  map(rename)

            