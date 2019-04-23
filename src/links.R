library(foreign)
library('ProjectTemplate')
setwd('D:/RiverWidth/Arcrwl/Planet')
load.project()
ortho_folder = 'D:/RiverWidth/Arcrwl/GIS/'
width_folder 
idlist=c('15493400','15511000','15493700','15477740','15356000','15514000','15453500',
         '15485500','15515500','15484000','15519100')
for(reachID in idlist){
  file_name = paste(ortho_folder,reachID,'_orthogonal_ID.dbf',sep='')
  if(reachID=='15515500'){
    file_name = paste(ortho_folder,reachID,'_orthogonal_ID_ToColin.dbf',sep='')
  }
  links = read.dbf(file_name)
  links = data.frame(plID=c(1:nrow(links)),ID=links$ID)
  write.table(links,paste('data/Widths/',reachID,'_links.txt',sep=''),row.names = F)
}
