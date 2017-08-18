path<-'collation Files Path'
filenames<-list.files(path)
train<-NULL
for(m in 1:length(filenames)){
  
  temp<-read.csv(paste0(path,filenames[m]),stringsAsFactors = FALSE)
  train<-rbind(train,temp)
  
}

write.csv(train,'CollatedSheet.csv',row.names=FALSE)

