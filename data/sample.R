


library(sas7bdat)

thoracic_data<-read.sas7bdat("https://auburn.box.com/shared/static/bcyulwlkmnhg21fmi1tvnuajv8xjhn1d.sas7bdat")

library(rio)
thoracic_data<-import("C:/Users/hza0020/Box/Transplant Dataset/SAS Dataset/Thoracic/thoracic_data.sas7bdat")



data<-thoracic_data
location<-"C:/Users/hza0020/Box/Transplant Dataset/SAS Dataset/Thoracic/crushed"


files<-list()
firsti<-0
lasti<-0


total_files<-ceiling(nrow(data)/9000)
 
for (i in 1:total_files){
  firsti<-9000*(i-1)+1
  lasti<-9000*i
  print(i)
  if(lasti>nrow(data)){lasti<-nrow(data)}
  files[[i]]<-data[firsti:lasti,]
  
  export(files[[i]],paste(location,"/thoracic_data_",
         firsti,"_",lasti,"L.RData",sep=""))
}
  


  
  



######################################

  
thoracic_data_1_9<-thoracic_data[1:9000,]  
export(thoracic_data_1_9,"C:/Users/hza0020/Box/Transplant Dataset/SAS Dataset/Thoracic/thoracic_data_1_9.sas7bdat")

thoracic_data_1_9L<-list()
thoracic_data_1_9L<-thoracic_data_1_9
export(thoracic_data_1_9,"C:/Users/hza0020/Box/Transplant Dataset/SAS Dataset/Thoracic/thoracic_data_1_9L.RData")








