setwd("C:\\Users\\divya\\Downloads\\Stat")
library(rJava)
library(xlsx)
library(varhandle)
library(dplyr)
  path<-paste("./T001/*CD/*.stm")
  pp<-paste("./T001/*CD/*.pp")
  pp_session<-Sys.glob(pp)
  pp_data<-read.xlsx2(pp_session,sheetIndex = 1,startRow = 9)
  p_time<-pp_data[,2]
  p_value<-pp_data[,4]
  p_time<-unfactor(p_time)
  br_session <- Sys.glob(path)
    cols<-c(1,2)
    brdata <- read.xlsx2(br_session,sheetIndex=1,startRow = 9,endRow = 11,colIndex = cols)
    br<-unfactor(brdata)
   t_list<-c(br$StartTime[1],br$EndTime[1],br$StartTime[2],br$EndTime[2])
    i<-c(1:4)
    x<-0;
    for(num in i)
    {
      if(num==1)
      {
        calc<-subset(pp_data,p_time>=0&p_time<t_list[num])
        print(nrow(calc))
        print(mean(unfactor(calc[,4])))
        calc<-subset(pp_data,p_time>=t_list[num]&p_time<t_list[num+1])
        print(nrow(calc))
        print(mean(unfactor(calc[,4])))
        }
      else if(num==4)
      {
        calc<-subset(pp_data,p_time>=t_list[num])
        print(nrow(calc))
        print(mean(unfactor(calc[,4])))
        }
      else{
        calc<-subset(pp_data,p_time>=t_list[num]&p_time<t_list[num+1])
        print(nrow(calc))
        print(mean(unfactor(calc[,4])))
      }
     
    }
    
 dev.off()
