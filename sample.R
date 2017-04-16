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
    
    cleaned<-subset(brdata, time<=0 | br>=30)
    
    # print(nrow(cleaned))
    if(nrow(cleaned)==0){
      num=num+1
      mtext(right_label,side=4,line=0.5)
      lines(time,br,type="l")
    }
    else{
      #print(h)
    }
    
  }
  text(xlim-(0.025*xlim),3,paste("n = ",num))
  
}

br_All('PD',600)
mtext("Breathing Rate [bpm] raw signal sets",
      side=3,line=1.97)
br_clean('PD',600)
mtext("Breathing Rate [bpm] valid signal sets",
      side=3,line=1.97)
br_All('RD',600)
br_clean('RD',600)
br_All('ED',800)
br_clean('ED',800)
br_All('MD',1000)
br_clean('MD',1000)
br_All('CD',800)
br_clean('CD',800)
br_All('ND',800)
br_clean('ND',800)
br_All('FD',250)
mtext("time(s)",
      side=1,line=1.97)
br_clean('FD',250)
mtext("time(s)",
      side=1,line=1.97)

dev.off()