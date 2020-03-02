filedataout2 <- function(filename, data = "acc", output) {
  
  subjdata <- read.csv(filename, quote="")
  subj1 <- subjdata
  
  ud0iso <- subset(subj1, cond == "ISOS" & ori == "0")
  ud0inc <- subset(subj1, cond == "DS" & ori == "0") 
  ud0cong <- subset(subj1, cond == "SS" & ori == "0") 
  id0iso <- subset(subj1, cond == "ISOS" & ori == "180")
  id0inc <- subset(subj1, cond == "DS" & ori == "180")  
  id0cong <- subset(subj1, cond == "SS" & ori == "180")
  
  ud15iso <- subset(subj1, cond == "ISOD" & dissim == "15" & ori == "0")
  ud15inc <- subset(subj1, cond == "SD" & dissim == "15" & ori == "0")  
  ud15cong <- subset(subj1, cond == "DD" & dissim == "15" & ori == "0")
  id15iso <- subset(subj1, cond == "ISOD" & dissim == "15" & ori == "180")
  id15inc <- subset(subj1, cond == "SD" & dissim == "15" & ori == "180")  
  id15cong <- subset(subj1, cond == "DD" & dissim == "15" & ori == "180")  
  
  ud30iso <- subset(subj1, cond == "ISOD" & dissim == "30" & ori == "0")
  ud30inc <- subset(subj1, cond == "SD" & dissim == "30" & ori == "0")  
  ud30cong <- subset(subj1, cond == "DD" & dissim == "30" & ori == "0")
  id30iso <- subset(subj1, cond == "ISOD" & dissim == "30" & ori == "180")
  id30inc <- subset(subj1, cond == "SD" & dissim == "30" & ori == "180")  
  id30cong <- subset(subj1, cond == "DD" & dissim == "30" & ori == "180")  
  
  ud60iso <- subset(subj1, cond == "ISOD" & dissim == "60" & ori == "0")
  ud60inc <- subset(subj1, cond == "SD" & dissim == "60" & ori == "0")  
  ud60cong <- subset(subj1, cond == "DD" & dissim == "60" & ori == "0") 
  id60iso <- subset(subj1, cond == "ISOD" & dissim == "60" & ori == "180")
  id60inc <- subset(subj1, cond == "SD" & dissim == "60" & ori == "180")  
  id60cong <- subset(subj1, cond == "DD" & dissim == "60" & ori == "180")  
  
  ud90iso <- subset(subj1, cond == "ISOD" & dissim == "90" & ori == "0")
  ud90inc <- subset(subj1, cond == "SD" & dissim == "90" & ori == "0")  
  ud90cong <- subset(subj1, cond == "DD" & dissim == "90" & ori == "0")
  id90iso <- subset(subj1, cond == "ISOD" & dissim == "90" & ori == "180")
  id90inc <- subset(subj1, cond == "SD" & dissim == "90" & ori == "180")  
  id90cong <- subset(subj1, cond == "DD" & dissim == "90" & ori == "180")
  
  sumaccdata <- c(mean(id0inc$acc),mean(id0cong$acc),mean(id0iso$acc),
                  mean(id15inc$acc),mean(id15cong$acc),mean(id15iso$acc),
                  mean(id30inc$acc),mean(id30cong$acc),mean(id30iso$acc),
                  mean(id60inc$acc),mean(id60cong$acc),mean(id60iso$acc),
                  mean(id90inc$acc),mean(id90cong$acc),mean(id90iso$acc),
                  mean(ud0inc$acc),mean(ud0cong$acc),mean(ud0iso$acc),
                  mean(ud15inc$acc),mean(ud15cong$acc),mean(ud15iso$acc),
                  mean(ud30inc$acc),mean(ud30cong$acc),mean(ud30iso$acc),
                  mean(ud60inc$acc),mean(ud60cong$acc),mean(ud60iso$acc),
                  mean(ud90inc$acc),mean(ud90cong$acc),mean(ud90iso$acc))
  
  sumrtdata <- c(mean(id0inc$rt),mean(id0cong$rt),mean(id0iso$rt),
                 mean(id15inc$rt),mean(id15cong$rt),mean(id15iso$rt),
                 mean(id30inc$rt),mean(id30cong$rt),mean(id30iso$rt),
                 mean(id60inc$rt),mean(id60cong$rt),mean(id60iso$rt),
                 mean(id90inc$rt),mean(id90cong$rt),mean(id90iso$rt),
                 mean(ud0inc$rt),mean(ud0cong$rt),mean(ud0iso$rt),
                 mean(ud15inc$rt),mean(ud15cong$rt),mean(ud15iso$rt),
                 mean(ud30inc$rt),mean(ud30cong$rt),mean(ud30iso$rt),
                 mean(ud60inc$rt),mean(ud60cong$rt),mean(ud60iso$rt),
                 mean(ud90inc$rt),mean(ud90cong$rt),mean(ud90iso$rt))
  
  sumdiffdata <- c(1-mean(id0inc$acc),1-mean(id0cong$acc),1-mean(id0iso$acc),
                   mean(id15inc$acc),mean(id15cong$acc),mean(id15iso$acc),
                   mean(id30inc$acc),mean(id30cong$acc),mean(id30iso$acc),
                   mean(id60inc$acc),mean(id60cong$acc),mean(id60iso$acc),
                   mean(id90inc$acc),mean(id90cong$acc),mean(id90iso$acc),
                   1-mean(ud0inc$acc),1-mean(ud0cong$acc),1-mean(ud0iso$acc),
                   mean(ud15inc$acc),mean(ud15cong$acc),mean(ud15iso$acc),
                   mean(ud30inc$acc),mean(ud30cong$acc),mean(ud30iso$acc),
                   mean(ud60inc$acc),mean(ud60cong$acc),mean(ud60iso$acc),
                   mean(ud90inc$acc),mean(ud90cong$acc),mean(ud90iso$acc))
  
  sumaccnames <- c("id0inc","id0cong","id0iso",
                   "id15inc","id15cong","id15iso",
                   "id30inc","id30cong","id30iso",
                   "id60inc","id60cong","id60iso",
                   "id90inc","id90cong","id90iso",
                   "ud0inc","ud0cong","udo0iso",
                   "ud15inc","ud15cong","ud15iso",
                   "ud30inc","ud30cong","ud30iso",
                   "ud60inc","ud60cong","ud60iso",
                   "ud90inc","ud90cong","ud90iso")
  
  sori <- c(rep("inv",15),rep("upr",15))
  sdissim <- rep(c(rep(c(0,15,30,60,90),each=3)),2)
  scond <- rep(c("inc","cong","iso"),10)
  
  subj1data <- data.frame(sumaccnames, sumaccdata, sumdiffdata, sumrtdata,
                          sori, sdissim, scond)
  
  #subj1datainv <- data.frame(sumaccnames[1:15],sumaccdata[1:15], sumdiffdata[1:15],
  #                          sumrtdata[1:15], sori[1:15], sdissim[1:15], scond[1:15])
  names(subj1data) <- c("name","acc","diff","rt","ori","dissim","cond")
  
  subname <- substr(basename(filename),start=1, stop = 7)
  
  colvars <- t(data.frame(subj1data$name))
  colvars[2:31] <- colvars[1:30]
  colvars[1] <- c("")
  colvars <- t(colvars)
  
  colvars2 <- c("id0CxtD","id0CxtS","id0Iso",
                "id15CxtS","id15CxtD","id15Iso",
                "id30CxtS","id30CxtD","id30Iso",
                "id60CxtS","id60CxtD","id60Iso",
                "id90CxtS","id90CxtD","id90Iso",
                "ud0CxtD","ud0CxtS","ud0Iso",
                "ud15CxtS","ud15CxtD","ud15Iso",
                "ud30CxtS","ud30CxtD","ud30Iso",
                "ud60CxtS","ud60CxtD","ud60Iso",
                "ud90CxtS","ud90CxtD","ud90Iso"
                  )
  colvars2[2:31]<- colvars2[1:30]
  colvars2[1] <- c("")
  colvars2<- t(colvars2)
  
  
  accdata <- t(data.frame(subj1data$acc))
  accdata[2:31] <- accdata[1:30]
  accdata[1] <- subname
  accdata <- t(accdata)
  rtdata <- t(data.frame(subj1data$rt))
  rtdata[2:31] <- rtdata[1:30]
  rtdata[1] <- subname
  rtdata <- t(rtdata)
  diffdata <- t(data.frame(subj1data$diff))
  diffdata[2:31] <- diffdata[1:30]
  diffdata[1] <- subname
  diffdata <- t(diffdata)
  
  
  
  if(file.exists(output) == FALSE){
    write.table(colvars, output, append = FALSE, row.names = FALSE, col.names = FALSE,sep = ",")
    #write.table(colvars2, output, append = TRUE, row.names = FALSE, col.names = FALSE,sep = ",")
  } 
  
  
  if(data == "acc"){ 
    write.table(accdata, output, append = TRUE, col.names = FALSE, row.names = FALSE,sep = ",")
  }  else if(data == "diff"){
    write.table(diffdata, output, append = TRUE, col.names = FALSE, row.names = FALSE,sep = ",")
  }  else if(data == "rt"){
    write.table(rtdata, output, append = TRUE, col.names = FALSE, row.names = FALSE,sep = ",")
  }
  
  
}


