makeReport <- function(fileName){
  library(dplyr)
#  library("xlsx")
  
  #read in file
  prodTem <- read.csv(fileName, header = TRUE, skip = 1, stringsAsFactors = FALSE)
 #prodTem <- read.csv("productionTemplate.csv", header = TRUE, skip = 1,  stringsAsFactors = FALSE)
 
  #derive the sums
  sums <- prodTem %>% 
         group_by(Catagory) %>% 
        summarise(SumOfNet.Weight = sum(SumOfNet.Weight))
  
  #put sums into variables
  
  Crimini <- select(filter(sums, Catagory == "Crimini"), SumOfNet.Weight)
  BabyPort <- select(filter(sums, Catagory == "Baby Port"), SumOfNet.Weight)
  CriminiTray <- select(filter(sums, Catagory == "Crimini Trays"), SumOfNet.Weight)
  LargePort <- select(filter(sums, Catagory == "Large Port"), SumOfNet.Weight)
  MediumPort <- select(filter(sums, Catagory == "Medium Port"), SumOfNet.Weight)
  Opens <- select(filter(sums, Catagory == "Opens"), SumOfNet.Weight)
  Slicers <- select(filter(sums, Catagory == "Slicers"), SumOfNet.Weight)
 
  #set the value to 0 if the category doesn't exist
  if(nrow(Crimini)==0){Crimini <- 0}
  if(nrow(BabyPort)==0){BabyPort <- 0}
  if(nrow(CriminiTray)==0){CriminiTray <- 0}
  if(nrow(LargePort)==0){LargePort <- 0}
  if(nrow(MediumPort)==0){MediumPort <- 0}
  if(nrow(Opens)==0){Opens <- 0}
  if(nrow(Slicers)==0){Slicers <- 0}
 
  #get room, break and fill date
  roomFDBreak <-  read.table(fileName, nrow = 1, sep = ",")
  #roomFDBreak <-  read.table("productionTemplate.csv", nrow = 1, sep = ",")
  
  room = roomFDBreak$V1
  room <- toString(room)
  fillDate <- roomFDBreak$V3
  fillDate <- toString(fillDate)
  breakNum <- roomFDBreak$V6
  
  #clean up breakNum
  breakNum <- toString(breakNum)
  breakNum <- gsub("st", "", breakNum, fixed = TRUE)
  breakNum <- gsub("nd", "", breakNum, fixed = TRUE)
  breakNum <- gsub("rd", "", breakNum, fixed = TRUE)

  
  #read in master file
  portAnalysis <- read.csv("portAnalysis.csv", header= TRUE, sep = ",", stringsAsFactors=FALSE)
  names(portAnalysis) <- gsub(".", "", names(portAnalysis), fixed = TRUE)
#  portAnalysis[is.na(portAnalysis)] <- 0
 # portAnalysis[,room] <- sapply(portAnalysis[,room],as.character)
#  portAnalysis[,room] <- sapply(portAnalysis[,room],as.numeric)
 # portAnalysis[is.na(portAnalysis)] <- 0
  
 
 # filter(portAnalysis,X== "weight", X1 == "Crimini", Break == breakNum)
  #select( filter(portAnalysis,X== "weight", X1 == "Crimini", Break == breakNum), room)
  #portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,]
  
  #find cells in master to replace
  portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- Crimini
  portAnalysis[portAnalysis$X1 =="Baby Port" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- BabyPort
  portAnalysis[portAnalysis$X1 =="CriminiTray" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- CriminiTray
  portAnalysis[portAnalysis$X1 =="Large Port" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- LargePort
  portAnalysis[portAnalysis$X1 =="Medium Port" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- MediumPort
  portAnalysis[portAnalysis$X1 =="S/O" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- Opens
  portAnalysis[portAnalysis$X1 =="Slicer" & portAnalysis$X == "weight" & portAnalysis$Break == breakNum,room] <- Slicers
  
  
  #calculate totals
 total1 <- sum(as.numeric(as.character(portAnalysis[1:9,room])), na.rm = TRUE)
 total2 <- sum(as.numeric(as.character(portAnalysis[11:19,room])), na.rm = TRUE)
 total3 <- sum(as.numeric(as.character(portAnalysis[21:29,room])), na.rm = TRUE)
  
 #break totals
  portAnalysis[portAnalysis$X1 =="Total (1)"& portAnalysis$X == "weight",room] <- total1
  portAnalysis[portAnalysis$X1 =="Total (2)"& portAnalysis$X == "weight",room] <- total2
  portAnalysis[portAnalysis$X1 =="Total (3)"& portAnalysis$X == "weight",room] <-  total3
  
  #Category totals
  CrimTotal <- portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight",room]
  CrimTotal <-  sum(as.numeric(as.character(CrimTotal)), na.rm= TRUE)
  
  BabyPortTotal <-  portAnalysis[portAnalysis$X1 =="Baby Port" & portAnalysis$X == "weight",room]
  BabyPortTotal <-  sum(as.numeric(as.character(BabyPortTotal)), na.rm= TRUE)
  
  CrimTrayTotal <- portAnalysis[portAnalysis$X1 =="CriminiTray" & portAnalysis$X == "weight",room]
  CrimTrayTotal <- sum(as.numeric(as.character(CrimTrayTotal)), na.rm= TRUE)

  LPortTotal <- portAnalysis[portAnalysis$X1 =="Large Port" & portAnalysis$X == "weight",room]
  LPortTotal <- sum(as.numeric(as.character(LPortTotal)), na.rm= TRUE)
  
  MPortTotal <- portAnalysis[portAnalysis$X1 =="Medium Port" & portAnalysis$X == "weight",room]
  MPortTotal <- sum(as.numeric(as.character(MPortTotal)), na.rm= TRUE)
  
  SOTotal <- portAnalysis[portAnalysis$X1 =="S/O" & portAnalysis$X == "weight",room]
  SOTotal <- sum(as.numeric(as.character(SOTotal)), na.rm= TRUE)
  
  SlicerTotal <- portAnalysis[portAnalysis$X1 =="Slicer" & portAnalysis$X == "weight",room]
  SlicerTotal <- sum(as.numeric(as.character(SlicerTotal)), na.rm= TRUE)
  
  #portAnalysis[is.na(portAnalysis)] <- 0
  
 # portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- as.character( portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room])
  
  portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- CrimTotal
  portAnalysis[portAnalysis$X1 =="Baby Port" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- BabyPortTotal
  portAnalysis[portAnalysis$X1 =="CrimTrayTotal" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- CrimTrayTotal
  portAnalysis[portAnalysis$X1 =="Large Port" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- LPortTotal
  portAnalysis[portAnalysis$X1 =="Medium Port" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- MPortTotal
  portAnalysis[portAnalysis$X1 =="S/O" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- SOTotal
  portAnalysis[portAnalysis$X1 =="Slicer" & portAnalysis$X == "weight" & portAnalysis$Break == "T",room] <- SlicerTotal
  
  totalTotal <- CrimTotal+BabyPortTotal +CrimTrayTotal +LPortTotal +MPortTotal + SOTotal +SlicerTotal
  portAnalysis[is.na(portAnalysis$X1 =="Total (T)" & portAnalysis$X == "weight" & portAnalysis$Break == "T"),room] <- totalTotal
  
  
  #calculate percentages
  CrimPerc = 100* Crimini/CrimTotal
  BabyPortPerc = 100* BabyPort/BabyPortTotal
  CriminiTrayPerc = 100 *CriminiTray/CrimTrayTotal
  LPortPerc = 100* LargePort/LPortTotal
  MPortPerc = 100*MediumPort/MPortTotal
  SOPerc = 100*Opens/SOTotal
  SlicerPerc = 100*Slicers/SlicerTotal
    
  portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- CrimPerc
  portAnalysis[portAnalysis$X1 =="Baby Port" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- BabyPortPerc
  portAnalysis[portAnalysis$X1 =="CriminiTray" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- CriminiTrayPerc
  portAnalysis[portAnalysis$X1 =="Large Port" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- LPortPerc
  portAnalysis[portAnalysis$X1 =="Medium Port" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- MPortPerc
  portAnalysis[portAnalysis$X1 =="S/O" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- SOPerc
  portAnalysis[portAnalysis$X1 =="Slicer" & portAnalysis$X == "Percent" & portAnalysis$Break == breakNum,room] <- SlicerPerc    
  
  #calculating Percentages of the totals
  CrimPercT = 100* CrimTotal/totalTotal 
  BabyPortPercT = 100 *BabyPortTotal/totalTotal
  CriminiTrayPercT = 100*CriminiTrayPerc/totalTotal
  LPortPercT = 100*LPortTotal/totalTotal
  MPortPercT = 100*MPortTotal/totalTotal
  SOPercT = 100*SOPerc/totalTotal
  SlicerPercT = 100*SlicerPerc/totalTotal
  TotalPercent = CrimPercT + BabyPortPercT + LPortPercT +MPortPercT+SOPercT+ SlicerPercT
  
  portAnalysis[portAnalysis$X1 =="Crimini" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- CrimPercT
  portAnalysis[portAnalysis$X1 =="Baby Port" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- BabyPortPercT
  portAnalysis[portAnalysis$X1 =="CriminiTray" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- CriminiTrayPercT
  portAnalysis[portAnalysis$X1 =="Large Port" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- LPortPercT
  portAnalysis[portAnalysis$X1 =="Medium Port" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- MPortPercT
  portAnalysis[portAnalysis$X1 =="S/O" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- SOPercT
  portAnalysis[portAnalysis$X1 =="Slicer" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- SlicerPercT    
  portAnalysis[portAnalysis$X1 =="Total (T)" & portAnalysis$X == "Percent" & portAnalysis$Break == "T",room] <- TotalPercent
  
  
  #Yields
  dolPerLb <- prodTem[45,"Item.Number"]
  lbsSqFt <- prodTem[45,"Item"]
  dolPerSqFt <- prodTem[45,"Item.Price"]
 
  dolPerLb <- gsub("$", "", dolPerLb, fixed = TRUE)
  dolPerLb <- as.numeric(dolPerLb)
  lbsSqFt <- as.numeric(lbsSqFt)
  dolPerSqFt <- gsub("$", "", dolPerSqFt, fixed = TRUE)
  
  portAnalysis[portAnalysis$Break == breakNum & portAnalysis$X=="Yield" & portAnalysis$X1 == "$ per lb", room] <-dolPerLb
  portAnalysis[portAnalysis$Break == breakNum & portAnalysis$X=="Yield" & portAnalysis$X1 == "Lbs sq ft", room] <-lbsSqFt
  portAnalysis[portAnalysis$Break == breakNum & portAnalysis$X=="Yield" & portAnalysis$X1 == "$/ ft2", room] <-dolPerSqFt
  
  fillingDates <- read.csv("fillingDates.csv", header = TRUE, stringsAsFactors = FALSE)
  fillingDates$V1 <- gsub(" ", "", fillingDates$V1, fixed = TRUE)
  fillDate <- toString(fillDate)
  
  fillingDates[fillingDates$V1 == room,2] <- fillDate

  write.csv(portAnalysis, file = "portAnalysis.csv", row.names=FALSE)
  write.csv(fillingDates, file = "fillingDates.csv", row.names=FALSE)
  portAnalysis[,room]
 # tapply(prodTem$SumOfNet.Weight, prodTem$Catagory, FUN=sum)
}