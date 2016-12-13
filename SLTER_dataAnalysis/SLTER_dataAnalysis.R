# HARVARD FOREST SCHOOLYARD ECOLOGY DATA ANALYSIS
# Data taken winter 2015-16 from http://harvardforest2.fas.harvard.edu/asp/hf/php/k12/k12_data.php
# Code by Nate Edelman

# Each data section (eg Fall Phenology Data) is self-contained. 
# Environmental variables should be cleared when switching from one section to the next to avoid 
#    mistakes due to re-used variable names

# Questions: are trees/pools/streams consistent across years within each teacher? school? 

#######################################################

#uncomment install lines if packages not yet installed
#install.packages("ggplot2")
#install.packages("reshape2")
library("ggplot2")
library("reshape2")
library("ggmap")

############################################
###### Fall Phenology Data ############
############################################

################### Formatting ########################

#read in the fall phenology data
fallPheno=read.csv("data/fall_phenology.csv")

#read in the species code info
speciesCodes=read.csv("data/species_codes.csv")

#give a unique identifier to each tree
fallPheno$schoolNum=as.numeric(as.factor(fallPheno$School.Code))
#In case schools accidentally gave the same ID to different trees of different species
fallPheno$speciesNum=as.numeric(as.factor(fallPheno$Species.Code))
fallPheno$uniqTreeID=(fallPheno$schoolNum*100 + fallPheno$Tree.ID+fallPheno$speciesNum)

#Get year as an identifier
fallPheno$Date=as.Date(fallPheno$Date)
fallPheno$Year=format(fallPheno$Date, "%Y")

#associate the tres with species
vec <- c()
for (i in seq(1,length(fallPheno$Species.Code))){
  tryCatch(vec[i] <- (which(speciesCodes$Species.Code==as.character(fallPheno$Species.Code[i]))), 
           error=function(e){vec[i] <- 89})
  }

#Assign Scientific names to each tree
SciNames = as.character(speciesCodes[vec,3])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
fallPheno$genus = genusVec
fallPheno$species = speciesVec
fallPheno$Tree.Color=as.numeric(as.character(fallPheno$Tree.Color))

#Get Fallen leaves as a percentage of total leaves
#First, correct the entries that were put in incorrectly. these were all done by TCS Donnelly, and they should all have a total of 12 leaves
for (i in seq(1, nrow(fallPheno))){
  if(is.na(fallPheno[i,]$Fallen.Leaves)){
    fallPheno[i,]$Fallen.Leaves = 0
  }
  if(is.na(fallPheno[i,]$Total.Leaves)){
    fallPheno[i,]$Total.Leaves = 0
  }
  if(fallPheno[i,]$Total.Leaves < fallPheno[i,]$Fallen.Leaves){
    fallPheno[i,]$Fallen.Leaves = fallPheno[i,]$Total.Leaves
    fallPheno[i,]$Total.Leaves = 12
  }
}

#some trees were recorded as never having lost leaves. These should be removed.
neverFellTeacher <- c()
neverFellTreeID <- c()
neverFellTree <- c()
neverFellYear <- c()
for (i in unique(fallPheno$uniqTreeID)){
  singleTree <- subset(fallPheno, uniqTreeID==i)
  for (j in unique(singleTree$Year)){
    singleYear <- subset(singleTree, Year==j)
    if (max(singleYear$Fallen.Leaves) == 0 & max(singleYear$Total.Leaves)>0 ){
      neverFellTree <- c(neverFellTree, i)
      neverFellYear <- c(neverFellYear, j)
      neverFellTeacher <- c(neverFellTeacher, as.character(unique(singleYear$Teacher)))
      neverFellTreeID <- c(neverFellTreeID, unique(singleYear$Tree.ID))
    }
  }
}

unWantedRows <- c()
for (i in seq(1,length(neverFellTree))){
  unWantedRows <- c(unWantedRows, which(fallPheno$uniqTreeID==neverFellTree[i] & fallPheno$Year == neverFellYear[i]))
}

fallPheno <- fallPheno[-unWantedRows,]
fallPheno$pctFallen=fallPheno$Fallen.Leaves/fallPheno$Total.Leaves

############# GRAPHING ######################

####################General Graphing Functions and preparation#############################################

graphingFrame=data.frame(julDate=fallPheno$Julian, pctFallen=fallPheno$pctFallen, leafColor=fallPheno$Tree.Color, genus=fallPheno$genus, species=fallPheno$species, year=fallPheno$Year, id=fallPheno$uniqTreeID, school=fallPheno$School.Code)
attach(graphingFrame)
sorted=graphingFrame[order(julDate),]
allDates=unique(sorted$julDate)

overallStatFrame=fallPheno[order(fallPheno$uniqTreeID),]
numTrees=length(unique(overallStatFrame$uniqTreeID))
numObs=length(overallStatFrame$uniqTreeID)
numYears=length(unique(overallStatFrame$Year))

overviewInfo <- function(data, column, allDates){
  allAverages=c()
  for (date in allDates){
    allVals=subset(data, julDate==date)[column][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}

######################fallen leaves as percent of total leaves, by Julian date,##############################

#correctlyDone=subset(sorted, pctFallen <=1)

fallenAverages=overviewInfo(sorted,"pctFallen",allDates)
overallAvgFallen=data.frame(julDate=allDates, values=fallenAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgFallen, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgFallen, aes(x=julDate, y=values)) +
  scale_y_continuous(limits=c(-.1, 1.1)) +
  scale_x_continuous(limits=c(240, 350)) +
  annotate("text", x = 253, y = 1.0, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 253, y = 0.9, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 253, y = 0.8, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Number of Leaves Fallen by Julian Date", x ="Julian Date", y="Percent Fallen")
ggsave(graph1, filename = "leafFallOverall.png")
graph1

####################### Tree Color as percent of total leaves, by Julian Date ----

sorted$leafColor=as.numeric(as.character(sorted$leafColor))
colorAverages=overviewInfo(sorted,"leafColor",allDates)
overallAvgColor=data.frame(julDate=allDates, values=colorAverages)

graph2=ggplot() +
  stat_smooth(data=overallAvgColor, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgColor, aes(x=julDate, y=values)) +
  scale_y_continuous(limits=c(0.9, 4.1)) +
  scale_x_continuous(limits=c(240, 350)) +
  annotate("text", x = 253, y = 4.0, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 253, y = 3.8, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 253, y = 3.6, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Tree Color by Julian Date", x ="Julian Date", y="Tree Color")
#ggsave(graph1, filename = "treeColorOverall.png")
graph2

######################## Tree Color and Leaf fall Combined ###################


combiDate=rep(allDates,2)

fallenAverages=overviewInfo(sorted,"pctFallen",allDates)
colorAverages=overviewInfo(sorted,"leafColor",allDates)
reScaledColors=(colorAverages-1)/3
combiValue=c(fallenAverages,reScaledColors)

combiVariable=c(rep("leafFall", length(allDates)), rep("color", length(allDates)))

combinedFrame=data.frame(combiDate,combiValue,combiVariable)
usable=subset(combinedFrame, combiValue<2)

graph1 <- ggplot() + 
  geom_point(data=usable, aes(x=combiDate, y=combiValue, colour=combiVariable)) + 
  stat_smooth(data=usable, formula=y~x, aes(x=combiDate, y=combiValue, colour=combiVariable)) + 
  scale_y_continuous(limits=c(-.1, 1.1)) +
  scale_x_continuous(limits=c(240, 350)) +
  annotate("text", x = 265, y = 1.0, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 265, y = 0.95, label = paste0("Number of Observations: ",numObs)) +
  annotate("text", x = 265, y = 0.9, label = paste0("Number of Years: ",numYears)) +
  labs(title="Tree Color and Leaf Fall by Julian Date", x="Julian Date", y="Normalized Tree Color, Percent of Leaves Fallen", colour="Variable")
ggsave(graph1,filename="combinedOverall.png")
graph1

######################By Genus Graphing Functions and preparation#############################################

byGenera=graphingFrame[order(graphingFrame$genus),]
allGenera=unique(as.character(byGenera$genus))

#Get info on the amount of data we have on each genus
genusInfoFrame=data.frame(Genus=c("numTrees", "numObs", "numYears"))
for (i in 1:length(allGenera)){
  thisFrame=subset(fallPheno, fallPheno$genus==allGenera[i])
  uniques=unique(thisFrame$uniqTreeID)
  numTrees=length(uniques)
  numObs=nrow(thisFrame)
  numYears=length(unique(thisFrame$Year))
  genusInfoFrame[allGenera[i]] <- c(numTrees,numObs, numYears)
}

#set up data frame
byGenusFrame=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- rep(0,length(allDates))
}
colnames(byGenusFrame) <- c("Date", allGenera)


f <- function(index,genusList, dateList, column){
  allVals=subset(sorted, genus==genusList[index])
  thisGenus=c()
  for (date in dateList){
    thisDate=subset(allVals, julDate==date)[column][[1]]
    avg=mean(thisDate, na.rm=TRUE)
    thisGenus=c(thisGenus,avg)
  }
  return (thisGenus)
}


#####################Edit 12/9/16: need info by species and location####################
#Answering the question of spatial distribution of trees across study area
#First, associate the school code with location info

schoolInfo=read.csv("data/school_info.csv", col.names = c("code","name","teachers","address","town","state",
                                                          "lat","lon","elev","buds","hwa", "vernal","stream","forest"))

graphingFrame$sciName <- paste(graphingFrame$genus, graphingFrame$species)
bySpecies=graphingFrame[order(graphingFrame$sciName),]
allSpecies=unique(as.character(byGenera$sciName))

locVec <- c()
for (i in seq(1,length(bySpecies$school))){
  (locVec[i] <- (which(schoolInfo$code==as.character(bySpecies$school[i]))))
}
bySpecies$town <- as.character(schoolInfo[locVec,5])
bySpecies$lat <- as.character(schoolInfo[locVec,7])
bySpecies$lon <- as.character(schoolInfo[locVec,8])
bySpecies$elev <- as.character(schoolInfo[locVec,9])

#First graph will be number of unique trees

reps <- data.frame(julDate=character(), pctFallen=numeric(), leafColor=numeric(), genus=character(), species=character(), year=integer(),id=integer(), school=character(),
                   sciName=character(),town=character(),lat=numeric(),lon=numeric(),elev=numeric())
for(id in unique(bySpecies$id)){
  firstOccur <- which(bySpecies$id == id)[1]
  rep=bySpecies[firstOccur,]
  reps <- rbind(reps,rep)
}

reducedReps <- data.frame(sciName=reps$sciName,town=reps$town,lat=reps$lat,lon=reps$lon, id=paste(reps$sciName,reps$town))
mostTrees <- subset(reducedReps, sciName %in% c("Acer rubrum", "Acer saccharum", "Quercus rubrum", "Fagus grandifolia"))

treeSummary <- data.frame(sciName=character(),town=character(),lat=numeric(),lon=numeric())

numberVec <- c()
for(id in unique(mostTrees$id)){
  treeSummary <- rbind(treeSummary,mostTrees[which(mostTrees$id==id)[1],][1:4])
  numberVec <- c(numberVec,length(which(mostTrees$id==id)))
}
treeSummary$num <- numberVec
treeSummary$lon <- as.numeric(as.character(treeSummary$lon))
treeSummary$lat <- as.numeric(as.character(treeSummary$lat))

mapString=ggmap(get_map(location="Massachusetts", zoom=7, maptype="satellite"), extent = "device")
mapString
map1=mapString+
  geom_jitter(data=treeSummary, aes(x=lon, y=lat, color=sciName, size=num),width=.03, height=.03)+
  scale_size_continuous(range=c(2,12))+
  theme(legend.text=element_text(size = 14))+
  scale_color_manual(values=c("red","forestgreen","blue","orange","yellow","violet"))
map1

#Second graph is number of unique observations

mostTrees_obs <- subset(bySpecies, sciName %in% c("Acer rubrum", "Acer saccharum", "Quercus rubrum", "Fagus grandifolia"))
reduced_obs <- data.frame(sciName=mostTrees_obs$sciName,town=mostTrees_obs$town,lat=mostTrees_obs$lat,lon=mostTrees_obs$lon, id=paste(mostTrees_obs$sciName,mostTrees_obs$town))
treeSummary_obs <- data.frame(sciName=character(),town=character(),lat=numeric(),lon=numeric())
numberVec <- c()
for(id in unique(reduced_obs$id)){
  treeSummary_obs <- rbind(treeSummary_obs,reduced_obs[which(reduced_obs$id==id)[1],][1:4])
  numberVec <- c(numberVec,length(which(reduced_obs$id==id)))
}
treeSummary_obs$num <- numberVec
treeSummary_obs$lon <- as.numeric(as.character(treeSummary_obs$lon))
treeSummary_obs$lat <- as.numeric(as.character(treeSummary_obs$lat))

map2=mapString+
  geom_jitter(data=treeSummary_obs, aes(x=lon, y=lat, color=sciName, size=num),width=.03, height=.03)+
  scale_size_continuous(range=c(2,12))+
  theme(legend.text=element_text(size = 14))+
  scale_color_manual(values=c("red","forestgreen","blue","orange","yellow"))
map2


#Get info on the amount of data we have on each genus
genusInfoFrame=data.frame(Genus=c("numTrees", "numObs", "numYears"))
for (i in 1:length(allGenera)){
  thisFrame=subset(fallPheno, fallPheno$genus==allGenera[i])
  uniques=unique(thisFrame$uniqTreeID)
  numTrees=length(uniques)
  numObs=nrow(thisFrame)
  numYears=length(unique(thisFrame$Year))
  genusInfoFrame[allGenera[i]] <- c(numTrees,numObs, numYears)
}

#set up data frame
byGenusFrame=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- rep(0,length(allDates))
}
colnames(byGenusFrame) <- c("Date", allGenera)


f <- function(index,genusList, dateList, column){
  allVals=subset(sorted, genus==genusList[index])
  thisGenus=c()
  for (date in dateList){
    thisDate=subset(allVals, julDate==date)[column][[1]]
    avg=mean(thisDate, na.rm=TRUE)
    thisGenus=c(thisGenus,avg)
  }
  return (thisGenus)
}


######################fallen leaves as percent of total leaves, by Julian date, by Genus######################

#correctlyDone=subset(sorted, pctFallen <=1)

for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- f(i,allGenera,allDates, "pctFallen")
}

#plot each species on its own graph

p <- function(x){
  nm=names(x)
  for (i in 2:length(nm)){
    plots <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
      scale_y_continuous(limits=c(-.1, 1.1)) +
      scale_x_continuous(limits=c(240, 350)) +
      annotate("text", x = 253, y = 1.0, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 253, y = 0.9, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 253, y = 0.8, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Number of Leaves Fallen by Julian Date, ", nm[i]), x="Julian Date", y="Percent Fallen")
    ggsave(plots,filename=paste("leafFall",nm[i],".png",sep=""))
  }
}
p(byGenusFrame)

justQAndF <- data.frame("date"=byGenusFrame[1],"Fagus"= byGenusFrame[13], "Quercus"=byGenusFrame[32])
p(justQAndF)

##########updates 12/9#######
#Address the questions in Pam's email from 12/7
#First: Is there a data quality issue at the end of the seaseon for oak and beech? Do teachers stop inputting data after their leaves have fallen?
quercus <- subset(byGenera, genus=="Quercus")
hist(quercus$julDate, breaks=50, main="Number of entries per Julian Day, Quercus", xlab="Julian Day", ylab="Number of Entries")

fagus <- subset(byGenera, genus=="Fagus")
hist(fagus$julDate, breaks=50, main="Number of entries per Julian Day, Fagus", xlab="Julian Day", ylab="Number of Entries")

#Answer: Yes, there is a steep dropoff of data entry towards the end of the season. Solutions: disregard that data? Need to investigate further. 

finalDays=subset(fagus, julDate>320)

plot <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
  scale_y_continuous(limits=c(-.1, 1.1)) +
  scale_x_continuous(limits=c(240, 350)) +
  annotate("text", x = 253, y = 1.0, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
  annotate("text", x = 253, y = 0.9, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
  annotate("text", x = 253, y = 0.8, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
  labs(title=paste0("Average Number of Leaves Fallen by Julian Date, ", nm[i]), x="Julian Date", y="Percent Fallen")



#Plot all the species on one graph (big mess!)
#data_long <- melt(byGenusFrame, id="Date")

######################Tree color by Julian Date

graph1=ggplot() +
  stat_smooth(data=overallAvgColor, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgColor, aes(x=julDate, y=values)) +
  scale_y_continuous(limits=c(0.9, 4.1)) +
  scale_x_continuous(limits=c(240, 350)) +
  annotate("text", x = 253, y = 4.0, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 253, y = 3.7, label = paste0("Number of Observations: ",numObs))+
  labs(title="Overall Average Tree Color by Julian Date", x ="Julian Date", y="Percent Fallen")+
  stat_smooth(data=overallAvgFallen, aes(x=julDate, y=values*3+1, colour="red"))
graph1

###################### tree color as percent of total leaves, by Julian date, by Genus########################

for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- f(i,allGenera,allDates, "leafColor")
}

#plot each species on its own graph

p <- function(x){
  nm=names(x)
  for (i in 2:length(nm)){
    plots <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
      scale_y_continuous(limits=c(0.9, 4.1)) +
      scale_x_continuous(limits=c(240, 350)) +
      annotate("text", x = 253, y = 4.0, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 253, y = 3.8, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 253, y = 3.6, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Tree Color Score by Julian Date, ", nm[i]), x="Julian Date", y="Tree Color")
    ggsave(plots,filename=paste("treeColor",nm[i],".png",sep=""))
  }
}
p(byGenusFrame)

######################Tree Color and Leaf Fall Combined by Genus ###########################

#Set up tree color data frame
byGenusColor=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusColor[i+1] <- rep(0,length(allDates))
}
colnames(byGenusColor) <- c("Date", allGenera)

for (i in 1:length(allGenera)){
  byGenusColor[i+1] <- f(i,allGenera,allDates, "leafColor")
}

#Set up leaf fall data frame
byGenusLeafFall=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusLeafFall[i+1] <- rep(0,length(allDates))
}
colnames(byGenusLeafFall) <- c("Date", allGenera)

for (i in 1:length(allGenera)){
  byGenusLeafFall[i+1] <- f(i,allGenera,allDates, "pctFallen")
}

#Graph them together
p <- function(x,y){
  nm=names(x)
  for (i in 2:length(nm)){
    normalizedColor=(x[nm[i]][[1]]-1)/3
    leafFallData=y[nm[i]][[1]]
    thisFrame=data.frame(Date=rep(x$Date , 2), value=c(normalizedColor,leafFallData), variable=c(rep("Color",length(x$Date)),rep("leafFall",length(x$Date))))
    usable=subset(thisFrame, value<5)
    plots <- ggplot() + 
      geom_point(data=usable, aes(x=Date, y=value, colour=variable)) + 
      stat_smooth(data=usable, aes(x=Date, y=value, colour=variable)) + 
      scale_y_continuous(limits=c(-0.1, 1.1)) +
      scale_x_continuous(limits=c(240, 350)) +
      annotate("text", x = 253, y = 1.1, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 253, y = 1.05, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 253, y = 1.0, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Tree Color and Leaf Fall by Julian Date, ", nm[i]), x="Julian Date", y="Normalized Tree Color Score, Percent of Leaves Fallen")
    ggsave(plots,filename=paste("combined",nm[i],".png",sep=""))
  }
}
p(byGenusColor, byGenusLeafFall)



############################################
###### Spring Phenology Data ############
############################################

################### Formatting ########################

#read in the spring phenology data
springPheno=read.csv("../spring_phenology.csv")

#read in the species code info
speciesCodes=read.csv("../species_codes.csv")

#give a unique identifier to each tree
springPheno$schoolNum=as.numeric(as.factor(springPheno$School.Code))
springPheno$uniqTreeID=(springPheno$schoolNum*100 + springPheno$Tree.ID)
springPheno$Date=as.Date(springPheno$Date)
springPheno$Year=format(springPheno$Date, "%Y")

#associate the trees with species
vec <- c()
for (i in seq(1,length(springPheno$Species.Code))){
  tryCatch(vec[i] <- (which(speciesCodes$Species.Code==as.character(springPheno$Species.Code[i]))), 
           error=function(e){vec[i] <- 89})
}

#Assign Scientific names to each tree
SciNames = as.character(speciesCodes[vec,3])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
springPheno$genus = genusVec
springPheno$species = speciesVec

#Get Open Buds as a percentage of total buds
springPheno$pctOpen=springPheno$Open.Buds/springPheno$Total.Buds

############# GRAPHING ######################

####################General Graphing Functions and preparation#############################################
graphingFrame=data.frame(julDate=springPheno$Julian, pctOpen=springPheno$pctOpen, leafLength=springPheno$Leaf.Length..cm., genus=springPheno$genus, species=springPheno$species, year=springPheno$Year)
attach(graphingFrame)
sorted=graphingFrame[order(julDate),]
allDates=unique(sorted$julDate)

overallStatFrame=springPheno[order(springPheno$uniqTreeID),]
numTrees=length(unique(overallStatFrame$uniqTreeID))
numObs=length(overallStatFrame$uniqTreeID)
numYears=length(unique(overallStatFrame$Year))

overviewInfo <- function(data, column, allDates){
  allAverages=c()
  for (date in allDates){
    allVals=subset(data, julDate==date)[column][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}

###################### Open buds as percent of total buds, by Julian date,##############################

correctlyDone=subset(sorted, pctOpen <=1)

OpenAverages=overviewInfo(correctlyDone,"pctOpen",allDates)
overallAvgOpen=data.frame(julDate=allDates, values=OpenAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgOpen, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgOpen, aes(x=julDate, y=values)) +
  scale_y_continuous(limits=c(-.1, 1.1)) +
  scale_x_continuous(limits=c(75, 170)) +
  annotate("text", x = 90, y = 1.1, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 90, y = 1.05, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 90, y = 1, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Percent of Buds Open by Julian Date", x ="Julian Date", y="Percent Open")
ggsave(graph1, filename = "budOpenOverall.png")
graph1

###################### Leaf length by Julian Date ############################

leafAverages=overviewInfo(sorted,"leafLength",allDates)
overallAvgLength=data.frame(julDate=allDates, values=leafAverages)

graph2=ggplot() +
  stat_smooth(data=overallAvgLength, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgLength, aes(x=julDate, y=values)) +
  #scale_y_continuous(limits=c(0.9, 4.1)) +
  scale_x_continuous(limits=c(75, 170)) +
  annotate("text", x = 90, y = 15, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 90, y = 14, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 90, y = 13, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Leaf Length by Julian Date", x ="Julian Date", y="Tree Color")
ggsave(graph2, filename = "leafLengthOverall.png")
graph2

######################## Open buds and leaf length Combined ###################


combiDate=rep(allDates,2)

openAverages=overviewInfo(sorted,"pctOpen",allDates)
lengthAverages=overviewInfo(sorted,"leafLength",allDates)
reScaledLengths=lengthAverages/max(lengthAverages, na.rm=TRUE)
combiValue=c(openAverages,reScaledLengths)

combiVariable=c(rep("budOpen", length(allDates)), rep("leafLength", length(allDates)))

combinedFrame=data.frame(combiDate,combiValue,combiVariable)
usable=subset(combinedFrame, combiValue<2)

graph1 <- ggplot() + 
  geom_point(data=usable, aes(x=combiDate, y=combiValue, colour=combiVariable)) + 
  stat_smooth(data=usable, aes(x=combiDate, y=combiValue, colour=combiVariable)) + 
  scale_y_continuous(limits=c(-.1, 1.1)) +
  scale_x_continuous(limits=c(75, 170)) +
  annotate("text", x = 90, y = 1.1, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 90, y = 1.05, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 90, y = 1, label = paste0("Number of Years: ",numYears))+
  labs(title="Bud Open and Leaf Length by Julian Date", x="Julian Date", y="Normalized Leaf Length, Percent of Buds Open", colour="Variable")
ggsave(graph1,filename="combinedSpringOverall.png")
graph1

######################By Genus Graphing Functions and preparation#############################################

byGenera=graphingFrame[order(graphingFrame$genus),]
allGenera=unique(as.character(byGenera$genus))

#Get info on the amount of data we have on each genus
genusInfoFrame=data.frame(Genus=c("numTrees", "numObs", "numYears"))
for (i in 1:length(allGenera)){
  thisFrame=subset(springPheno, springPheno$genus==allGenera[i])
  uniques=unique(thisFrame$uniqTreeID)
  numTrees=length(uniques)
  numObs=nrow(thisFrame)
  numYears=length(unique(thisFrame$Year))
  genusInfoFrame[allGenera[i]] <- c(numTrees,numObs, numYears)
}

#set up data frame
byGenusFrame=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- rep(0,length(allDates))
}
colnames(byGenusFrame) <- c("Date", allGenera)


f <- function(index,genusList, dateList, column){
  allVals=subset(sorted, genus==genusList[index])
  thisGenus=c()
  for (date in dateList){
    thisDate=subset(allVals, julDate==date)[column][[1]]
    avg=mean(thisDate, na.rm=TRUE)
    thisGenus=c(thisGenus,avg)
  }
  return (thisGenus)
}

###################### Open buds as percent of total buds, by Julian date, by Genus##############################

correctlyDone=subset(sorted, pctOpen<=1)

for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- f(i,allGenera,allDates, "pctOpen")
}

#plot each species on its own graph

p <- function(x){
  nm=names(x)
  for (i in 2:length(nm)){
    plots <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
      scale_y_continuous(limits=c(-.1, 1.1)) +
      scale_x_continuous(limits=c(75, 170)) +
      annotate("text", x = 85, y = 1.1, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 85, y = 1.05, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 85, y = 1.0, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Number of Buds Open by Julian Date, ", nm[i]), x="Julian Date", y="Percent Fallen")
    ggsave(plots,filename=paste("budOpen",nm[i],".png",sep=""))
  }
}
p(byGenusFrame)

###################### Leaf Length by Julian date, by Genus ##############################


#Absolute Length in mm

for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- f(i,allGenera,allDates, "leafLength")
}

#plot each species on its own graph

p <- function(x){
  nm=names(x)
  for (i in 2:length(nm)){
    maxVal=max(x[nm[i]][[1]], na.rm=TRUE)
    plots <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
      scale_y_continuous(limits=c(-.1, maxVal)) +
      scale_x_continuous(limits=c(75, 170)) +
      annotate("text", x = 85, y = maxVal, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 85, y = maxVal-0.75, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 85, y = maxVal-1.5, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Leaf Length by Julian Date, ", nm[i]), x="Julian Date", y="Leaf Length (mm)")
    ggsave(plots,filename=paste("leafLength",nm[i],".png",sep=""))
  }
}
p(byGenusFrame)


#Normalized Length

for (i in 1:length(allGenera)){
  byGenusFrame[i+1] <- f(i,allGenera,allDates, "leafLength")
  byGenusFrame[i+1] <- byGenusFrame[i+1]/max(byGenusFrame[i+1],na.rm=TRUE)
}
p <- function(x){
  nm=names(x)
  for (i in 2:length(nm)){
    plots <- ggplot(data=x, aes(x=Date)) + aes_string(y = nm[i]) + geom_point() + stat_smooth()+
      scale_y_continuous(limits=c(-.1, 1.1)) +
      scale_x_continuous(limits=c(75, 170)) +
      annotate("text", x = 85, y = 1.1, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 85, y = 1.05, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 85, y = 1.0, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Leaf Length by Julian Date, ", nm[i]), x="Julian Date", y="Leaf Length (normalized)")
    ggsave(plots,filename=paste("leafLengthNormalized",nm[i],".png",sep=""))
  }
}
p(byGenusFrame)

###################### Open buds and leaf length combined, by Genus ###########################

#Set up tree color data frame
byGenusBuds=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusBuds[i+1] <- rep(0,length(allDates))
}
colnames(byGenusBuds) <- c("Date", allGenera)

for (i in 1:length(allGenera)){
  byGenusBuds[i+1] <- f(i,allGenera,allDates, "pctOpen")
}

#Set up leaf length data frame
byGenusLeafLength=data.frame(Date=allDates)
for (i in 1:length(allGenera)){
  byGenusLeafLength[i+1] <- rep(0,length(allDates))
}
colnames(byGenusLeafLength) <- c("Date", allGenera)

for (i in 1:length(allGenera)){
  byGenusLeafLength[i+1] <- f(i,allGenera,allDates, "leafLength")
  byGenusLeafLength[i+1] <- byGenusLeafLength[i+1]/max(byGenusLeafLength[i+1],na.rm=TRUE)
}
#Graph them together
p <- function(x,y){
  nm=names(x)
  for (i in 2:length(nm)){
    normalizedLength=x[nm[i]][[1]]
    budOpenData=y[nm[i]][[1]]
    thisFrame=data.frame(Date=rep(x$Date , 2), value=c(normalizedLength,budOpenData), variable=c(rep("Leaf Length",length(x$Date)),rep("Buds Open",length(x$Date))))
    usable=subset(thisFrame, value<5)
    plots <- ggplot() + 
      geom_point(data=usable, aes(x=Date, y=value, colour=variable)) + 
      stat_smooth(data=usable, aes(x=Date, y=value, colour=variable)) + 
      scale_y_continuous(limits=c(-0.1, 1.1)) +
      scale_x_continuous(limits=c(75, 170)) +
      annotate("text", x = 85, y = 1.1, label = paste0("Number of Trees: ", genusInfoFrame[1,i])) +
      annotate("text", x = 85, y = 1.05, label = paste0("Number of Observations: ",genusInfoFrame[2,i])) +
      annotate("text", x = 85, y = 1.0, label = paste0("Number of Years: ",genusInfoFrame[3,i])) +
      labs(title=paste0("Average Leaf Length and Open Buds by Julian Date, ", nm[i]), x="Julian Date", y="Normalized Leaf Length, Percent of Buds Open")
    ggsave(plots,filename=paste("combinedSpring",nm[i],".png",sep=""))
  }
}
p(byGenusLeafLength, byGenusBuds)



############################################
###### Hemlock Wooly Adelgid Data ##########
############################################

##############FORMATTING###################

#read in the wooly adelgid data
hemWA=read.csv("../hemlock_wooly_adelgid.csv")


#give a unique identifier to each tree
hemWA$schoolNum=as.numeric(as.factor(hemWA$School.Code))
hemWA$uniqTreeID=(hemWA$schoolNum*100 + hemWA$Tree.ID)
hemWA$Date=as.Date(hemWA$Date)
hemWA$Year=format(hemWA$Date, "%Y")

####################General Graphing Functions and preparation#############################################
graphingFrame=data.frame(date=hemWA$Date, health=hemWA$Health,wool=hemWA$Wool, eggs=hemWA$Eggs, growth=hemWA$Growth..cm., tree=hemWA$uniqTreeID, year=hemWA$Year)
attach(graphingFrame)
sorted=graphingFrame[order(date),]
allDates=unique(sorted$date)
allYears=as.character(unique(sorted$year))

numTrees=length(unique(sorted$tree))
numObs=length(sorted$tree)
numYears=length(unique(sorted$year))

overviewInfo <- function(data, timeColumn, dataColumn, allDates){
  allAverages=c()
  for (time in allDates){
    allVals=subset(data, data[timeColumn][[1]]==time)[dataColumn][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}



############################################


############################################
###### Vernal Pool Data ####################
############################################

################### Formatting ########################

#read in the vernal pool data
vernalPool=read.csv("../vernal_pools.csv")


#Get year as an identifier
vernalPool$Date=as.Date(vernalPool$Date)
vernalPool$Year=format(vernalPool$Date, "%Y")

#give a unique identifier to each pool
vernalPool$teacherNum=as.numeric(as.factor(vernalPool$Teacher))
vernalPool$uniqPoolID=(vernalPool$teacherNum*100 + vernalPool$Pool)



#get max values of depth air temp, and water temp

maxVals <- function(data, variable, group){
  maxes=c()
  for (i in 1:nrow(data)){
  maxes=c(maxes,max(subset(data, data[group][[1]] == data[i,][group][[1]])[variable][[1]], na.rm=TRUE))
  }
  return(maxes)
}

vernalPool$maxDepth=maxVals(vernalPool, "Depth..cm.", "uniqPoolID")
vernalPool$maxAirTemp=maxVals(vernalPool, "Air.Temp..c.", "uniqPoolID")
vernalPool$maxWaterTemp=maxVals(vernalPool, "Water.Temp..c.", "uniqPoolID")

#get percentages of all variables
vernalPool$pctDiam=vernalPool$Diameter..m./vernalPool$Max.Diameter..m.
vernalPool$pctDepth=vernalPool$Depth..cm./vernalPool$maxDepth
vernalPool$pctAirTemp=vernalPool$Air.Temp..c./vernalPool$maxAirTemp
vernalPool$pctWaterTemp=vernalPool$Water.Temp..c./vernalPool$maxWaterTemp

####################General Graphing Functions and preparation#############################################
graphingFrame=data.frame(date=vernalPool$Date, pool=vernalPool$uniqPoolID,diameter=vernalPool$pctDiam, 
                         depth=vernalPool$pctDepth, airTemp=vernalPool$pctAirTemp, 
                         waterTemp=vernalPool$pctWaterTemp, julian=vernalPool$Julian, year=vernalPool$Year)
attach(graphingFrame)
sorted=graphingFrame[order(julian),]
allDates=unique(sorted$julian)
allYears=as.character(unique(sorted$year))

numPools=length(unique(sorted$pool))
numObs=length(sorted$pool)
numYears=length(unique(sorted$year))

equinoxDates=data.frame(julian=c(79,266), type=c("Spring Equinox","Fall Equinox"))
equinoxDates$type <- factor(equinoxDates$type, levels=c("Spring Equinox","Fall Equinox"))

# equinoxDates=data.frame(julian=c(79,171,266), type=c("Spring Equinox","Summer Solstice","Fall Equinox"))
# equinoxDates$type <- factor(equinoxDates$type, levels=c("Spring Equinox", "Summer Solstice","Fall Equinox"))

overviewInfo <- function(data, timeColumn, dataColumn, allDates){
  allAverages=c()
  for (time in allDates){
    allVals=subset(data, data[timeColumn][[1]]==time)[dataColumn][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}

###################### Pool Diameter by Julian Date ############################

diameterAverages=overviewInfo(graphingFrame,"julian", "diameter",allDates)
overallAvgDiam=data.frame(julDate=allDates, values=diameterAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgDiam, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgDiam, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  scale_color_manual(values=c("green", "red")) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Pool Diameter by Julian Date", x ="Julian Date", y="Diameter/Max Diameter", color="")
#ggsave(graph1, filename = "diameterOverall.png")
graph1

###################### Pool Diameter by Real Date ############################

diameterYearlyAverages=overviewInfo(graphingFrame, "date", "diameter", unique(graphingFrame$date))
overallYearlyAvgDiam=data.frame(date=unique(graphingFrame$date), values=diameterYearlyAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallYearlyAvgDiam, aes(x=date, y=values), span=.1) +
  geom_point(data=overallYearlyAvgDiam, aes(x=date, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  #scale_x_continuous(limits=c(0, 360)) +
  # geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  # scale_color_manual(values=c("green", "red")) +
  annotate("text", x = as.Date("2006-01-01"), y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = as.Date("2006-01-01"), y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = as.Date("2006-01-01"), y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Pool Diameter by Julian Date", x ="Julian Date", y="Diameter/Max Diameter", color="")
#ggsave(graph1, filename = "diameterOverall.png")
graph1

###################### Percent Pool Depth by Julian Date ############################

depthAverages=overviewInfo(graphingFrame,"julian", "depth",allDates)
overallAvgDepth=data.frame(julDate=allDates, values=depthAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgDepth, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgDepth, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  scale_color_manual(values=c("green", "red")) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Pool Depth by Julian Date", x ="Julian Date", y="Depth (cm)")
#ggsave(graph1, filename = "depthOverall.png")
graph1

###################### Percent Air Temp by Julian Date ############################

airAverages=overviewInfo(graphingFrame,"julian", "airTemp",allDates)
overallAvgAir=data.frame(julDate=allDates, values=airAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgAir, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgAir, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  scale_color_manual(values=c("green", "red")) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Air Temperature by Julian Date", x ="Julian Date", y="Air Temp/Max Air Temp")
#ggsave(graph1, filename = "airTempOverall.png")
graph1

###################### Percent Air Temp by Real Date ###################
airTempYearlyAverages=overviewInfo(graphingFrame, "date", "airTemp", unique(graphingFrame$date))
overallYearlyAvgAirTemp=data.frame(date=unique(graphingFrame$date), values=airTempYearlyAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallYearlyAvgDiam, aes(x=date, y=values), span=.1) +
  geom_point(data=overallYearlyAvgAirTemp, aes(x=date, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  #scale_x_continuous(limits=c(0, 360)) +
  # geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  # scale_color_manual(values=c("green", "red")) +
  annotate("text", x = as.Date("2006-01-01"), y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = as.Date("2006-01-01"), y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = as.Date("2006-01-01"), y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Pool Diameter by Julian Date", x ="Julian Date", y="Diameter/Max Diameter", color="")
#ggsave(graph1, filename = "diameterOverall.png")
graph1

###################### Percent Water Temp by Julian Date ############################

waterAverages=overviewInfo(graphingFrame,"julian", "waterTemp",allDates)
overallAvgWater=data.frame(julDate=allDates, values=waterAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgWater, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgWater, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  geom_vline(data=equinoxDates, aes(xintercept=julian, color=type), size=3, alpha=.6) +
  scale_color_manual(values=c("green", "red")) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Water Temperature by Julian Date", x ="Julian Date", y="Water Temp/Max Water Temp",
       color="")
#ggsave(graph1, filename = "waterTempOverall.png")
graph1

###################### All Smooth Lines ############################

allLines=data.frame(julDate=rep(overallAvgAir$julDate, 4), 
                    values=c(overallAvgAir$values, overallAvgWater$values, overallAvgDepth$values, overallAvgDiam$values),
                    measure=c(rep("Air Temp", nrow(overallAvgAir)), rep("Water Temp", nrow(overallAvgWater)),
                              rep("Pool Depth", nrow(overallAvgDepth)), rep("Pool Diameter", nrow(overallAvgDiam))))

graph1=ggplot() +
  stat_smooth(data=allLines, aes(x=julDate, y=values, color=measure, fill=measure), size=2) +
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  geom_vline(data=equinoxDates, aes(xintercept=julian, alpha=type), size=3) +
  scale_color_manual(values=c( "skyblue","orange", "deeppink","seagreen"), 
                     breaks=c("Air Temp", "Water Temp", "Pool Depth", "Pool Diameter")) +
  scale_alpha_manual(values=c(.4,.6))+
  scale_fill_manual(values=c("skyblue","orange", "deeppink","seagreen"),
                    breaks=c("Air Temp", "Water Temp", "Pool Depth", "Pool Diameter")) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="All Vernal Pool Measurements", x ="Julian Date", y="Percent of Max Value",
       alpha='')
ggsave(filename="vernalPool.png", plot=graph1)
graph1
  

###################### Percent Pool Diameter by Air Temp #############

tempFrame=data.frame(airTemp=round(graphingFrame$airTemp, 2), diameter=graphingFrame$diameter)
allTemps=unique(tempFrame$airTemp)

diamByTemp=overviewInfo(tempFrame, "airTemp", "diameter", allTemps)
diamByTempFrame=data.frame(temp=allTemps, values=diamByTemp)

graph1=ggplot() +
  stat_smooth(data=diamByTempFrame, aes(x=temp, y=values)) +
  geom_point(data=diamByTempFrame, aes(x=temp, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 1)) +
  annotate("text", x = .5, y = 1.15, label = paste0("Number of Pools: ", numPools)) +
  annotate("text", x = .5, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = .5, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Pool Diameter by Temperature", x ="Temp (pct of max)", y="Pool Diameter/Max Pool Diameter",
       color="")
#ggsave(graph1, filename = "diameterByTemp.png")
graph1



############################################
###### Streams Data ####################
############################################

################# Formatting ################

#read in the vernal pool data
streams=read.csv("../streams.csv")


#Get year as an identifier
streams$Date=as.Date(streams$Date)
streams$Year=format(streams$Date, "%Y")

#give a unique identifier to each location
streams$teacherNum=as.numeric(as.factor(streams$Teacher))
streams$uniqLocationID=(streams$teacherNum*100 + streams$Location)



#get max values of depth, air temp, and water temp

maxVals <- function(data, variable, group){
  maxes=c()
  for (i in 1:nrow(data)){
    maxes=c(maxes,max(subset(data, data[group][[1]] == data[i,][group][[1]])[variable][[1]], na.rm=TRUE))
  }
  return(maxes)
}

streams$maxDepth=maxVals(streams, "Depth..cm.", "uniqLocationID")
streams$maxAirTemp=maxVals(streams, "Air.Temp..c.", "uniqLocationID")
streams$maxWaterTemp=maxVals(streams, "Water.Temp..c.", "uniqLocationID")

#get percentages of all variables
streams$pctDepth=streams$Depth..cm./streams$maxDepth
streams$pctAirTemp=streams$Air.Temp..c./streams$maxAirTemp
streams$pctWaterTemp=streams$Water.Temp..c./streams$maxWaterTemp

####################General Graphing Functions and preparation ######################

graphingFrame=data.frame(date=streams$Date, location=streams$uniqLocationID, 
                         depth=streams$pctDepth, airTemp=streams$pctAirTemp, waterTemp=streams$pctWaterTemp, 
                         flow=streams$Flow.Rate, julian=streams$Julian, year=streams$Year)

attach(graphingFrame)
sorted=graphingFrame[order(julian),]
allDates=unique(sorted$julian)
allYears=as.character(unique(sorted$year))

numLocs=length(unique(sorted$location))
numObs=length(sorted$location)
numYears=length(unique(sorted$year))

equinoxDates=data.frame(julian=c(79,266), type=c("Spring Equinox","Fall Equinox"))
equinoxDates$type <- factor(equinoxDates$type, levels=c("Spring Equinox","Fall Equinox"))

# equinoxDates=data.frame(julian=c(79,171,266), type=c("Spring Equinox","Summer Solstice","Fall Equinox"))
# equinoxDates$type <- factor(equinoxDates$type, levels=c("Spring Equinox", "Summer Solstice","Fall Equinox"))

overviewInfo <- function(data, timeColumn, dataColumn, allDates){
  allAverages=c()
  for (time in allDates){
    allVals=subset(data, data[timeColumn][[1]]==time)[dataColumn][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}

###################### Percent Pool Depth by Julian Date ############################

depthAverages=overviewInfo(graphingFrame,"julian", "depth",allDates)
overallAvgDepth=data.frame(julDate=allDates, values=depthAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgDepth, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgDepth, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 1.15)) +
  scale_x_continuous(limits=c(0, 360)) +
  annotate("text", x = 180, y = 1.15, label = paste0("Number of Locations: ", numLocs)) +
  annotate("text", x = 180, y = 1.1, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 1.05, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Stream Depth by Julian Date", x ="Julian Date", y="Depth (percent of max)")
#ggsave(graph1, filename = "streamDepthOverall.png")
graph1

###################### Percent Pool Depth by Julian Date ############################

flowAverages=overviewInfo(graphingFrame,"julian", "flow",allDates)
overallAvgFlow=data.frame(julDate=allDates, values=flowAverages)

#Smoothed Line
graph1=ggplot() +
  stat_smooth(data=overallAvgFlow, aes(x=julDate, y=values)) +
  geom_point(data=overallAvgFlow, aes(x=julDate, y=values), color="grey40", alpha=.95) +
  guides(alpha=FALSE)+
  scale_y_continuous(limits=c(0, 5)) +
  scale_x_continuous(limits=c(0, 360)) +
  annotate("text", x = 180, y = 5, label = paste0("Number of Locations: ", numLocs)) +
  annotate("text", x = 180, y = 4.9, label = paste0("Number of Observations: ",numObs))+
  annotate("text", x = 180, y = 4.8, label = paste0("Number of Years: ",numYears))+
  labs(title="Overall Average Stream Depth by Julian Date", x ="Julian Date", y="Depth (percent of max)")
#ggsave(graph1, filename = "streamDepthOverall.png")
graph1

############################################
###### Changing Forests Data ####################
############################################

################# Formatting ################

#read in the vernal pool data
forests=read.csv("../changing_forests.csv")

#read in the species code info
speciesCodes=read.csv("../species_codes.csv")


#Get year as an identifier
forests$Date=as.Date(forests$Date)
forests$Year=format(forests$Date, "%Y")

#give a unique identifier to each location and tree
forests$teacherNum=as.numeric(as.factor(forests$Teacher))
forests$uniqPlotID=(forests$teacherNum*100 + forests$Plot)
forests$uniqTreeID=(forests$uniqPlotID*10)+forests$Tree.ID


vec <- c()
for (i in seq(1,length(forests$Species.Code))){
  tryCatch(vec[i] <- (which(speciesCodes$Species.Code==as.character(forests$Species.Code[i]))), 
           error=function(e){vec[i] <- 89})
}

#Assign Scientific names to each tree
SciNames = as.character(speciesCodes[vec,3])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
forests$genus = genusVec
forests$species = speciesVec



#get max values of depth, air temp, and water temp

maxVals <- function(data, variable, group){
  maxes=c()
  for (i in 1:nrow(data)){
    maxes=c(maxes,max(subset(data, data[group][[1]] == data[i,][group][[1]])[variable][[1]], na.rm=TRUE))
  }
  return(maxes)
}

streams$maxDepth=maxVals(streams, "Depth..cm.", "uniqLocationID")
streams$maxAirTemp=maxVals(streams, "Air.Temp..c.", "uniqLocationID")
streams$maxWaterTemp=maxVals(streams, "Water.Temp..c.", "uniqLocationID")

#get percentages of all variables
streams$pctDepth=streams$Depth..cm./streams$maxDepth
streams$pctAirTemp=streams$Air.Temp..c./streams$maxAirTemp
streams$pctWaterTemp=streams$Water.Temp..c./streams$maxWaterTemp


