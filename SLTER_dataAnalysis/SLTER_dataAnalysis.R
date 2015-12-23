library("ggplot2")
library("reshape2")

############################################
###### Fall Phenology Data ############
############################################

################### Formatting ########################

#read in the fall phenology data
fallPheno=read.csv("../fall_phenology.csv")

#read in the species code info
speciesCodes=read.csv("../species_codes.csv")

#give a unique identifier to each tree
fallPheno$schoolNum=as.numeric(as.factor(fallPheno$School.Code))
fallPheno$uniqTreeID=(fallPheno$schoolNum*100 + fallPheno$Tree.ID)
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
fallPheno$pctFallen=fallPheno$Fallen.Leaves/fallPheno$Total.Leaves

############# GRAPHING ######################

####################General Graphing Functions and preparation#############################################

graphingFrame=data.frame(julDate=fallPheno$Julian, pctFallen=fallPheno$pctFallen, leafColor=fallPheno$Tree.Color, genus=fallPheno$genus, species=fallPheno$species, year=fallPheno$Year)
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

correctlyDone=subset(sorted, pctFallen <=1)

fallenAverages=overviewInfo(correctlyDone,"pctFallen",allDates)
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
  annotate("text", x = 253, y = 1.0, label = paste0("Number of Trees: ", numTrees)) +
  annotate("text", x = 253, y = 0.95, label = paste0("Number of Observations: ",numObs)) +
  annotate("text", x = 253, y = 0.9, label = paste0("Number of Years: ",numYears)) +
  labs(title="Tree Color and Leaf Fall by Julian Date", x="Julian Date", y="Normalized Tree Color, Percent of Leaves Fallen")
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

######################fallen leaves as percent of total leaves, by Julian date, by Genus######################

correctlyDone=subset(sorted, pctFallen <=1)

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
  labs(title="Bud Open and Leaf Length by Julian Date", x="Julian Date", y="Normalized Leaf Length, Percent of Buds Open")
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
