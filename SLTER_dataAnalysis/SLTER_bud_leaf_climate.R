#growing degree days

######### Graphing data on growing season ###########
#####objective (from Pamela Snow):
# The first goal would be to graph the length of the growing season 
# for as many of our schoolyard sites as possible over time, 
# in an effort to answer our study question:  “Is the Growing season in 
# our study region getting longer as a result of Climate Change?

library(ggplot2)
#install.packages("extrafont")
library(extrafont)
font_import(pattern="[A/a]rial")
library(ggmap)

################# Format Data ####################
#read background data - school info and species info

schoolInfo=read.csv("data/school_info.csv", col.names = c("code","name","teachers","address","town","state",
                                                        "lat","lon","elev","buds","hwa", "vernal","stream","forest"))

speciesCodes=read.csv("data/species_codes.csv")

###### read and format bud burst
budBurst=read.csv("data/50p_budBurst.csv", col.names = c("school", "teacher", "year", "tree", "speciesCode", "budBurst"),
                  colClasses = c("character","character","numeric","numeric","character","numeric"))

#give a unique identifier to each tree
budBurst$schoolNum=as.numeric(as.factor(budBurst$school))
budBurst$uniqTreeID=(budBurst$schoolNum*100 + budBurst$tree)*10+1 #spring data ends with 1

######### read and format leaf fall
leafFall=read.csv("data/50p_leafFall.csv", col.names = c("school", "teacher", "year", "tree", "speciesCode", "leafFall"),
                  colClasses = c("character","character","numeric","numeric","character","numeric"))

#give a unique identifier to each tree
leafFall$schoolNum=as.numeric(as.factor(leafFall$school))
leafFall$uniqTreeID=(leafFall$schoolNum*100 + leafFall$tree)*10+2 #fall data ends with 2

####Combine Data
summaryData=data.frame(school=c(budBurst$school, leafFall$school), 
                       teacher=c(budBurst$teacher, leafFall$teacher),
                       year=c(as.character(budBurst$year), as.character(leafFall$year)), tree=c(budBurst$tree, leafFall$tree), 
                       speciesCode=c(budBurst$speciesCode,leafFall$speciesCode), 
                       value=c(budBurst$budBurst, leafFall$leafFall), schoolNum=c(budBurst$schoolNum, leafFall$schoolNum),
                       uniqTreeID=c(budBurst$uniqTreeID, leafFall$uniqTreeID), 
                       variable=c(rep("bud", nrow(budBurst)), rep("leaf", nrow(leafFall))), stringsAsFactors=FALSE)



#associate the tres with species
vec <- c()
for (i in seq(1,length(summaryData$speciesCode))){
  tryCatch(vec[i] <- (which(speciesCodes$Species.Code==as.character(summaryData$speciesCode[i]))), 
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
summaryData$genus = genusVec
summaryData$species = speciesVec

#Add location information
locVec <- c()
for (i in seq(1,length(summaryData$school))){
  (locVec[i] <- (which(schoolInfo$code==as.character(summaryData$school[i]))))
}
summaryData$town <- as.character(schoolInfo[locVec,5])
summaryData$lat <- as.character(schoolInfo[locVec,7])
summaryData$lon <- as.character(schoolInfo[locVec,8])
summaryData$elev <- as.character(schoolInfo[locVec,9])

################ Graphing Functions ################

overviewInfo <- function(data, timeColumn, dataColumn, allDates){
  allAverages=c()
  for (time in allDates){
    allVals=subset(data, data[timeColumn][[1]]==time)[dataColumn][[1]]
    avg=mean(allVals, na.rm=TRUE)
    allAverages=c(allAverages,avg)
  }
  return(allAverages)
}

allYears=unique(summaryData$year)

budsOnly=subset(summaryData, variable=="bud")
leavesOnly=subset(summaryData, variable=="leaf")


################## Overall Data Graph ########################

avgBudBurst=overviewInfo(budsOnly, "year", "value", allYears)
avgLeafFall=overviewInfo(leavesOnly, "year", "value", allYears)

graphingFrame=data.frame(x=as.character(allYears), budBurst=avgBudBurst, leafFall= avgLeafFall, group=rep(1, length(allYears)))
                         

graph1=ggplot()+
  geom_line(data=graphingFrame, aes(x=x, y=budBurst, group=group)) +
  geom_point(data=graphingFrame, aes(x=x, y=budBurst), size=2) +
  geom_line(data=graphingFrame, aes(x=x, y=leafFall, group=group)) +
  geom_point(data=graphingFrame, aes(x=x, y=leafFall), size=2) +  
  labs(x="Year", y="Julian Day", title="Yearly Bud Burst and Leaf Fall") +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL)
graph1

graphingFrame$x=as.numeric(as.character(graphingFrame$x))
graphingFrame$leavesOnDays=graphingFrame$leafFall-graphingFrame$budBurst

model <- lm(formula = leavesOnDays ~ x, data=graphingFrame, na.action=na.omit)
coefRM <- coef(model)
rsquare=round(summary(model)$r.squared, digits=4)

graph2=ggplot() + 
  geom_line(data=graphingFrame, aes(x=x, y=leavesOnDays, group=group))+
  geom_point(data=graphingFrame, aes(x=x, y=leavesOnDays)) +
  geom_abline(slope=coefRM[2], intercept=coefRM[1])+
  annotate("text", x=2015, y=153, label=paste("R^2:", rsquare))
  labs(x="Year", y="# Of Days", title="Leaves On Days, Overall")
graph2

################### Graph By Site ############################

#Set up data frame
allTowns=unique(summaryData$town)
rowNumber=length(allYears)*length(allTowns)
placeHolder=rep('NA', rowNumber)
siteFrame=data.frame(year=rep(allYears, length(allTowns)), town=placeHolder, budBurst=placeHolder, leafFall=placeHolder)

townVec=c()
for (i in allTowns){
  townVec=c(townVec, rep(i, length(allYears)))
}
siteFrame$town=townVec

budBurstVec=c()
for (i in allTowns){
  thisTown=subset(budsOnly, town==i)
  budBurstVec=c(budBurstVec,overviewInfo(thisTown, "year", "value", allYears))
}
siteFrame$budBurst=budBurstVec

leafFallVec=c()
for (i in allTowns){
  thisTown=subset(leavesOnly, town==i)
  leafFallVec=c(leafFallVec,overviewInfo(thisTown, "year", "value", allYears))
}
siteFrame$leafFall=leafFallVec

siteFrame$leavesOnDays <- siteFrame$leafFall - siteFrame$budBurst
siteFrame$year=as.numeric(as.character(siteFrame$year))

varVec=c()
interceptVec=c()
slopeVec=c()
r2Vec=c()
nNumVec=c()
latVec = c()
lonVec = c
for (i in allTowns){
  thisFrame=subset(siteFrame, town==i) 
  if(length(subset(thisFrame, is.na(leavesOnDays))$leavesOnDays) != length(thisFrame$leavesOnDays)){
    varVec <- c(varVec, i)
    thisModel <- lm(thisFrame$leavesOnDays~as.numeric(thisFrame$year), na.action=na.omit)
    thisCoef <- coef(thisModel)
    thisR2 <- round(summary(thisModel)$r.squared, digits=4)
    thisNNum <-  length(which(thisFrame$leavesOnDays != "NaN"))
    thisLat <- summaryData[which(summaryData$town ==i),][1,]$lat
    thisLon <- summaryData[which(summaryData$town ==i),][1,]$lon
    interceptVec <- c(interceptVec, as.numeric(thisCoef[1]))
    slopeVec <- c(slopeVec, as.numeric(thisCoef[2]))
    r2Vec <- c(r2Vec, thisR2)
    nNumVec <- c(nNumVec,thisNNum)
    latVec <- c(latVec, thisLat)
    lonVec <- c(lonVec, thisLon)
  }
}
modelFrame=data.frame(var=varVec, intercept=interceptVec, slope=slopeVec,r2=r2Vec, nNum=nNumVec)
modelFrame$lat=as.numeric(latVec)
modelFrame$lon=as.numeric(as.character(lonVec))[2:length(lonVec)]

modelFrame$lat=summaryData[which(summaryData$town ==modelFrame$var),][1,]$lat

graph1=ggplot()+
  geom_line(data=siteFrame, aes(x=year, y=budBurst, group=town, color=town)) +
  geom_point(data=siteFrame, aes(x=year, y=budBurst, color=town), size=2) +
  geom_line(data=siteFrame, aes(x=year, y=leafFall, group=town, color=town)) +
  geom_point(data=siteFrame, aes(x=year, y=leafFall, color=town), size=2) +  
  labs(x="Year", y="Julian Day", title="Yearly Bud Burst and Leaf Fall by Town") +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL)
graph1

mapString=ggmap(get_map(location="Massachusetts", zoom=9, source="stamen", maptype="terrain-background", color="bw"), extent = "device")
map1=mapString+
  geom_point(data=modelFrame, aes(x=lon, y=lat, color=slope, size=nNum)) + #, alpha=.01,na.rm=TRUE)+
  scale_color_gradient(low="blue", high="red")
  

################### Graph By Species ############################

#Set up data frame
allSpecies=unique(summaryData$speciesCode)
rowNumber=length(allYears)*length(allSpecies)
placeHolder=rep('NA', rowNumber)
speciesFrame=data.frame(year=rep(allYears, length(allSpecies)), species=placeHolder, budBurst=placeHolder, leafFall=placeHolder)
speciesFrame$year = as.numeric(as.character(speciesFrame$year))

speciesVec=c()
for (i in allSpecies){
  speciesVec=c(speciesVec, rep(i, length(allYears)))
}
speciesFrame$species=speciesVec

budBurstVec=c()
for (i in allSpecies){
  thisSpecies=subset(budsOnly, speciesCode==i)
  budBurstVec=c(budBurstVec,overviewInfo(thisSpecies, "year", "value", allYears))
}
speciesFrame$budBurst=budBurstVec

leafFallVec=c()
for (i in allSpecies){
  thisSpecies=subset(leavesOnly, speciesCode==i)
  leafFallVec=c(leafFallVec,overviewInfo(thisSpecies, "year", "value", allYears))
}
speciesFrame$leafFall=leafFallVec

speciesFrame$leavesOnDays = speciesFrame$leafFall - speciesFrame$budBurst

graph1=ggplot()+
  geom_line(data=speciesFrame, aes(x=year, y=budBurst, group=species, color=species)) +
  geom_point(data=speciesFrame, aes(x=year, y=budBurst, color=species), size=2) +
  geom_line(data=speciesFrame, aes(x=year, y=leafFall, group=species, color=species)) +
  geom_point(data=speciesFrame, aes(x=year, y=leafFall, color=species), size=2) +  
  labs(x="Year", y="Julian Day", title="Yearly Bud Burst and Leaf Fall by Species") +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL)
graph1


graph2=ggplot() + 
  geom_line(data=speciesFrame, aes(x=year, y=leavesOnDays, group=species, color=species))+
  geom_point(data=speciesFrame, aes(x=year, y=leavesOnDays, color=species))
graph2

redMaple=subset(speciesFrame, species=="RM")
graph1=ggplot()+
  geom_line(data=redMaple, aes(x=year, y=budBurst, group=species)) +
  geom_point(data=redMaple, aes(x=year, y=budBurst), size=2) +
  geom_line(data=redMaple, aes(x=year, y=leafFall, group=species)) +
  geom_point(data=redMaple, aes(x=year, y=leafFall), size=2) +  
  labs(x="Year", y="Julian Day", title="Yearly Bud Burst and Leaf Fall, Red Maple") +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL)
graph1

model <- lm(formula = leavesOnDays ~ year, data=redMaple, na.action=na.omit)
coefRM <- coef(model)

graph2=ggplot() + 
  geom_line(data=redMaple, aes(x=year, y=leavesOnDays, group=species, color=species))+
  geom_point(data=redMaple, aes(x=year, y=leavesOnDays, color=species)) +
  geom_abline(slope=coefRM[2], intercept=coefRM[1])
graph2

####Best Species####
allSpecs=as.character(unique(summaryData$speciesCode))
numTrees=data.frame(tree=allSpecs, numInds=rep(0, length(allSpecs)))
vec=c()
for (spec in allSpecs){
  thisSpec=subset(summaryData, summaryData$speciesCode==spec)
  trees=unique(thisSpec$uniqTreeID)
  vec=c(vec,length(unique(trees)))}
numTrees$numInds=vec
numTrees=numTrees[order(-numTrees$numInds),]
numTrees$tree=as.character(numTrees$tree)

bestTrees=subset(numTrees, numInds>=90)$tree

####Best Towns####
allTowns=as.character(unique(summaryData$town))
numTowns=data.frame(town=allTowns, numInds=rep(0, length(allTowns)))
vec=c()
for (aTown in allTowns){
  thisTown=subset(summaryData, town==aTown)
  towns=unique(thisTown$uniqTreeID)
  vec=c(vec,length(unique(towns)))}
numTowns$numInds=vec
numTowns=numTowns[order(-numTowns$numInds),]
numTowns$town=as.character(numTowns$town)

bestTowns=subset(numTowns, numInds>=100)$town


################### USER-DEFINED GRAPHING ##################


#SELECT SITES OF INTEREST#
#pick up to two area identifiers. If left blank, all will be included
town <- c(NA)
school <- c(NA)

##Pick one type of species identifier, and list as many as you want. If left blank, all will be included
genus <- c("Quercus")
species <- c(NA)
speciesCode <- c("RO")

#SELECT GRAPH TYPE#
#The x-axis will be years. Pick a category from above for the y axis
yAxis="town"

########                          ########
### Run everything below to see graphs ###
########                          ########

columns=list(town,school,genus,species,speciesCode)
key=c("town","school","genus","species","speciesCode")

graphData=summaryData
dataInfo="DATA  "
for (i in seq(1,5)){
  if (is.na(columns[i]) == FALSE){
    graphData=(subset(graphData, graphData[key[i]][[1]] %in% columns[i][[1]]))
    dataInfo=paste(dataInfo, paste0(key[i], ": ", columns[i]))
  }
}

budsOnly=subset(graphData, variable=="bud")
leavesOnly=subset(graphData, variable=="leaf")

allVars=unique(graphData[yAxis][[1]])
rowNumber=length(allYears)*length(allVars)
placeHolder=rep('NA', rowNumber)
siteFrame=data.frame(year=rep(allYears, length(allVars)), y=placeHolder, budBurst=placeHolder, leafFall=placeHolder)

yAxisVec=c()
for (i in allVars){
  yAxisVec=c(yAxisVec, rep(i, length(allYears)))
}
siteFrame$y=yAxisVec

budBurstVec=c()
for (i in allVars){
  thisVar=subset(budsOnly, budsOnly[yAxis][[1]]==i)
  budBurstVec=c(budBurstVec,overviewInfo(thisVar, "year", "value", allYears))
}
siteFrame$budBurst=budBurstVec

leafFallVec=c()
for (i in allVars){
  thisVar=subset(leavesOnly, leavesOnly[yAxis][[1]]==i)
  leafFallVec=c(leafFallVec,overviewInfo(thisVar, "year", "value", allYears))
}
siteFrame$leafFall=leafFallVec
siteFrame$leavesOnDays=siteFrame$leafFall-siteFrame$budBurst
siteFrame$year=as.numeric(as.character(siteFrame$year))

means <- c()
nNums <- c()
for (i in unique(siteFrame$y)){
  thisSite <- subset(siteFrame, y==i)
  avg <- mean(thisSite$leavesOnDays, na.rm=TRUE)
  nNum <- length(which(is.na(thisSite$leavesOnDays)==FALSE))
  means <- c(means,avg)
  nNums <- c(nNums,nNum)
}
avgLeavesOnDays <- data.frame(site = unique(siteFrame$y), mean = means, nNum=nNums)
schoolInfo$town <- as.character(schoolInfo$town)

lats <- c()
lons <- c()
for(site in avgLeavesOnDays$site){
  firstHit <- which(schoolInfo$town == site)[1]
  lat <- as.numeric(as.character(schoolInfo[firstHit,]$lat))
  lon <- as.numeric(as.character(schoolInfo[firstHit,]$lon))
  lats <- c(lats,lat)
  lons <- c(lons,lon)
}
avgLeavesOnDays$lat <- lats
avgLeavesOnDays$lon <- lons

varVec=c()
interceptVec=c()
slopeVec=c()
r2Vec=c()
nNumVec=c()
for (i in allVars){
  thisFrame=subset(siteFrame, y==i) 
  if(length(subset(thisFrame, is.na(leavesOnDays))$leavesOnDays) != length(thisFrame$leavesOnDays)){
    varVec <- c(varVec, i)
    thisModel <- lm(thisFrame$leavesOnDays~as.numeric(thisFrame$year), na.action=na.omit)
    thisCoef <- coef(thisModel)
    thisR2 <- round(summary(thisModel)$r.squared, digits=4)
    thisNNum <-  length(which(thisFrame$leavesOnDays != "NaN"))
    interceptVec <- c(interceptVec, as.numeric(thisCoef[1]))
    slopeVec <- c(slopeVec, as.numeric(thisCoef[2]))
    r2Vec <- c(r2Vec, thisR2)
    nNumVec <- c(nNumVec,thisNNum)
  }
}
modelFrame=data.frame(var=varVec, intercept=interceptVec, slope=slopeVec,r2=r2Vec, nNum=nNumVec)
lats <- c()
lons <- c()
for(var in modelFrame$var){
  firstHit <- which(schoolInfo$town == var)[1]
  lat <- as.numeric(as.character(schoolInfo[firstHit,]$lat))
  lon <- as.numeric(as.character(schoolInfo[firstHit,]$lon))
  lats <- c(lats,lat)
  lons <- c(lons,lon)
}
modelFrame$lat <- lats
modelFrame$lon <- lons


avgBudBurst=overviewInfo(budsOnly, "year", "value", allYears)
avgLeafFall=overviewInfo(leavesOnly, "year", "value", allYears)
leavesOnDays=avgLeafFall-avgBudBurst

graphingFrame=data.frame(year=rep(as.character(allYears),3), value=c(avgBudBurst,avgLeafFall, leavesOnDays), 
                         group=c(rep("Bud Burst", length(allYears)), rep("Leaf Fall", length(allYears)), rep("Leaves On Days", length(allYears))))

graphingFrame$year=as.numeric(as.character(graphingFrame$year))

overallModel <- lm(formula = leavesOnDays ~ as.numeric(allYears), na.action=na.omit)
coefOverall <- coef(overallModel)
rsquareOverall=round(summary(overallModel)$r.squared, digits=4)


title1="Yearly Bud Burst and Leaf Fall"
graph1=ggplot()+
  geom_line(data=subset(graphingFrame, group %in% c("Bud Burst", "Leaf Fall")), aes(x=year, y=value, group=group)) +
  geom_point(data=subset(graphingFrame, group %in% c("Bud Burst", "Leaf Fall")), aes(x=year, y=value, pch=group), size=2) +
  labs(x="Year", y="Julian Day", pch="Data Type") +
  ggtitle(bquote(atop(.(title1), atop(italic(.(dataInfo)), ""))))   +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL)+
  theme(text=element_text(size=12, family="Arial"))
ggsave(filename = paste0(title1, dataInfo,".png"))
graph1

title2=paste("Yearly Bud Burst and Leaf Fall by", yAxis)
graph2=ggplot()+
  geom_line(data=siteFrame, aes(x=year, y=budBurst, group=y, color=y)) +
  geom_point(data=siteFrame, aes(x=year, y=budBurst, color=y, pch="Bud Burst"), size=2) +
  geom_line(data=siteFrame, aes(x=year, y=leafFall, group=y, color=y)) +
  geom_point(data=siteFrame, aes(x=year, y=leafFall, color=y, pch="Leaf Fall"), size=2) +  
  labs(x="Year", y="Julian Day", color=yAxis, pch="Type") +
  ggtitle(bquote(atop(.(title2), atop(italic(.(dataInfo)), ""))))   +
  scale_y_continuous(breaks=seq(100,320,10), minor_breaks=NULL) +
  theme(text=element_text(size=12, family="Arial"))
ggsave(filename = paste0(title2, dataInfo,".png"))
graph2

title3=paste("Leaves On Days by", yAxis)
graph3=ggplot() + 
  geom_line(data=siteFrame, aes(x=year, y=leavesOnDays, group=y, color=y))+
  geom_point(data=siteFrame, aes(x=year, y=leavesOnDays, color=y)) +
  #geom_abline(aes(slope=slope, intercept=intercept, linetype=var), data=modelFrame) + 
  #annotate("text", x=2015, y=155, label= paste("R^2: ", rsquareOverall)) +
  labs(x="Year", y="# Of Days", color=yAxis, linetype="Linear") +
  ggtitle(bquote(atop(.(title3), atop(italic(.(dataInfo)), ""))))   +
  scale_y_continuous(breaks=seq(150,180,5), minor_breaks=NULL) +
  theme(text=element_text(size=12, family="Arial"))
ggsave(filename = paste0(title3, dataInfo,".png"))
graph3

title4="Leaves On Days Overall"
graph4=ggplot() + 
  geom_line(data=subset(graphingFrame, group == "Leaves On Days"), aes(x=year, y=value, group=group, color=group))+
  geom_point(data=subset(graphingFrame, group == "Leaves On Days"), aes(x=year, y=value, color=group)) +
  geom_abline(aes(slope=coefOverall[2], intercept=coefOverall[1], linetype="linear Leaves On Days"))+
  annotate("text", x=2015, y=153, label=paste("R^2:", rsquareOverall)) +
  labs(x="Year", y="# Of Days", color="", linetype="") +
  ggtitle(bquote(atop(.(title4), atop(italic(.(dataInfo)), ""))))   +
  scale_y_continuous(breaks=seq(150,180,5), minor_breaks=NULL) +
  theme(text=element_text(size=12, family="Arial"))
ggsave(filename = paste0(title4, dataInfo,".png"))
graph4
  
title5="Avg Leaves on Days Map"
mapString=ggmap(get_map(location="Massachusetts", zoom=8, source="stamen", maptype="terrain-background", color="bw"), extent = "device")
map1=mapString+
  geom_point(data=avgLeavesOnDays, aes(x=lon, y=lat,fill=mean, size=nNum), pch=21) + #, alpha=.01,na.rm=TRUE)+
  scale_fill_gradient(low="yellow", high="red") +
  labs(title="Average Leaves on Days", size="Number of Observations (years)", color="Leaves On Days")
ggsave(filename = paste0(title5, dataInfo,".png"))
map1

title6="Change in Leaves on Days Map"
mapString=ggmap(get_map(location="Massachusetts", zoom=8, source="stamen", maptype="terrain-background", color="bw"), extent = "device")
map1=mapString+
  geom_point(data=subset(modelFrame, nNum>2), aes(x=lon, y=lat,fill=slope, size=nNum),pch=21) + #, alpha=.01,na.rm=TRUE)+
  scale_fill_gradient2(low="blue", mid="white",high="red", midpoint=0) +
  scale_size(range=c(3,8))+
  labs(title="Change in Leaves on Days", size="Number of Observations (years)", color="Degree of Change")
ggsave(filename = paste0(title6, dataInfo,".png"))
map1
