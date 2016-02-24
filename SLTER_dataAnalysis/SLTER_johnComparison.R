##Script for analyses done for Pam Snow following meeting on 1/21/16 ##
#Objective 1: compare schoolyard data to John O'Keefe data


library(ggplot2)
#install.packages("extrafont")
library(extrafont)
font_import(pattern="[A/a]rial")

#############FORMAT SCHOOLYARD DATA########################
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

#############FORMAT John O'Keefe Data##################
johnTreeInfo=read.csv("data/hf003-01-plant.csv")
johnTreeInfo$speciesCode=strsplit(as.character(johnTreeInfo$tree.id), split="-")[1]

johnSpring=read.csv("data/hf003-03-spring.csv")
johnSpring$variable <- rep("bud", nrow(johnSpring))
johnFall=read.csv("data/hf003-04-fall.csv")
johnFall$variable <- rep("leaf", nrow(johnFall))


johnSpring$date=as.Date(johnSpring$date)
johnSpring$Year=format(johnSpring$date, "%Y")
johnFall$date=as.Date(johnFall$date)
johnFall$Year=format(johnFall$date, "%Y")


#associate the trees with species
#first, convert the specific tree names to tree types in the johnSpring and johnFall data. 
namevec=c()
for (i in johnSpring$tree.id){
  name <- strsplit(as.character(i), split="-")[[1]][1]
  namevec <- c(namevec, name)
}
johnSpring$treeName <- namevec

fallvec=c()
for (i in johnFall$tree.id){
  name <- strsplit(as.character(i), split="-")[[1]][1]
  fallvec <- c(fallvec, name)
}
johnFall$treeName <- fallvec

#Do the same thing for johnTreeInfo. This is important because not all the specific tree ID are actually present in the TreeInfo data set.
treevec=c()
for (i in johnTreeInfo$tree.id){
  name <- strsplit(as.character(i), split="-")[[1]][1]
  treevec <- c(treevec, name)
}
johnTreeInfo$treeName <- treevec

Svec <- c()
for (i in seq(1,length(johnSpring$treeName))){
  tryCatch(Svec[i] <- (which(johnTreeInfo$treeName==as.character(johnSpring$treeName[i]))), 
           error=function(e){Svec[i] <- 116})
}

Fvec <- c()
for (i in seq(1,length(johnFall$treeName))){
  tryCatch(Fvec[i] <- (which(johnTreeInfo$treeName==as.character(johnFall$treeName[i]))[1]), 
           error=function(e){Fvec[i] <- 116})
}

SciNames = as.character(johnTreeInfo[Svec,5])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
johnSpring$genus = genusVec
johnSpring$species = speciesVec

SciNames = as.character(johnTreeInfo[Fvec,5])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
johnFall$genus = genusVec
johnFall$species = speciesVec

#take only relevant values. spring first
johnSpringSelect=data.frame(date=johnSpring$date, julian=johnSpring$julian, year=johnSpring$Year, uniqTreeID=(johnSpring$tree.id),
                      genus=tolower(johnSpring$genus), species=tolower(johnSpring$species), treeName=johnSpring$treeName,
                        bBurst=johnSpring$bbrk)

#make data frame with the 50p bud burst dates
years <- c()
treeIDs <- c()
bb50s <- c()
for (i in unique(as.character(johnSpringSelect$uniqTreeID))){
  thisTree <- subset(johnSpringSelect, uniqTreeID == i)
  yearList <- unique(as.character(thisTree$year))
  treeID <- rep(i,length(yearList))
  bb50 <- c()
  for (y in yearList){
    bbDate <- NA
    thisYear <- subset(thisTree, year == y)
      if (50 %in% thisYear$bBurst){
        bbDate <- thisYear[which(thisYear$bBurst==50)[1],2]
        }
      else {
        over50 <- which(thisYear$bBurst > 50)[1]
        overDate <- thisYear[over50,2]
          if (is.na(over50)){
            bbDate=NA
          }
          else if (over50 == 1){
            bbDate=overDate
            }
          else{
            under50 <- over50-1
            underDate <- thisYear[under50,2]
            bbDate <- mean(overDate,underDate)
            }
        }
      bb50 <- c(bb50,bbDate)
      }
  if(length(bb50) ==length(yearList)){
    years <- c(years, yearList)
    treeIDs <- c(treeIDs, treeID)
    bb50s <- c(bb50s, bb50)
    }
  else{print(treeID[1])}
}

johnSpringSummary <- data.frame(year=years, treeID=treeIDs, value=bb50s, stringsAsFactors = FALSE)
johnSpringSummary$variable <- rep("bud", length(bb50s))
for (i in seq(1,length(treeIDs))){
  row=which(johnSpringSelect$uniqTreeID == treeIDs[i])[1]
  johnSpringSummary$code[i] <-  as.character(johnSpringSelect[row,7])
  johnSpringSummary$species[i] <-  as.character(johnSpringSelect[row,6])
  johnSpringSummary$genus[i] <-  as.character(johnSpringSelect[row,5])
}

#take only relevant values. spring first
johnFallSelect=data.frame(date=johnFall$date, julian=johnFall$julian, year=johnFall$Year, uniqTreeID=(johnFall$tree.id),
                            genus=tolower(johnFall$genus), species=tolower(johnFall$species), treeName=johnFall$treeName,
                            lfall=johnFall$lfall)

#make data frame with the 50p bud burst dates
years <- c()
treeIDs <- c()
lf50s <- c()
for (i in unique(as.character(johnFallSelect$uniqTreeID))){
  thisTree <- subset(johnFallSelect, uniqTreeID == i)
  yearList <- unique(as.character(thisTree$year))
  treeID <- rep(i,length(yearList))
  lf50 <- c()
  for (y in yearList){
    bbDate <- NA
    thisYear <- subset(thisTree, year == y)
    if (50 %in% thisYear$lfall){
      lfDate <- thisYear[which(thisYear$lfall==50)[1],2]
    }
    else {
      over50 <- which(thisYear$lfall > 50)[1]
      overDate <- thisYear[over50,2]
      if (is.na(over50)){
        lfDate=NA
      }
      else if (over50 == 1){
        lfDate=overDate
      }
      else{
        under50 <- over50-1
        underDate <- thisYear[under50,2]
        lfDate <- mean(overDate,underDate)
      }
    }
    lf50 <- c(lf50,lfDate)
  }
  if(length(lf50) ==length(yearList)){
    years <- c(years, yearList)
    treeIDs <- c(treeIDs, treeID)
    lf50s <- c(lf50s, lf50)
  }
  else{print(treeID[1])}
}

johnFallSummary <- data.frame(year=years, treeID=treeIDs, value=lf50s, stringsAsFactors = FALSE)
johnFallSummary$variable <- rep("leaf", length(lf50s))
for (i in seq(1,length(treeIDs))){
  row=which(johnFallSelect$uniqTreeID == treeIDs[i])[1]
  johnFallSummary$code[i] <-  as.character(johnFallSelect[row,7])
  johnFallSummary$species[i] <-  as.character(johnFallSelect[row,6])
  johnFallSummary$genus[i] <-  as.character(johnFallSelect[row,5])
}

JohnSummary <- data.frame()