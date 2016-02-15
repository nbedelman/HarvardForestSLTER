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

johnSpring$date=as.Date(johnSpring$date)
johnSpring$Year=format(johnSpring$date, "%Y")


#associate the trees with species
#first, convert the specific tree names to tree types in the johnSpring data. 
namevec=c()
for (i in johnSpring$tree.id){
  name <- strsplit(as.character(i), split="-")[[1]][1]
  namevec <- c(namevec, name)
}
johnSpring$treeName <- namevec

#Do the same thing for johnTreeInfo. This is important because not all the specific tree ID are actually present in the TreeInfo data set.
treevec=c()
for (i in johnTreeInfo$tree.id){
  name <- strsplit(as.character(i), split="-")[[1]][1]
  treevec <- c(treevec, name)
}
johnTreeInfo$treeName <- treevec

vec <- c()
for (i in seq(1,length(johnSpring$treeName))){
  tryCatch(vec[i] <- (which(johnTreeInfo$treeName==as.character(johnSpring$treeName[i]))), 
           error=function(e){vec[i] <- 116})
}

SciNames = as.character(johnTreeInfo[vec,5])
splitNames=strsplit(SciNames, " ")
genusVec = c()
speciesVec=c()
for(i in seq(1,length(splitNames))){
  genusVec[i]=splitNames[i][[1]][1]
  speciesVec[i]=splitNames[i][[1]][2]
}
johnSpring$genus = genusVec
johnSpring$species = speciesVec



#take only relevant values
johnSelect=data.frame(date=johnSpring$date, julian=johnSpring$julian, year=johnSpring$Year, uniqTreeID=as.numeric(johnSpring$tree.id),
                      genus=tolower(johnSpring$genus), species=tolower(johnSpring$species), bBurst=johnSpring$bbrk)


