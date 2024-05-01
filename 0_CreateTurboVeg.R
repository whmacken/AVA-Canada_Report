################Convert data to TurboVeg format############


.libPaths("E:/R packages351")
#install.packages("Hmisc")
devtools::install_github("miquelcaceres/VegX", build_vignettes=TRUE)
require(vegdata)
require(VegX)

require(reshape)
require(reshape2)
require(vegan)
require(caret)
require(tcltk)
require(randomForest)
require(Matrix)
require(labdsv)
require(gdata)
require(MASS)
require(openxlsx)
require (C50)
require(tidyr)
require(stringr)
require(rpart)
require(tree)
require(rattle)
require(rpart.plot)
require(partykit)
require(vegclust)
require(standardize)
require(dplyr)
require(tictoc)
require(plyr)
require(Hmisc)
require(foreign)

rm(list=ls())
wd=tk_choose.dir(); setwd(wd)

#####IMPORT AND CLEAN DATA##############################
#################################################

#####################importing veg data########################################
############### Uses 4 column list form R export FORMAT FROM Vpro with Lifeform option selected (Plot - Species - Cover - Lifeform)
#vegData <- read.table("BECMasterVeg_Feb7_2019.txt", header = TRUE) 
vegData <- read.table("Chars2014.txt", header = TRUE) 
vegData <- separate(vegData, Species, c("Species","Type"), "-", remove = TRUE)
vegData <- mutate_all(vegData, funs(toupper)) ### converts lower case characters to upper
vegData$Cover <- as.numeric(vegData$Cover)
###return on significant decimal places
vegData$Cover <- round(vegData$Cover, digits = 3)
## remove species with zero cover
vegData <- vegData[vegData$Cover > 0,]
###add space back in to Poa
vegData$Species <- str_replace_all(vegData$Species, "^POA", "POA ")
save(vegData, file = "CHARS2014_VegDat_Raw.RData")##includes type field for lifeform
lifeform <- unique(vegData[,c(2:3)])
save(lifeform, file= "SppLifeForm.RData")###library list of species lifeform codes
load("VegDat_Raw.RData")


##########update old codes###################
masterList <- read.csv("USysAllSpecs.csv", stringsAsFactors = FALSE)
sppList <- masterList[masterList$Codetype == "U",]
noMatch <- masterList[masterList$OldCode != masterList$Code,3:4]
temp <- merge(vegData,noMatch,by.x = "Species", by.y = "OldCode")
temp$Species <- temp$Code
temp <- temp[,-5]
vegData <- rbind(vegData,temp) ###Add section with new names
vegData <- vegData[!vegData$Species %in% noMatch$OldCode,] ##remove old codes

###remove codes not in master list
notIn <- vegData[!vegData$Species %in% masterList$Code,]
vegData <- vegData[vegData$Species %in% masterList$Code,]

if(length(notIn$Species) > 0){
  notIn <- dcast(notIn, PlotNumber ~ Species, value.var = "Species", fun.aggregate = length)
  write.csv(notIn, file = "CodesNotInMasterList.csv")
}
vegRemove <- vegData[vegData$Cover <= 0,] 
write.csv(vegRemove, file = "Plots_0Cover.csv") ## output of species with zero cover for review
##############Some stats on imported data
####Count number of columns (species) required
NTaxa = as.data.frame (length(unique(vegData$Species)))
NSpp <- as.data.frame (length(str_sub(vegData$Species,1,7)))
Taxa <- as.data.frame (unique(vegData$Species))
Spp <- as.data.frame (unique(str_sub(vegData$Species,1,7)))
Plots <- as.data.frame (unique(vegData$PlotNumber))
####Counts the number of instances of each unique species
CountsTaxa <- ddply(vegData,~Species,summarise,sppcount=length(Plots))
CountsSpp <- ddply(vegData,~str_sub(vegData$Species,1,7),summarise,sppcount=length(unique(PlotNumber)))
CountsSpp <- arrange(CountsSpp, -sppcount)
colnames (CountsSpp) [1] <- "Species"
rareSpp <- as.data.frame (CountsSpp[CountsSpp$sppcount <= 3,]) # set level for rare species removal
CountsSppReduced <- CountsSpp[!CountsSpp$Species %in% rareSpp$Species,]

#############Optional -- Remove subtaxa coding
#vegData$Species <-str_sub(vegData$Species,1,7) ### adds field with only species codes (no spp or varieties)
#vegData$Species <- as.factor(vegData$Species)

###Plot number of species in density graph
ggplot(data=CountsSpp,aes(x=reorder(Species, -sppcount), y=sppcount))+
  geom_point()
vegDatareduced <- vegData[!vegData$Species %in% rareSpp$Species,]
ggplot(vegDatareduced,aes(Species))+
  geom_density(adjust = 5)+
  theme(axis.text.x=element_text(angle = 90, hjust=1))
#####reduce vegData by eliminating rareSpp
vegData <- vegData[!vegData$Species %in% rareSpp$SppOnly,]

#####reduce vegData to only lifeforms listede (lifeform 1 and 2 = trees; 6 = grasses) only
#vegData <- vegData[vegData$Type %in% c(3,4,6),]
#vegData <- vegData[!is.na(vegData$Species),]
#treeSpp <- as.character(unique(vegData$Species))



#####################################################

#vegData$Species <- unlist(lapply(vegData$Species, toupper))
vegData3c <- vegData[,-3]##removes type field and setsback to 3-column format
save(vegData3c, file = "VegDat_Raw_3column.RData")
load("VegDat_Raw3column.RData")


vegData <- vegData[vegData$Cover > 0,]## remove records with zero cover
save(vegData, file = "VegDat_Clean.RData")
load("VegDat_Clean.RData")

###Optional application of lump species
lump <- read.csv("NewSppLump27Oct2018_Lump.csv", stringsAsFactors = FALSE)
lump$Lump <- unlist(lapply(lump$Lump, tolower))
lump <- lump[,1:2]
colnames(lump)[1:2] <- c("Lump","Species")
vegData <- merge(vegData, lump, by.x = "Species", all.x = TRUE) ##lump data
vegData$Species <- as.character(vegData$Species)
vegData$Species <- ifelse(!is.na(vegData$Lump), vegData$Lump, vegData$Species)
vegData <- vegData[-5]
save(vegData, file = "VegDat_Lumped.RData")
load("VegDat_Lumped.RData")


######Export Veg data to TurboVeg
##4 column data with layer is equivalent format to the DBF format of Turbo Veg - need to
####Translate species codes to numbers
######Translate releves to Numbers
vegData <- vegData[,c(1,2,4,3)]
colnames (vegData) <- c("RELEVE_NR", "Spp_Code", "COVER_CODE", "LAYER")
vegData$RELEVE_NR <- gsub("[-]","",vegData$RELEVE_NR)
Spp_include <- sppList [,c("Code", "ScientificName")]
colnames (Spp_include) [1:2] <- c("Spp_Code", "Scientific")
vegData2 <- merge (vegData, Spp_include, by.x = "Spp_Code")
vegData <- vegData2[,c(2,5,3,4)]
vegData$RELEVE_NR <- as.integer (vegData$RELEVE_NR)
vegData$SPECIES_NR <- as.character (vegData$SPECIES_NR)
vegData$COVER_CODE <- as.character (vegData$COVER_CODE)
vegData$LAYER <- as.character (vegData$LAYER)

write.csv (vegData2, "Chars2014_veg.csv", row.names = FALSE)
vegData <- read.csv ("Chars2014_veg.csv", stringsAsFactors = FALSE)
vegData3 <- vegData[vegData$RELEVE_NR == 20802,]
write.dbf (vegData3, "TVABUND.dbf", factor2char = TRUE, max_nchar = 254)
xx <- read.dbf ("TVABUND.dbf")
xx <- read.csv ("TVABUND.csv")
xx$RELEVE_NR <- as.integer (xx$RELEVE_NR)
xx$SPECIES_NR <- as.character (xx$SPECIES_NR)
xx$COVER_CODE <- as.character (xx$COVER_CODE)
xx$LAYER <- as.character (xx$LAYER)
write.dbf (xx, "TVABUND.dbf", factor2char = TRUE, max_nchar = 254)
 ############Import Header Data
#fplot=(file.choose()) 
#fplot = "CHARS2014_ENV.csv"
#Vars = c("PlotNumber", "ProjectID" , "PlotSize", "ProvinceStateTerritory", "Elevation"  ,"Aspect", "SlopeGradient",Longitude,Latitude, MoistureRegime, NutrientRegime  )           
#envData <- fread(fplot, select = Vars, stringsAsFactors = FALSE, data.table = FALSE)#

envData <- read.csv("CHARS2014_ENV.csv", header = TRUE) 
envData <- read.csv("CHARStest_ENV.csv", header = TRUE) 
envData2 <- as.data.frame (envData[1])
colnames (envData2) <- c("RELEVE_NR")
envData2$RELEVE_NR <- gsub("[-]","",envData2$RELEVE_NR)
envData2$RELEVE_NR <- as.integer (envData2$RELEVE_NR)
envData2 <- as.data.frame(envData2[envData2$RELEVE_NR == 20802,])
colnames (envData2) <- c("RELEVE_NR")
write.dbf (envData2, "TVHABITA.dbf", factor2char = TRUE, max_nchar = 254)
yy <- read.csv ("TVHABITA.csv", stringsAsFactors = FALSE)
yy$RELEVE_NR <- as.integer (yy$RELEVE_NR)
yy <- yy %>%
  mutate_all(funs(ifelse(is.na(.), "x", .))) 
write.dbf (yy, "TVHABITA.dbf", factor2char = TRUE, max_nchar = 254)
yy
yy2 <-read.dbf ("TVHABITA.dbf")
yy2

zz <- read.csv ("TvAdmin.csv", stringsAsFactors = FALSE)
zz <-read.dbf ("TvAdmin.dbf")
write.dbf (zz, "TvAdmin.dbf", factor2char = TRUE, max_nchar = 254)

ww <- read.csv ("TVwin.csv", stringsAsFactors = FALSE )
ww
yy$RELEVE_NR <- as.integer (yy$RELEVE_NR)
yy <- yy %>%
  mutate_all(funs(ifelse(is.na(.), "x", .))) 
write.dbf (ww, "TVwin.dbf", factor2char = TRUE, max_nchar = 254)
yy2 <-read.dbf ("TVHABITA.dbf")
