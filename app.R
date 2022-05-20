# Packages-----
library(DT)
library(shiny)
library(shapefiles) # read dbf, shx, shp
library(dplyr)
library(shinyjs)
library(sf)
library(tmap)
library(rgdal)
library(leaflet)
library(shinythemes)
library(ggplot2)
library(shinydashboard)
library(lubridate)
library(zoo)
library(gridExtra)
library(tidyr)
library(forcats)
library(hrbrthemes)
library(viridis)
library(Rmisc)
library(circlize)
library(png)
library(data.table)
library(raster)
library(stringr)

# Files ----- 
files <- list.files("Files/")
new_file_occ <- files[18]
new_file_names <- "Occurrence"
files <- files[-18]
file_names <- lapply(files,function(x){strsplit(x,"_")[[1]][2]})
file_names <- unlist(file_names)
file_names <- lapply(file_names,function(x){strsplit(x,"\\.")[[1]][1]})
file_names <- unlist(file_names)
file_names <- c(file_names,"Occurrence_v1")
files <- c(files,new_file_occ)

#eval(parse(file_names[1])) <- read.csv("Files/ILORA_Anthrome.csv",stringsAsFactors = F)

cols <- numeric(length(file_names))
rows <- numeric(length(file_names))

# Assign file name as a data frame containing the data from that file
for(i in 1:length(file_names)){
tempdf <- read.csv(paste0("Files/",files[i]),stringsAsFactors = F)
#tempdf <- enc2utf8(tempdf)
rows[i] <- dim(tempdf)[1]
cols[i] <- dim(tempdf)[2]
assign(file_names[i],tempdf)
}
rows

# Change this to replace occ. v1 file
# oldoccv1 <- read.csv("archives/ILORA_v1.1_Occurrence_19may22.csv",stringsAsFactors = F)
# Occurrence_v1 <- Occurrence_v1[names(oldoccv1)]
# write.csv(Occurrence_v1,"Files/ILORA_v1.1_Occurrence.csv")

# Fix column names of Eco Uses
names(EconomicUses)[-c(1,53)] <- lapply(names(EconomicUses)[-c(1,53)],function(x){strsplit(x,"_")[[1]][2]})
names(EconomicUses)[c(4,10,16,17,19,22,26,30:32,34:35,38:39,42:43,45:46,48:52)] <- c("Spices;Herbs", "Essential Oils", "Carved Material","Bark Products","Pesticide Fertilizer","Latex Rubber","Wrapping Paper","Social Use","Vert Posion","Invert Poision","Traditional Medicine","Agro forestry", "Soil Improver","Erosion Control","Landscape Improver","Green Manure","Land Reclamation","Soil Conservation","Lawn;Turf","Pollution Control","Ornamental Use","Gene Source","Research Model"    )

# Add invasion status names to Summary file
Summary$Invasion.Status <- SpCategorization$Invasion.Status
unique(Summary$Invasion.Status)
Summary$Invasion.Status.Full <- Summary$Invasion.Status
Summary$Invasion.Status.Full[which(Summary$Invasion.Status.Full == "N")] <- "Native"
Summary$Invasion.Status.Full[which(Summary$Invasion.Status.Full == "CA")] <- "Casual"
Summary$Invasion.Status.Full[which(Summary$Invasion.Status.Full == "In")] <- "Invasive"
Summary$Invasion.Status.Full[which(Summary$Invasion.Status.Full == "Nt")] <- "Naturalized"
Summary$Invasion.Status.Full[which(Summary$Invasion.Status.Full == "CG")] <- "Cryptogenic"
  
# Master file (currently not used anywhere)
combine_names <- file_names[which(rows == 1747)]

col_names <- vector(mode = "list",length = length(combine_names))
for(i in 1:length(combine_names)){
  col_names[[i]] <- names(get(combine_names[i]))
}
# sapply(col_names, "[[", 1)
# get(combine_names[4])[,1] == get(combine_names[5])[,1]

master <- get(combine_names[1])
for(i in 2:length(combine_names)){
master <- cbind(master,get(combine_names[i])[,-1])  
}

# all small
all_temp <- SpCategorization[,c(1:4,8)] # spcat
names(all_temp)[1] <- "Accepted Species Name"
all_temp$`Taxinomic information (Class, Order, Family)` <- all_temp$Class
for(i in 1:nrow(all_temp)){
  all_temp$`Taxinomic Information (Class, Order, Family)`[i] <- paste0(all_temp$Class[i],", ",all_temp$Order[i],", ",all_temp$Family[i])
}
all_temp <- all_temp[,c(1,6,5)]
names(all_temp)[3] <- "Invasion status"
all_temp$`Invasion status`[which(all_temp$`Invasion status` == "N")] <- "Native"
all_temp$`Invasion status`[which(all_temp$`Invasion status` == "CA")] <- "Casual"
all_temp$`Invasion status`[which(all_temp$`Invasion status` == "In")] <- "Invasive"
all_temp$`Invasion status`[which(all_temp$`Invasion status` == "Nt")] <- "Naturalized"
all_temp$`Invasion status`[which(all_temp$`Invasion status` == "CG")] <- "Cryptogenic"

all_temp <- cbind(all_temp,GeneralInformation[,c(2:6)]) # geninf
all_temp$`General information (Common name, Vernacular name, Growth habit, Duration, Group)` <- all_temp$Group
for(i in 1:nrow(all_temp)){
  if(all_temp$Common.Name[i] == ""){all_temp$Common.Name[i] <- "NA"}
  if(all_temp$Vernacular.Name[i] == ""){all_temp$Vernacular.Name[i] <- "NA"}
  if(all_temp$Growth.Habit[i] == ""){all_temp$Growth.Habit[i] <- "NA"}
  if(all_temp$Duration[i] == ""){all_temp$Duration[i] <- "NA"}
  if(all_temp$Group[i] == ""){all_temp$Group[i] <- "NA"}
  all_temp$`General information (Common name, Vernacular name, Growth habit, Duration, Group)`[i] <- paste0(all_temp$Common.Name[i],", ",all_temp$Vernacular.Name[i],", ",all_temp$Growth.Habit[i],", ",all_temp$Duration[i],", ",all_temp$Group[i])
}
all_temp <- all_temp[,-c(4:8)]

natrang <- data.table(NativeRange[,c(1,3)])
natrang <- natrang[,toString(TDWG_Level2_Names),by = Acc_Species_Name]
natrang <- natrang[-which(!(natrang$Acc_Species_Name %in% all_temp$`Accepted Species Name`)),]
natrang_temp <- data.frame("Acc_Species_Name" = all_temp$`Accepted Species Name`[which(!(all_temp$`Accepted Species Name` %in% unique(natrang$Acc_Species_Name)))],"V1" = "")
natrang <- rbind(natrang,natrang_temp)
natrang <- natrang[order(natrang$Acc_Species_Name),]
names(natrang)[2] <- names(NativeRange)[3]
sum(natrang$Acc_Species_Name == all_temp$`Accepted Species Name`)
for(i in 1:nrow(natrang)){
  if(natrang$TDWG_Level2_Names[i] == ""){natrang$TDWG_Level2_Names[i] <- "No Data Available"}
}
all_temp <- cbind(all_temp,natrang[,2]) # nat range
names(all_temp)[5] <- "Native range (TDWG Level 2 Names)"

names(Introduction);nrow(Introduction)
all_temp <- cbind(all_temp,Introduction[,c(4,6)]) #introduction
for(i in 1:nrow(all_temp)){
  if(all_temp$Introduction_pathways_subcategory[i] == ""){all_temp$Introduction_pathways_subcategory[i] <- "No Data Available"}
}
names(all_temp)[6:7] <- c("Introduction pathway(s)","First record date")

names(EconomicUses);nrow(EconomicUses)
econus <- EconomicUses[,c(1,2)]
for(i in 1:nrow(econus)){
  econus$Food[i] <- toString(names(EconomicUses[,-c(1,ncol(EconomicUses))])[which(!is.na(EconomicUses[i,-c(1,ncol(EconomicUses))]))])#rowSums(EconomicUses[,-c(1,ncol(EconomicUses))],na.rm = T)  
  if(econus$Food[i] == ""){econus$Food[i] <- "No Data Available"}
}

names(econus)[2] <- "Economic uses"
all_temp <- cbind(all_temp,econus[,2]) #eco uses
names(all_temp)[8] <- names(econus)[2]

names(MarketDynamics);nrow(MarketDynamics)
mdtemp <- MarketDynamics[,c(1,11)]
mdtemp$sbn <- mdtemp$Source
mdtemp$sbn[which(mdtemp$Source == "")] <- "No"
mdtemp$sbn[which(mdtemp$Source != "")] <- "Yes"
all_temp <- cbind(all_temp,mdtemp[,3]) #market dynamics
names(all_temp)[9] <- "Sold by nurseries"

names(Habitat);nrow(Habitat)
habtemp <- Habitat[,c(1,2)]  
for(i in 1:nrow(habtemp)){
  fillhab <- c()
  if(sum(!is.na(Habitat[i,2:9])) > 0){
    fillhab <- c(fillhab,"Terrestrial Managed")
  }
  if(sum(!is.na(Habitat[i,10:20])) > 0){
    fillhab <- c(fillhab,"Terrestrial Natural")
  }
  if(sum(!is.na(Habitat[i,21:26])) > 0){
    fillhab <- c(fillhab,"Litteral")
  }
  if(sum(!is.na(Habitat[i,27:34])) > 0){
    fillhab <- c(fillhab,"Fresh Water")
  }
  if(sum(!is.na(Habitat[i,35:39])) > 0){
    fillhab <- c(fillhab,"Brackish")
  }
  if(is.null(fillhab)){
    habtemp$TerrM_Forest_Plantation[i] <- "No Data Available"
  }else{
    habtemp$TerrM_Forest_Plantation[i] <- toString(fillhab)
  }
}
all_temp <- cbind(all_temp,habtemp$TerrM_Forest_Plantation)
names(all_temp)[10] <- "Habitat"

names(NaturalizedRange);nrow(NaturalizedRange)
naturzdrang <- data.table(NaturalizedRange)
naturzdrang <- naturzdrang[,toString(unique(TDWG_Level2_Names)),by = Acc_Species_Name]
naturzdrang <- naturzdrang[-which(!(naturzdrang$Acc_Species_Name %in% all_temp$`Accepted Species Name`)),]
naturzdrang_temp <- data.frame("Acc_Species_Name" = all_temp$`Accepted Species Name`[which(!(all_temp$`Accepted Species Name` %in% unique(naturzdrang$Acc_Species_Name)))],"V1" = "No Data Available")
naturzdrang <- rbind(naturzdrang,naturzdrang_temp)
naturzdrang <- naturzdrang[order(naturzdrang$Acc_Species_Name),]
all_temp <- cbind(all_temp,naturzdrang$V1)
names(all_temp)[11] <- "Naturalized range (TDWG Level 2 names) [TDWG Level 4 names in ILORA_8_NaturalizedRange.csv]"

all_temp11 <- all_temp

names(Occurrence);nrow(Occurrence)
occrns <- data.table(Occurrence)
occrns$latlong <- paste0(Occurrence$Longitude,Occurrence$Latitude)
occrns <- occrns[,length(unique(paste0(latlong))),by = Acc_Species_Name]
occrns <- occrns[-which(!(occrns$Acc_Species_Name %in% all_temp$`Accepted Species Name`)),]
occrns_temp <- data.frame("Acc_Species_Name" = all_temp$`Accepted Species Name`[which(!(all_temp$`Accepted Species Name` %in% unique(occrns$Acc_Species_Name)))],"V1" = NA)
occrns <- rbind(occrns,occrns_temp)
occrns <- occrns[order(occrns$Acc_Species_Name),]
all_temp <- cbind(all_temp,occrns$V1)
names(all_temp)[12] <- "Number of occurrence records"

names(Occurrence_v1);nrow(Occurrence_v1)
occrns11 <- data.table(Occurrence_v1)
occrns11$latlong <- paste0(Occurrence_v1$decimalLongitude,Occurrence_v1$decimalLatitude)
occrns11 <- occrns11[,length(unique(paste0(latlong))),by = scientificName]
occrns11 <- occrns11[-which(!(occrns11$scientificName %in% all_temp11$`Accepted Species Name`)),]
occrns11_temp <- data.frame("Acc_Species_Name" = all_temp11$`Accepted Species Name`[which(!(all_temp11$`Accepted Species Name` %in% unique(occrns11$scientificName)))],"V1" = NA)
names(occrns11) <- names(occrns_temp)
occrns11 <- rbind(occrns11,occrns11_temp)
occrns11 <- occrns11[order(occrns11$Acc_Species_Name),]
all_temp11 <- cbind(all_temp11,occrns11$V1)
names(all_temp11)[12] <- "Number of Occurrence records"

names(State);nrow(State)
names(State)[-1] <-gsub(".", " ", names(State)[-1], fixed=TRUE)
state <- State[,c(1:2)]
for(i in 1:nrow(state)){
  state[i,2] <- toString(names(State[,-c(1,ncol(State))])[which(State[i,-c(1,ncol(State))]!="")])
  if(state[i,2]==""){state[i,2] <- "No Data Avaiable"}
}
all_temp <- cbind(all_temp,state$Kerala)
names(all_temp)[13] <- "Distribution in states and union territories"
all_temp11 <- cbind(all_temp11,state$Kerala)
names(all_temp11)[13] <- "Distribution in states and union territories"

names(LULC);nrow(LULC)
lulc <- LULC[,c(1,2)]
lulc$Wet.Evergreen.forest <- rowSums(LULC[,2:35],na.rm = T)
all_temp <- cbind(all_temp,lulc$Wet.Evergreen.forest)
names(all_temp)[14] <- "Distributed in LULC classes (count)"
all_temp11 <- cbind(all_temp11,lulc$Wet.Evergreen.forest)
names(all_temp11)[14] <- "Distributed in LULC classes (count)"

names(Anthromes);nrow(Anthromes)
anth <- Anthromes[,c(1,2)]
anth$Urban <- rowSums(Anthromes[,2:19],na.rm = T)
all_temp <- cbind(all_temp,anth$Urban)
names(all_temp)[15] <- "Distributed in Anthromes classes (count)"
all_temp11 <- cbind(all_temp11,anth$Urban)
names(all_temp11)[15] <- "Distributed in Anthromes classes (count)"

names(Ecoregions);nrow(Ecoregions)
ecolreg <- Ecoregions[,c(1,2)]
ecolreg$South.Western.Ghats.Montane.Rain.Forests <- rowSums(Ecoregions[,2:43],na.rm = T)
all_temp <- cbind(all_temp,ecolreg$South.Western.Ghats.Montane.Rain.Forests)
names(all_temp)[16] <- "Distributed in Ecoregions classes (count)"
all_temp11 <- cbind(all_temp11,ecolreg$South.Western.Ghats.Montane.Rain.Forests)
names(all_temp11)[16] <- "Distributed in Ecoregions classes (count)"

names(Climate);nrow(Climate)
clim <- Climate[,c(1:3,6:7)]
clim$clim <- clim$Min.Ppt
for(i in 1:nrow(clim)){
  clim$clim[i] <- toString(clim[i,2:5])  
  if(clim$clim[i] == "NA, NA, NA, NA"){clim$clim[i] <- "NA"}
}
all_temp <- cbind(all_temp,clim$clim)
names(all_temp)[17] <- "Climate [Max. Temp. (degree C), Min. Temp. (degree C), Max. Ppt. (mm), Min. Ppt. (mm)]"
all_temp11 <- cbind(all_temp11,clim$clim)
names(all_temp11)[17] <- "Climate [Max. Temp. (degree C), Min. Temp. (degree C), Max. Ppt. (mm), Min. Ppt. (mm)]"

tat <- data.table(all_temp)
tat <- dcast(melt(tat, id.vars = "Accepted Species Name"), variable ~ `Accepted Species Name`)
names(tat)[1] <- "Variable"
tat <- as.data.frame(tat)

tat11 <- data.table(all_temp11)
tat11 <- dcast(melt(tat11, id.vars = "Accepted Species Name"), variable ~ `Accepted Species Name`)
names(tat11)[1] <- "Variable"
tat11 <- as.data.frame(tat11)

state_dat1 <- Occurrence_v1[,c(3,15)]
sp_srch11 <- Occurrence_v1
names(sp_srch11)
sp_srch1 <- data.frame("Variable" = names(sp_srch11))
for(i in 1:length(unique(sp_srch11$scientificName))){
  sp_srch1[,i+1] <- 0
  names(sp_srch1)[i+1] <- unique(sp_srch11$scientificName)[i]
}
names(sp_srch11) <- gsub('\\.', ' ', names(sp_srch11))
names(sp_srch11)

names(sp_srch11) <- gsub('decimal', '', names(sp_srch11))
names(sp_srch11)

sp_srch1[,1] <- gsub('\\.', ' ', sp_srch1[,1])
sp_srch1[,1]

sp_srch1 <- sp_srch1[c(2,4:7,9:10,12,15:23),]
sp_srch1[,1]

sp_srch1[,1] <- gsub('decimal', '', sp_srch1[,1])
sp_srch1[,1]
col_inds <- as.numeric(rownames(sp_srch1))
rownames(sp_srch1) <- 1:nrow(sp_srch1)

#i <- 365
for(i in 1:length(unique(state_dat1$scientificName))){
tmpspsrch <- sp_srch1[,c(1,i+1)]
tmpmainsrch <- subset(sp_srch11,scientificName == unique(state_dat1$scientificName)[i])
#names(tmpmainsrch)[col_inds]
for(j in col_inds){
  if((j!= 20) & (j!=12)){
    tmpspsrch[which(names(tmpmainsrch)[j] == tmpspsrch[,1]),2] <- paste0(unique(unique(tmpmainsrch[,j])),collapse = "; ")#toString(unique(unique(tmpmainsrch[,j])))
  }else{
    if(j == 20){
    tmpspsrch[which(names(tmpmainsrch)[j] == tmpspsrch[,1]),2] <- mean(tmpmainsrch[,j],na.rm = T)
    }else{
    tmpspsrch[which(names(tmpmainsrch)[j] == tmpspsrch[,1]),2] <- nrow(tmpmainsrch)
    }
  }
}
sp_srch1[,i+1] <- tmpspsrch[,2] 
cnttbl <- table(tmpmainsrch$state)
string1 <- c()
for(k in 1:length(cnttbl)){
  string1[k] <- paste0(names(cnttbl)[k],"(",cnttbl[k],")")
}
sp_srch1[9,i+1] <-  paste0(string1,collapse="; ")#toString(string1)
dtsextr <- strsplit(sp_srch1[6,i+1], ",")[[1]]
yrsextr <- as.numeric(unique(unlist(str_extract_all(dtsextr,"[0-9]{4}+"))))
sp_srch1[6,i+1] <-  paste0(min(yrsextr),"-",max(yrsextr))

}

sp_srch1[8,1] <- "Number of occurrence records"
sp_srch1[14,1] <- "Mean Coordinate Uncertainty In Meters"

names(sp_srch11)[3] <- "Name"
adv_srch1 <- names(sp_srch11)[c(3,2,4:7,10,15:18,23)]

sp_srch1 <- sp_srch1[-c(7,10,11,13:15),]
sp_srch1[c(6,8),1] <- c("eventDate (in years, earliest-latest)",
                        "state (count of occurrences)")

#sp_srch1[,1]

for(i in 2:ncol(sp_srch1)){
sp_srch19itmp <-  paste0(strsplit(sp_srch1[9,i], "; ")[[1]],collapse = ", ")
sp_srch19itmp <-  strsplit(sp_srch19itmp, ", ")[[1]]
sp_srch1[9,i] <- paste0(unique(sp_srch19itmp),collapse = "; ")#toString(sp_srch19itmp)  
}

# Variable file names
req_files <- file_names[-c(3,6,14:15,18)]

# Choices for Table search
TABLE <- file_names[-which(file_names == "CultivatedSpecies"|file_names == "Summary"|file_names == "Metadata")]
table_search <- TABLE[-15]
reorder <- c(12,14,5,13,2,7,4,11,6,3,8,9,1,10)
tbl_df <- data.frame(table_search,reorder)
tbl_df$label <- tbl_df$table_search
tbl_df$label[c(3,4,5,9:11,13)] <- c("Economic Uses", "Ecological Regions","General Information","Market Dynamics", "Native Range","Naturalized Range","Species Categorization")
tbl_search <- tbl_df[order(tbl_df$reorder),3]
tbl_df[15,] <- c("Metadata",15,"Metadata")
tbl_df$reorder <- as.numeric(tbl_df$reorder)
#tbl_df <- rbind(tbl_df,data.frame("table_search" = "master","reorder"=0,"label" = "Species Name"))
str(tbl_df)
tbl_search <- c(tbl_search,"Metadata")
#tbl_search <- c("Species Name",tbl_search)

tbl_df11 <- tbl_df
tbl_df11[12,1] <- "Occurrence_v1"

# Choices for Advanced search
adv_search_spcat <- names(SpCategorization)[-c(2:3,5:7,9)]
adsrch_spcat <- data.frame("name"=adv_search_spcat,"label"=adv_search_spcat)
adsrch_spcat$label[c(1,3)] <- c("Species Name", "Invasion Status")

adv_search_geninf <- names(GeneralInformation)[2:6]
adsrch_geninf <- data.frame("name"=adv_search_geninf,"label"=adv_search_geninf)
adsrch_geninf$label[1:3] <- c("Common Name","Vernacular Name","Growth Habit")
adv_search <- c(adsrch_spcat$label,adsrch_geninf$label)

adv_srch_var <- data.frame("name" = file_names[-18])
#adv_srch_var <- adv_srch_var[-16,]
adv_srch_var$reorder <- c(12,14,16,5,13,2,7,4,11,6,17,3,8,9,1,10,15)
adv_srch_var$label <- adv_srch_var$name
adv_srch_var$label[c(3:6,10,12:13,15)] <- c("Cultivated Species", "Economic Uses", "Ecological Regions","General Information","Market Dynamics", "Native Range", "Naturalized Range", "Species Categorization")
#adv_srch_var <- rbind(adv_srch_var,data.frame("name" = "master","reorder"=0,"label" = "All"))
as_var <- adv_srch_var$label[order(adv_srch_var$reorder)]
#as_var <- c("All",as_var)
#file_names <- c(file_names,"master")

adv_srch_var11 <- adv_srch_var
adv_srch_var11[14,1] <- "Occurrence_v1"

# Call R files containing code for plots
source("plots.R")

# Shape file -----
#shp_new <- readOGR(dsn = "India_map/India_project2.shp",stringsAsFactors = FALSE)
shp_new <- readOGR(dsn = "States/Admin2.shp",stringsAsFactors = FALSE)
shp_new1 <- readOGR(dsn = "States/Admin2.shp",stringsAsFactors = FALSE)
# shp_new@data <- shp_new@data[,c(1,2)]
# shp_new@data$KA.14_C <- 0
# names(shp_new)[2] <- "Temp"
# proj4string(shp_new)
# proj4string(shp_new) <- CRS("+init=epsg:24383")
#shp_new <- spTransform(shp_new,CRS("+init=epsg:24383"))
#writeOGR(shp_new,".",dsn = "States/Admin2.shp",driver = "ESRI Shapefile")

state_dat <- data.table(State,stringsAsFactors = F)
state_dat <- dcast(melt(state_dat, id.vars = "Acc_Species_Name"), variable ~ `Acc_Species_Name`)
state_dat <- state_dat[-35,]
state_dat <- as.data.frame(state_dat,stringsAsFactors = F)
state_dat[,1] <- as.character(state_dat[,1])
#str(state_dat)
names(state_dat)[1]

state_dat <- state_dat[order(state_dat[,1]),]
state_dat[,1] <- toupper(state_dat[,1])
state_dat[33,1] <- "UTTARAKHAND"
state_dat[,1] <- gsub(" AND "," & ",state_dat[,1])
missin <- data.frame("variable" = (shp_new@data$State)[-which(shp_new@data$State %in% state_dat[,1])])
missin <- cbind(missin, matrix(NA,nrow = 2,ncol = ncol(state_dat)-1))
names(missin) <- names(state_dat)
state_dat <- rbind(state_dat,missin)
state_dat <- state_dat[order(state_dat[,1]),]
rownames(state_dat) <- 1:nrow(state_dat)
state_dat[is.na(state_dat)] <- 0
shp_new@data <- state_dat
names(shp_new@data)[2]
shp_new@data[,c(1,2)]
shp_new@data[,2:ncol(shp_new)] <- apply(shp_new@data[,2:ncol(shp_new)],MARGIN = 2,FUN = as.numeric)

unique(shp_new@data[,1])
state_dat1$state <- toupper(state_dat1$state)
state_dat11 <- data.frame("state" = unique(state_dat1$state)[order(unique(state_dat1$state))])
state_dat11 <- data.frame("state" = state_dat11[-which(state_dat11$state == "LADAKH"),])

for(i in 1:length(unique(state_dat1$scientificName))){
  state_dat11[,i+1] <- 0
  names(state_dat11)[i+1] <- unique(state_dat1$scientificName)[i]
  sub_sp <- subset(state_dat1, scientificName == unique(state_dat1$scientificName)[i])
  for(j in 1:nrow(state_dat11)){
    if(state_dat11$state[j] %in% sub_sp$state){
      state_dat11[j,i+1] <- 1
    }
  }
  if("LADAKH" %in% sub_sp$state){
   state_dat11[which(state_dat11$state == "JAMMU & KASHMIR"),i+1] <- 1
  }
}
state_dat11$state[1] <- "ANDAMAN & NICOBAR ISLANDS"
state_dat11$state[23] <- "ORISSA"
missin <- shp_new@data[,1][-which(shp_new@data[,1] %in% state_dat11$state)]

dummydf <- data.frame(matrix(0,nrow = length(missin),ncol = ncol(state_dat11)))
names(dummydf) <- names(state_dat11)
dummydf$state <- missin

state_dat11 <- rbind(state_dat11,dummydf)
state_dat11 <- state_dat11[order(state_dat11$state),]
shp_new1@data <- state_dat11  

# colz <-  37
# tm_shape(shp_new)+
#   tm_polygons(names(shp_new)[colz], palette="-Blues", contrast=.7, id="name", title=names(shp_new)[colz])+
#   tm_text(names(shp_new)[1],size = 0.5)
# 
# bins <- seq(0,1,0.2)
# pal <- colorBin("RdYlBu",bins = bins)
# labels <- paste("<p>",shp_new@data[,1],"</p>")
# leaflet() %>%
#   addPolygons(data = shp_new,fillColor = pal(shp_new@data[,3]),
#               fillOpacity = 0.8,weight = 1,smoothFactor = 0.5,color = "black",
#               highlight = highlightOptions(weight = 3,color = "#666666",bringToFront = T),
#               label = lapply(labels, HTML)
#               )

# Parameter tabs --------
# Table search parameter tabs, id is "params1"
parameter_tabs1 <- tagList(
  tags$style("#params1 { display:none; }"),
  tabsetPanel(id = "params1",
       tabPanel("Species Search",
                selectizeInput(inputId = "SS",label="Choose Species or clear with backspace and start typing",choice=NULL
                               ),
                mainPanel(
                  # downloadButton("download00", "Download",
                  #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                )
       ),
      tabPanel("Variable Search",
                           selectizeInput(inputId = "SC",label="Choose Variable",choice=tbl_search),
                mainPanel(
                  # downloadButton("download0", "Download",
                  #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                )
      ),
      tabPanel("Advanced Search Options",
               selectizeInput(inputId = "adv",label="Search via",choice=adv_search),
               selectizeInput(inputId = "cat", label = "Select", choices = NULL),
               selectizeInput(inputId = "cv",label="Choose Variable",choice=NULL), #,selected = "All"
                mainPanel(
                                # downloadButton("download", "Download",
                                #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                )
      )
  )
)

# Visualisation search parameter tabs, id is "params2"
parameter_tabs2 <- tagList(
  tags$style("#params2 { display:none; }"),
  tabsetPanel(id = "params2",
              tabPanel("Maps",
                       selectizeInput(inputId = "sel",label="Which Species do you want to see in the map ?",
                                      choice=unique(Occurrence$Acc_Species_Name))#,multiple = T)
              ),
              tabPanel("Species Charts",
                       selectizeInput(inputId = "ch1",label="Search via",choice=c("All",unique(Summary$Invasion.Status.Full)[c(3:4,2)]))
                       #selectizeInput(inputId = "cat", label = "Select", choices = NULL),
                       #selectizeInput(inputId = "cv",label="Choose Variable",choice=file_names)
              ),
              tabPanel("Variable Charts",
                       #selectizeInput(inputId = "ch2",label="Search via",choice=unique(Summary$Invasion.Status.Full))
                       selectizeInput(inputId = "ch2",label="Search via",choice=c("Growth Form","Classes","Economic Uses","Market Dynamics", "Chord Diagram"))
                       #selectizeInput(inputId = "cat", label = "Select", choices = NULL),
                       #selectizeInput(inputId = "cv",label="Choose Variable",choice=file_names)
              )        
  )
)

parameter_tabs11 <- tagList(
  tags$style("#params11 { display:none; }"),
  tabsetPanel(id = "params11",
              tabPanel("Species Search",
                       selectizeInput(inputId = "SS1",label="Choose Species or clear with backspace and start typing",choice=NULL
                       ),
                       mainPanel(
                         # downloadButton("download001", "Download",
                         #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                       )
              ),
              tabPanel("Variable Search",
                       selectizeInput(inputId = "SC1",label="Choose Variable",choice=tbl_search),
                       mainPanel(
                         # downloadButton("download01", "Download",
                         #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                       )
              ),
              tabPanel("Advanced Search Options",
                       selectizeInput(inputId = "adv1",label="Search via",choice=adv_srch1),
                       selectizeInput(inputId = "cat1", label = "Select", choices = NULL),
                       #selectizeInput(inputId = "cv1",label="Choose Variable",choice=NULL), #,selected = "All"
                       mainPanel(
                         # downloadButton("download1", "Download",
                         #                style="color: #fff; background-color: green; border-color: Black; position: center;")
                       )
              )
  )
)

# Visualisation search parameter tabs, id is "params2"
parameter_tabs21 <- tagList(
  tags$style("#params21 { display:none; }"),
  tabsetPanel(id = "params21",
              tabPanel("Maps",
                       selectizeInput(inputId = "sel1",label="Which Species do you want to see in the map ?",
                                      choice=unique(Occurrence_v1$scientificName))#,multiple = T)
              ),
              tabPanel("Species Charts",
                       selectizeInput(inputId = "ch11",label="Search via",choice=c("All",unique(Summary$Invasion.Status.Full)[c(3:4,2)]))
                       #selectizeInput(inputId = "cat", label = "Select", choices = NULL),
                       #selectizeInput(inputId = "cv",label="Choose Variable",choice=file_names)
              ),
              tabPanel("Variable Charts",
                       #selectizeInput(inputId = "ch2",label="Search via",choice=unique(Summary$Invasion.Status.Full))
                       selectizeInput(inputId = "ch21",label="Search via",choice=c("Growth Form","Classes","Economic Uses","Market Dynamics", "Chord Diagram"))
                       #selectizeInput(inputId = "cat", label = "Select", choices = NULL),
                       #selectizeInput(inputId = "cv",label="Choose Variable",choice=file_names)
              )        
  )
)


# js <- '
# $(document).on("shiny:connected", function(){
#   $("#maps").css({
#     width: window.innerWidth, 
#     height: window.innerHeight
#   });
#   $(window).on("resize", function(e){
#     if(e.target instanceof Window){
#       $("#maps").css({width: window.innerWidth, height: window.innerHeight});
#     }
#   });
# })
# '
# 

# ui ------
ui <- dashboardPage(
  skin = "green",
      # Dashboard ----
      dashboardHeader(title = "ILORA"),
  
                dashboardSidebar(
                   sidebarMenu(
                       id="menu", # Menu has 4 options (Home, Data, Vis, Release)
                       sidebarMenuOutput("menu")
                     ),
                 conditionalPanel(condition = "input.menu==`2`",
                                  selectizeInput( 
                                    inputId = "main1",
                                    label = "How do you want to look at the data ?",
                                    choices=c(
                                              "Species Search",
                                              "Variable Search",
                                              "Advanced Search Options"
                                    ),
                                    selected = "Species Search"
                                  ),
                                  parameter_tabs1
                 ),
                 conditionalPanel(condition = "input.menu==`3`",
                                  selectizeInput( 
                                    inputId = "main2",
                                    label = "How do you want to look at the data ?",
                                    choices=c(
                                      "Maps",
                                      "Species Charts",
                                      "Variable Charts"
                                    ),
                                    selected = "Species Charts"
                                  ),
                                  parameter_tabs2
                 ),
                 conditionalPanel(condition = "input.menu==`4`",
                                  selectizeInput( 
                                    inputId = "main11",
                                    label = "How do you want to look at the data ?",
                                    choices=c(
                                      "Species Search",
                                      #"Variable Search",
                                      "Advanced Search Options"
                                    ),
                                    selected = "Species Search"
                                  ),
                                  parameter_tabs11
                 ),
                 conditionalPanel(condition = "input.menu==`5`",
                                  selectizeInput( 
                                    inputId = "main21",
                                    label = "How do you want to look at the data ?",
                                    choices=c(
                                      "Maps"#,
                                      #"Species Charts",
                                      #"Variable Charts"
                                    ),
                                    selected = "Maps"
                                  ),
                                  parameter_tabs21
                 )
                 ),
                
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "1", # Home ----- 
                            br(), 
                            div(
                              h1(strong("Introduction")),
                            p("The Indian aLien flOra infoRmAtion (ILORA) database is a platform providing ecological, socio-economic, and geographic attributes for more than 1700 alien plant 
                              species ever reported from India. This informed database, being developed from extensive curation of several Indian as well as global resources, is intended to provide 
                              an integrated repository capable of assisting ongoing and upcoming research activities and development of the existing national scenario of alien plant species in India. 
                              ILORA is novel and dynamic in every possible way and therefore, is open for discussions, ideas and assistance from all quarters. Currently ILORA has 2 versions, 1.1 and 1.0 which are described below.",style="text-align:justify;color:black;background:rgba(255,153,0,0.5);padding:15px;border-radius:10px"),
                            style="text-align:justify;color:black;background:rgba(255,153,0,0.5);padding:15px;border-radius:10px"
                            ),
                            br(),
                            div(h1(strong("Brief Overview :")),
                            p(
                              br(),
                              strong("ILORA version 1.1"), "has 12,347 occurrence records for the 365 invasive and naturalized alien plant species of India, and supplements the GBIF occurrences by 60.75%. ",
                              br(),
                              "The data has been curated from 3140 published literature records and 357 herbarium sheets housed in the regional herbaria of the country.",
                              br(),
                              "For each of the occurrence records, we provided information on the three dimensions of data quality, viz.",
                              br(),
                              em(
                                "taxonomy, space, and time, ",
                              ),
                              br(),
                              "so that users can make informed decisions about the uncertainty and biases associated with these records.",
                              style="text-align:justify;color:black;background:rgba(51,150,204,0.5);padding:15px;border-radius:10px"
                              ),
                             
                             p(
                              strong("ILORA version 1.0"), "currently holds the following traits for 1747 plant species:",
                              br(),
                              em(
                              "Invasion status ",br(),"General information",br(),"Native Range",br(),"Introduction Pathways",br(),"Economic uses",br(),"Market Dynamics",br(),
                              "Habitat",br(),"Naturalised Range",br(),"Occurrence records",br(),"Distribution in India",br(),"Climatic requirement"
                              ),
                              style="text-align:justify;color:black;background:rgba(51,150,204,0.5);padding:15px;border-radius:10px"
                              ),
                            style="text-align:justify;color:black;background:rgba(51,150,204,0.5);padding:15px;border-radius:10px"
                            ),
                            br(),#tableOutput("traits")
                            p(
                            strong(
                            "From the left navigation panel, you will be able to access and visualize the data at both
                            species and variable levels.
                            It has been made with", tags$a(href="https://shiny.rstudio.com/", "Shiny", target = "blank"), " and is accessible both",tags$a(href="https://github.com/Ilora2021/ILORA", "independently", target = "blank"), "and through the database", # tags$a(href="https://github.com/Ilora2021/ILORA", "independently", target = "blank"),
                            tags$a(href="https://ilora2020.wixsite.com/ilora2020", "website", target = "blank")
                            ), "."),
                            br(),
                            # p(strong("View Data :"),"You can view the data in tables and also see the distribution of various species across India.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                            # br(),
                            div(
                              h1(strong("Navigating the app")),
                              p(
                                strong("Version 1.1"), 
                                br(),  
                                strong("Data: Species search"),
                                br(),
                                "In this section you can search the database by using a species name. The output is a table that summarizes the available data for the selected species.",
                            br(), br(),
                            strong("Data: Advanced search options"),
                            br(),
                            "In this section you can search the database by using a combination of variable names.",
                            br(), br(),
                            strong("Visualization: Maps"),
                            br(),
                            "In this section, you can search for a species and view its distribution on the geographic
                            landscape of India. States are coloured blue or red indicating absence or presence of the species. Clicking on the white dot shows additional information and the map is zoomable. The data is collected from multiple sources.", style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px",
                            br(), br(),
                            strong("Data submission: Occurrence data submission "),
                            br(),
                            "You can submit your own data and make it a part of our database. There are 3 types of data submission based on the type of data you may have. The data can be submitted via Google forms that we have made. More information available on the data submission page.", style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px"
                              ),
                            
                            p(
                            strong("Version 1.0"), 
                            br(),  
                            strong("Data: Species search"),
                            br(),
                            "In this section you can search the database by using a species name. The output is a table that summarizes the available data for the selected species.",
                            br(), br(),
                              strong("Data: Variable search"),
                            br(),
                            "In this section you can search the database by using a variable name.",
                            br(), br(),
                            strong("Data: Advanced search options"),
                            br(),
                            "In this section you can search the database by using a combination of variable names.",
                            br(), br(),
                            strong("Visualization: by species charts"),
                            br(),
                            "In this section you can view the data availability of 13 variables for all species, and for
                            three categories of alien species – invasive, naturalized, and casual.",
                            br(), br(),
                            strong("Visualization: by variable charts"),
                            br(),
                            "In this section you can view the distribution of data for selected variables across the three
                            categories of alien species – invasive, naturalized, and casual.", 
                            br(), br(),
                            strong("Visualization: Maps"),
                            br(),
                            "In this section, you can search for a species and view its distribution on the geographic
                            landscape of India. States are coloured blue or red indicating absence or presence of the species. Clicking on the white dot shows additional information and the map is zoomable. The data is collecetd from GBIF only.", style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px"
                            ),
                            style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px"
                            ),
                            
                            br(),
                            
                            div(
                              h1(strong("Quick Numbers :")),
                            p(strong("ILORA version 1.1"),"currently has data across:",
                                br(),
                                em(
                                  "Species: 365",br(),
                                  "Occurrences: 12,361",br(),
                                  "Invasive alien occurrences: 9,108",br(),
                                  "Naturalized alien occurrences: 3,253",br(),
                                  "Data sources: 3,140 literature records; 357 herbarium sheets",br(),
                                  "Distribution in number of States: 34",br(),
                                  "Highest occurrences: 600 (for Lantana camara)"
                                ), 
                                style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                              ),
                            p(strong("ILORA version 1.0"), "currently has data across:",
                              br(),
                              em(
                                "Families: 175",br(),"Genus: 910",br(),"Species: 1747",br(),"Traits: 13",br(),
                                  "Habitats: 38",br(),"Economic uses: 50",br(),"Occurrences: 40,724",br(),"Data sources: 22"
                              ), 
                              style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                            ),
                            style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"
                            )
                            
                              #tableOutput("nums"),style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px")#,
                            #br(),
                            # p(strong("ILORA welcomes new data submission:"),"ILORA is dynamic in every possible way. 
                            #   Therefore, we now call for experts to check the data and contribute new data. For any potential mistake, 
                            #   submission of new data for the existing variables, and a proposal for novel variables to be included in ILORA, 
                            #   please contact any of the core team members.",
                            #   style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                    ),
                    tabItem(tabName = "2", # Data -------
                            uiOutput("ss"),
                            uiOutput("data"), 
                            uiOutput("as")
                    ),
                    tabItem(tabName = "3", # Vis -------
                            #tags$style(type = "text/css", "#maps {height: calc(100vh - 80px) !important;}"),
                            uiOutput("maps"),
                            uiOutput("spchart",width = "100%", height = "100%"),
                            uiOutput("varchart",width = "100%", height = "100%")
                    ),
                    tabItem(tabName = "4", # Data 1.1 -------
                            uiOutput("ss1"),
                            #uiOutput("data1"), 
                            uiOutput("as1")
                    ),
                    tabItem(tabName = "5", # Vis 1.1 -------
                            #tags$style(type = "text/css", "#maps {height: calc(100vh - 80px) !important;}"),
                            uiOutput("maps1"),
                            #uiOutput("spchart1",width = "100%", height = "100%"),
                            #uiOutput("varchart1",width = "100%", height = "100%")
                    ),
                    tabItem(tabName = "6", # Data Submission -------
                            #tags$style(type = "text/css", "#maps {height: calc(100vh - 80px) !important;}"),
                            br(),#tableOutput("traits")
                            p(strong("Thanks for considering to send us your data! Please go through",tags$a(href="https://ilora2020.wixsite.com/ilora2020/submit-data", "this", target = "blank"),"document to understand the format in which we would like the data to be." )),
                            br(),
                            p("If you have only one observation of a single species, please submit it via this",
                            strong(tags$a(href="https://forms.gle/Us9VSxsj6eXw12s99", "form", target = "blank")),
                            ".",style="text-align:justify;color:black;background:rgba(255,153,0,0.5);padding:15px;border-radius:10px"),
                            br(),
                            p("If you have multiple observations of a single species, please submit your data via this",
                              strong(tags$a(href="https://forms.gle/Uj7nQ6fABcqTrsqt5", "form", target = "blank")),
                               ".",style="text-align:justify;color:black;background:rgba(51,150,204,0.5);padding:15px;border-radius:10px"),
                            br(),
                            p("If you have only multiple observations across multiple species, please submit your data via this",
                              strong(tags$a(href="https://forms.gle/ABDpLeEZJhdn4e8C8", "form", target = "blank")),
                              ".",style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px"),
                    ),
                    tabItem(tabName = "7", # Release -------
                            br(), 
                            p("Version 1.0 of the shiny webpage was developed early in 2021. The last update was released on 11 September, 2021 (IST).",style="text-align:justify;color:black;background:rgba(153,52,153,0.5);padding:15px;border-radius:10px"),
                            br(),
                            p("Version 1.1 of the shiny webpage was developed in December, 2021 using newly sourced occurrence data. The last update was released on 19 May, 2022 (PDT).",style="text-align:justify;color:black;background:rgba(153,52,153,0.5);padding:15px;border-radius:10px"),
                            br()
                    )
                  )
                )
)

# server -----
server <- function(input, output,session) {
  
  output$menu <- renderMenu({
    sidebarMenu(
              menuItem(
                div(style="display:inline-block;margin-left: 15%;padding-bottom: 10px;",
                    uiOutput("logo")
                    )  # logo is displayed as an output with tag "logo" (defined below)
                   ),
              
              menuItem("Home",tabName = "1",icon = icon("university")), # Describe menu items
              menuItem("version 1.1",icon = icon("info"),
                       menuSubItem("Data",tabName = "4",icon = icon("table")),
                       menuSubItem("Visualisation",tabName = "5",icon = icon("line-chart")),
                       menuSubItem("Occurrence Data Submission",tabName = "6",icon = icon("upload"))
              ),
              menuItem("version 1.0",icon = icon("info"),
                       menuSubItem("Data",tabName = "2",icon = icon("table")),
                       menuSubItem("Visualisation",tabName = "3",icon = icon("line-chart"))
              ),
              menuItem("Release",tabName = "7",icon = icon("file-alt"))
                )
})
  
  # Logo is rendered ad an image, which is then rendered as a UI 
  output$myImage <- renderImage({
      # Return a list containing the filename
      list(src = "ilora_logo_transparent.png", 
           contentType = 'image/png',
           width = 125,
           align="center",
#           height = 100,
           alt = "This is alternate text")
  },deleteFile = F)
  
  output$logo <- renderUI({
    tags$a(imageOutput("myImage",inline = T),href="https://ilora2020.wixsite.com/ilora2020",target = "blank")
  })
  
  # Observe which input is chosen and update tabs accordingly
  observeEvent(list(input$main1,input$main2,input$main11,input$main21), {
    updateTabsetPanel(session, "params1", selected = input$main1)
    updateTabsetPanel(session, "params2", selected = input$main2)
    updateTabsetPanel(session, "params11", selected = input$main11)
    updateTabsetPanel(session, "params21", selected = input$main21)
  })
  
  # Species Search -------
  # species search v1.0
  updateSelectizeInput(session,inputId = "SS", choices =  unique(all_temp$`Accepted Species Name`), label = paste0("Choose Species or clear with backspace and start typing"),
                       server = T)     

  var0 <- renderText(req(input$SS)) # See which species is chosen
  
  toListen0 <- reactive({ # these are the variables i want to observe 
    list(var0())
  })
  
  observeEvent(toListen0(),{ # observe them and show the corresponding table
    
    add_to_df0 <- reactive({
        data_avail0 <- tat[,c(1,which(names(tat)==var0()))]
        data_avail0
    }) 
    
    output$ss <- renderUI({
      conditionalPanel(condition = "input.main1==`Species Search`", 
                       renderDT({validate(need(input$main1=="Species Search", message = FALSE)) 
                         datatable(add_to_df0(),rownames = F,options = list(scrollX = T,searching = T,"pageLength" = 20,
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#FF69B4'});",  #e75480
                                                                              "}")),filter = "top")
                       })
      )
    })
    
    output$download00 <- downloadHandler(
      filename = function(){paste0(var0(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df0(), fname)
      }
    )
    
  })
  
  # species search v1.1
  updateSelectizeInput(session,inputId = "SS1", choices =  names(sp_srch1)[-1], label = paste0("Choose Species or clear with backspace and start typing"),
                       server = T)     
  
  var01 <- renderText(req(input$SS1)) # See which species is chosen
  
  toListen01 <- reactive({ # these are the variables i want to observe 
    list(var01())
  })
  
  observeEvent(toListen01(),{ # observe them and show the corresponding table
    
    add_to_df01 <- reactive({
      data_avail01 <- sp_srch1[,c(1,which(names(sp_srch1)==var01()))]
      data_avail01
    }) 
    
    output$ss1 <- renderUI({
      conditionalPanel(condition = "input.main11==`Species Search`", 
                       renderDT({validate(need(input$main11=="Species Search", message = FALSE)) 
                         datatable(add_to_df01(),rownames = F,options = list(scrollX = T,searching = T,"pageLength" = 20,columnDefs = list(list(width = '340px', targets = "_all")),
                                                                            initComplete = JS(
                                                                              "function(settings, json) {",
                                                                              "$(this.api().table().header()).css({'background-color': 'grey', 'color': '#FF69B4'});",  #e75480
                                                                              "}")),filter = "top")
                       })
      )
    })
    
    output$download001 <- downloadHandler(
      filename = function(){paste0(var01(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df01(), fname)
      }
    )
    
  })
  
  
  # Tables ---------- 
  # tables v1.0
  var2 <- renderText(input$SC) # See which variable is chosen
  observeEvent(var2, {
    var2a <- reactive({tbl_df$table_search[which(tbl_df$label == var2())]})
    add_to_df1 <- reactive({
      df1 <- get(var2a()) # call that file
      df1[,1] <- enc2utf8(df1[,1])
      df1
    }) 
    
    output$data <- renderUI({
        conditionalPanel(condition = "input.main1==`Variable Search`", 
                         HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_d1331e6446614cf4883792d44ba60384.pdf',  target = \'_blank\'>
                         here  </a>.</p>"), # show info on how to read
                         renderDT({ validate(need(input$main1=="Variable Search", message = FALSE)) # display that table
                          datatable(add_to_df1(),rownames = F,options = list(scrollX = T,searching = T),filter = "top") #colnames = names,
                         })
        )
      })
    
    output$download0 <- downloadHandler( # download the table
      filename = function(){paste0(var2(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df1(), fname)
      }
    )
    
  })
  
  # tables v1.1
  var21 <- renderText(input$SC1) # See which variable is chosen
  observeEvent(var21, {
    var2a1 <- reactive({tbl_df11$table_search[which(tbl_df11$label == var21())]})
    add_to_df11 <- reactive({
      df11 <- get(var2a1()) # call that file
      df11[,1] <- enc2utf8(df11[,1])
      df11
    }) 
    
    output$data1 <- renderUI({
      conditionalPanel(condition = "input.main11==`Variable Search`", 
                       HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_d1331e6446614cf4883792d44ba60384.pdf',  target = \'_blank\'>
                         here  </a>.</p>"), # show info on how to read
                       renderDT({ validate(need(input$main11=="Variable Search", message = FALSE)) # display that table
                         datatable(add_to_df11(),rownames = F,options = list(scrollX = T,searching = T),filter = "top") #colnames = names,
                       })
      )
    })
    
    output$download01 <- downloadHandler( # download the table
      filename = function(){paste0(var21(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df11(), fname)
      }
    )
    
  })
  
  # Advanced Search -------
  # adv search v1.0
  var3 <- renderText(input$adv) # See which top level category is chosen
   observeEvent(var3(), {
     if(var3() %in% adsrch_geninf$label){
       var3a <- reactive({adsrch_geninf$name[which(adsrch_geninf$label == var3())]})
       choices <- unique(GeneralInformation[,which(names(GeneralInformation) == var3a())])
       #df1 <- GeneralInformation
     }
     if(var3() %in% adsrch_spcat$label){
       var3a <- reactive({adsrch_spcat$name[which(adsrch_spcat$label == var3())]})
       choices <- unique(SpCategorization[,which(names(SpCategorization) == var3a())])
       #df1 <- GeneralInformation
     }
     # update options with unique ids of var3
     updateSelectizeInput(session,inputId = "cat", choices = choices, label = paste0("Select ",var3()," or clear with backspace and start typing"),server = T) 
     # if(var3() == "Species Name"){
     #   updateSelectizeInput(session,inputId = "cv", choices = c("All",as_var), label = paste0("Choose Variable"),server = T)     
     # }else{
       updateSelectizeInput(session,inputId = "cv", choices = as_var, label = paste0("Choose Variable"),server = T)
    # }
   })
 
 var4 <- renderText(req(input$cat)) # see which unique id is chosen
 var5 <- renderText(req(input$cv)) # see which variable (file) is chosen
 
 toListen <- reactive({ # these are the variables i want to observe 
   list(var3(),var4(),var5())
 })
 
  observeEvent(toListen(),{ # observe them and show the corresponding table
    if(var3() %in% adsrch_geninf$label){
      var3a <- reactive({adsrch_geninf$name[which(adsrch_geninf$label == var3())]})
      var3_ind <- reactive({ which(names(GeneralInformation) == var3a())})
      sp_names <- reactive({GeneralInformation[which(GeneralInformation[,var3_ind()] == var4()),1]})
    }
    if(var3() %in% adsrch_spcat$label){
      var3a <- reactive({adsrch_spcat$name[which(adsrch_spcat$label == var3())]})
      var3_ind <- reactive({ which(names(SpCategorization) == var3a()) })
      sp_names <- reactive({SpCategorization[which(SpCategorization[,var3_ind()] == var4()),1]})
    }
    
    add_to_df <- reactive({
 #     if(var5()!="All"){
        var5a <- reactive({adv_srch_var$name[which(adv_srch_var$label == var5())]})
        data_avail1 <- get(file_names[which(file_names == var5a())])
        data_avail1 <- data_avail1[which(data_avail1[,1] %in% sp_names()),]
        data_avail1
      # }else{
      #   data_avail1 <- tat[,c(1,which(names(tat)==var4()))]
      #   data_avail1
      # }
    }) 
    
    output$as <- renderUI({
      conditionalPanel(condition = "input.main1==`Advanced Search Options`", 
                         HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_d1331e6446614cf4883792d44ba60384.pdf',  target = \'_blank\'>
                         here</a>.</p>"),
                       renderDT({validate(need(input$main1=="Advanced Search Options", message = FALSE)) 
                         datatable(add_to_df(),rownames = F,options = list(scrollX = T,searching = T,"pageLength" = 25),filter = "top")
                       })
      )
    })
    
    output$download <- downloadHandler(
      filename = function(){paste0(var3(),"_",var4(),"_",var5(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df(), fname)
      }
    )

  })
  
  # adv search v1.1
  var31 <- renderText(input$adv1) # See which top level category is chosen
  observeEvent(var31(), {
    # if(var31() %in% adsrch_geninf$label){
    #   var3a1 <- reactive({adsrch_geninf$name[which(adsrch_geninf$label == var31())]})
    #   choices1 <- unique(GeneralInformation[,which(names(GeneralInformation) == var3a1())])
    #   #df1 <- GeneralInformation
    # }
    #if(var31() %in% adsrch_spcat$label){
      #var3a1 <- reactive({adsrch_spcat$name[which(adsrch_spcat$label == var31())]})
      choices1 <- unique(sp_srch11[,which(names(sp_srch11) == var31())])
      #df1 <- GeneralInformation
    #}
    # update options with unique ids of var3
    updateSelectizeInput(session,inputId = "cat1", choices = choices1, label = paste0("Select ",var31()," or clear with backspace and start typing"),server = T) 
    # if(var3() == "Species Name"){
    #   updateSelectizeInput(session,inputId = "cv", choices = c("All",as_var), label = paste0("Choose Variable"),server = T)     
    # }else{
    #updateSelectizeInput(session,inputId = "cv1", choices = as_var, label = paste0("Choose Variable"),server = T)
    # }
  })
  
  var41 <- renderText(req(input$cat1)) # see which unique id is chosen
  #var51 <- renderText(req(input$cv1)) # see which variable (file) is chosen
  
  toListen1 <- reactive({ # these are the variables i want to observe 
    list(var31(),var41())#,var51())
  })
  
  observeEvent(toListen1(),{ # observe them and show the corresponding table
    # if(var31() %in% adsrch_geninf$label){
    #   var3a1 <- reactive({adsrch_geninf$name[which(adsrch_geninf$label == var31())]})
    #   var3_ind1 <- reactive({ which(names(GeneralInformation) == var3a1())})
    #   sp_names1 <- reactive({GeneralInformation[which(GeneralInformation[,var3_ind1()] == var41()),1]})
    # }
    # if(var31() %in% adsrch_spcat$label){
    #   var3a1 <- reactive({adsrch_spcat$name[which(adsrch_spcat$label == var31())]})
    #   var3_ind1 <- reactive({ which(names(SpCategorization) == var3a1()) })
    #   sp_names1 <- reactive({SpCategorization[which(SpCategorization[,var3_ind1()] == var41()),1]})
    # }
    
    add_to_df1a <- reactive({
      #     if(var5()!="All"){
      # var5a1 <- reactive({adv_srch_var11$name[which(adv_srch_var11$label == var51())]})
      # data_avail11 <- get(file_names[which(file_names == var5a1())])
      data_avail11 <- sp_srch11[which(sp_srch11[,which(names(sp_srch11)==var31())] == var41()),]
        #subset(sp_srch11, var31() == var41()) #sp_srch11[which(data_avail11[,1] %in% sp_names1()),]
      data_avail11
      # }else{
      #   data_avail1 <- tat[,c(1,which(names(tat)==var4()))]
      #   data_avail1
      # }
    }) 
    
    output$as1 <- renderUI({
      conditionalPanel(condition = "input.main11==`Advanced Search Options`", 
                       HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_d1331e6446614cf4883792d44ba60384.pdf',  target = \'_blank\'>
                         here</a>.</p>"),
                       renderDT({validate(need(input$main11=="Advanced Search Options", message = FALSE)) 
                         datatable(add_to_df1a(),rownames = F,options = list(scrollX = T,searching = T,"pageLength" = 25),filter = "top")
                       })
      )
    })
    
    output$download1 <- downloadHandler(
      filename = function(){paste0(var31(),"_",var41(),"_",var51(),".csv")}, 
      content = function(fname){
        write.csv(add_to_df1a(), fname)
      }
    )
    
  })
  
  
  # Maps ------
  # maps v1.0
  var1 <- renderText(input$sel) # The selected species
  
  observeEvent(var1(),{
    if(var1() == ""){ # If no input, return message
      output$maps <- renderUI({
        conditionalPanel(condition = "input.main2==`Maps`",
                         HTML("<p><strong>Please choose a Species</strong></p>")
        )
      })
    }else{
      add_to_df2 <- reactive({ # Create data frame and add information to be displayed about that species
        df2 <- Occurrence[which(Occurrence$Acc_Species_Name == var1()),-4]
        cn <- GeneralInformation[which(GeneralInformation$Accepted.scientific.name.of.species == var1()),2]
        if(identical(cn, character(0))){
          df2$Common_Name <- "Data Unavailable"
        }else{
          df2$Common_Name <- cn
        } 
        fam <- SpCategorization[which(SpCategorization$Acc_Species_Name == var1()),4]
        if(identical(fam, character(0))){
          df2$Family <- "Data Unavailable"
        }else{
          df2$Family <- fam
        }
        invs <- SpCategorization[which(SpCategorization$Acc_Species_Name == var1()),8]
        if(identical(invs, character(0))){
          df2$Invasion_Status <- "Data Unavailable"
        }else{
          df2$Invasion_Status <- invs
        }
        names(df2) <- c("Name","Longitude", "Latitude","Common Name", "Family","Invasion Status")
        df2
      }) 
      
      output$maps <- renderUI({ # Call leaflet function to display stuff on map
        conditionalPanel(condition = "input.main2==`Maps`",
                          renderLeaflet({ validate(need(input$main2=="Maps", message = FALSE))
                            bins <- seq(0,1,0.2)
                            pal <- colorBin("RdYlBu",bins = bins,reverse = T)
                            labels <- paste("<p>",shp_new@data[,1],"</p>")
                            leaflet(add_to_df2()) %>%
                              addPolygons(data = shp_new,fillColor = pal(shp_new@data[,var1()]),
                                          fillOpacity = 0.8,weight = 1,smoothFactor = 0.5,color = "black",
                                          highlight = highlightOptions(weight = 3,color = "#666666"), #,bringToFront = T
                                          label = lapply(labels, HTML)
                              ) %>%
                              addCircles(lng = ~Longitude, lat = ~Latitude) %>%
                              addTiles() %>%
                              addCircleMarkers(data = add_to_df2(), lat =  ~Latitude, lng =~Longitude,
                                               radius = 1, color = "white", opacity = 1,
                                               popup = ~paste0('<strong>Name: </strong>',Name,
                                                                           '<br><strong>Longitude:</strong> ', Longitude,
                                                                           '<br><strong>Latitude:</strong> ', Latitude,
                                                                           '<br><strong>Common Name: </strong>',`Common Name`,
                                                                           '<br><strong>Family:</strong> ', Family,
                                                                           '<br><strong>Invasion Status:</strong> ', `Invasion Status`
                                                                            )
                                              )
                          })
                        # renderLeaflet({
                        #   tmap_mode("view")
                        #   working_map <- tm_shape(shp_new)+
                        #     tm_polygons(var1(), palette="-Blues", contrast=.7, id="name", title=var1())+
                        #     tm_text(names(shp_new)[1],size = 0.5)
                        #   tmap_leaflet(working_map)
                        # })
                        # renderTmap({
                        #   tm_shape(shp_new)+
                        #     tm_polygons(var1(), palette="-Blues", contrast=.7, id="name", title=var1())+
                        #     tm_text(names(shp_new)[1],size = 0.5)
                        # })
                        
        )
      })
    }
  })

  # maps v1.1
  var11 <- renderText(input$sel1) # The selected species
  
  observeEvent(var11(),{
    if(var11() == ""){ # If no input, return message
      output$maps1 <- renderUI({
        conditionalPanel(condition = "input.main21==`Maps`",
                         HTML("<p><strong>Please choose a Species</strong></p>")
        )
      })
    }else{
      add_to_df21 <- reactive({ # Create data frame and add information to be displayed about that species
        #df21 <- Occurrence[which(Occurrence$Acc_Species_Name == var11()),-4]
        df21 <- Occurrence_v1[which(Occurrence_v1$scientificName == var11()),c(3,7,12,11,2,9,18:20,23)]
        cn1 <- GeneralInformation[which(GeneralInformation$Accepted.scientific.name.of.species == var11()),2]
        if(identical(cn1, character(0))){
          df21$Common_Name <- "Data Unavailable"
        }else{
          df21$Common_Name <- cn1
        } 
        names(df21)[ncol(df21)] <- "Common Name"
        df21
      }) 
      
      output$maps1 <- renderUI({ # Call leaflet function to display stuff on map
        conditionalPanel(condition = "input.main21==`Maps`",
                         renderLeaflet({ validate(need(input$main21=="Maps", message = FALSE))
                           bins <- seq(0,1,0.2)
                           pal <- colorBin("RdYlBu",bins = bins,reverse = T)
                           labels <- paste("<p>",shp_new1@data[,1],"</p>")
                           labels[15] <- paste("<p>",shp_new1@data[15,1],", LADAKH</p>")
                           leaflet(add_to_df21()) %>%
                             addPolygons(data = shp_new1,fillColor = pal(shp_new1@data[,var11()]),
                                         fillOpacity = 0.8,weight = 1,smoothFactor = 0.5,color = "black",
                                         highlight = highlightOptions(weight = 3,color = "#666666"), #,bringToFront = T
                                         label = lapply(labels, HTML)
                             ) %>%
                             addCircles(lng = ~decimalLongitude, lat = ~decimalLatitude) %>%
                             addTiles() %>%
                             addCircleMarkers(data = add_to_df21(), lat =  ~decimalLatitude, lng =~decimalLongitude,
                                              radius = 1, color = "white", opacity = 1,
                                              popup = ~paste0('<strong>Common Name: </strong>',`Common Name`,
                                                              '<br><strong>scientificName: </strong>',scientificName,
                                                              '<br><strong>family:</strong> ', family,
                                                              '<br><strong>decimalLongitude:</strong> ', decimalLongitude,
                                                              '<br><strong>decimalLatitude:</strong> ', decimalLatitude,
                                                              '<br><strong>basisOfRecord:</strong> ', basisOfRecord,
                                                              '<br><strong>eventDate:</strong> ', eventDate,
                                                              '<br><strong>habitat:</strong> ', habitat,
                                                              '<br><strong>occurrenceRemarks:</strong> ', occurrenceRemarks,
                                                              '<br><strong>coordinateUncertaintyInMeters:</strong> ', coordinateUncertaintyInMeters,
                                                              '<br><strong>taxonRemarks:</strong> ', taxonRemarks
                                              )
                             )
                         })
                         # renderLeaflet({
                         #   tmap_mode("view")
                         #   working_map <- tm_shape(shp_new1)+
                         #     tm_polygons(var1(), palette="-Blues", contrast=.7, id="name", title=var1())+
                         #     tm_text(names(shp_new1)[1],size = 0.5)
                         #   tmap_leaflet(working_map)
                         # })
                         # renderTmap({
                         #   tm_shape(shp_new1)+
                         #     tm_polygons(var1(), palette="-Blues", contrast=.7, id="name", title=var1())+
                         #     tm_text(names(shp_new1)[1],size = 0.5)
                         # })
                         
        )
      })
    }
  })
  
  
  # Species Charts --------
  # species charts v1.0
  var6 <- renderText(input$ch1) # 3 choices: Invasie (In), Naturalised(Nt), Casual (CA)
  
  observeEvent(var6(),{
    add_to_df3 <- reactive({ # make dataframe of chosen invasion status and calculate data availability
      if(var6()!="All"){
        sumsub <- Summary[which(Summary$Invasion.Status.Full == var6()),]
      }else{
        sumsub <- Summary
      }
      
      sumsub1 <- sumsub[1,-c(1,-1:0+ncol(sumsub))]
      
      for(i in 1:ncol(sumsub1)){
        sumsub1[1,i] <- sum(sumsub[,i+1],na.rm = T)/nrow(sumsub)*100
      }
      sumsub1 <- data.frame(t(sumsub1))
      sumsub1$x <- rownames(sumsub1)
      names(sumsub1) <- c("x","y")
      sumsub1$x <- as.numeric(sumsub1$x)
      
      sumsub1
    })
      
    output$spchart <- renderUI({ # Show plots
      conditionalPanel(condition = "input.main2==`Species Charts`",
                       renderPlot({ validate(need(input$main2=="Species Charts", message = FALSE))
                         p1 <- ggplot(add_to_df3(),aes(x=x,y=y))+geom_segment(aes(x=0, xend=x, y=y, yend=y), color="black") +
                           geom_point(color="red", size=4, alpha=0.6)+ggtitle("Data Availability")+
                           xlab(paste0("Percentage of ",var6()," species data available"))+
                           ylab("Variable")
                         
                         pltlist <- list(p1) 
                         
                          if(var6() == "All"){ # if "ALL", then i want to show pie chart also
                            p <- ggplot(pie_data, aes(x = "", ymin = ymin, ymax = ymax, fill = Invasion.status)) +
                              geom_bar(aes(y = Species),width = 1, stat = "identity", color = "black") +
                              coord_polar(theta = "y")+xlab("")+ylab("")+ggtitle("Number of species in each invasion status category")+
                              geom_text(aes(y = lab.ypos, label = Species), color = "black", size = 4.5,fontface = "bold")+
                              theme(axis.text.y = element_text(face = "bold",size = 3))+
                              scale_y_continuous(breaks = pie_data$ymax) + scale_fill_manual(values = c("Invasive" = "coral",
                                                             "Naturalized" = "deepskyblue",
                                                             "Casual" = "blue",
                                                             "Native" = "forestgreen",
                                                             "Cryptogenic"="cornsilk"))
                            pltlist <- list(p,p1)
                          }
                         
                         grid.arrange(grobs=pltlist,ncol=length(pltlist))
                       })
      )
    })
    
    output$meaning <- renderUI({ # meaning of charts (unused)
      conditionalPanel(condition = "input.main2==`Species Charts`", 
                       renderText({ validate(need(input$main2=="Species Charts", message = FALSE)) 
                         paste0(
                           "Consider data variable Anthromes. So out of all ", input$ch1, " type species in the Anthromes file, data for ",
                          round(add_to_df3()$x[which(add_to_df3()$y == "Anthromes")],2)," % of these species is available."
                          )
                       })
      ) 
    })
    
  })
  
  # species charts v1.1
  var61 <- renderText(input$ch11) # 3 choices: Invasie (In), Naturalised(Nt), Casual (CA)
  
  observeEvent(var61(),{
    add_to_df31 <- reactive({ # make dataframe of chosen invasion status and calculate data availability
      if(var61()!="All"){
        sumsub11 <- Summary[which(Summary$Invasion.Status.Full == var61()),]
      }else{
        sumsub11 <- Summary
      }
      
      sumsub1a <- sumsub11[1,-c(1,-1:0+ncol(sumsub11))]
      
      for(i in 1:ncol(sumsub1a)){
        sumsub1a[1,i] <- sum(sumsub11[,i+1],na.rm = T)/nrow(sumsub11)*100
      }
      sumsub1a <- data.frame(t(sumsub1a))
      sumsub1a$x <- rownames(sumsub1a)
      names(sumsub1a) <- c("x","y")
      sumsub1a$x <- as.numeric(sumsub1a$x)
      
      sumsub1a
    })
    
    output$spchart1 <- renderUI({ # Show plots
      conditionalPanel(condition = "input.main21==`Species Charts`",
                       renderPlot({ validate(need(input$main21=="Species Charts", message = FALSE))
                         p11 <- ggplot(add_to_df31(),aes(x=x,y=y))+geom_segment(aes(x=0, xend=x, y=y, yend=y), color="black") +
                           geom_point(color="red", size=4, alpha=0.6)+ggtitle("Data Availability")+
                           xlab(paste0("Percentage of ",var61()," species data available"))+
                           ylab("Variable")
                         
                         pltlist1 <- list(p11) 
                         
                         if(var61() == "All"){ # if "ALL", then i want to show pie chart also
                           p1a <- ggplot(pie_data, aes(x = "", ymin = ymin, ymax = ymax, fill = Invasion.status)) +
                             geom_bar(aes(y = Species),width = 1, stat = "identity", color = "black") +
                             coord_polar(theta = "y")+xlab("")+ylab("")+ggtitle("Number of species in each invasion status category")+
                             geom_text(aes(y = lab.ypos, label = Species), color = "black", size = 4.5,fontface = "bold")+
                             theme(axis.text.y = element_text(face = "bold",size = 3))+
                             scale_y_continuous(breaks = pie_data$ymax) + scale_fill_manual(values = c("Invasive" = "coral",
                                                                                                       "Naturalized" = "deepskyblue",
                                                                                                       "Casual" = "blue",
                                                                                                       "Native" = "forestgreen",
                                                                                                       "Cryptogenic"="cornsilk"))
                           pltlist1 <- list(p1a,p11)
                         }
                         
                         grid.arrange(grobs=pltlist1,ncol=length(pltlist1))
                       })
      )
    })
    
    output$meaning1 <- renderUI({ # meaning of charts (unused)
      conditionalPanel(condition = "input.main21==`Species Charts`", 
                       renderText({ validate(need(input$main21=="Species Charts", message = FALSE)) 
                         paste0(
                           "Consider data variable Anthromes. So out of all ", input$ch1, " type species in the Anthromes file, data for ",
                           round(add_to_df31()$x[which(add_to_df31()$y == "Anthromes")],2)," % of these species is available."
                         )
                       })
      ) 
    })
    
  })
  
  
  # Variable Charts ------
  # var charts v1.0
  var7 <- renderText(input$ch2) # See variable selected
  
  observeEvent(var7(),{
    output$varchart <- renderUI({ # show corresponding chart, these plots are written in "plots.R"
      conditionalPanel(condition = "input.main2==`Variable Charts`",
                       renderPlot({ validate(need(input$main2=="Variable Charts", message = FALSE))
                         if(var7() != "Chord Diagram"){
                           if(var7() == "Growth Form"){
                              pltlist1 <- list(gfp)
                           }
                           if(var7() == "Classes"){
                             pltlist1 <- list(clp)
                           }
                           if(var7() == "Economic Uses"){
                             pltlist1 <- list(ecop)
                           }
                           if(var7() == "Market Dynamics"){
                             pltlist1 <- list(mktp)
                           }
                           grid.arrange(grobs=pltlist1,ncol=length(pltlist1))
                         }else{
                           chordDiagram(chdf, grid.col = grid.col, directional = 1, direction.type = c("diffHeight", "arrows"),
                                        link.arr.type = "big.arrow",big.gap = 20, annotationTrack = "grid", 
                                        preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chdf))))))
                           
                           circos.track(track.index = 1, panel.fun = function(x, y) {
                             xlim = get.cell.meta.data("xlim")
                             xplot = get.cell.meta.data("xplot")
                             ylim = get.cell.meta.data("ylim")
                             sector.name = get.cell.meta.data("sector.index")
                             
                             if(abs(xplot[2] - xplot[1]) < 10) {
                               circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                                           niceFacing = TRUE, adj = c(0, 0.5), col = "blue")
                             } else {
                               circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                                           niceFacing = TRUE, adj = c(0.5, 0), col= "blue")
                             }
                           }, bg.border = NA)
                         }
                       }),
                       
                       if(var7() == "Chord Diagram"){
                         HTML(
                           "<p><strong>IP: Introduction Pathways</strong></p>",
                           "<p><strong>1.2: Erosion Control, 1.5: Landscape Movement, 1.6: Conservation, 1.7: Release in nature for use</strong></p>",
                           "<p><strong>2.1: Agriculture, 2.9: Ornamental, 2.Others: Botanical Garden + Aquariam Species + Forestry + Horticulture + Food + Other escape</strong></p>",
                           "<p><strong>3.4.5: Containment (Nursery, Food, Plant, Seed, Habitat materials), Stoaway(Packing, Ballast water), Interconnected waterways</strong></p>",
                           "<br>",
                           "<p><strong>S1: Europe, S2: Africa, S3: Asia-Temperate, S4: Asia-Tropical</strong></p>",
                           "<p><strong>S5: Australasia, S6: Pacific, S7: Nothern America, S8: Southern America</strong></p>"
                         )
                       }
                       
      )
    })
    
  })
  
  # var charts v1.1
  var71 <- renderText(input$ch21) # See variable selected
  
  observeEvent(var71(),{
    output$varchart1 <- renderUI({ # show corresponding chart, these plots are written in "plots.R"
      conditionalPanel(condition = "input.main21==`Variable Charts`",
                       renderPlot({ validate(need(input$main21=="Variable Charts", message = FALSE))
                         if(var71() != "Chord Diagram"){
                           if(var71() == "Growth Form"){
                             pltlist1b <- list(gfp)
                           }
                           if(var71() == "Classes"){
                             pltlist1b <- list(clp)
                           }
                           if(var71() == "Economic Uses"){
                             pltlist1b <- list(ecop)
                           }
                           if(var71() == "Market Dynamics"){
                             pltlist1b <- list(mktp)
                           }
                           grid.arrange(grobs=pltlist1b,ncol=length(pltlist1b))
                         }else{
                           chordDiagram(chdf, grid.col = grid.col, directional = 1, direction.type = c("diffHeight", "arrows"),
                                        link.arr.type = "big.arrow",big.gap = 20, annotationTrack = "grid", 
                                        preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chdf))))))
                           
                           circos.track(track.index = 1, panel.fun = function(x, y) {
                             xlim = get.cell.meta.data("xlim")
                             xplot = get.cell.meta.data("xplot")
                             ylim = get.cell.meta.data("ylim")
                             sector.name = get.cell.meta.data("sector.index")
                             
                             if(abs(xplot[2] - xplot[1]) < 10) {
                               circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                                           niceFacing = TRUE, adj = c(0, 0.5), col = "blue")
                             } else {
                               circos.text(mean(xlim), ylim[1], sector.name, facing = "inside", 
                                           niceFacing = TRUE, adj = c(0.5, 0), col= "blue")
                             }
                           }, bg.border = NA)
                         }
                       }),
                       
                       if(var71() == "Chord Diagram"){
                         HTML(
                           "<p><strong>IP: Introduction Pathways</strong></p>",
                           "<p><strong>1.2: Erosion Control, 1.5: Landscape Movement, 1.6: Conservation, 1.7: Release in nature for use</strong></p>",
                           "<p><strong>2.1: Agriculture, 2.9: Ornamental, 2.Others: Botanical Garden + Aquariam Species + Forestry + Horticulture + Food + Other escape</strong></p>",
                           "<p><strong>3.4.5: Containment (Nursery, Food, Plant, Seed, Habitat materials), Stoaway(Packing, Ballast water), Interconnected waterways</strong></p>",
                           "<br>",
                           "<p><strong>S1: Europe, S2: Africa, S3: Asia-Temperate, S4: Asia-Tropical</strong></p>",
                           "<p><strong>S5: Australasia, S6: Pacific, S7: Nothern America, S8: Southern America</strong></p>"
                         )
                       }
                       
      )
    })
    
  })
  
  
} 

shinyApp(ui, server)
