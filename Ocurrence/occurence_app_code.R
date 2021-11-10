# Note that this file only contains (all) data and code for the maps part of the app. To see the app code
# please look at app.R

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

# Files ----- 
files <- list.files("Files/")
file_names <- lapply(files,function(x){strsplit(x,"_")[[1]][2]})
file_names <- unlist(file_names)
file_names <- lapply(file_names,function(x){strsplit(x,"\\.")[[1]][1]})
file_names <- unlist(file_names)

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

names(State);nrow(State)
names(State)[-1] <-gsub(".", " ", names(State)[-1], fixed=TRUE)
state <- State[,c(1:2)]
for(i in 1:nrow(state)){
  state[i,2] <- toString(names(State[,-c(1,ncol(State))])[which(State[i,-c(1,ncol(State))]!="")])
  if(state[i,2]==""){state[i,2] <- "No Data Avaiable"}
}
all_temp <- cbind(all_temp,state$Kerala)
names(all_temp)[13] <- "Distribution in states and union territories"

names(LULC);nrow(LULC)
lulc <- LULC[,c(1,2)]
lulc$Wet.Evergreen.forest <- rowSums(LULC[,2:35],na.rm = T)
all_temp <- cbind(all_temp,lulc$Wet.Evergreen.forest)
names(all_temp)[14] <- "Distributed in LULC classes (count)"

names(Anthromes);nrow(Anthromes)
anth <- Anthromes[,c(1,2)]
anth$Urban <- rowSums(Anthromes[,2:19],na.rm = T)
all_temp <- cbind(all_temp,anth$Urban)
names(all_temp)[15] <- "Distributed in Anthromes classes (count)"

names(Ecoregions);nrow(Ecoregions)
ecolreg <- Ecoregions[,c(1,2)]
ecolreg$South.Western.Ghats.Montane.Rain.Forests <- rowSums(Ecoregions[,2:43],na.rm = T)
all_temp <- cbind(all_temp,ecolreg$South.Western.Ghats.Montane.Rain.Forests)
names(all_temp)[16] <- "Distributed in Ecoregions classes (count)"

names(Climate);nrow(Climate)
clim <- Climate[,c(1:3,6:7)]
clim$clim <- clim$Min.Ppt
for(i in 1:nrow(clim)){
  clim$clim[i] <- toString(clim[i,2:5])  
  if(clim$clim[i] == "NA, NA, NA, NA"){clim$clim[i] <- "NA"}
}

all_temp <- cbind(all_temp,clim$clim)
names(all_temp)[17] <- "Climate [Max. Temp. (degree C), Min. Temp. (degree C), Max. Ppt. (mm), Min. Ppt. (mm)]"

tat <- data.table(all_temp)
tat <- dcast(melt(tat, id.vars = "Accepted Species Name"), variable ~ `Accepted Species Name`)
names(tat)[1] <- "Variable"
tat <- as.data.frame(tat)

# Variable file names
req_files <- file_names[-c(3,6,14:15)]

# Choices for Table search
table_search <- file_names[-which(file_names == "CultivatedSpecies"|file_names == "Summary"|file_names == "Metadata")]
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

# Choices for Advanced search
adv_search_spcat <- names(SpCategorization)[-c(2:3,5:7,9)]
adsrch_spcat <- data.frame("name"=adv_search_spcat,"label"=adv_search_spcat)
adsrch_spcat$label[c(1,3)] <- c("Species Name", "Invasion Status")

adv_search_geninf <- names(GeneralInformation)[2:6]
adsrch_geninf <- data.frame("name"=adv_search_geninf,"label"=adv_search_geninf)
adsrch_geninf$label[1:3] <- c("Common Name","Vernacular Name","Growth Habit")
adv_search <- c(adsrch_spcat$label,adsrch_geninf$label)

adv_srch_var <- data.frame("name" = file_names)
#adv_srch_var <- adv_srch_var[-16,]
adv_srch_var$reorder <- c(12,14,16,5,13,2,7,4,11,6,17,3,8,9,1,10,15)
adv_srch_var$label <- adv_srch_var$name
adv_srch_var$label[c(3:6,10,12:13,15)] <- c("Cultivated Species", "Economic Uses", "Ecological Regions","General Information","Market Dynamics", "Native Range", "Naturalized Range", "Species Categorization")
#adv_srch_var <- rbind(adv_srch_var,data.frame("name" = "master","reorder"=0,"label" = "All"))
as_var <- adv_srch_var$label[order(adv_srch_var$reorder)]
#as_var <- c("All",as_var)

#file_names <- c(file_names,"master")

# Call R files containing code for plots
source("plots.R")

# Shape file -----
#shp_new <- readOGR(dsn = "India_map/India_project2.shp",stringsAsFactors = FALSE)
shp_new <- readOGR(dsn = "States/Admin2.shp",stringsAsFactors = FALSE)
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


# Tables ---------- Occurence is an option in variable
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

# Advanced Search ------- Occurence is an option
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


# Maps ------
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
  
