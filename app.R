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

# Variable file names
req_files <- file_names[-c(3,6,14:15)]

# Choices for Table search
table_search <- file_names[-which(file_names == "CultivatedSpecies"|file_names == "Summary")]
reorder <- c(11,13,5,12,2,7,4,10,6,3,8,9,1)
tbl_df <- data.frame(table_search,reorder)
tbl_df$label <- tbl_df$table_search
tbl_df$label[c(3,4,5,9:11,13)] <- c("Economic Uses", "Ecological Regions","General Information","Market Dynamics", "Native Range","Naturalized Range","Species Categorization")
tbl_search <- tbl_df[order(tbl_df$reorder),3]

# Choices for Advanced search
adv_search_spcat <- names(SpCategorization)[-c(2:3,5:7,9)]
adsrch_spcat <- data.frame("name"=adv_search_spcat,"label"=adv_search_spcat)
adsrch_spcat$label[c(1,3)] <- c("Species Name", "Invasion Status")

adv_search_geninf <- names(GeneralInformation)[2:6]
adsrch_geninf <- data.frame("name"=adv_search_geninf,"label"=adv_search_geninf)
adsrch_geninf$label[1:3] <- c("Common Name","Vernacular Name","Growth Habit")
adv_search <- c(adsrch_spcat$label,adsrch_geninf$label)

adv_srch_var <- data.frame("name" = file_names)
adv_srch_var$reorder <- c(11,13,15,5,12,2,7,4,10,6,3,8,9,1,14)
adv_srch_var$label <- adv_srch_var$name
adv_srch_var$label[c(3:6,10:12,14)] <- c("Cultivated Species", "Economic Uses", "Ecological Regions","General Information","Market Dynamics", "Native Range", "Naturalized Range", "Species Categorization")
as_var <- adv_srch_var$label[order(adv_srch_var$reorder)]

# Fix column names of Eco Uses
names(EconomicUses)[-c(1,53)] <- lapply(names(EconomicUses)[-c(1,53)],function(x){strsplit(x,"_")[[1]][2]})

# Call R files containing code for plots
source("plots.R")


# Parameter tabs --------
# Table search parameter tabs, id is "params1"
parameter_tabs1 <- tagList(
  tags$style("#params1 { display:none; }"),
  tabsetPanel(id = "params1",
      tabPanel("Tables",
                           selectizeInput(inputId = "SC",label="Choose Variable",choice=tbl_search),
               mainPanel(
                 downloadButton("download0", "Download",
                                style="color: #fff; background-color: green; border-color: Black; position: center;")
               )
      ),
      tabPanel("Advanced Search",
               selectizeInput(inputId = "adv",label="Search via",choice=adv_search),
               selectizeInput(inputId = "cat", label = "Select", choices = NULL),
               selectizeInput(inputId = "cv",label="Choose Variable",choice=as_var),
               mainPanel(
                               downloadButton("download", "Download",
                                              style="color: #fff; background-color: green; border-color: Black; position: center;")
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
                                              "Tables",
                                              "Advanced Search"
                                    ),
                                    selected = "Tables"
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
                                    selected = "Maps"
                                  ),
                                  parameter_tabs2
                 )
                 ),
                
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "1", # Home ----- 
                            br(), 
                            p("The Indian aLien flOra infoRmAtion (ILORA) database is a platform providing ecological, socio-economic, and geographic attributes for more than 1700 alien plant 
                              species ever reported from India. This informed database, being developed from extensive curation of several Indian as well as global resources, is intended to provide 
                              an integrated repository capable of assisting ongoing and upcoming research activities and development of the existing national scenario of alien plant species in India. 
                              ILORA is novel and dynamic in every possible way and therefore, is open for discussions, ideas and assistance from all quarters.",style="text-align:justify;color:black;background:rgba(255,153,0,0.5);padding:15px;border-radius:10px"),
                            br(),
                            p(strong("Brief Overview :"),"ILORA version 1.0 currently holds the following traits for 1747 plant species:",
                              br(),br(),
                              em(
                              "Invasion status ",br(),"General information",br(),"Native Range",br(),"Introduction Pathways",br(),"Economic uses",br(),"Market Dynamics",br(),
                              "Habitat",br(),"Naturalised Range",br(),"Occurrence records",br(),"Distribution in India",br(),"Climatic requirement"
                              ),
                              style="text-align:justify;color:black;background:rgba(51,150,204,0.5);padding:15px;border-radius:10px"),
                            br(),#tableOutput("traits")
                            p(
                            strong(
                            "From the left navigation panel, you will be able to access and visualize the data at both
                            species and variable levels.
                            It has been made with", tags$a(href="https://shiny.rstudio.com/", "Shiny", target = "blank"), " and is accessible both independently and through the database",
                            tags$a(href="https://ilora2020.wixsite.com/ilora2020", "website", target = "blank")
                            ), "."),
                            br(),
                            # p(strong("View Data :"),"You can view the data in tables and also see the distribution of various species across India.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                            # br(),
                            p(
                            strong("Search by species"),
                            br(),
                            "In this section you can search the database by using a species name. Along with the
                            general information, the variable names for which data are available have been marked.
                            You can click on the variable names to get the detail information.",
                            br(), br(),
                              strong("Search by variables"),
                            br(),
                            "In this section you can search the database by using a variable name.",
                            br(), br(),
                            strong("Visualization by species charts"),
                            br(),
                            "In this section you can view the data availability of 13 variables for all species, and for
                            three categories of alien species – invasive, naturalized, and casual.",
                            br(), br(),
                            strong("Visualization by variable charts"),
                            br(),
                            "In this section you can view the distribution of data for selected variables across the three
                            categories of alien species – invasive, naturalized, and casual.", 
                            br(), br(),
                            strong("Visualization of map"),
                            br(),
                            "In this section, you can search for a species and view its distribution on the geographic
                            landscape of India. Click on a point will provide additional info for a species along with its
                            location information in decimal degrees.", style="text-align:justify;color:black;background:rgba(0,153,0,0.5);padding:15px;border-radius:10px"
                            ),
                            br(),
                            p(strong("Quick Numbers :"),"ILORA version 1.0 currently has data across:",
                              br(),
                              em(
                                "Families: 175",br(),"Genus: 910",br(),"Species: 1747",br(),"Traits: 13",br(),
                                  "Habitats: 38",br(),"Economic uses: 50",br(),"Occurrences: 40,724",br(),"Data sources: 22"
                              ),
                              tableOutput("nums"),style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                            br(),
                            p(strong("ILORA welcomes new data submission:"),"ILORA is dynamic in every possible way. 
                              Therefore, we now call for experts to check the data and contribute new data. For any potential mistake, 
                              submission of new data for the existing variables, and a proposal for novel variables to be included in ILORA, 
                              please contact any of the core team members.",
                              style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                    ),
                    tabItem(tabName = "2", # Data -------
                            uiOutput("data"), 
                            uiOutput("as")
                    ),
                    tabItem(tabName = "3", # Vis -------
                            uiOutput("maps"),
                            uiOutput("spchart",width = "100%", height = "100%"),
                            uiOutput("varchart",width = "100%", height = "100%")
                    ),
                    tabItem(tabName = "4", # Release -------
                            br(), 
                            p("This is version 1.0 of the shiny webpage. The last update was released on 22 May, 2021.",style="text-align:justify;color:black;background:rgba(153,52,153,0.5);padding:15px;border-radius:10px"),
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
              menuItem("Home",tabName = "1"), # Describe menu items
              menuItem("Data",tabName = "2"),
              menuItem("Visualisation",tabName = "3"),
              menuItem("Release",tabName = "4")
                )
})
  
  # Logo is rendered ad an image, which is then rendered as a UI 
  output$myImage <- renderImage({
      # Return a list containing the filename
      list(src = "Logo_1.png",
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
  observeEvent(list(input$main1,input$main2), {
    updateTabsetPanel(session, "params1", selected = input$main1)
    updateTabsetPanel(session, "params2", selected = input$main2)
  })
  
  # Tables ---------- 
  var2 <- renderText(input$SC) # See which variable is chosen
  observeEvent(var2, {
    var2a <- reactive({tbl_df$table_search[which(tbl_df$label == var2())]})
    add_to_df1 <- reactive({
      df1 <- get(var2a()) # call that file
      df1[,1] <- enc2utf8(df1[,1])
      df1
    }) 
    
    output$data <- renderUI({
        conditionalPanel(condition = "input.main1==`Tables`", 
                         HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_7471ed3ba1be44349e95458fe62d4279.pdf' ,  target = \'_blank\'>
                         here  </a>.</p>"), # show info on how to read
                         renderDT({ validate(need(input$main1=="Tables", message = FALSE)) # display that table
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
  
  # Advanced Search -------
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
     updateSelectizeInput(session,inputId = "cat", choices = choices, label = paste0("Select ",var3()),server = T) 
   })
 
 var4 <- renderText(input$cat) # see which unique id is chosen
 var5 <- renderText(input$cv) # see which variable (file) is chosen
 
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
      var5a <- reactive({adv_srch_var$name[which(adv_srch_var$label == var5())]})
      data_avail1 <- get(file_names[which(file_names == var5a())])
      data_avail1 <- data_avail1[which(data_avail1[,1] %in% sp_names()),]
      data_avail1
    }) 
    
    output$as <- renderUI({
      conditionalPanel(condition = "input.main1==`Advanced Search`", 
                         HTML("<p><strong>The
                         abbreviations used in the table have been explained in
                         the ‘How-To-Read’ document, the PDF of which is available </strong>
                         <a href='https://ccb04108-04f3-44b1-8634-1e97f0f55bab.filesusr.com/ugd/8fc17d_7471ed3ba1be44349e95458fe62d4279.pdf'>
                         here</a>.</p>"),
                       renderDT({validate(need(input$main1=="Advanced Search", message = FALSE)) 
                         datatable(add_to_df(),rownames = F,options = list(scrollX = T,searching = T),filter = "top")
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
                           leaflet(add_to_df2()) %>% 
                             addCircles(lng = ~Longitude, lat = ~Latitude) %>% 
                             addTiles() %>%
                             addCircleMarkers(data = add_to_df2(), lat =  ~Latitude, lng =~Longitude,
                                              radius = 3, popup = ~paste0('<strong>Name: </strong>',Name,
                                                                          '<br><strong>Longitude:</strong> ', Longitude,
                                                                          '<br><strong>Latitude:</strong> ', Latitude,
                                                                          '<br><strong>Common Name: </strong>',`Common Name`,
                                                                          '<br><strong>Family:</strong> ', Family,
                                                                          '<br><strong>Invasion Status:</strong> ', `Invasion Status`
                                              ),
                                              stroke = FALSE, fillOpacity = 0.8)
                         })
        )
      })
    }
  })
  
  
  # Species Charts --------
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
  
  # Variable Charts ------
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
  
} 

shinyApp(ui, server)
