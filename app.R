#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyr)
library(leaflet)
library(DT)
library(waffle)

# Waltham Forest boundaries with house price (mean data for year end 2017) from:
#https://data.london.gov.uk/dataset/average-house-prices
# Read cirme data derived from:
# https://data.london.gov.uk/dataset/recorded_crime_summary 
wf<-readRDS(file="data/waltham_forest.Rds")
wf_crime<-readRDS(file="data/waltham_forest_crime.Rds")
wf_price<-readRDS(file="data/waltham_forest_price.Rds")

nearest_polygons_boys<-readRDS(file="data/nearest_polygons_boys.Rds")
nearest_polygons_girls<-readRDS(file="data/nearest_polygons_girls.Rds")

wf_price@data$Value<-wf_price@data$Value/1000

pal_crime <- colorQuantile("Reds", wf_crime$crime_count, n = 7)

# set palete for house price values
bins <- c(0, 300, 350, 400, 450, 500, 550, 600, Inf)
pal_house <- colorBin("YlOrRd", domain = wf_price$Value, bins = bins)

# Conversion factor for cut off distances
milestometers=1609.34

# Read data exported from www.compare-school-performance.service.gov.uk/
keystage4<-read.csv("data/320_ks4revised.csv")

#  select(URN, SCHNAME,TOTPUPS, NUMBOYS, NUMGIRLS, TPRIORLO:P8MEAOPEN_CIUPP)
keystage4_2018<-read.csv("data/2017-2018/320_ks4provisional.csv")

# Read location data.   This is converted from the format in the above website.
LatLong<-read.csv("data/schoolLatLong1.csv")
loc_boys<-filter(LatLong, Gender!="G")
loc_girls<-filter(LatLong, Gender!="B")

# Read the cut off distances.  These are compiled from multiple years of allocation tables.
# www.walthamforest.gov.uk/sites/default/files/Starting%20Secondary%20School%202019.pdf
cutOff<-read.csv("data/CutOff.csv")

# These are the school websites
web<-read.csv("data/web.csv")

# Read the allocation table.  These are from 
# www.walthamforest.gov.uk/sites/default/files/Starting%20Secondary%20School%202019.pdf
allocation<-read.csv("data/place allocation 2024.csv")

# Get rid of na values
allocation[is.na(allocation)] <- 0

# Merge the small allocation catagories to make the graph simpler
allocation<-allocation %>% 
  mutate(LAC=LAC+Medical+Staff+Social+Catchment) %>%
  rename(Other=LAC)%>%
  select(-Medical, -Staff, -Social, -Catchment)

# Reserve the station cut off for Eden girls
queens_eden<-filter(cutOff,School=="Eden Queens Road Station")
queens_eden$Lat=51.581421
queens_eden$Lon=-0.023849

# Set the display bounds for the map
bounds<-bbox(wf)

# Merge and tidy the data
keystage<-mutate(keystage4,NUMBOYS=as.numeric(as.character(NUMBOYS)))
keystage<-mutate(keystage,NUMGIRLS=as.numeric(as.character(NUMGIRLS)))
keystage<-merge(keystage,cutOff, by="URN")
keystage<-merge(keystage,LatLong, by="URN")
keystage$NUMBOYS[is.na(keystage$NUMBOYS)]<-0
keystage$NUMGIRLS[is.na(keystage$NUMGIRLS)]<-0

# Calculate the gender balance
keystage<-mutate(keystage,genderbalance=
             (NUMBOYS/(NUMBOYS+NUMGIRLS))*100)

# Write data into text labels
keystage<-mutate(keystage,label=paste(as.character(SCHNAME),
                          " Places: ", PAN,
                          " Total pupils:",as.character(round(TOTPUPS)),
                          " (",as.character(round(genderbalance)),"% Boys)"))
keystage<-mutate(keystage,label_CO18=paste(as.character(SCHNAME),
                               " 2024 Cut off  ",as.character(CO_2018),"miles"))

# Clean the data for the attainment graph
keystage4_2018<-select(keystage4_2018, SCHNAME,P8MEA,PTL2BASICS_94,
                 P8CIUPP, P8CILOW, URN)                          %>%
  mutate(P8MEA=as.numeric(as.character(P8MEA)),
         P8CIUPP=as.numeric(as.character(P8CIUPP)),
         P8CILOW=as.numeric(as.character(P8CILOW)),         
         PTL2BASICS_94=as.numeric(sub("%", "",PTL2BASICS_94))
  )                                                              %>%
  rename(progress8_18=P8MEA, maths_englishAC_18=PTL2BASICS_94, 
         P8CILOW_18=P8CILOW, P8CIUPP_18=P8CIUPP)

# Read finance data
finance<-read.csv("data/320_cfr.csv")
finance<-finance%>%select(URN, PUPILS, FSM, TOTALINCOME, PTEACHINGSTAFF, PSUPPLYTEACHERS, PEDUCATIONSUPPORTSTAFF, PPREMISES, PBACKOFFICE,PCATERING, POTHERSTAFF, PENERGY, PLEARNINGRESOURCES, PICT, POTHER)

finance<-finance%>%mutate_at(.funs = funs(as.numeric(as.character(sub("%", "",.)))),
                             .vars = vars(PTEACHINGSTAFF:POTHER))
finance$URN<-as.numeric(finance$URN)
finance<-finance[complete.cases(finance),]

finance<-inner_join(keystage,finance, by="URN")

# gather to long format for bar chart
finance_long <- finance %>% select(SCHNAME, PTEACHINGSTAFF:POTHER) %>% 
  mutate(PPREMISES=PPREMISES+PBACKOFFICE) %>%
  select(-PBACKOFFICE) %>%
  gather(Type, Percentage, PTEACHINGSTAFF:POTHER)


# Clean the data for the attainment graph
keystage_prior<-select(keystage, SCHNAME,PTPRIORLO,PTPRIORAV,PTPRIORHI,PTEALGRP2,PSENSE4,P8MEA,PTL2BASICS_94,
                 P8CIUPP, P8CILOW, URN) %>%
  mutate(PTPRIORLO=-as.numeric(sub("%", "",PTPRIORLO)),
         PTPRIORAV=as.numeric(sub("%", "",PTPRIORAV)),
         PTPRIORHI=as.numeric(sub("%", "",PTPRIORHI)),
         PTEALGRP2=as.numeric(sub("%", "",PTEALGRP2)),
         P8MEA=suppressWarnings(as.numeric(as.character(P8MEA))),
         P8CIUPP=as.numeric(as.character(P8CIUPP)),
         P8CILOW=as.numeric(as.character(P8CILOW)),         
         PSENSE4=as.numeric(sub("%", "",PSENSE4)),
         PTL2BASICS_94=as.numeric(sub("%", "",PTL2BASICS_94))
  )%>%
  inner_join(keystage4_2018, by="URN")%>%
  mutate(balance=PTPRIORHI+PTPRIORLO,SCHNAME=SCHNAME.x)%>%
  rename(High=PTPRIORHI, Low=PTPRIORLO, sec_lang=PTEALGRP2, SEN=PSENSE4, progress8=P8MEA, maths_englishAC=PTL2BASICS_94)
keystage_prior_long<-gather(keystage_prior, key="Attainment",value="Percent",High,Low)
#keystage_prior_long<-mutate(keystage_prior_long, Percent=as.numeric(sub("%", "",Percent)))
keystage_prior_long<-drop_na(keystage_prior_long)%>%
  filter(!SCHNAME=="")
keystage_prior$Specific<-0

# set palete for cut off circles
pal <- colorNumeric(
  palette = "Dark2",
  domain = 1:4)

# Define UI for application that draws a histogram
ui <- fluidPage(
   # Application title
   titlePanel("Waltham Forest Secondary Schools"),
   
   helpText("Select a school and a year to see the cut off area."
            ,"No circle will be shown if there was no cut off."),
   
   # Sidebar to select school and year 
   sidebarLayout(
      sidebarPanel(
        radioButtons("schools", "Schools List:",
                           c("Buxton School"="Buxton School",
                             "Chingford Foundation School" = "Chingford Foundation School",
                             "Connaught School for Girls" = "Connaught School for Girls",
                             "Eden Girls' School Waltham Forest"="Eden Girls' School Waltham Forest",
                             "Frederick Bremer School"="Frederick Bremer School",
                             "George Mitchell School" = "George Mitchell School",
                             "Heathcote School & Science College" = "Heathcote School & Science College",
                             "Highams Park School"="Highams Park School",
                             "Holy Family Catholic School"="Holy Family Catholic School",
                             "Kelmscott School" = "Kelmscott School",
                             "Lammas School and Sixth Form" = "Lammas School and Sixth Form",
                             "Leytonstone School"="Leytonstone School",
                             "Norlington School and 6th Form" = "Norlington School and 6th Form",
                             "South Chingford Foundation (was Rushcroft Foundation School)"="South Chingford Foundation School",
                             "Walthamstow Academy"="Walthamstow Academy",
                             "Walthamstow School for Girls" = "Walthamstow School for Girls",
                             "Willowfield School (was Humanities College)" = "Willowfield School"
                      ),
                     selected = "Walthamstow School for Girls"
                     ),
        checkboxGroupInput("year", "Cut off for year:",
                        c("2015"="2015",
                          "2016"="2016",
                          "2017"="2017",
                          "2018"="2018",
                          "2019"="2019",
                          "2020"="2020",
                          "2021"="2021",
                          "2022"="2022",
                          "2023"="2023",
                          "2024"="2024" 
                          ))
#                        selected = "2024")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
 
                    tabPanel("Maps", 
                            selectInput("map_type", "Map Overlay:", c("None","Nearest School (Girls)","Nearest School (Boys)","Crime","House Prices"),
                                         selected="Nearest School (Girls)"),
                            textOutput("selectedSchool"),
                            leafletOutput("mymap"),
                            actionButton("reset", "Reset Map"),
                            textOutput("gender_balance"),
                            helpText("Each tile in the graph below represents a place in the school selected in 2024."
                              ,"The colour of the tile shows how it was allocated"),
                            plotOutput("waffle_plot")
                    ),
                    tabPanel("Academic",
                              plotOutput("maths_englishAC"),
                              plotOutput("progress8"),
                              plotOutput("priorAttainment"),
                              helpText("Percentage of pupils with average attainment not shown."),
                              plotOutput("secondLanguage"),
                              plotOutput("SEN"),
                              helpText("")
                             ),

                    tabPanel("Finance", 
                            plotOutput("spend_pc"),
                            selectInput("financeGraph", label="Select Expenditure Catagory", 
                            c("Education Support Staff"="PEDUCATIONSUPPORTSTAFF",
                            "Premises & Back Office"="PPREMISES", "Supply Teachers"="PSUPPLYTEACHERS", 
                            "Teaching Staff"="PTEACHINGSTAFF"), selected = "PPREMISES", multiple = FALSE,
                                   selectize = TRUE, width = NULL, size = NULL),
                            plotOutput("individual_spend_pc")
                            ),
                    tabPanel("Cutoff Distance Table", 
                             tableOutput("table")
                             ),
                    tabPanel("Trends", 
                             plotOutput("cutoff_trend"),
                    )
                    ),

        helpText("Allocation and cut off data from:",
            "https://www.walthamforest.gov.uk/sites/default/files/2022-03/How%20School%20Places%20Were%20allocated%20secondary%202022.pdf",
            "Role and location data from:",
            "www.compare-school-performance.service.gov.uk/")
        )
   )
)

test <- function(school){
  leafletProxy("mymap") %>% 
    clearMarkers()      %>%
    addMarkers(lat = school$Lat, lng = school$Lon)
}

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
# Allocation places plot
  output$waffle_plot <- renderPlot({
    school<-filter(allocation, SCHNAME==input$schools)
    school<-school[1,4:11]

# first remember the names
     names <- t(colnames(school))
     school<-as.numeric(school)
     school<-setNames(school, names)
     waffle(school, size=1.5, rows=10, equal=TRUE)
  })
  
  filtered <- reactive({
    keystage_prior$Specific <- ifelse((keystage_prior$SCHNAME == input$schools), 1,0)
    return(keystage_prior)
  })
  
  finance_single <- reactive({
    specific_finance<-finance_long %>%
                    filter(Type==input$financeGraph) 
    specific_finance$Specific<- ifelse((specific_finance$SCHNAME == input$schools), 1,0)
    return(specific_finance)
  })

 graph_8 <- function(graph)
    {
      main_data = "progress8"
      err_high = "P8CIUPP"
      err_low = "P8CILOW"
      filtered()                  %>% 
        filter(!is.na(progress8)) %>%
        ggplot(aes(x=reorder(SCHNAME,progress8), y=progress8, fill = Specific) )+
        geom_col()+
        labs(title ="Progress 8 - 2018", subtitle="", 
           x = "School Name", y="Grade") +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
           ylim(-1.5, 1.5)+
           guides(fill=FALSE) 
  }
  
  
  filtered_long <- reactive({
    keystage_prior_long$Specific <- ifelse((keystage_prior_long$SCHNAME == input$schools), 1,0.5)
    return(keystage_prior_long)
  })
  
  # prints prior attainment Hi and Lo.
  output$priorAttainment <- renderPlot({
  filtered_long() %>%
    ggplot( aes(x=reorder(SCHNAME,balance)) )+
    geom_col(aes( y=Percent, fill=Attainment,  alpha=Specific))+
    scale_y_continuous(breaks=seq(-40,40,10),labels=abs(seq(-40,40,10)))+
      scale_alpha(range=c(0.1,1), limits=c(0,1))+
    coord_flip() + 
    labs(title ="Balance of prior attainment",
         subtitle="% pupils end of KS4 with low and high attainment at KS2",
         x = "School Name") +
      guides(alpha=FALSE)
  })
  
  output$secondLanguage <- renderPlot({
    filtered() %>% filter(!is.na(sec_lang))%>%
      ggplot(aes(x=reorder(SCHNAME,sec_lang), y=sec_lang, fill = Specific) )+
      geom_col()+
      labs(title ="English as an addition language",
           subtitle="% pupils end of KS4",
           x = "School Name", y="%") +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
           guides(fill=FALSE) + ylim(0,100)
  })
  
  output$cutoff_trend <- renderPlot({
    filtered() %>% filter(!is.na(sec_lang))%>%
      ggplot(aes(x=reorder(SCHNAME,sec_lang), y=sec_lang, fill = Specific) )+
      geom_col()+
      labs(title ="English as an addition language",
           subtitle="% pupils end of KS4",
           x = "School Name", y="%") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      guides(fill=FALSE) + ylim(0,100)
  })
  

  output$progress8 <- renderPlot({
    graph_8(input$resultGraph)
  })
  
  # prints all expenditure percentages.
  output$spend_pc <- renderPlot({
      ggplot(finance_long, aes(x = SCHNAME, y = Percentage, fill = Type)) +
      geom_col() +
      labs(title ="Expenditure by type",
           x = "School Name", y="Percentage") +
      coord_flip()
  })   
  
  output$individual_spend_pc <- renderPlot({
     ggplot(finance_single(), aes(x = reorder(SCHNAME,Percentage), y = Percentage, fill = Specific)) +
       geom_col() +
        labs(title ="Expenditure by type",
            x = "School Name", y="Percentage") +
        guides(fill=FALSE)+
        coord_flip()
 })
  
  output$SEN <- renderPlot({
    filtered() %>% filter(!is.na(SEN))%>%
      ggplot(aes(x=reorder(SCHNAME,SEN), y=SEN, fill = Specific) )+
      geom_col()+
      labs(title ="Special Educational Needs",
           subtitle="Pupils end of KS4",
           x = "School Name", y="Number") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      guides(fill=FALSE) #+ ylim(0,10)
  })
  
  output$secondLanguage <- renderPlot({
    filtered() %>% filter(!is.na(sec_lang))%>%
      ggplot(aes(x=reorder(SCHNAME,sec_lang), y=sec_lang, fill = Specific) )+
      geom_col()+
      labs(title ="English as an addition language",
           subtitle="% pupils end of KS4",
           x = "School Name", y="%") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      guides(fill=FALSE) + ylim(0,100)
  })
  output$maths_englishAC<- renderPlot({
    filtered() %>% filter(!is.na(maths_englishAC))%>%
      ggplot(aes(x=reorder(SCHNAME,maths_englishAC), y=maths_englishAC, fill = Specific) )+
      geom_col()+
      labs(title ="Pupils achieving standard passes in english and mathematics",
           subtitle="% (GCSE grades 9-4)",
           x = "School Name", y="%") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      guides(fill=FALSE) + ylim(0,100)
  })

# Main map
  output$mymap <- renderLeaflet({
    if (input$map_type=="House Prices"){
    keystage %>%  leaflet() %>%
      addTiles() %>%
      addPolygons(data = wf_price,
                  fillOpacity = 0.6, 
                  fillColor = ~pal_house(Value),
 #                 color = "#BDBDC3", 
                  weight = 2)%>%
      addMarkers(popup = keystage$SCHNAME)%>% 
      addLegend("bottomright", pal = pal_house, values=wf_price@data$Value,
                        title = "House Prices 2017 (1000s)",
                        opacity = 1) %>%
      setView(mean(bounds[1,]),
              mean(bounds[2,]),
              zoom=12)
    }
    else if (input$map_type=="Crime"){
      keystage %>%  leaflet() %>%
        addTiles() %>%
        addPolygons(data = wf_crime,
                    fillOpacity = 0.6, 
                    fillColor = ~pal_crime(crime_count),
                    #                 color = "#BDBDC3", 
                    weight = 2)%>%
        addMarkers(popup = keystage$SCHNAME)%>% 
        addLegend("bottomright", pal = pal_crime, values=wf_crime$crime_count,
                  title = "Reported crime 2014-17 \n (Quantiles)",
                  opacity = 1) %>%
        setView(mean(bounds[1,]),
                mean(bounds[2,]),
                zoom=12)
    }
    else if (input$map_type=="Nearest School (Girls)"){
      keystage %>%  leaflet() %>%
        addTiles() %>%
        addPolygons(data = nearest_polygons_girls,
                    fillOpacity = 0.1, 
                    fillColor = "#BDBDC3", 
                    weight = 2)%>%
        addMarkers(data=loc_girls, popup = loc_girls$School, lng=~Lon, lat=~Lat)%>% 
        setView(mean(bounds[1,]),
                mean(bounds[2,]),
                zoom=12)
    }
    else if (input$map_type=="Nearest School (Boys)"){
      keystage %>%  leaflet() %>%
        addTiles() %>%
        addPolygons(data = nearest_polygons_boys,
                    fillOpacity = 0.1, 
                    fillColor = "#BDBDC3", 
                    weight = 2)%>%
        addMarkers(data=loc_boys, popup = loc_boys$School, lng=~Lon, lat=~Lat)%>%
        setView(mean(bounds[1,]),
                mean(bounds[2,]),
                zoom=12)
    }
    else     {
      keystage     %>%  
        leaflet()  %>%
        addTiles() %>%
        addPolygons(data = wf,
                    fillOpacity = 0.1, 
                    color = "#BDBDC3", 
                    weight = 2)%>%
        addMarkers(popup = keystage$SCHNAME)%>%
        setView(mean(bounds[1,]),
                mean(bounds[2,]),
                zoom=12)
      }
    })

# Reset for map
  observeEvent(input$reset, {
    output$mymap <- renderLeaflet({
        keystage %>%  
        leaflet() %>%
        addTiles() %>%
        addPolygons(data = wf,
                    fillOpacity = 0.1, 
                    color = "#BDBDC3", 
                    weight = 2)%>%
        addMarkers(popup = keystage$SCHNAME)%>%
        setView(mean(bounds[1,]),
                mean(bounds[2,]),
                zoom=12)
    })
  })
  
# Over plot for cut off areas 
  observeEvent({input$schools
                input$year}, {
    school<-filter(keystage, SCHNAME==input$schools)
    eden<-input$schools=="Eden Girls' School Waltham Forest"
    leafletProxy("mymap") %>% 
     clearMarkers()      %>%
     addMarkers(lat = school$Lat, lng = school$Lon)
    
  popup=input$schools
  lat = school$Lat
  lng = school$Lon
  EdenPopup="Eden Girls Queens Road Station Cut Off"

   
  if ("2015" %in% input$year) {
       radius=school$CO_2015*milestometers
       leafletProxy("mymap")%>%
         addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                    radius=radius, color = pal(1),fillOpacity = 0)    
  }
  
  if ("2016" %in% input$year) {
    radius=school$CO_2016*milestometers
    radiusQueens=queens_eden$CO_2016*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
 
  if ("2017" %in% input$year) {
    radius=school$CO_2017*milestometers
    radiusQueens=queens_eden$CO_2017*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  } 
  
  if ("2018" %in% input$year) {
    radius=school$CO_2018*milestometers
    radiusQueens=queens_eden$CO_2018*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2019" %in% input$year) {
    radius=school$CO_2019*milestometers
    radiusQueens=queens_eden$CO_2019*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2020" %in% input$year) {
    radius=school$CO_2020*milestometers
    radiusQueens=queens_eden$CO_2020*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2021" %in% input$year) {
    radius=school$CO_2021*milestometers
    radiusQueens=queens_eden$CO_20121*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2022" %in% input$year) {
    radius=school$CO_2022*milestometers
    radiusQueens=queens_eden$CO_2022*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2023" %in% input$year) {
    radius=school$CO_2023*milestometers
    radiusQueens=queens_eden$CO_2023*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  
  if ("2024" %in% input$year) {
    radius=school$CO_2024*milestometers
    radiusQueens=queens_eden$CO_2024*milestometers
    leafletProxy("mymap")%>%
      addCircles(lat = lat, lng = lng, popup=popup, weight = 2, 
                 radius=radius, color = pal(1),fillOpacity = 0) 
    if (eden) {
      leafletProxy("mymap")%>%
        addCircles(lat = queens_eden$Lat, lng = queens_eden$Lon, 
                   popup=EdenPopup, weight = 2, radius=radiusQueens,
                   color = pal(2),fillOpacity = 0)
      
    }
  }
  })
  
  drawCutoff<- function(year, school) {
    leafletProxy("mymap")%>%
      addMarkers(lat = school$Lat, lng = school$Lon, popup = keystage$label)
    leafletProxy("mymap")%>%
      addCircles(lat = school$Lat, lng = school$Lon, popup=school$label_CO21, weight = 2, radius=school$CO_2022*milestometers,
                 color=pal(4),fillOpacity = 0) 
  }
  
  output$selectedSchool<- renderText({ 
    paste(input$schools)
  })
  output$gender_balance<- renderText({ 
    filter(keystage, SCHNAME==input$schools)$label
  })
  
  output$table <- renderTable({
    cutOff
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

