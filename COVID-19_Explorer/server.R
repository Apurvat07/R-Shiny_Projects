# Data from:
# Johns Hopkins University Center for System Science and Engineering (JHU CCSE)
library(dplyr)
library(tidyr)  
library(gganimate)
library(ggplot2)
library(gganimate)
library(tidyr)  
library(ggrepel)
library(reshape2)
library(gifski)
library(png)
library(magick)
library(prettyunits)
library(bsplus)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(RColorBrewer)
library(shinyWidgets)
library("viridis") 
set.seed(2000)

baseURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series"

f1 = list(family="Courier New, monospace", size=12, color="rgb(30,30,30)")

minutesSinceLastUpdate = function(fileName) {
  (as.numeric(as.POSIXlt(Sys.time())) - as.numeric(file.info(fileName)$ctime)) / 60
}

loadData = function(fileName, columnName) {
  if(!file.exists(fileName) || minutesSinceLastUpdate(fileName) > 10) {
    data = read.csv(file.path(baseURL, fileName), check.names=FALSE, stringsAsFactors=FALSE) %>%
      select(-Lat, -Long) %>% 
      pivot_longer(-(1:2), names_to="date", values_to=columnName) %>% 
      mutate(
        date=as.Date(date, format="%m/%d/%y"),
        `Country/Region`=if_else(`Country/Region` == "", "?", `Country/Region`)
        #`Province/State`=if_else(`Province/State` == "", "<all>", `Province/State`)
      )
    save(data, file=fileName)  
  } else {
    load(file=fileName)
  }
  return(data)
}

allData = 
  loadData(
    "time_series_covid19_confirmed_global.csv", "CumConfirmed") %>%
  inner_join(loadData(
    "time_series_covid19_deaths_global.csv", "CumDeaths")) %>%
  inner_join(loadData(
    "time_series_covid19_recovered_global.csv","CumRecovered"))

confirmed_global = 
  loadData("time_series_covid19_confirmed_global.csv","confirmed_global")


function(input, output, session) {
  
  output$vx <- renderUI({
    selectInput("country", label=h6("Select Country"), choices=sort(unique(allData$`Country/Region`)),selected = "US")
  })
  output$vx1 <- renderUI({
    pickerInput(
      inputId = "country1", label = "Select/deselect Countries ",
      choices = sort(unique(allData$`Country/Region`)),
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None",
        `select-all-text` = "All"
      ),multiple = TRUE,selected = c("US","Italy","India")
    )
    
  })
  data = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
 
    d = d %>% 
      group_by(date) %>% 
      summarise_if(is.numeric, sum, na.rm=TRUE)
 
    
    d %>%
      mutate(
        dateStr = format(date, format="%b %d, %Y"),    # Jan 20, 2020
        NewConfirmed=CumConfirmed - lag(CumConfirmed, default=0),
        NewRecovered=CumRecovered - lag(CumRecovered, default=0),
        NewDeaths=CumDeaths - lag(CumDeaths, default=0)
      )
  })
  
  
  data2 = reactive({
    d = allData %>%
      filter(`Country/Region` == input$country)
  
    d = d %>%
      group_by(date) %>%
      summarise_if(is.numeric, sum, na.rm=TRUE)
    
    
  })
  
  output$plot1 <- renderImage({
    
    d1<-data2()
    print(d1)
    d2 <- melt(d1, id.var="date")
    
    
    outfile <- tempfile(fileext='.gif')
    
    p <- ggplot(d2,aes(x=date, y=value, col=variable),group = 1 ) +
      geom_point(aes(group = seq_along(date)), 
                 size = 2, # size of points
                 alpha = 0.8) + # slightly transparent
      geom_line() + # slightly transparent
      labs(y = "Total cases",
           x = "Date",
           title = "Overall COVID-19 cases till Date: ") + 
      transition_reveal(along = date,range = NULL,keep_last = TRUE)+
      geom_text_repel(aes(label = value),
                      nudge_x      = 0.15,
                      direction    = "y",
                      hjust        = 0,
                      segment.size = 0.2
      )+
      scale_color_manual(values = c("CumConfirmed" = "#2A00B6", # assign colors to groups
                                    "CumRecovered" = "#9B0E84",
                                    "CumDeaths" = "#E94657"),
                         name = "Cumulative Cases", labels = c("Confirmed", "Recovered", "Death")) +
      theme_minimal() + # apply minimal theme
      theme(text = element_text(size = 14),legend.position="top")+
      view_follow(fixed_x = TRUE)+
      enter_fade() +
      exit_fade()
    
    # use animate() to control nframes and pass shiny progress update function
    anim_save(filename = "outfile.gif", animate(p),path = ".")
    #anim_save(filename = "outfile.gif", animation = p)
    
    list(src = "outfile.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  
  
  output$plot2 <- renderImage({
    
    d1<-data()
    d1<-d1[,c("date","NewConfirmed","NewRecovered","NewDeaths")]
    print(d1)
    d3 <- melt(d1, id.var="date")
    
    
    outfile1 <- tempfile(fileext='.gif')
    
    p1 <- ggplot(d3,aes(x=date, y=value, col=variable),group = 1 ) +
      geom_point(aes(group = seq_along(date)), 
                 size = 2, # size of points
                 alpha = 0.8) + # slightly transparent
      geom_line() + # slightly transparent
      labs(y = "Total cases",
           x = "Date",
           title = "Overall COVID-19 cases till Date: ") + 
      transition_reveal(along = date,range = NULL,keep_last = TRUE)+
      geom_text_repel(aes(label = value),
                      nudge_x      = 0.15,
                      direction    = "y",
                      hjust        = 0,
                      segment.size = 0.2
      )+
      scale_color_manual(values = c("NewConfirmed" = "#2A00B6", # assign colors to groups
                                    "NewRecovered" = "#9B0E84",
                                    "NewDeaths" = "#E94657"),
                         name = "New Cases", labels = c("Confirmed", "Recovered", "Death")) +
      theme_minimal() + # apply minimal theme
      theme(text = element_text(size = 14),legend.position="top")+
      view_follow(fixed_x = TRUE)+
      enter_fade() +
      exit_fade()
    
    # use animate() to control nframes and pass shiny progress update function
    anim_save(filename = "outfile1.gif", animate(p1),path = ".")
    #anim_save(filename = "outfile.gif", animation = p)
    
    list(src = "outfile1.gif",
         contentType = 'image/gif' 
    )}, deleteFile = TRUE)
  
  Lat_Lag<-read.csv("Lat_Log.csv")
  Lat_Lag<-Lat_Lag[,c(2:5)]
  allData$`Country/Region`<-trimws( allData$`Country/Region`)
  merge_data<-merge(allData,Lat_Lag,by.x=c("Country/Region","Province/State"),by.y=c("Country.Region","Province.State"),all.x = TRUE)

  ################################# 
  #define the color pallate for the magnitidue of the earthquake
  pal <- colorNumeric(
    palette =  viridis(5),
    domain = merge_data$CumConfirmed)
  
  
  
  #create the map
  output$mymap <- renderLeaflet({
    leaflet(merge_data) %>%
      #fitBounds(~min(Long), ~min(Lat), ~max(l=Long), ~max(Lat)) %>%
      setView(lng = 0, lat = 0, zoom = 2)  %>% #setting the view over ~ center of North America
      addTiles() %>%
      addCircles(data = merge_data, lat = ~ Lat, lng = ~ Long, weight = 1,
                 radius = ~sqrt(CumConfirmed)*1000, 
                 popup = ~as.character(CumConfirmed),
                 label = ~as.character(paste("Total cases: ",CumConfirmed,
                                             " Country: ",`Country/Region`)),
                 labelOptions = labelOptions(noHide = T, direction = "top",
                                             style = list(
                                               "color" = "red",
                                               "font-family" = "serif",
                                               "font-style" = "italic",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = "12px",
                                               "border-color" = "rgba(0,0,0,0.5)"
                                             )),
                 color = ~pal(CumConfirmed), 
                 fillOpacity = 0.4)%>%
      addLegend("topleft", pal = pal, values = ~CumConfirmed,
                title = "Confirmed Cases",
                opacity = 1
      )
  })

  output$plot3 <- renderImage({
    
    d3<-allData
    outfile3 <- tempfile(fileext='.gif')
    lbl_countries<-paste(input$country1,sep = ",")
    print(lbl_countries)
    p3 <- ggplot(
      d3, 
      aes(x = date, y=CumConfirmed, size = CumConfirmed, colour = `Country/Region`)) + 
      # geom_text(aes(x=12500, y=800000, label=as.character(date))
      #           , size=15, colour="lightgrey", alpha=0.5, inherit.aes = FALSE)+
      geom_point(show.legend = FALSE, alpha = 0.7) +
      scale_color_viridis_d() +
      scale_size(range = c(2, 12)) +
      #scale_x_log10() +
      #view_follow(fixed_x = TRUE)+
      theme_ipsum_rc(grid_col = "grey90",axis_title_just = 'm')+
      labs( x = 'Date', y = 'Confirmed Cases') +
      ease_aes('linear')+
      transition_time(date) +
      labs(title = "Year: {frame_time}")+
      #shadow_wake(wake_length = 0.05, alpha = FALSE,wrap = TRUE)+
      geom_label_repel(data=. %>% filter(`Country/Region` %in% lbl_countries),
                 aes(x=date, CumConfirmed, label=`Country/Region`),
                 nudge_x =10, inherit.aes = FALSE)
    
    
    # use animate() to control nframes and pass shiny progress update function
    anim_save(filename = "outfile3.gif", animate(p3),path = ".")
    #anim_save(filename = "outfile.gif", animation = p)
    
    list(src = "outfile3.gif",
         contentType = 'image/gif'
    )}, deleteFile = TRUE)
  
  
}


