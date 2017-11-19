## PROJECT: Python - Web Scapping

library(shiny)
library(ggthemes)
library(shinythemes)
library(scales)
library(shinydashboard)
library(ggrepel)

library(googleVis)
library(ggmap)
library(maps)

library(dplyr)
library(tidyr)
library(ggplot2)
library(NLP)
library(tm)
#library(readxl)
#library(readr)
library(lubridate)
library(wordcloud)

load("dataW.RData")

body <- dashboardBody(
  offset = 0, style='padding:0px;',
  fluidPage(
    offset = 0, style='padding:0px;',
    wellPanel(id = "tPanel",style = "overflow-y:scroll; max-height: 720px; width: 1150px; margin:0;",
              
              fluidRow(
              
                box(title = "IT Jobs USA map",
                    collapsible = TRUE,
                    status = "success",
                    width = 7,
                    htmlOutput("heatmap")
                ),
                
                box(title = "Top Hiring Cities in State",
                    collapsible = TRUE,
                    status = "warning", 
                    width = 5,
                    plotOutput("statemap")
                )
              ),
              
              fluidRow(
                box(
                  title = "Top Hiring States by Keywords:",
                  width = 7,
                  solidHeader = TRUE, status = "primary",
                  plotOutput("topStates")
                ),

                box(
                  title = "Top Hiring Cities by Keywords:",
                  width = 5,
                  solidHeader = TRUE,
                  plotOutput("topCities")
                )
              ),
              
              fluidRow(
                box(
                  title = "Job Posting Time:", 
                  width = 7, 
                  solidHeader = TRUE, status = "primary",
                  plotOutput("postTime")
                ),
                
                box(
                  title = "Latest Posts:", 
                  width = 5, 
                  solidHeader = TRUE,
                  htmlOutput("jobLinks")
                )
              )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "MONSTER IT Jobs Statistic",
                  titleWidth = 300
  ),
  dashboardSidebar(
    width = 300,
    # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
    #                   label = "Enter Job Category..."),
    textInput("searchText", label = "Enter Job Title,...:"),
  
    htmlOutput("note"),
    tags$style(type="text/css", "#note { height: 15px; width: 90%; text-align:right; font-size: 13px;margin:5; padding:0px;}"),
    
    actionButton("searchButton", label = "Search"),
    
    selectInput("selectState", label = "Select State:", choices = Location_state_sum$state, 
                selected = Location_state_sum$state[1]),
    
    sliderInput("duration", "Select Timeframe for Job Display:",
                min = -15, max = 0, value = c(-7, 0)),
    
    box(
      title = "IT Job Titles:", 
      width = 250, height = 400, solidHeader = TRUE,
      htmlOutput("title")
    )
  ),
  body
)



server = function(input, output) {
  
  selectedJobs <- function(Jobs){
    jobSelectedList = character()
    for (i in 1:length(Jobs)) {
      jobSelectedList = c(jobSelectedList, Title_df$job_cleansed[grepl(Jobs[i],Title_df$job_cleansed)])
    }
    jobSelectedList = unique(jobSelectedList)
    jobSelectedList
  }
  
  searchedJob <- reactive({
    input$searchText
  })
  
  shownState <- reactive({
    input$selectState
  })
  
  values <- reactiveValues(default = 0)
  
  getKeys <- function(text){
    
    if (text==""){
      keys = ""
    } else {
      keys <- trimws(text)
      keys <- unlist(strsplit(keys, ","))
      keys <- trimws(keys)
      keys <- tolower(keys)
    }
    keys
  }
  
  action <- eventReactive(input$searchButton, {
    keys <- getKeys(input$searchText)
    keys
  })
  
  observeEvent(input$searchButton,{
    values$default <- input$searchText
  })
  
  output$note <- renderUI({
    HTML('<font color="grey"> <i>Multiple titles are separated by comma (",")</i>')
  })
  
  output$title <- renderUI({
    
    jobText <- getKeys(searchedJob())
    
    jobList <- selectedJobs(jobText)
    
    Title_job <- Title_df %>% filter(job_cleansed %in% jobList)
    
    
    tags$div(style="width:290px;height:350px;overflow:scroll;overflow-y:scroll;overflow-x:hidden;",
             HTML(paste('<font color="black">',Title_job$job, sep = '<br/>'))
    )
  })
  
  output$heatmap <- renderGvis({
    
    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }
    
    jobList <- selectedJobs(jobT)
    
    job_map_state <- Location_df %>% na.omit() %>% filter(job_cleansed %in% jobList) %>%
      group_by(state)  %>% summarise(jobs_count = sum(count))
    
    gvisGeoChart(job_map_state, 
                 locationvar = "state", 
                 colorvar = "jobs_count",
                 options=list(region="US", 
                              displayMode="regions", 
                              resolution="provinces",
                              width=600, height=400))
    
  })
  
  get_State_br <- function(State){
    state_fn <- tolower(as.character(State))
    if(state_fn %in% states$state){
      states[states$state==state_fn,]$Abbreviation
    } else {
      FALSE
    }
  }
  
  output$statemap <- renderPlot({
    
    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }
    
    region = shownState()
    stateBR <- get_State_br(region)
    
    jobList <- selectedJobs(jobT)
  
    job_map_city <- Location_df %>% na.omit() %>% filter(job_cleansed %in% jobList) %>%
        group_by(state_br, city)  %>% 
        summarise(jobs_count = sum(count)) %>%
        arrange(desc(jobs_count)) 
      
      LA <- map_data("state", region)
      
      if (length(job_map_city$state_br) > 10){
        n = 10
      } else {
        n = length(job_map_city$state_br)
      }
      
      cityPosts <- data.frame(state_br=job_map_city[job_map_city$state_br==stateBR,]$state_br[1:n], 
                               city=job_map_city[job_map_city$state_br==stateBR,]$city[1:n],
                               post=job_map_city[job_map_city$state_br==stateBR,]$jobs_count[1:n])

      cityPosts <- left_join(cityPosts,geoDat, by = c("city","state_br"))
      cityPosts <- drop_na(cityPosts)
      
      # salesCalls
      # #         lon      lat     State        City Calls
      # # 1 -91.14032 30.45828 louisiana Baton Rouge    10
      # # 2 -90.07153 29.95107 louisiana New Orleans     5
      # # 3 -93.75018 32.52515 louisiana  Shreveport     8
      # # 4 -92.01984 30.22409 louisiana   Lafayette    13
      # # 5 -90.06563 30.35825 louisiana  Mandeville     2
      
      ggplot(LA, aes(x=long, y=lat)) +
         geom_polygon(fill = "light blue") +
         coord_map() +
         geom_point(data=cityPosts, aes(x=lon, y=lat, size=post), color="orange") + 
         geom_text_repel(data =cityPosts, aes(label = city, x = lon, y = lat), size = 3.5, color = "blue") +
        scale_size(labels = comma)
    })
  
  sliderValues <- reactive({
    
       input$duration
    
  })
  
  output$postTime <- renderPlot({
    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }
    
    jobList <- selectedJobs(jobT)

    selectJobs <- Title_df %>% na.omit() %>% filter(job_cleansed %in% jobList)
    
    topJob <- selectJobs$job
    
    Time_post <- Time_df %>% na.omit() %>% filter(job %in% topJob) %>%
      group_by(time) %>%
      summarise(post_count = n())
    
    timeTo <- max(Time_post$time) - ddays(-input$duration[2])
    timeFrom <- timeTo - ddays(input$duration[2]-input$duration[1]) #duration
  
    
    plot1 <- ggplot(data = Time_post[Time_post$time>timeFrom & Time_post$time<timeTo,]) + 
      geom_line(aes(y = post_count, x = time), color = "blue") + 
      theme_economist() 
    
    plot1
  })
  
  output$jobLinks <- renderUI({
    
    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }
    
    jobList <- selectedJobs(jobT)
    
    #Filtering based on the Job Category entered by User
    selectJobs <- Title_df %>% na.omit() %>% filter(job_cleansed %in% jobList)
    
    topJob <- selectJobs$job
    
    Time_post <- Time_df %>% na.omit() %>% filter(job %in% topJob) %>%
      arrange(desc(time))
    
    timeTo <- max(Time_post$time) - ddays(-input$duration[2])
    timeFrom <- timeTo - ddays(input$duration[2]-input$duration[1]) #duration
    
    Time_job <- Time_post %>% filter(time>timeFrom & time<timeTo)
    
    if (length(Time_job$job) > 200){
      l = 200
    } else {
      l = length(Time_job$job)
    }
    
    ###
    Title_job <- Time_job$title[1:l]
    Title_link <- Time_job$link[1:l]
    Job_list <- character()
    
    for (i in 1:length(Title_job)){
      Job_list <- c(Job_list,
                    paste('<a href=',Title_link[i],'target="_blank"','<font color="black">',Title_job[i],'</a>','<br/>'))
    }
    
    tags$div(style="width:400px;height:400px;overflow:scroll;overflow-y:scroll;overflow-x:hidden;",
             HTML(paste(Job_list))
    )
  })
  
  output$topStates <- renderPlot({

    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }

  job_map_state <- data_frame()
  
  for (i in 1:length(jobT)){
    jobList <- selectedJobs(jobT[i])
    state_key <- Location_df %>% na.omit() %>% filter(job_cleansed %in% jobList) %>%
      mutate(key = jobT[i])
    job_map_state <- bind_rows(job_map_state,state_key)
  }
  
  state_key <- job_map_state %>% na.omit() %>%
    group_by(state,key)  %>% summarise(jobs_count = sum(count)) %>%
    arrange(desc(jobs_count)) #%>%
  #top_n(10)
  
  state_sum = job_map_state %>% na.omit() %>%
    group_by(state)  %>% summarise(jobs_count = sum(count)) %>%
    arrange(desc(jobs_count)) %>% top_n(10, jobs_count)
  
  state_key = state_key %>% filter(state %in% state_sum$state)
  
  if (length(jobT)==1 & jobT[1]==""){
    g <- ggplot(state_key, aes(reorder(state, -jobs_count), jobs_count)) +
      geom_bar(stat = "identity", position = "stack", color = "#FF6666", fill="palegreen3") 
  } else {
    g <- ggplot(state_key, aes(reorder(state, -jobs_count), jobs_count, fill = key)) +
      geom_bar(stat = "identity", position = "stack") 
  }
  g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Hiring States", y = "Jobs Count") + scale_y_continuous(labels=comma)
  g

  })
  
  output$topCities <- renderPlot({
    if(values$default == 0){
      jobT = ""
    }
    else{
      jobT = action()
    }
    
    region = shownState()
    stateBR <- get_State_br(region)
    
    job_map_city <- data_frame()
    
    for (i in 1:length(jobT)){
      jobList <- selectedJobs(jobT[i])
      city_key <- Location_df %>% na.omit() %>% filter(job_cleansed %in% jobList & state_br == stateBR) %>%
        mutate(key = jobT[i])
      job_map_city <- bind_rows(job_map_city,city_key)
    }
    
    city_key <- job_map_city %>% na.omit() %>%
      group_by(city,key)  %>% summarise(jobs_count = sum(count)) %>%
      arrange(desc(jobs_count)) #%>%
    #top_n(10)
    
    city_sum = job_map_city %>% na.omit() %>%
      group_by(city)  %>% summarise(jobs_count = sum(count)) %>%
      arrange(desc(jobs_count)) %>% top_n(10, jobs_count)
    
    city_key = city_key %>% filter(city %in% city_sum$city)
    
    if (length(jobT)==1 & jobT[1]==""){
      g <- ggplot(city_key, aes(reorder(city, -jobs_count), jobs_count)) +
        geom_bar(stat = "identity", position = "stack", color = "#FF6666", fill="turquoise3") 
    } else {
      g <- ggplot(city_key, aes(reorder(city, -jobs_count), jobs_count, fill = key)) +
        geom_bar(stat = "identity", position = "stack") 
    }
    g <- g + theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = "Hiring Cities", y = "Jobs Count") + scale_y_continuous(labels=comma) 
    g
  })

}

# Preview the UI in the console
shinyApp(ui = ui, server = server)



