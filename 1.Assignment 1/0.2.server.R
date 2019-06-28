#2.Server-----------------------------------------------------------------------------

server <- function(input, output) {
  
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- m
    if (input$Sex != "All") {
      data <- data[data$Sex == input$Sex,]
    }
    if (input$Sport != "All") {
      data <- data[data$Sport == input$Sport,]
    }
    if (input$Season != "All") {
      data <- data[data$Season == input$Season,]
    }
    data
  }))
  
  
  output$Number <- renderPrint({
    dataset <- data.frame(Age = m$Age,Height = m$Height,Weight = m$Weight)
    summary(dataset)
    
  })
  
  output$Plot <- renderPrint({
    dataset <- data.frame(Age = m$Age,Height = m$Height,Weight = m$Weight)
    library(naniar)
    library(dplyr)
    dataset %>% miss_var_summary()
  })
  
  
  library(ggplot2)
  output$Amsterdam <- renderPlot({
    data_regions <- m %>% filter(!is.na(region))
    amsterdam <- data_regions %>% 
      filter(Games == "1928 Summer") %>%
      group_by(region) %>%
      summarize(Amsterdam = length(unique(ID)))
    
    library(maps)
    world <- map_data("world")
    mapdat <- tibble(region=unique(world$region))
    mapdat <- mapdat %>% 
      left_join(amsterdam, by="region") 
    mapdat$Amsterdam[is.na(mapdat$Amsterdam)] <- 0
    world <- left_join(world, mapdat, by="region")
    
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = Amsterdam)) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_colourbar(title="Athletes")) +
      scale_fill_gradient(low="white",high="red")
  })
  
  
  output$Melbourne <- renderPlot({
    data_regions <- m %>% filter(!is.na(region))
    melbourne <- data_regions %>% 
      filter(Games == "1956 Summer") %>%
      group_by(region) %>%
      summarize(Melbourne = length(unique(ID)))
    
    library(maps)
    world <- map_data("world")
    mapdat <- tibble(region=unique(world$region))
    mapdat <- mapdat %>% 
      left_join(melbourne, by="region") 
    mapdat$Melbourne[is.na(mapdat$Melbourne)] <- 0
    world <- left_join(world, mapdat, by="region")
    
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = Melbourne)) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_colourbar(title="Athletes")) +
      scale_fill_gradient2(low="white",high = "red")
  })
  
  
  output$Munich <- renderPlot({
    data_regions <- m %>% filter(!is.na(region))
    munich <- data_regions %>% 
      filter(Games == "1972 Summer") %>%
      group_by(region) %>%
      summarize(Munich = length(unique(ID)))
    
    library(maps)
    world <- map_data("world")
    mapdat <- tibble(region=unique(world$region))
    mapdat <- mapdat %>% 
      left_join(munich, by="region") 
    mapdat$Munich[is.na(mapdat$Munich)] <- 0
    world <- left_join(world, mapdat, by="region")
    
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = Munich)) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_colourbar(title="Athletes")) +
      scale_fill_gradient2(low = "white", high = "red")
  })
  
  output$Rio <- renderPlot({
    data_regions <- m %>% filter(!is.na(region))
    rio <- data_regions %>% 
      filter(Games == "2016 Summer") %>%
      group_by(region) %>%
      summarize(Rio = length(unique(ID)))
    
    library(maps)
    world <- map_data("world")
    mapdat <- tibble(region=unique(world$region))
    mapdat <- mapdat %>% 
      left_join(rio, by="region") 
    mapdat$Rio[is.na(mapdat$Rio)] <- 0
    world <- left_join(world, mapdat, by="region")
    
    ggplot(world, aes(x = long, y = lat, group = group)) +
      geom_polygon(aes(fill = Rio)) +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.background = element_rect(fill = "navy"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill=guide_colourbar(title="Athletes")) +
      scale_fill_gradient2(low="white",high = "red")
  })
  
  
  
  
  library(ggplot2)
  
  output$weight <- renderPlot({
    m %>% ggplot(aes(x=as.factor(Year), y=Weight, fill=Sex)) +
      geom_boxplot(alpha=0.75) +
      xlab("Olympiad Year") + ylab("Weight (kg)") +
      scale_fill_manual(values=c("blue","red")) + ggtitle("Athlete weight over time")
  })
  
  
  output$Height <- renderPlot({   
    m %>% ggplot(aes(x=as.factor(Year), y=Height, fill=Sex)) +
      geom_boxplot(alpha=0.75) +
      xlab("Olympiad Year") + ylab("Height (cm)") +
      scale_fill_manual(values=c("blue","red"))+ggtitle("Athlete height over time")
  })
  
  
  
  output$correlation <- renderPlot({   
    l<-m %>% select(Height,Weight,Year)
    l2<-na.omit(l)
    ggplot(l2,aes(x= Height,y=Weight))+geom_point()+ stat_smooth(method=lm,se =F) + ggtitle("The correlation between height and weight over time")
  })
  
  
  output$Ratios <- renderPlot({
    l<-m %>% select(Height,Weight,Year)
    l2<-na.omit(l)
    l2$Ratio <- l2$Height/l2$Weight
    l1<- l2 %>% group_by(Year) %>% summarise(Mean = mean(Ratio))
    ggplot(l1,aes(Year,l1$Mean)) + geom_line(color="green")+xlab("Year")+ylab("Height /  Weight")+ ggtitle("Athlete height/weigth over time")
  })
  
  
  
  output$wordcloud <- renderPlot({
    library(tm)
    library(SnowballC)
    library(wordcloud)
    library(RColorBrewer) 
    text <- readLines(wordcloud_text) 
    docs <- Corpus(VectorSource(text))
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    docs <- tm_map(docs, toSpace, "/")
    docs <- tm_map(docs, toSpace, "@")
    docs <- tm_map(docs, toSpace, "\\|")
    docs <- tm_map(docs, content_transformer(tolower))
    docs <- tm_map(docs, removeNumbers)
    docs <- tm_map(docs, removePunctuation)
    docs <- tm_map(docs, stripWhitespace)
    dtm <- TermDocumentMatrix(docs)
    v <- sort(rowSums(as.matrix(dtm)),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    set.seed(1234)
    wordcloud(words = d$word, scale=c(4,0.5),freq = d$freq, min.freq = 2,
              max.words=50, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
    
  })
  
  
  output$summer <- renderPlot({
    library(ggplot2)
    m %>%
      group_by(Season, Sex) %>% 
      summarise(
        mean = mean(Age, na.rm = TRUE)
      )
    summer_medalist <-
      m %>% 
      group_by(Year, Season, Medal) %>% 
      filter(Season == "Summer") %>% 
      summarise(
        mean = mean(Age, na.rm = TRUE)
      )
    ggplot(data = summer_medalist, 
           mapping = aes(x = Year, y= mean, group = Medal, color = Medal)) +
      geom_point(size=2) +
      geom_line() 
    
  })
  
  output$winter <- renderPlot({
    m %>%
      group_by(Season, Sex) %>% 
      summarise(
        mean = mean(Age, na.rm = TRUE)
      )
    winter_medalist <-
      m %>% 
      group_by(Year, Season, Medal) %>% 
      filter(Season == "Winter") %>% 
      summarise(
        mean = mean(Age, na.rm = TRUE)
      )
    ggplot(data = winter_medalist, 
           mapping = aes(x = Year, y= mean, group = Medal, color = Medal)) +
      geom_point(size=2) +
      geom_line()
    
  })
  
}







