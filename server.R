library(shiny)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)

allDailyHomeB <- read.csv('data/homeB-all/allDaily.csv')

shinyServer(function(input, output){
  
## genral functions  
  
  returnOne <- function(data){return(1)}
  
  #translate selectedDate format (2012-07-01) to the format of the data files (2012-Jul-1)
  translateDate <- function(userSelectedDate){
    
    dateString <- userSelectedDate %>%
      format('%Y-%b-%d') %>% #translates numeric month to abreviated month
      strsplit('-') %>% #take apart the date 
      unlist()
    
    #reassembles the date and changes 
    dateString <-  paste(dateString[1],'-',dateString[2],'-',as.numeric(unlist(strsplit(dateString,'-'))[3]), sep = '')
    
    return(dateString)
    
  }

  
  #return Watt averages and temprature for the requested timeInterval
  granulate <- function(largeData,timeInterval,dataDescription,timeSelect,timeShift,logicalTemp,tempFileLocation,logicalCircuit = FALSE){
     
     #creates a progress bar
     withProgress(message = paste('Formatting', dataDescription ,'Data'), value = 0, {
       
       if(logicalTemp == TRUE){
         
       #mergeTempData(largeData)
       data <- readData(
          selectedDate = input$selectedDate,
          dateRange = 0:1,
          fileLocation = tempFileLocation,
          columnNumbers = c(1,3),
          columnNames = c('Time','outsideTemp'),
          dataDescription = 'Temprature data')
     
        largeData$outsideTemp <- data[match(as.character(largeData$fiveMinGroup), as.character(data$Time)), 'outsideTemp']
       
        }
       
       #create a column which indicates which time group each row belongs to   
       largeData$group <- as.factor(floor(as.numeric(largeData$Minute) / as.numeric(timeInterval))) #how to pipe this?
     
         
       #create dataframe for the refined smallerData  
       smallerData <- as.data.frame(levels(largeData$group))
       #id rows as the group levels for easy storage
       row.names(smallerData) <- levels(largeData$group)
       
       countLoops <- 0
       for(n in levels(largeData$group)){
         #creat columns of smallerData which store the meanWatt and associated Time 
         holdThis <- filter(largeData, group == n)
         smallerData[n,'meanWatt'] <- mean(holdThis$Watt) 
         smallerData[n, 'Time'] <- (holdThis[do.call(timeSelect, list(holdThis)),'Time'] - timeShift)
         if(logicalTemp == TRUE){
         smallerData[n, 'outsideTemp'] <- holdThis[1,'outsideTemp']
         }
         if(logicalCircuit == TRUE){
         smallerData[n, 'Circuit'] <- holdThis[1,'Circuit']
         }
         #incrament progress bar by appropreate fraction 
         countLoops <- countLoops + 1
         incProgress(amount = 1/nrow(smallerData), detail = paste(countLoops, 'of', nrow(smallerData)))
       }
     
     })
     
     #convert Time to readable form 
     smallerData[,'Time'] <- as.POSIXct(smallerData[,'Time'], origin = '1970-01-01')
    
    return(smallerData)
    
  } 
  
  #DEPENDENCIES: translateDate
  readData <- function(selectedDate, dateRange, fileLocation, columnNumbers, columnNames, dataDescription){
   
   data <- as.data.frame(NULL)
    
    withProgress(message = paste( 'Loading', dataDescription), value = 0, {
      
      countLoops <- 0
      for(n in dateRange){
      holdThis <- selectedDate %>% 
       as.Date() %>%
       +n %>%
       translateDate() %>%
       paste('data/', fileLocation , . , '.csv', sep = '') %>%
       read.csv(header=FALSE)
      
      data <- rbind(data, holdThis)
      
      countLoops <- countLoops + 1
      incProgress(amount = 1/length(dateRange), detail = paste(countLoops, 'of', length(dateRange)))
      }
    })
    
    withProgress(message = paste('Massaging', dataDescription) , detail = NULL, value = NULL, {
      
      data <- data[,columnNumbers]
      names(data) <- columnNames 
      
      data$utcTime <- data$Time
      data$Time <- as.POSIXct(data$Time, origin = '1970-01-01')
      
      incProgress(detail = 'Timing')
      data$Minute <- format(data$Time, '%H%M')
      
    })
    
    return(data)
   
   }

## Home B code
  
    output$dateName <- renderText({ format(as.Date(input$selectedDate), '%A %B %e')  })
    
    graphData <- reactive({
      
       data <- readData(selectedDate = input$selectedDate,
         dateRange = c(0:1),
         fileLocation = 'homeB-all/homeB-power/',
         columnNumbers = 1:2,
         columnNames = c('Time','Watts'),
         dataDescription = 'daily data')
       
       graphData <- granulate(largeData = data,
               timeInterval = input$timeInterval,
               dataDescription = 'Daily',
               logicalTemp = FALSE,
               timeSelect = 'returnOne',
               timeShift = 0)
        })
    
    graphDataTemp <- reactive({
          
          data <- readData(selectedDate = input$selectedDate,
         dateRange = c(0:1),
         fileLocation = 'homeB-all/homeB-power/',
         columnNumbers = 1:2,
         columnNames = c('Time','Watts'),
         dataDescription = 'daily data')
         
      #creates fiveMinGroup catagorie which aligns with and allows for the merging of temp data
       data$fiveMinGroup <- cut(data$Time, breaks = (24*60/5))
       
       graphData <- granulate(largeData = data,
               timeInterval = input$timeInterval,
               dataDescription = 'Daily',
               logicalTemp = TRUE,
               tempFileLocation = 'homeB-all/homeB-environmental/',
               timeSelect = 'returnOne',
               timeShift = 0)
          
        })
        
    graphDataWeek <- reactive({
          
          #read in weekly data
  weekData <- readData(selectedDate = input$selectedDate,
         dateRange = c(-8:0),#c(-7:1),
         fileLocation = 'homeB-all/homeB-power/',
         columnNumbers = 1:2,
         columnNames = c('Time','Watts'),
         dataDescription = 'weekly data')
  
  #granulate it to the appropriate size  
  graphDataWeek <- granulate(largeData = weekData,
              timeInterval = 100,
              dataDescription = 'Weekly',
              logicalTemp = FALSE,
              timeSelect = 'nrow',
              timeShift = 1799)
          
          })
    
    
    #generates main graphic for home B
    output$masterPlot <- renderPlot({ 
    
    #creat a checkBox which guides what plot is produced based on if-then logic
    checkBox <- c(length(grep('1', input$suplamentalInfo)), length(grep('2', input$suplamentalInfo)))
     
    #update the y-axis lable to match the selected unit
     if (input$unit == .12/1000){yLabel <- '$ per Hour'}
     else if (input$unit == .7/1000){yLabel <- 'kg CO2 per Hour'}
     else {yLabel <- 'kilowatts'}
    
    #set the width of the bar graphs to match the selected timeInterval from the user
    setWidth <- 1
    
#if 'Prior Week Average' box is checked 
if (checkBox[2] == 1){
  
  #if 'Temprature' box is checked
	if (checkBox[1] == 1){
	  
	ggData <- graphDataTemp() 
	  
	setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
	
	ggplot(data = ggData, aes(x = Time, y = meanWatt*as.numeric(input$unit)))+ 
          geom_bar(data = graphDataWeek(), aes(x = Time, y = meanWatt*as.numeric(input$unit)), stat = 'identity', width = 3600, fill = 'grey70', show.legend = TRUE) +
	        geom_bar(stat = 'identity', fill = 'royalblue4', aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), alpha = outsideTemp, width = setWidth)) +
	        theme_minimal() +
          labs(y = yLabel) +
	        labs(x = 'Time')
		  
	}
  
  #if 'Temprature' box is not checked
	else {
	  
	  ggData <- graphData() 
	  
  	setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
	
	ggplot(data = ggData, aes(x = Time, y = meanWatt*as.numeric(input$unit))) +
         geom_bar(data = graphDataWeek(),  aes(x = Time, y = meanWatt*as.numeric(input$unit)), stat = 'identity', width = 3600, fill = 'grey70', show.legend = TRUE)+
         geom_bar(stat = 'identity', fill = 'royalblue4', aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit)), alpha = .5, width = setWidth) +
         theme_minimal() +
         labs(y = yLabel) +
	       labs(x = 'Time') 
	
	}  
}
    
#if 'Prior Week Average' box is not checked    
else {
      
    #if 'Temprature' box is checked
    if (checkBox[1] == 1){
      
      ggData <- graphDataTemp() 
	  
  	  setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
	  
	  ggplot(data = ggData, aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), alpha = outsideTemp, width = setWidth)) +
           geom_bar(stat = 'identity', fill = 'royalblue4') +
           theme_minimal() +
           labs(y = yLabel) +
	         labs(x = 'Time')
		 
      }
  
    #if 'Temprature' box is not checked
	  else {
	    
	    ggData <- graphData() 
	  
  	  setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
	  
	  ggplot(data = ggData, aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), width = setWidth)) +
        geom_bar(stat = "identity", position = 'identity', fill = 'royalblue4') +
        theme_minimal() +
        labs(y = yLabel) +
	      labs(x = 'Time')
	  
	  }

}

    
    })
    
    #Monthly tab plot  
    output$monthlyHomeBPlot <- renderPlot({
      
      output$testing <- renderText({ c(format(as.Date(input$dateRange[1]),'%A %B %e'), ' to ', format(as.Date(input$dateRange[2]),'%A %B %e'))  }) 
      
      allDailyHomeB <- filter(allDailyHomeB, input$dateRange[1] <= as.Date(TimeAsDate) & as.Date(TimeAsDate) <= input$dateRange[2])
      
      checkBox <- c(length(grep('1', input$suplamentalInfo2)), length(grep('2', input$suplamentalInfo2)))
      
      if (input$unit2 == .12/24000){yLabel <- 'Cost in $'}
      else if (input$unit2 == .7/24000){yLabel <- 'kg CO2'}
      else {yLabel <- 'kilowatts'}
      
      allDailyHomeB$Watts <- as.numeric(allDailyHomeB$Watts)*as.numeric(input$unit2)
      
      if(checkBox[1] == 1 & checkBox[2] == 1){
        ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = weekDay, alpha = outsideTemp)) +
        geom_bar(stat = 'identity') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = yLabel)
      }
      else if(checkBox[1] == 1 & checkBox[2] == 0){
        ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = NULL, alpha = outsideTemp)) +
        geom_bar(stat = 'identity', fill = 'royalblue4') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = yLabel)
      }
      else if(checkBox[1] == 0 & checkBox[2] == 1){
        ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = weekDay, alpha = NULL)) +
        geom_bar(stat = 'identity') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = yLabel)
      }
      else{
        ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = NULL, alpha = NULL)) +
        geom_bar(stat = 'identity', fill = 'royalblue4') +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
        labs(y = yLabel)
        }
  
      
    })
    
     fileName <- 'server.R'
     output$server <- renderText(readChar(fileName, file.info(fileName)$size))
     fileName2 <- 'ui.R'
     output$ui <- renderText(readChar(fileName2, file.info(fileName2)$size))

})