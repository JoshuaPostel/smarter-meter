library(shiny)
library(magrittr)
library(dplyr)
library(reshape2)
library(ggplot2)

## CODE OVERVIEW ##
# (1) data management functions 
#     -translateDate
#     -readData
#     -granulate
# (2) reactive, data gathering functions
# (3) plotting functions
#     -masterPlot
#     -historyPlot
# (4) miscellaneous processes

shinyServer(function(input, output){

########## data management functions ##########

#translate selectedDate format (2012-07-01) to the format of the data files (2012-Jul-1)
translateDate <- function(userSelectedDate){
  
  dateString <- userSelectedDate %>%
    format('%Y-%b-%d') %>% #translates numeric month to abbreviated month
    strsplit('-') %>% #take apart the date 
    unlist()
  
  #reassembles the date and changes 
  dateString <-  paste(dateString[1],'-',dateString[2],'-',as.numeric(unlist(strsplit(dateString,'-'))[3]), sep = '')
  
  return(dateString)
}

#use slectedDate to gather, combine, clean, and return 
#dependencies: translateDate
readData <- function(selectedDate,   #day selected by user
                     dateRange,      #days surrounding selectedDate required (for example: previous 7 days for the weeks average)
                     fileLocation,   #selects the house and subgroup of data 
                     columnNumbers,  #desired data
                     columnNames,    #corresponding names
                     dataDescription #description for loading bar
                     ){
 
 data <- as.data.frame(NULL)
  
 #progress bar for file loading 
  withProgress(message = paste( 'Loading', dataDescription), value = 0, {
    
    #gather the desired data
    countLoops <- 0
    for(n in dateRange){
      holdThis <- selectedDate %>%
       as.Date() %>% 
       +n %>% #for each day within the desired range of the selectedDate
       translateDate() %>% #translate date to the files' naming convention
       paste('data/', fileLocation , . , '.csv', sep = '') %>% #construct the file name
       read.csv(header=FALSE) #read in the file
      
      data <- rbind(data, holdThis) #bind the current file to the previous files that have been read in  
      
      #increment progress bar by one
      countLoops <- countLoops + 1
      incProgress(amount = 1/length(dateRange), detail = paste(countLoops, 'of', length(dateRange)))
    }
    
  })
  
  #progress bar for data cleaning
  withProgress(message = paste('Massaging', dataDescription) , detail = NULL, value = NULL, {
    
    #select and name desired columns
    data <- data[,columnNumbers]
    names(data) <- columnNames 
    
    #UTC data to useful format for graphing
    data$utcTime <- data$Time
    data$Time <- as.POSIXct(data$Time, origin = '1970-01-01')
    
    #create a column for easy grouping based on time intervals
    incProgress(detail = 'Timing')
    data$Minute <- format(data$Time, '%H%M')
    
  })
  
  return(data) #cleaned and organized data ready for further manipulation
}

#return Watt averages and temperature for the requested timeInterval (so-called "granulated data")
#dependencies: readData
granulate <- function(largeData,                #input formatted data from readData 
                      timeInterval,             #time granularity requested by user (data by the hour, minute, etc.)  
                      dataDescription,          #description for loading bar
                      timeSelect,               #function managing how time is assigned to the granulated data
                      timeShift,                #integer aligning data from different days (if necessary)
                      logicalTemp,              #is temperature data requested?
                      tempFileLocation,         #file location of temperature data
                      logicalCircuit = FALSE,   #option for loading in disaggregated data (House A) 
                      logicalGeneration = FALSE #option for loading in generation data (House C) 
                      ){
   
   #creates a progress bar
   withProgress(message = paste('Formatting', dataDescription ,'Data'), value = 0, {
     
     #if temperature data is requested
     if(logicalTemp == TRUE){
      #read in temperature data 
      data <- readData(
        selectedDate = input$selectedDate,
        dateRange = 0:1,
        fileLocation = tempFileLocation,
        columnNumbers = c(1,3),
        columnNames = c('Time','outsideTemp'),
        dataDescription = 'Temperature data')
      
      #store temperatures with their matching time 
      largeData$outsideTemp <- data[match(as.character(largeData$fiveMinGroup), as.character(data$Time)), 'outsideTemp']
      }
     
     #create a column indicating which timeGroup each row belongs to (dividing by timeInterval granulates the data)
     largeData$timeGroup <- as.factor(floor(as.numeric(largeData$Minute) / as.numeric(timeInterval)))
   
     #create dataframe to store the granulated data in  
     granulatedData <- as.data.frame(levels(largeData$timeGroup))
     #id rows as the group levels for easy storage
     row.names(granulatedData) <- levels(largeData$timeGroup)
     
     countLoops <- 0
     for(n in levels(largeData$timeGroup)){
       
       #gather subsetted data by timeGroup
       holdThis <- filter(largeData, timeGroup == n)
       #store the timeGroup's mean temperature
       granulatedData[n,'meanWatt'] <- mean(holdThis$Watt) 
       #associate the correct hourly time with the timeGroup
       #adjust for time data from different days by selecting the last entry and shifting appropriately
       granulatedData[n, 'Time'] <- (holdThis[do.call(timeSelect, list(holdThis)),'Time'] - timeShift)
       
       #should temperature info be included? 
       if(logicalTemp == TRUE)
          granulatedData[n, 'outsideTemp'] <- holdThis[1,'outsideTemp']
       
       #for pulling extra variables from house A or C (not used presently)
       if(logicalCircuit == TRUE)
          granulatedData[n, 'Circuit'] <- holdThis[1,'Circuit']
       
       if(logicalGeneration == TRUE)
          granulatedData[n, 'Circuit'] <- holdThis[1,'Circuit']
       
       #increment progress bar by the reciprocal of timeGroup 
       countLoops <- countLoops + 1
       incProgress(amount = 1/nrow(granulatedData), detail = paste(countLoops, 'of', nrow(granulatedData)))
     }
   })
   
   #convert Time to readable form 
   granulatedData[,'Time'] <- as.POSIXct(granulatedData[,'Time'], origin = '1970-01-01')
  
  return(granulatedData)
} 


########## reactive, data gathering functions ##########
#these functions are defined outside of graphing functions to save on redundant computation time
    
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
         
  #creates fiveMinGroup category which aligns with and allows for the merging of temp data
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
         dateRange = c(-8:0),
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
    
    
########## plotting functions ##########
        
#generates main graphic for daily tab
output$masterPlot <- renderPlot({ 
    
  #create a checkBox which guides what plot is produced based on logical conditions below
  #checkBox[1] and checkBox[2] correspond to Temperature and Prior Week Average respectively (0 = not checked, 1 = checked)
  checkBox <- c(length(grep('1', input$suplamentalInfo)), length(grep('2', input$suplamentalInfo)))
   
  #update the y-axis label to match the selected unit
  if (input$unit == .12/1000){yLabel <- '$ per Hour'}
  else if (input$unit == .7/1000){yLabel <- 'kg CO2 per Hour'}
  else {yLabel <- 'kilowatts'}
  
  #readable text of user selected date
  output$dateName <- renderText({ format(as.Date(input$selectedDate), '%A %B %e')})
    
###logical conditions to determine which graph to plot###
  
  #if 'Temperature' box is checked and 'Prior Week Average' box is checked 
	if (checkBox[1] == 1 & checkBox[2] == 1){
	  #load in the data to be graphed
  	ggData <- graphDataTemp()
  	#set the width of the bar graphs to match the selected timeInterval from the user
  	setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
  	
  	#graphic creation
  	ggplot(data = ggData, aes(x = Time, y = meanWatt*as.numeric(input$unit)))+ 
            geom_bar(data = graphDataWeek(), aes(x = Time, y = meanWatt*as.numeric(input$unit)), stat = 'identity', width = 3600, fill = 'grey70', show.legend = TRUE) +
  	        geom_bar(stat = 'identity', fill = 'royalblue4', aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), alpha = outsideTemp, width = setWidth)) +
  	        theme_minimal() +
            labs(y = yLabel) +
  	        labs(x = 'Time')
	}
  #if 'Temperature' box is not checked and 'Prior Week Average' box is checked 
	else if (checkBox[1] == 0 & checkBox[2] == 1){
	  ggData <- graphData() 
  	setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
	
  	ggplot(data = ggData, aes(x = Time, y = meanWatt*as.numeric(input$unit))) +
           geom_bar(data = graphDataWeek(),  aes(x = Time, y = meanWatt*as.numeric(input$unit)), stat = 'identity', width = 3600, fill = 'grey70', show.legend = TRUE)+
           geom_bar(stat = 'identity', fill = 'royalblue4', aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit)), alpha = .5, width = setWidth) +
           theme_minimal() +
           labs(y = yLabel) +
  	       labs(x = 'Time')
	 }  
  #if 'Temperature' box is checked and 'Prior Week Average' box is not checked
  else if (checkBox[1] == 1 & checkBox[2] == 0){
    ggData <- graphDataTemp() 
	  setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
  
	  ggplot(data = ggData, aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), alpha = outsideTemp, width = setWidth)) +
           geom_bar(stat = 'identity', fill = 'royalblue4') +
           theme_minimal() +
           labs(y = yLabel) +
	         labs(x = 'Time')
  }
  #if 'Temperature' box is not checked and 'Prior Week Average' box is not checked
  else {
    ggData <- graphData() 
	  setWidth <- (as.numeric(ggData$Time[2]) - as.numeric(ggData$Time[1]))
  
	  ggplot(data = ggData, aes(x = Time + .5*setWidth, y = meanWatt*as.numeric(input$unit), width = setWidth)) +
        geom_bar(stat = "identity", position = 'identity', fill = 'royalblue4') +
        theme_minimal() +
        labs(y = yLabel) +
	      labs(x = 'Time')
  }
})
    
#generates main graphic for history tab  
output$historyPlot <- renderPlot({
      
  #trim out and times exceed the expected range (some occur due to data inconsistencies) 
  allDailyHomeB <- filter(allDailyHomeB, input$dateRange[1] <= as.Date(TimeAsDate) & as.Date(TimeAsDate) <= input$dateRange[2])
  
  #create a checkBox which guides what plot is produced based on logical conditions below
  #checkBox[1] and checkBox[2] correspond to Temperature and Week Day respectively
  checkBox <- c(length(grep('1', input$suplamentalInfo2)), length(grep('2', input$suplamentalInfo2)))
  
  #update the y-axis label to match the selected unit
  if (input$unit2 == .12/24000){yLabel <- 'Cost in $'}
  else if (input$unit2 == .7/24000){yLabel <- 'kg CO2'}
  else {yLabel <- 'kilowatts'}
      
  #readable text of user selected date
  output$dateRangeText <- renderText({ c(format(as.Date(input$dateRange[1]),'%A %B %e'), ' to ', format(as.Date(input$dateRange[2]),'%A %B %e'))}) 
  
  #scale the variable Watts to reflect the selected unit    
  allDailyHomeB$Watts <- as.numeric(allDailyHomeB$Watts)*as.numeric(input$unit2)
      
  #if 'Temperature' box is checked and 'Week Day' box is checked
  if (checkBox[1] == 1 & checkBox[2] == 1){
     ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = weekDay, alpha = outsideTemp)) +
            geom_bar(stat = 'identity') +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(y = yLabel)
  }
  #if 'Temperature' box is checked and 'Week Day' box is not checked
  else if (checkBox[1] == 1 & checkBox[2] == 0){
    ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = NULL, alpha = outsideTemp)) +
           geom_bar(stat = 'identity', fill = 'royalblue4') +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
           labs(y = yLabel)
  }
  #if 'Temperature' box is not checked and 'Week Day' box is checked
  else if (checkBox[1] == 0 & checkBox[2] == 1){
    ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = weekDay, alpha = NULL)) +
           geom_bar(stat = 'identity') +
           theme_minimal() +
           theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
           labs(y = yLabel)
  }
  #if 'Temperature' box is not checked and 'Week Day' box is not checked
  else{
    ggplot(allDailyHomeB, aes(x = Time, y = Watts, fill = NULL, alpha = NULL)) +
    geom_bar(stat = 'identity', fill = 'royalblue4') +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = yLabel)
    }
})
    
    
########## miscellaneous processes ##########
    
    #simple function used for a workaround elsewhere
    returnOne <- function(data){return(1)}
    
    #load daily summary data
    allDailyHomeB <- read.csv('data/homeB-all/allDaily.csv')
    
    #print this program's code
    fileName <- 'server.R'
    output$server <- renderText(readChar(fileName, file.info(fileName)$size))
    fileName2 <- 'ui.R'
    output$ui <- renderText(readChar(fileName2, file.info(fileName2)$size))

})
