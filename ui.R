library(shiny)
library(dplyr)
library(reshape2)



shinyUI(fluidPage( 
  
  
  navbarPage(title = 'Energy Visualization', #position = 'static-top', 
             
            
    tabPanel('Smart Meter Home', icon = icon('home'),{
        tabsetPanel('asdf', type = 'tabs', #position = 'above',
          tabPanel('Daily',
                   

      mainPanel( width = 12, 
        plotOutput('masterPlot')
      ),

      
      fluidRow(

     column(3,
	   dateInput('selectedDate', label = h3('Date'), value = as.Date('2012-05-09'), min = '2012-05-07', max = '2012-07-04', width = '160px'),
		textOutput('dateName')
	   ),
    column(3,
       radioButtons('unit', label = h3('Unit Choice'), choices = list('Cost per Hour' = .12/1000, 'kilowatt' = 1/1000, 'kg of CO2 per hour' = .7/1000), selected = .12/1000)
    ),
    column(3,
      radioButtons('timeInterval', label = h3('Time interval'),
          choices = list('Hour' = 100, '20 Minutes' = 20, '5 Minutes' = 5, 'Minute' = 1), selected = 100)
    ),
    column(3, 
      checkboxGroupInput('suplamentalInfo', label = h3('Overlays'), 
          choices = list('Temprature' = 1, 'Prior Week Average (grey)' = 2), selected = 0)
    )
    )

    
  ),
          tabPanel('History', 
                   
                   
                   plotOutput('monthlyHomeBPlot'),
    
    fluidRow(

    column(5,
	   dateRangeInput('dateRange', label = h3("Date range"), start = '2012-05-01', min = '2012-05-01', end = '2012-07-04', max = '2012-07-04'),
    textOutput('testing')
	   ),
    column(3,
      radioButtons('unit2', label = h3('Unit Choice'), choices = list('Cost' = .12/24000, 'kilowatt' = 1/24000, 'kg of CO2' = .7/24000), selected = .12/24000)
      
    ),
    column(3,
        checkboxGroupInput('suplamentalInfo2', label = h3('Overlays'), 
          choices = list('Temprature' = 1, 'Week Day' = 2), selected = NULL)
    ),
    column(1, 
      h1('')
    )
    )
    
    
    
    )
  
        )#
    }),
  
  

   
tabPanel('About This App', icon = icon('lightbulb-o'),'This application is an interactive visualization of electricity consumption data from the',
 a('UMass Smart* Home Data Set.', href = 'http://traces.cs.umass.edu/index.php/Smart/Smart'),
 'Using data from House B, which resembles the information captured by today\'s \"smart meters,\" this app is compareable to that of',
 a('DTE energy insight', href = 'https://www2.dteenergy.com/wps/portal/splashpage/DTE%20Insight/!ut/p/b1/hY_JdoIwAEW_hQ8oCUgYlkljBaFMLQlk08PgEZSxRVG_vrb71rd759y7uECAFIg-Pzf7fG6GPm9_vtA_VqbnM5QE0fuGWdAxXGe7wd4KRsodyO4A_GMYPvK3QDRFJy9lJ0NZ1xAyVaSYqo6QpuqA0fJo0AHv15jGE6yJm8zzZ8atupqIM9JFLXEeeig5GldiR8iuSZCmXsFrv4pT2l6fSh65Y5WZsxrCmFduLqg3xc-GfqJnBbOXt1M5vTZ-eIDC6tuiPjBWXBTy5dysQOOhiCQJcCB-M_6p9O2h24GxS2-7fpG-AY7XfDw!/dl4/d5/L2dBISEvZ0FBIS9nQSEh/'),
'which attempts to improve energy efficeny and literacy among residential consumers.',
#The frame work of this built in a flexable manner so that data from House A and House C can also be visuallized.  The data from these houses represent ",
hr(),
'The programming language R was used to make this application, and the code can be found down below or downloaded at',
a('my github.', href = 'http://traces.cs.umass.edu/index.php/Smart/Smart'),
'The server file is programed in a flexable manner so that data from House A and House C of the Smart* Data Set can also be visuallized.  House A has data broken down by circut and appliance and House C has energy consumption and production data, both of which depict scenarios that could become widely addopted in the not-so-distant-future.  Visuallizing the data of these houses could demonstrate their comarative advantages and is currently being worked on.',  
hr(),
'This project was developed during an independent study at the University of Michigan-Dearborn.  Goals: research developments surrounding the data of the electrical grid (',
a('Advanced Metering Infastructure,', href = 'http://aceee.org/research-report/e105'),
a('Signal Dissagregation', href = 'http://www.sciencedirect.com/science/article/pii/S0301421512007446'),
'), learn about data visualization (',
a('The Grammar of Graphics', href = 'http://www.springer.com/us/book/9780387245447'),
'), and to improve programming skills pretaining to data management.',
hr(),
#tags$style(type='text/css', '#server {background-color: rgba(255,255,0,0.40); color: green;}'), 
h3('server.R'),
verbatimTextOutput('server'),
hr(),
h3('ui.R'),
verbatimTextOutput('ui')
             )
  )#navbarPage
)
)#shinyU1


