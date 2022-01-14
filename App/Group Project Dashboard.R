#create small functions for easier access and load libraries

h <- function(x){head(x,1)}

#to get number of unique values easier
uni <- function(x){length(unique(x))}

#to get the mode of character values
mode <- function(x){names(tail(sort(table(x)),1))}

#create categories by using two variables
categories <- function(x,y){
  x <- as.numeric(x)
  y <- as.numeric(y)
  
  q <- quantile(x,c(0.3,0.6,0.9))
  q2 <- quantile(y,c(0.3,0.6,0.9))
  
  catx<-ifelse(x<=q[1],1,ifelse(x>q[1]&x<=q[2],2,ifelse(x>q[2]&x<=q[3],3,4)))
  caty<-ifelse(y<=q2[1],1,ifelse(y>q2[1]&y<=q2[2],2,ifelse(y>q2[2]&y<=q2[3],3,4)))
  
  z<-catx+caty
  
  catf<-ifelse(z<=2,"Rookie",ifelse(z>2&z<=4,"Amateur",ifelse(z>4&z<=6,"Advanced","Elite"))) 
  catf
}

colors <- c('#D0CBCA', '#E5562A', '#336EF9','#151F47')

#libraries
library(shiny)
library(data.table)
library(shinythemes)
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)


################ PREPARATION ------------------------------

#setwd('C:/Users/mserrano/OneDrive - IESEG/MSc/BA TOOLS OPEN SOURCE/Group Project/App')

daily <- data.table(read.csv("UserDaily Dep.csv"))
dtmart<- data.table(read.csv("Data Mart Bet and Poker.csv"))
chips<- data.table(read.csv("PokerChips Dep.csv"))

summary_daily <- function(prod,start,end){
  daily <-data.table(daily)
  daily2<- daily[Product_Name==prod][Date>=start&Date<=end][,.(Stakes=sum(Stakes),Winnings=sum(Winnings),Bets=sum(Bets),DaysPlayed = .N,
                                        FirstDate = min(Date), LastDate = max(Date), 
                                        N_Products = uni(Product_Name), 
                                        Bet_Products = head(Product_Name,1),
                                        MostActiveDay=mode(weekday)),by=UserID]
  
  daily2$balance <- daily2$Winnings-daily2$Stakes #balance per person
  daily2$casino_profitability <- 1-daily2$Winnings/daily2$Stakes #how much the casino wins per person (1- client profitability)
  daily2$client_profitability <- daily2$Winnings/daily2$Stakes #client profitability
  daily2$meanBets <- round(daily2$Bets/daily2$DaysPlayed,0) #mean bets per day
  daily2$StakesBet <- round(daily2$Stakes/daily2$Bets,2) #mean stakes per bet
  daily2$WinningsBet <- round(daily2$Winnings/daily2$Bets,2) #mean wins per bet
  daily2$BalanceBet <- round(daily2$balance/daily2$Bets,2) #mean balance (win or lose) per bet
  daily2$Recency <- as.numeric(as.Date("2005-09-30")- as.Date(daily2$LastDate)) #recency is number of days since last game
  daily2$since_first <- as.numeric(as.Date("2005-09-30")- as.Date(daily2$FirstDate)) #number of days since first game
  daily2$one_time_player <- ifelse(daily2$since_first == daily2$Recency,"YES","NO") #user only played one day until now
  daily2
}

summary_chips <-function(start, end){
  
  chips$TransDateTime<-as.Date(chips$TransDateTime)
  chips_sub<- chips[TransDateTime>=start&TransDateTime<=end]
  sum_chips2<- chips_sub[,.(Users=uni(UserID)),by=TransDateTime]
  sum_chipsB<- chips_sub[TransType=="Buy"][,.(Buys=sum(TransAmount,na.rm=F),MeanBuys=mean(TransAmount,na.rm=F)),by=TransDateTime]
  sum_chipsS<- chips_sub[TransType=="Sell"][,.(Sells=sum(TransAmount,na.rm=F),MeanSell=mean(TransAmount,na.rm=F)),by=TransDateTime]
  
  sum_chips2<-merge(sum_chips2,sum_chipsB,by="TransDateTime",all.x=T)
  sum_chips2<-merge(sum_chips2,sum_chipsS,by="TransDateTime",all.x=T)
  sum_chips2<- sum_chips2[order(TransDateTime)]
  sum_chips2
  
} 

summary_chips2 <-function(start, end){
  
  chips$TransDateTime<-as.Date(chips$TransDateTime)
  chips_sub<- chips[TransDateTime>=start&TransDateTime<=end]
  chipsb<-chips_sub[TransType=="Buy"][,.(Buys=mean(TransAmount),time_of_day=h(time_of_day)),by=hour_of_day]
  chipss<-chips_sub[TransType=="Sell"][,.(Sells=mean(TransAmount)),by=hour_of_day]
  chipss2<-merge(chipsb,chipss,by="hour_of_day")
  chipss2
} 


#users daily
users_daily<-daily[,.(Users=uni(UserID)),by=Date][order(Date)]
users_daily_p<- chips[,.(Poker_Users=uni(UserID)),by=as.Date(TransDateTime)][order(as.Date)]
names(users_daily_p)[1]<-"Date"


#map info
maps <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
dfmap <- dtmart %>%  count(Country_Name)
names(dfmap)[1]<-"COUNTRY"
dfmap <- merge(dfmap,maps,by="COUNTRY",all.x = T)
#fix some NAs
dfmap$COUNTRY[is.na(dfmap$CODE)]
dfmap$CODE[dfmap$COUNTRY=="USA"] <- "USA"
dfmap$CODE[dfmap$COUNTRY=="Russian Federation"] <- "RUS"
dfmap$CODE[dfmap$COUNTRY=="Moldavia"] <- "MDA"
dfmap$CODE[dfmap$COUNTRY=="Tunesia"] <- "TUN"
dfmap$CODE[dfmap$COUNTRY=="Holland"] <- "NLD"


#mean daily
mean_daily<- daily[,.(Users=uni(UserID),Product=h(Product_Name)),by=paste(Date,ProductID)][,.(Average=mean(Users)),by=Product]
mean_daily_p<- chips[,.(Users=uni(UserID),Product="Poker"),by=as.Date(TransDateTime)][,.(Average=mean(Users)),by=Product]
mean_daily<-rbind(mean_daily,mean_daily_p)

poker<-subset(dtmart,!is.na(dtmart$MostActiveDayPoker))
poker2<-data.frame(poker)

################ UI ------------------------------

ui <- fluidPage(theme = shinytheme("simplex"),
                tags$img(src="IESEG-Logo-2012-rgb.jpg", align="right", height = 60, width=200),

  h2(strong("Dashboard Bwin")),
  navlistPanel( widths = c(2,10),
    tabPanel(h4(strong("Users")),
          
               mainPanel(
                 tabsetPanel(type="tabs",
                             tabPanel('General',
                                      fluidRow(align="center", 
                                        splitLayout(cellWidths = c("45%","55%"), h4(strong("Gender Distrubution")),h4(strong("Registered by Date"))),
                                        splitLayout(cellWidths = c("45%","55%"), plotlyOutput("plot1"),plotlyOutput("plot2")),
                                      ),
                                      fluidRow(align="center",
                                               column(4,
                                                      h4("Total Users"),
                                                      h3(strong(textOutput("totUsers")))
                                                      ),
                                               column(4,
                                                      h4("Daily Users"),
                                                      h3(strong(textOutput("UsersDailyGen")))
                                               ),
                                               column(4,
                                                      h4("Daily Users Poker"),
                                                      h3(strong(textOutput("UsersDailyGenP")))
                                               ),
                                               
                                        
                                      ),
                                      fluidRow(h5("For the following plots you can select the date range to view:"),
                                               align="center",
                                               column(6,dateInput("date_start", strong("Start Date"),min(dtmart$FirstAct),min=min(dtmart$FirstAct),max=max(dtmart$FirstAct))),
                                               column(6,dateInput("date_end", strong("End Date"),"2005-03-31",min=min(dtmart$FirstAct),max=max(dtmart$FirstAct))),
                                               
                                      ),
                                      fluidRow(align="center", 
                                               splitLayout(cellWidths = c("50%","50%"), h4(strong("First Activity by Date")),h4(strong("First Activity and First Pay"))),
                                               splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot3"),plotlyOutput("plot4")),
                                      ),
                                      fluidRow(align="center",h4(strong("Interval First Pay and Registration Date")),plotlyOutput("plot5")
                                               )
                                      ),
                             tabPanel("Countries and App",
                                      fluidRow(align="center",
                                               h4(strong("Location of Users")),
                                               plotlyOutput("mapPlot")
                                               ),
                                      fluidRow(align = "center",
                                               column(6,
                                                      checkboxInput("removeGerman","Remove German Language"),
                                                      ),
                                               column(6,
                                                      checkboxInput("removeGermany","Remove Germany")
                                                      )
                                               ),
                                      fluidRow(align="center", 
                                               splitLayout(cellWidths = c("50%","50%"), h4(strong("Users' Language")),h4(strong("Top 10 Countries"))),
                                               splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot6"),plotlyOutput("plot7")),
                                               h4(strong("Most Used Application")),
                                               plotlyOutput("plotApps")
                                               )
                             )
                             
                 )
                 
               )
               
      
    ),
    tabPanel(h4(strong("Bets")),
             
             mainPanel(
               tabsetPanel(type="tabs",
                           tabPanel('Main',
                                    fluidRow(h5("For a better visualization you can select to remove outliers for some plots:"),
                                      column(6,
                                             checkboxInput("removeSport","Remove Sports"),
                                             ),
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Average Daily Users")),h4(strong("Daily Users by Date"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot8"),plotlyOutput("plot9")),
                                    ),
                                    fluidRow(align="center",
                                      h4("Most Active Days by Product"),
                                      plotlyOutput("plot10")
                                    )
                                    ),
                           tabPanel('Products',
                                    fluidRow(align="center",
                                             column(4,
                                                    selectInput("selProd",strong("Select Product"),unique(daily$Product_Name))
                                                    ),
                                             column(4,
                                                    dateInput("date_start2", strong("Start Date"),min(dtmart$FirstAct),min=min(dtmart$FirstAct),max=max(dtmart$FirstAct))
                                             ),
                                             column(4,
                                                    dateInput("date_end2", strong("End Date"),"2005-09-30",min=min(dtmart$FirstAct),max=max(dtmart$FirstAct))                                             
                                                    )
                                             ),
                                    fluidRow(align="center",
                                             column(3,
                                                    h4("Daily Users"),
                                                    h3(strong(textOutput("DailyUsers")))
                                             ),
                                             column(3,
                                                    h4("Total Stakes"),
                                                    h3(strong(textOutput("TotStakes")))
                                             ),
                                             column(3,
                                                    h4("Total Winnings"),
                                                    h3(strong(textOutput("TotWins")))
                                             ),
                                             column(3,
                                                    h4("Total Profit"),
                                                    h3(strong(textOutput("TotBal")))
                                             ),
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Profitability (%)")),h4(strong("One Time Players"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot11"),plotlyOutput("plot12"))
                                    ),
                                    fluidRow(align="center",
                                             column(3,
                                                    h4("Mean Bets per Day"),
                                                    h3(strong(textOutput("MeanBets")))
                                             ),
                                             column(3,
                                                    h4("Mean Stakes per Bet"),
                                                    h3(strong(textOutput("MeanStakes")))
                                             ),
                                             column(3,
                                                    h4("Mean Wins per Bet"),
                                                    h3(strong(textOutput("MeanWins")))
                                             ),
                                             column(3,
                                                    h4("Mean Balance per Bet"),
                                                    h3(strong(textOutput("MeanBal")))
                                             ),
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("40%","60%"), h4(strong("Most Active Day")),h4(strong("Recency"))),
                                             splitLayout(cellWidths = c("40%","60%"), plotlyOutput("plot13"),plotlyOutput("plot14"))
                                    )
                                    )
                                    
                                    
                           )
                           
               )
               
             ),
    tabPanel(h4(strong("Poker")),
             
             mainPanel(
               tabsetPanel(type="tabs",
                           tabPanel('Main',
                                    fluidRow(align="center",
                                             column(6,
                                                    dateInput("date_start3", strong("Start Date"),min(poker$FirstAct),min=min(poker$FirstAct),max="2005-09-30")
                                             ),
                                             column(6,
                                                    dateInput("date_end3", strong("End Date"),"2005-09-30",min=min(poker$FirstAct),max="2005-09-30")                                             
                                             )
                                    ),
                                    fluidRow(align="center",
                                             column(4,
                                                    h4("Daily Users"),
                                                    h3(strong(textOutput("DailyUsersP")))
                                             ),
                                             column(4,
                                                    h4("Total Buys"),
                                                    h3(strong(textOutput("TotBuys")))
                                             ),
                                             column(4,
                                                    h4("Total Sells"),
                                                    h3(strong(textOutput("TotSells")))
                                             )
                                    ),
                                    fluidRow(
                                      checkboxInput("removeOutliers","Remove Outliers",value=TRUE)
                                      ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Buys per Time of Day")),h4(strong("Sells per Time of Day"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot15"),plotlyOutput("plot16"))
                                    ),
                                    fluidRow(align="center",
                                             column(6,
                                                    h4("Mean Buy"),
                                                    h3(strong(textOutput("MeanBuys")))
                                             ),
                                             column(6,
                                                    h4("Mean Sell"),
                                                    h3(strong(textOutput("MeanSells")))
                                             ),
                                             
                                             h4(strong("Mean Buys and Mean Sells per Time of Day")),
                                             plotlyOutput("plot17"),
                                             h5("In the previous plot you can click on the legend to deselect any time of day."),
                                             h4(strong("Balance per Preffered Day and Time of Day")),
                                             plotlyOutput("plot18")
                                    ),
                                    fluidRow(align="center",
                                             h4(strong("Daily Users")),
                                             plotlyOutput("plot19")
                                    ),
                           ),
                           tabPanel("Categories",
                                    fluidRow(h5("In this page we show player categories obtained using 'BuyAmount' and 'Sell_Ratio', you can select two different variables to create new categories.")),
                                    fluidRow(align="center",
                                             column(6,
                                                    selectInput("var1","First Column",names(poker)[48:55])
                                                    ),
                                             column(6,
                                                    selectInput("var2","Second Column",names(poker)[48:55])
                                             )
                                      
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Players by Category")),h4(strong("Players by New Category"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot20",height = "300px"),plotlyOutput("plot22",height = "300px"))
                                    ),
                                    fluidRow(checkboxInput("removeOutliers2","Remove Outliers",value=TRUE)
                                    ),
                                    fluidRow(align="center",
                                             splitLayout(cellWidths = c("50%","50%"), h4(strong("Sell Ratio by Player Category")),h4(strong("Sell Ratio by Player New Category"))),
                                             splitLayout(cellWidths = c("50%","50%"), plotlyOutput("plot21"),plotlyOutput("plot23"))
                                    )
                           )
                           
               )
               
             )
             
             
    )
    
             
             
    )
    
)
  
  
################ SERVER ------------------------------


server <- function(input, output){
  
  ############### first page users 
  
  #plots
  output$plot1 <- renderPlotly({
    dtmart %>% count(Gender) %>%
      plot_ly(labels = ~Gender,values = ~n, type = "pie", textinfo = "label+percent", 
              showlegend = F) %>% layout(autosize=F,width = 300, height =300) 
    })
  
  output$plot2 <- renderPlotly({
    dtmart<-data.table(dtmart)
    regs<- dtmart[,.N,by=RegDate]
    plot_ly(regs, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_trace(x = ~RegDate, y = ~N,marker = list(color = "rgb(21, 31, 71)"),
                line = list(color ='rgb(21,31,71'),
                fillcolor='rgba(50, 100, 250, 0.4)') %>%
      layout(showlegend = F, xaxis=list(title=""),yaxis=list(title="Number of registered users"))
    })
  
  output$plot3 <- renderPlotly({
    firstAct<- dtmart[FirstAct>=input$date_start&FirstAct<=input$date_end][,.N,by=FirstAct][order(FirstAct)]
    plot_ly(firstAct, type = 'scatter', mode = 'lines')%>%
      add_trace(x = ~FirstAct, y = ~N,marker = list(color = "rgb(21, 31, 71)"),line = list(color ='rgb(21,31,71')) %>%
      layout(showlegend = F,yaxis=list(title="Number of Users"),xaxis=list(title="")) 
    })
  
  output$plot4 <- renderPlotly({
    plot_ly(alpha=0.4) %>% add_histogram(dtmart[FirstPay>=input$date_start&FirstPay<=input$date_end]$FirstPay, 
                                         name="First Pay", marker = list(color = "rgba(21, 31, 71,0.8)")) %>%
      add_histogram(dtmart[FirstAct>=input$date_start&FirstAct<=input$date_end]$FirstAct, 
                    name="First Activity",marker = list(color = "rgba(254, 95, 48,0.8)")) %>% 
      layout(barmode="stack",legend = list(x=0.5,y=0.9))
    
  })
  output$plot5 <- renderPlotly({
    plot_ly(alpha=0.4) %>% 
      add_histogram(dtmart[interval_firstPay<quantile(interval_firstPay,0.95)]$interval_firstPay, 
                    name="Interval First Pay", marker = list(color = "rgb(21, 31, 71)")) %>%
      layout(xaxis=list(title="Days"),yaxis=list(title="Number of Users"))
  })
  
  output$mapPlot <- renderPlotly({
    
    plot_ly(dfmap,type='choropleth', locations=dfmap$CODE, z=dfmap$n, text=dfmap$Country_Name, colorscale="Greens")
  })
  
  output$plot6 <- renderPlotly({
    if (input$removeGerman==TRUE){
      dtmart %>%filter(Language_Name!="German")%>% count(Language_Name) %>% 
        plot_ly(x=~Language_Name,y=~n) %>% add_bars(marker = list(color = c(colors,colors,colors,colors))) %>%
        layout(xaxis = list(title="Language",categoryorder = "total descending"),yaxis=list(title="Users"))
    }
    else {
      dtmart %>% count(Language_Name) %>% 
        plot_ly(x=~Language_Name,y=~n) %>% add_bars(marker = list(color = c(colors,colors,colors,colors))) %>%
        layout(xaxis = list(title="Language",categoryorder = "total descending"),yaxis=list(title="Users"))
    }
    
  })
  
  output$plot7 <- renderPlotly({
    if (input$removeGermany==TRUE){
      topCountries<-names(tail(sort(table(dtmart[Country_Name!="Germany"]$Country_Name)),10))
      
      dtmart %>% filter(Country_Name %in%topCountries)%>% count(Country_Name) %>% arrange(n)  %>% 
        plot_ly(x = ~Country_Name,y = ~n) %>% add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
        layout(xaxis = list(title="Country",categoryorder = "total descending"),yaxis=list(title="Users"))
      
    }
    else {
      topCountries<-names(tail(sort(table(dtmart$Country_Name)),10))
      
      dtmart %>% filter(Country_Name %in%topCountries)%>% count(Country_Name) %>% arrange(n)  %>% 
        plot_ly(x = ~Country_Name,y = ~n) %>% add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
        layout(xaxis = list(title="Country",categoryorder = "total descending"),yaxis=list(title="Users"))
    }
    
  })
  output$plotApps <- renderPlotly({
    topApps<-names(tail(sort(table(dtmart$Application_Name)),10))
    
    dtmart %>% filter(Application_Name %in%topApps) %>%count(Application_Name) %>%
      plot_ly(labels = ~Application_Name,values = ~n, textinfo = "label+percent",
              showlegend = F) %>% add_pie(hole = 0.4)
  })
  
  #text outputs
  
  output$totUsers<-renderText({format(nrow(dtmart),big.mark=',')})
  output$UsersDailyGen<-renderText({format(round(mean(users_daily$Users),0),big.mark=',')})
  output$UsersDailyGenP<-renderText({format(round(mean(users_daily_p$Poker_Users),0),big.mark=',')})
  
  ############### second page bets 
  
  #plots

  output$plot8 <- renderPlotly({
    if (input$removeSport==TRUE){
      plot_ly(mean_daily[!agrepl("Sports",Product)],x = ~Product,y = ~Average) %>% 
        add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
        layout(yaxis=list(title="Daily Users"))
    }
    else {
      plot_ly(mean_daily,x = ~Product,y = ~Average) %>% 
        add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)"))) %>%
        layout(yaxis=list(title="Daily Users"))
    }
    
  })
  
  output$plot9 <- renderPlotly({
    
    plot_ly(users_daily, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_trace(x = ~Date, y = ~Users,line = list(color ='rgb(21, 31, 71)'),
                fillcolor='rgba(50, 100, 250, 0.4)') %>%
      layout(showlegend = F)
  })
  
  output$plot10 <- renderPlotly({
    
    if (input$removeSport==TRUE) {
      byprod<- dtmart[,.(CasinoBossMedia=sum(CasinoBossMedia),CasinoChartwell=sum(CasinoChartwell),
                         GamesBwin=sum(GamesBwin),GamesVS=sum(GamesVS),
                         Supertoto=sum(Supertoto)),by=MostActiveDay][!is.na(MostActiveDay)]
      plot_ly(byprod, x=~MostActiveDay,y=~GamesBwin, name="Games Bwin", type="bar") %>%
        add_trace(y=~GamesVS, name="Games VS" ) %>%
        add_trace(y=~CasinoBossMedia, name="Casino Boss Media" ) %>%
        add_trace(y=~CasinoChartwell, name="Casino Chartwell" ) %>%
        add_trace(y=~Supertoto, name= "Supertoto"  )  %>% 
        layout(yaxis = list(title = 'Count'),xaxis = list(title = "",
                                                          categoryorder = "total descending"), barmode = 'stack')
    }
    else {
      byprod<- dtmart[,.(CasinoBossMedia=sum(CasinoBossMedia),CasinoChartwell=sum(CasinoChartwell),
                         GamesBwin=sum(GamesBwin),GamesVS=sum(GamesVS),SportsBookFixed=sum(SportsBookFixed),
                         SportsBookLive=sum(SportsBookLive),Supertoto=sum(Supertoto)),by=MostActiveDay][!is.na(MostActiveDay)]
      plot_ly(byprod, x=~MostActiveDay,y=~SportsBookFixed, name="Sports Book Fixed", type="bar") %>%
        add_trace(y=~SportsBookLive, name="Sports Book Live") %>%
        add_trace(y=~GamesBwin, name="Games Bwin") %>%
        add_trace(y=~GamesVS, name="Games VS" ) %>%
        add_trace(y=~CasinoBossMedia, name="Casino Boss Media" ) %>%
        add_trace(y=~CasinoChartwell, name="Casino Chartwell" ) %>%
        add_trace(y=~Supertoto, name= "Supertoto"  )  %>% 
        layout(yaxis = list(title = 'Count'),xaxis = list(title = "",
                                                          categoryorder = "total descending"), barmode = 'stack')
    }
   
    
  })
  
  daily2<-reactive({summary_daily(prod=input$selProd,start=input$date_start2,end=input$date_end2)}) 
  
  
  output$plot11 <- renderPlotly({
    
    plot_ly(
      domain = list(x = c(0, 100), y = c(0, 100)),
      value = mean(daily2()$casino_profitability,na.rm=T)*100,
      title = list(text = ""), type = "indicator",
      mode = "gauge+number+delta", number = list(suffix = "%"),
      gauge = list(bar = list(color = "rgb(21,31,71)"),
                   bordercolor = "gray"),
      delta = list(reference = 15, increasing = list(color = "darkgreen"),decreasing=list(color = "darkred"))) %>%
      layout(margin = list(l=20,r=30))
  })
  
  output$plot12 <- renderPlotly({
    daily2() %>% count(one_time_player) %>%
      plot_ly(x = ~one_time_player,y = ~n) %>% 
      add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)"))) %>%
      layout(xaxis=list(title=""),yaxis=list(title="Count"))
    
  
  })
  
  output$plot13 <- renderPlotly({
    
    daily2() %>% count(MostActiveDay) %>%
      plot_ly(labels = ~MostActiveDay,values = ~n, type = "pie", textinfo = "label+percent",
              marker = list(color = c("#151F47", "#AA3344")),
              showlegend = F)
  })
  
  output$plot14 <- renderPlotly({
    
    plot_ly(alpha=0.4) %>% add_histogram(daily2()$Recency, marker = list(color = "rgba(27, 63, 10,0.8)")) %>%
      layout(barmode="stack",legend = list(orientation = 'v'),xaxis=list(title="Days"),yaxis=list(title="Number of Users"))
    
  })
  
  
  
  #text outputs
  users_daily_prod<-reactive({
    daily[Product_Name==input$selProd][Date>=input$date_start2&Date<=input$date_end2][,.(Users=uni(UserID)),by=Date]
    }) 
  
  output$DailyUsers<-renderText({format(round(mean(users_daily_prod()$Users,na.rm=T),0),big.mark=',')})
  output$TotStakes<-renderText({format(round(sum(daily2()$Stakes,na.rm=T),2),big.mark=',')})
  output$TotWins<-renderText({format(round(sum(daily2()$Winnings,na.rm=T),2),big.mark=',')})
  output$TotBal<-renderText({format(round(sum(daily2()$Stakes,na.rm=T)-sum(daily2()$Winnings,na.rm=T),2),big.mark=',')})
  
  output$MeanBets<-renderText({format(round(mean(daily2()$meanBets,na.rm=T),0),big.mark=',')})
  output$MeanStakes<-renderText({format(round(mean(daily2()$StakesBet,na.rm=T),2),big.mark=',')})
  output$MeanWins<-renderText({format(round(mean(daily2()$WinningsBet,na.rm=T),2),big.mark=',')})
  output$MeanBal<-renderText({format(round(mean(daily2()$BalanceBet,na.rm=T),2),big.mark=',')})
  
  ############### third page poker 
  
  output$plot15 <- renderPlotly({
    if (input$removeOutliers==TRUE){
      plot_ly(poker[MeanBuy<quantile(MeanBuy,0.98,na.rm=T)], y=~MeanBuy, color= ~PrefTimeOfDay, type="box")  %>% 
        layout(showlegend = F)
    }
    else {
      plot_ly(poker, y=~MeanBuy, color= ~PrefTimeOfDay, type="box")  %>% 
        layout(showlegend = F)
    }  
    
  })
  
  output$plot16 <- renderPlotly({
    if (input$removeOutliers==TRUE){
      plot_ly(poker[MeanSell<quantile(MeanSell,0.98,na.rm=T)], y=~MeanSell, color= ~PrefTimeOfDay, type="box") %>%
        layout(showlegend=F)
    }
    else {
      plot_ly(poker, y=~MeanSell, color= ~PrefTimeOfDay, type="box") %>%
        layout(showlegend=F)
    }
    
  })
  
  chipss2<-reactive({
    summary_chips2(start=input$date_start3,end=input$date_end3)
  }) 
  
  output$plot17 <- renderPlotly({
   
    plot_ly(chipss2(), x=~Sells, y=~Buys, color=~time_of_day, size=~Buys, marker = list(opacity = 0.8),
            sizes=c(10,400))
    
  })
  
  output$plot18 <- renderPlotly({
    if (input$removeOutliers==TRUE){
      plot_ly(data = poker[MeanBuy<quantile(MeanBuy,0.98,na.rm=T)],
              x = ~MostActiveDayPoker, y = ~round(BalancePoker,2),
              color = ~PrefTimeOfDay, colors=colors) %>% 
        layout(yaxis=list(title="Balance"),xaxis=list(title=""),legend = list(orientation="h"))
    }
    else {
      plot_ly(data = poker,
              x = ~MostActiveDayPoker, y = ~round(BalancePoker,2),
              color = ~PrefTimeOfDay, colors=colors) %>% 
        layout(yaxis=list(title="Balance"),xaxis=list(title=""),legend = list(orientation="h"))
    }
    
    
  })
  
  output$plot19 <- renderPlotly({
    plot_ly(users_daily_p, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
      add_trace(x = ~Date, y = ~Poker_Users,line = list(color ='rgb(254, 95, 48)'),
                fillcolor='rgba(246, 143, 112, 0.4)') %>%
      layout(showlegend = F, yaxis=list(title="Poker Users"))
    
  })
  
  output$plot20 <- renderPlotly({
    poker %>% count(Player_Category) %>%
      plot_ly(labels = ~Player_Category,values = ~n, type = "pie", textinfo = "label+percent",
              showlegend = F)
    
  })
  
  output$plot21 <- renderPlotly({
    if (input$removeOutliers2==TRUE){
      poker[sell_ratio<quantile(sell_ratio,0.98,na.rm=T)] %>% 
        plot_ly(x=~Player_Category,y=~sell_ratio,type = "box",color= ~Player_Category) %>%
        layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
      
    }
      else {
       poker %>% 
          plot_ly(x=~Player_Category,y=~sell_ratio,type = "box",color= ~Player_Category) %>%
          layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
        
      }
    
  })
  
  output$plot22 <- renderPlotly({
    poker2$NewCat<-categories(poker2[,input$var1],poker2[,input$var2])
    poker2 %>% count(NewCat) %>%
      plot_ly(labels = ~NewCat,values = ~n, type = "pie", textinfo = "label+percent",
              showlegend = F)
    
  })
  
  output$plot23 <- renderPlotly({
    poker2$NewCat<-categories(poker2[,input$var1],poker2[,input$var2])
    
    if (input$removeOutliers2==TRUE){
      poker2 %>% filter(sell_ratio<quantile(sell_ratio,0.98,na.rm=T)) %>% 
        plot_ly(x=~NewCat,y=~sell_ratio,type = "box",color= ~NewCat) %>%
        layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
      
    }
    else {
      poker2 %>% 
        plot_ly(x=~NewCat,y=~sell_ratio,type = "box",color= ~NewCat) %>%
        layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))
      
    }
    
  })
  
  #text outputs
  users_daily_pok<-reactive({
    summary_chips(start=input$date_start3,end=input$date_end3)
  }) 
  
  output$DailyUsersP<-renderText({format(round(mean(users_daily_pok()$Users,na.rm=T),0),big.mark=',')})
  output$TotBuys<-renderText({format(round(sum(users_daily_pok()$Buys,na.rm=T),2),big.mark=',')})
  output$TotSells<-renderText({format(round(sum(users_daily_pok()$Sells,na.rm=T),2),big.mark=',')})

  output$MeanBuys<-renderText({format(round(mean(users_daily_pok()$MeanBuys,na.rm=T),2),big.mark=',')})
  output$MeanSells<-renderText({format(round(mean(users_daily_pok()$MeanSell,na.rm=T),2),big.mark=',')})

} 

shinyApp(ui = ui, server = server)