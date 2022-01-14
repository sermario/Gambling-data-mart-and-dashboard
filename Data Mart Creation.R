######################### GROUP PROJECT OPEN SOURCE ##############################


options(scipen=999) #so we dont have numbers in scientific

#create small functions for easier access and load libraries

h <- function(x){head(x,1)}

#to get number of unique values easier
uni <- function(x){length(unique(x))}

#to get the mode of character values
mode <- function(x){names(tail(sort(table(x)),1))}

#create categories by using two variables
categories <- function(x,y){
  q <- quantile(x,c(0.3,0.6,0.9))
  q2 <- quantile(y,c(0.3,0.6,0.9))
  
  catx<-ifelse(x<=q[1],1,ifelse(x>q[1]&x<=q[2],2,ifelse(x>q[2]&x<=q[3],3,4)))
  caty<-ifelse(y<=q2[1],1,ifelse(y>q2[1]&y<=q2[2],2,ifelse(y>q2[2]&y<=q2[3],3,4)))
  
  z<-catx+caty
  
  catf<-ifelse(z<=2,"Rookie",ifelse(z>2&z<=4,"Amateur",ifelse(z>4&z<=6,"Advanced","Elite"))) 
  catf
}


#libraries

library(lubridate)
library(data.table)
library(dplyr)
library(readxl)
library(ggplot2)
library(plotly)


#Base Script

setwd('C:/Users/mserrano/OneDrive - IESEG/MSc/BA TOOLS OPEN SOURCE/Group Project')

#Load the information

load("DataGroupAssignment.Rdata")

#We have 3 tables and we are going to change names

demo<- data.frame(Demographics)
chips<- data.frame(PokerChipConversions)
daily<- data.frame(UserDailyAggregation)

rm(Demographics,PokerChipConversions,UserDailyAggregation)

# 1.- CLEANING DATA -----------


# DEMOGRAPHICS -----------

#first fix dates 
names(demo)

demo$RegDate<-as.Date(demo$RegDate)

for (i in c(5:10)){
  demo[,i]<-as.Date(demo[,i],format="%Y%m%d")
}


#bring the names of languages and countries

countries<-read_excel("Appendices Group Assignment.xlsx", sheet = 2)
languages<-read_excel("Appendices Group Assignment.xlsx", sheet = 3)
application<-read_excel("Appendices Group Assignment.xlsx", sheet = 4)

names(countries)[2]<-"Country_Name"
names(languages)[2]<-"Language_Name"
names(application)[2]<-"Application_Name"

demo<-merge(demo,countries,by="Country")
demo<-merge(demo,languages,by="Language")
demo<-merge(demo,application,by="ApplicationID")

#drop codes

demo$ApplicationID<-NULL
demo$Language<-NULL
demo$Country<-NULL

#change gender

demo$Gender <- ifelse(demo$Gender == 1, "Male","Female")

#check dates of register and first play

summary(demo$RegDate) #all of them are within the period
summary(demo$FirstAct) #2 NAs
table(demo$FirstAct>=demo$RegDate) #all of them are >=

demo <- subset(demo,!is.na(demo$FirstAct)) #remove 2 NAs

# USER DAILY AGGREGATION -----------

#first fix dates

daily$Date<-as.Date(daily$Date,format="%Y%m%d")

#merge product name

prod<-read_excel("Appendices Group Assignment.xlsx", sheet = 1)

names(prod)[2]<-"Product_Name"

daily<-merge(daily,prod,by="ProductID")

#bring first pay date from demographics

firstpay <- demo[,c("UserID","FirstPay")]

table(daily$UserID %in% firstpay$UserID)

daily <- merge(daily,firstpay,by="UserID",all.x=T)

# POKERCHIPS  -----------

#first fix dates in pokerchips

chips$TransDateTime<-as.POSIXct(chips$TransDateTime,format="%Y-%m-%d %H:%M:%S")

chips$TransType <- ifelse(chips$TransType == 124, "Buy","Sell")

# 2.- CREATION OF NEW VARIABLES ----------- 

# DEMOGRAPHICS ------

demo$N_Cats_Played <- ifelse(!is.na(demo$FirstSp),1,0) + ifelse(!is.na(demo$FirstCa),1,0) +
  ifelse(!is.na(demo$FirstGa),1,0)+ifelse(!is.na(demo$FirstPo),1,0)

demo$interval_firstPay<- as.numeric(demo$FirstPay-demo$RegDate)
hist(demo$interval_firstPay)

demo$Poker <- ifelse(!is.na(demo$FirstPo),"Yes","No")



# USER DAILY AGREGGATION ------

daily <- data.table(daily)

#we are going to subset for only the dates that happened after the first pay and the ones the dont match

daily <- subset(daily, daily$Date>=daily$FirstPay)

#we are going to eliminate the transactions that dont have stakes, bets, or wins because they were inactive

daily <- subset(daily, !(daily$Stakes==0&daily$Winnings==0&daily$Bets==0))

#days of week
daily$weekday <- weekdays(daily$Date)

#write tables
write.csv(demo,"Demographics Dep.csv",row.names=F)
write.csv(daily,"UserDaily Dep.csv",row.names=F)

#group by user id

daily2<- daily[,.(Stakes=sum(Stakes),Winnings=sum(Winnings),Bets=sum(Bets),DaysPlayed = .N,
                  FirstDate = min(Date), LastDate = max(Date), 
                  N_Products = uni(Product_Name), 
                  Bet_Products = paste(unique(Product_Name),collapse = ", "),
                  CasinoBossMedia=sum(Product_Name=="Casino BossMedia"),
                  CasinoChartwell=sum(Product_Name=="Casino Chartwell"),
                  GamesBwin=sum(Product_Name=="Games bwin"),
                  GamesVS=sum(Product_Name=="Games VS"),
                  SportsBookFixed=sum(Product_Name=="Sports book fixed-odd"),
                  SportsBookLive=sum(Product_Name=="Sports book live-action"),
                  Supertoto=sum(Product_Name=="Supertoto"),
                  MostActiveDay=mode(weekday)),by=UserID]

daily2$balance <- daily2$Winnings-daily2$Stakes #balance per person
daily2$casino_profitability <- 1-daily2$Winnings/daily2$Stakes #how much the casino wins per person (1- client profitability)
daily2$client_profitability <- daily2$Winnings/daily2$Stakes #client profitability
daily2$meanBets <- round(daily2$Bets/daily2$DaysPlayed,0) #mean bets per day
daily2$StakesBet <- round(daily2$Stakes/daily2$Bets,2) #mean stakes per bet
daily2$WinningsBet <- round(daily2$Winnings/daily2$Bets,2) #mean wins per bet
daily2$BalanceBet <- round(daily2$balance/daily2$Bets,2) #mean balance (win or lose) per bet
daily2$Recency <- as.numeric(as.Date("2005-09-30")- daily2$LastDate) #recency is number of days since last game
daily2$since_first <- as.numeric(as.Date("2005-09-30")- daily2$FirstDate) #number of days since first game
daily2$one_time_player <- ifelse(daily2$since_first == daily2$Recency,"YES","NO") #user only played one day until now


# POKERCHIPS  ------

#filter dates before pay in

chips <- merge(chips, firstpay, by="UserID",all.x = T)
table(chips$TransDateTime>=chips$FirstPay)


nrow(chips) #277907
chips <- subset(chips,chips$TransDateTime>=chips$FirstPay)
summary(chips$TransDateTime) #remove dates after september 30

nrow(chips) #277809

chips <- subset(chips,chips$TransDateTime<=as.Date("2005-09-30"))

#create additional variables
chips$weekday <- weekdays(chips$TransDateTime)

chips$hour_of_day<-hour(chips$TransDateTime)
summary(chips$hour_of_day)

chips$difftime <- diffDate(chips$TransDateTime)

difftime(chips$TransDateTime[1],chips$TransDateTime[2])

chips$time_of_day<-ifelse(chips$hour_of_day>=2&chips$hour_of_day<=8,"Early Morning",
                          ifelse(chips$hour_of_day>8&chips$hour_of_day<=14,"Morning",
                                 ifelse(chips$hour_of_day>14&chips$hour_of_day<=20,"Afternoon",
                                        "Night")))


chips <- data.table(chips)
write.csv(chips,"PokerChips Dep.csv",row.names=F)

table_buy <- chips[TransType=="Buy"][,.(BuyAmount=sum(TransAmount),N_buys=.N),by=UserID]
table_sell <- chips[TransType=="Sell"][,.(SellAmount=sum(TransAmount),N_sells=.N),by=UserID]


#general table
table(chips$time_of_day)

table_chips<-chips[,.(MostActiveDayPoker=mode(weekday),Early_Morning = sum(time_of_day=="Early Morning"),
                      Morning = sum(time_of_day=="Morning"),
                      Afternoon = sum(time_of_day=="Afternoon"),
                      Night = sum(time_of_day=="Night"),
                      PrefTimeOfDay = mode(time_of_day)),by=UserID]

table_chips<- merge(table_chips,table_buy,by="UserID",all.x = T, all.y = T)
table_chips<- merge(table_chips,table_sell,by="UserID",all.x = T, all.y = T)

table_chips$BuyAmount<- ifelse(is.na(table_chips$BuyAmount),0,table_chips$BuyAmount)
table_chips$SellAmount<- ifelse(is.na(table_chips$SellAmount),0,table_chips$SellAmount)
table_chips$N_buys<- ifelse(is.na(table_chips$N_buys),0,table_chips$N_buys)
table_chips$N_sells<- ifelse(is.na(table_chips$N_sells),0,table_chips$N_sells)


table_chips$BalancePoker <- table_chips$SellAmount-table_chips$BuyAmount

table_chips$MeanBuy <- ifelse(is.na(table_chips$BuyAmount/table_chips$N_buys),0,table_chips$BuyAmount/table_chips$N_buys)
table_chips$MeanSell <- ifelse(is.na(table_chips$SellAmount/table_chips$N_sells),0,table_chips$SellAmount/table_chips$N_sells)


table_chips$sell_ratio <- ifelse(is.infinite(table_chips$SellAmount/table_chips$BuyAmount),0,table_chips$SellAmount/table_chips$BuyAmount)

#create some categories to filter later

quantile(table_chips$BuyAmount,c(0.1,0.5,0.9))

table_chips$Player_Category <- categories(table_chips$BuyAmount,table_chips$sell_ratio)
barplot(table(table_chips$Player_Category))


#this part is using a loop to get the mean time lapse (per player) between buy ins within the same game

users<-data.frame("UserID"=unique(chips$UserID)) 
users$mean_buy_interval <-NA

test <-data.frame()
for (i in 1:nrow(users)){
  
  test <- subset(chips,chips$UserID==users$UserID[i])
  
  if (sum(test$TransType=="Buy")==1|!test$TransType%in%"Buy"){next}
  
  timedif <- vector()
  for (j in 2:nrow(test)){
    timedif[j] <- difftime(test$TransDateTime[j],test$TransDateTime[j-1]) 
  }
  
  sells <- c(which(test$TransType=="Sell")+1,which(test$TransType=="Sell"))
  rows <-c(1:nrow(test))
  buys <- !rows%in%sells
  users$mean_buy_interval[i]<- mean(timedif[buys],na.rm = T)
  if(i %% 50==0) {
    cat(paste0("iteration: ", i, "\n"))
  }
}
  
#the ones with NA are the ones that have only one buy in or only sell in their transactions 

#merge
table_chips<- merge(table_chips,users,by="UserID")

summary(users$mean_buy_interval)


# 3.- DATA MART  -----------

dtmart <- merge(demo,daily2,by="UserID",all.x = T)
dtmart <- merge(dtmart,table_chips,by="UserID",all.x = T)

#fix a gender that is empty with male

dtmart$Gender<-ifelse(is.na(dtmart$Gender),"Male",dtmart$Gender)

write.csv(dtmart,"Data Mart Bet and Poker.csv",row.names = F)


# 4.- GRAPHICS  -----------
colors <- c('#D0CBCA', '#E5562A', '#336EF9','#151F47')

names(dtmart)

#Gender

dtmart %>% count(Gender) %>%
  plot_ly(x = ~Gender,y = ~n) %>% add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)")))

dtmart %>% group_by(Gender) %>% summarize(mean=mean(N_Cats_Played)) %>%
  plot_ly(x = ~Gender,y = ~mean) %>% add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)")))

########### 1 GENDER -----------------------
#OK
dtmart %>% count(Gender) %>%
  plot_ly(labels = ~Gender,values = ~n, type = "pie", textinfo = "label+percent", 
          showlegend = F) 

#Country
#OK
topCountries<-names(tail(sort(table(dtmart$Country_Name)),10))

dtmart %>% filter(Country_Name %in%topCountries)%>% count(Country_Name) %>% arrange(n)  %>% 
  plot_ly(x = ~Country_Name,y = ~n) %>% add_bars(marker = list(color = c("rgb(21, 31, 71)"))) %>%
  layout(xaxis = list(categoryorder = "total descending"))

#map

#reference: https://plotly.com/r/choropleth-maps/
maps <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
names(maps)

df <- dtmart %>%  count(Country_Name)
names(df)[1]<-"COUNTRY"

df <- merge(df,maps,by="COUNTRY",all.x = T)

#fix some NAs

df$COUNTRY[is.na(df$CODE)]
df$CODE[df$COUNTRY=="USA"] <- "USA"
df$CODE[df$COUNTRY=="Russian Federation"] <- "RUS"
df$CODE[df$COUNTRY=="Moldavia"] <- "MDA"
df$CODE[df$COUNTRY=="Tunesia"] <- "TUN"
df$CODE[df$COUNTRY=="Holland"] <- "NLD"

########### 2 MAP -----------------------
#OK
plot_ly(df,type='choropleth', locations=df$CODE, z=df$n, text=df$Country_Name, colorscale="Greens")


#registered by date
dtmart<-data.table(dtmart)
regs<- dtmart[,.N,by=RegDate]

########### 3 REGISTERED BY DATE ----------------------- 
#OK

plot_ly(regs, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  add_trace(x = ~RegDate, y = ~N,marker = list(color = "rgb(21, 31, 71)"),
            line = list(color ='rgb(21,31,71'),
            fillcolor='rgba(50, 100, 250, 0.4)') %>%
  layout(showlegend = F) 

#first act by date
#OK
firstAct<- dtmart[,.N,by=FirstAct][order(FirstAct)]

plot_ly(firstAct, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~FirstAct, y = ~N,marker = list(color = "rgb(21, 31, 71)"),line = list(color ='rgb(21,31,71')) %>%
  layout(showlegend = F) 


########### 4 FIRST ACTIVITY AND FIRST PAY -----------------------
#OK

plot_ly(alpha=0.4) %>% add_histogram(dtmart$FirstPay, name="First Pay", marker = list(color = "rgba(21, 31, 71,0.8)")) %>%
  add_histogram(dtmart$FirstAct, name="First Activity",marker = list(color = "rgba(254, 95, 48,0.8)")) %>% 
  layout(barmode="stack",legend = list(orientation = 'h'))

#interval first pay 

########### 5 INTERVAL FIRST PAY -----------------------
#OK

plot_ly(alpha=0.4) %>% 
  add_histogram(dtmart[interval_firstPay<quantile(interval_firstPay,0.95)]$interval_firstPay, 
                name="Interval First Pay", marker = list(color = "rgb(21, 31, 71)"))



#first act by date

users_daily<- daily[,.(Users=uni(UserID)),by=Date][order(Date)]

users_daily1<- daily[ProductID==1][,.(Users_1=uni(UserID)),by=Date][order(Date)]
users_daily2<- daily[ProductID==2][,.(Users_2=uni(UserID)),by=Date][order(Date)]
users_daily4<- daily[ProductID==4][,.(Users_4=uni(UserID)),by=Date][order(Date)]
users_daily5<- daily[ProductID==5][,.(Users_5=uni(UserID)),by=Date][order(Date)]
users_daily6<- daily[ProductID==6][,.(Users_6=uni(UserID)),by=Date][order(Date)]
users_daily7<- daily[ProductID==7][,.(Users_7=uni(UserID)),by=Date][order(Date)]
users_daily8<- daily[ProductID==8][,.(Users_8=uni(UserID)),by=Date][order(Date)]

users_daily_p<- chips[,.(Poker_Users=uni(UserID)),by=as.Date(TransDateTime)][order(as.Date)]
names(users_daily_p)[1] <-"Date" 

users_daily<-merge(users_daily,users_daily1,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily2,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily4,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily5,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily6,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily7,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily8,by="Date",all.x=T,all.y=T)
users_daily<-merge(users_daily,users_daily_p,by="Date",all.x=T,all.y=T)



plot_ly(users_daily, type = 'scatter', mode = 'lines')%>%
  add_trace(x = ~Date, y = ~Users_1,line = list(color ='skyblue')) %>%
  add_trace(x = ~Date, y = ~Users_2,line = list(color ='lightblue')) %>%
  add_trace(x = ~Date, y = ~Users_4,line = list(color ='rgb(21, 31, 71')) %>%
  add_trace(x = ~Date, y = ~Users_5,line = list(color ='rgb(21, 31, 71')) %>%
  add_trace(x = ~Date, y = ~Users_6,line = list(color ='rgb(21, 31, 71')) %>%
  add_trace(x = ~Date, y = ~Users_7,line = list(color ='rgb(21, 31, 71')) %>%
  add_trace(x = ~Date, y = ~Users_8,line = list(color ='rgb(21, 31, 71')) %>%
  add_trace(x = ~Date, y = ~Poker_Users,line = list(color ='rgb(254, 95, 48')) %>%
  layout(showlegend = T) 

#lets try mean daily users by product
########### 6 MEAN DAILY USERS BY PRODUCT -----------------------
#OK

mean_daily<- daily[,.(Users=uni(UserID),Product=h(Product_Name)),by=paste(Date,ProductID)][,.(Average=mean(Users)),by=Product]
mean_daily_p<- chips[,.(Users=uni(UserID),Product="Poker"),by=as.Date(TransDateTime)][,.(Average=mean(Users)),by=Product]
mean_daily<-rbind(mean_daily,mean_daily_p)

plot_ly(mean_daily,x = ~Product,y = ~Average) %>% 
  add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)")))


#daily users by day in general 

########### 7 DAILY USERS BETS -----------------------
#OK

plot_ly(users_daily, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Users,line = list(color ='rgb(21, 31, 71)'),
            fillcolor='rgba(50, 100, 250, 0.4)') %>%
  layout(showlegend = F)

#daily users by day poker 

########### 8 DAILY USERS POKER -----------------------
#OK

plot_ly(users_daily, type = 'scatter', mode = 'lines', fill = 'tozeroy')%>%
  add_trace(x = ~Date, y = ~Poker_Users,line = list(color ='rgb(254, 95, 48)'),
            fillcolor='rgba(246, 143, 112, 0.4)') %>%
  layout(showlegend = F)

# languages

########### 9 LANGUAGES -----------------------
#OK

dtmart %>% count(Language_Name) %>% 
  plot_ly(x=~Language_Name,y=~n) %>% add_bars(marker = list(color = c(colors,colors,colors,colors))) %>%
  layout(xaxis = list(categoryorder = "total descending"))


#most active play in general# #piechart#

########### 10 MOST ACTIVE DAY BETS -----------------------
#OK

dtmart %>% count(MostActiveDay) %>%
  plot_ly(labels = ~MostActiveDay,values = ~n, type = "pie", textinfo = "label+percent",
          marker = list(color = c("#151F47", "#AA3344")),
          showlegend = F)

########### 11 RECENCY BETS -----------------------
#OK

#distribution of the receny
plot_ly(alpha=0.4) %>% add_histogram(dtmart$Recency, marker = list(color = "rgba(27, 63, 10,0.8)")) %>%
  layout(barmode="stack",legend = list(orientation = 'v'))


########### 12 ONE TIME PLAYERS BETS -----------------------
#OK

#one time players vs more active ones-- try to offer incentives to convert one time players into regular players#

dtmart %>% count(one_time_player) %>%
  plot_ly(x = ~one_time_player,y = ~n) %>% add_bars(marker = list(color = c("rgb(254, 95, 48)","rgb(21, 31, 71)")))


########### 13 BALANCE POKER PER DAY AND TIME OF DAY -----------------------
#OK

plot_ly(data = dtmart[MeanBuy<quantile(MeanBuy,0.99,na.rm=T)],
        x = ~MostActiveDayPoker, y = ~round(BalancePoker,2),
        color = ~PrefTimeOfDay, colors=colors) %>% 
  layout(yaxis=list(title="Balance"),xaxis=list(title=""),legend = list(orientation="h"))

########### 14 MEAN BUY POKER  TIME OF DAY -----------------------
#OK
plot_ly(dtmart[MeanBuy<quantile(MeanBuy,0.98,na.rm=T)], y=~MeanBuy, color= ~PrefTimeOfDay, type="box")  %>% 
  layout(showlegend = F)


########### 15 MEAN SELL POKER  TIME OF DAY -----------------------
#OK

plot_ly(dtmart[MeanSell<quantile(MeanSell,0.98,na.rm=T)], y=~MeanSell, color= ~PrefTimeOfDay, type="box") %>%
  layout(showlegend=F)

########### 16 PRODUCTS PER DAY -----------------------
#OK

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
  layout(yaxis = list(title = 'Count'),xaxis = list(categoryorder = "total descending"), barmode = 'stack')

########### 17 APPLICATION USED -----------------------
# _ Application_Name (pie chart or bar chart)
#OK

dtmart %>% count(Application_Name) %>%
  plot_ly(labels = ~Application_Name,values = ~n, type = "pie", textinfo = "label+percent",
          showlegend = F)
dtmart %>% count(Application_Name) %>%
  plot_ly(labels = ~Application_Name,values = ~n, textinfo = "label+percent",
          showlegend = F) %>% add_pie(hole = 0.4)

# 

########### 18 GAUGE BY PRODUCT  -----------------------
#OK

#we need to filter before grouping 

plot_ly(
  domain = list(x = c(0, 100), y = c(0, 100)),
  value = mean(dtmart$casino_profitability,na.rm=T)*100,
  title = list(text = "Profitability"), type = "indicator",
  mode = "gauge+number+delta", number = list(suffix = "%"),
  gauge = list(bar = list(color = "rgb(21,31,71)"),
               bordercolor = "gray"),
  delta = list(reference = 15, increasing = list(color = "darkgreen"),decreasing=list(color = "darkred"))) %>%
  layout(margin = list(l=20,r=30))

########### 19 POKER PLAYERS BY CATEGORIE  -----------------------

dtmart[Poker=="Yes"] %>% count(Player_Category) %>%
  plot_ly(labels = ~Player_Category,values = ~n, type = "pie", textinfo = "label+percent",
          showlegend = F)
 

########### 20 SELL RATIO BY CATEGORIE  -----------------------

dtmart[Poker=="Yes"][sell_ratio<quantile(sell_ratio,0.99,na.rm=T)] %>% 
  plot_ly(x=~Player_Category,y=~sell_ratio,type = "box",color= ~Player_Category) %>%
  layout(showlegend=F,yaxis = list(title = 'Sell Ratio (%)'), xaxis = list(title = 'Player Category'))



# *Customers / sources
# _ RegDate / FirstPay / FirstAct / FirstSp / (line) done!


#remove outliers for the plots
plot_ly(data = dtmart[MeanBuy<quantile(MeanBuy,0.99,na.rm=T)], 
        x = ~MeanBuy, y = ~BalancePoker, color = ~PrefTimeOfDay)

########### 21 MEAN BUYS AND SELLS BY TIME OF DAY  -----------------------
#OK

chipsb<-chips[TransType=="Buy"][,.(Buys=mean(TransAmount),time_of_day=h(time_of_day)),by=hour_of_day]
chipss<-chips[TransType=="Sell"][,.(Sells=mean(TransAmount)),by=hour_of_day]
chipss2<-merge(chipsb,chipss,by="hour_of_day")

plot_ly(chipss2, x=~Sells, y=~Buys, color=~time_of_day, size=~Buys, marker = list(opacity = 0.8),
        sizes=c(10,400))


########### FUNCTION TO SUMMARIZE USER DAILY  -----------------------

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
table(daily$Product_Name)
a<-summary_daily("Casino Chartwell")

########### FUNCTION TO SUMMARIZE POKER  -----------------------

chips[TransDateTime>=input$date_start3&TransDateTime<=input$date_end3][,.(Users=uni(UserID)),by=as.Date(TransDateTime)]

summary_chips <-function(start, end){
  
 chips$TransDateTime<-as.Date(chips$TransDateTime)
 chips_sub<- chips[TransDateTime>=start&TransDateTime<=end]
 sum_chips2<- chips_sub[,.(Users=uni(UserID)),by=TransDateTime]
 sum_chipsB<- chips_sub[TransType=="Buy"][,.(Buys=sum(TransAmount),MeanBuys=mean(TransAmount)),by=TransDateTime]
 sum_chipsS<- chips_sub[TransType=="Sell"][,.(Sells=sum(TransAmount),MeanSell=mean(TransAmount)),by=TransDateTime]
 
 sum_chips2<-merge(sum_chips2,sum_chipsB,by="TransDateTime",all.x=T)
 sum_chips2<-merge(sum_chips2,sum_chipsS,by="TransDateTime",all.x=T)
 sum_chips2<- sum_chips2[order(TransDateTime)]
 sum_chips2
 
} 

a<-summary_chips(start=as.Date("2005-02-01"),end=as.Date("2005-03-01"))


# select bar for each product
# multiple select bar:
#   _ Stakes Winnings Bets of each product
# _ average DaysPlayed of each product
# 
# 
# 
# * Poker
# _ MostActiveDayPoker
# 
# 
# * Revenue / Profit / Lost
# _ BuyAmount / SellAmount
# _ N_buys / N_sells
# 
# 
# 
# _ MeanBuy / MeanSell
# 
# _ sell_ratio
# 
# 
# 
# _ Player_Categorie
# 
# 
# 
# _ mean_buy_interval



#IDEAS FOR SHINY --------

# Daily (DAU) and monthly (MAU) Active Users
# stickiness (DAU/MAU)*30
# total revenue per user
#make a button to exclude germany and german language
#function to create own categories  by variables





