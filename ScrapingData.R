
#For rbindlist in the moneyball function (combines list of dataframes into one big one)
library(data.table)

#For data manipulation
library(dplyr)
#Web scraping package
library(rvest)



fetch<-function(ballclub, season)
  #Start the function
{
  
  url <- paste("https://www.baseball-reference.com/teams/", ballclub, "/", season, "-schedule-scores.shtml", sep="")
  
  webpage<-read_html(url)
  
  schedule_tbl <- html_nodes(webpage, '#team_schedule .right , #team_schedule .left , .center')
  
  brTable <- html_text(schedule_tbl)
  
  columns<-c("Game", "Date", "Boxscore", "ProjTeam", "Home","Team",
             "W/L", "R","RA", "Innings", "Record", "Rank", "GB",
             "WinPitch", "LosePitch", "Save", "Duration", "Day", "Attendance", "Streak")
  
  mat <- matrix(brTable, ncol=20, byrow=TRUE, dimnames= list(NULL, columns))
  
  df <- as.data.frame(mat, col.names=FALSE)
  
  #before we do anything to the dataframe, let's impute the year as part of the function (change 1990 to 'year')
  df$Year<-season
  
  df<- df %>%
    filter(ProjTeam == ballclub)%>%
    select(Game,
           Date,
           Home,
           Team,
           Record,
           Rank,
           GB,
           WinPitch,
           LosePitch,
           Day,
           Attendance,
           Streak,
           Year)
  
  #Make specific columns factors
  columns_factor<-c("Game", "Rank")
  df[,columns_factor]<-apply(df[,columns_factor], 2, function(x){as.numeric(as.character(x))})
  
  #Do somethign special with Attendance because it needs to remove the comma
  df$Attendance <- as.numeric(sub(",", "", df$Attendance))
  
  #And do something special with GB so that when they're winning, it comes up as negative
  df$GB <- as.character(df$GB)
  df$GB <- sub("up ", "-", df$GB)
  df$GB <- sub("up", "-", df$GB)
  df$GB <- sub("Tied", "0", df$GB)
  df$GB <- as.numeric(df$GB)
  
  #Let's recode the "Home" column to "Location', similar to what we did in the first Jays dataset
  df<-df %>%
    mutate(Location = ifelse(as.character(Home)=="@","Away","Home"))
  
  #Start with Day Number
  df<-df %>%
    mutate(DayNum = sapply(strsplit(as.character(Date)," "), "[[", 3))
  
  #Now let's do Months
  
  df<-df %>% 
    mutate(Month = sapply(strsplit(as.character(df$Date)," "), "[[", 2)) 
  
  df$MonthNum<-recode(df$Month, 
                      Apr = "4",
                      May = "5",
                      Jun = "6",
                      Jul = "7",
                      Aug = "8",
                      Sep = "9",
                      Oct = "10")
  
  #And now we can put it back into a date
  df <- df %>% 
    mutate(Date = paste(Year, MonthNum, DayNum, sep='-'))
  
  #Change Streak to a numeric with negatives for losses
  df$Streak<- as.character(df$Streak)
  
  streak_num<-nchar(as.character(df$Streak))
  
  streak_type<-if_else(substring(df$Streak,1 ,1) == '-', "Lose", "Win")
  
  df <- df %>% 
    mutate(Streak = streak_num) 
  
  df <- df %>%
    mutate(Streak = ifelse(streak_type=="Lose", Streak*-1, Streak))
  
  temp_streak<-as.vector(0)
  for(i in 2:length(df$Streak))
  {
    temp_streak[i]<-df$Streak[i-1]
  }
  df$Streak <- temp_streak
  
  #Fix Day variable to be a character of either D or N
  df$Day <- as.character(df$Day)
  
  #Make the function return the columns that we need for analysis
  df <- df %>%
    select("Date",
           "Game",
           "Attendance",
           "Team",
           "Rank",
           "GB",
           "Day",
           "Streak",
           "WinPitch",
           "LosePitch",
           "Location")
  
  
  return(df)
  rm(list=ls())
  
  #End the function
}


test<-fetch("DET","2016")

#Create a function that performs fetch on multiple seasons and then binds them to one dataframe

moneyball<- function(ballclub, start, end){
  
  #First we'll generate the list of seasons to which we'll apply fetch
  seasons<- (as.vector(as.character(seq.int(from= start, to= end, by=1))))
  
  list<-lapply(seasons, FUN = function(x){fetch(ballclub, x)})
  
  df<-rbindlist(list)
  
  return(data.frame(df))
  rm(list=ls())
}

basic<-data.frame(moneyball("CHC", 2004, 2016))
