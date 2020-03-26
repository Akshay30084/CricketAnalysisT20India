library(rvest)
library(RSelenium)
library(stringr)
library(dplyr)
library(ggplot2)
library(NLP)


BattingBowlingSrorecardFunction=function(url_each,css,flag,as_html = TRUE)
{
  
  f = 1
  a = 1
  b = 2
  k = 3
  l = 4
  m = 5
  n = 6
  
  BattingBowlingData <-
    read_html(url_each) %>% html_nodes(css) %>%
    html_text()
  
  BattingBowlingDRemove <- BattingBowlingData[1:6]
  
  idx = which(BattingBowlingData %in% BattingBowlingDRemove)
  BattingBowlingData_new = BattingBowlingData[-idx]
  
  if (flag=="Batting")
  {
    RemoveBatting <- c(" Did not Bat ")
    idx = which(BattingBowlingData_new %in% RemoveBatting)
    BattingBowlingData_new = BattingBowlingData_new[-idx]
  }
  
  
  while (f <= length(BattingBowlingData_new))
  {
    if (a <= f)
    {
      Player_name <- BattingBowlingData_new[a]
      if (length(Player_name) == 0)
      Player_name <- "NA"
      PlayerName <- append(PlayerName, Player_name)
      Match_Title <- BattingBowlingData[1]
      FindIndexMatchTitle <- str_locate(Match_Title, " - ")
      MatchTitleActual <- substr(Match_Title, 1, FindIndexMatchTitle[, "start"])
      MatchTitle <- append(MatchTitle, MatchTitleActual)
      a = a + 6
    }
    if (b <= f)
    {
      Runs_Over_v <- BattingBowlingData_new[b]
      if (length(Runs_Over_v) == 0)
        Runs_Over_v <- "NA"
      Runs_Over <- append(Runs_Over, Runs_Over_v)
      
      b = b + 6
    }
    if (k <= f)
    {
      Balls_Runs_v <- BattingBowlingData_new[k]
      if (length(Balls_Runs_v) == 0)
        Balls_Runs_v <- "NA"
      Balls_Runs <- append(Balls_Runs, Balls_Runs_v)
      k = k + 6
    }
    if (l <= f)
    {
      Four_Wicket_v <- BattingBowlingData_new[l]
      if (length(Four_Wicket_v) == 0)
        Four_Wicket_v <- "NA"
      Four_Wicket <- append(Four_Wicket, Four_Wicket_v)
      l = l + 6
    }
    if (m <= f)
    {
      Six_Wide_v <- BattingBowlingData_new[m]
      if (length(Six_Wide_v) == 0)
        Six_v <- "NA"
      Six_Wide <- append(Six_Wide, Six_Wide_v)
      m = m + 6
    }
    if (n <= f)
    {
      SR_ECO_v <- BattingBowlingData_new[n]
      if (length(SR_ECO_v) == 0)
        SR_v <- "NA"
      SR_ECO <- append(SR_ECO, SR_ECO_v)
      n = n + 6
    }
    
    f = f + 1
  }
  
  
  if (flag=="Bowling")
  {
    
    ScorecardBowling<-
      data.frame(
        
        PlayerName=PlayerName,
        Overs=Runs_Over,
        RunsGiven=Balls_Runs,
        Wickets=Four_Wicket,
        Economy=SR_ECO,
        Wide=Six_Wide,
        MatchTitle=MatchTitle
      )
    
    return(ScorecardBowling)
    
  }
  
  else if (flag=="Batting")
    
  {
    SrorecardBatting <-
      data.frame(
        MatchTitle= MatchTitle,
        PlayerName = PlayerName,
        Runs = Runs_Over,
        Ball = Balls_Runs,
        Four = Four_Wicket,
        six = Six_Wide,
        SR = SR_ECO )
    
    return(SrorecardBatting)
  }
  
  
}

IndianTeamPlaying11=function(url_each,as_html = TRUE)
{
  
  IndiaSquad<-read_html(url_each)%>%html_nodes('.cb-minfo-tm-nm')%>%html_text()
  IndiaSquad_data_frame<-as.String(IndiaSquad)
  IndiaSquad_v<-as.vector(IndiaSquad_data_frame)
  str<-gsub("Playing  "," ",gsub("\n", ",", IndiaSquad_v))
  IndexIndia<-str_locate(str, "India")
  IndexIndiaBench<-as.data.frame(str_locate_all(pattern ='Bench  ', str))
  
  if(IndexIndia[, "start"]==1)
  {
    MatchSquad <- substr(str, IndexIndia[, "start"], IndexIndiaBench[1, "start"]-3)
  }  else
  {
    MatchSquad <- substr(str, IndexIndia[, "start"], IndexIndiaBench[2, "start"]-3)
  }
  MatchSquad_v <- as.vector(unlist(strsplit(MatchSquad,",")))
  IndiaPlaying11Team <-
    data.frame(
      PlayerName= MatchSquad_v[2:length(MatchSquad_v)]
    )
  return(IndiaPlaying11Team)
  
}


SummaryDataT20=function(link,as_html = TRUE)
{
  z = 1
  
  Match_Url_T20 <- read_html(link)
  Match_Name <- Match_Url_T20 %>% html_nodes('title') %>% html_text()
  MatchName = append(MatchName, Match_Name)
  
  Stedium_Name <-Match_Url_T20 %>% html_nodes('.text-gray span:nth-child(1)') %>% html_text()
  Stedium_Name <- gsub(",", " ", Stedium_Name)
  StediumName = append(StediumName, Stedium_Name)
  
  Result_Comment <-Match_Url_T20 %>% html_nodes('.cb-text-complete') %>% html_text()
  if (length(Result_Comment) == 0)
    Result_Comment <- "Match abandoned"
  Result = append(Result, Result_Comment)
  
  Player_Of_The_Match <-Match_Url_T20 %>% html_nodes('.cb-text-complete+ .cb-mom-itm .cb-link-undrln') %>%html_text()
  if (length(Player_Of_The_Match) == 0)
    Player_Of_The_Match <- "NA"
  PlayerOfTheMatch = append(PlayerOfTheMatch, Player_Of_The_Match)
  
  if(length(StediumName)!=0)
  {
    for (z in 1:length(StediumName))
    {
      if (((z %% 2) != 0))
      {
        Name <- StediumName[z]
        STADIUM_NAME = append(STADIUM_NAME, Name)
      }
      else
      {
        Addr <- StediumName[z]
        STADIUM_ADDRESS = append(STADIUM_ADDRESS, Addr)
      }
    }
  }
  
  FindIndexMatchname <- str_locate(MatchName, " - ")
  MatchNameActual <- substr(MatchName, 1, FindIndexMatchname[, "start"])
  
  FindIndexDate <- str_locate(MatchName, " commentary ")
  
  Date <-
    substr(MatchName, FindIndexMatchname[, "end"], FindIndexDate[, "start"])
  
  SummaryT20Data <-
    data.frame(
      MatchName = MatchNameActual,
      Date = Date,
      StadiumName = STADIUM_NAME,
      Stadium_Address = STADIUM_ADDRESS,
      Result = Result,
      PlayerOfTheMatch = PlayerOfTheMatch
    )
  return(SummaryT20Data)
}


i=1;
j=1;

SrorecardBatting_Final<-
  data.frame(MatchTitle=factor(),PlayerName=factor(), Runs=factor(),  Ball=factor(),Four=factor(),six=factor(),SR=factor(),stringsAsFactors = FALSE) 

SummaryDataFrame_Final<-
  data.frame(MatchName=factor(),Date=factor(),StadiumName=factor(),Stadium_Address=factor(),Result=factor(),PlayerOfTheMatch=factor(),stringsAsFactors = FALSE)          

SrorecardBowling_Final<-
  data.frame(PlayerName=factor(),Overs=factor(),RunsGiven=factor(),Wickets=factor(),Economy=factor(),Wide=factor(),  MatchTitle=factor(),stringsAsFactors = FALSE) 

IndiaPlaying11Team_Final<-
  data.frame(PlayerName=factor())

PlayerName <- c()
Runs_Over <- c()
Balls_Runs <- c()
Four_Wicket <- c()
Six_Wide <- c()
SR_ECO <- c()
MatchTitle<-c()

MatchName <- c()
StediumName <- c()
Result <- c()
PlayerOfTheMatch <- c()
STADIUM_NAME <- c()
STADIUM_ADDRESS <- c()


selServ <-
  wdman::selenium(
    retcommand = FALSE,
    port = 4444L,
    check = FALSE,
    geckover = "0.26.0"
  )
remDr <-
  remoteDriver(
    browserName = "firefox",
    port = 4444L,
    extraCapabilities = list(
      `moz:firefoxOptions` = list(binary = "C:/Program Files/Mozilla Firefox/firefox.exe")
    )
  )

remDr$open()
url <- "https://www.cricbuzz.com"
Crickbuzz <- remDr$navigate(url)
Archives <-remDr$findElement(using = 'css', ".cb-hm-mnu-itm~ .cb-hm-mnu-itm+ a")
Archives$clickElement()
Urlnew <- as.character(Archives$getCurrentUrl())
AllPastSeriesText<-read_html(Urlnew)%>%html_nodes('.cb-srs-lst-itm')%>%html_text() 
AllFilteredIndiaTextSeriesText<-grep("India",AllPastSeriesText, value = TRUE)
FilteredIndiaText<-grep("in India|India A", AllFilteredIndiaTextSeriesText, value = TRUE,invert = TRUE)
FindIndextext <- str_locate(FilteredIndiaText, ",")
IndiaText <- substr(FilteredIndiaText, 1, FindIndextext[, "start"]+5)
IndiaText_v<-as.vector(IndiaText)

while(i<=length(IndiaText_v))
{
  IndiaSeriesText<-IndiaText_v[i]
  NavigatingToSeries<-remDr$findElement(using ='link text',IndiaSeriesText)
  
  NavigatingToSeries$clickElement()
  SeriesUrl<-as.character(NavigatingToSeries$getCurrentUrl()) 
  T20MatchText<-read_html(SeriesUrl)%>%html_nodes(' .text-hvr-underline span')%>%html_text()
  T20MatchTextFiltered<-grep("T20I",T20MatchText, value = TRUE)
  T20Match_v<-as.vector(T20MatchTextFiltered)
  
  while(j<=length(T20Match_v))
  {
    
    T20Match_text<-T20Match_v[j]
    NavigatingToMatchT20<-remDr$findElement(using ='link text',T20Match_text)
    NavigatingToMatchT20$clickElement()
    Match<-NavigatingToMatchT20$getCurrentUrl()
    Matchchar<-as.character(Match)
    SummaryDataFrame<-SummaryDataT20(Matchchar) 
    SummaryDataFrame_Final<-bind_rows(SummaryDataFrame_Final,SummaryDataFrame)
    Sys.sleep(2)
    NavigatingToMatchT20Scorecard<-remDr$findElement(using ='css','.active+ .cb-nav-tab')
    NavigatingToMatchT20Scorecard$clickElement()
    url_each<-NavigatingToMatchT20Scorecard$getCurrentUrl()
    url_eachchar<-as.character(url_each) 
    
    IndiaPlying11<-IndianTeamPlaying11(url_eachchar)
    
    if(length(as.vector(IndiaPlying11))!=0)
    {
      
      IndiaPlaying11Team_Final<-bind_rows(IndiaPlaying11Team_Final,IndiaPlying11)
    }   
    
    flag<-"Batting"
    csstag<-".cb-scrd-itms .cb-col-27 , .cb-col-33~ .cb-col-8 , .cb-col-33+ .text-bold , .line-ht24"
    
    Battingdataframe<-BattingBowlingSrorecardFunction(url_eachchar,csstag,flag)
    
    
    
    if(length(as.vector(Battingdataframe))!=0)
    {
      #Battingdataframe<-inner_join(Battingdataframe,IndiaPlying11,by = "PlayerName") 
      SrorecardBatting_Final<-bind_rows(SrorecardBatting_Final,Battingdataframe)
    }
    
    flag<-"Bowling"
    csstag<-".cb-font-13+ .cb-ltst-wgt-hdr .text-bold.text-right~ .text-right+ .text-right , .line-ht24 , .text-right+ .text-bold , .cb-col-40+ .text-right , .cb-col-10 , .cb-col-40 .cb-text-link"
    BawlingDataframe=BattingBowlingSrorecardFunction(url_eachchar,csstag,flag)
    
    if(length(as.vector(BawlingDataframe))!=0)
    {
      #BawlingDataframe<-inner_join(BawlingDataframe,IndiaPlying11,by = "PlayerName") 
      SrorecardBowling_Final<-bind_rows(SrorecardBowling_Final,BawlingDataframe)
    } 
    
    
    Sys.sleep(2)
    
    NavigatingToMatchT20Scorecard$goBack()
    Sys.sleep(2)
    NavigatingToMatchT20Scorecard$goBack()
    
    j=j+1
  }
  
  Sys.sleep(2)
  NavigatingToSeries$goBack()
  
  Sys.sleep(2)
  
  i=i+1
  j= 1
  
}


remDr$close()


Summary<-data.frame(MatchName =SummaryDataFrame_Final$MatchName
                    ,Date=as.Date(trimws(SummaryDataFrame_Final$Date),format='%B %d, %Y')
                    ,StadiumName=SummaryDataFrame_Final$StadiumName
                    , Stadium_Address=SummaryDataFrame_Final$Stadium_Address
                    ,Result=SummaryDataFrame_Final$Result
                    ,PlayerOfTheMatch=SummaryDataFrame_Final$PlayerOfTheMatch)

Batting<-data.frame(MatchTitle=SrorecardBatting_Final$MatchTitle
                    ,PlayerName=trimws(as.character(SrorecardBatting_Final$PlayerName))
                    ,Runs=as.numeric(trimws(SrorecardBatting_Final$Runs))
                    ,Ball=as.numeric(trimws(SrorecardBatting_Final$Ball))
                    ,Four=as.numeric(trimws(SrorecardBatting_Final$Four))
                    ,six = as.numeric(trimws(SrorecardBatting_Final$six))
                    ,SR=as.double(trimws(SrorecardBatting_Final$SR)))


Bowling<-data.frame(PlayerName= trimws(as.character(SrorecardBowling_Final$PlayerName))
                    ,Overs= as.numeric(trimws(SrorecardBowling_Final$Overs))
                    ,RunsGiven =as.numeric(trimws(SrorecardBowling_Final$RunsGiven ))
                    ,Wickets= as.numeric(trimws(SrorecardBowling_Final$Wickets))
                    ,Economy= as.double(trimws(SrorecardBowling_Final$Economy ))
                    ,Wide= as.numeric(trimws(SrorecardBowling_Final$Wide))
                    ,MatchTitle =SrorecardBowling_Final$MatchTitle)     

IndiaPlaying11<-data.frame(PlayerName= trimws(as.character(IndiaPlaying11Team_Final$PlayerName)))
Bowling<-inner_join(Bowling,IndiaPlaying11%>%distinct(PlayerName),by="PlayerName")
Batting<-inner_join(Batting,IndiaPlaying11%>%distinct(PlayerName),by="PlayerName")


write.csv(Summary, "C:\\R Learning\\Practice\\Project\\CricketT20AnalyzerIndia\\CricketT20Dataset\\SummaryData.csv")

write.csv(Batting, "C:\\R Learning\\Practice\\Project\\CricketT20AnalyzerIndia\\CricketT20Dataset\\BattingData.csv")

write.csv(Bowling, "C:\\R Learning\\Practice\\Project\\CricketT20AnalyzerIndia\\CricketT20Dataset\\BowlingData.csv")
