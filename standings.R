---
  title: "NHL Standings"
author: "Rich Carder"
date: "February 13, 2020"
output: html_document
---
  
 # install.packages("tidyverse")
#install.packages("googlesheets4")
  #install.packages("googlesheets4")
  #install.packages("formattable")
  #install.packages("htmltools")
  #install.packages("geojsonio")
  install.packages("curl")
install.packages("digest")
install.packages("googlesheets4")
  
  #devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(formattable)
library(kableExtra)
library(ggthemes)
library(knitr)
library(tidycensus)
library(htmltools)
library(webshot)
library(sf)
library(haven)
library(jsonlite)
library(geojsonio)
library(tidyverse)
#This script extracts ACS 5-year estimates at the block group (or any larger 
#geography) using the tidycensus package. To run tidycensus, you first need
#to set up a Census API key and run census_api_key(). Set working directory
#to where you want output files to save, or use the collect_acs_data function 
#to set a different outpath.
#

setwd("C:/Users/rcarder/Documents/dev/AMBFHL")

tourneydate<-"2020-06-13 12:00:00"

hotness<-read_sheet("https://docs.google.com/spreadsheets/d/1jfwFMbRqg6XfOwaC-WQd2naHw4-1J9c-F0FCV-TrAE4/edit#gid=1928466151")%>%
  dplyr::rename("Wk5"=3,"Wk6"=5,"Change"=6)%>%
  mutate(Wk5=sprintf("%.2f", round(Wk5,2)),
         Wk6=sprintf("%.2f", round(Wk6,2)))

nhl_raw<-read_sheet("https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1609610797")%>%
  mutate(id=row_number())%>%
  pivot_longer(cols=starts_with("Teams"),
               names_to="Player",
               values_to="side")%>%
  filter(!is.na(side))%>%
  mutate(WL=ifelse(side=="Home Team"&`Home Score`>`Away Score`,"W",
                   ifelse(side=="Home Team"&`Home Score`<`Away Score`,"L",
                          ifelse(side=="Away Team"&`Home Score`<`Away Score`,"W","L" ))))%>%
  mutate(W=ifelse(WL=="W",1,0),
         L=ifelse(WL=="L",1,0),
         OTL=ifelse(WL=="L"&`OT/SO`=="Yes",1,0),
         GF=ifelse(side=="Home Team",`Home Score`,`Away Score`),
         GA=ifelse(side=="Home Team",`Away Score`,`Home Score`))%>%
  mutate(GD=GF-GA)%>%
  mutate(Team=str_replace(Player,"Teams ",""))%>%
  mutate(Team=substring(Team,2))%>%
  mutate(Team=substring(Team,1,(str_length(Team)-1)))%>%
  mutate(OTL=ifelse(is.na(OTL),0,OTL))%>%
  mutate(RawScore=W-L+(.5*OTL)+(GD/10))%>%
  mutate(mult=ifelse(`Game Type`=="Exhibition",.5,
                     ifelse(`Game Type`=="Round Robin",1,
                            ifelse(`Game Type`=="Tournament Bracket"&WL=="W",1.5,
                                   ifelse(`Game Type`=="Championship"&WL=="W",2,1)))),
         rating=mult*RawScore)
  #filter(Timestamp>=as.POSIXct("2020-04-11 12:00:00")

UltimateStandings<-nhl_raw%>%
  group_by(Team)%>%
  summarize(W=sum(W),L=sum(L),OL=sum(OTL),GF=sum(GF),GA=sum(GA),GD=sum(GD),Rating=sum(rating))%>%
  mutate(GP=W+L,
         Pt=2*W+1*OL,
         UR=round(Rating/GP,2),
         offset=ifelse(UR>=0,20,-20))

tourneyStandings<-nhl_raw%>%
  filter(Timestamp>=as.POSIXct(tourneydate))%>%
  filter(`Game Type`!="Exhibition")%>%
  group_by(Team)%>%
  summarize(W=sum(W),L=sum(L),OL=sum(OTL),GF=sum(GF),GA=sum(GA),GD=sum(GD),Rating=sum(rating))%>%
  mutate(GP=W+L,
         Pt=2*W+1*OL,
         UR=round(Rating/GP,2),
         offset=ifelse(UR>=0,20,-20))


tourneygames<-nhl_raw%>%
  filter(Timestamp>=as.POSIXct(tourneydate))%>%
  filter(`Game Type`!="Exhibition")%>%
  group_by(id)%>%
  summarize(Away=paste(Team[side=="Away Team"],collapse=", "),
            AwayScore=first(`Away Score`),
            HomeScore=first(`Home Score`),
            Home=paste(Team[side=="Home Team"],collapse=", "),
            OT=first(`OT/SO`))

allgames<-nhl_raw%>%
 # filter(Timestamp>=as.POSIXct("2020-04-11 12:00:00"))%>%
  #filter(`Game Type`=="Round Robin")%>%
  group_by(id)%>%
  summarize(Date=format(first(as.Date(Timestamp)),format="%m-%d"),
            Away=paste(Team[side=="Away Team"],collapse=", "),
            A=first(`Away Score`),
            H=first(`Home Score`),
            Home=paste(Team[side=="Home Team"],collapse=", "),
            OT=first(`OT/SO`),
            Type=first(`Game Type`))

displaygames<-allgames%>%
  arrange(desc(Date))%>%
  mutate(otstring=ifelse(OT=="Yes","(OT)"," "))%>%
 mutate(otstring=replace(otstring,is.na(otstring),''),
        Home=paste(Home,otstring))%>%
  dplyr::select(2,3,4,5,6,8)

standings1<-tourneygames%>%
  mutate(homewin=ifelse(HomeScore>AwayScore,1,0),
         homeloss=ifelse(HomeScore<AwayScore,1,0),
         homeotl=ifelse(HomeScore<AwayScore&OT=="Yes",1,0),)%>%
  mutate(homeotl=ifelse(is.na(homeotl),0,homeotl))%>%
  group_by(Home)%>%
  summarize(W=sum(homewin),L=sum(homeloss),OTL=sum(homeotl),GF=sum(HomeScore),GA=sum(AwayScore))%>%
  dplyr::rename("Team"=1)%>%
  mutate(OTL=ifelse(is.na(OTL),0,OTL))

standings2<-tourneygames%>%
  mutate(awaywin=ifelse(HomeScore<AwayScore,1,0),
         awayloss=ifelse(HomeScore>AwayScore,1,0),
         awayotl=ifelse(HomeScore>AwayScore&OT=="Yes",1,0))%>%
  mutate(awayotl=ifelse(is.na(awayotl),0,awayotl))%>%
  group_by(Away)%>%
  summarize(W=sum(awaywin),L=sum(awayloss),OTL=sum(awayotl),GF=sum(AwayScore),GA=sum(HomeScore))%>%
  dplyr::rename("Team"=1)%>%
  mutate(OTL=ifelse(is.na(OTL),0,OTL))

standings<-bind_rows(standings1,standings2)%>%
  group_by(Team)%>%
  summarize(W=sum(W),L=sum(L),OTL=sum(OTL),GF=sum(GF),GA=sum(GA),GD=GF-GA)%>%
  mutate(Pts=(W*2)+OTL)

write.csv(standings,"standings.csv")


DisplayStandings<-UltimateStandings%>%
  dplyr::select(1,2,3,4,7,10,11)

tourneyStandings<-tourneyStandings%>%
  dplyr::select(1,2,3,4,7,10,11)

write.csv(DisplayStandings,"UltimateStandings.csv",row.names = FALSE)
write.csv(hotness,"hotness.csv",row.names = FALSE)

?write_sheet
write_sheet(DisplayStandings, ss = "https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1188732431", sheet = "DisplayStandings")
write_sheet(displaygames, ss = "https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1188732431", sheet = "displaygames")
write_sheet(tourneyStandings, ss = "https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1188732431", sheet = "tourneystandings")
