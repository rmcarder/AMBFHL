---
  title: "Gather EP Related ACS Data"
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

nhl_raw<-read_sheet("https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1609610797")

nhl_raw<-nhl_raw%>%
  mutate(id=row_number())%>%
  pivot_longer(cols=starts_with("Teams"),
               names_to="Player",
               values_to="side")%>%
  filter(!is.na(side))%>%
  mutate(WL=ifelse(side=="Home Team"&`Home Score`>`Away Score`,"W",
                   ifelse(side=="Home Team"&`Home Score`<`Away Score`,"L",
                          ifelse(side=="Away Team"&`Home Score`<`Away Score`,"W","L" ))))%>%
  mutate(Team=str_replace(Player,"Teams ",""))%>%
  filter(Timestamp>=as.POSIXct("2020-04-11 12:00:00"))

games<-nhl_raw%>%
  filter(Timestamp>=as.POSIXct("2020-04-11 12:00:00"))%>%
  group_by(id)%>%
  summarize(Away=paste(Team[side=="Away Team"],collapse=", "),
            AwayScore=first(`Away Score`),
            HomeScore=first(`Home Score`),
            Home=paste(Team[side=="Home Team"],collapse=", "),
            OT=first(`OT/SO`))

standings1<-games%>%
  mutate(homewin=ifelse(HomeScore>AwayScore,1,0),
         homeloss=ifelse(HomeScore<AwayScore,1,0),
         homeotl=ifelse(HomeScore<AwayScore&OT=="Yes",1,0),)%>%
  mutate(homeotl=ifelse(is.na(homeotl),0,homeotl))%>%
  group_by(Home)%>%
  summarize(W=sum(homewin),L=sum(homeloss),OTL=sum(homeotl),GF=sum(HomeScore),GA=sum(AwayScore))%>%
  dplyr::rename("Team"=1)%>%
  mutate(OTL=ifelse(is.na(OTL),0,OTL))

standings2<-games%>%
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
write.csv(games,"games.csv")

write_sheet(games, ss = "https://docs.google.com/spreadsheets/d/1hkVB4eg3x_jTpcbxqyRVuGmie4AnrNifczxVdi_wum4/edit#gid=1910726882", sheet = NULL)
         