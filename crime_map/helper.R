library(leaflet)
library(leaflet.extras)
library(RSocrata)
library(lubridate)
library(shiny)
library(openair)
library(data.table)
library(rsconnect)
library(plyr)
library(stringr)
library(tidyr)
library(pivottabler)
library(basictabler)
library(dplyr)

#save the urls needed to access OB APIs
incident_url = c("https://data.baltimorecity.gov/resource/wsfq-mvij.csv?")
callsforservice_url = c("https://data.baltimorecity.gov/resource/xviu-ezkt.csv?")

#get the api login info
api_login <- as.character(read.delim("api_login.txt", sep = ",", header = FALSE))
email <- api_login[1]
password <- api_login[2]
app_token <- api_login[3]

#define function to get updated crime data from OpenBaltimore
get_new_data1 <- function(url1, d1, d2){
  ob_query <- paste(c("$where=crimedate between "), 
                    c("'"),d1,c("'"), 
                    c(" and "), 
                    c("'"),d2,c("'"),
                    sep="")
  df <- read.socrata(
    url = paste(url1, ob_query, sep=""),
    app_token = app_token,
    email = email,
    password = password)
  df1 <-  df[complete.cases(df[ ,c("longitude", "latitude")]),]
  return(df1)
}

#define function to get updated crime data from OpenBaltimore
get_new_data2 <- function(url1, d1, d2, district){
  ob_query <- paste(c("$where=crimedate between "), 
                    c("'"),d1,c("'"), 
                    c(" and "), 
                    c("'"),d2, c("'"),
                    c("&district='"),district,c("'"),
                    sep="")
  df <- read.socrata(
    url = paste(url1, ob_query, sep=""),
    app_token = app_token,
    email = email,
    password = password)
  df1 <-  df[complete.cases(df[ ,c("longitude", "latitude")]),]
  return(df1)
}

#define function to get updated 911 call data from OpenBaltimore WITH NEIGHBORHOOD FILTER
get_new_data3 <- function(url, d1, d2){
  ob_query <- paste(c("$where=calldatetime between "), 
                    c("'"),d1,c("'"), 
                    c(" and "), 
                    c("'"),d2,c("'"),
                    sep="")
  df <- read.socrata(
    url = paste(url, ob_query, sep=""),
    app_token = app_token,
    email = email,
    password = password)
  df_update2 <- df
  df_update2$x <- str_extract(df$location, "\\(.+?\\)")
  df_update3 <- df_update2[!is.na(df_update2$x),]
  df_update3 <- extract(df_update3, x, c("latitude", "longitude"), "\\(([^,]+), ([^)]+)\\)")
  df_update4 <- df_update3[rev(order(df_update3$calldatetime)),]
  df_update5 <- df_update4[!is.na(df_update4$calldatetime),]
  df_update5$latitude <- as.numeric(df_update5$latitude)
  df_update5$longitude <- as.numeric(df_update5$longitude)
  return(df_update5)
}

#define function to create lists of values to go into UI / arguments are the vector containing the names, the name of the eventual list
get_UI_list <- function(url, column_name) {
  #get OB data
  ob_query <- paste(c("$where=crimedate between "), 
                                 c("'"),c(paste(as.character(year(Sys.Date())), "-01", "-01", sep="")),c("'"), 
                                 c(" and "), 
                                 c("'"),c(Sys.Date()),c("'"),
                                 sep="")
  df <- read.socrata(
    url = paste(url, ob_query, sep=""),
    app_token = app_token,
    email = email,
    password = password)
  #save names of options to character vector
  y <- sort(
    as.character(
      unique(
        factor(
          df[,column_name]))))
  #convert character vector to list
  x <- vector(mode = "list", length = length(y))
  #name the list
  names(x) <- y
  #fill the list with the options you want to appear in your UI
  x[1:length(x)] <- y[1:length(y)]
  return(x)
}

#define function to create lists of values to go into UI / arguments are the vector containing the names, the name of the eventual list
get_UI_list2 <- function(url, column_name) {
  #get OB data
  ob_query <- paste(c("$where=calldatetime between "), 
                    c("'"),c(paste(as.character(year(Sys.Date())), "-01", "-01", sep="")),c("'"), 
                    c(" and "), 
                    c("'"), c(Sys.Date()),c("'"),
                    sep="")
  df <- read.socrata(
    url = paste(url, ob_query, sep=""),
    app_token = app_token,
    email = email,
    password = password)
  #save names of options to character vector
  y <- sort(
    as.character(
      unique(
        factor(
          df[,column_name]))))
  #convert character vector to list
  x <- vector(mode = "list", length = length(y))
  #name the list
  names(x) <- y
  #fill the list with the options you want to appear in your UI
  x[1:length(x)] <- y[1:length(y)]
  return(x)
}

#read KML file with Police District Boundaries into R
kml <- readr::read_file("Baltimore City Police District Boundary Map.kml")









