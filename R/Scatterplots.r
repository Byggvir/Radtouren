#!/usr/bin/env Rscript
#
#
# Script: yield.r
#
# Stand: 2023-07-08
# (c) 2023 by Thomas Arend, Rheinbach
# E-Mail: thomas@arend-rhb.de
#

require(data.table)
library(tidyverse)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(scales)
library(ragg)

library(jsonlite)

# Daten

# Set Working directory to git root

if (rstudioapi::isAvailable()){
  
  # When executed in RStudio
  SD <- unlist(str_split(dirname(rstudioapi::getSourceEditorContext()$path),'/'))
  
} else {
  
  #  When executing on command line 
  SD = (function() return( if(length(sys.parents())==1) getwd() else dirname(sys.frame(1)$ofile) ))()
  SD <- unlist(str_split(SD,'/'))
  
}

WD <- paste(SD[1:(length(SD)-1)],collapse='/')

setwd(WD)

source("R/lib/myfunctions.r")
source("R/lib/sql.r")

outdir <- 'png/'
dir.create( outdir , showWarnings = FALSE, recursive = FALSE, mode = "0777")

options( 
  digits = 7
  , scipen = 7
  , Outdec = "."
  , max.print = 3000
)

today <- Sys.Date()
heute <- format(today, "%d %b %Y")

td <- Sys.time()

# get data

json2dt <- function (fJSON, N = 1 ) {
  
  # Init data table
  
  n = length(fJSON$locations)
  
  cat(N,' ',  n, '\n' )
  
  temp = data.table(
      tn = rep(N,n) # tournumber
    , aw = rep( '', n ) #Alarm
    , lat  = rep(0,n) # la
    , lon = rep(0,n) # lo
    , ele = rep(0,n) # ge
    , distance  = rep(0,n) # gd
    , slope     = rep(0,n) # sl 
    , speed     = rep(0,n) # sp
    , cadence   = rep(0,n) # cp
    
    , motor_p   = rep(0,n) # mw
    , rider_p   = rep(0,n) # rw
    , tm        = rep(0,n) # tm
    , ts        = rep(Sys.time(),n) # ts
    
  )
  
  print(nrow(temp))
  
  # if (n > 1000) { n = 1005  }
  
  for (i in 1:n ) {
    
    if ( ! is_empty(which( "cp" == names(fJSON$locations[[i]] ) ) ) ) {
      temp$lat[i]       = fJSON$locations[[i]]$la
      temp$lon[i]       = fJSON$locations[[i]]$lo
      temp$ele[i]       = fJSON$locations[[i]]$ge
      temp$distance[i]  = fJSON$locations[[i]]$gd
      temp$slope[i]     = fJSON$locations[[i]]$sl
      
      temp$speed[i]   = fJSON$locations[[i]]$sp
      temp$cadence[i] = fJSON$locations[[i]]$cp
      
      temp$motor_p[i] = fJSON$locations[[i]]$mw
      temp$rider_p[i] = fJSON$locations[[i]]$rw
      temp$tm[i]       = fJSON$locations[[i]]$tm
      temp$ts[i]       = as_datetime(fJSON$locations[[i]]$ts, tz = 'Europe/Berlin')
      
    }
    else {
      temp$aw[i] = 'PAUSE' 
      }
  }
  
  return(temp)
  
}

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)

fnames = dir('data', pattern = '*.json')

nj <- jsonlite::read_json( paste0('data/',fnames[1]) ) 
tour <- json2dt(nj, N = 1)

stitle <- nj$activity_name

for ( j in 2:length(fnames)) {
  
  nj <- jsonlite::read_json( paste0('data/', fnames[j] ) )
  nt <- json2dt( nj, N = j )
  tour <- rbind( tour, nt )
  stitle <- c( stitle, nj$activity_name )
  
  }

tour$tn = factor( tour$tn, levels = unique(tour$tn), labels= stitle )


  #
  # Scatterplots
  #

  tour %>%
    filter( speed > 0 & cadence > 0 & aw != 'PAUSE' ) %>%
    ggplot( aes( x = cadence , y = speed, group = tn, colour = tn ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit vs Trittfrequenz', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Trittfrequenz [rpm]'
           , y = 'Geschwindigkeit [km/h]'
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() -> p

  ggsave(  file = paste( outdir, 'cadence_speed.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  tour %>%
    filter( cadence > 0 & rider_p > 0 & aw != 'PAUSE'  ) %>%
    ggplot( aes( x = cadence , y = rider_p, group = tn, colour = tn ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Fahrerleistung ~ Trittfrequenz', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Trittfrequenz [rpm]'
           , y = 'Fahrerleistung [W]'
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() -> p

  ggsave(  file = paste( outdir, 'cadence_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  tour %>%
    filter( speed > 0 & rider_p > 0 & aw != 'PAUSE' ) %>%
    ggplot( aes( x = speed , y = rider_p, group = tn, colour = tn ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Fahrerleistung ~ Geschwindigkeit', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Geschwindigkeit [km/h]'
           , y = 'Fahrerleistung [W]'
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() -> p

  ggsave(  file = paste( outdir, 'speed_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  tour %>%
    filter( aw != 'PAUSE' ) %>%
    ggplot( aes( x = slope , y = rider_p, group = tn, colour = tn ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Fahrerleitungs vs. Steigung', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Steigung [%]'
           , y = 'Fahrerleistung [W]'
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() -> p

  ggsave(  file = paste( outdir, 'slope_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )


  tour %>%
    filter( speed > 0 & aw != 'PAUSE' ) %>%
    ggplot( aes( x = slope , y = speed, group = tn, colour = tn ) ) +
    geom_smooth ( 
      method = 'glm'
      , formula = y ~ x
    ) +
    geom_point(
      ) +
    
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit vs Steigung', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Steigung [%]'
           , y = 'Geschwindigkeit [km/h]'
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'slope_speed_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  