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

dTS <- jsonlite::read_json('data/Rheinbach - Odendorf.json')

# Constant values

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)
stitle <- dTS$activity_name

# Init data table

n = length(dTS$locations)

# "cp": 0,
# "gd": 868,
# "ge": 193.66,
# "la": 50.6182385,
# "lo": 6.9523295,
# "mw": 0,
# "or": 74400,
# "rw": 0,
# "sl": 2.5,
# "sp": 19,
# "tm": 26,
# "ts": "2024-07-23 18:08:20.684"


tour = data.table(
  lat  = rep(0,n) # la
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

for (i in 1:n ) {
   
   tour$lat[i]       = dTS$locations[[i]]$la
   tour$lon[i]       = dTS$locations[[i]]$lo
   tour$ele[i]       = dTS$locations[[i]]$ge
   tour$distance[i]  = dTS$locations[[i]]$gd
   tour$slope[i]     = dTS$locations[[i]]$sl
   
   tour$speed[i]   = dTS$locations[[i]]$sp
   tour$cadence[i] = dTS$locations[[i]]$cp
   
   tour$motor_p[i] = dTS$locations[[i]]$mw
   tour$rider_p[i] = dTS$locations[[i]]$rw
   tour$tm[i]       = dTS$locations[[i]]$tm
   tour$ts[i]       = as_datetime(dTS$locations[[i]]$ts, tz = 'Europe/Berlin')
   
}

  tour %>% 
    filter( rider_p >= 0 ) %>% 
    ggplot( aes( x = rider_p ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Fahrerleistung', sep='')
           , subtitle = stitle
           , x = 'Fahrerleistung [W]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'density_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  tour %>% 
    filter( motor_p >= 0 ) %>% 
    ggplot( aes( x = motor_p ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Motorleistung', sep='')
           , subtitle = stitle
           , x = 'Motorleistung [W]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'density_motor_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  tour %>% 
    filter( cadence > 0 ) %>% 
    ggplot( aes( x = cadence ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Trittfrequenz', sep='')
           , subtitle = stitle
           , x = 'Trittfrequenz'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'density_cadence.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  tour %>% 
    filter( speed > 0 ) %>% 
    ggplot( aes( x = speed ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = stitle
           , x = 'Geschwindigkeit [km/h]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'density_speed.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  #
  # Scatterplots
  #
  
  tour %>% 
    filter( speed > 0 & cadence > 0 ) %>% 
    ggplot( aes( x = cadence , y = speed ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = stitle
           , x = 'Trittfrequenz [rpm]' 
           , y = 'Geschwindigkeit [km/h]'
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'cadence_speed.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  tour %>% 
    filter( cadence > 0 & rider_p > 0 ) %>% 
    ggplot( aes( x = cadence , y = rider_p ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = stitle
           , x = 'Trittfrequenz [rpm]' 
           , y = 'Fahrerleistung [W]'
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'cadence_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  tour %>% 
    filter( speed > 0 ) %>% 
    ggplot( aes( x = speed , y = rider_p ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = stitle
           , x = 'Geschwindigkeit [km/h]' 
           , y = 'Fahrerleistung [W]'
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'speed_rider_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  tour %>% 
    filter( speed > 0 & cadence > 0) %>% 
    ggplot( aes( x = speed , y = motor_p ) ) +
    geom_point(
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = stitle
           , x = 'Geschwindigkeit [km/h]' 
           , y = 'Motorleistung [W]'
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'speed_motor_p.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  