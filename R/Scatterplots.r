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
source("R/lib/tourdata.r")
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

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)

#
# Get tours from data/*.json
#

tour = read_touren( fnames = dir('data', pattern = '*.json') )
stitle = tour$titles

  #
  # Scatterplots
  #

  tour$touren %>%
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

  tour$touren %>%
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

  tour$touren %>%
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

  tour$touren %>%
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


  tour$touren %>%
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
  
  