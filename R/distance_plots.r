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


#
# Constant values
# 

# Copyright

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)


#
# Get tours from data/*.json
#

tour = read_touren(fnames = dir('data', pattern = '*.json') )
stitle = tour$titles

#
# Plot elevation distance
#

  tour$touren %>% 
    filter( aw != 'PAUSE' ) %>% 
    ggplot( aes( x = distance / 1000, y = ele, group = tn, colour = tn ) ) +
    geom_line(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Höhenprofil', sep = '')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Distanz [km]'
           , y = 'Höhe [m]' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom'
    ) -> p1
  
  ggsave(  file = paste( outdir, 'tour_elevation.png', sep = '')
           , plot = p1
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

  #
  # Plot elevation distance
  #
  
  tour$touren %>% 
    filter( aw != 'PAUSE' ) %>% 
    ggplot( aes( x = distance / 1000, y = speed, group = tn, colour = tn ) ) +
    geom_line(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Geschwindigkeitsprofil', sep = '')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Distanz [km]'
           , y = 'Geschwindigkeit [km/h]' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom'
    ) -> p1
  
  ggsave(  file = paste( outdir, 'tour_speed.png', sep = '')
           , plot = p1
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  