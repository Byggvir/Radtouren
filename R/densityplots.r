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
# Plot density rider power 
#

  tour$touren %>% 
    filter( rider_p >= 0 & aw != 'PAUSE' ) %>% 
    ggplot( aes( x = rider_p , group = tn, colour = tn ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Fahrerleistung', sep = '')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Fahrerleistung [W]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom'
    ) -> p1
  
  ggsave(  file = paste( outdir, 'density_rider_p.png', sep = '')
           , plot = p1
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

#
# Plot density motor power
#
  
  tour$touren  %>% 
    filter( motor_p > 0  & aw != 'PAUSE' ) %>% 
    ggplot( aes( x = motor_p, group = tn, colour = tn  ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Motorleistung', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Motorleistung [W]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom'
    ) -> p2
  
  ggsave(  file = paste( outdir, 'density_motor_p.png', sep = '')
           , plot = p2
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
#
# Plot density cadence
#
  
  tour$touren  %>% 
    filter( cadence > 0 & aw != 'PAUSE' ) %>% 
    ggplot( aes( x = cadence, group = tn, colour = tn  ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Trittfrequenz', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Trittfrequenz'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom'
      ) -> p3
  
  ggsave(  file = paste( outdir, 'density_cadence.png', sep = '')
           , plot = p3
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

#
# Plot density speed
#
  
  tour$touren  %>% 
    filter( speed > 0  & aw != 'PAUSE' ) %>% 
    ggplot( aes( x = speed, group = tn, colour = tn  ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Geschwindigkeit', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Geschwindigkeit [km/h]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom') -> p4
  
  ggsave(  file = paste( outdir, 'density_speed.png', sep = '')
           , plot = p4
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

#
# Plot density slope
#
  
  tour$touren  %>% 
    filter( aw != 'PAUSE' ) %>% 
    ggplot( aes( x = slope, group = tn, colour = tn  ) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste( 'Steigung', sep='')
           , subtitle = paste( stitle, collapse = '\n' )
           , x = 'Steigung [%]'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Touren'  ) +
    theme_ipsum() +
    theme( 
      legend.position = 'bottom') -> p5
  
  ggsave(  file = paste( outdir, 'density_slope.png', sep = '')
           , plot = p5
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )

#
# Plot 4 in 1
#


pa <- grid.arrange( p4, p3, p1, nrow = 1 )
ggsave(  file = paste( outdir, 'density.png', sep = '')
         , plot = pa
         , bg = "white"
         , width = 1920 * 3
         , height = 1080
         , units = "px"
         , dpi = 144 )

