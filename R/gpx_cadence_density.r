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

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)
stitle <- paste ( 'Fahrradfahren')

cadence <- read.csv('data/touren.csv')

  cadence %>% ggplot( aes( x = cadence, group = Ziel, colour = Ziel) ) +
    stat_ecdf(
      # binwidth = 10 
      ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Trittfrequenz', sep='')
           , subtitle = stitle
           , x = 'Trittfrequenz'
           , y = 'Empirical Cumulative Distribution Function (ECDF)' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'Cadence_ECDF.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  
  cadence %>% ggplot( aes( x = cadence, group = Ziel, colour = Ziel) ) +
    geom_density(
      # binwidth = 10 
    ) +
    scale_x_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    scale_y_continuous( labels = function (x) format(x, big.mark = ".", decimal.mark= ',', scientific = FALSE ) ) +
    labs(  title = paste('Trittfrequenz beim Fahrradfahren', sep='')
           , subtitle = stitle
           , x = 'Trittfrequenz'
           , y = 'Dichte' 
           , caption = citation
           , colour = 'Legend'  ) +
    theme_ipsum() -> p
  
  ggsave(  file = paste( outdir, 'Cadence_density.png', sep = '')
           , plot = p
           , bg = "white"
           , width = 1920
           , height = 1080
           , units = "px"
           , dpi = 144 )
  