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


#
# Function to convert JSON into data.table
#

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

#
# End of function
# 

#
# Constant values
# 

# Copyright

citation <- paste( '(cc by 4.0) 2024 by Thomas Arend; Stand:', heute)


#
# List files in data/*.jsom
#

fnames = dir('data', pattern = '*.json')

#
# Get first JSON-file an convert into data.table tour
#

nj <- jsonlite::read_json( paste0('data/',fnames[1]) ) 
tour <- json2dt(nj, N = 1)

# Create sub-title vector with name of first activity

stitle <- nj$activity_name

# Read remaining JSON-files and append to tour

for ( j in 2:length(fnames)) {
  
  # Get next tour
  
  nj <- jsonlite::read_json( paste0('data/', fnames[j] ) )
  
  # Convert to data.table and append to tour
  
  nt <- json2dt( nj, N = j )
  tour <- rbind( tour, nt )
  
  # append activity name to sub-title
  
  stitle <- c( stitle, nj$activity_name )
  
  }

# End of for loop

# Convert tour number into factor with activity names (stitle)

tour$tn = factor( tour$tn, levels = unique(tour$tn), labels= stitle )

#
# Plot density rider power 
#

  tour %>% 
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
  
  tour %>% 
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
  
  tour %>% 
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
  
  tour %>% 
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

