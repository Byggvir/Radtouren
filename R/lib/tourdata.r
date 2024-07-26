#
# Function to convert JSON into data.table
#

json2tour <- function (fJSON ) {
  
  # Init data table
  
  n = length(fJSON$locations)
  
  temp = data.table(
      aw = rep( '', n ) #Alarm
    , lat  = rep(0,n) # la
    , lon = rep(0,n) # lo
    , ele = rep(0,n) # ge
    , distance  = rep(0,n) # gd
    , slope     = rep(0,n) # sl 
    , speed     = rep(0,n) # sp
    , cadence   = rep(0,n) # cp
    , al        = rep(0,n) # al
    , motor_p   = rep(0,n) # mw
    , rider_p   = rep(0,n) # rw
    , tm        = rep(0,n) # tm
    , ts        = rep(Sys.time(),n) # ts
    
  )
  
  print(nrow(temp))
  
  for (i in 1:n ) {
    
    if ( ! is_empty(which( "cp" == names(fJSON$locations[[i]] ) ) ) ) {
      temp$lat[i]     = fJSON$locations[[i]]$la
      temp$lon[i]     = fJSON$locations[[i]]$lo
      temp$ele[i]     = fJSON$locations[[i]]$ge
      temp$distance[i]= fJSON$locations[[i]]$gd
      temp$slope[i]   = fJSON$locations[[i]]$sl
      
      temp$speed[i]   = fJSON$locations[[i]]$sp
      temp$cadence[i] = fJSON$locations[[i]]$cp
      
      temp$al[i]      = fJSON$locations[[i]]$al
      temp$motor_p[i] = fJSON$locations[[i]]$mw
      temp$rider_p[i] = fJSON$locations[[i]]$rw
      temp$tm[i]      = fJSON$locations[[i]]$tm
      temp$ts[i]      = as_datetime(fJSON$locations[[i]]$ts, tz = 'Europe/Berlin')
      
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
# Title of tour in JSON file = activity name
#

tour_title <- function( tour_json ) {
  
  return(tour_json$activity_name)
}

read_touren <- function ( fnames ) {
  
  #
  # Get first JSON-file and convert into data.table tour
  #
  
  tour_json <- lapply( fnames, function(x) jsonlite::read_json( x ) )
  touren <- lapply(tour_json, function (x) json2tour (x) )
  titles <- lapply(tour_json, function (x) tour_title(x) )
  
  for (i in 1:length(touren)) {
    touren[[i]]$tn = i
  }
  
  mtouren= touren[[1]]
  if ( length(touren) > 1) {
    
    mtouren <- touren[[1]]
    for (i in 2:length(touren)) {
      mtouren <- rbind(mtouren, touren[[i]])
    }
  }
  
  mtouren$tn = factor(mtouren$tn, levels = 1:length(touren) , labels = titles )
  return( list(titles = titles, touren = mtouren  ) )
  
}
