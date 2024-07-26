#
# Function to convert JSON into data.table
#

json2tour <- function (fJSON, N = 1 ) {
  
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
    , asssit_l  = rep(0,n) # al
    , motor_p   = rep(0,n) # mw
    , rider_p   = rep(0,n) # rw
    , tm        = rep(0,n) # tm
    , ts        = rep(Sys.time(),n) # ts
    
  )
  
  print(nrow(temp))
  
  # if (n > 1000) { n = 1005  }
  
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


read_touren <- function ( fnames ) {
  
  #
  # Get first JSON-file and convert into data.table tour
  #
  
  tour_json <- jsonlite::read_json( paste0('data/',fnames[1]) ) 
  tour <- json2tour(tour_json, N = 1)
  
  # Create sub-title vector with name of first activity
  
  ttitles <- tour_json$activity_name
  
  # Read remaining JSON-files and append to tour
  
  for ( j in 2:length(fnames)) {
    
    # Get next tour
    
    tour_json <- jsonlite::read_json( paste0('data/', fnames[j] ) )
    
    # Convert to data.table and append to tour
    
    tour <- rbind( tour, json2tour( tour_json, N = j ) )
    
    # append activity name to sub-title
    
    ttitles <- c( ttitles, tour_json$activity_name )
    
  } # End of for loop
  
  # Convert tour number into factor with activity names (stitle)
  
  tour$tn = factor( tour$tn, levels = unique(tour$tn), labels= ttitles )
  
  return( list( titles = ttitles , touren = tour ))

}
