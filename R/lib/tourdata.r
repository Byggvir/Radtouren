#
# Function to convert JSON into data.table
#


locnames = c( "al", "aw", "cp", "gd", "ge", "la", "lo", "mp", "mw", "or", "rw", "sl", "sp", "tm", "ts" )

json2tour <- function (fJSON ) {
  
  # Init data table
  
  n = length(fJSON$locations)
  
  temp = data.table(
    
      al = rep(0,n) # al Unterstützungslevel
    , aw = rep( '', n ) # Alarm
   # , bt = rep( '', n ) # Batterie
    , cp = rep(0,n) # cp
    , gd = rep(0,n) # gd Distanz ab Start
    , ge = rep(0,n) # ge Elevation
    , la = rep(0,n) # la latitude
    , lo = rep(0,n) # lo Longitude
    , mp = rep(0,n) # mw Motor Power?
    , mw = rep(0,n) # mw Motor Watt
    , or = rep(0,n) # or ??
    , rw = rep(0,n) # rw
    , sl = rep(0,n) # sl 
    , sp = rep(0,n) # sp
    , tm = rep(0,n) # tm
    , ts = rep('',n) # ts
    
  )
  
  print(nrow(temp))
  
  for (i in 1:n ) {
    
    for (j in 1:length(locnames) ) {
      
      vnames = names(fJSON$locations[[i]])
      
      if ( ! is_empty(which( locnames[j] == vnames ) ) ) {
         
       temp[i,j] = get(locnames[j],fJSON$locations[[i]])
       
      }
    }
    
  #   if ( ! is_empty(which( "cp" == vnames ) ) ) {
  #     temp$la[i] = fJSON$locations[[i]]$la
  #     temp$lo[i] = fJSON$locations[[i]]$lo
  #     temp$el[i] = fJSON$locations[[i]]$ge
  #     temp$gd[i] = fJSON$locations[[i]]$gd
  #     temp$sl[i] = fJSON$locations[[i]]$sl
  #     
  #     temp$sp[i] = fJSON$locations[[i]]$sp
  #     temp$cp[i] = fJSON$locations[[i]]$cp
  #     
  #     temp$al[i] = fJSON$locations[[i]]$al
  #     temp$mw[i] = fJSON$locations[[i]]$mw
  #     temp$rw[i] = fJSON$locations[[i]]$rw
  #     temp$tm[i] = fJSON$locations[[i]]$tm
  #     temp$ts[i] = as_datetime(fJSON$locations[[i]]$ts, tz = 'Europe/Berlin')
  #     
  #   }
  #   else {
  #     temp$aw[i] = 'PAUSE' 
  #   }
  
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
  return( list(titles = titles, locations = mtouren  ) )
  
}
