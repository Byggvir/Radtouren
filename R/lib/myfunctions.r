Wochentage <- c("Mo","Di","Mi","Do","Fr","Sa","So")
WochentageLang <- c("Montag","Dienstag","Mittwoch","Donnerstag","Freitag","Samstag","Sonntag")
Monatsnamen <- c("Januar","Februar","März","April","Mai","Juni","Juli","August","September","Oktober","November","Dezember")


limbounds <- function (x, zeromin=TRUE) {
  
  if (zeromin == TRUE) {
    range <- c(0,max(x,na.rm = TRUE))
  } else
  { range <- c(min(x, na.rm = TRUE),max(x,na.rm = TRUE))
  }
  if (range[1] != range[2])
  {  f <- 10^(floor(log10(range[2]-range[1])))
  } else {
    f <- 1
  }
  
  return ( c(floor(range[1]/f),ceiling(range[2]/f)) * f) 
}