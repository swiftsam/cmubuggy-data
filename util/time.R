####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Time formating, etc
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# format seconds to MM:SS
# solution from Jeff@StackOverflow
# http://stackoverflow.com/questions/10835908/is-there-a-way-to-convert-mmss-00-to-seconds-00-in-r
SecondsToString <- function(x,digits=2){
  unlist(
    lapply(x,
           function(i){
             # fractional seconds
             fs <- as.integer(round((i - round(i))*(10^digits)))
             fmt <- ''
             if (i >= 3600)
               fmt <- '%H:%M:%S'
             else if (i >= 60)
               fmt <- '%M:%S'
             else
               fmt <- '%OS'

             i <- format(as.POSIXct(strptime("0:0:0","%H:%M:%S")) + i, format=fmt)
             if (fs > 0)
               sub('[0]+$','',paste(i,fs,sep='.'))
             else
               i
           }
    )
  )
}
