# Are there any airlines with flight delays that are systematically worse
# than other airlines? Please examine this using 25 years of flight data for
# the entire country. 

# --- setting up parameters to access the data base --- #
endpoint <- "flights.cwick.co.nz"
user <- "student"
password <- "password"

ontime <- src_postgres("ontime", 
                       host = endpoint,
                       port = 5432,
                       user = user,
                       password = password)
# -------- #

flights <- tbl(ontime, "flights")
#' `flights` is special object, it points to the remote database, but
#' you can work with it in dplyr like it is a data.frame.
str(flights)

#' This isn't your usual head function, but it acts the same way:
head(flights) 
# or you could use tbl_df to print it pretty
tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,carrierdelay:lateaircraftdelay,origin:dest)
tbl_df(head(delay))
# Getting number of flights by carrier
num=delay %.% group_by(uniquecarrier) %.% summarise(num=n())
# Getting one carrier 
PS = as.data.frame(delay %.% filter(uniquecarrier=="PS"))
head(PS)
PS$date=paste(PS$dayofmonth,"-",PS$month,"-",PS$year,sep="")
dmy(PS$date)
? ISOdate
