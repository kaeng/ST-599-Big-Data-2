library(dplyr)
library(RPostgreSQL)
library(ggplot2)

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
#str(flights)

#' This isn't your usual head function, but it acts the same way:
#head(flights) 
# or you could use tbl_df to print it pretty
#tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,carrierdelay:lateaircraftdelay,origin:dest)
<<<<<<< HEAD
#tbl_df(head(delay))


#########
# getting data set and stratum stats calculations
#########

# have EnvKatie.RData loaded

carriers2013 <- unique(Avgs_by_month_local$uniquecarrier)

get2013dat <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "US") %.% filter(random() < .001) %.% summarise(avgarrdelay = mean(arrdelay),
                                                                          stratsize = n())

#|
#           uniquecarrier == "MQ" |  #KT
#           uniquecarrier == "YV" |  #KT
#           uniquecarrier == "AS" |  #KT
#           uniquecarrier == "F9" |  #LW
#           uniquecarrier == "DL" |  #LW
#           uniquecarrier == "UA" |  #LW
#           uniquecarrier == "AA" |  #LW
#           uniquecarrier == "HA" |  #BB
#           uniquecarrier == "VX" |  #BB
#           uniquecarrier == "B6" |  #BB
#           uniquecarrier == "OO" |  #BB
#           uniquecarrier == "FL" |  #FW
#           uniquecarrier == "9E" |  #FW
#           uniquecarrier == "EV" |  #FW
#           uniquecarrier == "WN")   #FW
#%.% group_by(uniquecarrier)

  
explain(get2013dat)


sample2013 <- collect(get2013dat)

