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
str(flights)

#' This isn't your usual head function, but it acts the same way:
head(flights) 
# or you could use tbl_df to print it pretty
tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,carrierdelay:lateaircraftdelay,origin:dest)
tbl_df(head(delay))


xistair=df.a %.% filter(year=="2013")
xistair1=as.character(unique(xistair$uniquecarrier))
  
sampledat=delay %.% group_by(uniquecarrier) %.% 
  filter(uniquecarrier=="US"
         |uniquecarrier=="MQ"
         |uniquecarrier=="YV"
         |uniquecarrier=="AS"
         |uniquecarrier=="F9"
         |uniquecarrier=="DL"
         |uniquecarrier=="UA"
         |uniquecarrier=="AA"
         |uniquecarrier=="HA"
         |uniquecarrier=="VX"
         |uniquecarrier=="B6"
         |uniquecarrier=="OO"
         |uniquecarrier=="FL"
         |uniquecarrier=="9E"
         |uniquecarrier=="EV"
         |uniquecarrier=="WN")

newsamp=as.data.frame(sampledat)

