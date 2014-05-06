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

#tbl_df(head(delay))


#########
# getting data set and stratum stats calculations
#########

# have EnvKatie.RData loaded

carriers2013 <- unique(Avgs_by_month_local$uniquecarrier)

get2013dat <- delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == "US") %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())

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

  
explain(get2013dat)


sample2013 <- collect(get2013dat)



##############################
# Lu's Code
##############################


##############################
# Ben's Code
##############################

get2013dat=data.frame(sampleavg=0,samplevar=0,samplesize=0)
for (i in c("US","MQ","YV","AS","F9","DL","UA","AA","HA","VX","B6","OO","FL","9E","EV","WN")){
  get2013dat <- rbind(get2013dat, collect(delay %.% 
  select(year,uniquecarrier,arrdelay,depdelay) %.%
  filter(uniquecarrier == i) %.% filter(random() < .001) %.% 
  summarise(sampleavg = mean(arrdelay), samplevar = var(arrdelay), samplesize = n())))
  print(i)
}
##############################
# FangWu's Code
##############################


