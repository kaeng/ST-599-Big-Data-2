# Are there any airlines with flight delays that are systematically worse
# than other airlines? Please examine this using 25 years of flight data for
# the entire country. 

library(RPostgreSQL)
library(dplyr)
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
#head(flights) 


#####################################
# For 2013 only
#####################################

# or you could use tbl_df to print it pretty
#tbl_df(head(flights))
delay=flights %.% select(year:dayofweek,uniquecarrier,depdelay,origin:dest) %.% filter(year=="2013")
tbl_df(head(delay))

# Getting number of flights by carrier
#num=delay %.% group_by(uniquecarrier) %.% summarise(num=n())

Avgs_by_month <- delay %.%
  group_by(uniquecarrier,year,month) %.%
  summarise(avg=mean(depdelay))

explain(Avgs_by_month)

Avgs_by_month_local <- collect(Avgs_by_month)

Avgs_by_month_local <- Avgs_by_month_local %.% mutate(date=ISOdate(year,month,1))


head(Avgs_by_month_local)
qplot(x=date,y=avg,data=Avgs_by_month_local,geom="line",group=uniquecarrier,colour=uniquecarrier)




######################################
#Experiment with for loop over years
######################################



# or you could use tbl_df to print it pretty
#tbl_df(head(flights))
alldata <- NA

for (i in 1987:1988){
delay=flights %.% select(year:dayofweek,uniquecarrier,arrdelay,depdelay,origin:dest) %.% filter(year==i)


# Getting number of flights by carrier
#num=delay %.% group_by(uniquecarrier) %.% summarise(num=n())

Avgs_by_month <- delay %.%
  group_by(uniquecarrier,year,month) %.%
  summarise(avg=mean(depdelay))

explain(Avgs_by_month)

Avgs_by_month_local <- collect(Avgs_by_month)

Avgs_by_month_local <- Avgs_by_month_local %.% mutate(date=ISOdate(year,month,1))

alldata <- rbind(alldata,Avgs_by_month_local)

print(i)
}

qplot(x=date,y=avg,data=alldata,geom="line",group=uniquecarrier,colour=uniquecarrier)
