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
# Getting one carrier with data frame (takes long time for big carriers)
PS = as.data.frame(delay %.% filter(uniquecarrier=="PS"))
PS$date=ISOdate(PS$year,PS$month,PS$dayofmonth)
head(PS)
PSd = PS %.% group_by(date) %.% summarise(avgdel=mean(arrdelay,na.rm=TRUE))
qplot(date,avgdel,data=PSd,geom="line")
## Try SQL code...
delay2 = delay %.% mutate(date=cast(to_date(as.character(month) || as.character(year), 'MMYYYY')))
head(delay2)
### Does not work!

df.a=data.frame(uniquecarrier=c(0),year=c(0),month=c(0),avgdely=c(0))
for(i in 1987:1989){
  df.a=rbind(df.a,as.data.frame(delay %.% 
  filter(year==i) %.%
  group_by(uniquecarrier,year,month) %.% 
  summarise(avgdely=mean(arrdelay))))
  print(i)
  }
head(df.a)


df.a$date=ISOdate(df.a$year,df.a$month,1)

qplot(date,avgdely,data=df.a,group=uniquecarrier,color=uniquecarrier,geom="line")
