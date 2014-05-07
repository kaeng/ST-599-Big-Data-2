###########################
# Load EnvKatie
###########################

library(ggplot2); library(ggvis); library(dplyr)

dat2013 <- Avgs_by_month_local

#theme_set(theme_grey(base_size=18))

#pdf("2013plot.pdf",height=7,width=12)
#qplot(x=date,y=avg,data=alldata[alldata$year==2013,],geom="line",group=uniquecarrier,colour=uniquecarrier,
#      ylab="Average Departure Delay",ylim=c(-10,30),xlab="") + geom_hline(yintercept=0)
#dev.off()


# doing with ggplot, to model the ggvis
#h <- ggplot(dat2013, aes(date, avgarrdelay))
#h + geom_line(aes(group = uniquecarrier,colour=uniquecarrier))

# read in carrier codes
carriercodes <- read.csv("carriers.csv",header=TRUE,col.names=c("uniquecarrier","carrier"),
                         stringsAsFactors = FALSE)

new <- left_join(x=dat2013,y=carriercodes,by="uniquecarrier")

# creating label to only have carrier name appear once on plot
new <- new %.% mutate(carrier.y = ifelse(month==12,carrier,""))

# ggvis code to at least get the profile plot
ggvis(new, props(x = ~date, y = ~avgarrdelay, stroke = ~as.factor(carrier), strokeOpacity := .5)) +
  layer_line(props(strokeWidth.hover := 4, strokeWidth := 2, strokeOpacity.hover := 1)) +
  guide_legend(fill = "stroke") +
  layer_text(props(text := ~carrier.y, dx := 5, dy := 0, strokeOpacity := 0,
                   strokeOpacity.hover:= 1, fontSize := 5, fontSize.hover := 18))
