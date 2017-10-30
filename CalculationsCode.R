setwd("C:/Users/meglarse/GitHub/wickham-thesis/")

# Install packages for the document

## install.packages("ggplot2")
require(ggplot2)

# Package options
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

## ggplot theme and figure parameters
theme_std <- function (base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.ticks = element_line(colour = "black", size = 1), 
          legend.key = element_rect(colour = "grey80"), 
          panel.background = element_rect(fill = "white", colour = NA), 
          panel.border = element_rect(fill = NA, colour = NA),
          axis.line = element_line(size = 1, colour = "black"),
          panel.grid.major = element_line(NA), 
          panel.grid.minor = element_line(NA), 
          strip.background = element_rect(fill = "grey80", colour = "grey50", size = 0.2),
          axis.text  = element_text(size=rel(0.9)),
          axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm"),size=rel(1)),
          axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm"),size=rel(1), angle = 90),
          strip.text = element_text(size = rel(1.15), colour = "black", face = "bold"),
          plot.margin=unit(c(10,10,10,10),"pt")
    )
}

theme_set(theme_std())

test.plot <- ggplot(mtcars, aes(x = hp , y = mpg, col = mpg)) +
  geom_point()
print(test.plot)

# Data Conversion ####
dat <- read.csv("./data/result.csv", header = TRUE)
str(dat)   # What do you notice?

# Do some data cleanup for this analysis
dat$ResultMeasureValue <- as.numeric(as.character(dat$ResultMeasureValue))

## Convert ActivityStartDate to a Juilan value to look at annual and decadal patterns
require(lubridate)
require(ggplot2)

dat$tmp <- as.Date(dat$ActivityStartDate, format = "%Y-%m-%d")  #
dat$yr <- as.numeric(format(dat$tmp,'%Y'))                      # 
dat$yr.fac <- as.factor(as.numeric(format(dat$tmp,'%Y')))       #
dat$mo <- as.factor(format(dat$tmp,'%m'))
dat$jul.dat <- yday(dat$tmp)                                    #
dat$wk <- week(dat$tmp)

dat <- dat[dat$ActivityMediaName != "Sediment",]

## Subset the microcystins data
bmp <- dat[dat$CharacteristicName == "Microcystin"& 
             dat$OrganizationIdentifier == "21NEB001_WQX",] 
head(bmp)

# Remove quality control samples
bmp <- bmp[-grep("Quality Control",bmp$ActivityTypeCode),]
unique(bmp$ActivityTypeCode)
dim(bmp)

# Check and onvert units if needed 
unique(bmp$DetectionQuantitationLimitMeasure.MeasureUnitCode)
## 1 ug/L = 1 ppb

# Check error codes
unique(bmp$ResultCommentText)
dim(bmp[bmp$ResultCommentText == "Sampled for, but analysis lost or not used for not meeting QC criteria. Accompanying value is not meaningful for analysis.",])
bmp <- bmp[bmp$ResultCommentText != "Sampled for, but analysis lost or not used for not meeting QC criteria. Accompanying value is not meaningful for analysis.",]

unique(bmp$ResultDetectionConditionText)
head(bmp[bmp$ResultDetectionConditionText == "Not Reported",])
head(bmp[bmp$ResultDetectionConditionText == "Below Reporting Limit",])

# change all not reported to 0
#bmp$ResultMeasureValue[bmp$ResultDetectionConditionText == "Not Reported"] <- 0
#bmp$ResultMeasureValue[bmp$ResultDetectionConditionText == "Below Reporting Limit"] <- 0


# detection limit is 0.15, change all values less than 0.15 to 0
#bmp$ResultMeasureValue[bmp$ResultMeasureValue <0.15] <- 0
bmp <- bmp[bmp$yr < 2016,]

## Let's Visualize our data ----
ggplot(bmp, aes(x = yr, y = ResultMeasureValue)) +
  geom_jitter(cex = 1, col = "grey50", width = 0.2, height = 0.1)  +
  stat_summary(fun.data = "mean_cl_boot", color = "black", size = 1) +
  #geom_point(data = bmp[bmp$ResultMeasureValue <=0.15,],aes(yr,ResultMeasureValue),color = "red") +
  ylab(expression(Microcystin ~ Concentration ~ (mu*g/L))) +
  xlab("Year") 

ggplot(bmp, aes(x = yr.fac, y = ResultMeasureValue)) +
  geom_boxplot()

ggplot(bmp, aes(x = mo, y = ResultMeasureValue)) +
  geom_point() +
  #geom_jitter(cex = 1, col = "grey50", width = 0.2, height = 0.1)  +
  #stat_summary(fun.data = "mean_cl_boot", color = "black", size = 1) +
  geom_point(data = bmp[bmp$ResultMeasureValue <=0.15,],aes(mo,ResultMeasureValue),color = "red") +
  ylab(expression(Microcystin ~ Concentration ~ (mu*g/L))) +
  xlab("Julian Date") 
ggplot(bmp, aes(x = mo, y = ResultMeasureValue)) +
  geom_boxplot()

ggplot(bmp, aes(x = jul.dat, y = ResultMeasureValue)) +
  geom_point() +
  #geom_jitter(cex = 1, col = "grey50", width = 0.2, height = 0.1)  +
  #stat_summary(fun.data = "mean_cl_boot", color = "black", size = 1) +
  geom_point(data = bmp[bmp$ResultMeasureValue <=0.15,],aes(jul.dat,ResultMeasureValue),color = "red") +
  ylab(expression(Microcystin ~ Concentration ~ (mu*g/L))) +
  xlab("Julian Date") 


# Let's do some calculations for FREQUENCY ####
## Notes for Megan: Is it more appropropriate to calculate data in graphic with all data rather than by %Detected/lake/season?

## Detectable Microcystins ----
#Q: What percentage of samples had detectable microcystins (> 0.15 $\mu$/L for ELISA? What percentage of samples had total microcystin concentrations exceeding the WHO limit?
## What percentage of samples have detectable microcystins
#For each location > for each year > for each month within a year

loc.list1 <- c()
loc.list2 <-c()
yr.list1 <- c() 
yr.list2 <- c() 
mo.list <- c() 
yr.perc.list <- c() 
mo.perc.list <- c()
yr.above.list <-c()
yr.total.list <-c()
mo.above.list <-c()
mo.total.list <-c()

for(i in unique(bmp$MonitoringLocationIdentifier)){
  tmp <- bmp[bmp$MonitoringLocationIdentifier == i,]
  
  
  for(j in unique(tmp$yr.fac)){
    yr.above <- dim(tmp[tmp$yr.fac == j & tmp$ResultMeasureValue > 0.15,])[1]
    yr.total <- dim(tmp[tmp$yr.fac == j,])[1]
    yr.perc <- round((yr.above/yr.total)*100, digits = 2)
    
    
    yr.above.list <- append(yr.above.list, yr.above)
    yr.total.list <- append(yr.total.list, yr.total)
    
    yr.list1 <- append(yr.list1, rep(j,length(yr.perc)))
    yr.perc.list <- append(yr.perc.list, yr.perc)
    loc.list1 <- append(loc.list1, rep(i, length(yr.perc)))
    
    for(k in unique(tmp$mo)){
      tmp2 <- tmp[tmp$mo == k & tmp$yr.fac == j,]
      mo.above <- dim(tmp2[tmp2$ResultMeasureValue > 0.15,])[1]
      mo.total <- dim(tmp2)[1]
      mo.perc <- round((mo.above/mo.total)*100, digits = 2)
      
      mo.above.list <- append(mo.above.list, mo.above)
      mo.total.list <- append(mo.total.list, mo.total)
      
      yr.list2 <- append(yr.list2, rep(j, length(mo.perc)))
      mo.list <- append(mo.list, rep(k, length(mo.perc)))
      mo.perc.list <- append(mo.perc.list, mo.perc)
      loc.list2 <- append(loc.list2, rep(i, length(mo.perc)))
    }
  }
}

yr.perc <- data.frame(loc.list1, yr.list1, yr.above.list, yr.total.list,yr.perc.list)
mo.perc <- data.frame(loc.list2, yr.list2, mo.list, 
                      mo.above.list, mo.total.list,mo.perc.list)

# information summary 
yr.ag1 <- aggregate(yr.above.list ~ loc.list1, data = yr.perc, sum)
yr.ag2 <- aggregate(yr.total.list ~ loc.list1, data = yr.perc, sum)

ggplot(yr.perc, aes(yr.list1,yr.perc.list))+geom_jitter()+ylim(0,100)

## Above NE limit ----
#For each location > for each year > for each month within a year

loc.list1 <- c()
loc.list2 <-c()
yr.list1 <- c() 
yr.list2 <- c() 
mo.list <- c() 
yr.perc.list <- c() 
mo.perc.list <- c()
yr.above.list <-c()
yr.total.list <-c()
mo.above.list <-c()
mo.total.list <-c()

for(i in unique(bmp$MonitoringLocationIdentifier)){
  tmp <- bmp[bmp$MonitoringLocationIdentifier == i,]
  
  
  for(j in unique(tmp$yr.fac)){
    yr.above <- dim(tmp[tmp$yr.fac == j & tmp$ResultMeasureValue >= 20,])[1]
    yr.total <- dim(tmp[tmp$yr.fac == j,])[1]
    yr.percent <- round((yr.above/yr.total)*100, digits = 2)
    
    
    yr.above.list <- append(yr.above.list, yr.above)
    yr.total.list <- append(yr.total.list, yr.total)
    
    yr.list1 <- append(yr.list1, rep(j,length(yr.percent)))
    yr.perc.list <- append(yr.perc.list, yr.percent)
    loc.list1 <- append(loc.list1, rep(i, length(yr.percent)))
    
    for(k in unique(tmp$mo)){
      tmp2 <- tmp[tmp$mo == k & tmp$yr.fac == j,]
      mo.above <- dim(tmp2[tmp2$ResultMeasureValue >= 20,])[1]
      mo.total <- dim(tmp2)[1]
      mo.percent <- round((mo.above/mo.total)*100, digits = 2)
      
      mo.above.list <- append(mo.above.list, mo.above)
      mo.total.list <- append(mo.total.list, mo.total)
      
      yr.list2 <- append(yr.list2, rep(j, length(mo.percent)))
      mo.list <- append(mo.list, rep(k, length(mo.percent)))
      mo.perc.list <- append(mo.perc.list, mo.percent)
      loc.list2 <- append(loc.list2, rep(i, length(mo.percent)))
    }
  }
}

yr.perc2 <- data.frame(loc.list1, yr.list1, 
                       yr.above.list, yr.total.list,
                       yr.perc.list)
mo.perc2 <- data.frame(loc.list2, yr.list2, mo.list, 
                       mo.above.list, mo.total.list,
                       mo.perc.list)

ggplot(yr.perc2, aes(yr.list1,yr.perc.list))+geom_jitter()+ylim(0,100)+stat_summary("mean_se")

yr.perc2.ag1 <- aggregate(yr.above.list ~ loc.list1, data = yr.perc2, sum)
yr.perc2.ag2 <- aggregate(yr.total.list ~ loc.list1, data = yr.perc2, sum)

# Merge data and visualize ####
# Merge data frames for detected and above limit

dat2 <- merge(yr.perc,yr.perc2, by = c("loc.list1","yr.list1"))
dat2 <- dat2[,c(1,2,5,8)]
colnames(dat2) <- c("MonitoringLocationIdentifier", "Year", "DetMic", "WHOMic")

dat3 <- dat2 %>%
  group_by(Year) %>%
  summarize(MeanDetMic = mean(DetMic),
            MeanWHOMic = mean(WHOMic))

p = ggplot(data = dat3, aes(Year, MeanDetMic)) +
  scale_x_discrete(breaks = c(2010,2011,2012,2013,2014)) + 
  geom_bar(stat = "identity") +
  geom_bar(data = dat3, aes(Year,MeanWHOMic), 
           stat = "identity", col = "white",fill = "red") +
  #facet_wrap(~MonitoringLocationIdentifier, ncol = 2) +
  ylab("Percent Detected") +
  theme(strip.text = element_text(size = rel(1.15), colour = "black"))
print(p)

