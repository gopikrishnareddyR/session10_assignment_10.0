#session10_assignment_10.1

#Import dataset from the following link: 
AirQuality Data Set
#Perform the following written operations:
#1.Read the file in Zip format and get it into R.


library(readr)

AirQualityUCI <- read_delim("AirQualityUCI.zip",";", quote = "'", 
  escape_double = FALSE,col_types = cols(AH = col_number(), 
`C6H6(GT)` = col_number(),`CO(GT)` = col_number(), RH = col_number(), 
   Time = col_character()), locale = locale(grouping_mark = ".", tz = ""), 
  na = "null", trim_ws = TRUE)
AirQualityUCI$Time<-gsub("\\.",":",AirQualityUCI$Time)
View(AirQualityUCI)

#2.Create Univariate for all the columns.
summary(AQU)
sample(nrow(AQU),20)
boxplot(AQU$`CO(GT)`,main="box plot", ylab="`CO(GT)`",col="purple")
hist(AQU$`CO(GT)`,col="gold")
rug(AQU$`CO(GT)`)

boxplot(AQU$`C6H6(GT)`,col="purple")

boxplot(AQU$`NO2(GT)`,col="purple")
hist(AQU$`NMHC(GT)`,col="gold")
hist(AQU$`NO2(GT)`,col="gold")

sampleAQU<- AQU[1:5000,] # we used first 100 rows
with(sampleAQU ,plot(`PT08.S2(NMHC)`, col=  "green"))
with(sampleAQU ,plot(AQU$`NO2(GT)`, col=  "blue"))


library(psych)
t<-describe(AQU)

#3.Check for missing values in all columns.

library(mice) 
md.pattern(AirQualityUCI) 
sum(is.na(AirQualityUCI))
colSums(is.na(AirQualityUCI))

#4.Impute the missing values using appropriate methods.
AirQualityUCI$X16<-NULL
AirQualityUCI$X17<-NULL
AQU<-AirQualityUCI[1:9357, ]
sum(is.na(AQU))

#5.Create bi-variate analysis for all relationships.
t.test(AQU$`PT08.S1(CO)`, AQU$`NOx(GT)`,paired = TRUE)
t.test(AQU$`C6H6(GT)`, AQU$`NOx(GT)`,paired = TRUE)
aov(AQU$`CO(GT)`~AQU$`PT08.S1(CO)`)
aov(AQU$`CO(GT)`~AQU$`NOx(GT)`)
summary(aov(AQU$`CO(GT)`~AQU$`PT08.S1(CO)`))
summary(aov(AQU$`CO(GT)`~AQU$`PT08.S2(NMHC)`))
summary(aov(AQU$`NO2(GT)`~AQU$T))

chisq.test(AQU$`CO(GT)`,AQU$`PT08.S1(CO)`, correct=FALSE)
chisq.test(AQU$RH,AQU$AH, correct=FALSE)

cor(AQU$`CO(GT)`,AQU$`PT08.S1(CO)`)
cor(AQU$RH,AQU$AH, use="complete.obs")
cor(AQU$`NO2(GT)`,AQU$T, use="complete.obs")
cor(AQU$`NO2(GT)`,AQU$`NOx(GT)`)
cor.test(AQU$`NO2(GT)`,AQU$`NOx(GT)`)
#6.Test relevant hypothesis for valid relations.

t.test(AQU$`PT08.S1(CO)`, AQU$`NOx(GT)`,paired = TRUE)
t.test(AQU$`C6H6(GT)`, AQU$`NOx(GT)`,paired = TRUE)
t.test(AQU$RH, AQU$AH,paired = TRUE)


#7.Create cross tabulations with derived variables.

with(AQU,tapply(T, list(RH=RH, AH=AH), mean))
with(AQU,tapply(T, list(RH=RH, AH=AH), sd))
with(AQU,tapply(Date, list(RH=RH, AH=AH), mean))


#8.Check for trends and patterns in time series.
library(timeSeries)

data<-matrix(c(AQU$`CO(GT)`,AQU$`PT08.S1(CO)`,AQU$`NMHC(GT)`),nrow = 9)
trend<-ts(data,frequency = 9)
trend

data1<-ts(AQU$`CO(GT)`, frequency = 12 )
decompose(data1)

par(mfrow=c(1,2))
plot(decompose(data1,type = "multi"))

#9.Find out the most polluted time of the day and the name of the chemical compound
library(fpp)



