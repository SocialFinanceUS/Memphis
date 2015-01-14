## Crime stats for Shelby County 2012

# Set up for work
rm(list = ls())
setwd("C:/Users/ajutca/Google Drive/Team (Shared)/Advisory/Memphis_2014_Landscape/Background Materials/Recidivism")

# Import Data
mydata <- read.csv("crime_and_recid.csv") 

# Transform vars into logs
mydata[,2:4]<-lapply(mydata[,2:4], FUN=function(x) log(x))

# Regression + Scatterplot

reg1 <- lm(mydata$Poverty ~ mydata$Recidivism)
reg2 <- lm(mydata$Poverty ~ mydata$Arrest)
reg3 <- lm(mydata$Arrest ~ mydata$Recidivism)

par(cex=0.8)
library(ggplot2)
#install.packages("calibrate")
library(calibrate)

# set white background for plots
old <- theme_update(panel.background = element_rect(colour = "pink"))

# put new ZIP code vector that only contains the 3 highest and lowest recidivism ZIPs
mydata$ZIP[order(-mydata$Recidivism)[1:3]]
mydata$ZIP[order(mydata$Recidivism)[1:3]]

mydata$ZIP2 <- c(38126, 38105, NA, NA, NA, NA, 38107, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 38120, NA, 38138, NA, NA, 38139)
mydata$ZIP3 <- c("Arlington","Downtown", NA, NA, NA, NA, "North Memphis", NA, NA, NA, NA, NA, 
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "East Memphis", NA, "Germantown", NA, NA, "Germantown")

## PLOT RECIDIVISM AND POVERTY SCATTER + REGRESSION LINE & COEFFICIENTS
pdf("recid_pov.pdf")
p1<- ggplot(data = mydata, aes(x = Poverty, y = Recidivism, label = mydata$ZIP3)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point() + 
  geom_text(vjust=ifelse(mydata$ZIP3=="Downtown", -1,-0.5), hjust=ifelse(mydata$ZIP3 %in% c("Arlington","Downtown","North Memphis"),0.8, 0.3), size=4) +
  theme_grey()
p1

lm_eqn <- function(mydata){
  m<-lm(Recidivism ~ Poverty, mydata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

#p1.txt <- p1 + geom_text(aes(x = 1.25, y = 3.75, label = lm_eqn(mydata)), parse = TRUE) + xlab("log(Poverty)") + ylab("log(Recidivism)") + labs(title = "Relationship Between Poverty and Recidivism in Shelby County") 
#p1.txt
p1.txt <- p1 + xlab("log(Poverty)") + ylab("log(Recidivism)")  + labs(title = "Relationship Between Poverty and Recidivism in Shelby County") 
p1.txt
dev.off()

## PLOT ARRESTS AND POVERTY SCATTER + REGRESSION LINE & COEFFICIENTS
pdf("arrest_pov.pdf")
p2<- ggplot(data = mydata, aes(x = Poverty, y = Arrest, label = mydata$ZIP)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point() + geom_text(vjust=-.15)
p2

 
lm_eqn <- function(mydata){
  m <- lm(Arrest ~ Poverty, mydata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


p2.txt <- p2 + geom_text(aes(x = 3.2, y = 4, label = lm_eqn(mydata)), parse = TRUE) + xlab("log(Poverty)") + ylab("log(Arrest)") + labs(title = "Relationship Between Poverty and Arrest Rates in Shelby County")
p2.txt
dev.off()

## PLOT ARRESTS AND RECIDIVISM SCATTER + REGRESSION LINE & COEFFICIENTS
p3<- ggplot(data = mydata, aes(x = Arrest, y = Recidivism, label = mydata$ZIP)) +
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point() + geom_text(vjust=-.15)
p3


lm_eqn <- function(mydata){
  m<-lm(Recidivism ~ Arrest, mydata);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

p3.txt <- p3 + geom_text(aes(x = 4, y = 3.75, label = lm_eqn(mydata)), parse = TRUE) + xlab("log(Arrest)") + ylab("log(Recidivism)") + labs(title = "Relationship Between Recidivism and Arrest Rates in Shelby County")
p3.txt

### PLOTS WITHOUT FANCY GRAPHICS AND R^2 AND REGRESSION COEFFICIENTS
svg("recid_pov.svg")
plot(mydata$Recidivism, mydata$Poverty)
abline(reg1)
dev.off()

svg("arrest_pov.svg")
plot(mydata$Arrest, mydata$Poverty)
abline(reg2)
dev.off()

svg("arrest_recid.svg")
plot(mydata$Recidivism, mydata$Arrest)
abline(reg3)
dev.off()

### UNRELATED HISTOGRAM WORK FOR DC PROJECT
set.seed(01238)
n <- 100000
effect <- 0.4
se1 <- 0.1
se2 <- 0.125
se3 <- 0.15

set1<-rnorm(n,effect,se1)
q025.1<-quantile(set1,.025) #2 SDs
q975.1<-quantile(set1,.975) #2 SDs
q84.1<-quantile(set1,.84)   #1 SDs
q16.1<-quantile(set1,.16)   #1 SDs
q99.1<-quantile(set1,.995)  #3 SDs
q01.1<-quantile(set1,.005)  #3 SDs


set2<-rnorm(n,effect,se2)
q025.2<-quantile(set2,.025)
q975.2<-quantile(set2,.975)
q84.2<-quantile(set2,.84)
q16.2<-quantile(set2,.16)
q99.2<-quantile(set2,.995)
q01.2<-quantile(set2,.005)

set3<-rnorm(n,effect,se3)
q025.3<-quantile(set3,.025)
q975.3<-quantile(set3,.975)
q84.3<-quantile(set3,.84)
q16.3<-quantile(set3,.16)
q99.3<-quantile(set3,.995)
q01.3<-quantile(set3,.005)


### PLOT THE HISTOGRAM W/ 95% CIs 
plot(density(set1, bw="sj"), xlab="Reduction in Probability of Teen Pregnancy", ylab="Density", main="Simulations of Teen Pregnancy Reduction")
lines(density(set2, bw="sj"), col = "blue")
lines(density(set3, bw="sj"), col = "red")
abline(v=q975.1,col="black")
abline(v=q025.1,col="black")
abline(v=q975.2,col="blue")
abline(v=q025.2,col="blue")
abline(v=q975.3,col="red")
abline(v=q025.3,col="red")

legend(.7, 3, c("Std. Dev. = 0.1","Std. Dev. = 0.125","Std. Dev. = 0.15"), cex=0.8, col=c(1,4,2), lty=c(1), bty="n", title="Assumptions")



rbind(q025.1,q025.2,q025.3)
rbind(q975.1,q975.2,q975.3)
rbind(q16.1,q16.2,q16.3)
rbind(q84.1,q84.2,q84.3)
rbind(q01.1,q01.2,q01.3)
rbind(q99.1,q99.2,q99.3)

mymatrix<-as.data.frame(cbind(rbind(q01.1,q01.2,q01.3),
      rbind(q99.1,q99.2,q99.3),
      rbind(q025.1,q025.2,q025.3),
      rbind(q975.1,q975.2,q975.3),
      rbind(q01.1,q01.2,q01.3),
      rbind(q84.1,q84.2,q84.3)))
setwd("C:/Users/ajutca/Documents")
write.csv(mymatrix,"DCsims.csv")

