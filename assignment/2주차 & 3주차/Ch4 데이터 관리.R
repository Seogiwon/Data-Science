setwd()
custdata <- read.table('custdata.csv',
                       header=TRUE,sep='\t')

summary(custdata[is.na(custdata$housing.type), 	# Note: 1 
                 c("recent.move","num.vehicles")]) 	# Note: 2

summary(custdata$housing.type)
summary(custdata$is.employed)

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),  	# Note: 1 
                                   "missing",                    	# Note: 2 
                                   ifelse(custdata$is.employed==T, 	# Note: 3 
                                          "employed",
                                          "not employed"))  	# Note: 4 

summary(as.factor(custdata$is.employed.fix)) 	# Note: 5 

custdata$is.employed.fix <- ifelse(is.na(custdata$is.employed),
                                   "not in active workforce",
                                   ifelse(custdata$is.employed==T,
                                          "employed",
                                          "not employed"))

summary(custdata$income)

meanIncome <- mean(custdata$income, na.rm=T) 	# Note: 1 
Income.fix <- ifelse(is.na(custdata$income),
                     meanIncome,
                     custdata$income)
summary(Income.fix)

breaks <-c(0, 10000, 50000, 100000, 250000, 1000000)           	# Note: 1 

Income.groups <- cut(custdata$income,
                     breaks=breaks, include.lowest=T)  	# Note: 2 

summary(Income.groups)                                        	# Note: 3 

Income.groups <- as.character(Income.groups)                   	# Note: 4 

Income.groups <- ifelse(is.na(Income.groups),                  	# Note: 5 
                        "no income", Income.groups)

summary(as.factor(Income.groups))

missingIncome <- is.na(custdata$income)  	# Note: 1 
Income.fix <- ifelse(is.na(custdata$income), 0, custdata$income) 	# Note: 2

medianincome <- aggregate(income~state.of.res,custdata,FUN=median)
colnames(medianincome) <- c('State','Median.Income')
summary(medianincome)  	# Note: 1 
custdata <- merge(custdata, medianincome,
                  by.x="state.of.res", by.y="State")  	# Note: 2 

summary(custdata[,c("state.of.res", "income", "Median.Income")]) 	# Note: 3 
custdata$income.norm <- with(custdata, income/Median.Income) 	# Note: 4 
summary(custdata$income.norm)

custdata$income.lt.20K <- custdata$income < 20000
summary(custdata$income.lt.20K)

brks <- c(0, 25, 65, Inf)  	# Note: 1 
custdata$age.range <- cut(custdata$age,
                          breaks=brks, include.lowest=T) 	# Note: 2 
summary(custdata$age.range) 	# Note: 3 


summary(custdata$age)

meanage <- mean(custdata$age)
custdata$age.normalized <- custdata$age/meanage
summary(custdata$age.normalized)

summary(custdata$age)
meanage <- mean(custdata$age)  	# Note: 1 
stdage <- sd(custdata$age)     	# Note: 2 
meanage
stdage

custdata$age.normalized <- (custdata$age-meanage)/stdage 	# Note: 3 
summary(custdata$age.normalized)

signedlog10 <- function(x) {
  ifelse(abs(x) <= 1, 0, sign(x)*log10(abs(x)))
}

custdata$gp <- runif(dim(custdata)[1])  	# Note: 1 
testSet <- subset(custdata, custdata$gp <= 0.1) 	# Note: 2 
trainingSet <- subset(custdata, custdata$gp > 0.1) 	# Note: 3 
dim(testSet)[1]

dim(trainingSet)[1]



library('ggplot2')
load('exampleData.rData')
ggplot(subset(custdata, custdata$income > 1000), aes(x=income, y=as.numeric(health.ins))) +
  geom_point(alpha=0.5, position=position_jitter(w=0.05, h=0.05)) + geom_smooth() + scale_x_log10() + 
  annotation_logticks(sides="bt")

