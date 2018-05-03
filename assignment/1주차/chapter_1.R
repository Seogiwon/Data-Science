#1.1 exercise
library('rpart')
load('GCDData.RData')
model <- rpart(Good.Loan ~
                 Duration.in.month +
                 Installment.rate.in.percentage.of.disposable.income +
                 Credit.amount  +
                 Other.installment.plans,
               data=d,
               control=rpart.control(maxdepth=4),
               method="class")

plot(model); text(model);

library(rpart.plot)
prp(model,type=0 ,extra=8)
rpart.plot(model,type=0,extra=8)

#1.2 exercise
creditdata <- d
resultframe <- data.frame(Good.Loan=creditdata$Good.Loan,
                          pred=predict(model, type="class"))

rtab <- table(resultframe) 	
rtab

sum(diag(rtab))/sum(rtab)  

sum(rtab[1,1])/sum(rtab[,1]) 	

sum(rtab[1,1])/sum(rtab[1,]) 	

sum(rtab[2,1])/sum(rtab[2,]) 

#1.3 exercise
tab1 <- as.table(matrix(data=c(50,6,0,44),nrow=2,ncol=2))
dimnames(tab1) <- list('loan.as.pct.disposable.income'=
                         c('LT.15pct','GT.15pct'),
                       'loan.quality.pop1'=
                         c('goodloan','badloan'))
tab2 <- as.table(matrix(data=c(34,18,16,32),nrow=2,ncol=2))
dimnames(tab2) <- list('loan.as.pct.disposable.income'=
                         c('LT.15pct','GT.15pct'),
                       'loan.quality.pop2'=
                         c('goodloan','badloan'))
tab1
