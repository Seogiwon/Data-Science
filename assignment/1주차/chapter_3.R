custdata <- read.table("custdata.tsv", header = T, sep = '\t')

# exercise 3.1
summary(custdata)

# exercise 3.2
summary(custdata$is.employed)

# exercise 3.3
summary(custdata$income)
summary(custdata$age)

# exercise 3.5
summary(custdata$income/1000)

# figure 3.1
library(ggplot2)
p <- ggplot(custdata, aes(x=age)) + 
  geom_density()
p

# exercise 3.6
ggplot(custdata) +
  geom_histogram(aes(x=age),
  binwidth=5, fill="gray") 

# figure 3.4
library(scales)

ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_continuous(labels=dollar)

# exercise 3.8
ggplot(custdata) + geom_density(aes(x=income)) +
  scale_x_log10(breaks=c(100,1000,10000,100000), labels=dollar) +   
  annotation_logticks(sides="bt")

# figure 3.6
ggplot(custdata) + geom_bar(aes(x=marital.stat), fill="gray")

# exercise 3.9
ggplot(custdata) +
  geom_bar(aes(x=state.of.res), fill="gray") +
  coord_flip() + 	# Note: 2 
  theme(axis.text.y=element_text(size=rel(0.8)))  

# exercise 3.10
statesums <- table(custdata$state.of.res) 	
statef <- as.data.frame(statesums) 	
colnames(statef)<-c("state.of.res", "count") 	
summary(statef)  	

statef <- transform(statef,
            state.of.res=reorder(state.of.res, count)) 
summary(statef) 

ggplot(statef)+ geom_bar(aes(x=state.of.res,y=count),
                         stat="identity",              	 
                         fill="gray") +
                coord_flip() +                                       	 
                theme(axis.text.y=element_text(size=rel(0.8)))

# exercise 3.11
x <- runif(100)   	 
y <- x^2 + 0.2*x   	
ggplot(data.frame(x=x,y=y), aes(x=x,y=y)) + geom_line()

# exercise 3.12
custdata2 <- subset(custdata,
                    (custdata$age > 0 & custdata$age < 100
                     & custdata$income > 0))                  	

cor(custdata2$age, custdata2$income) #corrlation

# figure 3.10
ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + ylim(0, 200000)

# figure 3.11
ggplot(custdata2, aes(x=age, y=income)) + geom_point() +
  stat_smooth(method="lm") +
  ylim(0, 200000) # loose =  non-parametric strategies

# figure 3.12
ggplot(custdata2, aes(x=age, y=income)) +
  geom_point() + geom_smooth() +
  ylim(0, 200000)

# exercise 3.13
ggplot(custdata2, aes(x=age, y=as.numeric(health.ins))) + 
  geom_point(position=position_jitter(w=0.05, h=0.05)) +  
  geom_smooth() 

# exercise 3.14
library(hexbin)

ggplot(custdata2, aes(x=age, y=income)) +
  geom_hex(binwidth=c(5, 10000)) +   	 
  geom_smooth(color="white", se=F) +  	 
  ylim(0,200000)

# figure 3.15
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins)) 	

# figure 3.16
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="dodge")      	

# figure 3.17
ggplot(custdata) + geom_bar(aes(x=marital.stat,
                                fill=health.ins),
                            position="fill")   

# figure 3.18
ggplot(custdata, aes(x=marital.stat)) +
  geom_bar(aes(fill=health.ins), position="fill") +
  geom_point(aes(y=-0.05), size=0.75, alpha=0.3, 	
             position=position_jitter(h=0.01))

# figure 3.19
ggplot(custdata2) +                                          	
  geom_bar(aes(x=housing.type, fill=marital.stat ),
           position="dodge") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   	

# figure 3.20
ggplot(custdata2) +                                          	
  geom_bar(aes(x=marital.stat), position="dodge",
           fill="darkgray") +
  facet_wrap(~housing.type, scales="free_y") +               
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
