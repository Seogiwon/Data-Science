library("data.table")
cow_data<-as.data.frame(fread("./Data/cow_data.csv",encoding="UTF-8"))
cowNA_data<-as.data.frame(fread("./Data/cowNA_data.csv",encoding="UTF-8"))
co<-as.data.frame(fread("./Data/co.csv",encoding="UTF-8"))

## 1. cow_data에서 is_edible 이라는 새로운 열을 추가하고
##    나이(age)가 50(개월)이상이면서
##    등급(grade)이 "3" 또는 "등외"이라면 "폐기용", 아니면 "식용"을 작성해주세요 

head(cow_data)
str(cow_data)
attach(cow_data)
cow_data$is.edible<-ifelse(age>=50&grade==3|grade=="등외","폐기용","식용")



## 2. cowNA_data에서 결측치가 발생한 곳에 평균 값을 집어넣은 새로운 열을 만드세요
head(cowNA_data)
str(cowNA_data)
summary(cowNA_data)
cowNA_data$weight[is.na(cowNA_data$weight)]<-mean(cowNA_data$weight,na.rm = T)

  
## 3. co 데이터에서 taste 이라는 새로운 열을 추가하고
##    grade가 3,2,1 순서대로 새로운 taste 열을 만들어
##    이곳에 등급에 따라"normal","good","best" 을 작성해주세요 
head(co)
str(co)
co$taste<-ifelse(co$grade==3,"normal",ifelse(co$grade==2,"good","best"))
  
## 4. co 데이터에서 결측치 행을 지운 새로운 데이터 co1을 만들고
##    결측치가 없어진 co1의 price와 grade의 관계를 구하세요(ex : 회귀,corr 등)
co1<-na.omit(co)

cor(co1$price,co1$grade)
lm(formula = price~grade, data = co1)



## 5. co1 데이터에서 패키지 ggplot2를 이용하여
##    가격과 등급의 산점도(크기 0.5의 파란색 point)를 그리세요
  
library("ggplot2")
ggplot(co1,aes(x=grade,y=price))+
  geom_point(color="blue",size=0.5)


## 5-1(회귀 분석을 수강하신 분만)
##    co1 데이터에서 ggplot2과gcookbook패키지를 이용하여
##    가격과 등급의 산점도와 회귀식 모델선을 추가하여 그리세요(99% 확률)
library("gcookbook")
sp<-ggplot(co1, aes(x=grade, y=price))
lm(formula = price~grade, data = co1)
sp+geom_point()+stat_smooth(method=lm, level=0.99)


## 6. co1 데이터의 region name 에서 앞글자와 두번째 글자를 따와
##    city라는 새로운 열에 넣으세요 (ex : 영남,경북 등) (hint : substring)
co1$city<-substring(co1$`region name`,1,2)



## 6-1 #6번을 이용하여 각 도별 평균 가격과 등급을 구하세요



## 7. co1 데이터에서 price를 cut function을 사용하여
##    3개의 범주로 나누고 '저가' ,'중가' ,'고가'로 표시된
##    groups라는 새로운 열을 만드세요 (범주기준 : [6000 ~9000),[9000~12000),[12000~15200) )
breaks <-c(6000,9000,12000,15200)  
co1$groups<- cut(co1$price,
                  breaks=breaks,labels = c("저가","중가","고가"))      

