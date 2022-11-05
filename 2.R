x<-c(3.36,1.98,3.80,2.35,5.94,2.96,5.30,3.95,
     3.33,3.43,9.26,4.24,3.33,2.52,2.90,2.70,
     4.41,2.00,3.67,2.30,4.50,4.14,9.85,3.73,
     1.50,1.93,1.93,1.78,3.63,2.65,4.14,2.02,
     4.42,2.53,9.00,3.88,2.10,2.22,7.27,4.11)
#- 도수분포표에 대한 히스토그램, 상대도수분포표에 대한 히스토그램, 도수다각형, 상자그림 그리고 줄기-잎 그림을 작성
#- 기술통계량을 각각의 함수를 사용하여 결과 도출
#- 주어진 결과에 대한 수강생 본인의 분석한 의견을 5줄 이상으로 작성
#조건(1)
hist(x, breaks='Sturges',freq=NULL) #도수분포
hist(x, breaks='Sturges',freq=FALSE) #상대도수분포
hist.result=hist(x)
lines(x=hist.result$mids,y=hist.result$counts,type='b',pch=20)
table(x)
freq.table<-table(x)
boxplot(x, xlab = "뿌리", ylab = "weight")
stem(x,scale=1)	

#조건(2)
mean(x) 
median(x) 
var(x) 
sd(x) 
min(x) 
max(x) 
max(x)-min(x)
quantile(x,c(0.1,0.9))
quantile(x,c(0,0.9))
range(x)
IQR(x)
quantile(x,0.75)
quantile(x,0,25)
quantile(x,0.75)-quantile(x,0,25)
#조건(3)
#1g 단위로 도수 분포표에 대한 여러 그래프를 그렸을때 2g 과 3g 사이의 무게를 가진 뿌리 빈도수가 높고 
#1g이상 5g 미만에서 대부분의 뿌리들이 분포해 있다. 기술적 통계량을 보면 평균 무게가 3.7765g , 표준편차는 2.01g이고,
# x의 범위는 1.5g에서 9.85g이다. 다른 자료값보다 특이하게 큰 4개의 자료값(9.85, 9.26, 9.00, 7.27)의 뿌리들을 제외하면
# 기술적 통계량 에서 보이듯이 90%의 자료들이 전부 포함되어있다. 

#일반정규분포
#m=105,분산=15

#5-16-(1)
qnorm(0.95,105,sqrt(15),lower.tail=TRUE)
qnorm(0.05,105,sqrt(15),lower.tail=FALSE)
#답 : 111.3705 이상

#5-16-(2)
qnorm(0.1,105,sqrt(15),lower.tail=TRUE)
qnorm(0.9,105,sqrt(15),lower.tail=FALSE)
#답 : 100.0366 이하

#5-16-(3)
pnorm(115,105,sqrt(15),lower.tail = FALSE)
1-pnorm(115,105,sqrt(15),lower.tail = TRUE)
x=0.004911637*5000
#답 : 25명

#표준정규분포
# z= x-105/sqrt(15), m=0, 분산=1
#(1)
qnorm(0.95,0,1,lower.tail = TRUE)
z1 <- 1.644854
x1 <-z1*sqrt(15)+105
x1
#답 : 111.3705 이상

#(2)
z2 <- qnorm(0.1,0,1,lower.tail = T)
x2<- z2*sqrt(15)+105
x2
#답 : 100.0366 이하

#(3)
z3 <-pnorm((115-105)/sqrt(15),0,1,lower.tail = T)
z3
(1-z3)*5000
#답 : 25명

x<-c(3.36,1.98,3.80,2.35,5.94,2.96,5.30,3.95,3.33,3.43,9.26,4.24,3.33,2.52,2.90,2.70,
     4.41,2.00,3.67,2.30,4.50,4.14,9.85,3.73,1.50,1.93,1.93,1.78,3.63,2.65,4.14,2.02,
     4.42,2.53,9.00,3.88,2.10,2.22,7.27,4.11)
y<-table(x) #도수분포표
y
#조건(1)
hist(x, breaks='Sturges',freq=NULL) #도수분포
hist(x, breaks='Sturges',freq=FALSE) #상대도수분포
hist.result=hist(x)
lines(x=hist.result$mids,y=hist.result$counts,type='b',pch=20) #도수다각형
table(x)
freq.table<-table(x)
boxplot(x, xlab = "뿌리", ylab = "weight") #상자그림
stem(x,scale=1) #줄기-잎 그림


library(regbook)
data("elementheight")
fit1 <- lm(meanheight~age,elementheight,weights = 1/sdheight^2)
fit1
summary(fit1)

