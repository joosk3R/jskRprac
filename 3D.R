#7-9
library(regbook)
english1$grade<- factor(english1$grade, levels =  c(4,1:3))
english1.aov<- aov(score ~ factor(grade),english1)
english1.lm <- lm(score~ factor(grade),english1)
anova(english1.aov)
summary(english1.aov)
fit1<-lm(english1.aov)
anova(english1.lm)
summary(english1.lm)
anova(fit1)
summary(fit1)
#anova(english1.lm), summary(english1.lm) 와 anova(english1.aov), summary(english1.aov)
#의 차이가 없다.
#결론: 차이가 없고 동일하다


#7-10
#(1)
#H0 = a1 = a3=0
fit2 <- lm(score ~ grade, english1)
vcov(fit2)
summary(fit2)
#v(a1-a3) = var(a1)+var(a3)-2cov(a1,a3)
v.a1.a3 = 4.535^2 + 4.713^2 - 2*12.33873
v.a1.a3
sd.a1.a3=sqrt(v.a1.a3)
sd.a1.a3
t1= (-9.167 - (-16.1))/sd.a1.a3
t1
t0=qt(0.975,17)
t0
# t1 < t0 이므로 기각할수 없다.
# t 검정 통계량이 t(0.975, 17) 보다 작으므로 채택한다.

#(2)
english1$grade<- factor(english1$grade, levels =  c(3,1,2,4))
fit11<-lm(score ~ grade, english1)
summary(fit11)
#p-value가 0.05보다 크므로 채택한다.

#7-13
#(1)
english2$method <- relevel(english2$method , ref ="C")
fit3 <- lm(postscore ~ method, english2)
summary(fit3)
#p-value가 둘다 0.05보다 크므로 유의한 차이가 생기지 않는다.

#(2)
fit4 <- lm(postscore ~ method + prescore,english2)
summary(fit4)
#회귀식: Y = 22.67685 + 3.055Za + -1.87530Zb + 0.74X(1)
#A방법:  Y(a)= 25.735 + 0.74X(1)
#B방법:  Y(b)= 20.802 + 0.74X(1)
#C방법:  Y(c)= 22.677 + 0.74x(1)

#(3)
fit5 <- lm(postscore ~ method + prescore + prescore:method,english2)#교호작용항항
summary(fit5)
#p-value 가 각각 0.46048, 0.48275이므로 기울기가 같다는 가설을 채택한다.

#7-15
#(1)
data("blood2001m")
plot(bp~age,blood2001m)
#(2)
blood2001m <- within(blood2001m,{
  z<-ifelse(age>40,1,0)
  agez<-(age-40)*z
})
fit6 <- lm(bp ~ age + z + agez,blood2001m)
fit6
#(3)
# Y = 120.227 + 0.034x +0.2146z + 0.5582zx
# 40세 초과 : Y = 120.4916 + 0.5922x  
# 40세 이하 : Y = 120.277  + 0.034x
summary(fit6)
#agez의 p-value가 0.0012 이므로
#40세 전후 기준으로 통계적으로 유의미하게 달라진다.

#(5) 
#age의 p-value값이 0.05보다 크다, 즉 나이대로 통계를 내린다면 유의한 결과를 얻을수 없다
#agez의 p-value가 0.05보다 작으므로 40세 전후에서 혈압과 나이의 관계가 통계적으로 유의하다
#결론적으로 40대 이전과 이후를 고려하여 적합시킨다면 유의한 통계값을 얻을수 있을것이다.

A1=c(84.3,83.9,84.2)
A2=c(87.3,86.8,87.2)
A3=c(89.5,89.8,90.1)
A4=c(92.0,93.1,92.8)
pain=c(A1,A2,A3,A4)
group=c("A1(1.0%)","A2(1.5%)","A3(2.0%)","A4(2.5%)")
group<-rep(group,c(3,3,3,3))
pain.dat<-data.frame(group,pain)
attach(pain.dat)
tapply(pain,group,sum)
tapply(pain,group,mean)
sum(pain)
mean(pain)
aov.out=aov(pain~group,data=pain.dat)
aov.out
qf(0.01,df1=3,df2=8,lower.tail = F)
summary(aov.out)