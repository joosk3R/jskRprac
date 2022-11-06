#1번 문제
name <- c("유관순","홍길동","이순신","신사임당")
gender <- c("F","M","M","F")
price <- c(50,65,45,75)
#조건(1)
client <- data.frame(name,gender,price) #데이터 프레임 생성
client
#조건(2)
result <- ifelse(client$price >= 65 , "Best", "Normal") #조건문 사용
result
#조건(3)
client1 <- data.frame(client, result)
client1
table(client1$result)

#2번 문제
install.packages("stringr")
library(stringr)
EMP <- c("2014홍길동220","2002이순신300","2010유관순260")
emp_pay <- function(x){
  a=str_extract_all(x,"[가-힝]{3}[0-9]{3}") # 이름과 급여만 남김
  b=unlist(a)                     #벡터로 변환
  c=str_extract_all(b,"[0-9]{3}") #급여만 추출 c는 급여 리스트
  d=mean(unlist(as.numeric(c)))   #숫자로 변환후 벡터로 변환 후 평균 구하기
  cat("전체 급여 평균 : ",d,"\n평균 이상 급여 수령자\n")
  c1=unlist(c)
  c1=as.numeric(c1)
  names(c1) = unlist(str_extract_all(x,"[가-힝]{3}")) # c1의 이름들 저장
  for(i in 1:length(c1)){         #c1의 길이만큼 반복
    if(c1[i] >= d ){
      cat(names(c1[i]),"=>",c1[i],'\n')
    }}
}
emp_pay(EMP)    

#3번 문제
f_x <- function(x){             #f(X)= x^3 +4를 구해주는 함수 만들기
  return(x^3+4)
} 
f_mean_ratio <-function(x1,x2){ #평균 변화율을 구하는 함수 만들기
  return((f_x(x1)-f_x(x2))/(x1-x2))
}
f_mean_ratio(3,1)
#답 : 13

#4번 문제
install.packages("RSADBE")
library(RSADBE)
data("Bug_Metrics_Software")
head(Bug_Metrics_Software)
apply(Bug_Metrics_Software,1,sum)
apply(Bug_Metrics_Software,2,mean)
class(Bug_Metrics_Software)
mode(Bug_Metrics_Software)
df1 <-data.frame(Bug_Metrics_Software)
table(df1$BA_Ind)
str(Bug_Metrics_Software)
#답 :
summary(Bug_Metrics_Software)
rowSums(Bug_Metrics_Software[,,1])
colMeans(Bug_Metrics_Software[,,1])
rowSums(Bug_Metrics_Software[,,2])
colMeans(Bug_Metrics_Software[,,2])
rowSums(Bug_Metrics_Software[,,])
colMeans(Bug_Metrics_Software[,,])

#2-1
Vec1 <- rep("R",5)
Vec2 <- seq(1,10,3)
Vec3 <- rep(Vec2,3)
Vec4 <- c(Vec2,Vec3)
Vec4
seq(25,15,-5)
length(Vec4)
Vec5 <- Vec4[seq(1,16,by=2)]
Vec5

#2-2
name <- c('최민수','유관순','이순신','김유신','홍길동')
age <- c(55,45,45,53,15)
gender <- c(1,2,1,1,1)
job <-c('연예인','주부','군인','직장인','학생')
sat <- c(3,4,2,5,5)
grade <- c('c','c','a','d','a')
total <- c(44.4,28.5,43.5,NA,27.1)
user <-data.frame(name,age,gender,job,sat, grade,total)
hist(gender,col='red')
user2 <- user[seq(2,nrow(user),2),]

#2-3
kor <-c(90,85,90)
eng <-c(70,85,75)
mat <-c(86,92,88)
df1 <-data.frame(kor,eng,mat)
df1
apply(df1,1,max) #행
apply(df1,2,max) #열
apply(df1,1,mean)#행
apply(df1,2,mean)#열
c.sd=apply(df1,1,sd)
apply(df1,1,var)
c.sd2 = c*c

#2-4
Data2 <-c('2017-02-05수입3000원',
          '2017-02-06수입4500원',
          '2017-02-07수입2500원')
library(stringr)
a<-str_extract_all(Data2,'[0-9]{4}[가-힣]')
unlist(a)
b<-str_replace_all(Data2,'[0-9]','')
b
c<-str_replace_all(Data2,'-',"/")
c
paste(Data2,collapse = ",")

#3-1
name1 <-c("유관순",'홍길동','이순신','신사임당')
gender1 <-c("F","M","M","F")
price1 <-c(50,65,45,75)
client3 <- data.frame(name1,gender1,price1)
result <- ifelse(price1 >=65,'Best','Normol')
result
client3$result <- result
table(client3$result)

#3-2
EMP1 <- c("2014홍길동220","2002이순신300","2010유관순260")
emp_pay2<-function(x){
  a1<- str_extract_all(x,'[가-힝][0-9]{3}') #한자리 + 급여
  a2<- str_extract_all(unlist(a1),'[0-9]{3}') #급여
  price2 <- as.numeric(unlist(a2)) #급여 벡터, 숫자 변환
  mean.avg<- mean(price2) #급여 평균
  col.names<-unlist(str_extract_all(x,"[가-힝]{3}"))
  cat("전체 급여 평균 : ",mean.avg,'\n')
  cat('평균 이상 급여 수령자\n')
  for(i in 1:length(x)){
    if(price2[i]>= mean.avg){
      cat(col.names[i],'=>',price2[i],'\n')
    }
  }
}
emp_pay2(EMP1)

#3-3
#평균변화율
f1<-function(x){return(x^3+4)}
f2<-function(x,y){return((f1(x)-f1(y))/(x-y))}
f2(1,3)

#3-4
install.packages("RSADBE")
library(RSADBE)
data("Bug_Metrics_Software")
rowSums(Bug_Metrics_Software[,,2])
colMeans(Bug_Metrics_Software[,,2])
summary(Bug_Metrics_Software[,,2])