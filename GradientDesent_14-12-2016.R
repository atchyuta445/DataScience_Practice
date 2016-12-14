#Optimization problem
#1. Explore all possibilites

#2. Solving Equations by making slop=0(difficult for non linear equations in multi dimentional cases)


#3. Solve using guess & refine approach(gardient vector), 
#It starts with some random initial guess and converges further

#For single Varible
f =function(x){
  1.2 * (x-2)^2 + 3.2
}

grad = function(x){
  1.2 * 2 * (x-2)
}

#?seq()
xs=seq(0,4,len=10)
x11()
plot(xs,f(xs), type='l',xlab='xs',ylab=expression(1.2 * (x-2)^2 + 3.2))

x=3
xtrace=x
ftrace=f(x)
stepfactor=0.1
iterations=1000
threshold=0.0001

for(iter_cnt in 1:iterations){
  
  x=x-stepfactor*grad(x)
  
  xtrace=c(xtrace, x)
  ftrace=c(ftrace, f(x))
  if(iter_cnt>1 && abs(ftrace[iter_cnt]-ftrace[iter_cnt-1]) < threshold) {
    break
  }
  
}
x11()
plot(xtrace,ftrace,type='b')

#For double variable case
#install.packages("plot3D") 
library(plot3D) 


f2=function(x,y){
  x^2+y^2
}

#Now gradient also a vector
grad2=function(x,y){
  c(2*x,2*y)
}

x=seq(-5,5,len=100)
y=seq(-5,5,len=100)


x=3
y=3
xtrace=x
ytrace=y
ftrace=f2(x,y)
stepfactor=0.1
iterations=1000
threshold=0.0001

for(iter_cnt in 1:iterations){
  GD=grad2(x,y)
  x=x-stepfactor*GD[1]
  y=y-stepfactor*GD[2]
  xtrace=c(xtrace, x)
  ytrace=c(ytrace, y)
  ftrace=c(ftrace, f2(x,y))
  if(iter_cnt>1 && abs(ftrace[iter_cnt]-ftrace[iter_cnt-1]) < threshold) {
    break
  }
  
}
x11()
plot(xtrace,ftrace,type='b')
plot(ytrace,ftrace,type='b')

points3D(xtrace,ytrace,ftrace,col='red')
