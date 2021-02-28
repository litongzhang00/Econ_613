setwd("~/Desktop/613")
stu = read.csv('datstu.csv')
jss = read.csv('datjss.csv')
sss = read.csv('datsss.csv')

library(sqldf)
library(plyr)
library(dplyr)
library(tidyr)
library(doBy)
library(grid)
library(gridExtra)


# Ex1

nrow(stu) #Total of 340,823 students.

jss = unique(subset(jss, select = -c(X)))
nrow(jss) # Total of 139 junior high schools.

sss = unique(subset(sss, select = c(schoolcode, sssdistrict, ssslong, ssslat)))
sss = sqldf('SELECT * FROM sss GROUP BY schoolcode')
nrow(sss)
# Total of 898 senior high schools.

prog = as.matrix(stu[,11:16])
prog = matrix(prog, ncol=1)
prog = unique(prog)
prog = prog[prog!='']
length(prog) # Total of 32 programs.

prognum = matrix(data=c(prog, 1:32),ncol=2)
for (i in 1:6){
  stu[,i+10] = mapvalues(stu[,i+10], from=prognum[,1], to=prognum[,2], warn_missing = FALSE)
  stu[,i+10][stu[,i+10] == ''] = NA
}
for (i in 1:6){
  stu[,i+18] = stu[,i+4]*100+as.numeric(levels(stu[,i+10]))[stu[,i+10]]
}
choice = as.matrix(stu[,19:24])
choice = matrix(choice, ncol=1)
choice = unique(choice)
choice = choice[!is.na(choice)]
length(choice) # Total of 2,773 choices.

summary(stu$score)["NA's"] # Total of 179,887 missing test scores.

cc = apply(stu[,5:10], 1, function(x) length(x[!is.na(x)]) - length(unique(x[!is.na(x)])))
length(cc[cc != 0])
rm(cc) # Total of 120,071 students apply to the same school.

sum(apply(stu[,11:16],1,function(x) sign(sum(is.na(x))))) 
# Total of 20,988 students apply to less than 6 choices.

# Ex2

stu$schoolcode =  NA
stu$prog = NA
for (i in 1:6){
  stu$schoolcode[which(stu$rankplace == i)] = stu[,i+4][which(stu$rankplace == i)]
  stu$prog[which(stu$rankplace == i)] = stu[,i+18][which(stu$rankplace == i)]%%100
}
sch = summaryBy(score ~ schoolcode + prog, FUN = c(min, mean, length), data = stu)
sch = sqldf("SELECT * FROM sch LEFT JOIN sss USING(schoolcode)")
sch$sc = sch$schoolcode*100 + sch$prog

# Ex3

stu = sqldf("SELECT * FROM stu LEFT JOIN jss USING(jssdistrict)")
stu = sqldf("SELECT * FROM stu LEFT JOIN sss USING(schoolcode)")
stu$dist = sqrt((69.172*(stu$point_x - stu$ssslong) * cos(stu$point_y/57.3))^2
                + (69.172*(stu$point_y - stu$ssslat))^2)

# Ex4

# cutoff (V33:V38):
for (i in 1:6){
  stu[,i+32] = mapvalues(stu[,i+18], from=sch[,9], to=sch[,3], warn_missing = FALSE)
  stu[,i+32][stu[,i+32] > 500] = NA
}
# quality (V39:V44):
for (i in 1:6){
  stu[,i+38] = mapvalues(stu[,i+18], from=sch[,9], to=sch[,4], warn_missing = FALSE)
  stu[,i+38][stu[,i+38] > 500] = NA
}
# longtitude (V45:V50):
for (i in 1:6){
  stu[,i+44] = mapvalues(stu[,i+18], from=sch[,9], to=sch[,7], warn_missing = FALSE)
  stu[,i+44][stu[,i+44] > 100] = NA
}
# latitude (V51:V55):
for (i in 1:6){
  stu[,i+50] = mapvalues(stu[,i+18], from=sch[,9], to=sch[,8], warn_missing = FALSE)
  stu[,i+50][stu[,i+50] > 100] = NA
}
# distance (V56:V61):
for (i in 1:6){
  stu[,i+56] = sqrt((69.172*(stu$point_x - stu[,i+44]) * cos(stu[,i+50]/57.3))^2
                    + (69.172*(stu$point_y - stu[,i+50]))^2)
}

# create tables to report Exercise 4:

report4 = matrix(rep(NA, 36), ncol=6, byrow=TRUE)
colnames(report4) = c('Choice 1', 'Choice 2', 'Choice 3', 'Choice 4', 'Choice 5', 'Choice 6')
rownames(report4) = c('Cutoff avg', 'Cutoff sd', 'Quality avg', 'Quality sd', 'Distance avg', 'Distance sd')
for (i in 1:6){
  report4[1,i] = mean(stu[,i+32], na.rm=TRUE)
  report4[2,i] = sd(stu[,i+32], na.rm=TRUE)
  report4[3,i] = mean(stu[,i+38], na.rm=TRUE)
  report4[4,i] = sd(stu[,i+38], na.rm=TRUE)
  report4[5,i] = mean(stu[,i+56], na.rm=TRUE)
  report4[6,i] = sd(stu[,i+56], na.rm=TRUE)
}
grid.arrange(tableGrob(round(report4,2), theme = ttheme_default(base_size = 10, padding = unit(c(1, 1), "mm"))))
setwd("~/Desktop/613")
dev.copy(png, 'report4.png')
dev.off()

# students' quartile of score:
stu$quar = cut(stu$score, breaks = quantile(stu$score, probs = seq(0, 1, .25), na.rm = TRUE), include.lowest = TRUE, labels = 1:4)
report4q = matrix(rep(NA, 36*4), ncol=6, byrow=TRUE)
colnames(report4q) = colnames(report4)
rownames(report4q) = c('Cutoff avg q1', 'Cutoff sd q1', 'Quality avg q1', 'Quality sd q1', 'Distance avg q1', 'Distance sd q1',
                       'Cutoff avg q2', 'Cutoff sd q2', 'Quality avg q2', 'Quality sd q2', 'Distance avg q2', 'Distance sd q2',
                       'Cutoff avg q3', 'Cutoff sd q3', 'Quality avg q3', 'Quality sd q3', 'Distance avg q3', 'Distance sd q3',
                       'Cutoff avg q4', 'Cutoff sd q4', 'Quality avg q4', 'Quality sd q4', 'Distance avg q4', 'Distance sd q4'
                       )
for (j in 1:4){
  for (i in 1:6){
    report4q[6*j-5,i] = mean(stu[,i+32][stu$quar == j], na.rm=TRUE)
    report4q[6*j-4,i] = sd(stu[,i+32][stu$quar == j], na.rm=TRUE)
    report4q[6*j-3,i] = mean(stu[,i+38][stu$quar == j], na.rm=TRUE)
    report4q[6*j-2,i] = sd(stu[,i+38][stu$quar == j], na.rm=TRUE)
    report4q[6*j-1,i] = mean(stu[,i+56][stu$quar == j], na.rm=TRUE)
    report4q[6*j,i] = sd(stu[,i+56][stu$quar == j], na.rm=TRUE)
  }
}
grid.arrange(tableGrob(round(report4q,2), theme = ttheme_default(base_size = 10, padding = unit(c(1, 1), "mm"))))
dev.copy(png, 'report4q.png')
dev.off()

# Ex5

set.seed(1)
X1 = runif(10000, 1, 3)
X2 = rgamma(10000, 3, scale=2)
X3 = rbinom(10000, 1, .3)
eps = rnorm(10000, 2, 1)
b = c(.5, 1.2, -.9, .1)
X = matrix(c(rep(1, 10000), X1, X2, X3), ncol=4)
Y = (X %*% as.matrix(b)) + eps
ydum = as.numeric(Y > mean(Y))

# Ex6

cor(X1, Y)
# The correlation is significantly different from 1.2.

coefYX = solve(t(X)%*%X)%*%t(X)%*%Y
residYX = Y - X%*%coefYX
varYX = (t(residYX)%*%residYX)/(length(Y)-length(b))
vcovYX = c(varYX) * solve(t(X)%*%X)
seYX = sqrt(diag(vcovYX))

# likelihood function for the probit model:
probitl = function(beta, y, X){
  p = pnorm(X%*%beta)
  l = sum(y*log(p)) + sum((1-y)*log(1-p))
  return(l)
}
probitlb = function(par){
  -probitl(par, ydum, X)
}
# gradient function for the probit model:
dprobitl = function(beta, y, X){
  p = pnorm(X%*%beta)
  f = dnorm(X%*%beta)
  n = length(y)
  k = length(beta)
  d = t(matrix(rep(f/p, k), nrow=n)*X)%*%y -
    t(matrix(rep(f/(1-p), k), nrow=n)*X)%*%(1-y)
  return(d)
}
dprobitlb = function(par){
  -dprobitl(par, ydum, X)
}
# steepest ascent optimization algorithm:
par = rep(0,4)
for (i in 1:10000){
  par = par - .00001 * c(dprobitlb(par))
}

# Ex7

# probit model:
optimprobit = optim(par=rep(0,4), fn=probitlb, hessian=T)
# logit model:
logitl = function(beta, y, X){
  p = 1/(1 + exp(-X%*%beta))
  l = sum(y*log(p)) + sum((1-y)*log(1-p))
  return(l)
}
logitlb = function(par){
  -logitl(par, ydum, X)
}
optimlogit = optim(par=rep(0,4), fn=logitlb, hessian=T)
# linear probability model:
lpms = function(par){
  s = (ydum - X%*%par)^2
  return(sum(s))
}
optimlpm = optim(par=rep(0,4), fn=lpms, hessian=T)
# The probit model returns parameters closest to the actual values,
# and the other two models have approximately the same accuracy.
tprobit = optimprobit$par/sqrt(diag(solve(optimprobit$hessian)))
tlogit = optimlogit$par/sqrt(diag(solve(optimlogit$hessian)))
tlpm = optimlpm$par/sqrt(diag(solve(optimlpm$hessian)))
# All the estimates from all the three models are statistically
# significant, except the last parameter in the linear probability
# model under 95% confidence level.

# Ex8

# probit model: F'(x) = (-exp(-x^2/2))/sqrt(2*pi)
# logit model: F'(x) = exp(-x)/(1 + exp(-x))^2
meoptimprobit = dnorm(X%*%optimprobit$par)%*%optimprobit$par
meoptimlogit = (exp(-X%*%optimlogit$par)/(1 + exp(-X%*%optimlogit$par))^2)%*%optimlogit$par
# probit standard error:
vcovprobit = solve(optimprobit$hessian)
jacprobit = array(0,dim=c(4,4,10000))
for (k in 1:10000){
  jacprobit[,,k] = diag(4)
  f = dnorm(X[k,]%*%optimprobit$par)
  for (i in 1:4){
    for (j in 1:4){
      jacprobit[i,j,k] = f*(jacprobit[i,j,k]-
                              (X[k,]%*%optimprobit$par)*optimprobit$par[j]*X[k,i])
    }
  }
}
jbprobit = rowMeans(jacprobit, dims=2)
deltaprobit = sqrt(diag(t(jbprobit)%*%vcovprobit%*%jbprobit))
# logit standard error:
vcovlogit = solve(optimlogit$hessian)
jaclogit = array(0,dim=c(4,4,10000))
for (k in 1:10000){
  jaclogit[,,k] = diag(4)
  f = 1/(1 + exp(-X[k,]%*%optimlogit$par))
  for (i in 1:4){
    for (j in 1:4){
      jaclogit[i,j,k] = f * (1-f) * (
        jaclogit[i,j,k] + (1-2*f)*(optimlogit$par[j]*X[k,i])
      )
    }
  }
}
jblogit = rowMeans(jaclogit, dims=2)
deltalogit = sqrt(diag(t(jblogit)%*%vcovlogit%*%jblogit))
