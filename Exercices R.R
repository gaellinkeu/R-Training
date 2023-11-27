##Exercises 1: Vectors

#1
a = c(1:20)
b = c(20:1)
c = c(1:20,19:1)
tmp = c(4,6,3)
e = rep(tmp,10)
f = rep(tmp, l=31)
g = rep(tmp,times=c(10,20,30))
g

#2
fun <- function(x){ exp(x)*cos(x)}
data = seq(3, 6, by=.1)
data
fun(data)

#3
#a
a = rep(0.1, 12)
b = rep(0.2, 12)
c = seq(3,36,3)
d = c - 2
ans= a^c*b^d
ans

#b
a = seq(1,25,1)
b = rep(2,25)
ans = (b^a)/a
ans

#4
x <- function(i) {i^3 + 4*i^2}
data = seq(10,100,1)
a = sum(x(data))

y <- function(i) {(2^i)/i + (3^i)/(i^2)}
data2 = c(1:25)
b = sum(y(data2))

#5
a = paste("label ", c(1:30))
b = paste("fn", c(1:30))

#6
xVec <- sample(0:999, 250, replace=T)
yVec <- sample(0:999, 250, replace=T)
a = yVec[-1]-xVec[-250]
b =sin(yVec[-250])/cos(xVec[-1]) 
c = xVec[-c(249,250)]+2*xVec[-c(1,250)] - xVec[-c(1,2)]
d = sum(exp(-xVec[-1])/(xVec[-c(length(xVec))]+10))

#7
a = yVec[yVec>600]
b = which(yVec>600)
c = xVec[yVec>600]
d = abs(xVec - mean(xVec))^0.5
e = length(yVec[yVec>200])
f = length(xVec[xVec%%2])
g = xVec[order(yVec)]
h = yVec[seq(1,length(yVec), 3)]

#8
a = seq(1,39, 2)
b = c(1,a[2:length(a)]-1)
c = cumprod(b/a)
ans = sum(c)


EXERCICES 2: MATRICES

#1
(A = matrix(c(1,1,3,5,2,6,-2,-1,-3), ncol=3, byrow=TRUE))
(A%*%A%*%A)
A[,3] = A[,3]+A[,2]
#2
(B = matrix(rep(-10,15*3),nrow=15))
(crossprod(B))
#3
matE = matrix(rep(0,6*6), nrow=6)
row(matE)
col(matE)
matE[abs(col(matE)-row(matE))==1] <- 1
#4
outer(0:4,0:4,"+")
#5
A = outer(0:4,0:4,"+")%%5
A%%dim(A[1])
outer(0:9,0:9,"+")%%10
outer(0:8,9:1,"+")%%9
#6
yVec <- c(7,-1,-3,5,17)
A = matrix(0, nr=5, nc=5)
A <- abs(col(A)-row(A))+1
xVec = solve(A,matrix(yVec, nc=1))
#7
set.seed(75)
aMat <- matrix(sample(10, size=60, replace=T), nr=6)
a = apply(aMat, 1, function(x){sum(x>4)})
which(apply(aMat,1,function(x){sum(x==7)==2}))

colsum = apply(aMat,2,function(x){sum(x)})
final <- outer(colsum,colsum,'+')
which(final>75, arr.ind=TRUE)

final[upper.tri(final)] <- 0
which(final>75, arr.ind=TRUE)

#8
(sum(1/((1:5)+3)))*sum((1:20)^4)

a = c(1:20); b = c(1:5)
sum(outer(a,b,function(i,j){(i^4)/(3+i*j)}))

sum(outer(1:10,1:10,function(i,j){(j<=i)*(i^4)/(3+i*j)}))


EXERCICES 3: SIMPLE FUNCTIONS

#1
tmpFn1 = function(xVec){xVec^c(1:length(xVec))}
tmpFn2 = function(xVec){(xVec^c(1:length(xVec)))/c(1:length(xVec))}
tmpFn3 = function(x,n){sum(c(1,tmpFn2(rep(x,n))))}

#2
tmpFn = function(xVec){n=length(xVec); (xVec[-c(n,n-1)]+xVec[-c(1,n)]+xVec[-c(1,2)])/3}
tmpFn(c(1:5,6:1))

#3
f <- function(x){if(x<0){x^2+2*x+3}; if(x<2){x+3}; if(x>=2){x^2+4*x-7}}
tmpFn <- function(x){
	ifelse(x<0,x^2+2*x+3,ifelse(x<2,x+3,x^2+4*x-7))
}
tmp <- seq(-3,3,len=100)
plot(tmp,tmpFn(tmp),type='l')

#4
tmp <- function(mat)
{
	mat[mat%%2==1] <- 2 * mat[mat%%2==1]
	mat
}

#5
f <- function(n, k)
{
	mat <- matrix(rep(0,n*n),ncol=n)
	mat[abs(col(mat)-row(mat))==0] <- k
	mat[abs(col(mat)-row(mat))==1] <- 1
	mat
}

#6
quadrant <- function(alpha)
{
	1 + floor((alpha%%360)/90)
}

#7
zeller <- function(day,month,year)
{
	k <- day
	m <- (month-3)%%12+1
	if(month>10)
	{
		year <- year -1
	}
	c <- floor(year/100)
	y <- year - c*100
	z <- (floor(2.6*m-0.2)+k+y+floor(y/4)+floor(c/4)-2*c)%%7
	z <- (z-1)%%7+1
	days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
	days[z]
}

zeller2 <- function(day,month,year)
{
	k <- day
	m <- (month-3)%%12+1
	flag <- month<10
	year <- year - flag
	c <- floor(year/100)
	y <- year - c*100
	z <- (floor(2.6*m-0.2)+k+y+floor(y/4)+floor(c/4)-2*c)%%7
	z <- (z-1)%%7+1
	days <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
	days[z]
}


#8
testLoop <- function(n)
{
	xVec <- rep(NA,n-1)
	xVec[1] <- 1
	xVec[2] <- 2
	for( j in 3:(n-1) )
		xVec[j] <- xVec[j-1] + 2/(xVec[j-1])
	xVec
}

testLoop2 <- function(yVec)
{
	sum(exp(seq(along=yVec)))
}


#9
quadmap <- function(start, rho, niter)
{
	xVec <- rep(NA, niter)
	xVec[1] <- start
	for(i in 1:(niter-1)){
		xVec[i+1] <- rho * xVec[i]*(1-xVec[i])
	}
	xVec
}
tmp <- quadmap(start=0.95, rho=2.99, niter=500)
plot(tmp, type="l")


quadmap2 <- function(start, rho, eps = 0.02)
{

	x1 <- start
	x2 <- x2 <- rho * x1*(1-x1)
	i <- 1
	while(abs(x1-x2)>= eps){
		x1 <- x2
		x2 <- rho * x1*(1-x1)
		i <- i+1
	}
	i
}


#10

tmpFn <- function(xVec, k)
{
	n <- length(xVec)
	m <- mean(xVec)
	x <- xVec - m
	xc = sum(x^2)
	r1 <- sum((xVec[2:n] - m)*(xVec[1:(n-1)]-m))/xc
	r2 <- sum((xVec[3:n] - m)*(xVec[1:(n-2)]-m))/xc
	list(r1=r1, r2=r2)
}

tmpFn1 <- function(xVec, k)
{
	n <- length(xVec)
	m <- mean(xVec)
	x <- xVec - m
	xc = sum(x^2)
	res <- c(1)
	for(i in 1:k)
	{
		r <- sum(x[(i+1):n]*x[1:(n-i)])/xc
		res <- c(res, r)
	}

	res
}

tmpFn1_1 <- function(xVec, k)
{
	n <- length(xVec)
	m <- mean(xVec)
	xc <- xVec - m
	denom <- sum(xc^2)
	tmpFn <- function(j){sum(xc[(j+1):n] * xc[1:(n-j)])/denom}
	c(1, sapply(1:k, tmpFn)) 
}


# EXERCICES 4: HARDER FUNCTIONS

x

#1
tmpFun_a <- function(xVec, yvec)
{
	rowSums(outer(xVec, yVec, ">"))
}

tmpFun_b <- function(xVec, yVec)
{
	sapply(xVec, FUN=function(x){length(yVec[yVec<x])})
}

tmpFun_c <- function(xVec, yVec)
{
	vapply(xVec, FUN=function(x){length(yVec[yVec<x])}, FUN.VALUE=c(1))
}

#2
tmpFun_a <- function(matA)
{
	matA[, !apply(is.na(matA), 2, any), drop=F]
}

tmpFunc_b <- function(matA)
{
	matA[!apply(is.na(matA), 1, any), !apply(is.na(matA), 2, any), drop=F]
}

#3
empCopula_ <- function(u,v,xVec,yVec)
{
	n <- length(xVec)
	r = rank(xVec)/(n+1)
	r <- r<=u
	s = rank(yVec)/(n+1)
	s <- s<=v
	sum(r&s)/n
}

empCopula2 <- function(u,v,xVec,yVec)
{
	n <- length(xVec)
	r = rank(xVec)/(n+1)
	r_ <- outer(xVec, u,"<=")
	s = rank(yVec)/(n+1)
	s_ <- outer(yVec, v,"<=")
	res <- c()
	for (i in 1:length(u))
	{
		res <- c(res,sum(r_[,i]&s_[,i])/n)
	}
	res
}


empCopula21 <- function( u, v, xVec, yVec )
{
n <- length(xVec)
rVecN <- rank(xVec)/(n+1)
sVecN <- rank(yVec)/(n+1)
valuesN <- colSums( outer(rVecN, u, "<=")&outer(sVecN, v, "<=") )
cbind( uCoord = u, vCoord = v, empCop=valuesN/n )
}

