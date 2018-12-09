##Question 2
library(MASS)
covdelta <- matrix(c(1,0.5,0.5,1),2,2)
MCsimas4 <- function(fstp,rntime,covdelta,p){
  fora <- NULL
  foastp1 = foastp2 = foastp3 = NULL
  hdrtn <- NULL
  hderr <- NULL
  for (i in 1:rntime){
    rt <- c(10,rep(0,fstp))
    at <- rep(0,2+fstp)
    for (j in 1:fstp){
      delta <- mvrnorm(1,rep(0,2),covdelta)
      at[j+2] <- sum(0.5*delta*at[j:(j+1)])+rnorm(1)
      rt[j+1] <- 0.5*rt[j]+at[j+2]
    }
    #foastp1[i] <- rt[2]
    #foastp2[i] <- rt[3]
    #foastp3[i] <- rt[4]
    #fora <- foastp1+foastp2+foastp3
    hdrtn[i] <- sum(rt[2:4]);
  }
  MCsimas4 <- hdrtn[order(hdrtn)[(1-p)*length(hdrtn)]]*1e+05
  #MCsimas4 <- c(mean(fora),sd(foastp1)+sd(foastp2)+sd(foastp3))
    #mean(fora)+pnorm(1-p)*(sd(foastp1)+sd(foastp2)+sd(foastp3))
}
var <- MCsimas4(3,1000,covdelta,0.05)

##Question 3d
psi <- matrix(c(0.6,-0.3,0.1,-0.4,0.2,-0.4,0.4,0.3,0.9),3,3,byrow = T)
Sig <- matrix(c(1,0.2,0,0.2,1,0.24,0,0.24,1),3,3,byrow = T)
rt <- rep(0,3)
pt = qt = rep(0,1000)
set.seed(413)
for (i in 1:1000){
  rt = psi%*%rt+mvrnorm(1,rep(0,3),Sig)
  pt[i] <- sum(c(1/sqrt(6),2/sqrt(6),1/sqrt(6))*rt)
  qt[i] <- sum(c(8/5/sqrt(98/25),-3/5/sqrt(98/25),1/sqrt(98/25))*rt)
}
pt <- ts(pt,start = 1,frequency = 1)
qt <- ts(qt,start = 1,frequency = 1)
prwsstrg <- function(pt,thrsh){
  c <- 1*(pt>=thrsh)-1*(pt<=-thrsh)
  tmp <- which(c == -1)
  simb <- rep(0,length(tmp))
  for (i in 1:(length(tmp)-1)) {
    v = tmp[i]:tmp[i+1]
    simb[i] = sum(c[v]==1)
  }
  simb[length(tmp)] <- sum(pt[tmp[length(tmp)]:length(pt)] == 1)*(tmp[length(tmp)]<length(pt))
  prwsstrg <- c(2*thrsh*sum(simb>0),sum(simb>0))
}
thrsh <- c(0.1,0.2,0.5,1)
tstotcm <- matrix(rep(0,16),4,4)
for (i in 1:4){
  tstotcm[i,] <- c(prwsstrg(pt,thrsh[i]),prwsstrg(qt,thrsh[i]))
}
tstotcm
##判断逻辑时,单写|或者&是对所有判断语句一一判断而双写是对全部元素求一个逻辑判断,xor()/all()/any()
