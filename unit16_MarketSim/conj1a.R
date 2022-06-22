###################################################################
### 1. 聯合分析 Conjoint Analysis
# https://bap2.cm.nsysu.edu.tw/bap2/ebooks/conjoint_book/index.html
###################################################################
library(conjoint)
library(fpc)
library(grDevices)
library(latex2exp)
library(manipulate)
library(rAmCharts)
library(magrittr)
library(highcharter)
rm(list=ls(all=TRUE))
options(digits=4); par0 = par(cex=0.8)
col1 = c('magenta','steelblue','orange','green3','brown')

### 1.1 案例資料
(profiles = read.csv("./tea/profiles.csv") )
(levels =   read.csv("./tea/levels.csv",stringsAsFactors=F) )
(ratings =  read.csv("./tea/ratings.csv") )
(att = apply(profiles,2,max) )
(attl = unlist(sapply(1:length(att), function(x) rep(x , att[x]))) )

### 1.2 屬性的重要性
im = caImportance(y=ratings, x=profiles)
names(im) = names(att)
im

### 1.3 選項的平均價值 (APW)
apw = caUtilities(y=ratings, x=profiles, z=levels)
names(apw) = c('intercept', levels[,1])
apw  

### 1.4 受測群體的平均喜好
par(mfcol=c(2,1),cex=.8)
barplot(im,las=2,col=1:length(im),main="Attribute Importance")
barplot(apw[2:length(apw)],las=2,col=attl,main="Average Part Worths (utils)")

### 1.5 受測者的個人差異
W = caPartUtilities(y=ratings, x=profiles, z=levels)
dim(W)
head(W)

### 1.6 效用函數
UT = function(v) rowSums( W[, c(1, v + c(1,4,7,10))] )
UT(c(1,2,1,2))

### 1.7 市占率
pds = list(c(1,1,1,1), c(2,2,2,2), c(1,2,1,2), c(2,1,2,1))
uts = sapply(pds, UT)
dim(uts); head(uts)
(tb = table(apply(uts, 1, which.max)))
amPie(data.frame(label=names(tb), value=as.vector(tb)),
      inner_radius=50, depth=10, show_values=TRUE, legend=TRUE)

###################################################################
### 2. 產品設計與定價策略 New Product Design
###################################################################
### 2.1 成本函數
costs = list(bitter =  c(0.3, 0.2, 0.1),
             variety = c(0.9, 0.5, 0.2),
             kind  =   c(0.5, 0.3, 0.6),
             arom =    c(0.7, 0.4) )
PC = function(pd) {
  2.5 + sum(sapply(1:length(att), function(i) costs[[i]][pd[i]] )) }
PC(c(1,1,3,1)); PC(c(3,3,2,2))

### 2.2 量關係、營收、獲利
pqr = function(p, m, u) {
  q = sapply(p, function(x) sum(u > x) )
  pf = q * (p-m); ip = which.max(pf)
  r  = q * p;     ir = which.max(r)
  par(cex=0.7,mar=c(5,4,4,3))
  plot(p,q,type='l',main="Demand Cruve",xlab="price",ylab="qty",ylim=c(0,80))
  abline(v=p[ip],col='pink'); abline(v=p[ir],col='cyan')
  abline(h=seq(20,80,20),lty=3,col='grey')
  plot(p, r, type='l',main="Revenue",ylab="",ylim=c(0,300),font.lab=2,
       xlab=sprintf("P = %.1f, Q = %d, R = %.1f, Pf = %.1f",
                    p[ir], q[ir], r[ir], pf[ir] ))
  abline(v=p[ir],col='cyan');
  abline(h=seq(50,250,50),lty=3,col='grey')
  plot(p, pf, type='l',main="Profit",ylab="",ylim=c(0,110),font.lab=2,
       xlab=sprintf("P = %.1f, Q = %d, R = %.1f, Pf =%.1f",
                    p[ip], q[ip], r[ip], pf[ip] )  )
  abline(v=p[ip],col='pink');
  abline(h=seq(20,100,20),lty=3,col='grey')
  text(3.5,100,sprintf("MC = %.1f",m),pos=4,font=2)
}

pd1 = c(1,1,3,1)
ut1 = UT(pd1)
par(mfcol=c(2,2),cex=0.7,mar=c(5,3,4,3))
hist(ut1,-5:13,main="Distribution of Utility",xaxt='n',col='gray',
     border='white',xlab="Utility",ylab="",xlim=c(-4,14))
axis(1,at=seq(-4,14,2))
pqr(seq(3.5,11,0.1),PC(pd1),ut1)

### SIM1 模擬程式1
source("sim1.R")

### 2.3 程式模擬
pds = as.matrix(expand.grid(1:3,1:3,1:3,1:2))
X = t(apply(pds, 1, function (v) {
  c = PC(v)
  u = rowSums(W[, c(1, v + c(1,4,7,10)) ])
  X = t( sapply(seq(3,10,0.1), function (p) {
    q = sum(u > p)
    c(p, q, q * p, q * (p - c)) }) )
  c(mean(u), c, X[which.max(X[,3]),], X[which.max(X[,4]),])
  }))
X = data.frame(cbind(pds,X))
colnames(X) = c('v1','v2','v3','v4','ut','cost',
                'p1','q1','r1','pf1','p2','q2','r2','pf2')
head(X[order(- X$r1),],10)  # Max Revenue
head(X[order(- X$pf2),],10) # Max Profit

subset(X, pf1>0 & q1>50)

subset(X, q2>30)

### 2.5 策略空間
df = data.frame(revenue=c(X$r1,X$r2),profit=c(X$pf1,X$pf2),
                p=c(X$p1,X$p2),q=c(X$q1,X$q2),
                opt=c(rep('opt.revenue',nrow(X)),rep('opt.profit',nrow(X))),
                lab=rep(apply(X[,1:4],1,paste0,collapse=''),2)  )
hchart(df, "scatter", hcaes(x=revenue, y=profit, group=opt, lab, p, q)) %>%
  hc_plotOptions(series=list(allowPointSelect=T)) %>%
  hc_chart(zoomType = "xy") %>% hc_add_theme(hc_theme_flat()) %>%
  hc_tooltip(headerFormat = "",valueDecimals=1,borderWidth=2,
             hideDelay=100,useHTML=T,padding=3,
             pointFormat="<center><b>({point.lab})</b></center> price: {point.p}<br>
                 qty: {point.q}<br> RV: {point.x}<br> PF: {point.y}") %>%
  hc_colors(hex_to_rgba(c('darkgreen','orange'), alpha = 0.65)) %>%
  hc_legend(floating=T,align='left',verticalAlign='bottom')



###################################################################
### 3. 集群分析與市場區隔 Segmentation
###################################################################
### 3.1 族群分析 (K-means)
S = kmeans(W,3)$cluster
table(S)

### 3.2 Average Part Worth per Segment (APWPS)
sapply(1:max(S), function(i) colMeans(W[S==i,]) )

### 模擬程式2
Seg = function(k, seed, seeding=F) {
  P = matrix(rep(0, k*4 ), ncol=4)
  U = matrix(rep(0, k*nrow(W)), ncol=k )
  lx = c(1,2,2)
  for(i in 2:k) lx = c(lx, (i-1)*2 + c(1,2,2) )
  h = rep.int(1,k)
  if(k==3) h[1]=2 else if(k>=4) h[1]=3
  layout(matrix(c(1,2,2,2+lx),k+1,3,byrow=T),heights=h  )
  sd = ifelse(seeding, sample.int(1000,1), seed)
  set.seed(sd)
  S = kmeans(W,k)$cluster
  n = as.vector(table(S))
  cat(k, 'segments, seed =', sd, ', N =', n, '\n')
  m = apply(W[,2:ncol(W)], 2, function(x) tapply(x ,S, mean))
  par(mar=c(2,1,2,1))
  pie(table(S),radius=1,col=col1[1:k],font=2)
  dc = discrcoord(W, S)$proj[,1:2]
  par(mar=c(1,2,1,2))
  plot(dc[,1],dc[,2],type='p',col=col1[S],pch=19,cex=1.5)
  for(i in 1:k) {
    par(mar=c(2,2,2,1))
    barplot(m[i,],las=2,axes=F,axisnames=F,col=col1[i],
            width=.5,space=1,border=NA)
    abline(h=0, v=c(0,3,6,9,11)+0.25 )
    P[i,1] = which.max(m[i,1:3])
    P[i,2] = which.max(m[i,4:6])
    P[i,3] = which.max(m[i,7:9])
    P[i,4] = which.max(m[i,10:11])
    U[,i] = rowSums(W[, c(1, P[i,]+c(1,4,7,10)) ])
    mtx = sapply(1:k, function (k) sapply(-4:13, function(x)
      sum(U[S==k,i] >= x & U[S==k,i] < x+1 ))  )
    mtx = t(as.matrix(mtx))
    par(cex=0.6,mar=c(2,3,2,2))
    barplot(mtx,las=2,ylim=c(0,30),col=col1)
    abline(h=seq(5,25,5),col='lightgrey',lty=3)
    z = paste(P[i,],collapse=', ')
    text(0,28,sprintf("Produnt %d: {%s}, %.1f%%",
                      i,z,100*n[i]/nrow(W)),cex=1.2,pos=4,col=col1[i])
  }}
Seg(2,734)

### SIM2 模擬程式2
manipulate( Seg(k, 123, seeding),
  k=slider(2,5,2,step=1),
  seeding=button("Reset Seed") )

###################################################################
### 4. 雙產品市場 Markets of Two Products
###################################################################
### 4.1 競爭之下的價量關係
p1 = seq(4,12,0.1); p2 = seq(4,12,0.1)
pd1 = c(1,2,3,1); pd2 = c(3,1,1,1)
mc1 = PC(pd1); mc2 = PC(pd2)
u1 = rowSums( W[,c(1,c(1,4,7,10)+pd1)] )
u2 = rowSums( W[,c(1,c(1,4,7,10)+pd2)] )
x1 = 8; x2 = 7
s1 = u1 - x1; s2 = u2 -x2
dcs = rep(4, nrow(W))
dcs[s1 > 0 & s1 > s2] = 1
dcs[s2 > 0 & s2 > s1] = 2
dcs[s1 > 0 & s1 == s2] = 3

clr = c('magenta','blue','gold','gray')
Hist = function(id, p, u, pd1, pd2, dcs) {
  mtx = sapply(1:max(dcs), function (d) sapply(-4:13, function(x)
    sum(u[dcs==d] >= x & u[dcs==d] < x+1 ))  )
  mtx = t(as.matrix(mtx))
  colnames(mtx) = -4:13
  par(cex=0.7,mar=c(3,3,2,2))
  barplot(mtx,ylim=c(0,30),col=c(clr[1:2],'pink','gray'),cex.axis=0.8,
          width=0.8,space=0.25,border=NA)
  abline(v = p + 4.1, col=clr[id])
  abline(h=seq(5,25,5),col='lightgrey',lty=3)
  if(id==1) pdi=pd1 else pdi=pd2
  tz = sprintf("Dist of Utility (%d): %s", id, paste(pdi,collapse=',') )
  text(0,28,tz,col=clr[id],pos=4,font=2)
}

Profit = function (id, p, px, m, u, s2, flag=F) {
  q = sapply(p, function(x) sum(u > x & u - x > s2) )
  pf = (p-m)*q
  pst = p[which.max(pf)]
  qx = sum(u > px & u - px > s2) + 0.5*sum(u > px & u - px == s2)
  rx = qx * px
  pfx = qx * (px-m)
  par(cex=0.7,mar=c(3,3,3,2))
  plot(p, pf, type='l',ylab="",ylim=c(0,100),xlab="",main=
          sprintf("mc = %.1f\nProfit = %.1f @ %.1f",m,pfx,px),
          col.main=clr[id], cex.axis=0.8, cex.main=1)
  abline(v=px,col=clr[id])
  abline(v=pst,lty=2)
  abline(h=c(20,40,60,80),col='lightgray',lty=2)
  text(max(p)-0.5, 103,TeX(sprintf("%.1f^*",pst)),pos=1)
  if(flag) points(max(p)-0.6, 100, pch=19, col='green')
  return( c(pfx,rx,pst) )
}

layout(matrix(c(1,2,3,4,5,7,6,7),4,2,byrow=T),
       widths=c(1,1.5),heights=c(1,1,0.8,1.2))
f1 = Profit(1, p1, x1, mc1, u1, s2)
Hist(1,x1,u1,pd1,pd2,dcs)
f2 = Profit(2, p2, x2, mc2, u2, s1)
Hist(2,x2,u2,pd1,pd2,dcs)

pene = 100 * sum(dcs!=4)/nrow(W)
tpro = sum(dcs==1)*(x1 - mc1) + sum(dcs==2)*(x2 - mc2)
trev = sum(dcs==1)*x1 + sum(dcs==2)*x2

tb = table(dcs)
par(mar=c(1,1,2,1))
pie(tb,paste0(as.character(tb),'%'),col=clr[as.integer(names(tb))],
    main=sprintf("Panetration = %.1f%%",pene),
    cex.main=1.1,font=2,cex=1,radius=0.9)

par(mar=c(3,4,3,2),cex=0.7)
bp = cbind(c(f1[1],f1[2]-f1[1],0,0),c(0,0,f2[1],f2[2]-f2[1]))
barplot(bp,names=c('prod-1','prod-2'),main="Revenue & Profit (Margin)",
        ylim=c(0,280),col=c(clr[1],'pink',clr[2],'cyan'),
        cex.main=1.1,border=NA )
abline(h=seq(50,250,50),lty=3,col='gray')
for(t in list(c(0.7,f1[1],f1[1]),c(0.7,f1[2],f1[2]),
              c(1.9,f2[1],f2[1]),c(1.9,f2[2],f2[2]) ) )
  text(t[1],t[2]-10,t[3],font=2,pos=3)
text(0.7,250,sprintf("(%.1f%%)",100*f1[1]/f1[2]),font=2,pos=3)
text(1.9,250,sprintf("(%.1f%%)",100*f2[1]/f2[2]),font=2,pos=3)

par(mar=c(6,3,6,2))
plot(u1,u2,pch=20,col=clr[dcs],cex=2,ylab="",xlab="",main=
       sprintf("\n\nUtility Space\nTotal Revenue (%.1f) & Profit (%.1f)",
               trev,tpro),cex.main=1.1)
abline(v=x1,col=clr[1]);abline(h=x2,col=clr[2])


### SIM3 模擬程式3
source('sim3.R')

### 4.2 競爭策略 
p1 = seq(4,12,0.1); p2 = seq(4,12,0.1)
pd1 = c(1,2,3,1); pd2 = c(3,2,3,1)  # 4.2
#pd1 = c(1,2,3,1); pd2 = c(3,3,3,2)  # 4.4
mc1 = PC(pd1); mc2 = PC(pd2)
u1 = rowSums( W[,c(1,c(1,4,7,10)+pd1)] )
u2 = rowSums( W[,c(1,c(1,4,7,10)+pd2)] )

q1 = q2 = f0 = f1 = f2 = matrix(rep(0, length(p1)*length(p2)), nrow=length(p1))
for(i in 1:length(p1)) for(j in 1:length(p2)) {
  s1= u1 - p1[i]; s2 = u2 - p2[j]
  q1[i,j] = sum( s1 > 0 & s1 >= s2 )
  q2[i,j] = sum( s2 > 0 & s1 <  s2 )
  f1[i,j] = (p1[i]-mc1) * q1[i,j]
  f2[i,j] = (p2[j]-mc2) * q2[i,j]
  f0 = f1  + f2 }
m0 = max(f0); w0 = which(f0 == max(f0), arr.ind = T)
p1s = p1[w0[1]]; p2s = p2[w0[2]]

par(mfcol=c(1,1),mar=c(3,3,3,4))
filled.contour(p1, p2, f0, color=terrain.colors, nlevels=8,
  plot.title=title(main="\nPrice Space",
                   xlab='Price 1',ylab='Price 2',cex.main=1),
  key.title=title(main="\nTotal Profit",cex.main=0.8),
  key.axes=axis(4, cex.axis=0.8),
  plot.axes={
    axis(1, cex.axis=0.8); axis(2, cex.axis=0.8)
    lines(p1[apply(f1,2,which.max)],p2,col=clr[1])
    lines(p1,p2[apply(f2,1,which.max)],col=clr[2])
    text(8.5,12,TeX(sprintf("$\\pi^*(%.1f,%.1f)=%.1f$",
                p1s,p2s,max(f0))),pos=1)
    points(p1s,p2s,pch=19)
  })

### 
cbn = combn(1:nrow(pds),2)
pds[cbn[1,1431],]; pds[cbn[2,1431],]

library(doParallel)
library(foreach)
(core <- makeCluster(detectCores()))
registerDoParallel(core)
getDoParWorkers()

system.time({
fe = foreach(k = 1:ncol(cbn), .combine = rbind) %dopar% {
  pd1 = as.vector(pds[cbn[1,k],])
  pd2 = as.vector(pds[cbn[2,k],])
  u1 = rowSums(W[,c(1, pd1+c(1,4,7,10))])
  u2 = rowSums(W[,c(1, pd2+c(1,4,7,10))])
  mc1 = PC(pd1); mc2 = PC(pd2)
  q1 = q2 = f0 = f1 = f2 = matrix(rep(0, length(p1)*length(p2)), nrow=length(p1))
  for(i in 1:length(p1)) for(j in 1:length(p2)) {
    s1= u1 - p1[i]; s2 = u2 - p2[j]
    q1[i,j] = sum( s1 > 0 & s1 >= s2 )
    q2[i,j] = sum( s2 > 0 & s1 <  s2 )
    f1[i,j] = (p1[i]-mc1) * q1[i,j]
    f2[i,j] = (p2[j]-mc2) * q2[i,j]
    f0 = f1  + f2 }
  m0 = max(f0)
  w0 = which(f0 == max(f0), arr.ind = T)
  p1s = p1[w0[1]]; p2s = p2[w0[2]]
  c(m0, p1s, p2s, k)
} })

stopCluster(core)
fe = fe[order(- fe[,1]),]
head(fe); tail(fe)
pds[cbn[1,935],]; pds[cbn[2,935],]

### 4.4 re-used the code in 4.2

###################################################################
### 5. A mLogit CBC Example
###################################################################
### 5.1
pw = read.csv("./tv/tv.csv")
levels = names(pw)
att = c(3,3,3,2,2,4)
names(att) = c("Brand","Screen","Sound","CB","PIP","Price")
levcol = c(1,1,1,2,2,2,3,3,3,4,4,5,5,6,6,6,6)
apw = apply(pw,2,mean)
ppw = 200 * apw[1:13]/(apw[14]-apw[17])
im = c(apw[3]-apw[1], apw[6]-apw[4], apw[9]-apw[7],
       apw[11]-apw[10], apw[13]-apw[12], apw[14]-apw[17])
im = 100 * im/sum(im)
names(im) = names(att)

par(mar=c(5,3,4,2),mfrow=c(3,1))
barplot(im,las=2,col=1:length(im),main="Attribute Importance (%)")
abline(h=seq(5,25,5),lty=3,col='grey')
barplot(apw,las=2,col=levcol,main="Part Worths (utils)")
abline(h=seq(-1.5,1.0,0.5),lty=3,col='grey')
barplot(ppw,las=2,col=levcol,main="Part Worths ($)",ylim=c(-120,100))
abline(h=seq(-100,100,50),lty=3,col='grey')

###5.2
pds = list(c(1,2,1,2,1,1),c(2,1,1,2,2,2),c(3,2,2,1,2,3),c(3,3,3,2,1,4))
ut = sapply(pds, function(pd) rowSums(pw[ pd + c(0,3,6,9,11,13) ]))
pb = t(apply(ut, 1, function (u) exp(u)/sum(exp(u)) ))
qty = colSums(pb)
100 * qty/sum(qty)

### 5.3
#utP = function(p) {
#  if     (p >= 300 & p < 350)  ((350-p)*pw$p300 + (p-300)*pw$p350)/50
#  else if(p >= 350 & p < 400)  ((400-p)*pw$p350 + (p-350)*pw$p400)/50
#  else if(p >= 400 & p <= 450) ((450-p)*pw$p400 + (p-400)*pw$p450)/50 }
p1 = c(300,350,400,450)
coef = apply(pw[,14:17], 1, function(x) lm(ut~.,data.frame(ut=x,p1,p1^2,p1^3))$coef )
utP = function(p) t(coef) %*% c(1, p, p^2, p^3)

pdp = list(c(1,2,1,2,1,325),c(2,1,1,2,2,399),c(3,2,2,1,2,410),c(3,3,3,2,1,450))
ut = sapply(pdp, function(x) rowSums(pw[ x[1:5] + c(0,3,6,9,11) ]) + utP(x[6]))
pb = t(apply(ut, 1, function (u) exp(u)/sum(exp(u)) ))
qty = colSums(pb)
100 * qty/sum(qty)

### 5.4
costs = rbind(c(60,70,70),c(50,70,120),c(0,20,80),c(10,50,0),c(0,50,0))
MC = function(pd) sum(sapply(1:5, function(i) costs[i, pd[i]] ))

pdp = list(c(1,2,2,1,2,0),
           c(2,1,1,2,2,300),
           c(3,2,2,1,2,350),c(3,3,3,2,1,400))
plst = seq(300,450)

mx = sapply(plst, function(x) {
  pdp[[1]][6] = x
  ut = sapply(pdp, function(z) {rowSums(pw[ z[1:5] + c(0,3,6,9,11) ]) + utP(z[6])})
  pb = t(apply(ut, 1, function (u) exp(u)/sum(exp(u)) ))
  qty = colSums(pb)
  100 * qty / sum(qty)  })
par(mfrow=c(2,1),cex=0.8)
profit = mx[1,] * (plst - MC(pdp[[1]]))
w = which.max(profit); pst=plst[w]
plot(plst,mx[1,],type='l',col=1,ylim=c(0,45),main="Market Share",
     ylab="",xlab="Price")
abline(v=pst,col='pink' )
for(i in 2:nrow(mx)) lines(plst,mx[i,],col=i)
plot(plst,profit,type='l',ylim=c(0,3000),ylab="",xlab="Price",
     main=sprintf("Profit (%.1f @ %d)",profit[w],pst))
abline(v=pst,col='pink' )

source("sim4.R")

