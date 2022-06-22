### 4.3
require(doParallel)
require(foreach)

cbn = combn(1:nrow(pds),2)
p1=p2=seq(5,10,0.1)
(core <- makeCluster(detectCores()))
registerDoParallel(core)
getDoParWorkers()
print( system.time({
  X = foreach(k = 1:ncol(cbn), .combine = rbind) %dopar% {
    pd1 = as.vector(pds[cbn[1,k],])
    pd2 = as.vector(pds[cbn[2,k],])
    u1 = UT(pd1); u2 = UT(pd2)
    mc1 = PC(pd1); mc2 = PC(pd2)
    q1=q2=f0=f1=f2=g0=g1=g2 = matrix(rep(0, length(p1)*length(p2)), nrow=length(p1))
    for(i in 1:length(p1)) for(j in 1:length(p2)) {
      s1= u1 - p1[i]; s2 = u2 - p2[j]
      q1[i,j] = sum( s1 > 0 & s1 >= s2 )
      q2[i,j] = sum( s2 > 0 & s1 <  s2 )
      f1[i,j] = (p1[i]-mc1) * q1[i,j]
      f2[i,j] = (p2[j]-mc2) * q2[i,j]
      g1[i,j] = p1[i] * q1[i,j]
      g2[i,j] = p2[j] * q2[i,j]
      f0 = f1  + f2;  g0 = g1  + g2}
    pf0 = max(f0)
    w = which(f0 == pf0, arr.ind = T)
    p10 = p1[w[1]]; p20 = p2[w[2]]
    rv0 = g0[w[1],w[2]]
    qt0 = q1[w[1],w[2]] + q2[w[1],w[2]]
    rv1 = max(g0)
    w = which(g0 == rv1, arr.ind = T)
    p11 = p1[w[1]]; p21 = p2[w[2]]
    pf1 = f0[w[1],w[2]]
    qt1 = q1[w[1],w[2]] + q2[w[1],w[2]]
    c(pf0,rv0,qt0,p10,p20, pf1,rv1,qt1,p11,p21, k)
  } }))
stopCluster(core)

X = data.frame(X)
names(X) = c('pf0','rv0','qt0','p10','p20','pf1','rv1','qt1','p11','p21','k')
X$lab = paste("(",apply(pds[cbn[1,X$k],],1,paste0,collapse=""),
              "+",apply(pds[cbn[2,X$k],],1,paste0,collapse=""), ")",sep="")
df = data.frame(
  revenue=c(X$rv0,X$rv1),profit=c(X$pf0,X$pf1),q=c(X$qt0,X$qt1),
  p1=c(X$p10,X$p11),p2=c(X$p20,X$p21),
  opt=c(rep('opt.profit',nrow(X)),rep('opt.revenue',nrow(X))),
  lab=rep(X$lab,2))
save(df,file='df.Rdata')


