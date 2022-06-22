manipulate( { #################################
  P = matrix(rep(0, k*4 ), ncol=4)
  U = matrix(rep(0, k*nrow(W)), ncol=k )
  lx = c(1,2,2)
  for(i in 2:k) lx = c(lx, (i-1)*2 + c(1,2,2) )
  h = rep.int(1,k)
  if(k==3) h[1]=2 else if(k>=4) h[1]=3
  layout(matrix(c(1,2,2,2+lx),k+1,3,byrow=T),heights=h  )
  sd = ifelse(seeding, sample.int(1000,1), 123)
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
  }
}, ############################################
k = slider(2,5,2,step=1),
seeding = button("Reset Seed")
) #############################################
