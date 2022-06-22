manipulate( { #################################
  pdp = list(c(a1,a2,a3,a4,a5,0),
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
  plot(plst,mx[1,],type='l',col=1,ylim=c(0,h1),main="Market Share",
       ylab="",xlab="Price")
  abline(v=pst,col='pink' )
  for(i in 2:nrow(mx)) lines(plst,mx[i,],col=i)
  legend("topleft",as.character(1:4),lwd=2,col=1:4)
  plot(plst,profit,type='l',ylim=c(0,h2),ylab="",xlab="Price",
       main=sprintf("Profit (%.1f @ %d)",profit[w],pst))
  abline(v=pst,col='pink' )
}, ############################################
a1 = picker(1,2,3),a2 = picker(1,2,3),a3 = picker(1,2,3),
a4 = picker(1,2),a5 = picker(1,2),
h1 = slider(40,100,60,step=10),h2 = slider(1000,6000,3000,step=1000)
) #############################################
