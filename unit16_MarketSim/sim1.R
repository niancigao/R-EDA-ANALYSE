manipulate( { #################################
  pd1 = c(a1, a2, a3, a4)
  ut1 = rowSums(W[, c(1, c(1,4,7,10)+pd1) ] )
  par(mfcol=c(2,2),cex=0.7,mar=c(5,3,4,3))
  hist(ut1,-5:13,main="Distribution of Utility",xaxt='n',col='gray',
       border='white',xlab="Utility",ylab="",xlim=c(-4,14))
  axis(1,at=seq(-4,14,2))
  pqr(seq(3.5,11,0.1),PC(pd1),ut1)
}, ############################################
a1 = picker(1,2,3),
a2 = picker(1,2,3),
a3 = picker(1,2,3),
a4 = picker(1,2)
) #############################################
