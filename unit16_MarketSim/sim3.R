L1 = TRUE
manipulate( { #################################
  if(Auto) {
    if(L1) x1 = pst1 else x2 = pst2
    L1 = ! L1 }
  pd1 = as.integer(c(A11, A12, A13, A14))
  pd2 = as.integer(c(A21, A22, A23, A24))
  mc1 = PC(pd1); mc2 = PC(pd2)
  u1 = rowSums( W[, c(1,c(1,4,7,10)+pd1) ] )
  u2 = rowSums( W[, c(1,c(1,4,7,10)+pd2) ] )
  s1 = u1 - x1; s2 = u2 -x2
  dcs = rep(4, nrow(W))
  dcs[s1 > 0 & s1 > s2] = 1
  dcs[s2 > 0 & s2 > s1] = 2
  dcs[s1 > 0 & s1 == s2] = 3

  layout(matrix(c(1,2,3,4,4,5),3,2,byrow=F),
         widths=c(0.9,1.2),heights=c(1,1,1.2))
  # nm = sapply(list(pd1,pd2),paste0, collapse="")
  f1 = Profit(1, p1, x1, mc1, u1, s2, L1)
  f2 = Profit(2, p2, x2, mc2, u2, s1, !L1)
  pst1 = f1[3]; pst2=f2[3]
  cat(L1, pst1, pst2, '\n')

  pene = 100 * sum(dcs!=4)/nrow(W)
  tpro = sum(dcs==1)*(x1 - mc1) + sum(dcs==2)*(x2 - mc2)
  trev = sum(dcs==1)*x1 + sum(dcs==2)*x2
  tb = table(dcs)
  par(mar=c(3,3,4,3))
  pie(tb,paste0(as.character(tb),'%'),col=clr[as.integer(names(tb))],
      main=sprintf("Panetration = %.1f%%",pene),
      cex.main=1.1,font=2,cex=1,border='white')

  par(mar=c(5,4,5,2))
  plot(u1,u2,pch=20,col=clr[dcs],cex=2,cex.lab=1.2,
    xlab="U1",ylab="U2",main=
      sprintf("\n\nUtility Space\nTotal Revenue (%.1f) & Profit (%.1f)",
              trev,tpro),cex.main=1.1)
  abline(v=x1,col=clr[1]);abline(h=x2,col=clr[2])
  lines(x=c(x1,x1+10),y=c(x2,x2+10),col='gray',lty=3,lwd=2)

  par(mar=c(3,4,3,2),cex=0.7)
  bp = cbind(c(f1[1],f1[2]-f1[1],0,0),c(0,0,f2[1],f2[2]-f2[1]))
  barplot(bp,names=c('prod-1','prod-2'),main="Revenue & Profit (Margin)",
          ylim=c(0,280),col=c(clr[1],'pink',clr[2],'cyan'),
          cex.main=1.1,border=NA )
  abline(h=seq(50,250,50),lty=3,col='gray')
  for(t in list(c(0.7,f1[1],f1[1]),c(0.7,f1[2],f1[2]),
                c(1.9,f2[1],f2[1]),c(1.9,f2[2],f2[2]) ) )
    text(round(t[1],1),round(t[2]-10,1),round(t[3],1),font=2,pos=3)
  text(0.7,250,sprintf("(%.1f%%)",100*f1[1]/f1[2]),font=2,pos=3)
  text(1.9,250,sprintf("(%.1f%%)",100*f2[1]/f2[2]),font=2,pos=3)
}, ############################################
Auto = button("Auto Price"),
x1 = slider(5,12,7,label="(1) Price",step=0.1,ticks=6:11),
A11 = picker("1","2","3",initial="1",label="(1) a1"),
A12 = picker("1","2","3",initial="2",label="(1) a2"),
A13 = picker("1","2","3",initial="3",label="(1) a3"),
A14 = picker("1","2",    initial="1",label="(1) a4"),
x2 = slider(5,12,6,label="(2) Price",step=0.1,ticks=6:11),
A21 = picker("1","2","3",initial="1",label="(2) a1"),
A22 = picker("1","2","3",initial="2",label="(2) a2"),
A23 = picker("1","2","3",initial="3",label="(2) a3"),
A24 = picker("1","2",    initial="2",label="(2) a4")
) #############################################
