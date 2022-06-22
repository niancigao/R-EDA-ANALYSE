load("Sim12B.Rdata")

library(manipulate) # 操縱

manipulate({
  payoff = matrix(c(TN,FN,FP,TP),2,2)                         # 報酬矩陣
  
  cutoff = seq(0, 1, 0.01)
  
  result = sapply(cutoff, function(p) {
    cm = table(factor(y==1, c(F,T)), factor(pred>p, c(F,T))) 
    sum(cm * payoff) # sum of confusion * payoff matrix
  }) / 1000
  
  i = which.max(result)
  
  par(cex=0.7)
  
  plot(cutoff, result, type='l', col='cyan', lwd=2, main=sprintf(
    "Optomal Expected Result: K$%.2f @ %.2f",result[i],cutoff[i]),
    ylim=c(-320, -120))
  abline(v=seq(0,1,0.1),h=seq(-400,-100,25),col='lightgray',lty=3)
  points(cutoff[i], result[i], pch=20, col='red', cex=2)
},
                                                              # slider : 滑塊
TN = slider(-100,0,   0,"TN(無行動)",step=5),                 # 報酬矩陣,初始值: 0
FN = slider(-100,0,-100,"FN(平均風險成本)",step=5),           # 報酬矩陣,初始值: -100
FP = slider(-100,0, -10,"FP(行動成本)",step=5),               # 報酬矩陣,初始值: -10
TP = slider(-100,0, -50,"TP(行動成本+較小風險成本)",step=5)   # 報酬矩陣,初始值: -50
) 

# getwd() 看本地路徑
# dir()   列出那裡存在的文件
# 記得設為主路徑
