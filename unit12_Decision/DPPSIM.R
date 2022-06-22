library(shiny); library(caTools); library(ROCR)

style = "
.irs-bar {width: 100%; height: 25px; background: black; 
          border-top: 1px solid black; border-bottom: 1px solid black;}
.irs-bar-edge {background: black; border: 1px solid black; height: 25px; 
               border-radius: 0px; width: 20px;}
.irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
.irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
.irs-grid-pol {display: none;}
.irs-max {font-family: 'arial'; color: black;}
.irs-min {font-family: 'arial'; color: black;}
.irs-single {color:black; background:#6666ff;}
.irs-slider {width: 30px; height: 30px; top: 22px;}
p,li,title,body,h1,h2,h3,h4,h5{
  font-family: 'Trebuchet MS', '微軟正黑體', 'Microsoft JhengHei';
  }
"

options(shiny.usecairo = FALSE)

ui <- fluidPage(
  # tags$style(type = "text/css", style),
  titlePanel("分類預測機率分佈(cDPP)模擬器"),
  hr(),
  plotOutput("distPlot"),
  hr(), br(),
  fluidRow(
    column(4, wellPanel(
      style = "background-color: #f3e6ff;",
      sliderInput("cx", "臨界機率", ticks=F, 0, 1, 0.50, 0.01)
    )),
    column(4,  wellPanel( 
      style = "background-color: #ecffe6;",
      h4("綠點(Y=0)預測機率分佈："),
      sliderInput("mu0", "平均值", ticks=F, 0.04, 0.95, 0.45, 0.01),
      sliderInput("sd0", "標準差", ticks=F, 0.02, 0.20, 0.15, 0.005)
    )),
    column(4, wellPanel(
      style = "background-color: #ffe6e6;",
      h4("紅點(Y=1)預測機率分佈："),
      sliderInput("mu1", "平均值", ticks=F, 0.04, 0.95, 0.55, 0.01),
      sliderInput("sd1", "標準差", ticks=F, 0.02, 0.20, 0.15, 0.005)
    ))
  ),
  hr()
)

server <- function(input, output) {
  DPP4K = function(pred, y) {
    mx = table(y, pred > input$cx) 
    tn = sum(!y & pred <= input$cx)
    fn = sum( y & pred <= input$cx)
    fp = sum(!y & pred > input$cx)
    tp = sum( y & pred > input$cx)
    acc = (tn + tp)/length(pred)
    sens = tp/(fn+tp)
    spec = tn/(tn+fp)
    layout(matrix(c(1,1,1,2,2,2,2,2,2,2),1,10))
    par(mar=c(5,5,5,2),cex=1)
    auc = colAUC(pred,y)
    plot(performance(prediction(pred, y), "sens", "spec"),
         col='cyan',lwd=2,cex.main=1.25,
         main="\nROC曲線")
    abline(v=seq(0,1,0.1), h=seq(0,1,0.1), col='lightgray', lty=3)
    points(spec, sens, pch=20, col='purple', cex=2)
    text(spec, sens, input$cx, col='purple', cex=1.25, pos=1, font=2)
    
    str1 = "分類預測機率分佈\n"
    str2 = "auc=%.3f, acc=%.3f, sens=%.3f, spec=%.3f"
    str = sprintf(paste0(str1,str2),auc,acc,sens,spec)
    breaks = seq(0,1,0.02)
    cols = c(rgb(0,1,0,1/4),rgb(1,0,0,1/4))
    hist(pred[!y], breaks, col=cols[1], xlim=c(0,1), ylim=c(0, 1000),
         main=str, xlab= "模型所預測的機率，Pr[Y=1]", ylab="count",
         cex.sub=1, cex.main=1.25, cex.lab=1,
         sub="假設測試樣本中有3000/1000個紅/綠點，它們的預測機率分別都是常態分佈")
    hist(pred[y], breaks, col=cols[2], add=T)
    legend("topright",legend=0:1,fill=cols,title="實際Y值",
           box.col=rgb(1,1,1,0))
    abline(v=input$cx, col='purple', lty=3)
    text(input$cx,1000,sprintf("%.2f",input$cx),col='purple',font=2)
    text(input$cx,c(925,850,925,850),
         paste0(c("TN:","FN:","FP:","TP:"),c(tn,fn,fp,tp)),
         pos=c(2,2,4,4),col=rep(c("seagreen","firebrick1"),2), cex=0.8, font=2 )
    
  }
  
  output$distPlot <- renderPlot({
    N0=3000; N1=1000
    set.seed(100)
    pred = c(rnorm(N0,input$mu0,input$sd0), 
             rnorm(N1,input$mu1,input$sd1))
    pred[pred < 0] = 0; pred[pred > 1] = 1 
    y = c(rep(F,N0), rep(T,N1))
    DPP4K(pred, y)
  })}

shinyApp(ui = ui, server = server)



