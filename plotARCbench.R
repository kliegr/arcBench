sens_data <-  read.csv("result/arc-rule-sensitivity.csv")

# Graph cars using a y axis that ranges from 0 to 12
plot(sens_data$input.rules,sens_data$time_arc,  cex.lab = 1.5,cex.axis=1.5, type="o", col="blue",xlab="Number of input rules",ylab="Time [seconds]", xaxt="n", pch=21)
axis(1,  cex.axis=1.5,at=c(1000,10000,20000,30000,40000,50000, 60000, 70000, 80000,90000, 100000), labels=c("1k","10k","20k","30k","40k","50k","60k","70k","80k","90k","100k")) 
# Graph trucks with red dashed line and square points
lines(sens_data$input.rules, sens_data$time_rcba, type="o", pch=22, col="red")
lines(sens_data$input.rules, sens_data$time_acba, type="o", pch=23,  col="green")
legend("topleft", c("arc","rcba","arulesCBA"), cex=1.4, 
       col=c("blue","red","green"), pch=21:23, lty=1:2)


size_data <-  read.csv("result/arc-data-size.csv")
plot(size_data$input.rows,size_data$time_arc,  cex.lab = 1.5,cex.axis=1.5,ylim=c(0,35),type="o", col="blue",xlab="Data size [instances]",ylab="Time [seconds]", xaxt="n",pch=21)
axis(1, cex.axis=1.5, at=c(0,25000,50000,75000,100000,125000,150000,175000,200000,225000,250000,275000), labels=c("0","25k","50k","75k","100k","125k","150k","175k","200k","225k","250k","275k")) 
# Graph trucks with red dashed line and square points
lines(size_data$input.rows, size_data$time_rcba, type="o", pch=22, col="red")
lines(size_data$input.rows, size_data$time_acba, type="o", pch=23,  col="green")
legend("topleft", c("arc","rcba","arulesCBA"), cex=1.4, 
       col=c("blue","red","green"), pch=21:23, lty=1:2)


