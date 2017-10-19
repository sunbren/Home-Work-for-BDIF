HW2
Question1
memory.df = read.csv("byte.csv",header = TRUE)
plot(memory.df$Byte~memory.df$year,type="o",main="The development of internal memory")

Question2
splines.reg.l1 = smooth.spline(x = memory.df$year, y = memory.df$Byte, spar = 0.2)
splines.reg.l2 = smooth.spline(x = memory.df$year, y = memory.df$Byte, spar = 1)
splines.reg.l3= smooth.spline(x = memory.df$year, y = memory.df$Byte, spar = 2)
lines(splines.reg.l1, col = "green", lwd = 2)  
lines(splines.reg.l2, col = "pink", lwd = 2)  
lines(splines.reg.l3, col = "blue", lwd = 2)  

Question3

lambda=4
x=6
dpois(x,lambda)

lambda=5
x=0
dpois(x,lambda)

