######################################
### This file generates Figure 26.2
### Nested Choice
######################################
### Uses package spatstat
######################################

library(spatstat)

pdf("HANSEN26-2.pdf",family="Helvetica",colormodel='cmyk',width=4.5,height=4.5)
plot.new()
plot.window(c(-1,1),c(-1,1))
text(0,.8,"Car",cex=.9)
text(-.5,0,"Compact",cex=.75)
text(.5,0,"Sports Utility",cex=.75)
text(-.8,-.8,"Honda",cex=.75)
text(-.8,-.95,"Civic",cex=.75)
text(-.2,-.8,"Ford",cex=.75)
text(-.2,-.95,"Fusion",cex=.75)
text(.3,-.8,"Honda",cex=.75)
text(.3,-.95,"CR-V",cex=.75)
text(.88,-.8,"Ford",cex=.75)
text(.88,-.95,"Escape",cex=.75)
lines(c(-.05,-.475),c(.65,.1))
lines(c(.05,.475),c(.65,.1))
lines(c(-.55,-.8),c(-.1,-.65))
lines(c(-.47,-.25),c(-.1,-.65))
lines(c(.47,.3),c(-.1,-.65))
lines(c(.55,.87),c(-.1,-.65))
dev.off()

postscript("HANSEN26-2.eps",paper="special",width=4.5,height=4.5,horizontal=FALSE,onefile=FALSE,family="Helvetica",colormodel='cmyk')
plot.new()
plot.window(c(-1,1),c(-1,1))
text(0,.8,"Car",cex=.9)
text(-.5,0,"Compact",cex=.75)
text(.5,0,"Sports Utility",cex=.75)
text(-.8,-.8,"Honda",cex=.75)
text(-.8,-.95,"Civic",cex=.75)
text(-.2,-.8,"Ford",cex=.75)
text(-.2,-.95,"Fusion",cex=.75)
text(.3,-.8,"Honda",cex=.75)
text(.3,-.95,"CR-V",cex=.75)
text(.88,-.8,"Ford",cex=.75)
text(.88,-.95,"Escape",cex=.75)
lines(c(-.05,-.475),c(.65,.1))
lines(c(.05,.475),c(.65,.1))
lines(c(-.55,-.8),c(-.1,-.65))
lines(c(-.47,-.25),c(-.1,-.65))
lines(c(.47,.3),c(-.1,-.65))
lines(c(.55,.87),c(-.1,-.65))
dev.off()

