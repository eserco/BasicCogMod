## ---------------------------------------------------------------------------
##
## Some code to plot the Jazayeri & Shadlen (2010) data
##
## Hedderik van Rijn, December 2015 
##
## ---------------------------------------------------------------------------

load("dataJS.Rdat");

brown <- "#8b4513";
red <- "#ff1100";
black <- "#000000";
brownT <- "#8b451322";
redT <- "#ff110022";
blackT <- "#00000022";

## ---

par(mfrow=c(1,1))

plotDatJS <- with(datJS,aggregate(list(Tp=Tp),list(Ts=Ts,Cond=Cond),mean))

yrange <- range(plotDatJS$Ts)*c(.95,1.05)

with(plotDatJS[plotDatJS$Cond==3,],plot(Ts,Tp,type="b",col=red,lwd=2,ylim=yrange,xlim=yrange,main="J&S All"))
with(plotDatJS[plotDatJS$Cond==2,],lines(Ts,Tp,type="b",col=brown,lwd=2,ylim=yrange,xlim=yrange))
with(plotDatJS[plotDatJS$Cond==1,],lines(Ts,Tp,type="b",col=black,lwd=2,ylim=yrange,xlim=yrange))

lines(c(yrange[1],yrange[2]),c(yrange[1],yrange[2]),col="darkgrey",lty=2)

with(datJS[datJS$Cond==3,],points(jitter(Ts),Tp,col=redT,pch=".",cex=3))
with(datJS[datJS$Cond==2,],points(jitter(Ts),Tp,col=brownT,pch=".",cex=3))
with(datJS[datJS$Cond==1,],points(jitter(Ts),Tp,col=blackT,pch=".",cex=3))

