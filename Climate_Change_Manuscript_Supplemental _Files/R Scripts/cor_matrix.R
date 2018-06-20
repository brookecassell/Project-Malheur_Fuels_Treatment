#This script was obtained from Dr. Yangdong Pan, Department of Environmental Science and Management,
#Portland State University, 2015. It creates a plot of Pearson's correlations between multiple variables.

cor.matrix<-function(x,data=NA,cor.method="pearson",add1to1=F,...){
  # panel.hist function adds the histogram
  options(warning=F)
  panel.hist <- function(x)
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE,breaks=30)
    breaks <- h$breaks
    nB <- length(breaks)
    y <- h$counts
    y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="lightblue")
    box()
  }
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
  {
    usr <- par("usr")
    on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y,method="spearman")
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * 1.5*abs(r))
  }
  panel.smooth2<-function(x, y, bg = NA, pch = par("pch"),cex = 1, col.smooth = "red", span = 2/3,iter = 3,plot1to1=add1to1)
  {
    points(x, y, pch = pch, cex = cex,...)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)){
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), col = col.smooth)
    }
    if(plot1to1){
      mn<-min(c(x,y),na.rm=T)
      mx<-max(c(x,y),na.rm=T)
      new.x<-seq(mn,mx,length.out=10)
      points(new.x,new.x,type='l',col="blue",lwd=2)
    }
  }
  if (class(x)=="formula"){
    x<-model.frame(x,data=data)
    
  }
  pairs(x,upper.panel=panel.smooth2,lower.panel=panel.cor,diag.panel=panel.hist,
        cex.labels = 1, font.labels=2)
  options(warning=T)
}