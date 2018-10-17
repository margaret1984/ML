#!	/bin/sh
library(ggplot2)
library(psych)
###############
####Read data and set parametres####
###############
dat<-read.table	("cross-validation-stats.txt",	header=T,	sep="\t") 
dat[dat=="NA"]=NA
dat[dat==" "]=NA
###############
###external function for multiple plots of ggplot2###
################
###############
###this is a code downloaded from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/###
###############
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
#   # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {  
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
 }
###############
###plot summary  CV###
################
palette.jco = c('#0073C2', '#EFC000', '#868686', '#CD534C', '#7AA6DC','#003C67', '#8F7700', '#3B3B3B', '#A73030', '#4A6990')
myplot <- function(data,clas, clas.name){
    ggplot(data, aes(x=Alg, y=clas, colour=Alg))+ 
    geom_boxplot(width=0.5, size=1)+
    theme_bw() + 
    theme(legend.position="none",axis.title.y = element_text(colour="Black",size=15),axis.text = element_text(colour="Black",size=12))+ 
    xlab("")+ 
    ylab(clas.name)+
    scale_colour_manual(values=palette.jco)
}
p.acc <-myplot(data=dat, clas=dat$Accuracy,clas.name="Accuracy")
p.sens <- myplot(data=dat, clas=dat$Sensitivity,clas.name="Sensitivity") 
p.spec <- myplot(data=dat, clas=dat$Specificity,clas.name="Specificity")
jpeg("cross-validation-stats.jpg", width=4200, height=2200,units="px",res=300)
multiplot(p.acc,p.sens,p.spec,cols=2)
dev.off()
###############
###get summary stats###
################
perf.stat = describeBy(dat[,c("Accuracy","Sensitivity" ,"Specificity")], dat$Alg,digits=3) ####choose columns where performance stats are stored
perf.stat 2 <- do.call("rbind",perf.stat )
write.table(a2,file="cross-validation-stats-summary-stats.txt", quote=F, sep="\t")
