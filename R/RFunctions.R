stan_results<-function(m,params=paramnames){
  m_extr<-extract(m,pars=paramnames)
  par_names<-names(m_extr)
  means<-lapply(m_extr,mean)
  quantiles<-lapply(m_extr,
                    function(x)quantile(x,probs=c(0.025,0.975)))
  means<-data.frame(means)
  quants<-data.frame(quantiles)
  summry<-t(rbind(means,quants))
  colnames(summry)<-c("mean","lower","upper")
  summry
}

convert_smry<-function(msmry,model="m0"){
  m_smry<-data.frame(models=rep(model,
                                dim(msmry)[1]),parameter=rownames(msmry),
                     msmry)
  m_smry
}


magnifytext<-function(){
  theme(plot.title = element_text(lineheight=.8, size=14,face="bold"))+
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"))+
    theme(legend.text = element_text(colour="black", size = 14, face = "bold"))+
    theme(legend.title = element_text(colour="black", size = 14, face = "bold"))
}

plotresults<-function(dat,ylabel="log ms",maintitle="M4"){
  resultsplot<-ggplot(dat, aes(x=parameter, 
                               y=mean))+
    geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1)+
    ggtitle(maintitle)+
    xlab("Parameter")+
    ylab(ylabel)+
    geom_hline(yintercept=0)+
    geom_point(size=1)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    magnifytext()
  resultsplot
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
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
