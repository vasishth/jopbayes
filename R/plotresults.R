plotresults<-function(dat,ylabel="log ms",maintitle="Posterior distributions vs original values"){
  pd <- position_dodge(1)
  resultsplot<-ggplot(dat, aes(x=parameter, 
                               y=mean))+
    geom_errorbar(aes(ymin=lower, ymax=upper),width=0.1,position=pd)+
    ggtitle(maintitle)+
    xlab("Parameter")+
    ylab(ylabel)+
    geom_hline(yintercept=0)+
    geom_point(size=6,position=pd)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 60, hjust = 1))+
    magnifytext()
  resultsplot
}
