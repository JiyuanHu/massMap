simulate.clusters.info <-
function(d,screening.rank='Family'){
  #1. two-stage info; 2. neighboring-stage info
#  screening.ranks = c('Phylum','Class','Order','Family','Genus')
  tax.tab = d$tax.tab;
  info.all= list();i=1
  for(up in screening.rank){
    info.all[i:(i+1)]=get.two.stage.info(tax.tab,upper.level=up)
    i = i+2
  }
  name.info = info.all[seq(1,2*length(screening.rank)-1,by=2)]
  two.stage.cluster.info = info.all[seq(2,2*length(screening.rank),by=2)]
  names(name.info)= names(two.stage.cluster.info)=screening.rank
  d$name.info=name.info
  d$two.stage.cluster.info=two.stage.cluster.info
  return(d)
}
