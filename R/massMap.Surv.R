massMap.Surv <-
function(X=NULL,obstime,delta,otu.tab,tax.tab,tree,
	screening.rank= 'Family',target.rank= ranks,alpha=0.05,n.perm=1e4){
  #set.seed(1234)
  #mode='OMiAT'
  #outcome.trait = match.arg(outcome.trait,outcome.traits)
  screening.rank= match.arg(screening.rank,ranks)
  target.rank= match.arg(target.rank,ranks)
  d = prepare.data(otu.tab,tax.tab,tree,X,Y=obstime,screening.rank,target.rank)
  d$delta = delta
  d = simulate.clusters.info(d,screening.rank)
  ##OMiSA
  d = OTU.level.anal.func.Surv(d)
  d = cluster.statistical.test.func.Surv(d,screening.rank,n.perm)
  ###
  d = all.BH.methods(d,alpha,target.rank)
  d = organize.res.screening(d)
  return(d)
}
