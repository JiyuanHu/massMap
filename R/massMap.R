massMap <-
function(X=NULL,Y,otu.tab,tax.tab,tree,outcome.trait=outcome.traits,
	screening.rank= 'Family',target.rank= ranks,alpha=0.05,n.perm=1e4){
  mode='OMiAT'
  outcome.trait = match.arg(outcome.trait,outcome.traits)
  screening.rank= match.arg(screening.rank,ranks)
  target.rank= match.arg(target.rank,ranks)
  d = prepare.data(otu.tab,tax.tab,tree,X,Y,screening.rank,target.rank)
  d = simulate.clusters.info (d,screening.rank)
  d = residual.perm(d,outcome.trait,n.perm)
  d = OTU.level.anal.func(d) 
  d = cluster.statistical.test.func(d,outcome.trait,mode) 
  d = all.BH.methods(d,mode,alpha,target.rank)
  d = organize.res.screening(d)
  return(d)
}
