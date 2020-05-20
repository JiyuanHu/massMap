massMap.glm <-
function(X=NULL,Y,otu.tab,is.count.otu.tab,tax.tab,tree,outcome.trait,
	screening.rank,target.rank,alpha,n.perm){
#  mode='OMiAT'
#  set.seed(1234)
#  outcome.trait = match.arg(outcome.trait,outcome.traits)
 # screening.rank= match.arg(screening.rank,ranks)
  #target.rank= match.arg(target.rank,ranks)
  d = prepare.data(otu.tab,is.count.otu.tab,tax.tab,tree,X,Y,screening.rank,target.rank)
  d = simulate.clusters.info (d,screening.rank)
  d = residual.perm(d,outcome.trait,n.perm)
  d = OTU.level.anal.func(d) 
  mode = ifelse(is.null(tree) | is.count.otu.tab==FALSE,'aSPU','OMiAT')
  d = cluster.statistical.test.func(d,outcome.trait,mode) 
  d = all.BH.methods(d,alpha,target.rank)
  d = organize.res.screening(d)
  return(d)
}
