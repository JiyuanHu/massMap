massMap <-
function(X=NULL,Y=NULL,obstime = NULL,delta= NULL,otu.tab,tax.tab,tree,outcome.trait=outcome.traits,
	screening.rank= 'Family',target.rank= ranks,alpha=0.05,n.perm=1e4){
	set.seed(1234)
  outcome.trait = match.arg(outcome.trait,outcome.traits)
  screening.rank= match.arg(screening.rank,ranks)
  target.rank= match.arg(target.rank,ranks)
  if(outcome.trait =='survival'){
	d = massMap.Surv(X,obstime,delta,otu.tab,tax.tab,tree,
	screening.rank,target.rank,alpha=alpha,n.perm)
  }else{
	mode='OMiAT'
	d = massMap.glm (X,Y,otu.tab,tax.tab,tree,outcome.trait,
	screening.rank,target.rank,alpha=alpha,n.perm)
  }
  return(d)
}
