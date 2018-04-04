cluster.statistical.test.func.Surv <-
function(d,screening.rank,n.perm){
  two.stage.cluster.info =d$two.stage.cluster.info
  sim.otu.tab = d$sim.otu.tab
  tree = d$tree
  total.reads = rowSums(sim.otu.tab)
  cov = as.matrix(d$cov)
  cluster.size = lapply(two.stage.cluster.info,function(x)sapply(x,length))
  pval = NULL
  for (j in 1:length(cluster.size[[1]])){
  print(j)
    clus= cluster.size[[1]][j]
    if(clus==1){
	  taxon = two.stage.cluster.info[[1]][[j]]
      otu.tab.sub = sim.otu.tab[,taxa]	  
	  fit = summary(coxph(Surv(d$Y,d$delta)~cov+otu.tab.sub))$coefficients
	  pval[j] = fit[nrow(fit),5]      
    }else{
	  taxa = two.stage.cluster.info[[1]][[j]]
      otu.tab.sub = sim.otu.tab[,taxa]
	  tree.sub = prune_taxa(taxa,tree)
	  pval[j]=OMiSA(obstime=d$Y,delta = d$delta,X = otu.tab.sub,total.reads = total.reads,
	  tree = tree.sub,cov = cov,pow = c(1/4,1/3,1/2,1),g.unif.alpha= c(0.5),
	  n.perm=n.perm)$p.omisa
	}
  }
  names(pval) = names(two.stage.cluster.info[[1]])
  pval.upper = list(pval = pval)
  names(pval.upper) = screening.rank
  d$pval.upper = pval.upper
  return(d)
}
