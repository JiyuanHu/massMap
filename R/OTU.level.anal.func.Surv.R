OTU.level.anal.func.Surv <-
function(d){ 
  sim.otu.tab = d$sim.otu.tab
  ####
  total.reads = rowSums(sim.otu.tab)
  cov = as.matrix(d$cov)
  pval = NULL
  for (j in 1:ncol(sim.otu.tab)){
  #j=1
	  otu.tab.sub = scale(sim.otu.tab[,j]/total.reads)
	  taxon = colnames(sim.otu.tab)[j]
	  fit = summary(coxph(Surv(d$Y,d$delta)~cov+otu.tab.sub))$coefficients
	  pval[j] = fit[nrow(fit),5]
  }
  pval.OTU =data.frame(pval = pval,true.status = rep(NA,length(pval)))
  rownames(pval.OTU)= colnames(sim.otu.tab)
  d$pval.OTU = pval.OTU
  ###
  return(d)
}
