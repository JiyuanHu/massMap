OTU.level.anal.func4clusters <-
function(d,OTU.in.sigleton.cluster){
  sim.otu.tab.rela = d$sim.otu.tab.rela
  ind = match(OTU.in.sigleton.cluster,colnames(sim.otu.tab.rela))
  sim.otu.tab.rela = sim.otu.tab.rela[,ind]
  permY = d$permY
  sig.otu.ids = d$sig.otu.ids
  T.OTU = T.OTU.cal(sim.otu.tab.rela,permY)
  pval.OTU =pval.OTU.cal(T.OTU,sig.otu.ids)
  pval.OTU = data.frame(OTU =names(OTU.in.sigleton.cluster), pval = pval.OTU[,1])
  return(pval.OTU)
}
