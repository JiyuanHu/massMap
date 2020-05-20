OTU.level.anal.func <-
function(d){ 
  sim.otu.tab.rela = d$sim.otu.tab.rela
  permY = d$permY
  sig.otu.ids = d$sig.otu.ids
  T.OTU = T.OTU.cal(sim.otu.tab.rela,permY)
  pval.OTU =pval.OTU.cal(T.OTU,sig.otu.ids)
  d$pval.OTU = pval.OTU
  d$U = T.OTU[['U']]
  return(d)
}
