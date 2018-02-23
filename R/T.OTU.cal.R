T.OTU.cal <-
function(sim.otu.tab.rela,permY){
##OTU level analysis.(stage 2) score statistics
  aggr.stand.prop = apply(sim.otu.tab.rela,2,scale)
  r.s = permY$r.s;r = permY$r

  U = t(aggr.stand.prop)%*%r
  U0 = t(aggr.stand.prop)%*%r.s
  rownames(U) = rownames(U0)= colnames(sim.otu.tab.rela)
  T = list(U0= U0,U=U)
  T
}
