calculate.T.aSPU <-
function(sim.otu.tab.rela,OTUs.in.upp.level,permY,pow = c(1:8, Inf)){
  prop = apply(sim.otu.tab.rela,2,scale)
  r= matrix(permY$r,ncol=1)
  r.s = permY$r.s
  U <- t(prop) %*% r
  U0 <- t(prop)%*%r.s
  Ts= TSPUgamma(U,colnames(sim.otu.tab.rela),OTUs.in.upp.level,pow)
  T0s= TSPUgamma(U0,colnames(sim.otu.tab.rela),OTUs.in.upp.level,pow)
  T.aspu.all = minP.SPUgamma.all (Ts,T0s)
  T.aspu = T.aspu.all$T.aspu.all
  T0.aspu= T.aspu.all$T0.aspu.all
  names(T.aspu) = colnames(T0.aspu) = names(OTUs.in.upp.level)
  TaSPU = list(T.aspu = T.aspu,T0.aspu = T0.aspu)
  TaSPU
}
