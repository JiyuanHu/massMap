minP.SPUgamma.all <-
function(Ts,T0s){
  T.aspu.all = NULL;
  T0.aspu.all = NULL;
  for (i in 1:length(Ts)){
    T = abs(Ts[[i]])
    T0 = abs(T0s[[i]])
    n.perm = nrow(T0)
    true.false = t(apply(T0,1,function(x)x>=T))#M*9
    pSPUgamma = colMeans(true.false)
    T.aspu =min(pSPUgamma)
    rank.mat = apply(T0,2,rank)
    pval.mat = apply(rank.mat,2,function(x){
       ((n.perm+1)-x)/n.perm  
    })
    T0.aspu = apply(pval.mat,1,min)
    T.aspu.all = c(T.aspu.all,T.aspu)
    T0.aspu.all = cbind(T0.aspu.all,T0.aspu)
  }
  res = list(T.aspu.all = T.aspu.all, T0.aspu.all = T0.aspu.all)
  return(res)
}
