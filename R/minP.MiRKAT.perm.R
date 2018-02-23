minP.MiRKAT.perm <-
function(Q0){
  if(is.vector(Q0)){
    n.perm = length(Q0)
    rank = rank(Q0)
    T0.aspu = ((n.perm+1)-rank)/n.perm  
  }else{
  n.perm = nrow(Q0)
  rank.mat = apply(Q0,2,rank)
  pval.mat = apply(rank.mat,2,function(x){
    ((n.perm+1)-x)/n.perm  
  })
  T0.aspu = apply(pval.mat,1,min)
  }
  T0.aspu
}
