TSPUgamma <-
function(U,OTUname,OTUs.in.upp.level,pow){
  T = list()
  for (i in 1:length(OTUs.in.upp.level)){
    OTUsub = OTUs.in.upp.level[[i]]
    ind = match(OTUsub,OTUname)
    Usub = U[ind,,drop =FALSE]
    Tpart1 = sapply(pow[pow!=Inf],function(x) {colSums(Usub^x)})
    Tmax = apply(Usub,2,max)
    if(length(Tmax)==1){
      T[[i]] = c(Tpart1,Tmax)
    }else{
      T[[i]] = cbind(Tpart1,Tmax)
    }
    }
  T
}
