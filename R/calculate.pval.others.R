calculate.pval.others <-
function(TaSPU){
  Tobs = TaSPU[[1]]
  Tperms = TaSPU[[2]]
  pval = NULL
  for (i in 1:length(Tobs)){
    Tob = Tobs[i]
    Tperm = Tperms[,i]
    pval[i]=mean(Tperm<= Tob)
  }
  names(pval) = names(Tobs)
  pval
}
