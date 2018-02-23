minP.MiRKAT <-
function(Qtest,Q0){
  if(is.vector(Q0)){
    res = mean(Q0>= Qtest)
  }else{
    true.false = t(apply(Q0,1,function(x)x>=Qtest))
    pMiRKATk = colMeans(true.false)
    res =min(pMiRKATk)
  }
  res
}
