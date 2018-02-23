QMiRKATk.cal <-
function(distance.measures,r,model){
  u.unif = distance.measures[['u.unif']]
  if(sum(is.na(u.unif))){
    kernels = D2K(distance.measures[['bray.curtis']]) 
  }else{
    kernels = lapply(distance.measures,D2K)
  }
  if(is.matrix(kernels) & ncol(r)==1){
    Q = QMiRKAT(r,kernels,model)
  }else if(is.matrix(kernels) & ncol(r)!=1){
    Q = QMiRKAT.perm(r,kernels,model)
  }else if(!is.matrix(kernels) & ncol(r)==1){
    Q = sapply(kernels,QMiRKAT,r=r,model = model)
  }else{
    Q = matrix(NA,nrow = ncol(r),ncol = length(kernels))
    for (i in 1:length(kernels)){
      ker = kernels[[i]]
      Qtmp = QMiRKAT.perm(r,ker,model)
      Q[,i] = Qtmp
    }
  }
  Q
}
