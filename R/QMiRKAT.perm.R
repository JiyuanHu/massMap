QMiRKAT.perm <-
function(r,ker,model){
  n = nrow(ker)
  Q = NULL;
  if(model =='gaussian'){
    vars = apply(r,2,var)
    Q = matrix(1,1,n)%*% (r * (ker %*% r))
    Q = 1/vars* 1/2*Q[1,]
  }else{
	Q = matrix(1,1,n)%*% (r * (ker %*% r))
    Q = Q[1,]/2
  }
  Q	
}
