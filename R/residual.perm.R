residual.perm <-
function(d,outcome.trait,n.perm){
  Y = d$Y
  cov = d$cov
  model = ifelse(outcome.trait=='continuous','gaussian','binomial')

  #Y = y1;cov=as.matrix(covariates);n.perm =n.perm.OTU
  if (is.null(cov)){
    r <- Y - mean(Y)
  } else {
    fit <- glm(Y ~ as.matrix(cov), family = model)
    res <- Y - fitted.values(fit)
    r <- res - mean(res)
  }
  rank = unlist(lapply(1:n.perm,function(x)sample(length(r))))
  r.s = r[rank]
  r.s = matrix(r.s,ncol = n.perm)
  permY = list(r.s = r.s,r = matrix(r,ncol=1))#r.s: n*M;r:n*1
  d$permY = permY
  return(d)
}
