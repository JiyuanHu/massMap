distance.cal <-
function(x,tree,g.unif.alpha){
	unifs <- GUniFrac(x, tree, alpha = c(g.unif.alpha, 1))$unifracs
	bray.curtis <- as.matrix(bcdist(x))
	dist.measures = list()
	L1 = dim(unifs)[3]-1
	for (i in 1:L1){
		dist.measures[[i]]= unifs[,,i]
	}
	dist.measures[[i+1]] = bray.curtis
	names(dist.measures) = c(paste0('d_',g.unif.alpha),'w.unif','u.unif','bray.curtis')
	dist.measures
}
