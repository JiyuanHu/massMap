pval.OTU.cal <-
function(T.OTU,sig.otu.ids){
	T.perm = abs(T.OTU[['U0']])
	T.obs = abs(T.OTU[['U']])
	pval = rowMeans(apply(T.perm,2,function(t.perm){
		t.perm>= T.obs
	}))
	pval[pval==0]= 1/ncol(T.perm)
	names(pval) = rownames(T.obs)
	true.status = rep(NA,length(pval))
	ind = match(sig.otu.ids,rownames(T.obs))
	true.status[ind]= 1
	res = data.frame(pval=pval,true.status=true.status)
	res
}
