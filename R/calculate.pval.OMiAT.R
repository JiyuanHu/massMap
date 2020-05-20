calculate.pval.OMiAT <-
function(TaSPU,QOMiRKAT){
	T.OMiAT.obs = minP.OMiAT(TaSPU$T.aspu,QOMiRKAT$Q.OMiRKAT)
	T0.aspu= TaSPU$T0.aspu;Q0.OMiRKAT =QOMiRKAT$Q0.OMiRKAT
	T.OMiAT.perm = matrix(NA,nrow = nrow(T0.aspu),ncol = ncol(T0.aspu))
	for (i in 1:ncol(T0.aspu)){
	  T1 = T0.aspu[,i]
	  T2 = Q0.OMiRKAT[,i]
	  T= cbind(T1,T2)
	  T.OMiAT.perm[,i] = apply(T,1,min)
	}
	pval = rowMeans(apply(T.OMiAT.perm,1,function(Tperm){Tperm<= T.OMiAT.obs}))
	pval[pval==0] = 1/nrow(T0.aspu)
	pval
}
