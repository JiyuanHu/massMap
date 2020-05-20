calculate.T.OMiRKAT <-
function(sim.otu.tab,permY,OTUs.in.upp.level,tree,model,g.unif.alpha=c(0, 0.25, 0.50, 0.75)){
	r = matrix(permY$r,ncol=1);
	r.s = permY$r.s
	Qs = Q0s =list()
	Q.OMiRKAT= NULL;
	Q0.OMiRKAT= matrix(NA,nrow = ncol(r.s),ncol = length(OTUs.in.upp.level))
	for (i in 1:length(OTUs.in.upp.level)){
		cluster1 = OTUs.in.upp.level[[i]]
		x = sim.otu.tab[,match(cluster1,colnames(sim.otu.tab))]
		tree.surv = prune_taxa(cluster1, tree)
		distance.measures = distance.cal (x,tree.surv,g.unif.alpha)
		Qs[[i]] = QMiRKATk.cal(distance.measures,r,model)
		Q0s[[i]] = QMiRKATk.cal(distance.measures,r.s,model) 
		Q.OMiRKAT[i] = minP.MiRKAT(Qs[[i]],Q0s[[i]])
		Q0.OMiRKAT[,i]=minP.MiRKAT.perm(Q0s[[i]])
	}
	names(Q.OMiRKAT) = colnames(Q0.OMiRKAT) = names(OTUs.in.upp.level)
	QOMiRKAT = list(Q.OMiRKAT = Q.OMiRKAT,Q0.OMiRKAT = Q0.OMiRKAT)
	QOMiRKAT
}
