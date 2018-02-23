all.BH.methods <-
function(d,mode,alpha=0.05,target.rank){
  res.target = NULL;
  padj.meth= 'BH'
  name.pval.upper = 'pval.upper';name.pval.OTU = 'pval.OTU'
  res.traditional = traditional.BH(d,alpha,'BH',name.pval.OTU)
  pval.upper = d[[name.pval.upper]]
  res.SST = selected.subset.testing (d,pval.upper,alpha1=alpha,alpha2=alpha,padj.meth,name.pval.OTU)
  res.HBH.two.stage = Hirarchical.two.stage (d,pval.upper,alpha1=alpha,alpha2=alpha,padj.meth,name.pval.OTU)
	if(target.rank=='OTU'){
     ind = 7
	}else{
	taxonomic.ranks = c('Domain','Phylum','Class','Order','Family','Genus','Species')
     ind = match(target.rank,taxonomic.ranks)
	 }
     names.target.rank =apply(d$tax.tab[,2:ind],1,function(x){
       paste(x,collapse = '|')
     })
  res.target = data.frame(lineage = names.target.rank,p.raw = res.traditional$pval,
	p.BH = res.traditional$p.adj.tradition,
	p.HBH = res.HBH.two.stage[,3],
	p.SST = res.SST[,3])
	rownames(res.target) = rownames(res.traditional) 
	res.target$status.BH = ifelse(res.target$p.BH<alpha,1,0)
	res.target$status.HBH = ifelse(res.target$p.HBH<alpha,1,0)
	res.target$status.HBH[is.na(res.target$status.HBH)] = 0
	res.target$status.SST = ifelse(res.target$p.SST<alpha,1,0)
	res.target$status.SST[is.na(res.target$status.SST)] = 0
  d$res.target = res.target
  d
}
