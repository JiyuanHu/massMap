organize.res.screening <-
function(d){
	pval.raw = d$pval.upper[[1]]
	pval.adj = p.adjust(pval.raw,'BH')
	pval.raw = data.frame(lineage = names(pval.raw),pval.raw = pval.raw,pval.adj = pval.adj)
	two.stage.info = d$two.stage.cluster.info[[1]]
	size =sapply(two.stage.info,length)
	size = data.frame(lineage = names(size),size=size)
	res.screening = merge(x = size,y=pval.raw,by.x = 'lineage',by.y = 'lineage')
	d$res.screening = res.screening
	d = d[c('res.screening','res.target')]
	gc()
	return (d)
}
