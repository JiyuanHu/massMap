selected.subset.testing <-
function(d,pval.upper,alpha1=0.05,alpha2=0.05,padj.meth,name.pval.OTU){
  two.stage.cluster.info = d$two.stage.cluster.info
  pval.OTU = d[[name.pval.OTU]]
  for(i in 1:length(pval.upper)){
    up.level = names(pval.upper)[i]
    pval.up = pval.upper[[i]]
    padj.up = p.adjust(pval.up,padj.meth)
    clusters.left = names(pval.up[padj.up<alpha1]) #could be null
    two.stage = two.stage.cluster.info[[i]]
    p.adj.lower = rep(NA,nrow(pval.OTU))
    pred.lower = rep(0,nrow(pval.OTU))
    tmp= data.frame(p.adj.lower,pred.lower)
    if(length(clusters.left)){
      ind = match(clusters.left,names(two.stage))
      OTUs.further.consideration = unlist(two.stage[ind])
      ind = match(OTUs.further.consideration,rownames(pval.OTU))
      pval.left = pval.OTU[ind,1]
      p.adj = p.adjust(pval.left,padj.meth)
      pred = rep(0,length(p.adj))
      pred[p.adj<alpha2]=1
      tmp$p.adj.lower[ind]= p.adj
      tmp$pred.lower[ind]= pred
    }
    colnames(tmp)= paste(c('p.adj','pred'),up.level,sep = '.')
    pval.OTU = data.frame(pval.OTU,tmp)
  }
  return(pval.OTU)
}
