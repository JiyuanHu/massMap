traditional.BH <-
function(d,alpha= 0.05,padj.meth,name.pval.OTU){
  pval.OTU = d[[name.pval.OTU]]
  p.adj = p.adjust(pval.OTU[,1],padj.meth)
  pred = rep(0,length(p.adj))
  pred[p.adj<alpha]=1
  res = data.frame(pval.OTU,p.adj.tradition = p.adj,pred.tradition = pred)
  res
}
