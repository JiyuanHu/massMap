reorganize.pval <-
function(cluster.size,pval.OMiAT,pval.OTU){
  pval.list = cluster.size
  for (i in 1:length(cluster.size)){
    ind1= match(names(pval.OMiAT),names(cluster.size[[i]]))
    if(sum(!is.na(ind1))){
      pval.match = pval.OMiAT[!is.na(ind1)]
      ind1 = ind1[!is.na(ind1)]
      pval.list[[i]][ind1]= pval.match
    }
    ind2= match(pval.OTU[,1],names(cluster.size[[i]]))
    if(sum(!is.na(ind2))){
      pval.match = pval.OTU[!is.na(ind2),2]
      ind2 = ind2[!is.na(ind2)]
      pval.list[[i]][ind2]= pval.match
    }
  }
  pval.list
}
