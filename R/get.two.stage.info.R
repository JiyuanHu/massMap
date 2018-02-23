get.two.stage.info <-
function(tax.tab,upper.level){
  #upper.level = 'Genus'
  full.name = as.character(tax.tab[,2])
  level.name = NULL;
  #initiated at Phylum
  OTUs = rownames(tax.tab)
  if(upper.level!='Phylum'){
    ind = match(upper.level,colnames(tax.tab))
    for (j in 3:ind){
      full.name = paste(full.name,tax.tab[,j],sep = '|')
    }
    level.name = tax.tab[,ind]
  }else{#upper.level ='Phylum'
    level.name = full.name
  }
  name.info = data.frame(full.name,level.name)
  names(name.info)= c('full.name','level.name')
  rownames(name.info)= OTUs
  full.name.uniq = unique(full.name)
  two.stage.cluster.info = lapply(full.name.uniq,function(x){
    ind = which(full.name==x)
    OTUs[ind]
  })
  names(two.stage.cluster.info) = full.name.uniq
  info = list(name.info = name.info,two.stage.cluster.info=two.stage.cluster.info)
  info
}
