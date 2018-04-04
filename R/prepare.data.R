prepare.data <-
function(otu.tab,tax.tab,tree,X,Y,screening.rank = 'Family',target.rank='Species'){
  if(!is.numeric(Y)){
    stop('The response variable Y should be numeric. Please change the class if it is character or factor')
  }
  taxonomic.ranks = ranks
  ind= match(c(screening.rank,target.rank),taxonomic.ranks)
  if(sum(is.na(ind))){
    stop('The screening rank and target rank should be either Kingdom,Phylum,Class,Order,Family,Genus, Species or OTU!') 
  }else if(ind[1]>= ind[2]){
    stop('Screening rank must be higher than the target rank!') 
  }
  tax.tab = preprocess4tax(tax.tab)
  if(target.rank != 'OTU'){
	  phy = merge_phyloseq(otu_table(otu.tab),tax_table(tax.tab),tree)
	  phy = tax_glom(phy, taxrank=target.rank)
	  phy= filter_taxa(phy,function(x) sum(x > 0) > 0, TRUE)
	  otu.tab = otu_table(phy)
	  tax.tab = tax_table(phy)
	  tree = phy_tree(phy)
  }
  sim.otu.tab = otu.tab
  sample.names = rownames(sim.otu.tab)
  if(length(Y) != nrow(otu.tab)){
	sim.otu.tab = t(otu.tab)
	sample.names = rownames(sim.otu.tab)
  }
  sim.otu.tab.rela= relative.abundance(sim.otu.tab)
  if(is.null(X)){
    cov = NULL
  }else{
    cov.after.transformation = NULL
    for (j in 1:ncol(X)){
      x = X[[j]]
      if(is.character(x)){
        stop(paste0(j,'\'thCovariate is character variable! Covariate(s) X should be numeric or factor variables. Please change it'))
      }
      if(is.factor(x)){
        print('factor found')
        x = model.matrix(~x)[,-c(1)]
        print(head(x))
      }
      cov.after.transformation =cbind(cov.after.transformation,x)
    }
    
    cov = data.frame(cov.after.transformation)
    colnames(cov)= paste0('var',1:ncol(cov))
    rownames(cov)= sample.names
  }

  d= list(sim.otu.tab = sim.otu.tab,
          sim.otu.tab.rela= sim.otu.tab.rela,
          tree = tree,
          tax.tab = tax.tab,
		  Y = Y,
		  cov = cov)
  return (d)
}
