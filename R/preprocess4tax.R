preprocess4tax <-
function(tax.tab){
	ind = which(as.character(tax.tab[,1]) !='Unassigned')
	tax.tab = tax.tab[ind,]
	names = paste0(c('p','c','o','f','g','s'),'__')
	for (j in 2:ncol(tax.tab)){
	  ind = which(is.na(tax.tab[,j]))
	  if(length(ind)){
	  tax.tab[ind,j] = names[j-1]
	  }
	}
	colnames(tax.tab) = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
	return(tax.tab)
}
