relative.abundance <-
function(sim.otu.tab){
  rowSum = rowSums(sim.otu.tab)#count for each individual
  rela = matrix(NA,nrow = nrow(sim.otu.tab),ncol = ncol(sim.otu.tab))
  for (j in 1:ncol(sim.otu.tab)){
    rela[,j] = sim.otu.tab[,j]/rowSum
  }
  rownames(rela) = rownames(sim.otu.tab)
  colnames(rela) = colnames(sim.otu.tab)
  rela
}
