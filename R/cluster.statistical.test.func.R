cluster.statistical.test.func <-
function(d,outcome.trait,mode ='OMiAT',rarefy=1){
  model = ifelse(outcome.trait=='continuous','gaussian','binomial')
  two.stage.cluster.info =d$two.stage.cluster.info
  permY = d$permY
  r = permY$r;r.s = permY$r.s
  sim.otu.tab.rela = d$sim.otu.tab.rela
  sim.otu.tab = d$sim.otu.tab
  tree = d$tree
  cluster.size = lapply(two.stage.cluster.info,function(x)sapply(x,length))
  OTU.in.sigleton.cluster = NULL
  taxa.in.multisize.cluster = list()
  inds = list()
  for (i in 1:length(cluster.size)){
    clus= cluster.size[[i]]
    ind = which(clus==1)
    inds[[i]]= ind
    if(length(ind)){
      OTU.in.sigleton.cluster = c(OTU.in.sigleton.cluster,unlist(two.stage.cluster.info[[i]][ind]))
      taxa.in.multisize.cluster = c(taxa.in.multisize.cluster,two.stage.cluster.info[[i]][-c(ind)])
    }else{
      taxa.in.multisize.cluster = c(taxa.in.multisize.cluster,two.stage.cluster.info[[i]])
	}
  }
  names(inds)=names(cluster.size)
  OTUs.in.upp.level =taxa.in.multisize.cluster
  ti = Sys.time()

  if(mode =='aSPU' | mode == 'OMiAT'){
    print('We are running aMiSPU part')
    TaSPU = calculate.T.aSPU (sim.otu.tab.rela,OTUs.in.upp.level,permY)
    ti1 = Sys.time()
    print('aMiSPU part end, time spent:')
	print(round(ti1-ti,2))
  }
  if(mode =='MiRKAT' | mode == 'OMiAT'){
    print('We are running Opt.MiRKAT part. Be patient because this will take a while.')
    if(rarefy){sim.otu.tab = Rarefy(sim.otu.tab)$otu.tab.rff}
    QOMiRKAT = calculate.T.OMiRKAT(sim.otu.tab,permY,OTUs.in.upp.level,tree,model)
    ti2 = Sys.time()
    print('Opt.MiRKAT part end, time spent:')
    print(round(ti2-ti,2))
  }  
  if(mode=='aSPU'){
    pval.aSPU = calculate.pval.others(TaSPU) 
  }
  if(mode=='MiRKAT'){
    pval.MiRKAT = calculate.pval.others(QOMiRKAT) 
  }

  if(mode =='OMiAT'){
    pval.aSPU = calculate.pval.others(TaSPU)
    pval.MiRKAT = calculate.pval.others(QOMiRKAT) #wrong
    pval.OMiAT = calculate.pval.OMiAT(TaSPU,QOMiRKAT)
  }
  pval.OTU = OTU.level.anal.func4clusters(d,OTU.in.sigleton.cluster)
  if(mode=='aSPU'){
    d$pval.upper = reorganize.pval (cluster.size,pval.aSPU,pval.OTU)
  }
  if(mode=='MiRKAT'){
    d$pval.upper = reorganize.pval (cluster.size,pval.MiRKAT,pval.OTU)
  }
  if(mode =='OMiAT'){
  d$pval.upper = reorganize.pval (cluster.size,pval.OMiAT,pval.OTU)
  }
  return(d)
}
