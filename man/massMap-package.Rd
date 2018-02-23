\name{massMap-package}
\alias{massMap-package}
\docType{package}
\title{
\packageTitle{massMap}
}
\description{
This package is developed to powerfully discover trait-associated taxa at the taget rank, such as Genus or species. A two-stage microbial association mapping framework (massMap) is implemented which uses grouping information from the taxonomic tree to strengthen statistical power in association tests at the target rank. MassMap first screens the association of taxonomic groups at a pre-selected higher taxonomic rank using a powerful microbial group test OMiAT. Then it proceeds to test the association for each candidate taxon at the target rank within the significant taxonomic groups identified in the first stage. Hierarchical BH (HBH) and selected subset testing (SST) procedures are evaluated to control the FDR for the two-stage structured tests. Both simulation studies and real data applications demonstrated marked statistical power improvement of massMap over competing methods.
}
\details{
\tabular{ll}{
Package: \tab massMap\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2018-02-22\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
Jiyuan Hu

Maintainer: Jiyuan Hu <jiyuan_hu@fudan.edu.cn>
}
\references{
HU J, Koh H, He L, Liu M, Blaser J M, Li H(2018). A two-stage microbial association mapping framework with advanced FDR control.
}
\keyword{ package}
\examples{
data(X)#covariates of age and gender
data(ABH)#the antibiotic history of 100 subjects
data(otu.tab)# the otu table of microbiome data for 100 subjects and 90 species.
data(tax.tab)#the taxonomy alignment of 90 species
data(tree) #the phylogenetic tree of the microbiome data
############
# Not run:
#res = massMap(X=X, Y=ABH,otu.tab=otu.tab,tax.tab=tax.tab,tree=tree,outcome.trait="binary", 
#  screening.rank = "Family", target.rank="Species",alpha=0.2,n.perm=1e4)
}

