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
Jiyuan Hu, Hyunwook Koh, Linchen He, Menghan Liu, Martin J. Blaser, Huilin Li.

Maintainer: Jiyuan Hu <Jiyuan.Hu@nyumc.org>
}
\references{
HU J, Koh H, He L, Liu M, Blaser J M, Li H(2018). A two-stage microbial association mapping framework with advanced FDR control.
}
\keyword{package}
\examples{
require(massMap)
data(phy)
map = as.data.frame(sample_data(phy))
X = map[,c('age','gender')] # a data frame, each element could either numeric or factor.
Y = map[['ABH']]#a vector instead of a data frame.
otu.tab = otu_table(phy)
tax.tab = tax_table(phy)
tree = phy_tree(phy)
############
# Not run:
#res = massMap(X=X, Y=Y,otu.tab=otu.tab,tax.tab=tax.tab,tree=tree,outcome.trait="binary",
#              screening.rank = "Family", target.rank="Species",alpha=0.05,n.perm=1e4)
}

