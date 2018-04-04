\name{massMap-package}
\alias{massMap-package}
\docType{package}
\title{
Massmap: a two-stage microbial association mapping framework with advanced FDR control
}
\description{
This package is developed to powerfully discover trait-associated taxa at the taget rank, such as Genus or species. A two-stage microbial association mapping framework (massMap) is implemented which uses grouping information from the taxonomic tree to strengthen statistical power in association tests at the target rank. MassMap can apply to binary, continuous and survival traits. MassMap first screens the association of taxonomic groups at a pre-selected higher taxonomic rank using a powerful microbial group test OMiAT for the binary/continuous trait or optimal microbiome-based survival analysis tool OMiSA for the survival trait respectively. Then it proceeds to test the association for each candidate taxon at the target rank within the significant taxonomic groups identified in the first stage. Hierarchical BH (HBH) and selected subset testing (SST) procedures are evaluated to control the FDR for the two-stage structured tests. Both simulation studies and real data applications demonstrated marked statistical power improvement of massMap over competing methods.
}
\details{
\tabular{ll}{
Package: \tab massMap\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2018-04-04\cr
License: \tab GPL (>= 2)\cr
}
}

\author{
Jiyuan Hu, Hyunwook Koh, Linchen He, Menghan Liu, Martin J. Blaser, Huilin Li.

Maintainer: Jiyuan Hu <Jiyuan.Hu@nyumc.org>
}
\references{
HU J, Koh H, He L, Liu M, Blaser M J, Li H (2018). A two-stage microbial association mapping framework with advanced FDR control. (submitted)

Koh H, Livanos A E, Blaser M J, Li H (2018). A highly adaptive microbiome based association test for survival traits. BMC genomics, 19(1), 210.

Koh H, Blaser M J and Li H (2017). A powerful microbiome-based association test and a microbial taxa discovery framework for comprehensive association mapping. Microbiome, 5(1), p.45.

}
\keyword{package}
\examples{
require(massMap)
###Illustration 1: binary trait
##MassMap for the continuous trait is the same as the binary trait.
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
#              screening.rank = "Family", target.rank="Species",alpha=0.05,n.perm=1e2)
#res$res.screening ##The group association test at the screening rank
#res$res.target ##The microbial association test at the target rank
###

###Illustration 2: survival trait
##It requires the observed survival time (obstime) and the event indicator (delta) when running massMap for the surval trait
data("MiSurv.Data",package= 'OMiSA')
otu.tab <- otu_table(MiSurv.Data)
tax.tab <- tax_table(MiSurv.Data)
tree <- phy_tree(MiSurv.Data)
map = sample_data(MiSurv.Data)
obstime <- as.numeric(unlist(map[,1]))
delta <- as.numeric(unlist(map[,2]))
X = data.frame(map[,3:4])
############
# Not run:
#res.Surv = massMap(X=X, obstime = obstime,delta= delta,otu.tab=otu.tab,tax.tab=tax.tab,
#                   tree=tree,outcome.trait="survival",screening.rank = "Family",
#                   target.rank="Species",alpha=0.05,n.perm=1e4)
#res$res.screening ##The group association test at the screening rank
#res$res.target ##The microbial association test at the target rank

}

