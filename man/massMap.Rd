\name{massMap}
\alias{massMap}
\title{
The main function of the two-stage microbial association mapping framework.
}
\description{
This function detects the trait-associated taxa at the specified taget rank. We recommend Family to be the screening rank. The results of Hierarchical Benjamini-Hochberge (HBH), selected subset testing with BH procedures (SST) and the traditional one-stage BH procedure are reported.
}
\usage{
##binary or continuous trait
massMap(X, Y, otu.tab, is.count.otu.tab = TRUE, tax.tab, tree, 
	outcome.trait = outcome.traits, screening.rank = "Family", target.rank = ranks, 
	alpha = 0.05, n.perm = 1e4)
##survival trait
massMap(X, obstime, delta, otu.tab, is.count.otu.tab = TRUE, tax.tab, tree,
   outcome.trait = outcome.traits, screening.rank= "Family", target.rank= ranks,
   alpha=0.05,n.perm=1e4)

outcome.traits
#c("binary", "continuous", "survival")
ranks
#c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU")
}
\arguments{
  \item{X}{
A data frame which contains covariates to be adjusted in the regression model. The elements of the data frame must be numeric or factor. Set X = NULL if there is no covariate.
}
  \item{Y}{
A numeric vector of the binary/continuous outcome trait with length = sample size.
}
  \item{obstime}{
A numeric vector of the survival time for the survival outcome trait with length = sample size.
}
   \item{delta}{
A numeric vector of the status indicator for the survival outcome trait with length = sample size.
}
  \item{otu.tab}{
The taxonomic table of the microbiome data. Each row represents the taxonomy alignment of each taxon on Kingdom (Domain), Phylum, Class, Order, Family, Genus and Species respectively. Best if it is a class otu_table from package "phyloseq".
}
  \item{is.count.otu.tab}{
  An indicator of whether the OTU table contains the count data or relative abundance data for OTUs. 
}
  \item{tax.tab}{
The OTU table of the microbiome data. Each row represents a subject and each column represents the OTU. Best if it is a class taxonomyTable from package "phyloseq".
}
  \item{tree}{
The phylogenetic tree of the microbiome data. Best if it is a class phylogenetic tree from package "phyloseq".
}
  \item{outcome.trait}{
Specify the type of outcome trait. Must be either "binary", "continuous"  or "survival".
}
  \item{screening.rank}{
Specify the screening rank. The recommend and default setting is "Family".
}
  \item{target.rank}{
Specify the target rank.The default setting is "Species".
}
  \item{alpha}{
Significance level for the adjusted p-values. The default setting is 0.05.
}
  \item{n.perm}{
Numver of permutations in order to calculate p-values. The default setting 1e4 is large enough to obtain accurate p-values.
}
}
\value{
  \item{res.screening}{A data frame which contains the group association test results at the screening rank. There are four elements:
  \describe{
  \item{lineage}{The lineage of each taxonomic group;}
  \item{size}{The number of taxa within the lineage;}
  \item{pval.raw}{The raw p-value of OMiAT test;}
  \item{pval.adj}{The adjusted p-value of OMiAT test.}
  }
  }
  \item{res.target}{
  A data frame which contains the association test results at the target rank. There are eight elements:
  \describe{
	\item{lineage}{The lineage of each taxon at the target rank;}
	\item{p.raw}{The raw p-value of the non-parametric score association test;}
	\item{p.BH}{The BH adjusted p-values;}
	\item{p.HBH}{Adjusted p-values using Hierarchical BH procedure;}
	\item{p.SST}{Adjusted p-values using selected subset testing procedure;}
	\item{status.BH}{The association status of each taxon after adjustment of p-values using BH procedure. status.BH=1 if the taxon is significantly associated with the outcome trait. status.BH=0 otherwise;}
	\item{status.HBH}{The association status of each taxon after adjustment of p-values using two-stage HBH procedure;}
	\item{status.SST}{The association status of each taxon after adjustment of p-values using two-stage SST procedure.}

	The p-value of HBH and SST is NA if the corresponding group test at the screening rank is insignificant.
	}
}
}
\references{
Hu, Jiyuan, Hyunwook Koh, Linchen He, Menghan Liu, Martin J. Blaser, and Huilin Li. "A two-stage microbial association mapping framework with advanced FDR control." Microbiome 6, no. 1 (2018): 131.

}
\author{
Jiyuan Hu, Hyunwook Koh, Linchen He, Menghan Liu, Martin J. Blaser, Huilin Li.
}

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
#res = massMap(X=X, Y=Y,otu.tab=otu.tab,is.count.otu.tab=TRUE, 
#  tax.tab=tax.tab,tree=tree,outcome.trait="binary",
#  screening.rank = "Family", target.rank="Species",alpha=0.05,n.perm=1e2)
#res$res.screening ##The group association test at the screening rank
#res$res.target ##The microbial association test at the target rank
###

###Illustration 2: survival trait
##The observed survival time (obstime) and the event indicator (delta) are required 
##when running massMap for the surval trait
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
#res.Surv = massMap(X=X, obstime = obstime,delta= delta,otu.tab=otu.tab, is.count.otu.tab=TRUE, 
#	tax.tab=tax.tab, tree=tree,outcome.trait="survival",
#   screening.rank = "Family",target.rank="Species",alpha=0.05,n.perm=1e4)
                   
#res$res.screening ##The group association test at the screening rank
#res$res.target ##The microbial association test at the target rank

}
\keyword{ two-stage}

