\name{massMap}
\alias{massMap}
\title{
The main function of the two-stage microbial association mapping framework.
}
\description{
This function detects the trait-associated taxa at the specified taget rank. We recommend Family to be the screening rank. The results of Hierarchical Benjamini-Hochberge (HBH), selected subset testing with BH procedures (SST) and the traditional one-stage BH procedure are reported.
}
\usage{
massMap(X, Y, otu.tab, tax.tab, tree, outcome.trait = outcome.traits,
	screening.rank = ranks, target.rank = ranks,
	alpha = 0.05, n.perm = 10000)
outcome.traits
#c("binary", "continuous")
ranks
#c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species", "OTU")
}
\arguments{
  \item{X}{
A data frame which contains covariates to be adjusted in the regression model. The elements of the data frame must be numeric or factor. Set X = NULL if there is no covariate.
}
  \item{Y}{
A numeric vector of the outcome trait with length = sample size.
}
  \item{otu.tab}{
The taxonomic table of the microbiome data. Each row represents the taxonomy alignment of each taxon on Kingdom (Domain), Phylum, Class, Order, Family, Genus and Species respectively. Best if it is a class otu_table from package "phyloseq".
}
  \item{tax.tab}{
The OTU table of the microbiome data. Each row represents a subject and each column represents the OTU. Best if it is a class taxonomyTable from package "phyloseq".
}
  \item{tree}{
The phylogenetic tree of the microbiome data. Best if it is a class phylogenetic tree from package "phyloseq".
}
  \item{outcome.trait}{
Specify the type of outcome trait. Must be either "binary" or "continuous".
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
HU J, Koh H, He L, Liu M, Blaser J M, Li H(2018). A two-stage microbial association mapping framework with advanced FDR control.
}
\author{
Jiyuan Hu
}

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
\keyword{ two-stage}

