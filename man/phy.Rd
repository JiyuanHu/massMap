\name{phy}
\alias{phy}
\docType{data}
\title{
A phyloseq-class experiment-level object example data.
}
\description{
This data is a phyloseq-class object which contains the OTU table, sample data, taxonomy table and phylogenetic tree information. Details about the phyloseq class please see the R package "phyloseq".
}
\usage{data(phy)}
\format{
phyloseq-class experiment-level object
\describe{

\item{\code{otu_table()}}{   OTU Table:         [ 90 taxa and 100 samples ]

  \code{..@ .Data}: a matrix where each row represents the suject,each column represents the OTU.

  \code{..@ taxa_are_rows}: logi FALSE
}

\item{\code{sample_data()}}{Sample Data:       [ 100 samples by 3 sample variables ]

A data frame with 100 observations for one outcome variable ABH and two covariates age and gender.

\code{age}: a numeric vector;

\code{gender}: a factor with levels \code{female}, \code{male}.
}

\item{\code{tax_table()}}{Taxonomy Table:    [ 90 taxa by 7 taxonomic ranks ]

  \code{..@ .Data}: a matrix where each row represents the taxonomy alignment on Kingdom (Domain), Phylum, Class, Order, Family, Genus and Species represently for each taxon.
}

\item{\code{phy_tree()}}{Phylogenetic Tree: [ 90 tips and 89 internal nodes ]


The phylogenetic tree of the example microbiome data. Look for details in the help file of phy_tree() from "phyloseq" package.
}
}
}
\keyword{datasets}
