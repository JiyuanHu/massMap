\name{otu.tab}
\alias{otu.tab}
\docType{data}
\title{
The OTU table of the example microbiome data.
}
\description{
A class 'otu_table' from the R package "phyloseq". Each row is the microbial abundance profile for each individual. Each column is the abundance for each OTU.
}
\usage{data("otu.tab")}
\format{
A Formal class 'otu_table' [package "phyloseq"] with 2 slots

  ..@ .Data        : a matrix where each row represents the suject,each column represents the OTU.
  
  ..@ taxa_are_rows: logi FALSE
}

\references{
HU J, Koh H, He L, Liu M, Blaser J M, Li H(2018). A two-stage microbial association mapping framework with advanced FDR control.
}

\keyword{datasets}
