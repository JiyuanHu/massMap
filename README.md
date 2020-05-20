# massMap
A two-stage microbial association mapping framework with advanced FDR control

This package is developed to powerfully discover trait-associated taxa at the taget rank, such as Genus or species. A two-stage microbial association mapping framework (massMap) is implemented which uses grouping information from the taxonomic tree to strengthen statistical power in association tests at the target rank. MassMap can apply to **binary, continuous and survival traits**. MassMap first screens the association of taxonomic groups at a pre-selected higher taxonomic rank using a powerful microbial group test OMiAT for the binary/continuous trait or optimal microbiome-based survival analysis tool OMiSA for the survival trait respectively. Then it proceeds to test the association for each candidate taxon at the target rank within the significant taxonomic groups identified in the first stage. Hierarchical BH (HBH) and selected subset testing (SST) procedures are evaluated to control the FDR for the two-stage structured tests. Both simulation studies and real data applications demonstrated marked statistical power improvement of massMap over competing methods. 

The manual file is **"massMap-manual.pdf"**. 

Installation of massMap in R:

> library("devtools");

> install_github("JiyuanHu/massMap");

A remark should be make that there requires some patience to install the package. It is a little bit tricky to install the dependent package "phyloseq". But once phyloseq is installed, it's very simply to install massMap.

If you find this R package useful, please cite:
**Hu, Jiyuan, Hyunwook Koh, Linchen He, Menghan Liu, Martin J. Blaser, and Huilin Li. "A two-stage microbial association mapping framework with advanced FDR control." Microbiome 6, no. 1 (2018): 131.**

