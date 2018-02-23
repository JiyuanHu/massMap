# massMap
A two-stage microbial association mapping framework with advanced FDR control

This package is developed to efficiently estimate the disease odds ratios of candidate SNPs for the follow-up genetic association studies. OR estimates from the initial GWAS scan suffers the well known selection bias, or the Winner’s curse, which can be rather severe when the association tests for the candidate markers lacks statistical power. The newly developed OR estimator GFcom produces both point and CI estimates for the candidate SNPs. Both the point and CI estimate of GFcom shows efficient performance compared with other OR estimators. 

The manual file is **"massMap-manual.pdf"**. 

Installation of GFcom in R:

> library("devtools");

> install_github("JiyuanHu/massMap");

A remark should be make that there requires some patience to install the package. It is a little bit tricky to install the dependent package "phyloseq". But once phyloseq is installed, it's very simply to install massMap.

If you find this R package useful, please cite:
**HU J, Koh H, He L, Liu M, Blaser J M, Li H(2018). A two-stage microbial association mapping framework with advanced FDR control.**

