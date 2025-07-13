## Repository Overview

# Meta-analysis: Latitudinal Herbivory Gradients in Native and Non-native Plants

This repository contains all R code, data, and supplementary files for the study:

> **"Herbivory increases towards lower latitudes in native but not introduced plants"**  
> Yaolin Guo, Rui-Ting Ju*, Madalin Parepa, Hui Wang, Min Wang, Jinan Lu, Jihua Wu, Bo Li, Oliver Bossdorf  
> *Corresponding author: Rui-Ting Ju (jurt@fudan.edu.cn)

## 🔗 Supplementary Materials Website

A browsable, formatted version of the supplementary material (including figures and interactive tables) is available at:

👉 https://yaolinguo.github.io/latitudinal_gradient_in_herbivory/

---

## 📂 Repository Structure

```text
.
├── data/
│   ├── meta_analysis_dataset.csv         # Source data used to calculate all herbivory effect sizes
│   ├── Eligibility_Workbook.csv          # Table documenting inclusion/exclusion decisions
│   └── Outcome.descriptions.csv          # Table describing herbivory metrics across studies
├── figures/
│   ├── ForestPlot_large.pdf              # High-resolution version of the forest plot (Fig. 2b)
│   └── ForestPlot_large.png              # PNG version for web embedding
├── manuscript/
│   ├── meta_analysis_latitudinal_herbivory.rmd   # RMarkdown source of the manuscript
│   ├── meta_analysis_latitudinal_herbivory.pdf   # Compiled manuscript (with figures/tables)
│   ├── bibliography_meta_analysis.bib            # BibTeX citation file
│   └── citation_style.csl                         # Citation style file (e.g., Ecology Letters)
├── analysis/
│   ├── meta_analysis_herbivory_latitude.rmd       # All R code for main analysis (effect size calculation, REML models)
│   └── .R_functions/                              # Custom R functions called from the main Rmd
├── response_to_reviewers/
│   ├── Response_to_Reviewers.rmd
│   └── Response_to_Reviewers.pdf
├── index.html                          # HTML version of the full supplementary analysis (hosted online)
└── README.md
