[![INFORMS Journal on Computing Logo](https://INFORMSJoC.github.io/logos/INFORMS_Journal_on_Computing_Header.jpg)](https://pubsonline.informs.org/journal/ijoc)

# Data for the Empirical Analysis of the Paper ["Now You See it, Now You Don't: Obfuscation of Online Third-Party Information Sharing"](https://doi.org/) (DOI TBA)

This archive is distributed in association with the [INFORMS Journal on Computing](https://pubsonline.informs.org/journal/ijoc) under the [Creative Commons Attribution-NonCommercial-NoDerivs](LICENSE).

This repository includes the data used in the empirical analysis of the paper
[Now You See it, Now You Don't: Obfuscation of Online Third-Party Information Sharing](https://doi.org/) by A. Eshghi, R. Gopal, H. Hidaji, and R. Patterson.


## Cite
To cite this material, please cite the [paper](https://doi.org/) using its DOI.

[![DOI](https://zenodo.org/badge/)](https://zenodo.org/badge/latestdoi/) (TBA)


## Description

The goal of this repository is to share the data of the empirical analysis in our paper. Our motivation is to present our data so that our analysis can be readily replicated or further analysis can be done.

## Repository Structure
We have a simple repository structure as explained below.

The [Data](Data) folder contains three subfolders for the list of website URLs, the raw data, and the cleaned data. In the subfolder for [website URLs](Data/Websites_URLs), there are two separate csv files for each of the Health and News website categories, one ordered according to size, and the other cleaned and ordered randomly. In the subfolder for the [raw data](Data/Raw_Data), the raw data from browsing the Health and News website categories using with and without DNT is provided in the JSON format. Finally, the subfolder for [clean data](Data/Clean_Data) includes the final clean dataset that was used to create our empirical models. The FinalData.csv includes the websites' attributes and the extent of third-party usage by each website. The two xlsx files in this folder include additional information regarding the extent of third-party usage by websites.

