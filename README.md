## Improved findability and AI/ML-readiness of non-omics metadata

The *OmicsMLRepo project* aims to harmonize and standardize clinical metadata 
from public Omics data resources. Currently, [curatedMetagenomicData][] and the 
part of [cBioPortalData][]'s metadata are processed under this project.

[curatedMetagenomicData]: https://www.bioconductor.org/packages/release/data/experiment/html/curatedMetagenomicData.html
[cBioPortalData]: https://www.bioconductor.org/packages/release/bioc/html/cBioPortalData.html

### Harmonized metadata
The harmonized metadata are featured with the following standards:

<img src="https://raw.githubusercontent.com/shbrief/OmicsMLRepoR/master/vignettes/4C_Diagram.png" width="60%" height="60%"/>

### Rubust data searching
*OmicsMLRepoR* is the R package to facilitate the easy access and search of 
the harmonized metadata produced under the OmicsMLRepo project. Thanks to the
ontology, metadata searching in OmicsMLRepoR is much more robust - your query 
automatically includes [OLS][]-defined _**synonyms**_ and all the 
_**descendants**_ from the ontology tree.

[OLS]: https://www.ebi.ac.uk/ols4