## Improved findability and AI/ML-readiness of non-omics metadata

The *OmicsMLRepo project* aims to harmonize and standardize clinical metadata 
from public Omics data resources. Currently, [curatedMetagenomicData][] and the 
part of [cBioPortalData][]'s metadata are processed under this project.

[curatedMetagenomicData]: https://www.bioconductor.org/packages/release/data/experiment/html/curatedMetagenomicData.html
[cBioPortalData]: https://www.bioconductor.org/packages/release/bioc/html/cBioPortalData.html

### Harmonized metadata
The harmonized metadata are featured with,   
1. _**Compression**_: original attributes with same or relevant information are 
merged into a new curated attribute   
2. _**Consolidation**_: different forms of the same concept is replaced to one
controlled term   
3. _**Completeness**_: improved the completeness of the attribute from 
compression   
4. _**Correction rate**_: incorporate ontology, fix wrong or conflicting 
information, remove arbitrary abbreviations

### Rubust data searching
*OmicsMLRepoR* is the R package to facilitate the easy access and search of 
the harmonized metadata produced under the OmicsMLRepo project. Thanks to the
ontology, metadata searching in OmicsMLRepoR is much more robust - your query 
automatically includes [OLS][]-defined _**synonyms**_ and all the 
_**descendants**_ from the ontology tree.

[OLS]: https://www.ebi.ac.uk/ols4