## OmicsMLRepo Project

The OmicsMLRepo project aims to harmonize and standardize clinical metadata 
from public Omics data resources. Currently, [curatedMetagenomicData][] and the 
part of [cBioPortalData][]'s metadata are processed under this project.

[curatedMetagenomicData]: https://www.bioconductor.org/packages/release/data/experiment/html/curatedMetagenomicData.html
[cBioPortalData]: https://www.bioconductor.org/packages/release/bioc/html/cBioPortalData.html

The harmonized metadata are featured with,\
1. Compression: original attributes with same or relevant information are 
merged into a new curated attribute\
2. Consolidation: different forms of the same concept is replaced to one
controlled term\
3. Completeness: improved the completeness of the attribute from compression

## OmicsMLRepoR
OmicsMLRepoR is the R package to facilitate the easy access and search of 
the harmonized metadata produced under the OmicsMLRepo project. 