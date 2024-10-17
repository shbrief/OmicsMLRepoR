## Improved findability and AI/ML-readiness of non-omics metadata

The *OmicsMLRepo project* aims to harmonize and standardize clinical metadata 
from public Omics data resources. Currently, [curatedMetagenomicData][] and the 
part of [cBioPortalData][]'s metadata are processed under this project.

[curatedMetagenomicData]: https://www.bioconductor.org/packages/release/data/experiment/html/curatedMetagenomicData.html
[cBioPortalData]: https://www.bioconductor.org/packages/release/bioc/html/cBioPortalData.html

### Installation
```
if (!require("BiocManager"))
    install.packages("BiocManager")

BiocManager::install("OmicsMLRepoR")
```

### Harmonized metadata
The harmonized metadata are featured with the following standards:

<img src="https://raw.githubusercontent.com/shbrief/OmicsMLRepoR/master/vignettes/4C_Diagram.png" width="60%" height="60%"/>

### Rubust data searching
*OmicsMLRepoR* is the R package to facilitate the easy access and search of 
the harmonized metadata produced under the OmicsMLRepo project. Thanks to the
ontology, metadata searching in OmicsMLRepoR is much more robust - your query 
automatically includes [OLS][]-defined _**synonyms**_ (Example B) and all the 
_**descendants**_ (Example C) from the ontology tree.

[OLS]: https://www.ebi.ac.uk/ols4

##### A. Before harmonization (original metadata, `sampleMetadata`)
```
> library(curatedMetagenomicData)
> nrow(sampleMetadata |> filter(study_condition == "CRC"))
[1] 701
> nrow(sampleMetadata |> filter(disease == "CRC"))
[1] 625
> nrow(sampleMetadata |> filter(study_condition == "crc"))
[1] 0
> nrow(sampleMetadata |> filter(study_condition == "Colorectal Carcinoma"))
[1] 0
> nrow(sampleMetadata |> filter(study_condition == "Colorectal Cancer"))
[1] 0
> nrow(sampleMetadata |> filter(study_condition == "Intestinal Disorder"))
[1] 0
```

##### B. Harmonized metadata (`cmd`)
```
> library(OmicsMLRepoR)
> cmd <- getMetadata("cMD")
> nrow(cmd |> tree_filter(disease, "CRC"))
[1] 701
> nrow(cmd |> tree_filter(disease, "crc")) # not case-sensitive
[1] 701
> nrow(cmd |> tree_filter(disease, "Colorectal Carcinoma")) # synonym
[1] 701
> nrow(cmd |> tree_filter(disease, "Colorectal Cancer")) # synonym
[1] 701
```

##### C. Search descendants of the query in harmonized metadata
```
> onto_res <- cmd |> tree_filter(disease, "Intestinal Disorder")
> unique(onto_res$disease)
 [1] "Crohn Disease;Schizophrenia"                                                           
 [2] "Colorectal Carcinoma;Hepatic Steatosis;Hypertension;Carcinoma"                         
 [3] "Colorectal Carcinoma;Carcinoma"                                                        
 [4] "Colorectal Carcinoma;Type 2 Diabetes Mellitus;Hepatic Steatosis;Hypertension;Carcinoma"
 [5] "Colorectal Carcinoma;Hypertension;Carcinoma"                                           
 [6] "Colorectal Carcinoma;Hepatic Steatosis;Carcinoma"                                      
 [7] "Colorectal Carcinoma;Type 2 Diabetes Mellitus;Hypertension;Carcinoma"                  
 [8] "Colorectal Carcinoma;Adenocarcinoma"                                                   
 [9] "Inflammatory Bowel Disease;Crohn Disease"                                              
[10] "Inflammatory Bowel Disease;Ulcerative Colitis"                                         
[11] "Colorectal Carcinoma"                                                                  
[12] "Cytomegaloviral Infection;Celiac Disease;Gestational Diabetes"                         
[13] "Type 1 Diabetes Mellitus;Celiac Disease;Irritable Bowel Syndrome"                      
[14] "Inflammatory Bowel Disease"                                                            
[15] "Inflammatory Bowel Disease;Fecal Microbiota Transplantation"                           
[16] "Melanoma;Colitis"                                                                      
[17] "Inflammatory Bowel Disease;Anorectal Fistula;Crohn Disease"                            
[18] "Colorectal Carcinoma;Hypercholesterolemia;Adenocarcinoma"                              
[19] "Colorectal Carcinoma;Hypertension;Adenocarcinoma"                                      
[20] "Colorectal Carcinoma;Hypercholesterolemia;Hypertension;Adenocarcinoma"                 
[21] "Colorectal Carcinoma;Metastatic Malignant Neoplasm;Adenocarcinoma"                     
[22] "Inflammatory Bowel Disease;Colitis"                                                    
[23] "Colorectal Carcinoma;Type 2 Diabetes Mellitus;Carcinoma"                               
[24] "Adenoma;Small Intestinal Adenoma"                                                      
[25] "Adenoma;Colorectal Adenoma"       
```