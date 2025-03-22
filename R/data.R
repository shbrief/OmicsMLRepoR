#' @name sample_metadata 
#' @title sample_metadata
#' @description A small data table to demonstrating the data reshaping 
#' functions in OmicsMLRepoR.
#' @keywords data
#' @format A data frame with 4 rows and 7 columns
#' @author Sehyun Oh \email{shbrief@gmail.com} 
"sample_metadata"


#' @name mini_cmd 
#' @title A subset of cMD metadata
#' @description A subset of curated version of `sampleMetadata` from the 
#' curatedMetagenomicData (cMD, ver.3.8.0) package.
#' @keywords data
#' @format A data frame with 200 samples and 3 columns ('curation_id',
#' 'hla', and 'package')
#' @author Sehyun Oh \email{shbrief@gmail.com}
"mini_cmd"


#' @name mini_cmd2
#' @title A subset of cMD metadata
#' @description A subset of curated version of `sampleMetadata` from the 
#' curatedMetagenomicData (cMD, ver.3.8.0) package.
#' @keywords data
#' @format A data frame with 200 samples and 7 columns ('curation_id', 
#' 'target_condition', 'target_condition_ontology_term_id',
#' 'pmid', 'disease', 'disease_ontology_term_id', 'package'). 
#' The two key exemplary attributes ('target_condition' and 'disease') 
#' selected here contain multiple values. 
#' @author Sehyun Oh \email{shbrief@gmail.com}
"mini_cmd2"


#' @name mini_cmd3
#' @title A subset of cMD metadata
#' @description A subset of curated version of `sampleMetadata` from the 
#' curatedMetagenomicData (cMD, ver.3.8.0) package.
#' @keywords data
#' @format A data frame with 200 samples and 7 columns ('curation_id', 
#' 'pmid', 'package', 'target_condition', 'feces_phenotype', 
#' 'probing_pocket_depth', 'target_condition_ontology_term_id',
#' 'feces_phenotype_ontology_term_id', 'probing_pocket_depth_ontology_term_id'). 
#' The 'target_condition' is multi-valued attribute, and 'feces_phenotype' and
#' 'probing_pocket_depth' are composite attributes.
#' @author Sehyun Oh \email{shbrief@gmail.com}
"mini_cmd3"

#' @name mini_cbio
#' @title A subset of cBioPortalData metadata
#' @description A subset of curated version of cBioPortal's clinical metadata.
#' @keywords data
#' @format A data frame with 10 samples and 9 columns ('curation_id',
#' 'acronym', 'acronym_ontology_term_id', 'sex', 'package', 'treatment_name',
#' 'treatment_name_ontology_term_id', 'treatment_type', 
#' 'treatment_type_ontology_term_id')
#' @author Sehyun Oh \email{shbrief@gmail.com}
"mini_cbio"