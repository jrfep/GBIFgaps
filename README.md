# GBIFgaps
GBIF vs range maps: ranking novel occurrences and knowlege gaps in GBIF data

This code uses range maps as a source of expert knowlege to validate occurrence data from GBIF. 

Species occurrence records are downloaded from GBIF and converted into a distribution range using the alpha-hull method. The resulting GBIF-range is compared to existing range maps for the corresponding species, and the intersection of these two ranges is evaluated. If they overlap, then GBIF occurrence records can be used with confidence. Mismatch between sources can have multiple interpretations: GBIF data might provides new distribution records, the expert's-range might be incomplete, both sources might be complementary or there could also be a mismatch due to changes in taxonomy or differences in nomenclature.  

Comparisons of species data can be summarized by genus, family or higher taxonomic rank in order to provide estimates of the overall performance of GBIF data. 
