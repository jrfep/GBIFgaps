# GBIFgaps
GBIF vs range maps: ranking novel occurrences and knowlege gaps in GBIF data

I developed a simple R-code in order to assess spatial gaps in GBIF data. I use range maps as a source of expert knowlege to validate the geographical extent of occurrence data from GBIF. I summarize the results in a triangular plot that identifies three possible outcomes: overlap, data gaps, knowledge gaps. This will be helpful for users of GBIF data, but also to data providers. Users will be able to assess how representative is the GBIF-data for a species of interest. Data providers will be able to see which areas are under-sampled or under-represented in the database for each species, and might be able to spot inconsistencies in ranges that might be related to taxonomic problems (wrong identification or changes in taxonomy). 

Why is this important?

GBIF's distribution data is based on the contribution of several individuals and organizations worldwide, but this might not be synchronized with the current state of expert knowledge about species distributions. Some geographical regions might be under-represented in GBIF database (DATA GAP), but also new discoveries and considerable range extensions might be recorded in GBIF sources before they get incorporated in other sources of knowledge (KNOWLEDGE GAP).

How does this work?

My approach to compare these sources is based on simple comparisons of polygon overlaps. I consider two sources of distribution knowledge: expert's opinion and GBIF data. Published range maps from different sources can be used as a proxy for expert's opinion. I use the alpha-hull method to convert species occurrence records downloaded from GBIF into a comparable distribution range.

The resulting GBIF-range (set G) is compared to existing range maps (set E) for the corresponding species. Using set theory there are three posible regions:
 * the intersect between both sets (E intersect G, or OVERLAP)
 * the expert range without GBIF data (E-G, or DATA GAP)
 * the range with GBIF data/hull not included in the expert's range (G-E, or KNOWLEDGE GAP)

If both sources have high overlap then the area in (E intersect G) will be much higher than the areas in (E-G) and (G-E). In this optimal case, GBIF occurrence records can be used with confidence. Mismatch between sources can have multiple interpretations: higher (E-G) usually represents lack of sampling or digitalization effort, higher (G-E) might have positive or negative implications. On the one hand GBIF data might be providing new distribution records to complement the expert's-range, but there can also be a mismatch due to errors in identification, changes in taxonomy or differences in nomenclature between sources. 

Why is this useful?

I use a triangular plot in order to summarize the proportion of area in each of these three regions. This is a simple visual aid to assess the status of GBIF data for a species, and how to improve it. It can be used to compare different sources of expert's ranges, or to compare species within a genus, family or higher taxonomic range.

A user, data provider or data manager can see at a glance whether the results for a particular species or group of species points to OVERLAP, DATA GAP or KNOWLEDGE GAP, and decide which actions to take.

In case of OVERLAP the results can be used with higher confidence. In case of spotting a DATA GAP the users might decide to use alternative data sources, while data providers could gather efforts to close this geographical gaps. In case of KNOWLEDGE GAP, data providers and data managers should check for errors in identification or georeferencing in their data, or check the consistency of the taxonomy used.

SOURCES USED

_Software and packages_

I used R version 3.3.1 and packages raster (version 2.5-8), sp (version 1.2-3), rgbif (version 0.9.4), alphahull (version 2.1), spdep (version 0.6-6), Matrix (version 1.2-6), RColorBrewer (version 1.1-2).

* Douglas Bates and Martin Maechler (2016). Matrix: Sparse and Dense
  Matrix Classes and Methods. R package version 1.2-6.
  https://CRAN.R-project.org/package=Matrix

* Roger S. Bivand, Edzer Pebesma, Virgilio Gomez-Rubio, 2013. Applied
  spatial data analysis with R, Second edition. Springer, NY.
  http://www.asdar-book.org/

* Roger S. Bivand, Gianfranco Piras (2015). Comparing Implementations of
  Estimation Methods for Spatial Econometrics. Journal of Statistical
  Software, 63(18), 1-36. URL http://www.jstatsoft.org/v63/i18/.

* Scott Chamberlain, Karthik Ram, Vijay Barve and Dan Mcglinn (2016).
  rgbif: Interface to the Global 'Biodiversity' Information Facility
  'API'. R package version 0.9.4.
  https://CRAN.R-project.org/package=rgbif

* Robert J. Hijmans (2016). raster: Geographic Data Analysis and
  Modeling. R package version 2.5-8.
  https://CRAN.R-project.org/package=raster

* Erich Neuwirth (2014). RColorBrewer: ColorBrewer Palettes. R package
  version 1.1-2. https://CRAN.R-project.org/package=RColorBrewer

* Beatriz Pateiro-Lopez, Alberto Rodriguez-Casal and . (2016).
  alphahull: Generalization of the Convex Hull of a Sample of Points in
  the Plane. R package version 2.1.
  https://CRAN.R-project.org/package=alphahull

* R Core Team (2016). R: A language and environment for statistical
  computing. R Foundation for Statistical Computing, Vienna, Austria.
  URL https://www.R-project.org/.

_Species range data and other vector files_

For the example with 8 species of Amazona (Aves, Psittacidae) I used an old version of range maps of neotropical birds developed by CIESIN and NatureServe, that was provided by SEDAC. This data is freely available and redistributable and is included in the 'data'-folder. The original source is:

* Center for International Earth Science Information Network (CIESIN)/Columbia University, and NatureServe. 2008. Gridded Species Distribution, Version 1: Birds of the Americas Presence Grids. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). http://sedac.ciesin.columbia.edu/data/set/species-v1-americas-bird-presence. Accessed 4 Nov 2015

An updated version of range data for world birds is provided by Birdlife International and NatureServe. This data is available by request, but can not be redistributed. Thus I show an example code and results of the analysis, but users have to download the data from the original source:

* BirdLife International and NatureServe (2014) Bird Species Distribution Maps of the World. Available at http://www.birdlife.org/datazone/species/search

As a source of political borders I used the file TM_WORLD_BORDERS-0.1.ZIP, provided by Bjorn Sandvik at http://thematicmapping.org, accessed in july 2015.
