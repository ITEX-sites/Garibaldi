## Trampling Data & Analyses

Chardon, N.I.; Stone, P.; Hilbert, C.; Maclachlan, T.; Ragsdale, B.; Zhao, A.; Goodwin, K.; Collins, C.G.; Hewitt, N.; Elphinstone, C. (2023). Species-Specific Responses to Human Trampling Indicate Alpine Plant Size Is More Sensitive than Reproduction to Disturbance. Plants, 12(17):3040. https://doi.org/10.3390/plants12173040

### Description

Tests i) effects of human trampling disturbance on plant growth and reproduction proxies of common plants across an elevation gradient, and ii) which species are most sensitive to trampling.

Contributors: Nathalie Isabelle Chardon (nathalie.chardon@gmail.com), Carly Hilbert (chilbe02@student.ubc.ca), Philippa Stone (philippa.stone@botany.ubc.ca), Cassandra Elphinstone (cassandra.elphinstone@shaw.ca), Allen Zhao (allen10to11@gmail.com)

Last edited: 24 August 2023

### Data collection - Who, when, where, why and how the data were collected

Dates: July-August 2022

Location: T’aḵ’t’aḵ’múy̓in tl’a In̓inyáx̱a7n region in Garibaldi Provincial Park, British Columbia, Canada on Taylor Meadow (TM), Black Tusk (BT), and Panorama Ridge (PR) trails.

Samplers: Nathalie Chardon, Carly Hilbert, Mackenzie Urquhart-Cronish, Brianna Ragsdale, Teagan MacLachlan, Vickie Lee, Christian Lauber, Carolyn Chong

Data entered by: Carly Hilbert

Methods: In the summer of 2022, we established long-term transects near the major trails in the park (Taylor Meadows, Black Tusk, Panorama Ridge) to quantify the effects of trampling by recreational users. To address how trampling affects charismatic plant communities (blueberry, heath, heather, and sedge meadows) along elevational gradients, we chose sites at multiple elevations per trail. We established transects directly adjacent to the trail and at least a 5 m perpendicular distance away from the trail (control) to compare the effects of trampling on the same vegetation types. 

Plant communities. We recorded landscape characteristics (slope, aspect, latitude, longitude, presence of trees) for each transect and used 0.5m x 1m quadrats to record height, maximum diameter, and bud/flower/fruit counts of our focal plant species (Vaccinium ovalifolium, Cassiope mertensiana, Phyllodoce empetriformis, Phyllodoce grandiflora, Carex sp.). Because we only found Phyllodoce grandiflora at one site, we did not use this species in our analyses. Please see N. Chardon’s recent work on human trampling for greater detail on this sampling approach: 
https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/1365-2664.13384
https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.4276

We also used a standardized approach to photograph each quadrat, and are used these images to calculate how vegetation greenness is impacted by human trampling. We calcluated plant percent cover with a custom algorithm. 

Microbial communities. We selected representative transects per trail to study microbial activity. As a proxy for measuring litter decomposition by microbes, we buried tea bags at these transects to undergo microbial decomposition for one year.

Upcoming work. In Summer 2023, we will return to each transect to re-survey transects for species diversity. We will dig out the buried teabags, and bury new ones to decompose for another year. The species diversity and microbial data are not currently used in analyses.

Goals: We aim to quantify the continued effects of human trampling on these plant and microbial communities by returning each year, thus generating a multi year dataset. Such a dataset will allow us to answer how these plant communities are responding to continuous trampling and ambient warming. We will incorporate BC Parks data on visitation numbers in our analyses to answer how strongly yearly visitation rates correlate with community responses, or if these responses show more of a lag response to past visitation rates. 

### Data files

#### raw_data
_FILENAME <description of file contents, date collected>_

Trampling-TRANSECTS_data.csv/xlsx: 10 m x 0.5 m transect-level data (see merge_fielddata.R for abbreviations); July-Aug 2022

Trampling-QUADS_data.csv/xlsx: quad-level data (see merge_fielddata.R for abbreviations); July-Aug 2022

GPS_with-elev: lat, long, elev for each transect (see merge_fielddata.R for data details); July-Aug 2022

#### compiled_data
_FILENAME <description of file contents, date modified, whether mid or final version>_

quad.RData: gps & transect data matched to quad data; created in merge_fielddata.R; modifed on 25 Apr 2023; final version; variables described in merge_fielddata.R 

trans_ALL.RData: all transect field data, gps, and altitude; created on 15 Nov 2022; final version; variables described in merge_fielddata.R 

plant-percent-cover.csv: Calculated plant percent cover values for each quadrat (Allen Zhao, allen10to11@gmail.com); created on 27 March 2023; final version; analyses in Quadrat_Analysis folder

#### scripts
_SCRIPTNAME <description of script purpose, coding language, versions of software/libraries used>_

merge_fielddata.R: merge GPS, elevation, transect, and quad-level data; add plant percent cover values from Quadrat_Analysis; R; tidyverse, dplyr, lattice

clean_cover.R: check plant percent cover data; R; dplyr

repro.R: define and calculate a standardized reproductive metric, add to quads.RData, and test relationship between reproductive density and plant area by species; R; ggplot2, ggtext, scales, tidyverse, lmerTest, egg

bayesian_int.R: analyses in Bayesian framework to model disturbance*elevation effects on plant height, diameter, repro, and plant cover; R; rjags, R2jags, ggplot2, brms, tidybayes, priorsense, bayesplot, tidyverse, loo

ms_figs.R: figures for manuscript; R; ggplot2, tidybayes, tidyverse, gridExtra, scales

ms_tables.R: tables for manuscript; R; tidyverse

#### outputs
_FOLDERNAME <description of contents>_

test_int/: Bayesian RDS model files (named by trait_model-family_species) and tables for manuscript (May 2023)

ms_figs/: plot results for manuscript (June 2023)


#### Quadrat_Analysis

Analysis code for estimating plant coverage from photos of quadrats. Additional details can be found in the included README.
