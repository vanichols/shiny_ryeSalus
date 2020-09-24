---
  title: "About"
author: "Gina Nichols"
date: "9/22/2020"
output: html_document
---


## This is simulated data

The data used to create this visualization was produced using the Systems Approach to Land-Use Sustainability (SALUS) model [(Basso and Ritchie, 2015)](https://lter.kbs.msu.edu/wp-content/uploads/2015/04/Basso-and-Ritchie-Ch10-Simulating-crop-growth-and-biogeochemical-fluxes-SALUS-model-KBS-long-term-ecological-research-LTER-site-volume-synthesis-book-2015.pdf) and is part of an open-access publication regarding the effects of cover cropping on weeds [(Nichols et al. 2020)](https://acsess.onlinelibrary.wiley.com/doi/full/10.1002/ael2.20022).


## More detailed model description

# Model inputs
The model uses daily values of incoming solar radiation (MJ m−2), maximum and minimum air temperature (°C), and rainfall (mm), as well as information on soil characteristics and management. The model was calibrated using published literature studies conducted within the Corn Belt. All of these studies reported measurements of winter rye cover crop biomass at termination, as well as cover crop planting and termination dates. This dataset contains observations from 12 studies. 

For each of the 15 calibration sites, we retrieved daily weather data from the North American Land Data Assimilation System project phase 2 (NLDAS-2) dataset (Xia et al., 2012) using the single-pixel (0.125° resolution) extraction tool and formatter for SALUS (https://salusmodel.ees.msu.edu/NLDAS/). Soil information for each site was retrieved from the Soil SURvey GeOgraphic database (SSURGO; Soil Survey Staff), from which we selected data for the predominant soil series (map unit key) at each location. 

# Simulations
Simulation for each experiment were run independently, from 1-Jan to 30-June of the following year, meaning that each simulation comprised a period of 18 months. We assumed both water- and N-limited rye cover crop growth. To provide for realistic initial conditions for soil water at cover crop planting, we simulated a maize crop, prior to cover crop planting. In the model, maize was planted in early May, fertilized with 150 kg N ha-1 at planting and harvested 10 days before the prescribed cover crop planting date. Planting density for rye cover crop was assumed at 300 plants m-2, 1.0 cm depth and 20 cm row spacing. No fertilizer was applied to rye in the model.
