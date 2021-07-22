---
  title: "About"
author: "Gina Nichols"
date: "9/22/2020"
output: html_document
---

# You can cite this tools using the following:

*V. Nichols and R. Martinez-Feria. Midwest Cover Crop Biomass Explorer. Web Application Released 2020. https://vanichols.shinyapps.io/MidwestRyeCoverCrop/*

# This is simulated data

The data used to create this visualization was produced using the Systems Approach to Land-Use Sustainability (SALUS) model [(Basso and Ritchie, 2015)](https://lter.kbs.msu.edu/wp-content/uploads/2015/04/Basso-and-Ritchie-Ch10-Simulating-crop-growth-and-biogeochemical-fluxes-SALUS-model-KBS-long-term-ecological-research-LTER-site-volume-synthesis-book-2015.pdf) and is part of an open-access publication regarding the effects of cover cropping on weeds [(Nichols et al. 2020)](https://acsess.onlinelibrary.wiley.com/doi/full/10.1002/ael2.20022).

## Model inputs

Inputs to the model included:
- ⛅ incoming solar radiation (MJ m−2)
- 🌡️ maximum and minimum air temperature (°C)
- 🌧️ rainfall (mm)
- 🐛 soil characteristics
- 🚜 management 

The model was calibrated using 12 published studies conducted within the Corn Belt. All of these studies reported measurements of winter rye cover crop biomass at termination, as well as cover crop planting and termination dates. 

*Weather data*

We retrieved daily weather data from 1988-2019 from the North American Land Data Assimilation System project phase 2 (NLDAS-2) dataset.

*Soil data*

Soil information for each site was retrieved from the Soil SURvey GeOgraphic database (SSURGO; Soil Survey Staff), from which we selected data for the predominant soil series (map unit key) at each location. 

*Management*

To provide for realistic initial conditions for soil water at cover crop planting, we simulated a maize crop prior to cover crop planting. In the model, maize was planted in early May, fertilized with 150 kg N ha-1 at planting and harvested 10 days before the prescribed cover crop planting date. Planting density for rye cover crop was assumed to be 300 plants m-2, planted a a 1 cm depth and 20 cm row spacing. No fertilizer was applied to rye in the model.

## Simulations
Simulation for each experiment were run independently, from 1-Jan to 30-Jun of the following year, meaning that each simulation comprised a period of 18 months. We assumed both water- and N-limited rye cover crop growth. 

For questions regarding the visualizations you can email Gina Nichols at: vnichols (replace this with at) iastate.edu
For questions regarding the simulations you can email Rafa Martinez-Feria at: mart2225 (replace this with at) msu.edu

## More detailed model description

A complete model description is available [here](https://github.com/vanichols/shiny_ryeSalus/blob/master/IowaRyeCoverCrop/SALUS-simulation-details.pdf).

# Funding
This material is based upon work that is supported by the National Institute of Food and Agriculture, U.S. Department of Agriculture, under award number 2017-38640-26916 through the North Central Region SARE program under project number GNC18-266. USDA is an equal opportunity employer and service provider. Any opinions, findings, conclusions, or recommendations expressed in this publication are those of the author(s) and do not necessarily reflect the view of the U.S. Department of Agriculture.
