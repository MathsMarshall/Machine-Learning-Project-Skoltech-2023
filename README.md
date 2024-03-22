# Machine-Learning-Project-Skoltech-2024
Spatial Cross-Validation to Mitigate Spatial Autocorrelation (SAC)

## Overview
This project focuses on the critical challenge of Spatial Autocorrelation (SAC) in spatial data analysis, particularly its impact on the prediction performance of spatial models. The presence of SAC in spatial data violates the assumption of independence among observations, which can lead to biased and unreliable model predictions. To address this issue, our project explores the application of spatial cross-validation (CV) methods designed to account for and mitigate the effects of SAC.

## Objectives
The primary goal of this project is to evaluate and enhance the prediction performance of spatial models by effectively incorporating spatial CV methods that consider SAC. Our specific objectives include:

- Understanding SAC and Spatial CV Methods: Investigate the nature of SAC and review various CV methods capable of addressing this problem.
- Implementing Spatial CV Types: Apply spatial, environmental, and buffered leave-one-out (LOO) CV methods to our dataset.
- Exploring Additional SAC Mitigation Methods: Identify and utilize 1-2 other strategies beyond spatial CV to address SAC.
- Developing a Novel Approach: Innovate and implement a new method to mitigate the SAC problem effectively.

## Dataset


## Methodology
Our methodology encompasses a comprehensive approach to understanding and addressing SAC:

- Literature Review: Deep dive into existing research to understand SAC and spatial CV methods thoroughly.
- Spatial CV Implementation: Practical application of spatial, environmental, and buffered-LOO CV to our selected dataset.
- Additional SAC Mitigation Strategies: Exploration and application of other techniques to address SAC.
- Innovation and Development: Creation of a novel method to mitigate SAC, leveraging insights gained through the project.

## Tools and Libraries
This project is implemented using R, focusing on packages such as `sf` for spatial data manipulation, `ggplot2` and `leaflet` for data visualization, and `randomForest` for modeling. We also explore the use of spatial cross-validation techniques available through custom functions and the use of packages such as `sp`, `raster`, and `dplyr` for data handling and preprocessing.

## Getting Started
To get started with this project, please ensure you have R and RStudio installed. Additionally, install the necessary R packages mentioned in the Tools and Libraries section.

## Team
- Holdings Ogon
- Joshua Udobang 
- Nwachukwu Mmesomachi
- Okechukwu Okeke
