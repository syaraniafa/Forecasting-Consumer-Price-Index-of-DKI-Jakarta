# Forecasting the Monthly Consumer Price Index (CPI) of DKI Jakarta - Time Series Analysis Project

## Overview
This project focuses on forecasting the Consumer Price Index (CPI) for DKI Jakarta. The CPI is a crucial indicator for understanding inflation rates, cost of living adjustments, and the overall economic health of the region. Accurate CPI forecasts are essential for policymakers and stakeholders to maintain economic stability and develop effective strategies.

## Objective
The primary objective of this project is to develop and evaluate various predictive models to forecast the monthly CPI of DKI Jakarta, using historical data from 2009 to 2023. This analysis is conducted to identify the model that provides the most accurate CPI forecasts.

## Data Source
The dataset used for this project was sourced from the **Central Bureau of Statistics (BPS)**. It includes monthly CPI data from January 2009 to December 2023 for DKI Jakarta.

## Predictive Models
The following predictive models were implemented and evaluated in this project:
- Naïve Model
- Holt-Winters Exponential Smoothing
- Time Series Regression (TSR)
- Seasonal Autoregressive Integrated Moving Average (SARIMA)
- Neural Networks

## Results
The Neural Network model (with parameters p=3, P=1, size=10) outperformed the other models. It delivered the most accurate forecasts, as evidenced by the following evaluation metrics:
- Mean Absolute Error (MAE): 0.798
- Root Mean Squared Error (RMSE): 1.041
- Mean Absolute Percentage Error (MAPE): 0.721

The Neural Network model successfully captured the seasonal patterns and trends present in the CPI data, making it highly reliable for predicting future CPI movements.

## Conclusion
The Neural Network model proved to be the most effective for forecasting CPI in DKI Jakarta. Its ability to capture seasonal trends and minimize forecasting errors makes it a valuable tool for economic planning and policy development.

## Contributors
2502008630 - CYNTIA ANGELICA <br>
2502037864 - SYARANI AFA NATIRA KUSUMAH
