# Sales Data Exploratory Analysis

---

## Overview

This project involves analysis of sales data for a global electronics retailer. The main goal is to address concerns from senior management about recent sales trends. Despite experiencing significant growth between June 2018 and Feb 2020, sales have started to return to earlier levels. There is a concern that sales could fall below the levels seen before June 2018. Through detailed data analysis, we aim to identify if there are factors contributing to the recent sales decline (post Feb 2020) and develop strategies to maintain and improve sales performance.

**Checkout the full analysis write up on [Medium](https://clfo2014.medium.com/sales-data-analysis-7b3b3b3b1b7).**

---

## Objectives
* To examine historical sales trends and segment them into distinct periods for detailed analysis.
* To identify potential reasons behind the recent sales decline.
* To address concerns from senior executives about sales possibly dipping below pre-May 2018 levels.
* To provide actionable recommendations for stabilizing and improving sales performance.

---

## Methodology
1. __Data Collection:__ Data used in this analysis came from [Maven Analytics](https://www.mavenanalytics.io/data-playground).
2. __Segmentation:__ Divided the analysis period into three phases:
3. __Pre-Peak Sales Period:__ January 2016 - May 2018
4. __Peak Sales Period:__ June 2018 - February 2020
5. __Post-Peak Sales Period:__ March 2020 - February 2021
6. __Exploratory Data Analysis (EDA):__ Analyzed sales trends, customer behavior, and product performance across these periods.
7. __Hypothesis Formulation:__ Developed hypotheses to explain the sales decline based on observed data.
8. __Insight Generation:__ Derived insights to inform marketing and sales strategies.

---

## Summary of Insights
* __Sales Trends:__ Identified a peak in sales during February 2019, with notable high points in December 2018, May 2019, and August 2019. Post-February 2020, a steady decline in sales was observed.

* __Customer Behavior:__ Repeat customers significantly contribute to overall sales, with 23% of customers making at least two purchases and 18% making three or more purchases.

* __Factors in Sales Decline:__
    - A noticeable decline in the total customer count during the Post-Peak Sales Period.
    - A slight decrease in the proportion of repeat customers.
    - A minor decline in average spend per customer.

* __Recommendations:__ Emphasized the need for a comprehensive customer strategy focused on acquiring new customers and encouraging more frequent purchases from existing ones.

---

## Limitations
* __Lack of Contextual Data:__ The analysis does not include detailed information on sales strategies, promotions, or external market conditions during the peak sales period, which limits the ability to conclusively determine the causes of increased sales during this phase.

* __Data Scope:__ The analysis is based on available sales data and may not account for all variables influencing sales trends.

---

## Tools of the Trade
* __tidyverse:__ An integrated collection of R packages designed to make data science faster, easier, and more fun.

* __janitor:__ A package that provides simple functions for examining and cleaning dirty data.

* __lubridate:__ Facilitates working with dates and times in R, making it easier to perform date calculations and parsing.

* __rlang:__ A toolbox for working with base R's non-standard evaluation, providing tools for programming with data structures.

* __ggthemes:__ Provides additional themes and scales to enhance the visual presentation of graphics made with ggplot2.

* __gt:__ An R package designed to create beautiful, flexible, table outputs from R.

* __gtExtras:__ Extends the gt package by adding support for additional stylings, themes, and useful functionalities in creating tables.

---

## Project Structure
* R: Contains the R script for the analysis.

* data: Contains the raw and cleaned up data used for the analysis.

* functions: Contains custom functions used in the analysis.

* png: Contains images generated from the analysis.

* analysis: Contains the detailed analysis write-up.
