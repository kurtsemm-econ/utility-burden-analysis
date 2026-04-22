# Utility Burdens in the United States (2009–2024)

This project analyzes household utility burdens in the United States using microdata from [IPUMS CPS](https://cps.ipums.org/cps/). Utility burden is defined as the share of household income spent on essential services, including water, electricity, and gas.

The objective is to measure affordability and understand how utility costs are distributed across households, with particular attention to patterns that are not captured by income alone.

## Methodology

I construct utility burden as total utility expenditures divided by household income. The analysis incorporates three key adjustments.

Household income is equivalized by household size to allow for comparability across households.
All monetary values are adjusted for inflation using CPI.
Extreme values are addressed by top-coding burdens at the 98th percentile within each year.

In addition to descriptive statistics, I apply information-theoretic measures such as entropy and mutual information to evaluate how much variation in utility burden is explained by income versus demographic characteristics.

## Key Findings

Utility burdens are highly skewed, with a subset of households facing disproportionately high costs relative to income.

Differences across social groups persist even after accounting for income, suggesting structural disparities in utility access and pricing.

Information-theoretic results indicate that utility burden cannot be reduced to income alone, highlighting the role of institutional and demographic factors.

## Repository Structure

data/ contains raw and processed CPS extracts
scripts/ includes R code for data cleaning, burden construction, and analysis
outputs/ contains summary tables and figures

## Policy Relevance

Utility affordability is a central concern for regulators and public agencies. This analysis provides a framework for evaluating:

how utility costs impact households relative to income
distributional effects of rate structures
equity considerations in infrastructure and pricing decisions

## Tools

R (tidyverse, ipumsr, survey)
Large-scale microdata processing and statistical analysis