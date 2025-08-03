# Understanding the Drivers of Customer Journey Conversion in the Beachwear Category

## Table of Contents

- [Project Overview](#project-overview)
- [Data Sources](#data-sources)
- [Tools](#tools)
- [Data Cleaning/Preparation](#data-cleaningpreparation)
- [Data Analysis](#data-analysis)
- [Results/Findings](#resultsfindings)
- [Recommendations](#recommendations)
- [Limitations](#limitations)
- [References](#references)

### Project Overview
---

This project explores the internal and external factors that affect customer journey conversion rates (CJCR) for the **beachwear product category** on a **Dutch e-commerce platform**. The objective was to identify actionable insights that could help increase CJCR by 1 percentage point through data-driven decision making.

### Data Sources
---

**Internal:**
- Clickstream data (sessions, article events, customer info, transactions)
- Structured relational database tables joined and aggregated to session-level data

**External:**
- *National Meteorological Institute*: Daily weather data (temperature, rainfall)
- *Google Trends*: Weekly search interest for relevant online retail competitors

### Tools
---

- SQL (PostgreSQL)
- R (packages: `dplyr`, `ggplot2`, `mice`, `gmodels`, `car`, `fitdistrplus`)
- Data visualization and logistic regression
- Outlier and missing data handling tools

### Data Cleaning/Preparation
---

- Merged multiple data sources using session ID and timestamps
- Imputed missing values using *Multiple Imputation by Chained Equations (MICE)*
- Removed ~3.8% of outliers using Mahalanobis distance
- Normalized categorical variables and handled inconsistencies in weather data

### Data Analysis
---

- Statistical hypothesis testing for six key research themes:
  1. Session behavior and abandonment
  2. Product attributes (price, popularity)
  3. User demographics and history
  4. Temporal factors (season, weekday, time of day)
  5. Environmental influence (weather)
  6. Market dynamics (search trends of competitors)

- Applied:
  - Chi-square tests (categorical data)
  - Logistic regression models (continuous predictors)
  - Descriptive statistics and plots

### Results/Findings
---

- **Session Time**: Longer session durations positively impact conversion
- **Price Sensitivity**: Higher prices reduce conversion likelihood
- **Product Popularity**: More popular items lead to higher conversion rates
- **User Demographics**: Male users converted slightly more often than female users
- **Repeat Buyers**: Returning customers showed higher conversion rates
- **Seasonality**: Conversion peaks in summer, with some unexpected lift in winter
- **Weekday vs Weekend**: Weekdays showed higher conversions than weekends
- **Time of Day**: No significant effect
- **Temperature**: Warmer days lead to increased conversions
- **Rainfall**: Rainy days reduce conversion rates
- **Search Interest in Competitors**: Surprisingly, increased competitor search interest correlated with slightly higher conversion, possibly due to general market activity

### Recommendations
---

- Prioritize weekday campaigns and align promotions with favorable weather forecasts
- Highlight affordable and popular products in key landing pages
- Focus retention strategies on returning and high-value customers
- Consider segmenting email or ad campaigns by gender
- Use external market trends (e.g., search volume) as a proxy for general shopping activity

### Limitations
---

- Demographic data was incomplete (e.g., gender ~21% missing)
- No data on user age, cart abandonment behavior, or exact checkout flow
- External data (weather, search trends) not personalized at the user level
- Conversion defined at session-level without contextual session path analysis

---

*This project was conducted as part of an academic course in data engineering. Data and results are anonymized and do not represent any specific organization’s proprietary information.*
