# NYC-Complaint-Analysis

An overview of my work with NYC311 Data concerning complaints along with factors such as income. Contributors also credited.

## Project Report: Social and Economic Factors Influencing New Yorkers' Despair

### Group: FP-3
- Robin Klaver
- Jose Medina
- Xinyu Zhang
- Tirth R Gandhi

### Introduction
A March 2024 survey by the Citizens Budget Commission revealed a decline in residents' satisfaction with the quality of life in NYC, with only 30% rating conditions as "excellent or good," down from 50% in 2017. Safety concerns have also increased, with only 37% rating neighborhood safety as "excellent or good."

### Research Problem
How do social and economic factors influence 311 service requests in New York City?

### Research Questions
- Are residents from higher-income or higher-education neighborhoods more likely to file complaints due to better knowledge of government channels?
- Do complaints get resolved faster in higher-income, higher-education neighborhoods?
- What does text mining reveal about the resolution time and complaint type?

### Data Overview
The dataset includes 36.8M rows and 41 columns for the year 2023, with additional datasets on demographics, density, income, and weather.

### Data Analysis Methods
1. **Clustering Residents:** Identified clusters based on density, education, income, and age.
2. **Regression Analysis:** Examined the duration of complaint resolution and the number of complaints per resident.
3. **Text Mining:** Analyzed sentiment and resolution descriptions to identify trends in resident dissatisfaction.

### Key Findings
- Complaints are more frequent in lower socioeconomic areas.
- Higher education levels lead to faster complaint resolutions.
- Wealthier areas require more targeted resources due to complex complaints.
- Sentiment analysis reveals significant dissatisfaction with water-related issues and noise disturbances.

### Recommendations
- Focus on timely complaint resolutions.
- Target interventions for water-related issues and noise disturbances.
- Educate residents on effective communication with city services.
- Optimize resource allocation based on time of day.

### Conclusion
Lower socioeconomic factors are associated with higher complaint frequencies, while higher education levels improve complaint resolution efficiency. Wealthier areas may need more resources for handling complex complaints effectively.

### References
- Paolicelli, A. (2024). Survey finds New York City residents unhappy with quality of life. [Link](https://ny1.com/nyc/all-boroughs/news/2024/03/20/survey-finds-new-york-city-residents-unhappy-with-quality-of-life)

### Appendix
- Sum of squared plot and clusters (including weather data)
- Enclosed files and R scripts for analysis

---

This project provides valuable insights into how social and economic factors influence the nature and resolution of 311 complaints in NYC, with actionable recommendations for improving resident satisfaction and service efficiency.
