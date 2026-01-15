# Nuclear Power Plant Safety

## 1. Project Intent
**Research Question:** *Does high power output correlate with higher safety risk in nuclear power plants?*

This project analyzes the operational performance of US nuclear power plants between 2007 and 2009. By integrating safety metrics (such as fire inspection findings) with power generation data, this study aims to determine if facilities pushing for higher thermal output compromise safety protocols.

The goal is to identify top-performing plants that maximize energy production while minimizing safety incidents, utilizing R and the tidyverse for data analysis.

## 2. Data Sources
The data for this analysis spans the years 2007–2009 and is aggregated from the [Nuclear Regulatory Commission (NRC)](https://www.nrc.gov/reactors/operating/oversight/prevqtr#plants) and historical generation reports.

### Raw Data Files
| File Name | Description |
| :--- | :--- |
| `usreact07.xls` | 2007 Monthly Utility Generation by Reactor |
| `usreact08.xls` | 2008 Monthly Utility Generation by Reactor |
| `usreact09.xls` | 2009 Monthly Utility Generation by Reactor |
| `fire-inspection-findings.xls` | Detailed reports of fire safety violations |
| `materials-licensing-apps.xls` | Applications for material licensing (Contextual data) |
