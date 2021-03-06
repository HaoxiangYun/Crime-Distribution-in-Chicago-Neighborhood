# randomly-assigned-group1
Analysis on Noises around Different Chicago Neighborhood
Project Proposal
-by Group 1


Introduction

Today noise pollution is an inevitable issue of urban living. A number of studies show that noise impacts on related health problems. The article, Noise and well-being in urban residential environments: The potential role of perceived availability to nearby green areas by AnitaGidlöf-Gunnarsson, addresses that urban noise have influence on negative psychological effects such as changes in the physiological systems such as elevated blood pressure, various cognitive deficits such as poor sustained attention, memory/assembly problems, sleep disturbances, modifications of social behavior, psychosocial stress-related symptoms, and emotional/motivational effects. Our team will analyze how the noise in Chicago is related to economics and public health level, and how the distribution of police station in Chicago will influence noise in Chicago as comparing the datasets. To analyze the datasets, we will sort by economics and public health levels. We will also identify the areas where noise complaints come from, and where the police stations are located, and then project the information into a map. This topic is interesting and as well as important because finding the relationships would be beneficial to those who are considering to move to Chicago. In addition, based on the analysis, the government and related organizations could improve communities’ livelihood in Chicago with better approaches. 


 
AnitaGidlöf-Gunnarsson(2007) Noise and well-being in urban residential environments: The potential role of perceived availability to nearby green areas. Retrieved from https://www.sciencedirect.com/science/article/pii/S0169204607000722

Related Work
Address the following questions:
•	What other ideas have been attempted?
•	Why is your team’s idea original compared to prior work?

https://ac.els-cdn.com/0003682X91900072/1-s2.0-0003682X91900072-main.pdf?_tid=bda38cb4-ba68-4424-99db-e01ea8f6c6ef&acdnat=1531416442_633e316424cc9faad0a1320f58bb30d8

This data analysis by A. Garcia & L. J. Faus (1991) only focus on traffic noise levels in Spain by time and location. However, our team will gather data from noise complaint reports in Chicago, and more focus on relationship between noise and several factors like public health level, economics, and distribution of police stations. 

Methods
Packages:
The packages we are going use are “dplyr” for the summarization of data, “rmarkdown” for the demonstration of our analysis process,  “ggplot2” for the visualization of data into graphs, and maybe “magrittr” will be used to write piped instructions.
Functions:
“Data.frame” function will be used to create the data table. There will be three variables, neighborhood categorizes the neighborhood, decibels shows the decibels of sound of the neighborhood, and health shows the public health condition of the neighborhood.
“Lm” function will perform linear regression analysis on two variables
“Summary ” function helps summarize the compilation of basic information of the data.
“Attribute” function  shows the variables in the summary table
“___$___” function extract the value you want from the table of data
“Plot” function visualizes the relationship between two variables into linear functions

Example visualizations:
 
-Given the example graph and relationship function between variables. Government may use them to increase public health level or apply the function on further city construction sites to avoid external losses.
What we have learned:
For now, we have figured out how to perform the analysis over our data. We will probably add on more variables to make the data analysis more accurate in the next two weeks.

Feasibility

•	Is this project able to be completed before the end of the semester?
•	What steps must occur to complete the project before the end of the semester?
•	What is the work plan to accomplish the necessary tasks before the end of the semester?
o	Specify who is doing what and when.
o	Consider making a Gantt chart to highlight each stage of the project. 
This project will be able to be completed before the end of the semester, considering that the data imported will be in .csv format.  The first step is importing the raw data into R, and then tidying it so that it will be in a format that can be easily interpreted and manipulated.  The next step is to visualize that data, into plots or summaries that can easily reflect our findings.  The final step is then to create an interactive shiny app that allows our group to communicate what we’ve done.  Aj wil first import and tidy the data before the 17th of June.  Juhee will then visualize the data before the 23rd of July.  This leaves sufficient time for the final and likely most difficult phase, creating the shiny app.  Howard and Aj will have from the 23rd of July to the 31st of July to create the shiny app.  Then the whole group will work from August 1st to the 4th to display all of our work in a final report and video.

Conclusion

Through analyzing the noise data in comparison to economic and public health data, we will be able to explore their relationships and better understand how to improve our public health.  After importing, tidying, transforming, and finally visualizing our results, in a timely manner, we will be able to communicate these results with others.  This project is also relatively unresearched, since no reports published online relate noise levels with other variables such as public health and economic status.  While we believe this proposal outlines a project which we will be able to complete before our deadline, it is also a challenging topic that we believe will effectively showcase our coding abilities.

Sources
Datasets we will use:
https://www.healthdata.gov/dataset/census-data-selected-socioeconomic-indicators-chicago-2008-%E2%80%93-2012/resource/7f5ad2d0-afc8

Degrees of noise in decibels in different areas of Chicago:
https://maps.bts.dot.gov/arcgis/apps/webappviewer/index.html?id=a303ff5924c9474790464cc0e9d5c9fb

noise complaint datasets in Chicago https://data.cityofchicago.org/Environment-Sustainable-Development/Noise-Complaints-in-Chicago/6ttz-f3sv/data

Public health statistics in selected community area in Chicago:
https://data.cityofchicago.org/Health-Human-Services/Public-Health-Statistics-Selected-public-health-in/iqnk-2tcu

Chicago police station:
https://data.cityofchicago.org/Public-Safety/Police-Stations-Map/gkur-vufi

