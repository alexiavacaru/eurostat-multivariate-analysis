The goal of this project was to take raw data from Eurostat and guide it through a complete process of "cleaning" and advanced statistical analysis. Instead of just looking at basic numbers, I explored how socio-economic indicators influence each other and how we can group European regions based on their statistical similarities.

Step-by-Step:

1. Data Preparation & Cleaning
I started by importing data from multiple formats (CSV, Excel, TXT), paying close attention to removing unnecessary codes and handling missing values (those ":" or empty spaces typical of Eurostat). I brought all figures to the same scale (Standardization) so that different units of measurement wouldn't bias the final results.

2. The Math Behind the Data
I analyzed correlation matrices to see which variables "go hand in hand." I verified mathematical properties like orthogonality and calculated the cosine of angles between vectors to see how aligned my indicators were. Essentially, I made sure the "engine" under the data was running correctly before moving to heavy analysis.

3. Simplifying Reality (PCA)
Since I had many indicators, I used Principal Component Analysis to reduce complexity. I managed to concentrate the information into a few large components that I named manually (e.g., Education Level or Youth Employment), making the data much easier to interpret.

4. Testing the Ground for Factor Analysis
Before searching for hidden factors, I ran rigorous tests (KMO and Bartlett) to be sure my data was suitable for this method. I also used linear regression to calculate partial correlations, isolating the influence of one variable from the others.

5. Categorical Transformation & Correspondence Analysis
I took the numeric data and manually divided it into categories: "Low," "Medium," and "High." From these, I built a contingency table which allowed me to visualize, through a correspondence map, how these levels associate with each other.

6. Regional Grouping (Clustering)
In the end, I wanted to see which regions are similar. I used Ward’s Method to create a hierarchy (Dendrogram) and divided the regions into 3 main groups. I validated this division using the Silhouette score to ensure that regions in the same group are truly similar.

Technologies Used
R Language for all calculations and graphics.
Specialized libraries: psych, factoextra, cluster, corrplot, and ca.
