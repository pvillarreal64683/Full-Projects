/* 
Task 1: Inspecting The Data
* Inspecting the data by looking at the first 10 results
*/
SELECT TOP 10 *
FROM [dbo].[scores]

/* 
Task 2: Finding Missing Values
* Looking at the amount of missing data
*/
SELECT COUNT(*) - COUNT(Percent_Tested) as num_tested_missing, COUNT(*) as num_schools
FROM [dbo].[scores];

/* 
Task 3: Schools By Building Code
* How many distinct schools are there?
*/
SELECT COUNT(DISTINCT Building_Code) as num_school_buildings
FROM [dbo].[scores];

/* 
Task 4: Best Schools for Math
* Which schools scored highest for math? Looking at scores that are above 640
*/
SELECT School_Name, Average_Score_SAT_Math 
FROM [dbo].[scores]
WHERE Average_Score_SAT_Math >= 640
ORDER BY Average_Score_SAT_Math DESC;

/* 
Task 5: Lowest Reading Score
* What was the lowest score for reading?
*/
SELECT MIN(Average_Score_SAT_Reading) as lowest_reading
FROM [dbo].[scores];

/* 
Task 6: Best Writing School
* Which school scored highest for writing?
*/
SELECT TOP 1 School_Name, MAX(Average_Score_SAT_Writing) as max_writing
FROM [dbo].[scores]
GROUP BY School_Name
ORDER BY max_writing DESC;

/* 
Task 7: Top 10 Schools
* Which are the top 10 schools for all scores?
*/
SELECT TOP 10 School_Name, SUM(Average_Score_SAT_Math + Average_Score_SAT_Reading + Average_Score_SAT_Writing) as average_sat
FROM [dbo].[scores]
GROUP BY School_Name 
ORDER BY average_sat DESC;

/* 
Task 8: Ranking Boroughs
* Which are the top boroughs in terms of average scores and how many schools in each?
*/
SELECT Borough, COUNT(*) as num_schools, SUM(Average_Score_SAT_Math + Average_Score_SAT_Reading + Average_Score_SAT_Writing) / COUNT(*) as average_borough_sat
FROM [dbo].[scores]
GROUP BY Borough
ORDER BY average_borough_sat DESC;

/* 
Task 9: Brooklyn Numbers
* What are the top math scoring schools in Brookly?
*/
SELECT TOP 5 School_Name, Average_Score_SAT_Math
FROM [dbo].[scores]
WHERE Borough = 'Brooklyn'
ORDER BY Average_Score_SAT_Math DESC;