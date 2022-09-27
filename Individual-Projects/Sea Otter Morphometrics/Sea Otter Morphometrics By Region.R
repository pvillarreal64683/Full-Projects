##Overall Weights & Lengths Colored By Region 
weight_vs_region <- ggplot(seot_morphometricsReproStatus_ak_monson, aes(x = WEIGHT, fill = REGION)) + geom_bar() 
weight_vs_region 
#Weight vs Total Length colored by Region
weight_vs_LGTH1_vs_region <- ggplot(seot_morphometricsReproStatus_ak_monson, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point()
weight_vs_LGTH1_vs_region


## Data Cleaning: Removing all entries in the "LGTH1" column with entry "-9"
# Filter data to include only entries with valid "LGTH1", NOT "-9" (code for missing information or N/A)
sea_otters_cleanLGTH1 <- seot_morphometricsReproStatus_ak_monson %>% filter(LGTH1 != -9) 
sea_otters_cleanLGTH1
sea_otters_cleanLGTH1_cleanWEIGHT <- sea_otters_cleanLGTH1 %>% filter(WEIGHT > 0)
sea_otters_cleanLGTH1_cleanWEIGHT
#Now graph the new clean data
sea_otters_cleanLGTH1_cleanWEIGHT_graph <- ggplot(sea_otters_cleanLGTH1_cleanWEIGHT, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point()
sea_otters_cleanLGTH1_cleanWEIGHT_graph


#Weight vs Total Length colored by Region - Clean Data
weight_vs_LGTH1_vs_region_clean <- ggplot(sea_otters_cleanLGTH1_cleanWEIGHT, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point() + facet_wrap(~ REGION) 
weight_vs_LGTH1_vs_region_clean
#Removing "east aleutians" due to a sample size of 1
east_aleutians_removed <- sea_otters_cleanLGTH1_cleanWEIGHT %>% filter(REGION != "east aleutians") 
#Re-graph, now without the "east aleutians" region
weight_vs_LGTH1_vs_region_clean_2 <- ggplot(east_aleutians_removed, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point() + facet_wrap(~ REGION)
weight_vs_LGTH1_vs_region_clean_2
#Now add the model
weight_vs_LGTH1_vs_region_clean_model <- ggplot(east_aleutians_removed, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point() + geom_parallel_slopes(se = FALSE) + facet_wrap(~ REGION) 
weight_vs_LGTH1_vs_region_clean_model


##Average Weights and lengths by Region
average_weight_per_region <- sea_otters_cleanLGTH1_cleanWEIGHT %>% group_by(REGION) %>% summarize(average_weight = mean(WEIGHT), average_length = mean(LGTH1))
average_weight_per_region