##Overall Weights & Lengths Colored By Region 
weight_vs_region <- ggplot(seot_morphometricsReproStatus_ak_monson, aes(x = WEIGHT, fill = REGION)) + geom_bar() 
weight_vs_region 
#Weight vs Total Length colored by Region
weight_vs_LGTH1_vs_region <- ggplot(seot_morphometricsReproStatus_ak_monson, aes(x = LGTH1, y = WEIGHT, color = REGION)) + geom_point()
weight_vs_LGTH1_vs_region

##Isolated Sea Otters of the AK Peninsula
#AK Peninsula Filtered
sea_otters_of_AK_peninsula <- seot_morphometricsReproStatus_ak_monson %>% filter(REGION == "AK peninsula")
sea_otters_of_AK_peninsula 
#Bar Graph with Weights on the x axis
AK_peninsula_weights <- ggplot(sea_otters_of_AK_peninsula, aes(x = WEIGHT)) + geom_bar() 
AK_peninsula_weights
#Bar Graph with Weights on the y axis
AK_peninsula_weights_reverse <- ggplot(sea_otters_of_AK_peninsula, aes(y = WEIGHT)) + geom_bar()
AK_peninsula_weights_reverse
#Scatter Plot of LGTH1 vs Weight colored by Factor of Age Category
AK_peninsula_LGTH1_vs_weight <- ggplot(sea_otters_of_AK_peninsula, aes(x = LGTH1, y = WEIGHT, color = factor(AGE_CATEGORY), size = factor(AGE_CATEGORY))) + geom_point() + scale_color_brewer(palette = "Set1") + ggtitle("Body Weight vs Full Length in Four Age Categories of Sea Otters of the Alaskan Peninsula")
AK_peninsula_LGTH1_vs_weight
#Scatter Plot of LGTH1 vs Weight Faceted by Sex
AK_peninsula_LGTH1_vs_weight_vs_sex <- ggplot(sea_otters_of_AK_peninsula, aes(x = LGTH1, y = WEIGHT, color = factor(AGE_CATEGORY), size = factor(AGE_CATEGORY))) + geom_point() + scale_color_brewer(palette = "Set1") + facet_grid(~ SEX) + ggtitle("Body Weight vs Full Length in Four Age Categories of Sea Otters of the Alaskan Peninsula")
AK_peninsula_LGTH1_vs_weight_vs_sex

##AK Peninsula vs West Aleutians
AK_peninsula_and_West_Aleutians <- seot_morphometricsReproStatus_ak_monson %>% filter(REGION %in% c("AK peninsula", "west aleutians"))
average_weight_per_region <- AK_peninsula_and_West_Aleutians %>% group_by(REGION) %>% summarize(average_weight = mean(WEIGHT))
average_weight_per_region
