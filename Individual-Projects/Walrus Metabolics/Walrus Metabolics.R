#Biometrics Graphed
body_mass_vs_oxygen_consumed <- ggplot(Walrus.Metabolics...Sheet1, aes(x = Body.Mass, y = Oxygen.Consumed, fill = Animal.ID)) + geom_point(shape = 21, size = 3, alpha = 0.6) + geom_smooth(method = "lm", se = FALSE) + ggtitle("Body Mass vs Oxygen Consumption of Three Walruses")
body_mass_vs_oxygen_consumed
body_mass_vs_CO2_produced <- ggplot(Walrus.Metabolics...Sheet1, aes(x = Body.Mass, y = Carbon.Dioxide.Produced, fill = Animal.ID)) + geom_point(shape = 22, size = 3, alpha = 0.6) + ggtitle("Body Mass vs Carbon Dioxide Produced in Three Walruses")
body_mass_vs_CO2_produced
body_mass_vs_oxygen.per.minute_vs_activity <- ggplot(Walrus.Metabolics...Sheet1, aes(x = Body.Mass, y = O2.Per.Min, fill = Activity)) + geom_point(shape = 24, alpha = 0.6) + ggtitle("Body Mass vs O2/min with Activity Type") 
animal_facet <- body_mass_vs_oxygen.per.minute_vs_activity + facet_wrap(~ Animal.ID) 
animal_facet
body_mass_vs_oxygen.consumed_vs_activity <- ggplot(Walrus.Metabolics...Sheet1, aes(x = Body.Mass, y = Oxygen.Consumed, fill = Activity)) + geom_point(shape = 25, alpha = 0.6) + ggtitle("Body Mass vs Oxygen Consumed with Activity Type") 
animal_facet_2 <- body_mass_vs_oxygen.consumed_vs_activity + facet_wrap(~ Animal.ID) 
animal_facet_2

#Average Body Masses
individuals <- Walrus.Metabolics...Sheet1 %>% group_by(Animal.ID) %>% summarize(average_body_mass = mean(Body.Mass))
individuals

#Average Oxygen Consumption per Activity Level
activity_O2_consumption <- Walrus.Metabolics...Sheet1 %>% group_by(Activity) %>% summarize(average_O2_consumption = mean(Oxygen.Consumed))
activity_O2_consumption

#Average Oxygen Consumption per Activity Level in 26005388
walrus_5388 <- Walrus.Metabolics...Sheet1 %>% filter(Animal.ID == 26005388)
average_O2_consumption_5388 <- walrus_5388 %>% group_by(Activity) %>% summarize(average_activity_O2_consumption = mean(Oxygen.Consumed))
average_O2_consumption_5388

#Average Oxygen Consumption per Activity Level in 26005389
walrus_5389 <- Walrus.Metabolics...Sheet1 %>% filter(Animal.ID == 26005389)
average_O2_consumption_5389 <- walrus_5389 %>% group_by(Activity) %>% summarize(average_activity_O2_consumption = mean(Oxygen.Consumed))
average_O2_consumption_5389

#Average Oxygen Consumption per Activity Level in 26005390
walrus_5390 <- Walrus.Metabolics...Sheet1 %>% filter(Animal.ID == 26005390)
average_O2_consumption_5390 <- walrus_5390 %>% group_by(Activity) %>% summarize(average_activity_O2_consumption = mean(Oxygen.Consumed))
average_O2_consumption_5390

#Average Body Mass vs Average Oxygen Consumption in Swimming/Dynamic Apnea
dynamic_body_mass_vs_oxygen_consumption <- ggplot(Swimming_Dynamic.Apnea.Metrics...Sheet1, aes(x = Avg..Body.Mass..kg., y = Avg..Oxygen.Consumed..liters., color = Animal.ID)) + geom_point() + geom_smooth(method = "lm", se = FALSE)  
dynamic_body_mass_vs_oxygen_consumption
body_mass_vs_oxygen_regression <- lm(Avg..Oxygen.Consumed..liters. ~ Avg..Body.Mass..kg., data = Swimming_Dynamic.Apnea.Metrics...Sheet1) 
summary(body_mass_vs_oxygen_regression) 

#Full Scale Body Mass vs Oxygen Consumption in Swimming/Dynamic Apnea
full_dynamic_body_mass_vs_oxygen_consumption <- Walrus.Metabolics...Sheet1 %>% filter(Activity == "Swimming/Dynamic Apnea") 
full_dynamic_body_mass_vs_oxygen_consumption_plot <- ggplot(full_dynamic_body_mass_vs_oxygen_consumption, aes(x = Body.Mass, y = Oxygen.Consumed)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
full_dynamic_body_mass_vs_oxygen_consumption_plot 
full_dynamic_body_mass_vs_oxygen_consumption_regression <- lm(Oxygen.Consumed ~ Body.Mass, data = Walrus.Metabolics...Sheet1) 
summary(full_dynamic_body_mass_vs_oxygen_consumption_regression) 
