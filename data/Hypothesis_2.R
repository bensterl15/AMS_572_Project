library(corrplot)
library(leaps)

# Disable pdf generation:
pdf(NULL);

data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake_updated_monthly adjustment_v3.csv"); #1287.8m elevation

#Corresponds to 1965 to 2004
starting_index = 206;
ending_index = 685;

Salt_Lake_Temperature = (data_Salt_Lake$TAVG)[starting_index:ending_index];
Salt_Lake_Sun = (data_Salt_Lake$TSUN)[starting_index:ending_index];
Salt_Lake_dThunderstorm = (data_Salt_Lake$DYTS)[starting_index:ending_index];
Salt_Lake_Precipitation = (data_Salt_Lake$PRCP)[starting_index:ending_index];

Salt_Lake_Temperature = (data_Salt_Lake$TAVG)[starting_index:ending_index];

# All values of 0 should go to NA:
for(i in 1:length(Salt_Lake_Sun)){
	if(!is.na(Salt_Lake_Sun[i])){
		if(Salt_Lake_Sun[i] == 0){
			Salt_Lake_Sun[i] = NA;		
		}
	}
}

for(i  in 1:length(Salt_Lake_Sun)){
	if(is.na(Salt_Lake_Sun[i])| is.na(Salt_Lake_Temperature[i]) | is.na(Salt_Lake_dThunderstorm[i]) | is.na(Salt_Lake_Precipitation[i])){
		Salt_Lake_Sun[i] = NA;
		Salt_Lake_Temperature[i] = NA;
		Salt_Lake_dThunderstorm[i] = NA;
		Salt_Lake_Precipitation[i] = NA;
	}
}

# Remove missing data (values of NA) here:
Salt_Lake_Sun = Salt_Lake_Sun[!is.na(Salt_Lake_Sun)];
Salt_Lake_Temperature = Salt_Lake_Temperature[!is.na(Salt_Lake_Temperature)];
Salt_Lake_dThunderstorm = Salt_Lake_dThunderstorm[!is.na(Salt_Lake_dThunderstorm)];
Salt_Lake_Precipitation = Salt_Lake_Precipitation[!is.na(Salt_Lake_Precipitation)];

shapiro.test(Salt_Lake_Temperature);
shapiro.test(Salt_Lake_Sun);
shapiro.test(Salt_Lake_dThunderstorm);
shapiro.test(Salt_Lake_Precipitation);

# Generate Correlation Matrix:
M = cbind(Salt_Lake_Sun, Salt_Lake_dThunderstorm, Salt_Lake_Precipitation)
colnames(M) = c("Sunlight","Days of Thunderstorms","Precipitation");
png("img/correlation_plot.png");
corrplot(cor(M));

print("AIC Values");
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

print("BIC Values");
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

print("Multilinear Regression Reports");
model1 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun);
summary(model1);

model2 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm);
summary(model2);

model3 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation);
summary(model3);

summary(regsubsets(Salt_Lake_Temperature ~ ., data=as.data.frame(M)));

png("img/Temp_vs_sun.png");
plot(Salt_Lake_Sun, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_Sun));

png("img/Temp_vs_dThunderstorm.png");
plot(Salt_Lake_dThunderstorm, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_dThunderstorm));

png("img/Temp_vs_Precipitation.png");
plot(Salt_Lake_Precipitation, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_Precipitation));

print(length(Salt_Lake_Sun))

# Calculate the prediction interval:
new_value = data.frame(Salt_Lake_Sun = 15000, Salt_Lake_dThunderstorm = 4, Salt_Lake_Precipitation = 35)
predict(model3, new_value, interval = "prediction", level = 0.95)







# Remove 5% of the data from each vector, then perform analysis again:

Salt_Lake_Sun[sample(1:length(Salt_Lake_Sun), ceiling(0.05*length(Salt_Lake_Sun)))] = NA;
Salt_Lake_Temperature[sample(1:length(Salt_Lake_Temperature), ceiling(0.05*length(Salt_Lake_Temperature)))] = NA;
Salt_Lake_dThunderstorm[sample(1:length(Salt_Lake_dThunderstorm), ceiling(0.05*length(Salt_Lake_dThunderstorm)))] = NA;
Salt_Lake_Precipitation[sample(1:length(Salt_Lake_Precipitation), ceiling(0.05*length(Salt_Lake_Precipitation)))] = NA;

for(i  in 1:length(Salt_Lake_Sun)){
	if(is.na(Salt_Lake_Sun[i])| is.na(Salt_Lake_Temperature[i]) | is.na(Salt_Lake_dThunderstorm[i]) | is.na(Salt_Lake_Precipitation[i])){
		Salt_Lake_Sun[i] = NA;
		Salt_Lake_Temperature[i] = NA;
		Salt_Lake_dThunderstorm[i] = NA;
		Salt_Lake_Precipitation[i] = NA;
	}
}

# Remove missing data (values of NA) here:
Salt_Lake_Sun = Salt_Lake_Sun[!is.na(Salt_Lake_Sun)];
Salt_Lake_Temperature = Salt_Lake_Temperature[!is.na(Salt_Lake_Temperature)];
Salt_Lake_dThunderstorm = Salt_Lake_dThunderstorm[!is.na(Salt_Lake_dThunderstorm)];
Salt_Lake_Precipitation = Salt_Lake_Precipitation[!is.na(Salt_Lake_Precipitation)];

shapiro.test(Salt_Lake_Temperature);
shapiro.test(Salt_Lake_Sun);
shapiro.test(Salt_Lake_dThunderstorm);
shapiro.test(Salt_Lake_Precipitation);

# Generate Correlation Matrix:
M = cbind(Salt_Lake_Sun, Salt_Lake_dThunderstorm, Salt_Lake_Precipitation)
colnames(M) = c("Sunlight","Days of Thunderstorms","Precipitation");
png("img/correlation_plot_missing_data.png");
corrplot(cor(M));

print("AIC Values");
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

print("BIC Values");
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
BIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

print("Multilinear Regression Reports");
model1 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun);
summary(model1);

model2 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm);
summary(model2);

model3 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation);
summary(model3);

summary(regsubsets(Salt_Lake_Temperature ~ ., data=as.data.frame(M)));

print(length(Salt_Lake_Sun))

# Calculate the prediction interval:
new_value = data.frame(Salt_Lake_Sun = 15000, Salt_Lake_dThunderstorm = 4, Salt_Lake_Precipitation = 35)
predict(model3, new_value, interval = "prediction", level = 0.95)