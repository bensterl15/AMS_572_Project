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

"
for(i  in 1:length(Salt_Lake_Wind)){
	if(is.na(Salt_Lake_Temperature[i])| is.na(Salt_Lake_Wind[i])){
		Salt_Lake_Temperature[i] = NA;
		Salt_Lake_Wind[i] = NA;
	}
}
Salt_Lake_Wind = Salt_Lake_Wind[!is.na(Salt_Lake_Wind)];
Salt_Lake_Temperature = Salt_Lake_Temperature[!is.na(Salt_Lake_Temperature)];
print(length(Salt_Lake_Wind));
"
#png("img/Wind_v_temp.png");
#plot(Salt_Lake_Wind, Salt_Lake_Temperature);
#abline(lm(Salt_Lake_Temperature~Salt_Lake_Wind));

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

AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

fit1 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation);
summary(fit1);

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
print(length(Salt_Lake_Temperature))

Salt_Lake_dThunderstorm = Salt_Lake_dThunderstorm / length(Salt_Lake_dThunderstorm);

#plot(Salt_Lake_dThunderstorm, Salt_Lake_Precipitation);

#SANITY CHECK:
"
Salt_Lake_Temperature = (data_Salt_Lake$TAVG)[1:length(data_Salt_Lake$TAVG)];
sun = (data_Salt_Lake$TSUN)[1:length(data_Salt_Lake$TSUN)];
for(i  in 1:length(sun)){
	if(is.na(sun[i])| is.na(Salt_Lake_Temperature[i])){
		sun[i] = NA;
		Salt_Lake_Temperature[i] = NA;
	}
}
sun = sun[!is.na(sun)];
Salt_Lake_Temperature = Salt_Lake_Temperature[!is.na(Salt_Lake_Temperature)];
fit3 = lm(Salt_Lake_Temperature ~ sun);
summary(fit3);
"


# Remove 10% of the data from each vector, then perform analysis again:
#print("10% extra missing data:");

Salt_Lake_Sun[sample(1:length(Salt_Lake_Sun), ceiling(0.1*length(Salt_Lake_Sun)))] = NA;
Salt_Lake_Temperature[sample(1:length(Salt_Lake_Temperature), ceiling(0.1*length(Salt_Lake_Temperature)))] = NA;
Salt_Lake_dThunderstorm[sample(1:length(Salt_Lake_dThunderstorm), ceiling(0.1*length(Salt_Lake_dThunderstorm)))] = NA;
Salt_Lake_Precipitation[sample(1:length(Salt_Lake_Precipitation), ceiling(0.1*length(Salt_Lake_Precipitation)))] = NA;

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

AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm))
AIC(lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation))

fit3 = lm(Salt_Lake_Temperature ~ Salt_Lake_Sun + Salt_Lake_dThunderstorm + Salt_Lake_Precipitation);
summary(fit3);

summary(regsubsets(Salt_Lake_Temperature ~ ., data=as.data.frame(M)));

png("img/Temp_vs_sun_missing_data.png");
plot(Salt_Lake_Sun, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_Sun));

png("img/Temp_vs_dThunderstorm_missing_data.png");
plot(Salt_Lake_dThunderstorm, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_dThunderstorm));

png("img/Temp_vs_Precipitation_missing_data.png");
plot(Salt_Lake_Precipitation, Salt_Lake_Temperature);
abline(lm(Salt_Lake_Temperature~Salt_Lake_Precipitation));

print(length(Salt_Lake_Sun))
print(length(Salt_Lake_Temperature))

Salt_Lake_dThunderstorm = Salt_Lake_dThunderstorm / length(Salt_Lake_dThunderstorm);