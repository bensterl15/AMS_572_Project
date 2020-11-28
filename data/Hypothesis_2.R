

# Disable pdf generation:
pdf(NULL);

data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake_updated_monthly adjustment_v3.csv"); #1287.8m elevation

Salt_Lake_Percipitation = (data_Salt_Lake$PRCP)[1:length(data_Salt_Lake$PRCP)];
Salt_Lake_dThunderstorm = (data_Salt_Lake$DYTS)[1:length(data_Salt_Lake$DYTS)];


# All values of NA should go to 0:
for(i  in 1:length(Salt_Lake_dThunderstorm)){
	if(is.na(Salt_Lake_dThunderstorm[i])){
		Salt_Lake_dThunderstorm[i] = 0;
	}
}

fit = lm(Salt_Lake_Percipitation ~ Salt_Lake_dThunderstorm);

png("Linear_Regression_Plot");
summary(fit);