library(ggpubr);
library(ggplot2);
library(latex2exp);
library(car);

# Disable pdf generation:
pdf(NULL);

data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake_updated_monthly adjustment_v3.csv"); #1287.8m elevation
data_rye_patch = read.csv("GSOY/GSOM_rye_patch_updated dates_monthly adjustment_v3.csv"); #1260.3m elevation

Salt_Lake_Diff = (data_Salt_Lake$TAVG.ADJ)[1:length(data_Salt_Lake$TAVG.ADJ)];
rye_patch_Diff = (data_rye_patch$TAVG.ADJ)[1:length(data_rye_patch$TAVG.ADJ)];

# If one is missing value in a given year, remove the value from the other dataset:
for(i  in 1:length(Salt_Lake_Diff)){
	if(is.na(Salt_Lake_Diff[i])| is.na(rye_patch_Diff[i])){
		Salt_Lake_Diff[i] = NA;
		rye_patch_Diff[i] = NA;
	}
}

# Remove missing data (values of NA) here:
Salt_Lake_Diff = Salt_Lake_Diff[!is.na(Salt_Lake_Diff)];
rye_patch_Diff = rye_patch_Diff[!is.na(rye_patch_Diff)];

# Print mean, variance, and sample size for each dataset:
print("Salt_Lake");
print(paste("\U03BC", "=", mean(Salt_Lake_Diff, na.rm=TRUE)));
print(paste("\U03C3", "=", sqrt(var(Salt_Lake_Diff, na.rm=TRUE))));
print(paste("n =",length(Salt_Lake_Diff)));
# New line:
cat("\n");

print("rye_patch");
print(paste("\U03BC", "=", mean(rye_patch_Diff, na.rm=TRUE)));
print(paste("\U03C3", "=", sqrt(var(rye_patch_Diff, na.rm=TRUE))));
print(paste("n =",length(rye_patch_Diff)));
# New line:
cat("\n");

# Perform the Shapiro Tests:
shapiro.test(Salt_Lake_Diff);
shapiro.test(rye_patch_Diff);

# Visualize with QQ plots:
png(file = "img/Salt_Lake_Diff_QQ_Plot.png");
qqnorm(Salt_Lake_Diff, , main=expression(paste("Q-Q Plot for ", Delta, "T in Salt Lake City")));
qqline(Salt_Lake_Diff);
dev.off();

png(file = "img/Rye_Patch_Diff_QQ_Plot.png");
qqnorm(rye_patch_Diff, main=expression(paste("Q-Q Plot for ", Delta, "T in Rye Patch Dam")));
qqline(rye_patch_Diff);
dev.off();

#Perform paired two sided t-test:
t.test(rye_patch_Diff, Salt_Lake_Diff, paired=TRUE);






# Remove 10% of the data from each vector, then perform analysis again:
print("10% extra missing data:");
Salt_Lake_Diff[sample(1:length(Salt_Lake_Diff), ceiling(0.1*length(Salt_Lake_Diff)))] = NA;
rye_patch_Diff[sample(1:length(rye_patch_Diff), ceiling(0.1*length(rye_patch_Diff)))] = NA;

# If one is missing value in a given year, remove the value from the other dataset:
for(i  in 1:length(Salt_Lake_Diff)){
	if(is.na(Salt_Lake_Diff[i])| is.na(rye_patch_Diff[i])){
		Salt_Lake_Diff[i] = NA;
		rye_patch_Diff[i] = NA;
	}
}

# Remove missing data (values of NA) here:
Salt_Lake_Diff = Salt_Lake_Diff[!is.na(Salt_Lake_Diff)];
rye_patch_Diff = rye_patch_Diff[!is.na(rye_patch_Diff)];

# Print mean, variance, and sample size for each dataset:
print("Salt_Lake");
print(paste("\U03BC", "=", mean(Salt_Lake_Diff, na.rm=TRUE)));
print(paste("\U03C3", "=", sqrt(var(Salt_Lake_Diff, na.rm=TRUE))));
print(paste("n =",length(Salt_Lake_Diff)));
# New line:
cat("\n");

print("rye_patch");
print(paste("\U03BC", "=", mean(rye_patch_Diff, na.rm=TRUE)));
print(paste("\U03C3", "=", sqrt(var(rye_patch_Diff, na.rm=TRUE))));
print(paste("n =",length(rye_patch_Diff)));
# New line:
cat("\n");

# Perform the Shapiro Tests:
shapiro.test(Salt_Lake_Diff);
shapiro.test(rye_patch_Diff);

# Comment out so we don't override old QQ plots (so they match what is in report)
# Visualize with QQ plots:
#png(file = "img/Salt_Lake_Diff_QQ_Plot_missing_data.png");
#qqnorm(Salt_Lake_Diff, main=expression(paste("Q-Q Plot for ", Delta, "T in Salt Lake City")));
#qqline(Salt_Lake_Diff);
#dev.off();

#png(file = "img/Rye_Patch_Diff_QQ_Plot_missing_data.png");
#qqnorm(rye_patch_Diff, main=expression(paste("Q-Q Plot for ", Delta, "T in Rye Patch Dam")));
#qqline(rye_patch_Diff);
#dev.off();

#Perform paired two sided t-test:
t.test(rye_patch_Diff, Salt_Lake_Diff, paired=TRUE);