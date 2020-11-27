library(ggpubr);
library(ggplot2);
library(latex2exp);

# Disable pdf generation:
pdf(NULL);

data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake.csv"); #1287.8m elevation
#data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake_updated_monthly adjustment.csv"); #1287.8m elevation
data_rye_patch = read.csv("GSOY/GSOM_rye_patch_updated dates_annual adjustment.csv"); #1260.3m elevation
#data_rye_patch = read.csv("GSOY/GSOM_rye_patch_updated dates_monthly adjustment.csv"); #1260.3m elevation
#data_rye_patch = read.csv("GSOY/GSOM_rye_patch.csv"); #1260.3m elevation

Salt_Lake_Temp_Data = data_Salt_Lake$TAVG;
rye_patch_Temp_Data = data_rye_patch$TAVG;

# Calculate means: (must round to first decimal place because this is the precision of our data:
Salt_Lake_mean = round(10 * mean(Salt_Lake_Temp_Data[1:264], na.rm=TRUE)) / 10;

rye_patch_mean = round(10 * mean(rye_patch_Temp_Data[1:264], na.rm=TRUE)) / 10;

Salt_Lake_Diff = Salt_Lake_Temp_Data[265:length(Salt_Lake_Temp_Data)] - Salt_Lake_mean;
rye_patch_Diff = rye_patch_Temp_Data[265:length(rye_patch_Temp_Data)] - rye_patch_mean;


#Salt_Lake_Diff = (data_Salt_Lake$TAVG.ADJ)[265:length(data_Salt_Lake$TAVG.ADJ)];
rye_patch_Diff = (data_rye_patch$TAVG.ADJ)[265:length(data_rye_patch$TAVG.ADJ)];


for(i  in 1:length(Salt_Lake_Diff)){
	if(is.na(Salt_Lake_Diff[i])| is.na(rye_patch_Diff[i])){
		Salt_Lake_Diff[i] = NA;
		rye_patch_Diff[i] = NA;
	}
}

#png(file = "Missing value Salt.png");
#plot(is.na(Salt_Lake_Diff));

#png(file = "Missing value Rye.png");
#plot(is.na(rye_patch_Diff));

# Remove missing data (values of NA) here:
Salt_Lake_Diff = Salt_Lake_Diff[!is.na(Salt_Lake_Diff)];
rye_patch_Diff = rye_patch_Diff[!is.na(rye_patch_Diff)];

# Print mean, variance, and sample size for each dataset:
print("Salt_Lake");
#print(paste("\U03BC", "=", mean(Salt_Lake_Diff, na.rm=TRUE)));
#print(paste("\U03C3", "=", sqrt(var(Salt_Lake_Diff, na.rm=TRUE))));
print(paste("mean =", mean(Salt_Lake_Diff, na.rm=TRUE)));
print(paste("std =", sqrt(var(Salt_Lake_Diff, na.rm=TRUE))));
print(paste("n =",length(Salt_Lake_Diff)));
# New line:
cat("\n");

print("rye_patch");
#print(paste("\U03BC", "=", mean(rye_patch_Diff, na.rm=TRUE)));
#print(paste("\U03C3", "=", sqrt(var(rye_patch_Diff, na.rm=TRUE))));
print(paste("mean =", mean(rye_patch_Diff, na.rm=TRUE)));
print(paste("std =", sqrt(var(rye_patch_Diff, na.rm=TRUE))));
print(paste("n =",length(rye_patch_Diff)));
# New line:
cat("\n");

# Perform the Shapiro Tests:
shapiro.test(Salt_Lake_Diff);
shapiro.test(rye_patch_Diff);

# Visualize with QQ plots:
png(file = "Salt_Lake_Diff_QQ_Plot.png");
qqplot(y=Salt_Lake_Diff, x=1:length(Salt_Lake_Diff), xlab="x index", ylab="Data value", 
main=TeX("Salt Lake City Temperature Increases: $p \\approx 1.084 \\times 10^{-12}$"));
abline(a = min(Salt_Lake_Diff), b = (max(Salt_Lake_Diff) - min(Salt_Lake_Diff)) / length(Salt_Lake_Diff));
dev.off();

png(file = "Rye_Patch_Diff_QQ_Plot.png");
qqplot(y=rye_patch_Diff, x=1:length(rye_patch_Diff), xlab="x index", ylab="Data value", 
main=TeX("Rye Patch Temperature Increases: $p \\approx 5.759 \\times 10^{-11}$"));

abline(a = min(rye_patch_Diff), b = (max(rye_patch_Diff) - min(rye_patch_Diff)) / length(rye_patch_Diff));
dev.off();

#Perform two sided t-test with unequal variances:
t.test(rye_patch_Diff, Salt_Lake_Diff, paired=TRUE);