library(greekLetters);
library(latex2exp);
library(utf8);

data_Salt_Lake = read.csv("GSOY/GSOM_Salt_Lake.csv"); #1189.3m elevation
data_rye_patch = read.csv("GSOY/GSOM_rye_patch.csv"); #1287.8m elevation

Salt_Lake_Temp_Data = data_Salt_Lake$TAVG;
rye_patch_Temp_Data = data_rye_patch$TAVG;

# Calculate means: (must round to first decimal place because this is the precision of our data:
Salt_Lake_mean = round(10 * mean(Salt_Lake_Temp_Data[1:258], na.rm=TRUE)) / 10;
rye_patch_mean = round(10 * mean(rye_patch_Temp_Data[151:415], na.rm=TRUE)) / 10;

Salt_Lake_Diff = Salt_Lake_Temp_Data[259:length(Salt_Lake_Temp_Data)] - Salt_Lake_mean;
rye_patch_Diff = rye_patch_Temp_Data[416:length(rye_patch_Temp_Data)] - rye_patch_mean;

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
print(mean(rye_patch_Diff, na.rm=TRUE));
print(sqrt(var(rye_patch_Diff, na.rm=TRUE)));
print("");

# Perform the Shapiro Tests:
shapiro.test(Salt_Lake_Diff);
shapiro.test(rye_patch_Diff);

t.test(rye_patch_Diff, Salt_Lake_Diff);