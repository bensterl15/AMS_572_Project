# High Elevation:
data_Derby = read.csv("GSOY/GSOM_Salt_Lake.csv"); #1189.3m elevation
data_rye_patch = read.csv("GSOY/GSOM_rye_patch.csv"); #1287.8m elevation
data_winnemucca = read.csv("GSOY/GSOM_Winnemucca_airport.csv"); #1309.4m elevation

#Low Elevation:
data_Redbluff = read.csv("GSOY/GSOM_RedBluff.csv"); #93.0m elevation
data_eureka = read.csv("GSOY/GSOM_Eureka.csv"); # 6.1m elevation

rye_patch_Temp_Data = data_rye_patch$TAVG;
Derby_Temp_Data = data_Derby$TAVG;
Winnemucca_Temp_Data = data_winnemucca$TAVG;

Redbluff_Temp_Data = data_Redbluff$TAVG;
eureka_Temp_Data = data_eureka$TAVG;

# Calculate means: (must round to first decimal place because this is the precision of our data:
rye_patch_mean = round(10 * mean(rye_patch_Temp_Data[151:415], na.rm=TRUE)) / 10;
Derby_mean = round(10 * mean(Derby_Temp_Data[1:258], na.rm=TRUE)) / 10;
Winnemucca_mean = round(10 * mean(Winnemucca_Temp_Data[830:1093])) / 10;

Redbluff_mean = round(10*mean(Redbluff_Temp_Data[639:903],na.rm=TRUE))/10;
eureka_mean = round(10*mean(eureka_Temp_Data[75:338],na.rm=TRUE))/10;


rye_patch_Diff = rye_patch_Temp_Data[416:length(rye_patch_Temp_Data)] - rye_patch_mean;
Derby_Diff = Derby_Temp_Data[259:length(Derby_Temp_Data)] - Derby_mean;
Winnemucca_Diff = Winnemucca_Temp_Data[1094:length(Winnemucca_Temp_Data)] - Winnemucca_mean;

Redbluff_Diff = Redbluff_Temp_Data[904:length(Redbluff_Temp_Data)] - Redbluff_mean;
eureka_Diff = eureka_Temp_Data[339:length(eureka_Temp_Data)] - eureka_mean;

rye_patch_Diff = rye_patch_Diff[!is.na(rye_patch_Diff)];
Derby_Diff = Derby_Diff[!is.na(Derby_Diff)];
Winnemucca_Diff = Winnemucca_Diff[!is.na(Winnemucca_Diff)];

Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
eureka_Diff = eureka_Diff[!is.na(eureka_Diff)];

# Create vector of low elevation temperature deviations:
low_elevation_diffs = c(Redbluff_Diff, eureka_Diff);

# Create vector of high elevation temperature deviations:
high_elevation_diffs = c(rye_patch_Diff, Derby_Diff, Winnemucca_Diff);

shapiro.test(Winnemucca_Diff);
shapiro.test(rye_patch_Diff);
shapiro.test(high_elevation_diffs);

print("Winnemucca");
print(mean(Winnemucca_Diff, na.rm=TRUE));
print(sqrt(var(Winnemucca_Diff, na.rm=TRUE)));
print("");

print("Derby");
print(mean(Derby_Diff, na.rm=TRUE));
print(sqrt(var(Derby_Diff, na.rm=TRUE)));
print("");

print("rye_patch");
print(mean(rye_patch_Diff, na.rm=TRUE));
print(sqrt(var(rye_patch_Diff, na.rm=TRUE)));
print("");

print("Redbluff");
print(mean(Redbluff_Diff, na.rm=TRUE));
print(sqrt(var(Redbluff_Diff, na.rm=TRUE)));
print("");

print("Eureka");
print(mean(eureka_Diff, na.rm=TRUE));
print(sqrt(var(eureka_Diff, na.rm=TRUE)));
print("");

t.test(rye_patch_Diff, Derby_Diff);

#print(eureka_Diff);
#shapiro.test(eureka_Diff);

#Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
#Derby_Diff = Derby_Diff[!is.na(Derby_Diff)];

#t.test(Redbluff_Diff, Derby_Diff);