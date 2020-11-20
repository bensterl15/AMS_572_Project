# High Elevation:
data_salt_lake_city = read.csv("salt_lake_city.csv"); #1287.8m elevation
data_lovelock = read.csv("lovelock_derby.csv"); #1189.3m elevation

#Low Elevation:
data_Redbluff = read.csv("Redbluff_R_Formatted.csv"); #93.0m elevation
data_orlando = read.csv("orlando.csv"); #32.9m elevation
data_sweetfield = read.csv("sweet_field_oregon.csv"); # 107.6m elevation

salt_lake_Temp_Data = data_salt_lake_city$TMAX;

Redbluff_Temp_Data = data_Redbluff$Avg..Annual.Max.Temp;
orlando_Temp_Data = data_orlando$TMAX;
sweetfield_Temp_Data = data_sweetfield$TMAX;
lovelock_Temp_Data = data_lovelock$TMAX;

# Calculate means: (must round to first decimal place because this is the precision of our data:
salt_lake_mean = round(10*mean(salt_lake_Temp_Data[1:26],na.rm=TRUE))/10;
lovelock_mean = round(10*mean(lovelock_Temp_Data[1:10],na.rm=TRUE))/10;


Redbluff_mean = round(10*mean(Redbluff_Temp_Data[1:49],na.rm=TRUE))/10;
orlando_mean = round(10*mean(orlando_Temp_Data[1:19],na.rm=TRUE))/10;
sweetfield_mean = round(10*mean(sweetfield_Temp_Data[1:26],na.rm=TRUE))/10;


salt_lake_Diff = salt_lake_Temp_Data[27:length(salt_lake_Temp_Data)] - salt_lake_mean;
Redbluff_Diff = Redbluff_Temp_Data[50:length(Redbluff_Temp_Data)] - Redbluff_mean;
orlando_Diff = orlando_Temp_Data[1:length(orlando_Temp_Data)] - orlando_mean;
sweetfield_Diff = sweetfield_Temp_Data[27:length(sweetfield_Temp_Data)] - sweetfield_mean;
lovelock_Diff = lovelock_Temp_Data[11:length(lovelock_Temp_Data)] - lovelock_mean;

salt_lake_Diff = salt_lake_Diff[!is.na(salt_lake_Diff)];
Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
orlando_Diff = orlando_Diff[!is.na(orlando_Diff)];
sweetfield_Diff = sweetfield_Diff[!is.na(sweetfield_Diff)];
lovelock_Diff = lovelock_Diff[!is.na(lovelock_Diff)];

#hist(salt_lake_Diff);
#hist(lovelock_Diff, breaks = seq(-5,5,0.5));

print(lovelock_Diff)

# Create vector of low elevation temperature deviations:
low_elevation_diffs = c(Redbluff_Diff, orlando_Diff, sweetfield_Diff);

# Create vector of high elevation temperature deviations:
high_elevation_diffs = c(salt_lake_Diff, lovelock_Diff);

shapiro.test(salt_lake_Diff);
shapiro.test(lovelock_Diff);
shapiro.test(high_elevation_diffs);
#shapiro.test(low_elevation_diffs);

t.test(salt_lake_Diff);

t.test(low_elevation_diffs);

#print(sweetfield_Diff);
#shapiro.test(sweetfield_Diff);

#Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
#orlando_Diff = orlando_Diff[!is.na(orlando_Diff)];

#t.test(Redbluff_Diff, orlando_Diff);