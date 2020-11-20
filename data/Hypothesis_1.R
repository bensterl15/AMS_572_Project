data_Redbluff = read.csv("Redbluff_R_Formatted.csv");
data_roseburg = read.csv("roseburg.csv");
#data_sweetfield = read.csv("sweet_field_oregon.csv");
data_sweetfield = read.csv("vegas.csv");

Redbluff_Temp_Data = data_Redbluff$Avg..Annual.Temp;
roseburg_Temp_Data = data_roseburg$TMAX;
sweetfield_Temp_Data = data_sweetfield$TMAX;

Redbluff_Diff = NA;

for(i in 1:(length(Redbluff_Temp_Data) - 1)){
	Redbluff_Diff = c(Redbluff_Diff, Redbluff_Temp_Data[i+1] - Redbluff_Temp_Data[i]);
}

roseburg_Diff = NA;

for(i in 1:(length(roseburg_Temp_Data) - 1)){
	roseburg_Diff = c(roseburg_Diff, roseburg_Temp_Data[i+1] - roseburg_Temp_Data[i]);
}

sweetfield_Diff = NA;

for(i in 1:(length(sweetfield_Temp_Data) - 1)){
	sweetfield_Diff = c(sweetfield_Diff, sweetfield_Temp_Data[i+1] - sweetfield_Temp_Data[i]);
}

Redbluff_Diff = Redbluff_Diff[79:length(Redbluff_Diff)]
roseburg_Diff = roseburg_Diff[40:length(roseburg_Diff)]

Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
roseburg_Diff = roseburg_Diff[!is.na(roseburg_Diff)];
sweetfield_Diff = sweetfield_Diff[!is.na(sweetfield_Diff)];

#print(Redbluff_Diff);
#shapiro.test(Redbluff_Diff);

#print(roseburg_Diff);
#shapiro.test(roseburg_Diff);

print(var(Redbluff_Diff));
print(mean(Redbluff_Diff));

shapiro.test(Redbluff_Diff);

#low_elevation_diffs = c(Redbluff_Diff, roseburg_Diff, sweetfield_Diff)

low_elevation_diffs = c(roseburg_Diff, sweetfield_Diff)

print(var(low_elevation_diffs));
print(mean(low_elevation_diffs));

#shapiro.test(low_elevation_diffs);

t.test(Redbluff_Diff);

t.test(roseburg_Diff);

t.test(sweetfield_Diff);

t.test(low_elevation_diffs);

#print(sweetfield_Diff);
#shapiro.test(sweetfield_Diff);

#Redbluff_Diff = Redbluff_Diff[!is.na(Redbluff_Diff)];
#roseburg_Diff = roseburg_Diff[!is.na(roseburg_Diff)];

#t.test(Redbluff_Diff, roseburg_Diff);