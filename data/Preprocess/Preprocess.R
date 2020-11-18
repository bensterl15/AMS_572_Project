#NOTE: TO RUN THIS CORRECTLY, REMOVE "data.csv" FROM THE SEARCH PATH!!

files = list.files(".", pattern=".csv", full.names=TRUE, recursive=TRUE);

data = NULL;

data_ = read.csv(files[1]);

# typeof(data_);

#files = list(files[1],files[2])

last_year = 1968;

for(f in files){
	print(f);
	data_ = read.csv(f);

	b_reached_year = FALSE;
	b_years_plus = 0;
	for(year in last_year:2020){
		v = data_[grepl(toString(year), data_[2])];

		if(ncol(v) == 0){
			# To speed up execution, if we reach a year not in the excel file, but we already got to a valid year, this means we can stop searching through the years and move onto the next file:
			if(b_reached_year){
				b_years_plus = b_years_plus + 1;
				if(b_years_plus == 3){
					# Set last_year so we don't need to look in previous years.. HUGE time savings:
					last_year = year - 3;
					break;
				}else{
					next;
				}
			}else{
				next;
			}
		}

		b_years_plus = 0;
		b_reached_year = TRUE;
		
		entry = vector(mode="list", length = 124);
		names(entry) = names(v)
		
		#ugh = unlist(v)
		#print(typeof(ugh[1][2]));
		#print(v$STATION);
		#print(names(entry))
		for(name in names(entry)){
			a = v[name];
			#print(a[[1]])
			a = a[a != ""];
			if(any(!is.na(as.numeric(a)))){
				entry[name] = mean(as.numeric(a),na.rm=TRUE);
				#print('yes');
			}else{
				entry[name] = a[[1]][1];
				#print('no');
			}
			
		}
		entry["DATE"] = year;
		print(year)
		#print(entry)
		
		# Now that we have constructed our entry, add it to our grand excel file:
		data = merge(data, entry, all=TRUE);

		if(year == 2020){
			last_year = 1968;
			b_years_plus = 0;
		}
	}
}

write.csv(data, "data.csv");