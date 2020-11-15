files = list.files(".", pattern=".csv", full.names=TRUE, recursive=TRUE);

data = NULL;

data_ = read.csv(files[1]);

# typeof(data_);

#files = list(files[1],files[2])

for(f in files){
	print(f);
	data_ = read.csv(f);

	for(year in 1968:2020){
		v = data_[grepl(toString(year), data_[2])];
		if(ncol(v) == 0){
			next;
		}
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
			#print(name)
			#print(as.numeric(a))
			if(!is.na(as.numeric(a[[1]]))){
				entry[name] = mean(a,na.rm=TRUE);
			}else{
				entry[name] = a[[1]][1];
			}
		}
		entry["DATE"] = year;
		print(year)
		#print(entry)
		
		# Now that we have constructed our entry, add it to our grand excel file:
		data = merge(data, entry, all=TRUE);
	}
}

write.csv(data,"ugh.csv");