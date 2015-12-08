# Copyright 2015 IBM Corp. All Rights Reserved.
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# 	http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# ***** AUTHOR *****
#   Irving A Duran/Dubuque/IBM (iaduran@us.ibm.com)
# ***** CREATED *****
#   Aug 25, 2015
# ***** LAST DATE MODIFIED *****
#   Sept 16, 2015
# ***** PROGRAM NAME *****
#   Hagen Ranking
# ***** VERSION *****
#   1.3
# ***** BUILD *****
#   0
# ***** PROGRAM DESCRIPTION *****
# Percentile ranking based on book "Introductory Statistics: Concepts, Models, and Applications by David W. Stockburger"
# R Built > 2.15.0
# ********************************************************************************
# Any code/syntax that is comment out means that might still be in development
# OR would allow you to perform some validation on your dataset.
# ********************************************************************************

# install packages if they don't exist
if (!require("plyr")){
	install.packages("plyr", dependencies = TRUE)
	library("plyr")
}
completeFun <- function(data, desiredCols) {
	completeVec <- complete.cases(data[, desiredCols])
	return(data[completeVec, ])
}
#population/parameters formulas
#population mean = μ = ( Σ Xi ) / N
pop.mean <- function(x){ sum(x) / length(x) }
#population variance = σ2 = Σ ( Xi - μ )2 / N
pop.var <- function(x){ sum((x-pop.mean(x))^2)/ length(x) }
#population standard deviation = σ = sqrt [ Σ ( Xi - μ )2 / N ]
pop.sd <- function(x){ sqrt(pop.var(x)) }
#standardized score (z-score)= Z = (X - μ) / σ
z.s <- function(x, mean, sd){ (x - mean) / sd }

# store variables from modeler
df <- modelerData
df$idx <- as.numeric(rownames(df))
s <- "%%rankingfield%%" #transfer field names selected to modeler
namev <- strsplit(s, ", ") #split variables based on commas

# check if z-score check-box is checked
if ("%%zscorerank%%"=="TRUE"){
	# ********************************************************************************
	# Working with z-scores instead of actual raw score to be ranked
	# ********************************************************************************
	for (j in 1:length(namev[[1]])){
		z.score <- 0 #sanitize
		z.scoreidx <- 0 #sanitize
		df.zs <- completeFun(df[, c("idx", namev[[1]][j])], namev[[1]][j])
		df.zs <- data.frame(df.zs, row.names = seq_along(df.zs$idx)) #reset table index to avoid issues with counts
		
		z.score <- z.s(df.zs[, namev[[1]][j]], 
						pop.mean(df.zs[, namev[[1]][j]]), 
						pop.sd(df.zs[, namev[[1]][j]])) #get z-scores based on function
		
		for (i in 1:length(df.zs[, namev[[1]][j]])){
			z.scoreidx[i] <- df.zs[, "idx"][i]
		}

		df.r <- data.frame(z.score, z.scoreidx)
		df.r <- rename(df.r, c("z.score"=c(paste(namev[[1]],".zscore", sep="")[j]),
								"z.scoreidx"="idx")) #rename column							
		df <- merge(df, df.r, by="idx", all.x=TRUE) #merge data by x
		#clean up after loop
		if(j==length(namev[[1]])){
			df <- df[with(df, order(idx)), ] # sort by initial index
			#df <- df[ , -which(names(df) %in% c("idx"))] #exclude idx column
			rm(z.score, z.scoreidx, df.zs, df.r)
			# replace name prior of ranking using z-score
			#namev <- paste(namev[[1]],".zscore", sep=""); namev <- list(namev);
		}
	}
	# ********************************************************************************
	# Start ranking process
	# ********************************************************************************
	# get frequency count
	freq <- apply(df[namev[[1]]], 2, count)

	# provide names to variables from list
	lst <- lapply(seq(namev[[1]]), 
			function(j){
				y <- data.frame(freq[[j]])
				names(y) <- c(namev[[1]][j], paste(namev[[1]],".freq", sep="")[j])
				return(y)
			}
	)
	# provide names to variables from list
	names(lst) <- namev[[1]]

	# loop through list and perform merge
	for (i in 1:length(namev[[1]])){
		t <- as.data.frame(lst[i])
		t <- t[complete.cases(t), ] #skip records with NA/Blank/Null
		names(t) <- c(namev[[1]][i], paste(namev[[1]],".freq", sep="")[i])
		df <- merge(df, t, by=namev[[1]][i], all=TRUE) #merge data by x
	}

	# ranking formula for Hagen -> source=http://www.psychstat.missouristate.edu/introbook/sbk14.htm
	# PR=Probability Ranking
	# Fb=Frequency below -> is the frequency below; the number of scores which are less than the score value of the percentile rank
	# Fw=Frequency within -> is the frequency within; the number of scores which have the same value as the score value of the percentile rank
	# N=Number of scores
	# PR = ((Fb + (1/2 * Fw)) / N) * 100
	# loop through name list and then rank them according to Hagen ranking formula for each data frame 
	rank <- 0
	rankidx <- 0
	for (j in 1:length(namev[[1]])){
		rank <- 0 #sanitize
		rankidx <- 0 #sanitize
		df.t <- completeFun(df[, c("idx", namev[[1]][j], paste(namev[[1]][j],".freq", sep=""))], namev[[1]][j])
		df.t <- data.frame(df.t, row.names = seq_along(df.t$idx)) #reset table index to avoid issues with counts
		for (i in 1:length(df.t[, namev[[1]][j]])){
			fw <- 1 #sanitize
			fb <- length(which(df.t[, namev[[1]][j]] < df.t[, namev[[1]][j]][i]))
			if((df.t[, paste(namev[[1]][j],".freq", sep="")][i] > 1)=="TRUE"){
				fw <- df.t[, paste(namev[[1]][j],".freq", sep="")][i] #store freq count
				rank[i] <- ((fb + ((1/2)*fw)) / length(df.t[, namev[[1]][j]]))
				rankidx[i] <- df.t[, "idx"][i]
			}
			else{
				rank[i] <- ((fb + ((1/2)*fw)) / length(df.t[, namev[[1]][j]]))
				rankidx[i] <- df.t[, "idx"][i]
			}
		}
		df.r <- data.frame(rank, rankidx)
		df.r <- rename(df.r, c("rank"=c(paste(namev[[1]],".rank", sep="")[j]), 
								"rankidx"="idx")) #rename column
		df <- merge(df, df.r, by="idx", all.x=TRUE) #merge data by x
		df <- df[ , -which(names(df) %in% c(paste(namev[[1]],".freq", sep="")[j]))] #exclude freq columns
		#clean up after loop
		if(j==length(namev[[1]])){
			df <- df[with(df, order(idx)), ] # sort by initial index
			df <- df[ , -which(names(df) %in% c("idx"))] #exclude idx column
			rm(rank, rankidx, df.t, df.r)
		}
	}
	#df;

	# store created data back into modeler
	for (j in 1:length(namev[[1]])){
		modelerData <- cbind(modelerData, df[paste(namev[[1]],".rank", sep="")[j]])
		var1 <- c(fieldName=paste(namev[[1]],".rank", sep="")[j], fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
		modelerDataModel <- data.frame(modelerDataModel, var1)
		
		modelerData <- cbind(modelerData, df[paste(namev[[1]],".zscore", sep="")[j]])
		var2 <- c(fieldName=paste(namev[[1]],".zscore", sep="")[j], fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
		modelerDataModel <- data.frame(modelerDataModel, var2)
	}
}
if ("%%zscorerank%%"=="FALSE"){
	# ********************************************************************************
	# Start ranking process
	# ********************************************************************************
	# get frequency count
	freq <- apply(df[namev[[1]]], 2, count)

	# provide names to variables from list
	lst <- lapply(seq(namev[[1]]), 
			function(j){
				y <- data.frame(freq[[j]])
				names(y) <- c(namev[[1]][j], paste(namev[[1]],".freq", sep="")[j])
				return(y)
			}
	)
	# provide names to variables from list
	names(lst) <- namev[[1]]

	# loop through list and perform merge
	for (i in 1:length(namev[[1]])){
		t <- as.data.frame(lst[i])
		t <- t[complete.cases(t), ] #skip records with NA/Blank/Null
		names(t) <- c(namev[[1]][i], paste(namev[[1]],".freq", sep="")[i])
		df <- merge(df, t, by=namev[[1]][i], all=TRUE) #merge data by x
	}

	# ranking formula for Hagen -> source=http://www.psychstat.missouristate.edu/introbook/sbk14.htm
	# PR=Probability Ranking
	# Fb=Frequency below -> is the frequency below; the number of scores which are less than the score value of the percentile rank
	# Fw=Frequency within -> is the frequency within; the number of scores which have the same value as the score value of the percentile rank
	# N=Number of scores
	# PR = ((Fb + (1/2 * Fw)) / N) * 100
	# loop through name list and then rank them according to Hagen ranking formula for each data frame 
	rank <- 0
	rankidx <- 0
	for (j in 1:length(namev[[1]])){
		rank <- 0 #sanitize
		rankidx <- 0 #sanitize
		df.t <- completeFun(df[, c("idx", namev[[1]][j], paste(namev[[1]][j],".freq", sep=""))], namev[[1]][j])
		df.t <- data.frame(df.t, row.names = seq_along(df.t$idx)) #reset table index to avoid issues with counts
		for (i in 1:length(df.t[, namev[[1]][j]])){
			fw <- 1 #sanitize
			fb <- length(which(df.t[, namev[[1]][j]] < df.t[, namev[[1]][j]][i]))
			if((df.t[, paste(namev[[1]][j],".freq", sep="")][i] > 1)=="TRUE"){
				fw <- df.t[, paste(namev[[1]][j],".freq", sep="")][i] #store freq count
				rank[i] <- ((fb + ((1/2)*fw)) / length(df.t[, namev[[1]][j]]))
				rankidx[i] <- df.t[, "idx"][i]
			}
			else{
				rank[i] <- ((fb + ((1/2)*fw)) / length(df.t[, namev[[1]][j]]))
				rankidx[i] <- df.t[, "idx"][i]
			}
		}
		df.r <- data.frame(rank, rankidx, qnorm(rank))
		df.r <- rename(df.r, c("rank"=c(paste(namev[[1]],".rank", sep="")[j]), 
								"rankidx"="idx",
								"qnorm.rank."=c(paste(namev[[1]],".zscore", sep="")[j]))) #rename column
		df <- merge(df, df.r, by="idx", all.x=TRUE) #merge data by x
		df <- df[ , -which(names(df) %in% c(paste(namev[[1]],".freq", sep="")[j]))] #exclude freq columns
		#clean up after loop
		if(j==length(namev[[1]])){
			df <- df[with(df, order(idx)), ] # sort by initial index
			df <- df[ , -which(names(df) %in% c("idx"))] #exclude idx column
			rm(rank, rankidx, df.t, df.r)
		}
	}
	#df;

	# store created data back into modeler
	for (j in 1:length(namev[[1]])){
		modelerData <- cbind(modelerData, df[paste(namev[[1]],".rank", sep="")[j]])
		var1 <- c(fieldName=paste(namev[[1]],".rank", sep="")[j], fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
		modelerDataModel <- data.frame(modelerDataModel, var1)
		
		modelerData <- cbind(modelerData, df[paste(namev[[1]],".zscore", sep="")[j]])
		var2 <- c(fieldName=paste(namev[[1]],".zscore", sep="")[j], fieldLabel="", fieldStorage="real", fieldMeasure="", fieldFormat="", fieldRole="")
		modelerDataModel <- data.frame(modelerDataModel, var2)
	}
}
