# Einlesen der CSV Datei
meineDaten = read.csv2("transact_train_na.txt", sep = "|", header = TRUE, dec = ".")

# Ausgabe der CSV Datei
meineDaten

# Datenanalyse
str(meineDaten)
head(meineDaten, 10)

# Optimierung auf Vollständigkeit
meineDaten$completeness[meineDaten$sessionNo %in% NA] <- "N"
meineDaten$completeness[meineDaten$startHour %in% NA] <- "N"
meineDaten$completeness[meineDaten$startWeekday %in% NA] <- "N"
meineDaten$completeness[meineDaten$duration %in% NA] <- "N"
meineDaten$completeness[meineDaten$cCount %in% NA] <- "N"
meineDaten$completeness[meineDaten$cMinPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$cMaxPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$cSumPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$bCount %in% NA] <- "N"
meineDaten$completeness[meineDaten$bMinPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$bSumPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$bMaxPrice %in% NA] <- "N"
meineDaten$completeness[meineDaten$bStep %in% NA] <- "N"
meineDaten$completeness[meineDaten$onlineStatus %in% NA] <- "N"
meineDaten$completeness[meineDaten$availability %in% NA] <- "N"
meineDaten$completeness[meineDaten$customerNo %in% NA] <- "N"
meineDaten$completeness[meineDaten$maxVal %in% NA] <- "N"
meineDaten$completeness[meineDaten$customerScore %in% NA] <- "N"
meineDaten$completeness[meineDaten$accountLifetime %in% NA] <- "N"
meineDaten$completeness[meineDaten$payments %in% NA] <- "N"
meineDaten$completeness[meineDaten$age %in% NA] <- "N"
meineDaten$completeness[meineDaten$address %in% NA] <- "N"
meineDaten$completeness[meineDaten$order %in% NA] <- "N"
meineDaten$completeness[meineDaten$lastOrder %in% NA] <- "N"
meineDaten$completeness[meineDaten$completeness %in% NA] <- "Y"

# Entfernen von uninteressanten Variablen
meineDaten$cCount <- NULL
meineDaten$cMinPrice <- NULL
meineDaten$cMaxPrice <- NULL
meineDaten$cSumPrice <- NULL
meineDaten$bMinPrice <- NULL
meineDaten$bMaxPrice <- NULL
meineDaten$maxVal <- NULL
meineDaten$accountLifetime <- NULL
meineDaten$onlineStatus <- NULL
meineDaten$customerScore <- NULL
meineDaten$customerNo <- NULL
meineDaten$lastOrder <- NULL

# Erzeugung der separaten Data Frames für jede Session und Übertragung der Werte
for (i in 0:max(meineDaten$sessionNo, na.rm=T))
{
  	# Erzeugen eines Data Frames mit dem Wert i+1
	assign(paste0("session", i), subset(meineDaten, meineDaten$sessionNo == i))

	# Ausgabe des prozentualen Fortschritts
	prozentsatz = i / max(meineDaten$sessionNo, na.rm=T) * 100
	print(paste(prozentsatz, "% erfolgreich abgeschlossen"))
}

# Hinzufügen der Variable countTransactions
for (i in 1:max(meineDaten$sessionNo, na.rm=T))
{
	session = paste0("session", i)
	countTransaction <- nrow(eval(parse(text = session)))
	meineDaten$countTransactions[meineDaten$sessionNo %in% i] <- countTransaction
	prozentsatz = i / max(meineDaten$sessionNo, na.rm=T) * 100
	print(paste(prozentsatz, "% erfolgreich abgeschlossen"))
}

# Erneute Erzeugung der separaten Data Frames für jede Session und Übertragung der Wert
startzeit <- Sys.time()
for (i in 0:max(meineDaten$sessionNo, na.rm=T))
{
  	# Erzeugen eines Data Frames mit dem Wert i+1
	assign(paste0("session", i), subset(meineDaten, meineDaten$sessionNo == i))
	prozentsatz = i / max(meineDaten$sessionNo, na.rm=T) * 100
	print(paste(prozentsatz, "% erfolgreich abgeschlossen"))
}
endzeit <- Sys.time()
print(endzeit - startzeit)

# Definition der mode-Funktion
mode <- function(v) {
	uniqv <- unique(v)
	uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Erzeugen eines End Data Frames mit gleichen Typen
output <- meineDaten[0,]
print(output)

# Löschen einer Zeile
output <- output[-c(0)]

# Berechnung und hinzufügen in Output - weniger Variablen
for (i in 1:max(meineDaten$sessionNo, na.rm=T))
{
	sessionNo <- paste0("session", i, "$sessionNo")
	startHour <- paste0("session", i, "$startHour")
	startWeekday <- paste0("session", i, "$startWeekday")
	duration <- paste0("session", i, "$duration")
	countTransactions <- paste0("session", i, "$countTransactions")
	bCount <- paste0("session", i, "$bCount")
	bSumPrice <- paste0("session", i, "$bSumPrice")
	bStep <- paste0("session", i, "$bStep")
	onlineStatus <- paste0("session", i, "$onlineStatus")
	availability <- paste0("session", i, "$availability")
	payments <- paste0("session", i, "$payments")
	age <- paste0("session", i, "$age")
	address <- paste0("session", i, "$address")
	order <- paste0("session", i, "$order")
	completeness <- paste0("session", i, "$completeness")
	output <- rbind(output, data.frame(		"sessionNo"=mean(eval(parse(text = sessionNo))),
							"startHour"=mean(eval(parse(text = startHour))),
							"startWeekday"=mean(eval(parse(text = startWeekday))),
							"completeness"=mode(as.character(eval(parse(text = completeness)))),
							"duration"=max(eval(parse(text = duration))),
							"countTransactions"=max(eval(parse(text = countTransactions))),
							"bCount"=max(eval(parse(text = bCount))),
							"bSumPrice"=max(as.character(eval(parse(text = bSumPrice)))),
							"bStep"=max(eval(parse(text = bStep)), na.rm = TRUE),
							"onlineStatus"=mode((eval(parse(text = onlineStatus)))),
							"availability"=mode((eval(parse(text = availability)))),
							"payments"=max(as.character(eval(parse(text = payments)))),
							"age"=max(as.character(eval(parse(text = age)))),
							"address"=max(as.character(eval(parse(text = address)))),
							"order"=mode(as.character(eval(parse(text = order))))
))
	prozentsatz = i / max(meineDaten$sessionNo, na.rm=T) * 100
	print(paste(prozentsatz, "% erfolgreich abgeschlossen"))
}

# Berechnung und hinzufügen in Output - alle Variablen
for (i in 1:max(meineDaten$sessionNo, na.rm=T))
{
	sessionNo <- paste0("session", i, "$sessionNo")
	startHour <- paste0("session", i, "$startHour")
	startWeekday <- paste0("session", i, "$startWeekday")
	duration <- paste0("session", i, "$duration")
	# cCount <- paste0("session", i, "$cCount")
	# cMinPrice <- paste0("session", i, "$cMinPrice")
	# cMaxPrice <- paste0("session", i, "$cMaxPrice")
	# cSumPrice <- paste0("session", i, "$cSumPrice")
	bCount <- paste0("session", i, "$bCount")
	# bMinPrice <- paste0("session", i, "$bMinPrice")
	# bMaxPrice <- paste0("session", i, "$bMaxPrice")
	bSumPrice <- paste0("session", i, "$bSumPrice")
	bStep <- paste0("session", i, "$bStep")
	onlineStatus <- paste0("session", i, "$onlineStatus")
	availability <- paste0("session", i, "$availability")
	# customerNo <- paste0("session", i, "$customerNo")
	# maxVal <- paste0("session", i, "$maxVal")
	# customerScore <- paste0("session", i, "$customerScore")
	# accountLifetime <- paste0("session", i, "$accountLifetime")
	payments <- paste0("session", i, "$payments")
	age <- paste0("session", i, "$age")
	address <- paste0("session", i, "$address")
	# lastOrder <- paste0("session", i, "$lastOrder")
	order <- paste0("session", i, "$order")
	completeness <- paste0("session", i, "$completeness")
	output <- rbind(output, data.frame(		"sessionNo"=mean(eval(parse(text = sessionNo))),
							"startHour"=mean(eval(parse(text = startHour))),
							"startWeekday"=mean(eval(parse(text = startWeekday))),
							"duration"=sum(eval(parse(text = duration))),
							"cCount"=sum(eval(parse(text = cCount))),
							"cMinPrice"=min(as.character(eval(parse(text = cMinPrice)))),
							"cMaxPrice"=max(as.character(eval(parse(text = cMaxPrice)))),
							"cSumPrice"=max(as.character(eval(parse(text = cSumPrice)))),
							"bCount"=max(eval(parse(text = bCount))),
							"bMinPrice"=min(as.character(eval(parse(text = bMinPrice)))),
							"bMaxPrice"=max(as.character(eval(parse(text = bMaxPrice)))),
							"bSumPrice"=max(as.character(eval(parse(text = bSumPrice)))),
							"bStep"=max(eval(parse(text = bStep)), na.rm = TRUE),
							"onlineStatus"=getmodeonlinestatus((eval(parse(text = onlineStatus)))),
							"availability"=getmodeonlinestatus((eval(parse(text = availability)))),
							"customerNo"=max(as.character(eval(parse(text = customerNo)))),
							"maxVal"=max(as.character(eval(parse(text = maxVal)))),
							"customerScore"=max(as.character(eval(parse(text = customerScore)))),
							"accountLifetime"=max(as.character(eval(parse(text = accountLifetime)))),
							"payments"=max(as.character(eval(parse(text = payments)))),
							"age"=max(as.character(eval(parse(text = age)))),
							"address"=max(as.character(eval(parse(text = address)))),
							"lastOrder"=max(as.character(eval(parse(text = address)))),
							"order"=mode(as.character(eval(parse(text = order)))),
							"completeness"=mode(as.character(eval(parse(text = completeness))))
))
	prozentsatz = i / max(meineDaten$sessionNo, na.rm=T) * 100
	print(paste(prozentsatz, "% erfolgreich abgeschlossen"))
}

# Optimierung der Anordnung
output <- output[c("sessionNo", "startHour", "startWeekday", "completeness", "duration", "countTransactions", "bCount", "bSumPrice", "bStep", "availability", "payments", "age", "address", "order")]

# Optimierung bei keinerlei bStep Daten
output$bStep[output$bStep %in% -Inf] <- NA
output$duration[output$duration %in% -Inf] <- NA

# Splitting in Train und Testdaten
set.seed(2005)
test.rows <- sample(1:nrow(output), 20000)
test.set <- output[test.rows,]
train.set <- output[-test.rows,]
test.set <- test.set[order(test.set$sessionNo),]

# Ausgabe als CSV
write.csv2(train.set, file = "train.txt", row.names=F)
write.csv2(test.set, file = "test.txt", row.names=F)
