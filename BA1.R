setwd("C:/Users/HP/Documents/CEPB/07_BA/Daten Leo")
library(data.table)
library(Matrix)
library(igraph)
library(tidyverse)
library(sjlabelled)
library(sjmisc)
library(sjstats)
library(sjPlot)


#Daten einlesen

data_q1 <- read.csv2("q1_final_demand_fig21.csv")
data_q2 <- read.csv2("q2_final_demand_fig21.csv")
data_q3 <- read.csv2("q3_final_demand_fig21.csv")
data_q4 <- read.csv2("q4_final_demand_fig21.csv")
data_q5 <- read.csv2("q5_final_demand_fig21.csv")
data_eu <- read.csv("eu22.csv")

#build matrix Z
matrixZ <- data_eu[, -(2946:ncol(data_eu))] #including value added
matrixZ <- matrixZ[-(2945:2950), -(1)]


#Technische Matrix erzeugen

#Gross Output: alle Spalten werden aufsummiert 
total <- rowSums(data_eu[1:2944, -(1)])
#Check: alle Zeilen werden aufsummiert, sollte das gleiche ergeben
total2 <- colSums(data_eu[,2:2945]) #Differenz: 3.897466e-12 --> klein genug
#als intermediate demand matrix initialisieren
matrixA <- matrixZ
#in jeder Zeile wird durch den GO geteilt, es sei denn dieser ist Null (dann wird Null eingetragen)
for (i in 1:nrow(matrixA)) {
  for (j in 1:ncol(matrixA)) {
    # Überprüfen, ob der Wert in x 0 ist
    if (total[j] == 0) {
      matrixA[i, j] <- 0  # Setze Ergebnis auf 0, wenn total[j] == 0
    } else {
      matrixA[i, j] <- matrixA[i, j] / total[j]  # Division durch x[j]
    }
  }
} 
#A wird transponiert zu A'
matrixA <- (t(matrixA))
#checke Nan
rows_with_nan <- apply(is.nan(matrixA), 1, any)
which(rows_with_nan)
#speichern
write.csv(matrixA, "techMatrix2.csv", row.names = FALSE)
matrixA <- read.csv("techMatrix2.csv")


#Schockgrößen ermitteln

#finde US Sektoren im Datensatz
find_var("US", data = matrixZ) # sectors 2817 - 2880
# Füge die Variable 'US' mit den Zahlen 2817 bis 2880 hinzu, da US Sektoren in diesen Zeilen
US <- 2817:2880
#Liste der relevanten Präfixe (Länderkürzel der EU27), Finland braucht Unterstrich, da sonst FIGW (ROW) mit im Datensatz auftaucht
eu27_prefixes <- c("BE", "BG", "CZ", "DK", "DE", "EE", "IE", "GR", "ES", "FR", "HR", 
                   "IT", "CY", "LV", "LT", "LU", "HU", "MT", "NL", "AT", "PL", 
                   "PT", "RO", "SI", "SK", "FI_", "SE")
# Suche nach Zeilennummern, deren Variablen mit einem der Präfixe beginnen
EU27 <- which(sapply(colnames(matrixZ), function(name) {
  any(startsWith(name, eu27_prefixes))
}))

#leerer Datensatz für Ergebnisse
US_Shocks <- data.frame(Variable = character(), GesamtSumme = numeric(), TeilSumme = numeric(), Verhältnis = numeric())
#Der Loop berechnet für jeden US-Sektor den Anteil der Importe aus den EU27 an den gesamten Inputs 
# Iteriere über die Spalten/Inputs der US
for (j in US) {
  # Spaltenname extrahieren
  col_name <- colnames(matrixZ)[j]
  # berechne die Gesamtsumme der Spalten/Inputs
  total_sum <- sum(matrixZ[[j]], na.rm = TRUE)
  # berechne die Summe der Inputs aus der EU
  partial_sum <- 0
  for (i in EU27){
    partial_sum <- partial_sum + matrixZ[i, j]
  }
  # Verhältnis berechnen (Teilsumme / Gesamtsumme)
  ratio <- ifelse(total_sum != 0, partial_sum / total_sum, 0)
  # Ergebnisse hinzufügen
  US_Shocks <- rbind(US_Shocks, data.frame(Variable = col_name, GesamtSumme = total_sum, TeilSumme = partial_sum, Verhältnis = ratio))
}
#Variable erstellen, die die Shocksize 10% enthält
US_Shocks$ten_percent <- US_Shocks$Verhältnis * 0.1
#Variable erstellen, die die Shocksize 20% enthält
US_Shocks$twenty_percent <- US_Shocks$Verhältnis * 0.2

#selbe Vorgehensweise für die EU
#leerer Datensatz für die Ergebnisse
EU27_Shocks <- data.frame(Variable = character(), GesamtSumme = numeric(), TeilSumme = numeric(), Verhältnis = numeric())
# Iteriere über die Spalten der EU
for (j in EU27) {
  # Spaltenname extrahieren
  col_name <- colnames(matrixZ)[j]
  # Gesamtsumme der Spalte berechnen
  total_sum <- sum(matrixZ[[j]], na.rm = TRUE)
  # Teilsumme der Zeilen 2817 bis 2880 in der aktuellen Spalte berechnen
  partial_sum <- 0
  for (i in US){
    partial_sum <- partial_sum + matrixZ[i, j]
  }
  # Verhältnis berechnen (Teilsumme / Gesamtsumme)
  ratio <- ifelse(total_sum != 0, partial_sum / total_sum, 0)
  # Ergebnisse hinzufügen
  EU27_Shocks <- rbind(EU27_Shocks, data.frame(Variable = col_name, GesamtSumme = total_sum, TeilSumme = partial_sum, Verhältnis = ratio))
}
#Variable erstellen, die die Shocksize 10% enthält
EU27_Shocks$ten_percent <- EU27_Shocks$Verhältnis * 0.1
#Variable erstellen, die die Shocksize 20% enthält
EU27_Shocks$twenty_percent <- EU27_Shocks$Verhältnis * 0.2




#Schockvektoren erstellen

# Szenario 1: Nur USA 10%
# Initialisiere einen Vektor mit 2944 Nullen
shocks_scenario1 <- numeric(2944)
# Setze Werte aus US_Shocks$ten_percent an den Positionen, die in EU angegeben sind
shocks_scenario1[US] <- US_Shocks$ten_percent

# Szenario 2: nur USA 20%
# Initialisiere einen Vektor mit 2944 Nullen
shocks_scenario2 <- numeric(2944)
shocks_scenario2[US] <- US_Shocks$twenty_percent

# Szenario 3: USA und EU 10%
# Initialisiere einen Vektor mit 2944 Nullen
shocks_scenario3 <- numeric(2944)
shocks_scenario3[US] <- US_Shocks$ten_percent
shocks_scenario3[EU27] <- EU27_Shocks$ten_percent

#Szenario 4: USA und EU 20%
# Initialisiere einen Vektor mit 2944 Nullen
shocks_scenario4 <- numeric(2944)
shocks_scenario4[US] <- US_Shocks$twenty_percent
shocks_scenario4[EU27] <- EU27_Shocks$twenty_percent

#Szenario 5: USA 20% und EU 10%
# Initialisiere einen Vektor mit 2944 Nullen
shocks_scenario5 <- numeric(2944)
shocks_scenario5[US] <- US_Shocks$twenty_percent
shocks_scenario5[EU27] <- EU27_Shocks$ten_percent




#Ergebnisse einfangen

#Dieser Codeabschnitt läuft sehr lange, da hier für jedes Scenario die Inversen berechnet werden
#am besten Szenario für Szenario laufen lassen

newresults <- data.frame(Sector = data_eu$rowLabels[1:2944])
#Szenario 1: (I-A')^(-1) * A_xe' * Schockvektor Szenario 1
newresults$scenario1 <- numeric(2944)

for(i in US){
  A_local <- solve(diag(nrow(matrixA[-i,-i])) - as.matrix(matrixA[-i,-i]))
  sectorresult <- A_local %*% as.matrix(matrixA[-i,i]) %*% shocks_scenario1[i]
  newresults$scenario1[1:(i-1)] <- newresults$scenario1[1:(i-1)] + sectorresult[1:(i-1)]
  newresults$scenario1[(i+1):2944] <- newresults$scenario1[(i+1):2944] + sectorresult[i:2943]
}

write.csv(newresults, "newresults_weber.csv", row.names = FALSE)

#Szenario 2: (I-A')^(-1) * A_xe' * Schockvektor Szenario 2
newresults$scenario2 <- numeric(2944)
for(i in US){
  A_local <- solve(diag(nrow(matrixA[-i,-i])) - as.matrix(matrixA[-i,-i]))
  sectorresult <- A_local %*% as.matrix(matrixA[-i,i]) %*% shocks_scenario2[i]
  newresults$scenario2[1:(i-1)] <- newresults$scenario2[1:(i-1)] + sectorresult[1:(i-1)]
  newresults$scenario2[(i+1):2944] <- newresults$scenario2[(i+1):2944] + sectorresult[i:2943]
}

#Szenario 3: (I-A')^(-1) * A_xe' * Schockvektor Szenario 3
#hier sind USA und EU betroffen, daher werden noch mehr 

USEU27 <- c(US,EU27)

newresults$scenario3 <- numeric(2944)
for(i in USEU27){
  A_local <- solve(diag(nrow(matrixA[-i,-i])) - as.matrix(matrixA[-i,-i]))
  sectorresult <- A_local %*% as.matrix(matrixA[-i,i]) %*% shocks_scenario3[i]
  newresults$scenario3[1:(i-1)] <- newresults$scenario3[1:(i-1)] + sectorresult[1:(i-1)]
  newresults$scenario3[(i+1):2944] <- newresults$scenario3[(i+1):2944] + sectorresult[i:2943]
}

#Szenario 4: (I-A')^(-1) * A_xe' * Schockvektor Szenario 4
newresults$scenario4 <- numeric(2944)
for(i in USEU27){
  A_local <- solve(diag(nrow(matrixA[-i,-i])) - as.matrix(matrixA[-i,-i]))
  sectorresult <- A_local %*% as.matrix(matrixA[-i,i]) %*% shocks_scenario4[i]
  newresults$scenario4[1:(i-1)] <- newresults$scenario4[1:(i-1)] + sectorresult[1:(i-1)]
  newresults$scenario4[(i+1):2944] <- newresults$scenario4[(i+1):2944] + sectorresult[i:2943]
}

#Szenario 5: (I-A')^(-1) * A_xe' * Schockvektor Szenario 5
newresults$scenario5 <- numeric(2944)
for(i in USEU27){
  A_local <- solve(diag(nrow(matrixA[-i,-i])) - as.matrix(matrixA[-i,-i]))
  sectorresult <- A_local %*% as.matrix(matrixA[-i,i]) %*% shocks_scenario5[i]
  newresults$scenario5[1:(i-1)] <- newresults$scenario5[1:(i-1)] + sectorresult[1:(i-1)]
  newresults$scenario5[(i+1):2944] <- newresults$scenario5[(i+1):2944] + sectorresult[i:2943]
}

results <- newresults



#consumer share berechnen

#Alle Final Demand Vektoren in CShare speichern
CShare<- data_eu[, grep("P3_S14", names(data_eu))]
#Summe über jede 
finaldemand_total <- apply(CShare, 2, sum)
CShare<- sweep(CShare, 2, finaldemand_total, "/")
CShare <- CShare[1:2944,]

#für Eu
CShare_EU <- CShare[, grepl(paste0("^(", paste(eu27_prefixes, collapse = "|"), ")"), names(CShare))]
total_EU <- sum(CShare_EU)
Cshare_EU <- sweep(CShare_EU, 2, total_EU, "/")
finaldemand_SectorShare_EU_dropped <- finaldemand_SectorShare_EU[1:2944,]
finaldemand_SectorShare_EU_US <- finaldemand_SectorShare_EU_dropped[-US,]
finaldemand_SectorShare_EU_USincluded <- finaldemand_SectorShare_EU_dropped[US,]

Gesamteffekte <- data.frame(Result = c("direct", "indirect", "total"))


#USA: direkter, indirekter und totaler Effekt je Szenario (in %)

#Szenario1
results$sc1_dir_us <- shocks_scenario1 * CShare$US_P3_S14 *100
results$sc1_ind_us <- results$scenario1 * CShare$US_P3_S14 *100
results$sc1_tot_us <- results$sc1_dir_us + results$sc1_ind_us
Gesamteffekte$US_sc1 <- c(sum(results$sc1_dir_us),sum(results$sc1_ind_us), sum(results$sc1_tot_us))

#Szenario2
results$sc2_dir_us <- shocks_scenario2 * CShare$US_P3_S14 *100
results$sc2_ind_us <- results$scenario2 * CShare$US_P3_S14 *100
results$sc2_tot_us <- results$sc2_dir_us + results$sc2_ind_us
Gesamteffekte$US_sc2 <- c(sum(results$sc2_dir_us),sum(results$sc2_ind_us), sum(results$sc2_tot_us))

#Szenario3
results$sc3_dir_us <- numeric(2944)
results$sc3_dir_us[US] <- shocks_scenario3[US] * CShare$US_P3_S14[US] *100
results$sc3_ind_us <- results$scenario3 * CShare$US_P3_S14 *100
results$sc3_tot_us <- results$sc3_dir_us + results$sc3_ind_us
Gesamteffekte$US_sc3 <- c(sum(results$sc3_dir_us),sum(results$sc3_ind_us), sum(results$sc3_tot_us))

#Szenario4
results$sc4_dir_us <- numeric(2944)
results$sc4_dir_us[US] <- shocks_scenario4[US] * CShare$US_P3_S14[US] *100
results$sc4_ind_us <- results$scenario4 * CShare$US_P3_S14 *100
results$sc4_tot_us <- results$sc4_dir_us + results$sc4_ind_us
Gesamteffekte$US_sc4 <- c(sum(results$sc4_dir_us),sum(results$sc4_ind_us), sum(results$sc4_tot_us))

#Szenario5
results$sc5_dir_us <- numeric(2944)
results$sc5_dir_us[US] <- shocks_scenario5[US] * CShare$US_P3_S14[US] *100
results$sc5_ind_us <- results$scenario5 * CShare$US_P3_S14 *100
results$sc5_tot_us <- results$sc5_dir_us + results$sc5_ind_us
Gesamteffekte$US_sc5 <- c(sum(results$sc5_dir_us),sum(results$sc5_ind_us), sum(results$sc5_tot_us))



#DE: direkter, indirekter und totaler Effekt je Szenario (in %)


#DE finden und Indizes speichern
find_var("DE", data = matrixZ)
DE <- 705:768
#Szenario1
results$sc1_dir_de <- numeric(2944)
results$sc1_ind_de <- results$scenario1 * CShare$DE_P3_S14 *100
results$sc1_tot_de <- results$sc1_dir_de + results$sc1_ind_de
Gesamteffekte$DE_sc1 <- c(sum(results$sc1_dir_de),sum(results$sc1_ind_de), sum(results$sc1_tot_de))

#Szenario2
results$sc2_dir_de <- numeric(2944)
results$sc2_ind_de <- results$scenario2 * CShare$DE_P3_S14 *100
results$sc2_tot_de <- results$sc2_dir_de + results$sc2_ind_de
Gesamteffekte$DE_sc2 <- c(sum(results$sc2_dir_de),sum(results$sc2_ind_de), sum(results$sc2_tot_de))

#Szenario3
results$sc3_dir_de <- numeric(2944)
results$sc3_dir_de[DE] <- shocks_scenario3[DE] * CShare$DE_P3_S14[DE] *100
results$sc3_ind_de <- results$scenario3 * CShare$DE_P3_S14 *100
results$sc3_tot_de <- results$sc3_dir_de + results$sc3_ind_de
Gesamteffekte$DE_sc3 <- c(sum(results$sc3_dir_de),sum(results$sc3_ind_de), sum(results$sc3_tot_de))

#Szenario4
results$sc4_dir_de <- numeric(2944)
results$sc4_dir_de[DE] <- shocks_scenario4[DE] * CShare$DE_P3_S14[DE] *100
results$sc4_ind_de <- results$scenario4 * CShare$DE_P3_S14 *100
results$sc4_tot_de <- results$sc4_dir_de + results$sc4_ind_de
Gesamteffekte$DE_sc4 <- c(sum(results$sc4_dir_de),sum(results$sc4_ind_de), sum(results$sc4_tot_de))

#Szenario5
results$sc5_dir_de <- numeric(2944)
results$sc5_dir_de[DE] <- shocks_scenario5[DE] * CShare$DE_P3_S14[DE] *100
results$sc5_ind_de <- results$scenario5 * CShare$DE_P3_S14 *100
results$sc5_tot_de <- results$sc5_dir_de + results$sc5_ind_de
Gesamteffekte$DE_sc5 <- c(sum(results$sc5_dir_de),sum(results$sc5_ind_de), sum(results$sc5_tot_de))


#FR: direkter, indirekter und totaler Effekt je Szenario (in %)


#FR finden und Indizes speichern
find_var("FR", data = matrixZ)
FR <- 1089:1152
#Szenario1
results$sc1_dir_fr <- numeric(2944)
results$sc1_ind_fr <- results$scenario1 * CShare$FR_P3_S14 *100
results$sc1_tot_fr <- results$sc1_dir_fr + results$sc1_ind_fr
Gesamteffekte$FR_sc1 <- c(sum(results$sc1_dir_fr),sum(results$sc1_ind_fr), sum(results$sc1_tot_fr))

#Szenario2
results$sc2_dir_fr <- numeric(2944)
results$sc2_ind_fr <- results$scenario2 * CShare$FR_P3_S14 *100
results$sc2_tot_fr <- results$sc2_dir_fr + results$sc2_ind_fr
Gesamteffekte$FR_sc2 <- c(sum(results$sc2_dir_fr),sum(results$sc2_ind_fr), sum(results$sc2_tot_fr))

#Szenario3
results$sc3_dir_fr <- numeric(2944)
results$sc3_dir_fr[FR] <- shocks_scenario3[FR] * CShare$FR_P3_S14[FR] *100
results$sc3_ind_fr <- results$scenario3 * CShare$FR_P3_S14 *100
results$sc3_tot_fr <- results$sc3_dir_fr + results$sc3_ind_fr
Gesamteffekte$FR_sc3 <- c(sum(results$sc3_dir_fr),sum(results$sc3_ind_fr), sum(results$sc3_tot_fr))

#Szenario4
results$sc4_dir_fr <- numeric(2944)
results$sc4_dir_fr[FR] <- shocks_scenario4[FR] * CShare$FR_P3_S14[FR] *100
results$sc4_ind_fr <- results$scenario4 * CShare$FR_P3_S14 *100
results$sc4_tot_fr <- results$sc4_dir_fr+ results$sc4_ind_fr
Gesamteffekte$FR_sc4 <- c(sum(results$sc4_dir_fr),sum(results$sc4_ind_fr), sum(results$sc4_tot_fr))

#Szenario5
results$sc5_dir_fr <- numeric(2944)
results$sc5_dir_fr[FR] <- shocks_scenario5[FR] * CShare$DE_P3_S14[FR] *100
results$sc5_ind_fr <- results$scenario5 * CShare$FR_P3_S14 *100
results$sc5_tot_fr <- results$sc5_dir_fr + results$sc5_ind_fr
Gesamteffekte$FR_sc5 <- c(sum(results$sc5_dir_fr),sum(results$sc5_ind_fr), sum(results$sc5_tot_fr))





#IE: direkter, indirekter und totaler Effekt je Szenario (in %)


#IE finden und Indizes speichern
find_var("IE", data = matrixZ)
IE <- 1473:1563
#Szenario1
results$sc1_dir_ie <- numeric(2944)
results$sc1_ind_ie <- results$scenario1 * CShare$IE_P3_S14 *100
results$sc1_tot_ie <- results$sc1_dir_ie + results$sc1_ind_ie
Gesamteffekte$IE_sc1 <- c(sum(results$sc1_dir_ie),sum(results$sc1_ind_ie), sum(results$sc1_tot_ie))

#Szenario2
results$sc2_dir_ie <- numeric(2944)
results$sc2_ind_ie <- results$scenario2 * CShare$IE_P3_S14 *100
results$sc2_tot_ie <- results$sc2_dir_ie + results$sc2_ind_ie
Gesamteffekte$IE_sc2 <- c(sum(results$sc2_dir_ie),sum(results$sc2_ind_ie), sum(results$sc2_tot_ie))


#Szenario3
results$sc3_dir_ie <- numeric(2944)
results$sc3_dir_ie[IE] <- shocks_scenario3[IE] * CShare$IE_P3_S14[IE] *100
results$sc3_ind_ie <- results$scenario3 * CShare$IE_P3_S14 *100
results$sc3_tot_ie <- results$sc3_dir_ie + results$sc3_ind_ie
Gesamteffekte$IE_sc3 <- c(sum(results$sc3_dir_ie),sum(results$sc3_ind_ie), sum(results$sc3_tot_ie))


#Szenario4
results$sc4_dir_ie <- numeric(2944)
results$sc4_dir_ie[IE] <- shocks_scenario4[IE] * CShare$IE_P3_S14[IE] *100
results$sc4_ind_ie <- results$scenario4 * CShare$IE_P3_S14 *100
results$sc4_tot_ie <- results$sc4_dir_ie + results$sc4_ind_ie
Gesamteffekte$IE_sc4 <- c(sum(results$sc4_dir_ie),sum(results$sc4_ind_ie), sum(results$sc4_tot_ie))


#Szenario5
results$sc5_dir_ie <- numeric(2944)
results$sc5_dir_ie[IE] <- shocks_scenario5[IE] * CShare$IE_P3_S14[IE] *100
results$sc5_ind_ie <- results$scenario5 * CShare$IE_P3_S14 *100
results$sc5_tot_ie <- results$sc5_dir_ie + results$sc5_ind_ie
Gesamteffekte$IE_sc5 <- c(sum(results$sc5_dir_ie),sum(results$sc5_ind_ie), sum(results$sc5_tot_ie))


#IE: direkter, indirekter und totaler Effekt je Szenario (in %)


#IE finden und Indizes speichern
find_var("ES", data = matrixZ)
ES <- 897:960
#Szenario1
results$sc1_dir_es <- numeric(2944)
results$sc1_ind_es <- results$scenario1 * CShare$ES_P3_S14 *100
results$sc1_tot_es <- results$sc1_dir_es + results$sc1_ind_es
Gesamteffekte$ES_sc1 <- c(sum(results$sc1_dir_es),sum(results$sc1_ind_es), sum(results$sc1_tot_es))

#Szenario2
results$sc2_dir_es <- numeric(2944)
results$sc2_ind_es <- results$scenario2 * CShare$ES_P3_S14 *100
results$sc2_tot_es <- results$sc2_dir_es + results$sc2_ind_es
Gesamteffekte$ES_sc2 <- c(sum(results$sc2_dir_es),sum(results$sc2_ind_es), sum(results$sc2_tot_es))


#Szenario3
results$sc3_dir_es <- numeric(2944)
results$sc3_dir_es[ES] <- shocks_scenario3[ES] * CShare$ES_P3_S14[ES] *100
results$sc3_ind_es <- results$scenario3 * CShare$ES_P3_S14 *100
results$sc3_tot_es <- results$sc3_dir_es + results$sc3_ind_es
Gesamteffekte$ES_sc3 <- c(sum(results$sc3_dir_es),sum(results$sc3_ind_es), sum(results$sc3_tot_es))


#Szenario4
results$sc4_dir_es <- numeric(2944)
results$sc4_dir_es[ES] <- shocks_scenario4[ES] * CShare$ES_P3_S14[ES] *100
results$sc4_ind_es <- results$scenario4 * CShare$ES_P3_S14 *100
results$sc4_tot_es <- results$sc4_dir_es + results$sc4_ind_es
Gesamteffekte$ES_sc4 <- c(sum(results$sc4_dir_es),sum(results$sc4_ind_es), sum(results$sc4_tot_es))


#Szenario5
results$sc5_dir_es <- numeric(2944)
results$sc5_dir_es[ES] <- shocks_scenario5[ES] * CShare$ES_P3_S14[ES] *100
results$sc5_ind_es <- results$scenario5 * CShare$ES_P3_S14 *100
results$sc5_tot_es <- results$sc5_dir_es + results$sc5_ind_es
Gesamteffekte$es_sc5 <- c(sum(results$sc5_dir_es),sum(results$sc5_ind_es), sum(results$sc5_tot_es))




#IT: direkter, indirekter und totaler Effekt je Szenario (in %)


#IT finden und Indizes speichern
find_var("IT", data = matrixZ)
IT <- 1601:1664
#Szenario1
results$sc1_dir_it <- numeric(2944)
results$sc1_ind_it <- results$scenario1 * CShare$IT_P3_S14 *100
results$sc1_tot_it <- results$sc1_dir_it + results$sc1_ind_it
Gesamteffekte$IT_sc1 <- c(sum(results$sc1_dir_it),sum(results$sc1_ind_it), sum(results$sc1_tot_it))

#Szenario2
results$sc2_dir_it <- numeric(2944)
results$sc2_ind_it <- results$scenario2 * CShare$IT_P3_S14 *100
results$sc2_tot_it <- results$sc2_dir_it + results$sc2_ind_it
Gesamteffekte$IT_sc2 <- c(sum(results$sc2_dir_it),sum(results$sc2_ind_it), sum(results$sc2_tot_it))


#Szenario3
results$sc3_dir_it <- numeric(2944)
results$sc3_dir_it[IT] <- shocks_scenario3[IT] * CShare$IT_P3_S14[IT] *100
results$sc3_ind_it <- results$scenario3 * CShare$IT_P3_S14 *100
results$sc3_tot_it <- results$sc3_dir_it + results$sc3_ind_it
Gesamteffekte$IT_sc3 <- c(sum(results$sc3_dir_it),sum(results$sc3_ind_it), sum(results$sc3_tot_it))


#Szenario4
results$sc4_dir_it <- numeric(2944)
results$sc4_dir_it[IT] <- shocks_scenario4[IT] * CShare$IT_P3_S14[IT] *100
results$sc4_ind_it <- results$scenario4 * CShare$IT_P3_S14 *100
results$sc4_tot_it <- results$sc4_dir_it + results$sc4_ind_it
Gesamteffekte$IT_sc4 <- c(sum(results$sc4_dir_it),sum(results$sc4_ind_it), sum(results$sc4_tot_it))


#Szenario5
results$sc5_dir_it <- numeric(2944)
results$sc5_dir_it[IT] <- shocks_scenario5[IT] * CShare$IT_P3_S14[IT] *100
results$sc5_ind_it <- results$scenario5 * CShare$IT_P3_S14 *100
results$sc5_tot_it <- results$sc5_dir_it + results$sc5_ind_it
Gesamteffekte$IT_sc5 <- c(sum(results$sc5_dir_it),sum(results$sc5_ind_it), sum(results$sc5_tot_it))


#AT: direkter, indirekter und totaler Effekt je Szenario (in %)


#AT finden und Indizes speichern
find_var("AT", data = matrixZ)
AT <- 65:128
#Szenario1
results$sc1_dir_at <- numeric(2944)
results$sc1_ind_at <- results$scenario1 * CShare$AT_P3_S14 *100
results$sc1_tot_at <- results$sc1_dir_at + results$sc1_ind_at
Gesamteffekte$AT_sc1 <- c(sum(results$sc1_dir_at),sum(results$sc1_ind_at), sum(results$sc1_tot_at))

#Szenario2
results$sc2_dir_at <- numeric(2944)
results$sc2_ind_at <- results$scenario2 * CShare$AT_P3_S14 *100
results$sc2_tot_at <- results$sc2_dir_at + results$sc2_ind_at
Gesamteffekte$AT_sc2 <- c(sum(results$sc2_dir_at),sum(results$sc2_ind_at), sum(results$sc2_tot_at))


#Szenario3
results$sc3_dir_at <- numeric(2944)
results$sc3_dir_at[AT] <- shocks_scenario3[AT] * CShare$AT_P3_S14[AT] *100
results$sc3_ind_at <- results$scenario3 * CShare$AT_P3_S14 *100
results$sc3_tot_at <- results$sc3_dir_at + results$sc3_ind_at
Gesamteffekte$AT_sc3 <- c(sum(results$sc3_dir_at),sum(results$sc3_ind_at), sum(results$sc3_tot_at))


#Szenario4
results$sc4_dir_at <- numeric(2944)
results$sc4_dir_at[AT] <- shocks_scenario4[AT] * CShare$AT_P3_S14[AT] *100
results$sc4_ind_at <- results$scenario4 * CShare$AT_P3_S14 *100
results$sc4_tot_at <- results$sc4_dir_at + results$sc4_ind_at
Gesamteffekte$AT_sc4 <- c(sum(results$sc4_dir_at),sum(results$sc4_ind_at), sum(results$sc4_tot_at))


#Szenario5
results$sc5_dir_at <- numeric(2944)
results$sc5_dir_at[AT] <- shocks_scenario5[AT] * CShare$AT_P3_S14[AT] *100
results$sc5_ind_at <- results$scenario5 * CShare$AT_P3_S14 *100
results$sc5_tot_at <- results$sc5_dir_at + results$sc5_ind_at
Gesamteffekte$AT_sc5 <- c(sum(results$sc5_dir_at),sum(results$sc5_ind_at), sum(results$sc5_tot_at))



#DK: direkter, indirekter und totaler Effekt je Szenario (in %)


#DK finden und Indizes speichern
find_var("DK", data = matrixZ)
DK <- 769:832
#Szenario1
results$sc1_dir_dk <- numeric(2944)
results$sc1_ind_dk <- results$scenario1 * CShare$DK_P3_S14 *100
results$sc1_tot_dk <- results$sc1_dir_dk + results$sc1_ind_dk
Gesamteffekte$DK_sc1 <- c(sum(results$sc1_dir_dk),sum(results$sc1_ind_dk), sum(results$sc1_tot_dk))

#Szenario2
results$sc2_dir_dk <- numeric(2944)
results$sc2_ind_dk <- results$scenario2 * CShare$DK_P3_S14 *100
results$sc2_tot_dk <- results$sc2_dir_dk + results$sc2_ind_dk
Gesamteffekte$DK_sc2 <- c(sum(results$sc2_dir_dk),sum(results$sc2_ind_dk), sum(results$sc2_tot_dk))


#Szenario3
results$sc3_dir_dk <- numeric(2944)
results$sc3_dir_dk[DK] <- shocks_scenario3[DK] * CShare$DK_P3_S14[DK] *100
results$sc3_ind_dk <- results$scenario3 * CShare$DK_P3_S14 *100
results$sc3_tot_dk <- results$sc3_dir_dk + results$sc3_ind_dk
Gesamteffekte$DK_sc3 <- c(sum(results$sc3_dir_dk),sum(results$sc3_ind_dk), sum(results$sc3_tot_dk))


#Szenario4
results$sc4_dir_dk <- numeric(2944)
results$sc4_dir_dk[DK] <- shocks_scenario4[DK] * CShare$DK_P3_S14[DK] *100
results$sc4_ind_dk <- results$scenario4 * CShare$DK_P3_S14 *100
results$sc4_tot_dk <- results$sc4_dir_dk + results$sc4_ind_dk
Gesamteffekte$DK_sc4 <- c(sum(results$sc4_dir_dk),sum(results$sc4_ind_dk), sum(results$sc4_tot_dk))


#Szenario5
results$sc5_dir_dk <- numeric(2944)
results$sc5_dir_dk[DK] <- shocks_scenario5[DK] * CShare$DK_P3_S14[DK] *100
results$sc5_ind_dk <- results$scenario5 * CShare$DK_P3_S14 *100
results$sc5_tot_dk <- results$sc5_dir_dk + results$sc5_ind_dk
Gesamteffekte$DK_sc5 <- c(sum(results$sc5_dir_dk),sum(results$sc5_ind_dk), sum(results$sc5_tot_dk))



#BE: direkter, indirekter und totaler Effekt je Szenario (in %)


#BE finden und Indizes speichern
find_var("BE", data = matrixZ)
BE <- 193:256
#Szenario1
results$sc1_dir_be <- numeric(2944)
results$sc1_ind_be <- results$scenario1 * CShare$BE_P3_S14 *100
results$sc1_tot_be <- results$sc1_dir_be + results$sc1_ind_be
Gesamteffekte$BE_sc1 <- c(sum(results$sc1_dir_be),sum(results$sc1_ind_be), sum(results$sc1_tot_be))

#Szenario2
results$sc2_dir_be <- numeric(2944)
results$sc2_ind_be <- results$scenario2 * CShare$BE_P3_S14 *100
results$sc2_tot_be <- results$sc2_dir_be + results$sc2_ind_be
Gesamteffekte$BE_sc2 <- c(sum(results$sc2_dir_be),sum(results$sc2_ind_be), sum(results$sc2_tot_be))


#Szenario3
results$sc3_dir_be <- numeric(2944)
results$sc3_dir_be[BE] <- shocks_scenario3[BE] * CShare$BE_P3_S14[BE] *100
results$sc3_ind_be <- results$scenario3 * CShare$BE_P3_S14 *100
results$sc3_tot_be <- results$sc3_dir_be + results$sc3_ind_be
Gesamteffekte$BE_sc3 <- c(sum(results$sc3_dir_be),sum(results$sc3_ind_be), sum(results$sc3_tot_be))


#Szenario4
results$sc4_dir_be <- numeric(2944)
results$sc4_dir_be[BE] <- shocks_scenario4[BE] * CShare$BE_P3_S14[BE] *100
results$sc4_ind_be <- results$scenario4 * CShare$BE_P3_S14 *100
results$sc4_tot_be <- results$sc4_dir_be + results$sc4_ind_be
Gesamteffekte$BE_sc4 <- c(sum(results$sc4_dir_be),sum(results$sc4_ind_be), sum(results$sc4_tot_be))


#Szenario5
results$sc5_dir_be <- numeric(2944)
results$sc5_dir_be[BE] <- shocks_scenario5[BE] * CShare$BE_P3_S14[BE] *100
results$sc5_ind_be <- results$scenario5 * CShare$BE_P3_S14 *100
results$sc5_tot_be <- results$sc5_dir_be + results$sc5_ind_be
Gesamteffekte$BE_sc5 <- c(sum(results$sc5_dir_be),sum(results$sc5_ind_be), sum(results$sc5_tot_be))



#BG: direkter, indirekter und totaler Effekt je Szenario (in %)


#BG finden und Indizes speichern
find_var("BG", data = matrixZ)
BG <- 257:320
#Szenario1
results$sc1_dir_bg <- numeric(2944)
results$sc1_ind_bg <- results$scenario1 * CShare$BG_P3_S14 *100
results$sc1_tot_bg <- results$sc1_dir_bg + results$sc1_ind_bg
Gesamteffekte$BG_sc1 <- c(sum(results$sc1_dir_bg),sum(results$sc1_ind_bg), sum(results$sc1_tot_bg))

#Szenario2
results$sc2_dir_bg <- numeric(2944)
results$sc2_ind_bg <- results$scenario2 * CShare$BG_P3_S14 *100
results$sc2_tot_bg <- results$sc2_dir_bg + results$sc2_ind_bg
Gesamteffekte$BG_sc2 <- c(sum(results$sc2_dir_bg),sum(results$sc2_ind_bg), sum(results$sc2_tot_bg))


#Szenario3
results$sc3_dir_bg <- numeric(2944)
results$sc3_dir_bg[BG] <- shocks_scenario3[BG] * CShare$BG_P3_S14[BG] *100
results$sc3_ind_bg <- results$scenario3 * CShare$BG_P3_S14 *100
results$sc3_tot_bg <- results$sc3_dir_bg + results$sc3_ind_bg
Gesamteffekte$BG_sc3 <- c(sum(results$sc3_dir_bg),sum(results$sc3_ind_bg), sum(results$sc3_tot_bg))


#Szenario4
results$sc4_dir_bg <- numeric(2944)
results$sc4_dir_bg[BG] <- shocks_scenario4[BG] * CShare$BG_P3_S14[BG] *100
results$sc4_ind_bg <- results$scenario4 * CShare$BG_P3_S14 *100
results$sc4_tot_bg <- results$sc4_dir_bg + results$sc4_ind_bg
Gesamteffekte$BG_sc4 <- c(sum(results$sc4_dir_bg),sum(results$sc4_ind_bg), sum(results$sc4_tot_bg))


#Szenario5
results$sc5_dir_bg <- numeric(2944)
results$sc5_dir_bg[BG] <- shocks_scenario5[BG] * CShare$BG_P3_S14[BG] *100
results$sc5_ind_bg <- results$scenario5 * CShare$BG_P3_S14 *100
results$sc5_tot_bg <- results$sc5_dir_bg + results$sc5_ind_bg
Gesamteffekte$BG_sc5 <- c(sum(results$sc5_dir_bg),sum(results$sc5_ind_bg), sum(results$sc5_tot_bg))



#CZ: direkter, indirekter und totaler Effekt je Szenario (in %)


#CZ finden und Indizes speichern
find_var("CZ", data = matrixZ)
CZ <- 641:704
#Szenario1
results$sc1_dir_cz <- numeric(2944)
results$sc1_ind_cz <- results$scenario1 * CShare$CZ_P3_S14 *100
results$sc1_tot_cz <- results$sc1_dir_cz + results$sc1_ind_cz
Gesamteffekte$CZ_sc1 <- c(sum(results$sc1_dir_cz),sum(results$sc1_ind_cz), sum(results$sc1_tot_cz))

#Szenario2
results$sc2_dir_cz <- numeric(2944)
results$sc2_ind_cz <- results$scenario2 * CShare$CZ_P3_S14 *100
results$sc2_tot_cz <- results$sc2_dir_cz + results$sc2_ind_cz
Gesamteffekte$CZ_sc2 <- c(sum(results$sc2_dir_cz),sum(results$sc2_ind_cz), sum(results$sc2_tot_cz))


#Szenario3
results$sc3_dir_cz <- numeric(2944)
results$sc3_dir_cz[CZ] <- shocks_scenario3[CZ] * CShare$CZ_P3_S14[CZ] *100
results$sc3_ind_cz <- results$scenario3 * CShare$CZ_P3_S14 *100
results$sc3_tot_cz <- results$sc3_dir_cz + results$sc3_ind_cz
Gesamteffekte$CZ_sc3 <- c(sum(results$sc3_dir_cz),sum(results$sc3_ind_cz), sum(results$sc3_tot_cz))


#Szenario4
results$sc4_dir_cz <- numeric(2944)
results$sc4_dir_cz[CZ] <- shocks_scenario4[CZ] * CShare$CZ_P3_S14[CZ] *100
results$sc4_ind_cz <- results$scenario4 * CShare$CZ_P3_S14 *100
results$sc4_tot_cz <- results$sc4_dir_cz + results$sc4_ind_cz
Gesamteffekte$CZ_sc4 <- c(sum(results$sc4_dir_cz),sum(results$sc4_ind_cz), sum(results$sc4_tot_cz))


#Szenario5
results$sc5_dir_cz <- numeric(2944)
results$sc5_dir_cz[CZ] <- shocks_scenario5[CZ] * CShare$CZ_P3_S14[CZ] *100
results$sc5_ind_cz <- results$scenario5 * CShare$CZ_P3_S14 *100
results$sc5_tot_cz <- results$sc5_dir_cz + results$sc5_ind_cz
Gesamteffekte$CZ_sc5 <- c(sum(results$sc5_dir_cz),sum(results$sc5_ind_cz), sum(results$sc5_tot_cz))



#EE: direkter, indirekter und totaler Effekt je Szenario (in %)


#EE finden und Indizes speichern
find_var("EE", data = matrixZ)
EE <- 833:896
#Szenario1
results$sc1_dir_ee <- numeric(2944)
results$sc1_ind_ee <- results$scenario1 * CShare$EE_P3_S14 *100
results$sc1_tot_ee <- results$sc1_dir_ee + results$sc1_ind_ee
Gesamteffekte$EE_sc1 <- c(sum(results$sc1_dir_ee),sum(results$sc1_ind_ee), sum(results$sc1_tot_ee))

#Szenario2
results$sc2_dir_ee <- numeric(2944)
results$sc2_ind_ee <- results$scenario2 * CShare$EE_P3_S14 *100
results$sc2_tot_ee <- results$sc2_dir_ee + results$sc2_ind_ee
Gesamteffekte$EE_sc2 <- c(sum(results$sc2_dir_ee),sum(results$sc2_ind_ee), sum(results$sc2_tot_ee))


#Szenario3
results$sc3_dir_ee <- numeric(2944)
results$sc3_dir_ee[EE] <- shocks_scenario3[EE] * CShare$EE_P3_S14[EE] *100
results$sc3_ind_ee <- results$scenario3 * CShare$EE_P3_S14 *100
results$sc3_tot_ee <- results$sc3_dir_ee + results$sc3_ind_ee
Gesamteffekte$EE_sc3 <- c(sum(results$sc3_dir_ee),sum(results$sc3_ind_ee), sum(results$sc3_tot_ee))


#Szenario4
results$sc4_dir_ee <- numeric(2944)
results$sc4_dir_ee[EE] <- shocks_scenario4[EE] * CShare$EE_P3_S14[EE] *100
results$sc4_ind_ee <- results$scenario4 * CShare$EE_P3_S14 *100
results$sc4_tot_ee <- results$sc4_dir_ee + results$sc4_ind_ee
Gesamteffekte$EE_sc4 <- c(sum(results$sc4_dir_ee),sum(results$sc4_ind_ee), sum(results$sc4_tot_ee))


#Szenario5
results$sc5_dir_ee <- numeric(2944)
results$sc5_dir_ee[EE] <- shocks_scenario5[EE] * CShare$EE_P3_S14[EE] *100
results$sc5_ind_ee <- results$scenario5 * CShare$EE_P3_S14 *100
results$sc5_tot_ee <- results$sc5_dir_ee + results$sc5_ind_ee
Gesamteffekte$EE_sc5 <- c(sum(results$sc5_dir_ee),sum(results$sc5_ind_ee), sum(results$sc5_tot_ee))


#GR: direkter, indirekter und totaler Effekt je Szenario (in %)


#GR finden und Indizes speichern
find_var("GR", data = matrixZ)
GR <- 1217:1280
#Szenario1
results$sc1_dir_gr <- numeric(2944)
results$sc1_ind_gr <- results$scenario1 * CShare$GR_P3_S14 *100
results$sc1_tot_gr <- results$sc1_dir_gr + results$sc1_ind_gr
Gesamteffekte$GR_sc1 <- c(sum(results$sc1_dir_gr),sum(results$sc1_ind_gr), sum(results$sc1_tot_gr))

#Szenario2
results$sc2_dir_gr <- numeric(2944)
results$sc2_ind_gr <- results$scenario2 * CShare$GR_P3_S14 *100
results$sc2_tot_gr <- results$sc2_dir_gr + results$sc2_ind_gr
Gesamteffekte$GR_sc2 <- c(sum(results$sc2_dir_gr),sum(results$sc2_ind_gr), sum(results$sc2_tot_gr))


#Szenario3
results$sc3_dir_gr <- numeric(2944)
results$sc3_dir_gr[GR] <- shocks_scenario3[GR] * CShare$GR_P3_S14[GR] *100
results$sc3_ind_gr <- results$scenario3 * CShare$GR_P3_S14 *100
results$sc3_tot_gr <- results$sc3_dir_gr + results$sc3_ind_gr
Gesamteffekte$GR_sc3 <- c(sum(results$sc3_dir_gr),sum(results$sc3_ind_gr), sum(results$sc3_tot_gr))


#Szenario4
results$sc4_dir_gr <- numeric(2944)
results$sc4_dir_gr[GR] <- shocks_scenario4[GR] * CShare$GR_P3_S14[GR] *100
results$sc4_ind_gr <- results$scenario4 * CShare$GR_P3_S14 *100
results$sc4_tot_gr <- results$sc4_dir_gr + results$sc4_ind_gr
Gesamteffekte$GR_sc4 <- c(sum(results$sc4_dir_gr),sum(results$sc4_ind_gr), sum(results$sc4_tot_gr))


#Szenario5
results$sc5_dir_gr <- numeric(2944)
results$sc5_dir_gr[GR] <- shocks_scenario5[GR] * CShare$GR_P3_S14[GR] *100
results$sc5_ind_gr <- results$scenario5 * CShare$GR_P3_S14 *100
results$sc5_tot_gr <- results$sc5_dir_gr + results$sc5_ind_gr
Gesamteffekte$GR_sc5 <- c(sum(results$sc5_dir_gr),sum(results$sc5_ind_gr), sum(results$sc5_tot_gr))


#HR: direkter, indirekter und totaler Effekt je Szenario (in %)


#HR finden und Indizes speichern
find_var("HR", data = matrixZ)
HR <- 1281:1344
#Szenario1
results$sc1_dir_hr <- numeric(2944)
results$sc1_ind_hr <- results$scenario1 * CShare$HR_P3_S14 *100
results$sc1_tot_hr <- results$sc1_dir_hr + results$sc1_ind_hr
Gesamteffekte$HR_sc1 <- c(sum(results$sc1_dir_hr),sum(results$sc1_ind_hr), sum(results$sc1_tot_hr))

#Szenario2
results$sc2_dir_hr <- numeric(2944)
results$sc2_ind_hr <- results$scenario2 * CShare$HR_P3_S14 *100
results$sc2_tot_hr <- results$sc2_dir_hr + results$sc2_ind_hr
Gesamteffekte$HR_sc2 <- c(sum(results$sc2_dir_hr),sum(results$sc2_ind_hr), sum(results$sc2_tot_hr))


#Szenario3
results$sc3_dir_hr <- numeric(2944)
results$sc3_dir_hr[HR] <- shocks_scenario3[HR] * CShare$HR_P3_S14[HR] *100
results$sc3_ind_hr <- results$scenario3 * CShare$HR_P3_S14 *100
results$sc3_tot_hr <- results$sc3_dir_hr + results$sc3_ind_hr
Gesamteffekte$HR_sc3 <- c(sum(results$sc3_dir_hr),sum(results$sc3_ind_hr), sum(results$sc3_tot_hr))


#Szenario4
results$sc4_dir_hr <- numeric(2944)
results$sc4_dir_hr[HR] <- shocks_scenario4[HR] * CShare$HR_P3_S14[HR] *100
results$sc4_ind_hr <- results$scenario4 * CShare$HR_P3_S14 *100
results$sc4_tot_hr <- results$sc4_dir_hr + results$sc4_ind_hr
Gesamteffekte$HR_sc4 <- c(sum(results$sc4_dir_hr),sum(results$sc4_ind_hr), sum(results$sc4_tot_hr))


#Szenario5
results$sc5_dir_hr <- numeric(2944)
results$sc5_dir_hr[HR] <- shocks_scenario5[HR] * CShare$HR_P3_S14[HR] *100
results$sc5_ind_hr <- results$scenario5 * CShare$HR_P3_S14 *100
results$sc5_tot_hr <- results$sc5_dir_hr + results$sc5_ind_hr
Gesamteffekte$HR_sc5 <- c(sum(results$sc5_dir_hr),sum(results$sc5_ind_hr), sum(results$sc5_tot_hr))


#CY: direkter, indirekter und totaler Effekt je Szenario (in %)


#CY finden und Indizes speichern
find_var("CY", data = matrixZ)
CY <- 577:640
#Szenario1
results$sc1_dir_cy <- numeric(2944)
results$sc1_ind_cy <- results$scenario1 * CShare$CY_P3_S14 *100
results$sc1_tot_cy <- results$sc1_dir_cy + results$sc1_ind_cy
Gesamteffekte$CY_sc1 <- c(sum(results$sc1_dir_cy),sum(results$sc1_ind_cy), sum(results$sc1_tot_cy))

#Szenario2
results$sc2_dir_cy <- numeric(2944)
results$sc2_ind_cy <- results$scenario2 * CShare$CY_P3_S14 *100
results$sc2_tot_cy <- results$sc2_dir_cy + results$sc2_ind_cy
Gesamteffekte$CY_sc2 <- c(sum(results$sc2_dir_cy),sum(results$sc2_ind_cy), sum(results$sc2_tot_cy))


#Szenario3
results$sc3_dir_cy <- numeric(2944)
results$sc3_dir_cy[CY] <- shocks_scenario3[CY] * CShare$CY_P3_S14[CY] *100
results$sc3_ind_cy <- results$scenario3 * CShare$CY_P3_S14 *100
results$sc3_tot_cy <- results$sc3_dir_cy + results$sc3_ind_cy
Gesamteffekte$CY_sc3 <- c(sum(results$sc3_dir_cy),sum(results$sc3_ind_cy), sum(results$sc3_tot_cy))


#Szenario4
results$sc4_dir_cy <- numeric(2944)
results$sc4_dir_cy[CY] <- shocks_scenario4[CY] * CShare$CY_P3_S14[CY] *100
results$sc4_ind_cy <- results$scenario4 * CShare$CY_P3_S14 *100
results$sc4_tot_cy <- results$sc4_dir_cy + results$sc4_ind_cy
Gesamteffekte$CY_sc4 <- c(sum(results$sc4_dir_cy),sum(results$sc4_ind_cy), sum(results$sc4_tot_cy))


#Szenario5
results$sc5_dir_cy <- numeric(2944)
results$sc5_dir_cy[CY] <- shocks_scenario5[CY] * CShare$CY_P3_S14[CY] *100
results$sc5_ind_cy <- results$scenario5 * CShare$CY_P3_S14 *100
results$sc5_tot_cy <- results$sc5_dir_cy + results$sc5_ind_cy
Gesamteffekte$CY_sc5 <- c(sum(results$sc5_dir_cy),sum(results$sc5_ind_cy), sum(results$sc5_tot_cy))



#LV: direkter, indirekter und totaler Effekt je Szenario (in %)


#LV finden und Indizes speichern
find_var("LV", data = matrixZ)
LV <- 1921:1984
#Szenario1
results$sc1_dir_lv <- numeric(2944)
results$sc1_ind_lv <- results$scenario1 * CShare$LV_P3_S14 *100
results$sc1_tot_lv <- results$sc1_dir_lv + results$sc1_ind_lv
Gesamteffekte$LV_sc1 <- c(sum(results$sc1_dir_lv),sum(results$sc1_ind_lv), sum(results$sc1_tot_lv))

#Szenario2
results$sc2_dir_lv <- numeric(2944)
results$sc2_ind_lv <- results$scenario2 * CShare$LV_P3_S14 *100
results$sc2_tot_lv <- results$sc2_dir_lv + results$sc2_ind_lv
Gesamteffekte$LV_sc2 <- c(sum(results$sc2_dir_lv),sum(results$sc2_ind_lv), sum(results$sc2_tot_lv))


#Szenario3
results$sc3_dir_lv <- numeric(2944)
results$sc3_dir_lv[LV] <- shocks_scenario3[LV] * CShare$LV_P3_S14[LV] *100
results$sc3_ind_lv <- results$scenario3 * CShare$LV_P3_S14 *100
results$sc3_tot_lv <- results$sc3_dir_lv + results$sc3_ind_lv
Gesamteffekte$LV_sc3 <- c(sum(results$sc3_dir_lv),sum(results$sc3_ind_lv), sum(results$sc3_tot_lv))


#Szenario4
results$sc4_dir_lv <- numeric(2944)
results$sc4_dir_lv[LV] <- shocks_scenario4[LV] * CShare$LV_P3_S14[LV] *100
results$sc4_ind_lv <- results$scenario4 * CShare$LV_P3_S14 *100
results$sc4_tot_lv <- results$sc4_dir_lv + results$sc4_ind_lv
Gesamteffekte$LV_sc4 <- c(sum(results$sc4_dir_lv),sum(results$sc4_ind_lv), sum(results$sc4_tot_lv))


#Szenario5
results$sc5_dir_lv <- numeric(2944)
results$sc5_dir_lv[LV] <- shocks_scenario5[LV] * CShare$LV_P3_S14[LV] *100
results$sc5_ind_lv <- results$scenario5 * CShare$LV_P3_S14 *100
results$sc5_tot_lv <- results$sc5_dir_lv + results$sc5_ind_lv
Gesamteffekte$LV_sc5 <- c(sum(results$sc5_dir_lv),sum(results$sc5_ind_lv), sum(results$sc5_tot_lv))


#LT: direkter, indirekter und totaler Effekt je Szenario (in %)


#LT finden und Indizes speichern
find_var("LT", data = matrixZ)
LT <- 1793:1856
#Szenario1
results$sc1_dir_lt <- numeric(2944)
results$sc1_ind_lt <- results$scenario1 * CShare$LT_P3_S14 *100
results$sc1_tot_lt <- results$sc1_dir_lt + results$sc1_ind_lt
Gesamteffekte$LT_sc1 <- c(sum(results$sc1_dir_lt),sum(results$sc1_ind_lt), sum(results$sc1_tot_lt))

#Szenario2
results$sc2_dir_lt <- numeric(2944)
results$sc2_ind_lt <- results$scenario2 * CShare$LT_P3_S14 *100
results$sc2_tot_lt <- results$sc2_dir_lt + results$sc2_ind_lt
Gesamteffekte$LT_sc2 <- c(sum(results$sc2_dir_lt),sum(results$sc2_ind_lt), sum(results$sc2_tot_lt))


#Szenario3
results$sc3_dir_lt <- numeric(2944)
results$sc3_dir_lt[LT] <- shocks_scenario3[LT] * CShare$LT_P3_S14[LT] *100
results$sc3_ind_lt <- results$scenario3 * CShare$LT_P3_S14 *100
results$sc3_tot_lt <- results$sc3_dir_lt + results$sc3_ind_lt
Gesamteffekte$LT_sc3 <- c(sum(results$sc3_dir_lt),sum(results$sc3_ind_lt), sum(results$sc3_tot_lt))


#Szenario4
results$sc4_dir_lt <- numeric(2944)
results$sc4_dir_lt[LT] <- shocks_scenario4[LT] * CShare$LT_P3_S14[LT] *100
results$sc4_ind_lt <- results$scenario4 * CShare$LT_P3_S14 *100
results$sc4_tot_lt <- results$sc4_dir_lt + results$sc4_ind_lt
Gesamteffekte$LT_sc4 <- c(sum(results$sc4_dir_lt),sum(results$sc4_ind_lt), sum(results$sc4_tot_lt))


#Szenario5
results$sc5_dir_lt <- numeric(2944)
results$sc5_dir_lt[LT] <- shocks_scenario5[LT] * CShare$LT_P3_S14[LT] *100
results$sc5_ind_lt <- results$scenario5 * CShare$LT_P3_S14 *100
results$sc5_tot_lt <- results$sc5_dir_lt + results$sc5_ind_lt
Gesamteffekte$LT_sc5 <- c(sum(results$sc5_dir_lt),sum(results$sc5_ind_lt), sum(results$sc5_tot_lt))


#LU: direkter, indirekter und totaler Effekt je Szenario (in %)

#LU finden und Indizes speichern
find_var("LU", data = matrixZ)
LU <- 1857:1920

#Szenario1
results$sc1_dir_lu <- numeric(2944)
results$sc1_ind_lu <- results$scenario1 * CShare$LU_P3_S14 * 100
results$sc1_tot_lu <- results$sc1_dir_lu + results$sc1_ind_lu
Gesamteffekte$LU_sc1 <- c(sum(results$sc1_dir_lu), sum(results$sc1_ind_lu), sum(results$sc1_tot_lu))

#Szenario2
results$sc2_dir_lu <- numeric(2944)
results$sc2_ind_lu <- results$scenario2 * CShare$LU_P3_S14 * 100
results$sc2_tot_lu <- results$sc2_dir_lu + results$sc2_ind_lu
Gesamteffekte$LU_sc2 <- c(sum(results$sc2_dir_lu), sum(results$sc2_ind_lu), sum(results$sc2_tot_lu))

#Szenario3
results$sc3_dir_lu <- numeric(2944)
results$sc3_dir_lu[LU] <- shocks_scenario3[LU] * CShare$LU_P3_S14[LU] * 100
results$sc3_ind_lu <- results$scenario3 * CShare$LU_P3_S14 * 100
results$sc3_tot_lu <- results$sc3_dir_lu + results$sc3_ind_lu
Gesamteffekte$LU_sc3 <- c(sum(results$sc3_dir_lu), sum(results$sc3_ind_lu), sum(results$sc3_tot_lu))

#Szenario4
results$sc4_dir_lu <- numeric(2944)
results$sc4_dir_lu[LU] <- shocks_scenario4[LU] * CShare$LU_P3_S14[LU] * 100
results$sc4_ind_lu <- results$scenario4 * CShare$LU_P3_S14 * 100
results$sc4_tot_lu <- results$sc4_dir_lu + results$sc4_ind_lu
Gesamteffekte$LU_sc4 <- c(sum(results$sc4_dir_lu), sum(results$sc4_ind_lu), sum(results$sc4_tot_lu))

#Szenario5
results$sc5_dir_lu <- numeric(2944)
results$sc5_dir_lu[LU] <- shocks_scenario5[LU] * CShare$LU_P3_S14[LU] * 100
results$sc5_ind_lu <- results$scenario5 * CShare$LU_P3_S14 * 100
results$sc5_tot_lu <- results$sc5_dir_lu + results$sc5_ind_lu
Gesamteffekte$LU_sc5 <- c(sum(results$sc5_dir_lu), sum(results$sc5_ind_lu), sum(results$sc5_tot_lu))


#HU: direkter, indirekter und totaler Effekt je Szenario (in %)

#HU finden und Indizes speichern
find_var("HU", data = matrixZ)
HU <- 1345:1408

#Szenario1
results$sc1_dir_hu <- numeric(2944)
results$sc1_ind_hu <- results$scenario1 * CShare$HU_P3_S14 * 100
results$sc1_tot_hu <- results$sc1_dir_hu + results$sc1_ind_hu
Gesamteffekte$HU_sc1 <- c(sum(results$sc1_dir_hu), sum(results$sc1_ind_hu), sum(results$sc1_tot_hu))

#Szenario2
results$sc2_dir_hu <- numeric(2944)
results$sc2_ind_hu <- results$scenario2 * CShare$HU_P3_S14 * 100
results$sc2_tot_hu <- results$sc2_dir_hu + results$sc2_ind_hu
Gesamteffekte$HU_sc2 <- c(sum(results$sc2_dir_hu), sum(results$sc2_ind_hu), sum(results$sc2_tot_hu))

#Szenario3
results$sc3_dir_hu <- numeric(2944)
results$sc3_dir_hu[HU] <- shocks_scenario3[HU] * CShare$HU_P3_S14[HU] * 100
results$sc3_ind_hu <- results$scenario3 * CShare$HU_P3_S14 * 100
results$sc3_tot_hu <- results$sc3_dir_hu + results$sc3_ind_hu
Gesamteffekte$HU_sc3 <- c(sum(results$sc3_dir_hu), sum(results$sc3_ind_hu), sum(results$sc3_tot_hu))

#Szenario4
results$sc4_dir_hu <- numeric(2944)
results$sc4_dir_hu[HU] <- shocks_scenario4[HU] * CShare$HU_P3_S14[HU] * 100
results$sc4_ind_hu <- results$scenario4 * CShare$HU_P3_S14 * 100
results$sc4_tot_hu <- results$sc4_dir_hu + results$sc4_ind_hu
Gesamteffekte$HU_sc4 <- c(sum(results$sc4_dir_hu), sum(results$sc4_ind_hu), sum(results$sc4_tot_hu))

#Szenario5
results$sc5_dir_hu <- numeric(2944)
results$sc5_dir_hu[HU] <- shocks_scenario5[HU] * CShare$HU_P3_S14[HU] * 100
results$sc5_ind_hu <- results$scenario5 * CShare$HU_P3_S14 * 100
results$sc5_tot_hu <- results$sc5_dir_hu + results$sc5_ind_hu
Gesamteffekte$HU_sc5 <- c(sum(results$sc5_dir_hu), sum(results$sc5_ind_hu), sum(results$sc5_tot_hu))


#MT: direkter, indirekter und totaler Effekt je Szenario (in %)

#MT finden und Indizes speichern
find_var("MT", data = matrixZ)
MT <- 1985:2048

#Szenario1
results$sc1_dir_mt <- numeric(2944)
results$sc1_ind_mt <- results$scenario1 * CShare$MT_P3_S14 * 100
results$sc1_tot_mt <- results$sc1_dir_mt + results$sc1_ind_mt
Gesamteffekte$MT_sc1 <- c(sum(results$sc1_dir_mt), sum(results$sc1_ind_mt), sum(results$sc1_tot_mt))

#Szenario2
results$sc2_dir_mt <- numeric(2944)
results$sc2_ind_mt <- results$scenario2 * CShare$MT_P3_S14 * 100
results$sc2_tot_mt <- results$sc2_dir_mt + results$sc2_ind_mt
Gesamteffekte$MT_sc2 <- c(sum(results$sc2_dir_mt), sum(results$sc2_ind_mt), sum(results$sc2_tot_mt))

#Szenario3
results$sc3_dir_mt <- numeric(2944)
results$sc3_dir_mt[MT] <- shocks_scenario3[MT] * CShare$MT_P3_S14[MT] * 100
results$sc3_ind_mt <- results$scenario3 * CShare$MT_P3_S14 * 100
results$sc3_tot_mt <- results$sc3_dir_mt + results$sc3_ind_mt
Gesamteffekte$MT_sc3 <- c(sum(results$sc3_dir_mt), sum(results$sc3_ind_mt), sum(results$sc3_tot_mt))

#Szenario4
results$sc4_dir_mt <- numeric(2944)
results$sc4_dir_mt[MT] <- shocks_scenario4[MT] * CShare$MT_P3_S14[MT] * 100
results$sc4_ind_mt <- results$scenario4 * CShare$MT_P3_S14 * 100
results$sc4_tot_mt <- results$sc4_dir_mt + results$sc4_ind_mt
Gesamteffekte$MT_sc4 <- c(sum(results$sc4_dir_mt), sum(results$sc4_ind_mt), sum(results$sc4_tot_mt))

#Szenario5
results$sc5_dir_mt <- numeric(2944)
results$sc5_dir_mt[MT] <- shocks_scenario5[MT] * CShare$MT_P3_S14[MT] * 100
results$sc5_ind_mt <- results$scenario5 * CShare$MT_P3_S14 * 100
results$sc5_tot_mt <- results$sc5_dir_mt + results$sc5_ind_mt
Gesamteffekte$MT_sc5 <- c(sum(results$sc5_dir_mt), sum(results$sc5_ind_mt), sum(results$sc5_tot_mt))



#NL: direkter, indirekter und totaler Effekt je Szenario (in %)

#NL finden und Indizes speichern
find_var("NL", data = matrixZ)
NL <- 2113:2176

#Szenario1
results$sc1_dir_nl <- numeric(2944)
results$sc1_ind_nl <- results$scenario1 * CShare$NL_P3_S14 * 100
results$sc1_tot_nl <- results$sc1_dir_nl + results$sc1_ind_nl
Gesamteffekte$NL_sc1 <- c(sum(results$sc1_dir_nl), sum(results$sc1_ind_nl), sum(results$sc1_tot_nl))

#Szenario2
results$sc2_dir_nl <- numeric(2944)
results$sc2_ind_nl <- results$scenario2 * CShare$NL_P3_S14 * 100
results$sc2_tot_nl <- results$sc2_dir_nl + results$sc2_ind_nl
Gesamteffekte$NL_sc2 <- c(sum(results$sc2_dir_nl), sum(results$sc2_ind_nl), sum(results$sc2_tot_nl))

#Szenario3
results$sc3_dir_nl <- numeric(2944)
results$sc3_dir_nl[NL] <- shocks_scenario3[NL] * CShare$NL_P3_S14[NL] * 100
results$sc3_ind_nl <- results$scenario3 * CShare$NL_P3_S14 * 100
results$sc3_tot_nl <- results$sc3_dir_nl + results$sc3_ind_nl
Gesamteffekte$NL_sc3 <- c(sum(results$sc3_dir_nl), sum(results$sc3_ind_nl), sum(results$sc3_tot_nl))

#Szenario4
results$sc4_dir_nl <- numeric(2944)
results$sc4_dir_nl[NL] <- shocks_scenario4[NL] * CShare$NL_P3_S14[NL] * 100
results$sc4_ind_nl <- results$scenario4 * CShare$NL_P3_S14 * 100
results$sc4_tot_nl <- results$sc4_dir_nl + results$sc4_ind_nl
Gesamteffekte$NL_sc4 <- c(sum(results$sc4_dir_nl), sum(results$sc4_ind_nl), sum(results$sc4_tot_nl))

#Szenario5
results$sc5_dir_nl <- numeric(2944)
results$sc5_dir_nl[NL] <- shocks_scenario5[NL] * CShare$NL_P3_S14[NL] * 100
results$sc5_ind_nl <- results$scenario5 * CShare$NL_P3_S14 * 100
results$sc5_tot_nl <- results$sc5_dir_nl + results$sc5_ind_nl
Gesamteffekte$NL_sc5 <- c(sum(results$sc5_dir_nl), sum(results$sc5_ind_nl), sum(results$sc5_tot_nl))


#PL: direkter, indirekter und totaler Effekt je Szenario (in %)

#PL finden und Indizes speichern
find_var("PL", data = matrixZ)
PL <- 2241:2304

#Szenario1
results$sc1_dir_pl <- numeric(2944)
results$sc1_ind_pl <- results$scenario1 * CShare$PL_P3_S14 * 100
results$sc1_tot_pl <- results$sc1_dir_pl + results$sc1_ind_pl
Gesamteffekte$PL_sc1 <- c(sum(results$sc1_dir_pl), sum(results$sc1_ind_pl), sum(results$sc1_tot_pl))

#Szenario2
results$sc2_dir_pl <- numeric(2944)
results$sc2_ind_pl <- results$scenario2 * CShare$PL_P3_S14 * 100
results$sc2_tot_pl <- results$sc2_dir_pl + results$sc2_ind_pl
Gesamteffekte$PL_sc2 <- c(sum(results$sc2_dir_pl), sum(results$sc2_ind_pl), sum(results$sc2_tot_pl))

#Szenario3
results$sc3_dir_pl <- numeric(2944)
results$sc3_dir_pl[PL] <- shocks_scenario3[PL] * CShare$PL_P3_S14[PL] * 100
results$sc3_ind_pl <- results$scenario3 * CShare$PL_P3_S14 * 100
results$sc3_tot_pl <- results$sc3_dir_pl + results$sc3_ind_pl
Gesamteffekte$PL_sc3 <- c(sum(results$sc3_dir_pl), sum(results$sc3_ind_pl), sum(results$sc3_tot_pl))

#Szenario4
results$sc4_dir_pl <- numeric(2944)
results$sc4_dir_pl[PL] <- shocks_scenario4[PL] * CShare$PL_P3_S14[PL] * 100
results$sc4_ind_pl <- results$scenario4 * CShare$PL_P3_S14 * 100
results$sc4_tot_pl <- results$sc4_dir_pl + results$sc4_ind_pl
Gesamteffekte$PL_sc4 <- c(sum(results$sc4_dir_pl), sum(results$sc4_ind_pl), sum(results$sc4_tot_pl))

#Szenario5
results$sc5_dir_pl <- numeric(2944)
results$sc5_dir_pl[PL] <- shocks_scenario5[PL] * CShare$PL_P3_S14[PL] * 100
results$sc5_ind_pl <- results$scenario5 * CShare$PL_P3_S14 * 100
results$sc5_tot_pl <- results$sc5_dir_pl + results$sc5_ind_pl
Gesamteffekte$PL_sc5 <- c(sum(results$sc5_dir_pl), sum(results$sc5_ind_pl), sum(results$sc5_tot_pl))



#PT: direkter, indirekter und totaler Effekt je Szenario (in %)

#PT finden und Indizes speichern
find_var("PT", data = matrixZ)
PT <- 2305:2368

#Szenario1
results$sc1_dir_pt <- numeric(2944)
results$sc1_ind_pt <- results$scenario1 * CShare$PT_P3_S14 * 100
results$sc1_tot_pt <- results$sc1_dir_pt + results$sc1_ind_pt
Gesamteffekte$PT_sc1 <- c(sum(results$sc1_dir_pt), sum(results$sc1_ind_pt), sum(results$sc1_tot_pt))

#Szenario2
results$sc2_dir_pt <- numeric(2944)
results$sc2_ind_pt <- results$scenario2 * CShare$PT_P3_S14 * 100
results$sc2_tot_pt <- results$sc2_dir_pt + results$sc2_ind_pt
Gesamteffekte$PT_sc2 <- c(sum(results$sc2_dir_pt), sum(results$sc2_ind_pt), sum(results$sc2_tot_pt))

#Szenario3
results$sc3_dir_pt <- numeric(2944)
results$sc3_dir_pt[PT] <- shocks_scenario3[PT] * CShare$PT_P3_S14[PT] * 100
results$sc3_ind_pt <- results$scenario3 * CShare$PT_P3_S14 * 100
results$sc3_tot_pt <- results$sc3_dir_pt + results$sc3_ind_pt
Gesamteffekte$PT_sc3 <- c(sum(results$sc3_dir_pt), sum(results$sc3_ind_pt), sum(results$sc3_tot_pt))

#Szenario4
results$sc4_dir_pt <- numeric(2944)
results$sc4_dir_pt[PT] <- shocks_scenario4[PT] * CShare$PT_P3_S14[PT] * 100
results$sc4_ind_pt <- results$scenario4 * CShare$PT_P3_S14 * 100
results$sc4_tot_pt <- results$sc4_dir_pt + results$sc4_ind_pt
Gesamteffekte$PT_sc4 <- c(sum(results$sc4_dir_pt), sum(results$sc4_ind_pt), sum(results$sc4_tot_pt))

#Szenario5
results$sc5_dir_pt <- numeric(2944)
results$sc5_dir_pt[PT] <- shocks_scenario5[PT] * CShare$PT_P3_S14[PT] * 100
results$sc5_ind_pt <- results$scenario5 * CShare$PT_P3_S14 * 100
results$sc5_tot_pt <- results$sc5_dir_pt + results$sc5_ind_pt
Gesamteffekte$PT_sc5 <- c(sum(results$sc5_dir_pt), sum(results$sc5_ind_pt), sum(results$sc5_tot_pt))


#RO: direkter, indirekter und totaler Effekt je Szenario (in %)

#RO finden und Indizes speichern
find_var("RO", data = matrixZ)
RO <- 2369:2432

#Szenario1
results$sc1_dir_ro <- numeric(2944)
results$sc1_ind_ro <- results$scenario1 * CShare$RO_P3_S14 * 100
results$sc1_tot_ro <- results$sc1_dir_ro + results$sc1_ind_ro
Gesamteffekte$RO_sc1 <- c(sum(results$sc1_dir_ro), sum(results$sc1_ind_ro), sum(results$sc1_tot_ro))

#Szenario2
results$sc2_dir_ro <- numeric(2944)
results$sc2_ind_ro <- results$scenario2 * CShare$RO_P3_S14 * 100
results$sc2_tot_ro <- results$sc2_dir_ro + results$sc2_ind_ro
Gesamteffekte$RO_sc2 <- c(sum(results$sc2_dir_ro), sum(results$sc2_ind_ro), sum(results$sc2_tot_ro))

#Szenario3
results$sc3_dir_ro <- numeric(2944)
results$sc3_dir_ro[RO] <- shocks_scenario3[RO] * CShare$RO_P3_S14[RO] * 100
results$sc3_ind_ro <- results$scenario3 * CShare$RO_P3_S14 * 100
results$sc3_tot_ro <- results$sc3_dir_ro + results$sc3_ind_ro
Gesamteffekte$RO_sc3 <- c(sum(results$sc3_dir_ro), sum(results$sc3_ind_ro), sum(results$sc3_tot_ro))

#Szenario4
results$sc4_dir_ro <- numeric(2944)
results$sc4_dir_ro[RO] <- shocks_scenario4[RO] * CShare$RO_P3_S14[RO] * 100
results$sc4_ind_ro <- results$scenario4 * CShare$RO_P3_S14 * 100
results$sc4_tot_ro <- results$sc4_dir_ro + results$sc4_ind_ro
Gesamteffekte$RO_sc4 <- c(sum(results$sc4_dir_ro), sum(results$sc4_ind_ro), sum(results$sc4_tot_ro))

#Szenario5
results$sc5_dir_ro <- numeric(2944)
results$sc5_dir_ro[RO] <- shocks_scenario5[RO] * CShare$RO_P3_S14[RO] * 100
results$sc5_ind_ro <- results$scenario5 * CShare$RO_P3_S14 * 100
results$sc5_tot_ro <- results$sc5_dir_ro + results$sc5_ind_ro
Gesamteffekte$RO_sc5 <- c(sum(results$sc5_dir_ro), sum(results$sc5_ind_ro), sum(results$sc5_tot_ro))


#SI: direkter, indirekter und totaler Effekt je Szenario (in %)

#SI finden und Indizes speichern
find_var("SI", data = matrixZ)
SI <- 1625:2688

#Szenario1
results$sc1_dir_si <- numeric(2944)
results$sc1_ind_si <- results$scenario1 * CShare$SI_P3_S14 * 100
results$sc1_tot_si <- results$sc1_dir_si + results$sc1_ind_si
Gesamteffekte$SI_sc1 <- c(sum(results$sc1_dir_si), sum(results$sc1_ind_si), sum(results$sc1_tot_si))

#Szenario2
results$sc2_dir_si <- numeric(2944)
results$sc2_ind_si <- results$scenario2 * CShare$SI_P3_S14 * 100
results$sc2_tot_si <- results$sc2_dir_si + results$sc2_ind_si
Gesamteffekte$SI_sc2 <- c(sum(results$sc2_dir_si), sum(results$sc2_ind_si), sum(results$sc2_tot_si))

#Szenario3
results$sc3_dir_si <- numeric(2944)
results$sc3_dir_si[SI] <- shocks_scenario3[SI] * CShare$SI_P3_S14[SI] * 100
results$sc3_ind_si <- results$scenario3 * CShare$SI_P3_S14 * 100
results$sc3_tot_si <- results$sc3_dir_si + results$sc3_ind_si
Gesamteffekte$SI_sc3 <- c(sum(results$sc3_dir_si), sum(results$sc3_ind_si), sum(results$sc3_tot_si))

#Szenario4
results$sc4_dir_si <- numeric(2944)
results$sc4_dir_si[SI] <- shocks_scenario4[SI] * CShare$SI_P3_S14[SI] * 100
results$sc4_ind_si <- results$scenario4 * CShare$SI_P3_S14 * 100
results$sc4_tot_si <- results$sc4_dir_si + results$sc4_ind_si
Gesamteffekte$SI_sc4 <- c(sum(results$sc4_dir_si), sum(results$sc4_ind_si), sum(results$sc4_tot_si))

#Szenario5
results$sc5_dir_si <- numeric(2944)
results$sc5_dir_si[SI] <- shocks_scenario5[SI] * CShare$SI_P3_S14[SI] * 100
results$sc5_ind_si <- results$scenario5 * CShare$SI_P3_S14 * 100
results$sc5_tot_si <- results$sc5_dir_si + results$sc5_ind_si
Gesamteffekte$SI_sc5 <- c(sum(results$sc5_dir_si), sum(results$sc5_ind_si), sum(results$sc5_tot_si))


#SK: direkter, indirekter und totaler Effekt je Szenario (in %)

#SK finden und Indizes speichern
find_var("SK", data = matrixZ)
SK <- 2689:2752

#Szenario1
results$sc1_dir_sk <- numeric(2944)
results$sc1_ind_sk <- results$scenario1 * CShare$SK_P3_S14 * 100
results$sc1_tot_sk <- results$sc1_dir_sk + results$sc1_ind_sk
Gesamteffekte$SK_sc1 <- c(sum(results$sc1_dir_sk), sum(results$sc1_ind_sk), sum(results$sc1_tot_sk))

#Szenario2
results$sc2_dir_sk <- numeric(2944)
results$sc2_ind_sk <- results$scenario2 * CShare$SK_P3_S14 * 100
results$sc2_tot_sk <- results$sc2_dir_sk + results$sc2_ind_sk
Gesamteffekte$SK_sc2 <- c(sum(results$sc2_dir_sk), sum(results$sc2_ind_sk), sum(results$sc2_tot_sk))

#Szenario3
results$sc3_dir_sk <- numeric(2944)
results$sc3_dir_sk[SK] <- shocks_scenario3[SK] * CShare$SK_P3_S14[SK] * 100
results$sc3_ind_sk <- results$scenario3 * CShare$SK_P3_S14 * 100
results$sc3_tot_sk <- results$sc3_dir_sk + results$sc3_ind_sk
Gesamteffekte$SK_sc3 <- c(sum(results$sc3_dir_sk), sum(results$sc3_ind_sk), sum(results$sc3_tot_sk))

#Szenario4
results$sc4_dir_sk <- numeric(2944)
results$sc4_dir_sk[SK] <- shocks_scenario4[SK] * CShare$SK_P3_S14[SK] * 100
results$sc4_ind_sk <- results$scenario4 * CShare$SK_P3_S14 * 100
results$sc4_tot_sk <- results$sc4_dir_sk + results$sc4_ind_sk
Gesamteffekte$SK_sc4 <- c(sum(results$sc4_dir_sk), sum(results$sc4_ind_sk), sum(results$sc4_tot_sk))

#Szenario5
results$sc5_dir_sk <- numeric(2944)
results$sc5_dir_sk[SK] <- shocks_scenario5[SK] * CShare$SK_P3_S14[SK] * 100
results$sc5_ind_sk <- results$scenario5 * CShare$SK_P3_S14 * 100
results$sc5_tot_sk <- results$sc5_dir_sk + results$sc5_ind_sk
Gesamteffekte$SK_sc5 <- c(sum(results$sc5_dir_sk), sum(results$sc5_ind_sk), sum(results$sc5_tot_sk))


#FI: direkter, indirekter und totaler Effekt je Szenario (in %)

#FI finden und Indizes speichern
find_var("FI_", data = matrixZ)
FI <- 961:1024

#Szenario1
results$sc1_dir_fi <- numeric(2944)
results$sc1_ind_fi <- results$scenario1 * CShare$FI_P3_S14 * 100
results$sc1_tot_fi <- results$sc1_dir_fi + results$sc1_ind_fi
Gesamteffekte$FI_sc1 <- c(sum(results$sc1_dir_fi), sum(results$sc1_ind_fi), sum(results$sc1_tot_fi))

#Szenario2
results$sc2_dir_fi <- numeric(2944)
results$sc2_ind_fi <- results$scenario2 * CShare$FI_P3_S14 * 100
results$sc2_tot_fi <- results$sc2_dir_fi + results$sc2_ind_fi
Gesamteffekte$FI_sc2 <- c(sum(results$sc2_dir_fi), sum(results$sc2_ind_fi), sum(results$sc2_tot_fi))

#Szenario3
results$sc3_dir_fi <- numeric(2944)
results$sc3_dir_fi[FI] <- shocks_scenario3[FI] * CShare$FI_P3_S14[FI] * 100
results$sc3_ind_fi <- results$scenario3 * CShare$FI_P3_S14 * 100
results$sc3_tot_fi <- results$sc3_dir_fi + results$sc3_ind_fi
Gesamteffekte$FI_sc3 <- c(sum(results$sc3_dir_fi), sum(results$sc3_ind_fi), sum(results$sc3_tot_fi))

#Szenario4
results$sc4_dir_fi <- numeric(2944)
results$sc4_dir_fi[FI] <- shocks_scenario4[FI] * CShare$FI_P3_S14[FI] * 100
results$sc4_ind_fi <- results$scenario4 * CShare$FI_P3_S14 * 100
results$sc4_tot_fi <- results$sc4_dir_fi + results$sc4_ind_fi
Gesamteffekte$FI_sc4 <- c(sum(results$sc4_dir_fi), sum(results$sc4_ind_fi), sum(results$sc4_tot_fi))

#Szenario5
results$sc5_dir_fi <- numeric(2944)
results$sc5_dir_fi[FI] <- shocks_scenario5[FI] * CShare$FI_P3_S14[FI] * 100
results$sc5_ind_fi <- results$scenario5 * CShare$FI_P3_S14 * 100
results$sc5_tot_fi <- results$sc5_dir_fi + results$sc5_ind_fi
Gesamteffekte$FI_sc5 <- c(sum(results$sc5_dir_fi), sum(results$sc5_ind_fi), sum(results$sc5_tot_fi))



#SE: direkter, indirekter und totaler Effekt je Szenario (in %)

#SE finden und Indizes speichern
find_var("SE", data = matrixZ)
SE <- 2561:2624

#Szenario1
results$sc1_dir_se <- numeric(2944)
results$sc1_ind_se <- results$scenario1 * CShare$SE_P3_S14 * 100
results$sc1_tot_se <- results$sc1_dir_se + results$sc1_ind_se
Gesamteffekte$SE_sc1 <- c(sum(results$sc1_dir_se), sum(results$sc1_ind_se), sum(results$sc1_tot_se))

#Szenario2
results$sc2_dir_se <- numeric(2944)
results$sc2_ind_se <- results$scenario2 * CShare$SE_P3_S14 * 100
results$sc2_tot_se <- results$sc2_dir_se + results$sc2_ind_se
Gesamteffekte$SE_sc2 <- c(sum(results$sc2_dir_se), sum(results$sc2_ind_se), sum(results$sc2_tot_se))

#Szenario3
results$sc3_dir_se <- numeric(2944)
results$sc3_dir_se[SE] <- shocks_scenario3[SE] * CShare$SE_P3_S14[SE] * 100
results$sc3_ind_se <- results$scenario3 * CShare$SE_P3_S14 * 100
results$sc3_tot_se <- results$sc3_dir_se + results$sc3_ind_se
Gesamteffekte$SE_sc3 <- c(sum(results$sc3_dir_se), sum(results$sc3_ind_se), sum(results$sc3_tot_se))

#Szenario4
results$sc4_dir_se <- numeric(2944)
results$sc4_dir_se[SE] <- shocks_scenario4[SE] * CShare$SE_P3_S14[SE] * 100
results$sc4_ind_se <- results$scenario4 * CShare$SE_P3_S14 * 100
results$sc4_tot_se <- results$sc4_dir_se + results$sc4_ind_se
Gesamteffekte$SE_sc4 <- c(sum(results$sc4_dir_se), sum(results$sc4_ind_se), sum(results$sc4_tot_se))

#Szenario5
results$sc5_dir_se <- numeric(2944)
results$sc5_dir_se[SE] <- shocks_scenario5[SE] * CShare$SE_P3_S14[SE] * 100
results$sc5_ind_se <- results$scenario5 * CShare$SE_P3_S14 * 100
results$sc5_tot_se <- results$sc5_dir_se + results$sc5_ind_se
Gesamteffekte$SE_sc5 <- c(sum(results$sc5_dir_se), sum(results$sc5_ind_se), sum(results$sc5_tot_se))


#GesamtEU

#Szenario1
results$sc1_dir_eu <- numeric(2944)
results$sc1_ind_eu <- results$scenario1 * Cshare_EU$BE_P3_S14 * 100 + results$scenario1 * Cshare_EU$BG_P3_S14 * 100 + results$scenario1 * Cshare_EU$CZ_P3_S14 * 100 + results$scenario1 * Cshare_EU$DK_P3_S14 * 100 + results$scenario1 * Cshare_EU$DE_P3_S14 * 100 + results$scenario1 * Cshare_EU$EE_P3_S14 * 100 + results$scenario1 * Cshare_EU$IE_P3_S14 * 100 + results$scenario1 * Cshare_EU$GR_P3_S14 * 100 + results$scenario1 * Cshare_EU$ES_P3_S14 * 100 + results$scenario1 * Cshare_EU$FR_P3_S14 * 100 + results$scenario1 * Cshare_EU$HR_P3_S14 * 100 + results$scenario1 * Cshare_EU$IT_P3_S14 * 100 + results$scenario1 * Cshare_EU$CY_P3_S14 * 100 + results$scenario1 * Cshare_EU$LV_P3_S14 * 100 + results$scenario1 * Cshare_EU$LT_P3_S14 * 100 + results$scenario1 * Cshare_EU$LU_P3_S14 * 100 + results$scenario1 * Cshare_EU$HU_P3_S14 * 100 + results$scenario1 * Cshare_EU$MT_P3_S14 * 100 + results$scenario1 * Cshare_EU$NL_P3_S14 * 100 + results$scenario1 * Cshare_EU$AT_P3_S14 * 100 + results$scenario1 * Cshare_EU$PL_P3_S14 * 100 + results$scenario1 * Cshare_EU$PT_P3_S14 * 100 + results$scenario1 * Cshare_EU$RO_P3_S14 * 100 + results$scenario1 * Cshare_EU$SI_P3_S14 * 100 + results$scenario1 * Cshare_EU$SK_P3_S14 * 100 + results$scenario1 * Cshare_EU$FI_P3_S14 * 100 + results$scenario1 * Cshare_EU$SE_P3_S14 * 100
results$sc1_tot_eu <- results$sc1_dir_eu + results$sc1_ind_eu
Gesamteffekte$EU_sc1 <- c(sum(results$sc1_dir_eu),sum(results$sc1_ind_eu), sum(results$sc1_tot_eu))

#Szenario2
results$sc2_dir_eu <- numeric(2944)
results$sc2_ind_eu <- results$scenario2 * Cshare_EU$BE_P3_S14 * 100 + results$scenario2 * Cshare_EU$BG_P3_S14 * 100 + results$scenario2 * Cshare_EU$CZ_P3_S14 * 100 + results$scenario2 * Cshare_EU$DK_P3_S14 * 100 + results$scenario2 * Cshare_EU$DE_P3_S14 * 100 + results$scenario2 * Cshare_EU$EE_P3_S14 * 100 + results$scenario2 * Cshare_EU$IE_P3_S14 * 100 + results$scenario2 * Cshare_EU$GR_P3_S14 * 100 + results$scenario2 * Cshare_EU$ES_P3_S14 * 100 + results$scenario2 * Cshare_EU$FR_P3_S14 * 100 + results$scenario2 * Cshare_EU$HR_P3_S14 * 100 + results$scenario2 * Cshare_EU$IT_P3_S14 * 100 + results$scenario2 * Cshare_EU$CY_P3_S14 * 100 + results$scenario2 * Cshare_EU$LV_P3_S14 * 100 + results$scenario2 * Cshare_EU$LT_P3_S14 * 100 + results$scenario2 * Cshare_EU$LU_P3_S14 * 100 + results$scenario2 * Cshare_EU$HU_P3_S14 * 100 + results$scenario2 * Cshare_EU$MT_P3_S14 * 100 + results$scenario2 * Cshare_EU$NL_P3_S14 * 100 + results$scenario2 * Cshare_EU$AT_P3_S14 * 100 + results$scenario2 * Cshare_EU$PL_P3_S14 * 100 + results$scenario2 * Cshare_EU$PT_P3_S14 * 100 + results$scenario2 * Cshare_EU$RO_P3_S14 * 100 + results$scenario2 * Cshare_EU$SI_P3_S14 * 100 + results$scenario2 * Cshare_EU$SK_P3_S14 * 100 + results$scenario2 * Cshare_EU$FI_P3_S14 * 100 + results$scenario2 * Cshare_EU$SE_P3_S14 * 100
results$sc2_tot_eu <- results$sc2_dir_eu + results$sc2_ind_eu
Gesamteffekte$EU_sc2 <- c(sum(results$sc2_dir_eu), sum(results$sc2_ind_eu), sum(results$sc2_tot_eu))

#Szenario3
results$sc3_dir_eu <- numeric(2944)
results$sc3_dir_eu[BE] <- shocks_scenario3[BE] * Cshare_EU$BE_P3_S14[BE] * 100 
results$sc3_dir_eu[BG] <- shocks_scenario3[BG] * Cshare_EU$BG_P3_S14[BG] * 100 
results$sc3_dir_eu[CZ] <- shocks_scenario3[CZ] * Cshare_EU$CZ_P3_S14[CZ] * 100 
results$sc3_dir_eu[DK] <- shocks_scenario3[DK] * Cshare_EU$DK_P3_S14[DK] * 100 
results$sc3_dir_eu[DE] <- shocks_scenario3[DE] * Cshare_EU$DE_P3_S14[DE] * 100 
results$sc3_dir_eu[EE] <- shocks_scenario3[EE] * Cshare_EU$EE_P3_S14[EE] * 100 
results$sc3_dir_eu[IE] <- shocks_scenario3[IE] * Cshare_EU$IE_P3_S14[IE] * 100 
results$sc3_dir_eu[GR] <- shocks_scenario3[GR] * Cshare_EU$GR_P3_S14[GR] * 100 
results$sc3_dir_eu[ES] <- shocks_scenario3[ES] * Cshare_EU$ES_P3_S14[ES] * 100 
results$sc3_dir_eu[FR] <- shocks_scenario3[FR] * Cshare_EU$FR_P3_S14[FR] * 100 
results$sc3_dir_eu[HR] <- shocks_scenario3[HR] * Cshare_EU$HR_P3_S14[HR] * 100 
results$sc3_dir_eu[IT] <- shocks_scenario3[IT] * Cshare_EU$IT_P3_S14[IT] * 100 
results$sc3_dir_eu[CY] <- shocks_scenario3[CY] * Cshare_EU$CY_P3_S14[CY] * 100 
results$sc3_dir_eu[LV] <- shocks_scenario3[LV] * Cshare_EU$LV_P3_S14[LV] * 100 
results$sc3_dir_eu[LT] <- shocks_scenario3[LT] * Cshare_EU$LT_P3_S14[LT] * 100 
results$sc3_dir_eu[LU] <- shocks_scenario3[LU] * Cshare_EU$LU_P3_S14[LU] * 100 
results$sc3_dir_eu[HU] <- shocks_scenario3[HU] * Cshare_EU$HU_P3_S14[HU] * 100 
results$sc3_dir_eu[MT] <- shocks_scenario3[MT] * Cshare_EU$MT_P3_S14[MT] * 100 
results$sc3_dir_eu[NL] <- shocks_scenario3[NL] * Cshare_EU$NL_P3_S14[NL] * 100 
results$sc3_dir_eu[AT] <- shocks_scenario3[AT] * Cshare_EU$AT_P3_S14[AT] * 100 
results$sc3_dir_eu[PL] <- shocks_scenario3[PL] * Cshare_EU$PL_P3_S14[PL] * 100 
results$sc3_dir_eu[PT] <- shocks_scenario3[PT] * Cshare_EU$PT_P3_S14[PT] * 100 
results$sc3_dir_eu[RO] <- shocks_scenario3[RO] * Cshare_EU$RO_P3_S14[RO] * 100 
results$sc3_dir_eu[SI] <- shocks_scenario3[SI] * Cshare_EU$SI_P3_S14[SI] * 100 
results$sc3_dir_eu[SK] <- shocks_scenario3[SK] * Cshare_EU$SK_P3_S14[SK] * 100 
results$sc3_dir_eu[FI] <- shocks_scenario3[FI] * Cshare_EU$FI_P3_S14[FI] * 100 
results$sc3_dir_eu[SE] <- shocks_scenario3[SE] * Cshare_EU$SE_P3_S14[SE] * 100
results$sc3_ind_eu <- results$scenario3 * Cshare_EU$BE_P3_S14 * 100 + results$scenario3 * Cshare_EU$BG_P3_S14 * 100 + results$scenario3 * Cshare_EU$CZ_P3_S14 * 100 + results$scenario3 * Cshare_EU$DK_P3_S14 * 100 + results$scenario3 * Cshare_EU$DE_P3_S14 * 100 + results$scenario3 * Cshare_EU$EE_P3_S14 * 100 + results$scenario3 * Cshare_EU$IE_P3_S14 * 100 + results$scenario3 * Cshare_EU$GR_P3_S14 * 100 + results$scenario3 * Cshare_EU$ES_P3_S14 * 100 + results$scenario3 * Cshare_EU$FR_P3_S14 * 100 + results$scenario3 * Cshare_EU$HR_P3_S14 * 100 + results$scenario3 * Cshare_EU$IT_P3_S14 * 100 + results$scenario3 * Cshare_EU$CY_P3_S14 * 100 + results$scenario3 * Cshare_EU$LV_P3_S14 * 100 + results$scenario3 * Cshare_EU$LT_P3_S14 * 100 + results$scenario3 * Cshare_EU$LU_P3_S14 * 100 + results$scenario3 * Cshare_EU$HU_P3_S14 * 100 + results$scenario3 * Cshare_EU$MT_P3_S14 * 100 + results$scenario3 * Cshare_EU$NL_P3_S14 * 100 + results$scenario3 * Cshare_EU$AT_P3_S14 * 100 + results$scenario3 * Cshare_EU$PL_P3_S14 * 100 + results$scenario3 * Cshare_EU$PT_P3_S14 * 100 + results$scenario3 * Cshare_EU$RO_P3_S14 * 100 + results$scenario3 * Cshare_EU$SI_P3_S14 * 100 + results$scenario3 * Cshare_EU$SK_P3_S14 * 100 + results$scenario3 * Cshare_EU$FI_P3_S14 * 100 + results$scenario3 * Cshare_EU$SE_P3_S14 * 100 
results$sc3_tot_eu <- results$sc3_dir_eu + results$sc3_ind_eu
Gesamteffekte$EU_sc3 <- c(sum(results$sc3_dir_eu), sum(results$sc3_ind_eu), sum(results$sc3_tot_eu))

#Szenario4
results$sc4_dir_eu <- numeric(2944)
results$sc4_dir_eu[BE] <- shocks_scenario4[BE] * Cshare_EU$BE_P3_S14[BE] * 100 
results$sc4_dir_eu[BG] <- shocks_scenario4[BG] * Cshare_EU$BG_P3_S14[BG] * 100 
results$sc4_dir_eu[CZ] <- shocks_scenario4[CZ] * Cshare_EU$CZ_P3_S14[CZ] * 100 
results$sc4_dir_eu[DK] <- shocks_scenario4[DK] * Cshare_EU$DK_P3_S14[DK] * 100 
results$sc4_dir_eu[DE] <- shocks_scenario4[DE] * Cshare_EU$DE_P3_S14[DE] * 100 
results$sc4_dir_eu[EE] <- shocks_scenario4[EE] * Cshare_EU$EE_P3_S14[EE] * 100 
results$sc4_dir_eu[IE] <- shocks_scenario4[IE] * Cshare_EU$IE_P3_S14[IE] * 100 
results$sc4_dir_eu[GR] <- shocks_scenario4[GR] * Cshare_EU$GR_P3_S14[GR] * 100 
results$sc4_dir_eu[ES] <- shocks_scenario4[ES] * Cshare_EU$ES_P3_S14[ES] * 100 
results$sc4_dir_eu[FR] <- shocks_scenario4[FR] * Cshare_EU$FR_P3_S14[FR] * 100 
results$sc4_dir_eu[HR] <- shocks_scenario4[HR] * Cshare_EU$HR_P3_S14[HR] * 100 
results$sc4_dir_eu[IT] <- shocks_scenario4[IT] * Cshare_EU$IT_P3_S14[IT] * 100 
results$sc4_dir_eu[CY] <- shocks_scenario4[CY] * Cshare_EU$CY_P3_S14[CY] * 100 
results$sc4_dir_eu[LV] <- shocks_scenario4[LV] * Cshare_EU$LV_P3_S14[LV] * 100 
results$sc4_dir_eu[LT] <- shocks_scenario4[LT] * Cshare_EU$LT_P3_S14[LT] * 100 
results$sc4_dir_eu[LU] <- shocks_scenario4[LU] * Cshare_EU$LU_P3_S14[LU] * 100 
results$sc4_dir_eu[HU] <- shocks_scenario4[HU] * Cshare_EU$HU_P3_S14[HU] * 100 
results$sc4_dir_eu[MT] <- shocks_scenario4[MT] * Cshare_EU$MT_P3_S14[MT] * 100 
results$sc4_dir_eu[NL] <- shocks_scenario4[NL] * Cshare_EU$NL_P3_S14[NL] * 100 
results$sc4_dir_eu[AT] <- shocks_scenario4[AT] * Cshare_EU$AT_P3_S14[AT] * 100 
results$sc4_dir_eu[PL] <- shocks_scenario4[PL] * Cshare_EU$PL_P3_S14[PL] * 100 
results$sc4_dir_eu[PT] <- shocks_scenario4[PT] * Cshare_EU$PT_P3_S14[PT] * 100 
results$sc4_dir_eu[RO] <- shocks_scenario4[RO] * Cshare_EU$RO_P3_S14[RO] * 100 
results$sc4_dir_eu[SI] <- shocks_scenario4[SI] * Cshare_EU$SI_P3_S14[SI] * 100 
results$sc4_dir_eu[SK] <- shocks_scenario4[SK] * Cshare_EU$SK_P3_S14[SK] * 100 
results$sc4_dir_eu[FI] <- shocks_scenario4[FI] * Cshare_EU$FI_P3_S14[FI] * 100 
results$sc4_dir_eu[SE] <- shocks_scenario4[SE] * Cshare_EU$SE_P3_S14[SE] * 100
results$sc4_ind_eu <- results$scenario4 * rowSums(Cshare_EU) * 100
results$sc4_tot_eu <- results$sc4_dir_eu + results$sc4_ind_eu
Gesamteffekte$EU_sc4 <- c(sum(results$sc4_dir_eu), sum(results$sc4_ind_eu), sum(results$sc4_tot_eu))

#Szenario5
results$sc5_dir_eu <- numeric(2944)
results$sc5_dir_eu[BE] <- shocks_scenario5[BE] * Cshare_EU$BE_P3_S14[BE] * 100 
results$sc5_dir_eu[BG] <- shocks_scenario5[BG] * Cshare_EU$BG_P3_S14[BG] * 100 
results$sc5_dir_eu[CZ] <- shocks_scenario5[CZ] * Cshare_EU$CZ_P3_S14[CZ] * 100 
results$sc5_dir_eu[DK] <- shocks_scenario5[DK] * Cshare_EU$DK_P3_S14[DK] * 100 
results$sc5_dir_eu[DE] <- shocks_scenario5[DE] * Cshare_EU$DE_P3_S14[DE] * 100 
results$sc5_dir_eu[EE] <- shocks_scenario5[EE] * Cshare_EU$EE_P3_S14[EE] * 100 
results$sc5_dir_eu[IE] <- shocks_scenario5[IE] * Cshare_EU$IE_P3_S14[IE] * 100 
results$sc5_dir_eu[GR] <- shocks_scenario5[GR] * Cshare_EU$GR_P3_S14[GR] * 100 
results$sc5_dir_eu[ES] <- shocks_scenario5[ES] * Cshare_EU$ES_P3_S14[ES] * 100 
results$sc5_dir_eu[FR] <- shocks_scenario5[FR] * Cshare_EU$FR_P3_S14[FR] * 100 
results$sc5_dir_eu[HR] <- shocks_scenario5[HR] * Cshare_EU$HR_P3_S14[HR] * 100 
results$sc5_dir_eu[IT] <- shocks_scenario5[IT] * Cshare_EU$IT_P3_S14[IT] * 100 
results$sc5_dir_eu[CY] <- shocks_scenario5[CY] * Cshare_EU$CY_P3_S14[CY] * 100 
results$sc5_dir_eu[LV] <- shocks_scenario5[LV] * Cshare_EU$LV_P3_S14[LV] * 100 
results$sc5_dir_eu[LT] <- shocks_scenario5[LT] * Cshare_EU$LT_P3_S14[LT] * 100 
results$sc5_dir_eu[LU] <- shocks_scenario5[LU] * Cshare_EU$LU_P3_S14[LU] * 100 
results$sc5_dir_eu[HU] <- shocks_scenario5[HU] * Cshare_EU$HU_P3_S14[HU] * 100 
results$sc5_dir_eu[MT] <- shocks_scenario5[MT] * Cshare_EU$MT_P3_S14[MT] * 100 
results$sc5_dir_eu[NL] <- shocks_scenario5[NL] * Cshare_EU$NL_P3_S14[NL] * 100 
results$sc5_dir_eu[AT] <- shocks_scenario5[AT] * Cshare_EU$AT_P3_S14[AT] * 100 
results$sc5_dir_eu[PL] <- shocks_scenario5[PL] * Cshare_EU$PL_P3_S14[PL] * 100 
results$sc5_dir_eu[PT] <- shocks_scenario5[PT] * Cshare_EU$PT_P3_S14[PT] * 100 
results$sc5_dir_eu[RO] <- shocks_scenario5[RO] * Cshare_EU$RO_P3_S14[RO] * 100 
results$sc5_dir_eu[SI] <- shocks_scenario5[SI] * Cshare_EU$SI_P3_S14[SI] * 100 
results$sc5_dir_eu[SK] <- shocks_scenario5[SK] * Cshare_EU$SK_P3_S14[SK] * 100 
results$sc5_dir_eu[FI] <- shocks_scenario5[FI] * Cshare_EU$FI_P3_S14[FI] * 100 
results$sc5_dir_eu[SE] <- shocks_scenario5[SE] * Cshare_EU$SE_P3_S14[SE] * 100
results$sc5_ind_eu <- results$scenario5 * Cshare_EU$BE_P3_S14 * 100 + results$scenario5 * Cshare_EU$BG_P3_S14 * 100 + results$scenario5 * Cshare_EU$CZ_P3_S14 * 100 + results$scenario5 * Cshare_EU$DK_P3_S14 * 100 + results$scenario5 * Cshare_EU$DE_P3_S14 * 100 + results$scenario5 * Cshare_EU$EE_P3_S14 * 100 + results$scenario5 * Cshare_EU$IE_P3_S14 * 100 + results$scenario5 * Cshare_EU$GR_P3_S14 * 100 + results$scenario5 * Cshare_EU$ES_P3_S14 * 100 + results$scenario5 * Cshare_EU$FR_P3_S14 * 100 + results$scenario5 * Cshare_EU$HR_P3_S14 * 100 + results$scenario5 * Cshare_EU$IT_P3_S14 * 100 + results$scenario5 * Cshare_EU$CY_P3_S14 * 100 + results$scenario5 * Cshare_EU$LV_P3_S14 * 100 + results$scenario5 * Cshare_EU$LT_P3_S14 * 100 + results$scenario5 * Cshare_EU$LU_P3_S14 * 100 + results$scenario5 * Cshare_EU$HU_P3_S14 * 100 + results$scenario5 * Cshare_EU$MT_P3_S14 * 100 + results$scenario5 * Cshare_EU$NL_P3_S14 * 100 + results$scenario5 * Cshare_EU$AT_P3_S14 * 100 + results$scenario5 * Cshare_EU$PL_P3_S14 * 100 + results$scenario5 * Cshare_EU$PT_P3_S14 * 100 + results$scenario5 * Cshare_EU$RO_P3_S14 * 100 + results$scenario5 * Cshare_EU$SI_P3_S14 * 100 + results$scenario5 * Cshare_EU$SK_P3_S14 * 100 + results$scenario5 * Cshare_EU$FI_P3_S14 * 100 + results$scenario5 * Cshare_EU$SE_P3_S14 * 100
results$sc5_tot_eu <- results$sc5_dir_eu + results$sc5_ind_eu
Gesamteffekte$EU_sc5 <- c(sum(results$sc5_dir_eu), sum(results$sc5_ind_eu), sum(results$sc5_tot_eu))

#speichern
write.csv(results, "results_weber2.csv", row.names = FALSE)
write.csv(Gesamteffekte, "Gesamteffekte_weber2.csv", row.names = FALSE)
results <- read.csv("results_weber.csv")
Gesamteffekte <- read.csv("Gesamteffekte_weber.csv")
Gesamteffekte2 <- t(Gesamteffekte)



#quintilspezifisches Modell
#quintilsspezifische consumer share berechnen bzw. Überprüfen (expenditure weighted)


finaldemand_total_q1 <- colSums(data_q1[,-1])
finaldemand_total_q2 <- colSums(data_q2[,-1])
finaldemand_total_q3 <- colSums(data_q3[,-1])
finaldemand_total_q4 <- colSums(data_q4[,-1])
finaldemand_total_q5 <- colSums(data_q5[,-1])

#Näherungsweise 1


#Deutschland

#results df
results_de <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_de <- data.frame(Result = c("direct", "indirect", "total"))
#Szenario2
results_de$sc2_dir_de_q1 <- numeric(2944)
results_de$sc2_ind_de_q1 <- results$scenario2 * data_q1$DE_P3_S14 *100
results_de$sc2_tot_de_q1 <- results_de$sc2_dir_de_q1 + results_de$sc2_ind_de_q1
Gesamteffekte_de$de_sc2_q1 <- c(sum(results_de$sc2_dir_de_q1),sum(results_de$sc2_ind_de_q1), sum(results_de$sc2_tot_de_q1))

results_de$sc2_dir_de_q2 <- numeric(2944) 
results_de$sc2_ind_de_q2 <- results$scenario2 * data_q2$DE_P3_S14 *100 
results_de$sc2_tot_de_q2 <- results_de$sc2_dir_de_q2 + results_de$sc2_ind_de_q2 
Gesamteffekte_de$de_sc2_q2 <- c(sum(results_de$sc2_dir_de_q2),sum(results_de$sc2_ind_de_q2), sum(results_de$sc2_tot_de_q2))
results_de$sc2_dir_de_q3 <- numeric(2944) 
results_de$sc2_ind_de_q3 <- results$scenario2 * data_q3$DE_P3_S14 *100 
results_de$sc2_tot_de_q3 <- results_de$sc2_dir_de_q3 + results_de$sc2_ind_de_q3 
Gesamteffekte_de$de_sc2_q3 <- c(sum(results_de$sc2_dir_de_q3),sum(results_de$sc2_ind_de_q3), sum(results_de$sc2_tot_de_q3))
results_de$sc2_dir_de_q4 <- numeric(2944) 
results_de$sc2_ind_de_q4 <- results$scenario2 * data_q4$DE_P3_S14 *100 
results_de$sc2_tot_de_q4 <- results_de$sc2_dir_de_q4 + results_de$sc2_ind_de_q4 
Gesamteffekte_de$de_sc2_q4 <- c(sum(results_de$sc2_dir_de_q4),sum(results_de$sc2_ind_de_q4), sum(results_de$sc2_tot_de_q4))
results_de$sc2_dir_de_q5 <- numeric(2944) 
results_de$sc2_ind_de_q5 <- results$scenario2 * data_q5$DE_P3_S14 *100 
results_de$sc2_tot_de_q5 <- results_de$sc2_dir_de_q5 + results_de$sc2_ind_de_q5 
Gesamteffekte_de$de_sc2_q5 <- c(sum(results_de$sc2_dir_de_q5),sum(results_de$sc2_ind_de_q5), sum(results_de$sc2_tot_de_q5))

#Szenario4
results_de$sc4_dir_de_q1 <- numeric(2944)
results_de$sc4_dir_de_q1[DE] <- shocks_scenario4[DE] * data_q1$DE_P3_S14[DE] *100
results_de$sc4_ind_de_q1 <- results$scenario4 * data_q1$DE_P3_S14 *100
results_de$sc4_tot_de_q1 <- results_de$sc4_dir_de_q1 + results_de$sc4_ind_de_q1
Gesamteffekte_de$de_sc4_q1 <- c(sum(results_de$sc4_dir_de_q1),sum(results_de$sc4_ind_de_q1), sum(results_de$sc4_tot_de_q1))
results_de$sc4_dir_de_q2 <- numeric(2944)
results_de$sc4_dir_de_q2[DE] <- shocks_scenario4[DE] * data_q2$DE_P3_S14[DE] *100
results_de$sc4_ind_de_q2 <- results$scenario4 * data_q2$DE_P3_S14 *100
results_de$sc4_tot_de_q2 <- results_de$sc4_dir_de_q2 + results_de$sc4_ind_de_q2
Gesamteffekte_de$de_sc4_q2 <- c(sum(results_de$sc4_dir_de_q2),sum(results_de$sc4_ind_de_q2), sum(results_de$sc4_tot_de_q2))
results_de$sc4_dir_de_q3 <- numeric(2944)
results_de$sc4_dir_de_q3[DE] <- shocks_scenario4[DE] * data_q3$DE_P3_S14[DE] *100
results_de$sc4_ind_de_q3 <- results$scenario4 * data_q3$DE_P3_S14 *100
results_de$sc4_tot_de_q3 <- results_de$sc4_dir_de_q3 + results_de$sc4_ind_de_q3
Gesamteffekte_de$de_sc4_q3 <- c(sum(results_de$sc4_dir_de_q3),sum(results_de$sc4_ind_de_q3), sum(results_de$sc4_tot_de_q3))
results_de$sc4_dir_de_q4 <- numeric(2944)
results_de$sc4_dir_de_q4[DE] <- shocks_scenario4[DE] * data_q4$DE_P3_S14[DE] *100
results_de$sc4_ind_de_q4 <- results$scenario4 * data_q4$DE_P3_S14 *100
results_de$sc4_tot_de_q4 <- results_de$sc4_dir_de_q4 + results_de$sc4_ind_de_q4
Gesamteffekte_de$de_sc4_q4 <- c(sum(results_de$sc4_dir_de_q4),sum(results_de$sc4_ind_de_q4), sum(results_de$sc4_tot_de_q4))
results_de$sc4_dir_de_q5 <- numeric(2944)
results_de$sc4_ind_de_q5 <- results$scenario4 * data_q5$DE_P3_S14 *100
results_de$sc4_dir_de_q5[DE] <- shocks_scenario4[DE] * data_q5$DE_P3_S14[DE] *100
results_de$sc4_tot_de_q5 <- results_de$sc4_dir_de_q5 + results_de$sc4_ind_de_q5
Gesamteffekte_de$de_sc4_q5 <- c(sum(results_de$sc4_dir_de_q5),sum(results_de$sc4_ind_de_q5), sum(results_de$sc4_tot_de_q5))


#Österreich
#results df
results_at <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_at <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_at$sc2_dir_at_q1 <- numeric(2944)
 results_at$sc2_ind_at_q1 <- results$scenario2 * data_q1$AT_P3_S14 *100
 results_at$sc2_tot_at_q1 <- results_at$sc2_dir_at_q1 + results_at$sc2_ind_at_q1
 Gesamteffekte_at$at_sc2_q1 <- c(sum(results_at$sc2_dir_at_q1),sum(results_at$sc2_ind_at_q1), sum(results_at$sc2_tot_at_q1))
results_at$sc2_dir_at_q2 <- numeric(2944)
 results_at$sc2_ind_at_q2 <- results$scenario2 * data_q2$AT_P3_S14 *100
 results_at$sc2_tot_at_q2 <- results_at$sc2_dir_at_q2 + results_at$sc2_ind_at_q2
 Gesamteffekte_at$at_sc2_q2 <- c(sum(results_at$sc2_dir_at_q2),sum(results_at$sc2_ind_at_q2), sum(results_at$sc2_tot_at_q2))
results_at$sc2_dir_at_q3 <- numeric(2944)
 results_at$sc2_ind_at_q3 <- results$scenario2 * data_q3$AT_P3_S14 *100
 results_at$sc2_tot_at_q3 <- results_at$sc2_dir_at_q3 + results_at$sc2_ind_at_q3
 Gesamteffekte_at$at_sc2_q3 <- c(sum(results_at$sc2_dir_at_q3),sum(results_at$sc2_ind_at_q3), sum(results_at$sc2_tot_at_q3))
results_at$sc2_dir_at_q4 <- numeric(2944)
 results_at$sc2_ind_at_q4 <- results$scenario2 * data_q4$AT_P3_S14 *100
 results_at$sc2_tot_at_q4 <- results_at$sc2_dir_at_q4 + results_at$sc2_ind_at_q4
 Gesamteffekte_at$at_sc2_q4 <- c(sum(results_at$sc2_dir_at_q4),sum(results_at$sc2_ind_at_q4), sum(results_at$sc2_tot_at_q4))
results_at$sc2_dir_at_q5 <- numeric(2944)
 results_at$sc2_ind_at_q5 <- results$scenario2 * data_q5$AT_P3_S14 *100
 results_at$sc2_tot_at_q5 <- results_at$sc2_dir_at_q5 + results_at$sc2_ind_at_q5
 Gesamteffekte_at$at_sc2_q5 <- c(sum(results_at$sc2_dir_at_q5),sum(results_at$sc2_ind_at_q5), sum(results_at$sc2_tot_at_q5))

#Szenario4
results_at$sc4_dir_at_q1 <- numeric(2944) 
results_at$sc4_dir_at_q1[AT] <- shocks_scenario4[AT] * data_q1$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q1 <- results$scenario4 * data_q1$AT_P3_S14 *100
results_at$sc4_tot_at_q1 <- results_at$sc4_dir_at_q1 + results_at$sc4_ind_at_q1
Gesamteffekte_at$at_sc4_q1 <- c(sum(results_at$sc4_dir_at_q1),sum(results_at$sc4_ind_at_q1), sum(results_at$sc4_tot_at_q1))
results_at$sc4_dir_at_q2 <- numeric(2944) 
results_at$sc4_dir_at_q2[AT] <- shocks_scenario4[AT] * data_q2$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q2 <- results$scenario4 * data_q2$AT_P3_S14 *100
results_at$sc4_tot_at_q2 <- results_at$sc4_dir_at_q2 + results_at$sc4_ind_at_q2
Gesamteffekte_at$at_sc4_q2 <- c(sum(results_at$sc4_dir_at_q2),sum(results_at$sc4_ind_at_q2), sum(results_at$sc4_tot_at_q2))
results_at$sc4_dir_at_q2 <- numeric(2944) 
results_at$sc4_dir_at_q2[AT] <- shocks_scenario4[AT] * data_q2$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q2 <- results$scenario4 * data_q2$AT_P3_S14 *100
results_at$sc4_tot_at_q2 <- results_at$sc4_dir_at_q2 + results_at$sc4_ind_at_q2
Gesamteffekte_at$at_sc4_q2 <- c(sum(results_at$sc4_dir_at_q2),sum(results_at$sc4_ind_at_q2), sum(results_at$sc4_tot_at_q2))
results_at$sc4_dir_at_q3 <- numeric(2944) 
results_at$sc4_dir_at_q3[AT] <- shocks_scenario4[AT] * data_q3$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q3 <- results$scenario4 * data_q3$AT_P3_S14 *100
results_at$sc4_tot_at_q3 <- results_at$sc4_dir_at_q3 + results_at$sc4_ind_at_q3
Gesamteffekte_at$at_sc4_q3 <- c(sum(results_at$sc4_dir_at_q3),sum(results_at$sc4_ind_at_q3), sum(results_at$sc4_tot_at_q3))
results_at$sc4_dir_at_q4 <- numeric(2944) 
results_at$sc4_dir_at_q4[AT] <- shocks_scenario4[AT] * data_q4$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q4 <- results$scenario4 * data_q4$AT_P3_S14 *100
results_at$sc4_tot_at_q4 <- results_at$sc4_dir_at_q4 + results_at$sc4_ind_at_q4
Gesamteffekte_at$at_sc4_q4 <- c(sum(results_at$sc4_dir_at_q4),sum(results_at$sc4_ind_at_q4), sum(results_at$sc4_tot_at_q4))
results_at$sc4_dir_at_q5 <- numeric(2944) 
results_at$sc4_dir_at_q5[AT] <- shocks_scenario4[AT] * data_q5$AT_P3_S14[AT] *100
results_at$sc4_ind_at_q5 <- results$scenario4 * data_q5$AT_P3_S14 *100
results_at$sc4_tot_at_q5 <- results_at$sc4_dir_at_q5 + results_at$sc4_ind_at_q5
Gesamteffekte_at$at_sc4_q5 <- c(sum(results_at$sc4_dir_at_q5),sum(results_at$sc4_ind_at_q5), sum(results_at$sc4_tot_at_q5))


#Belgien
#results df
results_be <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_be <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_be$sc2_dir_be_q1 <- numeric(2944)
results_be$sc2_ind_be_q1 <- results$scenario2 * data_q1$BE_P3_S14 *100
results_be$sc2_tot_be_q1 <- results_be$sc2_dir_be_q1 + results_be$sc2_ind_be_q1
Gesamteffekte_be$be_sc2_q1 <- c(sum(results_be$sc2_dir_be_q1),sum(results_be$sc2_ind_be_q1), sum(results_be$sc2_tot_be_q1))
results_be$sc2_dir_be_q2 <- numeric(2944)
results_be$sc2_ind_be_q2 <- results$scenario2 * data_q2$BE_P3_S14 *100
results_be$sc2_tot_be_q2 <- results_be$sc2_dir_be_q2 + results_be$sc2_ind_be_q2
Gesamteffekte_be$be_sc2_q2 <- c(sum(results_be$sc2_dir_be_q2),sum(results_be$sc2_ind_be_q2), sum(results_be$sc2_tot_be_q2))
results_be$sc2_dir_be_q3 <- numeric(2944)
results_be$sc2_ind_be_q3 <- results$scenario2 * data_q3$BE_P3_S14 *100
results_be$sc2_tot_be_q3 <- results_be$sc2_dir_be_q3 + results_be$sc2_ind_be_q3
Gesamteffekte_be$be_sc2_q3 <- c(sum(results_be$sc2_dir_be_q3),sum(results_be$sc2_ind_be_q3), sum(results_be$sc2_tot_be_q3))
results_be$sc2_dir_be_q4 <- numeric(2944)
results_be$sc2_ind_be_q4 <- results$scenario2 * data_q4$BE_P3_S14 *100
results_be$sc2_tot_be_q4 <- results_be$sc2_dir_be_q4 + results_be$sc2_ind_be_q4
Gesamteffekte_be$be_sc2_q4 <- c(sum(results_be$sc2_dir_be_q4),sum(results_be$sc2_ind_be_q4), sum(results_be$sc2_tot_be_q4))
results_be$sc2_dir_be_q5 <- numeric(2944)
results_be$sc2_ind_be_q5 <- results$scenario2 * data_q5$BE_P3_S14 *100
results_be$sc2_tot_be_q5 <- results_be$sc2_dir_be_q5 + results_be$sc2_ind_be_q5
Gesamteffekte_be$be_sc2_q5 <- c(sum(results_be$sc2_dir_be_q5),sum(results_be$sc2_ind_be_q5), sum(results_be$sc2_tot_be_q5))

#Szenario4
results_be$sc4_dir_be_q1 <- numeric(2944) 
results_be$sc4_dir_be_q1[BE] <- shocks_scenario4[BE] * data_q1$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q1 <- results$scenario4 * data_q1$BE_P3_S14 *100
results_be$sc4_tot_be_q1 <- results_be$sc4_dir_be_q1 + results_be$sc4_ind_be_q1
Gesamteffekte_be$be_sc4_q1 <- c(sum(results_be$sc4_dir_be_q1),sum(results_be$sc4_ind_be_q1), sum(results_be$sc4_tot_be_q1))
results_be$sc4_dir_be_q2 <- numeric(2944) 
results_be$sc4_dir_be_q2[BE] <- shocks_scenario4[BE] * data_q2$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q2 <- results$scenario4 * data_q2$BE_P3_S14 *100
results_be$sc4_tot_be_q2 <- results_be$sc4_dir_be_q2 + results_be$sc4_ind_be_q2
Gesamteffekte_be$be_sc4_q2 <- c(sum(results_be$sc4_dir_be_q2),sum(results_be$sc4_ind_be_q2), sum(results_be$sc4_tot_be_q2))
results_be$sc4_dir_be_q2 <- numeric(2944) 
results_be$sc4_dir_be_q2[BE] <- shocks_scenario4[BE] * data_q2$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q2 <- results$scenario4 * data_q2$BE_P3_S14 *100
results_be$sc4_tot_be_q2 <- results_be$sc4_dir_be_q2 + results_be$sc4_ind_be_q2
Gesamteffekte_be$be_sc4_q2 <- c(sum(results_be$sc4_dir_be_q2),sum(results_be$sc4_ind_be_q2), sum(results_be$sc4_tot_be_q2))
results_be$sc4_dir_be_q3 <- numeric(2944) 
results_be$sc4_dir_be_q3[BE] <- shocks_scenario4[BE] * data_q3$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q3 <- results$scenario4 * data_q3$BE_P3_S14 *100
results_be$sc4_tot_be_q3 <- results_be$sc4_dir_be_q3 + results_be$sc4_ind_be_q3
Gesamteffekte_be$be_sc4_q3 <- c(sum(results_be$sc4_dir_be_q3),sum(results_be$sc4_ind_be_q3), sum(results_be$sc4_tot_be_q3))
results_be$sc4_dir_be_q4 <- numeric(2944) 
results_be$sc4_dir_be_q4[BE] <- shocks_scenario4[BE] * data_q4$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q4 <- results$scenario4 * data_q4$BE_P3_S14 *100
results_be$sc4_tot_be_q4 <- results_be$sc4_dir_be_q4 + results_be$sc4_ind_be_q4
Gesamteffekte_be$be_sc4_q4 <- c(sum(results_be$sc4_dir_be_q4),sum(results_be$sc4_ind_be_q4), sum(results_be$sc4_tot_be_q4))
results_be$sc4_dir_be_q5 <- numeric(2944) 
results_be$sc4_dir_be_q5[BE] <- shocks_scenario4[BE] * data_q5$BE_P3_S14[BE] *100
results_be$sc4_ind_be_q5 <- results$scenario4 * data_q5$BE_P3_S14 *100
results_be$sc4_tot_be_q5 <- results_be$sc4_dir_be_q5 + results_be$sc4_ind_be_q5
Gesamteffekte_be$be_sc4_q5 <- c(sum(results_be$sc4_dir_be_q5),sum(results_be$sc4_ind_be_q5), sum(results_be$sc4_tot_be_q5))


#Bulgarien

#results df
results_bg <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_bg <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_bg$sc2_dir_bg_q1 <- numeric(2944)
results_bg$sc2_ind_bg_q1 <- results$scenario2 * data_q1$BG_P3_S14 *100
results_bg$sc2_tot_bg_q1 <- results_bg$sc2_dir_bg_q1 + results_bg$sc2_ind_bg_q1
Gesamteffekte_bg$bg_sc2_q1 <- c(sum(results_bg$sc2_dir_bg_q1),sum(results_bg$sc2_ind_bg_q1), sum(results_bg$sc2_tot_bg_q1))
results_bg$sc2_dir_bg_q2 <- numeric(2944)
results_bg$sc2_ind_bg_q2 <- results$scenario2 * data_q2$BG_P3_S14 *100
results_bg$sc2_tot_bg_q2 <- results_bg$sc2_dir_bg_q2 + results_bg$sc2_ind_bg_q2
Gesamteffekte_bg$bg_sc2_q2 <- c(sum(results_bg$sc2_dir_bg_q2),sum(results_bg$sc2_ind_bg_q2), sum(results_bg$sc2_tot_bg_q2))
results_bg$sc2_dir_bg_q3 <- numeric(2944)
results_bg$sc2_ind_bg_q3 <- results$scenario2 * data_q3$BG_P3_S14 *100
results_bg$sc2_tot_bg_q3 <- results_bg$sc2_dir_bg_q3 + results_bg$sc2_ind_bg_q3
Gesamteffekte_bg$bg_sc2_q3 <- c(sum(results_bg$sc2_dir_bg_q3),sum(results_bg$sc2_ind_bg_q3), sum(results_bg$sc2_tot_bg_q3))
results_bg$sc2_dir_bg_q4 <- numeric(2944)
results_bg$sc2_ind_bg_q4 <- results$scenario2 * data_q4$BG_P3_S14 *100
results_bg$sc2_tot_bg_q4 <- results_bg$sc2_dir_bg_q4 + results_bg$sc2_ind_bg_q4
Gesamteffekte_bg$bg_sc2_q4 <- c(sum(results_bg$sc2_dir_bg_q4),sum(results_bg$sc2_ind_bg_q4), sum(results_bg$sc2_tot_bg_q4))
results_bg$sc2_dir_bg_q5 <- numeric(2944)
results_bg$sc2_ind_bg_q5 <- results$scenario2 * data_q5$BG_P3_S14 *100
results_bg$sc2_tot_bg_q5 <- results_bg$sc2_dir_bg_q5 + results_bg$sc2_ind_bg_q5
Gesamteffekte_bg$bg_sc2_q5 <- c(sum(results_bg$sc2_dir_bg_q5),sum(results_bg$sc2_ind_bg_q5), sum(results_bg$sc2_tot_bg_q5))

#Szenario4
results_bg$sc4_dir_bg_q1 <- numeric(2944) 
results_bg$sc4_dir_bg_q1[BG] <- shocks_scenario4[BG] * data_q1$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q1 <- results$scenario4 * data_q1$BG_P3_S14 *100
results_bg$sc4_tot_bg_q1 <- results_bg$sc4_dir_bg_q1 + results_bg$sc4_ind_bg_q1
Gesamteffekte_bg$bg_sc4_q1 <- c(sum(results_bg$sc4_dir_bg_q1),sum(results_bg$sc4_ind_bg_q1), sum(results_bg$sc4_tot_bg_q1))
results_bg$sc4_dir_bg_q2 <- numeric(2944) 
results_bg$sc4_dir_bg_q2[BG] <- shocks_scenario4[BG] * data_q2$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q2 <- results$scenario4 * data_q2$BG_P3_S14 *100
results_bg$sc4_tot_bg_q2 <- results_bg$sc4_dir_bg_q2 + results_bg$sc4_ind_bg_q2
Gesamteffekte_bg$bg_sc4_q2 <- c(sum(results_bg$sc4_dir_bg_q2),sum(results_bg$sc4_ind_bg_q2), sum(results_bg$sc4_tot_bg_q2))
results_bg$sc4_dir_bg_q2 <- numeric(2944) 
results_bg$sc4_dir_bg_q2[BG] <- shocks_scenario4[BG] * data_q2$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q2 <- results$scenario4 * data_q2$BG_P3_S14 *100
results_bg$sc4_tot_bg_q2 <- results_bg$sc4_dir_bg_q2 + results_bg$sc4_ind_bg_q2
Gesamteffekte_bg$bg_sc4_q2 <- c(sum(results_bg$sc4_dir_bg_q2),sum(results_bg$sc4_ind_bg_q2), sum(results_bg$sc4_tot_bg_q2))
results_bg$sc4_dir_bg_q3 <- numeric(2944) 
results_bg$sc4_dir_bg_q3[BG] <- shocks_scenario4[BG] * data_q3$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q3 <- results$scenario4 * data_q3$BG_P3_S14 *100
results_bg$sc4_tot_bg_q3 <- results_bg$sc4_dir_bg_q3 + results_bg$sc4_ind_bg_q3
Gesamteffekte_bg$bg_sc4_q3 <- c(sum(results_bg$sc4_dir_bg_q3),sum(results_bg$sc4_ind_bg_q3), sum(results_bg$sc4_tot_bg_q3))
results_bg$sc4_dir_bg_q4 <- numeric(2944) 
results_bg$sc4_dir_bg_q4[BG] <- shocks_scenario4[BG] * data_q4$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q4 <- results$scenario4 * data_q4$BG_P3_S14 *100
results_bg$sc4_tot_bg_q4 <- results_bg$sc4_dir_bg_q4 + results_bg$sc4_ind_bg_q4
Gesamteffekte_bg$bg_sc4_q4 <- c(sum(results_bg$sc4_dir_bg_q4),sum(results_bg$sc4_ind_bg_q4), sum(results_bg$sc4_tot_bg_q4))
results_bg$sc4_dir_bg_q5 <- numeric(2944) 
results_bg$sc4_dir_bg_q5[BG] <- shocks_scenario4[BG] * data_q5$BG_P3_S14[BG] *100
results_bg$sc4_ind_bg_q5 <- results$scenario4 * data_q5$BG_P3_S14 *100
results_bg$sc4_tot_bg_q5 <- results_bg$sc4_dir_bg_q5 + results_bg$sc4_ind_bg_q5
Gesamteffekte_bg$bg_sc4_q5 <- c(sum(results_bg$sc4_dir_bg_q5),sum(results_bg$sc4_ind_bg_q5), sum(results_bg$sc4_tot_bg_q5))

#Zypern
#results df
results_cy <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_cy <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_cy$sc2_dir_cy_q1 <- numeric(2944)
results_cy$sc2_ind_cy_q1 <- results$scenario2 * data_q1$CY_P3_S14 *100
results_cy$sc2_tot_cy_q1 <- results_cy$sc2_dir_cy_q1 + results_cy$sc2_ind_cy_q1
Gesamteffekte_cy$cy_sc2_q1 <- c(sum(results_cy$sc2_dir_cy_q1),sum(results_cy$sc2_ind_cy_q1), sum(results_cy$sc2_tot_cy_q1))
results_cy$sc2_dir_cy_q2 <- numeric(2944)
results_cy$sc2_ind_cy_q2 <- results$scenario2 * data_q2$CY_P3_S14 *100
results_cy$sc2_tot_cy_q2 <- results_cy$sc2_dir_cy_q2 + results_cy$sc2_ind_cy_q2
Gesamteffekte_cy$cy_sc2_q2 <- c(sum(results_cy$sc2_dir_cy_q2),sum(results_cy$sc2_ind_cy_q2), sum(results_cy$sc2_tot_cy_q2))
results_cy$sc2_dir_cy_q3 <- numeric(2944)
results_cy$sc2_ind_cy_q3 <- results$scenario2 * data_q3$CY_P3_S14 *100
results_cy$sc2_tot_cy_q3 <- results_cy$sc2_dir_cy_q3 + results_cy$sc2_ind_cy_q3
Gesamteffekte_cy$cy_sc2_q3 <- c(sum(results_cy$sc2_dir_cy_q3),sum(results_cy$sc2_ind_cy_q3), sum(results_cy$sc2_tot_cy_q3))
results_cy$sc2_dir_cy_q4 <- numeric(2944)
results_cy$sc2_ind_cy_q4 <- results$scenario2 * data_q4$CY_P3_S14 *100
results_cy$sc2_tot_cy_q4 <- results_cy$sc2_dir_cy_q4 + results_cy$sc2_ind_cy_q4
Gesamteffekte_cy$cy_sc2_q4 <- c(sum(results_cy$sc2_dir_cy_q4),sum(results_cy$sc2_ind_cy_q4), sum(results_cy$sc2_tot_cy_q4))
results_cy$sc2_dir_cy_q5 <- numeric(2944)
results_cy$sc2_ind_cy_q5 <- results$scenario2 * data_q5$CY_P3_S14 *100
results_cy$sc2_tot_cy_q5 <- results_cy$sc2_dir_cy_q5 + results_cy$sc2_ind_cy_q5
Gesamteffekte_cy$cy_sc2_q5 <- c(sum(results_cy$sc2_dir_cy_q5),sum(results_cy$sc2_ind_cy_q5), sum(results_cy$sc2_tot_cy_q5))

#Szenario4
results_cy$sc4_dir_cy_q1 <- numeric(2944) 
results_cy$sc4_dir_cy_q1[CY] <- shocks_scenario4[CY] * data_q1$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q1 <- results$scenario4 * data_q1$CY_P3_S14 *100
results_cy$sc4_tot_cy_q1 <- results_cy$sc4_dir_cy_q1 + results_cy$sc4_ind_cy_q1
Gesamteffekte_cy$cy_sc4_q1 <- c(sum(results_cy$sc4_dir_cy_q1),sum(results_cy$sc4_ind_cy_q1), sum(results_cy$sc4_tot_cy_q1))
results_cy$sc4_dir_cy_q2 <- numeric(2944) 
results_cy$sc4_dir_cy_q2[CY] <- shocks_scenario4[CY] * data_q2$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q2 <- results$scenario4 * data_q2$CY_P3_S14 *100
results_cy$sc4_tot_cy_q2 <- results_cy$sc4_dir_cy_q2 + results_cy$sc4_ind_cy_q2
Gesamteffekte_cy$cy_sc4_q2 <- c(sum(results_cy$sc4_dir_cy_q2),sum(results_cy$sc4_ind_cy_q2), sum(results_cy$sc4_tot_cy_q2))
results_cy$sc4_dir_cy_q2 <- numeric(2944) 
results_cy$sc4_dir_cy_q2[CY] <- shocks_scenario4[CY] * data_q2$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q2 <- results$scenario4 * data_q2$CY_P3_S14 *100
results_cy$sc4_tot_cy_q2 <- results_cy$sc4_dir_cy_q2 + results_cy$sc4_ind_cy_q2
Gesamteffekte_cy$cy_sc4_q2 <- c(sum(results_cy$sc4_dir_cy_q2),sum(results_cy$sc4_ind_cy_q2), sum(results_cy$sc4_tot_cy_q2))
results_cy$sc4_dir_cy_q3 <- numeric(2944) 
results_cy$sc4_dir_cy_q3[CY] <- shocks_scenario4[CY] * data_q3$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q3 <- results$scenario4 * data_q3$CY_P3_S14 *100
results_cy$sc4_tot_cy_q3 <- results_cy$sc4_dir_cy_q3 + results_cy$sc4_ind_cy_q3
Gesamteffekte_cy$cy_sc4_q3 <- c(sum(results_cy$sc4_dir_cy_q3),sum(results_cy$sc4_ind_cy_q3), sum(results_cy$sc4_tot_cy_q3))
results_cy$sc4_dir_cy_q4 <- numeric(2944) 
results_cy$sc4_dir_cy_q4[CY] <- shocks_scenario4[CY] * data_q4$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q4 <- results$scenario4 * data_q4$CY_P3_S14 *100
results_cy$sc4_tot_cy_q4 <- results_cy$sc4_dir_cy_q4 + results_cy$sc4_ind_cy_q4
Gesamteffekte_cy$cy_sc4_q4 <- c(sum(results_cy$sc4_dir_cy_q4),sum(results_cy$sc4_ind_cy_q4), sum(results_cy$sc4_tot_cy_q4))
results_cy$sc4_dir_cy_q5 <- numeric(2944) 
results_cy$sc4_dir_cy_q5[CY] <- shocks_scenario4[CY] * data_q5$CY_P3_S14[CY] *100
results_cy$sc4_ind_cy_q5 <- results$scenario4 * data_q5$CY_P3_S14 *100
results_cy$sc4_tot_cy_q5 <- results_cy$sc4_dir_cy_q5 + results_cy$sc4_ind_cy_q5
Gesamteffekte_cy$cy_sc4_q5 <- c(sum(results_cy$sc4_dir_cy_q5),sum(results_cy$sc4_ind_cy_q5), sum(results_cy$sc4_tot_cy_q5))

#Dänemark
#results df
results_dk <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_dk <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_dk$sc2_dir_dk_q1 <- numeric(2944)
results_dk$sc2_ind_dk_q1 <- results$scenario2 * data_q1$DK_P3_S14 *100
results_dk$sc2_tot_dk_q1 <- results_dk$sc2_dir_dk_q1 + results_dk$sc2_ind_dk_q1
Gesamteffekte_dk$dk_sc2_q1 <- c(sum(results_dk$sc2_dir_dk_q1),sum(results_dk$sc2_ind_dk_q1), sum(results_dk$sc2_tot_dk_q1))
results_dk$sc2_dir_dk_q2 <- numeric(2944)
results_dk$sc2_ind_dk_q2 <- results$scenario2 * data_q2$DK_P3_S14 *100
results_dk$sc2_tot_dk_q2 <- results_dk$sc2_dir_dk_q2 + results_dk$sc2_ind_dk_q2
Gesamteffekte_dk$dk_sc2_q2 <- c(sum(results_dk$sc2_dir_dk_q2),sum(results_dk$sc2_ind_dk_q2), sum(results_dk$sc2_tot_dk_q2))
results_dk$sc2_dir_dk_q3 <- numeric(2944)
results_dk$sc2_ind_dk_q3 <- results$scenario2 * data_q3$DK_P3_S14 *100
results_dk$sc2_tot_dk_q3 <- results_dk$sc2_dir_dk_q3 + results_dk$sc2_ind_dk_q3
Gesamteffekte_dk$dk_sc2_q3 <- c(sum(results_dk$sc2_dir_dk_q3),sum(results_dk$sc2_ind_dk_q3), sum(results_dk$sc2_tot_dk_q3))
results_dk$sc2_dir_dk_q4 <- numeric(2944)
results_dk$sc2_ind_dk_q4 <- results$scenario2 * data_q4$DK_P3_S14 *100
results_dk$sc2_tot_dk_q4 <- results_dk$sc2_dir_dk_q4 + results_dk$sc2_ind_dk_q4
Gesamteffekte_dk$dk_sc2_q4 <- c(sum(results_dk$sc2_dir_dk_q4),sum(results_dk$sc2_ind_dk_q4), sum(results_dk$sc2_tot_dk_q4))
results_dk$sc2_dir_dk_q5 <- numeric(2944)
results_dk$sc2_ind_dk_q5 <- results$scenario2 * data_q5$DK_P3_S14 *100
results_dk$sc2_tot_dk_q5 <- results_dk$sc2_dir_dk_q5 + results_dk$sc2_ind_dk_q5
Gesamteffekte_dk$dk_sc2_q5 <- c(sum(results_dk$sc2_dir_dk_q5),sum(results_dk$sc2_ind_dk_q5), sum(results_dk$sc2_tot_dk_q5))

#Szenario4
results_dk$sc4_dir_dk_q1 <- numeric(2944) 
results_dk$sc4_dir_dk_q1[DK] <- shocks_scenario4[DK] * data_q1$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q1 <- results$scenario4 * data_q1$DK_P3_S14 *100
results_dk$sc4_tot_dk_q1 <- results_dk$sc4_dir_dk_q1 + results_dk$sc4_ind_dk_q1
Gesamteffekte_dk$dk_sc4_q1 <- c(sum(results_dk$sc4_dir_dk_q1),sum(results_dk$sc4_ind_dk_q1), sum(results_dk$sc4_tot_dk_q1))
results_dk$sc4_dir_dk_q2 <- numeric(2944) 
results_dk$sc4_dir_dk_q2[DK] <- shocks_scenario4[DK] * data_q2$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q2 <- results$scenario4 * data_q2$DK_P3_S14 *100
results_dk$sc4_tot_dk_q2 <- results_dk$sc4_dir_dk_q2 + results_dk$sc4_ind_dk_q2
Gesamteffekte_dk$dk_sc4_q2 <- c(sum(results_dk$sc4_dir_dk_q2),sum(results_dk$sc4_ind_dk_q2), sum(results_dk$sc4_tot_dk_q2))
results_dk$sc4_dir_dk_q2 <- numeric(2944) 
results_dk$sc4_dir_dk_q2[DK] <- shocks_scenario4[DK] * data_q2$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q2 <- results$scenario4 * data_q2$DK_P3_S14 *100
results_dk$sc4_tot_dk_q2 <- results_dk$sc4_dir_dk_q2 + results_dk$sc4_ind_dk_q2
Gesamteffekte_dk$dk_sc4_q2 <- c(sum(results_dk$sc4_dir_dk_q2),sum(results_dk$sc4_ind_dk_q2), sum(results_dk$sc4_tot_dk_q2))
results_dk$sc4_dir_dk_q3 <- numeric(2944) 
results_dk$sc4_dir_dk_q3[DK] <- shocks_scenario4[DK] * data_q3$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q3 <- results$scenario4 * data_q3$DK_P3_S14 *100
results_dk$sc4_tot_dk_q3 <- results_dk$sc4_dir_dk_q3 + results_dk$sc4_ind_dk_q3
Gesamteffekte_dk$dk_sc4_q3 <- c(sum(results_dk$sc4_dir_dk_q3),sum(results_dk$sc4_ind_dk_q3), sum(results_dk$sc4_tot_dk_q3))
results_dk$sc4_dir_dk_q4 <- numeric(2944) 
results_dk$sc4_dir_dk_q4[DK] <- shocks_scenario4[DK] * data_q4$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q4 <- results$scenario4 * data_q4$DK_P3_S14 *100
results_dk$sc4_tot_dk_q4 <- results_dk$sc4_dir_dk_q4 + results_dk$sc4_ind_dk_q4
Gesamteffekte_dk$dk_sc4_q4 <- c(sum(results_dk$sc4_dir_dk_q4),sum(results_dk$sc4_ind_dk_q4), sum(results_dk$sc4_tot_dk_q4))
results_dk$sc4_dir_dk_q5 <- numeric(2944) 
results_dk$sc4_dir_dk_q5[DK] <- shocks_scenario4[DK] * data_q5$DK_P3_S14[DK] *100
results_dk$sc4_ind_dk_q5 <- results$scenario4 * data_q5$DK_P3_S14 *100
results_dk$sc4_tot_dk_q5 <- results_dk$sc4_dir_dk_q5 + results_dk$sc4_ind_dk_q5
Gesamteffekte_dk$dk_sc4_q5 <- c(sum(results_dk$sc4_dir_dk_q5),sum(results_dk$sc4_ind_dk_q5), sum(results_dk$sc4_tot_dk_q5))

#EE

#results df
results_ee <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_ee <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_ee$sc2_dir_ee_q1 <- numeric(2944)
results_ee$sc2_ind_ee_q1 <- results$scenario2 * data_q1$EE_P3_S14 *100
results_ee$sc2_tot_ee_q1 <- results_ee$sc2_dir_ee_q1 + results_ee$sc2_ind_ee_q1
Gesamteffekte_ee$ee_sc2_q1 <- c(sum(results_ee$sc2_dir_ee_q1),sum(results_ee$sc2_ind_ee_q1), sum(results_ee$sc2_tot_ee_q1))
results_ee$sc2_dir_ee_q2 <- numeric(2944)
results_ee$sc2_ind_ee_q2 <- results$scenario2 * data_q2$EE_P3_S14 *100
results_ee$sc2_tot_ee_q2 <- results_ee$sc2_dir_ee_q2 + results_ee$sc2_ind_ee_q2
Gesamteffekte_ee$ee_sc2_q2 <- c(sum(results_ee$sc2_dir_ee_q2),sum(results_ee$sc2_ind_ee_q2), sum(results_ee$sc2_tot_ee_q2))
results_ee$sc2_dir_ee_q3 <- numeric(2944)
results_ee$sc2_ind_ee_q3 <- results$scenario2 * data_q3$EE_P3_S14 *100
results_ee$sc2_tot_ee_q3 <- results_ee$sc2_dir_ee_q3 + results_ee$sc2_ind_ee_q3
Gesamteffekte_ee$ee_sc2_q3 <- c(sum(results_ee$sc2_dir_ee_q3),sum(results_ee$sc2_ind_ee_q3), sum(results_ee$sc2_tot_ee_q3))
results_ee$sc2_dir_ee_q4 <- numeric(2944)
results_ee$sc2_ind_ee_q4 <- results$scenario2 * data_q4$EE_P3_S14 *100
results_ee$sc2_tot_ee_q4 <- results_ee$sc2_dir_ee_q4 + results_ee$sc2_ind_ee_q4
Gesamteffekte_ee$ee_sc2_q4 <- c(sum(results_ee$sc2_dir_ee_q4),sum(results_ee$sc2_ind_ee_q4), sum(results_ee$sc2_tot_ee_q4))
results_ee$sc2_dir_ee_q5 <- numeric(2944)
results_ee$sc2_ind_ee_q5 <- results$scenario2 * data_q5$EE_P3_S14 *100
results_ee$sc2_tot_ee_q5 <- results_ee$sc2_dir_ee_q5 + results_ee$sc2_ind_ee_q5
Gesamteffekte_ee$ee_sc2_q5 <- c(sum(results_ee$sc2_dir_ee_q5),sum(results_ee$sc2_ind_ee_q5), sum(results_ee$sc2_tot_ee_q5))

#Szenario4
results_ee$sc4_dir_ee_q1 <- numeric(2944) 
results_ee$sc4_dir_ee_q1[EE] <- shocks_scenario4[EE] * data_q1$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q1 <- results$scenario4 * data_q1$EE_P3_S14 *100
results_ee$sc4_tot_ee_q1 <- results_ee$sc4_dir_ee_q1 + results_ee$sc4_ind_ee_q1
Gesamteffekte_ee$ee_sc4_q1 <- c(sum(results_ee$sc4_dir_ee_q1),sum(results_ee$sc4_ind_ee_q1), sum(results_ee$sc4_tot_ee_q1))
results_ee$sc4_dir_ee_q2 <- numeric(2944) 
results_ee$sc4_dir_ee_q2[EE] <- shocks_scenario4[EE] * data_q2$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q2 <- results$scenario4 * data_q2$EE_P3_S14 *100
results_ee$sc4_tot_ee_q2 <- results_ee$sc4_dir_ee_q2 + results_ee$sc4_ind_ee_q2
Gesamteffekte_ee$ee_sc4_q2 <- c(sum(results_ee$sc4_dir_ee_q2),sum(results_ee$sc4_ind_ee_q2), sum(results_ee$sc4_tot_ee_q2))
results_ee$sc4_dir_ee_q2 <- numeric(2944) 
results_ee$sc4_dir_ee_q2[EE] <- shocks_scenario4[EE] * data_q2$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q2 <- results$scenario4 * data_q2$EE_P3_S14 *100
results_ee$sc4_tot_ee_q2 <- results_ee$sc4_dir_ee_q2 + results_ee$sc4_ind_ee_q2
Gesamteffekte_ee$ee_sc4_q2 <- c(sum(results_ee$sc4_dir_ee_q2),sum(results_ee$sc4_ind_ee_q2), sum(results_ee$sc4_tot_ee_q2))
results_ee$sc4_dir_ee_q3 <- numeric(2944) 
results_ee$sc4_dir_ee_q3[EE] <- shocks_scenario4[EE] * data_q3$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q3 <- results$scenario4 * data_q3$EE_P3_S14 *100
results_ee$sc4_tot_ee_q3 <- results_ee$sc4_dir_ee_q3 + results_ee$sc4_ind_ee_q3
Gesamteffekte_ee$ee_sc4_q3 <- c(sum(results_ee$sc4_dir_ee_q3),sum(results_ee$sc4_ind_ee_q3), sum(results_ee$sc4_tot_ee_q3))
results_ee$sc4_dir_ee_q4 <- numeric(2944) 
results_ee$sc4_dir_ee_q4[EE] <- shocks_scenario4[EE] * data_q4$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q4 <- results$scenario4 * data_q4$EE_P3_S14 *100
results_ee$sc4_tot_ee_q4 <- results_ee$sc4_dir_ee_q4 + results_ee$sc4_ind_ee_q4
Gesamteffekte_ee$ee_sc4_q4 <- c(sum(results_ee$sc4_dir_ee_q4),sum(results_ee$sc4_ind_ee_q4), sum(results_ee$sc4_tot_ee_q4))
results_ee$sc4_dir_ee_q5 <- numeric(2944) 
results_ee$sc4_dir_ee_q5[EE] <- shocks_scenario4[EE] * data_q5$EE_P3_S14[EE] *100
results_ee$sc4_ind_ee_q5 <- results$scenario4 * data_q5$EE_P3_S14 *100
results_ee$sc4_tot_ee_q5 <- results_ee$sc4_dir_ee_q5 + results_ee$sc4_ind_ee_q5
Gesamteffekte_ee$ee_sc4_q5 <- c(sum(results_ee$sc4_dir_ee_q5),sum(results_ee$sc4_ind_ee_q5), sum(results_ee$sc4_tot_ee_q5))


#Spanien

#results df
results_es <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_es <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_es$sc2_dir_es_q1 <- numeric(2944)
results_es$sc2_ind_es_q1 <- results$scenario2 * data_q1$ES_P3_S14 *100
results_es$sc2_tot_es_q1 <- results_es$sc2_dir_es_q1 + results_es$sc2_ind_es_q1
Gesamteffekte_es$es_sc2_q1 <- c(sum(results_es$sc2_dir_es_q1),sum(results_es$sc2_ind_es_q1), sum(results_es$sc2_tot_es_q1))
results_es$sc2_dir_es_q2 <- numeric(2944)
results_es$sc2_ind_es_q2 <- results$scenario2 * data_q2$ES_P3_S14 *100
results_es$sc2_tot_es_q2 <- results_es$sc2_dir_es_q2 + results_es$sc2_ind_es_q2
Gesamteffekte_es$es_sc2_q2 <- c(sum(results_es$sc2_dir_es_q2),sum(results_es$sc2_ind_es_q2), sum(results_es$sc2_tot_es_q2))
results_es$sc2_dir_es_q3 <- numeric(2944)
results_es$sc2_ind_es_q3 <- results$scenario2 * data_q3$ES_P3_S14 *100
results_es$sc2_tot_es_q3 <- results_es$sc2_dir_es_q3 + results_es$sc2_ind_es_q3
Gesamteffekte_es$es_sc2_q3 <- c(sum(results_es$sc2_dir_es_q3),sum(results_es$sc2_ind_es_q3), sum(results_es$sc2_tot_es_q3))
results_es$sc2_dir_es_q4 <- numeric(2944)
results_es$sc2_ind_es_q4 <- results$scenario2 * data_q4$ES_P3_S14 *100
results_es$sc2_tot_es_q4 <- results_es$sc2_dir_es_q4 + results_es$sc2_ind_es_q4
Gesamteffekte_es$es_sc2_q4 <- c(sum(results_es$sc2_dir_es_q4),sum(results_es$sc2_ind_es_q4), sum(results_es$sc2_tot_es_q4))
results_es$sc2_dir_es_q5 <- numeric(2944)
results_es$sc2_ind_es_q5 <- results$scenario2 * data_q5$ES_P3_S14 *100
results_es$sc2_tot_es_q5 <- results_es$sc2_dir_es_q5 + results_es$sc2_ind_es_q5
Gesamteffekte_es$es_sc2_q5 <- c(sum(results_es$sc2_dir_es_q5),sum(results_es$sc2_ind_es_q5), sum(results_es$sc2_tot_es_q5))

#Szenario4
results_es$sc4_dir_es_q1 <- numeric(2944) 
results_es$sc4_dir_es_q1[ES] <- shocks_scenario4[ES] * data_q1$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q1 <- results$scenario4 * data_q1$ES_P3_S14 *100
results_es$sc4_tot_es_q1 <- results_es$sc4_dir_es_q1 + results_es$sc4_ind_es_q1
Gesamteffekte_es$es_sc4_q1 <- c(sum(results_es$sc4_dir_es_q1),sum(results_es$sc4_ind_es_q1), sum(results_es$sc4_tot_es_q1))
results_es$sc4_dir_es_q2 <- numeric(2944) 
results_es$sc4_dir_es_q2[ES] <- shocks_scenario4[ES] * data_q2$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q2 <- results$scenario4 * data_q2$ES_P3_S14 *100
results_es$sc4_tot_es_q2 <- results_es$sc4_dir_es_q2 + results_es$sc4_ind_es_q2
Gesamteffekte_es$es_sc4_q2 <- c(sum(results_es$sc4_dir_es_q2),sum(results_es$sc4_ind_es_q2), sum(results_es$sc4_tot_es_q2))
results_es$sc4_dir_es_q2 <- numeric(2944) 
results_es$sc4_dir_es_q2[ES] <- shocks_scenario4[ES] * data_q2$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q2 <- results$scenario4 * data_q2$ES_P3_S14 *100
results_es$sc4_tot_es_q2 <- results_es$sc4_dir_es_q2 + results_es$sc4_ind_es_q2
Gesamteffekte_es$es_sc4_q2 <- c(sum(results_es$sc4_dir_es_q2),sum(results_es$sc4_ind_es_q2), sum(results_es$sc4_tot_es_q2))
results_es$sc4_dir_es_q3 <- numeric(2944) 
results_es$sc4_dir_es_q3[ES] <- shocks_scenario4[ES] * data_q3$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q3 <- results$scenario4 * data_q3$ES_P3_S14 *100
results_es$sc4_tot_es_q3 <- results_es$sc4_dir_es_q3 + results_es$sc4_ind_es_q3
Gesamteffekte_es$es_sc4_q3 <- c(sum(results_es$sc4_dir_es_q3),sum(results_es$sc4_ind_es_q3), sum(results_es$sc4_tot_es_q3))
results_es$sc4_dir_es_q4 <- numeric(2944) 
results_es$sc4_dir_es_q4[ES] <- shocks_scenario4[ES] * data_q4$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q4 <- results$scenario4 * data_q4$ES_P3_S14 *100
results_es$sc4_tot_es_q4 <- results_es$sc4_dir_es_q4 + results_es$sc4_ind_es_q4
Gesamteffekte_es$es_sc4_q4 <- c(sum(results_es$sc4_dir_es_q4),sum(results_es$sc4_ind_es_q4), sum(results_es$sc4_tot_es_q4))
results_es$sc4_dir_es_q5 <- numeric(2944) 
results_es$sc4_dir_es_q5[ES] <- shocks_scenario4[ES] * data_q5$ES_P3_S14[ES] *100
results_es$sc4_ind_es_q5 <- results$scenario4 * data_q5$ES_P3_S14 *100
results_es$sc4_tot_es_q5 <- results_es$sc4_dir_es_q5 + results_es$sc4_ind_es_q5
Gesamteffekte_es$es_sc4_q5 <- c(sum(results_es$sc4_dir_es_q5),sum(results_es$sc4_ind_es_q5), sum(results_es$sc4_tot_es_q5))


#Frankreich
#results df
results_fr <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_fr <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_fr$sc2_dir_fr_q1 <- numeric(2944)
results_fr$sc2_ind_fr_q1 <- results$scenario2 * data_q1$FR_P3_S14 *100
results_fr$sc2_tot_fr_q1 <- results_fr$sc2_dir_fr_q1 + results_fr$sc2_ind_fr_q1
Gesamteffekte_fr$fr_sc2_q1 <- c(sum(results_fr$sc2_dir_fr_q1),sum(results_fr$sc2_ind_fr_q1), sum(results_fr$sc2_tot_fr_q1))
results_fr$sc2_dir_fr_q2 <- numeric(2944)
results_fr$sc2_ind_fr_q2 <- results$scenario2 * data_q2$FR_P3_S14 *100
results_fr$sc2_tot_fr_q2 <- results_fr$sc2_dir_fr_q2 + results_fr$sc2_ind_fr_q2
Gesamteffekte_fr$fr_sc2_q2 <- c(sum(results_fr$sc2_dir_fr_q2),sum(results_fr$sc2_ind_fr_q2), sum(results_fr$sc2_tot_fr_q2))
results_fr$sc2_dir_fr_q3 <- numeric(2944)
results_fr$sc2_ind_fr_q3 <- results$scenario2 * data_q3$FR_P3_S14 *100
results_fr$sc2_tot_fr_q3 <- results_fr$sc2_dir_fr_q3 + results_fr$sc2_ind_fr_q3
Gesamteffekte_fr$fr_sc2_q3 <- c(sum(results_fr$sc2_dir_fr_q3),sum(results_fr$sc2_ind_fr_q3), sum(results_fr$sc2_tot_fr_q3))
results_fr$sc2_dir_fr_q4 <- numeric(2944)
results_fr$sc2_ind_fr_q4 <- results$scenario2 * data_q4$FR_P3_S14 *100
results_fr$sc2_tot_fr_q4 <- results_fr$sc2_dir_fr_q4 + results_fr$sc2_ind_fr_q4
Gesamteffekte_fr$fr_sc2_q4 <- c(sum(results_fr$sc2_dir_fr_q4),sum(results_fr$sc2_ind_fr_q4), sum(results_fr$sc2_tot_fr_q4))
results_fr$sc2_dir_fr_q5 <- numeric(2944)
results_fr$sc2_ind_fr_q5 <- results$scenario2 * data_q5$FR_P3_S14 *100
results_fr$sc2_tot_fr_q5 <- results_fr$sc2_dir_fr_q5 + results_fr$sc2_ind_fr_q5
Gesamteffekte_fr$fr_sc2_q5 <- c(sum(results_fr$sc2_dir_fr_q5),sum(results_fr$sc2_ind_fr_q5), sum(results_fr$sc2_tot_fr_q5))

#Szenario4
results_fr$sc4_dir_fr_q1 <- numeric(2944) 
results_fr$sc4_dir_fr_q1[FR] <- shocks_scenario4[FR] * data_q1$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q1 <- results$scenario4 * data_q1$FR_P3_S14 *100
results_fr$sc4_tot_fr_q1 <- results_fr$sc4_dir_fr_q1 + results_fr$sc4_ind_fr_q1
Gesamteffekte_fr$fr_sc4_q1 <- c(sum(results_fr$sc4_dir_fr_q1),sum(results_fr$sc4_ind_fr_q1), sum(results_fr$sc4_tot_fr_q1))
results_fr$sc4_dir_fr_q2 <- numeric(2944) 
results_fr$sc4_dir_fr_q2[FR] <- shocks_scenario4[FR] * data_q2$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q2 <- results$scenario4 * data_q2$FR_P3_S14 *100
results_fr$sc4_tot_fr_q2 <- results_fr$sc4_dir_fr_q2 + results_fr$sc4_ind_fr_q2
Gesamteffekte_fr$fr_sc4_q2 <- c(sum(results_fr$sc4_dir_fr_q2),sum(results_fr$sc4_ind_fr_q2), sum(results_fr$sc4_tot_fr_q2))
results_fr$sc4_dir_fr_q2 <- numeric(2944) 
results_fr$sc4_dir_fr_q2[FR] <- shocks_scenario4[FR] * data_q2$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q2 <- results$scenario4 * data_q2$FR_P3_S14 *100
results_fr$sc4_tot_fr_q2 <- results_fr$sc4_dir_fr_q2 + results_fr$sc4_ind_fr_q2
Gesamteffekte_fr$fr_sc4_q2 <- c(sum(results_fr$sc4_dir_fr_q2),sum(results_fr$sc4_ind_fr_q2), sum(results_fr$sc4_tot_fr_q2))
results_fr$sc4_dir_fr_q3 <- numeric(2944) 
results_fr$sc4_dir_fr_q3[FR] <- shocks_scenario4[FR] * data_q3$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q3 <- results$scenario4 * data_q3$FR_P3_S14 *100
results_fr$sc4_tot_fr_q3 <- results_fr$sc4_dir_fr_q3 + results_fr$sc4_ind_fr_q3
Gesamteffekte_fr$fr_sc4_q3 <- c(sum(results_fr$sc4_dir_fr_q3),sum(results_fr$sc4_ind_fr_q3), sum(results_fr$sc4_tot_fr_q3))
results_fr$sc4_dir_fr_q4 <- numeric(2944) 
results_fr$sc4_dir_fr_q4[FR] <- shocks_scenario4[FR] * data_q4$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q4 <- results$scenario4 * data_q4$FR_P3_S14 *100
results_fr$sc4_tot_fr_q4 <- results_fr$sc4_dir_fr_q4 + results_fr$sc4_ind_fr_q4
Gesamteffekte_fr$fr_sc4_q4 <- c(sum(results_fr$sc4_dir_fr_q4),sum(results_fr$sc4_ind_fr_q4), sum(results_fr$sc4_tot_fr_q4))
results_fr$sc4_dir_fr_q5 <- numeric(2944) 
results_fr$sc4_dir_fr_q5[FR] <- shocks_scenario4[FR] * data_q5$FR_P3_S14[FR] *100
results_fr$sc4_ind_fr_q5 <- results$scenario4 * data_q5$FR_P3_S14 *100
results_fr$sc4_tot_fr_q5 <- results_fr$sc4_dir_fr_q5 + results_fr$sc4_ind_fr_q5
Gesamteffekte_fr$fr_sc4_q5 <- c(sum(results_fr$sc4_dir_fr_q5),sum(results_fr$sc4_ind_fr_q5), sum(results_fr$sc4_tot_fr_q5))


#Griechenland
#results df
results_gr <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_gr <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_gr$sc2_dir_gr_q1 <- numeric(2944)
results_gr$sc2_ind_gr_q1 <- results$scenario2 * data_q1$GR_P3_S14 *100
results_gr$sc2_tot_gr_q1 <- results_gr$sc2_dir_gr_q1 + results_gr$sc2_ind_gr_q1
Gesamteffekte_gr$gr_sc2_q1 <- c(sum(results_gr$sc2_dir_gr_q1),sum(results_gr$sc2_ind_gr_q1), sum(results_gr$sc2_tot_gr_q1))
results_gr$sc2_dir_gr_q2 <- numeric(2944)
results_gr$sc2_ind_gr_q2 <- results$scenario2 * data_q2$GR_P3_S14 *100
results_gr$sc2_tot_gr_q2 <- results_gr$sc2_dir_gr_q2 + results_gr$sc2_ind_gr_q2
Gesamteffekte_gr$gr_sc2_q2 <- c(sum(results_gr$sc2_dir_gr_q2),sum(results_gr$sc2_ind_gr_q2), sum(results_gr$sc2_tot_gr_q2))
results_gr$sc2_dir_gr_q3 <- numeric(2944)
results_gr$sc2_ind_gr_q3 <- results$scenario2 * data_q3$GR_P3_S14 *100
results_gr$sc2_tot_gr_q3 <- results_gr$sc2_dir_gr_q3 + results_gr$sc2_ind_gr_q3
Gesamteffekte_gr$gr_sc2_q3 <- c(sum(results_gr$sc2_dir_gr_q3),sum(results_gr$sc2_ind_gr_q3), sum(results_gr$sc2_tot_gr_q3))
results_gr$sc2_dir_gr_q4 <- numeric(2944)
results_gr$sc2_ind_gr_q4 <- results$scenario2 * data_q4$GR_P3_S14 *100
results_gr$sc2_tot_gr_q4 <- results_gr$sc2_dir_gr_q4 + results_gr$sc2_ind_gr_q4
Gesamteffekte_gr$gr_sc2_q4 <- c(sum(results_gr$sc2_dir_gr_q4),sum(results_gr$sc2_ind_gr_q4), sum(results_gr$sc2_tot_gr_q4))
results_gr$sc2_dir_gr_q5 <- numeric(2944)
results_gr$sc2_ind_gr_q5 <- results$scenario2 * data_q5$GR_P3_S14 *100
results_gr$sc2_tot_gr_q5 <- results_gr$sc2_dir_gr_q5 + results_gr$sc2_ind_gr_q5
Gesamteffekte_gr$gr_sc2_q5 <- c(sum(results_gr$sc2_dir_gr_q5),sum(results_gr$sc2_ind_gr_q5), sum(results_gr$sc2_tot_gr_q5))

#Szenario4
results_gr$sc4_dir_gr_q1 <- numeric(2944) 
results_gr$sc4_dir_gr_q1[GR] <- shocks_scenario4[GR] * data_q1$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q1 <- results$scenario4 * data_q1$GR_P3_S14 *100
results_gr$sc4_tot_gr_q1 <- results_gr$sc4_dir_gr_q1 + results_gr$sc4_ind_gr_q1
Gesamteffekte_gr$gr_sc4_q1 <- c(sum(results_gr$sc4_dir_gr_q1),sum(results_gr$sc4_ind_gr_q1), sum(results_gr$sc4_tot_gr_q1))
results_gr$sc4_dir_gr_q2 <- numeric(2944) 
results_gr$sc4_dir_gr_q2[GR] <- shocks_scenario4[GR] * data_q2$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q2 <- results$scenario4 * data_q2$GR_P3_S14 *100
results_gr$sc4_tot_gr_q2 <- results_gr$sc4_dir_gr_q2 + results_gr$sc4_ind_gr_q2
Gesamteffekte_gr$gr_sc4_q2 <- c(sum(results_gr$sc4_dir_gr_q2),sum(results_gr$sc4_ind_gr_q2), sum(results_gr$sc4_tot_gr_q2))
results_gr$sc4_dir_gr_q2 <- numeric(2944) 
results_gr$sc4_dir_gr_q2[GR] <- shocks_scenario4[GR] * data_q2$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q2 <- results$scenario4 * data_q2$GR_P3_S14 *100
results_gr$sc4_tot_gr_q2 <- results_gr$sc4_dir_gr_q2 + results_gr$sc4_ind_gr_q2
Gesamteffekte_gr$gr_sc4_q2 <- c(sum(results_gr$sc4_dir_gr_q2),sum(results_gr$sc4_ind_gr_q2), sum(results_gr$sc4_tot_gr_q2))
results_gr$sc4_dir_gr_q3 <- numeric(2944) 
results_gr$sc4_dir_gr_q3[GR] <- shocks_scenario4[GR] * data_q3$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q3 <- results$scenario4 * data_q3$GR_P3_S14 *100
results_gr$sc4_tot_gr_q3 <- results_gr$sc4_dir_gr_q3 + results_gr$sc4_ind_gr_q3
Gesamteffekte_gr$gr_sc4_q3 <- c(sum(results_gr$sc4_dir_gr_q3),sum(results_gr$sc4_ind_gr_q3), sum(results_gr$sc4_tot_gr_q3))
results_gr$sc4_dir_gr_q4 <- numeric(2944) 
results_gr$sc4_dir_gr_q4[GR] <- shocks_scenario4[GR] * data_q4$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q4 <- results$scenario4 * data_q4$GR_P3_S14 *100
results_gr$sc4_tot_gr_q4 <- results_gr$sc4_dir_gr_q4 + results_gr$sc4_ind_gr_q4
Gesamteffekte_gr$gr_sc4_q4 <- c(sum(results_gr$sc4_dir_gr_q4),sum(results_gr$sc4_ind_gr_q4), sum(results_gr$sc4_tot_gr_q4))
results_gr$sc4_dir_gr_q5 <- numeric(2944) 
results_gr$sc4_dir_gr_q5[GR] <- shocks_scenario4[GR] * data_q5$GR_P3_S14[GR] *100
results_gr$sc4_ind_gr_q5 <- results$scenario4 * data_q5$GR_P3_S14 *100
results_gr$sc4_tot_gr_q5 <- results_gr$sc4_dir_gr_q5 + results_gr$sc4_ind_gr_q5
Gesamteffekte_gr$gr_sc4_q5 <- c(sum(results_gr$sc4_dir_gr_q5),sum(results_gr$sc4_ind_gr_q5), sum(results_gr$sc4_tot_gr_q5))


#Croatia

#results df
results_hr <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_hr <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_hr$sc2_dir_hr_q1 <- numeric(2944)
results_hr$sc2_ind_hr_q1 <- results$scenario2 * data_q1$HR_P3_S14 *100
results_hr$sc2_tot_hr_q1 <- results_hr$sc2_dir_hr_q1 + results_hr$sc2_ind_hr_q1
Gesamteffekte_hr$hr_sc2_q1 <- c(sum(results_hr$sc2_dir_hr_q1),sum(results_hr$sc2_ind_hr_q1), sum(results_hr$sc2_tot_hr_q1))
results_hr$sc2_dir_hr_q2 <- numeric(2944)
results_hr$sc2_ind_hr_q2 <- results$scenario2 * data_q2$HR_P3_S14 *100
results_hr$sc2_tot_hr_q2 <- results_hr$sc2_dir_hr_q2 + results_hr$sc2_ind_hr_q2
Gesamteffekte_hr$hr_sc2_q2 <- c(sum(results_hr$sc2_dir_hr_q2),sum(results_hr$sc2_ind_hr_q2), sum(results_hr$sc2_tot_hr_q2))
results_hr$sc2_dir_hr_q3 <- numeric(2944)
results_hr$sc2_ind_hr_q3 <- results$scenario2 * data_q3$HR_P3_S14 *100
results_hr$sc2_tot_hr_q3 <- results_hr$sc2_dir_hr_q3 + results_hr$sc2_ind_hr_q3
Gesamteffekte_hr$hr_sc2_q3 <- c(sum(results_hr$sc2_dir_hr_q3),sum(results_hr$sc2_ind_hr_q3), sum(results_hr$sc2_tot_hr_q3))
results_hr$sc2_dir_hr_q4 <- numeric(2944)
results_hr$sc2_ind_hr_q4 <- results$scenario2 * data_q4$HR_P3_S14 *100
results_hr$sc2_tot_hr_q4 <- results_hr$sc2_dir_hr_q4 + results_hr$sc2_ind_hr_q4
Gesamteffekte_hr$hr_sc2_q4 <- c(sum(results_hr$sc2_dir_hr_q4),sum(results_hr$sc2_ind_hr_q4), sum(results_hr$sc2_tot_hr_q4))
results_hr$sc2_dir_hr_q5 <- numeric(2944)
results_hr$sc2_ind_hr_q5 <- results$scenario2 * data_q5$HR_P3_S14 *100
results_hr$sc2_tot_hr_q5 <- results_hr$sc2_dir_hr_q5 + results_hr$sc2_ind_hr_q5
Gesamteffekte_hr$hr_sc2_q5 <- c(sum(results_hr$sc2_dir_hr_q5),sum(results_hr$sc2_ind_hr_q5), sum(results_hr$sc2_tot_hr_q5))

#Szenario4
results_hr$sc4_dir_hr_q1 <- numeric(2944) 
results_hr$sc4_dir_hr_q1[HR] <- shocks_scenario4[HR] * data_q1$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q1 <- results$scenario4 * data_q1$HR_P3_S14 *100
results_hr$sc4_tot_hr_q1 <- results_hr$sc4_dir_hr_q1 + results_hr$sc4_ind_hr_q1
Gesamteffekte_hr$hr_sc4_q1 <- c(sum(results_hr$sc4_dir_hr_q1),sum(results_hr$sc4_ind_hr_q1), sum(results_hr$sc4_tot_hr_q1))
results_hr$sc4_dir_hr_q2 <- numeric(2944) 
results_hr$sc4_dir_hr_q2[HR] <- shocks_scenario4[HR] * data_q2$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q2 <- results$scenario4 * data_q2$HR_P3_S14 *100
results_hr$sc4_tot_hr_q2 <- results_hr$sc4_dir_hr_q2 + results_hr$sc4_ind_hr_q2
Gesamteffekte_hr$hr_sc4_q2 <- c(sum(results_hr$sc4_dir_hr_q2),sum(results_hr$sc4_ind_hr_q2), sum(results_hr$sc4_tot_hr_q2))
results_hr$sc4_dir_hr_q2 <- numeric(2944) 
results_hr$sc4_dir_hr_q2[HR] <- shocks_scenario4[HR] * data_q2$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q2 <- results$scenario4 * data_q2$HR_P3_S14 *100
results_hr$sc4_tot_hr_q2 <- results_hr$sc4_dir_hr_q2 + results_hr$sc4_ind_hr_q2
Gesamteffekte_hr$hr_sc4_q2 <- c(sum(results_hr$sc4_dir_hr_q2),sum(results_hr$sc4_ind_hr_q2), sum(results_hr$sc4_tot_hr_q2))
results_hr$sc4_dir_hr_q3 <- numeric(2944) 
results_hr$sc4_dir_hr_q3[HR] <- shocks_scenario4[HR] * data_q3$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q3 <- results$scenario4 * data_q3$HR_P3_S14 *100
results_hr$sc4_tot_hr_q3 <- results_hr$sc4_dir_hr_q3 + results_hr$sc4_ind_hr_q3
Gesamteffekte_hr$hr_sc4_q3 <- c(sum(results_hr$sc4_dir_hr_q3),sum(results_hr$sc4_ind_hr_q3), sum(results_hr$sc4_tot_hr_q3))
results_hr$sc4_dir_hr_q4 <- numeric(2944) 
results_hr$sc4_dir_hr_q4[HR] <- shocks_scenario4[HR] * data_q4$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q4 <- results$scenario4 * data_q4$HR_P3_S14 *100
results_hr$sc4_tot_hr_q4 <- results_hr$sc4_dir_hr_q4 + results_hr$sc4_ind_hr_q4
Gesamteffekte_hr$hr_sc4_q4 <- c(sum(results_hr$sc4_dir_hr_q4),sum(results_hr$sc4_ind_hr_q4), sum(results_hr$sc4_tot_hr_q4))
results_hr$sc4_dir_hr_q5 <- numeric(2944) 
results_hr$sc4_dir_hr_q5[HR] <- shocks_scenario4[HR] * data_q5$HR_P3_S14[HR] *100
results_hr$sc4_ind_hr_q5 <- results$scenario4 * data_q5$HR_P3_S14 *100
results_hr$sc4_tot_hr_q5 <- results_hr$sc4_dir_hr_q5 + results_hr$sc4_ind_hr_q5
Gesamteffekte_hr$hr_sc4_q5 <- c(sum(results_hr$sc4_dir_hr_q5),sum(results_hr$sc4_ind_hr_q5), sum(results_hr$sc4_tot_hr_q5))


#Ungarn
#results df
results_hu <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_hu <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_hu$sc2_dir_hu_q1 <- numeric(2944)
results_hu$sc2_ind_hu_q1 <- results$scenario2 * data_q1$HU_P3_S14 *100
results_hu$sc2_tot_hu_q1 <- results_hu$sc2_dir_hu_q1 + results_hu$sc2_ind_hu_q1
Gesamteffekte_hu$hu_sc2_q1 <- c(sum(results_hu$sc2_dir_hu_q1),sum(results_hu$sc2_ind_hu_q1), sum(results_hu$sc2_tot_hu_q1))
results_hu$sc2_dir_hu_q2 <- numeric(2944)
results_hu$sc2_ind_hu_q2 <- results$scenario2 * data_q2$HU_P3_S14 *100
results_hu$sc2_tot_hu_q2 <- results_hu$sc2_dir_hu_q2 + results_hu$sc2_ind_hu_q2
Gesamteffekte_hu$hu_sc2_q2 <- c(sum(results_hu$sc2_dir_hu_q2),sum(results_hu$sc2_ind_hu_q2), sum(results_hu$sc2_tot_hu_q2))
results_hu$sc2_dir_hu_q3 <- numeric(2944)
results_hu$sc2_ind_hu_q3 <- results$scenario2 * data_q3$HU_P3_S14 *100
results_hu$sc2_tot_hu_q3 <- results_hu$sc2_dir_hu_q3 + results_hu$sc2_ind_hu_q3
Gesamteffekte_hu$hu_sc2_q3 <- c(sum(results_hu$sc2_dir_hu_q3),sum(results_hu$sc2_ind_hu_q3), sum(results_hu$sc2_tot_hu_q3))
results_hu$sc2_dir_hu_q4 <- numeric(2944)
results_hu$sc2_ind_hu_q4 <- results$scenario2 * data_q4$HU_P3_S14 *100
results_hu$sc2_tot_hu_q4 <- results_hu$sc2_dir_hu_q4 + results_hu$sc2_ind_hu_q4
Gesamteffekte_hu$hu_sc2_q4 <- c(sum(results_hu$sc2_dir_hu_q4),sum(results_hu$sc2_ind_hu_q4), sum(results_hu$sc2_tot_hu_q4))
results_hu$sc2_dir_hu_q5 <- numeric(2944)
results_hu$sc2_ind_hu_q5 <- results$scenario2 * data_q5$HU_P3_S14 *100
results_hu$sc2_tot_hu_q5 <- results_hu$sc2_dir_hu_q5 + results_hu$sc2_ind_hu_q5
Gesamteffekte_hu$hu_sc2_q5 <- c(sum(results_hu$sc2_dir_hu_q5),sum(results_hu$sc2_ind_hu_q5), sum(results_hu$sc2_tot_hu_q5))

#Szenario4
results_hu$sc4_dir_hu_q1 <- numeric(2944) 
results_hu$sc4_dir_hu_q1[HU] <- shocks_scenario4[HU] * data_q1$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q1 <- results$scenario4 * data_q1$HU_P3_S14 *100
results_hu$sc4_tot_hu_q1 <- results_hu$sc4_dir_hu_q1 + results_hu$sc4_ind_hu_q1
Gesamteffekte_hu$hu_sc4_q1 <- c(sum(results_hu$sc4_dir_hu_q1),sum(results_hu$sc4_ind_hu_q1), sum(results_hu$sc4_tot_hu_q1))
results_hu$sc4_dir_hu_q2 <- numeric(2944) 
results_hu$sc4_dir_hu_q2[HU] <- shocks_scenario4[HU] * data_q2$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q2 <- results$scenario4 * data_q2$HU_P3_S14 *100
results_hu$sc4_tot_hu_q2 <- results_hu$sc4_dir_hu_q2 + results_hu$sc4_ind_hu_q2
Gesamteffekte_hu$hu_sc4_q2 <- c(sum(results_hu$sc4_dir_hu_q2),sum(results_hu$sc4_ind_hu_q2), sum(results_hu$sc4_tot_hu_q2))
results_hu$sc4_dir_hu_q2 <- numeric(2944) 
results_hu$sc4_dir_hu_q2[HU] <- shocks_scenario4[HU] * data_q2$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q2 <- results$scenario4 * data_q2$HU_P3_S14 *100
results_hu$sc4_tot_hu_q2 <- results_hu$sc4_dir_hu_q2 + results_hu$sc4_ind_hu_q2
Gesamteffekte_hu$hu_sc4_q2 <- c(sum(results_hu$sc4_dir_hu_q2),sum(results_hu$sc4_ind_hu_q2), sum(results_hu$sc4_tot_hu_q2))
results_hu$sc4_dir_hu_q3 <- numeric(2944) 
results_hu$sc4_dir_hu_q3[HU] <- shocks_scenario4[HU] * data_q3$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q3 <- results$scenario4 * data_q3$HU_P3_S14 *100
results_hu$sc4_tot_hu_q3 <- results_hu$sc4_dir_hu_q3 + results_hu$sc4_ind_hu_q3
Gesamteffekte_hu$hu_sc4_q3 <- c(sum(results_hu$sc4_dir_hu_q3),sum(results_hu$sc4_ind_hu_q3), sum(results_hu$sc4_tot_hu_q3))
results_hu$sc4_dir_hu_q4 <- numeric(2944) 
results_hu$sc4_dir_hu_q4[HU] <- shocks_scenario4[HU] * data_q4$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q4 <- results$scenario4 * data_q4$HU_P3_S14 *100
results_hu$sc4_tot_hu_q4 <- results_hu$sc4_dir_hu_q4 + results_hu$sc4_ind_hu_q4
Gesamteffekte_hu$hu_sc4_q4 <- c(sum(results_hu$sc4_dir_hu_q4),sum(results_hu$sc4_ind_hu_q4), sum(results_hu$sc4_tot_hu_q4))
results_hu$sc4_dir_hu_q5 <- numeric(2944) 
results_hu$sc4_dir_hu_q5[HU] <- shocks_scenario4[HU] * data_q5$HU_P3_S14[HU] *100
results_hu$sc4_ind_hu_q5 <- results$scenario4 * data_q5$HU_P3_S14 *100
results_hu$sc4_tot_hu_q5 <- results_hu$sc4_dir_hu_q5 + results_hu$sc4_ind_hu_q5
Gesamteffekte_hu$hu_sc4_q5 <- c(sum(results_hu$sc4_dir_hu_q5),sum(results_hu$sc4_ind_hu_q5), sum(results_hu$sc4_tot_hu_q5))



#Litauen

#results df
results_lt <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_lt <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_lt$sc2_dir_lt_q1 <- numeric(2944)
results_lt$sc2_ind_lt_q1 <- results$scenario2 * data_q1$LT_P3_S14 *100
results_lt$sc2_tot_lt_q1 <- results_lt$sc2_dir_lt_q1 + results_lt$sc2_ind_lt_q1
Gesamteffekte_lt$lt_sc2_q1 <- c(sum(results_lt$sc2_dir_lt_q1),sum(results_lt$sc2_ind_lt_q1), sum(results_lt$sc2_tot_lt_q1))
results_lt$sc2_dir_lt_q2 <- numeric(2944)
results_lt$sc2_ind_lt_q2 <- results$scenario2 * data_q2$LT_P3_S14 *100
results_lt$sc2_tot_lt_q2 <- results_lt$sc2_dir_lt_q2 + results_lt$sc2_ind_lt_q2
Gesamteffekte_lt$lt_sc2_q2 <- c(sum(results_lt$sc2_dir_lt_q2),sum(results_lt$sc2_ind_lt_q2), sum(results_lt$sc2_tot_lt_q2))
results_lt$sc2_dir_lt_q3 <- numeric(2944)
results_lt$sc2_ind_lt_q3 <- results$scenario2 * data_q3$LT_P3_S14 *100
results_lt$sc2_tot_lt_q3 <- results_lt$sc2_dir_lt_q3 + results_lt$sc2_ind_lt_q3
Gesamteffekte_lt$lt_sc2_q3 <- c(sum(results_lt$sc2_dir_lt_q3),sum(results_lt$sc2_ind_lt_q3), sum(results_lt$sc2_tot_lt_q3))
results_lt$sc2_dir_lt_q4 <- numeric(2944)
results_lt$sc2_ind_lt_q4 <- results$scenario2 * data_q4$LT_P3_S14 *100
results_lt$sc2_tot_lt_q4 <- results_lt$sc2_dir_lt_q4 + results_lt$sc2_ind_lt_q4
Gesamteffekte_lt$lt_sc2_q4 <- c(sum(results_lt$sc2_dir_lt_q4),sum(results_lt$sc2_ind_lt_q4), sum(results_lt$sc2_tot_lt_q4))
results_lt$sc2_dir_lt_q5 <- numeric(2944)
results_lt$sc2_ind_lt_q5 <- results$scenario2 * data_q5$LT_P3_S14 *100
results_lt$sc2_tot_lt_q5 <- results_lt$sc2_dir_lt_q5 + results_lt$sc2_ind_lt_q5
Gesamteffekte_lt$lt_sc2_q5 <- c(sum(results_lt$sc2_dir_lt_q5),sum(results_lt$sc2_ind_lt_q5), sum(results_lt$sc2_tot_lt_q5))

#Szenario4
results_lt$sc4_dir_lt_q1 <- numeric(2944) 
results_lt$sc4_dir_lt_q1[LT] <- shocks_scenario4[LT] * data_q1$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q1 <- results$scenario4 * data_q1$LT_P3_S14 *100
results_lt$sc4_tot_lt_q1 <- results_lt$sc4_dir_lt_q1 + results_lt$sc4_ind_lt_q1
Gesamteffekte_lt$lt_sc4_q1 <- c(sum(results_lt$sc4_dir_lt_q1),sum(results_lt$sc4_ind_lt_q1), sum(results_lt$sc4_tot_lt_q1))
results_lt$sc4_dir_lt_q2 <- numeric(2944) 
results_lt$sc4_dir_lt_q2[LT] <- shocks_scenario4[LT] * data_q2$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q2 <- results$scenario4 * data_q2$LT_P3_S14 *100
results_lt$sc4_tot_lt_q2 <- results_lt$sc4_dir_lt_q2 + results_lt$sc4_ind_lt_q2
Gesamteffekte_lt$lt_sc4_q2 <- c(sum(results_lt$sc4_dir_lt_q2),sum(results_lt$sc4_ind_lt_q2), sum(results_lt$sc4_tot_lt_q2))
results_lt$sc4_dir_lt_q2 <- numeric(2944) 
results_lt$sc4_dir_lt_q2[LT] <- shocks_scenario4[LT] * data_q2$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q2 <- results$scenario4 * data_q2$LT_P3_S14 *100
results_lt$sc4_tot_lt_q2 <- results_lt$sc4_dir_lt_q2 + results_lt$sc4_ind_lt_q2
Gesamteffekte_lt$lt_sc4_q2 <- c(sum(results_lt$sc4_dir_lt_q2),sum(results_lt$sc4_ind_lt_q2), sum(results_lt$sc4_tot_lt_q2))
results_lt$sc4_dir_lt_q3 <- numeric(2944) 
results_lt$sc4_dir_lt_q3[LT] <- shocks_scenario4[LT] * data_q3$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q3 <- results$scenario4 * data_q3$LT_P3_S14 *100
results_lt$sc4_tot_lt_q3 <- results_lt$sc4_dir_lt_q3 + results_lt$sc4_ind_lt_q3
Gesamteffekte_lt$lt_sc4_q3 <- c(sum(results_lt$sc4_dir_lt_q3),sum(results_lt$sc4_ind_lt_q3), sum(results_lt$sc4_tot_lt_q3))
results_lt$sc4_dir_lt_q4 <- numeric(2944) 
results_lt$sc4_dir_lt_q4[LT] <- shocks_scenario4[LT] * data_q4$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q4 <- results$scenario4 * data_q4$LT_P3_S14 *100
results_lt$sc4_tot_lt_q4 <- results_lt$sc4_dir_lt_q4 + results_lt$sc4_ind_lt_q4
Gesamteffekte_lt$lt_sc4_q4 <- c(sum(results_lt$sc4_dir_lt_q4),sum(results_lt$sc4_ind_lt_q4), sum(results_lt$sc4_tot_lt_q4))
results_lt$sc4_dir_lt_q5 <- numeric(2944) 
results_lt$sc4_dir_lt_q5[LT] <- shocks_scenario4[LT] * data_q5$LT_P3_S14[LT] *100
results_lt$sc4_ind_lt_q5 <- results$scenario4 * data_q5$LT_P3_S14 *100
results_lt$sc4_tot_lt_q5 <- results_lt$sc4_dir_lt_q5 + results_lt$sc4_ind_lt_q5
Gesamteffekte_lt$lt_sc4_q5 <- c(sum(results_lt$sc4_dir_lt_q5),sum(results_lt$sc4_ind_lt_q5), sum(results_lt$sc4_tot_lt_q5))

#Luxemburg
#results df
results_lu <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_lu <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_lu$sc2_dir_lu_q1 <- numeric(2944)
results_lu$sc2_ind_lu_q1 <- results$scenario2 * data_q1$LU_P3_S14 *100
results_lu$sc2_tot_lu_q1 <- results_lu$sc2_dir_lu_q1 + results_lu$sc2_ind_lu_q1
Gesamteffekte_lu$lu_sc2_q1 <- c(sum(results_lu$sc2_dir_lu_q1),sum(results_lu$sc2_ind_lu_q1), sum(results_lu$sc2_tot_lu_q1))
results_lu$sc2_dir_lu_q2 <- numeric(2944)
results_lu$sc2_ind_lu_q2 <- results$scenario2 * data_q2$LU_P3_S14 *100
results_lu$sc2_tot_lu_q2 <- results_lu$sc2_dir_lu_q2 + results_lu$sc2_ind_lu_q2
Gesamteffekte_lu$lu_sc2_q2 <- c(sum(results_lu$sc2_dir_lu_q2),sum(results_lu$sc2_ind_lu_q2), sum(results_lu$sc2_tot_lu_q2))
results_lu$sc2_dir_lu_q3 <- numeric(2944)
results_lu$sc2_ind_lu_q3 <- results$scenario2 * data_q3$LU_P3_S14 *100
results_lu$sc2_tot_lu_q3 <- results_lu$sc2_dir_lu_q3 + results_lu$sc2_ind_lu_q3
Gesamteffekte_lu$lu_sc2_q3 <- c(sum(results_lu$sc2_dir_lu_q3),sum(results_lu$sc2_ind_lu_q3), sum(results_lu$sc2_tot_lu_q3))
results_lu$sc2_dir_lu_q4 <- numeric(2944)
results_lu$sc2_ind_lu_q4 <- results$scenario2 * data_q4$LU_P3_S14 *100
results_lu$sc2_tot_lu_q4 <- results_lu$sc2_dir_lu_q4 + results_lu$sc2_ind_lu_q4
Gesamteffekte_lu$lu_sc2_q4 <- c(sum(results_lu$sc2_dir_lu_q4),sum(results_lu$sc2_ind_lu_q4), sum(results_lu$sc2_tot_lu_q4))
results_lu$sc2_dir_lu_q5 <- numeric(2944)
results_lu$sc2_ind_lu_q5 <- results$scenario2 * data_q5$LU_P3_S14 *100
results_lu$sc2_tot_lu_q5 <- results_lu$sc2_dir_lu_q5 + results_lu$sc2_ind_lu_q5
Gesamteffekte_lu$lu_sc2_q5 <- c(sum(results_lu$sc2_dir_lu_q5),sum(results_lu$sc2_ind_lu_q5), sum(results_lu$sc2_tot_lu_q5))

#Szenario4
results_lu$sc4_dir_lu_q1 <- numeric(2944) 
results_lu$sc4_dir_lu_q1[LU] <- shocks_scenario4[LU] * data_q1$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q1 <- results$scenario4 * data_q1$LU_P3_S14 *100
results_lu$sc4_tot_lu_q1 <- results_lu$sc4_dir_lu_q1 + results_lu$sc4_ind_lu_q1
Gesamteffekte_lu$lu_sc4_q1 <- c(sum(results_lu$sc4_dir_lu_q1),sum(results_lu$sc4_ind_lu_q1), sum(results_lu$sc4_tot_lu_q1))
results_lu$sc4_dir_lu_q2 <- numeric(2944) 
results_lu$sc4_dir_lu_q2[LU] <- shocks_scenario4[LU] * data_q2$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q2 <- results$scenario4 * data_q2$LU_P3_S14 *100
results_lu$sc4_tot_lu_q2 <- results_lu$sc4_dir_lu_q2 + results_lu$sc4_ind_lu_q2
Gesamteffekte_lu$lu_sc4_q2 <- c(sum(results_lu$sc4_dir_lu_q2),sum(results_lu$sc4_ind_lu_q2), sum(results_lu$sc4_tot_lu_q2))
results_lu$sc4_dir_lu_q2 <- numeric(2944) 
results_lu$sc4_dir_lu_q2[LU] <- shocks_scenario4[LU] * data_q2$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q2 <- results$scenario4 * data_q2$LU_P3_S14 *100
results_lu$sc4_tot_lu_q2 <- results_lu$sc4_dir_lu_q2 + results_lu$sc4_ind_lu_q2
Gesamteffekte_lu$lu_sc4_q2 <- c(sum(results_lu$sc4_dir_lu_q2),sum(results_lu$sc4_ind_lu_q2), sum(results_lu$sc4_tot_lu_q2))
results_lu$sc4_dir_lu_q3 <- numeric(2944) 
results_lu$sc4_dir_lu_q3[LU] <- shocks_scenario4[LU] * data_q3$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q3 <- results$scenario4 * data_q3$LU_P3_S14 *100
results_lu$sc4_tot_lu_q3 <- results_lu$sc4_dir_lu_q3 + results_lu$sc4_ind_lu_q3
Gesamteffekte_lu$lu_sc4_q3 <- c(sum(results_lu$sc4_dir_lu_q3),sum(results_lu$sc4_ind_lu_q3), sum(results_lu$sc4_tot_lu_q3))
results_lu$sc4_dir_lu_q4 <- numeric(2944) 
results_lu$sc4_dir_lu_q4[LU] <- shocks_scenario4[LU] * data_q4$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q4 <- results$scenario4 * data_q4$LU_P3_S14 *100
results_lu$sc4_tot_lu_q4 <- results_lu$sc4_dir_lu_q4 + results_lu$sc4_ind_lu_q4
Gesamteffekte_lu$lu_sc4_q4 <- c(sum(results_lu$sc4_dir_lu_q4),sum(results_lu$sc4_ind_lu_q4), sum(results_lu$sc4_tot_lu_q4))
results_lu$sc4_dir_lu_q5 <- numeric(2944) 
results_lu$sc4_dir_lu_q5[LU] <- shocks_scenario4[LU] * data_q5$LU_P3_S14[LU] *100
results_lu$sc4_ind_lu_q5 <- results$scenario4 * data_q5$LU_P3_S14 *100
results_lu$sc4_tot_lu_q5 <- results_lu$sc4_dir_lu_q5 + results_lu$sc4_ind_lu_q5
Gesamteffekte_lu$lu_sc4_q5 <- c(sum(results_lu$sc4_dir_lu_q5),sum(results_lu$sc4_ind_lu_q5), sum(results_lu$sc4_tot_lu_q5))



#Lettland
#results df
results_lv <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_lv <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_lv$sc2_dir_lv_q1 <- numeric(2944)
results_lv$sc2_ind_lv_q1 <- results$scenario2 * data_q1$LV_P3_S14 *100
results_lv$sc2_tot_lv_q1 <- results_lv$sc2_dir_lv_q1 + results_lv$sc2_ind_lv_q1
Gesamteffekte_lv$lv_sc2_q1 <- c(sum(results_lv$sc2_dir_lv_q1),sum(results_lv$sc2_ind_lv_q1), sum(results_lv$sc2_tot_lv_q1))
results_lv$sc2_dir_lv_q2 <- numeric(2944)
results_lv$sc2_ind_lv_q2 <- results$scenario2 * data_q2$LV_P3_S14 *100
results_lv$sc2_tot_lv_q2 <- results_lv$sc2_dir_lv_q2 + results_lv$sc2_ind_lv_q2
Gesamteffekte_lv$lv_sc2_q2 <- c(sum(results_lv$sc2_dir_lv_q2),sum(results_lv$sc2_ind_lv_q2), sum(results_lv$sc2_tot_lv_q2))
results_lv$sc2_dir_lv_q3 <- numeric(2944)
results_lv$sc2_ind_lv_q3 <- results$scenario2 * data_q3$LV_P3_S14 *100
results_lv$sc2_tot_lv_q3 <- results_lv$sc2_dir_lv_q3 + results_lv$sc2_ind_lv_q3
Gesamteffekte_lv$lv_sc2_q3 <- c(sum(results_lv$sc2_dir_lv_q3),sum(results_lv$sc2_ind_lv_q3), sum(results_lv$sc2_tot_lv_q3))
results_lv$sc2_dir_lv_q4 <- numeric(2944)
results_lv$sc2_ind_lv_q4 <- results$scenario2 * data_q4$LV_P3_S14 *100
results_lv$sc2_tot_lv_q4 <- results_lv$sc2_dir_lv_q4 + results_lv$sc2_ind_lv_q4
Gesamteffekte_lv$lv_sc2_q4 <- c(sum(results_lv$sc2_dir_lv_q4),sum(results_lv$sc2_ind_lv_q4), sum(results_lv$sc2_tot_lv_q4))
results_lv$sc2_dir_lv_q5 <- numeric(2944)
results_lv$sc2_ind_lv_q5 <- results$scenario2 * data_q5$LV_P3_S14 *100
results_lv$sc2_tot_lv_q5 <- results_lv$sc2_dir_lv_q5 + results_lv$sc2_ind_lv_q5
Gesamteffekte_lv$lv_sc2_q5 <- c(sum(results_lv$sc2_dir_lv_q5),sum(results_lv$sc2_ind_lv_q5), sum(results_lv$sc2_tot_lv_q5))

#Szenario4
results_lv$sc4_dir_lv_q1 <- numeric(2944) 
results_lv$sc4_dir_lv_q1[LV] <- shocks_scenario4[LV] * data_q1$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q1 <- results$scenario4 * data_q1$LV_P3_S14 *100
results_lv$sc4_tot_lv_q1 <- results_lv$sc4_dir_lv_q1 + results_lv$sc4_ind_lv_q1
Gesamteffekte_lv$lv_sc4_q1 <- c(sum(results_lv$sc4_dir_lv_q1),sum(results_lv$sc4_ind_lv_q1), sum(results_lv$sc4_tot_lv_q1))
results_lv$sc4_dir_lv_q2 <- numeric(2944) 
results_lv$sc4_dir_lv_q2[LV] <- shocks_scenario4[LV] * data_q2$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q2 <- results$scenario4 * data_q2$LV_P3_S14 *100
results_lv$sc4_tot_lv_q2 <- results_lv$sc4_dir_lv_q2 + results_lv$sc4_ind_lv_q2
Gesamteffekte_lv$lv_sc4_q2 <- c(sum(results_lv$sc4_dir_lv_q2),sum(results_lv$sc4_ind_lv_q2), sum(results_lv$sc4_tot_lv_q2))
results_lv$sc4_dir_lv_q2 <- numeric(2944) 
results_lv$sc4_dir_lv_q2[LV] <- shocks_scenario4[LV] * data_q2$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q2 <- results$scenario4 * data_q2$LV_P3_S14 *100
results_lv$sc4_tot_lv_q2 <- results_lv$sc4_dir_lv_q2 + results_lv$sc4_ind_lv_q2
Gesamteffekte_lv$lv_sc4_q2 <- c(sum(results_lv$sc4_dir_lv_q2),sum(results_lv$sc4_ind_lv_q2), sum(results_lv$sc4_tot_lv_q2))
results_lv$sc4_dir_lv_q3 <- numeric(2944) 
results_lv$sc4_dir_lv_q3[LV] <- shocks_scenario4[LV] * data_q3$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q3 <- results$scenario4 * data_q3$LV_P3_S14 *100
results_lv$sc4_tot_lv_q3 <- results_lv$sc4_dir_lv_q3 + results_lv$sc4_ind_lv_q3
Gesamteffekte_lv$lv_sc4_q3 <- c(sum(results_lv$sc4_dir_lv_q3),sum(results_lv$sc4_ind_lv_q3), sum(results_lv$sc4_tot_lv_q3))
results_lv$sc4_dir_lv_q4 <- numeric(2944) 
results_lv$sc4_dir_lv_q4[LV] <- shocks_scenario4[LV] * data_q4$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q4 <- results$scenario4 * data_q4$LV_P3_S14 *100
results_lv$sc4_tot_lv_q4 <- results_lv$sc4_dir_lv_q4 + results_lv$sc4_ind_lv_q4
Gesamteffekte_lv$lv_sc4_q4 <- c(sum(results_lv$sc4_dir_lv_q4),sum(results_lv$sc4_ind_lv_q4), sum(results_lv$sc4_tot_lv_q4))
results_lv$sc4_dir_lv_q5 <- numeric(2944) 
results_lv$sc4_dir_lv_q5[LV] <- shocks_scenario4[LV] * data_q5$LV_P3_S14[LV] *100
results_lv$sc4_ind_lv_q5 <- results$scenario4 * data_q5$LV_P3_S14 *100
results_lv$sc4_tot_lv_q5 <- results_lv$sc4_dir_lv_q5 + results_lv$sc4_ind_lv_q5
Gesamteffekte_lv$lv_sc4_q5 <- c(sum(results_lv$sc4_dir_lv_q5),sum(results_lv$sc4_ind_lv_q5), sum(results_lv$sc4_tot_lv_q5))


#Malta
#results df
results_mt <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_mt <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_mt$sc2_dir_mt_q1 <- numeric(2944)
results_mt$sc2_ind_mt_q1 <- results$scenario2 * data_q1$MT_P3_S14 *100
results_mt$sc2_tot_mt_q1 <- results_mt$sc2_dir_mt_q1 + results_mt$sc2_ind_mt_q1
Gesamteffekte_mt$mt_sc2_q1 <- c(sum(results_mt$sc2_dir_mt_q1),sum(results_mt$sc2_ind_mt_q1), sum(results_mt$sc2_tot_mt_q1))
results_mt$sc2_dir_mt_q2 <- numeric(2944)
results_mt$sc2_ind_mt_q2 <- results$scenario2 * data_q2$MT_P3_S14 *100
results_mt$sc2_tot_mt_q2 <- results_mt$sc2_dir_mt_q2 + results_mt$sc2_ind_mt_q2
Gesamteffekte_mt$mt_sc2_q2 <- c(sum(results_mt$sc2_dir_mt_q2),sum(results_mt$sc2_ind_mt_q2), sum(results_mt$sc2_tot_mt_q2))
results_mt$sc2_dir_mt_q3 <- numeric(2944)
results_mt$sc2_ind_mt_q3 <- results$scenario2 * data_q3$MT_P3_S14 *100
results_mt$sc2_tot_mt_q3 <- results_mt$sc2_dir_mt_q3 + results_mt$sc2_ind_mt_q3
Gesamteffekte_mt$mt_sc2_q3 <- c(sum(results_mt$sc2_dir_mt_q3),sum(results_mt$sc2_ind_mt_q3), sum(results_mt$sc2_tot_mt_q3))
results_mt$sc2_dir_mt_q4 <- numeric(2944)
results_mt$sc2_ind_mt_q4 <- results$scenario2 * data_q4$MT_P3_S14 *100
results_mt$sc2_tot_mt_q4 <- results_mt$sc2_dir_mt_q4 + results_mt$sc2_ind_mt_q4
Gesamteffekte_mt$mt_sc2_q4 <- c(sum(results_mt$sc2_dir_mt_q4),sum(results_mt$sc2_ind_mt_q4), sum(results_mt$sc2_tot_mt_q4))
results_mt$sc2_dir_mt_q5 <- numeric(2944)
results_mt$sc2_ind_mt_q5 <- results$scenario2 * data_q5$MT_P3_S14 *100
results_mt$sc2_tot_mt_q5 <- results_mt$sc2_dir_mt_q5 + results_mt$sc2_ind_mt_q5
Gesamteffekte_mt$mt_sc2_q5 <- c(sum(results_mt$sc2_dir_mt_q5),sum(results_mt$sc2_ind_mt_q5), sum(results_mt$sc2_tot_mt_q5))

#Szenario4
results_mt$sc4_dir_mt_q1 <- numeric(2944) 
results_mt$sc4_dir_mt_q1[MT] <- shocks_scenario4[MT] * data_q1$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q1 <- results$scenario4 * data_q1$MT_P3_S14 *100
results_mt$sc4_tot_mt_q1 <- results_mt$sc4_dir_mt_q1 + results_mt$sc4_ind_mt_q1
Gesamteffekte_mt$mt_sc4_q1 <- c(sum(results_mt$sc4_dir_mt_q1),sum(results_mt$sc4_ind_mt_q1), sum(results_mt$sc4_tot_mt_q1))
results_mt$sc4_dir_mt_q2 <- numeric(2944) 
results_mt$sc4_dir_mt_q2[MT] <- shocks_scenario4[MT] * data_q2$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q2 <- results$scenario4 * data_q2$MT_P3_S14 *100
results_mt$sc4_tot_mt_q2 <- results_mt$sc4_dir_mt_q2 + results_mt$sc4_ind_mt_q2
Gesamteffekte_mt$mt_sc4_q2 <- c(sum(results_mt$sc4_dir_mt_q2),sum(results_mt$sc4_ind_mt_q2), sum(results_mt$sc4_tot_mt_q2))
results_mt$sc4_dir_mt_q2 <- numeric(2944) 
results_mt$sc4_dir_mt_q2[MT] <- shocks_scenario4[MT] * data_q2$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q2 <- results$scenario4 * data_q2$MT_P3_S14 *100
results_mt$sc4_tot_mt_q2 <- results_mt$sc4_dir_mt_q2 + results_mt$sc4_ind_mt_q2
Gesamteffekte_mt$mt_sc4_q2 <- c(sum(results_mt$sc4_dir_mt_q2),sum(results_mt$sc4_ind_mt_q2), sum(results_mt$sc4_tot_mt_q2))
results_mt$sc4_dir_mt_q3 <- numeric(2944) 
results_mt$sc4_dir_mt_q3[MT] <- shocks_scenario4[MT] * data_q3$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q3 <- results$scenario4 * data_q3$MT_P3_S14 *100
results_mt$sc4_tot_mt_q3 <- results_mt$sc4_dir_mt_q3 + results_mt$sc4_ind_mt_q3
Gesamteffekte_mt$mt_sc4_q3 <- c(sum(results_mt$sc4_dir_mt_q3),sum(results_mt$sc4_ind_mt_q3), sum(results_mt$sc4_tot_mt_q3))
results_mt$sc4_dir_mt_q4 <- numeric(2944) 
results_mt$sc4_dir_mt_q4[MT] <- shocks_scenario4[MT] * data_q4$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q4 <- results$scenario4 * data_q4$MT_P3_S14 *100
results_mt$sc4_tot_mt_q4 <- results_mt$sc4_dir_mt_q4 + results_mt$sc4_ind_mt_q4
Gesamteffekte_mt$mt_sc4_q4 <- c(sum(results_mt$sc4_dir_mt_q4),sum(results_mt$sc4_ind_mt_q4), sum(results_mt$sc4_tot_mt_q4))
results_mt$sc4_dir_mt_q5 <- numeric(2944) 
results_mt$sc4_dir_mt_q5[MT] <- shocks_scenario4[MT] * data_q5$MT_P3_S14[MT] *100
results_mt$sc4_ind_mt_q5 <- results$scenario4 * data_q5$MT_P3_S14 *100
results_mt$sc4_tot_mt_q5 <- results_mt$sc4_dir_mt_q5 + results_mt$sc4_ind_mt_q5
Gesamteffekte_mt$mt_sc4_q5 <- c(sum(results_mt$sc4_dir_mt_q5),sum(results_mt$sc4_ind_mt_q5), sum(results_mt$sc4_tot_mt_q5))

#Niederlande
#results df
results_nl <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_nl <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_nl$sc2_dir_nl_q1 <- numeric(2944)
results_nl$sc2_ind_nl_q1 <- results$scenario2 * data_q1$NL_P3_S14 *100
results_nl$sc2_tot_nl_q1 <- results_nl$sc2_dir_nl_q1 + results_nl$sc2_ind_nl_q1
Gesamteffekte_nl$nl_sc2_q1 <- c(sum(results_nl$sc2_dir_nl_q1),sum(results_nl$sc2_ind_nl_q1), sum(results_nl$sc2_tot_nl_q1))
results_nl$sc2_dir_nl_q2 <- numeric(2944)
results_nl$sc2_ind_nl_q2 <- results$scenario2 * data_q2$NL_P3_S14 *100
results_nl$sc2_tot_nl_q2 <- results_nl$sc2_dir_nl_q2 + results_nl$sc2_ind_nl_q2
Gesamteffekte_nl$nl_sc2_q2 <- c(sum(results_nl$sc2_dir_nl_q2),sum(results_nl$sc2_ind_nl_q2), sum(results_nl$sc2_tot_nl_q2))
results_nl$sc2_dir_nl_q3 <- numeric(2944)
results_nl$sc2_ind_nl_q3 <- results$scenario2 * data_q3$NL_P3_S14 *100
results_nl$sc2_tot_nl_q3 <- results_nl$sc2_dir_nl_q3 + results_nl$sc2_ind_nl_q3
Gesamteffekte_nl$nl_sc2_q3 <- c(sum(results_nl$sc2_dir_nl_q3),sum(results_nl$sc2_ind_nl_q3), sum(results_nl$sc2_tot_nl_q3))
results_nl$sc2_dir_nl_q4 <- numeric(2944)
results_nl$sc2_ind_nl_q4 <- results$scenario2 * data_q4$NL_P3_S14 *100
results_nl$sc2_tot_nl_q4 <- results_nl$sc2_dir_nl_q4 + results_nl$sc2_ind_nl_q4
Gesamteffekte_nl$nl_sc2_q4 <- c(sum(results_nl$sc2_dir_nl_q4),sum(results_nl$sc2_ind_nl_q4), sum(results_nl$sc2_tot_nl_q4))
results_nl$sc2_dir_nl_q5 <- numeric(2944)
results_nl$sc2_ind_nl_q5 <- results$scenario2 * data_q5$NL_P3_S14 *100
results_nl$sc2_tot_nl_q5 <- results_nl$sc2_dir_nl_q5 + results_nl$sc2_ind_nl_q5
Gesamteffekte_nl$nl_sc2_q5 <- c(sum(results_nl$sc2_dir_nl_q5),sum(results_nl$sc2_ind_nl_q5), sum(results_nl$sc2_tot_nl_q5))

#Szenario4
results_nl$sc4_dir_nl_q1 <- numeric(2944) 
results_nl$sc4_dir_nl_q1[NL] <- shocks_scenario4[NL] * data_q1$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q1 <- results$scenario4 * data_q1$NL_P3_S14 *100
results_nl$sc4_tot_nl_q1 <- results_nl$sc4_dir_nl_q1 + results_nl$sc4_ind_nl_q1
Gesamteffekte_nl$nl_sc4_q1 <- c(sum(results_nl$sc4_dir_nl_q1),sum(results_nl$sc4_ind_nl_q1), sum(results_nl$sc4_tot_nl_q1))
results_nl$sc4_dir_nl_q2 <- numeric(2944) 
results_nl$sc4_dir_nl_q2[NL] <- shocks_scenario4[NL] * data_q2$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q2 <- results$scenario4 * data_q2$NL_P3_S14 *100
results_nl$sc4_tot_nl_q2 <- results_nl$sc4_dir_nl_q2 + results_nl$sc4_ind_nl_q2
Gesamteffekte_nl$nl_sc4_q2 <- c(sum(results_nl$sc4_dir_nl_q2),sum(results_nl$sc4_ind_nl_q2), sum(results_nl$sc4_tot_nl_q2))
results_nl$sc4_dir_nl_q2 <- numeric(2944) 
results_nl$sc4_dir_nl_q2[NL] <- shocks_scenario4[NL] * data_q2$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q2 <- results$scenario4 * data_q2$NL_P3_S14 *100
results_nl$sc4_tot_nl_q2 <- results_nl$sc4_dir_nl_q2 + results_nl$sc4_ind_nl_q2
Gesamteffekte_nl$nl_sc4_q2 <- c(sum(results_nl$sc4_dir_nl_q2),sum(results_nl$sc4_ind_nl_q2), sum(results_nl$sc4_tot_nl_q2))
results_nl$sc4_dir_nl_q3 <- numeric(2944) 
results_nl$sc4_dir_nl_q3[NL] <- shocks_scenario4[NL] * data_q3$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q3 <- results$scenario4 * data_q3$NL_P3_S14 *100
results_nl$sc4_tot_nl_q3 <- results_nl$sc4_dir_nl_q3 + results_nl$sc4_ind_nl_q3
Gesamteffekte_nl$nl_sc4_q3 <- c(sum(results_nl$sc4_dir_nl_q3),sum(results_nl$sc4_ind_nl_q3), sum(results_nl$sc4_tot_nl_q3))
results_nl$sc4_dir_nl_q4 <- numeric(2944) 
results_nl$sc4_dir_nl_q4[NL] <- shocks_scenario4[NL] * data_q4$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q4 <- results$scenario4 * data_q4$NL_P3_S14 *100
results_nl$sc4_tot_nl_q4 <- results_nl$sc4_dir_nl_q4 + results_nl$sc4_ind_nl_q4
Gesamteffekte_nl$nl_sc4_q4 <- c(sum(results_nl$sc4_dir_nl_q4),sum(results_nl$sc4_ind_nl_q4), sum(results_nl$sc4_tot_nl_q4))
results_nl$sc4_dir_nl_q5 <- numeric(2944) 
results_nl$sc4_dir_nl_q5[NL] <- shocks_scenario4[NL] * data_q5$NL_P3_S14[NL] *100
results_nl$sc4_ind_nl_q5 <- results$scenario4 * data_q5$NL_P3_S14 *100
results_nl$sc4_tot_nl_q5 <- results_nl$sc4_dir_nl_q5 + results_nl$sc4_ind_nl_q5
Gesamteffekte_nl$nl_sc4_q5 <- c(sum(results_nl$sc4_dir_nl_q5),sum(results_nl$sc4_ind_nl_q5), sum(results_nl$sc4_tot_nl_q5))



#Polen
#results df
results_pl <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_pl <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_pl$sc2_dir_pl_q1 <- numeric(2944)
results_pl$sc2_ind_pl_q1 <- results$scenario2 * data_q1$PL_P3_S14 *100
results_pl$sc2_tot_pl_q1 <- results_pl$sc2_dir_pl_q1 + results_pl$sc2_ind_pl_q1
Gesamteffekte_pl$pl_sc2_q1 <- c(sum(results_pl$sc2_dir_pl_q1),sum(results_pl$sc2_ind_pl_q1), sum(results_pl$sc2_tot_pl_q1))
results_pl$sc2_dir_pl_q2 <- numeric(2944)
results_pl$sc2_ind_pl_q2 <- results$scenario2 * data_q2$PL_P3_S14 *100
results_pl$sc2_tot_pl_q2 <- results_pl$sc2_dir_pl_q2 + results_pl$sc2_ind_pl_q2
Gesamteffekte_pl$pl_sc2_q2 <- c(sum(results_pl$sc2_dir_pl_q2),sum(results_pl$sc2_ind_pl_q2), sum(results_pl$sc2_tot_pl_q2))
results_pl$sc2_dir_pl_q3 <- numeric(2944)
results_pl$sc2_ind_pl_q3 <- results$scenario2 * data_q3$PL_P3_S14 *100
results_pl$sc2_tot_pl_q3 <- results_pl$sc2_dir_pl_q3 + results_pl$sc2_ind_pl_q3
Gesamteffekte_pl$pl_sc2_q3 <- c(sum(results_pl$sc2_dir_pl_q3),sum(results_pl$sc2_ind_pl_q3), sum(results_pl$sc2_tot_pl_q3))
results_pl$sc2_dir_pl_q4 <- numeric(2944)
results_pl$sc2_ind_pl_q4 <- results$scenario2 * data_q4$PL_P3_S14 *100
results_pl$sc2_tot_pl_q4 <- results_pl$sc2_dir_pl_q4 + results_pl$sc2_ind_pl_q4
Gesamteffekte_pl$pl_sc2_q4 <- c(sum(results_pl$sc2_dir_pl_q4),sum(results_pl$sc2_ind_pl_q4), sum(results_pl$sc2_tot_pl_q4))
results_pl$sc2_dir_pl_q5 <- numeric(2944)
results_pl$sc2_ind_pl_q5 <- results$scenario2 * data_q5$PL_P3_S14 *100
results_pl$sc2_tot_pl_q5 <- results_pl$sc2_dir_pl_q5 + results_pl$sc2_ind_pl_q5
Gesamteffekte_pl$pl_sc2_q5 <- c(sum(results_pl$sc2_dir_pl_q5),sum(results_pl$sc2_ind_pl_q5), sum(results_pl$sc2_tot_pl_q5))

#Szenario4
results_pl$sc4_dir_pl_q1 <- numeric(2944) 
results_pl$sc4_dir_pl_q1[PL] <- shocks_scenario4[PL] * data_q1$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q1 <- results$scenario4 * data_q1$PL_P3_S14 *100
results_pl$sc4_tot_pl_q1 <- results_pl$sc4_dir_pl_q1 + results_pl$sc4_ind_pl_q1
Gesamteffekte_pl$pl_sc4_q1 <- c(sum(results_pl$sc4_dir_pl_q1),sum(results_pl$sc4_ind_pl_q1), sum(results_pl$sc4_tot_pl_q1))
results_pl$sc4_dir_pl_q2 <- numeric(2944) 
results_pl$sc4_dir_pl_q2[PL] <- shocks_scenario4[PL] * data_q2$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q2 <- results$scenario4 * data_q2$PL_P3_S14 *100
results_pl$sc4_tot_pl_q2 <- results_pl$sc4_dir_pl_q2 + results_pl$sc4_ind_pl_q2
Gesamteffekte_pl$pl_sc4_q2 <- c(sum(results_pl$sc4_dir_pl_q2),sum(results_pl$sc4_ind_pl_q2), sum(results_pl$sc4_tot_pl_q2))
results_pl$sc4_dir_pl_q2 <- numeric(2944) 
results_pl$sc4_dir_pl_q2[PL] <- shocks_scenario4[PL] * data_q2$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q2 <- results$scenario4 * data_q2$PL_P3_S14 *100
results_pl$sc4_tot_pl_q2 <- results_pl$sc4_dir_pl_q2 + results_pl$sc4_ind_pl_q2
Gesamteffekte_pl$pl_sc4_q2 <- c(sum(results_pl$sc4_dir_pl_q2),sum(results_pl$sc4_ind_pl_q2), sum(results_pl$sc4_tot_pl_q2))
results_pl$sc4_dir_pl_q3 <- numeric(2944) 
results_pl$sc4_dir_pl_q3[PL] <- shocks_scenario4[PL] * data_q3$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q3 <- results$scenario4 * data_q3$PL_P3_S14 *100
results_pl$sc4_tot_pl_q3 <- results_pl$sc4_dir_pl_q3 + results_pl$sc4_ind_pl_q3
Gesamteffekte_pl$pl_sc4_q3 <- c(sum(results_pl$sc4_dir_pl_q3),sum(results_pl$sc4_ind_pl_q3), sum(results_pl$sc4_tot_pl_q3))
results_pl$sc4_dir_pl_q4 <- numeric(2944) 
results_pl$sc4_dir_pl_q4[PL] <- shocks_scenario4[PL] * data_q4$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q4 <- results$scenario4 * data_q4$PL_P3_S14 *100
results_pl$sc4_tot_pl_q4 <- results_pl$sc4_dir_pl_q4 + results_pl$sc4_ind_pl_q4
Gesamteffekte_pl$pl_sc4_q4 <- c(sum(results_pl$sc4_dir_pl_q4),sum(results_pl$sc4_ind_pl_q4), sum(results_pl$sc4_tot_pl_q4))
results_pl$sc4_dir_pl_q5 <- numeric(2944) 
results_pl$sc4_dir_pl_q5[PL] <- shocks_scenario4[PL] * data_q5$PL_P3_S14[PL] *100
results_pl$sc4_ind_pl_q5 <- results$scenario4 * data_q5$PL_P3_S14 *100
results_pl$sc4_tot_pl_q5 <- results_pl$sc4_dir_pl_q5 + results_pl$sc4_ind_pl_q5
Gesamteffekte_pl$pl_sc4_q5 <- c(sum(results_pl$sc4_dir_pl_q5),sum(results_pl$sc4_ind_pl_q5), sum(results_pl$sc4_tot_pl_q5))

#Rumänien
#results df
results_ro <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_ro <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_ro$sc2_dir_ro_q1 <- numeric(2944)
results_ro$sc2_ind_ro_q1 <- results$scenario2 * data_q1$RO_P3_S14 *100
results_ro$sc2_tot_ro_q1 <- results_ro$sc2_dir_ro_q1 + results_ro$sc2_ind_ro_q1
Gesamteffekte_ro$ro_sc2_q1 <- c(sum(results_ro$sc2_dir_ro_q1),sum(results_ro$sc2_ind_ro_q1), sum(results_ro$sc2_tot_ro_q1))
results_ro$sc2_dir_ro_q2 <- numeric(2944)
results_ro$sc2_ind_ro_q2 <- results$scenario2 * data_q2$RO_P3_S14 *100
results_ro$sc2_tot_ro_q2 <- results_ro$sc2_dir_ro_q2 + results_ro$sc2_ind_ro_q2
Gesamteffekte_ro$ro_sc2_q2 <- c(sum(results_ro$sc2_dir_ro_q2),sum(results_ro$sc2_ind_ro_q2), sum(results_ro$sc2_tot_ro_q2))
results_ro$sc2_dir_ro_q3 <- numeric(2944)
results_ro$sc2_ind_ro_q3 <- results$scenario2 * data_q3$RO_P3_S14 *100
results_ro$sc2_tot_ro_q3 <- results_ro$sc2_dir_ro_q3 + results_ro$sc2_ind_ro_q3
Gesamteffekte_ro$ro_sc2_q3 <- c(sum(results_ro$sc2_dir_ro_q3),sum(results_ro$sc2_ind_ro_q3), sum(results_ro$sc2_tot_ro_q3))
results_ro$sc2_dir_ro_q4 <- numeric(2944)
results_ro$sc2_ind_ro_q4 <- results$scenario2 * data_q4$RO_P3_S14 *100
results_ro$sc2_tot_ro_q4 <- results_ro$sc2_dir_ro_q4 + results_ro$sc2_ind_ro_q4
Gesamteffekte_ro$ro_sc2_q4 <- c(sum(results_ro$sc2_dir_ro_q4),sum(results_ro$sc2_ind_ro_q4), sum(results_ro$sc2_tot_ro_q4))
results_ro$sc2_dir_ro_q5 <- numeric(2944)
results_ro$sc2_ind_ro_q5 <- results$scenario2 * data_q5$RO_P3_S14 *100
results_ro$sc2_tot_ro_q5 <- results_ro$sc2_dir_ro_q5 + results_ro$sc2_ind_ro_q5
Gesamteffekte_ro$ro_sc2_q5 <- c(sum(results_ro$sc2_dir_ro_q5),sum(results_ro$sc2_ind_ro_q5), sum(results_ro$sc2_tot_ro_q5))

#Szenario4
results_ro$sc4_dir_ro_q1 <- numeric(2944) 
results_ro$sc4_dir_ro_q1[RO] <- shocks_scenario4[RO] * data_q1$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q1 <- results$scenario4 * data_q1$RO_P3_S14 *100
results_ro$sc4_tot_ro_q1 <- results_ro$sc4_dir_ro_q1 + results_ro$sc4_ind_ro_q1
Gesamteffekte_ro$ro_sc4_q1 <- c(sum(results_ro$sc4_dir_ro_q1),sum(results_ro$sc4_ind_ro_q1), sum(results_ro$sc4_tot_ro_q1))
results_ro$sc4_dir_ro_q2 <- numeric(2944) 
results_ro$sc4_dir_ro_q2[RO] <- shocks_scenario4[RO] * data_q2$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q2 <- results$scenario4 * data_q2$RO_P3_S14 *100
results_ro$sc4_tot_ro_q2 <- results_ro$sc4_dir_ro_q2 + results_ro$sc4_ind_ro_q2
Gesamteffekte_ro$ro_sc4_q2 <- c(sum(results_ro$sc4_dir_ro_q2),sum(results_ro$sc4_ind_ro_q2), sum(results_ro$sc4_tot_ro_q2))
results_ro$sc4_dir_ro_q2 <- numeric(2944) 
results_ro$sc4_dir_ro_q2[RO] <- shocks_scenario4[RO] * data_q2$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q2 <- results$scenario4 * data_q2$RO_P3_S14 *100
results_ro$sc4_tot_ro_q2 <- results_ro$sc4_dir_ro_q2 + results_ro$sc4_ind_ro_q2
Gesamteffekte_ro$ro_sc4_q2 <- c(sum(results_ro$sc4_dir_ro_q2),sum(results_ro$sc4_ind_ro_q2), sum(results_ro$sc4_tot_ro_q2))
results_ro$sc4_dir_ro_q3 <- numeric(2944) 
results_ro$sc4_dir_ro_q3[RO] <- shocks_scenario4[RO] * data_q3$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q3 <- results$scenario4 * data_q3$RO_P3_S14 *100
results_ro$sc4_tot_ro_q3 <- results_ro$sc4_dir_ro_q3 + results_ro$sc4_ind_ro_q3
Gesamteffekte_ro$ro_sc4_q3 <- c(sum(results_ro$sc4_dir_ro_q3),sum(results_ro$sc4_ind_ro_q3), sum(results_ro$sc4_tot_ro_q3))
results_ro$sc4_dir_ro_q4 <- numeric(2944) 
results_ro$sc4_dir_ro_q4[RO] <- shocks_scenario4[RO] * data_q4$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q4 <- results$scenario4 * data_q4$RO_P3_S14 *100
results_ro$sc4_tot_ro_q4 <- results_ro$sc4_dir_ro_q4 + results_ro$sc4_ind_ro_q4
Gesamteffekte_ro$ro_sc4_q4 <- c(sum(results_ro$sc4_dir_ro_q4),sum(results_ro$sc4_ind_ro_q4), sum(results_ro$sc4_tot_ro_q4))
results_ro$sc4_dir_ro_q5 <- numeric(2944) 
results_ro$sc4_dir_ro_q5[RO] <- shocks_scenario4[RO] * data_q5$RO_P3_S14[RO] *100
results_ro$sc4_ind_ro_q5 <- results$scenario4 * data_q5$RO_P3_S14 *100
results_ro$sc4_tot_ro_q5 <- results_ro$sc4_dir_ro_q5 + results_ro$sc4_ind_ro_q5
Gesamteffekte_ro$ro_sc4_q5 <- c(sum(results_ro$sc4_dir_ro_q5),sum(results_ro$sc4_ind_ro_q5), sum(results_ro$sc4_tot_ro_q5))


#Slowenien
#results df
results_si <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_si <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_si$sc2_dir_si_q1 <- numeric(2944)
results_si$sc2_ind_si_q1 <- results$scenario2 * data_q1$SI_P3_S14 *100
results_si$sc2_tot_si_q1 <- results_si$sc2_dir_si_q1 + results_si$sc2_ind_si_q1
Gesamteffekte_si$si_sc2_q1 <- c(sum(results_si$sc2_dir_si_q1),sum(results_si$sc2_ind_si_q1), sum(results_si$sc2_tot_si_q1))
results_si$sc2_dir_si_q2 <- numeric(2944)
results_si$sc2_ind_si_q2 <- results$scenario2 * data_q2$SI_P3_S14 *100
results_si$sc2_tot_si_q2 <- results_si$sc2_dir_si_q2 + results_si$sc2_ind_si_q2
Gesamteffekte_si$si_sc2_q2 <- c(sum(results_si$sc2_dir_si_q2),sum(results_si$sc2_ind_si_q2), sum(results_si$sc2_tot_si_q2))
results_si$sc2_dir_si_q3 <- numeric(2944)
results_si$sc2_ind_si_q3 <- results$scenario2 * data_q3$SI_P3_S14 *100
results_si$sc2_tot_si_q3 <- results_si$sc2_dir_si_q3 + results_si$sc2_ind_si_q3
Gesamteffekte_si$si_sc2_q3 <- c(sum(results_si$sc2_dir_si_q3),sum(results_si$sc2_ind_si_q3), sum(results_si$sc2_tot_si_q3))
results_si$sc2_dir_si_q4 <- numeric(2944)
results_si$sc2_ind_si_q4 <- results$scenario2 * data_q4$SI_P3_S14 *100
results_si$sc2_tot_si_q4 <- results_si$sc2_dir_si_q4 + results_si$sc2_ind_si_q4
Gesamteffekte_si$si_sc2_q4 <- c(sum(results_si$sc2_dir_si_q4),sum(results_si$sc2_ind_si_q4), sum(results_si$sc2_tot_si_q4))
results_si$sc2_dir_si_q5 <- numeric(2944)
results_si$sc2_ind_si_q5 <- results$scenario2 * data_q5$SI_P3_S14 *100
results_si$sc2_tot_si_q5 <- results_si$sc2_dir_si_q5 + results_si$sc2_ind_si_q5
Gesamteffekte_si$si_sc2_q5 <- c(sum(results_si$sc2_dir_si_q5),sum(results_si$sc2_ind_si_q5), sum(results_si$sc2_tot_si_q5))

#Szenario4
results_si$sc4_dir_si_q1 <- numeric(2944) 
results_si$sc4_dir_si_q1[SI] <- shocks_scenario4[SI] * data_q1$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q1 <- results$scenario4 * data_q1$SI_P3_S14 *100
results_si$sc4_tot_si_q1 <- results_si$sc4_dir_si_q1 + results_si$sc4_ind_si_q1
Gesamteffekte_si$si_sc4_q1 <- c(sum(results_si$sc4_dir_si_q1),sum(results_si$sc4_ind_si_q1), sum(results_si$sc4_tot_si_q1))
results_si$sc4_dir_si_q2 <- numeric(2944) 
results_si$sc4_dir_si_q2[SI] <- shocks_scenario4[SI] * data_q2$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q2 <- results$scenario4 * data_q2$SI_P3_S14 *100
results_si$sc4_tot_si_q2 <- results_si$sc4_dir_si_q2 + results_si$sc4_ind_si_q2
Gesamteffekte_si$si_sc4_q2 <- c(sum(results_si$sc4_dir_si_q2),sum(results_si$sc4_ind_si_q2), sum(results_si$sc4_tot_si_q2))
results_si$sc4_dir_si_q2 <- numeric(2944) 
results_si$sc4_dir_si_q2[SI] <- shocks_scenario4[SI] * data_q2$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q2 <- results$scenario4 * data_q2$SI_P3_S14 *100
results_si$sc4_tot_si_q2 <- results_si$sc4_dir_si_q2 + results_si$sc4_ind_si_q2
Gesamteffekte_si$si_sc4_q2 <- c(sum(results_si$sc4_dir_si_q2),sum(results_si$sc4_ind_si_q2), sum(results_si$sc4_tot_si_q2))
results_si$sc4_dir_si_q3 <- numeric(2944) 
results_si$sc4_dir_si_q3[SI] <- shocks_scenario4[SI] * data_q3$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q3 <- results$scenario4 * data_q3$SI_P3_S14 *100
results_si$sc4_tot_si_q3 <- results_si$sc4_dir_si_q3 + results_si$sc4_ind_si_q3
Gesamteffekte_si$si_sc4_q3 <- c(sum(results_si$sc4_dir_si_q3),sum(results_si$sc4_ind_si_q3), sum(results_si$sc4_tot_si_q3))
results_si$sc4_dir_si_q4 <- numeric(2944) 
results_si$sc4_dir_si_q4[SI] <- shocks_scenario4[SI] * data_q4$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q4 <- results$scenario4 * data_q4$SI_P3_S14 *100
results_si$sc4_tot_si_q4 <- results_si$sc4_dir_si_q4 + results_si$sc4_ind_si_q4
Gesamteffekte_si$si_sc4_q4 <- c(sum(results_si$sc4_dir_si_q4),sum(results_si$sc4_ind_si_q4), sum(results_si$sc4_tot_si_q4))
results_si$sc4_dir_si_q5 <- numeric(2944) 
results_si$sc4_dir_si_q5[SI] <- shocks_scenario4[SI] * data_q5$SI_P3_S14[SI] *100
results_si$sc4_ind_si_q5 <- results$scenario4 * data_q5$SI_P3_S14 *100
results_si$sc4_tot_si_q5 <- results_si$sc4_dir_si_q5 + results_si$sc4_ind_si_q5
Gesamteffekte_si$si_sc4_q5 <- c(sum(results_si$sc4_dir_si_q5),sum(results_si$sc4_ind_si_q5), sum(results_si$sc4_tot_si_q5))


#Slowakei

#results df
results_sk <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_sk <- data.frame(Result = c("direct", "indirect", "total"))

#Szenario2
results_sk$sc2_dir_sk_q1 <- numeric(2944)
results_sk$sc2_ind_sk_q1 <- results$scenario2 * data_q1$SK_P3_S14 *100
results_sk$sc2_tot_sk_q1 <- results_sk$sc2_dir_sk_q1 + results_sk$sc2_ind_sk_q1
Gesamteffekte_sk$sk_sc2_q1 <- c(sum(results_sk$sc2_dir_sk_q1),sum(results_sk$sc2_ind_sk_q1), sum(results_sk$sc2_tot_sk_q1))
results_sk$sc2_dir_sk_q2 <- numeric(2944)
results_sk$sc2_ind_sk_q2 <- results$scenario2 * data_q2$SK_P3_S14 *100
results_sk$sc2_tot_sk_q2 <- results_sk$sc2_dir_sk_q2 + results_sk$sc2_ind_sk_q2
Gesamteffekte_sk$sk_sc2_q2 <- c(sum(results_sk$sc2_dir_sk_q2),sum(results_sk$sc2_ind_sk_q2), sum(results_sk$sc2_tot_sk_q2))
results_sk$sc2_dir_sk_q3 <- numeric(2944)
results_sk$sc2_ind_sk_q3 <- results$scenario2 * data_q3$SK_P3_S14 *100
results_sk$sc2_tot_sk_q3 <- results_sk$sc2_dir_sk_q3 + results_sk$sc2_ind_sk_q3
Gesamteffekte_sk$sk_sc2_q3 <- c(sum(results_sk$sc2_dir_sk_q3),sum(results_sk$sc2_ind_sk_q3), sum(results_sk$sc2_tot_sk_q3))
results_sk$sc2_dir_sk_q4 <- numeric(2944)
results_sk$sc2_ind_sk_q4 <- results$scenario2 * data_q4$SK_P3_S14 *100
results_sk$sc2_tot_sk_q4 <- results_sk$sc2_dir_sk_q4 + results_sk$sc2_ind_sk_q4
Gesamteffekte_sk$sk_sc2_q4 <- c(sum(results_sk$sc2_dir_sk_q4),sum(results_sk$sc2_ind_sk_q4), sum(results_sk$sc2_tot_sk_q4))
results_sk$sc2_dir_sk_q5 <- numeric(2944)
results_sk$sc2_ind_sk_q5 <- results$scenario2 * data_q5$SK_P3_S14 *100
results_sk$sc2_tot_sk_q5 <- results_sk$sc2_dir_sk_q5 + results_sk$sc2_ind_sk_q5
Gesamteffekte_sk$sk_sc2_q5 <- c(sum(results_sk$sc2_dir_sk_q5),sum(results_sk$sc2_ind_sk_q5), sum(results_sk$sc2_tot_sk_q5))

#Szenario4
results_sk$sc4_dir_sk_q1 <- numeric(2944) 
results_sk$sc4_dir_sk_q1[SK] <- shocks_scenario4[SK] * data_q1$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q1 <- results$scenario4 * data_q1$SK_P3_S14 *100
results_sk$sc4_tot_sk_q1 <- results_sk$sc4_dir_sk_q1 + results_sk$sc4_ind_sk_q1
Gesamteffekte_sk$sk_sc4_q1 <- c(sum(results_sk$sc4_dir_sk_q1),sum(results_sk$sc4_ind_sk_q1), sum(results_sk$sc4_tot_sk_q1))
results_sk$sc4_dir_sk_q2 <- numeric(2944) 
results_sk$sc4_dir_sk_q2[SK] <- shocks_scenario4[SK] * data_q2$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q2 <- results$scenario4 * data_q2$SK_P3_S14 *100
results_sk$sc4_tot_sk_q2 <- results_sk$sc4_dir_sk_q2 + results_sk$sc4_ind_sk_q2
Gesamteffekte_sk$sk_sc4_q2 <- c(sum(results_sk$sc4_dir_sk_q2),sum(results_sk$sc4_ind_sk_q2), sum(results_sk$sc4_tot_sk_q2))
results_sk$sc4_dir_sk_q2 <- numeric(2944) 
results_sk$sc4_dir_sk_q2[SK] <- shocks_scenario4[SK] * data_q2$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q2 <- results$scenario4 * data_q2$SK_P3_S14 *100
results_sk$sc4_tot_sk_q2 <- results_sk$sc4_dir_sk_q2 + results_sk$sc4_ind_sk_q2
Gesamteffekte_sk$sk_sc4_q2 <- c(sum(results_sk$sc4_dir_sk_q2),sum(results_sk$sc4_ind_sk_q2), sum(results_sk$sc4_tot_sk_q2))
results_sk$sc4_dir_sk_q3 <- numeric(2944) 
results_sk$sc4_dir_sk_q3[SK] <- shocks_scenario4[SK] * data_q3$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q3 <- results$scenario4 * data_q3$SK_P3_S14 *100
results_sk$sc4_tot_sk_q3 <- results_sk$sc4_dir_sk_q3 + results_sk$sc4_ind_sk_q3
Gesamteffekte_sk$sk_sc4_q3 <- c(sum(results_sk$sc4_dir_sk_q3),sum(results_sk$sc4_ind_sk_q3), sum(results_sk$sc4_tot_sk_q3))
results_sk$sc4_dir_sk_q4 <- numeric(2944) 
results_sk$sc4_dir_sk_q4[SK] <- shocks_scenario4[SK] * data_q4$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q4 <- results$scenario4 * data_q4$SK_P3_S14 *100
results_sk$sc4_tot_sk_q4 <- results_sk$sc4_dir_sk_q4 + results_sk$sc4_ind_sk_q4
Gesamteffekte_sk$sk_sc4_q4 <- c(sum(results_sk$sc4_dir_sk_q4),sum(results_sk$sc4_ind_sk_q4), sum(results_sk$sc4_tot_sk_q4))
results_sk$sc4_dir_sk_q5 <- numeric(2944) 
results_sk$sc4_dir_sk_q5[SK] <- shocks_scenario4[SK] * data_q5$SK_P3_S14[SK] *100
results_sk$sc4_ind_sk_q5 <- results$scenario4 * data_q5$SK_P3_S14 *100
results_sk$sc4_tot_sk_q5 <- results_sk$sc4_dir_sk_q5 + results_sk$sc4_ind_sk_q5
Gesamteffekte_sk$sk_sc4_q5 <- c(sum(results_sk$sc4_dir_sk_q5),sum(results_sk$sc4_ind_sk_q5), sum(results_sk$sc4_tot_sk_q5))

#################################################################################################################
#################################################################################################################

#Regression für Inequality Estimate 

#################################################################################################################
#################################################################################################################

#Regression Dataset
#21 Countries aus COICOP Datensatz
countries <- c("AT", "BE", "BG", "CY", "DE", "DK", "EE", "ES", "FR", 
               "GR", "HR", "HU", "LT", "LU", "LV", "MT", "NL", "PL", 
               "RO", "SI", "SK")

regression <- data.frame( ID = 1:105,
                          Inflation = numeric(105),
                          Quintil = factor(rep(1:5, times = 21)),   
                          Country = rep(countries, each = 5),
                          CountryID = factor(rep(1:21, each = 5)))

for (i in 1:nrow(regression)) {
  land <- regression$Country[i]  # Das Land aus Country
  quintil <- regression$Quintil[i] # Das Quintil (ehemals Wert1)
  
  # Dynamischer Zugriff auf den passenden Datenrahmen und Spaltennamen
  var_name <- paste0(tolower(land), "_sc4_q", quintil)  # Spaltenname im Datenrahmen
  df_name <- paste0("Gesamteffekte_", tolower(land))  # Name des Datenrahmens
  
  # Prüfen, ob der Datenrahmen existiert
  if (exists(df_name)) {
    df <- get(df_name)  # Lade den Datenrahmen
    if (var_name %in% colnames(df)) {
      regression$Inflation[i] <- df[[var_name]][3]  # Entsprechenden Wert einfügen
      regression$Inflation_ind[i] <- df[[var_name]][2]
      regression$Inflation_dir[i] <- df[[var_name]][1]
    }
  }
}

# Den Datensatz anzeigen
print(regression)


income <- read.csv("income.csv")

income2 <- filter(income, TIME_PERIOD == 2023 & quantile %in% 
                    c("First quintile", "Second quintile", 
                      "Third quintile", "Fourth quintile", "Fifth quintile"))
income2 <- filter(income2, indic_il == "Share of national equivalised income")
income2 <- income2 %>%
  mutate(quantile = recode(quantile,
                           "First quintile" = 1,
                           "Second quintile" = 2,
                           "Third quintile" = 3,
                           "Fourth quintile" = 4,
                           "Fifth quintile" = 5))

income2 <- income2 %>%
  mutate(geo = recode(geo,
                      "Austria" = "AT",
                      "Belgium" = "BE",
                      "Bulgaria" = "BG",
                      "Cyprus" = "CY",
                      "Germany" = "DE",
                      "Denmark" = "DK",
                      "Estonia" = "EE",
                      "Spain" = "ES",
                      "France" = "FR",
                      "Greece" = "GR",
                      "Croatia" = "HR",
                      "Hungary" = "HU",
                      "Lithuania" = "LT",
                      "Luxembourg" = "LU",
                      "Latvia" = "LV",
                      "Malta" = "MT",
                      "Netherlands" = "NL",
                      "Poland" = "PL",
                      "Romania" = "RO",
                      "Slovenia" = "SI",
                      "Slovakia" = "SK"))

income2 <- income2 %>%
  mutate(quantile = as.character(quantile),  # Falls nicht schon als Charakter
         geo = as.character(geo))            # Sicherstellen, dass geo als Character vorliegt

regression <- regression %>%
  mutate(Quintil = as.character(Quintil),   # Sicherstellen, dass Quintil als Character vorliegt
         Country = as.character(Country)) 

regression <- regression %>%
  left_join(income2 %>% select(geo, quantile, OBS_VALUE), 
            by = c("Quintil" = "quantile", "Country" = "geo")) %>%
  rename(income = OBS_VALUE)  

#Dummy erstellen für country fixed effects 
regression <- regression %>%
  mutate(Country = as.factor(Country))
regression$Country <- relevel(regression$Country, ref = "DE")

model1 <- lm(log(Inflation) ~ log(income) + Country, data = regression)
summary(model1)

model2 <- lm(log(Inflation_dir) ~ log(income) + Country, data = regression)

model3 <- lm(log(Inflation_ind) ~ log(income) + Country, data = regression)

library(sjPlot)
tab_model(model1, model2, model3, digits=3)

mediating_effect <- abs(coef(model2)["log(income)"]) - abs(coef(model3)["log(income)"])

 

#Regression2


regression2 <- data.frame( ID = 1:105,
                          Inflation = numeric(105),
                          Quintil = factor(rep(1:5, times = 21)),   
                          Country = rep(countries, each = 5),
                          CountryID = factor(rep(1:21, each = 5)))

for (i in 1:nrow(regression2)) {
  land <- regression2$Country[i]  # Das Land aus Country
  quintil <- regression2$Quintil[i] # Das Quintil (ehemals Wert1)
  
  # Dynamischer Zugriff auf den passenden Datenrahmen und Spaltennamen
  var_name <- paste0(tolower(land), "_sc4_q", quintil)  # Spaltenname im Datenrahmen
  df_name <- paste0("Gesamteffekte_inc_", tolower(land))  # Name des Datenrahmens
  
  # Prüfen, ob der Datenrahmen existiert
  if (exists(df_name)) {
    df <- get(df_name)  # Lade den Datenrahmen
    if (var_name %in% colnames(df)) {
      regression2$Inflation[i] <- df[[var_name]][3]  # Entsprechenden Wert einfügen
      regression2$Inflation_ind[i] <- df[[var_name]][2]
      regression2$Inflation_dir[i] <- df[[var_name]][1]
    }
  }
}

income2 <- income2 %>%
  mutate(quantile = as.character(quantile),  # Falls nicht schon als Charakter
         geo = as.character(geo))            # Sicherstellen, dass geo als Character vorliegt

regression2 <- regression2 %>%
  mutate(Quintil = as.character(Quintil),   # Sicherstellen, dass Quintil als Character vorliegt
         Country = as.character(Country)) 

regression2 <- regression2 %>%
  left_join(income2 %>% select(geo, quantile, OBS_VALUE), 
            by = c("Quintil" = "quantile", "Country" = "geo")) %>%
  rename(income = OBS_VALUE)  

#Dummy erstellen
regression2 <- regression2 %>%
  mutate(Country = as.factor(Country))
regression2$Country <- relevel(regression2$Country, ref = "DE")

model4 <- lm(log(Inflation) ~ log(income) + Country, data = regression2)
summary(model4)

model5 <- lm(log(Inflation_dir) ~ log(income) + Country, data = regression2)

model6 <- lm(log(Inflation_ind) ~ log(income) + Country, data = regression2)
mediating_effect2 <- abs(coef(model5)["log(income)"]) - abs(coef(model6)["log(income)"])

tab_model(model4, model6, digits=3)

model7 <- lm(Inflation ~ log(income) + Country, data = regression)
tab_model(model7)

DE_dom<- c(sum(data_q1$DE_P3_S14[grepl("^DE", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$DE_P3_S14[grepl("^DE", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$DE_P3_S14[grepl("^DE", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$DE_P3_S14[grepl("^DE", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$DE_P3_S14[grepl("^DE", data_q5$rowLabels)], na.rm = TRUE))
DE_L <- c(data_q1$DE_P3_S14[grepl("DE_L", data_q1$rowLabels)],
          data_q2$DE_P3_S14[grepl("DE_L", data_q2$rowLabels)],
          data_q3$DE_P3_S14[grepl("DE_L", data_q3$rowLabels)],
          data_q4$DE_P3_S14[grepl("DE_L", data_q4$rowLabels)],
          data_q5$DE_P3_S14[grepl("DE_L", data_q5$rowLabels)])

AT_dom<- c(sum(data_q1$AT_P3_S14[grepl("^AT", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$AT_P3_S14[grepl("^AT", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$AT_P3_S14[grepl("^AT", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$AT_P3_S14[grepl("^AT", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$AT_P3_S14[grepl("^AT", data_q5$rowLabels)], na.rm = TRUE))
AT_L <- c(data_q1$AT_P3_S14[grepl("AT_L", data_q1$rowLabels)],
          data_q2$AT_P3_S14[grepl("AT_L", data_q2$rowLabels)],
          data_q3$AT_P3_S14[grepl("AT_L", data_q3$rowLabels)],
          data_q4$AT_P3_S14[grepl("AT_L", data_q4$rowLabels)],
          data_q5$AT_P3_S14[grepl("AT_L", data_q5$rowLabels)])

BE_dom<- c(sum(data_q1$BE_P3_S14[grepl("^BE", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$BE_P3_S14[grepl("^BE", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$BE_P3_S14[grepl("^BE", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$BE_P3_S14[grepl("^BE", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$BE_P3_S14[grepl("^BE", data_q5$rowLabels)], na.rm = TRUE))
BE_L <- c(data_q1$BE_P3_S14[grepl("BE_L", data_q1$rowLabels)],
          data_q2$BE_P3_S14[grepl("BE_L", data_q2$rowLabels)],
          data_q3$BE_P3_S14[grepl("BE_L", data_q3$rowLabels)],
          data_q4$BE_P3_S14[grepl("BE_L", data_q4$rowLabels)],
          data_q5$BE_P3_S14[grepl("BE_L", data_q5$rowLabels)])

BG_dom<- c(sum(data_q1$BG_P3_S14[grepl("^BG", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$BG_P3_S14[grepl("^BG", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$BG_P3_S14[grepl("^BG", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$BG_P3_S14[grepl("^BG", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$BG_P3_S14[grepl("^BG", data_q5$rowLabels)], na.rm = TRUE))
BG_L <- c(data_q1$BG_P3_S14[grepl("BG_L", data_q1$rowLabels)],
          data_q2$BG_P3_S14[grepl("BG_L", data_q2$rowLabels)],
          data_q3$BG_P3_S14[grepl("BG_L", data_q3$rowLabels)],
          data_q4$BG_P3_S14[grepl("BG_L", data_q4$rowLabels)],
          data_q5$BG_P3_S14[grepl("BG_L", data_q5$rowLabels)])
CY_dom<- c(sum(data_q1$CY_P3_S14[grepl("^CY", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$CY_P3_S14[grepl("^CY", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$CY_P3_S14[grepl("^CY", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$CY_P3_S14[grepl("^CY", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$CY_P3_S14[grepl("^CY", data_q5$rowLabels)], na.rm = TRUE))
CY_US<- c(sum(data_q1$CY_P3_S14[grepl("^US", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$CY_P3_S14[grepl("^US", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$CY_P3_S14[grepl("^US", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$CY_P3_S14[grepl("^US", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$CY_P3_S14[grepl("^US", data_q5$rowLabels)], na.rm = TRUE))
CY_L <- c(data_q1$CY_P3_S14[grepl("CY_L", data_q1$rowLabels)],
          data_q2$CY_P3_S14[grepl("CY_L", data_q2$rowLabels)],
          data_q3$CY_P3_S14[grepl("CY_L", data_q3$rowLabels)],
          data_q4$CY_P3_S14[grepl("CY_L", data_q4$rowLabels)],
          data_q5$CY_P3_S14[grepl("CY_L", data_q5$rowLabels)])
DK_dom<- c(sum(data_q1$DK_P3_S14[grepl("^DK", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$DK_P3_S14[grepl("^DK", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$DK_P3_S14[grepl("^DK", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$DK_P3_S14[grepl("^DK", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$DK_P3_S14[grepl("^DK", data_q5$rowLabels)], na.rm = TRUE))
DK_L <- c(data_q1$DK_P3_S14[grepl("DK_L", data_q1$rowLabels)],
          data_q2$DK_P3_S14[grepl("DK_L", data_q2$rowLabels)],
          data_q3$DK_P3_S14[grepl("DK_L", data_q3$rowLabels)],
          data_q4$DK_P3_S14[grepl("DK_L", data_q4$rowLabels)],
          data_q5$DK_P3_S14[grepl("DK_L", data_q5$rowLabels)])
EE_dom<- c(sum(data_q1$EE_P3_S14[grepl("^EE", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$EE_P3_S14[grepl("^EE", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$EE_P3_S14[grepl("^EE", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$EE_P3_S14[grepl("^EE", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$EE_P3_S14[grepl("^EE", data_q5$rowLabels)], na.rm = TRUE))
EE_L <- c(data_q1$EE_P3_S14[grepl("EE_L", data_q1$rowLabels)],
          data_q2$EE_P3_S14[grepl("EE_L", data_q2$rowLabels)],
          data_q3$EE_P3_S14[grepl("EE_L", data_q3$rowLabels)],
          data_q4$EE_P3_S14[grepl("EE_L", data_q4$rowLabels)],
          data_q5$EE_P3_S14[grepl("EE_L", data_q5$rowLabels)])
ES_dom<- c(sum(data_q1$ES_P3_S14[grepl("^ES", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$ES_P3_S14[grepl("^ES", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$ES_P3_S14[grepl("^ES", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$ES_P3_S14[grepl("^ES", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$ES_P3_S14[grepl("^ES", data_q5$rowLabels)], na.rm = TRUE))
ES_L <- c(data_q1$ES_P3_S14[grepl("ES_L", data_q1$rowLabels)],
          data_q2$ES_P3_S14[grepl("ES_L", data_q2$rowLabels)],
          data_q3$ES_P3_S14[grepl("ES_L", data_q3$rowLabels)],
          data_q4$ES_P3_S14[grepl("ES_L", data_q4$rowLabels)],
          data_q5$ES_P3_S14[grepl("ES_L", data_q5$rowLabels)])
FR_dom<- c(sum(data_q1$FR_P3_S14[grepl("^FR", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$FR_P3_S14[grepl("^FR", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$FR_P3_S14[grepl("^FR", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$FR_P3_S14[grepl("^FR", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$FR_P3_S14[grepl("^FR", data_q5$rowLabels)], na.rm = TRUE))
FR_L <- c(data_q1$FR_P3_S14[grepl("FR_L", data_q1$rowLabels)],
          data_q2$FR_P3_S14[grepl("FR_L", data_q2$rowLabels)],
          data_q3$FR_P3_S14[grepl("FR_L", data_q3$rowLabels)],
          data_q4$FR_P3_S14[grepl("FR_L", data_q4$rowLabels)],
          data_q5$FR_P3_S14[grepl("FR_L", data_q5$rowLabels)])
GR_dom<- c(sum(data_q1$GR_P3_S14[grepl("^GR", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$GR_P3_S14[grepl("^GR", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$GR_P3_S14[grepl("^GR", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$GR_P3_S14[grepl("^GR", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$GR_P3_S14[grepl("^GR", data_q5$rowLabels)], na.rm = TRUE))
GR_L <- c(data_q1$GR_P3_S14[grepl("GR_L", data_q1$rowLabels)],
          data_q2$GR_P3_S14[grepl("GR_L", data_q2$rowLabels)],
          data_q3$GR_P3_S14[grepl("GR_L", data_q3$rowLabels)],
          data_q4$GR_P3_S14[grepl("GR_L", data_q4$rowLabels)],
          data_q5$GR_P3_S14[grepl("GR_L", data_q5$rowLabels)])
HR_dom<- c(sum(data_q1$HR_P3_S14[grepl("^HR", data_q1$rowLabels)], na.rm = TRUE),
                                                                        sum(data_q2$HR_P3_S14[grepl("^HR", data_q2$rowLabels)], na.rm = TRUE),
                                                                        sum(data_q3$HR_P3_S14[grepl("^HR", data_q3$rowLabels)], na.rm = TRUE),
                                                                        sum(data_q4$HR_P3_S14[grepl("^HR", data_q4$rowLabels)], na.rm = TRUE),
                                                                        sum(data_q5$HR_P3_S14[grepl("^HR", data_q5$rowLabels)], na.rm = TRUE))
          HR_L <- c(data_q1$HR_P3_S14[grepl("HR_L", data_q1$rowLabels)],
                    data_q2$HR_P3_S14[grepl("HR_L", data_q2$rowLabels)],
                    data_q3$HR_P3_S14[grepl("HR_L", data_q3$rowLabels)],
                    data_q4$HR_P3_S14[grepl("HR_L", data_q4$rowLabels)],
                    data_q5$HR_P3_S14[grepl("HR_L", data_q5$rowLabels)])
HU_dom<- c(sum(data_q1$HU_P3_S14[grepl("^HU", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$HU_P3_S14[grepl("^HU", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$HU_P3_S14[grepl("^HU", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$HU_P3_S14[grepl("^HU", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$HU_P3_S14[grepl("^HU", data_q5$rowLabels)], na.rm = TRUE))
HU_L <- c(data_q1$HU_P3_S14[grepl("HU_L", data_q1$rowLabels)],
          data_q2$HU_P3_S14[grepl("HU_L", data_q2$rowLabels)],
          data_q3$HU_P3_S14[grepl("HU_L", data_q3$rowLabels)],
          data_q4$HU_P3_S14[grepl("HU_L", data_q4$rowLabels)],
          data_q5$HU_P3_S14[grepl("HU_L", data_q5$rowLabels)])
LT_dom<- c(sum(data_q1$LT_P3_S14[grepl("^LT", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$LT_P3_S14[grepl("^LT", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$LT_P3_S14[grepl("^LT", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$LT_P3_S14[grepl("^LT", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$LT_P3_S14[grepl("^LT", data_q5$rowLabels)], na.rm = TRUE))
LT_L <- c(data_q1$LT_P3_S14[grepl("LT_L", data_q1$rowLabels)],
          data_q2$LT_P3_S14[grepl("LT_L", data_q2$rowLabels)],
          data_q3$LT_P3_S14[grepl("LT_L", data_q3$rowLabels)],
          data_q4$LT_P3_S14[grepl("LT_L", data_q4$rowLabels)],
          data_q5$LT_P3_S14[grepl("LT_L", data_q5$rowLabels)])
LU_dom<- c(sum(data_q1$LU_P3_S14[grepl("^LU", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$LU_P3_S14[grepl("^LU", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$LU_P3_S14[grepl("^LU", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$LU_P3_S14[grepl("^LU", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$LU_P3_S14[grepl("^LU", data_q5$rowLabels)], na.rm = TRUE))
LU_L <- c(data_q1$LU_P3_S14[grepl("LU_L", data_q1$rowLabels)],
          data_q2$LU_P3_S14[grepl("LU_L", data_q2$rowLabels)],
          data_q3$LU_P3_S14[grepl("LU_L", data_q3$rowLabels)],
          data_q4$LU_P3_S14[grepl("LU_L", data_q4$rowLabels)],
          data_q5$LU_P3_S14[grepl("LU_L", data_q5$rowLabels)])
LU_dom<- c(sum(data_q1$LU_P3_S14[grepl("^LU", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$LU_P3_S14[grepl("^LU", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$LU_P3_S14[grepl("^LU", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$LU_P3_S14[grepl("^LU", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$LU_P3_S14[grepl("^LU", data_q5$rowLabels)], na.rm = TRUE))
LU_L <- c(data_q1$LU_P3_S14[grepl("LU_L", data_q1$rowLabels)],
          data_q2$LU_P3_S14[grepl("LU_L", data_q2$rowLabels)],
          data_q3$LU_P3_S14[grepl("LU_L", data_q3$rowLabels)],
          data_q4$LU_P3_S14[grepl("LU_L", data_q4$rowLabels)],
          data_q5$LU_P3_S14[grepl("LU_L", data_q5$rowLabels)])
LV_dom<- c(sum(data_q1$LV_P3_S14[grepl("^LV", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$LV_P3_S14[grepl("^LV", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$LV_P3_S14[grepl("^LV", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$LV_P3_S14[grepl("^LV", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$LV_P3_S14[grepl("^LV", data_q5$rowLabels)], na.rm = TRUE))
LV_L <- c(data_q1$LV_P3_S14[grepl("LV_L", data_q1$rowLabels)],
          data_q2$LV_P3_S14[grepl("LV_L", data_q2$rowLabels)],
          data_q3$LV_P3_S14[grepl("LV_L", data_q3$rowLabels)],
          data_q4$LV_P3_S14[grepl("LV_L", data_q4$rowLabels)],
          data_q5$LV_P3_S14[grepl("LV_L", data_q5$rowLabels)])
MT_dom<- c(sum(data_q1$MT_P3_S14[grepl("^MT", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$MT_P3_S14[grepl("^MT", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$MT_P3_S14[grepl("^MT", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$MT_P3_S14[grepl("^MT", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$MT_P3_S14[grepl("^MT", data_q5$rowLabels)], na.rm = TRUE))
MT_L <- c(data_q1$MT_P3_S14[grepl("MT_L", data_q1$rowLabels)],
          data_q2$MT_P3_S14[grepl("MT_L", data_q2$rowLabels)],
          data_q3$MT_P3_S14[grepl("MT_L", data_q3$rowLabels)],
          data_q4$MT_P3_S14[grepl("MT_L", data_q4$rowLabels)],
          data_q5$MT_P3_S14[grepl("MT_L", data_q5$rowLabels)])
NL_dom<- c(sum(data_q1$NL_P3_S14[grepl("^NL", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$NL_P3_S14[grepl("^NL", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$NL_P3_S14[grepl("^NL", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$NL_P3_S14[grepl("^NL", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$NL_P3_S14[grepl("^NL", data_q5$rowLabels)], na.rm = TRUE))
NL_L <- c(data_q1$NL_P3_S14[grepl("NL_L", data_q1$rowLabels)],
          data_q2$NL_P3_S14[grepl("NL_L", data_q2$rowLabels)],
          data_q3$NL_P3_S14[grepl("NL_L", data_q3$rowLabels)],
          data_q4$NL_P3_S14[grepl("NL_L", data_q4$rowLabels)],
          data_q5$NL_P3_S14[grepl("NL_L", data_q5$rowLabels)])


PL_dom<- c(sum(data_q1$PL_P3_S14[grepl("^PL", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$PL_P3_S14[grepl("^PL", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$PL_P3_S14[grepl("^PL", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$PL_P3_S14[grepl("^PL", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$PL_P3_S14[grepl("^PL", data_q5$rowLabels)], na.rm = TRUE))
PL_L <- c(data_q1$PL_P3_S14[grepl("PL_L", data_q1$rowLabels)],
          data_q2$PL_P3_S14[grepl("PL_L", data_q2$rowLabels)],
          data_q3$PL_P3_S14[grepl("PL_L", data_q3$rowLabels)],
          data_q4$PL_P3_S14[grepl("PL_L", data_q4$rowLabels)],
          data_q5$PL_P3_S14[grepl("PL_L", data_q5$rowLabels)])
RO_dom<- c(sum(data_q1$RO_P3_S14[grepl("^RO", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$RO_P3_S14[grepl("^RO", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$RO_P3_S14[grepl("^RO", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$RO_P3_S14[grepl("^RO", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$RO_P3_S14[grepl("^RO", data_q5$rowLabels)], na.rm = TRUE))
RO_L <- c(data_q1$RO_P3_S14[grepl("RO_L", data_q1$rowLabels)],
          data_q2$RO_P3_S14[grepl("RO_L", data_q2$rowLabels)],
          data_q3$RO_P3_S14[grepl("RO_L", data_q3$rowLabels)],
          data_q4$RO_P3_S14[grepl("RO_L", data_q4$rowLabels)],
          data_q5$RO_P3_S14[grepl("RO_L", data_q5$rowLabels)])
SI_dom<- c(sum(data_q1$SI_P3_S14[grepl("^SI", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$SI_P3_S14[grepl("^SI", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$SI_P3_S14[grepl("^SI", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$SI_P3_S14[grepl("^SI", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$SI_P3_S14[grepl("^SI", data_q5$rowLabels)], na.rm = TRUE))
SI_L <- c(data_q1$SI_P3_S14[grepl("SI_L", data_q1$rowLabels)],
          data_q2$SI_P3_S14[grepl("SI_L", data_q2$rowLabels)],
          data_q3$SI_P3_S14[grepl("SI_L", data_q3$rowLabels)],
          data_q4$SI_P3_S14[grepl("SI_L", data_q4$rowLabels)],
          data_q5$SI_P3_S14[grepl("SI_L", data_q5$rowLabels)])
SK_dom<- c(sum(data_q1$SK_P3_S14[grepl("^SK", data_q1$rowLabels)], na.rm = TRUE),
           sum(data_q2$SK_P3_S14[grepl("^SK", data_q2$rowLabels)], na.rm = TRUE),
           sum(data_q3$SK_P3_S14[grepl("^SK", data_q3$rowLabels)], na.rm = TRUE),
           sum(data_q4$SK_P3_S14[grepl("^SK", data_q4$rowLabels)], na.rm = TRUE),
           sum(data_q5$SK_P3_S14[grepl("^SK", data_q5$rowLabels)], na.rm = TRUE))
SK_L <- c(data_q1$SK_P3_S14[grepl("SK_L", data_q1$rowLabels)],
          data_q2$SK_P3_S14[grepl("SK_L", data_q2$rowLabels)],
          data_q3$SK_P3_S14[grepl("SK_L", data_q3$rowLabels)],
          data_q4$SK_P3_S14[grepl("SK_L", data_q4$rowLabels)],
          data_q5$SK_P3_S14[grepl("SK_L", data_q5$rowLabels)])

regression$predicted_mpg <- predict(model1, newdata = regression)

for (i in 1:nrow(regression)) {
  land <- regression$Country[i]  # Das Land aus Country
  quintil <- regression$Quintil[i]# Das Quintil (ehemals Wert1)
  print(quintil)
  
  # Dynamischer Zugriff auf den passenden Datenrahmen und Spaltennamen  # Spaltenname im Datenrahmen
  list1 <- paste0(land, "_dom")  # Name des Datenrahmens
  
  # Prüfen, ob der Datenrahmen existiert
  list2 <- get(list1)
  print(list2)
  regression$domestic[i] <- list2[as.numeric(quintil)]
  print(list2[as.numeric(quintil)])
}

for (i in 1:nrow(regression)) {
  land <- regression$Country[i]  # Das Land aus Country
  quintil <- regression$Quintil[i]# Das Quintil (ehemals Wert1)
  print(quintil)
  
  # Dynamischer Zugriff auf den passenden Datenrahmen und Spaltennamen  # Spaltenname im Datenrahmen
  list1 <- paste0(land, "_L")  # Name des Datenrahmens
  
  # Prüfen, ob der Datenrahmen existiert
  list2 <- get(list1)
  print(list2)
  regression$housing[i] <- list2[as.numeric(quintil)]
  print(list2[as.numeric(quintil)])
}

land <- regression$Country[1]
list1 <- paste0(land, "_dom")
list2 <- get(list1)
list2[1]





country_codes <- c("AT","BE", "BG", "CY", "DE", "DK", "EE", "ES", "FR", 
                   "GR", "HR", "HU", "LT", "LU", "LV", "MT", "NL", 
                   "PL", "RO", "SI", "SK")

quartile_data <- list(data_q1, data_q2, data_q3, data_q4, data_q5)

results <- list()

for (country in country_codes) {
  results[[country]] <- sapply(quartile_data, function(df) {
    sum(df[[paste0(country, "_P3_S14")]][grepl("^US", df$rowLabels)], na.rm = TRUE)
  })
}

#income weighted

prop_c <- read.csv("prop_c.csv")
prop_c <-  prop_c %>% filter(TIME_PERIOD == "2020")
prop_c <- prop_c %>%
  mutate(geo = recode(geo,
                      "Austria" = "AT",
                      "Belgium" = "BE",
                      "Bulgaria" = "BG",
                      "Cyprus" = "CY",
                      "Germany" = "DE",
                      "Denmark" = "DK",
                      "Estonia" = "EE",
                      "Spain" = "ES",
                      "France" = "FR",
                      "Greece" = "GR",
                      "Croatia" = "HR",
                      "Hungary" = "HU",
                      "Lithuania" = "LT",
                      "Luxembourg" = "LU",
                      "Latvia" = "LV",
                      "Malta" = "MT",
                      "Netherlands" = "NL",
                      "Poland" = "PL",
                      "Romania" = "RO",
                      "Slovenia" = "SI",
                      "Slovakia" = "SK"))

EU21 <- c("AT", "BE", "BG", "CY", "DE", "DK", "EE", "ES", "FR", "GR", "HR", "HU", "LT", "LU", "LV", "MT", "NL", "PL", "RO", "SI", "SK")
prop_c <-  prop_c %>% filter(geo %in% EU21)
prop_c <- prop_c %>%
  mutate(quant_inc = recode(quant_inc,
                           "First quintile" = 1,
                           "Second quintile" = 2,
                           "Third quintile" = 3,
                           "Fourth quintile" = 4,
                           "Fifth quintile" = 5))
#q1
prop_q1 <- prop_c %>% filter(quant_inc == 1) 
prop_q1 <- as.numeric(prop_q1$OBS_VALUE) * 0.01

inc_q1 <- data_q1[,-1]
inc_q1 <- as.matrix(inc_q1)
inc_q1 <- (inc_q1) * (prop_q1)


#q2
prop_q2 <- prop_c %>% filter(quant_inc == 2) 
prop_q2 <- as.numeric(prop_q2$OBS_VALUE) * 0.01

inc_q2 <- data_q2[,-1]
inc_q2 <- as.matrix(inc_q2)
inc_q2 <- (inc_q2) * (prop_q2)


#q3
prop_q3 <- prop_c %>% filter(quant_inc == 3) 
prop_q3 <- as.numeric(prop_q3$OBS_VALUE) * 0.01

inc_q3 <- data_q3[,-1]
inc_q3 <- as.matrix(inc_q3)
inc_q3 <- (inc_q3) * (prop_q3)


#q4
prop_q4 <- prop_c %>% filter(quant_inc == 4) 
prop_q4 <- as.numeric(prop_q4$OBS_VALUE) * 0.01

inc_q4 <- data_q4[,-1]
inc_q4 <- as.matrix(inc_q4)
inc_q4 <- (inc_q4) * (prop_q4)


#q5
prop_q5 <- prop_c %>% filter(quant_inc == 5) 
prop_q5 <- as.numeric(prop_q5$OBS_VALUE) * 0.01

inc_q5 <- data_q5[,-1]
inc_q5 <- as.matrix(inc_q5)
inc_q5 <- (inc_q5) * (prop_q5)



#Deutschland
#results df
results_de_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_de <- data.frame(Result = c("direct", "indirect", "total"))
#conversion
inc_q1 <- as.data.frame(inc_q1)
inc_q2 <- as.data.frame(inc_q2)
inc_q3 <- as.data.frame(inc_q3)
inc_q4 <- as.data.frame(inc_q4)
inc_q5 <- as.data.frame(inc_q5)
#results
results_de_inc$sc2_dir_de_q1 <- numeric(2944)
results_de_inc$sc2_ind_de_q1 <- results$scenario2 * inc_q1$DE_P3_S14 *100
results_de_inc$sc2_tot_de_q1 <- results_de_inc$sc2_dir_de_q1 + results_de_inc$sc2_ind_de_q1
Gesamteffekte_inc_de$de_sc2_q1 <- c(sum(results_de_inc$sc2_dir_de_q1),sum(results_de_inc$sc2_ind_de_q1), sum(results_de_inc$sc2_tot_de_q1))

results_de_inc$sc2_dir_de_q2 <- numeric(2944) 
results_de_inc$sc2_ind_de_q2 <- results$scenario2 * inc_q2$DE_P3_S14 *100 
results_de_inc$sc2_tot_de_q2 <- results_de_inc$sc2_dir_de_q2 + results_de_inc$sc2_ind_de_q2 
Gesamteffekte_inc_de$de_sc2_q2 <- c(sum(results_de_inc$sc2_dir_de_q2),sum(results_de_inc$sc2_ind_de_q2), sum(results_de_inc$sc2_tot_de_q2))
results_de_inc$sc2_dir_de_q3 <- numeric(2944) 
results_de_inc$sc2_ind_de_q3 <- results$scenario2 * inc_q3$DE_P3_S14 *100 
results_de_inc$sc2_tot_de_q3 <- results_de_inc$sc2_dir_de_q3 + results_de_inc$sc2_ind_de_q3 
Gesamteffekte_inc_de$de_sc2_q3 <- c(sum(results_de_inc$sc2_dir_de_q3),sum(results_de_inc$sc2_ind_de_q3), sum(results_de_inc$sc2_tot_de_q3))
results_de_inc$sc2_dir_de_q4 <- numeric(2944) 
results_de_inc$sc2_ind_de_q4 <- results$scenario2 * inc_q4$DE_P3_S14 *100 
results_de_inc$sc2_tot_de_q4 <- results_de_inc$sc2_dir_de_q4 + results_de_inc$sc2_ind_de_q4 
Gesamteffekte_inc_de$de_sc2_q4 <- c(sum(results_de_inc$sc2_dir_de_q4),sum(results_de_inc$sc2_ind_de_q4), sum(results_de_inc$sc2_tot_de_q4))
results_de_inc$sc2_dir_de_q5 <- numeric(2944) 
results_de_inc$sc2_ind_de_q5 <- results$scenario2 * inc_q5$DE_P3_S14 *100 
results_de_inc$sc2_tot_de_q5 <- results_de_inc$sc2_dir_de_q5 + results_de_inc$sc2_ind_de_q5 
Gesamteffekte_inc_de$de_sc2_q5 <- c(sum(results_de_inc$sc2_dir_de_q5),sum(results_de_inc$sc2_ind_de_q5), sum(results_de_inc$sc2_tot_de_q5))

#Szenario4
results_de_inc$sc4_dir_de_q1 <- numeric(2944)
results_de_inc$sc4_dir_de_q1[DE] <- shocks_scenario4[DE] * inc_q1$DE_P3_S14[DE] *100
results_de_inc$sc4_ind_de_q1 <- results$scenario4 * inc_q1$DE_P3_S14 *100
results_de_inc$sc4_tot_de_q1 <- results_de_inc$sc4_dir_de_q1 + results_de_inc$sc4_ind_de_q1
Gesamteffekte_inc_de$de_sc4_q1 <- c(sum(results_de_inc$sc4_dir_de_q1),sum(results_de_inc$sc4_ind_de_q1), sum(results_de_inc$sc4_tot_de_q1))
results_de_inc$sc4_dir_de_q2 <- numeric(2944)
results_de_inc$sc4_dir_de_q2[DE] <- shocks_scenario4[DE] * inc_q2$DE_P3_S14[DE] *100
results_de_inc$sc4_ind_de_q2 <- results$scenario4 * inc_q2$DE_P3_S14 *100
results_de_inc$sc4_tot_de_q2 <- results_de_inc$sc4_dir_de_q2 + results_de_inc$sc4_ind_de_q2
Gesamteffekte_inc_de$de_sc4_q2 <- c(sum(results_de_inc$sc4_dir_de_q2),sum(results_de_inc$sc4_ind_de_q2), sum(results_de_inc$sc4_tot_de_q2))
results_de_inc$sc4_dir_de_q3 <- numeric(2944)
results_de_inc$sc4_dir_de_q3[DE] <- shocks_scenario4[DE] * inc_q3$DE_P3_S14[DE] *100
results_de_inc$sc4_ind_de_q3 <- results$scenario4 * inc_q3$DE_P3_S14 *100
results_de_inc$sc4_tot_de_q3 <- results_de_inc$sc4_dir_de_q3 + results_de_inc$sc4_ind_de_q3
Gesamteffekte_inc_de$de_sc4_q3 <- c(sum(results_de_inc$sc4_dir_de_q3),sum(results_de_inc$sc4_ind_de_q3), sum(results_de_inc$sc4_tot_de_q3))
results_de_inc$sc4_dir_de_q4 <- numeric(2944)
results_de_inc$sc4_dir_de_q4[DE] <- shocks_scenario4[DE] * inc_q4$DE_P3_S14[DE] *100
results_de_inc$sc4_ind_de_q4 <- results$scenario4 * inc_q4$DE_P3_S14 *100
results_de_inc$sc4_tot_de_q4 <- results_de_inc$sc4_dir_de_q4 + results_de_inc$sc4_ind_de_q4
Gesamteffekte_inc_de$de_sc4_q4 <- c(sum(results_de_inc$sc4_dir_de_q4),sum(results_de_inc$sc4_ind_de_q4), sum(results_de_inc$sc4_tot_de_q4))
results_de_inc$sc4_dir_de_q5 <- numeric(2944)
results_de_inc$sc4_ind_de_q5 <- results$scenario4 * inc_q5$DE_P3_S14 *100
results_de_inc$sc4_dir_de_q5[DE] <- shocks_scenario4[DE] * inc_q5$DE_P3_S14[DE] *100
results_de_inc$sc4_tot_de_q5 <- results_de_inc$sc4_dir_de_q5 + results_de_inc$sc4_ind_de_q5
Gesamteffekte_inc_de$de_sc4_q5 <- c(sum(results_de_inc$sc4_dir_de_q5),sum(results_de_inc$sc4_ind_de_q5), sum(results_de_inc$sc4_tot_de_q5))

#Österreich
#results df
results_at_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_at <- data.frame(Result = c("direct", "indirect", "total"))
results_at_inc$sc2_dir_at_q1 <- numeric(2944)
results_at_inc$sc2_ind_at_q1 <- results$scenario2 * inc_q1$AT_P3_S14 *100
results_at_inc$sc2_tot_at_q1 <- results_at_inc$sc2_dir_at_q1 + results_at_inc$sc2_ind_at_q1
Gesamteffekte_inc_at$at_sc2_q1 <- c(sum(results_at_inc$sc2_dir_at_q1),sum(results_at_inc$sc2_ind_at_q1), sum(results_at_inc$sc2_tot_at_q1))

results_at_inc$sc2_dir_at_q2 <- numeric(2944) 
results_at_inc$sc2_ind_at_q2 <- results$scenario2 * inc_q2$AT_P3_S14 *100 
results_at_inc$sc2_tot_at_q2 <- results_at_inc$sc2_dir_at_q2 + results_at_inc$sc2_ind_at_q2 
Gesamteffekte_inc_at$at_sc2_q2 <- c(sum(results_at_inc$sc2_dir_at_q2),sum(results_at_inc$sc2_ind_at_q2), sum(results_at_inc$sc2_tot_at_q2))
results_at_inc$sc2_dir_at_q3 <- numeric(2944) 
results_at_inc$sc2_ind_at_q3 <- results$scenario2 * inc_q3$AT_P3_S14 *100 
results_at_inc$sc2_tot_at_q3 <- results_at_inc$sc2_dir_at_q3 + results_at_inc$sc2_ind_at_q3 
Gesamteffekte_inc_at$at_sc2_q3 <- c(sum(results_at_inc$sc2_dir_at_q3),sum(results_at_inc$sc2_ind_at_q3), sum(results_at_inc$sc2_tot_at_q3))
results_at_inc$sc2_dir_at_q4 <- numeric(2944) 
results_at_inc$sc2_ind_at_q4 <- results$scenario2 * inc_q4$AT_P3_S14 *100 
results_at_inc$sc2_tot_at_q4 <- results_at_inc$sc2_dir_at_q4 + results_at_inc$sc2_ind_at_q4 
Gesamteffekte_inc_at$at_sc2_q4 <- c(sum(results_at_inc$sc2_dir_at_q4),sum(results_at_inc$sc2_ind_at_q4), sum(results_at_inc$sc2_tot_at_q4))
results_at_inc$sc2_dir_at_q5 <- numeric(2944) 
results_at_inc$sc2_ind_at_q5 <- results$scenario2 * inc_q5$AT_P3_S14 *100 
results_at_inc$sc2_tot_at_q5 <- results_at_inc$sc2_dir_at_q5 + results_at_inc$sc2_ind_at_q5 
Gesamteffekte_inc_at$at_sc2_q5 <- c(sum(results_at_inc$sc2_dir_at_q5),sum(results_at_inc$sc2_ind_at_q5), sum(results_at_inc$sc2_tot_at_q5))

#Szenario4
results_at_inc$sc4_dir_at_q1 <- numeric(2944)
results_at_inc$sc4_dir_at_q1[AT] <- shocks_scenario4[AT] * inc_q1$AT_P3_S14[AT] *100
results_at_inc$sc4_ind_at_q1 <- results$scenario4 * inc_q1$AT_P3_S14 *100
results_at_inc$sc4_tot_at_q1 <- results_at_inc$sc4_dir_at_q1 + results_at_inc$sc4_ind_at_q1
Gesamteffekte_inc_at$at_sc4_q1 <- c(sum(results_at_inc$sc4_dir_at_q1),sum(results_at_inc$sc4_ind_at_q1), sum(results_at_inc$sc4_tot_at_q1))
results_at_inc$sc4_dir_at_q2 <- numeric(2944)
results_at_inc$sc4_dir_at_q2[AT] <- shocks_scenario4[AT] * inc_q2$AT_P3_S14[AT] *100
results_at_inc$sc4_ind_at_q2 <- results$scenario4 * inc_q2$AT_P3_S14 *100
results_at_inc$sc4_tot_at_q2 <- results_at_inc$sc4_dir_at_q2 + results_at_inc$sc4_ind_at_q2
Gesamteffekte_inc_at$at_sc4_q2 <- c(sum(results_at_inc$sc4_dir_at_q2),sum(results_at_inc$sc4_ind_at_q2), sum(results_at_inc$sc4_tot_at_q2))
results_at_inc$sc4_dir_at_q3 <- numeric(2944)
results_at_inc$sc4_dir_at_q3[AT] <- shocks_scenario4[AT] * inc_q3$AT_P3_S14[AT] *100
results_at_inc$sc4_ind_at_q3 <- results$scenario4 * inc_q3$AT_P3_S14 *100
results_at_inc$sc4_tot_at_q3 <- results_at_inc$sc4_dir_at_q3 + results_at_inc$sc4_ind_at_q3
Gesamteffekte_inc_at$at_sc4_q3 <- c(sum(results_at_inc$sc4_dir_at_q3),sum(results_at_inc$sc4_ind_at_q3), sum(results_at_inc$sc4_tot_at_q3))
results_at_inc$sc4_dir_at_q4 <- numeric(2944)
results_at_inc$sc4_dir_at_q4[AT] <- shocks_scenario4[AT] * inc_q4$AT_P3_S14[AT] *100
results_at_inc$sc4_ind_at_q4 <- results$scenario4 * inc_q4$AT_P3_S14 *100
results_at_inc$sc4_tot_at_q4 <- results_at_inc$sc4_dir_at_q4 + results_at_inc$sc4_ind_at_q4
Gesamteffekte_inc_at$at_sc4_q4 <- c(sum(results_at_inc$sc4_dir_at_q4),sum(results_at_inc$sc4_ind_at_q4), sum(results_at_inc$sc4_tot_at_q4))
results_at_inc$sc4_dir_at_q5 <- numeric(2944)
results_at_inc$sc4_ind_at_q5 <- results$scenario4 * inc_q5$AT_P3_S14 *100
results_at_inc$sc4_dir_at_q5[AT] <- shocks_scenario4[AT] * inc_q5$AT_P3_S14[AT] *100
results_at_inc$sc4_tot_at_q5 <- results_at_inc$sc4_dir_at_q5 + results_at_inc$sc4_ind_at_q5
Gesamteffekte_inc_at$at_sc4_q5 <- c(sum(results_at_inc$sc4_dir_at_q5),sum(results_at_inc$sc4_ind_at_q5), sum(results_at_inc$sc4_tot_at_q5))


#Belgien
#results df
results_be_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_be <- data.frame(Result = c("direct", "indirect", "total"))
results_be_inc$sc2_dir_be_q1 <- numeric(2944)
results_be_inc$sc2_ind_be_q1 <- results$scenario2 * inc_q1$BE_P3_S14 *100
results_be_inc$sc2_tot_be_q1 <- results_be_inc$sc2_dir_be_q1 + results_be_inc$sc2_ind_be_q1
Gesamteffekte_inc_be$be_sc2_q1 <- c(sum(results_be_inc$sc2_dir_be_q1),sum(results_be_inc$sc2_ind_be_q1), sum(results_be_inc$sc2_tot_be_q1))

results_be_inc$sc2_dir_be_q2 <- numeric(2944) 
results_be_inc$sc2_ind_be_q2 <- results$scenario2 * inc_q2$BE_P3_S14 *100 
results_be_inc$sc2_tot_be_q2 <- results_be_inc$sc2_dir_be_q2 + results_be_inc$sc2_ind_be_q2 
Gesamteffekte_inc_be$be_sc2_q2 <- c(sum(results_be_inc$sc2_dir_be_q2),sum(results_be_inc$sc2_ind_be_q2), sum(results_be_inc$sc2_tot_be_q2))
results_be_inc$sc2_dir_be_q3 <- numeric(2944) 
results_be_inc$sc2_ind_be_q3 <- results$scenario2 * inc_q3$BE_P3_S14 *100 
results_be_inc$sc2_tot_be_q3 <- results_be_inc$sc2_dir_be_q3 + results_be_inc$sc2_ind_be_q3 
Gesamteffekte_inc_be$be_sc2_q3 <- c(sum(results_be_inc$sc2_dir_be_q3),sum(results_be_inc$sc2_ind_be_q3), sum(results_be_inc$sc2_tot_be_q3))
results_be_inc$sc2_dir_be_q4 <- numeric(2944) 
results_be_inc$sc2_ind_be_q4 <- results$scenario2 * inc_q4$BE_P3_S14 *100 
results_be_inc$sc2_tot_be_q4 <- results_be_inc$sc2_dir_be_q4 + results_be_inc$sc2_ind_be_q4 
Gesamteffekte_inc_be$be_sc2_q4 <- c(sum(results_be_inc$sc2_dir_be_q4),sum(results_be_inc$sc2_ind_be_q4), sum(results_be_inc$sc2_tot_be_q4))
results_be_inc$sc2_dir_be_q5 <- numeric(2944) 
results_be_inc$sc2_ind_be_q5 <- results$scenario2 * inc_q5$BE_P3_S14 *100 
results_be_inc$sc2_tot_be_q5 <- results_be_inc$sc2_dir_be_q5 + results_be_inc$sc2_ind_be_q5 
Gesamteffekte_inc_be$be_sc2_q5 <- c(sum(results_be_inc$sc2_dir_be_q5),sum(results_be_inc$sc2_ind_be_q5), sum(results_be_inc$sc2_tot_be_q5))

#Szenario4
results_be_inc$sc4_dir_be_q1 <- numeric(2944)
results_be_inc$sc4_dir_be_q1[BE] <- shocks_scenario4[BE] * inc_q1$BE_P3_S14[BE] *100
results_be_inc$sc4_ind_be_q1 <- results$scenario4 * inc_q1$BE_P3_S14 *100
results_be_inc$sc4_tot_be_q1 <- results_be_inc$sc4_dir_be_q1 + results_be_inc$sc4_ind_be_q1
Gesamteffekte_inc_be$be_sc4_q1 <- c(sum(results_be_inc$sc4_dir_be_q1),sum(results_be_inc$sc4_ind_be_q1), sum(results_be_inc$sc4_tot_be_q1))
results_be_inc$sc4_dir_be_q2 <- numeric(2944)
results_be_inc$sc4_dir_be_q2[BE] <- shocks_scenario4[BE] * inc_q2$BE_P3_S14[BE] *100
results_be_inc$sc4_ind_be_q2 <- results$scenario4 * inc_q2$BE_P3_S14 *100
results_be_inc$sc4_tot_be_q2 <- results_be_inc$sc4_dir_be_q2 + results_be_inc$sc4_ind_be_q2
Gesamteffekte_inc_be$be_sc4_q2 <- c(sum(results_be_inc$sc4_dir_be_q2),sum(results_be_inc$sc4_ind_be_q2), sum(results_be_inc$sc4_tot_be_q2))
results_be_inc$sc4_dir_be_q3 <- numeric(2944)
results_be_inc$sc4_dir_be_q3[BE] <- shocks_scenario4[BE] * inc_q3$BE_P3_S14[BE] *100
results_be_inc$sc4_ind_be_q3 <- results$scenario4 * inc_q3$BE_P3_S14 *100
results_be_inc$sc4_tot_be_q3 <- results_be_inc$sc4_dir_be_q3 + results_be_inc$sc4_ind_be_q3
Gesamteffekte_inc_be$be_sc4_q3 <- c(sum(results_be_inc$sc4_dir_be_q3),sum(results_be_inc$sc4_ind_be_q3), sum(results_be_inc$sc4_tot_be_q3))
results_be_inc$sc4_dir_be_q4 <- numeric(2944)
results_be_inc$sc4_dir_be_q4[BE] <- shocks_scenario4[BE] * inc_q4$BE_P3_S14[BE] *100
results_be_inc$sc4_ind_be_q4 <- results$scenario4 * inc_q4$BE_P3_S14 *100
results_be_inc$sc4_tot_be_q4 <- results_be_inc$sc4_dir_be_q4 + results_be_inc$sc4_ind_be_q4
Gesamteffekte_inc_be$be_sc4_q4 <- c(sum(results_be_inc$sc4_dir_be_q4),sum(results_be_inc$sc4_ind_be_q4), sum(results_be_inc$sc4_tot_be_q4))
results_be_inc$sc4_dir_be_q5 <- numeric(2944)
results_be_inc$sc4_ind_be_q5 <- results$scenario4 * inc_q5$BE_P3_S14 *100
results_be_inc$sc4_dir_be_q5[BE] <- shocks_scenario4[BE] * inc_q5$BE_P3_S14[BE] *100
results_be_inc$sc4_tot_be_q5 <- results_be_inc$sc4_dir_be_q5 + results_be_inc$sc4_ind_be_q5
Gesamteffekte_inc_be$be_sc4_q5 <- c(sum(results_be_inc$sc4_dir_be_q5),sum(results_be_inc$sc4_ind_be_q5), sum(results_be_inc$sc4_tot_be_q5))



#Bulgarien
#results df
results_bg_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_bg <- data.frame(Result = c("direct", "indirect", "total"))
results_bg_inc$sc2_dir_bg_q1 <- numeric(2944)
results_bg_inc$sc2_ind_bg_q1 <- results$scenario2 * inc_q1$BG_P3_S14 *100
results_bg_inc$sc2_tot_bg_q1 <- results_bg_inc$sc2_dir_bg_q1 + results_bg_inc$sc2_ind_bg_q1
Gesamteffekte_inc_bg$bg_sc2_q1 <- c(sum(results_bg_inc$sc2_dir_bg_q1),sum(results_bg_inc$sc2_ind_bg_q1), sum(results_bg_inc$sc2_tot_bg_q1))

results_bg_inc$sc2_dir_bg_q2 <- numeric(2944) 
results_bg_inc$sc2_ind_bg_q2 <- results$scenario2 * inc_q2$BG_P3_S14 *100 
results_bg_inc$sc2_tot_bg_q2 <- results_bg_inc$sc2_dir_bg_q2 + results_bg_inc$sc2_ind_bg_q2 
Gesamteffekte_inc_bg$bg_sc2_q2 <- c(sum(results_bg_inc$sc2_dir_bg_q2),sum(results_bg_inc$sc2_ind_bg_q2), sum(results_bg_inc$sc2_tot_bg_q2))
results_bg_inc$sc2_dir_bg_q3 <- numeric(2944) 
results_bg_inc$sc2_ind_bg_q3 <- results$scenario2 * inc_q3$BG_P3_S14 *100 
results_bg_inc$sc2_tot_bg_q3 <- results_bg_inc$sc2_dir_bg_q3 + results_bg_inc$sc2_ind_bg_q3 
Gesamteffekte_inc_bg$bg_sc2_q3 <- c(sum(results_bg_inc$sc2_dir_bg_q3),sum(results_bg_inc$sc2_ind_bg_q3), sum(results_bg_inc$sc2_tot_bg_q3))
results_bg_inc$sc2_dir_bg_q4 <- numeric(2944) 
results_bg_inc$sc2_ind_bg_q4 <- results$scenario2 * inc_q4$BG_P3_S14 *100 
results_bg_inc$sc2_tot_bg_q4 <- results_bg_inc$sc2_dir_bg_q4 + results_bg_inc$sc2_ind_bg_q4 
Gesamteffekte_inc_bg$bg_sc2_q4 <- c(sum(results_bg_inc$sc2_dir_bg_q4),sum(results_bg_inc$sc2_ind_bg_q4), sum(results_bg_inc$sc2_tot_bg_q4))
results_bg_inc$sc2_dir_bg_q5 <- numeric(2944) 
results_bg_inc$sc2_ind_bg_q5 <- results$scenario2 * inc_q5$BG_P3_S14 *100 
results_bg_inc$sc2_tot_bg_q5 <- results_bg_inc$sc2_dir_bg_q5 + results_bg_inc$sc2_ind_bg_q5 
Gesamteffekte_inc_bg$bg_sc2_q5 <- c(sum(results_bg_inc$sc2_dir_bg_q5),sum(results_bg_inc$sc2_ind_bg_q5), sum(results_bg_inc$sc2_tot_bg_q5))

#Szenario4
results_bg_inc$sc4_dir_bg_q1 <- numeric(2944)
results_bg_inc$sc4_dir_bg_q1[BG] <- shocks_scenario4[BG] * inc_q1$BG_P3_S14[BG] *100
results_bg_inc$sc4_ind_bg_q1 <- results$scenario4 * inc_q1$BG_P3_S14 *100
results_bg_inc$sc4_tot_bg_q1 <- results_bg_inc$sc4_dir_bg_q1 + results_bg_inc$sc4_ind_bg_q1
Gesamteffekte_inc_bg$bg_sc4_q1 <- c(sum(results_bg_inc$sc4_dir_bg_q1),sum(results_bg_inc$sc4_ind_bg_q1), sum(results_bg_inc$sc4_tot_bg_q1))
results_bg_inc$sc4_dir_bg_q2 <- numeric(2944)
results_bg_inc$sc4_dir_bg_q2[BG] <- shocks_scenario4[BG] * inc_q2$BG_P3_S14[BG] *100
results_bg_inc$sc4_ind_bg_q2 <- results$scenario4 * inc_q2$BG_P3_S14 *100
results_bg_inc$sc4_tot_bg_q2 <- results_bg_inc$sc4_dir_bg_q2 + results_bg_inc$sc4_ind_bg_q2
Gesamteffekte_inc_bg$bg_sc4_q2 <- c(sum(results_bg_inc$sc4_dir_bg_q2),sum(results_bg_inc$sc4_ind_bg_q2), sum(results_bg_inc$sc4_tot_bg_q2))
results_bg_inc$sc4_dir_bg_q3 <- numeric(2944)
results_bg_inc$sc4_dir_bg_q3[BG] <- shocks_scenario4[BG] * inc_q3$BG_P3_S14[BG] *100
results_bg_inc$sc4_ind_bg_q3 <- results$scenario4 * inc_q3$BG_P3_S14 *100
results_bg_inc$sc4_tot_bg_q3 <- results_bg_inc$sc4_dir_bg_q3 + results_bg_inc$sc4_ind_bg_q3
Gesamteffekte_inc_bg$bg_sc4_q3 <- c(sum(results_bg_inc$sc4_dir_bg_q3),sum(results_bg_inc$sc4_ind_bg_q3), sum(results_bg_inc$sc4_tot_bg_q3))
results_bg_inc$sc4_dir_bg_q4 <- numeric(2944)
results_bg_inc$sc4_dir_bg_q4[BG] <- shocks_scenario4[BG] * inc_q4$BG_P3_S14[BG] *100
results_bg_inc$sc4_ind_bg_q4 <- results$scenario4 * inc_q4$BG_P3_S14 *100
results_bg_inc$sc4_tot_bg_q4 <- results_bg_inc$sc4_dir_bg_q4 + results_bg_inc$sc4_ind_bg_q4
Gesamteffekte_inc_bg$bg_sc4_q4 <- c(sum(results_bg_inc$sc4_dir_bg_q4),sum(results_bg_inc$sc4_ind_bg_q4), sum(results_bg_inc$sc4_tot_bg_q4))
results_bg_inc$sc4_dir_bg_q5 <- numeric(2944)
results_bg_inc$sc4_ind_bg_q5 <- results$scenario4 * inc_q5$BG_P3_S14 *100
results_bg_inc$sc4_dir_bg_q5[BG] <- shocks_scenario4[BG] * inc_q5$BG_P3_S14[BG] *100
results_bg_inc$sc4_tot_bg_q5 <- results_bg_inc$sc4_dir_bg_q5 + results_bg_inc$sc4_ind_bg_q5
Gesamteffekte_inc_bg$bg_sc4_q5 <- c(sum(results_bg_inc$sc4_dir_bg_q5),sum(results_bg_inc$sc4_ind_bg_q5), sum(results_bg_inc$sc4_tot_bg_q5))


#Zypern
#results df
results_cy_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_cy <- data.frame(Result = c("direct", "indirect", "total"))
results_cy_inc$sc2_dir_cy_q1 <- numeric(2944)
results_cy_inc$sc2_ind_cy_q1 <- results$scenario2 * inc_q1$CY_P3_S14 *100
results_cy_inc$sc2_tot_cy_q1 <- results_cy_inc$sc2_dir_cy_q1 + results_cy_inc$sc2_ind_cy_q1
Gesamteffekte_inc_cy$cy_sc2_q1 <- c(sum(results_cy_inc$sc2_dir_cy_q1),sum(results_cy_inc$sc2_ind_cy_q1), sum(results_cy_inc$sc2_tot_cy_q1))

results_cy_inc$sc2_dir_cy_q2 <- numeric(2944) 
results_cy_inc$sc2_ind_cy_q2 <- results$scenario2 * inc_q2$CY_P3_S14 *100 
results_cy_inc$sc2_tot_cy_q2 <- results_cy_inc$sc2_dir_cy_q2 + results_cy_inc$sc2_ind_cy_q2 
Gesamteffekte_inc_cy$cy_sc2_q2 <- c(sum(results_cy_inc$sc2_dir_cy_q2),sum(results_cy_inc$sc2_ind_cy_q2), sum(results_cy_inc$sc2_tot_cy_q2))
results_cy_inc$sc2_dir_cy_q3 <- numeric(2944) 
results_cy_inc$sc2_ind_cy_q3 <- results$scenario2 * inc_q3$CY_P3_S14 *100 
results_cy_inc$sc2_tot_cy_q3 <- results_cy_inc$sc2_dir_cy_q3 + results_cy_inc$sc2_ind_cy_q3 
Gesamteffekte_inc_cy$cy_sc2_q3 <- c(sum(results_cy_inc$sc2_dir_cy_q3),sum(results_cy_inc$sc2_ind_cy_q3), sum(results_cy_inc$sc2_tot_cy_q3))
results_cy_inc$sc2_dir_cy_q4 <- numeric(2944) 
results_cy_inc$sc2_ind_cy_q4 <- results$scenario2 * inc_q4$CY_P3_S14 *100 
results_cy_inc$sc2_tot_cy_q4 <- results_cy_inc$sc2_dir_cy_q4 + results_cy_inc$sc2_ind_cy_q4 
Gesamteffekte_inc_cy$cy_sc2_q4 <- c(sum(results_cy_inc$sc2_dir_cy_q4),sum(results_cy_inc$sc2_ind_cy_q4), sum(results_cy_inc$sc2_tot_cy_q4))
results_cy_inc$sc2_dir_cy_q5 <- numeric(2944) 
results_cy_inc$sc2_ind_cy_q5 <- results$scenario2 * inc_q5$CY_P3_S14 *100 
results_cy_inc$sc2_tot_cy_q5 <- results_cy_inc$sc2_dir_cy_q5 + results_cy_inc$sc2_ind_cy_q5 
Gesamteffekte_inc_cy$cy_sc2_q5 <- c(sum(results_cy_inc$sc2_dir_cy_q5),sum(results_cy_inc$sc2_ind_cy_q5), sum(results_cy_inc$sc2_tot_cy_q5))

#Szenario4
results_cy_inc$sc4_dir_cy_q1 <- numeric(2944)
results_cy_inc$sc4_dir_cy_q1[CY] <- shocks_scenario4[CY] * inc_q1$CY_P3_S14[CY] *100
results_cy_inc$sc4_ind_cy_q1 <- results$scenario4 * inc_q1$CY_P3_S14 *100
results_cy_inc$sc4_tot_cy_q1 <- results_cy_inc$sc4_dir_cy_q1 + results_cy_inc$sc4_ind_cy_q1
Gesamteffekte_inc_cy$cy_sc4_q1 <- c(sum(results_cy_inc$sc4_dir_cy_q1),sum(results_cy_inc$sc4_ind_cy_q1), sum(results_cy_inc$sc4_tot_cy_q1))
results_cy_inc$sc4_dir_cy_q2 <- numeric(2944)
results_cy_inc$sc4_dir_cy_q2[CY] <- shocks_scenario4[CY] * inc_q2$CY_P3_S14[CY] *100
results_cy_inc$sc4_ind_cy_q2 <- results$scenario4 * inc_q2$CY_P3_S14 *100
results_cy_inc$sc4_tot_cy_q2 <- results_cy_inc$sc4_dir_cy_q2 + results_cy_inc$sc4_ind_cy_q2
Gesamteffekte_inc_cy$cy_sc4_q2 <- c(sum(results_cy_inc$sc4_dir_cy_q2),sum(results_cy_inc$sc4_ind_cy_q2), sum(results_cy_inc$sc4_tot_cy_q2))
results_cy_inc$sc4_dir_cy_q3 <- numeric(2944)
results_cy_inc$sc4_dir_cy_q3[CY] <- shocks_scenario4[CY] * inc_q3$CY_P3_S14[CY] *100
results_cy_inc$sc4_ind_cy_q3 <- results$scenario4 * inc_q3$CY_P3_S14 *100
results_cy_inc$sc4_tot_cy_q3 <- results_cy_inc$sc4_dir_cy_q3 + results_cy_inc$sc4_ind_cy_q3
Gesamteffekte_inc_cy$cy_sc4_q3 <- c(sum(results_cy_inc$sc4_dir_cy_q3),sum(results_cy_inc$sc4_ind_cy_q3), sum(results_cy_inc$sc4_tot_cy_q3))
results_cy_inc$sc4_dir_cy_q4 <- numeric(2944)
results_cy_inc$sc4_dir_cy_q4[CY] <- shocks_scenario4[CY] * inc_q4$CY_P3_S14[CY] *100
results_cy_inc$sc4_ind_cy_q4 <- results$scenario4 * inc_q4$CY_P3_S14 *100
results_cy_inc$sc4_tot_cy_q4 <- results_cy_inc$sc4_dir_cy_q4 + results_cy_inc$sc4_ind_cy_q4
Gesamteffekte_inc_cy$cy_sc4_q4 <- c(sum(results_cy_inc$sc4_dir_cy_q4),sum(results_cy_inc$sc4_ind_cy_q4), sum(results_cy_inc$sc4_tot_cy_q4))
results_cy_inc$sc4_dir_cy_q5 <- numeric(2944)
results_cy_inc$sc4_ind_cy_q5 <- results$scenario4 * inc_q5$CY_P3_S14 *100
results_cy_inc$sc4_dir_cy_q5[CY] <- shocks_scenario4[CY] * inc_q5$CY_P3_S14[CY] *100
results_cy_inc$sc4_tot_cy_q5 <- results_cy_inc$sc4_dir_cy_q5 + results_cy_inc$sc4_ind_cy_q5
Gesamteffekte_inc_cy$cy_sc4_q5 <- c(sum(results_cy_inc$sc4_dir_cy_q5),sum(results_cy_inc$sc4_ind_cy_q5), sum(results_cy_inc$sc4_tot_cy_q5))



#Dänemark
#results df
results_dk_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_dk <- data.frame(Result = c("direct", "indirect", "total"))
results_dk_inc$sc2_dir_dk_q1 <- numeric(2944)
results_dk_inc$sc2_ind_dk_q1 <- results$scenario2 * inc_q1$DK_P3_S14 *100
results_dk_inc$sc2_tot_dk_q1 <- results_dk_inc$sc2_dir_dk_q1 + results_dk_inc$sc2_ind_dk_q1
Gesamteffekte_inc_dk$dk_sc2_q1 <- c(sum(results_dk_inc$sc2_dir_dk_q1),sum(results_dk_inc$sc2_ind_dk_q1), sum(results_dk_inc$sc2_tot_dk_q1))

results_dk_inc$sc2_dir_dk_q2 <- numeric(2944) 
results_dk_inc$sc2_ind_dk_q2 <- results$scenario2 * inc_q2$DK_P3_S14 *100 
results_dk_inc$sc2_tot_dk_q2 <- results_dk_inc$sc2_dir_dk_q2 + results_dk_inc$sc2_ind_dk_q2 
Gesamteffekte_inc_dk$dk_sc2_q2 <- c(sum(results_dk_inc$sc2_dir_dk_q2),sum(results_dk_inc$sc2_ind_dk_q2), sum(results_dk_inc$sc2_tot_dk_q2))
results_dk_inc$sc2_dir_dk_q3 <- numeric(2944) 
results_dk_inc$sc2_ind_dk_q3 <- results$scenario2 * inc_q3$DK_P3_S14 *100 
results_dk_inc$sc2_tot_dk_q3 <- results_dk_inc$sc2_dir_dk_q3 + results_dk_inc$sc2_ind_dk_q3 
Gesamteffekte_inc_dk$dk_sc2_q3 <- c(sum(results_dk_inc$sc2_dir_dk_q3),sum(results_dk_inc$sc2_ind_dk_q3), sum(results_dk_inc$sc2_tot_dk_q3))
results_dk_inc$sc2_dir_dk_q4 <- numeric(2944) 
results_dk_inc$sc2_ind_dk_q4 <- results$scenario2 * inc_q4$DK_P3_S14 *100 
results_dk_inc$sc2_tot_dk_q4 <- results_dk_inc$sc2_dir_dk_q4 + results_dk_inc$sc2_ind_dk_q4 
Gesamteffekte_inc_dk$dk_sc2_q4 <- c(sum(results_dk_inc$sc2_dir_dk_q4),sum(results_dk_inc$sc2_ind_dk_q4), sum(results_dk_inc$sc2_tot_dk_q4))
results_dk_inc$sc2_dir_dk_q5 <- numeric(2944) 
results_dk_inc$sc2_ind_dk_q5 <- results$scenario2 * inc_q5$DK_P3_S14 *100 
results_dk_inc$sc2_tot_dk_q5 <- results_dk_inc$sc2_dir_dk_q5 + results_dk_inc$sc2_ind_dk_q5 
Gesamteffekte_inc_dk$dk_sc2_q5 <- c(sum(results_dk_inc$sc2_dir_dk_q5),sum(results_dk_inc$sc2_ind_dk_q5), sum(results_dk_inc$sc2_tot_dk_q5))

#Szenario4
results_dk_inc$sc4_dir_dk_q1 <- numeric(2944)
results_dk_inc$sc4_dir_dk_q1[DK] <- shocks_scenario4[DK] * inc_q1$DK_P3_S14[DK] *100
results_dk_inc$sc4_ind_dk_q1 <- results$scenario4 * inc_q1$DK_P3_S14 *100
results_dk_inc$sc4_tot_dk_q1 <- results_dk_inc$sc4_dir_dk_q1 + results_dk_inc$sc4_ind_dk_q1
Gesamteffekte_inc_dk$dk_sc4_q1 <- c(sum(results_dk_inc$sc4_dir_dk_q1),sum(results_dk_inc$sc4_ind_dk_q1), sum(results_dk_inc$sc4_tot_dk_q1))
results_dk_inc$sc4_dir_dk_q2 <- numeric(2944)
results_dk_inc$sc4_dir_dk_q2[DK] <- shocks_scenario4[DK] * inc_q2$DK_P3_S14[DK] *100
results_dk_inc$sc4_ind_dk_q2 <- results$scenario4 * inc_q2$DK_P3_S14 *100
results_dk_inc$sc4_tot_dk_q2 <- results_dk_inc$sc4_dir_dk_q2 + results_dk_inc$sc4_ind_dk_q2
Gesamteffekte_inc_dk$dk_sc4_q2 <- c(sum(results_dk_inc$sc4_dir_dk_q2),sum(results_dk_inc$sc4_ind_dk_q2), sum(results_dk_inc$sc4_tot_dk_q2))
results_dk_inc$sc4_dir_dk_q3 <- numeric(2944)
results_dk_inc$sc4_dir_dk_q3[DK] <- shocks_scenario4[DK] * inc_q3$DK_P3_S14[DK] *100
results_dk_inc$sc4_ind_dk_q3 <- results$scenario4 * inc_q3$DK_P3_S14 *100
results_dk_inc$sc4_tot_dk_q3 <- results_dk_inc$sc4_dir_dk_q3 + results_dk_inc$sc4_ind_dk_q3
Gesamteffekte_inc_dk$dk_sc4_q3 <- c(sum(results_dk_inc$sc4_dir_dk_q3),sum(results_dk_inc$sc4_ind_dk_q3), sum(results_dk_inc$sc4_tot_dk_q3))
results_dk_inc$sc4_dir_dk_q4 <- numeric(2944)
results_dk_inc$sc4_dir_dk_q4[DK] <- shocks_scenario4[DK] * inc_q4$DK_P3_S14[DK] *100
results_dk_inc$sc4_ind_dk_q4 <- results$scenario4 * inc_q4$DK_P3_S14 *100
results_dk_inc$sc4_tot_dk_q4 <- results_dk_inc$sc4_dir_dk_q4 + results_dk_inc$sc4_ind_dk_q4
Gesamteffekte_inc_dk$dk_sc4_q4 <- c(sum(results_dk_inc$sc4_dir_dk_q4),sum(results_dk_inc$sc4_ind_dk_q4), sum(results_dk_inc$sc4_tot_dk_q4))
results_dk_inc$sc4_dir_dk_q5 <- numeric(2944)
results_dk_inc$sc4_ind_dk_q5 <- results$scenario4 * inc_q5$DK_P3_S14 *100
results_dk_inc$sc4_dir_dk_q5[DK] <- shocks_scenario4[DK] * inc_q5$DK_P3_S14[DK] *100
results_dk_inc$sc4_tot_dk_q5 <- results_dk_inc$sc4_dir_dk_q5 + results_dk_inc$sc4_ind_dk_q5
Gesamteffekte_inc_dk$dk_sc4_q5 <- c(sum(results_dk_inc$sc4_dir_dk_q5),sum(results_dk_inc$sc4_ind_dk_q5), sum(results_dk_inc$sc4_tot_dk_q5))


#EStland
#results df
results_ee_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_ee <- data.frame(Result = c("direct", "indirect", "total"))
results_ee_inc$sc2_dir_ee_q1 <- numeric(2944)
results_ee_inc$sc2_ind_ee_q1 <- results$scenario2 * inc_q1$EE_P3_S14 *100
results_ee_inc$sc2_tot_ee_q1 <- results_ee_inc$sc2_dir_ee_q1 + results_ee_inc$sc2_ind_ee_q1
Gesamteffekte_inc_ee$ee_sc2_q1 <- c(sum(results_ee_inc$sc2_dir_ee_q1),sum(results_ee_inc$sc2_ind_ee_q1), sum(results_ee_inc$sc2_tot_ee_q1))

results_ee_inc$sc2_dir_ee_q2 <- numeric(2944) 
results_ee_inc$sc2_ind_ee_q2 <- results$scenario2 * inc_q2$EE_P3_S14 *100 
results_ee_inc$sc2_tot_ee_q2 <- results_ee_inc$sc2_dir_ee_q2 + results_ee_inc$sc2_ind_ee_q2 
Gesamteffekte_inc_ee$ee_sc2_q2 <- c(sum(results_ee_inc$sc2_dir_ee_q2),sum(results_ee_inc$sc2_ind_ee_q2), sum(results_ee_inc$sc2_tot_ee_q2))
results_ee_inc$sc2_dir_ee_q3 <- numeric(2944) 
results_ee_inc$sc2_ind_ee_q3 <- results$scenario2 * inc_q3$EE_P3_S14 *100 
results_ee_inc$sc2_tot_ee_q3 <- results_ee_inc$sc2_dir_ee_q3 + results_ee_inc$sc2_ind_ee_q3 
Gesamteffekte_inc_ee$ee_sc2_q3 <- c(sum(results_ee_inc$sc2_dir_ee_q3),sum(results_ee_inc$sc2_ind_ee_q3), sum(results_ee_inc$sc2_tot_ee_q3))
results_ee_inc$sc2_dir_ee_q4 <- numeric(2944) 
results_ee_inc$sc2_ind_ee_q4 <- results$scenario2 * inc_q4$EE_P3_S14 *100 
results_ee_inc$sc2_tot_ee_q4 <- results_ee_inc$sc2_dir_ee_q4 + results_ee_inc$sc2_ind_ee_q4 
Gesamteffekte_inc_ee$ee_sc2_q4 <- c(sum(results_ee_inc$sc2_dir_ee_q4),sum(results_ee_inc$sc2_ind_ee_q4), sum(results_ee_inc$sc2_tot_ee_q4))
results_ee_inc$sc2_dir_ee_q5 <- numeric(2944) 
results_ee_inc$sc2_ind_ee_q5 <- results$scenario2 * inc_q5$EE_P3_S14 *100 
results_ee_inc$sc2_tot_ee_q5 <- results_ee_inc$sc2_dir_ee_q5 + results_ee_inc$sc2_ind_ee_q5 
Gesamteffekte_inc_ee$ee_sc2_q5 <- c(sum(results_ee_inc$sc2_dir_ee_q5),sum(results_ee_inc$sc2_ind_ee_q5), sum(results_ee_inc$sc2_tot_ee_q5))

#Szenario4
results_ee_inc$sc4_dir_ee_q1 <- numeric(2944)
results_ee_inc$sc4_dir_ee_q1[EE] <- shocks_scenario4[EE] * inc_q1$EE_P3_S14[EE] *100
results_ee_inc$sc4_ind_ee_q1 <- results$scenario4 * inc_q1$EE_P3_S14 *100
results_ee_inc$sc4_tot_ee_q1 <- results_ee_inc$sc4_dir_ee_q1 + results_ee_inc$sc4_ind_ee_q1
Gesamteffekte_inc_ee$ee_sc4_q1 <- c(sum(results_ee_inc$sc4_dir_ee_q1),sum(results_ee_inc$sc4_ind_ee_q1), sum(results_ee_inc$sc4_tot_ee_q1))
results_ee_inc$sc4_dir_ee_q2 <- numeric(2944)
results_ee_inc$sc4_dir_ee_q2[EE] <- shocks_scenario4[EE] * inc_q2$EE_P3_S14[EE] *100
results_ee_inc$sc4_ind_ee_q2 <- results$scenario4 * inc_q2$EE_P3_S14 *100
results_ee_inc$sc4_tot_ee_q2 <- results_ee_inc$sc4_dir_ee_q2 + results_ee_inc$sc4_ind_ee_q2
Gesamteffekte_inc_ee$ee_sc4_q2 <- c(sum(results_ee_inc$sc4_dir_ee_q2),sum(results_ee_inc$sc4_ind_ee_q2), sum(results_ee_inc$sc4_tot_ee_q2))
results_ee_inc$sc4_dir_ee_q3 <- numeric(2944)
results_ee_inc$sc4_dir_ee_q3[EE] <- shocks_scenario4[EE] * inc_q3$EE_P3_S14[EE] *100
results_ee_inc$sc4_ind_ee_q3 <- results$scenario4 * inc_q3$EE_P3_S14 *100
results_ee_inc$sc4_tot_ee_q3 <- results_ee_inc$sc4_dir_ee_q3 + results_ee_inc$sc4_ind_ee_q3
Gesamteffekte_inc_ee$ee_sc4_q3 <- c(sum(results_ee_inc$sc4_dir_ee_q3),sum(results_ee_inc$sc4_ind_ee_q3), sum(results_ee_inc$sc4_tot_ee_q3))
results_ee_inc$sc4_dir_ee_q4 <- numeric(2944)
results_ee_inc$sc4_dir_ee_q4[EE] <- shocks_scenario4[EE] * inc_q4$EE_P3_S14[EE] *100
results_ee_inc$sc4_ind_ee_q4 <- results$scenario4 * inc_q4$EE_P3_S14 *100
results_ee_inc$sc4_tot_ee_q4 <- results_ee_inc$sc4_dir_ee_q4 + results_ee_inc$sc4_ind_ee_q4
Gesamteffekte_inc_ee$ee_sc4_q4 <- c(sum(results_ee_inc$sc4_dir_ee_q4),sum(results_ee_inc$sc4_ind_ee_q4), sum(results_ee_inc$sc4_tot_ee_q4))
results_ee_inc$sc4_dir_ee_q5 <- numeric(2944)
results_ee_inc$sc4_ind_ee_q5 <- results$scenario4 * inc_q5$EE_P3_S14 *100
results_ee_inc$sc4_dir_ee_q5[EE] <- shocks_scenario4[EE] * inc_q5$EE_P3_S14[EE] *100
results_ee_inc$sc4_tot_ee_q5 <- results_ee_inc$sc4_dir_ee_q5 + results_ee_inc$sc4_ind_ee_q5
Gesamteffekte_inc_ee$ee_sc4_q5 <- c(sum(results_ee_inc$sc4_dir_ee_q5),sum(results_ee_inc$sc4_ind_ee_q5), sum(results_ee_inc$sc4_tot_ee_q5))


#Spanien
#results df
results_es_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_es <- data.frame(Result = c("direct", "indirect", "total"))
results_es_inc$sc2_dir_es_q1 <- numeric(2944)
results_es_inc$sc2_ind_es_q1 <- results$scenario2 * inc_q1$ES_P3_S14 *100
results_es_inc$sc2_tot_es_q1 <- results_es_inc$sc2_dir_es_q1 + results_es_inc$sc2_ind_es_q1
Gesamteffekte_inc_es$es_sc2_q1 <- c(sum(results_es_inc$sc2_dir_es_q1),sum(results_es_inc$sc2_ind_es_q1), sum(results_es_inc$sc2_tot_es_q1))

results_es_inc$sc2_dir_es_q2 <- numeric(2944) 
results_es_inc$sc2_ind_es_q2 <- results$scenario2 * inc_q2$ES_P3_S14 *100 
results_es_inc$sc2_tot_es_q2 <- results_es_inc$sc2_dir_es_q2 + results_es_inc$sc2_ind_es_q2 
Gesamteffekte_inc_es$es_sc2_q2 <- c(sum(results_es_inc$sc2_dir_es_q2),sum(results_es_inc$sc2_ind_es_q2), sum(results_es_inc$sc2_tot_es_q2))
results_es_inc$sc2_dir_es_q3 <- numeric(2944) 
results_es_inc$sc2_ind_es_q3 <- results$scenario2 * inc_q3$ES_P3_S14 *100 
results_es_inc$sc2_tot_es_q3 <- results_es_inc$sc2_dir_es_q3 + results_es_inc$sc2_ind_es_q3 
Gesamteffekte_inc_es$es_sc2_q3 <- c(sum(results_es_inc$sc2_dir_es_q3),sum(results_es_inc$sc2_ind_es_q3), sum(results_es_inc$sc2_tot_es_q3))
results_es_inc$sc2_dir_es_q4 <- numeric(2944) 
results_es_inc$sc2_ind_es_q4 <- results$scenario2 * inc_q4$ES_P3_S14 *100 
results_es_inc$sc2_tot_es_q4 <- results_es_inc$sc2_dir_es_q4 + results_es_inc$sc2_ind_es_q4 
Gesamteffekte_inc_es$es_sc2_q4 <- c(sum(results_es_inc$sc2_dir_es_q4),sum(results_es_inc$sc2_ind_es_q4), sum(results_es_inc$sc2_tot_es_q4))
results_es_inc$sc2_dir_es_q5 <- numeric(2944) 
results_es_inc$sc2_ind_es_q5 <- results$scenario2 * inc_q5$ES_P3_S14 *100 
results_es_inc$sc2_tot_es_q5 <- results_es_inc$sc2_dir_es_q5 + results_es_inc$sc2_ind_es_q5 
Gesamteffekte_inc_es$es_sc2_q5 <- c(sum(results_es_inc$sc2_dir_es_q5),sum(results_es_inc$sc2_ind_es_q5), sum(results_es_inc$sc2_tot_es_q5))

#Szenario4
results_es_inc$sc4_dir_es_q1 <- numeric(2944)
results_es_inc$sc4_dir_es_q1[ES] <- shocks_scenario4[ES] * inc_q1$ES_P3_S14[ES] *100
results_es_inc$sc4_ind_es_q1 <- results$scenario4 * inc_q1$ES_P3_S14 *100
results_es_inc$sc4_tot_es_q1 <- results_es_inc$sc4_dir_es_q1 + results_es_inc$sc4_ind_es_q1
Gesamteffekte_inc_es$es_sc4_q1 <- c(sum(results_es_inc$sc4_dir_es_q1),sum(results_es_inc$sc4_ind_es_q1), sum(results_es_inc$sc4_tot_es_q1))
results_es_inc$sc4_dir_es_q2 <- numeric(2944)
results_es_inc$sc4_dir_es_q2[ES] <- shocks_scenario4[ES] * inc_q2$ES_P3_S14[ES] *100
results_es_inc$sc4_ind_es_q2 <- results$scenario4 * inc_q2$ES_P3_S14 *100
results_es_inc$sc4_tot_es_q2 <- results_es_inc$sc4_dir_es_q2 + results_es_inc$sc4_ind_es_q2
Gesamteffekte_inc_es$es_sc4_q2 <- c(sum(results_es_inc$sc4_dir_es_q2),sum(results_es_inc$sc4_ind_es_q2), sum(results_es_inc$sc4_tot_es_q2))
results_es_inc$sc4_dir_es_q3 <- numeric(2944)
results_es_inc$sc4_dir_es_q3[ES] <- shocks_scenario4[ES] * inc_q3$ES_P3_S14[ES] *100
results_es_inc$sc4_ind_es_q3 <- results$scenario4 * inc_q3$ES_P3_S14 *100
results_es_inc$sc4_tot_es_q3 <- results_es_inc$sc4_dir_es_q3 + results_es_inc$sc4_ind_es_q3
Gesamteffekte_inc_es$es_sc4_q3 <- c(sum(results_es_inc$sc4_dir_es_q3),sum(results_es_inc$sc4_ind_es_q3), sum(results_es_inc$sc4_tot_es_q3))
results_es_inc$sc4_dir_es_q4 <- numeric(2944)
results_es_inc$sc4_dir_es_q4[ES] <- shocks_scenario4[ES] * inc_q4$ES_P3_S14[ES] *100
results_es_inc$sc4_ind_es_q4 <- results$scenario4 * inc_q4$ES_P3_S14 *100
results_es_inc$sc4_tot_es_q4 <- results_es_inc$sc4_dir_es_q4 + results_es_inc$sc4_ind_es_q4
Gesamteffekte_inc_es$es_sc4_q4 <- c(sum(results_es_inc$sc4_dir_es_q4),sum(results_es_inc$sc4_ind_es_q4), sum(results_es_inc$sc4_tot_es_q4))
results_es_inc$sc4_dir_es_q5 <- numeric(2944)
results_es_inc$sc4_ind_es_q5 <- results$scenario4 * inc_q5$ES_P3_S14 *100
results_es_inc$sc4_dir_es_q5[ES] <- shocks_scenario4[ES] * inc_q5$ES_P3_S14[ES] *100
results_es_inc$sc4_tot_es_q5 <- results_es_inc$sc4_dir_es_q5 + results_es_inc$sc4_ind_es_q5
Gesamteffekte_inc_es$es_sc4_q5 <- c(sum(results_es_inc$sc4_dir_es_q5),sum(results_es_inc$sc4_ind_es_q5), sum(results_es_inc$sc4_tot_es_q5))


#Frankreich

#results df
results_fr_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_fr <- data.frame(Result = c("direct", "indirect", "total"))
results_fr_inc$sc2_dir_fr_q1 <- numeric(2944)
results_fr_inc$sc2_ind_fr_q1 <- results$scenario2 * inc_q1$FR_P3_S14 *100
results_fr_inc$sc2_tot_fr_q1 <- results_fr_inc$sc2_dir_fr_q1 + results_fr_inc$sc2_ind_fr_q1
Gesamteffekte_inc_fr$fr_sc2_q1 <- c(sum(results_fr_inc$sc2_dir_fr_q1),sum(results_fr_inc$sc2_ind_fr_q1), sum(results_fr_inc$sc2_tot_fr_q1))

results_fr_inc$sc2_dir_fr_q2 <- numeric(2944) 
results_fr_inc$sc2_ind_fr_q2 <- results$scenario2 * inc_q2$FR_P3_S14 *100 
results_fr_inc$sc2_tot_fr_q2 <- results_fr_inc$sc2_dir_fr_q2 + results_fr_inc$sc2_ind_fr_q2 
Gesamteffekte_inc_fr$fr_sc2_q2 <- c(sum(results_fr_inc$sc2_dir_fr_q2),sum(results_fr_inc$sc2_ind_fr_q2), sum(results_fr_inc$sc2_tot_fr_q2))
results_fr_inc$sc2_dir_fr_q3 <- numeric(2944) 
results_fr_inc$sc2_ind_fr_q3 <- results$scenario2 * inc_q3$FR_P3_S14 *100 
results_fr_inc$sc2_tot_fr_q3 <- results_fr_inc$sc2_dir_fr_q3 + results_fr_inc$sc2_ind_fr_q3 
Gesamteffekte_inc_fr$fr_sc2_q3 <- c(sum(results_fr_inc$sc2_dir_fr_q3),sum(results_fr_inc$sc2_ind_fr_q3), sum(results_fr_inc$sc2_tot_fr_q3))
results_fr_inc$sc2_dir_fr_q4 <- numeric(2944) 
results_fr_inc$sc2_ind_fr_q4 <- results$scenario2 * inc_q4$FR_P3_S14 *100 
results_fr_inc$sc2_tot_fr_q4 <- results_fr_inc$sc2_dir_fr_q4 + results_fr_inc$sc2_ind_fr_q4 
Gesamteffekte_inc_fr$fr_sc2_q4 <- c(sum(results_fr_inc$sc2_dir_fr_q4),sum(results_fr_inc$sc2_ind_fr_q4), sum(results_fr_inc$sc2_tot_fr_q4))
results_fr_inc$sc2_dir_fr_q5 <- numeric(2944) 
results_fr_inc$sc2_ind_fr_q5 <- results$scenario2 * inc_q5$FR_P3_S14 *100 
results_fr_inc$sc2_tot_fr_q5 <- results_fr_inc$sc2_dir_fr_q5 + results_fr_inc$sc2_ind_fr_q5 
Gesamteffekte_inc_fr$fr_sc2_q5 <- c(sum(results_fr_inc$sc2_dir_fr_q5),sum(results_fr_inc$sc2_ind_fr_q5), sum(results_fr_inc$sc2_tot_fr_q5))

#Szenario4
results_fr_inc$sc4_dir_fr_q1 <- numeric(2944)
results_fr_inc$sc4_dir_fr_q1[FR] <- shocks_scenario4[FR] * inc_q1$FR_P3_S14[FR] *100
results_fr_inc$sc4_ind_fr_q1 <- results$scenario4 * inc_q1$FR_P3_S14 *100
results_fr_inc$sc4_tot_fr_q1 <- results_fr_inc$sc4_dir_fr_q1 + results_fr_inc$sc4_ind_fr_q1
Gesamteffekte_inc_fr$fr_sc4_q1 <- c(sum(results_fr_inc$sc4_dir_fr_q1),sum(results_fr_inc$sc4_ind_fr_q1), sum(results_fr_inc$sc4_tot_fr_q1))
results_fr_inc$sc4_dir_fr_q2 <- numeric(2944)
results_fr_inc$sc4_dir_fr_q2[FR] <- shocks_scenario4[FR] * inc_q2$FR_P3_S14[FR] *100
results_fr_inc$sc4_ind_fr_q2 <- results$scenario4 * inc_q2$FR_P3_S14 *100
results_fr_inc$sc4_tot_fr_q2 <- results_fr_inc$sc4_dir_fr_q2 + results_fr_inc$sc4_ind_fr_q2
Gesamteffekte_inc_fr$fr_sc4_q2 <- c(sum(results_fr_inc$sc4_dir_fr_q2),sum(results_fr_inc$sc4_ind_fr_q2), sum(results_fr_inc$sc4_tot_fr_q2))
results_fr_inc$sc4_dir_fr_q3 <- numeric(2944)
results_fr_inc$sc4_dir_fr_q3[FR] <- shocks_scenario4[FR] * inc_q3$FR_P3_S14[FR] *100
results_fr_inc$sc4_ind_fr_q3 <- results$scenario4 * inc_q3$FR_P3_S14 *100
results_fr_inc$sc4_tot_fr_q3 <- results_fr_inc$sc4_dir_fr_q3 + results_fr_inc$sc4_ind_fr_q3
Gesamteffekte_inc_fr$fr_sc4_q3 <- c(sum(results_fr_inc$sc4_dir_fr_q3),sum(results_fr_inc$sc4_ind_fr_q3), sum(results_fr_inc$sc4_tot_fr_q3))
results_fr_inc$sc4_dir_fr_q4 <- numeric(2944)
results_fr_inc$sc4_dir_fr_q4[FR] <- shocks_scenario4[FR] * inc_q4$FR_P3_S14[FR] *100
results_fr_inc$sc4_ind_fr_q4 <- results$scenario4 * inc_q4$FR_P3_S14 *100
results_fr_inc$sc4_tot_fr_q4 <- results_fr_inc$sc4_dir_fr_q4 + results_fr_inc$sc4_ind_fr_q4
Gesamteffekte_inc_fr$fr_sc4_q4 <- c(sum(results_fr_inc$sc4_dir_fr_q4),sum(results_fr_inc$sc4_ind_fr_q4), sum(results_fr_inc$sc4_tot_fr_q4))
results_fr_inc$sc4_dir_fr_q5 <- numeric(2944)
results_fr_inc$sc4_ind_fr_q5 <- results$scenario4 * inc_q5$FR_P3_S14 *100
results_fr_inc$sc4_dir_fr_q5[FR] <- shocks_scenario4[FR] * inc_q5$FR_P3_S14[FR] *100
results_fr_inc$sc4_tot_fr_q5 <- results_fr_inc$sc4_dir_fr_q5 + results_fr_inc$sc4_ind_fr_q5
Gesamteffekte_inc_fr$fr_sc4_q5 <- c(sum(results_fr_inc$sc4_dir_fr_q5),sum(results_fr_inc$sc4_ind_fr_q5), sum(results_fr_inc$sc4_tot_fr_q5))

#Griechenland
#results df
results_gr_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_gr <- data.frame(Result = c("direct", "indirect", "total"))
results_gr_inc$sc2_dir_gr_q1 <- numeric(2944)
results_gr_inc$sc2_ind_gr_q1 <- results$scenario2 * inc_q1$GR_P3_S14 *100
results_gr_inc$sc2_tot_gr_q1 <- results_gr_inc$sc2_dir_gr_q1 + results_gr_inc$sc2_ind_gr_q1
Gesamteffekte_inc_gr$gr_sc2_q1 <- c(sum(results_gr_inc$sc2_dir_gr_q1),sum(results_gr_inc$sc2_ind_gr_q1), sum(results_gr_inc$sc2_tot_gr_q1))

results_gr_inc$sc2_dir_gr_q2 <- numeric(2944) 
results_gr_inc$sc2_ind_gr_q2 <- results$scenario2 * inc_q2$GR_P3_S14 *100 
results_gr_inc$sc2_tot_gr_q2 <- results_gr_inc$sc2_dir_gr_q2 + results_gr_inc$sc2_ind_gr_q2 
Gesamteffekte_inc_gr$gr_sc2_q2 <- c(sum(results_gr_inc$sc2_dir_gr_q2),sum(results_gr_inc$sc2_ind_gr_q2), sum(results_gr_inc$sc2_tot_gr_q2))
results_gr_inc$sc2_dir_gr_q3 <- numeric(2944) 
results_gr_inc$sc2_ind_gr_q3 <- results$scenario2 * inc_q3$GR_P3_S14 *100 
results_gr_inc$sc2_tot_gr_q3 <- results_gr_inc$sc2_dir_gr_q3 + results_gr_inc$sc2_ind_gr_q3 
Gesamteffekte_inc_gr$gr_sc2_q3 <- c(sum(results_gr_inc$sc2_dir_gr_q3),sum(results_gr_inc$sc2_ind_gr_q3), sum(results_gr_inc$sc2_tot_gr_q3))
results_gr_inc$sc2_dir_gr_q4 <- numeric(2944) 
results_gr_inc$sc2_ind_gr_q4 <- results$scenario2 * inc_q4$GR_P3_S14 *100 
results_gr_inc$sc2_tot_gr_q4 <- results_gr_inc$sc2_dir_gr_q4 + results_gr_inc$sc2_ind_gr_q4 
Gesamteffekte_inc_gr$gr_sc2_q4 <- c(sum(results_gr_inc$sc2_dir_gr_q4),sum(results_gr_inc$sc2_ind_gr_q4), sum(results_gr_inc$sc2_tot_gr_q4))
results_gr_inc$sc2_dir_gr_q5 <- numeric(2944) 
results_gr_inc$sc2_ind_gr_q5 <- results$scenario2 * inc_q5$GR_P3_S14 *100 
results_gr_inc$sc2_tot_gr_q5 <- results_gr_inc$sc2_dir_gr_q5 + results_gr_inc$sc2_ind_gr_q5 
Gesamteffekte_inc_gr$gr_sc2_q5 <- c(sum(results_gr_inc$sc2_dir_gr_q5),sum(results_gr_inc$sc2_ind_gr_q5), sum(results_gr_inc$sc2_tot_gr_q5))

#Szenario4
results_gr_inc$sc4_dir_gr_q1 <- numeric(2944)
results_gr_inc$sc4_dir_gr_q1[GR] <- shocks_scenario4[GR] * inc_q1$GR_P3_S14[GR] *100
results_gr_inc$sc4_ind_gr_q1 <- results$scenario4 * inc_q1$GR_P3_S14 *100
results_gr_inc$sc4_tot_gr_q1 <- results_gr_inc$sc4_dir_gr_q1 + results_gr_inc$sc4_ind_gr_q1
Gesamteffekte_inc_gr$gr_sc4_q1 <- c(sum(results_gr_inc$sc4_dir_gr_q1),sum(results_gr_inc$sc4_ind_gr_q1), sum(results_gr_inc$sc4_tot_gr_q1))
results_gr_inc$sc4_dir_gr_q2 <- numeric(2944)
results_gr_inc$sc4_dir_gr_q2[GR] <- shocks_scenario4[GR] * inc_q2$GR_P3_S14[GR] *100
results_gr_inc$sc4_ind_gr_q2 <- results$scenario4 * inc_q2$GR_P3_S14 *100
results_gr_inc$sc4_tot_gr_q2 <- results_gr_inc$sc4_dir_gr_q2 + results_gr_inc$sc4_ind_gr_q2
Gesamteffekte_inc_gr$gr_sc4_q2 <- c(sum(results_gr_inc$sc4_dir_gr_q2),sum(results_gr_inc$sc4_ind_gr_q2), sum(results_gr_inc$sc4_tot_gr_q2))
results_gr_inc$sc4_dir_gr_q3 <- numeric(2944)
results_gr_inc$sc4_dir_gr_q3[GR] <- shocks_scenario4[GR] * inc_q3$GR_P3_S14[GR] *100
results_gr_inc$sc4_ind_gr_q3 <- results$scenario4 * inc_q3$GR_P3_S14 *100
results_gr_inc$sc4_tot_gr_q3 <- results_gr_inc$sc4_dir_gr_q3 + results_gr_inc$sc4_ind_gr_q3
Gesamteffekte_inc_gr$gr_sc4_q3 <- c(sum(results_gr_inc$sc4_dir_gr_q3),sum(results_gr_inc$sc4_ind_gr_q3), sum(results_gr_inc$sc4_tot_gr_q3))
results_gr_inc$sc4_dir_gr_q4 <- numeric(2944)
results_gr_inc$sc4_dir_gr_q4[GR] <- shocks_scenario4[GR] * inc_q4$GR_P3_S14[GR] *100
results_gr_inc$sc4_ind_gr_q4 <- results$scenario4 * inc_q4$GR_P3_S14 *100
results_gr_inc$sc4_tot_gr_q4 <- results_gr_inc$sc4_dir_gr_q4 + results_gr_inc$sc4_ind_gr_q4
Gesamteffekte_inc_gr$gr_sc4_q4 <- c(sum(results_gr_inc$sc4_dir_gr_q4),sum(results_gr_inc$sc4_ind_gr_q4), sum(results_gr_inc$sc4_tot_gr_q4))
results_gr_inc$sc4_dir_gr_q5 <- numeric(2944)
results_gr_inc$sc4_ind_gr_q5 <- results$scenario4 * inc_q5$GR_P3_S14 *100
results_gr_inc$sc4_dir_gr_q5[GR] <- shocks_scenario4[GR] * inc_q5$GR_P3_S14[GR] *100
results_gr_inc$sc4_tot_gr_q5 <- results_gr_inc$sc4_dir_gr_q5 + results_gr_inc$sc4_ind_gr_q5
Gesamteffekte_inc_gr$gr_sc4_q5 <- c(sum(results_gr_inc$sc4_dir_gr_q5),sum(results_gr_inc$sc4_ind_gr_q5), sum(results_gr_inc$sc4_tot_gr_q5))


#Kroatien

#results df
results_hr_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_hr <- data.frame(Result = c("direct", "indirect", "total"))
results_hr_inc$sc2_dir_hr_q1 <- numeric(2944)
results_hr_inc$sc2_ind_hr_q1 <- results$scenario2 * inc_q1$HR_P3_S14 *100
results_hr_inc$sc2_tot_hr_q1 <- results_hr_inc$sc2_dir_hr_q1 + results_hr_inc$sc2_ind_hr_q1
Gesamteffekte_inc_hr$hr_sc2_q1 <- c(sum(results_hr_inc$sc2_dir_hr_q1),sum(results_hr_inc$sc2_ind_hr_q1), sum(results_hr_inc$sc2_tot_hr_q1))

results_hr_inc$sc2_dir_hr_q2 <- numeric(2944) 
results_hr_inc$sc2_ind_hr_q2 <- results$scenario2 * inc_q2$HR_P3_S14 *100 
results_hr_inc$sc2_tot_hr_q2 <- results_hr_inc$sc2_dir_hr_q2 + results_hr_inc$sc2_ind_hr_q2 
Gesamteffekte_inc_hr$hr_sc2_q2 <- c(sum(results_hr_inc$sc2_dir_hr_q2),sum(results_hr_inc$sc2_ind_hr_q2), sum(results_hr_inc$sc2_tot_hr_q2))
results_hr_inc$sc2_dir_hr_q3 <- numeric(2944) 
results_hr_inc$sc2_ind_hr_q3 <- results$scenario2 * inc_q3$HR_P3_S14 *100 
results_hr_inc$sc2_tot_hr_q3 <- results_hr_inc$sc2_dir_hr_q3 + results_hr_inc$sc2_ind_hr_q3 
Gesamteffekte_inc_hr$hr_sc2_q3 <- c(sum(results_hr_inc$sc2_dir_hr_q3),sum(results_hr_inc$sc2_ind_hr_q3), sum(results_hr_inc$sc2_tot_hr_q3))
results_hr_inc$sc2_dir_hr_q4 <- numeric(2944) 
results_hr_inc$sc2_ind_hr_q4 <- results$scenario2 * inc_q4$HR_P3_S14 *100 
results_hr_inc$sc2_tot_hr_q4 <- results_hr_inc$sc2_dir_hr_q4 + results_hr_inc$sc2_ind_hr_q4 
Gesamteffekte_inc_hr$hr_sc2_q4 <- c(sum(results_hr_inc$sc2_dir_hr_q4),sum(results_hr_inc$sc2_ind_hr_q4), sum(results_hr_inc$sc2_tot_hr_q4))
results_hr_inc$sc2_dir_hr_q5 <- numeric(2944) 
results_hr_inc$sc2_ind_hr_q5 <- results$scenario2 * inc_q5$HR_P3_S14 *100 
results_hr_inc$sc2_tot_hr_q5 <- results_hr_inc$sc2_dir_hr_q5 + results_hr_inc$sc2_ind_hr_q5 
Gesamteffekte_inc_hr$hr_sc2_q5 <- c(sum(results_hr_inc$sc2_dir_hr_q5),sum(results_hr_inc$sc2_ind_hr_q5), sum(results_hr_inc$sc2_tot_hr_q5))

#Szenario4
results_hr_inc$sc4_dir_hr_q1 <- numeric(2944)
results_hr_inc$sc4_dir_hr_q1[HR] <- shocks_scenario4[HR] * inc_q1$HR_P3_S14[HR] *100
results_hr_inc$sc4_ind_hr_q1 <- results$scenario4 * inc_q1$HR_P3_S14 *100
results_hr_inc$sc4_tot_hr_q1 <- results_hr_inc$sc4_dir_hr_q1 + results_hr_inc$sc4_ind_hr_q1
Gesamteffekte_inc_hr$hr_sc4_q1 <- c(sum(results_hr_inc$sc4_dir_hr_q1),sum(results_hr_inc$sc4_ind_hr_q1), sum(results_hr_inc$sc4_tot_hr_q1))
results_hr_inc$sc4_dir_hr_q2 <- numeric(2944)
results_hr_inc$sc4_dir_hr_q2[HR] <- shocks_scenario4[HR] * inc_q2$HR_P3_S14[HR] *100
results_hr_inc$sc4_ind_hr_q2 <- results$scenario4 * inc_q2$HR_P3_S14 *100
results_hr_inc$sc4_tot_hr_q2 <- results_hr_inc$sc4_dir_hr_q2 + results_hr_inc$sc4_ind_hr_q2
Gesamteffekte_inc_hr$hr_sc4_q2 <- c(sum(results_hr_inc$sc4_dir_hr_q2),sum(results_hr_inc$sc4_ind_hr_q2), sum(results_hr_inc$sc4_tot_hr_q2))
results_hr_inc$sc4_dir_hr_q3 <- numeric(2944)
results_hr_inc$sc4_dir_hr_q3[HR] <- shocks_scenario4[HR] * inc_q3$HR_P3_S14[HR] *100
results_hr_inc$sc4_ind_hr_q3 <- results$scenario4 * inc_q3$HR_P3_S14 *100
results_hr_inc$sc4_tot_hr_q3 <- results_hr_inc$sc4_dir_hr_q3 + results_hr_inc$sc4_ind_hr_q3
Gesamteffekte_inc_hr$hr_sc4_q3 <- c(sum(results_hr_inc$sc4_dir_hr_q3),sum(results_hr_inc$sc4_ind_hr_q3), sum(results_hr_inc$sc4_tot_hr_q3))
results_hr_inc$sc4_dir_hr_q4 <- numeric(2944)
results_hr_inc$sc4_dir_hr_q4[HR] <- shocks_scenario4[HR] * inc_q4$HR_P3_S14[HR] *100
results_hr_inc$sc4_ind_hr_q4 <- results$scenario4 * inc_q4$HR_P3_S14 *100
results_hr_inc$sc4_tot_hr_q4 <- results_hr_inc$sc4_dir_hr_q4 + results_hr_inc$sc4_ind_hr_q4
Gesamteffekte_inc_hr$hr_sc4_q4 <- c(sum(results_hr_inc$sc4_dir_hr_q4),sum(results_hr_inc$sc4_ind_hr_q4), sum(results_hr_inc$sc4_tot_hr_q4))
results_hr_inc$sc4_dir_hr_q5 <- numeric(2944)
results_hr_inc$sc4_ind_hr_q5 <- results$scenario4 * inc_q5$HR_P3_S14 *100
results_hr_inc$sc4_dir_hr_q5[HR] <- shocks_scenario4[HR] * inc_q5$HR_P3_S14[HR] *100
results_hr_inc$sc4_tot_hr_q5 <- results_hr_inc$sc4_dir_hr_q5 + results_hr_inc$sc4_ind_hr_q5
Gesamteffekte_inc_hr$hr_sc4_q5 <- c(sum(results_hr_inc$sc4_dir_hr_q5),sum(results_hr_inc$sc4_ind_hr_q5), sum(results_hr_inc$sc4_tot_hr_q5))


#Ungarn
#results df
results_hu_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_hu <- data.frame(Result = c("direct", "indirect", "total"))
results_hu_inc$sc2_dir_hu_q1 <- numeric(2944)
results_hu_inc$sc2_ind_hu_q1 <- results$scenario2 * inc_q1$HU_P3_S14 *100
results_hu_inc$sc2_tot_hu_q1 <- results_hu_inc$sc2_dir_hu_q1 + results_hu_inc$sc2_ind_hu_q1
Gesamteffekte_inc_hu$hu_sc2_q1 <- c(sum(results_hu_inc$sc2_dir_hu_q1),sum(results_hu_inc$sc2_ind_hu_q1), sum(results_hu_inc$sc2_tot_hu_q1))

results_hu_inc$sc2_dir_hu_q2 <- numeric(2944) 
results_hu_inc$sc2_ind_hu_q2 <- results$scenario2 * inc_q2$HU_P3_S14 *100 
results_hu_inc$sc2_tot_hu_q2 <- results_hu_inc$sc2_dir_hu_q2 + results_hu_inc$sc2_ind_hu_q2 
Gesamteffekte_inc_hu$hu_sc2_q2 <- c(sum(results_hu_inc$sc2_dir_hu_q2),sum(results_hu_inc$sc2_ind_hu_q2), sum(results_hu_inc$sc2_tot_hu_q2))
results_hu_inc$sc2_dir_hu_q3 <- numeric(2944) 
results_hu_inc$sc2_ind_hu_q3 <- results$scenario2 * inc_q3$HU_P3_S14 *100 
results_hu_inc$sc2_tot_hu_q3 <- results_hu_inc$sc2_dir_hu_q3 + results_hu_inc$sc2_ind_hu_q3 
Gesamteffekte_inc_hu$hu_sc2_q3 <- c(sum(results_hu_inc$sc2_dir_hu_q3),sum(results_hu_inc$sc2_ind_hu_q3), sum(results_hu_inc$sc2_tot_hu_q3))
results_hu_inc$sc2_dir_hu_q4 <- numeric(2944) 
results_hu_inc$sc2_ind_hu_q4 <- results$scenario2 * inc_q4$HU_P3_S14 *100 
results_hu_inc$sc2_tot_hu_q4 <- results_hu_inc$sc2_dir_hu_q4 + results_hu_inc$sc2_ind_hu_q4 
Gesamteffekte_inc_hu$hu_sc2_q4 <- c(sum(results_hu_inc$sc2_dir_hu_q4),sum(results_hu_inc$sc2_ind_hu_q4), sum(results_hu_inc$sc2_tot_hu_q4))
results_hu_inc$sc2_dir_hu_q5 <- numeric(2944) 
results_hu_inc$sc2_ind_hu_q5 <- results$scenario2 * inc_q5$HU_P3_S14 *100 
results_hu_inc$sc2_tot_hu_q5 <- results_hu_inc$sc2_dir_hu_q5 + results_hu_inc$sc2_ind_hu_q5 
Gesamteffekte_inc_hu$hu_sc2_q5 <- c(sum(results_hu_inc$sc2_dir_hu_q5),sum(results_hu_inc$sc2_ind_hu_q5), sum(results_hu_inc$sc2_tot_hu_q5))

#Szenario4
results_hu_inc$sc4_dir_hu_q1 <- numeric(2944)
results_hu_inc$sc4_dir_hu_q1[HU] <- shocks_scenario4[HU] * inc_q1$HU_P3_S14[HU] *100
results_hu_inc$sc4_ind_hu_q1 <- results$scenario4 * inc_q1$HU_P3_S14 *100
results_hu_inc$sc4_tot_hu_q1 <- results_hu_inc$sc4_dir_hu_q1 + results_hu_inc$sc4_ind_hu_q1
Gesamteffekte_inc_hu$hu_sc4_q1 <- c(sum(results_hu_inc$sc4_dir_hu_q1),sum(results_hu_inc$sc4_ind_hu_q1), sum(results_hu_inc$sc4_tot_hu_q1))
results_hu_inc$sc4_dir_hu_q2 <- numeric(2944)
results_hu_inc$sc4_dir_hu_q2[HU] <- shocks_scenario4[HU] * inc_q2$HU_P3_S14[HU] *100
results_hu_inc$sc4_ind_hu_q2 <- results$scenario4 * inc_q2$HU_P3_S14 *100
results_hu_inc$sc4_tot_hu_q2 <- results_hu_inc$sc4_dir_hu_q2 + results_hu_inc$sc4_ind_hu_q2
Gesamteffekte_inc_hu$hu_sc4_q2 <- c(sum(results_hu_inc$sc4_dir_hu_q2),sum(results_hu_inc$sc4_ind_hu_q2), sum(results_hu_inc$sc4_tot_hu_q2))
results_hu_inc$sc4_dir_hu_q3 <- numeric(2944)
results_hu_inc$sc4_dir_hu_q3[HU] <- shocks_scenario4[HU] * inc_q3$HU_P3_S14[HU] *100
results_hu_inc$sc4_ind_hu_q3 <- results$scenario4 * inc_q3$HU_P3_S14 *100
results_hu_inc$sc4_tot_hu_q3 <- results_hu_inc$sc4_dir_hu_q3 + results_hu_inc$sc4_ind_hu_q3
Gesamteffekte_inc_hu$hu_sc4_q3 <- c(sum(results_hu_inc$sc4_dir_hu_q3),sum(results_hu_inc$sc4_ind_hu_q3), sum(results_hu_inc$sc4_tot_hu_q3))
results_hu_inc$sc4_dir_hu_q4 <- numeric(2944)
results_hu_inc$sc4_dir_hu_q4[HU] <- shocks_scenario4[HU] * inc_q4$HU_P3_S14[HU] *100
results_hu_inc$sc4_ind_hu_q4 <- results$scenario4 * inc_q4$HU_P3_S14 *100
results_hu_inc$sc4_tot_hu_q4 <- results_hu_inc$sc4_dir_hu_q4 + results_hu_inc$sc4_ind_hu_q4
Gesamteffekte_inc_hu$hu_sc4_q4 <- c(sum(results_hu_inc$sc4_dir_hu_q4),sum(results_hu_inc$sc4_ind_hu_q4), sum(results_hu_inc$sc4_tot_hu_q4))
results_hu_inc$sc4_dir_hu_q5 <- numeric(2944)
results_hu_inc$sc4_ind_hu_q5 <- results$scenario4 * inc_q5$HU_P3_S14 *100
results_hu_inc$sc4_dir_hu_q5[HU] <- shocks_scenario4[HU] * inc_q5$HU_P3_S14[HU] *100
results_hu_inc$sc4_tot_hu_q5 <- results_hu_inc$sc4_dir_hu_q5 + results_hu_inc$sc4_ind_hu_q5
Gesamteffekte_inc_hu$hu_sc4_q5 <- c(sum(results_hu_inc$sc4_dir_hu_q5),sum(results_hu_inc$sc4_ind_hu_q5), sum(results_hu_inc$sc4_tot_hu_q5))


#Litauen
#results df
results_lt_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_lt <- data.frame(Result = c("direct", "indirect", "total"))
results_lt_inc$sc2_dir_lt_q1 <- numeric(2944)
results_lt_inc$sc2_ind_lt_q1 <- results$scenario2 * inc_q1$LT_P3_S14 *100
results_lt_inc$sc2_tot_lt_q1 <- results_lt_inc$sc2_dir_lt_q1 + results_lt_inc$sc2_ind_lt_q1
Gesamteffekte_inc_lt$lt_sc2_q1 <- c(sum(results_lt_inc$sc2_dir_lt_q1),sum(results_lt_inc$sc2_ind_lt_q1), sum(results_lt_inc$sc2_tot_lt_q1))

results_lt_inc$sc2_dir_lt_q2 <- numeric(2944) 
results_lt_inc$sc2_ind_lt_q2 <- results$scenario2 * inc_q2$LT_P3_S14 *100 
results_lt_inc$sc2_tot_lt_q2 <- results_lt_inc$sc2_dir_lt_q2 + results_lt_inc$sc2_ind_lt_q2 
Gesamteffekte_inc_lt$lt_sc2_q2 <- c(sum(results_lt_inc$sc2_dir_lt_q2),sum(results_lt_inc$sc2_ind_lt_q2), sum(results_lt_inc$sc2_tot_lt_q2))
results_lt_inc$sc2_dir_lt_q3 <- numeric(2944) 
results_lt_inc$sc2_ind_lt_q3 <- results$scenario2 * inc_q3$LT_P3_S14 *100 
results_lt_inc$sc2_tot_lt_q3 <- results_lt_inc$sc2_dir_lt_q3 + results_lt_inc$sc2_ind_lt_q3 
Gesamteffekte_inc_lt$lt_sc2_q3 <- c(sum(results_lt_inc$sc2_dir_lt_q3),sum(results_lt_inc$sc2_ind_lt_q3), sum(results_lt_inc$sc2_tot_lt_q3))
results_lt_inc$sc2_dir_lt_q4 <- numeric(2944) 
results_lt_inc$sc2_ind_lt_q4 <- results$scenario2 * inc_q4$LT_P3_S14 *100 
results_lt_inc$sc2_tot_lt_q4 <- results_lt_inc$sc2_dir_lt_q4 + results_lt_inc$sc2_ind_lt_q4 
Gesamteffekte_inc_lt$lt_sc2_q4 <- c(sum(results_lt_inc$sc2_dir_lt_q4),sum(results_lt_inc$sc2_ind_lt_q4), sum(results_lt_inc$sc2_tot_lt_q4))
results_lt_inc$sc2_dir_lt_q5 <- numeric(2944) 
results_lt_inc$sc2_ind_lt_q5 <- results$scenario2 * inc_q5$LT_P3_S14 *100 
results_lt_inc$sc2_tot_lt_q5 <- results_lt_inc$sc2_dir_lt_q5 + results_lt_inc$sc2_ind_lt_q5 
Gesamteffekte_inc_lt$lt_sc2_q5 <- c(sum(results_lt_inc$sc2_dir_lt_q5),sum(results_lt_inc$sc2_ind_lt_q5), sum(results_lt_inc$sc2_tot_lt_q5))

#Szenario4
results_lt_inc$sc4_dir_lt_q1 <- numeric(2944)
results_lt_inc$sc4_dir_lt_q1[LT] <- shocks_scenario4[LT] * inc_q1$LT_P3_S14[LT] *100
results_lt_inc$sc4_ind_lt_q1 <- results$scenario4 * inc_q1$LT_P3_S14 *100
results_lt_inc$sc4_tot_lt_q1 <- results_lt_inc$sc4_dir_lt_q1 + results_lt_inc$sc4_ind_lt_q1
Gesamteffekte_inc_lt$lt_sc4_q1 <- c(sum(results_lt_inc$sc4_dir_lt_q1),sum(results_lt_inc$sc4_ind_lt_q1), sum(results_lt_inc$sc4_tot_lt_q1))
results_lt_inc$sc4_dir_lt_q2 <- numeric(2944)
results_lt_inc$sc4_dir_lt_q2[LT] <- shocks_scenario4[LT] * inc_q2$LT_P3_S14[LT] *100
results_lt_inc$sc4_ind_lt_q2 <- results$scenario4 * inc_q2$LT_P3_S14 *100
results_lt_inc$sc4_tot_lt_q2 <- results_lt_inc$sc4_dir_lt_q2 + results_lt_inc$sc4_ind_lt_q2
Gesamteffekte_inc_lt$lt_sc4_q2 <- c(sum(results_lt_inc$sc4_dir_lt_q2),sum(results_lt_inc$sc4_ind_lt_q2), sum(results_lt_inc$sc4_tot_lt_q2))
results_lt_inc$sc4_dir_lt_q3 <- numeric(2944)
results_lt_inc$sc4_dir_lt_q3[LT] <- shocks_scenario4[LT] * inc_q3$LT_P3_S14[LT] *100
results_lt_inc$sc4_ind_lt_q3 <- results$scenario4 * inc_q3$LT_P3_S14 *100
results_lt_inc$sc4_tot_lt_q3 <- results_lt_inc$sc4_dir_lt_q3 + results_lt_inc$sc4_ind_lt_q3
Gesamteffekte_inc_lt$lt_sc4_q3 <- c(sum(results_lt_inc$sc4_dir_lt_q3),sum(results_lt_inc$sc4_ind_lt_q3), sum(results_lt_inc$sc4_tot_lt_q3))
results_lt_inc$sc4_dir_lt_q4 <- numeric(2944)
results_lt_inc$sc4_dir_lt_q4[LT] <- shocks_scenario4[LT] * inc_q4$LT_P3_S14[LT] *100
results_lt_inc$sc4_ind_lt_q4 <- results$scenario4 * inc_q4$LT_P3_S14 *100
results_lt_inc$sc4_tot_lt_q4 <- results_lt_inc$sc4_dir_lt_q4 + results_lt_inc$sc4_ind_lt_q4
Gesamteffekte_inc_lt$lt_sc4_q4 <- c(sum(results_lt_inc$sc4_dir_lt_q4),sum(results_lt_inc$sc4_ind_lt_q4), sum(results_lt_inc$sc4_tot_lt_q4))
results_lt_inc$sc4_dir_lt_q5 <- numeric(2944)
results_lt_inc$sc4_ind_lt_q5 <- results$scenario4 * inc_q5$LT_P3_S14 *100
results_lt_inc$sc4_dir_lt_q5[LT] <- shocks_scenario4[LT] * inc_q5$LT_P3_S14[LT] *100
results_lt_inc$sc4_tot_lt_q5 <- results_lt_inc$sc4_dir_lt_q5 + results_lt_inc$sc4_ind_lt_q5
Gesamteffekte_inc_lt$lt_sc4_q5 <- c(sum(results_lt_inc$sc4_dir_lt_q5),sum(results_lt_inc$sc4_ind_lt_q5), sum(results_lt_inc$sc4_tot_lt_q5))


#Luxemburg
#results df
results_lu_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_lu <- data.frame(Result = c("direct", "indirect", "total"))
results_lu_inc$sc2_dir_lu_q1 <- numeric(2944)
results_lu_inc$sc2_ind_lu_q1 <- results$scenario2 * inc_q1$LU_P3_S14 *100
results_lu_inc$sc2_tot_lu_q1 <- results_lu_inc$sc2_dir_lu_q1 + results_lu_inc$sc2_ind_lu_q1
Gesamteffekte_inc_lu$lu_sc2_q1 <- c(sum(results_lu_inc$sc2_dir_lu_q1),sum(results_lu_inc$sc2_ind_lu_q1), sum(results_lu_inc$sc2_tot_lu_q1))

results_lu_inc$sc2_dir_lu_q2 <- numeric(2944) 
results_lu_inc$sc2_ind_lu_q2 <- results$scenario2 * inc_q2$LU_P3_S14 *100 
results_lu_inc$sc2_tot_lu_q2 <- results_lu_inc$sc2_dir_lu_q2 + results_lu_inc$sc2_ind_lu_q2 
Gesamteffekte_inc_lu$lu_sc2_q2 <- c(sum(results_lu_inc$sc2_dir_lu_q2),sum(results_lu_inc$sc2_ind_lu_q2), sum(results_lu_inc$sc2_tot_lu_q2))
results_lu_inc$sc2_dir_lu_q3 <- numeric(2944) 
results_lu_inc$sc2_ind_lu_q3 <- results$scenario2 * inc_q3$LU_P3_S14 *100 
results_lu_inc$sc2_tot_lu_q3 <- results_lu_inc$sc2_dir_lu_q3 + results_lu_inc$sc2_ind_lu_q3 
Gesamteffekte_inc_lu$lu_sc2_q3 <- c(sum(results_lu_inc$sc2_dir_lu_q3),sum(results_lu_inc$sc2_ind_lu_q3), sum(results_lu_inc$sc2_tot_lu_q3))
results_lu_inc$sc2_dir_lu_q4 <- numeric(2944) 
results_lu_inc$sc2_ind_lu_q4 <- results$scenario2 * inc_q4$LU_P3_S14 *100 
results_lu_inc$sc2_tot_lu_q4 <- results_lu_inc$sc2_dir_lu_q4 + results_lu_inc$sc2_ind_lu_q4 
Gesamteffekte_inc_lu$lu_sc2_q4 <- c(sum(results_lu_inc$sc2_dir_lu_q4),sum(results_lu_inc$sc2_ind_lu_q4), sum(results_lu_inc$sc2_tot_lu_q4))
results_lu_inc$sc2_dir_lu_q5 <- numeric(2944) 
results_lu_inc$sc2_ind_lu_q5 <- results$scenario2 * inc_q5$LU_P3_S14 *100 
results_lu_inc$sc2_tot_lu_q5 <- results_lu_inc$sc2_dir_lu_q5 + results_lu_inc$sc2_ind_lu_q5 
Gesamteffekte_inc_lu$lu_sc2_q5 <- c(sum(results_lu_inc$sc2_dir_lu_q5),sum(results_lu_inc$sc2_ind_lu_q5), sum(results_lu_inc$sc2_tot_lu_q5))

#Szenario4
results_lu_inc$sc4_dir_lu_q1 <- numeric(2944)
results_lu_inc$sc4_dir_lu_q1[LU] <- shocks_scenario4[LU] * inc_q1$LU_P3_S14[LU] *100
results_lu_inc$sc4_ind_lu_q1 <- results$scenario4 * inc_q1$LU_P3_S14 *100
results_lu_inc$sc4_tot_lu_q1 <- results_lu_inc$sc4_dir_lu_q1 + results_lu_inc$sc4_ind_lu_q1
Gesamteffekte_inc_lu$lu_sc4_q1 <- c(sum(results_lu_inc$sc4_dir_lu_q1),sum(results_lu_inc$sc4_ind_lu_q1), sum(results_lu_inc$sc4_tot_lu_q1))
results_lu_inc$sc4_dir_lu_q2 <- numeric(2944)
results_lu_inc$sc4_dir_lu_q2[LU] <- shocks_scenario4[LU] * inc_q2$LU_P3_S14[LU] *100
results_lu_inc$sc4_ind_lu_q2 <- results$scenario4 * inc_q2$LU_P3_S14 *100
results_lu_inc$sc4_tot_lu_q2 <- results_lu_inc$sc4_dir_lu_q2 + results_lu_inc$sc4_ind_lu_q2
Gesamteffekte_inc_lu$lu_sc4_q2 <- c(sum(results_lu_inc$sc4_dir_lu_q2),sum(results_lu_inc$sc4_ind_lu_q2), sum(results_lu_inc$sc4_tot_lu_q2))
results_lu_inc$sc4_dir_lu_q3 <- numeric(2944)
results_lu_inc$sc4_dir_lu_q3[LU] <- shocks_scenario4[LU] * inc_q3$LU_P3_S14[LU] *100
results_lu_inc$sc4_ind_lu_q3 <- results$scenario4 * inc_q3$LU_P3_S14 *100
results_lu_inc$sc4_tot_lu_q3 <- results_lu_inc$sc4_dir_lu_q3 + results_lu_inc$sc4_ind_lu_q3
Gesamteffekte_inc_lu$lu_sc4_q3 <- c(sum(results_lu_inc$sc4_dir_lu_q3),sum(results_lu_inc$sc4_ind_lu_q3), sum(results_lu_inc$sc4_tot_lu_q3))
results_lu_inc$sc4_dir_lu_q4 <- numeric(2944)
results_lu_inc$sc4_dir_lu_q4[LU] <- shocks_scenario4[LU] * inc_q4$LU_P3_S14[LU] *100
results_lu_inc$sc4_ind_lu_q4 <- results$scenario4 * inc_q4$LU_P3_S14 *100
results_lu_inc$sc4_tot_lu_q4 <- results_lu_inc$sc4_dir_lu_q4 + results_lu_inc$sc4_ind_lu_q4
Gesamteffekte_inc_lu$lu_sc4_q4 <- c(sum(results_lu_inc$sc4_dir_lu_q4),sum(results_lu_inc$sc4_ind_lu_q4), sum(results_lu_inc$sc4_tot_lu_q4))
results_lu_inc$sc4_dir_lu_q5 <- numeric(2944)
results_lu_inc$sc4_ind_lu_q5 <- results$scenario4 * inc_q5$LU_P3_S14 *100
results_lu_inc$sc4_dir_lu_q5[LU] <- shocks_scenario4[LU] * inc_q5$LU_P3_S14[LU] *100
results_lu_inc$sc4_tot_lu_q5 <- results_lu_inc$sc4_dir_lu_q5 + results_lu_inc$sc4_ind_lu_q5
Gesamteffekte_inc_lu$lu_sc4_q5 <- c(sum(results_lu_inc$sc4_dir_lu_q5),sum(results_lu_inc$sc4_ind_lu_q5), sum(results_lu_inc$sc4_tot_lu_q5))


#Lettland
#results df
results_lv_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_lv <- data.frame(Result = c("direct", "indirect", "total"))
results_lv_inc$sc2_dir_lv_q1 <- numeric(2944)
results_lv_inc$sc2_ind_lv_q1 <- results$scenario2 * inc_q1$LV_P3_S14 *100
results_lv_inc$sc2_tot_lv_q1 <- results_lv_inc$sc2_dir_lv_q1 + results_lv_inc$sc2_ind_lv_q1
Gesamteffekte_inc_lv$lv_sc2_q1 <- c(sum(results_lv_inc$sc2_dir_lv_q1),sum(results_lv_inc$sc2_ind_lv_q1), sum(results_lv_inc$sc2_tot_lv_q1))

results_lv_inc$sc2_dir_lv_q2 <- numeric(2944) 
results_lv_inc$sc2_ind_lv_q2 <- results$scenario2 * inc_q2$LV_P3_S14 *100 
results_lv_inc$sc2_tot_lv_q2 <- results_lv_inc$sc2_dir_lv_q2 + results_lv_inc$sc2_ind_lv_q2 
Gesamteffekte_inc_lv$lv_sc2_q2 <- c(sum(results_lv_inc$sc2_dir_lv_q2),sum(results_lv_inc$sc2_ind_lv_q2), sum(results_lv_inc$sc2_tot_lv_q2))
results_lv_inc$sc2_dir_lv_q3 <- numeric(2944) 
results_lv_inc$sc2_ind_lv_q3 <- results$scenario2 * inc_q3$LV_P3_S14 *100 
results_lv_inc$sc2_tot_lv_q3 <- results_lv_inc$sc2_dir_lv_q3 + results_lv_inc$sc2_ind_lv_q3 
Gesamteffekte_inc_lv$lv_sc2_q3 <- c(sum(results_lv_inc$sc2_dir_lv_q3),sum(results_lv_inc$sc2_ind_lv_q3), sum(results_lv_inc$sc2_tot_lv_q3))
results_lv_inc$sc2_dir_lv_q4 <- numeric(2944) 
results_lv_inc$sc2_ind_lv_q4 <- results$scenario2 * inc_q4$LV_P3_S14 *100 
results_lv_inc$sc2_tot_lv_q4 <- results_lv_inc$sc2_dir_lv_q4 + results_lv_inc$sc2_ind_lv_q4 
Gesamteffekte_inc_lv$lv_sc2_q4 <- c(sum(results_lv_inc$sc2_dir_lv_q4),sum(results_lv_inc$sc2_ind_lv_q4), sum(results_lv_inc$sc2_tot_lv_q4))
results_lv_inc$sc2_dir_lv_q5 <- numeric(2944) 
results_lv_inc$sc2_ind_lv_q5 <- results$scenario2 * inc_q5$LV_P3_S14 *100 
results_lv_inc$sc2_tot_lv_q5 <- results_lv_inc$sc2_dir_lv_q5 + results_lv_inc$sc2_ind_lv_q5 
Gesamteffekte_inc_lv$lv_sc2_q5 <- c(sum(results_lv_inc$sc2_dir_lv_q5),sum(results_lv_inc$sc2_ind_lv_q5), sum(results_lv_inc$sc2_tot_lv_q5))

#Szenario4
results_lv_inc$sc4_dir_lv_q1 <- numeric(2944)
results_lv_inc$sc4_dir_lv_q1[LV] <- shocks_scenario4[LV] * inc_q1$LV_P3_S14[LV] *100
results_lv_inc$sc4_ind_lv_q1 <- results$scenario4 * inc_q1$LV_P3_S14 *100
results_lv_inc$sc4_tot_lv_q1 <- results_lv_inc$sc4_dir_lv_q1 + results_lv_inc$sc4_ind_lv_q1
Gesamteffekte_inc_lv$lv_sc4_q1 <- c(sum(results_lv_inc$sc4_dir_lv_q1),sum(results_lv_inc$sc4_ind_lv_q1), sum(results_lv_inc$sc4_tot_lv_q1))
results_lv_inc$sc4_dir_lv_q2 <- numeric(2944)
results_lv_inc$sc4_dir_lv_q2[LV] <- shocks_scenario4[LV] * inc_q2$LV_P3_S14[LV] *100
results_lv_inc$sc4_ind_lv_q2 <- results$scenario4 * inc_q2$LV_P3_S14 *100
results_lv_inc$sc4_tot_lv_q2 <- results_lv_inc$sc4_dir_lv_q2 + results_lv_inc$sc4_ind_lv_q2
Gesamteffekte_inc_lv$lv_sc4_q2 <- c(sum(results_lv_inc$sc4_dir_lv_q2),sum(results_lv_inc$sc4_ind_lv_q2), sum(results_lv_inc$sc4_tot_lv_q2))
results_lv_inc$sc4_dir_lv_q3 <- numeric(2944)
results_lv_inc$sc4_dir_lv_q3[LV] <- shocks_scenario4[LV] * inc_q3$LV_P3_S14[LV] *100
results_lv_inc$sc4_ind_lv_q3 <- results$scenario4 * inc_q3$LV_P3_S14 *100
results_lv_inc$sc4_tot_lv_q3 <- results_lv_inc$sc4_dir_lv_q3 + results_lv_inc$sc4_ind_lv_q3
Gesamteffekte_inc_lv$lv_sc4_q3 <- c(sum(results_lv_inc$sc4_dir_lv_q3),sum(results_lv_inc$sc4_ind_lv_q3), sum(results_lv_inc$sc4_tot_lv_q3))
results_lv_inc$sc4_dir_lv_q4 <- numeric(2944)
results_lv_inc$sc4_dir_lv_q4[LV] <- shocks_scenario4[LV] * inc_q4$LV_P3_S14[LV] *100
results_lv_inc$sc4_ind_lv_q4 <- results$scenario4 * inc_q4$LV_P3_S14 *100
results_lv_inc$sc4_tot_lv_q4 <- results_lv_inc$sc4_dir_lv_q4 + results_lv_inc$sc4_ind_lv_q4
Gesamteffekte_inc_lv$lv_sc4_q4 <- c(sum(results_lv_inc$sc4_dir_lv_q4),sum(results_lv_inc$sc4_ind_lv_q4), sum(results_lv_inc$sc4_tot_lv_q4))
results_lv_inc$sc4_dir_lv_q5 <- numeric(2944)
results_lv_inc$sc4_ind_lv_q5 <- results$scenario4 * inc_q5$LV_P3_S14 *100
results_lv_inc$sc4_dir_lv_q5[LV] <- shocks_scenario4[LV] * inc_q5$LV_P3_S14[LV] *100
results_lv_inc$sc4_tot_lv_q5 <- results_lv_inc$sc4_dir_lv_q5 + results_lv_inc$sc4_ind_lv_q5
Gesamteffekte_inc_lv$lv_sc4_q5 <- c(sum(results_lv_inc$sc4_dir_lv_q5),sum(results_lv_inc$sc4_ind_lv_q5), sum(results_lv_inc$sc4_tot_lv_q5))

#Malta
#results df
results_mt_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_mt <- data.frame(Result = c("direct", "indirect", "total"))
results_mt_inc$sc2_dir_mt_q1 <- numeric(2944)
results_mt_inc$sc2_ind_mt_q1 <- results$scenario2 * inc_q1$MT_P3_S14 *100
results_mt_inc$sc2_tot_mt_q1 <- results_mt_inc$sc2_dir_mt_q1 + results_mt_inc$sc2_ind_mt_q1
Gesamteffekte_inc_mt$mt_sc2_q1 <- c(sum(results_mt_inc$sc2_dir_mt_q1),sum(results_mt_inc$sc2_ind_mt_q1), sum(results_mt_inc$sc2_tot_mt_q1))

results_mt_inc$sc2_dir_mt_q2 <- numeric(2944) 
results_mt_inc$sc2_ind_mt_q2 <- results$scenario2 * inc_q2$MT_P3_S14 *100 
results_mt_inc$sc2_tot_mt_q2 <- results_mt_inc$sc2_dir_mt_q2 + results_mt_inc$sc2_ind_mt_q2 
Gesamteffekte_inc_mt$mt_sc2_q2 <- c(sum(results_mt_inc$sc2_dir_mt_q2),sum(results_mt_inc$sc2_ind_mt_q2), sum(results_mt_inc$sc2_tot_mt_q2))
results_mt_inc$sc2_dir_mt_q3 <- numeric(2944) 
results_mt_inc$sc2_ind_mt_q3 <- results$scenario2 * inc_q3$MT_P3_S14 *100 
results_mt_inc$sc2_tot_mt_q3 <- results_mt_inc$sc2_dir_mt_q3 + results_mt_inc$sc2_ind_mt_q3 
Gesamteffekte_inc_mt$mt_sc2_q3 <- c(sum(results_mt_inc$sc2_dir_mt_q3),sum(results_mt_inc$sc2_ind_mt_q3), sum(results_mt_inc$sc2_tot_mt_q3))
results_mt_inc$sc2_dir_mt_q4 <- numeric(2944) 
results_mt_inc$sc2_ind_mt_q4 <- results$scenario2 * inc_q4$MT_P3_S14 *100 
results_mt_inc$sc2_tot_mt_q4 <- results_mt_inc$sc2_dir_mt_q4 + results_mt_inc$sc2_ind_mt_q4 
Gesamteffekte_inc_mt$mt_sc2_q4 <- c(sum(results_mt_inc$sc2_dir_mt_q4),sum(results_mt_inc$sc2_ind_mt_q4), sum(results_mt_inc$sc2_tot_mt_q4))
results_mt_inc$sc2_dir_mt_q5 <- numeric(2944) 
results_mt_inc$sc2_ind_mt_q5 <- results$scenario2 * inc_q5$MT_P3_S14 *100 
results_mt_inc$sc2_tot_mt_q5 <- results_mt_inc$sc2_dir_mt_q5 + results_mt_inc$sc2_ind_mt_q5 
Gesamteffekte_inc_mt$mt_sc2_q5 <- c(sum(results_mt_inc$sc2_dir_mt_q5),sum(results_mt_inc$sc2_ind_mt_q5), sum(results_mt_inc$sc2_tot_mt_q5))

#Szenario4
results_mt_inc$sc4_dir_mt_q1 <- numeric(2944)
results_mt_inc$sc4_dir_mt_q1[MT] <- shocks_scenario4[MT] * inc_q1$MT_P3_S14[MT] *100
results_mt_inc$sc4_ind_mt_q1 <- results$scenario4 * inc_q1$MT_P3_S14 *100
results_mt_inc$sc4_tot_mt_q1 <- results_mt_inc$sc4_dir_mt_q1 + results_mt_inc$sc4_ind_mt_q1
Gesamteffekte_inc_mt$mt_sc4_q1 <- c(sum(results_mt_inc$sc4_dir_mt_q1),sum(results_mt_inc$sc4_ind_mt_q1), sum(results_mt_inc$sc4_tot_mt_q1))
results_mt_inc$sc4_dir_mt_q2 <- numeric(2944)
results_mt_inc$sc4_dir_mt_q2[MT] <- shocks_scenario4[MT] * inc_q2$MT_P3_S14[MT] *100
results_mt_inc$sc4_ind_mt_q2 <- results$scenario4 * inc_q2$MT_P3_S14 *100
results_mt_inc$sc4_tot_mt_q2 <- results_mt_inc$sc4_dir_mt_q2 + results_mt_inc$sc4_ind_mt_q2
Gesamteffekte_inc_mt$mt_sc4_q2 <- c(sum(results_mt_inc$sc4_dir_mt_q2),sum(results_mt_inc$sc4_ind_mt_q2), sum(results_mt_inc$sc4_tot_mt_q2))
results_mt_inc$sc4_dir_mt_q3 <- numeric(2944)
results_mt_inc$sc4_dir_mt_q3[MT] <- shocks_scenario4[MT] * inc_q3$MT_P3_S14[MT] *100
results_mt_inc$sc4_ind_mt_q3 <- results$scenario4 * inc_q3$MT_P3_S14 *100
results_mt_inc$sc4_tot_mt_q3 <- results_mt_inc$sc4_dir_mt_q3 + results_mt_inc$sc4_ind_mt_q3
Gesamteffekte_inc_mt$mt_sc4_q3 <- c(sum(results_mt_inc$sc4_dir_mt_q3),sum(results_mt_inc$sc4_ind_mt_q3), sum(results_mt_inc$sc4_tot_mt_q3))
results_mt_inc$sc4_dir_mt_q4 <- numeric(2944)
results_mt_inc$sc4_dir_mt_q4[MT] <- shocks_scenario4[MT] * inc_q4$MT_P3_S14[MT] *100
results_mt_inc$sc4_ind_mt_q4 <- results$scenario4 * inc_q4$MT_P3_S14 *100
results_mt_inc$sc4_tot_mt_q4 <- results_mt_inc$sc4_dir_mt_q4 + results_mt_inc$sc4_ind_mt_q4
Gesamteffekte_inc_mt$mt_sc4_q4 <- c(sum(results_mt_inc$sc4_dir_mt_q4),sum(results_mt_inc$sc4_ind_mt_q4), sum(results_mt_inc$sc4_tot_mt_q4))
results_mt_inc$sc4_dir_mt_q5 <- numeric(2944)
results_mt_inc$sc4_ind_mt_q5 <- results$scenario4 * inc_q5$MT_P3_S14 *100
results_mt_inc$sc4_dir_mt_q5[MT] <- shocks_scenario4[MT] * inc_q5$MT_P3_S14[MT] *100
results_mt_inc$sc4_tot_mt_q5 <- results_mt_inc$sc4_dir_mt_q5 + results_mt_inc$sc4_ind_mt_q5
Gesamteffekte_inc_mt$mt_sc4_q5 <- c(sum(results_mt_inc$sc4_dir_mt_q5),sum(results_mt_inc$sc4_ind_mt_q5), sum(results_mt_inc$sc4_tot_mt_q5))

#Niederlande
#results df
results_nl_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_nl <- data.frame(Result = c("direct", "indirect", "total"))
results_nl_inc$sc2_dir_nl_q1 <- numeric(2944)
results_nl_inc$sc2_ind_nl_q1 <- results$scenario2 * inc_q1$NL_P3_S14 *100
results_nl_inc$sc2_tot_nl_q1 <- results_nl_inc$sc2_dir_nl_q1 + results_nl_inc$sc2_ind_nl_q1
Gesamteffekte_inc_nl$nl_sc2_q1 <- c(sum(results_nl_inc$sc2_dir_nl_q1),sum(results_nl_inc$sc2_ind_nl_q1), sum(results_nl_inc$sc2_tot_nl_q1))

results_nl_inc$sc2_dir_nl_q2 <- numeric(2944) 
results_nl_inc$sc2_ind_nl_q2 <- results$scenario2 * inc_q2$NL_P3_S14 *100 
results_nl_inc$sc2_tot_nl_q2 <- results_nl_inc$sc2_dir_nl_q2 + results_nl_inc$sc2_ind_nl_q2 
Gesamteffekte_inc_nl$nl_sc2_q2 <- c(sum(results_nl_inc$sc2_dir_nl_q2),sum(results_nl_inc$sc2_ind_nl_q2), sum(results_nl_inc$sc2_tot_nl_q2))
results_nl_inc$sc2_dir_nl_q3 <- numeric(2944) 
results_nl_inc$sc2_ind_nl_q3 <- results$scenario2 * inc_q3$NL_P3_S14 *100 
results_nl_inc$sc2_tot_nl_q3 <- results_nl_inc$sc2_dir_nl_q3 + results_nl_inc$sc2_ind_nl_q3 
Gesamteffekte_inc_nl$nl_sc2_q3 <- c(sum(results_nl_inc$sc2_dir_nl_q3),sum(results_nl_inc$sc2_ind_nl_q3), sum(results_nl_inc$sc2_tot_nl_q3))
results_nl_inc$sc2_dir_nl_q4 <- numeric(2944) 
results_nl_inc$sc2_ind_nl_q4 <- results$scenario2 * inc_q4$NL_P3_S14 *100 
results_nl_inc$sc2_tot_nl_q4 <- results_nl_inc$sc2_dir_nl_q4 + results_nl_inc$sc2_ind_nl_q4 
Gesamteffekte_inc_nl$nl_sc2_q4 <- c(sum(results_nl_inc$sc2_dir_nl_q4),sum(results_nl_inc$sc2_ind_nl_q4), sum(results_nl_inc$sc2_tot_nl_q4))
results_nl_inc$sc2_dir_nl_q5 <- numeric(2944) 
results_nl_inc$sc2_ind_nl_q5 <- results$scenario2 * inc_q5$NL_P3_S14 *100 
results_nl_inc$sc2_tot_nl_q5 <- results_nl_inc$sc2_dir_nl_q5 + results_nl_inc$sc2_ind_nl_q5 
Gesamteffekte_inc_nl$nl_sc2_q5 <- c(sum(results_nl_inc$sc2_dir_nl_q5),sum(results_nl_inc$sc2_ind_nl_q5), sum(results_nl_inc$sc2_tot_nl_q5))

#Szenario4
results_nl_inc$sc4_dir_nl_q1 <- numeric(2944)
results_nl_inc$sc4_dir_nl_q1[NL] <- shocks_scenario4[NL] * inc_q1$NL_P3_S14[NL] *100
results_nl_inc$sc4_ind_nl_q1 <- results$scenario4 * inc_q1$NL_P3_S14 *100
results_nl_inc$sc4_tot_nl_q1 <- results_nl_inc$sc4_dir_nl_q1 + results_nl_inc$sc4_ind_nl_q1
Gesamteffekte_inc_nl$nl_sc4_q1 <- c(sum(results_nl_inc$sc4_dir_nl_q1),sum(results_nl_inc$sc4_ind_nl_q1), sum(results_nl_inc$sc4_tot_nl_q1))
results_nl_inc$sc4_dir_nl_q2 <- numeric(2944)
results_nl_inc$sc4_dir_nl_q2[NL] <- shocks_scenario4[NL] * inc_q2$NL_P3_S14[NL] *100
results_nl_inc$sc4_ind_nl_q2 <- results$scenario4 * inc_q2$NL_P3_S14 *100
results_nl_inc$sc4_tot_nl_q2 <- results_nl_inc$sc4_dir_nl_q2 + results_nl_inc$sc4_ind_nl_q2
Gesamteffekte_inc_nl$nl_sc4_q2 <- c(sum(results_nl_inc$sc4_dir_nl_q2),sum(results_nl_inc$sc4_ind_nl_q2), sum(results_nl_inc$sc4_tot_nl_q2))
results_nl_inc$sc4_dir_nl_q3 <- numeric(2944)
results_nl_inc$sc4_dir_nl_q3[NL] <- shocks_scenario4[NL] * inc_q3$NL_P3_S14[NL] *100
results_nl_inc$sc4_ind_nl_q3 <- results$scenario4 * inc_q3$NL_P3_S14 *100
results_nl_inc$sc4_tot_nl_q3 <- results_nl_inc$sc4_dir_nl_q3 + results_nl_inc$sc4_ind_nl_q3
Gesamteffekte_inc_nl$nl_sc4_q3 <- c(sum(results_nl_inc$sc4_dir_nl_q3),sum(results_nl_inc$sc4_ind_nl_q3), sum(results_nl_inc$sc4_tot_nl_q3))
results_nl_inc$sc4_dir_nl_q4 <- numeric(2944)
results_nl_inc$sc4_dir_nl_q4[NL] <- shocks_scenario4[NL] * inc_q4$NL_P3_S14[NL] *100
results_nl_inc$sc4_ind_nl_q4 <- results$scenario4 * inc_q4$NL_P3_S14 *100
results_nl_inc$sc4_tot_nl_q4 <- results_nl_inc$sc4_dir_nl_q4 + results_nl_inc$sc4_ind_nl_q4
Gesamteffekte_inc_nl$nl_sc4_q4 <- c(sum(results_nl_inc$sc4_dir_nl_q4),sum(results_nl_inc$sc4_ind_nl_q4), sum(results_nl_inc$sc4_tot_nl_q4))
results_nl_inc$sc4_dir_nl_q5 <- numeric(2944)
results_nl_inc$sc4_ind_nl_q5 <- results$scenario4 * inc_q5$NL_P3_S14 *100
results_nl_inc$sc4_dir_nl_q5[NL] <- shocks_scenario4[NL] * inc_q5$NL_P3_S14[NL] *100
results_nl_inc$sc4_tot_nl_q5 <- results_nl_inc$sc4_dir_nl_q5 + results_nl_inc$sc4_ind_nl_q5
Gesamteffekte_inc_nl$nl_sc4_q5 <- c(sum(results_nl_inc$sc4_dir_nl_q5),sum(results_nl_inc$sc4_ind_nl_q5), sum(results_nl_inc$sc4_tot_nl_q5))



#Polen

#results df
results_pl_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_pl <- data.frame(Result = c("direct", "indirect", "total"))
results_pl_inc$sc2_dir_pl_q1 <- numeric(2944)
results_pl_inc$sc2_ind_pl_q1 <- results$scenario2 * inc_q1$PL_P3_S14 *100
results_pl_inc$sc2_tot_pl_q1 <- results_pl_inc$sc2_dir_pl_q1 + results_pl_inc$sc2_ind_pl_q1
Gesamteffekte_inc_pl$pl_sc2_q1 <- c(sum(results_pl_inc$sc2_dir_pl_q1),sum(results_pl_inc$sc2_ind_pl_q1), sum(results_pl_inc$sc2_tot_pl_q1))

results_pl_inc$sc2_dir_pl_q2 <- numeric(2944) 
results_pl_inc$sc2_ind_pl_q2 <- results$scenario2 * inc_q2$PL_P3_S14 *100 
results_pl_inc$sc2_tot_pl_q2 <- results_pl_inc$sc2_dir_pl_q2 + results_pl_inc$sc2_ind_pl_q2 
Gesamteffekte_inc_pl$pl_sc2_q2 <- c(sum(results_pl_inc$sc2_dir_pl_q2),sum(results_pl_inc$sc2_ind_pl_q2), sum(results_pl_inc$sc2_tot_pl_q2))
results_pl_inc$sc2_dir_pl_q3 <- numeric(2944) 
results_pl_inc$sc2_ind_pl_q3 <- results$scenario2 * inc_q3$PL_P3_S14 *100 
results_pl_inc$sc2_tot_pl_q3 <- results_pl_inc$sc2_dir_pl_q3 + results_pl_inc$sc2_ind_pl_q3 
Gesamteffekte_inc_pl$pl_sc2_q3 <- c(sum(results_pl_inc$sc2_dir_pl_q3),sum(results_pl_inc$sc2_ind_pl_q3), sum(results_pl_inc$sc2_tot_pl_q3))
results_pl_inc$sc2_dir_pl_q4 <- numeric(2944) 
results_pl_inc$sc2_ind_pl_q4 <- results$scenario2 * inc_q4$PL_P3_S14 *100 
results_pl_inc$sc2_tot_pl_q4 <- results_pl_inc$sc2_dir_pl_q4 + results_pl_inc$sc2_ind_pl_q4 
Gesamteffekte_inc_pl$pl_sc2_q4 <- c(sum(results_pl_inc$sc2_dir_pl_q4),sum(results_pl_inc$sc2_ind_pl_q4), sum(results_pl_inc$sc2_tot_pl_q4))
results_pl_inc$sc2_dir_pl_q5 <- numeric(2944) 
results_pl_inc$sc2_ind_pl_q5 <- results$scenario2 * inc_q5$PL_P3_S14 *100 
results_pl_inc$sc2_tot_pl_q5 <- results_pl_inc$sc2_dir_pl_q5 + results_pl_inc$sc2_ind_pl_q5 
Gesamteffekte_inc_pl$pl_sc2_q5 <- c(sum(results_pl_inc$sc2_dir_pl_q5),sum(results_pl_inc$sc2_ind_pl_q5), sum(results_pl_inc$sc2_tot_pl_q5))

#Szenario4
results_pl_inc$sc4_dir_pl_q1 <- numeric(2944)
results_pl_inc$sc4_dir_pl_q1[PL] <- shocks_scenario4[PL] * inc_q1$PL_P3_S14[PL] *100
results_pl_inc$sc4_ind_pl_q1 <- results$scenario4 * inc_q1$PL_P3_S14 *100
results_pl_inc$sc4_tot_pl_q1 <- results_pl_inc$sc4_dir_pl_q1 + results_pl_inc$sc4_ind_pl_q1
Gesamteffekte_inc_pl$pl_sc4_q1 <- c(sum(results_pl_inc$sc4_dir_pl_q1),sum(results_pl_inc$sc4_ind_pl_q1), sum(results_pl_inc$sc4_tot_pl_q1))
results_pl_inc$sc4_dir_pl_q2 <- numeric(2944)
results_pl_inc$sc4_dir_pl_q2[PL] <- shocks_scenario4[PL] * inc_q2$PL_P3_S14[PL] *100
results_pl_inc$sc4_ind_pl_q2 <- results$scenario4 * inc_q2$PL_P3_S14 *100
results_pl_inc$sc4_tot_pl_q2 <- results_pl_inc$sc4_dir_pl_q2 + results_pl_inc$sc4_ind_pl_q2
Gesamteffekte_inc_pl$pl_sc4_q2 <- c(sum(results_pl_inc$sc4_dir_pl_q2),sum(results_pl_inc$sc4_ind_pl_q2), sum(results_pl_inc$sc4_tot_pl_q2))
results_pl_inc$sc4_dir_pl_q3 <- numeric(2944)
results_pl_inc$sc4_dir_pl_q3[PL] <- shocks_scenario4[PL] * inc_q3$PL_P3_S14[PL] *100
results_pl_inc$sc4_ind_pl_q3 <- results$scenario4 * inc_q3$PL_P3_S14 *100
results_pl_inc$sc4_tot_pl_q3 <- results_pl_inc$sc4_dir_pl_q3 + results_pl_inc$sc4_ind_pl_q3
Gesamteffekte_inc_pl$pl_sc4_q3 <- c(sum(results_pl_inc$sc4_dir_pl_q3),sum(results_pl_inc$sc4_ind_pl_q3), sum(results_pl_inc$sc4_tot_pl_q3))
results_pl_inc$sc4_dir_pl_q4 <- numeric(2944)
results_pl_inc$sc4_dir_pl_q4[PL] <- shocks_scenario4[PL] * inc_q4$PL_P3_S14[PL] *100
results_pl_inc$sc4_ind_pl_q4 <- results$scenario4 * inc_q4$PL_P3_S14 *100
results_pl_inc$sc4_tot_pl_q4 <- results_pl_inc$sc4_dir_pl_q4 + results_pl_inc$sc4_ind_pl_q4
Gesamteffekte_inc_pl$pl_sc4_q4 <- c(sum(results_pl_inc$sc4_dir_pl_q4),sum(results_pl_inc$sc4_ind_pl_q4), sum(results_pl_inc$sc4_tot_pl_q4))
results_pl_inc$sc4_dir_pl_q5 <- numeric(2944)
results_pl_inc$sc4_ind_pl_q5 <- results$scenario4 * inc_q5$PL_P3_S14 *100
results_pl_inc$sc4_dir_pl_q5[PL] <- shocks_scenario4[PL] * inc_q5$PL_P3_S14[PL] *100
results_pl_inc$sc4_tot_pl_q5 <- results_pl_inc$sc4_dir_pl_q5 + results_pl_inc$sc4_ind_pl_q5
Gesamteffekte_inc_pl$pl_sc4_q5 <- c(sum(results_pl_inc$sc4_dir_pl_q5),sum(results_pl_inc$sc4_ind_pl_q5), sum(results_pl_inc$sc4_tot_pl_q5))

#Rumänien
#results df
results_ro_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_ro <- data.frame(Result = c("direct", "indirect", "total"))
results_ro_inc$sc2_dir_ro_q1 <- numeric(2944)
results_ro_inc$sc2_ind_ro_q1 <- results$scenario2 * inc_q1$RO_P3_S14 *100
results_ro_inc$sc2_tot_ro_q1 <- results_ro_inc$sc2_dir_ro_q1 + results_ro_inc$sc2_ind_ro_q1
Gesamteffekte_inc_ro$ro_sc2_q1 <- c(sum(results_ro_inc$sc2_dir_ro_q1),sum(results_ro_inc$sc2_ind_ro_q1), sum(results_ro_inc$sc2_tot_ro_q1))

results_ro_inc$sc2_dir_ro_q2 <- numeric(2944) 
results_ro_inc$sc2_ind_ro_q2 <- results$scenario2 * inc_q2$RO_P3_S14 *100 
results_ro_inc$sc2_tot_ro_q2 <- results_ro_inc$sc2_dir_ro_q2 + results_ro_inc$sc2_ind_ro_q2 
Gesamteffekte_inc_ro$ro_sc2_q2 <- c(sum(results_ro_inc$sc2_dir_ro_q2),sum(results_ro_inc$sc2_ind_ro_q2), sum(results_ro_inc$sc2_tot_ro_q2))
results_ro_inc$sc2_dir_ro_q3 <- numeric(2944) 
results_ro_inc$sc2_ind_ro_q3 <- results$scenario2 * inc_q3$RO_P3_S14 *100 
results_ro_inc$sc2_tot_ro_q3 <- results_ro_inc$sc2_dir_ro_q3 + results_ro_inc$sc2_ind_ro_q3 
Gesamteffekte_inc_ro$ro_sc2_q3 <- c(sum(results_ro_inc$sc2_dir_ro_q3),sum(results_ro_inc$sc2_ind_ro_q3), sum(results_ro_inc$sc2_tot_ro_q3))
results_ro_inc$sc2_dir_ro_q4 <- numeric(2944) 
results_ro_inc$sc2_ind_ro_q4 <- results$scenario2 * inc_q4$RO_P3_S14 *100 
results_ro_inc$sc2_tot_ro_q4 <- results_ro_inc$sc2_dir_ro_q4 + results_ro_inc$sc2_ind_ro_q4 
Gesamteffekte_inc_ro$ro_sc2_q4 <- c(sum(results_ro_inc$sc2_dir_ro_q4),sum(results_ro_inc$sc2_ind_ro_q4), sum(results_ro_inc$sc2_tot_ro_q4))
results_ro_inc$sc2_dir_ro_q5 <- numeric(2944) 
results_ro_inc$sc2_ind_ro_q5 <- results$scenario2 * inc_q5$RO_P3_S14 *100 
results_ro_inc$sc2_tot_ro_q5 <- results_ro_inc$sc2_dir_ro_q5 + results_ro_inc$sc2_ind_ro_q5 
Gesamteffekte_inc_ro$ro_sc2_q5 <- c(sum(results_ro_inc$sc2_dir_ro_q5),sum(results_ro_inc$sc2_ind_ro_q5), sum(results_ro_inc$sc2_tot_ro_q5))

#Szenario4
results_ro_inc$sc4_dir_ro_q1 <- numeric(2944)
results_ro_inc$sc4_dir_ro_q1[RO] <- shocks_scenario4[RO] * inc_q1$RO_P3_S14[RO] *100
results_ro_inc$sc4_ind_ro_q1 <- results$scenario4 * inc_q1$RO_P3_S14 *100
results_ro_inc$sc4_tot_ro_q1 <- results_ro_inc$sc4_dir_ro_q1 + results_ro_inc$sc4_ind_ro_q1
Gesamteffekte_inc_ro$ro_sc4_q1 <- c(sum(results_ro_inc$sc4_dir_ro_q1),sum(results_ro_inc$sc4_ind_ro_q1), sum(results_ro_inc$sc4_tot_ro_q1))
results_ro_inc$sc4_dir_ro_q2 <- numeric(2944)
results_ro_inc$sc4_dir_ro_q2[RO] <- shocks_scenario4[RO] * inc_q2$RO_P3_S14[RO] *100
results_ro_inc$sc4_ind_ro_q2 <- results$scenario4 * inc_q2$RO_P3_S14 *100
results_ro_inc$sc4_tot_ro_q2 <- results_ro_inc$sc4_dir_ro_q2 + results_ro_inc$sc4_ind_ro_q2
Gesamteffekte_inc_ro$ro_sc4_q2 <- c(sum(results_ro_inc$sc4_dir_ro_q2),sum(results_ro_inc$sc4_ind_ro_q2), sum(results_ro_inc$sc4_tot_ro_q2))
results_ro_inc$sc4_dir_ro_q3 <- numeric(2944)
results_ro_inc$sc4_dir_ro_q3[RO] <- shocks_scenario4[RO] * inc_q3$RO_P3_S14[RO] *100
results_ro_inc$sc4_ind_ro_q3 <- results$scenario4 * inc_q3$RO_P3_S14 *100
results_ro_inc$sc4_tot_ro_q3 <- results_ro_inc$sc4_dir_ro_q3 + results_ro_inc$sc4_ind_ro_q3
Gesamteffekte_inc_ro$ro_sc4_q3 <- c(sum(results_ro_inc$sc4_dir_ro_q3),sum(results_ro_inc$sc4_ind_ro_q3), sum(results_ro_inc$sc4_tot_ro_q3))
results_ro_inc$sc4_dir_ro_q4 <- numeric(2944)
results_ro_inc$sc4_dir_ro_q4[RO] <- shocks_scenario4[RO] * inc_q4$RO_P3_S14[RO] *100
results_ro_inc$sc4_ind_ro_q4 <- results$scenario4 * inc_q4$RO_P3_S14 *100
results_ro_inc$sc4_tot_ro_q4 <- results_ro_inc$sc4_dir_ro_q4 + results_ro_inc$sc4_ind_ro_q4
Gesamteffekte_inc_ro$ro_sc4_q4 <- c(sum(results_ro_inc$sc4_dir_ro_q4),sum(results_ro_inc$sc4_ind_ro_q4), sum(results_ro_inc$sc4_tot_ro_q4))
results_ro_inc$sc4_dir_ro_q5 <- numeric(2944)
results_ro_inc$sc4_ind_ro_q5 <- results$scenario4 * inc_q5$RO_P3_S14 *100
results_ro_inc$sc4_dir_ro_q5[RO] <- shocks_scenario4[RO] * inc_q5$RO_P3_S14[RO] *100
results_ro_inc$sc4_tot_ro_q5 <- results_ro_inc$sc4_dir_ro_q5 + results_ro_inc$sc4_ind_ro_q5
Gesamteffekte_inc_ro$ro_sc4_q5 <- c(sum(results_ro_inc$sc4_dir_ro_q5),sum(results_ro_inc$sc4_ind_ro_q5), sum(results_ro_inc$sc4_tot_ro_q5))



#Slowenien
#results df
results_si_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_si <- data.frame(Result = c("direct", "indirect", "total"))
results_si_inc$sc2_dir_si_q1 <- numeric(2944)
results_si_inc$sc2_ind_si_q1 <- results$scenario2 * inc_q1$SI_P3_S14 *100
results_si_inc$sc2_tot_si_q1 <- results_si_inc$sc2_dir_si_q1 + results_si_inc$sc2_ind_si_q1
Gesamteffekte_inc_si$si_sc2_q1 <- c(sum(results_si_inc$sc2_dir_si_q1),sum(results_si_inc$sc2_ind_si_q1), sum(results_si_inc$sc2_tot_si_q1))

results_si_inc$sc2_dir_si_q2 <- numeric(2944) 
results_si_inc$sc2_ind_si_q2 <- results$scenario2 * inc_q2$SI_P3_S14 *100 
results_si_inc$sc2_tot_si_q2 <- results_si_inc$sc2_dir_si_q2 + results_si_inc$sc2_ind_si_q2 
Gesamteffekte_inc_si$si_sc2_q2 <- c(sum(results_si_inc$sc2_dir_si_q2),sum(results_si_inc$sc2_ind_si_q2), sum(results_si_inc$sc2_tot_si_q2))
results_si_inc$sc2_dir_si_q3 <- numeric(2944) 
results_si_inc$sc2_ind_si_q3 <- results$scenario2 * inc_q3$SI_P3_S14 *100 
results_si_inc$sc2_tot_si_q3 <- results_si_inc$sc2_dir_si_q3 + results_si_inc$sc2_ind_si_q3 
Gesamteffekte_inc_si$si_sc2_q3 <- c(sum(results_si_inc$sc2_dir_si_q3),sum(results_si_inc$sc2_ind_si_q3), sum(results_si_inc$sc2_tot_si_q3))
results_si_inc$sc2_dir_si_q4 <- numeric(2944) 
results_si_inc$sc2_ind_si_q4 <- results$scenario2 * inc_q4$SI_P3_S14 *100 
results_si_inc$sc2_tot_si_q4 <- results_si_inc$sc2_dir_si_q4 + results_si_inc$sc2_ind_si_q4 
Gesamteffekte_inc_si$si_sc2_q4 <- c(sum(results_si_inc$sc2_dir_si_q4),sum(results_si_inc$sc2_ind_si_q4), sum(results_si_inc$sc2_tot_si_q4))
results_si_inc$sc2_dir_si_q5 <- numeric(2944) 
results_si_inc$sc2_ind_si_q5 <- results$scenario2 * inc_q5$SI_P3_S14 *100 
results_si_inc$sc2_tot_si_q5 <- results_si_inc$sc2_dir_si_q5 + results_si_inc$sc2_ind_si_q5 
Gesamteffekte_inc_si$si_sc2_q5 <- c(sum(results_si_inc$sc2_dir_si_q5),sum(results_si_inc$sc2_ind_si_q5), sum(results_si_inc$sc2_tot_si_q5))

#Szenario4
results_si_inc$sc4_dir_si_q1 <- numeric(2944)
results_si_inc$sc4_dir_si_q1[SI] <- shocks_scenario4[SI] * inc_q1$SI_P3_S14[SI] *100
results_si_inc$sc4_ind_si_q1 <- results$scenario4 * inc_q1$SI_P3_S14 *100
results_si_inc$sc4_tot_si_q1 <- results_si_inc$sc4_dir_si_q1 + results_si_inc$sc4_ind_si_q1
Gesamteffekte_inc_si$si_sc4_q1 <- c(sum(results_si_inc$sc4_dir_si_q1),sum(results_si_inc$sc4_ind_si_q1), sum(results_si_inc$sc4_tot_si_q1))
results_si_inc$sc4_dir_si_q2 <- numeric(2944)
results_si_inc$sc4_dir_si_q2[SI] <- shocks_scenario4[SI] * inc_q2$SI_P3_S14[SI] *100
results_si_inc$sc4_ind_si_q2 <- results$scenario4 * inc_q2$SI_P3_S14 *100
results_si_inc$sc4_tot_si_q2 <- results_si_inc$sc4_dir_si_q2 + results_si_inc$sc4_ind_si_q2
Gesamteffekte_inc_si$si_sc4_q2 <- c(sum(results_si_inc$sc4_dir_si_q2),sum(results_si_inc$sc4_ind_si_q2), sum(results_si_inc$sc4_tot_si_q2))
results_si_inc$sc4_dir_si_q3 <- numeric(2944)
results_si_inc$sc4_dir_si_q3[SI] <- shocks_scenario4[SI] * inc_q3$SI_P3_S14[SI] *100
results_si_inc$sc4_ind_si_q3 <- results$scenario4 * inc_q3$SI_P3_S14 *100
results_si_inc$sc4_tot_si_q3 <- results_si_inc$sc4_dir_si_q3 + results_si_inc$sc4_ind_si_q3
Gesamteffekte_inc_si$si_sc4_q3 <- c(sum(results_si_inc$sc4_dir_si_q3),sum(results_si_inc$sc4_ind_si_q3), sum(results_si_inc$sc4_tot_si_q3))
results_si_inc$sc4_dir_si_q4 <- numeric(2944)
results_si_inc$sc4_dir_si_q4[SI] <- shocks_scenario4[SI] * inc_q4$SI_P3_S14[SI] *100
results_si_inc$sc4_ind_si_q4 <- results$scenario4 * inc_q4$SI_P3_S14 *100
results_si_inc$sc4_tot_si_q4 <- results_si_inc$sc4_dir_si_q4 + results_si_inc$sc4_ind_si_q4
Gesamteffekte_inc_si$si_sc4_q4 <- c(sum(results_si_inc$sc4_dir_si_q4),sum(results_si_inc$sc4_ind_si_q4), sum(results_si_inc$sc4_tot_si_q4))
results_si_inc$sc4_dir_si_q5 <- numeric(2944)
results_si_inc$sc4_ind_si_q5 <- results$scenario4 * inc_q5$SI_P3_S14 *100
results_si_inc$sc4_dir_si_q5[SI] <- shocks_scenario4[SI] * inc_q5$SI_P3_S14[SI] *100
results_si_inc$sc4_tot_si_q5 <- results_si_inc$sc4_dir_si_q5 + results_si_inc$sc4_ind_si_q5
Gesamteffekte_inc_si$si_sc4_q5 <- c(sum(results_si_inc$sc4_dir_si_q5),sum(results_si_inc$sc4_ind_si_q5), sum(results_si_inc$sc4_tot_si_q5))


#Slowakei
#results df
results_sk_inc <- data.frame(Sector = data_eu$rowLabels[1:2944])
Gesamteffekte_inc_sk <- data.frame(Result = c("direct", "indirect", "total"))
results_sk_inc$sc2_dir_sk_q1 <- numeric(2944)
results_sk_inc$sc2_ind_sk_q1 <- results$scenario2 * inc_q1$SK_P3_S14 *100
results_sk_inc$sc2_tot_sk_q1 <- results_sk_inc$sc2_dir_sk_q1 + results_sk_inc$sc2_ind_sk_q1
Gesamteffekte_inc_sk$sk_sc2_q1 <- c(sum(results_sk_inc$sc2_dir_sk_q1),sum(results_sk_inc$sc2_ind_sk_q1), sum(results_sk_inc$sc2_tot_sk_q1))

results_sk_inc$sc2_dir_sk_q2 <- numeric(2944) 
results_sk_inc$sc2_ind_sk_q2 <- results$scenario2 * inc_q2$SK_P3_S14 *100 
results_sk_inc$sc2_tot_sk_q2 <- results_sk_inc$sc2_dir_sk_q2 + results_sk_inc$sc2_ind_sk_q2 
Gesamteffekte_inc_sk$sk_sc2_q2 <- c(sum(results_sk_inc$sc2_dir_sk_q2),sum(results_sk_inc$sc2_ind_sk_q2), sum(results_sk_inc$sc2_tot_sk_q2))
results_sk_inc$sc2_dir_sk_q3 <- numeric(2944) 
results_sk_inc$sc2_ind_sk_q3 <- results$scenario2 * inc_q3$SK_P3_S14 *100 
results_sk_inc$sc2_tot_sk_q3 <- results_sk_inc$sc2_dir_sk_q3 + results_sk_inc$sc2_ind_sk_q3 
Gesamteffekte_inc_sk$sk_sc2_q3 <- c(sum(results_sk_inc$sc2_dir_sk_q3),sum(results_sk_inc$sc2_ind_sk_q3), sum(results_sk_inc$sc2_tot_sk_q3))
results_sk_inc$sc2_dir_sk_q4 <- numeric(2944) 
results_sk_inc$sc2_ind_sk_q4 <- results$scenario2 * inc_q4$SK_P3_S14 *100 
results_sk_inc$sc2_tot_sk_q4 <- results_sk_inc$sc2_dir_sk_q4 + results_sk_inc$sc2_ind_sk_q4 
Gesamteffekte_inc_sk$sk_sc2_q4 <- c(sum(results_sk_inc$sc2_dir_sk_q4),sum(results_sk_inc$sc2_ind_sk_q4), sum(results_sk_inc$sc2_tot_sk_q4))
results_sk_inc$sc2_dir_sk_q5 <- numeric(2944) 
results_sk_inc$sc2_ind_sk_q5 <- results$scenario2 * inc_q5$SK_P3_S14 *100 
results_sk_inc$sc2_tot_sk_q5 <- results_sk_inc$sc2_dir_sk_q5 + results_sk_inc$sc2_ind_sk_q5 
Gesamteffekte_inc_sk$sk_sc2_q5 <- c(sum(results_sk_inc$sc2_dir_sk_q5),sum(results_sk_inc$sc2_ind_sk_q5), sum(results_sk_inc$sc2_tot_sk_q5))

#Szenario4
results_sk_inc$sc4_dir_sk_q1 <- numeric(2944)
results_sk_inc$sc4_dir_sk_q1[SK] <- shocks_scenario4[SK] * inc_q1$SK_P3_S14[SK] *100
results_sk_inc$sc4_ind_sk_q1 <- results$scenario4 * inc_q1$SK_P3_S14 *100
results_sk_inc$sc4_tot_sk_q1 <- results_sk_inc$sc4_dir_sk_q1 + results_sk_inc$sc4_ind_sk_q1
Gesamteffekte_inc_sk$sk_sc4_q1 <- c(sum(results_sk_inc$sc4_dir_sk_q1),sum(results_sk_inc$sc4_ind_sk_q1), sum(results_sk_inc$sc4_tot_sk_q1))
results_sk_inc$sc4_dir_sk_q2 <- numeric(2944)
results_sk_inc$sc4_dir_sk_q2[SK] <- shocks_scenario4[SK] * inc_q2$SK_P3_S14[SK] *100
results_sk_inc$sc4_ind_sk_q2 <- results$scenario4 * inc_q2$SK_P3_S14 *100
results_sk_inc$sc4_tot_sk_q2 <- results_sk_inc$sc4_dir_sk_q2 + results_sk_inc$sc4_ind_sk_q2
Gesamteffekte_inc_sk$sk_sc4_q2 <- c(sum(results_sk_inc$sc4_dir_sk_q2),sum(results_sk_inc$sc4_ind_sk_q2), sum(results_sk_inc$sc4_tot_sk_q2))
results_sk_inc$sc4_dir_sk_q3 <- numeric(2944)
results_sk_inc$sc4_dir_sk_q3[SK] <- shocks_scenario4[SK] * inc_q3$SK_P3_S14[SK] *100
results_sk_inc$sc4_ind_sk_q3 <- results$scenario4 * inc_q3$SK_P3_S14 *100
results_sk_inc$sc4_tot_sk_q3 <- results_sk_inc$sc4_dir_sk_q3 + results_sk_inc$sc4_ind_sk_q3
Gesamteffekte_inc_sk$sk_sc4_q3 <- c(sum(results_sk_inc$sc4_dir_sk_q3),sum(results_sk_inc$sc4_ind_sk_q3), sum(results_sk_inc$sc4_tot_sk_q3))
results_sk_inc$sc4_dir_sk_q4 <- numeric(2944)
results_sk_inc$sc4_dir_sk_q4[SK] <- shocks_scenario4[SK] * inc_q4$SK_P3_S14[SK] *100
results_sk_inc$sc4_ind_sk_q4 <- results$scenario4 * inc_q4$SK_P3_S14 *100
results_sk_inc$sc4_tot_sk_q4 <- results_sk_inc$sc4_dir_sk_q4 + results_sk_inc$sc4_ind_sk_q4
Gesamteffekte_inc_sk$sk_sc4_q4 <- c(sum(results_sk_inc$sc4_dir_sk_q4),sum(results_sk_inc$sc4_ind_sk_q4), sum(results_sk_inc$sc4_tot_sk_q4))
results_sk_inc$sc4_dir_sk_q5 <- numeric(2944)
results_sk_inc$sc4_ind_sk_q5 <- results$scenario4 * inc_q5$SK_P3_S14 *100
results_sk_inc$sc4_dir_sk_q5[SK] <- shocks_scenario4[SK] * inc_q5$SK_P3_S14[SK] *100
results_sk_inc$sc4_tot_sk_q5 <- results_sk_inc$sc4_dir_sk_q5 + results_sk_inc$sc4_ind_sk_q5
Gesamteffekte_inc_sk$sk_sc4_q5 <- c(sum(results_sk_inc$sc4_dir_sk_q5),sum(results_sk_inc$sc4_ind_sk_q5), sum(results_sk_inc$sc4_tot_sk_q5))


#speichern

write.csv(Gesamteffekte_at, "Gesamteffekte_at.csv", row.names = FALSE)
write.csv(Gesamteffekte_be, "Gesamteffekte_be.csv", row.names = FALSE)
write.csv(Gesamteffekte_bg, "Gesamteffekte_bg.csv", row.names = FALSE)
write.csv(Gesamteffekte_cy, "Gesamteffekte_cy.csv", row.names = FALSE)
write.csv(Gesamteffekte_de, "Gesamteffekte_de.csv", row.names = FALSE)
write.csv(Gesamteffekte_dk, "Gesamteffekte_dk.csv", row.names = FALSE)
write.csv(Gesamteffekte_ee, "Gesamteffekte_ee.csv", row.names = FALSE)
write.csv(Gesamteffekte_es, "Gesamteffekte_es.csv", row.names = FALSE)
write.csv(Gesamteffekte_fr, "Gesamteffekte_fr.csv", row.names = FALSE)
write.csv(Gesamteffekte_gr, "Gesamteffekte_gr.csv", row.names = FALSE)
write.csv(Gesamteffekte_hr, "Gesamteffekte_hr.csv", row.names = FALSE)
write.csv(Gesamteffekte_hu, "Gesamteffekte_hu.csv", row.names = FALSE)
write.csv(Gesamteffekte_lt, "Gesamteffekte_lt.csv", row.names = FALSE)
write.csv(Gesamteffekte_lv, "Gesamteffekte_lv.csv", row.names = FALSE)
write.csv(Gesamteffekte_lu, "Gesamteffekte_lu.csv", row.names = FALSE)
write.csv(Gesamteffekte_mt, "Gesamteffekte_mt.csv", row.names = FALSE)
write.csv(Gesamteffekte_nl, "Gesamteffekte_nl.csv", row.names = FALSE)
write.csv(Gesamteffekte_pl, "Gesamteffekte_pl.csv", row.names = FALSE)
write.csv(Gesamteffekte_ro, "Gesamteffekte_ro.csv", row.names = FALSE)
write.csv(Gesamteffekte_si, "Gesamteffekte_si.csv", row.names = FALSE)
write.csv(Gesamteffekte_sk, "Gesamteffekte_sk.csv", row.names = FALSE)

write.csv(Gesamteffekte_inc_at, "Gesamteffekte_inc_at.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_be, "Gesamteffekte_inc_be.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_bg, "Gesamteffekte_inc_bg.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_cy, "Gesamteffekte_inc_cy.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_de, "Gesamteffekte_inc_de.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_dk, "Gesamteffekte_inc_dk.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_ee, "Gesamteffekte_inc_ee.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_es, "Gesamteffekte_inc_es.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_fr, "Gesamteffekte_inc_fr.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_gr, "Gesamteffekte_inc_gr.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_hr, "Gesamteffekte_inc_hr.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_hu, "Gesamteffekte_inc_hu.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_lt, "Gesamteffekte_inc_lt.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_lv, "Gesamteffekte_inc_lv.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_lu, "Gesamteffekte_inc_lu.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_mt, "Gesamteffekte_inc_mt.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_nl, "Gesamteffekte_inc_nl.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_pl, "Gesamteffekte_inc_pl.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_ro, "Gesamteffekte_inc_ro.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_si, "Gesamteffekte_inc_si.csv", row.names = FALSE)
write.csv(Gesamteffekte_inc_sk, "Gesamteffekte_inc_sk.csv", row.names = FALSE)


#############################################################################################
#Gesamteffekte_inc_länderkürzel enthält Ergebnisse aufgeschlüsselt nach Einkommensgruppen
#je Land für alle 5 Szenarien INCOME weighted

#############################################################################################
#Gesamteffekte_länderkürzel enthält Ergebnisse aufgeschlüsselt nach Einkommensgruppen
#je Land für alle 5 Szenarien EXPENDITURE weighted

#############################################################################################
#Gesamteffekte enthält Ergebnisse NICHT nach Einkommen aufgeschlüsselt (Durchschnittswert) pro Land

#############################################################################################
#Model 1,2 und 3 enthalten die Regressionen von total, direct und indirect Effekt für EXPENDITURE weighting

#############################################################################################
#Model 4,5, und 6 enthalten die Regressionen von total, direct und indirect Effekt für INCOME weighting



