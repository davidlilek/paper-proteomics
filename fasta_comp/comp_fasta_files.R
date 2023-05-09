cm <- read.csv("./fasta_comp/fasta_headers_cm.txt", header = FALSE)
dl <- read.csv("./fasta_comp/fasta_headers_dl.txt", header = FALSE)

fasta <- c()
for (i in 1:length(cm[,1])){
  tmp <- strsplit(cm[i,1],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}

fasta_cm <- fasta
fasta_cm <- fasta_cm[!is.na(fasta_cm)]

fasta <- c()
for (i in 1:length(dl[,1])){
  tmp <- strsplit(dl[i,1],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}

fasta_dl <- fasta
fasta_dl <- fasta_dl[!is.na(fasta_dl)]

library(VennDiagram)
venn.diagram(
  x = list(fasta_cm, fasta_dl),
  category.names = c("cm" , "dl"),
  file = "test.png",
  output = FALSE
)




############## compare fasta cm with fasta dl maxquant evaluation
cm <- readRDS("results_v002/results_run1_mqpar_extracts_2gether_200520_reanalysisCM_secondpeptides_pooled_fastaCM_proteinGroups.txt_Unique.RDS")
cm <- cm[[1]]
fasta <- c()
for (i in 1:length(cm[,1])){
  tmp <- strsplit(cm[i,4],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
cm <- as.data.frame(cbind(cm[,1],fasta))
cm <- cm[!cm[,1]==FALSE,]
dl <- readRDS("results_v002/results_run1_mqpar_extracts_2gether_200520_reanalysisCM_secondpeptides_pooled_proteinGroups.txt_Unique.RDS")
dl <- dl[[1]]
fasta <- c()
for (i in 1:length(dl[,1])){
  tmp <- strsplit(dl[i,4],split='\\|')[[1]][2]
  fasta <- c(fasta,tmp)
}
dl <- as.data.frame(cbind(dl[,1],fasta))
dl <- dl[!dl[,1]==FALSE,]
comp  <- merge(cm, dl,
               all = TRUE,
               by = "fasta")
colnames(comp) <- c("fasta","cm","dl")

add_in_cm <- comp[which(is.na(comp$dl)),1]
add_in_dl <- comp[which(is.na(comp$cm)),1]

sum(add_in_cm %in% fasta_dl)

#A5YVX4
try_cm <- "MTTNIQQGSLLDVLKKKMRQTKEEMERYKDECEEYNKRLHAECMRREEAESEVAALNRRI
QLLEEDLERSEERLATATAKLAEASAAADESERIRKALENRTNMEDDRVGILETQLSQAK
LIAEEADKKYEEIFSKVRALGTVRQRKVLENRSLADEERMDALENQLKEARFLAEEADKK
YDEVARKLVLMEQDLERAEERAEQSESKIVELEEELRVVGNNLKSLEVSEEKANQREEEY
KNQIKNLTTRLKEATLKEEEYSVTLKQVEQRLAEAAVTREHSEDKIRAMSDKLREAEARA
EFAERSVQKLQKEVDRLEDELVAEKERYKEIGDDLDTAFVELIL"

try_dl <- "MTTNIQQGSLLDVLKKKMRQTKEEMERYKDECEEYNKRLHAECMRREEAESEVAALNRRI
QLLEEDLERSEERLATATAKLAEASAAADESERIRKALENRTNMEDDRVGILETQLSQAK
LIAEEADKKYEEIFSKVRALGTVRQRKVLENRSLADEERMDALENQLKEARFLAEEADKK
YDEVARKLVLMEQDLERAEERAEQSESKIVELEEELRVVGNNLKSLEVSEEKANQREEEY
KNQIKNLTTRLKEATLKEEEYSVTLKQVEQRLAEAAVTREHSEDKIRAMSDKLREAEARA
EFAERSVQKLQKEVDRLEDELVAEKERYKEIGDDLDTAFVELIL"

try_cm == try_dl
