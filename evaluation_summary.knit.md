---
output:
  html_document:
    df_print: paged
    fig.align: center
    self_contained: yes
    fig.height: 4
    fig.width: 8
    theme: united
    toc: yes
    toc_depth: 4
    toc_float: yes
    number_sections: yes
    code_folding: hide
title: "Auswertung TC Daten"
author: "David Lilek"
date: "04 November 2022, 09:45:27"
---

# Vergleich Unique-Razor | noMBR-MBR | Razor-LFQ

## Unique-Razor

* Boxplot
  + generell werden mit Razor etwas mehr Proteine gefunden egal ob MBR oder noMBR verwendet wird - das ist auch statistisch signifikant (paired t-Test)
  + bei MBR werden ca. 10 Proteine mehr gefunden wenn Razor verwendet wird
  + bei noMBR ca. 7.41
  + bei noMBR gibt es manche Proben die etwas mehr Proteine mit Razor zeigen als Unique
* es zeigt sich, dass der Unterschied Razor/Unique von der Anzahl der gefundenden Proteine anbhängt



```r
data <- read.csv("Extraktionen_gesamt_V2_DL.csv", sep=";")
# unique vs razor
MBR <- (data$Razor.MBR.2.peptides/data$Unique.MBR.2.peptides)*100
noMBR <- (data$Razor.noMBR.2.peptides/data$Unique.no.MBR.2.peptides)*100

boxplot(MBR, noMBR, main = "Abweichung Razor vs Unique [%]", names = c("MBR", "noMBR"))
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-1-1.png" width="672" />

```r
mean_MBR <- (data$Razor.MBR.2.peptides+data$Unique.MBR.2.peptide)/2
mean_noMBR <- (data$Razor.noMBR.2.peptides+data$Unique.no.MBR.2.peptides)/2

plot(mean_MBR,MBR, xlab= "Number proteins", ylab="Abweichung Razor vs. Unique", main = "MBR")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-1-2.png" width="672" />

```r
plot(mean_noMBR,noMBR, xlab= "Number proteins", ylab="Abweichung Razor vs. Unique", main = "noMBR")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-1-3.png" width="672" />

```r
#t.test(data$Razor.noMBR.2.peptides,data$Unique.no.MBR.2.peptides,paired = TRUE)
#t.test(data$Razor.MBR.2.peptides,data$Unique.MBR.2.peptides,paired = TRUE)
```

## MBR vs. noMBR

* Boxplot: Mit MBR können teilweise bis zu 4x mehr Proteine gefunden werden als bei Nichtverwendung von MBR. Die Unterschiede zwischen Razor bzw. Unique sind gering.
* klar ist eine Abhänigkeit von der Gesamtanzahl an gefundenen Proteinen ersichtlich
* ab ca. 350 gefundenen Proteinen bleiben die mehr gefundenen Proteinen auf einem ähnlichen Level
* darunter steigt die Anzahl der gefundenen Proteine mit MBR dramatatisch an wenn sich die Gesamtanzahl an Proteinen reduziert
* dies ist unabhängig ob Razor oder Unique verwendet wird



```r
#MBR evaluation
unique <- (data$Unique.MBR.2.peptides/data$Unique.no.MBR.2.peptides)*100
razor <- (data$Razor.MBR.2.peptides/data$Razor.noMBR.2.peptides)*100

boxplot(unique,razor, main = "Abweichung MBR", names = c("Unique", "Razor"))
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-2-1.png" width="672" />

```r
mean_unique <- (data$Unique.MBR.2.peptides+data$Unique.no.MBR.2.peptides)/2
mean_razor <- (data$Razor.noMBR.2.peptides+data$Razor.MBR.2.peptides)/2

plot(mean_razor,razor, xlab= "Number proteins", ylab="Abweichung MBR", main = "Razor")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-2-2.png" width="672" />

```r
plot(mean_unique,unique, xlab= "Number proteins", ylab="Abweichung MBR", main="Unique")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-2-3.png" width="672" />

## Razor-LFQ

* mit Razor werden im Median 6% mehr Proteine gefunden wenn MBR verwendet wird
* bei noMBR sind es 11%
* es zeigt sich, dass der Unterschied Razor/LFQ nicht von der Anzahl der gefundenen Proteine abhängt



```r
data <- read.csv("Extraktionen_gesamt_V2_DL.csv", sep=";")
# unique vs razor
MBR <- (data$Razor.MBR.2.peptides/data$LFQ.MBR)*100
noMBR <- (data$Razor.noMBR.2.peptides/data$LFQ.noMBR)*100

boxplot(MBR, noMBR, main = "Abweichung Razor vs LFQ [%]", names = c("MBR", "noMBR"))
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-3-1.png" width="672" />

```r
mean_MBR <- (data$Razor.MBR.2.peptides+data$LFQ.MBR)/2
mean_noMBR <- (data$Razor.noMBR.2.peptides+data$LFQ.noMBR)/2

plot(mean_MBR,MBR, xlab= "Number proteins", ylab="Abweichung Razor vs. LFQ", main = "MBR")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-3-2.png" width="672" />

```r
plot(mean_noMBR,noMBR, xlab= "Number proteins", ylab="Abweichung Razor vs. LFQ", main = "noMBR")
```

<img src="evaluation_summary_files/figure-html/unnamed-chunk-3-3.png" width="672" />

```r
#t.test(data$Razor.noMBR.2.peptides,data$Unique.no.MBR.2.peptides,paired = TRUE)
#t.test(data$Razor.MBR.2.peptides,data$Unique.MBR.2.peptides,paired = TRUE)
```

## Summary data analysis

* die unten folgenden Auswertungen wurden mit MBR und noMBR bzw Razor, Unique und LFQ durchgeführt
* es zeigt sich, dass unabhängig von MBR oder noMBR nur geringe Unterschiede zwischen Razor und Unique zu finden sind; die Aussage bleibt diesselbe
* vergleicht man MBR und noMBR so weißt MBR etwas höhere Identifikationsraten auf wie schon oben gezeigt
* in einem ersten Vergleich scheint es, dass es mehr gesharte Proteine zwischen den einzelnen Treatments gibt
  + zB B-AF-TF noMBR 30% gemeinsam; MBR 35% gemeinsam
  + zB B-TF noMBR 4%; MBR 7%
  + gerade bei gemeinsam prozessierten Extrakten steigert es sich noch etwas extremer
  + D2_pool mit D2_pool_fractions: noMBR 53% gemeinsam; MBR 69% gemeinsam
* LFQ
  + mit MBR mehr gemeinsam gerade wenn gemeinsam ausgewertet - siehe unten
  + sonst halt mit LFQ tendentiell weniger aber trotzdem ähnliche Verhältnisse wie bei Razor bzw Unique


MBR vergleicht ja die Läufe untereinander. Dh theoretisch sollten sich mehr gesharte Proteine zeigen wenn MBR verwendet wird UND die Samples 
miteinander ausgewertet werden. Gemeinsam ausgewertet wurden:

* AF und TF
* D2

Aus den Venn Diagrammen wurden jeweils die Anzahl der gesharten Proteine mit und ohne MBR notiert. Es zeigt sich, dass bei D2 eben weil alles gemeinsam ausgewertet wurde signifikant höhere Steigerungen mit MBR in den gesharten Proteinen im Vergleich zum Rest auftreten. Bei AF-TF ist das auch der Fall aber nicht so stark wie bei D2.

* AF_TF_B
  + LFQ 30noMBR 35MBR
  + die Proteine die nur in AF, TF oder B vorkommen sinken leicht
  + Razor 30noMBR 34MBR
  + unique 30noMBR 34MBR


* B_D1pool
  + LFQ 39noMBR 42MBR
  + Razor 38noMBR 41MBR
  + unique 38noMBR 41MBR

* D2
  + LFQ 58noMBR 73MBR
  + Razor 53noMBR 69MBR
  + unique 53noMBR 68MBR

* D1_D2
  + LFQ 43noMBR 46MBR
  + Razor 46noMBR 46MBR
  + unique 46noMBR 47MBR

Empfehlung:

* immer nur möglichst ähnliche Proben miteinander auswerten


# Venn diagrams


```r
pth <- "X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/"
filelist = list.files(path = pth, pattern = ".*.png")
filelist <-   c(filelist[grepl("AF_TF_B", filelist)],
    filelist[grepl("B_D1pool", filelist)],
    filelist[grepl("D1_D2.png", filelist)],
    filelist[grepl("_D2_.png", filelist)])
cat("\n")
```

```r
cat("##","AF_TF_B","\n")
```

## AF_TF_B 

```r
cat("\n")
```

```r
for (file in 1:6){
  filename <- paste(pth,filelist[file],sep="")
  name <- paste(filelist[file])
  #print(filename)
  cat("\n")
  cat("###",name,"\n")
  cat("\n")
  cat("![](",filename,")")
  cat("\n")
}
```


### LFQ_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_AF_TF_B.png )

### LFQ_MBR_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_MBR_AF_TF_B.png )

### razor_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_AF_TF_B.png )

### razor_MBR_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_MBR_AF_TF_B.png )

### unique_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_AF_TF_B.png )

### unique_MBR_AF_TF_B.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_MBR_AF_TF_B.png )

```r
cat("\n")
```

```r
cat("##","B_D1pool","\n")
```

## B_D1pool 

```r
cat("\n")
```

```r
for (file in 7:12){
  filename <- paste(pth,filelist[file],sep="")
  name <- paste(filelist[file])
  #print(filename)
  cat("\n")
  cat("###",name,"\n")
  cat("\n")
  cat("![](",filename,")")
  cat("\n")
}
```


### LFQ_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_B_D1pool.png )

### LFQ_MBR_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_MBR_B_D1pool.png )

### razor_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_B_D1pool.png )

### razor_MBR_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_MBR_B_D1pool.png )

### unique_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_B_D1pool.png )

### unique_MBR_B_D1pool.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_MBR_B_D1pool.png )

```r
cat("\n")
```

```r
cat("##","D1_D2","\n")
```

## D1_D2 

```r
cat("\n")
```

```r
for (file in 13:18){
  filename <- paste(pth,filelist[file],sep="")
  name <- paste(filelist[file])
  #print(filename)
  cat("\n")
  cat("###",name,"\n")
  cat("\n")
  cat("![](",filename,")")
  cat("\n")
}
```


### LFQ_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_D1_D2.png )

### LFQ_MBR_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_MBR_D1_D2.png )

### razor_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_D1_D2.png )

### razor_MBR_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_MBR_D1_D2.png )

### unique_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_D1_D2.png )

### unique_MBR_D1_D2.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_MBR_D1_D2.png )

```r
cat("\n")
```

```r
cat("##","D2","\n")
```

## D2 

```r
cat("\n")
```

```r
for (file in 19:24){
  filename <- paste(pth,filelist[file],sep="")
  name <- paste(filelist[file])
  #print(filename)
  cat("\n")
  cat("###",name,"\n")
  cat("\n")
  cat("![](",filename,")")
  cat("\n")
}
```


### LFQ_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_D2_.png )

### LFQ_MBR_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/LFQ_MBR_D2_.png )

### razor_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_D2_.png )

### razor_MBR_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/razor_MBR_D2_.png )

### unique_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_D2_.png )

### unique_MBR_D2_.png 

![]( X:/wissenschaftliche Veröffentlichungen/Publikationen/2022/Proteomics_JRS_DL/data/pics/unique_MBR_D2_.png )



# Besprechen/todo

* nehmen wir LFQ oder die identifizierten Proteine?
* nehmen wir razor oder unique? ich würde fast razor nehmen, weil ja ohnehin zu der proteingruppe zugeordnet wird die am wahrscheinlichsten ist
* wie poolen wir? zB B1_1, B1_2, B2_1, B2_2...
  + wenn ich dort alle Proteine zähle führt es ev. zu einem Überbefund?
  + wäre es nicht besser B1_1 und B1_2 -> nur wenn Protein in beiden vorkommt ist es auch da? weil ja technische replikate also 2x gemessen oder?
  + wenn 2 biologische replikate; also 2x extrahiert würde ich es so machen, dass ich auch sagen es ist da wenn es nur in einem Extrakt vorkommt | also zB B1_1 TRUE - B1_2 FALSE
* was gehört überhaupt gepooled und verglichen? können wir uns das bitte noch anschauen?
* was gehört bei D2 gepooled und verglichen?


* man könnte ja Razor nehmen und im paper bezug nehmen, dass ergebnisse ähnlich wie unique
* zusätzlich noch anschauen mit LFQ? wieviele proteine von den identifzierten sind überhaupt quantifizierbar? das könnte man auch ins paper geben, dass wenn man lfq nimmt man mit ca. 20-25%(also je nachdem was dann rauskommt) rechnen muss? weil ja später dann bei differential expression analysis ja auch quantifizierte proteine verwendet werden; also meistens halt

