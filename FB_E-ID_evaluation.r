# Dieses Script liest eine CSV-Datendatei in GNU R ein.
# Beim Einlesen werden fÃ¼r alle Variablen Beschriftungen (comment) angelegt.
# Die Beschriftungen fÃ¼r Werte wird ebenfalls als Attribute (attr) abgelegt.
### Arbeitsbereich säubern
rm(list=ls())
################################ Relevante Verzeichnisse anlegen ###############
#dirProject <- "d:\UNI Bremen - Student Files\Informatik B_o_Sc\2. Semester (fort.)\Informatik und Gesellschaft\Referat\FB Auswertung"
#dirProject <- "d:\UNI Bremen - Student Files\Informatik B_o_Sc\2. Semester (fort.)\Informatik und Gesellschaft\Referat\FB Auswertung\output"
#dirData <- paste(dirProject, "data", sep="")
#dirOutput <- paste(dirProject, "output", sep="")
######### Einmaliges Installieren aller benötigten Packete #####################
install.packages("datasets")
install.packages("ggplot2")
install.packages("gplots")
install.packages("Hmisc")
install.packages("plyr")
#install.packages("concord")
install.packages("psych")
install.packages("memisc")
install.packages("GPArotation")
install.packages("gmodels")
install.packages("nortest")
install.packages("plotrix")
######### Einmaliges Laden aller benötigten Packete ############################
require(graphics)
library(GPArotation)
require(datasets)
library(ggplot2)
library(gplots)
library(Hmisc)
library(plyr)
#library(concord)
library(lattice)
library(psych)
library(memisc)
library(car)
require(stats)
library(gmodels)
library(nortest)
library(plotrix)
##### Einmaliges Aufrufen aller benötigten Infos zu den einzelnen Befehlen #####
?plot
?rep
?points
?barplot
?qqplot
?boxplot
?hist
?coplot
?dotchart
?stripchart
?rainbow
?par
### Aufrufen des Arbeitspfades
getwd()
setwd("d:/UNI Bremen - Student Files/Informatik B_o_Sc/2. Semester (fort.)/Informatik und Gesellschaft/Referat/FB Auswertung")
fbeid_file = file.choose();
# setwd("./");
# fbeid_file = "rdata_FB_E-ID_2015-12-14_16-59.csv";

fbeid = read.table(
  file=fbeid_file, encoding="UTF-8",
  header = FALSE, sep = "\t", quote = "\"",
  dec = ".", row.names = "CASE",
  col.names = c(
    "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","S101_01","S102","S201",
    "S202","S203","S204","S205","S302_CN","S302_01","S302_02","S302_03","S302_04",
    "S302_05","S302_05a","S303_CN","S303_01","S303_02","S303_03","S303_04",
    "S303_05","S303_06","S303_06a","S304_CN","S304_01","S304_02","S304_03",
    "S304_04","S304_05","S304_06","S304_07","S304_07a","S305_01","S305_02",
    "S305_03","TIME001","TIME002","TIME003","TIME004","TIME_SUM","MAILSENT",
    "LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING","MISSREL",
    "DEG_MISS","DEG_TIME","DEGRADE"
  ),
  as.is = TRUE,
  colClasses = c(
    "integer","character","character","character","character","POSIXct","integer",
    "factor","factor","factor","factor","factor","factor","integer","logical",
    "logical","logical","logical","logical","character","integer","logical",
    "logical","logical","logical","logical","logical","character","integer",
    "logical","logical","logical","logical","logical","logical","logical",
    "character","integer","integer","integer","integer","integer","integer",
    "integer","integer","POSIXct","POSIXct","logical","logical","integer","integer",
    "integer","integer","integer","integer","integer"
  ),
  skip = 1,
  check.names = TRUE, fill = TRUE,
  strip.white = FALSE, blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
);

rm(fbeid_file);

attr(fbeid, "project") = "FB_E-ID";
attr(fbeid, "description") = "Nutzung und Akzeptanz des neuen Personalausweises";
attr(fbeid, "date") = "2015-12-14 16:59:06";
attr(fbeid, "server") = "https://www.soscisurvey.de";

# Variable und Value Labels
fbeid$S102 = factor(fbeid$S102, levels=c("1","2"), labels=c("maennlich","weiblich"), ordered=FALSE)
fbeid$S201 = factor(fbeid$S201, levels=c("1","2","3"), labels=c("Ja","Nein","Keine Angabe"), ordered=FALSE)
fbeid$S202 = factor(fbeid$S202, levels=c("1","2","3"), labels=c("Ja","Nein","Keine Angabe"), ordered=FALSE)
fbeid$S203 = factor(fbeid$S203, levels=c("1","2","3"), labels=c("Ja","Nein","Keine Angabe"), ordered=FALSE)
fbeid$S204 = factor(fbeid$S204, levels=c("1","2","3"), labels=c("Ja","Nein","Keine Angabe"), ordered=FALSE)
fbeid$S205 = factor(fbeid$S205, levels=c("1","2","3"), labels=c("Ja","Nein","Keine Angabe"), ordered=FALSE)
attr(fbeid$S302_01,"F") = "nicht gewaehlt"
attr(fbeid$S302_01,"T") = "ausgewaehlt"
attr(fbeid$S302_02,"F") = "nicht gewaehlt"
attr(fbeid$S302_02,"T") = "ausgewaehlt"
attr(fbeid$S302_03,"F") = "nicht gewaehlt"
attr(fbeid$S302_03,"T") = "ausgewaehlt"
attr(fbeid$S302_04,"F") = "nicht gewaehlt"
attr(fbeid$S302_04,"T") = "ausgewaehlt"
attr(fbeid$S302_05,"F") = "nicht gewaehlt"
attr(fbeid$S302_05,"T") = "ausgewaehlt"
attr(fbeid$S303_01,"F") = "nicht gewaehlt"
attr(fbeid$S303_01,"T") = "ausgewaehlt"
attr(fbeid$S303_02,"F") = "nicht gewaehlt"
attr(fbeid$S303_02,"T") = "ausgewaehlt"
attr(fbeid$S303_03,"F") = "nicht gewaehlt"
attr(fbeid$S303_03,"T") = "ausgewaehlt"
attr(fbeid$S303_04,"F") = "nicht gewaehlt"
attr(fbeid$S303_04,"T") = "ausgewaehlt"
attr(fbeid$S303_05,"F") = "nicht gewaehlt"
attr(fbeid$S303_05,"T") = "ausgewaehlt"
attr(fbeid$S303_06,"F") = "nicht gewaehlt"
attr(fbeid$S303_06,"T") = "ausgewaehlt"
attr(fbeid$S304_01,"F") = "nicht gewaehlt"
attr(fbeid$S304_01,"T") = "ausgewaehlt"
attr(fbeid$S304_02,"F") = "nicht gewaehlt"
attr(fbeid$S304_02,"T") = "ausgewaehlt"
attr(fbeid$S304_03,"F") = "nicht gewaehlt"
attr(fbeid$S304_03,"T") = "ausgewaehlt"
attr(fbeid$S304_04,"F") = "nicht gewaehlt"
attr(fbeid$S304_04,"T") = "ausgewaehlt"
attr(fbeid$S304_05,"F") = "nicht gewaehlt"
attr(fbeid$S304_05,"T") = "ausgewaehlt"
attr(fbeid$S304_06,"F") = "nicht gewaehlt"
attr(fbeid$S304_06,"T") = "ausgewaehlt"
attr(fbeid$S304_07,"F") = "nicht gewaehlt"
attr(fbeid$S304_07,"T") = "ausgewaehlt"
attr(fbeid$S305_01,"1") = "gar nicht"
attr(fbeid$S305_01,"2") = "wenig"
attr(fbeid$S305_01,"3") = "mittel"
attr(fbeid$S305_01,"4") = "oft"
attr(fbeid$S305_01,"5") = "regelmaessig"
attr(fbeid$S305_02,"1") = "gar nicht"
attr(fbeid$S305_02,"2") = "wenig"
attr(fbeid$S305_02,"3") = "mittel"
attr(fbeid$S305_02,"4") = "oft"
attr(fbeid$S305_02,"5") = "regelmaessig"
attr(fbeid$S305_03,"1") = "gar nicht"
attr(fbeid$S305_03,"2") = "wenig"
attr(fbeid$S305_03,"3") = "mittel"
attr(fbeid$S305_03,"4") = "oft"
attr(fbeid$S305_03,"5") = "regelmaessig"
attr(fbeid$FINISHED,"F") = "abgebrochen"
attr(fbeid$FINISHED,"F") = "ausgefaellt"
attr(fbeid$Q_VIEWER,"F") = "Teilnehmer"
attr(fbeid$Q_VIEWER,"F") = "Durchklicker"
comment(fbeid$SERIAL) = "Seriennummer (sofern verwendet)"
comment(fbeid$REF) = "Referenz (sofern im Link angegeben)"
comment(fbeid$QUESTNNR) = "Fragebogen, der im Interview verwendet wurde"
comment(fbeid$MODE) = "Interview-Modus"
comment(fbeid$STARTED) = "Zeitpunkt zu dem das Interview begonnen hat"
comment(fbeid$S101_01) = "Alter: [01]"
comment(fbeid$S102) = "Geschlecht"
comment(fbeid$S201) = "Besitz"
comment(fbeid$S202) = "Funktionen"
comment(fbeid$S203) = "E-ID-Funktion"
comment(fbeid$S204) = "Finger"
comment(fbeid$S205) = "Kartenleser"
comment(fbeid$S302_CN) = "Pro: Anzahl nicht-exklusiver, ausgewaehlter Optionen"
comment(fbeid$S302_01) = "Pro: Einfache Nutzbarkeit/Verstaendlichkeit"
comment(fbeid$S302_02) = "Pro: Geringer Aufwand (Zeit, Geld, ...)"
comment(fbeid$S302_03) = "Pro: Datenschutz/Datensicherheit"
comment(fbeid$S302_04) = "Pro: Viele Einsatzmoeglichkeiten"
comment(fbeid$S302_05) = "Pro: Sonstiges"
comment(fbeid$S302_05a) = "Pro: Sonstiges (offene Eingabe)"
comment(fbeid$S303_CN) = "Contra: Anzahl nicht-exklusiver, ausgewaehlter Optionen"
comment(fbeid$S303_01) = "Contra: Mangelnde Informationen ueber die Funktionen"
comment(fbeid$S303_02) = "Contra: Komplizierte Nutzung"
comment(fbeid$S303_03) = "Contra: Anschaffungskosten"
comment(fbeid$S303_04) = "Contra: Geringe Anwendungsmoeglichkeiten"
comment(fbeid$S303_05) = "Contra: Bedenken bezueglich Datenschutz/Datensicherheit"
comment(fbeid$S303_06) = "Contra: Sonstiges"
comment(fbeid$S303_06a) = "Contra: Sonstiges (offene Eingabe)"
comment(fbeid$S304_CN) = "Angebote: Anzahl nicht-exklusiver, ausgewaehlter Optionen"
comment(fbeid$S304_01) = "Angebote: Behoerden-/Buergerdienste"
comment(fbeid$S304_02) = "Angebote: Eroeffnung eines Kontos (Giro, Tagesgeld, ...)"
comment(fbeid$S304_03) = "Angebote: Abschluss einer Versicherung"
comment(fbeid$S304_04) = "Angebote: Registrierung/Login bei Kundenprotalen (Banken, Versicherungen, ...)"
comment(fbeid$S304_05) = "Angebote: Altersbestaetigung bei Online-Versandhandeln"
comment(fbeid$S304_06) = "Angebote: KFZ An-/Abmeldung"
comment(fbeid$S304_07) = "Angebote: Sonstiges"
comment(fbeid$S304_07a) = "Angebote: Sonstiges (offene Eingabe)"
comment(fbeid$S305_01) = "Haeufigkeit: Wie haeufig nutzen Sie die E-ID-Funktion?"
comment(fbeid$S305_02) = "Haeufigkeit: Wie haeufig nutzen Sie die Unterschriftsfunktion?"
comment(fbeid$S305_03) = "Haeufigkeit: Wie haeufig wurden Ihre Fingerabdruecke mit denen auf dem Personalausweis verglichen (Bspw. bei einer Personenkontrolle)?"
comment(fbeid$TIME001) = "Verweildauer Seite 1"
comment(fbeid$TIME002) = "Verweildauer Seite 2"
comment(fbeid$TIME003) = "Verweildauer Seite 3"
comment(fbeid$TIME004) = "Verweildauer Seite 4"
comment(fbeid$TIME_SUM) = "Verweildauer gesamt (ohne Ausreisser)"
comment(fbeid$MAILSENT) = "Versandzeitpunkt der Einladungsmail (nur fuer nicht-anonyme Adressaten)"
comment(fbeid$LASTDATA) = "Zeitpunkt als der Datensatz das letzte mal geaendert wurde"
comment(fbeid$FINISHED) = "Wurde die Befragung abgeschlossen (letzte Seite erreicht)?"
comment(fbeid$Q_VIEWER) = "Hat der Teilnehmer den Fragebogen nur angesehen, ohne die Pflichtfragen zu beantworten?"
comment(fbeid$LASTPAGE) = "Seite, die der Teilnehmer zuletzt bearbeitet hat"
comment(fbeid$MAXPAGE) = "Letzte Seite, die im Fragebogen bearbeitet wurde"
comment(fbeid$MISSING) = "Anteil fehlender Antworten in Prozent"
comment(fbeid$MISSREL) = "Anteil fehlender Antworten (gewichtet nach Relevanz)"
comment(fbeid$DEG_MISS) = "Maluspunkte fuer fehlende Antworten"
comment(fbeid$DEG_TIME) = "Maluspunkte fuer schnelles Ausfuellen"
comment(fbeid$DEGRADE) = "Maluspunkte gesamt"


# Assure that the comments are retained in subsets
as.data.frame.avector = as.data.frame.vector
`[.avector` <- function(x,i,...) {
  r <- NextMethod("[")
  mostattributes(r) <- attributes(x)
  r
}
fbeid_tmp = data.frame(
  lapply(fbeid, function(x) {
    structure( x, class = c("avector", class(x) ) )
  } )
);
mostattributes(fbeid_tmp) = attributes(fbeid);
fbeid = fbeid_tmp;
rm(fbeid_tmp);

### Auswertungskram ###
head(fbeid)
### zur Überprüfung der einzelnen Antwortmöglichkeiten pro Item
summary(fbeid)
### Anhängen der Datenmatrix prfd in den Suchpfad
attach(fbeid)
### list rows of data that have missing values
fbeid[!complete.cases(fbeid)]
unfinished <- c(10,12,14,28,50,63,64,74,101)
fbeid <- fbeid[-unfinished]
#head(fbeid_c)
### zur Überprüfung der einzelnen Antwortmöglichkeiten pro Item
#summary(fbeid_c)
### Anhängen der Datenmatrix prfd in den Suchpfad
#attach(fbeid_c)
### löschen des alten mit unvollständigen Angaben befindlichen Objekts "fbeid"
### aus dem Speicher
#rm(fbeid)
### missing values als solche festlegen
#fbeid[grep("False",fbeid$Finished),] <- NA
### missing values indizes anzeigen
#is.na(fbeid)
#fbeid[is.na(fbeid)]
### missing values entfernen
#fbeid_c <- na.omit(fbeid)
### Stichprobenübersicht ###
CrossTable(fbeid$S101_01, fbeid$S102, format="SPSS")
demographic <- table(fbeid$S102, fbeid$S101_01)
#fbeid$S101_01s <- levels(factor(fbeid$S101_01s))
#fbeid$S101_01sr <- recode(fbeid$S101_01s, "1:77='16 bis 30'; 78:84='31 bis 50'; 85:92='51 und älter'")
#fbeid$S101_01s31 <- fbeid$S101_01s[1:77]
geschlecht <- c("männlich","weiblich")
#y_label <- c(0,1,2,3,4,5,6,7,8,9,10)
#axis(2, at=seq(1:11), labels=y_label)
dev.new()
png(filename='Demographic_Chart%03d.png', width=1280, height=1024, pointsize=22, bg='transparent')
#barplot(demographic, col=c("blue","red"), ylim=c(0,10), border=T, main =list("Stichprobenübersicht über Altersstufen", font=2, cex=1.5), xlab=list("Alter", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, legend=rownames(demographic))
barplot(demographic, col=c("blue","red"), yaxt="n", ylim=c(0,24), border=T, main =list("Stichprobenübersicht über Altersstufen", font=2, cex=1.5), xlab=list("Alter", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=60,y=15, inset=.05, title="Legende", geschlecht, fill=c("blue","red"),cex=1.1)
axis(2, at=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13), labels=T, tick=T, las=2, cex=1.5, font=2)
#abline(v=3)
#qplot(fbeid$S101_01, data=fbeid, geom="histogram", xlim=c(16,99), ylab="Anzahl an Probanden in der Stichprobe", xlab="Alter", main = "Stichprobenübersicht über Altersstufen")
dev.off(which=dev.cur())
### nominalskalierte Fragen ###
#names(fbeid)
#describe(fbeid)
#slices <- factor(fbeid$S201)
#lbls <- c("Ja", "Nein", "Keine Angabe")
#pie3D(slices, labels=lbls ,explode=0.1, col=rainbow(length(lbls)), main="Kreisdiagramm: Besitzen Sie den neuen Personalausweis")
#,cex=1.5
Q3 <- table(factor(fbeid$S201, levels=c("Ja","Nein")))
Q3 <- table(fbeid$S201)
pctq3 <- round(Q3/sum(Q3)*100)
lbls3 <- paste(names(Q3), " ", Q3, " ", '(',pctq3,'%)', sep="")
#lbls3 <- names(Q3)
png(filename='Q3_Pie-Chart%03d_nka.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q3, lab="", col=c("blue","red"), main=list("Besitzen Sie den neuen \n Personalausweis?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls3, bg='transparent', fill=c("blue","red"),cex=1.1)
dev.off(which=dev.cur())
Q4 <- table(fbeid$S202)
pctq4 <- round(Q4/sum(Q4)*100, digits=1)
#pctq4 <- signif(Q4/sum(Q4)*100)
#pctq4 <- round_any(Q4/sum(Q4)*100, 1, f = floor)
lbls4 <- paste(names(Q4), " ", Q4, " ", '(',pctq4,'%)', sep="")
png(filename='Q4_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q4, lab="", col=c("blue","red","grey"), main=list("Fühlen Sie sich gut \n über die elektronischen Funktionen \n des Personalausweises informiert?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls4, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q5 <- table(fbeid$S203)
pctq5 <- round(Q5/sum(Q5)*100)
lbls5 <- paste(names(Q5), " ", Q5, " ", '(',pctq5,'%)', sep="")
png(filename='Q5_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q5, lab="", col=c("blue","red","grey"), main=list("Haben Sie die E-ID-Funktion \n ihres Personalausweises aktiviert?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls5, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q5oc <- fbeid$S203[-outcalculated]
Q5oc <- table(Q5oc)
pctq5oc <- round(Q5oc/sum(Q5oc)*100)
lbls5oc <- paste(names(Q5oc), " ", Q5oc, " ", '(',pctq5oc,'%)', sep="")
png(filename='Q5_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q5oc, lab="", col=c("blue","red","grey"), main=list("Haben Sie die E-ID-Funktion \n ihres Personalausweises aktiviert?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls5oc, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q6 <- table(fbeid$S204)
pctq6 <- round(Q6/sum(Q6)*100, digits=1)
lbls6 <- paste(names(Q6), " ", Q6, " ", '(',pctq6,'%)', sep="")
png(filename='Q6_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q6, lab="", col=c("blue","red","grey"), main=list("Haben Sie Fingerabdrücke \n auf dem Personalausweis \n speichern lassen?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls6, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q6oc <- fbeid$S204[-outcalculated]
Q6oc <- table(Q6oc)
pctq6oc <- round(Q6oc/sum(Q6oc)*100)
lbls6oc <- paste(names(Q6oc), " ", Q6oc, " ", '(',pctq6oc,'%)', sep="")
png(filename='Q6_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q6oc, lab="", col=c("blue","red","grey"), main=list("Haben Sie Fingerabdrücke \n auf dem Personalausweis \n speichern lassen?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls6oc, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q7 <- table(fbeid$S205)
pctq7 <- round(Q7/sum(Q7)*100)
lbls7 <- paste(names(Q7), " ", Q7, " ", '(',pctq7,'%)', sep="")
png(filename='Q7_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q7, lab="", col=c("blue","red","grey"), main=list("Besitzen Sie ein Kartenlesegerät \n für den Personalausweis?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls7, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q7oc <- fbeid$S205[-outcalculated]
Q7oc <- table(Q7oc)
pctq7oc <- round(Q7oc/sum(Q7oc)*100)
lbls7oc <- paste(names(Q7oc), " ", Q7oc, " ", '(',pctq7oc,'%)', sep="")
png(filename='Q7_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q7oc, lab="", col=c("blue","red","grey"), main=list("Besitzen Sie ein Kartenlesegerät \n für den Personalausweis?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls7oc, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
yesno <- c("Ja", "Nein")
#lbls <- paste(yesno, "\n", sep="")
#Q8.1 <- factor(fbeid$S302_01, levels=c(T,F), labels=lbls)
Q8.1 <- factor(fbeid$S302_01, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.1 <- table(Q8.1, exclude=c(NA,NaN), deparse.level=1)
Q8.1 <- factor(fbeid$S302_01, levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.1,')', sep=""))
Q8.1 <- table(Q8.1, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q8.1oc <- factor(fbeid$S302_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.1oc <- table(Q8.1oc, exclude=c(NA,NaN), deparse.level=1)
Q8.1oc <- factor(fbeid$S302_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.1oc,')', sep=""))
Q8.1oc <- table(Q8.1oc, exclude=c(NA,NaN), deparse.level=1)
#lbls2 <- paste(c("Ja", "Nein"), "\n", Q8.2, sep="")
#Q8.2 <- factor(fbeid$S302_02, levels=c(T,F), labels=lbls)
Q8.2 <- factor(fbeid$S302_02, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.2 <- table(Q8.2, exclude=c(NA,NaN), deparse.level=1)
Q8.2 <- factor(fbeid$S302_02, levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.2,')', sep=""))
Q8.2 <- table(Q8.2, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q8.2oc <- factor(fbeid$S302_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.2oc <- table(Q8.2oc, exclude=c(NA,NaN), deparse.level=1)
Q8.2oc <- factor(fbeid$S302_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.2oc,')', sep=""))
Q8.2oc <- table(Q8.2oc, exclude=c(NA,NaN), deparse.level=1)
#lbls3 <- paste(c("Ja", "Nein"), "\n", Q8.3, sep="")
#Q8.3 <- factor(fbeid$S302_03, levels=c(T,F), labels=lbls)
Q8.3 <- factor(fbeid$S302_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.3 <- table(Q8.3, exclude=c(NA,NaN), deparse.level=1)
Q8.3 <- factor(fbeid$S302_03, levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.3,')', sep=""))
Q8.3 <- table(Q8.3, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q8.3oc <- factor(fbeid$S302_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.3oc <- table(Q8.3oc, exclude=c(NA,NaN), deparse.level=1)
Q8.3oc <- factor(fbeid$S302_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.3oc,')', sep=""))
Q8.3oc <- table(Q8.3oc, exclude=c(NA,NaN), deparse.level=1)
#lbls4 <- paste(c("Ja", "Nein"), "\n", Q8.4, sep="")
#Q8.4 <- factor(fbeid$S302_04, levels=c(T,F), labels=lbls)
Q8.4 <- factor(fbeid$S302_04, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.4 <- table(Q8.4, exclude=c(NA,NaN), deparse.level=1)
Q8.4 <- factor(fbeid$S302_04, levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.4,')', sep=""))
Q8.4 <- table(Q8.4, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q8.4oc <- factor(fbeid$S302_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q8.4oc <- table(Q8.4oc, exclude=c(NA,NaN), deparse.level=1)
Q8.4oc <- factor(fbeid$S302_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q8.4oc,')', sep=""))
Q8.4oc <- table(Q8.4oc, exclude=c(NA,NaN), deparse.level=1)
Q8 <- c(Q8.1,Q8.2,Q8.3,Q8.4)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
Q8oc <- c(Q8.1oc,Q8.2oc,Q8.3oc,Q8.4oc)
leg1.txt <- c("Nutzbarkeit/Verständlichkeit+","Nutzbarkeit/Verständlichkeit-","Geringer Aufwand+","Geringer Aufwand-","Datenschutz/Datensicherheit+","Datenschutz/Datensicherheit-","Viele Einsatzmöglichkeiten+","Viele Einsatzmöglichkeiten-")
#barplot(Q8, col=rainbow(length(c(Q8.1,Q8.2,Q8.3,Q8.4))), ylim=c(0,65), border=T, main = "Pro Aspekte zur Benutzung der elektronischen Funktionen", xlab=list(Q8.1,Q8.2,Q8.3,Q8.4), ylab="Anzahl der Teilnehmer", beside=T, legend=leg.txt)
#par(xpd=T, mar=par()$mar+c(0,0,0,12))
#ylim=c(0,75),
#, args.legend=list(x=6,y=75)
dev.new()
png(filename='Q8_Bar-Chart%03d.png', width=1280, height=1024, pointsize=20, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
barplot(Q8, col=rainbow(length(c(Q8.1,Q8.2,Q8.3,Q8.4))), yaxt="n", ylim=c(0,110), border=T, main=list("Aspekte \n zur Benutzung \n der elektronischen Funktionen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=9.25,y=123, inset=.05, title="Legende", leg1.txt, bg='transparent', fill=rainbow(length(c(Q8.1,Q8.2,Q8.3,Q8.4))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85), labels=T, tick=T, las=2, cex=1.5, font=2)
#legend=c(9, 60, leg1.txt, cex=0.5)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
dev.new()
png(filename='Q8_Bar-Chart_oc%03d.png', width=1280, height=1024, pointsize=20, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
barplot(Q8oc, col=rainbow(length(c(Q8.1oc,Q8.2oc,Q8.3oc,Q8.4oc))), yaxt="n", ylim=c(0,110), border=T, main=list("Aspekte \n zur Benutzung \n der elektronischen Funktionen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=9.25,y=123, inset=.05, title="Legende", leg1.txt, bg='transparent', fill=rainbow(length(c(Q8.1oc,Q8.2oc,Q8.3oc,Q8.4oc))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85), labels=T, tick=T, las=2, cex=1.5, font=2)
#legend=c(9, 60, leg1.txt, cex=0.5)
dev.off(which=dev.cur())
Q9.1 <- factor(fbeid$S303_01, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.1 <- table(Q9.1, exclude=c(NA,NaN), deparse.level=1)
Q9.1 <- factor(fbeid$S303_01, levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.1,')', sep=""))
Q9.1 <- table(Q9.1, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q9.1oc <- factor(fbeid$S303_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.1oc <- table(Q9.1oc, exclude=c(NA,NaN), deparse.level=1)
Q9.1oc <- factor(fbeid$S303_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.1oc,')', sep=""))
Q9.1oc <- table(Q9.1oc, exclude=c(NA,NaN), deparse.level=1)
Q9.2 <- factor(fbeid$S303_02, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.2 <- table(Q9.2, exclude=c(NA,NaN), deparse.level=1)
Q9.2 <- factor(fbeid$S303_02, levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.2,')', sep=""))
Q9.2 <- table(Q9.2, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q9.2oc <- factor(fbeid$S303_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.2oc <- table(Q9.2oc, exclude=c(NA,NaN), deparse.level=1)
Q9.2oc <- factor(fbeid$S303_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.2oc,')', sep=""))
Q9.2oc <- table(Q9.2oc, exclude=c(NA,NaN), deparse.level=1)
Q9.3 <- factor(fbeid$S303_03, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.3 <- table(Q9.3, exclude=c(NA,NaN), deparse.level=1)
Q9.3 <- factor(fbeid$S303_03, levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.3,')', sep=""))
Q9.3 <- table(Q9.3, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q9.3oc <- factor(fbeid$S303_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.3oc <- table(Q9.3oc, exclude=c(NA,NaN), deparse.level=1)
Q9.3oc <- factor(fbeid$S303_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.3oc,')', sep=""))
Q9.3oc <- table(Q9.3oc, exclude=c(NA,NaN), deparse.level=1)
Q9.4 <- factor(fbeid$S303_04, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.4 <- table(Q9.4, exclude=c(NA,NaN), deparse.level=1)
Q9.4 <- factor(fbeid$S303_04, levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.4,')', sep=""))
Q9.4 <- table(Q9.4, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q9.4oc <- factor(fbeid$S303_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.4oc <- table(Q9.4oc, exclude=c(NA,NaN), deparse.level=1)
Q9.4oc <- factor(fbeid$S303_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.4oc,')', sep=""))
Q9.4oc <- table(Q9.4oc, exclude=c(NA,NaN), deparse.level=1)
Q9.5 <- factor(fbeid$S303_05, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.5 <- table(Q9.5, exclude=c(NA,NaN), deparse.level=1)
Q9.5 <- factor(fbeid$S303_05, levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.5,')', sep=""))
Q9.5 <- table(Q9.5, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q9.5oc <- factor(fbeid$S303_05[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q9.5oc <- table(Q9.5oc, exclude=c(NA,NaN), deparse.level=1)
Q9.5oc <- factor(fbeid$S303_05[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q9.5oc,')', sep=""))
Q9.5oc <- table(Q9.5oc, exclude=c(NA,NaN), deparse.level=1)
#Q9.6 <- factor(fbeid$S303_06, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
#Q9.6 <- table(Q9.6, exclude=c(NA,NaN), deparse.level=1)
#Q9.6 <- factor(fbeid$S303_06, levels=c(T,F), labels=paste(yesno, "\n", Q9.6, sep=""))
#Q9.6 <- table(Q9.6, exclude=c(NA,NaN), deparse.level=1)
Q9 <- c(Q9.1,Q9.2,Q9.3,Q9.4,Q9.5)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
Q9oc <- c(Q9.1oc,Q9.2oc,Q9.3oc,Q9.4oc,Q9.5oc)
leg2.txt <- c("Mangelnde Informationen+","Mangelnde Informationen-","Komplizierte Nutzung+","Komplizierte Nutzung-","Anschaffungskosten+","Anschaffungskosten-","Geringe Anwendungsmöglichkeiten+","Geringe Anwendungsmöglichkeiten-","Datenschutz/Datensicherheit+","Datenschutz/Datensicherheit-")
dev.new()
png(filename='Q9_Bar-Chart%03d.png', width=1280, height=1024, pointsize=20, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
barplot(Q9, col=rainbow(length(c(Q9.1,Q9.2,Q9.3,Q9.4,Q9.5))), yaxt="n", ylim=c(0,100), border=T, main=list("Aspekte \n gegen die Benutzung \n der elektronischen Funktionen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=10.7,y=112, inset=.05, title="Legende", leg2.txt, bg='transparent', fill=rainbow(length(c(Q9.1,Q9.2,Q9.3,Q9.4,Q9.5))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75), labels=T, tick=T, las=2, cex=1.5, font=2)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####    
dev.new()
png(filename='Q9_Bar-Chart_oc%03d.png', width=1280, height=1024, pointsize=20, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
barplot(Q9oc, col=rainbow(length(c(Q9.1oc,Q9.2oc,Q9.3oc,Q9.4oc,Q9.5oc))), yaxt="n", ylim=c(0,100), border=T, main=list("Aspekte \n gegen die Benutzung \n der elektronischen Funktionen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=10.7,y=112, inset=.05, title="Legende", leg2.txt, bg='transparent', fill=rainbow(length(c(Q9.1oc,Q9.2oc,Q9.3oc,Q9.4oc,Q9.5oc))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75), labels=T, tick=T, las=2, cex=1.5, font=2)
dev.off(which=dev.cur())
Q10.1 <- factor(fbeid$S304_01, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.1 <- table(Q10.1, exclude=c(NA,NaN), deparse.level=1)
Q10.1 <- factor(fbeid$S304_01, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.1,')', sep=""))
Q10.1 <- table(Q10.1, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####    
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q10.1oc <- factor(fbeid$S304_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.1oc <- table(Q10.1oc, exclude=c(NA,NaN), deparse.level=1)
Q10.1oc <- factor(fbeid$S304_01[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.1oc,')', sep=""))
Q10.1oc <- table(Q10.1oc, exclude=c(NA,NaN), deparse.level=1)
Q10.2 <- factor(fbeid$S304_02, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.2 <- table(Q10.2, exclude=c(NA,NaN), deparse.level=1)
Q10.2 <- factor(fbeid$S304_02, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.2,')', sep=""))
Q10.2 <- table(Q10.2, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten #### 
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)   
Q10.2oc <- factor(fbeid$S304_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.2oc <- table(Q10.2oc, exclude=c(NA,NaN), deparse.level=1)
Q10.2oc <- factor(fbeid$S304_02[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.2oc,')', sep=""))
Q10.2oc <- table(Q10.2oc, exclude=c(NA,NaN), deparse.level=1)
Q10.3 <- factor(fbeid$S304_03, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.3 <- table(Q10.3, exclude=c(NA,NaN), deparse.level=1)
Q10.3 <- factor(fbeid$S304_03, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.3,')', sep=""))
Q10.3 <- table(Q10.3, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####  
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)  
Q10.3oc <- factor(fbeid$S304_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.3oc <- table(Q10.3oc, exclude=c(NA,NaN), deparse.level=1)
Q10.3oc <- factor(fbeid$S304_03[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.3oc,')', sep=""))
Q10.3oc <- table(Q10.3oc, exclude=c(NA,NaN), deparse.level=1)
Q10.4 <- factor(fbeid$S304_04, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.4 <- table(Q10.4, exclude=c(NA,NaN), deparse.level=1)
Q10.4 <- factor(fbeid$S304_04, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.4,')', sep=""))
Q10.4 <- table(Q10.4, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####    
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q10.4oc <- factor(fbeid$S304_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.4oc <- table(Q10.4oc, exclude=c(NA,NaN), deparse.level=1)
Q10.4oc <- factor(fbeid$S304_04[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.4oc,')', sep=""))
Q10.4oc <- table(Q10.4oc, exclude=c(NA,NaN), deparse.level=1)
Q10.5 <- factor(fbeid$S304_05, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.5 <- table(Q10.5, exclude=c(NA,NaN), deparse.level=1)
Q10.5 <- factor(fbeid$S304_05, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.5,')', sep=""))
Q10.5 <- table(Q10.5, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####    
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q10.5oc <- factor(fbeid$S304_05[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.5oc <- table(Q10.5oc, exclude=c(NA,NaN), deparse.level=1)
Q10.5oc <- factor(fbeid$S304_05[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.5oc,')', sep=""))
Q10.5oc <- table(Q10.5oc, exclude=c(NA,NaN), deparse.level=1)
Q10.6 <- factor(fbeid$S304_06, levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.6 <- table(Q10.6, exclude=c(NA,NaN), deparse.level=1)
Q10.6 <- factor(fbeid$S304_06, levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.6,')', sep=""))
Q10.6 <- table(Q10.6, exclude=c(NA,NaN), deparse.level=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q10.6oc <- factor(fbeid$S304_06[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", sep=""))
Q10.6oc <- table(Q10.6oc, exclude=c(NA,NaN), deparse.level=1)
Q10.6oc <- factor(fbeid$S304_06[-outcalculated], levels=c(T,F), labels=paste(yesno, "\n", '(',Q10.6oc,')', sep=""))
Q10.6oc <- table(Q10.6oc, exclude=c(NA,NaN), deparse.level=1)
Q10 <- c(Q10.1,Q10.2,Q10.3,Q10.4,Q10.5,Q10.6)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
Q10oc <- c(Q10.1oc,Q10.2oc,Q10.3oc,Q10.4oc,Q10.5oc,Q10.6oc)
leg3.txt <- c("Behörden/Bürgerdienste+","Behörden/Bürgerdienste-","Kontoeröffnung+","Kontoeröffnung-","Versicherungsabschluss+","Versicherungsabschluss-","Registrierung/Login bei Kundenportalen+","Registrierung/Login bei Kundenportalen-","Altersbestätigung bei Online-Versandhandel+","Altersbestätigung bei Online-Versandhandel-","KFZ-An/Abmeldung+","KFZ-An/Abmeldung-")
dev.new()
png(filename='Q10_Bar-Chart%03d.png', width=1280, height=1024, pointsize=21, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,11))
barplot(Q10, col=rainbow(length(c(Q10.1,Q10.2,Q10.3,Q10.4,Q10.5,Q10.6))), yaxt="n", ylim=c(0,130), border=T, main =list("Welche der folgenden \n Online-Angebote nutzen Sie \n oder würden Sie nutzen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=11.5,y=147, inset=.05, title="Legende", leg3.txt, bg='transparent', fill=rainbow(length(c(Q10.1,Q10.2,Q10.3,Q10.4,Q10.5,Q10.6))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85), labels=T, tick=T, las=2, cex=1.5, font=2)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
dev.new()
png(filename='Q10_Bar-Chart_oc%03d.png', width=1280, height=1024, pointsize=21, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,11))
barplot(Q10oc, col=rainbow(length(c(Q10.1oc,Q10.2oc,Q10.3oc,Q10.4oc,Q10.5oc,Q10.6oc))), yaxt="n", ylim=c(0,130), border=T, main =list("Welche der folgenden \n Online-Angebote nutzen Sie \n oder würden Sie nutzen?", font=2, cex=1.5), xlab=list("Zustandswert", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=11.5,y=147, inset=.05, title="Legende", leg3.txt, bg='transparent', fill=rainbow(length(c(Q10.1oc,Q10.2oc,Q10.3oc,Q10.4oc,Q10.5oc,Q10.6oc))),cex=1.1)
axis(2, at=c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85), labels=T, tick=T, las=2, cex=1.5, font=2)
dev.off(which=dev.cur())
Q11 <- table(fbeid$S305_01)
gw <- sum(Q11)
pct <- c("100%")
#lbls <- c("Gar nicht", "Wenig", "Mittel", "Oft", "Regelmäßig")
lbls11 <- c("Gar nicht", "Wenig")
pctq11 <- round(Q11/sum(Q11)*100)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
Q11oc <- table(fbeid$S305_01[-outcalculated])
gwoc <- sum(Q11oc)
pctoc <- c("100%")
#lbls <- c("Gar nicht", "Wenig", "Mittel", "Oft", "Regelmäßig")
lbls11oc <- c("Gar nicht", "Wenig")
pctq11oc <- round(Q11oc/sum(Q11oc)*100)
png(filename='Q11_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q11, labels=paste(lbls, pctq11, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q11, lab="", col=rainbow(length(lbls11)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q11, lab="", col=rainbow(length(lbls11)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls11, " ", pctq11,'%', sep=""), bg='transparent', fill=rainbow(length(lbls11)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls11, " ", Q11, " ", '(',pctq11,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls11)),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
png(filename='Q11_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q11, labels=paste(lbls, pctq11, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q11, lab="", col=rainbow(length(lbls11)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q11oc, lab="", col=rainbow(length(lbls11oc)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls11, " ", pctq11,'%', sep=""), bg='transparent', fill=rainbow(length(lbls11)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls11oc, " ", Q11oc, " ", '(',pctq11oc,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls11oc)),cex=1.1)
dev.off(which=dev.cur())                        
#gw12 <- sum(Q12)
Q12 <- table(fbeid$S305_02)
lbls12 <- c("Gar nicht", "Wenig", "Mittel")
pctq12 <- round(Q12/sum(Q12)*100, digits=1)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
#gw12 <- sum(Q12)
Q12oc <- table(fbeid$S305_02[-outcalculated])
lbls12oc <- c("Gar nicht", "Wenig", "Mittel")
pctq12oc <- round(Q12oc/sum(Q12oc)*100, digits=1)
png(filename='Q12_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q12, labels=paste(lbls, pctq12, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q12, lab="", col=rainbow(length(lbls12)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q12, lab="", col=rainbow(length(lbls12)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls12, " ", pctq12,'%', sep=""), bg='transparent', fill=rainbow(length(lbls12)),cex=1.1)
legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls12, " ", Q12, " ", '(',pctq12,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls12)),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
png(filename='Q12_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q12, labels=paste(lbls, pctq12, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q12, lab="", col=rainbow(length(lbls12)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q12oc, lab="", col=rainbow(length(lbls12oc)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls12, " ", pctq12,'%', sep=""), bg='transparent', fill=rainbow(length(lbls12)),cex=1.1)
legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls12oc, " ", Q12oc, " ", '(',pctq12oc,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls12oc)),cex=1.1)
dev.off(which=dev.cur())
#gw13 <- sum(Q13)
Q13 <- table(fbeid$S305_03)
lbls13 <- c("Gar nicht", "Wenig", "Mittel")
pctq13 <- round(Q13/sum(Q13)*100)
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
outcalculated <- c(1,7,11,13,16,17,25,26,27,29,30,31,32,33,45,46,47,61,67,71,81,83)
#gw13 <- sum(Q13)
Q13oc <- table(fbeid$S305_03[-outcalculated])
lbls13oc <- c("Gar nicht", "Wenig", "Mittel")
pctq13oc <- round(Q13oc/sum(Q13oc)*100)
png(filename='Q13_Pie-Chart%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q13, labels=paste(lbls, pctq13, "%"), col=rainbow(length(lbls)), main=list("Wie häufig wurden Ihre Fingerabdrücke \n mit denen auf dem Personalausweis verglichen \n (Bspw. bei einer Personenkontrolle)?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q13, lab="", col=rainbow(length(lbls13)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q13, lab="", col=rainbow(length(lbls13)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls13, " ", pctq13,'%', sep=""), bg='transparent', fill=rainbow(length(lbls13)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls13, " ", Q13, " ", '(',pctq13,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls13)),cex=1.1)
dev.off(which=dev.cur())
#### Neu Berechnung der Frage mit rausrechnen derjenigen die noch den alten Personalausweis hatten ####
png(filename='Q13_Pie-Chart_oc%03d.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q13, labels=paste(lbls, pctq13, "%"), col=rainbow(length(lbls)), main=list("Wie häufig wurden Ihre Fingerabdrücke \n mit denen auf dem Personalausweis verglichen \n (Bspw. bei einer Personenkontrolle)?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q13, lab="", col=rainbow(length(lbls13)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q13oc, lab="", col=rainbow(length(lbls13oc)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls13, " ", pctq13,'%', sep=""), bg='transparent', fill=rainbow(length(lbls13)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls13oc, " ", Q13oc, " ", '(',pctq13oc,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls13oc)),cex=1.1)
dev.off(which=dev.cur())
### Additions Alterstufen vom Dozenten gefordert! ###
fbeid$S101_01s <- fbeid$S101_01
#CrossTable(fbeid$S101_01s, fbeid$S102, format="SPSS")
#demographic2[fbeid$S101_01s < 23 & fbeid$S101_02==c("maennlich","weiblich")] <- "16 bis 22"
fbeid$S101_01s[fbeid$S101_01 < 23] <- "16 bis 22"
#demographic2[fbeid$S101_01s > 22 & fbeid$S101_01s < 26 & fbeid$S101_02==c("maennlich","weiblich")] <- "23 bis 25"
fbeid$S101_01s[fbeid$S101_01 > 22 & fbeid$S101_01s < 26] <- "23 bis 25"
fbeid$S101_01s[fbeid$S101_01 > 22 & fbeid$S101_01s < 30] <- "23 bis 29"
#demographic2[fbeid$S101_01s > 25 & fbeid$S101_01s < 51 & fbeid$S101_02==c("maennlich","weiblich")] <- "26 bis 50"
fbeid$S101_01s[fbeid$S101_01 > 25 & fbeid$S101_01s < 51] <- "30 bis 50"
fbeid$S101_01s[fbeid$S101_01 >= 30] <- "30 und älter"
#demographic2[fbeid$S101_01s > 50 & fbeid$S101_02==c("maennlich","weiblich")] <- "51 und älter"
fbeid$S101_01s[fbeid$S101_01 > 50] <- "51 und älter"
#fbeid$S101_01s <- sort(as.factor(fbeid$S101_01s), decreasing=F, index.return=T)
levels(fbeid$S101_01s) <- c("16 bis 22", "23 bis 29", "30 und älter")
#fbeid$S101_01sr <- recode(fbeid$S101_01s, "1:77='18-19';78:84='20-29';85:92='30-39'")
demographic2 <- table(fbeid$S102, fbeid$S101_01s)
geschlecht <- c("männlich","weiblich")
dev.new()
png(filename='Demographic_Chart%03d_s.png', width=1280, height=1024, pointsize=22, bg='transparent')
barplot(demographic2, col=c("blue","red"), yaxt="n", ylim=c(0,30), border=T, main =list("Stichprobenübersicht über Altersstufen", font=2, cex=1.5), xlab=list("Alter", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=10,y=30, inset=.05, title="Legende", geschlecht, fill=c("blue","red"),cex=1.1)
axis(2, at=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30), labels=T, tick=T, las=2, cex=1.5, font=2)
dev.off(which=dev.cur())
### Additions Bedingung: Besitzen Sie den neuen Personalausweis == "JA" gefordert von Studenten! ###
Q4 <- c(fbeid$S202[2:6],fbeid$S202[8:10],fbeid$S202[12],fbeid$S202[14:15],fbeid$S202[18:24],fbeid$S202[28],fbeid$S202[34:44],fbeid$S202[48:60],fbeid$S202[62:66],fbeid$S202[68:70],fbeid$S202[72:80],fbeid$S202[82],fbeid$S202[84:92])
#Q4[Q4==1] <- "Ja"
#Q4[Q4==2] <- "Nein"
#Q4[Q4==3] <- "Keine Angabe"
Q4 <- factor(Q4)
levels(Q4) <- c("Ja", "Nein", "Keine Angabe")
Q4t <- table(Q4)
#Q4 <- table(fbeid$S202)
pctq4t <- round(Q4t/sum(Q4t)*100, digits=1)
#pctq4 <- signif(Q4/sum(Q4)*100)
#pctq4 <- round_any(Q4/sum(Q4)*100, 1, f = floor)
lbls4t <- paste(names(Q4t), " ", Q4t, " ", '(',pctq4t,'%)', sep="")
png(filename='Q4_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q4t, lab="", col=c("blue","red","grey"), main=list("Fühlen Sie sich gut \n über die elektronischen Funktionen \n des Personalausweises informiert?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls4t, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q5 <- c(fbeid$S203[2:6],fbeid$S203[8:10],fbeid$S203[12],fbeid$S203[14:15],fbeid$S203[18:24],fbeid$S203[28],fbeid$S203[34:44],fbeid$S203[48:60],fbeid$S203[62:66],fbeid$S203[68:70],fbeid$S203[72:80],fbeid$S203[82],fbeid$S203[84:92])
Q5 <- factor(Q5)
levels(Q5) <- c("Ja", "Nein", "Keine Angabe")
Q5t <- table(Q5)
pctq5t <- round(Q5t/sum(Q5t)*100, digits=1)
lbls5t <- paste(names(Q5t), " ", Q5t, " ", '(',pctq5t,'%)', sep="")
png(filename='Q5_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q5t, lab="", col=c("blue","red","grey"), main=list("Haben Sie die E-ID-Funktion \n ihres Personalausweises aktiviert?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls5t, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q6 <- c(fbeid$S204[2:6],fbeid$S204[8:10],fbeid$S204[12],fbeid$S204[14:15],fbeid$S204[18:24],fbeid$S204[28],fbeid$S204[34:44],fbeid$S204[48:60],fbeid$S204[62:66],fbeid$S204[68:70],fbeid$S204[72:80],fbeid$S204[82],fbeid$S204[84:92])
Q6 <- factor(Q6)
levels(Q6) <- c("Ja", "Nein", "Keine Angabe")
Q6t <- table(Q6)
pctq6t <- round(Q6t/sum(Q6t)*100, digits=1)
lbls6t <- paste(names(Q6t), " ", Q6t, " ", '(',pctq6t,'%)', sep="")
png(filename='Q6_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q6t, lab="", col=c("blue","red","grey"), main=list("Haben Sie Fingerabdrücke \n auf dem Personalausweis \n speichern lassen?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls6t, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
Q7 <- c(fbeid$S205[2:6],fbeid$S205[8:10],fbeid$S205[12],fbeid$S205[14:15],fbeid$S205[18:24],fbeid$S205[28],fbeid$S205[34:44],fbeid$S205[48:60],fbeid$S205[62:66],fbeid$S205[68:70],fbeid$S205[72:80],fbeid$S205[82],fbeid$S205[84:92])
Q7 <- factor(Q7)
levels(Q7) <- c("Ja", "Nein", "Keine Angabe")
Q7t <- table(Q7)
pctq7t <- round(Q7t/sum(Q7t)*100, digits=1)
lbls7t <- paste(names(Q7t), " ", Q7t, " ", '(',pctq7t,'%)', sep="")
png(filename='Q7_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,10))
pie(Q7t, lab="", col=c("blue","red","grey"), main=list("Besitzen Sie ein Kartenlesegerät \n für den Personalausweis?", font=2), radius=0.8)
legend(x=1,y=0.5, inset=.05, title="Legende", lbls7t, bg='transparent', fill=c("blue","red","grey"),cex=1.1)
dev.off(which=dev.cur())
demographic3 <- table(fbeid$S102, fbeid$S101_01s)
geschlecht <- c("männlich (60)","weiblich (32)")
dev.new()
png(filename='Demographic_Chart%03d_sn.png', width=1280, height=1024, pointsize=22, bg='transparent')
barplot(demographic3, col=c("blue","red"), yaxt="n", ylim=c(0,36), border=T, xlab=list("Alter", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=7,y=36, inset=.05, title="Legende", geschlecht, fill=c("blue","red"),cex=1.1)
axis(2, at=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36), labels=T, tick=T, las=2, cex=1.5, font=2)
#table (fbeid abline(h = 9, v = 9, col = "red", bg='transparent')
#lines(dmg3, type = "h", col = "red")
segments(0, 20, 2, 20, col = "blue", bg='transparent', lwd = 2, lty=1)
text(x=1.5, y=21, "20", font=2, cex=1.5)
segments(0, 9, 3, 9, col = "red", bg='transparent', lwd = 2, lty=1)
text(x=2.5, y=10, "9", font=2, cex=1.5)
segments(0, 31, 5, 31, col = "blue", bg='transparent', lwd = 2, lty=2)
text(x=4.5, y=32, "31", font=2, cex=1.5)
segments(0, 15, 6, 15, col = "red", bg='transparent', lwd = 2, lty=2)
text(x=5.5, y=16, "15", font=2, cex=1.5)
segments(3, 9, 8, 9, col = "blue", bg='transparent', lwd = 2, lty=3)
text(x=7.5, y=10, "9", font=2, cex=1.5)
segments(0, 8, 9, 8, col = "red", bg='transparent', lwd = 2, lty=3)
text(x=8.5, y=9, "8", font=2, cex=1.5)
dev.off(which=dev.cur())
### Additions Bedingung: Besitzen Sie den neuen Personalausweis == "JA" gefordert von Studenten! ###
Q11y <- c(fbeid$S305_01[2:6],fbeid$S305_01[8:10],fbeid$S305_01[12],fbeid$S305_01[14:15],fbeid$S305_01[18:24],fbeid$S305_01[28],fbeid$S305_01[34:44],fbeid$S305_01[48:60],fbeid$S305_01[62:66],fbeid$S305_01[68:70],fbeid$S305_01[72:80],fbeid$S305_01[82],fbeid$S305_01[84:92])
Q11 <- table(Q11y)
gw <- sum(Q11)
pct <- c("100%")
#lbls <- c("Gar nicht", "Wenig", "Mittel", "Oft", "Regelmäßig")
lbls11 <- c("Gar nicht", "Wenig")
pctq11 <- round(Q11/sum(Q11)*100)
png(filename='Q11_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q11, labels=paste(lbls, pctq11, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q11, lab="", col=rainbow(length(lbls11)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q11, lab="", col=rainbow(length(lbls11)), main=list("Wie häufig nutzen Sie \n die E-ID-Funktion?", font=2, cex=1.5), font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls11, " ", pctq11,'%', sep=""), bg='transparent', fill=rainbow(length(lbls11)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls11, " ", Q11, " ", '(',pctq11,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls11)),cex=1.1)
dev.off(which=dev.cur())
#gw12 <- sum(Q12)
### Additions Bedingung: Besitzen Sie den neuen Personalausweis == "JA" gefordert von Studenten! ###
Q12y <- c(fbeid$S305_02[2:6],fbeid$S305_02[8:10],fbeid$S305_02[12],fbeid$S305_02[14:15],fbeid$S305_02[18:24],fbeid$S305_02[28],fbeid$S305_02[34:44],fbeid$S305_02[48:60],fbeid$S305_02[62:66],fbeid$S305_02[68:70],fbeid$S305_02[72:80],fbeid$S305_02[82],fbeid$S305_02[84:92])
Q12 <- table(Q12y)
lbls12 <- c("Gar nicht", "Wenig", "Mittel")
pctq12 <- round(Q12/sum(Q12)*100, digits=1)
png(filename='Q12_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q12, labels=paste(lbls, pctq12, "%"), col=rainbow(length(lbls)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q12, lab="", col=rainbow(length(lbls12)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q12, lab="", col=rainbow(length(lbls12)), main=list("Wie häufig nutzen Sie \n die Unterschriftsfunktion?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=1,y=0.5, inset=.05, title="Legende", paste(lbls12, " ", pctq12,'%', sep=""), bg='transparent', fill=rainbow(length(lbls12)),cex=1.1)
legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls12, " ", Q12, " ", '(',pctq12,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls12)),cex=1.1)
dev.off(which=dev.cur())
#gw13 <- sum(Q13)
### Additions Bedingung: Besitzen Sie den neuen Personalausweis == "JA" gefordert von Studenten! ###
Q13y <- c(fbeid$S305_03[2:6],fbeid$S305_03[8:10],fbeid$S305_03[12],fbeid$S305_03[14:15],fbeid$S305_03[18:24],fbeid$S305_03[28],fbeid$S305_03[34:44],fbeid$S305_03[48:60],fbeid$S305_03[62:66],fbeid$S305_03[68:70],fbeid$S305_03[72:80],fbeid$S305_03[82],fbeid$S305_03[84:92])
Q13 <- table(Q13y)
lbls13 <- c("Gar nicht", "Wenig", "Mittel")
pctq13 <- round(Q13/sum(Q13)*100)
png(filename='Q13_Pie-Chart%03d_r.png', width=1280, height=1024, pointsize=38, bg='transparent')
par(xpd=T, mar=par()$mar+c(0,0,0,6))
#pie(Q13, labels=paste(lbls, pctq13, "%"), col=rainbow(length(lbls)), main=list("Wie häufig wurden Ihre Fingerabdrücke \n mit denen auf dem Personalausweis verglichen \n (Bspw. bei einer Personenkontrolle)?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.8)
#pie(Q13, lab="", col=rainbow(length(lbls13)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), sub=paste('Von insgesamt', gw,'befragten Personen \n (entspricht:100%)'), cex.sub=1.5, font.sub=2, radius=0.72)
pie(Q13, lab="", col=rainbow(length(lbls13)), main=list("Wie häufig wurden Ihre Fingerabdrücke\nmit denen auf dem Personalausweis verglichen\n Beispielsw. in einer Personenkontrolle?", font=2, cex=1.5), cex.sub=1.5, font.sub=2, radius=0.72)
#legend(x=0.75,y=0.5, inset=.05, title="Legende", paste(lbls13, " ", pctq13,'%', sep=""), bg='transparent', fill=rainbow(length(lbls13)),cex=1.1)
legend(x=0.9,y=0.5, inset=.05, title="Legende", paste(lbls13, " ", Q13, " ", '(',pctq13,'%',')', sep=""), bg='transparent', fill=rainbow(length(lbls13)),cex=1.1)
dev.off(which=dev.cur())
### Zusatz von Eike
demographicNo <- table(fbeid$S201, fbeid$S101_01)
#fbeid$S101_01s <- levels(factor(fbeid$S101_01s))
#fbeid$S101_01sr <- recode(fbeid$S101_01s, "1:77='16 bis 30'; 78:84='31 bis 50'; 85:92='51 und älter'")
#fbeid$S101_01s31 <- fbeid$S101_01s[1:77]
Jain <- c("Ja","Nein")
#y_label <- c(0,1,2,3,4,5,6,7,8,9,10)
#axis(2, at=seq(1:11), labels=y_label)
dev.new()
png(filename='Demographic_Chart%03dno.png', width=1280, height=1024, pointsize=22, bg='transparent')
#barplot(demographic, col=c("blue","red"), ylim=c(0,10), border=T, main =list("Stichprobenübersicht über Altersstufen", font=2, cex=1.5), xlab=list("Alter", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, legend=rownames(demographic))
barplot(demographicNo, yaxt="n", ylim=c(0,24), border=T, main =list("Stichprobenübersicht über Personbesitzer", font=2, cex=1.5), xlab=list("Personbesitzer", font=2, cex=1.5), ylab=list("Anzahl der Teilnehmer", font=2, cex=1.5), beside=T, cex.lab=1.5, cex.axis=1.5, font=2)
legend(x=60,y=15, inset=.05, title="Legende", Jain, fill=c("black","grey"), cex=1.1)
axis(2, at=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20), labels=T, tick=T, las=2, cex=1.5, font=2)
#abline(v=3)
#qplot(fbeid$S101_01, data=fbeid, geom="histogram", xlim=c(16,99), ylab="Anzahl an Probanden in der Stichprobe", xlab="Alter", main = "Stichprobenübersicht über Altersstufen")
dev.off(which=dev.cur())