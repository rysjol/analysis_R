#pomiaryZapylenia.txt
pomiary <- read.csv('../pomiaryZapylenia.txt', sep = ',', header = F, col.names = c('Miejsce', 'Zapylenie'), colClasses = c('factor', 'character'))

pomiary$Przekroczenie <- grepl('\\*', pomiary$Zapylenie)
pomiary$Przekroczenie <- as.logical(pomiary$Przekroczenie)
pomiary$Zapylenie <- gsub('\\*', '', pomiary$Zapylenie)
pomiary$Zapylenie <- as.numeric(pomiary$Zapylenie)

summary(pomiary)
head(pomiary)
str(pomiary)
