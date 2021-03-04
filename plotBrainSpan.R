### Vizualize the brain span data

require(ggplot2)

genes = c('ACE2', 'SOX2', 'STMN2', 'SST', 'GRIN2B', 'EGFR')

counts = read.csv('data/fetalBrain/BrainSpan/BrainSpan/expression_matrix.csv', header = FALSE)
counts = counts[,2:dim(counts)[2]]
columns_metadata = read.csv('data/fetalBrain/BrainSpan/BrainSpan/columns_metadata.csv')
rows_metadata = read.csv('data/fetalBrain/BrainSpan/BrainSpan/rows_metadata.csv')

table(columns_metadata['structure_name'])
ageOrder = unlist(unique(columns_metadata['age']))

plotData = data.frame(ageIndex = match(unlist(columns_metadata['age']), ageOrder),
                      brainRegion = unlist(columns_metadata['structure_name']),
                      ACE2 = unlist(counts[which(rows_metadata['gene_symbol'] == genes[1]),]),
                      SOX2 = unlist(counts[which(rows_metadata['gene_symbol'] == genes[2]),]),
                      STMN2 = unlist(counts[which(rows_metadata['gene_symbol'] == genes[3]),]),
                      SST = unlist(counts[which(rows_metadata['gene_symbol'] == genes[4]),]),
                      GRIN2B = unlist(counts[which(rows_metadata['gene_symbol'] == genes[5]),]),
                      EGFR = unlist(counts[which(rows_metadata['gene_symbol'] == genes[6]),]))

pdf(paste('BrainSpan/BrainSpan_', gene, '.pdf', sep = ''), width = 100, height = 7)
ggplot(plotData, aes(ageIndex)) + 
geom_point(aes(y = ACE2, colour = "ACE2")) + 
geom_point(aes(y = SOX2, colour = "SOX2")) +
geom_point(aes(y = STMN2, colour = "STMN2")) +
geom_point(aes(y = SST, colour = "SST")) +
geom_point(aes(y = GRIN2B, colour = "GRIN2B")) +
geom_point(aes(y = EGFR, colour = "EGFR")) +
scale_x_continuous(breaks = 1:length(ageOrder), labels = unname(ageOrder)) +
scale_y_log10() +
xlab('Age') +
ylab('RPKM') +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
facet_grid( .  ~ brainRegion)
dev.off()
