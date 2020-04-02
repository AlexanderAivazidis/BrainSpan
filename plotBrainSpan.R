### Vizualize the brain span data

require(ggplot2)

gene = 'ACE2'

counts = read.csv('data/fetalBrain/BrainSpan/BrainSpan/expression_matrix.csv', header = FALSE)
counts = counts[,2:dim(counts)[2]]
columns_metadata = read.csv('data/fetalBrain/BrainSpan/BrainSpan/columns_metadata.csv')
rows_metadata = read.csv('data/fetalBrain/BrainSpan/BrainSpan/rows_metadata.csv')

table(columns_metadata['structure_name'])
ageOrder = unlist(unique(columns_metadata['age']))

plotData = data.frame(ageIndex = match(unlist(columns_metadata['age']), ageOrder),
                      brainRegion = unlist(columns_metadata['structure_name']),
                      rpkm = unlist(counts[which(rows_metadata['gene_symbol'] == gene),]))

pdf(paste('BrainSpan/BrainSpan_', gene, '.pdf', sep = ''), width = 100, height = 7)
ggplot(plotData, aes(x=ageIndex, y=rpkm)) + 
geom_point() + 
scale_x_continuous(breaks = 1:length(ageOrder), labels = unname(ageOrder)) +
xlab('Age') +
ylab('RPKM') +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
facet_grid( .  ~ brainRegion)
dev.off()
