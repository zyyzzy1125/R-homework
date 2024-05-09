library(dplyr)
library(ggplot2)
genes <- read.table("D:/tencent/2023-2024-R/hg19_gene_table.txt", header = T, sep = "\t",stringsAsFactors = F)
ggplot(genes, aes(x = chrom,y = exonCount)) + geom_boxplot() + xlab("Chromosome") + ylab("Number of Exons") + ggtitle("Boxplot of Exon Counts per Chromosome") + theme(axis.text.x =element_text(angle = 45,hjust = 1))
x<-genes %>% group_by(chrom) %>% top_n(1,exonCount) %>% select(chrom,geneName,exonCount)
print(x)
y<-function(starts,ends){
  starts<-as.numeric(unlist(strsplit(starts,",")))
  ends<-as.numeric(unlist(strsplit(ends,",")))
  sum(ends - starts)
}
genes <- genes %>% mutate(exonLengths = mapply(y,exonStarts,exonEnds))
longest<-genes %>% top_n(1,exonLengths) %>% select(geneName,exonLengths)
print(longest)
