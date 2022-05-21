# OG data cleanup

library(tidyverse)

temp = read.csv("./Open Genes plots/1_gene-diseases.csv")
length(unique(temp$HGNC))

diseasecol = ""
for(i in 1:nrow(temp)){
  if(temp[i,2] != " "){
    a = gsub("','", ";", temp[i,2])
    a = sub("^ '", "", a)
    a = sub("'$", "", a)
    if(i==1){
      diseasecol = paste0(diseasecol, a)
    } else {
      diseasecol = paste0(diseasecol, ";", a)
    }
  }
}

summary = as.data.frame(table(strsplit(diseasecol, split = ";")))
colnames(summary) = c("Disease", "Count")
summary = summary %>% arrange(desc(Count))
write.csv(summary, file = "./Open Genes plots/disease_summary.csv")

temp = read.csv("./Open Genes plots/2_gene-aging-mechanisms.csv")

diseasecol = ""
for(i in 1:nrow(temp)){
  if(temp[i,2] != " "){
    a = gsub("','", ";", temp[i,2])
    a = sub("^ '", "", a)
    a = sub("'$", "", a)
    if(i==1){
      diseasecol = paste0(diseasecol, a)
    } else {
      diseasecol = paste0(diseasecol, ";", a)
    }
  }
}

summary = as.data.frame(table(strsplit(diseasecol, split = ";")))
colnames(summary) = c("Aging mechanism", "Count")
summary = summary %>% arrange(desc(Count))
write.csv(summary, file = "./Open Genes plots/aging_mechanism_summary.csv")

temp = read.csv("./Open Genes plots/4_gene-tissue-rpkm.csv")
write.csv(temp, "./Open Genes plots/tissueexp_cleaned.csv")

temp$tissue.or.organ = trimws(temp$tissue.or.organ)
colnames(temp) = c("Gene", "Tissue", "RPKM")
temp$RPKM = log2(temp$RPKM + 1)

pl = ggplot(temp, aes=(x=RPKM)) + geom_density(aes(x=RPKM, group=Tissue, color=Tissue)) + xlab("log(RPKM)") + ggtitle("Expression distribution by tissue") + theme_bw()
pdf("./Open Genes plots/tissue_distributions.pdf")
print(pl)
dev.off()

temp = read.csv("./Open Genes plots/5_gene-summarized-criteria.csv")
unique(temp$criteria)

lookup = c("Effect of gene activity modulation on lifespan in mammalian", "Perturbation effect on lifespan")
lookup = rbind(lookup, c("Age-related changes in gene expression or protein activity in mammalian", "Age-related change"))
lookup = rbind(lookup, c("Effect of gene activity modulation on the age-related process in mammalian", "Age-related process association"))
lookup = rbind(lookup, c("Genomic, transcriptomic, and proteomic associations with lifespan/age-related phenotype in human", "Mutation association with lifespan"))
lookup = rbind(lookup, c("Involvement of a gene product in the regulation of genes associated with aging", "None"))
lookup = rbind(lookup, c("Age-related changes in gene expression or protein activity in human", "Age-related change"))
lookup = rbind(lookup, c("Effect of gene activity modulation on the age-related process in human", "Age-related process association"))
lookup = rbind(lookup, c("Effect of gene activity modulation on lifespan in non-mammalian", "Perturbation effect on lifespan"))
lookup = rbind(lookup, c("Effect of gene activity modulation on the age-related process in cell culture", "Age-related process association"))
lookup = rbind(lookup, c("Age-related changes in gene expression or protein activity in cell culture", "Age-related change"))
lookup = rbind(lookup, c("Effect of gene activity modulation on lifespan in non-mammalian ", "Perturbation effect on lifespan"))
lookup = rbind(lookup, c("Age-related changes in gene expression or protein activity in non-mammalian", "Age-related change"))
lookup = rbind(lookup, c("Effect of gene activity modulation on the age-related process in non-mammalian", "Age-related process association"))
lookup = rbind(lookup, c("Effect of gene activity modulation on the age-related process in non-mammalian ", "Age-related process association"))
lookup = rbind(lookup, c("Age-related changes in gene expression or protein activity in non-mammalian ", "Age-related change"))
lookup = as.data.frame(lookup)
colnames(lookup) = c("criteria", "Criterion")

temp = temp %>% left_join(lookup, by="criteria")
colnames(temp) = c("Gene", "criteria", "research", "Organism",
                   "Sex", "line", "Tissue", "doi",           
                   "pmid", "Criterion")
write.csv(temp, file="./Open Genes plots/criteria.csv")

a = temp %>% group_by(Criterion) %>% summarize(n = n())
temp1 = temp[temp$Criterion != "None",]





