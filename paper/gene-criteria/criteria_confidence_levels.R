install.packages('tidyverse')
install.packages("devtools")
install.packages("readxl")
library(tidyverse)
library(ggplot2)
library(readxl)

temp = read_excel("gene_criteria.xlsx")

unique(temp$letter)
length(unique(temp$Gene))

intersecting groups:

(h1) highest = A + E

(h2) high = A - E

(m) moderate = B - A

(l)  low = min2(C,D,G) - A - B

(l2) lowest = max1(C,D,E), F, G, H, I, J, - A, - B (это все остальные чуваки)

```{r}
evidence_groups = list()
evidence_groups[["h1"]] = intersect(temp$Gene[temp$letter=="A"], temp$Gene[temp$letter=="E"])
evidence_groups[["h2"]] = setdiff(temp$Gene[temp$letter=="A"], temp$Gene[temp$letter=="E"])
evidence_groups[["m"]] = setdiff(temp$Gene[temp$letter=="B"], temp$Gene[temp$letter=="A"])

#number of genes per criterion without As and Es

let_gene = table(unique(temp[,c("Gene","letter")]))
let_gene = let_gene[let_gene[,"A"]==0,]
let_gene = let_gene[let_gene[,"B"]==0,]



evidence_groups[["l1"]] = rownames(let_gene)[rowSums(let_gene[,c("C", "D", "E", "F", "H")])>1]

evidence_groups[["l2"]] = rownames(let_gene)[rowSums(let_gene[,c("C", "D", "E", "F","H")])<=1]


evidence_group_numbers = sapply(evidence_groups, function(x)length(unique(x)))
```
```{r}
names(evidence_group_numbers) = c("highest","high","moderate","low", "lowest")

evidence_groups[["h1"]]
evidence_groups[["h2"]]
evidence_groups[["m"]]
evidence_groups[["l1"]]
evidence_groups[["l2"]]


```{r}
ggplot(data.frame(evidence_group_numbers),aes(seq_along(evidence_group_numbers),evidence_group_numbers, fill="red"))+
  geom_bar(stat="identity", width=0.8)+
  theme_minimal()+
  geom_text(stat='identity', aes(label=evidence_group_numbers), vjust=-1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position="none")+
  theme(plot.margin = margin(1,1,1,1, "pt"))+
  #ggtitle("Number of genes in five confidence level groups")+
  xlab("Confidence level")+
  ylab("Number of genes")+
  scale_y_continuous(limit=c(0,2500))+
  scale_x_discrete(limits=names(evidence_group_numbers))
