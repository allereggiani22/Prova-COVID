source(here('R', 'librerie.R'))

seqs <- readDNAStringSet(here("dati","Allineamento per poster.fas"))

aln <- msa(seqs, method = "ClustalW")

cv <- msaConvert(aln, type = c("bios2mds::align"))

library(bios2mds)
export.fasta(cv, outfile = "outfile.fas", ncol(aln), open = "w")



bin <- as.DNAbin(aln)

an <- as.alignment(bin)
nm <- as.matrix(an)
nbinmat <- as.matrix(labels(bin))
class(bin)
dnbin <- dist.dna(bin, model = "TN93")
tree <- nj(dnbin)
tree2 <- (dnbin)
ggt <- ggtree(tree, cex = 0.8)+
  geom_tiplab(align=F, size=3.5)+
  geom_treescale(y = - 1, x= -20, aes(color=branch),fontsize = 7,options(ignore.negative.edge=TRUE))
ggt
nodeid(ggt, )


njmsaplot<-msaplot(ggt, bin, offset = 0.009, width=1, height = 0.5, color = c(rep("rosybrown", 1), rep("sienna1", 1), rep("lightgoldenrod1", 1), rep("lightskyblue1", 1), rep("green",1), rep("pink",1), rep("darkred",1)))
njmsaplot


