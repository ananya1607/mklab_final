library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(igraph)
library(ggplot2)
library(GenomicRanges)
library(InteractionSet)

# TE ans SE data directly from SEDB

se1<-fread("~/Documents/Ananya/Thesis_4/apr2/files/se_ele/SE_01_0039_SE_ele_hg38.bed")
se2<-fread("~/Documents/Ananya/Thesis_4/apr2/files/se_ele/SE_02_0545_SE_ele_hg38.bed")
se3<-fread("~/Documents/Ananya/Thesis_4/apr2/files/se_ele/SE_02_1033_SE_ele_hg38.bed")

te1<-fread("~/Documents/Ananya/Thesis_4/apr2/files/te/SE_01_0039_TE_hg38.bed")
te2<-fread("~/Documents/Ananya/Thesis_4/apr2/files/te/SE_02_0545_TE_hg38.bed")
te3<-fread("~/Documents/Ananya/Thesis_4/apr2/files/te/SE_02_1033_TE_hg38.bed")

process_se_te<-function(se1,se2,se3,te1,te2,te3){
  se1 <- se1 %>% select(1,2,3,4) %>% group_by(se_id) %>%  mutate(se_id = paste0(se_id, "_", row_number())) %>% ungroup()
  se2 <- se2 %>% select(1,2,3,4) %>% group_by(se_id) %>%  mutate(se_id = paste0(se_id, "_", row_number())) %>% ungroup()
  se3 <- se3 %>% select(1,2,3,4) %>% group_by(se_id) %>%  mutate(se_id = paste0(se_id, "_", row_number())) %>% ungroup()
  
  se <- rbind(se1,se2,se3)
  colnames(se)<-c("chrom","start","end","id")
  se<-as.data.frame(se)
  se$chrom<-gsub("^chr","",se$chrom)
  
  te1 <- te1 %>% select(1,2,3,4)
  te2 <- te2 %>% select(1,2,3,4)
  te3 <- te3 %>% select(1,2,3,4)
  
  te <- rbind(te1,te2,te3)
  colnames(te)<-c("chrom","start","end","id")
  te <- as.data.frame(te)
  te$chrom<-gsub("^chr","",te$chrom)
  
  return(list(se=se,te=te))
}

a<-process_se_te(se1,se2,se3,te1,te2,te3)
se<-a$se
te<-a$te

se1<-fread("~/Documents/Ananya/Thesis_4/apr2/files/se_ele/SE_01_0039_SE_ele_hg38.bed")
te1<-fread("~/Documents/Ananya/Thesis_4/apr2/files/te/SE_01_0039_TE_hg38.bed")
se1 <- se1 %>% select(1,2,3,4) %>% group_by(se_id) %>%  mutate(se_id = paste0(se_id, "_", row_number())) %>% ungroup()
se <- se1
colnames(se)<-c("chrom","start","end","id")
se<-as.data.frame(se)
se$chrom<-gsub("^chr","",se$chrom)
te1 <- te1 %>% select(1,2,3,4)
te <- te1
colnames(te)<-c("chrom","start","end","id")
te <- as.data.frame(te)
te$chrom<-gsub("^chr","",te$chrom)


get_ensembl_EP<- function(reg_f_path){
  
  m <- fread(reg_f_path)
  
  enhancers <- m %>% filter(V3 == "enhancer") %>% select(V1, V4, V5, V9)
  promoters <- m %>% filter(V3 == "promoter") %>% select(V1, V4, V5, V9)
  
  colnames(enhancers) <- c("chrom", "start", "end", "meta")
  colnames(promoters) <- c("chrom", "start", "end", "meta")
  
  relevant_enh <- enhancers %>%
    separate(meta, into = c("id", "end1", "start1", "desc", "feattype"), sep = ";") %>%
    separate(id, into = c("type1", "id"), sep = ":") %>%
    select(chrom, start, end, id)
  
  relevant_pro <- promoters %>%
    separate(meta, into = c("id", "end1", "start1", "desc", "feattype"), sep = ";") %>%
    separate(id, into = c("type1", "id"), sep = ":") %>%
    select(chrom, start, end, id)
  
  relevant_enh$chrom <- gsub("^chr", "", relevant_enh$chrom)
  relevant_pro$chrom <- gsub("^chr", "", relevant_pro$chrom)
  
  return(list(enh=relevant_enh, pro=relevant_pro))
}

reg_f_path <- "~/Documents/Ananya/Thesis/random enhancer data/homo_sapiens.GRCh38.Regulatory_Build.regulatory_features.20231016_ensembl.gff"

a<-get_ensembl_EP(reg_f_path)

promoter_df<-a$pro

a<-fread("~/Documents/Ananya/Thesis/random enhancer data/Homo_sapiens.GRCh38.regulatory_activity.v112.tsv")
a %>% head()
colnames(a)
k562_sp<-a %>% select(1,2,39)
k562_sp %>% head()
prom_k562<-k562_sp %>% filter(feature_type=="promoter")
prom_k562 %>% head()
prom_k562<-prom_k562 %>% filter(K562=="ACTIVE")
prom_k562 %>% head()
prom_k562 %>% nrow()
promoter_df<-promoter_df %>% filter(id %in% prom_k562$feature_id) 
promoter_df %>% nrow()

get_loops_df<-function(loop_path){
  loops <- as.data.frame(read.table(loop_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
  loops_rel <- loops %>% select(chromosome1, x1, x2, chromosome2, y1, y2, value)
  loops_chr1 <- loops %>% select(chromosome1, x1, x2) %>% unique()
  loops_chr2 <- loops %>% select(chromosome2, y1, y2) %>% unique()
  return(list(loops_rel=loops_rel,loops_chr1=loops_chr1,loops_chr2=loops_chr2))
}

wt0_lp <- "~/Documents/Ananya/Thesis/HiC_data/K562_phanstielLab/SIP_K562_WT_360_new_paper_params/5kbLoops_2025-02-25T113855_683.txt"

wt0_loops_rel <- get_loops_df(wt0_lp)
wt0_loops_rel<-wt0_loops_rel$loops_rel
loops<-wt0_loops_rel

colnames(loops)<-c("chr1","start1","end1","chr2","start2", "end2","value")
loops<-loops%>%select(1,2,3,4,5,6,7)
loops<-loops %>% mutate(loop_length=abs(start2-start1))
loops %>% head()
loops<-loops %>% mutate(start1=start1-10000,end1=end1+10000,start2=start2-10000,end2=end2+10000)

# typical_enhancer_list<-final_chr %>% filter(final_alt_type=="TE") %>% select(1) %>% unique()
# super_enhancer_list<-final_chr %>% filter(final_alt_type=="SE")%>% select(1) %>% unique()

typical_enhancers <- te 
super_enhancers <- se


get_loops_df<-function(loop_path){
  loops <- as.data.frame(read.table(loop_path, header = TRUE, sep = "\t", stringsAsFactors = FALSE))
  loops_rel <- loops %>% select(chromosome1, x1, x2, chromosome2, y1, y2, value)
  loops_chr1 <- loops %>% select(chromosome1, x1, x2) %>% unique()
  loops_chr2 <- loops %>% select(chromosome2, y1, y2) %>% unique()
  return(list(loops_rel=loops_rel,loops_chr1=loops_chr1,loops_chr2=loops_chr2))
}

wt0_lp<-"~/Documents/Ananya/Thesis/HiC_data/K562_phanstielLab/SIP_K562_WT_0_new_paper_params/5kbLoops_2025-02-24T195045_475.txt"
wt360_lp<-"~/Documents/Ananya/Thesis/HiC_data/K562_phanstielLab/SIP_K562_WT_360_new_paper_params/5kbLoops_2025-02-25T113855_683.txt"
wt4320_lp<-"~/Documents/Ananya/Thesis/HiC_data/K562_phanstielLab/SIP_K562_WT_4320_new_paper_params/5kbLoops_2025-02-25T142439_868.txt"

wt0_loops_rel <- get_loops_df(wt0_lp)
wt0_loops_rel<-wt0_loops_rel$loops_rel
loops_wt0<-wt0_loops_rel

wt360_loops_rel <- get_loops_df(wt360_lp)
wt360_loops_rel<-wt360_loops_rel$loops_rel
loops_wt360<-wt360_loops_rel

wt4320_loops_rel <- get_loops_df(wt4320_lp)
wt4320_loops_rel<-wt4320_loops_rel$loops_rel
loops_wt4320<-wt4320_loops_rel


colnames(loops_wt0)<-c("chr1","start1","end1","chr2","start2", "end2","value")
loops_wt0<-loops_wt0%>%select(1,2,3,4,5,6,7)
loops_wt0<-loops_wt0 %>% mutate(loop_length=abs(start2-start1))
loops_wt0 %>% head()
loops_wt0<-loops_wt0 %>% mutate(start1=start1-10000,end1=end1+10000,start2=start2-10000,end2=end2+10000)


colnames(loops_wt360)<-c("chr1","start1","end1","chr2","start2", "end2","value")
loops_wt360<-loops_wt360%>%select(1,2,3,4,5,6,7)
loops_wt360<-loops_wt360 %>% mutate(loop_length=abs(start2-start1))
loops_wt360 %>% head()
loops_wt360<-loops_wt360 %>% mutate(start1=start1-10000,end1=end1+10000,start2=start2-10000,end2=end2+10000)


colnames(loops_wt4320)<-c("chr1","start1","end1","chr2","start2", "end2","value")
loops_wt4320<-loops_wt4320%>%select(1,2,3,4,5,6,7)
loops_wt4320<-loops_wt4320 %>% mutate(loop_length=abs(start2-start1))
loops_wt4320 %>% head()
loops_wt4320<-loops_wt4320 %>% mutate(start1=start1-10000,end1=end1+10000,start2=start2-10000,end2=end2+10000)

te %>% head()

# typical_enhancer_list<-final_chr %>% filter(final_alt_type=="TE") %>% select(1) %>% unique()
# super_enhancer_list<-final_chr %>% filter(final_alt_type=="SE")%>% select(1) %>% unique()

# typical_enhancers <- enhancer_df %>% filter(id %in% typical_enhancer_list$enh_id) 
# super_enhancers <- enhancer_df %>% filter(id %in% super_enhancer_list$enh_id)

typical_enhancers<-te
super_enhancers<-se
give_truth_table<-function(loops, typical_enhancers, super_enhancers, promoter_df){
  anchor1<-GRanges(seqnames = loops$chr1,ranges=IRanges(start=loops$start1, end=loops$end1))
  anchor2<-GRanges(seqnames = loops$chr2,ranges=IRanges(start=loops$start2, end=loops$end2))
  ?GInteractions
  loops_g1<-GInteractions(anchor1,anchor2)
  mcols(loops_g1)$value<-loops$value
  loops_g1 %>% head()
  
  # making promoter granges
  promoter_gr<-GRanges(seqnames = promoter_df$chrom,ranges = IRanges(start=promoter_df$start, end=promoter_df$end))
  mcols(promoter_gr)$id<-promoter_df$id
  promoter_gr %>% head()
  
  # making TE granges
  typical_enhancers_gr<-GRanges(seqnames = typical_enhancers$chrom,ranges = IRanges(start=typical_enhancers$start, end=typical_enhancers$end))
  mcols(typical_enhancers_gr)$id<-typical_enhancers$id
  typical_enhancers_gr %>% head()
  typical_enhancers_gr %>% length() # 16830
  
  # making SE granges
  super_enhancers_gr<-GRanges(seqnames = super_enhancers$chrom,ranges = IRanges(start=super_enhancers$start, end=super_enhancers$end))
  mcols(super_enhancers_gr)$id<-super_enhancers$id
  super_enhancers_gr %>% head()
  super_enhancers_gr %>% length() # 2575
  
  # relaxing the pro, te, se - if necessary
  promoters_relaxed<-promoter_gr
  super_enhancers_relaxed<-super_enhancers_gr
  typical_enhancers_relaxed<-typical_enhancers_gr
  
  # finding overlaps between anchors and super enhancers
  a1<-anchors(loops_g1, type="first")
  a2<-anchors(loops_g1, type="second")
  
  # 
  
  
  hits1_se<-findOverlaps(a1,super_enhancers_relaxed)#,maxgap = 100)
  hits2_se<-findOverlaps(a2,super_enhancers_relaxed)#,maxgap = 100)
  
  hits1_te<-findOverlaps(a1,typical_enhancers_relaxed)#,maxgap = 100)
  hits2_te<-findOverlaps(a2,typical_enhancers_relaxed)#,maxgap = 100)
  
  hits1_p<-findOverlaps(a1,promoters_relaxed)#,maxgap = 100)
  hits2_p<-findOverlaps(a2,promoters_relaxed)#,maxgap = 100)
  
  N <- length(loops_g1)
  
  is_P_a1  <- rep(FALSE, N)
  is_P_a2  <- rep(FALSE, N)
  is_TE_a1 <- rep(FALSE, N)
  is_TE_a2 <- rep(FALSE, N)
  is_SE_a1 <- rep(FALSE, N)
  is_SE_a2 <- rep(FALSE, N)
  
  is_P_a1[ unique(queryHits(hits1_p)) ]  <- TRUE
  is_P_a2[ unique(queryHits(hits2_p)) ]  <- TRUE
  is_TE_a1[ unique(queryHits(hits1_te)) ] <- TRUE
  is_TE_a2[ unique(queryHits(hits2_te)) ] <- TRUE
  is_SE_a1[ unique(queryHits(hits1_se)) ] <- TRUE
  is_SE_a2[ unique(queryHits(hits2_se)) ] <- TRUE
  
  mcols(loops_g1)$is_P_a1  <- is_P_a1
  mcols(loops_g1)$is_P_a2  <- is_P_a2
  mcols(loops_g1)$is_TE_a1 <- is_TE_a1
  mcols(loops_g1)$is_TE_a2 <- is_TE_a2
  mcols(loops_g1)$is_SE_a1 <- is_SE_a1
  mcols(loops_g1)$is_SE_a2 <- is_SE_a2
  
  return(list(loops_g1=loops_g1,
              promoters_relaxed=promoters_relaxed,
              super_enhancers_relaxed=super_enhancers_relaxed, 
              typical_enhancers_relaxed=typical_enhancers_relaxed,
              hits1_p=hits1_p,
              hits2_p=hits2_p,
              hits1_se=hits1_se,
              hits2_se=hits2_se,
              hits1_te=hits1_te,
              hits2_te=hits2_te))
}

loops_g1_wt0<-give_truth_table(loops_wt0,typical_enhancers, super_enhancers, promoter_df)
loops_g1_wt360<-give_truth_table(loops_wt360,typical_enhancers, super_enhancers, promoter_df)
loops_g1_wt4320<-give_truth_table(loops_wt4320,typical_enhancers, super_enhancers, promoter_df)

promoters_relaxed<-loops_g1_wt0$promoters_relaxed
super_enhancers_relaxed<-loops_g1_wt0$super_enhancers_relaxed
typical_enhancers_relaxed<-loops_g1_wt0$typical_enhancers_relaxed

hits1_p_wt0<-loops_g1_wt0$hits1_p
hits2_p_wt0<-loops_g1_wt0$hits2_p

hits1_te_wt0<-loops_g1_wt0$hits1_te
hits2_te_wt0<-loops_g1_wt0$hits2_te

hits1_se_wt0<-loops_g1_wt0$hits1_se
hits2_se_wt0<-loops_g1_wt0$hits2_se

loops_g1_wt0<-loops_g1_wt0$loops_g1


hits1_p_wt360<-loops_g1_wt360$hits1_p
hits2_p_wt360<-loops_g1_wt360$hits2_p

hits1_te_wt360<-loops_g1_wt360$hits1_te
hits2_te_wt360<-loops_g1_wt360$hits2_te

hits1_se_wt360<-loops_g1_wt360$hits1_se
hits2_se_wt360<-loops_g1_wt360$hits2_se

loops_g1_wt360<-loops_g1_wt360$loops_g1


hits1_p_wt4320<-loops_g1_wt4320$hits1_p
hits2_p_wt4320<-loops_g1_wt4320$hits2_p

hits1_te_wt4320<-loops_g1_wt4320$hits1_te
hits2_te_wt4320<-loops_g1_wt4320$hits2_te

hits1_se_wt4320<-loops_g1_wt4320$hits1_se
hits2_se_wt4320<-loops_g1_wt4320$hits2_se

loops_g1_wt4320<-loops_g1_wt4320$loops_g1

giveloop_ids<-function(loops_g1, promoters_relaxed,typical_enhancers_relaxed,super_enhancers_relaxed, hits1_p, hits2_p, hits1_se, hits2_se, hits1_te, hits2_te){
  anchor_wise_ids<-function(df_gr, hits){
    df_idx<-mcols(df_gr)$id[subjectHits(hits)]
    list<-split(df_idx, queryHits(hits))
    return(list)
  }
  
  # for promoter
  anchor1_prom_list <- anchor_wise_ids(promoters_relaxed, hits1_p)
  anchor2_prom_list <- anchor_wise_ids(promoters_relaxed, hits2_p)
  
  # appending to loops_g1
  N <- length(loops_g1)
  
  anchor1_promIDs <- vector("list", N)
  anchor2_promIDs <- vector("list", N)
  
  for (i in as.integer(names(anchor1_prom_list))) {
    anchor1_promIDs[[i]] <- anchor1_prom_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor1_promoter_ids <- anchor1_promIDs
  
  for (i in as.integer(names(anchor2_prom_list))) {
    anchor2_promIDs[[i]] <- anchor2_prom_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor2_promoter_ids <- anchor2_promIDs
  
  # for super enhancer
  anchor1_se_list <- anchor_wise_ids(super_enhancers_relaxed, hits1_se)
  anchor2_se_list <- anchor_wise_ids(super_enhancers_relaxed, hits2_se)
  
  # appending to loops_g1
  N <- length(loops_g1)
  anchor1_seIDs <- vector("list", N)
  anchor2_seIDs <- vector("list", N)
  
  for (i in as.integer(names(anchor1_se_list))) {
    anchor1_seIDs[[i]] <- anchor1_se_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor1_se_ids <- anchor1_seIDs
  
  for (i in as.integer(names(anchor2_se_list))) {
    anchor2_seIDs[[i]] <- anchor2_se_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor2_se_ids <- anchor2_seIDs
  
  # for typical enhancer
  anchor1_te_list <- anchor_wise_ids(typical_enhancers_relaxed, hits1_te)
  anchor2_te_list <- anchor_wise_ids(typical_enhancers_relaxed, hits2_te)
  
  # appending it to loops_g1
  N <- length(loops_g1)
  anchor1_teIDs <- vector("list", N)
  anchor2_teIDs <- vector("list", N)
  
  # scales the ids according to the promoter dataframe
  for (i in as.integer(names(anchor1_te_list))) {
    anchor1_teIDs[[i]] <- anchor1_te_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor1_te_ids <- anchor1_teIDs
  
  for (i in as.integer(names(anchor2_te_list))) {
    anchor2_teIDs[[i]] <- anchor2_te_list[[ as.character(i)]]
  }
  
  mcols(loops_g1)$anchor2_te_ids <- anchor2_teIDs
  return(loops_g1)
}

loops_g1_wt0<-giveloop_ids(loops_g1_wt0,promoters_relaxed, typical_enhancers_relaxed,super_enhancers_relaxed, hits1_p_wt0, hits2_p_wt0, hits1_se_wt0, hits2_se_wt0, hits1_te_wt0, hits2_te_wt0)
loops_g1_wt360<-giveloop_ids(loops_g1_wt360,promoters_relaxed, typical_enhancers_relaxed,super_enhancers_relaxed, hits1_p_wt360, hits2_p_wt360, hits1_se_wt360, hits2_se_wt360, hits1_te_wt360, hits2_te_wt360)
loops_g1_wt4320<-giveloop_ids(loops_g1_wt4320,promoters_relaxed, typical_enhancers_relaxed,super_enhancers_relaxed, hits1_p_wt4320, hits2_p_wt4320, hits1_se_wt4320, hits2_se_wt4320, hits1_te_wt4320, hits2_te_wt4320)


classify_interaction<-function(p1,se1,te1){
  if(p1){
    return("P")
  }
  else if(se1){
    return("SE")
  }
  else if(te1){
    return("TE")
  }
  else{
    return("other")
  }
}

df_wt0<-as.data.frame(loops_g1_wt0)
df_wt360<-as.data.frame(loops_g1_wt360)
df_wt4320<-as.data.frame(loops_g1_wt4320)


df_wt0$interaction_a1 <- mapply(
  classify_interaction,
  df_wt0$is_P_a1,  df_wt0$is_SE_a1,  df_wt0$is_TE_a1
)

df_wt0$interaction_a2 <- mapply(
  classify_interaction,
  df_wt0$is_P_a2,  df_wt0$is_SE_a2,  df_wt0$is_TE_a2
)

df_wt0$interaction_type <- paste0(df_wt0$interaction_a1,"-",df_wt0$interaction_a2)

mcols(loops_g1_wt0)$interaction_type <- df_wt0$interaction_type

b_wt0<-as.data.frame(mcols(loops_g1_wt0)) %>% group_by(interaction_type) %>% summarise(count=n())

ggplot(b_wt0, aes(x = interaction_type, y = count, fill =interaction_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Counts of the nodes",
       x = "",
       y = "Counts") +
  theme_minimal()

df_wt360$interaction_a1 <- mapply(
  classify_interaction,
  df_wt360$is_P_a1,  df_wt360$is_SE_a1,  df_wt360$is_TE_a1
)

df_wt360$interaction_a2 <- mapply(
  classify_interaction,
  df_wt360$is_P_a2,  df_wt360$is_SE_a2,  df_wt360$is_TE_a2
)

df_wt360$interaction_type <- paste0(df_wt360$interaction_a1,"-",df_wt360$interaction_a2)

mcols(loops_g1_wt360)$interaction_type <- df_wt360$interaction_type

b_wt360<-as.data.frame(mcols(loops_g1_wt360)) %>% group_by(interaction_type) %>% summarise(count=n())




df_wt4320$interaction_a1 <- mapply(
  classify_interaction,
  df_wt4320$is_P_a1,  df_wt4320$is_SE_a1,  df_wt4320$is_TE_a1
)

df_wt4320$interaction_a2 <- mapply(
  classify_interaction,
  df_wt4320$is_P_a2,  df_wt4320$is_SE_a2,  df_wt4320$is_TE_a2
)

df_wt4320$interaction_type <- paste0(df_wt4320$interaction_a1,"-",df_wt4320$interaction_a2)

mcols(loops_g1_wt4320)$interaction_type <- df_wt4320$interaction_type

b_wt4320<-as.data.frame(mcols(loops_g1_wt4320)) %>% group_by(interaction_type) %>% summarise(count=n())

loops_g1_wt0 %>% head()

as.data.frame(mcols(loops_g1_wt0)) %>% select(interaction_type) %>% filter(interaction_type %in% c("P-P","P-SE","P-TE","SE-P","TE-P","TE-TE","SE-SE","SE-TE","TE-SE")) %>% nrow()
# as.data.frame(mcols(loops_g1_wt0)) %>% select(interaction_type) %>% filter(interaction_type %in% c("P-SE","P-TE","SE-P","TE-P")) %>% nrow()

as.data.frame(mcols(loops_g1_wt0))  %>% select(10,11,12,13) %>% head() 
# Function to flatten a column of lists into a dataframe
decompose_column_to_df <- function(column_data, column_name = "enhancer") {
  unlisted <- unlist(column_data, use.names = FALSE)
  tibble(!!column_name := unlisted)
}

# Apply to each column
result_list <- lapply(names(df), function(colname) {
  decompose_column_to_df(df[[colname]], column_name = colname)
})

# Named list of dataframes
names(result_list) <- names(df)

# Example: access individual dataframes
result_list$A
result_list$B
as.data.frame(mcols(loops_g1_wt360)) %>% select(interaction_type) %>% filter(interaction_type %in% c("P-P","P-SE","P-TE","SE-P","TE-P","TE-TE","SE-SE","SE-TE","TE-SE")) %>% nrow()
loops_wt360 %>% nrow()

as.data.frame(mcols(loops_g1_wt4320)) %>% select(interaction_type) %>% filter(interaction_type %in% c("P-P","P-SE","P-TE","SE-P","TE-P","TE-TE","SE-SE","SE-TE","TE-SE")) %>% nrow()
loops_wt4320 %>% nrow()

make_edges_nodes<-function(granges_obj,anchor1, anchor2, anchor1_type, anchor2_type, loops,interaction_type){
  df<-as.data.frame(granges_obj)
  df_list<-lapply(seq_len(nrow(df)), function(i){
    # For row i, get all combos
    combos <- expand.grid(
      anchor1_ids = df$anchor1[[i]], 
      anchor2_ids = df$anchor2[[i]],
      KEEP.OUT.ATTRS = FALSE, 
      stringsAsFactors = FALSE
    )
    row_meta <- df[i, c("seqnames1", "start1", "end1", "seqnames2", "start2", "end2")]  
    cbind(row_meta[rep(1, nrow(combos)), ], combos)
  })
  df_df<- do.call(rbind, df_list)
  df_df$LA1 <- do.call(paste, c(df_df[, 1:3], sep="_"))
  df_df$LA2 <- do.call(paste, c(df_df[, 4:6], sep="_"))
  df_df<-df_df %>% left_join(loops,by=c("seqnames1"="chr1","start1", "end1","seqnames2"="chr2","start2", "end2"))
  
  ids<-df_df %>% select(anchor1_ids,anchor2_ids,value) %>% unique()
  anchors<-df_df %>% select(LA1,LA2, value) %>% unique()
  
  colnames(ids)<-c("LA1","LA2","value")
  ids<-ids %>% mutate("LA1_type"=as.character(anchor1_type),"LA2_type"=as.character(anchor2_type), "interaction_type"=as.character(interaction_type))
  edges<-ids %>% select(1,2,3,6) %>% unique()
  
  a1<-ids %>% select(1,4) %>% unique()
  a2<-ids %>% select(2,5) %>% unique()
  colnames(a2)<-colnames(a1)
  nodes<-rbind(a1,a2) %>% unique()
  
  anchors <- anchors %>% mutate("LA1_type"=as.character(anchor1_type),"LA2_type"=as.character(anchor2_type),"interaction_type"=as.character(interaction_type))
  anchors %>% head()
  
  anchors_edges<-anchors %>% select(1,2,3,6) %>% unique()
  a1<-anchors %>% select(1,4) %>% unique()
  a2<-anchors %>% select(2,5) %>% unique()
  colnames(a2)<-colnames(a1)
  anchors_nodes<-rbind(a1,a2) %>% unique()
  
  return(list(id_edges=edges,id_nodes=nodes,anchor_edges=anchors_edges, anchor_nodes=anchors_nodes))
}

give_total_edges_nodes<-function(loops_g1, loops){
  
  loops_by_type <- split(loops_g1, mcols(loops_g1)$interaction_type)
  loops_by_type %>% head()
  
  loops_PP <- loops_by_type[["P-P"]]
  mcols(loops_PP)<-mcols(loops_PP)[,c("anchor1_promoter_ids", "anchor2_promoter_ids","interaction_type")]
  loops_TETE<-loops_by_type[["TE-TE"]]
  mcols(loops_TETE)<-mcols(loops_TETE)[,c("anchor1_te_ids", "anchor2_te_ids","interaction_type")]
  loops_SESE<-loops_by_type[["SE-SE"]]
  mcols(loops_SESE)<-mcols(loops_SESE)[,c("anchor1_se_ids", "anchor2_se_ids","interaction_type")]
  
  loops_TESE <- loops_by_type[["TE-SE"]]
  mcols(loops_TESE)<-mcols(loops_TESE)[,c("anchor1_te_ids", "anchor2_se_ids","interaction_type")]
  loops_SETE <- loops_by_type[["SE-TE"]]
  mcols(loops_SETE)<-mcols(loops_SETE)[,c("anchor1_se_ids", "anchor2_te_ids","interaction_type")]
  
  loops_PTE <- loops_by_type[["P-TE"]]
  mcols(loops_PTE)<-mcols(loops_PTE)[,c("anchor1_promoter_ids", "anchor2_te_ids","interaction_type")]
  loops_TEP <- loops_by_type[["TE-P"]]
  mcols(loops_TEP)<-mcols(loops_TEP)[,c("anchor1_te_ids", "anchor2_promoter_ids","interaction_type")]
  
  loops_PSE <- loops_by_type[["P-SE"]]
  mcols(loops_PSE)<-mcols(loops_PSE)[,c("anchor1_promoter_ids", "anchor2_se_ids","interaction_type")]
  loops_SEP <- loops_by_type[["SE-P"]]
  mcols(loops_SEP)<-mcols(loops_SEP)[,c("anchor1_se_ids", "anchor2_promoter_ids","interaction_type")]
  
  
  
  # SE SE
  loops_SESE %>% head()
  
  a<-make_edges_nodes(granges_obj = loops_SESE,anchor1 = anchor1_se_ids,anchor2 = anchor2_se_ids, anchor1_type = "super enhancer",anchor2_type = "super enhancer",loops,interaction_type = "SE-SE" )
  
  SESE_edges<-a$id_edges
  SESE_nodes<-a$id_nodes
  
  # SESE_edges %>% head()
  # SESE_nodes %>% head()
  
  
  # SE TE
  a<-make_edges_nodes(granges_obj = loops_SETE, anchor1 = anchor1_se_ids,anchor2 = anchor2_te_ids, anchor1_type = "super enhancer",anchor2_type = "typical enhancer",loops,interaction_type = "SE-TE" )
  
  SETE_edges<-a$id_edges
  SETE_nodes<-a$id_nodes
  
  # SETE_edges %>% head()
  
  
  # TE SE
  
  a<-make_edges_nodes(granges_obj = loops_TESE, anchor1 = anchor1_te_ids,anchor2 = anchor2_se_ids, anchor1_type = "typical enhancer",anchor2_type = "super enhancer",loops,interaction_type = "TE-SE" )
  
  TESE_edges<-a$id_edges
  TESE_nodes<-a$id_nodes
  
  # TESE_edges %>% head()
  
  # SE P
  
  a<-make_edges_nodes(granges_obj = loops_SEP, anchor1 = anchor1_se_ids,anchor2 = anchor2_promoter_ids, anchor1_type = "super enhancer",anchor2_type = "promoter",loops,interaction_type = "SE-P" )
  
  SEP_edges<-a$id_edges
  SEP_nodes<-a$id_nodes
  
  # SEP_edges %>% head()
  
  # P SE
  
  a<-make_edges_nodes(granges_obj = loops_PSE, anchor1 = anchor1_promoter_ids,anchor2 = anchor2_se_ids, anchor1_type = "promoter",anchor2_type = "super enhancer",loops,interaction_type = "P-SE" )
  
  PSE_edges<-a$id_edges
  PSE_nodes<-a$id_nodes
  
  # PSE_edges %>% head()
  
  
  # P TE 
  a<-make_edges_nodes(granges_obj = loops_PTE, anchor1 = anchor1_promoter_ids,anchor2 = anchor2_te_ids, anchor1_type = "promoter",anchor2_type = "typical enhancer",loops,interaction_type = "P-TE" )
  
  PTE_edges<-a$id_edges
  PTE_nodes<-a$id_nodes
  
  # PTE_edges %>% head()
  
  # TE P
  a<-make_edges_nodes(granges_obj = loops_TEP, anchor1 = anchor1_te_ids,anchor2 = anchor2_promoter_ids, anchor1_type = "typical enhancer",anchor2_type = "promoter",loops,interaction_type = "TE-P" )
  
  TEP_edges<-a$id_edges
  TEP_nodes<-a$id_nodes
  
  # TEP_edges %>% head()
  
  # P P
  
  a<-make_edges_nodes(granges_obj = loops_PP, anchor1 = anchor1_promoter_ids,anchor2 = anchor2_promoter_ids, anchor1_type = "promoter",anchor2_type = "promoter",loops,interaction_type = "P-P" )
  
  PP_edges<-a$id_edges
  PP_nodes<-a$id_nodes
  
  # PP_edges %>% head()
  # PP_nodes %>% head()
  
  # TE TE
  
  a<-make_edges_nodes(granges_obj = loops_TETE, anchor1 = anchor1_te_ids,anchor2 = anchor2_te_ids, anchor1_type = "typical enhancer",anchor2_type = "typical enhancer",loops,interaction_type = "TE-TE" )
  
  TETE_edges<-a$id_edges
  TETE_nodes<-a$id_nodes
  
  # TETE_edges %>% head()
  # TETE_nodes %>% head()
  
  
  # TE TE 
  # P P 
  # SE SE
  
  # SE P
  # P SE
  
  # PSE_edges %>% head()
  # PSE_nodes %>% head()
  a <- PSE_edges %>% select(2,1,3) %>% mutate(interaction_type="SE-P") %>% rename("LA1"="LA2","LA2"="LA1")
  
  SEP_nodes_final<- rbind(PSE_nodes, SEP_nodes) %>% unique()
  SEP_edges_final<-rbind(SEP_edges, a) %>% unique()
  
  
  # P TE 
  # TE P
  # PTE_edges %>% head()
  # PTE_nodes %>% head()
  a <- PTE_edges %>% select(2,1,3) %>% mutate(interaction_type="TE-P") %>% rename("LA1"="LA2","LA2"="LA1")
  
  TEP_nodes_final<- rbind(PTE_nodes, TEP_nodes) %>% unique()
  TEP_edges_final<-rbind(TEP_edges, a) %>% unique()
  
  # TE SE
  # SE TE
  
  # TESE_edges %>% head()
  # SETE_edges %>% head()
  
  a <- TESE_edges %>% select(2,1,3) %>% mutate(interaction_type="SE-TE") %>% rename("LA1"="LA2","LA2"="LA1")
  SETE_nodes_final<- rbind(SETE_nodes, TESE_nodes) %>% unique()
  SETE_edges_final<- rbind(SETE_edges, a) %>% unique()
  
  
  # total
  # PP_edges %>% head()
  # SESE_edges %>% head()
  # TETE_edges %>% head()
  # SEP_edges_final %>% head()
  # TEP_edges_final %>% head()
  # SETE_edges_final %>% head()
  
  total_edges<-rbind(PP_edges,SESE_edges,TETE_edges,SEP_edges_final,TEP_edges_final,SETE_edges_final) %>% unique()
  # total_edges %>% nrow()
  # total_edges %>% unique() %>% nrow() # 1762
  # total_edges %>% select(interaction_type) %>% unique()
  
  
  total_nodes<-rbind(PP_nodes,SESE_nodes,TETE_nodes,SEP_nodes_final,TEP_nodes_final,SETE_nodes_final) %>% unique()
  total_nodes<-unique(total_nodes)
  
  return(list(total_nodes=total_nodes, total_edges=total_edges))
}

a_wt0<-give_total_edges_nodes(loops_g1_wt0, loops = loops_wt0)
total_edges_wt0<-a_wt0$total_edges
total_nodes_wt0<-a_wt0$total_nodes

a_wt360<-give_total_edges_nodes(loops_g1_wt360, loops = loops_wt360)
total_edges_wt360<-a_wt360$total_edges
total_nodes_wt360<-a_wt360$total_nodes

a_wt4320<-give_total_edges_nodes(loops_g1_wt4320, loops = loops_wt4320)
total_edges_wt4320<-a_wt4320$total_edges
total_nodes_wt4320<-a_wt4320$total_nodes

total_edges_wt0 %>% head()
total_edges_wt0 %>% group_by(interaction_type) %>% summarise(count=n()) %>% head()
m<-total_edges_wt0 %>% group_by(interaction_type) %>% summarise(count=n()) 
ggplot(m, aes(x =interaction_type, y = count, fill =interaction_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Counts of the nodes",
       x = "",
       y = "Counts") +
  theme_minimal()


setwd("~/Desktop/")
write.table(total_edges_wt0, "edges_sedb.csv", quote = F, sep = ",", row.names = F, col.names = T)
write.table( total_nodes_wt0, "nodes_sedb.csv", quote = F, sep = ",", row.names = F, col.names = T)
constrct_nw<-function(total_nodes,total_edges){
  g<-graph_from_data_frame(d = total_edges, directed = F, vertices = total_nodes)
  graph_wt0<-g
  
  E(g)$weight<-E(g)$value
  vcount(g) # 3641
  ecount(g) # 4177
  components(g)$no # 825
  
  a<-total_nodes %>% filter(LA1_type=="promoter") %>% unique() %>% nrow() # 2153
  b<-total_nodes %>% filter(LA1_type=="super enhancer") %>% unique() %>% nrow() # 3406
  c<-total_nodes %>% filter(LA1_type=="typical enhancer") %>% unique() %>% nrow() # 6221
  
  df<-data.frame(char=c("Promoter", "SE constituent","Typical Enhancer"), Count=c(a,b,c))
  
  components_g <- components(g)
  
  total_cwise <- data.frame(Node=names(components_g$membership),
                            Cluster=components_g$membership)
  
  clu_wise_enhancers<-total_cwise %>% left_join(total_nodes,by=c("Node"="LA1")) %>% filter(!LA1_type=="promoter")
  clu_wise_nodes<-total_cwise %>% left_join(total_nodes,by=c("Node"="LA1"))
  
  cluster_classification<-clu_wise_nodes %>%
    group_by(Cluster) %>%
    summarise(
      Types = list(unique(LA1_type))        # all LA1_type values in this cluster
    ) %>%  mutate(type_set = map(Types, ~ sort(unique(.x)))) %>%
    mutate(
      cluster_type = map_chr(type_set, function(ts) {
        # ts is the sorted unique vector for one cluster
        if (identical(ts, "promoter")) {
          "P"                # only P
        } else if (identical(ts, "typical enhancer")) {
          "TE"               # only TE
        } else if (identical(ts, "super enhancer")) {
          "SE"               # only SE
        } else if (identical(ts, c("promoter","super enhancer"))) {
          "P-SE"
        } else if (identical(ts, c("promoter","typical enhancer"))) {
          "P-TE"
        } else if (identical(ts, c("super enhancer","typical enhancer"))) {
          "SE-TE"
        } else if (identical(ts, c("promoter","super enhancer","typical enhancer"))) {
          "P-SE-TE"
        } else {
          paste(ts, collapse="-")
        }
      })
    )
  
  df1<-data.frame(Cluster_Type=character(), Count=integer(), stringsAsFactors = F)
  # df %>% head()
  
  for(i in unique(cluster_classification$cluster_type)){
    print(i)
    m<-cluster_classification %>% filter(cluster_type==i) %>% unique() %>% nrow()
    print(m)
    df1<-rbind(df1, data.frame(Cluster_Type=i, Count=m))
  }
  
  clusters<-components(g)
  cluster_sizes<-clusters$csize
  df3 <- data.frame(cluster_size = cluster_sizes) %>% mutate(Cluster=row_number())
  df3 %>% head()
  cluster_classification %>% head()
  df2<-df3 %>% left_join(cluster_classification,by="Cluster",relationship = "many-to-many")
  df2 %>% head()
  df2<-df2 %>% select(2,1,5)
  
  return(list(graph=g, df=df, df1=df1, df2=df2,  cluster_classification=cluster_classification, components_g=components_g))
}

a_wt0<-constrct_nw(total_nodes_wt0, total_edges_wt0)
df_wt0<-a_wt0$df
df1_wt0<-a_wt0$df1
df2_wt0<-a_wt0$df2
g_wt0<-a_wt0$graph
components_g_wt0<-a_wt0$components_g
cluster_classification_wt0=a_wt0$cluster_classification

a_wt360<-constrct_nw(total_nodes_wt360, total_edges_wt360)
df_wt360<-a_wt360$df
df1_wt360<-a_wt360$df1
df2_wt360<-a_wt360$df2
g_wt360<-a_wt360$graph
components_g_wt360<-a_wt360$components_g
cluster_classification_wt360=a_wt360$cluster_classification

a_wt4320<-constrct_nw(total_nodes_wt4320, total_edges_wt4320)
df_wt4320<-a_wt4320$df
df1_wt4320<-a_wt4320$df1
df2_wt4320<-a_wt4320$df2
g_wt4320<-a_wt4320$graph
components_g_wt4320<-a_wt4320$components_g
cluster_classification_wt4320=a_wt4320$cluster_classification

ggplot(df_wt0, aes(x = char, y = Count, fill =char)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Counts of the nodes",
       x = "",
       y = "Counts") +
  theme_minimal()

ggplot(df_wt360, aes(x = char, y = Count, fill =char)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Counts of the nodes",
       x = "",
       y = "Counts") +
  theme_minimal()

ggplot(df_wt4320, aes(x = char, y = Count, fill =char)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Counts of the nodes",
       x = "",
       y = "Counts") +
  theme_minimal()

ggplot(df1_wt0, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  theme_minimal()

ggplot(df1_wt360, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  theme_minimal()

ggplot(df1_wt4320, aes(x = Cluster_Type, y = Count, fill = Cluster_Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), vjust = 0, size = 4) +  # Add count labels above bars
  labs(title = "Number of clusters in each category",
       x = "",
       y = "Number of clusters") +
  theme_minimal()

ggplot(df2_wt0, aes(x = cluster_size)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(df2_wt0$cluster_size), max(df2_wt0$cluster_size), by = 1)) +  # Set x labels at interval of 1
  labs(title = "Cluster Size Distribution",
       x = "Cluster Size",
       y = "Frequency") +
  theme_minimal()

ggplot(df2_wt0, aes(x = cluster_type, y = cluster_size, fill = cluster_type)) +
  geom_boxplot() +
  labs(title = "Cluster Size Distribution by Cluster Category",
       x = "Cluster Category",
       y = "Cluster Size",
       fill = "Cluster Category") +
  theme_minimal()

ggplot(df2_wt360, aes(x = cluster_size)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(df2_wt360$cluster_size), max(df2_wt360$cluster_size), by = 1)) +  # Set x labels at interval of 1
  labs(title = "Cluster Size Distribution",
       x = "Cluster Size",
       y = "Frequency") +
  theme_minimal()

ggplot(df2_wt360, aes(x = cluster_type, y = cluster_size, fill = cluster_type)) +
  geom_boxplot() +
  labs(title = "Cluster Size Distribution by Cluster Category",
       x = "Cluster Category",
       y = "Cluster Size",
       fill = "Cluster Category") +
  theme_minimal()

ggplot(df2_wt4320, aes(x = cluster_size)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(min(df2_wt4320$cluster_size), max(df2_wt4320$cluster_size), by = 1)) +  # Set x labels at interval of 1
  labs(title = "Cluster Size Distribution",
       x = "Cluster Size",
       y = "Frequency") +
  theme_minimal()

ggplot(df2_wt4320, aes(x = cluster_type, y = cluster_size, fill = cluster_type)) +
  geom_boxplot() +
  labs(title = "Cluster Size Distribution by Cluster Category",
       x = "Cluster Category",
       y = "Cluster Size",
       fill = "Cluster Category") +
  theme_minimal()


plot_graphs <- function(df, yvar, title = "Enhancer degree Distribution", 
                        x_axis = "Cluster Category", y_axis = "degree") {
  
  # Plot 1
  plot1 <- ggplot(df, aes(x = LA1_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot() +
    labs(title = title, x = x_axis, y = y_axis) +
    scale_y_continuous(limits = c(0, max(pull(df, {{ yvar }}), na.rm = TRUE))) +
    theme_minimal()
  
  print(plot1)
  
  # Quantiles
  quantiles <- df %>%
    group_by(LA1_type) %>%
    summarise(
      Q1 = quantile({{ yvar }}, 0.25, na.rm = TRUE),
      Median = quantile({{ yvar }}, 0.5, na.rm = TRUE),
      Q3 = quantile({{ yvar }}, 0.75, na.rm = TRUE)
    )
  
  min_limit <- min(quantiles$Q1) - 1.5 * IQR(pull(df, {{ yvar }}), na.rm = TRUE)
  max_limit <- max(quantiles$Q3) + 1.5 * IQR(pull(df, {{ yvar }}), na.rm = TRUE)
  
  # Plot 2
  plot2 <- ggplot(df, aes(x = LA1_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(min_limit, max_limit)) +
    theme_minimal() +
    labs(title = "zoomed, ol removed", x = x_axis, y = y_axis) +
    geom_text(data = quantiles, aes(x = LA1_type, y = Q1, label = paste0("Q1: ", round(Q1, 2))), 
              vjust = 1.5, color = "red", size = 4, inherit.aes = FALSE) +
    geom_text(data = quantiles, aes(x = LA1_type, y = Median, label = paste0("Median: ", round(Median, 2))), 
              vjust = -0.5, color = "blue", size = 4, inherit.aes = FALSE)
  
  # print(plot2)
  
  # Subset to P-SE, P-SE-TE, P-TE
  df_sub <- df %>% filter(cluster_type %in% c("P-SE", "P-SE-TE", "P-TE"))
  
  # Plot 3
  plot3 <- ggplot(df_sub, aes(x = cluster_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot() +
    labs(title = title, x = x_axis, y = y_axis) +
    scale_y_continuous(limits = c(0, max(pull(df_sub, {{ yvar }}), na.rm = TRUE))) +
    theme_minimal()
  
  # print(plot3)
  
  # Quantiles for filtered df
  quantiles <- df_sub %>%
    group_by(cluster_type) %>%
    summarise(
      Q1 = quantile({{ yvar }}, 0.25, na.rm = TRUE),
      Median = quantile({{ yvar }}, 0.5, na.rm = TRUE),
      Q3 = quantile({{ yvar }}, 0.75, na.rm = TRUE)
    )
  
  min_limit <- min(quantiles$Q1) - 1.5 * IQR(pull(df_sub, {{ yvar }}), na.rm = TRUE)
  max_limit <- max(quantiles$Q3) + 1.5 * IQR(pull(df_sub, {{ yvar }}), na.rm = TRUE)
  
  # Plot 4
  plot4 <- ggplot(df_sub, aes(x = cluster_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(min_limit, max_limit)) +
    theme_minimal() +
    geom_text(data = quantiles, aes(x = cluster_type, y = Q1, label = paste0("Q1: ", round(Q1, 2))), 
              vjust = 1.5, color = "red", size = 4, inherit.aes = FALSE) +
    geom_text(data = quantiles, aes(x = cluster_type, y = Median, label = paste0("Median: ", round(Median, 2))), 
              vjust = -0.5, color = "blue", size = 4, inherit.aes = FALSE) +
    geom_text(data = quantiles, aes(x = cluster_type, y = Q3, label = paste0("Q3: ", round(Q3, 2))), 
              vjust = -1.5, color = "darkgreen", size = 4, inherit.aes = FALSE)
  
  # print(plot4)
  
  # Plot 5
  plot5 <- ggplot(df_sub, aes(x = LA1_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot() +
    labs(title = title, x = x_axis, y = y_axis) +
    scale_y_continuous(limits = c(0, max(pull(df_sub, {{ yvar }}), na.rm = TRUE))) +
    theme_minimal()
  
  # print(plot5)
  
  # Quantiles for plot5
  quantiles <- df_sub %>%
    group_by(LA1_type) %>%
    summarise(
      Q1 = quantile({{ yvar }}, 0.25, na.rm = TRUE),
      Median = quantile({{ yvar }}, 0.5, na.rm = TRUE),
      Q3 = quantile({{ yvar }}, 0.75, na.rm = TRUE)
    )
  
  min_limit <- min(quantiles$Q1) - 1.5 * IQR(pull(df_sub, {{ yvar }}), na.rm = TRUE)
  max_limit <- max(quantiles$Q3) + 1.5 * IQR(pull(df_sub, {{ yvar }}), na.rm = TRUE)
  
  # Plot 6
  plot6 <- ggplot(df_sub, aes(x = LA1_type, y = {{ yvar }}, fill = LA1_type)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = c(min_limit, max_limit)) +
    theme_minimal() +
    geom_text(data = quantiles, aes(x = LA1_type, y = Q1, label = paste0("Q1: ", round(Q1, 2))), 
              vjust = 1.5, color = "red", size = 4, inherit.aes = FALSE) +
    geom_text(data = quantiles, aes(x = LA1_type, y = Median, label = paste0("Median: ", round(Median, 2))), 
              vjust = -0.5, color = "blue", size = 4, inherit.aes = FALSE)
  
  # print(plot6)
  print("All done!")
}


calculate_nw_ppts<-function(g, components_g, cluster_classification){
  
  # degree
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_degree_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    subgraph_degree <- degree(subgraph, normalized = F)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, degree=subgraph_degree[match(name, V(subgraph)$name)]) %>% select(name,Cluster,degree, LA1_type)
    enhancer_degree_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_degree_df<- bind_rows(enhancer_degree_list, .id = "Cluster")
  enhancer_degree_df$Cluster<-as.character(enhancer_degree_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_degree_df<-enhancer_degree_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_degree<-enhancer_degree_df
  a<-plot_graphs(df = enhancer_degree_df,yvar = degree,title = "degree",x_axis = "cluster type",y_axis = "degree")
  # print(a)
  # between all super enhancers and typical enhancers
  enhancer_degree_df %>% head()
  all_se<-enhancer_degree_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_degree_df %>% filter(LA1_type=="typical enhancer")
  wilcox.test(all_se$degree,all_te$degree)
  print(wilcox.test(all_se$degree,all_te$degree))
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_degree_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_degree_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_degree_df %>% filter(cluster_type=="P-SE-TE")
  wilcox.test(pse$degree,pte$degree)
  print(wilcox.test(pse$degree,pte$degree))
  wilcox.test(pse$degree,psete$degree)
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  wilcox.test(psete_se$degree,psete_te$degree)
  print(wilcox.test(psete_se$degree,psete_te$degree))
  
  rel_clu_se<-enhancer_degree_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_degree_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  wilcox.test(rel_clu_se$degree,rel_clu_te$degree)
  print(wilcox.test(rel_clu_se$degree,rel_clu_te$degree))
  
  # normalised degree 
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_degree_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    subgraph_degree <- degree(subgraph, normalized = T)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, degree=subgraph_degree[match(name, V(subgraph)$name)]) %>% select(name,Cluster,degree, LA1_type)
    enhancer_degree_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_degree_df<- bind_rows(enhancer_degree_list, .id = "Cluster")
  enhancer_degree_df$Cluster<-as.character(enhancer_degree_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_degree_df<-enhancer_degree_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_norm_degree<-enhancer_degree_df
  b<-plot_graphs(df = enhancer_degree_df,yvar = degree,title = "normdegree",x_axis = "cluster type",y_axis = "degree")
  # print(b)
  # between all super enhancers and typical enhancers
  enhancer_degree_df %>% head()
  all_se<-enhancer_degree_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_degree_df %>% filter(LA1_type=="typical enhancer")
  wilcox.test(all_se$degree,all_te$degree)
  print(wilcox.test(all_se$degree,all_te$degree))
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_degree_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_degree_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_degree_df %>% filter(cluster_type=="P-SE-TE")
  wilcox.test(pse$degree,pte$degree)
  print(wilcox.test(pse$degree,pte$degree))
  wilcox.test(pse$degree,psete$degree)
  print(wilcox.test(pse$degree,psete$degree))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  wilcox.test(psete_se$degree,psete_te$degree)
  print(wilcox.test(psete_se$degree,psete_te$degree))
  
  rel_clu_se<-enhancer_degree_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_degree_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  wilcox.test(rel_clu_se$degree,rel_clu_te$degree)
  print(wilcox.test(rel_clu_se$degree,rel_clu_te$degree))
  
  # strength 
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_strength_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    subgraph_strength <- strength(subgraph)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, strength=subgraph_strength[match(name, V(subgraph)$name)]) %>% select(name,Cluster,strength, LA1_type)
    enhancer_strength_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_strength_df<- bind_rows(enhancer_strength_list, .id = "Cluster")
  enhancer_strength_df$Cluster<-as.character(enhancer_strength_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_strength_df<-enhancer_strength_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_strength<-enhancer_strength_df
  plot_graphs(df = enhancer_strength_df,yvar = strength,title ="strength",x_axis = "cluster type",y_axis = "strength")
  
  # between all super enhancers and typical enhancers
  enhancer_strength_df %>% head()
  all_se<-enhancer_strength_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_strength_df %>% filter(LA1_type=="typical enhancer")
  wilcox.test(all_se$strength,all_te$strength)
  print(wilcox.test(all_se$strength,all_te$strength))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_strength_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_strength_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_strength_df %>% filter(cluster_type=="P-SE-TE")
  wilcox.test(pse$strength,pte$strength)
  print(wilcox.test(pse$strength,pte$strength))
  wilcox.test(pse$strength,psete$strength)
  print(wilcox.test(pse$strength,psete$strength))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  wilcox.test(psete_se$strength,psete_te$strength)
  print(wilcox.test(psete_se$strength,psete_te$strength))
  
  rel_clu_se<-enhancer_strength_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_strength_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  wilcox.test(rel_clu_se$strength,rel_clu_te$strength)
  print(wilcox.test(rel_clu_se$strength,rel_clu_te$strength))
  
  # closeness 
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_closeness_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_closeness <- closeness(subgraph,weights = E(subgraph)$inv_weight, normalized = F)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, closeness=subgraph_closeness[match(name, V(subgraph)$name)]) %>% select(name,Cluster,closeness, LA1_type)
    enhancer_closeness_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_closeness_df<- bind_rows(enhancer_closeness_list, .id = "Cluster")
  enhancer_closeness_df$Cluster<-as.character(enhancer_closeness_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_closeness_df<-enhancer_closeness_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_closeness<-enhancer_closeness_df
  d<-plot_graphs(df = enhancer_closeness_df,yvar = closeness,title ="closeness",x_axis = "cluster type",y_axis = "closeness")
  # print(d)
  # between all super enhancers and typical enhancers
  enhancer_closeness_df %>% head()
  all_se<-enhancer_closeness_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_closeness_df %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(all_se$closeness,all_te$closeness))
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_closeness_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_closeness_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_closeness_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$closeness,pte$closeness))
  print(wilcox.test(pse$closeness,psete$closeness))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$closeness,psete_te$closeness))
  
  rel_clu_se<-enhancer_closeness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_closeness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$closeness,rel_clu_te$closeness))
  
  # normalised closeness 
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_closeness_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_closeness <- closeness(subgraph,weights = E(subgraph)$inv_weight,normalized = T)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, closeness=subgraph_closeness[match(name, V(subgraph)$name)]) %>% select(name,Cluster,closeness, LA1_type)
    enhancer_closeness_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_closeness_df<- bind_rows(enhancer_closeness_list, .id = "Cluster")
  enhancer_closeness_df$Cluster<-as.character(enhancer_closeness_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_closeness_df<-enhancer_closeness_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_norm_closeness<-enhancer_closeness_df
  e<-plot_graphs(df = enhancer_closeness_df,yvar = closeness,title ="closeness",x_axis = "cluster type",y_axis = "closeness")
  # print(e)
  # between all super enhancers and typical enhancers
  enhancer_closeness_df %>% head()
  all_se<-enhancer_closeness_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_closeness_df %>% filter(LA1_type=="typical enhancer")
  print(wilcox.test(all_se$closeness,all_te$closeness))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_closeness_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_closeness_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_closeness_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$closeness,pte$closeness))
  print(wilcox.test(pse$closeness,psete$closeness))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$closeness,psete_te$closeness))
  
  rel_clu_se<-enhancer_closeness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_closeness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$closeness,rel_clu_te$closeness))
  
  # betweenness
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_betweenness_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_betweenness <- betweenness(subgraph,weights = E(subgraph)$inv_weight,normalized = F)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, betweenness=subgraph_betweenness[match(name, V(subgraph)$name)]) %>% select(name,Cluster,betweenness, LA1_type)
    enhancer_betweenness_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_betweenness_df<- bind_rows(enhancer_betweenness_list, .id = "Cluster")
  enhancer_betweenness_df$Cluster<-as.character(enhancer_betweenness_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_betweenness_df<-enhancer_betweenness_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_betweenness<-enhancer_betweenness_df
  f<-plot_graphs(df = enhancer_betweenness_df,yvar = betweenness,title ="betweenness",x_axis = "cluster type",y_axis = "betweenness")
  # print(f)
  # between all super enhancers and typical enhancers
  enhancer_betweenness_df %>% head()
  all_se<-enhancer_betweenness_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_betweenness_df %>% filter(LA1_type=="typical enhancer")
  print(wilcox.test(all_se$betweenness,all_te$betweenness))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_betweenness_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_betweenness_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_betweenness_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$betweenness,pte$betweenness))
  print(wilcox.test(pse$betweenness,psete$betweenness))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$betweenness,psete_te$betweenness))
  
  rel_clu_se<-enhancer_betweenness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_betweenness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$betweenness,rel_clu_te$betweenness))
  
  
  ##############################################
  # norm betweenness
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_betweenness_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_betweenness <- betweenness(subgraph,weights = E(subgraph)$inv_weight,normalized = T)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, betweenness=subgraph_betweenness[match(name, V(subgraph)$name)]) %>% select(name,Cluster,betweenness, LA1_type)
    enhancer_betweenness_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_betweenness_df<- bind_rows(enhancer_betweenness_list, .id = "Cluster")
  enhancer_betweenness_df$Cluster<-as.character(enhancer_betweenness_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_betweenness_df<-enhancer_betweenness_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_norm_betweenness<-enhancer_betweenness_df
  h<-plot_graphs(df = enhancer_betweenness_df,yvar = betweenness,title ="betweenness",x_axis = "cluster type",y_axis = "betweenness")
  # print(h)
  # between all super enhancers and typical enhancers
  enhancer_betweenness_df %>% head()
  all_se<-enhancer_betweenness_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_betweenness_df %>% filter(LA1_type=="typical enhancer")
  print(wilcox.test(all_se$betweenness,all_te$betweenness))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_betweenness_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_betweenness_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_betweenness_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$betweenness,pte$betweenness))
  print(wilcox.test(pse$betweenness,psete$betweenness))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$betweenness,psete_te$betweenness))
  
  rel_clu_se<-enhancer_betweenness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_betweenness_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$betweenness,rel_clu_te$betweenness))
  
  # harmonic centrality
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_harmonic_centrality_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_harmonic_centrality <- harmonic_centrality(subgraph,weights = E(subgraph)$inv_weight,normalized = F)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, harmonic_centrality=subgraph_harmonic_centrality[match(name, V(subgraph)$name)]) %>% select(name,Cluster,harmonic_centrality, LA1_type)
    enhancer_harmonic_centrality_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_harmonic_centrality_df<- bind_rows(enhancer_harmonic_centrality_list, .id = "Cluster")
  enhancer_harmonic_centrality_df$Cluster<-as.character(enhancer_harmonic_centrality_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_harmonic_centrality_df<-enhancer_harmonic_centrality_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_harmonic_centrality<-enhancer_harmonic_centrality_df
  a<-plot_graphs(df = enhancer_harmonic_centrality_df,yvar = harmonic_centrality,title ="harmonic_centrality",x_axis = "cluster type",y_axis = "harmonic_centrality")
  # print(a)
  # between all super enhancers and typical enhancers
  enhancer_harmonic_centrality_df %>% head()
  all_se<-enhancer_harmonic_centrality_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_harmonic_centrality_df %>% filter(LA1_type=="typical enhancer")
  print(wilcox.test(all_se$harmonic_centrality,all_te$harmonic_centrality))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$harmonic_centrality,pte$harmonic_centrality))
  print(wilcox.test(pse$harmonic_centrality,psete$harmonic_centrality))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$harmonic_centrality,psete_te$harmonic_centrality))
  
  rel_clu_se<-enhancer_harmonic_centrality_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_harmonic_centrality_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$harmonic_centrality,rel_clu_te$harmonic_centrality))
  
  # normalised harmonic centrality
  
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_harmonic_centrality_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_harmonic_centrality <- harmonic_centrality(subgraph,weights = E(subgraph)$inv_weight,normalized = T)
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, harmonic_centrality=subgraph_harmonic_centrality[match(name, V(subgraph)$name)]) %>% select(name,Cluster,harmonic_centrality, LA1_type)
    enhancer_harmonic_centrality_list[[as.character(i)]] <- enhancer_data
  }
  
  enhancer_harmonic_centrality_df<- bind_rows(enhancer_harmonic_centrality_list, .id = "Cluster")
  enhancer_harmonic_centrality_df$Cluster<-as.character(enhancer_harmonic_centrality_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_harmonic_centrality_df<-enhancer_harmonic_centrality_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_norm_harmonic_centrality<-enhancer_harmonic_centrality_df
  a<-plot_graphs(df = enhancer_harmonic_centrality_df,yvar = harmonic_centrality,title ="harmonic_centrality",x_axis = "cluster type",y_axis = "harmonic_centrality")
  # print(a)
  
  # between all super enhancers and typical enhancers
  enhancer_harmonic_centrality_df %>% head()
  all_se<-enhancer_harmonic_centrality_df %>% filter(LA1_type=="super enhancer")
  all_te<-enhancer_harmonic_centrality_df %>% filter(LA1_type=="typical enhancer")
  print(wilcox.test(all_se$harmonic_centrality,all_te$harmonic_centrality))
  
  # between the PSE, PTE and PSETE clusters
  
  pse<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-SE",LA1_type=="super enhancer")
  pte<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-TE",LA1_type=="typical enhancer")
  psete<-enhancer_harmonic_centrality_df %>% filter(cluster_type=="P-SE-TE")
  print(wilcox.test(pse$harmonic_centrality,pte$harmonic_centrality))
  print(wilcox.test(pse$harmonic_centrality,psete$harmonic_centrality))
  
  psete_se <- psete %>% filter(LA1_type=="super enhancer" )
  psete_te <- psete %>% filter(LA1_type=="typical enhancer" )
  print(wilcox.test(psete_se$harmonic_centrality,psete_te$harmonic_centrality))
  
  rel_clu_se<-enhancer_harmonic_centrality_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="super enhancer")
  rel_clu_te<-enhancer_harmonic_centrality_df %>% filter(cluster_type%in%c("P-SE","P-TE","P-SE-TE")) %>% filter(LA1_type=="typical enhancer")
  
  print(wilcox.test(rel_clu_se$harmonic_centrality,rel_clu_te$harmonic_centrality))
  
  return(list(degree=wt0_degree, 
              norm_degree=wt0_norm_degree, 
              closeness=wt0_closeness, 
              norm_closeness=wt0_norm_closeness,
              strength=wt0_strength,
              betweenness=wt0_betweenness,
              norm_betweenness=wt0_betweenness,
              harmonic=wt0_harmonic_centrality,
              norm_harmonic=wt0_norm_harmonic_centrality))
}

a_wt0<-calculate_nw_ppts(g_wt0, components_g_wt0,cluster_classification_wt0)

a_wt360<-calculate_nw_ppts(g_wt360, components_g_wt360,cluster_classification_wt360)

a_wt4320<-calculate_nw_ppts(g_wt4320, components_g_wt4320,cluster_classification_wt4320)



find_eigen_centrality<-function(g, components_g, cluster_classification){
  components_g<-components(g)
  enhancer_data<-data.frame(enhancer=V(g)$name, Cluster=components_g$membership,Type=V(g)$LA1_type) %>% filter(Type%in%c("typical enhancer","super enhancer"))
  enhancer_eigen_centrality_list <- list()
  
  for(i in unique(components_g$membership)){
    subgraph_nodes<-which(components_g$membership==i)
    subgraph <- induced_subgraph(g, vids = subgraph_nodes)
    E(subgraph)$weight<-E(subgraph)$value
    # E(subgraph)$inv_weight<-1/(E(subgraph)$value)
    subgraph_eigen_centrality <- eigen_centrality(subgraph,weights = E(subgraph)$weight)$vector
    enhancer_data <- as_tibble(as_data_frame(subgraph, what = "vertices")) %>% filter(LA1_type %in% c("super enhancer", "typical enhancer")) %>% 
      mutate(Cluster=i, eigen_centrality=subgraph_eigen_centrality[match(name, V(subgraph)$name)]) %>% select(name,Cluster,eigen_centrality, LA1_type)
    enhancer_eigen_centrality_list[[as.character(i)]] <- enhancer_data
  }
  
  
  enhancer_eigen_centrality_df<- bind_rows(enhancer_eigen_centrality_list, .id = "Cluster")
  enhancer_eigen_centrality_df$Cluster<-as.character(enhancer_eigen_centrality_df$Cluster)
  cluster_classification$Cluster<-as.character(cluster_classification$Cluster)
  enhancer_eigen_centrality_df<-enhancer_eigen_centrality_df %>% left_join(cluster_classification,by="Cluster") %>% select(1,2,3,4,7) 
  
  wt0_eigen_centrality<-enhancer_eigen_centrality_df
  plot_graphs(df = enhancer_eigen_centrality_df,yvar = eigen_centrality,title ="eigen_centrality",x_axis = "cluster type",y_axis = "eigen_centrality")
  return(wt0_eigen_centrality)
}

eigen_centrality_wt0<-find_eigen_centrality(g_wt0, components_g_wt0,cluster_classification_wt0)
eigen_centrality_wt360<-find_eigen_centrality(g_wt360, components_g_wt360,cluster_classification_wt360)
eigen_centrality_wt4320<-find_eigen_centrality(g_wt4320, components_g_wt4320,cluster_classification_wt4320)


loops_wt0 %>% nrow()
loops_wt360 %>% nrow()
loops_wt4320 %>% nrow()

components_g_wt0$no
