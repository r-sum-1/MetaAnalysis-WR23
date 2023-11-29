# R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"
# Rstudio Version "2023.06.1+524" - "Mountain Hydrangea"

#remotes::install_github("MathiasHarrer/dmetar")


######    Load packages
# Install missing packages or load the package if already installed    
packages <- c("robumeta", "metafor", 
              "esc", "meta", 
              "ggplot2", "gridExtra", 
              "readxl", "rstudioapi", "clipr", 
              "weightr", "dplyr", "dmetar", "fastDummies")

# Install packages not yet installed

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))



######    Import data ##


setwd(dirname(getSourceEditorContext()$path))

df <- as.data.frame(read_excel("Datasets/Raw_Datasets/data amended.xlsx", sheet = "data_updated"))

# Define our variables of interest for subgrouping...
# Gendered_Pathways_pathway is 
subgroup_name <- c("Gendered_Groups", "Gendered Assessment", "Gendered Pathways", "Gendered Intervention", "Gendered Context", "Casework",
                   "RNR GN", "focus", "Pathway", "Therapeutic", "location_therapy", "pathway_gendered", "study_length_cat", "location_length",
                   "approach", "Female Only", "Gendered_Pathways_pathway", "focus_location")



# =============== Some Housekeeping ====================
# select only char cols and convert them to title case using sapply...
# df[, sapply(df, class) == 'character'] locates all of the str cols
# sapply(x, FUN) converts them to sentence case
df[, sapply(df, class) == 'character'] <- sapply(df[, sapply(df, class) == 'character'], stringr::str_to_title)

df$Casework <- gsub("Probation", "Yes", df$Casework)

# =============== Some Housekeeping ====================




df$study_length_cat <- df %>% 
  mutate(length = case_when(Span_Months < 7 ~ "0-6 Months",
                            Span_Months > 6 & Span_Months < 13   ~ "7-12 Months",
                            Span_Months > 12   ~ "12+ Months",
                            TRUE ~ "Other")) %>%
  .$length

# calculate the degree to which the study is gender-responsive.. just count
df$gscore <- df %>% 
  mutate(GA = if_else(`Gendered Assessment` == "Y", 1, 0),
         GP = if_else(`Gendered Pathways` == "Y", 1, 0),
         GI = if_else(`Gendered Intervention` == "Y", 1, 0),
         GC = if_else(`Gendered Context` == "Y", 1, 0)) %>% 
  mutate(G_Score = GA+GP+GI+GC) %>% .$G_Score



# now create a group for therapeutic/not for community/prison.
df$location_therapy <- df %>% 
  mutate(loc_ther = case_when(Therapeutic == "Therapeutic" & location == "Institution" ~ "Prison, Therapeutic",
                              Therapeutic == "Not Therapeutic" & location == "Institution" ~ "Prison, Not Therapeutic",
                              Therapeutic == "Therapeutic" & location == "Community" ~ "Community, Therapeutic",
                              Therapeutic == "Not Therapeutic" & location == "Community" ~ "Community, Not Therapeutic",
                              Therapeutic == "Therapeutic" & location == "Both" ~ "Both, Therapeutic",
                              Therapeutic == "Not Therapeutic" & location == "Both" ~ "Both, Not Therapeutic",
                              TRUE ~ "Other")) %>%
  .$loc_ther


# now create a group for community/institution/gendered... just combine the columns!
df$pathway_gendered <- paste(df$Gendered_Groups, df$Pathway)
df$location_gendered <- paste(df$Gendered_Groups, df$location)

df$location_length <- paste(df$study_length_cat, df$location)
df$Gendered_Pathways_pathway <- paste(df$`Gendered Pathways`, df$Pathway)

df$focus_location <- paste(df$location, df$focus)



# combine studies together where there are multiple for each - 
# 1 str_split in the " _" to separate it out... we only want the first bit.

stringr::str_split_fixed(df$author,  ' -', 2)[,1]


# --------------------------------------------------------
df[, c("pre.t", "pre.c", "post.tp", "post.cp")] <- sapply(df[, c("pre.t", "pre.c", "post.tp", "post.cp")], as.numeric)
df[, c("pre.t", "pre.c", "post.tp", "post.cp")] <- round(df[, c("pre.t", "pre.c", "post.tp", "post.cp")], 0)


# pre.t     number of subjects in treatment group
# pre.c     number of subjects in control group

# post.tp   number of subjects treatment group that offended
# post.cp   number of subjects in control group that offended

# post.tn   number of subjects in treatment group that didn't reoffend
# post.cn   number of subjects in control group that didn't reoffend

##################################
######     Performing the Meta-analysis   ######


#First, calculate post negative events
df$post.cn <- df$pre.c - df$post.cp 
df$post.tn <- df$pre.t - df$post.tp


#filter for empty
#filter filter
df <- df %>% filter(!is.na(pre.t), !is.na(pre.c), !is.na(post.tp), !is.na(post.cp))



# Assign to char 
df$Filter_Arrest <- as.character(df$Filter_Arrest)
df$Filter1 <- as.character(df$Filter1)
df$Filter_New_C <- as.character(df$Filter_New_C)
df$Filter_RTJ <- as.character(df$Filter_RTJ)

#assign to factors
df$focus <- as.factor(df$focus)
df$location <- as.factor(df$location)




##################################
#loop through each row and calculate odds ratio

for(i in 1:nrow(df)) {       # for-loop over rows
  # calculate odds ratio
  or<- esc_2x2(grp1yes = df[i, "post.tp"],
               grp1no =  df[i, "post.tn"],
               grp2yes = df[i, "post.cp"],
               grp2no =  df[i, "post.cn"],
               estype = "or")
  
  df[i, "es"] <- or$es
  df[i, "se"] <- or$se
  df[i, "var"] <- or$var
  df[i, "ci.lo"] <- or$ci.lo
  df[i, "ci.hi"] <- or$ci.hi
  df[i, "w"] <- or$w
  
  
}



# This is a botch
df$odds_treat <- (df$post.tp/ df$pre.t)  / (1 - df$post.tp/ df$pre.t)
df$odds_contr <- (df$post.cp/ df$pre.c)  / (1 - df$post.cp/ df$pre.c)

t.test(df$odds_treat, conf.level = 0.95)

t.test(df$odds_contr, conf.level = 0.95)

# Now filter for the studies we are grouping
# =====================================
df_filt <- df %>% filter(Filter1 == "1")

# --------------------------------------------
# --------------------------------------------
# now that we've filtered... loop through the variables of interest for subgroups... 
# Any group with < 5 studies, label as "other"

for (subgroup in subgroup_name){
  
  lessthan5 <- as.data.frame(table(df_filt[[subgroup]]))
  lessthan5$Var1 <- as.character(lessthan5$Var1)
  lessthan5 <- lessthan5[lessthan5$Freq < 5, "Var1"]
  
  for (x in lessthan5){
    df_filt[[subgroup]] <- gsub(x, "Other", df_filt[[subgroup]])
  }
}


##################################
## Pool effect size
m.bin <- metabin(event.e = post.tp,
                 n.e = pre.t,
                 event.c = post.cp,
                 n.c = pre.c,
                 studlab = author,
                 data = df_filt,
                 sm = "OR",
                 method = "MH",
                 MH.exact = FALSE,
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "PM",
                 prediction = TRUE,
                 title = "")





order <- data.frame(m.bin$studlab, exp(m.bin$TE))

order <- order %>% arrange(desc(exp.m.bin.TE.))

m.bin$studlab


summary(m.bin)


# look if the method used to estimate ??^2 has an impact on the results.
# Using the update.meta function, we re-run the analysis,
# but use the restricted maximum likelihood estimator this time.

m.bin_update <- update.meta(m.bin, method.tau = "REML")

exp(m.bin_update$TE.random)
m.bin_update$tau2


m.bin_or <- update.meta(m.bin, sm = "OR")
m.bin_or


##################################
## Between Study Heterogeneity
m.gen <- update.meta(m.bin_or, prediction = TRUE)

m.gen_sum <- summary(m.gen)
m.gen





########## FROM HERE YOU CAN JUMP STRAIGHT TO LOOPING


# Influence
m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)

jpeg("Outputs/baujat.jpg", units="in", width=7, height=5, res=300)

plot(m.gen.inf, "baujat", xmin=20, ymin=10, cex.studlab=10, studlab=TRUE) 

dev.off()


plot(m.gen.inf, "influence")

jpeg("Outputs/leave_one_out.jpg", units="in", width=12, height=15, res=300)
plot(m.gen.inf, "es")
dev.off()


plot(m.gen.inf, "i2")



plot(m.gen.inf, "i2")

##################################
# Funnel Plot
# Define fill colors for contour

jpeg("Outputs/funnel.jpg", width=5*300, height=7*300, res=300)

col.contour = c("gray75", "gray85", "gray95")

# Generate funnel plot (we do not include study labels here)
funnel.meta(m.gen, 
            #xlim = c(-0.5, 2),
            contour = c(0.9, 0.95, 0.99),
            col.contour = col.contour, cex = 1
)
# Add a legend
legend(x = 3.7, y = -0.05,cex = 0.7, 
       
       legend = c("p < 0.1", "p < 0.05", "p < 0.01"),border = FALSE,
       fill = col.contour)

# Add a title
title("Contour-Enhanced Funnel Plot")
dev.off()

# Egger's Regression Test
print("Eggers Test") 
eggs <- eggers.test(m.gen)

summary(eggs)

#################### TEST FOR ACTUAL PUBLICATION BIAS

weightfunct(effect = df_filt$es,v = df_filt$var)

##################################
#  Forest Plots

save_filename <- paste0("Outputs/MAINforest_plot.jpg") 

height <- 2 + length(m.gen$event.e) * 0.25

height <- height * 300

jpeg(save_filename, width=12*300, height=height, res=300)

forest <- forest.meta(m.gen,
                      sortvar = TE,
                      fontsize = 7,
                      fixed = TRUE,
                      print.tau2 = TRUE,
                      print.I2 = TRUE,
                      print.Q = TRUE,
                      prediction = TRUE,
                      rightcols = c("effect", "ci", "w.fixed","w.random", "focus", "Gendered", "location"),
                      rightlabs = c("effect", "ci", "w.fixed","w.random", "Study Focus", "Gendered", "Location"),
                      leftcols = c("studlab", "n.e", "n.c"),
                      colgap.left = unit(1, "cm"),
                      just.addcols.right = "right")



dev.off()


# # IMPORTANT FOR SUPGROU ANALYSIS PLOT.... HAS SOME ADDITIONAL OPTIONS...
# #filter where other < 5 studies...
# subgroup <- "pathway_gendered"
# 
# lessthan5 <- as.data.frame(table(df_filt[[subgroup]]))
# lessthan5$Var1 <- as.character(lessthan5$Var1)
# if ("Other" %in% c(lessthan5$Var1))
#   if (lessthan5[lessthan5$Var1 == "Other", "Freq"] < 5){
#     df_filt <- df_filt %>% filter(!!sym(subgroup) != "Other")
#   }
# # ----------------------------------------------------------------
# #filter where not other
# lessthan5 <- as.data.frame(table(df_filt[[subgroup]]))
# lessthan5$Var1 <- as.character(lessthan5$Var1)
# if ("Other" %in% c(lessthan5$Var1)){
#   df_filt <- df_filt %>% filter(!!sym(subgroup) != "Other")
# }
# 
# 
# sub_g <- metabin(event.e = post.tp,
#                  n.e = pre.t,
#                  event.c = post.cp,
#                  n.c = pre.c,
#                  studlab = author,
#                  data = df_filt,
#                  sm = "OR",
#                  method = "MH",
#                  MH.exact = FALSE,
#                  fixed = TRUE,
#                  random = TRUE,
#                  method.tau = "PM",
#                  hakn = TRUE,
#                  title = "")
# 
# 
# sub_g <- update.meta(m.bin_g, subgroup = pathway_gendered)
# sub_g
# 
# forest.meta(sub_g,
#             sortvar = TE,
#             fontsize = 7,
#             study.results = FALSE,
#             test.subgroup = TRUE,
#             fixed = FALSE,
#             common = FALSE,
#             print.tau2 = TRUE,
#             print.I2 = TRUE,
#             print.Q = TRUE,
#             prediction = TRUE,
#             rightcols = c("effect", "ci", "w.fixed","w.random", "focus", "Gendered_Groups", "Pathway"),
#             rightlabs = c("effect", "ci", "w.fixed","w.random", "Study Focus", "Gendered", "Pathway"),
#             just.addcols.right = "right"
# )
# 
# 


# --------------------------------------------
# Create subgroups and loop thgrough the analysis... create the output table first with predefined columns...
columns <- c("SMS", "groups_col", "subgroups" , "k", "n.e", "n.c", "TE_fixed", "TE_ci_fixed", "TE_rand", "TE_ci_rand", "I2", "I2_ci", "Q")
out_table <- data.frame(matrix(ncol = length(columns), nrow = 0))
out_outliers <- list()
df_filt_master <- df_filt


for (sms in c("All", "Higher Quality")){
  
  for (subgroup in subgroup_name){

    #restore master filt
    if (sms == "Higher Quality") {
      df_filt <- df_filt %>% filter(SMS > 3)}
    
    # ----------------------------------------------------------------    
    #filter where other < 5 studies...
    lessthan5 <- as.data.frame(table(df_filt[[subgroup]]))
    lessthan5$Var1 <- as.character(lessthan5$Var1)
    
    lessthan5 <- lessthan5[lessthan5$Freq > 4,]
    
    lessthan5 <- unlist(as.list(lessthan5$Var1))
    
    if(is.null(lessthan5)) 
      next # skip iteration
    
    df_filt <- df_filt %>% filter(!!sym(subgroup) %in% lessthan5)
    
    
    
    # ----------------------------------------------------------------
    #filter where not other
    lessthan5 <- as.data.frame(table(df_filt[[subgroup]]))
    lessthan5$Var1 <- as.character(lessthan5$Var1)
    if ("Other" %in% c(lessthan5$Var1)){
      df_filt <- df_filt %>% filter(!!sym(subgroup) != "Other")
    }
    # ----------------------------------------------------------------
    
    if (subgroup == "GOBEIL_GEND_HQS_COMP") {
      df_filt <- df %>% filter(!is.na(GOBEIL_GEND_HQS_COMP))
      
      
      m.bin_g <- metabin(event.e = post.tp,
                         n.e = pre.t,
                         event.c = post.cp,
                         n.c = pre.c,
                         studlab = author,
                         data = df_filt,
                         sm = "OR",
                         method = "MH",
                         MH.exact = FALSE,
                         fixed = TRUE,
                         random = TRUE,
                         method.tau = "PM",
                         title = "")
    } else {
      #
      ## Pool effect size
      m.bin_g <- metabin(event.e = post.tp,
                         n.e = pre.t,
                         event.c = post.cp,
                         n.c = pre.c,
                         studlab = author,
                         data = df_filt,
                         sm = "OR",
                         method = "MH",
                         MH.exact = FALSE,
                         fixed = TRUE,
                         random = TRUE,
                         method.tau = "PM",
                         hakn = TRUE,
                         title = "")
      
    }
    
    # a mess of a conditional statement, the update.meta function has to be declared manually...
    if (subgroup == "location"){sub_g <- update.meta(m.bin_g, subgroup = location)
    } else if (subgroup == "Gendered_Groups"){sub_g <- update.meta(m.bin_g, subgroup = Gendered_Groups)
    } else if (subgroup == "focus"){sub_g <- update.meta(m.bin_g, subgroup = focus)
    } else if (subgroup == "Casework"){sub_g <- update.meta(m.bin_g, subgroup = Casework)
    } else if (subgroup == "Therapeutic"){sub_g <- update.meta(m.bin_g, subgroup = Therapeutic)
    } else if (subgroup == "Gendered Assessment"){sub_g <- update.meta(m.bin_g, subgroup = `Gendered Assessment`)
    } else if (subgroup == "Gendered Pathways"){sub_g <- update.meta(m.bin_g, subgroup = `Gendered Pathways`)
    } else if (subgroup == "Gendered Intervention"){sub_g <- update.meta(m.bin_g, subgroup = `Gendered Intervention`)
    } else if (subgroup == "RNR GN"){sub_g <- update.meta(m.bin_g, subgroup = `RNR_GN`)
    } else if (subgroup == "Pathway"){sub_g <- update.meta(m.bin_g, subgroup = `Pathway`)
    } else if (subgroup == "location_therapy"){sub_g <- update.meta(m.bin_g, subgroup = location_therapy)
    } else if (subgroup == "pathway_gendered"){sub_g <- update.meta(m.bin_g, subgroup = pathway_gendered)
    } else if (subgroup == "location_gendered"){sub_g <- update.meta(m.bin_g, subgroup = location_gendered)
    } else if (subgroup == "GOBEIL_GEND_HQS_COMP"){sub_g <- update.meta(m.bin_g, subgroup = GOBEIL_GEND_HQS_COMP)
    } else if (subgroup == "study_length_cat"){sub_g <- update.meta(m.bin_g, subgroup = study_length_cat)
    } else if (subgroup == "approach"){sub_g <- update.meta(m.bin_g, subgroup = approach)
    } else if (subgroup == "Female Only"){sub_g <- update.meta(m.bin_g, subgroup = `Female Only`)
    } else if (subgroup == "location_length"){sub_g <- update.meta(m.bin_g, subgroup = `location_length`)
    } else if (subgroup == "Gendered_Pathways_pathway"){sub_g <- update.meta(m.bin_g, subgroup = `Gendered_Pathways_pathway`)
    } else if (subgroup == "focus_location"){sub_g <- update.meta(m.bin_g, subgroup = focus_location)
    }
    
    #identify outliers
    # so far, no outliers in any of the groupings...
    # conditional TODO... 
    outliers <- NULL
    
    tryCatch({outliers <- find.outliers(sub_g)
      }, error=function(e){cat("\n No outliers - ", subgroup)})

    if (!is.null(outliers)){out_outliers <- c(out_outliers, outliers)}
    
    
    filename <- paste0("Outputs/subg_forest/", sms, " ", subgroup, ".jpg")
    
    jpeg(filename, width=1950, height=1350, res=150)
    forest.meta(sub_g,
                sortvar = TE,
                fontsize = 7,
                study.results = FALSE,
                test.subgroup = TRUE,
                fixed = FALSE,
                common = FALSE,
                print.tau2 = TRUE,
                print.I2 = TRUE,
                print.Q = TRUE,
                prediction = TRUE,
                rightcols = c("effect", "ci", "w.fixed","w.random", "focus", "Gendered_Groups", "Pathway"),
                rightlabs = c("effect", "ci", "w.fixed","w.random", "Study Focus", "Gendered", "Pathway"),
                just.addcols.right = "right")
    
    dev.off()
    
    # create the table from analysis... we will extract from the subgroup update... 
    # The object is still the log of the TE, so we need to exp it...
    # extract various bits and pieces
    
    k <- sub_g$k.all.w
    row.names <- names(k)
    
    TE_fixed <- round(exp(sub_g$TE.common.w), 2)
    TE_ci_fixed <- paste0("[", round(exp(sub_g$lower.common.w), 2), ";", round(exp(sub_g$upper.common.w), 2), "]")
    
    TE_rand <- round(exp(sub_g$TE.random.w), 2)
    TE_ci_rand <- paste0("[", round(exp(sub_g$lower.random.w), 2), ";", round(exp(sub_g$upper.random.w), 2), "]")
    
    
    
    n.e <- sub_g$n.e.w
    n.c <- sub_g$n.c.w
    
    Q <- round(sub_g$Q.w, 2)
    Q.pval <- round(sub_g$pval.Q.w, 2)
    
    I2 <- round(sub_g$I2.w, 3)
    I2_ci <- paste0("[", round(sub_g$lower.I2.w, 2), ";", round(sub_g$upper.I2.w, 2), "]")
    
    Q.between <- round(sub_g$Q.b.random, 2)
    Q.between.pval <- sub_g$pval.Q.b.random
    
    
    table <- as.data.frame(cbind(k, n.e, n.c, TE_fixed, TE_ci_fixed, TE_rand, TE_ci_rand, I2, I2_ci, Q, Q.pval))
    
    #insert new row, and then insert the "between" stats
    table[nrow(table)+1,] <- NA
    
    #change rowname
    rownames(table)[nrow(table)] <- "Between"
    
    table[nrow(table), 10] <- Q.between
    table[nrow(table), 11] <- Q.between.pval
    
    
    table$Q.pval <- as.double(table$Q.pval)
    
    first_row <- c(sub_g$k.all, sum(sub_g$n.e), sum(sub_g$n.c), round(exp(sub_g$TE.fixed), 2), 
                   paste0("[", round(exp(sub_g$lower.common), 2),"; ",  round(exp(sub_g$upper.common), 2), "]"),
                   round(exp(sub_g$TE.random), 2),
                   paste0("[", round(exp(sub_g$lower.random), 2),"; ",  round(exp(sub_g$upper.random), 2), "]"),
                   round(sub_g$I2, 2),
                   paste0("[", round(sub_g$lower.I2, 2),"; ",  round(sub_g$upper.I2, 2), "]"),
                   round(sub_g$Q, 2), sub_g$pval.Q)
    
    table <- rbind(first_row, table)
    
    table <- table %>% mutate(sig = case_when(Q.pval < 0.001 ~ "***",
                                              Q.pval < 0.01 ~ "**",
                                              Q.pval < 0.05 ~ "*"))
    
    
    table$Q <- paste0(table$Q, table$sig)
    
    table$sig <- NULL
    table$Q.pval <- NULL
    
    subgroups <-rownames(table)
    
    rownames(table) <- NULL
    
    table
    
    table <- cbind(subgroups, table)
    
    
    table[1,1] <- "All"
    
    table <- as.data.frame(sapply(table, gsub, pattern = "NA", replacement = "", fixed = TRUE))
    
    
    # ===================================
    emptyrow <- head(table, 1)
    emptyrow[1,] <- NA
    
    table <- rbind(emptyrow, table)
    
    
    #insert column with just one 
    groups_col <- c(subgroup, rep("", nrow(table)-1))
    filt_col <- c(sms, rep("", nrow(table)-1))
    
    table <- cbind(groups_col, table)
    table <- cbind(filt_col, table)
    
    # ===================================
    out_table <- rbind(out_table, table)
    
    # RESTORE THE df_filt for further analysis
    df_filt <- df_filt_master
  }
}


# fuck it... null the Q for the individual groups...
# 
#out_table[out_table$subgroups != "Between" & !is.na(out_table$subgroups), "Q"] <- ""
#out_table[is.na(out_table)] <- ""

clipr::write_clip(out_table)

write.csv(out_table, "Outputs/Tables/out_table.csv")


# ===================================
# Some regressions
library(PerformanceAnalytics)


df_reg <- df_filt


df_reg$Gendered_Groups <- as.character(df_reg$Gendered_Groups)


df_reg$Gendered_Groups <- relevel(factor(df_reg$Gendered_Groups), ref = "Neutral")
df_reg$Pathway <-  relevel(factor(df_reg$Pathway), ref = "Community")
df_reg$SMS <-  relevel(factor(df_reg$SMS), ref = "3")


df_reg <- df_reg %>% filter(!is.infinite(es))

df_reg$Gendered_Groups <- as.factor(df_reg$Gendered_Groups)

df_reg$length_followup_months <- as.numeric(df_reg$length_followup_months)


# ===================================

library(MVN)
ass_test <- df_reg[,c("Gendered_Groups", "Pathway", "es")]

mvn(mvnTest = "mardia", data=ass_test)

reg1 <- rma(yi = es, 
            sei = se, 
            data = df_reg, 
            method = "ML",
            mods = ~ Gendered_Groups)

reg2 <- rma(yi = es, 
            sei = se, 
            data = df_reg, 
            method = "ML",
            mods = ~ Gendered_Groups + Pathway)

reg3 <- rma(yi = es, 
            sei = se, 
            data = df_reg, 
            method = "ML",
            mods = ~ Gendered_Groups + Pathway  + length_followup_months)

reg3a <- rma(yi = es, 
             sei = se, 
             data = df_reg, 
             method = "ML",
             mods = ~ Gendered_Groups + Pathway  + Outcome)



reg4 <- rma(yi = es, 
            sei = se, 
            data = df_reg, 
            method = "ML",
            mods = ~ Gendered_Groups + Pathway  + length_followup_months + Outcome)

reg4 <- rma(yi = es, 
            sei = se, 
            data = df_reg, 
            method = "ML",
            mods = ~ Gendered_Groups + Pathway  + length_followup_months + Outcome + SMS)




summary(reg4)

reg <- reg4

# extract the table and rebuild in APA format....
reg_table <- data.frame(b = round(reg$b, 3), 
                        se = round(reg$se, 3), 
                        zval = round(reg$zval, 3), 
                        pval = round(reg$pval, 3), 
                        ci.lb = round(reg$ci.lb, 3), 
                        ci.ub = round(reg$ci.ub, 3))

reg_table <- round(reg_table, 2)

reg_table <- reg_table %>% mutate(sig = case_when( pval < 0.001 ~ "***",
                                                   pval < 0.01 ~ "**",
                                                   pval < 0.05 ~ "*",
                                                   pval < 0.1 ~ ".",
                                                   TRUE ~ ""))



write.csv(reg_table, "Outputs/Tables/reg_gender.csv")

clipr::write_clip(reg_table)

# ===================================
#focus of studies - meta-regressions


focus_reg <- rma(yi = es, 
                 sei = se, 
                 data = df_reg, 
                 method = "ML",
                 mods = ~   focus + Pathway + length_followup_months + Outcome + SMS)

focus_reg <- rma(yi = es, 
                 sei = se, 
                 data = df_reg, 
                 method = "ML",
                 mods = ~ Gendered_Groups + focus  + length_followup_months + Outcome + SMS)


# extract the table and rebuild in APA format....
reg_table1 <- data.frame(b = round(focus_reg$b, 3), 
                         se = round(focus_reg$se, 3), 
                         zval = round(focus_reg$zval, 3), 
                         pval = round(focus_reg$pval, 3), 
                         ci.lb = round(focus_reg$ci.lb, 3), 
                         ci.ub = round(focus_reg$ci.ub, 3))


reg_table1 <- round(reg_table1, 2)

reg_table1 <- reg_table1 %>% mutate(sig = case_when( pval < 0.001 ~ "***",
                                                     pval < 0.01 ~ "**",
                                                     pval < 0.05 ~ "*",
                                                     pval < 0.1 ~ ".",
                                                     TRUE ~ ""))

write.csv(reg_table, "Outputs/Tables/reg_focus.csv")



