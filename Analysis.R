# R version 4.2.1 (2022-06-23 ucrt) -- "Funny-Looking Kid"


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


df_full <- read_excel("Datasets/Raw_Datasets/data amended.xlsx", sheet="data")

# some column housekeeping
df_full[, c("pre.t", "pre.c", "post.tp", "post.cp")] <- sapply(df_full[, c("pre.t", "pre.c", "post.tp", "post.cp")], as.numeric)
df_full[, c("pre.t", "pre.c", "post.tp", "post.cp")] <- round(df_full[, c("pre.t", "pre.c", "post.tp", "post.cp")], 0)


#### Make sure tables are necessarily
df_full$Filter_Arrest <- as.character(df_full$Filter_Arrest)
df_full$Filter1 <- as.character(df_full$Filter1)
df_full$Filter_New_C <- as.character(df_full$Filter_New_C)
df_full$Filter_RTJ <- as.character(df_full$Filter_RTJ)

# length of followup to int
df_full$length_followup_months <- as.numeric(df_full$length_followup_months)
df_full$Span_Months <- as.numeric(df_full$Span_Months)

#assign to factors
df_full$focus_Simon <- as.factor(df_full$focus_Simon)
df_full$location_Simon <- as.factor(df_full$location_Simon)



##################################
######     do necessary calculations


#First, calculate post negative events to calculate esc
df_full$post.cn <- df_full$pre.c - df_full$post.cp 
df_full$post.tn <- df_full$pre.t - df_full$post.tp

#filter for empty
#filter filter
df_full <- df_full %>% filter(!is.na(pre.t), !is.na(pre.c), !is.na(post.tp), !is.na(post.cp))

#loop through each row and calculate odds ratio

for(i in 1:nrow(df_full)) {       # for-loop over rows
  # calculate odds ratio
  or<- esc_2x2(grp1yes = df_full[i, "post.tp"],
               grp1no =  df_full[i, "post.tn"],
               grp2yes = df_full[i, "post.cp"],
               grp2no =  df_full[i, "post.cn"],
               estype = "or")
  
  df_full[i, "es"] <- or$es
  df_full[i, "se"] <- or$se
  df_full[i, "var"] <- or$var
  df_full[i, "ci.lo"] <- or$ci.lo
  df_full[i, "ci.hi"] <- or$ci.hi
  df_full[i, "w"] <- or$w
}

# create the inverse square weights
df_full$weight <- 1/df_full$se^2


# bins for ages and lengths of study
round(quantile(df_full$MeanAge, probs=c(0,.50,1),na.rm=TRUE))

df_full <- df_full %>% mutate_at("MeanAge", as.numeric) %>% 
  mutate(age_cat = ifelse(MeanAge < 34, "Less Than 34",
                          ifelse(MeanAge >= 34, "34 and Older", NaN)))


df_full <- df_full %>% mutate(studylength_cat = ifelse(Span_Months <= 6, "Less than 6 Months",
                                                       ifelse(Span_Months > 6, "More than 6 Months",NaN)))


df_full <- df_full %>% mutate(studylength_cat2 = ifelse(Span_Months < 12, "Less than 12 Months",
                                                        ifelse(Span_Months >= 12, "More than 12 Months",NaN)))

####################################################################
#do first bit of analysis with similar study characteristics
df_filt <- df_full %>% filter(Filter1 == "1")

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
                 MH.exact = TRUE,
                 fixed = TRUE,
                 random = TRUE,
                 method.tau = "PM",
                 hakn = TRUE,
                 title = "")

summary(m.bin)


# look if the method used to estimate ??^2 has an impact on the results.
# Using the update.meta function, we re-run the analysis,
# but use the restricted maximum likelihood estimator this time.

m.bin_update <- update.meta(m.bin, method.tau = "REML")

exp(m.bin_update$TE.random)
m.bin_update$tau2


m.bin_or <- update.meta(m.bin, sm = "OR")



##################################
## Between Study Heterogeneity
m.gen <- update.meta(m.bin_or, prediction = TRUE)

m.gen_sum <- summary(m.gen)

##################################
# Any outliers?


outliers <- dmetar::find.outliers(m.gen)


########## FROM HERE YOU CAN JUMP STRAIGHT TO LOOPING


# Influence
m.gen.inf <- InfluenceAnalysis(m.bin, random = TRUE)

jpg("baujat.jpg", units="in", width=7, height=5, res=300)

plot(m.gen.inf, "baujat", xmin=20, ymin=10, cex.studlab=10, studlab=TRUE) 

dev.off()


plot(m.gen.inf, "influence")

jpg("leave_one_out.jpg", units="in", width=12, height=15, res=300)
plot(m.gen.inf, "es")
dev.off()


plot(m.gen.inf, "i2")



##################################
# Funnel Plot
# Define fill colors for contour
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

# Egger's Regression Test
print("Eggers Test") 
eggs <- eggers.test(m.gen)

summary(eggs)

#################### TEST FOR ACTUAL PUBLICATION BIAS

weightfunct(effect = df_filt$es,v = df_filt$var)

##################################
#  Forest Plots



save_filename <- paste0("MAINforest_plot.jpg") 

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



####################################################################
####################################################################

###################     Looping    #################################

####################################################################
####################################################################

#create duplicate
refresh_df <- df_full


mylist <- c()
final_out <- data.frame()
final_appended <- data.frame()

dat_out <- data.frame()


for (filter in c("all", "arrest", "new_conv", "rtj", "SMS", "maxES" )){
  print(filter)
  df_full <- refresh_df
  
  for (col in c("location_Simon", "Pathway1", "Gendered", "focus_Simon", "approach", "Therapeutic",  "age_cat", "studylength_cat", "studylength_cat2", "Diversion")){
    
    if (col=="Diversion"){ 
      print("Div!!!")
      if (filter == "max_ES") {df_filt <- df_full %>% group_by(author) %>% filter(es == max(es))}
      else if (filter == "all") {df_filt <- df_full %>% filter(Alt_Filt == "1")}
      else if (filter == "arrest"){df_filt <- df_full %>% filter(Filter_Arrest == "1")}
      else if (filter == "new_conv") {df_filt <- df_full %>% filter(Filter_New_C == "1")}
      else if (filter == "rtj") {df_filt <- df_full %>% filter(Filter_RTJ == "1")}
      else if (filter == "SMS") {
        #firstly, we want to apply that filter to only select the 
        df_filt <- df_full %>% filter(Alt_Filt == "1")
        df_filt <- df_filt %>% filter(SMS > 3)}
      
      else if (filter == "outliers") {
        #firstly, we want to apply that filter to only select the 
        df_filt <- df_full %>% filter(Alt_Filt == "1")
        # everything without outliers...
        outliers_list <- outliers$out.study.random
        df_filt <- df_filt[!df_filt$author %in% outliers_list ,]
      }
      
    } 
    #----------------------------------
    if (col!="Diversion"){ 
      if (filter == "max_ES") {df_filt <- df_full %>% group_by(author) %>% filter(es == max(es))}
      else if (filter == "all") {df_filt <- df_full %>% filter(Filter1 == "1")}
      else if (filter == "arrest"){df_filt <- df_full %>% filter(Filter_Arrest == "1")}
      else if (filter == "new_conv") {df_filt <- df_full %>% filter(Filter_New_C == "1")}
      else if (filter == "rtj") {df_filt <- df_full %>% filter(Filter_RTJ == "1")}
      else if (filter == "SMS") {
        #firstly, we want to apply that filter to only select the 
        df_filt <- df_full %>% filter(Filter1 == "1")
        df_filt <- df_filt %>% filter(SMS > 3)}
      
      else if (filter == "outliers") {
        
        #firstly, we want to apply that filter to only select the 
        df_filt <- df_full %>% filter(Filter1 == "1")
        
        # everything without outliers...
        outliers_list <- outliers$out.study.random
        
        df_filt <- df_filt[!df_filt$author %in% outliers_list ,]
      }
    }
    
    
    if (col=="age_cat"){df_filt <- df_filt %>% filter(!is.na(age_cat))}
    else if (col=="studylength_cat"){df_filt <- df_filt %>% filter(!is.na(studylength_cat))}
    else if (col=="studylength_cat2"){df_filt <- df_filt %>% filter(!is.na(studylength_cat2))}
    
    ######################
    #we want subgroup plots per col and per filter... here goes
    # we want to do these plots before filtering... 
    ######################
    
    
    
    m.bin <- metabin(event.e = post.tp,
                     n.e = pre.t,
                     event.c = post.cp,
                     n.c = pre.c,
                     studlab = author,
                     data = df_filt,
                     sm = "OR",
                     method = "MH",
                     MH.exact = TRUE,
                     fixed = TRUE,
                     random = TRUE,
                     method.tau = "SJ",
                     hakn = TRUE,
                     title = "")
    
    save_filename <- paste0("Outputs/subgroup_forest/",filter, "_", col, ".jpg") 
    
    height <- 20* 300
    
    #use some fuckery to paste in the thing as a string!!!
    eval(parse(text=paste("sub_g <- update.meta(m.bin, subgroup = ", col, ")",sep="")))
    
    #pixels = inches*dpi 
    jpeg(save_filename, width=10.5*300, height=height, res=300)
    
    forest.meta(sub_g,
                sortvar = TE,
                fontsize = 7,
                fixed = TRUE,
                print.tau2 = TRUE,
                print.I2 = TRUE,
                print.Q = TRUE,
                subgroup.name = col,
                rightcols = c("effect", "ci", "w.fixed","w.random", "focus_Simon", "Gendered", "location_Simon"),
                rightlabs = c("effect", "ci", "w.fixed","w.random", "Study Focus", "Gendered", "Location"),
                just.addcols.right = "right")
    
    
    
    dev.off()
    
    
    df_filt <- as.data.frame(df_filt)
    unique_list <- unique(df_filt[,col])
    print(col)
    
    for (col_item in unique_list){
      if (!is.na(col_item)){
        
        df_filtfilt <- df_filt
        
        df_filtfilt <- df_filtfilt %>% filter((!!as.symbol(col)) == col_item)
        df_filtfilt <- as.data.frame(df_filtfilt)
        
        
        ##################### Begin Subgroup Analysis 
        out_df <- list(df_filtfilt)
        mylist <- append(mylist, out_df)
        
        m.bin <- metabin(event.e = post.tp,
                         n.e = pre.t,
                         event.c = post.cp,
                         n.c = pre.c,
                         studlab = author,
                         data = df_filtfilt,
                         sm = "OR",
                         method = "MH",
                         MH.exact = TRUE,
                         fixed = TRUE,
                         random = TRUE,
                         method.tau = "SJ",
                         hakn = TRUE,
                         title = "")
        
        m.bin_or <- update.meta(m.bin, sm = "OR")
        m.bin_or <- update.meta(m.bin, prediction = TRUE)
        
        save_filename <- paste0("Outputs/plots/",filter, "_", col, "_", col_item, ".jpg") 
        
        if (grepl("<", save_filename)) {save_filename <-  gsub("<", "less_than", save_filename)}
        if (grepl(">|+", save_filename)) {save_filename <-  gsub("<+", "more_than", save_filename)}
        
        print(save_filename)
        
        height <- 2 + length(m.bin_or$event.e) * 0.25
        height <- height *300
        
        jpeg(save_filename, width=10.5*300, height=height, res=300)
        
        forest <- forest.meta(m.bin_or,
                              sortvar = TE,
                              fontsize = 7,
                              fixed = TRUE,
                              print.tau2 = TRUE,
                              print.I2 = TRUE,
                              print.Q = TRUE,
                              rightcols = c("effect", "ci", "w.fixed","w.random", "focus_Simon", "Gendered", "location_Simon"),
                              rightlabs = c("effect", "ci", "w.fixed","w.random", "Study Focus", "Gendered", "Location"),
                              just.addcols.right = "right")
        
        
        dev.off()
        
        
        fixedOR <- forest$effect.format[1]
        fixedORci <- forest$ci.format[1]
        
        randomOR <- forest$effect.format[2]
        randomORci <- forest$ci.format[2]
        
        zval.fixed <- round(m.bin_or$statistic.fixed, 3)
        pval.fixed <- round(m.bin_or$pval.fixed, 3)
        
        
        zval.random <- round(as.double(m.bin_or$statistic.random, 2))
        pval.random <- round(as.double(m.bin_or$pval.random, 2))
        
        
        fixed_df <- data.frame("Fixed", fixedOR, fixedORci, zval.fixed, pval.fixed)
        colnames(fixed_df) <- c("Model", "OR", "ci", "z", "p")
        
        random_df <- data.frame("Random", randomOR, randomORci, zval.random, pval.random)
        colnames(random_df) <- c("Model", "OR", "ci", "z", "p")
        
        out_df <- rbind(fixed_df, random_df)
        
        
        k <- m.bin_or$k
        
        H <- round(m.bin_or$H, 2)
        lower.H <-m.bin_or$lower.H
        upper.H <-m.bin_or$upper.H
        H.ci <- paste0("[", round(lower.H, 2), ";", round(upper.H, 2), "]")
        
        I <- round(m.bin_or$I2, 2)
        lower.I <-m.bin_or$lower.I2
        upper.I <-m.bin_or$upper.I2
        I.ci <- paste0("[", round(lower.I, 2), ";", round(upper.I, 2), "]")
        
        tau <- round(m.bin_or$tau, 2)
        lower.tau <-m.bin_or$lower.tau
        upper.tau <-m.bin_or$upper.tau
        tau.ci <- paste0("[", round(lower.tau, 2), ";", round(upper.tau, 2), "]")
        
        
        tau2 <- round(m.bin_or$tau2, 2)
        lower.tau2 <-m.bin_or$lower.tau2
        upper.tau2 <-m.bin_or$upper.tau2
        tau2.ci <- paste0("[", round(lower.tau2, 2), ";", round(upper.tau2, 2), "]")
        
        
        Q <- round(m.bin_or$Q, 2)
        Q.pval <- round(m.bin_or$pval.Q, 3)
        
        
        out_df1 <- data.frame(k, H, H.ci, I, I.ci, tau, tau.ci, tau2, tau2.ci, Q, Q.pval)
        
        final_out <- cbind(out_df, out_df1)
        
        
        
        final_out <- cbind(item = col_item, final_out)
        final_out <- cbind(col = col, final_out)
        final_out <- cbind(filter = filter, final_out)
        
        final_appended <- rbind(final_appended,final_out)
        
        es <- forest$effect.format
        es <- es[4:length(es)]
        filter <- filter
        item <- col_item
        
        
        dat <- data.frame(es)
        dat$es <- es
        dat$filter <- filter
        dat$item <- item
        
        
        dat_out <- rbind(dat_out, dat)
        
      }#end if col item not nan
    }
    
    
  }#close variable
  
}



df_filt<- df_filt %>% mutate_at("MeanAge", as.numeric)



filt_appended <- final_appended %>% filter(Model=="Random")


write.csv(final_appended, "Outputs/Tables/final_appended.csv")

write.csv(filt_appended, "filt_appended.csv")




