input <- NULL
input$algorithm <- "PROCAM"
input$age <- 99
input$cholesterin_LDL_mg <- 129 #mg/dl
input$cholesterin_HDL_mg <- 35 #mg/dl
input$triglycerides <- 200 #mg/dl
input$smoker <- 0 #dummy 1 = yes
input$diabetes <- 1 #dummy 1 = yes
input$MI_family <- 1 #dummy 1 = yes
input$RR_sys <- 140 #mmHg


calculate_score <- function(input){
  if(input$algorithm == "PROCAM"){
    
    ###PROCAM https://www.ahajournals.org/doi/full/10.1161/hc0302.102575
    label_to_num <- function(fac){
      return(as.numeric(as.character(fac)))
    }
    score_procam <- 0
    
    score_procam = score_procam + label_to_num((cut(input$age, breaks = c(0, 40, 45, 50, 55, 60, 150), right = FALSE, labels = c(0,6,11,16,21,26))))
    score_procam = score_procam + label_to_num(cut(input$cholesterin_LDL_mg, breaks = c(0, 100, 130, 160, 190, 1000), right = FALSE, labels = c(0, 5, 10, 14, 20)))
    score_procam = score_procam + label_to_num(cut(input$cholesterin_HDL_mg, breaks = c(0, 35, 45, 55, Inf), right = FALSE, labels = c(11, 8, 5, 0)))
    score_procam = score_procam + label_to_num(cut(input$triglycerides, breaks = c(0, 100, 150, 200, Inf), right = FALSE, labels = c(0,2,3,4)))
    score_procam = score_procam + ifelse(input$smoker == 1, 8, 0)
    score_procam = score_procam + ifelse(input$diabetes == 1, 6, 0)
    score_procam = score_procam + ifelse(input$MI_family == 1, 4, 0)
    score_procam = score_procam + label_to_num(cut(input$RR_sys, breaks = c(0, 120, 130, 140, 160, Inf), right= FALSE, labels = c(0, 2, 3, 5, 8)))
    
    #cut(score_procam, breaks = c(0, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, Inf))
    return(score_procam)
  }else{
    ###SCORE https://www.medizin.uni-muenster.de/fileadmin/einrichtung/epi/download/aerzteblatt_methoden.pdf
    #x_1 <- input$cholesterin_total #in mmol/l
    x_2 <- input$RR_sys #mmHg systolisch
    x_3 <- input$smoker #1 ja
    c_1 <- 6 #mmol/l
    c_2 <- 120 #mmHg
  }
}


calculate_score(input)


p <- 0.21
pop <- 100


factorize <- function(x) {
  x <- as.integer(x)
  div <- seq_len(abs(x))
  factors <- div[x %% div == 0L]
  factors <- list(neg = -factors, pos = factors)
  return(factors)
}
factz <- as.numeric(factorize(pop)$pos)
ind <- round(length(factz)/2, 0)
nrows <- factz[ind+1]
ncols <- pop/nrows


nrows <- 10
ncols <- 10
pep <- p*nrows*ncols

df <- expand.grid(y = 1:nrows, x = 1:ncols)
df <- df %>% arrange(desc(x)) %>% arrange(desc(y))

df$ind <- 1:nrow(df)
df$group <- "healthy"
df$group[1:pep] <- "risk"
df$emo <- ifelse(df$group == "healthy", emoji('smile'), emoji('disappointed'))


#pre-plot

ggplot(df, aes(x = x, y = y, fill = group)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  labs(title="", subtitle="",
       caption=paste("Quelle:", input$algorithm)) +
  theme(panel.border = element_rect(size = 2, fill = NA),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "right")

load.emojifont('OpenSansEmoji.ttf')
ggplot(df, aes(x = x, y = y, fill = group, label = emo)) + 
  geom_tile(color = "black", size = 0.5) +
  geom_text(family = "OpenSansEmoji", size = 5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0,0), trans = 'reverse') +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  scale_fill_manual(values = c("#67a9cf", "#ef8a62")) +
  labs(title="", subtitle="",
       caption=paste("Quelle:", input$algorithm)) +
  theme(panel.border = element_rect(size = 2, fill = NA),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")

ggplot(df, aes(x = x, y = y, fill = group, label = emo, color = emo)) + 
  geom_text(family = "OpenSansEmoji", size = 8) +
  scale_y_continuous(trans = 'reverse') +
  scale_color_manual(values = c("#67a9cf", "#ef8a62")) +
  labs(title="", subtitle="",
       caption=paste("Quelle:", input$algorithm)) +
  theme_void() +
  theme(panel.border = element_rect(size = 2, fill = NA),
        legend.position = "none")


#post_plot

stop_smoking <- 0.10
pep_stop_smoking <- stop_smoking*pop

df <- df %>% arrange(desc(group), y, x) 
df$improv <- FALSE

if (pep_stop_smoking>0){
  df$improv[1:pep_stop_smoking] <- TRUE
}

df$group_new <- paste(df$group, df$improv, sep = "_")
df$emo <- ifelse(df$group == "healthy", emoji('smile'), emoji('disappointed'))
df$emo <- ifelse(df$improv == TRUE, emoji('smile'), df$emo)


plot <- ggplot(df, aes(x = x, y = y, fill = group, label = emo, color = group_new)) + 
  geom_text(family = "OpenSansEmoji", size = 8) +
  scale_x_continuous(expand = c(0.08, 0.08)) +
  scale_y_continuous(expand = c(0.08, 0.08), trans = 'reverse') +
  scale_color_manual(values = c("#67a9cf", "#ef8a62", "#053061")) +
  theme_void() +
  theme(legend.position = "none")
plot

grob <- ggplotGrob(plot)

plot.rrg <- roundrectGrob(gp = gpar(fill = "#f5f5f5", col = "#e3e3e3", lwd = 1.5),
                          r = unit(0.03, "npc"))
grid.draw(gList(plot.rrg, grob))

library(grid)

input <- NULL
input$smoker <- 0 #dummy 1 = yes
input$diabetes <- 1 #dummy 1 = yes
input$MI_family <- 1 #dummy 1 = yes
input$RR_sys <- 140 #mmHg
input$improv <- 1



mod_improv <- mod_improv[which(mod_improv != 0)]
mod_improv[0]

sum(c(2, 3) %in% 1)

input$improv %in% 1

ifelse(input$improv %in% 1, 0, ifelse(input$smoker == 1, 8, 0))

score_procam <- 0
score_procam = score_procam + label_to_num((cut(input$age, breaks = c(0, 40, 45, 50, 55, 60, 150), right = FALSE, labels = c(0,6,11,16,21,26))))
score_procam = score_procam + label_to_num(cut(input$cholesterin_LDL_mg, breaks = c(0, 100, 130, 160, 190, 1000), right = FALSE, labels = c(0, 5, 10, 14, 20)))
score_procam = score_procam + label_to_num(cut(input$cholesterin_HDL_mg, breaks = c(0, 35, 45, 55, Inf), right = FALSE, labels = c(11, 8, 5, 0)))
score_procam = score_procam + label_to_num(cut(input$triglycerides, breaks = c(0, 100, 150, 200, Inf), right = FALSE, labels = c(0,2,3,4)))
score_procam = score_procam + ifelse(input$improv %in% 1, 0, ifelse(input$smoker == 1, 8, 0))
score_procam = score_procam + ifelse(input$improv %in% 2, 0, ifelse(input$diabetes == 1, 6, 0))
score_procam = score_procam + ifelse(input$MI_family == 1, 4, 0)
score_procam = score_procam + ifelse(input$improv %in% 3, 0, label_to_num(cut(input$RR_sys, breaks = c(0, 120, 130, 140, 160, Inf), right= FALSE, labels = c(0, 2, 3, 5, 8))))

