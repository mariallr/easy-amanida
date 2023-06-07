library(amanida)
library(tidyverse)
library(ggplot2)
library(plotly)

# data("sample_data")
# 
# amanida_result <- compute_amanida(sample_data)

amanida_palette <- function() {
  
  set.seed(123) 
  
  c("#F4A460", "#87CEEB", "#CD5C5C", "#A9A9A9", "#FFEFD5")
}

# Volcano plot amb opció de labels o no

volcano_plot <- function(mets, cutoff = NULL, names = T) {
  
  articles = NULL; pval = NULL; fc = NULL; lfc = NULL; lpval = NULL; . = NULL; 
  label = NULL; sig = NULL;
  set.seed(123)
  
  col_palette <- amanida_palette()
  
  # Search for cutoff argument
  if (hasArg(cutoff)) { 
    cuts <- cutoff
    
    if (length(cuts) != 2) {
      stop( "Please indicate one cut-off for p-value and one for fold-change")
    }
    cut_pval <- -log10(cuts[1])
    cut_fc <- log2(cuts[2])
    
    # If not cutoff argument convention values are established: 
  } else {
    # Alpha < 0.05 
    cut_pval <- -log10(0.05)
    # Log(fold-change) = 1.5
    cut_fc <- log2(2.83)
  }
  
  message(paste("The cut-off used are "), (10^-cut_pval), " for p-value (", round(cut_pval,2), 
          " in log10 scale) and ", 2^cut_fc, 
          " for fold-change (", round(cut_fc,2), " in log2 scale).", sep = "")
  
  # Compounds with 2 or more reports
  cont <- as_tibble(mets@vote) %>% 
    mutate(articles = as.numeric(articles)) %>% 
    filter(articles >= 2)
  
  cont_ids <- cont %>% pull(id)
  
  # Function for labels
  case_character_type <- function(lfc, lpval) {
    case_when(
      (lfc < -cut_fc & lpval > cut_pval) ~ paste("p-value < ", 10^-cut_pval, 
                                                 "& fold-change < ", -2^cut_fc),
      (lpval > cut_pval & abs(lfc) < cut_fc) ~ paste("p-value < ", 10^-cut_pval),
      (lfc > cut_fc & lpval > cut_pval) ~ paste("p-value <", 10^-cut_pval, 
                                                "& fold-change >", 2^cut_fc),
      T ~ "under cut-offs"
    )}
  
  ## Volcano plot
  
  # Scatter plot for logarithmic fold-change vs. -logarithmic p-value
  dt_p <- as_tibble(mets@stat) %>%
    mutate( 
      # Format data needed
      across(c(pval,fc), as.numeric),
      # Negative logarithm of p-value for plot              
      lpval = -log10(pval),
      # Logarithm of fold-change
      lfc = log2(fc)) %>%
    mutate(sig = case_character_type(lfc, lpval),
           label = case_when(
             sig == paste("p-value < ", 10^-cut_pval) ~ "",
             sig == "under cut-offs" ~ "",
             T ~ id),
           reports = case_when(
             id %in% cont_ids ~ "> 1 report",
             T ~ "single report" )) %>%
    dplyr::group_by('sig') 
  if ( names == T) {
    ggplot(dt_p, aes(lfc, lpval, label = label, colour = sig)) +
      geom_point(aes(shape = dt_p$reports), size = 2.5) + 
      scale_shape_manual(values = c(8, 16), name = "") +
      theme_minimal() +
      ggrepel::geom_text_repel(size = 4,
                               fontface = "bold",
                               segment.size = 0.4,
                               point.padding = (unit(0.3, "lines")),
                               box.padding = unit(0.3, "lines"),
                               colour = "black",
                               max.overlaps = Inf) +
      # Axis titles
      xlab( "log2(Fold-change)") + 
      ylab(expression(paste("-log10(", italic(p), "-value)"))) + 
      labs(colour = "", tag = "Created with amanida") +
      # X axis breaks
      scale_x_continuous(breaks = seq(round(min(dt_p$lfc),0) - 1, 
                                      round(max(dt_p$lfc),0) + 1, 1), 
                         limits = c(min(dt_p$lfc), max(dt_p$lfc))) + 
      # Cutoff marks
      geom_hline(yintercept = cut_pval, 
                 colour = "black", 
                 linetype = "dashed") + 
      geom_vline(xintercept = c(cut_fc, -cut_fc),
                 colour = "black", 
                 linetype = "dashed") + 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
            legend.text = element_text(size = 10), 
            plot.tag = element_text(size = 9, colour = "grey"),
            plot.tag.position = "bottomright") +
      guides(col = guide_legend(nrow = 2, byrow = T)) + 
      guides(shape = guide_legend(nrow = 2, byrow = T)) +
      scale_color_manual(values = col_palette) +
      ggtitle("Volcano plot of adapated meta-analysis results")
 } else {
   ggplot(dt_p, aes(lfc, lpval, colour = sig)) +
      geom_point(aes(shape = dt_p$reports), size = 2.5) + 
      scale_shape_manual(values = c(8, 16), name = "") +
      theme_minimal() +
      # Axis titles
      xlab( "log2(Fold-change)") + 
      ylab(expression(paste("-log10(", italic(p), "-value)"))) + 
      labs(colour = "", tag = "Created with amanida") +
      # X axis breaks
      scale_x_continuous(breaks = seq(round(min(dt_p$lfc),0) - 1, 
                                    round(max(dt_p$lfc),0) + 1, 1), 
                       limits = c(min(dt_p$lfc), max(dt_p$lfc))) + 
      # Cutoff marks
      geom_hline(yintercept = cut_pval, colour = "black", linetype = "dashed") + 
      geom_vline(xintercept = c(cut_fc, -cut_fc), colour = "black", linetype = "dashed") + 
      theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5),
              legend.text = element_text(size = 10), 
            plot.tag = element_text(size = 9, colour = "grey"),
            plot.tag.position = "bottomright") +
      guides(col = guide_legend(nrow = 2, byrow = T)) + 
      guides(shape = guide_legend(nrow = 2, byrow = T)) +
      scale_color_manual(values = col_palette) +
      ggtitle("Volcano plot of adapated meta-analysis results")
 }
}

# volcano_plot(amanida_result, names = F)

## Volcano plotly

volcano_plotly <- function(mets, cutoff = NULL){
  set.seed(123)
  
  col_palette <- amanida_palette()
  
  # Search for cutoff argument
  if (hasArg(cutoff)) { 
    cuts <- cutoff
    
    if (length(cuts) != 2) {
      stop( "Please indicate one cut-off for p-value and one for fold-change")
    }
    cut_pval <- -log10(cuts[1])
    cut_fc <- log2(cuts[2])
    
    # If not cutoff argument convention values are established: 
  } else {
    # Alpha < 0.05 
    cut_pval <- -log10(0.05)
    # Log(fold-change) = 1.5
    cut_fc <- log2(2.83)
  }
  
  # Function for labels
  case_character_type <- function(lfc, lpval) {
    case_when(
      (lfc < -cut_fc & lpval > cut_pval) ~ paste("p-value < ", 10^-cut_pval, 
                                                 "& fold-change < ", -2^cut_fc),
      (lpval > cut_pval & abs(lfc) < cut_fc) ~ paste("p-value < ", 10^-cut_pval),
      (lfc > cut_fc & lpval > cut_pval) ~ paste("p-value <", 10^-cut_pval, 
                                                "& fold-change >", 2^cut_fc),
      T ~ "under cut-offs"
    )}
  # Compounds with 2 or more reports
  cont <- as_tibble(mets@vote) %>% 
    mutate(articles = as.numeric(articles)) %>% 
    filter(articles >= 2)
  
  cont_ids <- cont %>% pull(id)
  
  # Data
  dt_p <- as_tibble(mets@stat) %>%
    mutate( 
      # Format data needed
      across(c(pval,fc), as.numeric),
      # Negative logarithm of p-value for plot              
      lpval = -log10(pval),
      # Logarithm of fold-change
      lfc = log2(fc)) %>%
    mutate(sig = case_character_type(lfc, lpval),
           label = case_when(
             sig == paste("p-value < ", 10^-cut_pval) ~ "",
             sig == "under cut-offs" ~ "",
             T ~ id),
           reports = case_when(
             id %in% cont_ids ~ "> 1 report",
             T ~ "single report" )) %>%
    dplyr::group_by('sig')
  
  ## Aquí el canvi codi
  
  lev <- as.factor(dt_p$sig)
  
  col_palette <- setNames(col_palette, c(levels(lev),
                                         "yellow2"))
  ##
  
  plot_ly(dt_p, x = ~lfc, y = ~lpval,
          color = ~sig, colors = col_palette, type = "scatter", 
          mode = "markers", symbol = ~reports,
          symbols = c("circle", "o"),
          hoverinfo = 'text',
          text = ~paste('</br>  ', id,
                        '</br> log(p-value): ', round(lpval,2),
                        '</br> log(fold-change): ', round(lfc,2))) %>%
    layout(title = "Volcano plot of adapated meta-analysis results", 
           legend = list(orientation = 'h')) 
  
}

# volcano_plotly(amanida_result)


## Per fer amb tots els noms

vote_plot <- function(mets, counts = NULL) {
  
  votes = NULL; . = NULL;
  set.seed(123)
  
  col_palette <- amanida_palette()
  
  if (hasArg(counts)) { 
    cuts <- counts
    
    if (length(counts) != 1) {
      stop( "Please indicate one cut-off only")
    }
  } else {
    cuts <- 1
  }
  
  message("Cut-off for votes is ", cuts, ".", sep = "")
  
  # Subset vote-couting data
  tb <- as_tibble(mets@vote) %>% 
    mutate(
      votes = as.numeric(votes)) %>%
    filter (abs(votes) >= cuts)
  
  if (nrow(tb) < 1) {
    stop( "Cut-off value out of limit")
  }
  
  tb %>%
    {
      ggplot(., aes(reorder(id, votes), votes, fill = votes)) + 
        geom_bar(stat = "identity", show.legend = F, width = .5
        ) +
        geom_text(aes(label = reorder(id, votes)), vjust = 0.2, size = 3.5, 
                  hjust = ifelse(test = .$votes > 0, yes = 0, no = 1)) +
        scale_fill_gradient(low = col_palette[3], high = col_palette[5]) +
        theme_light() + 
        theme(axis.text.y = element_blank(),
              axis.text.x = element_text(size = 10),
              axis.title = element_text(size = 10),
              axis.ticks.y = element_blank(), 
              plot.title = element_text(size = 12), 
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(), 
              panel.border = element_blank(), 
              panel.grid.major.x = element_line(linetype = "dashed"), 
              plot.tag = element_text(size = 9, colour = "grey"),
              plot.tag.position = "bottomright") +
        labs(tag = "Created with amanida") +
        coord_flip() +
        ylab("Vote-counting") +
        xlab('')+
        ggtitle("Total vote count of compounds behaviour") +
        scale_y_continuous(expand = c(0.6, 0), 
                           breaks = seq(max(abs(.$votes))*-1, max(abs(.$votes)), by = 1),
                           limits = c(max(abs(.$votes))*-1,
                                      max(abs(.$votes)) + 1)
        )
    }
  
}

# coln = c("Compound Name", "Behaviour", "References")
# input_file <- system.file("extdata", "dataset2.csv", package = "amanida")
# data_votes <- amanida_read(input_file, mode = "qual", coln, separator = ";")
# 
# vote_result <- amanida_vote(data_votes)
# 
# vote_plot(vote_result, counts = 0)

## Explore amb tots els valors

explore_plot <- function(data, type = "all", counts = NULL, return_data=FALSE) {
  
  trend = NULL; trend_l = NULL; N = NULL; vc = NULL; . = NULL; 
  cont = NULL; lab = NULL;
  set.seed(123)
  
  col_palette <- amanida_palette()
  
  if (hasArg(counts)) { 
    cuts <- counts
    
    if (length(cuts) != 1) {
      stop( "Please indicate one cut-off")
    }
  } else {
    stop("Function needs counts parameter")
  } 
  
  message("Cut-off for votes is ", cuts, ".", sep = "")
  
  if (type == "all") {
    dt <- data %>%
      mutate(
        trend_l = case_when(
          trend == -1 ~ "Down-regulated", 
          T ~ "Up-regulated"
        )
      ) %>% dplyr::group_by(id) %>% 
      mutate(vc = sum(trend)) %>%
      dplyr::group_by(id, trend_l) %>%
      summarise(
        cont = n(),
        total_N = sum(N),
        vc = unique(vc),
        lab = c("Vote-counting")
      ) %>%
      mutate(cont = case_when(
        trend_l == "Down-regulated" ~ cont*-1,
        T ~ cont*1
      )) %>%
      filter(vc >= cuts | vc <= -1*cuts)
    
  } else if (type == "sub") {
    dt <- data %>%
      mutate(
        trend_l = case_when(
          trend == -1 ~ "Down-regulated", 
          T ~ "Up-regulated"
        )
      ) %>% dplyr::group_by(id) %>%
      mutate(vc = sum(trend)) %>%
      dplyr::group_by(id, trend_l) %>%
      summarise(
        cont = n(),
        total_N = sum(N),
        vc = unique(vc),
        lab = c("Vote-counting")
      ) %>%
      mutate(cont = case_when(
        trend_l == "Down-regulated" ~ cont*-1,
        T ~ cont*1
      )) %>%
      filter(vc >= cuts | vc <= -1*cuts)
    
  } else if (type == "mix") {
    dt <- data %>%
      mutate(
        trend_l = case_when(
          trend == -1 ~ "Down-regulated", 
          T ~ "Up-regulated"
        )
      ) %>% dplyr::group_by(id) %>% 
      mutate(vc = sum(trend)) %>%
      dplyr::group_by(id, trend_l) %>%
      summarise(
        cont = n(),
        total_N = sum(N),
        vc = unique(vc),
        lab = c("Vote-counting")
      ) %>%
      mutate(cont = case_when(
        trend_l == "Down-regulated" ~ cont*-1,
        T ~ cont*1
      )) %>%
      filter(vc >= cuts | vc <= -1*cuts |
               vc != cont)
  }
  
  if (nrow(dt) < 1) {
    stop( "Cut-off value out of limit")
  }
  
  # Prepare data for plot
  
  if(return_data==TRUE){
    dt
  }else{
    dt %>%
      {
        ggplot(., mapping = aes(x = cont,
                                y = reorder(id, vc), fill = trend_l)) +
          geom_bar(aes(x = ifelse(test = trend_l == "Up-regulated",
                                  yes = cont, no = cont)), stat = "identity",
                   alpha = .3) +
          scale_fill_manual(values = col_palette[2:3]) +
          geom_segment(aes(x = 0, xend = vc, 
                           y = id, yend = id, linetype = lab),
                       size = 0.4, alpha = 0.9, 
                       arrow = arrow(length = unit(0.1, "cm")), 
                       lineend = "round", linejoin = "round") +
          scale_x_continuous(labels = abs, limits = max(abs(.$cont)) * c(-1,1) * 1.01) +
          scale_color_manual(values = c(col_palette[2], col_palette[3])) +
          theme_minimal() +
          xlab("Counts by trend") + 
          ylab("") +
          labs(fill = "Counts by trend", tag = "Created with amanida") +
          ggtitle("Qualitative compounds trend plot") +
          theme(legend.position = "bottom", legend.title = element_blank(),
                axis.text.y = element_text(size = 14), plot.tag = element_text(size = 9, colour = "grey"),
                plot.tag.position = "bottomright") +
          guides(col = guide_legend(nrow = 2, byrow = T)) + 
          guides(shape = guide_legend(nrow = 2, byrow = T)) 
      }
  }
  
}

# explore_plot(sample_data, counts = 0)


