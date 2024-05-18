# Function: Trend Plot ----
get_metrics_trend_plot <- function(data, x_vars = "engage_month", yp_vars = "deals_total",
                                         ys_vars = "deals_close_rate", combo = FALSE, facet = TRUE,
                                         x_axis_text_size = 10, y_axis_text_size = 10, y_axis_label_size = 10,
                                         plot_title_size = 16, plot_subtitle_size = 14,
                                         facet_ncol = 4) {
    
    # rlang setup
    x_vars_sym  <- rlang::ensym(x_vars)
    yp_vars_sym <- rlang::ensym(yp_vars)
    ys_vars_sym <- rlang::ensym(ys_vars)
    
    # combo chart setup
    max_deals_total <- max(data$deals_total, na.rm = TRUE)
    
    if (!combo) {
        
        p <- data %>% 
            ggplot(aes(x = engage_month, y = !!yp_vars_sym)) +
            geom_line(size = 1.3, color = "orange")+
            geom_smooth(method = "lm", se = FALSE, color = "blue", size = 0.7, alpha = 0.8) +
            scale_y_continuous(labels = scales::dollar) 
        #facet_wrap(vars(!!segment_name_sym))
    } else {
        
        p <- data %>%
            ggplot(aes(x = !!x_vars_sym)) +
            geom_bar(aes(y = !!yp_vars_sym, fill = "Total Deals"), stat = "identity") +
            geom_line(aes(y = !!ys_vars_sym * max_deals_total, group = 1, color = "Close Rate"), size = 1.2) +
            geom_point(aes(y = !!ys_vars_sym * max_deals_total, color = "Close Rate"), size = 2) +
            scale_y_continuous(
                name = "Total Deals",
                sec.axis = sec_axis(
                    ~ . / max_deals_total, 
                    name = "Deal Close Rate (%)",
                    labels = scales::percent
                )
            ) +
            scale_fill_manual(name = "Metrics", values = c("Total Deals" = "lightgrey")) +
            scale_color_manual(name = "Metrics", values = c("Close Rate" = "orange"))
    }
    
    # theme
    p <- p +
        theme_bw() +
        #labs(title = title, subtitle = subtitle, x = x_lab, y = ylab) +
        theme(
            
            # axis text
            axis.text.y = element_text(size = y_axis_text_size, color = "black"),   
            axis.text.x = element_text(size = x_axis_text_size, color = "black"), 
            
            # axis labels
            axis.title.y = element_text(size = y_axis_label_size, color = "black"),
            axis.title.x = element_blank(),
            
            # plot title size
            plot.title = element_text(size = plot_title_size, color = "black", face = "bold"),
            plot.subtitle = element_text(size = plot_subtitle_size, color = "black")
        )
    
    
    # legend
    if (!facet) {
        p <- p + 
            theme(
                legend.position = c(0.9, 0.9),  # Position the legend inside the plot at the top right
                legend.justification = c("right", "top"),  # Anchor point of the legend
                legend.box.just = "right",  # Justify the legend box at the right
                legend.margin = margin(t = -10, r = 10, b = 10, l = 10),  # Adjust legend margin if necessary
                legend.direction = "horizontal",  # Layout legend vertically
                legend.title = element_blank()
            )
    } else {
        p <- p + 
            #theme(legend.position = "none")+
            theme(legend.position = "bottom") + 
            theme(legend.title = element_blank())
    }
    
    # text labels
    if (!facet) {
        p <- p + 
            geom_text(
                aes(y = deals_close_rate * max_deals_total, label = scales::percent(deals_close_rate, accuracy = 0.1)),
                hjust = 0.5, vjust = -0.5, size = 4, fontface = "bold", color = "#2c3e50"
            ) +
            geom_text(
                aes(y = deals_total, label = deals_total),
                hjust = 0.5, vjust = -0.5, size = 4, fontface = "bold", color = "#2c3e50"
            )
    }
    
    # faceting
    if (facet) {
        segment_name <- data[, 2] %>% colnames()
        segment_name_sym <- rlang::ensym(segment_name)
        
        p <- p + 
            facet_wrap(vars(!!segment_name_sym), ncol = facet_ncol) +
            theme(strip.text = element_text(size = 12, color = "black", face = "bold"))
    }
    
    # return
    return(p)
    
}

