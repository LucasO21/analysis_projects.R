get_campaign_data <-
function() {
    
    # column names
    colnames <- c("date", "campaign_id", "impressions", "clicks", "cost", "ctr", 
                  "conversions", "cvr")
    
    # ad words tbl
    adwords_tbl <- readxl::read_excel(
        path  = "../data/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("ad_words")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "AdWords")
    
    # facebook tbl
    facebook_tbl <- readxl::read_excel(
        path  = "../data/Marketing+Analytics+Case+Study.xlsm",
        sheet = "PPC Data",
        skip  = 3
    ) %>% 
        clean_names() %>% 
        select(date, starts_with("facebook")) %>% 
        `colnames<-`(colnames) %>% 
        mutate(campaign = "Facebook")
    
    # combined tbl
    combined_tbl <- bind_rows(adwords_tbl, facebook_tbl) %>% 
        mutate(date = ymd(date))
    
    return(combined_tbl)
    
}
get_campaign_metrics <-
function(data, metric = "cost", level = "campaign", campaign = NULL) {
    
    # rlang setup
    metric <- rlang::sym(metric)
    level  <- rlang::sym(level)
    
    # filtering setup
    if (is.null(campaign)) {
        campaign_name = campaign
    } else if (campaign == "AdWords") {
        campaign_name = "AdWords"
    } else {
        campaign_name = "Facebook"
    }
    
    # data prep
    if (is.null(campaign)) {
        data <- data
    } else {
        data <- data %>% filter(campaign == campaign_name)
    }
    ret <- data %>% 
        select(!!level, !!metric) %>% 
        mutate(total = "Total") %>% 
        pivot_longer(cols = -!!metric, names_to = "key", values_to = "campaign") %>% 
        summarise(!!paste0("total_", metric):= sum(!!metric), .by = campaign) %>% 
        arrange(campaign)
    
    # return
    return(ret)
    
}
get_campaign_metrics_table <-
function(data, level = "campaign", campaign = NULL) {
    
    # aggregates
    cost_tbl <- get_campaign_metrics(data, "cost", level, campaign)
    
    impr_tbl <- get_campaign_metrics(data, "impressions", level, campaign)
    
    clicks_tbl <- get_campaign_metrics(data, "clicks", level, campaign)
    
    leads_tbl <- get_campaign_metrics(data, "conversions", level, campaign)
    
    
    # ret
    ret <- cost_tbl %>% 
        left_join(impr_tbl) %>% 
        left_join(clicks_tbl) %>% 
        left_join(leads_tbl) %>% 
        rename(total_leads = total_conversions) %>% 
        mutate(
            ctr      = total_clicks / total_impressions,
            cpc      = total_cost / total_clicks,
            cvr = total_leads / total_clicks
        )
    
    # return
    return(ret)
    
}
