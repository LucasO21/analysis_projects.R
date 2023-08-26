get_ppc_metrics <-
function(data, metric = "cost", level = "campaign", 
                            campaign = NULL) {
    
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
get_ppc_metrics_table <-
function(data, level = "campaign", campaign = NULL) {
    
    # aggregates
    cost_tbl   <- get_ppc_metrics(data, "cost", level, campaign)
    
    impr_tbl   <- get_ppc_metrics(data, "impressions", level, campaign)
    
    clicks_tbl <- get_ppc_metrics(data, "clicks", level, campaign)
    
    leads_tbl  <- get_ppc_metrics(data, "conversions", level, campaign)
    
    
    # calculation
    ppc_metrics_tbl <- cost_tbl %>% 
        left_join(impr_tbl) %>% 
        left_join(clicks_tbl) %>% 
        left_join(leads_tbl) %>% 
        rename(total_leads = total_conversions) %>% 
        mutate(
            ctr     = total_clicks / total_impressions,
            cpc     = total_cost / total_clicks,
            ctl_cvr = total_leads / total_clicks
        )
    
    # return
    return(ppc_metrics_tbl)
    
}
get_cost_per_lead <-
function(data, budget) {
    
    # uses ppc metrics data
    cost_per_lead_tbl <- data %>% 
        select(campaign, ctr, cpc, ctl_cvr) %>% 
        mutate(clicks = budget / cpc) %>% 
        mutate(leads = ctl_cvr * clicks) %>% 
        mutate(cpl = budget / leads)
    
    return(cost_per_lead_tbl)
    
}
get_ppc_final_output <-
function(data, budget = 1000, level = "campaign",
                                 output = "cpl") {
    
    metrics <- get_ppc_metrics_table(data, level = "campaign") 
        
    cpl <- get_cost_per_lead(data = metrics, budget = budget)
    
    if (output == "cpl") {
        ret <- cpl
    } else {
        ret <- metrics
    }
    
    return(ret)
    
}
