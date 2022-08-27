# Plain R ----------
# setwd(getSrcDirectory()[1])
# RStudio ----------
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Libraries ----------

library(openxlsx)
library(pastecs)
library(ggplot2)
library(gtools)
library(stringr)
library(lmtest)
library(urca)
library(vars)
library(dplyr)


# Define string formatting operator ----------

`%format%` <- function(x, y) {
    
    do.call(sprintf, c(list(x), y))
}


# Modeling functions ----------

do_granger <- function(data_to_test,
                       num_of_possible_lags,
                       alpha_value = 0.1) {
    vars_to_test <- names(data_to_test)
    x <- permutations(length(vars_to_test), 2)
    
    for (i in 1:NROW(x)) {
        x1 <- x[i, 1]
        x2 <- x[i, 2]
        names <- c(vars_to_test[x1],
                   vars_to_test[x2])
        for (j in 1:length(num_of_possible_lags)) {
            test <- grangertest(formula = as.formula("%s ~ %s" %format% names),
                                order = num_of_possible_lags[j],
                                data = data_to_test)
            if (test$`Pr(>F)`[2] < alpha_value) {
                print(test)
            }
        }
    }
}

etl_step0 <- function(country, years, lag_max, currencies_file_name, tags) {
    data_file_country <- "%s_data.xlsx" %format% c(country)
    sheet_names <- openxlsx::getSheetNames(data_file_country)
    analysis_data <- list()
    
    for (name in sheet_names) {
        N <- lag_max + 2
        analysis_data[[name]] <- t(openxlsx::read.xlsx(data_file_country, sheet = name)[1, 2:N])
    }
    
    currency_rates <- t(openxlsx::read.xlsx(currencies_file_name, sheet = "Sheet1"))
    
    currency_rates <- data.frame(currency_rates[2:NROW(currency_rates), ])
    names(currency_rates) <- tags
    
    for (column in names(currency_rates)) {
        currency_rates[[column]] <- as.double(currency_rates[[column]])
    }
    
    for (column in names(analysis_data)) {
        y <- analysis_data[[column]] 
        z <- na.spline.default(as.double(y[1:NROW(y), ]))
        analysis_data[[column]] <- ifelse(z <= 0 | is.na(z),
                                          na.fill(y, fill = c("extend", "extend", "extend")),
                                          z)
    }
    
    label <- substring(country, 1, 3)
    new_data_country <- data.frame(year = years)
    
    
    
    if (grepl("cuip", country)) {
        new_data_country$cuip <- analysis_data$ict * analysis_data$gdp_defl_lcu * currency_rates[[label]]
    } else if (grepl("fix", country)) {
        new_data_country$fix <- analysis_data$ict
    } else if (grepl("se", country)) {
        new_data_country$se <- analysis_data$ict * analysis_data$gdp_defl_lcu * currency_rates[[label]]
    } else if (grepl("rd", country)) {
        new_data_country$rd <- analysis_data$ict * analysis_data$gdp_defl_lcu
    }
    
    new_data_country$gdp <- analysis_data$gdp_lcu * analysis_data$gdp_defl_lcu
    new_data_country$l <- analysis_data$total_wages_lcu * analysis_data$gdp_defl_lcu
    new_data_country$k <- analysis_data$gross_cap_form_lcu * analysis_data$gdp_defl_lcu
    
    return(new_data_country)
}

etl_step1_combine_ict_factors <- function(list_of_all_frames) {
    out_df <- list_of_all_frames[[1]]
    
    if (length(list_of_all_frames) >= 2) {
        for (i in 2:length(list_of_all_frames)) {
            out_df <- merge(out_df,
                            list_of_all_frames[[i]],
                            by = c("year", "gdp", "l", "k"))
        }
    }
    
    
    return(out_df)
}

save_acf <- function(data_vector,
                     subtitle,
                     filename) {
    
    if (grepl("dlog", subtitle)) {
        data <- diff(log(data_vector), lag = 1)
    } else if (grepl("log", subtitle)) {
        data <- log(data_vector)
    } else if (grepl("d", subtitle)) {
        data <- diff(data_vector, lag = 1)
    } else {
        data <- data_vector
    }
    
    a <- acf(data, plot = FALSE, lag.max = lag_max)
    plot_x <- ggplot(data = data.frame(Lag = a$lag, ACF = a$acf),
                     aes(x = Lag, y = ACF)) +
        geom_line() +
        ggtitle(acf_graph_title, subtitle = subtitle)
    pdf(filename)
    print(plot_x)
    dev.off()
}

save_irf <- function(VAR_model,
                     filename,
                     impulse) {
    
    pdf(filename)
    print(plot(irf(VAR_model,
                   n.ahead = 20,
                   ci = 0.9,
                   boot = FALSE)))
    dev.off()
}

write_desc_stats_and_plot <- function(data,
                             filename_xlsx,
                             filename_plot,
                             country) {
    
    desc <- pastecs::stat.desc(data)
    # View(new_data_country)
    openxlsx::write.xlsx(desc, file = filename_xlsx, rowNames = TRUE)
    
    d <- data.frame(year = data$year, value = data$gdp, name = 'gdp')
    for (column in names(data)[-c(1)]) {
        d <- rbind(d, data.frame(year = data$year,
                                 value = data[[column]],
                                 name = column))
    }
    names(d) <- c('year', 'value', 'name')
    
    plot_data <- ggplot(data = d, aes(x = year, y = value)) +
        geom_point() +
        xlab('Year') + ylab('Constant LCU (or Fixed broadband subscriptions (per 100 people))') +
        facet_wrap(~name, scales = 'free_y') +
        ggtitle("Data", subtitle = country)
    pdf(filename_plot)
    print(plot_data)
    dev.off()
}

# Main function ----------

main <- function(new_data_country, num_of_possible_lags, country) {
    
    # Plot ACF for input data ---------- DONE!!!
    
    # titles <- c('logGDP', 'logCapital', 'logLabor', 'logSE', 'logRD', 'logFix',
    #             'dlogGDP', 'dlogCapital', 'dlogLabor', 'dlogSE', 'dlogRD', 'dlogFix',
    #             'dGDP', 'dCapital', 'dLabor',  'dSE', 'dRD', 'dFix')
    # cols <- c('gdp', 'k', 'l', 'se', 'rd', 'fix',
    #           'gdp', 'k', 'l', 'se', 'rd', 'fix',
    #           'gdp', 'k', 'l',  'se', 'rd', 'fix')
    # 
    # subtitle_pattern <- "%s %s" # %country %title
    # filename_pattern <- "newdata/%s/%s_plot_%s.pdf" # %country %country %title
    
    # DESC ----- DONE!!!
    # write_desc_stats_and_plot(new_data_country,
    #                           filename_xlsx = "newdata/%s/%s_desc.xlsx" %format% c(country, country),
    #                           filename_plot = "newdata/%s/%s_data.pdf" %format% c(country, country),
    #                           country = country)
    # 
    # for (title_num in 1:length(titles)) {
    #     filename <- filename_pattern %format% c(country, country, titles[title_num])
    #     subtitle <- subtitle_pattern %format% c(country, titles[title_num])
    #     data_vector <- new_data_country[[cols[title_num]]]
    #     
    #     save_acf(data_vector = data_vector,
    #              subtitle = subtitle,
    #              filename = filename)
    # }

    # Granger causality -----------

    # data_to_test <- data.frame(log_gdp_d = c(NA, diff(log(new_data_country$gdp), lag = 1)),
    #                            log_capital_d = c(NA, diff(log(new_data_country$k), lag = 1)),
    #                            log_labor_d = c(NA, diff(log(new_data_country$l), lag = 1)),
    #                            ict_d = c(NA, diff(new_data_country$ict, lag = 1)))
    # data_to_test <- data_to_test[-c(1), ]
    # do_granger(data_to_test = data_to_test,
    #            num_of_possible_lags = num_of_possible_lags)
    # another_data_to_test <- data.frame(gdp_d = c(NA, diff(new_data_country$gdp, lag = 1)),
    #                                    capital_d = c(NA, diff(new_data_country$k, lag = 1)),
    #                                    labor_d = c(NA, diff(new_data_country$l, lag = 1)),
    #                                    ict_d = c(NA, diff(new_data_country$ict, lag = 1)))
    # another_data_to_test <- another_data_to_test[-c(1), ]
    # do_granger(data_to_test = another_data_to_test,
    #            num_of_possible_lags = num_of_possible_lags)
    # data_to_test <- data.frame(log_gdp_d = c(NA, diff(log(new_data_country$gdp), lag = 1)),
    #                            log_capital_d = c(NA, diff(log(new_data_country$k), lag = 1)),
    #                            log_labor_d = c(NA, diff(log(new_data_country$l), lag = 1)),
    #                            ict_d = c(NA, diff(log(new_data_country$ict), lag = 1)))
    # data_to_test <- data_to_test[-c(1), ]
    # do_granger(data_to_test = data_to_test,
    #            num_of_possible_lags = num_of_possible_lags)

    
    # VECM ----------

    vecm_data <- data.frame(row.names = new_data_country$year)
    vecm_data$gdp <- new_data_country$gdp
    vecm_data$fix <- new_data_country$fix
    vecm_data$se <- new_data_country$se
    vecm_data$rd <- new_data_country$rd
    vecm_data$cuip <- new_data_country$cuip
    vecm_data$capital <- new_data_country$k
    vecm_data$labor <- new_data_country$l
    vecm_data_log <- log(scale(vecm_data) + 1)
    optimal_VAR <- VARselect(vecm_data_log,
                             lag.max = num_of_possible_lags[length(num_of_possible_lags)],
                             type = "const")
    optimal_VAR_lag <- max(max(optimal_VAR$selection) - 1, 2)
    coint_relations <- ca.jo(vecm_data_log,
                             type = "eigen",
                             ecdet = "const",
                             K = optimal_VAR_lag,
                             spec = "longrun")
    z <- summary(coint_relations)
    coint_rank <- 3
    bool_test <- as.data.frame(z@cval > z@teststat)
    bool_test
    count_rank <- 0
    for (row in NROW(bool_test):1) {
        a <- bool_test[row, ]
        condition <- which(TRUE == a)
        if (length(condition)) {
            coint_rank <- count_rank
            count_rank <- 0
            break
        }
        count_rank <- count_rank + 1
    }
    if (coint_rank == 0) {
        print(country)
        print("MODEL UNACHIEVABLE -- COINTEGRATION RANK IS ZERO")
    } else {
        vecm_fit <- cajorls(coint_relations,
                            r = coint_rank)
        # print(vecm_fit)
        # print(summary(vecm_fit$rlm))
        to_var <- vec2var(coint_relations, r = coint_rank)

        # Plot shock tests ------ DONE!!!
        
        # irf_filename_pattern <- "newdata/%s/VECM/%s_IRF_plot.pdf" # %country %country
        # save_irf(filename = irf_filename_pattern %format% c(country, country),
        #              VAR_model = to_var,
        #              impulse = col)

        # FEVD WILL WORK FOR MULTIPLE ICT --- DONE!!!
        # fevd_filename_pattern <- "newdata/%s/VECM/%s_FEVD_plot.pdf" # %country %country
        # pdf(fevd_filename_pattern %format% c(country, country))
        # print(plot(fevd(to_var), asp = 10))
        # dev.off()
        #
        # plotres(coint_relations)

        # Tests -----------


        # print(serial.test(to_var, type = "PT.asymptotic"))
        # print(arch.test(to_var))
        # print(normality.test(to_var))
    }

}



# Run ----------


countries <- c()
years <- 1990:2021
currencies_file_name <- "../Exchange_Rates_Selected.xlsx"
countries_tags <- c("BRA", "CHN", "IND", "RUS", "ZAF")
num_of_possible_lags <- c(1, 2, 3)
lag_max <- years[length(years)] - years[1]
acf_graph_title <- "Auto-Correlation Function"

for (coun in countries_tags) {
    all_country_data <- c('%sse' %format% c(coun),
                          '%srd' %format% c(coun),
                          '%sfix' %format% c(coun))
    col_lst = list()
    for (country_num in 1:length(all_country_data)) {
        data_to_process <- etl_step0(all_country_data[country_num],
                                     years = years,
                                     lag_max = lag_max,
                                     currencies_file_name,
                                     countries_tags)
        col_lst[[country_num]] = data_to_process
    }
    data_to_model <- etl_step1_combine_ict_factors(col_lst)
    curret_country <- substring(all_country_data[1], 1, 3)
    print("_____ CURRENT COUNTRY _____")
    print(curret_country)
    print("___________________________")
    main(new_data_country = data_to_model,
             num_of_possible_lags = num_of_possible_lags,
             country = coun)

}


