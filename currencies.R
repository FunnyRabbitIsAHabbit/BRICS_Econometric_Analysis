library(zoo)

`%format%` <- function(x, y) {
    
    do.call(sprintf, c(list(x), y))
}

main <- function(country, years, currencies_file_name, tags) {
    # Get data ----------
    
    data_file_country <- "%s_data.xlsx" %format% c(country)
    sheet_names <- openxlsx::getSheetNames(data_file_country)
    analysis_data <- list()
    
    for (name in sheet_names) {
        analysis_data[[name]] <- t(openxlsx::read.xlsx(data_file_country, sheet = name))
    }
    
    currency_rates <- t(openxlsx::read.xlsx(currencies_file_name, sheet = "Sheet1"))
    
    new_data_country <- data.frame(year = years)
    currency_rates <- data.frame(currency_rates[2:NROW(currency_rates), ])
    names(currency_rates) <- tags
    
    for (column in names(currency_rates)) {
        currency_rates[[column]] <- as.double(currency_rates[[column]])
    }
    
    for (column in names(analysis_data)) {
        y <- analysis_data[[column]] 
        analysis_data[[column]] <- exp(na.approx(log(as.double(y[2:NROW(y), ]))))
    }
    
    
    label <- substring(country, 1, 3)
    new_data_country$ict <- analysis_data$ict * currency_rates[[label]]

    openxlsx::write.xlsx(new_data_country,
                         file = '../%s_data.xlsx' %format% c(country),
                         row.names = TRUE)
}

countries <- c()
years <- 1990:2021
currencies_file_name <- "../Exchange_Rates_Selected.xlsx"
countries_tags <- c("BRA", "CHN", "IND", "RUS", "ZAF")

for (coun in countries_tags) {
    countries <- append(countries, '%scuip' %format% c(coun))
    countries <- append(countries, '%sse' %format% c(coun))
}
print(countries)

for (col in countries) {
    main(col, years, currencies_file_name, countries_tags)
}
