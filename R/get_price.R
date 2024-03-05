library(tidyverse)
library(rvest)


x <- config::get(file = "products.yml")

error_length_zero <- function(x){
  if(length(x) == 0) stop()
  return(x)
}
get_domain <- \(url) stringr::str_extract(url, "(?<=\\.)\\w+")

extractors <- list(
  maxipali = function(url){
    read_html(url) |> 
      html_elements(xpath = "//meta[@property = 'product:price:amount']") |> 
      html_attr("content") |> 
      as.numeric() |> 
      error_length_zero()
  },
  masxmenos = function(url){
    read_html(url) |> 
      html_elements(xpath = "//meta[@property = 'product:price:amount']") |> 
      html_attr("content") |> 
      as.numeric()|> 
      error_length_zero()
  },
  peridomicilio = function(url){
    read_html(url) |> 
      html_elements(xpath = "//meta[@itemprop = 'price']") |> 
      html_attr("content") |> 
      as.numeric()|> 
      error_length_zero()
  },
  walmart = function(url){
    read_html(url) |> 
      html_elements(xpath = "//meta[@property = 'product:price:amount']") |> 
      html_attr("content") |> 
      as.numeric()|> 
      error_length_zero()
  }
)



get_price <- function(url){
  domain <- get_domain(url)
  fun <- extractors[[domain]]
  fun(url)
}

get_price <- possibly(get_price, otherwise = NA)
get_price <- insistently(get_price, rate = rate_backoff(pause_base = 3, max_times = 3))

datos <- map(x, as_tibble) |> 
  list_rbind() |> 
  mutate(
    precio = map_vec(url, get_price, .progress = TRUE),
    fecha = today()
  )
