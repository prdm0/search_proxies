library(stringr)
library(glue)
library(dplyr)

encontrando_proxies <- function(
    n = 1L, 
    nivel = "alto",
    tipo = "https",
    paises = NULL){
  
  nivel <- tolower(nivel)
  
  nivel <- dplyr::case_when(
    nivel == "alto"         ~ "High",
    nivel == "anonimo"      ~ "Anonymous",
    nivel == "transparente" ~ "Transparent"
  )

  if(is.null(paises)) 
    lista_paises <- ""
  else 
    lista_paises <- glue("--countries {paste(paises, collapse = ' ')}")
  
  command <- 
    "docker run bluet/proxybroker2 find --types {tipo}\\
     --lvl {nivel} {lista_paises} --strict -l {n}
    " |> glue()

  proxy <-
    system(
      command = command,
      intern = TRUE,
      ignore.stderr = TRUE
    ) |> 
      as.character() |> 
      stringr::str_extract("\\d+.\\d+.\\d+.\\d+:\\d+")
  
  ip <- proxy |> stringr::str_extract("\\d+.\\d+.\\d+.\\d+")
  porta <- proxy |>
    stringr::str_remove(glue("{ip}:"))
  
  list(ip = ip, porta = as.integer(porta))
}

proxy <- encontrando_proxys()

# Testando o acesso ao site da linguagem
httr::GET(
  "https://www.r-project.org",
  httr::use_proxy(
    url = proxy$ip,
    port = proxy$porta
  )
)


