baixar_paginas_jornalvozdeibiuna <- function(diretorio = ".",caderno = "noticias", paginas = 10){
  
  url <- paste0("http://jornalvozdeibiuna.com.br/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_jornalvozdeibiuna(diretorio = "pagina")


ler_paginas_jornalvozdeibiuna <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="category3-text"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="category3-text"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="category3-text"]/p') %>%  
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//ul[@class="category3"]//ul[@class="headlines-info"]/li[1]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

jvi <- ler_paginas_jornalvozdeibiuna(arquivos = arquivos)  


baixar_noticias_jornalvozdeibiuna <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalvozdeibiuna_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_jornalvozdeibiuna(jvi$url[1:10],"noticias")



ler_noticias_jornalvozdeibiuna <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@id="main"]//h1[1]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@id="content-area"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n") %>% 
     stringr::str_sub(start = 1, end = -11)
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@id="post-info-left"]') %>% 
     xml2::xml_text()%>% 
     stringr::str_sub(start = 20, end = -6)
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_jornalvozdeibiuna(arquivos = arquivos)
