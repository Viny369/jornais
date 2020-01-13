baixar_paginas_clickguarulhos <- function(diretorio = ".",caderno = "policia-canais", paginas = 10){
  
  url <- paste0("https://www.clickguarulhos.com.br/canais/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}


b <- baixar_paginas_clickguarulhos

ler_paginas_clickguarulhos <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="td-ss-main-content"]//h3[@class="entry-title td-module-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="td-ss-main-content"]//h3[@class="entry-title td-module-title"]/a') %>% 
    xml2::xml_text()
  
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="td-ss-main-content"]//div[@class="td-excerpt"]') %>% 
    xml2::xml_text()%>% 
    stringr::str_sub(start = 22, end = 10000)
  
  autor <- x %>% 
    xml2::xml_find_all('//div[@class="td-ss-main-content"]//span[@class="td-post-author-name"]/a') %>% 
    xml2::xml_text() 
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="td-ss-main-content"]//time[@class="entry-date updated td-module-date"]') %>% 
    xml2::xml_text() %>% 
    stringr::str_sub(start = 16, end = 40)
  
  tibble::tibble(headline,descricao,data_publicacao,autor,url)
    
  }),NULL))
  
}

click <- ler_paginas_clickguarulhos(arquivos = arquivos) 


baixar_noticias_clickguarulhos <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"\\d.+") %>% 
     stringr::str_replace_all("\\W","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_clickguarulhos_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}


baixar_noticias_clickguarulhos(click$url,"noticias")



ler_noticias_clickguarulhos <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="td-post-content tagdiv-type"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="td-ss-main-content"]//time[@class="entry-date updated td-module-date"]') %>% 
     xml2::xml_text()
   
   autor <- x %>% 
     xml2::xml_find_first('//div[@class="td-post-author-name"]//a') %>% 
     xml2::xml_text()
  

   
   tibble::tibble(headline,texto,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_clickguarulhos(arquivos = arquivos)
