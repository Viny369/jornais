baixar_paginas_justificando <- function(diretorio = ".",caderno = "noticias", paginas = 10){
  
  url <- paste0("https://www.justificando.com/categoria/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_justificando(diretorio = "pagina")


ler_paginas_justificando <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h4[@class="entry-title entry-title-left"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h4[@class="entry-title entry-title-left"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="entry"]/p') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//span[@class="entry-tdate"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

jf <- ler_paginas_justificando(arquivos = arquivos)  


baixar_noticias_justificando <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_justificando_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_justificando(jf$url,"noticias")



ler_noticias_justificando <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//span[@class="current"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="local_date"]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_justificando(arquivos = arquivos)
