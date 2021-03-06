baixar_paginas_taboaoemfoco <- function(diretorio = ".",caderno = "politica", paginas = 10){
  
  url <- paste0("http://www.taboaoemfoco.com.br/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_taboaoemfoco(diretorio = "pagina")


ler_paginas_taboaoemfoco <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2[@class="cmsmasters_archive_item_title entry-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2[@class="cmsmasters_archive_item_title entry-title"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="cmsmasters_archive_item_content entry-content"]/p') %>% 
    xml2::xml_text() 
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//span[@class="cmsmasters_archive_item_date_wrap"]/abbr[@class="published cmsmasters_archive_item_date"]') %>% 
    xml2::xml_text()%>% 
    lubridate::dmy()
  
  data_atualização <- x %>% 
    xml2::xml_find_all('//span[@class="cmsmasters_archive_item_date_wrap"]/abbr[@class="dn date updated"]') %>% 
    xml2::xml_text()%>% 
    lubridate::dmy()
  
  
  tibble::tibble(headline,descricao,data_publicacao,data_atualização,url)
    
  }),NULL))
  
}

tef <- ler_paginas_taboaoemfoco(arquivos = arquivos)  


baixar_noticias_taboaoemfoco <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_taboaoemfoco_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_taboaoemfoco(tef$url[1:10],"noticias")



ler_noticias_taboaoemfoco <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="elementor-heading-title elementor-size-default"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="elementor-widget-container"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="elementor-icon-list-text elementor-post-info__item elementor-post-info__item--type-date"]') %>% 
     xml2::xml_text() %>% 
     lubridate::dmy()
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_taboaoemfoco(arquivos = arquivos)
