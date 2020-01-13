baixar_paginas_amazonasatual <- function(diretorio = ".",caderno = "politica", paginas = 10){
  
  url <- paste0("https://amazonasatual.com.br/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_amazonasatual(diretorio = "pagina")


ler_paginas_amazonasatual <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="jeg_main_content jeg_column col-sm-8"]//h3[@class="jeg_post_title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="jeg_main_content jeg_column col-sm-8"]//h3[@class="jeg_post_title"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="jeg_main_content jeg_column col-sm-8"]//div[@class="jeg_post_excerpt"]/p') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="jeg_main_content jeg_column col-sm-8"]//div[@class="jeg_meta_date"]//a//following-sibling::text()') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

ama <- ler_paginas_amazonasatual(arquivos = arquivos)  


baixar_noticias_amazonasatual <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_amazonasatual_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_amazonasatual(ama$url[1:10],"noticias")



ler_noticias_amazonasatual <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="jeg_post_title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content no-share"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="meta_left"]//div[@class="jeg_meta_date"]/a[1]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_amazonasatual(arquivos = arquivos)
