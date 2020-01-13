baixar_paginas_oprogressodetatui <- function(diretorio = ".",caderno = "policiais", paginas = 10){
  
  url <- paste0("http://oprogressodetatui.com.br/n/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_oprogressodetatui(diretorio = "pagina")


ler_paginas_oprogressodetatui <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="meta-info-container"]/h3[@class="entry-title td-module-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="meta-info-container"]/h3[@class="entry-title td-module-title"]/a') %>% 
    xml2::xml_text() %>% 
    iconv("utf-8","latin1//TRANSLIT")

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="meta-info-container"]//div[@class="td-excerpt"]') %>% 
    xml2::xml_text() %>% 
    stringr::str_sub(22) %>% 
    iconv("utf-8","latin1//TRANSLIT")
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="td-left-meta"]//span[@class="td-post-date"]/time') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

opt <- ler_paginas_oprogressodetatui(arquivos = arquivos)  


baixar_noticias_oprogressodetatui <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_oprogressodetatui_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_oprogressodetatui(opt$url[1:10],"noticias")



ler_noticias_oprogressodetatui <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title"]') %>% 
         xml2::xml_text() %>% 
      iconv("utf-8","latin1//TRANSLIT")
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="td-ss-main-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="td-post-date td-post-date-no-dot"]/time') %>% 
     xml2::xml_text() %>% 
     lubridate::dmy()
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_oprogressodetatui(arquivos = arquivos)
