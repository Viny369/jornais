baixar_paginas_tribunadeituverava <- function(diretorio = ".",caderno = "policia", paginas = 10){
  
  url <- paste0("http://www.tribunadeituverava.com.br/categoria/1o-caderno/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_tribunadeituverava(diretorio = "pagina")


ler_paginas_tribunadeituverava <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2[@class="entry-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2[@class="entry-title"]/a') %>% 
    xml2::xml_text()

  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="revistaweb4-content-left col-md-9"]//span[@class="tp-post-item-date"]/i[@class="fa fa-calendar"]/following-sibling::text()') %>% 
    xml2::xml_text() %>% 
    lubridate::dmy()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

tdi <- ler_paginas_tribunadeituverava(arquivos = arquivos)  


baixar_noticias_tribunadeituverava <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_tribunadeituverava_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_tribunadeituverava(tdi$url[1:10],"noticias")



ler_noticias_tribunadeituverava <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@class="col-md-12"]/article/h2') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="col-md-12"]/article//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_atualizacao <- x %>% 
     xml2::xml_find_all('//span[@class="tp-post-item-date"]/i[@class="fa fa-refresh"]//following-sibling::text()') %>% 
     xml2::xml_text()%>%
     stringr::str_sub(start = 14, end = 23)%>% 
     lubridate::dmy()
   
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="entry-meta-inner"]/span[@class="tp-post-item-date"]/i[@class="fa fa-calendar"]/following-sibling::text()') %>% 
     xml2::xml_text() %>%
     stringr::str_sub(-11)%>% 
     lubridate::dmy()

   
   tibble::tibble(headline,texto,data_publicacao,data_atualizacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_tribunadeituverava(arquivos = arquivos)
