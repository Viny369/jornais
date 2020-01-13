baixar_paginas_oguaira <- function(diretorio = ".",caderno = "cidade", paginas = 10){
  
  url <- paste0("https://oguaira.com.br/noticias/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

b <- baixar_paginas_oguaira


ler_paginas_oguaira <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="col-md-8 col-sm-7 col-xs-12 ultima-not2-tit"]//a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2[@class="cor-noticias-cidade"]') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//i[@style="color: #ccc"]') %>% 
    xml2::xml_text()
  
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="col-md-8 col-sm-7 col-xs-12 ultima-not2-tit"]//p') %>% 
    xml2::xml_text() 
  
  tibble::tibble(headline,data_publicacao,descricao,url)
    
  }),NULL))
  
}

ogr <- ler_paginas_oguaira(arquivos = arquivos)


baixar_noticias_oguaira <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_oguaira_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_oguaira(ogr$url[1:50],"noticias")



ler_noticias_oguaira <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="tit-not"]') %>% 
         xml2::xml_text()
         
    
    descricao <- x %>% 
      xml2::xml_find_all('//p/em') %>% 
      xml2::xml_text()
    
   corpo <- x %>% 
      xml2::xml_find_all('//div[@class="col-md-12 col-sm-12 col-xs-12"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="col-md-12 col-sm-12 col-xs-12 data-post"]') %>% 
     xml2::xml_text() %>% 
     stringr::str_sub(start = 8, end = 30)
   
   
   tibble::tibble(headline,corpo,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_oguaira(arquivos = arquivos)
