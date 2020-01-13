baixar_paginas_apublica <- function(diretorio = ".",caderno = "reportagem", paginas = 10){
  
  url <- paste0("https://apublica.org/tipo/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}




#diretorio <- "pagina" e "noticias"

#arquivos <- list-files

#.x <- arquivos[NUMERO]


ler_paginas_apublica <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h4[@class="card-title"]/a') %>% 
        xml2::xml_attr("href")
  
  url2 <- x %>% 
    xml2::xml_find_all('//h2/a') %>% 
    xml2::xml_attr("href")  
  
  url <- c(url2,url)
  
  headline <- x %>% 
    xml2::xml_find_all('//h4/a') %>% 
    xml2::xml_text()
  
  headline2 <- x %>% 
    xml2::xml_find_all('//h2/a') %>% 
    xml2::xml_text() 
  
  headline <- c(headline2,headline)
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//p[@class="meta"]') %>% 
    xml2::xml_text()
   
  
  autor <- data_publicacao %>% 
    stringr::str_extract("(?<=por ).+")
  
  data_publicacao <- data_publicacao %>% 
    stringr::str_extract("\\X*?(?= \\|)")
  
  descricao <- x %>% 
    xml2::xml_find_all('//p[@class="card-text summary"]') %>% 
    xml2::xml_text() %>% 
    c("",.)
  
  
  
  tibble::tibble(headline,data_publicacao,descricao,autor,url)
    
  }),NULL))
  
}

apu <- ler_paginas_apublica(arquivos = arquivos) 


baixar_noticias_apublica <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_apublica_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}


baixar_noticias_apublica(apu$url,"noticias")



.x <- arquivos[3]

ler_noticias_apublica <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE) 
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@class="col col-md-10 mb-3"]/h1') %>% 
         xml2::xml_text()
         
    
   corpo <- x %>% 
      xml2::xml_find_all('//div[@class="post-contents"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="date"]') %>% 
     xml2::xml_text() %>% 
     stringr::str_extract(".+?\\d{4}")
     
   
   autor <- x %>% 
     xml2::xml_find_first('//a[@class="storyAuthor"]') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,corpo,data_publicacao,autor)
   
  }),NULL))
    
}




noticias <- ler_noticias_apublica(arquivos = arquivos)
