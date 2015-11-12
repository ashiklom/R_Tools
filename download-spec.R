query.from.list <- function(l){
    nl <- length(l)
    l.names <- names(l)
    l.strings <- lapply(l, function(x) paste0(sprintf('"%s"', x), collapse=" , "))
    query.string <- sprintf('"%s": {"$in": [%s]}', l.names, l.strings)
    objects.string <- paste0(rep("{%s}", nl), collapse=",\n")
    objects.filled <- do.call(sprintf, c(as.list(objects.string), query.string))
    master.string <- sprintf("[\n %s \n]\n", objects.filled)
    return(master.string)
}

download.from.db <- function(id, query){
    require(RCurl)
    require(data.table)
    qjson <- minify(query)
    qjson.url <- URLencode(qjson)
    query.url <- sprintf("http://ecospectra.org/rest/download?package_id=%s&filters=%s&metadata=true", 
                         id, qjson.url)
    spec <- fread(getURL(query.url), header=TRUE)
    return(spec)
}

search.db.id <- function(query){
    require(jsonlite)
# Format query
    qjson <- minify(query)
    qjson.url <- URLencode(qjson)
# Get package IDs
    db.url <- sprintf("http://ecospectra.org/rest/query?filters=%s", qjson.url)
    db.raw <- fromJSON(db.url)
    if(db.raw$total == 0){
        warning("No datasets matched")
        return()
    }
    db.items <- db.raw$items
    db.dat <- flatten(db.items)
    db.out <- db.dat[, c("_id", "ecosis.package_title", "ecosis.package_name", "ecosis.spectra_count")]
    return(db.out)
}

spectra.from.query <- function(query){
    require(jsonlite)
    
# Get database IDs matching query
    db.dat <- search.db.id(query)
    if(is.null(db.dat)) {
        warning("No spectra retrieved because no datasets matched")
        return()
    }
    db.ids <- db.dat[,"_id"]
    db.names <- db.dat[, "ecosis.package_name"]

# Download all relevant data sets and store as list of data.tables
    spec.list <- lapply(db.ids, download.from.db, query=query)
    names(spec.list) <- db.names
    return(spec.list)
}


# An example
l <- list("Latin Genus" = c("Tsuga", "Abies"),
          "Latin Species" = c("canadensis", "balsamea"))
query <- query.from.list(l)
dat <- spectra.from.query(query)

