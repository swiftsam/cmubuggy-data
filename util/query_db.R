####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Betterment Database Querying Tools
###
### Purpose
###  * Takes a DB name and a SQL query and returns data
###
### Notes:
###  * Loads configuration and authentication information from a locally stored YAML file
###  * YAML Configuration: To connect to Beterment DBs, you need to use your username and password credentials.
###    To use this function, credentials should be stored in a db_creds.yml file.
###    Consider saving it in the default location: ~/src/db_creds.yml.
###    Run queryDB(query = "", yaml.path="") to see an example of the file structure.
###
### Example Usage
### QueryDB(query = "SELECT * FROM table WHERE id = 1000;",
###         db='datawarehouse')
###
### Primary Creator(s): Sam Swift (swift@betterment.com)
####~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(yaml)
require(RMySQL)

QueryDB <- function(query       = NULL,
                    db          = "cmubuggy",
                    yaml.path   = file.path("~","src","db_creds.yml"),
                    prompt.pass = FALSE,
                    return.type = "data.table",
                    silent      = FALSE){
  # Loads configuration and authentication information from a locally stored YAML
  #   file, creates a connection, and executes the requested query.
  #
  # Args:
  #   query: A string containining the full MySQL query
  #   db:    The name of the db you are connecting to (actually, the name of the
  #            yaml config block you wish to use)
  #   yaml.path: File path to the location of the yaml configuration file
  #              defining the db connection.  Default is ~/src/db_creds.yml
  #
  # Returns:
  #    Returns a data.frame of results as produced by DBI:::dbGetQuery()
  time.start = Sys.time()

  # check for YAML configuration file
  if(is.null(query)){
    stop("queryDB: query is null, but must be specified")
  }

  # load config / credentials for each locally configured DB
  if(file.exists(file.path(yaml.path))){
    config <- yaml.load_file(yaml.path)
  } else {
    stop("queryDB: YAML config file not found at: '", yaml.path,"'
         Your YAML file should include a block for each database you have configured, like this:
         datawarehouse:
           name : datawarehouse
           username : sswift
           password : WKj*****_J
           host : dwh-db.betterment.com
           port : 3306

         clientanalytics:
           name : clientanalytics
           username : sswift
           password : WKj*****_J
           host : dwh-db.betterment.com
           port : 3306")
  }

  # check that config file contains params for the specified DB
  if(is.null(config[[db]])){
    stop("queryDB: YAML file found, but doesn't contain param block for ", db)
  }

  # check that config block contains the mininum params
  required.params <- c("name","username","password","host","port")
  missing.params  <- setdiff(required.params,
                             names(config[[db]]))
  if(length(missing.params) > 0){
    stop("queryDB: YAML config for '", db,
         "' missing required parameter(s): ",
         paste(missing.params, collapse=", "))
  }

  # allow manual entry of passwords so that they don't need to be stored
  if(prompt.pass){
    config[[db]]$password <- readline(prompt = paste("Enter password for ",
                                                     config[[db]]$username,
                                                     "@", config[[db]]$host,
                                                     ": ", sep=""))
  }

  # create database connection
  db.con <- dbConnect(RMySQL::MySQL(),
                      dbname   = config[[db]]$name,
                      user     = config[[db]]$username,
                      password = config[[db]]$password,
                      host     = config[[db]]$host,
                      port     = config[[db]]$port)
  # run query
  result <- dbGetQuery(db.con, query)

  # close connection to DB
  dbDisconnect(db.con)

  if(return.type == "data.table"){
    require(data.table)
    result <- data.table(result)
  }

  # report time elapsed
  if(!silent){
    time.end   = Sys.time()
    message(Sys.time(), " QueryDB: query completed in ",
            round(difftime(time.end, time.start, units="mins"), 3),
            " minutes")
  }

  # return query results
  return(result)
}
