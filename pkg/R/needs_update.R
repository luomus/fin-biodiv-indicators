needs_update <- function(index, filter, table, db) {

  last_mod_date <- last_mod(filter)

  last_cached_on <- last_cached(index, table, db)

  last_mod_date > last_cached_on

}
