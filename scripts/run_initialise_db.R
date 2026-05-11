# Run initialise_db() against the live Postgres database.
#
# Non-destructive: every CREATE uses IF NOT EXISTS, and the seed insert
# uses ON CONFLICT DO NOTHING. Existing tables and their data are left
# untouched. Run this from the project root so the seed CSV at
# data/measurement_data_types.csv resolves correctly.

lapply(list.files("R", full.names = TRUE), FUN = source)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("NUTRITION_DB_HOST"),
  dbname   = Sys.getenv("NUTRITION_DB_NAME"),
  user     = Sys.getenv("NUTRITION_DB_USER"),
  password = Sys.getenv("NUTRITION_DB_PASSWORD"),
  port     = 5432,
  sslmode  = "require"
)

on.exit(DBI::dbDisconnect(con), add = TRUE)

DBI::dbGetQuery(con, "SELECT 1 AS ok")

tables_before <- DBI::dbListTables(con)
message("Tables before: ", paste(sort(tables_before), collapse = ", "))

initialise_db(con)

tables_after <- DBI::dbListTables(con)
message("Tables after:  ", paste(sort(tables_after), collapse = ", "))

new_tables <- setdiff(tables_after, tables_before)
message("Newly created: ",
        if (length(new_tables)) paste(new_tables, collapse = ", ") else "(none)")

seed_rows <- DBI::dbGetQuery(
  con,
  "SELECT data_type_id, label, base_type, ordered FROM measurement_data_type
   ORDER BY data_type_id"
)
print(seed_rows)
