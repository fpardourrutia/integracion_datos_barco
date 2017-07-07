# base_output <- dbConnect(RSQLite::SQLite(), "~/Desktop/base_prueba_barco.sqlite")
# 
# # Insertando las tabla en la base
# dbWriteTable(base_output, "Project", bentos_project)
# dbWriteTable(base_output, "Site_sample", bentos_site_sample)
# 
# dbDisconnect(base_output)