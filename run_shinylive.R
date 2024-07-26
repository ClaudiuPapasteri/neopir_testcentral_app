app_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)

shinylive::export(app_dir, "docs")  # use this output folder for Github Pages deploy

# httpuv::runStaticServer("docs")