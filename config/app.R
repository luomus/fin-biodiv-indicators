library(shiny)
library(shinyAce)
library(shinyauthr)
library(bslib)

if (!file.exists("/home/user/var/config.yml")) {

  config <- file.copy("/home/user/config.yml", "/home/user/var/config.yml")

  stopifnot("Config not found" = config)

}

if (!file.exists("/home/user/var/pass.csv")) {

  write.csv(
    data.frame(user = character(), password = character()),
    "/home/user/var/pass.csv",
    quote = FALSE,
    row.names = FALSE,
  )

}

users <- read.csv("/home/user/var/pass.csv")

ui <- shinyUI(
  fluidPage(
    theme = bs_theme(version = 4, bootswatch = "darkly"),
    div(class = "pull-right", logoutUI(id = "logout")),
    loginUI(id = "login"),
    uiOutput("ui")
  )
)

server <- function(input, output, session) {

  credentials <- loginServer(
    id = "login",
    data = users,
    user_col = user,
    pwd_col = password,
    sodium_hashed = TRUE,
    log_out = reactive(logout_init())
  )

  logout_init <- logoutServer(
    id = "logout", active = reactive(credentials()[["user_auth"]])
  )

  txt <- readLines("/home/user/var/config.yml")

  output[["ui"]] <- renderUI({

    req(credentials()[["user_auth"]])

    fluidRow(
      column(
        6,
        h2("config.yml"),
        aceEditor("yml", txt, "yaml", "twilight"),
        actionButton("save", "Save")
      ),
      column(
        6,
        htmlOutput("validation")
      )
    )

  })

  output[["validation"]] <- renderUI({

    req(credentials()[["user_auth"]])

    input[["save"]]

    yml <- isolate(input[["yml"]])

    validate <- try(yaml::yaml.load(yml))

    if (inherits(validate, "list")) {

      writeLines(yml, "/home/user/var/config.yml")
      HTML("Valid")

    } else {

      HTML("Invalid!")

    }


  })

}

shinyApp(ui = ui, server = server)
