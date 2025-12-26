library(mainsequenceR)
library(reticulate)

use_virtualenv("C:/Users/Dell/mainsequence-sdk/.venv", required = TRUE)

Sys.setenv(TDAG_ENDPOINT = "https://dev-tsorm.ngrok.app")
Sys.setenv(MAINSEQUENCE_TOKEN = "<token-from-manager>") #'I need token here
Sys.setenv(VFB_PROJECT_PATH = "<path-to-r-project>") #' After, I have the PATH

df <- ms_data_node_read_by_identifier(
  node_identifier = "some-node-id",
  start = "2024-01-01",
  end   = "2024-03-01"
)

head(df)
