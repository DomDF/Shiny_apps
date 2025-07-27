# Script to prepare for Posit Cloud deployment
# Run this script to generate/update the manifest.json file

# Install rsconnect if not already installed
if (!requireNamespace("rsconnect", quietly = TRUE)) {
  install.packages("rsconnect")
}

library(rsconnect)

# Write the manifest file for the prior_pred_app
writeManifest(
  appDir = ".",
  appFiles = c("prior_pred_app.R", "app.R"),
  appPrimaryDoc = "prior_pred_app.R"
)

# Optional: If you want to deploy directly to Posit Cloud
# You'll need to configure your account first:
# rsconnect::setAccountInfo(
#   name = "YOUR_ACCOUNT_NAME",
#   token = "YOUR_TOKEN",
#   secret = "YOUR_SECRET"
# )

# Then deploy with:
# rsconnect::deployApp(
#   appDir = ".",
#   appFiles = c("prior_pred_app.R", "app.R", "manifest.json"),
#   appName = "prior-predictive-explorer"
# )

cat("Manifest file created successfully!\n")
cat("You can now deploy to Posit Cloud using the manifest.json file.\n")