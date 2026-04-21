
# Deployment of App

```r
rsconnect::deployApp(
  appDir      = ".",
  appName     = "meal-planner",
  appTitle    = "Nutrition Tracker",
  account     = "dkenn",
  appPrimaryDoc = "run_app.R",
  forceUpdate = FALSE
)
```