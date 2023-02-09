###############################
##
##    Short Blurb for 
##        subStat
##
###############################

blurb <- wellPanel(id="blurb",
  "Welcome to ",tags$b("KidStats: Stature"),"- a graphical user interface for stature estimation 
using the appendicular skeleton, integrated with KidStats. Reference data are currently a subset from the ",
  a(href="https://doi.org/10.3390/forensicsci2010003",
    "Subadult Virtual Anthropology Database.")," Ages range from birth 
to 21 years old. Current efforts are underway to expand the reference data and 
demographics.",hr(),"Results displayed in the ",tags$b("Output"),
  " tab are first filtered for testing accuracy >= 95% and subsequently ordered 
by increasing ",tags$b("MAD")," (Mean Absolute Deviation), which is a measure 
of the average error (in cm) for a given model."
)








