library(plyr)

### Functions and data for eBird Rarity Viewer ###

# Function to sort a list taxonomically
taxify <- function(birds){
  toSort = c()
  for(i in 1:length(birds)){
    taxNums = match(birds[i], tax$PRIMARY_COM_NAME)
    toSort[i] = taxNums
  }
  df = data.frame(birds, toSort)
  sorted = arrange(df, df$toSort)
  output = as.character(sorted$birds)
  return(output)
}

# Pull from second API
api2 = function(regionCode, back){
  url = paste('https://ebird.org/ws2.0/data/obs/', regionCode, 
              '/recent/notable?detail=full&key=phvesltiq609&back=',back, sep = "")
  data = fromJSON(readLines(url, warn=FALSE))
}

# Concatenate checklist urls for popups
subIDurl = function(subID){
  paste0("<a href = 'http://www.ebird.org/ebird/view/checklist/", 
         subID,"' target='_blank'","> ", subID," </a>",collapse = "")}

# Adding Google analytics
gtag = function(){
  "<!-- Global site tag (gtag.js) - Google Analytics -->
  <script async src='https://www.googletagmanager.com/gtag/js?id=UA-111749651-1'></script>
  <script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());
  
  gtag('config', 'UA-111749651-1');
  </script>"
}

# Adding user geolocation code
geoloc = function(){
  '
  $(document).ready(function () {
  
  function getLocation(callback){
  var options = {
  enableHighAccuracy: true,
  timeout: 5000,
  maximumAge: 0
  };
  
  navigator.geolocation.getCurrentPosition(onSuccess, onError);
  
  function onError (err) {
  Shiny.onInputChange("geolocation", false);
  }
  
  function onSuccess (position) {
  setTimeout(function () {
  var coords = position.coords;
  var timestamp = new Date();
  
  console.log(coords.latitude + ", " + coords.longitude, "," + coords.accuracy);
  Shiny.onInputChange("geolocation", true);
  Shiny.onInputChange("lat", coords.latitude);
  Shiny.onInputChange("long", coords.longitude);
  Shiny.onInputChange("accuracy", coords.accuracy);
  Shiny.onInputChange("time", timestamp)
  
  console.log(timestamp);
  
  if (callback) {
  callback();
  }
  }, 1100)
  }
  }
  
  var TIMEOUT = 3000; //SPECIFY
  var started = false;
  function getLocationRepeat(){
  //first time only - no delay needed
  if (!started) {
  started = true;
  getLocation(getLocationRepeat);
  return;
  }
  
  setTimeout(function () {
  getLocation(getLocationRepeat);
  }, TIMEOUT);
  
  };
  
  getLocationRepeat();
  
  });
  '
}