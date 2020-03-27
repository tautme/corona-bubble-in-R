var lati = 36;
var lon = -94;

var R = 3959;
var circumference = 2 * Math.PI * R;

var D_lat_1_deg = circumference / 360;
var D_lat_1_deg = D_lat_1_deg.toFixed(2);

var lat = Math.abs(lati);


var radian = (90 - lat) * (2 * Math.PI / 360);
var R_lat = Math.sin(radian) * R;
var circumference_lat = 2 * Math.PI * R_lat;

var D_lon_1_deg = circumference_lat / 360;

var D_lon_1_deg = D_lon_1_deg.toFixed(2);

document.getElementById("demo").innerHTML =
"Convert a degree in coordinates on Earth to miles, at Longitude " +
lon + " degrees and Latitude " + lati + " degrees" +
". <br>One degree Latitude on Earth is: " + 
D_lat_1_deg + " miles. <br>" + 
"One degree Longitude on Earth, at " + 
lat + " degrees Latitude, is: " + D_lon_1_deg + " miles. <br>" +
"Then filter Longitude from " + (lon - 3) + " to " + (lon + 3) + " degrees. <br> And Latitude from " +
(lati - 3) + " to " + (lati + 3) + " degrees.";