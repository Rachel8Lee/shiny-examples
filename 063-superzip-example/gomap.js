<script src="leaflet-svg-shape-markers/dist/leaflet-svg-shape-markers.min.js"></script>

// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var site = $el.data("site");
  $($("#nav a")[0]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    site: site,
    nonce: Math.random()
  });
});
