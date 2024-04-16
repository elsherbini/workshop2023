console.log("Script 'keep_open.js' is being executed.");




document.addEventListener("DOMContentLoaded", function() {
  console.log("Script is running!");
  var workshop2Section = document.getElementById("quarto-sidebar-section-2");
  if (workshop2Section) {
      workshop2Section.classList.add("open");
  }
});