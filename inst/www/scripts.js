// Add tooltips for cells that are too narrow for text
function setTooltips() {
  // For each cell with class .rt-td-inner...
  document.querySelectorAll('.reactable .rt-td-inner').forEach(cell => {
    // ... if scrollWidth is greater than clientWidth...
    if (cell.clientWidth < cell.scrollWidth) {
      // ... then add the 'data-bs-original-title' attribute, which is
      // used for the tooltip content...
      cell.setAttribute('data-bs-original-title', cell.textContent);
    } else {
      // ... otherwise, remove the attribute so tooltip won't be shown.
      cell.removeAttribute('data-bs-original-title');
    }
  });
}

// Add tooltips with instructions for frequency tables and copy vars
function setInstructiveTooltips() {
  // For each cell with class .rt-td-inner within container 
  // with class .freq-tables...
  document.querySelectorAll('.freq-tables').forEach(cell => {
    // ... add the 'data-bs-original-title' for tooltip.
    cell.setAttribute('data-bs-original-title', 'Click for frequency table...');
  });
  // For each cell with class .rt-td-inner within container 
  // with class .copy-var...
  document.querySelectorAll('.copy-var').forEach(cell => {
    // ... add the 'data-bs-original-title' for tooltip.
    cell.setAttribute('data-bs-original-title', 'Click to copy variable to active document...');
  });
};

// When window is resized, set tooltips that depend on column widths
window.onresize = (event) => {
  // Add tooltips to columns depending on cell widths
  setTooltips();
  // Initiate tooltips to make sure any newly created tooltips are initiated
  $('#wlsData').find('[data-bs-original-title]').tooltip({container: 'body'});
};

// Show spinner. When shiny is busy, change display to 'block'
$(document).on('shiny:busy', function(event) {
  document.getElementById('spinner_overlay').style.display = "block";
  document.getElementById('spinner').style.display = 'block';
})

// Show spinner. When shiny is busy, change display to 'none'
$(document).on('shiny:idle', function(event) {
  document.getElementById('spinner_overlay').style.display = "none";
  document.getElementById('spinner').style.display = 'none';
})

// Set input value. Takes a message with two elements. Essentially does `input${message.var} <– message.val`
Shiny.addCustomMessageHandler('set_input_value', (message) => {
  Shiny.setInputValue(message.var, message.val);
});
