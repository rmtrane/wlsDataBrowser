// Add tooltips for cells that are too narrow for text
function setTooltips() {
  // For each cell with class .rt-td-inner...
  document.querySelectorAll('.reactable .rt-td-inner').forEach(cell => {
    // ... if scrollWidth is greater than clientWidth...
    if (cell.scrollWidth > cell.clientWidth) {
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
  document.querySelectorAll('.freq-tables .rt-td-inner').forEach(cell => {
    // ... add the 'data-bs-original-title' for tooltip.
    cell.setAttribute('data-bs-original-title', 'Click for frequency table...');
  });
  // For each cell with class .rt-td-inner within container 
  // with class .copy-var...
  document.querySelectorAll('.copy-var .rt-td-inner').forEach(cell => {
    // ... add the 'data-bs-original-title' for tooltip.
    cell.setAttribute('data-bs-original-title', 'Click to copy variable to active document...');
  });
};

// When window is resized, set tooltips that depend on column widths
window.onresize = (event) => {
  console.log('Resizing...');
  // Add tooltips to columns depending on cell widths
  setTooltips();
  // Initiate tooltips to make sure any newly created tooltips are initiated
  $('#wlsData').find('.rt-td-inner[data-bs-original-title]').tooltip({container: 'body'});
};

// Show/hide spinner. Set value to true to show spinner, and false to hide spinner.
Shiny.addCustomMessageHandler('showSpinner', (value) => {
  if (value) {
    document.getElementById('content').classList.add('blur-background');
    document.getElementById('spinner').style.display = 'block';
  } else {
    document.getElementById('content').classList.remove('blur-background');
    document.getElementById('spinner').style.display = 'none';
  }
});

// Set input value to NULL
Shiny.addCustomMessageHandler('reset_input', (value) => {
  Shiny.setInputValue(value, null);
});

// Set input.data_path_missing to true
Shiny.addCustomMessageHandler('set_data_path_missing', (value) => {
  Shiny.setInputValue('data_path_missing', value);
});