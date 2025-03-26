#' Define JS functions for inclusion in wlsDataBrowser()
my_js_functions <- function() {
  "function setTooltips() {
    const cells = document.querySelectorAll('.reactable .rt-td-inner');
    cells.forEach(cell => {
      if (cell.scrollWidth > cell.clientWidth) {
        cell.setAttribute('data-bs-original-title', cell.textContent);
      } else {
        cell.removeAttribute('data-bs-original-title');
      }
    });

    document.querySelectorAll('.freq-tables .rt-td-inner').forEach(cell => {
      cell.setAttribute('data-bs-original-title', 'Click for frequency table...');
    });
    document.querySelectorAll('.copy-var .rt-td-inner').forEach(cell => {
      cell.setAttribute('data-bs-original-title', 'Click to copy variable to active document...');
    });

  };

  window.onresize = (event) => {
    console.log('Resizing...');
    setTooltips();
  };

  Shiny.addCustomMessageHandler('showSpinner', function(value) {
    if (value) {
      document.getElementById('content').classList.add('blur-background');
      document.getElementById('spinner').style.display = 'block';
    } else {
      document.getElementById('content').classList.remove('blur-background');
      document.getElementById('spinner').style.display = 'none';
    }
  });

  Shiny.addCustomMessageHandler('reset_input', function(value) {
    Shiny.setInputValue(value, null);
  });
"
}
