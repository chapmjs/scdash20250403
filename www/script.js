// Supply Chain Dashboard JavaScript enhancements

$(document).ready(function() {
  // Add visual feedback when cells are being edited
  $(document).on('click', 'table.dataTable tbody td', function() {
    $(this).addClass('cell-editing');
  });
  
  $(document).on('blur', 'table.dataTable tbody td', function() {
    $(this).removeClass('cell-editing');
  });
  
  // Confirmation dialog for Save/Update button
  $('#saveData').on('click', function(e) {
    if (!window.confirm('Are you sure you want to save changes? This will update the source data.')) {
      e.stopPropagation();
      return false;
    }
  });
  
  // Auto-update based on current date
  function checkDateForUpdate() {
    // Get the current date display
    var currentDisplayDate = $('#currentMonthDisplay').text().trim();
    
    // Get actual current date (1st of current month)
    var now = new Date();
    var currentMonth = new Date(now.getFullYear(), now.getMonth(), 1);
    var currentMonthFormatted = currentMonth.toLocaleString('default', { month: 'long', year: 'numeric' });
    
    // If dates don't match, trigger retrieve data
    if (currentDisplayDate && currentDisplayDate !== currentMonthFormatted) {
      // Reload the page to get fresh data with new date
      location.reload();
    }
  }
  
  // Check date once when page loads
  setTimeout(checkDateForUpdate, 2000);
  
  // Check date periodically (every hour)
  setInterval(checkDateForUpdate, 3600000);
  
  // Highlight shortage warnings with color coding
  function updateShortageHighlighting() {
    $('#shortageWarningTable table tbody tr').each(function() {
      var $row = $(this);
      var $statusCell = $row.find('td:contains("SHORTAGE")');
      
      if ($statusCell.length) {
        $row.addClass('shortage-warning');
      } else {
        $row.addClass('good-supply');
      }
    });
  }
  
  // Apply shortage highlighting whenever the table refreshes
  // Using MutationObserver to detect when the table content changes
  var shortageTableObserver = new MutationObserver(function(mutations) {
    mutations.forEach(function(mutation) {
      if (mutation.type === 'childList') {
        updateShortageHighlighting();
      }
    });
  });
  
  // Start observing the shortage warning table container
  var shortageTableContainer = document.getElementById('shortageWarningTable');
  if (shortageTableContainer) {
    shortageTableObserver.observe(shortageTableContainer, { childList: true, subtree: true });
  }
  
  // Add helper function to format numbers appropriately in editable cells
  function formatNumberInCell(cell, value) {
    // If it's a numeric value
    if (!isNaN(parseFloat(value)) && isFinite(value)) {
      // Format based on column type (can be expanded as needed)
      if (cell.hasClass('currency')) {
        return '$' + parseFloat(value).toFixed(2);
      } else if (cell.hasClass('percentage')) {
        return parseFloat(value).toFixed(1) + '%';
      } else {
        return parseFloat(value).toFixed(0);
      }
    }
    return value;
  }
  
  // Apply formatting to cells after edit
  $(document).on('change', '.cell-editing', function() {
    var value = $(this).text();
    $(this).text(formatNumberInCell($(this), value));
  });
});