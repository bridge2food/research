window.onload = function() {
  // Initialize Bootstrap popovers
  var popoverTriggerList = [].slice.call(
    document.querySelectorAll('[data-bs-toggle="popover"]')
  );
  var popoverList = popoverTriggerList.map(function (popoverTriggerEl) {
    return new bootstrap.Popover(popoverTriggerEl);
  });

  // Function to simulate a click on the sidebar toggle button
  function clickSidebarToggle() {
    const toggleButton = document.querySelector('.bslib-sidebar-layout .collapse-toggle');
    if (toggleButton) {
      toggleButton.click();
    } else {
      // If the toggle button is not yet available, try again after a short delay
      setTimeout(clickSidebarToggle, 100);
    }
  }

  // Start the function to click the toggle button
  clickSidebarToggle();
};
