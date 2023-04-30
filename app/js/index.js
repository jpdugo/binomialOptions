export function addClick(targetElement, shinyInputName) {
  $(document).ready(() => {
   document.addEventListener('click', () => {
     if ($(`#${targetElement}`).is(':visible')) {
        Shiny.setInputValue(shinyInputName, Math.random(), { priority: 'event' });
      }
  });
  });
}
