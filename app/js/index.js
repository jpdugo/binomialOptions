export function addClick(targetElement, shinyInputName) {
  document.getElementById(targetElement).addEventListener('click', () => {
    Shiny.setInputValue(shinyInputName, Math.random());
  });
}
