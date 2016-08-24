// wait for html to finish loading, as we need document.body
window.onload = function () {
	// listen for clicks on the document and close open dropdowns
	document.body.addEventListener('click', function(element) {
		if (element === undefined || element.classList === undefined || !element.classList.contains(".dropdown_content"))
		document.querySelectorAll(".dropdown_content.show_content").forEach(function(element) {element.classList.remove("show_content")});
	}, true);
}

function dropdown(id) {
	var d = document.getElementById(id);
	var rect = d.previousElementSibling.getBoundingClientRect();
	// show the dropdown
	d.classList.toggle('show_content');
	// make sure it knows its place
	d.style.left = (rect.left-((rect.right-rect.left)/2)) + "px";
}