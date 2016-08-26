// wait for html to finish loading, as we need document.body
window.onload = function () {
	// listen for clicks on the document and close open dropdowns
	document.body.addEventListener('click', function(element) {
		if (element === undefined || element.classList === undefined || !element.classList.contains(".dropdown_content"))
		document.querySelectorAll(".dropdown_content.show_content").forEach(function(element) {element.classList.remove("show_content")});
	}, true);

	var manualTOCitems = document.body.querySelectorAll("#main-content>h1,#main-content>h2,#main-content>h3,#main-content>h4");
	var manualTOC = document.body.querySelector("#toc");
	var currentList = manualTOC;
	var currentDepth = 1;
	manualTOCitems.forEach(function (headline) {
		var depth = headline.nodeName == "H1" ? 1 : headline.nodeName == "H2" ? 2 : headline.nodeName == "H3" ? 3 : 4

		while (currentDepth < depth) {
			var subList = document.createElement("ol");
			currentList.appendChild(wrapInLi(subList));
			currentList = subList;
			currentDepth++;
		}
		while (currentDepth > depth) {
			currentList = currentList.parentNode.parentNode;
			currentDepth--;
		}
		// now we can assume that currentDepth === depth
		var button = document.createElement("a");
		button.innerHTML = headline.innerHTML;
		button.href = "#" + headline.id;
		currentList.appendChild(wrapInLi(button));
	});

	window.onscroll = function () {
		updateArrowVisibility();
	};
}

function dropdown(id) {
	var d = document.getElementById(id);
	var rect = d.previousElementSibling.getBoundingClientRect();
	// show the dropdown
	d.classList.toggle('show_content');
	// make sure it knows its place
	d.style.left = (rect.left-((rect.right-rect.left)/2)) + "px";
}

function wrapIn(element, wrapperTag) {
	var wrapperElement = document.createElement(wrapperTag);
	wrapperElement.appendChild(element);
	return wrapperElement;
}

function wrapInLi(element) {
	return wrapIn(element, "li");
}

function updateArrowVisibility() {
	var scrollArrow = document.body.querySelector("#back-to-top");
	if (scrollY > 500)
		scrollArrow.classList.add("show");
	else
		scrollArrow.classList.remove("show");
}
