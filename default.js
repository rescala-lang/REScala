// wait for html to finish loading, as we need document.body
window.onload = function () {
	// listen for clicks on the document and close open dropdowns
	document.body.addEventListener('click', function(event) {
		var element = event.target;
		if (element === undefined || element.classList === undefined || !element.classList.contains("dropdown")) {
			Util.DOMQueryAll(".dropdown_content.show_content").forEach(function(element) {element.classList.remove("show_content")});
		}
	}, true);

	var manualTOC = document.body.querySelector("#toc");
	if (manualTOC != undefined) {
		var manualTOCitems = document.body.querySelectorAll("#main-content>h1,#main-content>h2,#main-content>h3,#main-content>h4");
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
			button.href = "#" + headline.id
			button.classList.add("list-group-item");
			button.style.paddingLeft = ((depth-1)*30)+10 + "px";
			currentList.appendChild(wrapInLi(button));
		});
	}
	window.onscroll = function () {
		updateArrowVisibility();
	};
	
	Util.DOMQueryAll("li").filter(function(li) {
		var next = li.nextElementSibling;
		return next != null && next.nodeName == "LI" && next.firstElementChild != null && next.firstElementChild.nodeName == "OL";
	}).forEach(function(li) {
		var c = document.createElement("span");
		c.classList.add("collapseIcon");
		c.addEventListener("click", function(e) {
			li.classList.toggle("collapsed");
			e.stopPropagation();
			e.preventDefault();
		}, true);
		li.firstElementChild.appendChild(c);
		li.classList.add("collapsible");
		
		var collapsibleItems = li.nextElementSibling.firstElementChild.querySelectorAll("a").length - 1;
		if (collapsibleItems > 0) {
			var hr = document.createElement("hr");
			hr.style.height = (collapsibleItems * 29) + "px";
			
			var node = li;
			var level = 1;
			while (node.id != "toc") {
				if (node.nodeName == "OL")
					level += 1;
				node = node.parentElement;
			}
			
			hr.style.left = (30 * level) + "px";
			li.appendChild(hr);
		}
	});
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
	var scrollArrow = Util.DOMQuery("#back-to-top");
	if (scrollArrow != undefined) {
		if (scrollY > 500)
			scrollArrow.classList.add("show");
		else
			scrollArrow.classList.remove("show");
	}
}

var Util = {
	toArray: function(e) {
		return Array.prototype.slice.call(e);
	},
	DOMQuery: function(s) {
		return document.querySelector(s);
	},
	DOMQueryAll: function(s) {
		return Util.toArray(document.querySelectorAll(s));
	}
}
