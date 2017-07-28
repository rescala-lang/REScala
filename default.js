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

/**
 * add a collapsibleIcon to the collapsible element
 * @param {DOMElement} header - The header that is always visible
 * @param {DOMElement} iconParent - Where the Icon should be placed
 */
	function addCollapseIcon(header, iconParent) {
		var c = document.createElement("span");
		c.classList.add("collapseIcon");
		c.addEventListener("click", function(e) {
			header.classList.toggle("collapsed");
			updateTOClines();
			e.stopPropagation();
			e.preventDefault();
		}, true);
		iconParent.appendChild(c);
		header.classList.add("collapsible");
	}

/**
 * add a vertical lines to the collapsible element
 * @param {DOMElement} parentNode - The parent node of all elements that can be hidden by collapsing
 * @param {DOMElement} header - The header that is always visible
 */
	function addVerticalLines(parentNode, header) {
		var collapsibleItems = parentNode.querySelectorAll("a").length - 1;
		if (collapsibleItems >= 0) {
			var hr = document.createElement("hr");
			var node = header;
			var level = 1;
			while (node.id != "toc") {
				if (node.nodeName == "OL")
					level += 1;
				node = node.parentElement;
			}

			hr.style.left = (30 * level) + "px";
			header.appendChild(hr);
		}
	}

	Util.DOMQueryAll("li").filter(function(li) {
		var next = li.nextElementSibling;
		return next != null && next.nodeName == "LI" && next.firstElementChild != null && next.firstElementChild.nodeName == "OL";
	}).forEach(function(li) {
		addCollapseIcon(li, li.firstElementChild);
		addVerticalLines(li.nextElementSibling.firstElementChild, li);

	});
	updateTOClines();

	var toc = Util.DOMQuery("#toc");
	if (toc != null && Util.isWindowWidth(680)) {
		var tocHeader = toc.previousElementSibling
		addCollapseIcon(tocHeader, tocHeader);
		Util.DOMQueryAll(".collapsible").forEach(function(li) { li.classList.add("collapsed")});
	}
	var imgs = Util.DOMQueryAll("#slideshow img");
	if (imgs.length > 0) {
		var indicator = Util.DOMQuery("#slideshow-indicator");
		imgs.forEach(function(img, i) {
			var span = document.createElement("span");
			span.onclick = function() {reStartSlideTimer(); showSlide(i+1)};
			indicator.appendChild(span);
		});
		reStartSlideTimer();
		updateSlide();
	}
	var container = document.createElement("div");
	container.id = "info-box-container";
	var infoBoxes = Util.DOMQueryAll(".info-box");
	if (infoBoxes.length > 0) {
		infoBoxes[0].parentElement.insertBefore(container, infoBoxes[0]);
		infoBoxes.forEach(function(box) {
			var p = box.nextElementSibling;
			var div = document.createElement("div");
			box.classList.remove("info-box");
			div.classList.add("info-box");
			container.appendChild(div);
			div.appendChild(box);
			div.appendChild(p);
		});
		container.nextElementSibling.style = "clear: left;";
	}
}


function updateTOClines() {
	Util.DOMQueryAll("#toc hr").forEach(function(hr) {
		var li = hr.parentElement;
		var collapsibleItems = Util.QueryAll(li.nextElementSibling.firstElementChild,"a").filter(function(a) {
			return a.offsetParent !== null;
		}).length-1;
		hr.style.height = "0px";
		if (collapsibleItems >= 0) {
			hr.style.height = (collapsibleItems * 29) + "px";
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

var slideIndex = 1;
var slideTimer;

function nextSlide() {
	slideIndex += 1;
	updateSlide();
}

function showSlide(n) {
	slideIndex = n;
	updateSlide();
}

function updateSlide() {
	var imgs = Util.DOMQueryAll("#slideshow img");
	var indicators = Util.DOMQueryAll("#slideshow-indicator span");
	if (slideIndex > imgs.length)
		slideIndex = 1;
	if (slideIndex < 1)
		slideIndex = imgs.length;
	imgs.forEach(function(e) {
		e.style.display = "";
	});
	indicators.forEach(function(e) {
		e.classList.remove("current");
	});
	indicators[slideIndex-1].classList.add("current");
	imgs[slideIndex-1].style.display = "block";
}

function reStartSlideTimer() {
	if (slideTimer)
		clearInterval(slideTimer);
	slideTimer = setInterval(nextSlide, 6000);
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
	},
	Query: function(element, s) {
		return Util.toArray(element.querySelector(s));
	},
	QueryAll: function(element, s) {
		return Util.toArray(element.querySelectorAll(s));
	},
	isWindowWidth: function(size) {
		var mq = window.matchMedia( "(max-width: "+ size + "px)" );
		return mq.matches;
	}
}
