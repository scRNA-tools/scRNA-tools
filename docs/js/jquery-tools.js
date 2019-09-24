$(document).ready(function () {

	/* --Variables------------------------------------------------------------- */

	var toolsContainer = $('#tools-list')
	var jsonPath = 'data/tools.json'
	var fixed_height = 200;
	var first_time = true;

	/* --Functions------------------------------------------------------------- */

	function panelInteractions(){

		function prettyGitDate(date_time){

			months = ["January", "February", "March", "April",
					  "May", "June", "July", "August",
					  "September", "October", "November", "December"];

			date = date_time.split("T")[0];
			date_elements = date.split("-");

			year = parseInt(date_elements[0]);
			month = parseInt(date_elements[1]);

			current_year = new Date().getFullYear();

			// Can make this more fancy if required, i.e. "last week, today!"
			if(current_year !== year){
				return(String(months[month-1]) + " " + String(year))
			} else {
				return(String(months[month-1]))
			}

		}

		function githubShields(panel, url){

			// split by forward slash, get username and package name

			url_split = url.split("/");
			username = url_split.slice(-2)[0];
			package_name = url_split.slice(-1)[0];

			json_location = "https://api.github.com/search/repositories?q=repo:" + username + "/" + package_name;

			// Fetch Lassy, then update the required fields

			console.log(json_location)

			$.getJSON(json_location).done(function(json) {
				forks = json.items[0].forks;
				stargazers = json.items[0].stargazers_count;
				last_commit = prettyGitDate(json.items[0].pushed_at);

				// Update panel to reflect requested information
				panel.find(".stars span.blue").text(stargazers)
				panel.find(".forks span.blue").text(forks)
				panel.find(".commits span.green").text(last_commit)

			}).fail(function( jqxhr, textStatus, error ) {
				var err = textStatus + ", " + error;
				console.log( "Request Failed: " + err );
			})

		}

		$(".tool").click(function(){

			package_name = $(this).find("h4").attr("id");

			panel_name = package_name + "_c";
			panel = $("#" + panel_name);
			package_last_commit = panel.find(".commits span.green");

			// Check to see whether this has been populated already... otherwise ping Github
			if(package_last_commit.text() == "Unknown"){
				codebase = panel.find(".codebase_url").text();
				githubShields(panel, codebase)
			}

			letterPositions();
		})

	}


	function expandLinked () {

		var url = document.location.toString()
		var hash = url.split('#')[1]

		if (typeof hash !== 'undefined') {
			var title = '#' + hash
			var panel = title + '_c'

			// collapse the expanded panel
			var allPanels = $('#accordion .accordion-collapse')

			allPanels.removeClass('in')
			allPanels.find('.accordion-toggle').addClass('collapsed')

			// expand the requested panel, change the title
			$(panel).addClass('in')
			$(title).find('.accordion-toggle').removeClass('collapsed')

			location.href = title
		}
	}


	function checkFixed(from_top){

		current_fixed = toolsContainer.hasClass("fixed_state");

		if(from_top > 200 && current_fixed != true){

			$("#manipulators").addClass("fixed");
			if($("#selectsort").val() == "name"){
				$("#name-bookmarks").addClass("fixed");
			}
			toolsContainer.addClass("fixed_state");

			// Now, find the current letter position

		} else if(from_top < 200 &&  current_fixed != false) {
			$("#manipulators").removeClass("fixed");
			$("#name-bookmarks").removeClass("fixed");
			toolsContainer.removeClass("fixed_state");
		}

	}

	function letterPositions(){

		alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
		window.alpha_positions = {}

		$.each(alphabet, function(index, letter){
			if($('#anchor' + letter).length > 0){
				window.alpha_positions[letter] = $('#anchor' + letter).offset().top;
			}
		})

	}

	function trackLetters(from_top){

		if(from_top + 10 < fixed_height){
			$("#name-bookmarks li").removeClass("active");

		} else {

			current_letter = false;

			$.each(window.alpha_positions, function(letter, position){
				if(from_top + 10 > position){
					current_letter = letter;
				}
			})

			$("#name-bookmarks li").removeClass("active");
			$(".letter-menu"+current_letter).addClass("active");

		}
	}

	$(window).scroll(function(){

		from_top = $(window).scrollTop();
		checkFixed(from_top);

		if($("#selectsort").val() == "name"){
			trackLetters(from_top);
		}

	})

	function printList (urlParams) {

		/* -- Open JSON file, parse the contents, loop through & print markup -- */

		$.ajaxSetup({
		  cache: false
		})

		$.getJSON(jsonPath, function(data) {

			/* -- Sort data -- */

			if (urlParams.has('sort')) {
				switch(urlParams.get('sort')) {
					case 'cites':
					data.sort(function(obj1, obj2) {
						return obj2.Citations - obj1.Citations
					})
					break
					case 'refs':
					data.sort(function(obj1, obj2) {
						return (obj2.Publications + obj2.Preprints) - (obj1.Publications + obj1.Preprints)
					})
					break
					case 'pubs':
					data.sort(function(obj1, obj2) {
						return obj2.Publications - obj1.Publications
					})
					break
					case 'pres':
					data.sort(function(obj1, obj2) {
						return obj2.Preprints - obj1.Preprints
					})
					break
					case 'added':
					data.sort(function(obj1, obj2) {
						var x = new Date(obj1.Added);
						var y = new Date(obj2.Added);
						return (y > x) - (y < x)
					})
					break
					case 'updated':
					data.sort(function(obj1, obj2) {
						var x = new Date(obj1.Updated);
						var y = new Date(obj2.Updated);
						return (y > x) - (y < x)
					})
					break
				}
			}

			// Setup tools columns ----------------

			tool_index = 1
			sort_method = $("#selectsort").val();

			if(sort_method == "name"){

				current_letter = false;
				$("#name-bookmarks").show();
				toolsContainer.addClass("name-sort");

			} else {
				$("#name-bookmarks").hide();
				toolsContainer.append('<div class="first-tools col-lg-6 col-md-12 col-sm-12 col-xs-12"></div>');
				toolsContainer.append('<div class="second-tools col-lg-6 col-md-12 col-sm-12 col-xs-12"></div>');
			}

			$.each(data, function (key, value) {

				/* -- Assign returned data -- */

				var tool_information = {
					"name" : value.Name,
					"doi" : value.DOIs,
					"doiURL" : value.DOIURL,
					"pubDate" : value.PubDates,
					"preprint" : value.Preprint,
					"citations" : value.Citations,
					"refs" : value.Refs,
					"description" : value.Description,
					"platform" : value.Platform,
					"code" : value.Code,
					"github" : value.Github,
					"added" : value.Added,
					"updated" : value.Updated,
					"license" : value.License,
					"cats" : value.Categories,
					"bioc" : value.BioC,
					"pypi" : value.PyPI,
					"cran" : value.CRAN,
					"nPubs" : value.Publications,
					"nPres" : value.Preprints,
					"totalRefs" : value.Publications + value.Preprints,
					"cats": value.Categories
				}

				entry = toolItem(tool_information)

				// If any categories are selected for filtering, pass non-matches.

				keep = false;

				selected_categories = $("[name=category_filter]").val();

				if($("[name=category_filter]").val().length > 0){
					$.each(selected_categories, function(index, cat){
						if(value.Categories.indexOf(cat) > -1){
							keep = true;
							return false;
						}
					})

					if(!keep){
						return;
					}
				}



				// Add it to the list

				if(sort_method == "name"){

					first_letter = tool_information["name"][0].toUpperCase()
					if(first_letter != current_letter){
						toolsContainer.append('<h3 id="anchor' + first_letter + '" class="tools-list">'+first_letter+'</h3>')
						toolsContainer.append('<div id="alpha' + first_letter + '-left" class="first-tools col-lg-6"></div>');
						toolsContainer.append('<div id="alpha' + first_letter + '-right" class="second-tools col-lg-6"></div>');

						current_letter = first_letter;
						tool_index = 1;
					}

					if(tool_index%2 == 0) {
						$('#alpha' + first_letter + '-right').append(entry)
					} else {
						$('#alpha' + first_letter + '-left').append(entry)
					}
				} else {
					if(tool_index%2 == 0) {
						$('.second-tools').append(entry);
					} else {
						$('.first-tools').append(entry)
					}
				}

				tool_index = tool_index + 1

			})

			if(sort_method == "name"){
				// Tool headings

				headings = $(".tools-list");
				//alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('');
				$.each(headings, function (index, heading) {
					letter = $(heading).html();
					$("#name-bookmarks").append('<li class="letter-menu' + letter + '"><a href="#anchor' + letter + '">' + letter + '</a></li>');
				})

			}

			expandLinked();
			panelInteractions();
			letterPositions();

		})
	}


	/* --Calls----------------------------------------------------------------- */


	// Check URL for queryies

	var urlParams = new URLSearchParams(window.location.search);

	if (urlParams.has('sort')) {
		$("[name=selectsort]").val(urlParams.get('sort')).change()
	}

	if (urlParams.has('cats')){

		cat_url = urlParams.get('cats').split(",");
		cats = [];

		$.each(cat_url, function(index, cat){
			cats.push(cat);
		})

		$("[name=category_filter]").val(cats);
	}

	// Refresh page on change on select or filter button

	function pushURL(){

		select_item = $("[name=selectsort]");

		// Sort Select
		var val = select_item.val()
		var sorter

		if (typeof val !== 'undefined') {
			sorter = val
		}

		// Check for cats
		selected_cats = $("[name=category_filter]").val();
		selected_cats = selected_cats.toString();

		// Check for anchors
		var url = document.location.toString();

		var hash
		if (url.includes('#')) {
			hash = url.split('#')[1]
			url = url.split('#')[0]
		}

		url = url.split('?')[0];

		if (hash !== undefined) {
			if(selected_cats.length > 0){
				window.location.href = url + '?sort=' + sorter + "&cats=" + selected_cats + '#' + hash
			} else {
				window.location.href = url + '?sort=' + sorter + '#' + hash
			}
		} else {
			if(selected_cats.length > 0){
				window.location.href = url + '?sort=' + sorter + "&cats=" + selected_cats
			} else {
				window.location.href = url + '?sort=' + sorter
			}
		}

		return true;
	}

	$(function(){
		$("[name=selectsort]").change(function(){
			pushURL();
		})
		$("#category_submit").click(function(){
			pushURL();
		});
	})

	// Refresh

	printList(urlParams)
})
