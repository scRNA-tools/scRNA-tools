$(document).ready(function () {
  /* --Variables------------------------------------------------------------- */

  var catsContainer = $('#categories-list')
  var jsonPath = 'data/categories.json'

  /* --Functions------------------------------------------------------------- */

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

  function printList () {
    /* -- Open JSON file, parse the contents, loop through & print markup-- */

    $.ajaxSetup({
      cache: false
    })

    $.getJSON(jsonPath, function (data) {
      $.each(data, function (key, value) {
        /* -- Assign returned data -- */
        var category = value.Category
        var desc = value.Description
        var tools = value.Tools

        var entry = ''
        entry += '<div class="panel-heading">' +
                 '<h4 id="' + category + '" class="panel-title">' +
                 '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#' + category + '_c">' + category.replace(/([a-z])([A-Z])/g, '$1 $2')

        entry += '</a></h4></div>' +
                 '<div id="' + category + '_c" class="panel-collapse collapse">' +
                 '<ul class="list-group">'

        // Add description
        entry += '<li class="list-group-item"><h5>' + desc + '</h5></li>'

        // Loop over tools
        $.each(tools, function (k, val) {
          var name = val.Name
          var bioc = val.BioC
          var pypi = val.PyPI
          var cran = val.CRAN
          var github = val.Github

          entry += '<li class="list-group-item"><a href="tools.html#' + name + '">' + name + '</a>'

          if (typeof bioc !== 'undefined') {
            entry += ' <img border="0" height="15" src="http://bioconductor.org/shields/years-in-bioc/' + bioc + '.svg">' +
                     ' <img border="0" height="15" src="http://bioconductor.org/shields/downloads/' + bioc + '.svg">'
          }

          if (typeof cran !== 'undefined') {
            entry += ' <img border="0" height="15" src="http://www.r-pkg.org/badges/version/' + cran + '">' +
                     ' <img border="0" height="15" src="http://cranlogs.r-pkg.org/badges/grand-total/' + cran + '">'
          }

          if (typeof pypi !== 'undefined') {
            entry += ' <img border="0" height="15" src="https://img.shields.io/pypi/v/' + pypi + '.svg">' +
                     ' <img border="0" height="15" src="https://img.shields.io/pypi/pyversions/' + pypi + '.svg">' +
                     ' <img border="0" height="15" src="https://img.shields.io/pypi/status/' + pypi + '.svg">'
          }

          entry += '</li>'
        })

        entry += '</ul>' + '</div>'

        /* -- Add it to the list! -- */
        catsContainer.append(entry)
      })

      expandLinked()
    })
  }

  /* --Calls----------------------------------------------------------------- */

  printList()
})
