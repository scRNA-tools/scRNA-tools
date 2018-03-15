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

  function printList (urlParams) {
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

        if (urlParams.has('sort')) {
          switch(urlParams.get('sort')) {
            case 'cites':
              tools.sort(function(obj1, obj2) {
                return obj2.Citations - obj1.Citations
              })
              break
            case 'refs':
              tools.sort(function(obj1, obj2) {
                return (obj2.Publications + obj2.Preprints) - (obj1.Publications + obj1.Preprints)
              })
              break
            case 'pubs':
              tools.sort(function(obj1, obj2) {
                return obj2.Publications - obj1.Publications
              })
              break
            case 'pres':
              tools.sort(function(obj1, obj2) {
                return obj2.Preprints - obj1.Preprints
              })
              break
            case 'added':
              tools.sort(function(obj1, obj2) {
                var x = new Date(obj1.Added);
                var y = new Date(obj2.Added);
                return (y > x) - (y < x)
              })
              break
            case 'updated':
              tools.sort(function(obj1, obj2) {
                var x = new Date(obj1.Updated);
                var y = new Date(obj2.Updated);
                return (y > x) - (y < x)
              })
              break
          }
        }

        // Loop over tools
        $.each(tools, function (k, val) {
          var name = val.Name
          var bioc = val.BioC
          var pypi = val.PyPI
          var cran = val.CRAN
          var github = val.Github

          entry += '<li class="list-group-item"><a href="tools.html#' + name + '">' + name + '</a>'

          if (typeof bioc !== 'undefined') {
            entry += ' <img border="0" height="15" src="img/shields/BioC/' + bioc + '_years.svg">' +
                     ' <img border="0" height="15" src="img/shields/BioC/' + bioc + '_downloads.svg">'
          }

          if (typeof cran !== 'undefined') {
            entry += ' <img border="0" height="15" src="img/shields/CRAN/' + cran + '_version.svg">' +
                     ' <img border="0" height="15" src="img/shields/CRAN/' + cran + '_downloads.svg">'
          }

          if (typeof pypi !== 'undefined') {
            entry += ' <img border="0" height="15" src="img/shields/PyPI/' + pypi + '_version.svg">' +
                     ' <img border="0" height="15" src="img/shields/PyPI/' + pypi + '_python.svg">' +
                     ' <img border="0" height="15" src="img/shields/PyPI/' + pypi + '_status.svg">'
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

  var urlParams = new URLSearchParams(window.location.search);

  if (urlParams.has('sort')) {
    $("[name=selectsort]").val(urlParams.get('sort')).change()
  }

  $(function(){
    $("[name=selectsort]").change(function(){
        var val = $(this).val()
        var sorter
        if (typeof val !== 'undefined') {
            sorter = val
        }

        var url = document.location.toString()

        var hash
        if (url.includes('#')) {
          hash = url.split('#')[1]
          url = url.split('#')[0]
        }

        url = url.split('?')[0]

        if (hash !== undefined) {
          window.location.href = url + '?sort=' + sorter + '#' + hash
        } else {
          window.location.href = url + '?sort=' + sorter
        }

        return true
    })
  })

  printList(urlParams)
})
