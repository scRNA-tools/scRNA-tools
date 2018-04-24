$(document).ready(function () {
  /* --Variables------------------------------------------------------------- */

  var toolsContainer = $('#tools-list')
  var jsonPath = 'data/tools.json'

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

  function linkCats (cats) {
    var linked = []

    for (var i = 0; i < cats.length; i++) {
      var cat = cats[i]
      linked.push('<a href="categories.html#' + cat + '">' + cat.replace(/([a-z])([A-Z])/g, '$1 $2') + '</a>')
    }

    return linked.join(', ')
  }

  function printList (urlParams) {
    /* -- Open JSON file, parse the contents, loop through & print markup -- */

    $.ajaxSetup({
      cache: false
    })

    $.getJSON(jsonPath, function (data) {

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

      $.each(data, function (key, value) {
        /* -- Assign returned data -- */
        var name = value.Name
        var doi = value.DOIs
        var doiURL = value.DOIURL
        var pubDate = value.PubDates
        var preprint = value.Preprint
        var citations = value.Citations
        var refs = value.Refs
        var description = value.Description
        var platform = value.Platform
        var code = value.Code
        var github = value.Github
        var added = value.Added
        var updated = value.Updated
        var license = value.License
        var cats = value.Categories
        var bioc = value.BioC
        var pypi = value.PyPI
        var cran = value.CRAN
        var nPubs = value.Publications
        var nPres = value.Preprints
        var totalRefs = nPubs + nPres

        var entry = ''
        entry += '<div class="panel-heading">' +
                 '<h4 id="' + name + '" class="panel-title">' +
                 '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#' + name + '_c">' + name

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

        entry += '</a></h4></div>' +
                 '<div id="' + name + '_c" class="panel-collapse collapse">' +
                 '<ul class="list-group">' +
                 '<li class="list-group-item">' + description + '</li>'

        // Loop over references
        if (totalRefs > 0) {

          entry += '<div class="panel-heading">' +
                   '<p id="' + name + '_pubs" class="panel-title">' +
                   '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#' + name + '_pubs_c">' +
                   '<strong>Publications:</strong> ' + nPubs +
                   ', <strong>Preprints:</strong> ' + nPres +
                   ', <strong>Total citations:</strong> ' + citations
          entry += '</a></h4></div>' +
                   '<div id="' + name + '_pubs_c" class="panel-collapse collapse">' +
                   '<ul class="list-group">'

          if (nPubs > 0) {
            var pubs = refs.Publications

            entry += '<li class="list-group-item"><strong>Publications</strong></li>'

            $.each(pubs, function (k, val) {

              var title = val.Title
              var doi = val.DOI
              var date = val.PubDate
              var cites = val.Citations

              entry += '<li class="list-group-item">'
              entry += '<em>"' + title + '"</em><br/>'
              entry += '<strong>DOI: </strong> <a href="https://doi.org/' + doi + '">' + doi + '</a>'
              entry += ', <strong>Published: </strong>' + date
              entry += ', <strong>Citations: </strong> ' + cites
              entry += '</li>'
            })
          }

          if (nPres > 0) {
            var pres = refs.Preprints

            entry += '<li class="list-group-item"><strong>Preprints</strong></li>'

            $.each(pres, function (k, val) {

              var title = val.Title
              var doi = val.DOI
              var cites = val.Citations

              entry += '<li class="list-group-item">'
              entry += '<em>"' + title + '"</em><br/>'
              if (doi.includes('arxiv')) {
                var id = doi.replace("arxiv/", "")
                entry += '<strong>arXiv: </strong> <a href="https://arxiv.org/abs/' + id + '">' + id + '</a>'
              } else {
                entry += '<strong>DOI: </strong> <a href="https://doi.org/' + doi + '">' + doi + '</a>'
              }
              if (typeof cites !== 'undefined') {
                entry += ', <strong>Citations: </strong> ' + cites
              }
              entry += '</li>'
            })
          }
          entry += '</ul></div>'
        }

        entry += '<li class="list-group-item"><strong>Platform: </strong> ' + platform + '</li>'
        if (typeof code !== 'undefined') {
            entry += '<li class="list-group-item"><strong>Code: </strong> <a href="' + code + '">' + code + '</a>'
            if (typeof github !== 'undefined') {
              var github_clean = github.replace("/", "_")

              entry += ' <img border="0" height="15" src="img/shields/GitHub/' + github_clean + '_stars.svg">' +
              ' <img border="0" height="15" src="img/shields/GitHub/' + github_clean + '_forks.svg">' +
              ' <img border="0" height="15" src="img/shields/GitHub/' + github_clean + '_commit.svg">'
            }
            entry += '</li>'
        }
        if (typeof license !== 'undefined') {
          entry += '<li class="list-group-item"><strong>License: </strong> ' + license + '</li>'
        }

        entry += '<li class="list-group-item"><strong>Categories: </strong> ' + linkCats(cats) + '</li>' +
                 '<li class="list-group-item">' +
                 '<strong>Added: </strong> ' + added +
                 ', <strong>Updated: </strong>' + updated +
                 '</li>' +
                 '</ul>' +
                 '</div>'

        /* -- Add it to the list! -- */
        toolsContainer.append(entry)
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
