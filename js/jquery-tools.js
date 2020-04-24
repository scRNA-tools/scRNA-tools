$(document).ready(function () {
  /* --Variables------------------------------------------------------------- */

  var toolsContainer = $('#tools-list')
  var jsonPath = 'data/tools.json'
  var fixed_height = 200

  /* --Functions------------------------------------------------------------- */

  function panelInteractions() {
    $('.tool').click(function () {
      package_name = $(this).find('h4').attr('id')

      panel_name = package_name + '_c'
      panel = $('#' + panel_name)
      package_last_commit = panel.find('.commits span.commit-date')

      // Check to see whether this has been populated already... otherwise ping Github
      if (package_last_commit.text() == 'Unknown') {
        codebase = panel.find('.codebase_url').text()
        githubShields(panel, codebase)
      }

      letterPositions()
    })
  }

  function prettyGitDate(date_time) {
    months = [
      'January',
      'February',
      'March',
      'April',
      'May',
      'June',
      'July',
      'August',
      'September',
      'October',
      'November',
      'December',
    ]

    date = new Date(date_time)
    month = date.getMonth()
    year = date.getFullYear()

    today = new Date()
    current_month = today.getMonth()
    current_year = today.getFullYear()

    const date_utc = Date.UTC(year, month, date.getDate())
    const today_utc = Date.UTC(current_year, current_month, today.getDate())
    const diff_days = Math.floor((today_utc - date_utc) / (1000 * 60 * 60 * 24))

    var date_str = months[month]
    var colour = 'red'
    switch (true) {
      case diff_days == 0:
        date_str = 'today'
        colour = 'bright-green'
        break
      case diff_days == 1:
        date_str = 'yesterday'
        colour = 'bright-green'
        break
      case diff_days <= 7:
        date_str = 'this week'
        colour = 'bright-green'
        break
      case diff_days <= 30:
        colour = 'light-green'
        break
      case diff_days < 183:
        colour = 'citron'
        break
      case diff_days < 365:
        colour = 'yellow'
        break
      case diff_days < 548:
        colour = 'orange'
        break
    }

    // Add year if not a special label and not the current year
    if (months.includes(date_str) && current_year !== year) {
      date_str = date_str + ' ' + year
    }

    return [date_str, colour]
  }

  function githubShields(panel, url) {
    // split by forward slash, get username and package name

    url_split = url.split('/')
    username = url_split.slice(-2)[0]
    package_name = url_split.slice(-1)[0]

    json_location =
      'https://api.github.com/search/repositories?q=repo:' +
      username +
      '/' +
      package_name

    // Fetch Lassy, then update the required fields
    $.getJSON(json_location)
      .done(function (json) {
        forks = json.items[0].forks
        stargazers = json.items[0].stargazers_count
        last_commit = prettyGitDate(json.items[0].pushed_at)

        // Update panel to reflect requested information
        panel.find('.stars span.blue').text(stargazers)
        panel.find('.forks span.blue').text(forks)
        panel.find('.commits span.commit-date').text(last_commit[0])
        panel.find('.commits span.commit-date').removeClass('blue')
        panel.find('.commits span.commit-date').addClass(last_commit[1])
      })
      .fail(function (jqxhr, textStatus, error) {
        var err = textStatus + ', ' + error
        console.log('Request Failed: ' + err)
      })
  }

  function expandLinked() {
    var url = document.location.toString()
    var hash = url.split('#')[1]

    if (typeof hash !== 'undefined') {
      var title = '#' + hash
      var panel = title + '_c'

      // collapse expanded panels
      var allPanels = $('#accordion .accordion-collapse')

      allPanels.removeClass('in')
      allPanels.find('.accordion-toggle').addClass('collapsed')

      // expand the requested panel, change the title
      $(panel).addClass('in')
      $(title).find('.accordion-toggle').removeClass('collapsed')

      location.href = title

      package_last_commit = $(panel).find('.commits span.commit-date')

      // Check to see whether this has been populated already... otherwise ping Github
      if (package_last_commit.text() == 'Unknown') {
        codebase = $(panel).find('.codebase_url').text()
        githubShields($(panel), codebase)
      }
    }
  }

  $('.tool').click(function () {
    package_name = $(this).find('h4').attr('id')

    panel_name = package_name + '_c'
    panel = $('#' + panel_name)
    package_last_commit = panel.find('.commits span.commit-date')

    // Check to see whether this has been populated already... otherwise ping Github
    if (package_last_commit.text() == 'Unknown') {
      codebase = panel.find('.codebase_url').text()
      githubShields(panel, codebase)
    }

    letterPositions()
  })

  function checkFixed(from_top) {
    current_fixed = toolsContainer.hasClass('fixed_state')

    if (from_top > 200 && current_fixed != true) {
      $('#manipulators').addClass('fixed')
      if ($('#selectsort').val() == 'name') {
        $('#name-bookmarks').addClass('fixed')
      }
      toolsContainer.addClass('fixed_state')

      // Now, find the current letter position
    } else if (from_top < 200 && current_fixed != false) {
      $('#manipulators').removeClass('fixed')
      $('#name-bookmarks').removeClass('fixed')
      toolsContainer.removeClass('fixed_state')
    }
  }

  function letterPositions() {
    alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.split('')
    window.alpha_positions = {}

    $.each(alphabet, function (index, letter) {
      if ($('#anchor' + letter).length > 0) {
        window.alpha_positions[letter] = $('#anchor' + letter).offset().top
      }
    })
  }

  function trackLetters(from_top) {
    if (from_top + 10 < fixed_height) {
      $('#name-bookmarks li').removeClass('active')
    } else {
      current_letter = false

      $.each(window.alpha_positions, function (letter, position) {
        if (from_top + 10 > position) {
          current_letter = letter
        }
      })

      $('#name-bookmarks li').removeClass('active')
      $('.letter-menu' + current_letter).addClass('active')
    }
  }

  $(window).scroll(function () {
    from_top = $(window).scrollTop()
    checkFixed(from_top)

    if ($('#selectsort').val() == 'name') {
      trackLetters(from_top)
    }
  })

  function printList(urlParams) {
    /* -- Open JSON file, parse the contents, loop through & print markup -- */

    $.ajaxSetup({
      cache: false,
    })

    $.getJSON(jsonPath, function (data) {
      /* -- Sort data -- */

      if (urlParams.has('sort')) {
        switch (urlParams.get('sort')) {
          case 'cites':
            data.sort(function (obj1, obj2) {
              return obj2.Citations - obj1.Citations
            })
            break
          case 'refs':
            data.sort(function (obj1, obj2) {
              return (
                obj2.NumPubs +
                obj2.NumPreprints -
                (obj1.NumPubs + obj1.NumPreprints)
              )
            })
            break
          case 'pubs':
            data.sort(function (obj1, obj2) {
              return obj2.NumPubs - obj1.NumPubs
            })
            break
          case 'pres':
            data.sort(function (obj1, obj2) {
              return obj2.NumPreprints - obj1.NumPreprints
            })
            break
          case 'added':
            data.sort(function (obj1, obj2) {
              var x = new Date(obj1.Added)
              var y = new Date(obj2.Added)
              return (y > x) - (y < x)
            })
            break
          case 'updated':
            data.sort(function (obj1, obj2) {
              var x = new Date(obj1.Updated)
              var y = new Date(obj2.Updated)
              return (y > x) - (y < x)
            })
            break
        }
      }

      // Setup tools columns ----------------

      tool_index = 1
      sort_method = $('#selectsort').val()

      if (sort_method == 'name') {
        current_letter = false
        $('#name-bookmarks').show()
        toolsContainer.addClass('name-sort')
      } else {
        $('#name-bookmarks').hide()
        toolsContainer.append(
          '<div class="first-tools col-lg-6 col-md-12 col-sm-12 col-xs-12"></div>'
        )
        toolsContainer.append(
          '<div class="second-tools col-lg-6 col-md-12 col-sm-12 col-xs-12"></div>'
        )
      }

      const n_tools = Object.keys(data).length
      const half_tools = Math.ceil(n_tools / 2)
      var letter_tools = []
      $.each(data, function (key, value) {
        /* -- Assign returned data -- */

        var tool_information = {
          name: value.Tool,
          citations: value.Citations,
          description: value.Description,
          platform: value.Platform,
          code: value.Code,
          github: value.GitHub,
          added: value.Added,
          updated: value.Updated,
          license: value.License,
          cats: value.Categories,
          bioc: value.Bioc,
          pypi: value.PyPI,
          cran: value.CRAN,
          pubs: value.Publications,
          pres: value.Preprints,
          nPubs: value.NumPubs,
          nPres: value.NumPreprints,
          totalRefs: value.NumPubs + value.NumPreprints,
        }

        // If any categories are selected for filtering, pass non-matches.

        keep = false

        selected_categories = $('[name=category_filter]').val()

        if ($('[name=category_filter]').val().length > 0) {
          $.each(selected_categories, function (index, cat) {
            if (value.Categories.indexOf(cat) > -1) {
              keep = true
              return false
            }
          })

          if (!keep) {
            return
          }
        }

        // Add it to the list

        if (sort_method == 'name') {
          first_letter = tool_information['name'][0].toUpperCase()

          if (first_letter != current_letter) {
            half_letter = Math.ceil(letter_tools.length / 2)
            $.each(letter_tools, function (idx, tool) {
              entry = toolItem(tool)
              if (idx > half_letter) {
                $('#alpha' + current_letter + '-right').append(entry)
              } else {
                $('#alpha' + current_letter + '-left').append(entry)
              }
            })

            toolsContainer.append(
              '<h3 id="anchor' +
                first_letter +
                '" class="tools-list">' +
                first_letter +
                '</h3>'
            )
            toolsContainer.append(
              '<div id="alpha' +
                first_letter +
                '-left" class="first-tools col-lg-6"></div>'
            )
            toolsContainer.append(
              '<div id="alpha' +
                first_letter +
                '-right" class="second-tools col-lg-6"></div>'
            )

            current_letter = first_letter
            letter_tools = []
          }

          letter_tools.push(tool_information)
        } else {
          entry = toolItem(tool_information, tool_index)

          if (tool_index > half_tools) {
            $('.second-tools').append(entry)
          } else {
            $('.first-tools').append(entry)
          }
        }

        tool_index = tool_index + 1
      })

      if (sort_method == 'name') {
        // Tool headings

        // Add tools for final letter
        half_letter = Math.ceil(letter_tools.length / 2)
        $.each(letter_tools, function (idx, tool) {
          entry = toolItem(tool)
          if (idx > half_letter) {
            $('#alpha' + current_letter + '-right').append(entry)
          } else {
            $('#alpha' + current_letter + '-left').append(entry)
          }
        })

        headings = $('.tools-list')
        $.each(headings, function (index, heading) {
          letter = $(heading).html()
          $('#name-bookmarks').append(
            '<li class="letter-menu' +
              letter +
              '"><a href="#anchor' +
              letter +
              '">' +
              letter +
              '</a></li>'
          )
        })
      }

      expandLinked()
      panelInteractions()
      letterPositions()
    })
  }

  /* --Calls----------------------------------------------------------------- */

  // Check URL for queries

  var urlParams = new URLSearchParams(window.location.search)

  if (urlParams.has('sort')) {
    $('[name=selectsort]').val(urlParams.get('sort')).change()
  }

  if (urlParams.has('cats')) {
    cat_url = urlParams.get('cats').split(',')
    cats = []

    $.each(cat_url, function (index, cat) {
      cats.push(cat)
    })

    $('[name=category_filter]').val(cats)
  }

  // Refresh page on change on select or filter button

  function pushURL() {
    select_item = $('[name=selectsort]')

    // Sort Select
    var val = select_item.val()
    var sorter

    if (typeof val !== 'undefined') {
      sorter = val
    }

    // Check for cats
    selected_cats = $('[name=category_filter]').val()
    selected_cats = selected_cats.toString()

    // Check for anchors
    var url = document.location.toString()

    var hash
    if (url.includes('#')) {
      hash = url.split('#')[1]
      url = url.split('#')[0]
    }

    url = url.split('?')[0]

    if (hash !== undefined) {
      if (selected_cats.length > 0) {
        window.location.href =
          url + '?sort=' + sorter + '&cats=' + selected_cats + '#' + hash
      } else {
        window.location.href = url + '?sort=' + sorter + '#' + hash
      }
    } else {
      if (selected_cats.length > 0) {
        window.location.href =
          url + '?sort=' + sorter + '&cats=' + selected_cats
      } else {
        window.location.href = url + '?sort=' + sorter
      }
    }

    return true
  }

  $(function () {
    $('[name=selectsort]').change(function () {
      $('[name=category_filter]').val('')
      pushURL()
    })
    $('#category_submit').click(function () {
      pushURL()
    })
    $('#category_reset').click(function () {
      $('[name=category_filter]').val('')
      pushURL()
    })
  })

  // Refresh
  printList(urlParams)
})
