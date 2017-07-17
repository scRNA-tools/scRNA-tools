$(document).ready(function () {
  /* --Variables------------------------------------------------------------- */

  var faqsContainer = $('#faqs-list')
  var jsonPath = 'data/faqs.json'

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
    /* -- Open JSON file, parse the contents, loop through & print markup -- */

    $.ajaxSetup({
      cache: false
    })

    $.getJSON(jsonPath, function (data) {
      $.each(data, function (key, value) {
        /* -- Assign returned data -- */
        var id = value.id
        var question = value.question
        var answer = value.answer

        var entry = ''
        entry += '<div class="panel-heading">' +
                    '<h4 id="' + id + '" class="panel-title">' +
                        '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#' + id + '_c">' + question +
                        '</a>' +
                    '</h4>' +
                 '</div>' +
                 '<div id="' + id + '_c" class="panel-collapse collapse">' +
                    '<ul class="list-group">' +
                        '<li class="list-group-item">' + answer + '</li>' +
                    '</ul>' +
                 '</div>'

        /* -- Add it to the list! -- */
        faqsContainer.append(entry)
      })

      expandLinked()
    })
  }

  /* --Calls----------------------------------------------------------------- */

  printList()
})
