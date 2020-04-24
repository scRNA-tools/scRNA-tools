$(document).ready(function () {
  // Yes yes, I know there's a better way. But golly!

  var jsonPathTools = 'data/tools.json'
  var jsonPathCat = 'data/categories.json'

  $.ajaxSetup({
    cache: false,
  })

  $.getJSON(jsonPathTools, function (data) {
    no_tools = 0
    $.each(data, function (key, value) {
      no_tools += 1
    })

    $('.tool-no').html(no_tools)
  })

  $.getJSON(jsonPathCat, function (data) {
    no_cats = 0
    $.each(data, function (key, value) {
      no_cats += 1
    })

    no_cats = Math.floor(no_cats / 10) * 10

    $('.cat-no').html(no_cats)
  })
})
