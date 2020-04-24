// Banner Trigger if Not Closed
if (!sessionStorage.getItem('alertClosed')) {
  $('.alert-banner').css('display', 'inherit')
} else {
  $('.alert-banner').css('display', 'none')
}

$('.alert-banner button').click(function () {
  $('.alert-banner').css('display', 'none')
  sessionStorage.setItem('alertClosed', 'true')
})

$('.banner-accept').click(function () {
  $('.alert-banner').css('display', 'none')
  sessionStorage.setItem('alertClosed', 'true')
})

if (navigator.userAgent.match(/Opera|OPR\//)) {
  $('.alert-banner').css('display', 'inherit')
}
