$(document).ready(function(){

    /*--Variables-------------------------------------------------------------*/

    platform_container = $("#platform-list");
    json_location = "data/single-cell-software.json";

    /*--Functions-------------------------------------------------------------*/

    function print_list(){

        /*-- Open JSON file, parse the contents, loop through & print markup--*/

        $.getJSON(json_location, function(data) {
            $.each(data, function(key, value) {

                /*-- Assign returned data --*/
                name = value.Name;
                doi = value.DOI;
                doi_url = value.DOI_url;
                description = value.Description;
                platform = value.Platform;
                code = value.Code;
                added = value.Added;
                updated = value.Updated;
                license = value.License;
                cats = value.categories.join(', ');

                /*-- Create markup --*/
                /*entry = '<li>'+
                            '<h2>'+name+'</h2>'+
                            '<ul>'+
                                '<li>DOI: '+doi+'</li>'+
                            '</ul>'+
                        '</li>';*/

                entry = '<div class="panel-heading">'+
                            '<h4 id="'+name+'" class="panel-title">'+
                                '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#'+name+'_c">'+name+'</a>'+
                            '</h4>'+
                        '</div>'+
                        '<div id="'+name+'_c" class="panel-collapse collapse">'+
                            '<ul class="list-group">'+
                                '<li class="list-group-item">'+description+'</li>'

                if ( typeof doi !== 'undefined' ) {
                    entry += '<li class="list-group-item"><strong>DOI:</strong> <a href="'+doi_url+'">'+doi+'</a></li>'
                }

                entry += '<li class="list-group-item"><strong>Platform: </strong> '+platform+', <strong>Code: </strong> <a href="'+code+'">'+code+'</a></li>'

                if ( typeof license != 'undefined') {
                    entry += '<li class="list-group-item"><strong>License: </strong> '+license+'</li>'
                }

                entry += '<li class="list-group-item"><strong>Categories: </strong> '+cats+'</li>'+
                '<li class="list-group-item"><strong>Added: </strong> '+added+', <strong>Updated: </strong>'+updated+'</li>'+
                    '</ul>'+
                '</div>'

                /*-- Add it to the list! --*/
                platform_container.append(entry);

            });
        });

    }

    /*--Calls-----------------------------------------------------------------*/

    print_list();

});

$(window).on('load', function(){
    /*
    var url = document.location.toString();

    if ( url.match('#') ) {
        var hash = url.split('#')[1];

        // collapse the expanded panel
        //$('#accordion .accordion-collapse').removeClass('in');

        // expand the requested panel
        $('#' + hash + '_c').addClass('in');
    }
    */
 });
