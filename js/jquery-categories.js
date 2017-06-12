$(document).ready(function(){

    /*--Variables-------------------------------------------------------------*/

    platform_container = $("#categories-list");
    json_location = "data/categories.json";

    /*--Functions-------------------------------------------------------------*/

    function print_list(){

        /*-- Open JSON file, parse the contents, loop through & print markup--*/

        $.ajaxSetup({
            cache:false
        });

        $.getJSON(json_location, function(data) {
            $.each(data, function(key, value) {

                /*-- Assign returned data --*/
                category = value.category;
                //cat_clean = category.replace(/ /g,"_");
                software = value.software;

                entry = '<div class="panel-heading">'+
                            '<h4 id="'+category+'" class="panel-title">'+
                                '<a data-toggle="collapse" class="accordion-toggle collapsed" href="#'+category+'_c">'+category

                entry += '</a></h4></div>'+
                    '<div id="'+category+'_c" class="panel-collapse collapse">'+
                    '<ul class="list-group">'

                $.each(software, function(k, val) {
                    name = val.Name;
                    bioc = val.Bioconductor;
                    pypi = val.pypi;
                    cran = val.CRAN;

                    entry += '<li class="list-group-item">'+name;

                    if ( typeof bioc !== 'undefined' ) {
                        entry +=
                            ' <img border="0" height="15" src="http://bioconductor.org/shields/years-in-bioc/'+bioc+'.svg">'+' <img border="0" height="15" src="http://bioconductor.org/shields/downloads/'+bioc+'.svg">';
                    }

                    if ( typeof cran !== 'undefined' ) {
                        entry +=
                            ' <img border="0" height="15" src="http://www.r-pkg.org/badges/version/'+cran+'">'+' <img border="0" height="15" src="http://cranlogs.r-pkg.org/badges/grand-total/'+cran+'">';
                    }

                    if ( typeof pypi !== 'undefined' ) {
                        entry +=
                            ' <img border="0" height="15" src="https://img.shields.io/pypi/v/'+pypi+'.svg">'+' <img border="0" height="15" src="https://img.shields.io/pypi/pyversions/'+pypi+'.svg">'+' <img border="0" height="15" src="https://img.shields.io/pypi/dm/'+pypi+'.svg">';
                    }

                    entry += '</li>';
                });

                entry += '</ul>'+'</div>'

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
