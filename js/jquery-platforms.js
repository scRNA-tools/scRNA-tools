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

                /*-- Create markup --*/
                entry = '<li>'+
                            '<h2>'+name+'</h2>'+
                        '</li>';

                /*-- Add it to the list! --*/
                platform_container.append(entry);

            });
        });

    }

    /*--Calls-----------------------------------------------------------------*/

    print_list();

})
