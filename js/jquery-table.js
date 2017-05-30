$(document).ready(function(){

    /*--Variables-------------------------------------------------------------*/

    table_headings = $("#platforms th");
    table_filter = $("#table-filter");
    csv_location = "data/software-table.tsv";
    table_body = $("#main-table tbody");

    /*--Functions-------------------------------------------------------------*/

    function hide_column(field_name){

        column = $('#platforms th[data-field="'+field_name+'"]');

        /*-- Get location of column --*/
        column_no = table_headings.index(column);
        console.log(column_no);

        /*-- Hide it! --*/
        column.hide();
        $("#platforms td:nth-child("+column_no+")").hide();

    }

    function form_filter(){

        table_columns = $("#platforms td");

        table_headings.show();
        table_columns.show();

        /*--Hide everything else --*/
        table_filter.find('input').each(function(){
            if(!$(this).is(':checked')){

                field_name = $(this).attr("name");
                hide_column(field_name);
            }
        })
    }

    function populate_table(){

        /*-- Asynchronously request the CSV ----------------------------------*/

        $.ajax({
            url: csv_location,
            success: function (data) {

                /*-- Split the data by lines --*/
                csv_contents = data.split(/\r\n|\n/);

                /*-shoot off the data for... appending --*/
                process_csv(csv_contents);

            },
            dataType: "text"
        });

    }

    function process_csv(csv_contents){

        /*-- iterate through the lines, create the markup--*/
        for(var i=1; i < csv_contents.length; i++){

            markup = "<tr><td>";

            columns = String(csv_contents[i]);
            columns = columns.replace(/\t/g,"</td><td>");

            markup += columns;
            markup += "</td></tr>";

            /*-- Add it to the table --*/
            table_body.append(markup);

        }

        /*-- Now apply functions to the data--*/

        $("#main-table").DataTable({
            'drawCallback': function( settings ) {
                form_filter();
            }
        });


    }

    table_filter.find("input").click(function(){
        form_filter();
    })

    /*--Calls-----------------------------------------------------------------*/

    populate_table();


})
