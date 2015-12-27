app.factory('EventChartObjectFactory', function(CountEventsService,MapEventRows){

  /**
   * General Chart Object definition
   * @type {Object}
   */
  var chartObject= {
      "type": "AreaChart",
      "displayed": false,
      "data": {
        "cols": [
          {
            "id": "day",
            "label": "Day",
            "type": "string",
            "p": {}
          },
          {
            "id": "event-id",
            "label": "Events",
            "type": "number",
            "p": {}
          }
        ],
        "rows": []
    },
    "options": {
        "title": "Events per day",
        "isStacked": "true",
        "fill": 20,
        "displayExactValues": true,
        "vAxis": {
          "title": "Events",
          "gridlines": {
            "count": 10
          }
        },
        "hAxis": {
          "title": "Date"
        }
      },
      "formatters": {}
  };
  /**
   * creates chart object with given data
   * @param  {[type]} data [description]
   * @return {[type]}      [description]
   */
  var getChartObjectFromEvents= function(events){
    var start=new Date();
    
    start.setDate(start.getDate()-30);
    var end= new Date();
    
    var rows=CountEventsService.countEvents(start,end,events);
    chartObject.data.rows=MapEventRows.mapRows(rows);
    return chartObject;
  }



  //--------------------
  // Interface definition
  //--------------------
  return {
    getChartObjectFromEvents:getChartObjectFromEvents    
  };

})