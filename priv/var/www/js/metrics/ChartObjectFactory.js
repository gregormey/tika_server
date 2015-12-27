app.factory('ChartObjectFactory', function(CountUsersService,MapUserRows){

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
            "id": "created-id",
            "label": "Installs",
            "type": "number",
            "p": {}
          },
          {
            "id": "invited-id",
            "label": "Invites",
            "type": "number",
            "p": {}
          },
          {
            "id": "registered-id",
            "label": "Registrations",
            "type": "number",
            "p": {}
          }
        ],
        "rows": [],
      "options": {
        "title": "User per day",
        "isStacked": "true",
        "fill": 20,
        "displayExactValues": true,
        "vAxis": {
          "title": "Users",
          "gridlines": {
            "count": 10
          }
        },
        "hAxis": {
          "title": "Date"
        }
      },
      "formatters": {}
    }
  };
  /**
   * creates chart object with given data
   * @param  {[type]} data [description]
   * @return {[type]}      [description]
   */
  var getChartObjectFromUsers= function(users){
    var start=new Date();
    
    start.setDate(start.getDate()-30);
    var end= new Date();
    
    var rows=CountUsersService.countUsers(start,end,users);
    chartObject.data.rows=MapUserRows.mapRows(rows);
    return chartObject;
  }



  //--------------------
  // Interface definition
  //--------------------
  return {
    getChartObjectFromUsers:getChartObjectFromUsers    
  };

})