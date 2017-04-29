// Source: https://s3.amazonaws.com/mturk-public/externalHIT_v1.js
function turkGetParam( name, defaultValue ) { 
    var regexS = "[\?&]"+name+"=([^&#]*)"; 
    var regex = new RegExp( regexS ); 
    var tmpURL = window.location.href; 
    var results = regex.exec( tmpURL ); 
    if( results == null ) { 
        return defaultValue; 
    } else { 
        return results[1];    
    } 
}

function UTWorkerLimitReached(ut_id, workerId, assignmentId) {
    var assignmentId = assignmentId || turkGetParam('assignmentId', '');
    if (assignmentId != '' && assignmentId != 'ASSIGNMENT_ID_NOT_AVAILABLE') {
        var workerId = workerId || turkGetParam('workerId', '');
        var url = '//uniqueturker.myleott.com/'+ut_id+'/'+workerId+'/'+assignmentId;

        var response = turkGetParam('ut_response', '-1');
        if (window.XDomainRequest) {
            if (response == '-1') {
                // Use Microsoft XDR
                var xdr = new XDomainRequest();
                xdr.open("get", url);
                xdr.onload = function() {
                    response = xdr.responseText;
                    window.location.replace(window.location.href + "&ut_response="+response);
                };
                xdr.send();
            }
        } else {
            var request = new XMLHttpRequest();
            request.open('GET', url, false);
            request.send();
            response = request.responseText;
        }

        if (response == '0') {
            return true;
        }
    }
    return false;
}

(function(){
    var ut_id = "bennett-intensifiers-replication-friends-purchases-10-24-2016";
    if (UTWorkerLimitReached(ut_id)) {
    	console.log("nope!");
    	$(".slide").hide();
    	//document.body.innerHTML = "";

        var div = document.createElement('div'), style = div.style;
        style.backgroundColor = "gray";
        style.color = "white";
        
        style.position = "absolute";
        style.margin = "0";
        style.padding = "0";
        style.paddingTop = "15px";
        style.paddingBottom = "15px";
        style.top = "0";
        style.width = "98%";
        style.textAlign = "center";
        style.fontFamily = "arial";
        style.fontSize = "24px";
        style.fontWeight = "bold";
        
        style.opacity = "0.5";
        style.filter = "alpha(opacity = 50)";
        
        div.innerHTML = "You have already completed the maximum number of HITs allowed by this requester. Please click 'Return HIT'.";
        
        document.body.appendChild(div);
    }
})();
