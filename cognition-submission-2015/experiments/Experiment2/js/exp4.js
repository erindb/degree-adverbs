if (typeof JSON.clone !== "function")
{
    JSON.clone = function(obj)
    {
        return JSON.parse(JSON.stringify(obj));
    };
}

function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.ordering = slide({
    name : "ordering",
    present : exp.adverb_lists,
    present_handle : function(stim) {
      this.startTime = Date.now();
      $("#response").empty();
      this.stim = JSON.clone(stim);
      $(".err").hide();
      var adverbs = stim.adverbs;
      var adjective = stim.adjective;
      $(".most").html(adjective.most);
      $(".least").html(adjective.least);
      for (var i=0; i<(adverbs.length); i++) {
        var adverb = adverbs[i];
        $("#wordbank").append(
          "<li class='adverb ui-state-default'>" + adverb + " " +
          adjective.adjective + "</li>"
        );
      }
      $(function() {
        $( "#wordbank" ).sortable();
        $( "#wordbank" ).disableSelection();
      });

      $( "ul.droptrue" ).sortable({
        connectWith: "ul"
      });
      $( "#wordbank, #response").disableSelection();

      $(".adverb").mousedown(function(e) {
        exp.clicks.push({
          "type":"mousedown",
          "time": Date.now(),
          "x" : e.pageX,
          "y" : e.pageY,
          "adverb" : $(this).html()
        })
      })
      $(".adverb").click(function(e) {
        exp.clicks.push({
          "type": "click",
          "time": Date.now(),
          "x" : e.pageX,
          "y" : e.pageY,
          "adverb" : $(this).html()
        })
      });
    },
    button : function() {
      var num_adverbs = this.stim.adverbs.length;
      if ($("#response li").length == num_adverbs) {
        for (var i=0; i<num_adverbs; i++) {
          var adverb = $($("#response li")[i]).html();
          exp.data_trials.push({
            "trial" : this.stim.trial.toString(),
            "adverb" : adverb,
            "ranking" : i.toString(),
            "trial_time" : (Date.now() - this.startTime)
          })
          console.log(adverb);
        }
        _stream.apply(_s); //use exp.go() if and only if there is no "present" data.
      } else {
        $(".err").show();
      }
    }
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        language : $("#language").val(),
        enjoyment : $("#enjoyment").val(),
        asses : $('input[name="assess"]:checked').val(),
        age : $("#age").val(),
        gender : $("#gender").val(),
        education : $("#education").val(),
        comments : $("#comments").val(),
      };
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.thanks = slide({
    name : "thanks",
    start : function() {
      exp.data= {
          "trials" : exp.data_trials,
          "catch_trials" : exp.catch_trials,
          "system" : exp.system,
          "condition" : exp.condition,
          "subject_information" : exp.subj_data,
          "clicks" : exp.clicks,
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

function make_adverb_lists() {
  var adjs = _.shuffle([
    {
      "adjective": "expensive",
      "most" : "highest price",
      "least" : "lowest price"
    },
    {
      "adjective": "tall",
      "most" : "highest height",
      "least" : "lowest height"
    },
    {
      "adjective": "beautiful",
      "most" : "most beautiful",
      "least" : "least beautiful"
    },
    {
      "adjective": "old",
      "most" : "oldest",
      "least" : "least old"
    }/*,
    {
      "adjective": "sad",
      "most" : "most sad",
      "least" : "least sad"
    }*/
  ])
  adverb_lists = _.shuffle([
    {
      "list" : "A",
      "adjective": adjs.shift(),
      "adverbs": [
        "surpassingly",
        "astoundingly",
        "fantastically",
        "strikingly",
        "excessively",
        "markedly",
        "remarkably",
        "utterly",
        "truly",
        "particularly",
      ]
    },
    {
      "list" : "B",
      "adjective": adjs.shift(),
      "adverbs": [
        "colossally",
        "phenomenally",
        "mightily",
        "acutely",
        "extraordinarily",
        "amazingly",
        "terribly",
        "notably",
        "significantly",
        "quite",
      ]
    },
    {
      "list" : "C",
      "adjective": adjs.shift(),
      "adverbs": [
        "terrifically",
        "uncommonly",
        "supremely",
        "awfully",
        "exceedingly",
        "radically",
        "exceptionally",
        "incredibly",
        "totally",
        "especially",
      ]
    },
    {
      "list" : "D",
      "adjective": adjs.shift(),
      "adverbs": [
        "frightfully",
        "outrageously",
        "insanely",
        "decidedly",
        "intensely",
        "unusually",
        "desperately",
        "seriously",
        "extremely",
        "very"
      ]
    }
  ])
  for (var i=0; i<adverb_lists.length; i++) {
    adverb_lists[i].trial = i.toString();
  }
  return adverb_lists;
}

/// init ///
function init() {
  exp.clicks = [];
  exp.adverb_lists = make_adverb_lists();

  exp.catch_trials = [];
  exp.condition = {}; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "ordering", 'subj_info', 'thanks'];
  
  exp.data_trials = [];
  //make corresponding slides:
  exp.slides = make_slides(exp);

  exp.nQs = utils.get_exp_length(); //this does not work if there are stacks of stims (but does work for an experiment with this structure)
                    //relies on structure and slides being defined

  $('.slide').hide(); //hide everything

  //make sure turkers have accepted HIT (or you're not in mturk)
  $("#start_button").click(function() {
    if (turk.previewMode) {
      $("#mustaccept").show();
    } else {
      $("#start_button").click(function() {$("#mustaccept").show();});
      exp.go();
    }
  });

  exp.go(); //show first slide
}