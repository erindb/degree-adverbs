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
    start : function() {
      $(".err").hide();
    },
    button : function() {
      if ($("#response li").length == exp.adverbs.length) {
        for (var i=0; i<exp.adverbs.length; i++) {
          var adverb = $($("#response li")[i]).html();
          exp.data_trials.push({
            "adverb": adverb,
            "ranking": i.toString()
          })
          console.log(adverb);
        }
        exp.go(); //use exp.go() if and only if there is no "present" data.
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
          "time_in_minutes" : (Date.now() - exp.startT)/60000
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {

  exp.adverbs = _.shuffle([
    "surpassingly", "colossally", "immoderately",
    "terrifically", "frightfully", "astoundingly", "inordinately",
    "phenomenally", "uncommonly", "outrageously", "fantastically",
    "mightily", "supremely", "insanely", "strikingly", "acutely",
    "awfully", "unduly", "decidedly", "excessively", "extraordinarily",
    "exceedingly", "immensely", "intensely", "markedly", "amazingly",
    "radically", "unusually", "cracking", "remarkably", "terribly",
    "exceptionally", "desperately", "utterly", "notably", "incredibly",
    "seriously", "truly", "significantly", "totally", "extremely",
    "highly", "particularly", "quite", "especially", "very"
  ]);
  for (var i=0; i<(exp.adverbs.length); i++) {
    var adverb = exp.adverbs[i];
    $("#wordbank").append(
      "<li class='adverb ui-state-default'>" + adverb + " expensive</li>"
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