function make_slides(f) {
  var   slides = {};

  slides.i0 = slide({
     name : "i0",
     start: function() {
      exp.startT = Date.now();
     }
  });

  slides.instructions = slide({
    name : "instructions",
    button : function() {
      exp.go(); //use exp.go() if and only if there is no "present" data.
    }
  });

  slides.trial = slide({
    name : "trial",
    start : function() {
      exp.current_question_num = 0;
      var target = exp.condition.target;
      var condition = exp.condition.condition;
      var context = exp.condition.context;
      if (condition == "repeat_target") {
        for (var i=0; i<3; i++) {
          $("#context" + (i+1)).html(target);
        }
      } else if (condition == "no_adverbs") {
        for (var i=0; i<3; i++) {
          $("#context" + (i+1)).html("");
        }
      } else if (condition == "variety_of_adverbs") {
        for (var i=0; i<3; i++) {
          $("#context" + (i+1)).html(context.shift());
        }
      }
      $("#target").html(target);
      if (target == "colossally") {
        $("#article").html("a");
      } else {
        $("#article").html("an");
      }
    },
    present : [
      {
        "type" : "none",
      },
      {
        "type": "attention_check",
        "correct" : "baseball",
        "measure" : "free",
        "question" : "What kind of camps does Tom Lyons hold?",
        "response_start" : "He holds",
        "response_end" : "camps."
      },
      {
        "type": "attention_check",
        "correct" : "pitcher",
        "measure" : "free",
        "question" : "What position does Tom Lyons play?",
        "response_start" : "Tom Lyons is a",
        "response_end" : "."
      },
      {
        "type": "amount",
        "measure" : "slider",
        "question" : "During the off-season, about what percent of his free time do you think Tom Lyons spends working with children one-on-one and coaching them? (adjust the slider to answer)",
      },
      {
        "type": "explanation",
        "measure" : "free",
        "question" : "Why do you think Tom Lyons puts so much time and energy into baseball camps instead of taking a vacation?",
        "response_start" : "Because",
        "response_end" : "."
      },
      {
        "type": "counterfactual",
        "measure" : "free",
        "question" : "Please fill in the blank with your best guess:",
        "response_start" : "If it wasn't the case that",
        "response_end" : ", Tom Lyons would not hold baseball camps instead of taking a vacation."
      }
    ],
    present_handle : function(stim) {
      this.start_slide = Date.now();
      $(".err").hide();
      $("#response_div").hide();
      $("#percent_div").hide();
      $("#question").hide();
      this.trial_data = _.clone(stim);
      if (stim.type == "none") {
        $("#response_div").hide();
        this.log_responses = function(continuation) {
          this.trial_data.time_on_slide_in_seconds = (Date.now() - this.start_slide) / 1000;
          exp.data_trials.push(_.clone(this.trial_data));
          continuation();
        }
      } else {
        $("#question").show();
        $("#question").html(stim.question);
        this.trial_data.trialnum = exp.current_question_num;
        if (stim.measure == "free") {
          $("#response_div").show()
          $("#response").val("");
          $("#response_start").html(stim.response_start);
          $("#response_end").html(stim.response_end);
          this.log_responses = function(continuation) {
            if ($("#response").val().length > 0) {
              this.trial_data.time_on_slide_in_seconds = (Date.now() - this.start_slide) / 1000;
              this.trial_data.response = $("#response").val();
              exp.data_trials.push(_.clone(this.trial_data));
              continuation();
            } else {
              $(".err").show();
            }
          }
        } else {
          $("#percent_div").show();
          utils.make_slider("#percent_slider", function(event, ui) {
            $("#percent").html(Math.round(ui.value * 100));
            exp.sliderPost = ui.value;
          });
          exp.sliderPost = null;
          this.log_responses = function(continuation) {
            if (exp.sliderPost == null) {
              $(".err").show();
            } else {
              this.trial_data.time_on_slide_in_seconds = (Date.now() - this.start_slide) / 1000;
              this.trial_data.response = exp.sliderPost;
              exp.data_trials.push(_.clone(this.trial_data));
              continuation();
            }
          }
        }
      }
    },
    button : function() {
      this.log_responses(
        function() {
          _stream.apply(_s);
          exp.current_question_num++;
        }
      );
    },
  });

  slides.subj_info =  slide({
    name : "subj_info",
    submit : function(e){
      //if (e.preventDefault) e.preventDefault(); // I don't know what this means.
      exp.subj_data = {
        "language" : $("#language").val(),
        "enjoyment" : $("#enjoyment").val(),
        "assess" : $('input[name="assess"]:checked').val(),
        "age" : $("#age").val(),
        "gender" : $("#gender").val(),
        "education" : $("#education").val(),
        "comments" : $("#comments").val(),
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
  exp.catch_trials = [];
  exp.condition = {
    "condition" : _.sample(["repeat_target", "variety_of_adverbs"/*, "no_adverbs"*/]),
    "target" : "astoundingly",
    "context" : _.shuffle(["insanely", "colossally", "outrageously"])
  }; //can randomize between subject conditions here
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "trial", 'subj_info', 'thanks'];
  
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