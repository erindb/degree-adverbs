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

  slides.story = slide({
    name : "story",
    start : function() {
      $(".err").hide();
      _s.impatient = false;
      $("#comic_story").html("<img class='wide_comic' src='images/" + exp.condition.target + "_story.png'></img>");
      _s.trial_start = Date.now();
    },
    button : function() {
      var rt = Date.now() - _s.trial_start;
      if (rt < 10000) {
        $(".err").show();
        _s.impatient = true;
      } else {
        exp.data_trials.push({"rt": rt, "impatient": _s.impatient})
        exp.go();
      }
    }
  });

  slides.questions = slide({
    name : "questions",
    present : exp.condition.questions,
    present_handle : function(stim) {
      _s.trial_start = Date.now();
      $(".err").hide();

      _s.word = stim.slice(stim.length - 1)
      _s.number = stim.slice(0, stim.length - 1)
      if (_s.word == exp.condition.target) {
        _s.type = "target";
      } else if (_s.word == exp.condition.control) {
        _s.type = "control";
      } else if (_s.word == "expensive") {
        _s.type = "bare";
      } else {
        console.log("error 15");
      }

      //clear response
      $("#response").val("");

      //show image
      $("#comic_question").html("<img class='single_comic' src='images/" + stim + ".png'></img>");
    },
    button : function() {
      var response = $("#response").val();
      var rt = Date.now() - _s.trial_start;
      if (response.match(/^[0-9]*(\.[0-9][0-9])?$/) == null || response.length == 0) {
        $(".err").show();
      } else {
        exp.data_trials.push({
          "rt": rt,
          "response": response,
          "number": _s.number,
          "word": _s.word,
          "type": _s.type
        });
        $("#response").val("");
        _stream.apply(_s);
      }
    }
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
          "time_in_minutes" : (Date.now() - exp.startT)/60000,
      };
      setTimeout(function() {turk.submit(exp.data);}, 1000);
    }
  });

  return slides;
}

/// init ///
function init() {
  exp.catch_trials = [];
  exp.condition = _.sample([
    {target: "very", control: "quite", questions: ["very1", "quite2", "expensive3"]},
    {target: "very", control: "quite", questions: ["very1", "expensive2", "quite3"]},
    {target: "very", control: "quite", questions: ["quite1", "very2", "expensive3"]},
    {target: "very", control: "quite", questions: ["quite1", "expensive2", "very3"]},
    {target: "very", control: "quite", questions: ["expensive1", "quite2", "very3"]},
    {target: "very", control: "quite", questions: ["expensive1", "very2", "quite3"]},
    {target: "quite", control: "very", questions: ["very1", "quite2", "expensive3"]},
    {target: "quite", control: "very", questions: ["very1", "expensive2", "quite3"]},
    {target: "quite", control: "very", questions: ["quite1", "very2", "expensive3"]},
    {target: "quite", control: "very", questions: ["quite1", "expensive2", "very3"]},
    {target: "quite", control: "very", questions: ["expensive1", "quite2", "very3"]},
    {target: "quite", control: "very", questions: ["expensive1", "very2", "quite3"]},
  ]);
  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "story", "questions", "subj_info", "thanks"];
  
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