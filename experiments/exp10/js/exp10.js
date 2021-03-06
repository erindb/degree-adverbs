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
    present : ["story1_" + exp.target, "story2_" + exp.target, "story3_" + exp.target],
    start : function() {
      $("#num_pages").html(_s.present.length);
    },
    present_handle : function(stim) {
      $(".err").hide();
      _s.impatient = false;
      $("#comic_story").html("<img class='wide_comic' src='images/" + stim + ".png'></img>");
      _s.trial_start = Date.now();

    },
    button : function() {
      var rt = Date.now() - _s.trial_start;
      if (rt < 10000) {
        $(".err").show();
        _s.impatient = true;
      } else {
        exp.data_trials.push({"rt": rt, "impatient": _s.impatient})
        _stream.apply(_s);
      }
    }
  });

  slides.target_question = slide({
    name : "target_question",
    start : function() {
      _s.trial_start = Date.now();
      $(".err").hide();

      //clear response
      $(".response").val("");

      var lines = [
        '<tr><td class="prompt">truly expensive</td><td>$<input class="response" id="truly_price" type="text"></input></td></tr>',
        '<tr><td class="prompt">madly expensive</td><td>$<input class="response" id="madly_price" type="text"></input></td></tr>',
        '<tr><td class="prompt">expensive</td><td>$<input class="response" id="bare_price" type="text"></input></td></tr>'
      ]

      $("#target_question_responses").html(_.shuffle(lines).join(""));

      //show image
      $("#target_question_comic").html("<img class='single_comic' src='images/target_question.png'></img>");
    },
    button : function() {
      var rt = Date.now() - _s.trial_start;
      var complete = true;
      var words = ["truly", "madly", "bare"];
      for (var i=0; i<words.length; i++) {
        var response = $("#" + words[i] + "_price").val();
        if (response.match(/^[0-9]*(\.[0-9][0-9])?$/) == null || response.length == 0) {
          complete = false;
        }
      }
      if (complete) {
        for (var i=0; i<words.length; i++) {
          var response = $("#" + words[i] + "_price").val();
          exp.data_trials.push({
            "rt": rt,
            "response": response,
            "word_type": words[i] == "bare" ? "bare" : exp.word_types[words[i]],
            "word": words[i],
            "qtype": "price"
          });
        }
        $(".response").val("");
        exp.go();
      } else {
        $(".err").show();
      }
    }
  });

  slides.frequency_question = slide({
    name : "frequency_question",
    start : function() {
      _s.trial_start = Date.now();
      $(".err").hide();

      var lines = [
        '<tr><td class="prompt">truly</td><td><input class="response" id="truly_freq" type="text"></input></td></tr>',
        '<tr><td class="prompt">madly</td><td><input class="response" id="madly_freq" type="text"></input></td></tr>',
        '<tr><td class="prompt">yep</td><td><input class="response" id="yep_freq" type="text"></input></td></tr>'
      ]
      $("#frequency_question_responses").html(_.shuffle(lines).join(""));

      //clear response
      $(".response").val("");
    },
    button : function() {
      var rt = Date.now() - _s.trial_start;
      var complete = true;
      var words = ["truly", "madly", "yep"];
      for (var i=0; i<words.length; i++) {
        var response = $("#" + words[i] + "_freq").val();
        if (response.match(/^[0-9]*(\.[0-9][0-9])?$/) == null || response.length == 0) {
          complete = false;
        }
      }
      if (complete) {
        for (var i=0; i<words.length; i++) {
          var response = $("#" + words[i] + "_freq").val();
          exp.data_trials.push({
            "rt": rt,
            "response": response,
            "word_type": words[i] == "yep" ? "yep" : exp.word_types[words[i]],
            "word" : words[i],
            "qtype": "frequency"
          });
        }
        $(".response").val("");
        exp.go();
      } else {
        $(".err").show();
      }
    }
  });

  slides.summary = slide({
    name : "summary",
    start : function() {
      _s.trial_start = Date.now();
      $(".err").hide();
      $(".response").val("");
    },
    button : function() {
      var rt = Date.now() - _s.trial_start;
      var response = $("#summary_response").val();
      if (response.length > 0) {
        exp.data_trials.push({
          "rt": rt,
          "response": response,
          "qtype": "summary"
        });
        exp.go();
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
          "target" : exp.target,
          "control" : exp.control,
          "catch_trials" : [],
          "system" : exp.system,
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
  exp.word_types = _.shuffle([
    {
      "truly": "target",
      "madly": "control"
    },
    {
      "truly": "control",
      "madly": "target"
    }
  ])[0]
  exp.target = exp.word_types["truly"] == "target" ? "truly" : "madly";
  exp.control = exp.word_types["truly"] == "target" ? "madly" : "truly";

  exp.system = {
      Browser : BrowserDetect.browser,
      OS : BrowserDetect.OS,
      screenH: screen.height,
      screenUH: exp.height,
      screenW: screen.width,
      screenUW: exp.width
    };
  //blocks of the experiment:
  exp.structure=["i0", "instructions", "story", "target_question", "frequency_question", "summary", "subj_info", "thanks"];
  
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