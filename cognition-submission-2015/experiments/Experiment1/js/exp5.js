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
    present : exp.items,
    start : function() {/*what to do at the beginning of a block*/},
    present_handle : function(stim) {
      this.trial_data = _.clone(stim);
      var plural = stim.object == "headphones";
      $(".err").hide();
      $(".friend").html(stim.name);
      $(".object_article").html(plural ? "some" : "a");
      $(".object").html(color(stim.object));
      $(".description_intro").html(plural ? "They were" : "It was");
      if (stim.condition == "extreme") {
        $(".description_article").html("");
        $(".description").html("<b>" + stim.adverb + " expensive</b>");
      } else if (stim.condition == "average") {
        $(".description_article").html(stim.adverb.match(/^[aeiou].*$|^$/) == null ? "a" : "an");
        $(".description").html("<b>" + stim.adverb + " average price</b>");
      } else {
        console.log("error 2900");
      }
      $(".plain_object").html(stim.object);
      $("#response").focus();
    },
    button : function() {
      var response = $("#response").val();
      if (response.match(/^[0-9]*(\.[0-9][0-9])?$/) == null || response.length == 0) {
        $(".err").show();
      } else {
        this.trial_data["response"] = response;
        exp.data_trials.push(this.trial_data);
        $("#response").val("");
        _stream.apply(this);
      }
    },
    end : function() {/*what to do at the end of a block*/}
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

function get_items() {
  var adverbs = [
    "surpassingly", "astoundingly", "fantastically", "strikingly",
    "excessively", "markedly", "remarkably", "utterly", "truly",
    "particularly", "colossally", "phenomenally", "mightily", "acutely",
    "extraordinarily", "amazingly", "terribly", "notably", "significantly",
    "quite", "terrifically", "uncommonly", "supremely", "awfully",
    "exceedingly", "radically", "exceptionally", "incredibly", "totally",
    "especially", "frightfully", "outrageously", "insanely", "decidedly",
    "intensely", "unusually", "desperately", "seriously", "extremely",
    "very"
  ];
  var male_names = _.shuffle([
    "Michael", "Christopher", "Matthew", "Joshua", "Jacob", "Nicholas",
    "Andrew", "Daniel", "Tyler", "Joseph", "Brandon", "David", "James",
    "Ryan", "John", "Zach", "Justin", "Will", "Anthony", "Robert", "Austin",
    "Alexander", "Kyle", "Kevin", "Thomas", "Cody", "Jordan", "Eric", "Benjamin",
    "Aaron", "Christian", "Samuel", "Dylan", "Steven", "Brian", "Jose",
    "Timothy", "Nathan", "Adam", "Rick", "Patrick", "Charles", "Sean",
    "Jason", "Cameron", "Jeremy", "Mark", "Juan", "Travis", "Jeff", "Ethan",
    "Caleb", "Luis", "Jared", "Logan", "Hunter", "Trevor", "Evan", "Paul", "Kenneth",
    "Connor", "Dustin", "Noah", "Carlos", "Devin", "Gabriel", "Ian", "Greg",
    "Derek", "Corey", "Scott", "Bradley"
  ]);
  var female_names = _.shuffle([
    "Jessica", "Ashley", "Emily", "Sarah",
    "Samantha", "Amanda", "Brittany", "Elizabeth", "Megan", "Hannah", "Kayla",
    "Lauren", "Stephanie", "Rachel", "Jennifer", "Nicole", "Alexis", "Victoria",
    "Amber", "Alyssa", "Courtney", "Rebecca", "Danielle", "Jasmine", "Brianna",
    "Katherine", "Alexandra", "Madison", "Morgan", "Melissa", "Michelle", "Kelsey",
    "Chelsea", "Anna", "Kim", "Tiffany", "Olivia", "Mary", "Christina", "Allison",
    "Abigail", "Heather", "Haley", "Maria", "Kaitlyn", "Laura", "Erin", "Andrea",
    "Natalie", "Brooke", "Julia", "Emma", "Vanessa", "Erica", "Kelly", "Kristen",
    "Marissa", "Amy", "Crystal", "Paige", "Cassandra", "Gabrielle", "Katie",
    "Lindsey", "Destiny", "Kathryn", "Jacqueline", "Shannon", "Jenna", "Angela",
    "Savannah", "Miranda"
  ]);
  var items = []
  for (var i=0; i<adverbs.length; i++) {
    for (var j=0; j<exp.objects.length; j++) {
      var adverb = adverbs[i];
      var object = exp.objects[j];
      var gender = _.sample(["male", "female"]);
      var names = gender == "female" ? female_names : male_names;
      items.push({
        "adverb": adverb,
        "gender": gender,
        "object": object,
        "condition": "extreme",
        "name": names.pop()
      });
    }
  }
  return _.shuffle(items);
}

function color(object) {
  return "<b><font color='" + exp.colormap[object] + "'>" + object + "</font></b>";
}

/// init ///
function init() {
  exp.objects = _.shuffle(["coffee maker", "laptop", "watch"]);
  exp.items = get_items();
  colors = _.shuffle(["red", "orange", "green", "blue", "purple"]);
  exp.colormap = {}
  for (var i=0; i<exp.objects.length; i++) {
    exp.colormap[exp.objects[i]] = colors[i];
  }
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