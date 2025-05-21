/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_welcome"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_welcome',
    description: '',
    parameters: {
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      stimuli: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array ',
        default: [],
        description: 'All the pirates that could be displayed on the screen.'
      },
      text: {
        type: jsPsych.plugins.parameterType.OBJECT,
        pretty_name: 'Array ',
        default: [],
        description: 'All the pirates that could be displayed on the screen.'
      },
      island_num: {
        type: jsPsych.plugins.parameterType.OBJECT,
        pretty_name: 'Array ',
        default: false,
        description: 'All the pirates that could be displayed on the screen.'
      },
    }
  }

  plugin.trial = function (display_element, trial) {

    // store response
  var response = {
    rt: null,
    key: null
  };

  function sound(src) {
    this.sound = document.createElement("audio");
    this.sound.src = src;
    this.sound.setAttribute("preload", "auto");
    this.sound.setAttribute("controls", "none");
    this.sound.style.display = "none";
    document.body.appendChild(this.sound);
    this.play = function(){
      this.sound.play();
    }
    this.stop = function(){
      this.sound.pause();
    }
  }
  var src = "run_exp/static/audio/pirates_life_for_me.mp3#t=0.0,"
  var tt_sec = 3.5; //need to convert to seconds
  var audio = new sound(src.concat(tt_sec.toString()))
  audio.play()

  trial.stimuli[4].show();
  trial.text.show();
  trial.stimuli[5].show();
  trial.stimuli[8].show();
  if (trial.island_num != false) {
    trial.island_num.show();
  }



  // function to end trial when it is time
  var end_trial = function () {

  // kill any remaining setTimeout handlers
  jsPsych.pluginAPI.clearAllTimeouts();

  // kill keyboard listeners
  if (typeof keyboardListener !== 'undefined') {
    jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);
  }

  // gather the data to store for the trial
  var trial_data = {
    rt: response.rt,
    response: response.key
  };

  // move on to the next trial
  jsPsych.finishTrial(trial_data);
};


  jsPsych.pluginAPI.setTimeout(function () {
    trial.stimuli[4].hide();
    trial.text.hide();
    trial.stimuli[5].hide();
    trial.stimuli[8].hide();
    if (trial.island_num != false) {
      trial.island_num.hide();
    }


    end_trial();
  }, trial.trial_duration);


}
return plugin;
})();
