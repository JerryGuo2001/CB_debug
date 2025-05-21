
/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_travel"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_travel',
    description: '',
    parameters: {
      frame_time: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      stimuli: {
        type: jsPsych.plugins.parameterType.Array,
        pretty_name: 'Array ',
        default: [],
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
var src = "run_exp/static/audio/sailing.mp3#t=2.0,"
var tt_sec = 9; //need to convert to seconds
var audio = new sound(src.concat(tt_sec.toString()))
audio.play()

  trial.stimuli[0].show();

  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[0].hide();
      trial.stimuli[1].show();
    }, trial.frame_time);

  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[1].hide();
      trial.stimuli[0].show();
    }, trial.frame_time*2);

  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[0].hide();
      trial.stimuli[1].show();
    }, trial.frame_time*3);

  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[1].hide();
      trial.stimuli[0].show();
    }, trial.frame_time*4);


  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[0].hide();
      trial.stimuli[1].show();
    }, trial.frame_time*5);

  jsPsych.pluginAPI.setTimeout(function () {
      trial.stimuli[1].hide();
      end_trial();
    }, trial.frame_time*6);



}
return plugin;
})();
