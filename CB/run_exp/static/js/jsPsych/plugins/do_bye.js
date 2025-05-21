/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_bye"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_bye',
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
    }
  }

  plugin.trial = function (display_element, trial) {

    // store response
  var response = {
    rt: null,
    key: null
  };
  trial.stimuli[4].show();
  trial.stimuli[5].show();
  trial.stimuli[9].show();


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
    trial.stimuli[5].hide();
    trial.stimuli[9].hide();

    end_trial();
  }, trial.trial_duration);


}
return plugin;
})();
