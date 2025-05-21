/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_pick_best_pirate"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_pick_best_pirate',
    description: '',
    parameters: {
      choices: {
        type: jsPsych.plugins.parameterType.KEY,
        array: true,
        pretty_name: 'Choices',
        default: jsPsych.ALL_KEYS,
        description: 'The keys the subject is allowed to press to respond to the stimulus.'
      },
      prompt: {
        type: jsPsych.plugins.parameterType.STRING,
        pretty_name: 'Prompt',
        default: null,
        description: 'Any content here will be displayed below the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      pirates: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of Pirates.',
        default: [],
        description: 'All the pirates that could be displayed on the screen.'
      },
      context: {
        type: jsPsych.plugins.parameterType.OBJECT,
        pretty_name: 'Contexts to show.',
        default: [],
        description: 'The contexts that can be chosen between.'
      },
      best_array: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Contexts to show.',
        default: [],
        description: 'The contexts that can be chosen between.'
      },
      miscell: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of miscelleanous things.',
        default: [],
        description: 'All the miscelleanous things that could be displayed on the screen.'
      }
    }
  }

  plugin.trial = function (display_element, trial) {

    // store response
  var response = {
    rt: null,
    key: null
  };

  var response_key_dict = {
    49: 1,
    50: 2,
    51: 3
  }

  trial.pirates[0].show();
  trial.context.show();
  trial.best_array[0].show();

  // var show_choice = function() {
  //
  //   //trial.pirates[0].hide();
  //   let key_pressed = response_key_dict[response.key]
  //
  //   trial.best_array[key_pressed+1].show();
  //
  //
  //   jsPsych.pluginAPI.setTimeout(function(){
  //     trial.best_array[0].hide();
  //     trial.best_array[1].show();
  //     end_trial();}, 500);
  // }

  // function to end trial when it is time
  var end_trial = function () {

  // kill any remaining setTimeout handlers
  jsPsych.pluginAPI.clearAllTimeouts();

  // kill keyboard listeners
  jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);


  // gather the data to store for the trial
  var trial_data = {
    rt: response.rt,
    response: response.key
  };

  trial.pirates[0].hide();
  trial.context.hide();
  trial.best_array[0].hide();

  // move on to the next trial
  jsPsych.finishTrial(trial_data);
};

// function to handle responses by the subject
var after_response = function (info) {

// after a valid response, the stimulus will have the CSS class 'responded'
// which can be used to provide visual feedback that a response was recorded
// display_element.querySelector('#jspsych-canvas-keyboard-response-stimulus').className += ' responded';

// only record the first response
  if (response.key == null) {
    response = info;
  }
  jsPsych.pluginAPI.cancelAllKeyboardResponses()
  //show_choice();
  end_trial()
};


var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
  callback_function: after_response,
  valid_responses: trial.choices,
  rt_method: 'performance',
  persist: false,
  allow_held_key: false});

}
return plugin;
})();
