/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_source"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_source',
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
      probe: {
        type: jsPsych.plugins.parameterType.OBJECT,
        pretty_name: 'Dictionary with info on a specific probe. ',
        default: [],
        description: 'This dictionary contrains which trial this probe image was associated with, the context that trial was in and the probe and context image ids.'
      },
      source_contexts: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of Contexts.',
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
    49: '1',
    50: '2',
    51: '3',
    52: '4',
    53: '5',
    54: '6'
  }

  // make sure to hide deck and destination
  trial.miscell[4].hide();
  trial.miscell[5].hide();

  // show probe
  trial.miscell[10].show();
  trial.probe["probe"].show();

  console.log("source contexts")
  console.log(trial.source_contexts)

  // show prompt and options
  for (let i = 0; i < 7; i++) {
    console.log(i)
    trial.source_contexts[i].show();
 }

  // did they get it correct ?;
  var show_choice= function() {
    let key_pressed = response_key_dict[response.key]


    // hide prompt and unchosen options
    for (let i = 1; i < 7; i++) {
      trial.source_contexts[i].hide();
    }
    trial.source_contexts[0].show(); // hide all contexts except the chosen one
    trial.source_contexts[key_pressed].show(); // hide all contexts except the chosen one
    jsPsych.pluginAPI.setTimeout(function(){trial.source_contexts[key_pressed].hide();
      trial.miscell[10].hide();trial.source_contexts[0].hide();trial.probe["probe"].hide();end_trial();}, 1000);
  }

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
  show_choice();
  console.log('here 4')
};

// start the response listener
if (trial.choices != jsPsych.NO_KEYS) {
  var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
    callback_function: after_response,
    valid_responses: trial.choices,
    rt_method: 'performance',
    persist: false,
    allow_held_key: false});
};

// end trial if trial_duration is set
if (trial.trial_duration !== null) {
  jsPsych.pluginAPI.setTimeout(function () {
    if (response.key == null) {
      console.log('here ooop')
      // kill keyboard listeners
      jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);

      // hide everything on screen
      trial.probe['probe'].hide()
      trial.miscell[10].hide();

      // show prompt and options
      for (let i = 0; i < 7; i++) {
        trial.source_contexts[i].hide();
      }

      // tell them they are too slow
      trial.miscell[2].show();
      jsPsych.pluginAPI.setTimeout(function(){trial.miscell[2].hide(); end_trial();}, 3000);
    }
  }, trial.trial_duration);
};

}
return plugin;
})();
