/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_trial"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_trial',
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
      rewards: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of Rewards.',
        default: [],
        description: 'All the rewards that could be displayed on the screen.'
      },
      miscell: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of miscelleanous things.',
        default: [],
        description: 'All the miscelleanous things that could be displayed on the screen.'
      },
      choice_outcomes: {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of the outcomes for choosing each pirate',
        default: [],
        description: 'The outcome of each possible choice.'
      },
      probes : {
        type: jsPsych.plugins.parameterType.ARRAY,
        pretty_name: 'Array of miscelleanous things.',
        default: [],
        description: 'All the miscelleanous things that could be displayed on the screen.'
      },
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
    51: '3'
  }

  console.log(trial.miscell)

  trial.pirates[0].show();
  trial.miscell[4].show();
  trial.miscell[5].show();

  var show_outcome = function() {
    trial.pirates[0].hide();
    let key_pressed = response_key_dict[response.key]
    trial.pirates[key_pressed].show();
    // get predetermined reward based on choice
    reward = trial.choice_outcomes[key_pressed-1]; // this is a list and js uses zero-indexing that's why need to subtract

    if (trial.miscell[5].id == 'blank') { // final context
      // set timer for showing choice
      jsPsych.pluginAPI.setTimeout(function(){trial.rewards[reward].show();}, 1000);

      // set timer for showing outcome of choice (1.5)
      jsPsych.pluginAPI.setTimeout(function(){trial.pirates[key_pressed].hide();
        trial.rewards[reward].hide();end_trial();}, 2500);
        //trial.rewards[reward].hide();trial.miscell[4].hide();end_trial();}, 2500);

    } else {
      jsPsych.pluginAPI.setTimeout(function(){
        trial.miscell[10].show();trial.miscell[0].show();trial.miscell[1].show();}, 1000);

      // set timer for showing probe (2 sec)
      jsPsych.pluginAPI.setTimeout(function(){trial.rewards[reward].show();}, 3000);

      // set timer for showing outcome of choice (1.5)
      jsPsych.pluginAPI.setTimeout(function(){trial.miscell[0].hide(); trial.miscell[1].hide();trial.pirates[key_pressed].hide();
        trial.rewards[reward].hide();trial.miscell[10].hide();end_trial();}, 4500);
        //trial.rewards[reward].hide();trial.miscell[10].hide();trial.miscell[4].hide();trial.miscell[5].hide();end_trial();}, 4500);

    }

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
  show_outcome();
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
      // kill keyboard listeners
      jsPsych.pluginAPI.cancelKeyboardResponse(keyboardListener);

      // hide everything on screen
      trial.miscell[4].hide();
      trial.miscell[5].hide();
      trial.pirates[0].hide();
      //trial.pirates[1].hide();

      // show time out screen
      trial.miscell[2].show();
      jsPsych.pluginAPI.setTimeout(function(){trial.miscell[2].hide(); end_trial();}, 3000);
    }
  }, trial.trial_duration);
};

}
return plugin;
})();
