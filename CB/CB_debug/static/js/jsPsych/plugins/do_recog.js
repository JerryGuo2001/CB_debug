/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/

jsPsych.plugins["do_recog"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'do_recog',
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
      trial_num: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'N trial we on',
        default: null,
        description: 'Trial number we are on '
      },
      is_valid: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Array of miscelleanous things.',
        default: [],
        description: 'all new items images'
      },
      image_to_show: {
        type: jsPsych.plugins.parameterType.OBJECT,
        pretty_name: 'Array of miscelleanous things.',
        default: [],
        description: 'all old items images'
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
        description: ''
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
    53: '5',
    54: '6',
    55: '7',
    56: '8'
  }
  console.log('trial')
  console.log(trial)

  trial.miscell[4].hide()
  trial.miscell[5].hide()

  // get confidence rating
  trial.image_to_show.show();
  trial.miscell[3].show()
  trial.miscell[10].show()


  var show_outcome = function() {
    trial.miscell[3].hide();
    key_pressed = response_key_dict[response.key]

    if ((trial.is_valid == 0) & ((key_pressed == 7) | (key_pressed == 8))) {
      // reward them for correct answer
      trial.rewards[2].show();
    } else if ((trial.is_valid == 1) & ((key_pressed == 5) | (key_pressed == 6))) {
      // reward them for correct answer
      trial.rewards[2].show();
    } else {
      trial.rewards[0].show();
    }

    jsPsych.pluginAPI.setTimeout(function(){trial.image_to_show.hide();trial.miscell[10].hide();trial.rewards[2].hide();trial.rewards[0].hide(); end_trial();}, 1500);

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

  console.log('trial_data')
  console.log(trial_data)

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

// end trial if trial_duration is set --> check what happens in aaron's task
if (trial.trial_duration !== null) {
  jsPsych.pluginAPI.setTimeout(function () {
    if (response.key == null) {
      // kill the keyboard
      jsPsych.pluginAPI.cancelAllKeyboardResponses()

      // hide everything currently on screen
      trial.image_to_show.hide();
      trial.miscell[3].hide();
      trial.miscell[10].hide();

      trial.miscell[2].show()
      jsPsych.pluginAPI.setTimeout(function(){trial.miscell[2].hide(); end_trial();}, 3000);
    }
  }, trial.trial_duration);
};

}
return plugin;
})();
