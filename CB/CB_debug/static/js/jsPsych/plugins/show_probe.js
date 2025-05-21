/**
 * jspsych-c-keyboard-response
 * Chris Jungerius (modified from Josh de Leeuw)
 *
 * a jsPsych plugin for displaying a c stimulus and getting a keyboard response
 *
 * documentation: docs.jspsych.org
 *
 **/


jsPsych.plugins["show_probe"] = (function () {

  var plugin = {};

  plugin.info = {
    name: 'show_probe',
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
      stimulus_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Stimulus duration',
        default: null,
        description: 'How long to hide the stimulus.'
      },
      trial_duration: {
        type: jsPsych.plugins.parameterType.INT,
        pretty_name: 'Trial duration',
        default: null,
        description: 'How long to show trial before it ends.'
      },
      response_ends_trial: {
        type: jsPsych.plugins.parameterType.BOOL,
        pretty_name: 'Response ends trial',
        default: true,
        description: 'If true, trial will end when subject makes a response.'
      },
      c_size: {
        type: jsPsych.plugins.parameterType.INT,
        array: true,
        pretty_name: 'c size',
        default: [800, 1200],
        description: 'Array containing the height (first value) and width (second value) of the c element.'
      },
      context_image: {
        type: jsPsych.plugins.parameterType.FUNCTION,
        pretty_name: 'Context image',
        default: undefined,
        description: 'Place scene to indicate the current context.'
      },
      probe_image: {
        type: jsPsych.plugins.parameterType.FUNCTION,
        pretty_name: 'Probe image',
        default: undefined,
        description: 'Show probe.'
      }
    }
  }

  plugin.trial = function (display_element, trial) {

    var new_html = '<div id="jspsych-canvas-keyboard-response-stimulus">' + '<canvas id="myCanvas" height="' + trial.c_size[0] + '" width="' + trial.c_size[1] + '"></canvas>' + '</div>';
    // add prompt
    if (trial.prompt !== null) {
      new_html += trial.prompt;
    }
    // draw
    display_element.innerHTML = new_html;
    var c = document.getElementById("myCanvas");
    var ctx  = c.getContext('2d');

    function draw(c_ctx) {

    deck = new Image();
    deck.src = '../../make_stim/stims-04.png';

    deck.onload = function() {
      //c_ctx.drawImage(deck,c.width / 2 - (deck.width*0.6) / 2,
      //c.height / 2 - (deck.height*0.6)/ 2,deck.width*0.6, deck.height*0.6);
      c_ctx.drawImage(deck,c.width / 2 - (deck.width*0.6) / 2,
      c.height - (deck.height*0.6)/1.0,deck.width*0.6, deck.height*0.6);
    }

    ctx = new Image();
    ctx.src = trial.context_image;

    ctx.onload = function(){
      c_ctx.drawImage(ctx, c.width / 2 - (ctx.width*0.6) / 2,
      c.height- (ctx.height*0.6) / 1.0, ctx.width*0.6, ctx.height*0.6);
    }

    probe = new Image();
    probe.src = trial.probe_image;

    probe.onload = function(){
      c_ctx.drawImage(probe, c.width / 2 - (probe.width*0.6) / 2,
      c.height - (probe.height*0.6)/1.0,probe.width*0.6, probe.height*0.6);
    }
  }

  draw(ctx)

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

      // clear the display
      display_element.innerHTML = '';

      // move on to the next trial
      jsPsych.finishTrial(trial_data);
    };

    // function to handle responses by the subject
    var after_response = function (info) {

      // after a valid response, the stimulus will have the CSS class 'responded'
      // which can be used to provide visual feedback that a response was recorded
      display_element.querySelector('#jspsych-canvas-keyboard-response-stimulus').className += ' responded';

      // only record the first response
      if (response.key == null) {
        response = info;
      }

      if (trial.response_ends_trial) {
        end_trial();
      }
    };

    // start the response listener
    if (trial.choices != jsPsych.NO_KEYS) {
      var keyboardListener = jsPsych.pluginAPI.getKeyboardResponse({
        callback_function: after_response,
        valid_responses: trial.choices,
        rt_method: 'performance',
        persist: false,
        allow_held_key: false
      });
    }

    // hide stimulus if stimulus_duration is set
    if (trial.stimulus_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function () {
        display_element.querySelector('#jspsych-canvas-keyboard-response-stimulus').style.visibility = 'hidden';
      }, trial.stimulus_duration);
    }

    // end trial if trial_duration is set
    if (trial.trial_duration !== null) {
      jsPsych.pluginAPI.setTimeout(function () {
        end_trial();
      }, trial.trial_duration);
    }

  };

  return plugin;
})();
