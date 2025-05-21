'use strict';

////////// PAVLOVIA ////////////
var pavlovia_init = {
	type: "pavlovia",
	command: "init"
};

var pavlovia_finish = {
	type: "pavlovia",
	command: "finish"
	};
///////////////////////////////
// load in json file


// Initialize canvas
var canvas = $('#canvasdiv');

var urlvar = jsPsych.data.urlVariables()

// get subj id from url
var participant_id = jsPsych.data.getURLVariable('participant_ID');
var subject_id = jsPsych.data.getURLVariable('subject_ID');
var age = jsPsych.data.getURLVariable('age');
var gender = jsPsych.data.getURLVariable('gender');

function makeObject(img_path,id) {
  // create the image
	let image = $('<img>', {
	    src: img_path,
			width:"1000px",
	      // width: SIZE,
	   })
	  .appendTo(canvas)
		.css("position","absolute") // always string
		.css("padding-top","100px") // always string
		.css("left","0") // always string
		.css("right","0") // always string
		.css("margin-left","auto") // always string
		.css("margin-right","auto") // always string
  // add to canvas once loaded
  //image.onload = function(){
    //ctx.drawImage(image,canvas.width / 2 - (image.width*0.6) / 2,
    //canvas.height - (image.height*0.6)/1.0,image.width*0.6, image.height*0.6);
//  }

  window.image = image;

  return {
    hide() {
      image.hide();
    },
    show () {
      image.show();
    },
    image: image,
    id: id,
  }
};

function randomize(array) {
  array = array
    .map((value) => ({ value, sort: Math.random() }))
    .sort((a, b) => a.sort - b.sort)
    .map(({ value }) => value)
    return array;
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// load in data
let contextArray_ind = exp_struc[participant_id]["contextArray_ind"];
let valid_probe_images_ind = exp_struc[participant_id]["validImageProbes"];
let invalid_probe_images_ind = exp_struc[participant_id]["invalidImageProbes"];
let pRew_Red = exp_struc[participant_id]["pRew_Red"];
let pRew_White = exp_struc[participant_id]["pRew_White"];
let pRew_Black = exp_struc[participant_id]["pRew_Black"];
let availableForMemProbe = exp_struc[participant_id]["availableForMemProbe"];
let choice_blocks = exp_struc[participant_id]["choice_blocks"];

console.log(choice_blocks)

var mem_probe_trials = choice_blocks.map(function (x) {return x+1}).map(cumulativeSum).map(function (x) {return x+((block_len*num_blocks)-10)})
console.log(mem_probe_trials)

mem_probe_trials = mem_probe_trials.map(function (x) {return x-1}) // zero-indexing
console.log(mem_probe_trials)

var trial_nums = numberRange(0,num_trials)
var choice_trials = trial_nums.filter(function(x) { return mem_probe_trials.indexOf(x) < 0 }) // set difference

// randomize the list of memory probe indexes
// mem_probe_trials = mem_probe_trials
//   .map((value) => ({ value, sort: Math.random() }))
//   .sort((a, b) => a.sort - b.sort)
//   .map(({ value }) => value)

	console.log(mem_probe_trials)

// Now take the first numInvalidProbes # of indexes.
// These will be the invalid probes (novel images).
var invalidProbeTrials  = getRandom(mem_probe_trials,num_invalid_probes)
console.log(invalidProbeTrials)
////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var deck = makeObject('run_exp/static/images/miscellaneous/deck.png','deck') // stays constant thru out exp

let contextArray_true = [makeObject('run_exp/static/images/contexts/context_coast.png','coast'),
makeObject('run_exp/static/images/contexts/context_countryside.png','countryside'),
makeObject('run_exp/static/images/contexts/context_mountain.png','mountain'),
makeObject('run_exp/static/images/contexts/context_forest.png','forest'),
makeObject('run_exp/static/images/contexts/context_highway.png','highway'),
makeObject('run_exp/static/images/contexts/context_city.png','city')]
let contextArray = []
for (let i = 0; i < 6; i++) {
  contextArray.push(contextArray_true[contextArray_ind[i]])
}
contextArray.push(makeObject('run_exp/static/images/contexts/context_blank.png','blank'))
contextArray.push(makeObject('run_exp/static/images/contexts/context_desert.png','desert'))
contextArray.push(makeObject('run_exp/static/images/contexts/context_cavern.png','cavern'))
contextArray.forEach(context => context.hide())


var cargo_ship = makeObject('run_exp/static/images/miscellaneous/cargo_ship.png','cargo_ship');

let shipArray = [makeObject('run_exp/static/images/travel/ship_up.png','ship_up'), // ship up
makeObject('run_exp/static/images/travel/ship_down.png','ship_down')] // ship down] // map for welcome
shipArray.forEach(item => item.hide())

let pirateArray = [makeObject('run_exp/static/images/pirates/pirates_all.png','all'), // all pirates
makeObject('run_exp/static/images/pirates/red_beard.png','red'), // red beard
makeObject('run_exp/static/images/pirates/white_beard.png','white'), // white beard
makeObject('run_exp/static/images/pirates/black_beard.png','black')] // black beard
pirateArray.forEach(pirate => pirate.hide())

let rewardArray = [makeObject('run_exp/static/images/rewards/reward_no.png','noReward'), // nothing
makeObject('run_exp/static/images/rewards/reward.png','reward'),
makeObject('run_exp/static/images/rewards/reward_small.png','small_reward'),
makeObject('run_exp/static/images/rewards/money_bag.png','money_bag')] // money bag
rewardArray.forEach(reward => reward.hide())

// make array of probe images
let valid_probe_images_true = [];
let invalid_probe_images_true = [];
let prac_probe_images = [];

for (let i = 1; i < 10; i++) {
  valid_probe_images_true.push(makeObject('run_exp/static/images/probes/probes-0'+i+'.png','probe-0'+i))
}

for (let i = 10; i < 231; i++) {
  valid_probe_images_true.push(makeObject('run_exp/static/images/probes/probes-'+i+'.png','probe-'+i))
}


for (let i = 231; i < 256; i++) {
  invalid_probe_images_true.push(makeObject('run_exp/static/images/probes/probes-'+i+'.png','probe-'+i))
}

for (let i = 256; i < 266; i++) {
  prac_probe_images.push(makeObject('run_exp/static/images/probes/probes-'+i+'.png','probe-'+i))
}

let valid_probe_images = [];
let invalid_probe_images = [];

for (let i = 0; i < 230; i++) {
  valid_probe_images.push(valid_probe_images_true[valid_probe_images_ind[i]])
}

for (let i = 0; i < 25; i++) {
  invalid_probe_images.push(invalid_probe_images_true[invalid_probe_images_ind[i]])
}

// hide the array of images
valid_probe_images.forEach(image => image.hide())
invalid_probe_images.forEach(image => image.hide())
prac_probe_images.forEach(image => image.hide())

var contexts = [];
var current_trial = 230;
var current_context = 5;
var context_pick_n = 0;
var current_prac_trial = 0;
var corr_old_new_judge = 0;

let miscellArray = [makeObject('run_exp/static/images/probes/gas.png','probe'), // probe
makeObject('run_exp/static/images/miscellaneous/remember.png','remember'), // remember
makeObject('run_exp/static/images/miscellaneous/hurry_up.png','hurryUp'), // hurry up
makeObject('run_exp/static/images/miscellaneous/probe_recog.png','confidence_rate'),//confidence ratings
deck,
contextArray[7],
makeObject('run_exp/static/images/rewards/feedback_correct.png','correct'),
makeObject('run_exp/static/images/rewards/feedback_incorrect.png','incorrect'),
makeObject('run_exp/static/images/travel/ahoy.png','ahoy'),
makeObject('run_exp/static/images/travel/bye.png','bye'),cargo_ship]
miscellArray.forEach(item => item.hide())

let welcomeArray_true = [makeObject('run_exp/static/images/travel/welcome_coast.png','welcome_coast'),
makeObject('run_exp/static/images/travel/welcome_meadow.png','welcome_meadow'),
makeObject('run_exp/static/images/travel/welcome_mountain.png','welcome_mountain'),
makeObject('run_exp/static/images/travel/welcome_forest.png','welcome_forest'),
makeObject('run_exp/static/images/travel/welcome_road.png','welcome_road'),
makeObject('run_exp/static/images/travel/welcome_city.png','welcome_city')]
let welcomeArray = []
for (let i = 0; i < 6; i++) {
  welcomeArray.push(welcomeArray_true[contextArray_ind[i]])
}
welcomeArray.push(makeObject('run_exp/static/images/travel/welcome_final.png','welcome_final'))
welcomeArray.push(makeObject('run_exp/static/images/travel/welcome_desert.png','welcome_desert'))
welcomeArray.push(makeObject('run_exp/static/images/travel/welcome_cavern.png','welcome_cavern'))

welcomeArray.forEach(item => item.hide())

let sourceArray = [makeObject('run_exp/static/images/contexts/source_question.png','source_question'),
makeObject('run_exp/static/images/contexts/source_1_highway.png','source_1'),
makeObject('run_exp/static/images/contexts/source_2_coast.png','source_2'),
makeObject('run_exp/static/images/contexts/source_3_mountain.png','source_3'),
makeObject('run_exp/static/images/contexts/source_4_forest.png','source_4'),
makeObject('run_exp/static/images/contexts/source_5_city.png','source_5'),
makeObject('run_exp/static/images/contexts/source_6_countryside.png','source_6')]

sourceArray.forEach(item => item.hide())

let bestArray = [makeObject('run_exp/static/images/pick_best/best_question.png','best_question'),
makeObject('run_exp/static/images/pick_best/second_best_question.png','second_best_question'),
makeObject('run_exp/static/images/pick_best/red_best.png','red_best'),
makeObject('run_exp/static/images/pick_best/white_best.png','white_best'),
makeObject('run_exp/static/images/pick_best/black_best.png','black_best'),
makeObject('run_exp/static/images/pick_best/red_second_best.png','red_second_best'),
makeObject('run_exp/static/images/pick_best/white_second_best.png','white_second_best'),
makeObject('run_exp/static/images/pick_best/black_second_best.png','black_second_best')]

bestArray.forEach(item => item.hide())


var probabilities = { 1: pRew_Red,
  2: pRew_White,
  3: pRew_Black,
}

var response_key_dict = {
  49: '1',
  50: '2',
  51: '3',
  52: '4',
  53: '5',
  54: '6',
  55: '7',
  56: '8'
}

var probe_dict = [];

var intertrial = {
	type: 'html-keyboard-response',
  stimulus:"<p></p>",
  choices: jsPsych.NO_KEYS,
	trial_duration: 1000
}

var welcome = {
  type: 'do_welcome',
  stimuli: miscellArray,
  trial_duration:4000,
	on_start: function (welcome) {
		current_context += 1; // next this in welcome
		miscellArray[5] = contextArray[current_context];
		welcome.stimuli = miscellArray;
		welcome.text = welcomeArray[current_context];
	}
}

	var pick_best = {
  type: 'do_pick_best_pirate',
  choices: ['1','2','3'],
  pirates: pirateArray,
  context: null,
  miscell: miscellArray,
  on_start: function(pick_best) {
    let context_im = contextArray[context_pick_n];
    pick_best.context = context_im;
    pick_best.context_n = context_pick_n;
    pick_best.context_id = context_im.id;
},
on_finish: function(data) {
  let last_trial_data = jsPsych.data.get().last(1).values()[0];
  data.context_n = last_trial_data.context_n;
  data.context_id = last_trial_data.context_id;
  context_pick_n += 1;
}};

var bye = {
  type: 'do_bye',
  stimuli: miscellArray,
  trial_duration:1500
}

var take_break = {
	type: 'html-keyboard-response',
  stimulus:"<div class='center'><p>Time to take a quick break! You have 2 minutes to rest, but you can move on sooner if you'd like.</p><p> Press the <b>space bar</b> to continue.</p></div>",
	trial_duration:120000,
  choices: ['space'],
	on_finish: function(data) {
		data.trial_type="break"
	}
}

var travel = {
  type: 'do_travel',
  stimuli: shipArray,
  choices:jsPsych.NO_KEYS,
  frame_time: 1000
}


var choice_trial = {
  type: 'do_trial',
  choices: ['1','2','3'],
  prompt:'<p>which pirate do you want to rob the next ship?</p>',
  trial_duration: 3000,
  pirates: pirateArray,
  rewards: rewardArray,
  miscell: null,
  probes: valid_probe_images,
  on_start: function(choice_trial) {
    miscellArray[0] = valid_probe_images[current_trial];

    let potential_outcomes = [];
    for (let i = 1; i < 4; i++) {
      var outcome = Sampling.Bernoulli(probabilities[i][0]).draw()
      potential_outcomes.push(outcome)
    }

    choice_trial.choice_outcomes = potential_outcomes;
    choice_trial.miscell = miscellArray;
    choice_trial.data = {choice_outcomes : potential_outcomes};
  },
  on_finish: function (data) {
    // if no response was made need to remove this trial from available for probeing
    if (data.response == null) {
      availableForMemProbe = availableForMemProbe.filter((ar)=> ar != current_trial)
    }

    let last_trial_data = jsPsych.data.get().last(1).values()[0];
    let choice_outcomes = last_trial_data.choice_outcomes;
    let key_pressed =response_key_dict[data.response];

    data.context_n = current_context;
    contexts[current_trial] = current_context;
    data.context_img_id = contextArray[current_context].id;
    data.trial_n = current_trial;
		if (contextArray[current_context].id == 'blank') {
			data.probe_img_id = 'null';
		} else {
			data.probe_img_id = valid_probe_images[current_trial].id;
		}
    current_trial += 1;
    data.pR_red = probabilities[1].shift();
    data.pR_white = probabilities[2].shift();
    data.pR_black = probabilities[3].shift();
    data.outcome = choice_outcomes[key_pressed-1];
  }
}

var choice_trial_2 = {
  type: 'do_trial',
  choices: ['1','2','3'],
  prompt:'<p>which pirate do you want to rob the next ship?</p>',
  trial_duration: 3000,
  pirates: pirateArray,
  rewards: rewardArray,
  miscell: null,
  probes: valid_probe_images,
	after_blank: true,
  on_start: function(choice_trial) {
    miscellArray[0] = valid_probe_images[current_trial];

    let potential_outcomes = [];
    for (let i = 1; i < 4; i++) {
      var outcome = Sampling.Bernoulli(probabilities[i][0]).draw()
      potential_outcomes.push(outcome)
    }

    choice_trial.choice_outcomes = potential_outcomes;
    choice_trial.miscell = miscellArray;
    choice_trial.data = {choice_outcomes : potential_outcomes};
  },
  on_finish: function (data) {
    // if no response was made need to remove this trial from available for probeing
    if (data.response == null) {
      availableForMemProbe = availableForMemProbe.filter((ar)=> ar != current_trial)
    }

    let last_trial_data = jsPsych.data.get().last(1).values()[0];
    let choice_outcomes = last_trial_data.choice_outcomes;
    let key_pressed =response_key_dict[data.response];

    data.context_n = current_context;
    contexts[current_trial] = current_context;
    data.context_img_id = contextArray[current_context].id;
    data.trial_n = current_trial;
		if (contextArray[current_context].id == 'blank') {
			data.probe_img_id = 'null';
		} else {
			data.probe_img_id = valid_probe_images[current_trial].id;
		}
    current_trial += 1;
    data.pR_red = probabilities[1].shift();
    data.pR_white = probabilities[2].shift();
    data.pR_black = probabilities[3].shift();
    data.outcome = choice_outcomes[key_pressed-1];
  }
}

var mem_trial = {
  type: 'do_recog',
  choices: ['5','6','7','8'],
  prompt:'<p>do you remember this?</p>',
  trial_duration: 3000,
  trial_num: current_trial,
  rewards: rewardArray,
  miscell: miscellArray,
  on_start: function (mem_trial) {
		console.log(current_trial)
    // is this a trial to show a valid or invalid probe
    if ((invalidProbeTrials.includes(current_trial)) | (availableForMemProbe.length == 0)) {
      // show new probe invalid
      mem_trial.is_valid = 0;
      mem_trial.image_to_show = invalid_probe_images[0];

    } else {
      mem_trial.is_valid = 1;
      let probe_index = availableForMemProbe[0];
      mem_trial.image_to_show = valid_probe_images[probe_index];
    }
    mem_trial.data = {is_valid : mem_trial.is_valid, image_to_show: mem_trial.image_to_show};

  },
  on_finish: function (data) {
    data.trial_n = current_trial;
    current_trial += 1;
    let key_pressed = response_key_dict[data.response];
    let last_trial_data = jsPsych.data.get().last(1).values()[0];
    if (last_trial_data.is_valid == 0) {
      data.probed_trial = null;
      data.probed_context = null;
      // pop first image
      invalid_probe_images.shift();
      // pop trial number
      invalidProbeTrials.shift();
      if ((key_pressed == 7) | (key_pressed == 8)) {
        data.correct = 1;
				window.corr_old_new_judge += 1;
      } else {
        data.correct = 0;
      }
    } else { // showed valid probe
      data.probed_trial = availableForMemProbe[0];
      data.probed_context = Math.floor(availableForMemProbe[0]/block_len);
      // pop trial number
      availableForMemProbe.shift();

      if ((key_pressed == 5) | (key_pressed == 6)) {
        data.correct = 1;
				window.corr_old_new_judge += 1;
      } else {
        data.correct = 0;
      }

    }
    data.probe_img_id = last_trial_data.image_to_show.id;

		//  9/19/22 this should be inside conditional, currently invalid probes are now shown on source memory trials
		var probe_info = {"probe":last_trial_data.image_to_show,"probed_trial_n": availableForMemProbe[0],
		 	"probed_context_n":Math.floor(availableForMemProbe[0]/block_len)}
		probe_dict.push(probe_info)
  }
}

var source_trial = {
	type: 'do_source',
	choices: ['1','2','3','4','5','6'],
	trial_duration: 3000,
	source_contexts: sourceArray,
	miscell: miscellArray,
	on_start: function(source_trial) {
		source_trial.probe = probe_dict.shift();
		source_trial.data = {probe : source_trial.probe["probe"].id,
		probed_trial_n: source_trial.probe["probed_trial_n"],
		probed_context_n: source_trial.probe["probed_context_n"]};
	},
	on_finish: function(data) {
		let key_pressed = response_key_dict[data.response];
		data.correct = ((data.probed_context_n+1) == key_pressed);
	}
}

var pick_best_trial = {
	type: 'do_pick_best_pirate',
	choices: ['1','2','3'],
	pirates: pirateArray,
	context: contextArray[0],
	best_array: bestArray,
	miscell: miscellArray,
	on_start: function(pick_best_trial) {
		pick_best_trial.context = contextArray[pick_ind];
	},
	on_finish: function(data) {
		data.context_n = pick_ind;
		data.context_im = contextArray[pick_ind].id;
		pick_ind += 1; // 01.15.2022
	}
}

var pick_secondBest_trial = {
	type: 'do_pick_secondBest_pirate',
	choices: ['1','2','3'],
	pirates: pirateArray,
	context: contextArray[0],
	best_array: bestArray,
	miscell: miscellArray,
	on_start: function(pick_secondBest_trial) {
		pick_secondBest_trial.context = contextArray[pick_ind];
	},
	on_finish: function(data) {
		data.context_n = pick_ind;
		data.context_im = contextArray[pick_ind].id;
		//pick_ind += 1; 01.15.2022
	}
}

////////////////////////////////////////////////////////////////////////////////

function  make_test_phase() {
	var n_trial = 0
	console.log()
	for (let i = 0; i < choice_blocks.length; i++) {
	  for (let j = 0; j < choice_blocks[i]; j++) {
			n_trial += 1
			console.log(n_trial)
			if (((n_trial%40)==0) & (n_trial != 0)) {
				console.log('break')
				jsPsych.addNodeToEndOfTimeline({timeline: [take_break,choice_trial,intertrial],}, jsPsych.resumeExperiment);
			} else {
				jsPsych.addNodeToEndOfTimeline({timeline: [choice_trial,intertrial],}, jsPsych.resumeExperiment);
			}
	  }
		n_trial += 1
		if (((n_trial%40)==0) & (n_trial != 0)) {
			console.log('break')
			jsPsych.addNodeToEndOfTimeline({timeline: [take_break,mem_trial,intertrial],}, jsPsych.resumeExperiment);
		} else {
			jsPsych.addNodeToEndOfTimeline({timeline: [mem_trial,intertrial],}, jsPsych.resumeExperiment);
		}
	}

	jsPsych.addNodeToEndOfTimeline({timeline: [begin_source_mem],}, jsPsych.resumeExperiment);
	console.log(timeline)

}

function make_source_mem() {
	for (let i = 0; i < probe_dict.length; i++) {
		jsPsych.addNodeToEndOfTimeline({timeline: [source_trial],}, jsPsych.resumeExperiment);
	}
	make_rank_pirate() // 01/15/2022
	//make_end() 01/15/2022
	//jsPsych.addNodeToEndOfTimeline({timeline: [begin_pick_best],}, jsPsych.resumeExperiment); NH: 12/30/21
}

function make_rank_pirate() { //NH: 12/30/21
	for (let i = 0; i < 6; i++) {
		//jsPsych.addNodeToEndOfTimeline({timeline: [pick_best_trial,pick_secondBest_trial],}, jsPsych.resumeExperiment);
		jsPsych.addNodeToEndOfTimeline({timeline: [pick_best_trial],}, jsPsych.resumeExperiment); // 01.15.2022
	}
	make_end()
}

function make_end() {
    jsPsych.addNodeToEndOfTimeline({timeline: [all_done,pavlovia_finish],}, jsPsych.resumeExperiment);
}

var pick_ind = 0;

var timeline = []

// timeline.push({
// 	type: 'fullscreen',
// 	fullscreen_mode: true
// });

timeline.push(pavlovia_init);
timeline.push(welc_back);
timeline.push(move_forward);
timeline.push(day_1_summary_goal);
timeline.push(move_forward);
timeline.push(day_1_summary_pick);
timeline.push(move_forward);
timeline.push(test_phase_begin);
timeline.push(move_forward);
timeline.push(explain_probe_1);
timeline.push(move_forward);
timeline.push(explain_probe_2);
timeline.push(move_forward);
timeline.push(explain_outcome);
timeline.push(move_forward);
timeline.push(begin_test_phase);
timeline.push(move_forward);
timeline.push(welcome);


jsPsych.init({
  timeline: timeline,
  display_element: 'jspsych-target',
	preload_images: all_images,
	preload_audio: all_audio,
	use_webaudio:false,
	max_load_time: 600000,
	on_trial_start: function(data) {
		var interaction_data = jsPsych.data.getInteractionData();
		var blur_events = interaction_data.filter({event: 'blur'});
		var focus_events = interaction_data.filter({event: 'focus'});
		var fullscreenenter_events = interaction_data.filter({event: 'fullscreenenter'});
		var fullscreenexit_events = interaction_data.filter({event: 'fullscreenexit'});
		jsPsych.data.get().addToLast({interactions: interaction_data.csv()});
		jsPsych.data.get().addToLast({blur_events: blur_events.csv()});
		jsPsych.data.get().addToLast({focus_events: focus_events.csv()});
		jsPsych.data.get().addToLast({fullscreenenter_events: fullscreenenter_events.csv()});
		jsPsych.data.get().addToLast({fullscreenexit_events: fullscreenexit_events.csv()});},
	on_interaction_data_update: function (data) {
		var interaction_data = jsPsych.data.getInteractionData();
		var blur_events = interaction_data.filter({event: 'blur'});
		var focus_events = interaction_data.filter({event: 'focus'});
		var fullscreenenter_events = interaction_data.filter({event: 'fullscreenenter'});
		var fullscreenexit_events = interaction_data.filter({event: 'fullscreenexit'});
		jsPsych.data.get().addToLast({interactions: interaction_data.csv()});
		jsPsych.data.get().addToLast({blur_events: blur_events.csv()});
		jsPsych.data.get().addToLast({focus_events: focus_events.csv()});
		jsPsych.data.get().addToLast({fullscreenenter_events: fullscreenenter_events.csv()});
		jsPsych.data.get().addToLast({fullscreenexit_events: fullscreenexit_events.csv()});},
	on_close: function (data) {
		var interaction_data = jsPsych.data.getInteractionData();
		var blur_events = interaction_data.filter({event: 'blur'});
		var focus_events = interaction_data.filter({event: 'focus'});
		var fullscreenenter_events = interaction_data.filter({event: 'fullscreenenter'});
		var fullscreenexit_events = interaction_data.filter({event: 'fullscreenexit'});
		jsPsych.data.get().addToLast({interactions: interaction_data.csv()});
		jsPsych.data.get().addToLast({blur_events: blur_events.csv()});
		jsPsych.data.get().addToLast({focus_events: focus_events.csv()});
		jsPsych.data.get().addToLast({fullscreenenter_events: fullscreenenter_events.csv()});
		jsPsych.data.get().addToLast({fullscreenexit_events: fullscreenexit_events.csv()});},
  on_finish: function(data) {
		var interaction_data = jsPsych.data.getInteractionData();
		var blur_events = interaction_data.filter({event: 'blur'});
		var focus_events = interaction_data.filter({event: 'focus'});
		var fullscreenenter_events = interaction_data.filter({event: 'fullscreenenter'});
		var fullscreenexit_events = interaction_data.filter({event: 'fullscreenexit'});
		jsPsych.data.get().addToLast({interactions: interaction_data.csv()});
		jsPsych.data.get().addToLast({blur_events: blur_events.csv()});
		jsPsych.data.get().addToLast({focus_events: focus_events.csv()});
		jsPsych.data.get().addToLast({fullscreenenter_events: fullscreenenter_events.csv()});
		jsPsych.data.get().addToLast({fullscreenexit_events: fullscreenexit_events.csv()});
}
})
