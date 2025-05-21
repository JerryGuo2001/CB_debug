'use strict';

// subject_id to index in

function numberRange (start, end) {
  return new Array(end - start).fill().map((d, i) => i + start);
}

function getRandom(arr, n) {
    var result = new Array(n),
        len = arr.length,
        taken = new Array(len);
    if (n > len)
        throw new RangeError("getRandom: more elements taken than available");
    while (n--) {
        var x = Math.floor(Math.random() * len);
        result[n] = arr[x in taken ? taken[x] : x];
        taken[x] = --len in taken ? taken[len] : len;
    }
    return result;
}

var context_offset = 10;
var sample_window = 10; // first sample_window trials of each context are available for later memory probes
var num_blocks = 6;
var num_contexts = 6;
var block_len =40;
var ctx_bump = 2;

var num_bandits = 3;
var init_payoff = [60,30,10];
var decayTheta = init_payoff;
var payoff_bounds = [5,95];
var decay_lambda = 0.6;
var drift_sigma = 8;
var drift_noise = 8;

var num_trials = block_len * num_blocks * 2;
var num_probes = sample_window * (num_blocks-1);
var num_invalid_probes = Math.round(num_probes/5);
num_probes = num_probes + num_invalid_probes;
var mean_ct = 5;
var max_ct = 8;
var min_ct = 2;

const isAboveThreshold = (currentValue) => currentValue > max_ct;
const isBelowThreshold  = (currentValue) => currentValue < min_ct;
const sum_mer = (accumulator, curr) => accumulator + curr;
const cumulativeSum = (sum => value => sum += value)(0);
const mean = (array) => array.reduce((a, b) => a + b) / array.length;

var rotation_trials = [40,80,120,160,200,240,280,320,360,400,440];
var deterministic_trials = [30,70,110,150,190];

for (let i = 0; i < num_contexts; i++) {
  for (let j = 1; j < ctx_bump; j++) {
    var trial_to_add = deterministic_trials[i] + j;
    deterministic_trials.push(trial_to_add)
  }
}
