'use strict';
console.log('hi')

function numberRange (start, end) {
  return new Array(end - start).fill().map((d, i) => i + start);
}

var context_offset = 10;
var sample_window = 10; // first sample_window trials of each context are available for later memory probes
var num_blocks = 6;
var num_contexts = 6;
var block_len = 30;
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

var log_rand = Array.apply(null, {length: num_probes}).map(x=>Math.random()).map(Math.log);
var log_rand_div_mean_ct = log_rand.map(function(x) {return x/(1/mean_ct)});
var choice_blocks = log_rand_div_mean_ct.map(Math.ceil).map(function(x){return (x*-1) + min_ct});
choice_blocks = choice_blocks.map(function(x) {if (x > max_ct) {return max_ct} else {return x}});


const isAboveThreshold = (currentValue) => currentValue > max_ct;
const isBelowThreshold  = (currentValue) => currentValue < min_ct;
const sum_mer = (accumulator, curr) => accumulator + curr;
const cumulativeSum = (sum => value => sum += value)(0);
const mean = (array) => array.reduce((a, b) => a + b) / array.length;



while ((choice_blocks.reduce(sum_mer) != (num_trials/2)) | (choice_blocks.some(isBelowThreshold)) | (choice_blocks.some(isAboveThreshold))) {
  // 1. randomly select index
  var ind = parseInt(Math.ceil(Math.random()*num_probes),10) - 1;
  // 2. make equal to num_trials/2
  choice_blocks[ind] = choice_blocks[ind] - Math.sign(choice_blocks.reduce(sum_mer) - (num_trials/2));
  // 3. ensure no element goes below threshold
  choice_blocks = choice_blocks.map(function(x) {if (x < min_ct) {return min_ct} else {return x}});
  // 4. ensure no element goes above threshold
  choice_blocks = choice_blocks.map(function(x) {if (x > max_ct) {return max_ct} else {return x}});
}

choice_blocks = choice_blocks.map(function (x) {return x-1})
console.log("choice_blocks")
console.log(choice_blocks)


var mem_probe_trials = choice_blocks.map(function (x) {return x+1}).map(cumulativeSum).map(function (x) {return x+(block_len*num_blocks)})
mem_probe_trials = mem_probe_trials.map(function (x) {return x-1}) // zero-indexing

var trial_nums = numberRange(0,num_trials)
var choice_trials = trial_nums.filter(function(x) { return mem_probe_trials.indexOf(x) < 0 }) // set difference

// randomize the list of memory probe indexes
mem_probe_trials = mem_probe_trials
  .map((value) => ({ value, sort: Math.random() }))
  .sort((a, b) => a.sort - b.sort)
  .map(({ value }) => value)

// Now take the first numInvalidProbes # of indexes.
// These will be the invalid probes (novel images).
var invalidProbeTrials  = mem_probe_trials.slice(0,num_invalid_probes).sort();

// Now first sampleWindow trials of each context are available for later memory probes

var availableForMemProbe = [];
for (let bIdx = 0; bIdx < num_blocks; bIdx++) {
    availableForMemProbe.push(numberRange(block_len*bIdx,((block_len*bIdx)+sample_window-1)))
}

// Initialize contexts and probe images
var contexts = Array.apply(null, Array(num_trials)).map(Number.prototype.valueOf,0);

// First context is short, 1-20 have probes
contexts = contexts.map(function(x) {if (x < (block_len-context_offset+1)) {return 0} else {return x}})


for (let ci = 1; ci < num_blocks; ci++) {
  var sp = (block_len - context_offset) + ((ci-1)*block_len);
  var ep = sp + block_len;
  for (let j = sp; j < ep; j++) {
    contexts[j] = ci;
  }
}

var last_block = Math.max(...contexts)+1;
for (let j = ep; j < contexts.length; j++) {
  contexts[j] = last_block;
}

var payout = new Array(num_bandits);

for (var i = 0; i < num_bandits; i++) {
  payout[i] = new Array(num_trials);
  payout[i][0] = init_payoff[i];
}

var drift = function(trial_idx) {
  if (((trial_idx+1) % block_len) == 0) {
    var bestOpt = decayTheta.indexOf(Math.max(...decayTheta)); // get index (bandit) of max value

    while (decayTheta[bestOpt] == Math.max(decayTheta)) { // while the index (bandit) of the max value is the same shuffle list --> change highest paying
      decayTheta = decayTheta
        .map((value) => ({ value, sort: Math.random() }))
        .sort((a, b) => a.sort - b.sort)
        .map(({ value }) => value)
    }
  }

  if (trial_idx > ((num_trials/2)-1)) {  // second half of the experiment
    // set the average payoff of each bandit to the mean of the bandits in the first half
    decayTheta = Array.apply(null, {length: num_bandits}).map(x=>mean(decayTheta))
  }

  if ((trial_idx+1) > 1) {  // after the first one

    // slower transitions for the first 2 trials
    if ((trial_idx+1)%block_len < 3) {
      var decay_lambda_eff = 0.95;
    } else {
      var decay_lambda_eff = decay_lambda;
    }

    for (var this_bandit = 0; this_bandit < num_bandits; this_bandit++) {
      payout[this_bandit][trial_idx] = decay_lambda_eff * payout[this_bandit][trial_idx-1] + (1-decay_lambda_eff)*decayTheta[this_bandit] + normalRandom()*drift_noise;

      // Reflect at specified boundaries [lower, upper]
      if (payout[this_bandit][trial_idx] > payoff_bounds[1]) { // if above then move it back in within bounds
          payout[this_bandit][trial_idx] = payoffBounds[1] - (payout[thisBandit][trial_idx] - payoff_bounds[1]);
      }

      if (payout[this_bandit][trial_idx] < payoff_bounds[0]) {
          payout[this_bandit][trial_idx] = payoff_bounds[0] + (payoff_bounds[0] - payout[this_bandit][trial_idx]);
      };
    };
  };
};

for (var trial_idx = 0; trial_idx < num_trials; trial_idx++) {
  drift(trial_idx)
};

console.log("choice_blocks")
console.log(choice_blocks)

console.log("choice_trials")
console.log(choice_trials)

console.log("mem probe trials")
console.log(mem_probe_trials)

console.log("invalid")
console.log(invalidProbeTrials)

console.log("available")
console.log(availableForMemProbe)

console.log("contexts")
console.log(contexts)

console.log("payout")
console.log(payout)
