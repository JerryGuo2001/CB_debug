console.log('hi')

var space_bar = "<p><b>[Press the space bar to continue]</b></p></div>";

var welcome_back = "<div class='center'><p>Welcome to day 2!</p><p> Yesterday, you were the head captain of a pirate ship. You asked your fellow pirates to rob other ships of their gold across six different islands.</p></div>"

var goal_summary = "<div class='center'><p>Today, like yesterday, you get to pick which pirate you want to rob the next ship.</p><p>If the pirate is successful in robbing a ship, you get gold coins that look like this: </p><p><img src='run_exp/static/images/tutorial/reward.png' height='150'></p><p> If they were not successful in robbing the ship, then you will get no gold coins and you'll see a big red x like this: </p><p><img src='run_exp/static/images/tutorial/reward_no.png' height='150'></p></div>"

var how_to_pick_summary = "<div class='center'><p>Also, just like yesterday, you will use the 1,2,3 keys on your keyboard to pick a pirate. Press '1' to choose red beard, '2' to choose white beard, and '3' to choose black beard . You only have 3 seconds to pick a pirate, so please make a choice quickly!</p></div>"

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

var final_place = "<div class='center'><p>You’ve arrived at the final island of your journey! Unfortunately, it’s very foggy out, so you won’t be able to see it in the distance like the other islands you've visited.</p><p>You’ll still be deciding which pirate you want to rob a ship like before. However, because of the fog, you also won’t be able to see the ship.</p><p>This means you do not have to remember the ships you rob on this island.</p><p>Unlike yesterday, you won't be visiting multiple islands today. You will stay on this island for the entire game. This game will last ~30 minutes.</p></div>";

var recognition_1 = "<div class='center'><p> Sometimes you’ll be shown a ship, and you will be asked if you saw this ship on a past island.</p><p> Here is an example of what you will see.</p><p><img src='run_exp/static/images/tutorial/example_recog_trial.png' height='500'></p></div>"

var recognition_2 = "<div class='center'><p> You will press the 5, 6, 7, 8 keys on your keyboard to respond.</p><p>If you are sure you saw the ship, press <b>5</b> on your keyboard.</p><p> If you think you saw it before but aren't sure, then press <b>6</b>.</p><p> If you think you have <b>not</b> seen it before but aren't sure, then press <b>7</b>.</p><p> If you are sure you did <b>not</b> see the ship before, then press <b>8</b>.</p><p><img src='run_exp/static/images/tutorial/example_recog_trial.png' height='500'></p></div>"

var recognition_3 = "<div class='center'><p> If you remember correctly, then you’ll get a gold coin like this: </p><p><img src='run_exp/static/images/tutorial/reward_small.png' height='200'></p><p>If you do not remember correctly, then you'll see a red x like this: </p><p><img src='run_exp/static/images/tutorial/reward_no.png' height='200'></p><p>Importantly, when you are shown a ship, take the time to remind yourself which island you saw it on. This will help you in the next part of the study.</p></div>"

var begin_final = "<div class='center'><p>Let's go over the important points again. Your job is to: </p><p>1. Pick the pirate who is best at robbing ships and will bring you back gold coins. </p><p>2. Correctly remember whether or not you saw a ship before. If you did see the ship before, remind yourself on which island you saw it. </p></div>"

var source_memory = "<div class='center'><p> Ok, you’re almost done! In this part, you’ll see a ship you saw yesterday, and you will have to pick the island on which you saw it using the keys 1, 2, 3, 4, 5, 6 on your keyboard. The number above each picture tells you which key to press to pick that island. Every time you pick the right island you’ll win some more bonus money, so try your best to remember! </p><p><img src='run_exp/static/images/tutorial/example_source_memory.png' height='500'></p></div>"

var pick_best_pirate = "<div class='center'><p> Ok, this is your final game of the day. You’ll be shown an island, and you’ll have to pick the pirate you thought was <b>the best</b> at robbing ships on that island. You will use the 1, 2, 3 keys on your keyboard. Just like before, press '1' to choose red beard, '2' to choose white beard, and '3' to choose black beard. Once you pick a pirate, a gold box will surround your choice. </p><p>Then, you’ll have to pick the pirate you thought was <b>the second best</b> at robbing ships on that island. Once you pick a pirate, a silver box will surround your choice. Then, you’ll move on to the next island.  </p><p><img src='run_exp/static/images/tutorial/example_pick_best.png' height='500'></p></div>"

/////////////////////////////////////////////////////////////////////////

var welc_back = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_29_welcome_back.m4a',
  prompt:welcome_back,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "welc_back";
    data.subject_id = subject_id;
    data.participant_id = participant_id;
    data.age = age;
    data.gender = gender;
    data.prompt = welcome_back;
  }
}

var move_forward = {
  type: 'html-keyboard-response',
  stimulus:welc_back,
  choices: ['space'],
  on_start: function(move_forward) {
    var last_trial = jsPsych.data.get().last(1).values()[0];
    move_forward.stimulus = last_trial.prompt.slice(0,-6).concat(space_bar);
  },
}



var day_1_summary_goal = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_30_goal_of_game.m4a',
  prompt:goal_summary,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "day_1_summary_goal";
    data.prompt = goal_summary;
  }
}


var day_1_summary_pick = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_31_how_to_pick_a_pirate.m4a',
  prompt:how_to_pick_summary,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "day_1_summary_pick";
    data.prompt = how_to_pick_summary;
  }
}


var test_phase_begin = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_32_final_place.m4a',
  prompt:final_place,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "test_phase_begin";
    data.prompt = final_place;
  }
}

var explain_probe_1 = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_33_recognition_1.m4a',
  prompt:recognition_1,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "explain_probe_1";
    data.prompt = recognition_1;
  }
}

var explain_probe_2 = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_34_recognition_2.m4a',
  prompt:recognition_2,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "explain_probe_2";
    data.prompt = recognition_2;
  }}

var explain_outcome = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_35_recognition_3.m4a',
  prompt:recognition_3,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "explain_outcome";
    data.prompt = recognition_3;
  }
}

var begin_test_phase = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_36_begin_final.m4a',
  prompt:begin_final,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "begin_test_phase";
    data.prompt = begin_final;
    make_test_phase()
  }
}

var begin_source_mem = {
  type: 'audio-keyboard-response',
  stimulus:'run_exp/static/audio/clip_37_source_memory.m4a',
  prompt:source_memory,
  choices: jsPsych.NO_KEYS,
  trial_ends_after_audio: true,
  on_finish: function(data) {
    data.trial_type = "begin_source_mem";
    data.prompt = source_memory;
    make_source_mem()
  }
}

// var begin_pick_best = {
//   type: 'audio-keyboard-response',
//   stimulus:'run_exp/static/audio/clip_38_pick_best_pirate.m4a',
//   prompt:pick_best_pirate,
//   choices: jsPsych.NO_KEYS,
//   trial_ends_after_audio: true,
//   on_finish: function(data) {
//       data.trial_type = "begin_pick_best";
//       data.prompt = pick_best_pirate;
//       make_rank_pirate()
//       make_end()
//   }
// }

var all_done = {
  type: 'audio-keyboard-response',
  stimulus: 'run_exp/static/audio/clip_39_all_done.m4a',
  prompt:"<div class='center'><p>You are all done with the first two parts of the experiment! You’ll be invited to participate in part 3 in a week.<p><b>[Press the space bar to exit]</b></p></div>",
  choices: ['space'],
//   on_start: function(all_done) {
//   var bonus_pay = 0.05*corr_old_new_judge;
//   var bonus = Math.round(bonus_pay);
//   var begin_para = "<p>You are all done with the first two parts of the experiment! /p>";
//   var begin_bonus = "<p> You made $16 plus $";
//   var bonus_str = bonus.toString();
//   var end_bonus = " in bonus payment!</p>";
//   var end_para = "<p>Thank you for participating. You will recieve your payment within 3 days.<p><b>[Press the space bar to end the experiment]</b></p></div>"
//   all_done.stimulus = begin_para.concat(begin_bonus,bonus_str,end_bonus,end_para)
// },
}
