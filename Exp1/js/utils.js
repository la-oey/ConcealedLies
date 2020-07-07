function fillUrn(whooccl) {
    var perc = 0.25;
    var marblesAcross = 14;
    var marblesDown = 10;
    var totalMarbles = marblesAcross * marblesDown;

    trial.prob.bullshitterRed = sample(expt.allTrialProbs);
    trial.prob.bullshitDetectorRed = sample(expt.allTrialProbs);
    trial.prob.global = (Math.pow(perc, 2))*trial.prob.bullshitDetectorRed + (1 - Math.pow(perc, 2))*trial.prob.bullshitterRed;
    trial.asymm = trial.prob.bullshitterRed != trial.prob.bullshitDetectorRed;
    debugLog("Asymmetrical? " + trial.asymm);
    // if(asymm){
    //     trial.prob.bullshitterRed = sample(expt.allTrialProbs.slice(0,2)); //global base rate is 0.2 or 0.5
    //     if(trial.prob.bullshitterRed == 0.5){
    //         trial.prob.bullshitDetectorRed = 0.8; //local base rate is 0.8, if global is 0.5
    //     } else{
    //         trial.prob.bullshitDetectorRed = sample(expt.allTrialProbs.slice(1,3)); //local base rate is 0.5 or 0.8, if gloabl is 0.2
    //     }
    // } else{
    //     trial.prob.bullshitterRed = sample(expt.allTrialProbs);
    //     trial.prob.bullshitDetectorRed = trial.prob.bullshitterRed;
    // }
    

    // var probability = 0.5
    // var exactRed = Math.round(totalMarbles * probability);
    // var exactBlue = totalMarbles - exactRed;

    var minUrnWidth = .05*$('#urn').width();
    var maxUrnWidth = .95*$('#urn').width();
    var minUrnHeight = .05*$('#urn').height();
    var maxUrnHeight = .95*$('#urn').height();

    var spaceWidth = (maxUrnWidth - minUrnWidth)/marblesAcross;
    var spaceHeight = (maxUrnHeight - minUrnHeight)/marblesDown;

    var occXL = $('#urn').width()*(1-perc)/2;
    var occXR = $('#urn').width()*(1-(1-perc)/2);
    var occYT = $('#urn').height()*(1-perc)/2;
    var occYB = $('#urn').height()*(1-(1-perc)/2);

    debugLog("sender probability: " + trial.prob.bullshitterRed);
    debugLog("detector probability: " + trial.prob.bullshitDetectorRed);
    debugLog("global probability: " + trial.prob.global);

    if(whooccl == "bullshitter"){
        occluder("#urnsvg", $('#urn').width(), $('#urn').height(), occXL, occXR, occYT, occYB, 0.9);
        for(var i=0; i<totalMarbles; i++){
            var gridX = (i % marblesAcross) * spaceWidth + minUrnWidth;
            var gridY = Math.floor(i / marblesAcross) * spaceHeight + minUrnHeight;
            var locX = randomDouble(gridX, gridX + spaceWidth);
            var locY = randomDouble(gridY, gridY + spaceHeight);

            if(locX > (occXL - 10) && locX < (occXR + 10) && 
                locY > (occYT - 10) && locY < (occYB + 10)){
                var color = "blue";
                // balls in urn correspond to exact probability distribution, i.e. no random sampling
                if(Math.random() < trial.prob.bullshitDetectorRed){
                    color = "red"
                    trial.numRed += 1;
                    //exactRed -= 1;
                } else{
                    trial.numBlue += 1;
                    //exactBlue -= 1;
                }

                marble("#urnsvg", color, 17.5, locX, locY);
            } else{
                var color = "blue";
                // balls in urn correspond to exact probability distribution, i.e. no random sampling
                if(Math.random() < trial.prob.bullshitterRed){
                    color = "red"
                    trial.numRed += 1;
                    //exactRed -= 1;
                } else{
                    trial.numBlue += 1;
                    //exactBlue -= 1;
                }

                marble("#urnsvg", color, 17.5, locX, locY);
            }
        }
        occluder("#urnsvg", $('#urn').width(), $('#urn').height(), occXL, occXR, occYT, occYB, 0.4);
    } else {
        for(var i=0; i<totalMarbles; i++){
            var gridX = (i % marblesAcross) * spaceWidth + minUrnWidth;
            var gridY = Math.floor(i / marblesAcross) * spaceHeight + minUrnHeight;
            var locX = randomDouble(gridX, gridX + spaceWidth);
            var locY = randomDouble(gridY, gridY + spaceHeight);

            if(locX > (occXL - 10) && locX < (occXR + 10) && 
                locY > (occYT - 10) && locY < (occYB + 10)){
                var color = "blue";

                // balls in urn correspond to exact probability distribution, i.e. no random sampling
                if(Math.random() < trial.prob.bullshitDetectorRed){
                    color = "red"
                    trial.numRed += 1;
                    //exactRed -= 1;
                } else{
                    trial.numBlue += 1;
                    //exactBlue -= 1;
                }

                marble("#urnsvg", color, 17.5, locX, locY);
            } else{
                marble("#urnsvg", "black", 17.5, locX, locY);
            }
        }
        occluder("#urnsvg", $('#urn').width(), $('#urn').height(), occXL, occXR, occYT, occYB, 0.9);
    }
    

}

function marble(container, color, size, locX, locY){
    d3.select(container).append("circle").attr("cx",locX).attr("cy",locY).attr("r",size).attr("stroke-width",2).attr("stroke","black").style("fill",color);
}

//function occluder(container, xwidth, yheight, percX, percY, opac){
function occluder(container, xwidth, yheight, occLeft, occRight, occTop, occBottom, opac){
    //d3.select(container).append("rect").attr("x",0).attr("y",0).attr("width",xwidth).attr("height",yheight).style("fill","white").attr("stroke-width",yheight*(1-percY)/2).attr("stroke","black").style("opacity",opac);
    //d3.select(container).append("rect").attr("x",xwidth*(1-percX)/2).attr("y",yheight*(1-percY)/2).attr("width",xwidth*percX).attr("height",yheight*percY).style("fill","white").attr("stroke-width",yheight*(1-percY)/2).attr("stroke","black");
    
    d3.select(container).append("rect").attr("x",0).attr("y",0).attr("width",xwidth).attr("height",occTop+0.1).style("fill","black").style("opacity",opac);
    d3.select(container).append("rect").attr("x",0).attr("y",occTop).attr("width",occLeft).attr("height",occBottom-occTop).style("fill","black").style("opacity",opac);
    d3.select(container).append("rect").attr("x",occRight).attr("y",occTop).attr("width",xwidth-occRight).attr("height",occBottom-occTop).style("fill","black").style("opacity",opac);
    d3.select(container).append("rect").attr("x",0).attr("y",occBottom-0.1).attr("width",xwidth).attr("height",yheight-occBottom+0.1).style("fill","black").style("opacity",opac);
}

function draw(){
    //predetermine ordering of marbles drawn
    if(turn.numDrawn == 0){
        if(trial.pseudoRound){
            var tempOrder = [];
            for(var i=0; i<expt.marblesSampled; i++){
                if(i < expt.pseudo[trial.trialNumber]){
                    tempOrder.push("red");
                    trial.drawnRed += 1;
                } else{
                    tempOrder.push("blue");
                    trial.drawnBlue += 1;
                }
            }
            trial.marblesDrawn = shuffle(tempOrder);
        } else{
            for(var i=0; i<expt.marblesSampled; i++){
                var color = "blue";
                if(Math.random() < trial.prob.global){
                    color = "red";
                    trial.drawnRed += 1;
                } else{
                    trial.drawnBlue += 1;
                }
                trial.marblesDrawn.push(color);
            }
        }
    }
    
    marble("#tubesvg", trial.marblesDrawn[turn.numDrawn], 17.5, .5*$('#tube').width(), ($('#tube').height()*.95)-(turn.numDrawn/expt.marblesSampled)*$('#tube').height())

    turn.numDrawn += 1;

    if(turn.numDrawn == expt.marblesSampled){
        $('#draw-button').prop('disabled',true);
        $('#subjResponse').css('opacity','1');
        $('#reportMarbles').prop('disabled',false);
        var marbleInstruct2 = "<p class='instructText'>Type into the textbox a number <b>between 0 and 10</b>. Then, click 'Report!'</p>";
        marbleInstruct2 += "<p class='instructText instructTextSmall'>Here's how points work if your opponent thinks you're telling the truth:</p>";
        marbleInstruct2 += "<p class='instructText instructTextSmall tab'>each <b style='color:red'>red</b> is 1 point for you (marble-drawer); each <b style='color:blue'>blue</b> is 1 point for your opponent (responder).</p>";
        marbleInstruct2 += "<p class='instructText instructTextSmall'>Remember, your opponent can only see the marbles visible through the cut out hole.</p>";
        $('#trialInstruct').html(marbleInstruct2);
        trial.responseStartTime = Date.now();
    } 
}

function report(){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('#report-button').prop('disabled', true);

    function bullshitterWait() {
        flickerWait();
        
        trial.waitTime = 1000 + 3000*exponential(0.75);
        setTimeout(function(){
            clearInterval(trial.timer);
            $('#subjResponse').html("<p><br>Your opponent made a decision. Click 'Next!' to continue.<br><br></p>")
            $('#subjResponse').css('opacity','1');
            $('#next').prop('disabled',false);
        }, trial.waitTime);
    }
    bullshitterWait();
}

function computerDraw(){
    //groundTruth
    for(var i=0; i<expt.marblesSampled; i++){
        if(Math.random() < trial.prob.global){
            trial.drawnRed += 1;
        } else{
            trial.drawnBlue += 1;
        }
    }

    if(trial.pseudoRound){
        trial.reportedDrawn = expt.pseudo[trial.trialNumber];
    } else{
        if(Math.random() < 0.2){
            console.log("random lie!");
            trial.compUnifLie = true;
            trial.reportedDrawn = Math.floor(randomDouble(0,11));
        } else{
            console.log("non-random lie!");
            var rand = Math.random();
            var lie = getK(expt.marblesSampled, trial.prob.bullshitDetectorRed, rand); //detector's belief about the distribution
            trial.compLie = lie;
            trial.compDetect = -1;
            //console.log("CompLie: " + trial.compLie)
            if(lie <= trial.drawnRed){
                trial.reportedDrawn = trial.drawnRed;
            } else{
                trial.reportedDrawn = lie;
            }
        }
    }    
}

function callout(call){
    trial.responseTime = Date.now() - trial.responseStartTime;
    $('.callout-button').prop('disabled', true);
    if(call == 'accept'){
        $('#accept-button').css('opacity','1');
        $('#reject-button').css('opacity','0.5');
        trial.callBS = false;
    } else{
        $('#reject-button').css('opacity','1');
        $('#accept-button').css('opacity','0.5');
        trial.callBS = true;
    }
    $('#next').prop('disabled',false);
}

function computerBSDetector(){
    trial.callBS = false;
    trial.compDetect = cbinom(expt.marblesSampled, trial.prob.bullshitDetectorRed, trial.reportedDrawn) - (cbinom(expt.marblesSampled, trial.prob.bullshitDetectorRed, (expt.marblesSampled*trial.prob.bullshitDetectorRed)) - 0.5) //lowers prob of celling out by centering cbinom at expected mean
    trial.compLie = -1;
    if(Math.random() < trial.compDetect){
        trial.callBS = true;
    }
}

function restartTrial(){
    document.getElementById('trial').style.display = 'block';
    if(trial.roleCurrent == "bullshitter"){
        var roletxt = "marble-drawer"
    } else{
        var roletxt = "responder"
    }
    $('.trialNum').html("Round " + (trial.trialNumber+1) + ": You are the <i>" + roletxt + "</i>");
    $('#urnsvg').empty();
    $('#tubesvg').empty();

    //trial.prob.bullshitterRed = expt.trialProbs; //can set this to a number that changes across trials
    trial.numRed = 0;
    trial.numBlue = 0;
    trial.drawnRed = 0;
    trial.drawnBlue = 0;
    // if(trial.exptPart == "trial"){
    //     trial.asymm = expt.asymmTrials.includes(trial.trialNumber)
    // }
    //trial.asymm = true;
    fillUrn(trial.roleCurrent);
    trial.compUnifLie = false;
    
    $('#subjResponse').css('opacity','0');
    $('.callout-button').css('opacity','0.8');
    $('.callout-button').prop('disabled', false);
    $('#buttonResponse').css('opacity','0');
    turn.numDrawn = 0;
    trial.marblesDrawn = [];
    $('input[type=text]').val("");
    $('#reportMarbles').prop('disabled',true);
    $('#next').prop('disabled',true);

    if(trial.exptPart != 'practice'){
        trial.pseudoRound = trial.trialNumber in expt.pseudo;
    }

    trial.catch.key = -1;
    trial.catch.response = -1;
    trial.catch.responseTime = -1;
    $('#catchQ').hide();

    trial.startTime = Date.now();
}

function flickerWait(){
    var op = 0.1;
    var increment = 0.1;
    $('#subjResponse').html('<p><br>Waiting for your opponent...<br><br></p>');
    $('#subjResponse').css('opacity','0');
    trial.timer = setInterval(go, 50)
    function go(){
        op += increment;
        $('#subjResponse').css('opacity', op);
        if(op >= 1){
            increment = -increment;
        }
        if(op <= 0){
            increment = -increment;
        }
    }
}


function submitCatch(){
    trial.catch.responseTime = Date.now() - trial.catch.responseStartTime;
    $('input[type=text]').prop('disabled',true);
    $('input[type=text]').css('opacity','0.7');
    $('#catch-button').prop('disabled', true);
    var timeoutTime = 0;
    if(trial.catch.key == trial.catch.response){
        $('#catchQ').append('<img src="img/yup.png" height=18 vertical-align="middle" hspace="20">');
    } else{
        $('#catchQ').append('<img src="img/nah.png" height=18 vertical-align="middle" hspace="20">');
        timeoutTime = 3000;
    }
    setTimeout(function(){
        if(trial.exptPart == 'practice' | (trial.trialNumber + 1) % 5 == 0){
            $('.scoreboardDiv').css('opacity','1');
        } 
        $('.scoreReport').css('opacity','1');
        $('#nextScoreboard').css('opacity','1');
    }, timeoutTime);
}

function catchTrial(role, exptPart){
    if(role == 'bullshitter'){
        trial.catch.question = 'How many red marbles did you actually draw?'
        trial.catch.key = trial.drawnRed;
    } else{
        trial.catch.question = 'How many red marbles did your opponent report drawing?'
        trial.catch.key = trial.reportedDrawn;
    }
    $('#catchQ').html('<label>'+trial.catch.question+'</label>');
    $('#catchQ').append('<input type="text" id="reportCatch" value="" size="2" maxlength="2" autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false"/> <button class="active-button" id="catch-button" type="button" onclick="submitCatch();">Submit</button> ');

    $('#catch-button').prop('disabled',true);
    $('input[type=text]').on('input',
        function(){
            trial.catch.response = parseInt($(this).val());
            if(trial.catch.response >= 0 && trial.catch.response <= 10 ){
                $('#catch-button').prop('disabled',false);
            } else{
                $('#catch-button').prop('disabled',true);
            }
    });

    $('.scoreReport').css('opacity','0');
    $('.scoreboardDiv').css('opacity','0');
    $('#nextScoreboard').css('opacity','0');
}



// helper functions
function sample(set) {
    return (set[Math.floor(Math.random() * set.length)]);
}

function sample_without_replacement(sampleSize, sample){
    var urn = [];
    if(Number.isInteger(sample)){
        urn = [...Array(sample).keys()];
    } else {
        urn = sample.slice(0);
    }
    var return_sample = [];
    for(var i=0; i<sampleSize; i++){
        var randomIndex = Math.floor(Math.random()*urn.length);
        return_sample.push(urn.splice(randomIndex, 1)[0]);
    }
    return return_sample;
}

function randomDouble(min, max){
    return Math.random() * (max - min) + min;
}

function shuffle(set){
    var j, x, i;
    for (i = set.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = set[i];
        set[i] = set[j];
        set[j] = x;
    }
    return set;
}

function recordData(){
    trialData.push({
        exptPart: trial.exptPart,
        trialNumber: trial.trialNumber,
        roleCurrent: trial.roleCurrent,
        marblesSampled: expt.marblesSampled,
        asymm: trial.asymm,
        probBullshitterRed: trial.prob.bullshitterRed,
        probBullshitDetectorRed: trial.prob.bullshitDetectorRed,
        probGlobal: trial.prob.global,
        drawnRed: trial.drawnRed,
        reportedDrawn: trial.reportedDrawn,
        compLie: trial.compLie,
        compUnifLie: trial.compUnifLie,
        compDetect: trial.compDetect,
        callBS: trial.callBS,
        playerTrialScore: trial.playerTrialScore,
        oppTrialScore: trial.oppTrialScore,
        playerTotalScore: expt.stat.playerTotalScore,
        oppTotalScore: expt.stat.oppTotalScore,
        waitTime: trial.waitTime,
        responseTime: trial.responseTime,
        catchQuestion: trial.catch.question,
        catchKey: trial.catch.key,
        catchResponse: trial.catch.response,
        catchResponseTime: trial.catch.responseTime,
        pseudoRound: trial.pseudoRound,
        trialTime: trial.trialTime
    })
}

function debugLog(message) {
    if(expt.debug){
        console.log(message);
    }
}

function binom(n, p, k){
    return (factorial(n)/(factorial(k)*factorial(n-k))) * p ** k * (1-p) ** (n-k);
}

function factorial(x){
    if(x == 0){
        return 1;
    } else{
        return x*factorial(x-1);
    }
}

function cbinom(n, p, k){
    if(k == 0){
        return binom(n, p, 0);
    } else{
        return binom(n, p, k) + cbinom(n, p, k-1);
    }
}

function getK(n, p, r){
    var i = 0;
    while(r > cbinom(n, p, i)){
        i += 1;
    }
    return i;
}

function exponential(lambda){
    return lambda * Math.E ** (-lambda*Math.random())
}

function calculateStats(string, numer, denom){
    if(denom == 0){
        $(string).html("N/A");
    } else{
        $(string).html(Math.round(numer * 100 / denom)+"%");
    }
}

function scorePrefix(score){
    if(score <= 0){
        return(score);
    } else{
        return("+" + score);
    }
}

function distributeChecks(totalTrials, freq){
    var round = Math.floor(totalTrials * freq);
    var checkRounds = [];
    for(var i=0; i<totalTrials/round; i++){
        checkRounds.push(round*i + Math.floor(randomDouble(0,round)));
    }
    return(checkRounds);
}

// function distributeAsymm(totalTrials, freq){
//     senderTrials = totalTrials/2;
//     var rounds = Math.floor(senderTrials * freq);
//     var asymmRounds = sample_without_replacement(rounds, senderTrials);
//     if(expt.roleFirst == "bullshitDetector"){
//         var roleVal = 1;
//     } else{
//         var roleVal = 0;
//     }
//     asymmRounds = asymmRounds.map(function(value){
//         return value*2 + roleVal;
//     })
//     return(asymmRounds);
// }

function distributePseudo(totalTrials, minArrPseudo, maxArrPseudo){
    var pseudoDict = {};
    var arrPseudo = [];
    var bucketOdd = [];

    for(var a=minArrPseudo; a <= maxArrPseudo; a++){
        arrPseudo.push(a);
    }
    for(var i=0; i<=totalTrials/2; i++){
        bucketOdd.push(i);
    }
    var bucketEven = bucketOdd.slice(0);

    for(var o=0; o<arrPseudo.length; o++){
        index = Math.floor(randomDouble(0, bucketOdd.length));
        pseudoDict[(2*bucketOdd.splice(index, 1)[0]+1)] = arrPseudo[o];
    }
    for(var e=0; e<arrPseudo.length; e++){
        index = Math.floor(randomDouble(0, bucketEven.length));
        pseudoDict[(2*bucketEven.splice(index, 1)[0])] = arrPseudo[e];
    }
    return(pseudoDict);
}








