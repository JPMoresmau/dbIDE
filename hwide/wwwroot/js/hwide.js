var myCodeMirror;
var chg=0;
var doTick=true;
	
function tick(){
	if (doTick){
		console.log(chg);
		$("#changeTick").val(""+chg);
		var e = jQuery.Event("keydown");
		e.which = 13; 
		e.keyCode = 13;
		$("#changeTick").trigger(e);
		chg++;		
	}

}

function initCM(txt){
	myCodeMirror = CodeMirror.fromTextArea($(txt)[0],{lineNumbers: true,mode: 'haskell'});
	myCodeMirror.on("change",function(cm){
		tick();
		});
	// uurrrgghhh magic numbers for padding and such
	myCodeMirror.setSize($( window ).width()-20 ,$( window ).height() - 40);
}

function loadCM(mode,contents){
	doTick=false;
	myCodeMirror.setOption('mode',mode);
	myCodeMirror.getDoc().setValue(contents);
	doTick=true;
}

function getCMContents(){
	return myCodeMirror.getDoc().getValue();
}