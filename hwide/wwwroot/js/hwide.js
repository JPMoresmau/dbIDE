var myCodeMirror;
var chg=0;
var doTick=true;

/**
 * Increment change counter on hidden field and submit it
 **/
function tick(){
	if (doTick){
		$("#changeTick").val(""+chg);
		// simulate enty
		var e = jQuery.Event("keydown");
		e.which = 13; 
		e.keyCode = 13;
		$("#changeTick").trigger(e);
		chg++;		
	}

}

/**
 * init code mirror
 * @param txt the text area id
 */
function initCM(txt){
	myCodeMirror = CodeMirror.fromTextArea($(txt)[0],{lineNumbers: true,mode: 'haskell'});
	// we can't easily capture the code mirror change event in threepenny-gui, so use an intermediate field
	myCodeMirror.on("change",function(cm){
		tick();
		});
	// uurrrgghhh magic numbers for padding and such
	myCodeMirror.setSize($( window ).width()-20 ,$( window ).height() - 40);
}

/**
 * load code
 * @param mode the mode
 * @param contents the contents
 */
function loadCM(mode,contents){
	doTick=false;
	myCodeMirror.setOption('mode',mode);
	myCodeMirror.getDoc().setValue(contents);
	doTick=true;
}

/**
 * get current contents
 * @returns the contents
 */
function getCMContents(){
	return myCodeMirror.getDoc().getValue();
}