var myCodeMirror;

function initCM(txt){
	myCodeMirror = CodeMirror.fromTextArea($(txt)[0],{lineNumbers: true,mode: 'haskell'});
}

function loadCM(mode,contents){
	myCodeMirror.setOption('mode',mode);
	myCodeMirror.getDoc().setValue(contents);
}

function getCMContents(){
	return myCodeMirror.getDoc().getValue();
}