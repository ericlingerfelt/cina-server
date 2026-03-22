<?php
if(array_key_exists("FILENAME", $_POST)){
	$openname = "/var/www/html/".$_POST["FILENAME"];
	$fp=fopen($openname,'rb');
    if(!$fp) {
      return;
    }
    $rtn=fpassthru($fp);
    if(!$rtn) {
      return;
    }
    fclose($fp);
}else if(array_key_exists("FILEPATH", $_POST)){
	$openname = $_POST["FILEPATH"];
	$fp=fopen($openname,'rb');
    if(!$fp) {
      return;
    }
    $rtn=fpassthru($fp);
    if(!$rtn) {
      return;
    }
    fclose($fp);
}
?>