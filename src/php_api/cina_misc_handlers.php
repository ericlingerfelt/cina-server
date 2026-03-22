<?php
function get_alias($database_info, $username){
	$mylink=mysql_connect('localhost:3306',$database_info ->username ,
	$database_info->password);
	if(! $mylink) {
		$err= "Could not connect to MySQL server as ";
		$err.= $database_info->username . "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		return;
	}
	$rtn=mysql_query("USE $database_info->dbname");
	if(! $rtn) {
		$err= "USE $database_info->dbname query caused: ";
		$err.= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	$query_str = "SELECT alias FROM accounts WHERE username='".$username."'";
	$rtn=mysql_query($query_str);
	if(! $rtn) {
		$err= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	if(mysql_num_rows($rtn)>0){
		$row = mysql_fetch_assoc($rtn);
		return $row["alias"];
	}
	return;
}

function get_email($database_info, $username){
	$mylink=mysql_connect('localhost:3306',$database_info ->username ,
	$database_info->password);
	if(! $mylink) {
		$err= "Could not connect to MySQL server as ";
		$err.= $database_info->username . "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		return;
	}
	$rtn=mysql_query("USE $database_info->dbname");
	if(! $rtn) {
		$err= "USE $database_info->dbname query caused: ";
		$err.= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	$query_str = "SELECT email FROM accounts WHERE username='".$username."'";
	$rtn=mysql_query($query_str);
	if(! $rtn) {
		$err= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	if(mysql_num_rows($rtn)>0){
		$row = mysql_fetch_assoc($rtn);
		return $row["email"];
	}
	return;
}

function get_reaction_string($database_info, $reaction_index){
	$mylink=mysql_connect('localhost:3306',$database_info ->username ,
	$database_info->password);
	if(! $mylink) {
		$err= "Could not connect to MySQL server as ";
		$err.= $database_info->username . "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		return;
	}
	$rtn=mysql_query("USE $database_info->reac_dbname");
	if(! $rtn) {
		$err= "USE $database_info->dbname query caused: ";
		$err.= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	$query_str = "SELECT reac_string FROM reaction WHERE r_index=".$reaction_index;
	$rtn=mysql_query($query_str);
	if(! $rtn) {
		$err= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	if(mysql_num_rows($rtn)==0){
		return "";
	}
	$row = mysql_fetch_assoc($rtn);
	$reaction_string = $row["reac_string"];
	
	$query_str = "SELECT decay FROM rates WHERE react_index=".$reaction_index;
	$rtn=mysql_query($query_str);
	if(! $rtn) {
		$err= "MYSQL ERROR=" . mysql_error() ;
		$err.= "{$t_array['eol']}";
		trigger_error($err,E_USER_ERROR);
		mysql_close($mylink);
		return;
	}
	if(mysql_num_rows($rtn)==0){
		return $reaction_string;
	}
	$row = mysql_fetch_assoc($rtn);
	$decay_string = $row["decay"];
	if(trim($decay_string)!=""){
		return $reaction_string." [".$decay_string."]";
	}
	return $reaction_string;
}

?>
