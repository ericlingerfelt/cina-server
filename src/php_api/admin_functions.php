<?php
function validate_session_id($database_info)
  {
    global $text_array;
    if(array_key_exists('ID',$_POST)) {
      $mylink=mysql_connect('localhost:3306',$database_info->username ,
	 $database_info->password);
      if(! $mylink) {
	$err= "ERROR: COULD NOT CONNECT to MySQL server as ";
	$err.= $database_info->username . "{$text_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return false;
	}
	$rtn=mysql_query("USE " . $database_info->dbname);
      if(! $rtn) {
	$err= "USE $database_info->dbname query caused: ";
	$err.= "MYSQL ERROR: " . mysql_error() ;
	$err.="{$text_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	mysql_close($mylink);
	return false;
       }
    $query_str="SELECT sessions.username,alias FROM sessions,accounts ";
    $query_str.="WHERE session = ";
    $query_str.= "'" . trim($_POST['ID']) ."' AND " ;
    $query_str.= "accounts.username = sessions.username";
    $rtn=mysql_query($query_str,$mylink);
    if($rtn) {
      if(mysql_num_rows($rtn) > 0) {
	  $row=mysql_fetch_row($rtn);
	  return array("USERNAME" => $row[0],"ALIAS" => $row[1]);
	  }
      else
      $err="ERROR: No such session id: {$_POST['ID']}: ";
      $err.=$query_str;
      $err.="{$text_array['eol']}";
      trigger_error($err,E_USER_ERROR);
      return false;
      }
    else {
      $err="Could not validate session id: {$_POST['ID']}: ";
      if(mysql_errno()) $err.=mysql_error();
      $err.="{$text_array['eol']}";
      trigger_error($err,E_USER_ERROR);
      return false;
	}
    }
    else {
      $err="ERROR: Missing ID from input string ";
      $err.="{$text_array['eol']}";
      trigger_error($err,E_USER_ERROR);
      return false;
      }
  }
?>