<?php

function handle_get_account_data(& $database_info, $t_array) {
    
    $mylink=mysql_connect('localhost:3306',$database_info ->username ,
	  $database_info->password);
    if(! $mylink) {
      $err="ERROR: COULD NOT CONNECT to MySQL server as ";
      $err.= $database_info->username . "<br>\n";
      trigger_error($err,E_USER_ERROR);
      return;
      }
    $rtn=mysql_query("USE $database_info->dbname");
    if(! $rtn) {
      $err= "ERROR: USE " . $database_info->dbname ;
      $err.=" query caused: MYSQL ERROR: " . mysql_error() ;
      $err.= "<br>\n";
      trigger_error($err,E_USER_ERROR);
      mysql_close($mylink);
      return;
      }
    $query_str="SELECT username,alias,email,institution,country ";
    $query_str.="FROM accounts ORDER BY country,institution";
    $rtn=mysql_query($query_str,$mylink);	
    if($rtn) {
      if(mysql_num_rows($rtn) > 0 ) {
	echo "<h2>Current Accounts for ";
	echo strtoupper($database_info->dbname) . "</h2>\n"; 
	echo '<table width="99%" border="1">' ."\n";
	echo "<tr>\n";
	echo "<th>Username</th><th>Full Name</th>\n";
	echo "<th>Email</th><th>Institution</th>\n";
	echo "<th>Country</th></tr>\n";
	$row=mysql_fetch_row($rtn);
	while($row) {
	  echo "<tr><td>{$row[0]}</td><td>{$row[1]}</td>\n";
	  echo "<td>{$row[2]}</td><td>{$row[3]}</td>\n";
	  echo "<td>{$row[4]}</td></tr>\n";
	  $row=mysql_fetch_row($rtn);
	  }
	echo "</table>\n";
	}
      else {
	// assume that there is really no error
	echo "<h3><i>There are no accounts for</i> - ";
	echo $database_info->dbname . "</h3>\n";
	  }
      }
    else {
      $err="Could not retrieve account info from database: ";
      $err.="MYSQL ERROR: " . mysql_error();
      trigger_error($err,E_USER_ERROR);
      }
    mysql_close($mylink);
    return;
  }

$map_array = array(
    "ERRORENTRY"     => "tr",
    "DATETIME" => "td",
    "ERRORNUM"  => "td",
    "ERRORTYPE"  => "td",
    "ERRORMSG"  => "td",
    "SCRIPTNAME"  => "td",
    "SCRIPTLINENUM"  => "td"
);

$dump=0;

function start_element($parser, $name, $attrs)
{
    global $map_array;
    global $dump;
    if (array_key_exists(trim($name),$map_array)) {
	echo "<{$map_array[trim($name)]}>\n";
	if($name != 'ERRORENTRY') {
	  $dump++ ;
	  }
	elseif( $dump==0) $dump++;
    }
}

/*!
* \deprecated Do not believe we will continue with XML in the error logs
* so this function will soon be removed.
*
* Function called when xml parser encounters an end element.
* Parameters are specified in the xml library.
* \param[in] $parser A reference to the parser object
* \param[in] $name The string representing the element
*
* \input the $map_array and the $dump variable
* \output depending on the $name, the base element just sets the depth, the
* others will get printed
*
* \sideeffect May decrement $dump
*/
function end_element($parser, $name)
{
    global $map_array,$dump;
    if (array_key_exists(trim($name),$map_array)) {
	if($name='ERRORENTRY') $dump--;
	else echo "</{$map_array[trim($name)]}>\n";
    }
}

/*!
* \deprecated Do not believe we will continue with XML in the error logs
* so this function will soon be removed.
*
* Function called when xml parser encounters the data surrounded by
* start and end elements.
*
* Parameters are specified in the xml library.
* \param[in] $parser A reference to the parser object
* \param[in] $data The string representing the data encountered
*
* \output That data
*
* \sideeffect None
*/
function character_data($parser, $data)
{
    global $dump;
    if( $dump > 1) {
      echo $data;
      }
}

/*!
* \deprecated Do not believe we will continue with XML in the error logs
* so this function will soon be removed.
*
* Function called when xml parser encounters other components. I am unsure
* exactly what causes this to be called but it seems to be anything other than
* start,end or data.  Like the DTD identifier, and comments.
*
* Parameters are specified in the xml library.
* \param[in] $parser A reference to the parser object
* \param[in] $data The string representing the whatever was matched that was
* not an error.
*
* \output None at this time
* \sideeffect None
*/
function mydefault($parser, $data)
{
    //do nothing
    //echo $data;
}


/*****************************************************************************/
/*!  This function prints out the CINA error log.  This log is in the 
* <b>cina</b> database.  All validation was done before this and
*
* \param[in] $my_dbinfo  A DB_info object passed as a reference
* \param[in] $text_array An array of text chosen at the top level for
* formatting the form
* \output An html document that has the CINA error log data in tablular form
* \return None
* \sideeffect Outputs a table containing the contents of the CINA error log
*****************************************************************************/

function handle_get_cina_errorlog($my_dbinfo,$text_array)
  {

    $mylink=mysql_connect('localhost:3306',$my_dbinfo ->username ,
	  $my_dbinfo->password);
    if(! $mylink) {
      $err="ERROR: COULD NOT CONNECT to MySQL server as ";
      $err.= $my_dbinfo->username . "<br>\n";
      trigger_error($err,E_USER_ERROR);
      return;
      }
    $rtn=mysql_query("USE $my_dbinfo->dbname");
    if(! $rtn) {
      $err= "ERROR: USE " . $my_dbinfo->dbname ;
      $err.=" query caused: MYSQL ERROR: " . mysql_error() ;
      $err.= "<br>\n";
      trigger_error($err,E_USER_ERROR);
      mysql_close($mylink);
      return;
      }
    $query_str="SELECT time,file,line,msg,dmsg,str ";
    $query_str.="FROM logs_error_full";
    if(array_key_exists('RANGE',$_POST)) {
      switch($_POST['RANGE']) {
        case '2':
	  $dstr=date("Y-m-d",strtotime('-2 days'));
	  $query_str.=" WHERE time > '$dstr 00:00:01'";
	  break;
        case '30':
	  $dstr=date("Y-m-d",strtotime('-30 days'));
	  $query_str.=" WHERE time > '$dstr 00:00:01'";
	  break;
        case '60':
	  $dstr=date("Y-m-d",strtotime('-60 days'));
	  $query_str.=" WHERE time > '$dstr 00:00:01'";
	  break;
        case '90':
	  $dstr=date("Y-m-d",strtotime('-90 days'));
	  $query_str.=" WHERE time > '$dstr 00:00:01'";
	  break;
        case '120':
	  $dstr=date("Y-m-d",strtotime('-120 days'));
	  $query_str.=" WHERE time > '$dstr 00:00:01'";
	  break;
        case 'all':
	  break;
	}
      }
    $rtn=mysql_query($query_str,$mylink);	
    if($rtn) {
      if(mysql_num_rows($rtn)>0) {
	echo '<h2>CINA errors</h2>'. "\n";
	echo '<table border="1" width="99%">'. "\n";
	echo '<tr><th>Date</th><th>Filename</th><th>Line #</th>'. "\n";
	echo '<th>Msg</th><th>DMsg</th><th>Error String</th></tr>'."\n";
	$row=mysql_fetch_row($rtn);
	while($row) {
	  if(array_key_exists(0,$row))
	    echo "<tr><td>" . trim($row[0]) . "</td>\n";
	  else
	    echo "<tr><td>&nbsp;</td>\n";
	  if(array_key_exists(1,$row))
	    echo "<td>" . trim($row[1]) . "</td>\n";
	  else
	    echo "<tr><td>&nbsp;</td>\n";
	  if(array_key_exists(2,$row))
	    echo "<td>" . trim($row[2]) . "</td>\n";
	  else
	    echo "<td>&nbsp;</td>\n";
	  if(array_key_exists(3,$row))
	    echo "<td>" . trim($row[3]) . "</td>\n";
	  else
	    echo "<td>&nbsp;</td>\n";
	  if(array_key_exists(4,$row))
	    echo "<td>" . trim($row[4]) . "</td>\n";
	  else
	    echo "<td>&nbsp;</td>\n";
	  if(array_key_exists(5,$row))
	    echo "<td>" . trim($row[4]) . "</td></tr>\n";
	  else
	    echo "<td>&nbsp;</td></tr>\n";
	  $row=mysql_fetch_row($rtn);
	  }
	echo "</table>";
	mysql_close($mylink);
	}
      else {
	echo 'NO Errors found in log<br>'."\n";
	}
      }
    else {
      $err= "ERROR: could not retrieve log data from " ;
      $err.= $my_dbinfo->dbname ;
      $err.=" query caused: MYSQL ERROR: " . mysql_error() ;
      $err.= "<br>\n";
      trigger_error($err,E_USER_ERROR);
      mysql_close($mylink);
      }
    return;
    }


/*****************************************************************************/
/*!  This function prints out the PHP error log.  This log is the file
* <tt>/var/log/php_error.log</tt>.  All validation was done before this and
* we only have to return the data that we have recovered.  \deprecated In
* this case, part of the information in the php error logs is XML encoded.
* Just because it is.  If you do not understand XML just go to www.w3c.org
* and use one of the very basic XML tutorials. Nothing earthshaking here
* except that our parser requires a VALID document. This will be removed
* soon because I don't believe that we will continue with XML in the error
* logs, takes up too much space and does not really add anything. 
* 
* There is one major difference between this and the rest of the functions
* we have been looking at, php errors are in a file so we won't use mysql
* functions.
*
* The format of the error_log entries is always the date/time of the entry
* enclosed in square brackets followed by ANY other text. 
*
* \param[in] $my_dbinfo  A DB_info object passed as a reference
* \param[in] $text_array An array of text chosen at the top level for
* formatting the form
* \output An html document that has the PHP error log data in tablular form
* \return None
* \sideeffect Outputs a table containing the contents of the PHP error log
*****************************************************************************/

function handle_get_php_errorlog($my_dbinfo,$text_array)
{

  // Again we use the @ to supress an error message
  // opening the command output as a readonly file 
  $fp=popen('tail -50 /var/log/php_error.log','r');
  if(!$fp) {
    $err= "ERROR: Could not open tail -50 /var/log/php_error.log<br>\n";
    trigger_error($err,E_USER_ERROR);
    return;
    }
  echo '<h2>USER errors</h2>'. "\n";
  echo '<table border="1" width="99%">'. "\n";
  echo '<tr><th >Date</th><th>Type</th>'. "\n";
  echo '<th width="40%">Msg</th><th width="20%">Filename</th>';
  echo '<th width="5%">Lineno</th></tr>'."\n";

  while($input=fgets($fp)) {
    if(preg_match('/\[.*\]/',$input)) {
      $fields=preg_split("/:/",$input,4);
      //$mydate=preg_replace("/\].*/","",$fields[0]);
      $mydate=preg_replace("/\[/","",$fields[0]);
      $mydate.= ":" . $fields[1] ;
      $mydate.= ":" . preg_replace("/\].*/","",$fields[2]);
      // the date
      echo "<tr><td>$mydate</td>\n";
      //echo "1. " . $fields[1] . "<br>";
      //echo "2. " . $fields[2] . "<hr>";
      $buf=preg_replace("/.*\]/","",$fields[2]);
      // the error type
      echo "<td>$buf </td>\n";
      if(array_key_exists(3,$fields)) {
	$fields=preg_split("/ in /",$fields[3],2);
	}
      // the message
      echo "<td>" . $fields[0] . "</td>\n";
      if(array_key_exists(1,$fields)) {
	$fields=preg_split("/ on line /",$fields[1],2);
	// the filename
	echo "<td>" . preg_replace("/^.*www/","",$fields[0]) . "</td>\n";
	// the line number
	if(array_key_exists(1,$fields)) {
	  echo "<td>" . trim($fields[1]) . "</td>\n</tr>\n";
	  }
	else {
	  echo "<td>&nbsp;</td></tr>\n";
	  }
	}
      else {
	echo "<td>&nbsp;</td><td>&nbsp;</td></tr>\n";
	}
    }
  }
  fclose($fp);
  // this is the end of the valid document body and we 
  // call the parser with <b><i>true</i></b> to tell it to 
  // clean up and finish.
  echo "</table></div></body></html>";
  return;
  }


/*****************************************************************************/
/*!  This functions purpose is to display information on the current
* sessions.  All validation was done before this and we only have to return
* the data that we have recovered.
*
* \param[in] $database_info  A DB_info object passed as a reference
* \param[in] $t_array An array of text chosen at the top level for
* formatting the form
* \output An html document that has the session information in tablular
* \sideeffect None
*****************************************************************************/
function handle_get_session_data(& $database_info, $t_array) {
    
    $mylink=mysql_connect('localhost:3306',$database_info ->username ,
	  $database_info->password);
    if(! $mylink) {
      $err="ERROR: COULD NOT CONNECT to MySQL server as ";
      $err.= $database_info->username . "<br>\n";
      trigger_error($err,E_USER_ERROR);
      return;
      }
    $rtn=mysql_query("USE $database_info->dbname");
    if(! $rtn) {
      $err= "ERROR: USE " . $database_info->dbname ;
      $err.=" query caused: MYSQL ERROR: " . mysql_error() ;
      $err.= "<br>\n";
      trigger_error($err,E_USER_ERROR);
      mysql_close($mylink);
      return;
      }
    $query_str="SELECT sessions.username,alias,sessions.status,lastrequest ";
    $query_str.="FROM sessions,accounts WHERE ";
    $query_str.="accounts.username=sessions.username ORDER BY lastrequest";
    $rtn=mysql_query($query_str,$mylink);	
    if($rtn) {
      if(mysql_num_rows($rtn) > 0 ) {
	echo "<h2>Current Sessions for";
	echo strtoupper($database_info->dbname) . "</h2>\n"; 
	echo '<table width="99%" border="1">' ."\n";
	echo "<tr>\n";
	echo "<th>User</th><th>Full Name</th>\n";
	echo "<th>Status</th><th>Last Request</th></tr>\n";
	$row=mysql_fetch_row($rtn);
	while($row) {
	  echo "<tr><td>{$row[0]}</td><td>{$row[1]}</td>\n";
	  echo "<td>{$row[2]}</td><td>{$row[3]}</td></tr>\n";
	  $row=mysql_fetch_row($rtn);
	  }
	echo "</table>\n";
	}
      else {
	// assume that there is really no error
	echo "<h3><i>There are no current sessions for</i> - ";
	echo $database_info->dbname . "</h3>\n";
	  }
      }
    else {
      $err="Could not retrieve session info from database: ";
      $err.="MYSQL ERROR: " . mysql_error();
      trigger_error($err,E_USER_ERROR);
      }
    mysql_close($mylink);
    return;
  }


function handle_get_registrations($my_dbinfo,$t_array)
{
  $file=true;
  switch($_POST['DATABASE']) {
    case 'cina':
      $regarray=file("/var/www/cina_files/adm/Registrations");
      break;
    case 'cinad':
      $regarray=file("/var/www/cina_files_dev/adm/Registrations");
    case 'bbn':
    case 'bbndev':
      $file=false;
      $mylink=mysql_connect('localhost:3306',$my_dbinfo ->username ,
	    $my_dbinfo->password);
      if(! $mylink) {
	$err="ERROR: COULD NOT CONNECT to MySQL server as ";
	$err.= $my_dbinfo->username . "<br>\n";
	trigger_error($err,E_USER_ERROR);
	return;
	}
      $rtn=mysql_query("USE $my_dbinfo->dbname");
      if(! $rtn) {
	$err= "ERROR: USE " . $my_dbinfo->dbname ;
	$err.=" query caused: MYSQL ERROR: " . mysql_error() ;
	$err.= "<br>\n";
	trigger_error($err,E_USER_ERROR);
	mysql_close($mylink);
	return;
	}
      $query_str="SELECT lastname,firstname,createdate,email,";
      $query_str.="institution,";
      $query_str.="address,researchdesc,whereheardofsuite,info ";
      $query_str.="FROM users_info";
      $regarray=mysql_query($query_str);
    }
  if(! $regarray) {
    echo "Error getting Registration information<br>\n";
    if(mysql_errno() > 0) {
      echo mysql_error() . "<br>";
      }
    return;
    }
  echo '<table width="99%" border="2"><caption>Raw Registration Data</caption>';
  echo "<tr>\n";
  echo "<th>Last Name</th>\n";
  echo "<th>First Name</th>\n";
  echo "<th>Date</th>\n";
  echo "<th>Time</th>\n";
  echo "<th>IP Address</th>\n";
  echo "<th>Email</th>\n";
  echo "</tr>\n";
  echo "<tr>\n";
  echo '<th colspan="3">Institution</th>' . "\n";
  echo '<th colspan="3">Mailing Address</th>' . "\n";
  echo "</tr>\n";
  echo "<tr>\n";
  echo '<th colspan="2">Research Type</th>' . "\n";
  echo '<th colspan="2">Hear About Us</th>' . "\n";
  echo '<th colspan="2">Additional Info</th>' . "\n";
  echo "</tr>\n";
  if($file) {
    for($i=0;$i<count($regarray);$i++) {
      $pat=array('/\\\\0A/','/\\\\09/');
      $str=preg_replace($pat,' ',trim($regarray[$i]));
      $strarray=preg_split('/\t/',$str);
      //print_r($strarray);
      //echo '<br>';
      echo '<tr >' . "\n";
      echo '<td class="regtl">' . "{$strarray[0]}</td>\n";
      echo '<td class="regtm">' . "{$strarray[1]}</td>\n";
      echo '<td class="regtm">' . "{$strarray[2]}</td>\n";
      echo '<td class="regtm">' . "{$strarray[3]}</td>\n";
      echo '<td class="regtm">' . "{$strarray[4]}</td>\n";
      echo '<td class="regtr">' . "{$strarray[5]}</td>\n";
      echo "</tr>\n";
      echo '<tr >' . "\n";
      echo '<td colspan="3" class="regml">' . $strarray[6] . '</td>' . "\n";
      echo '<td colspan="3" class="regmr">' . $strarray[7] . '</td>' . "\n";
      echo "</tr>\n";
      echo '<tr class="reg">' . "\n";
      echo '<td colspan="2" class="regbl">' . $strarray[8] . '</td>' . "\n";
      if(array_key_exists(9,$strarray)) {
	echo '<td colspan="2" class="regbm">' . $strarray[9] . ' </td>' . "\n";
	} else {
	  echo '<td colspan="2" class="regbm">' . '&nbsp;' . ' </td>' . "\n";
	  }
      if(array_key_exists(10,$strarray)) {
	echo '<td colspan="2" class="regbr">' . $strarray[10] . '</td>' . "\n";
	} else {
	  echo '<td colspan="2" class="regbm">' . '&nbsp;' . ' </td>' . "\n";
	  }
      echo "</tr>\n";
      }
    }
  else {
    $row=mysql_fetch_row($regarray);
    while($row) {
      echo '<tr >' . "\n";
      echo '<td class="regtl">' . "{$row[0]}</td>\n"; // last
      echo '<td class="regtm">' . "{$row[1]}</td>\n"; // first
      $dte=explode(' ',$row[2]);
      echo '<td class="regtm">' . "{$dte[0]}</td>\n"; //date
      echo '<td class="regtm">' . "{$dte[1]}</td>\n"; // time
      echo '<td class="regtm">' . "&nbsp;</td>\n"; //IP (not saved)
      echo '<td class="regtr">' . "{$row[3]}</td>\n"; //email
      echo "</tr>\n";
      echo '<tr >' . "\n";
      //institution
      echo '<td colspan="3" class="regml">' . $row[4] . '</td>' . "\n";
      //address
      echo '<td colspan="3" class="regmr">' . $row[5] . '</td>' . "\n";
      echo "</tr>\n";
      echo '<tr class="reg">' . "\n";
      //research
      echo '<td colspan="2" class="regbl">' . $row[6] . '</td>' . "\n";
      //heardof
      echo '<td colspan="2" class="regbm">' . $row[7] . ' </td>' . "\n";
      //info
      echo '<td colspan="2" class="regbr">' . $row[8] . '</td>' . "\n";
      echo "</tr>\n";
      $row=mysql_fetch_row($regarray);
      }
    }
  echo "</table>\n";
  return;
}

?>
