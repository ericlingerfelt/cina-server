<?php
function auth_validate($dbname)
{
	include_once('admin_functions.php');
	include_once('information.php');
	if(array_key_exists('PHP_AUTH_PW',$_SERVER) &&
	array_key_exists('PHP_AUTH_USER',$_SERVER)) {
		$my_dbinfo =& get_db_info($dbname);
		if(! $my_dbinfo) {
			$err="ERROR: Could not get database information for";
			$err.= $dbname;
			$err.="\n";
			error_log($err);
			return false;
		}


		$mylink=mysql_connect('localhost:3306',$my_dbinfo ->username ,
		$my_dbinfo->password);
		if(! $mylink) {
			$err="ERROR: COULD NOT CONNECT to MySQL server as ";
			$err.= $my_dbinfo->username . "\n";
			error_log($err);
			return false;
		}
		$rtn=mysql_query("USE $my_dbinfo->dbname");
		if(! $rtn) {
			$err= "ERROR: USE " . $my_dbinfo->dbname ;
			$err.=" query caused: MYSQL ERROR: " . mysql_error() ;
			$err.= "\n";
			error_log($err);
			mysql_close($mylink);
			return false;
		}
		// please note that the permissions is a "set".  This is like an
		// enum except that it can contain multiple values and the 'names'
		// correspond to bit patterns in this fashion:
		// 'cina' = 00001 =1
		// 'cinad' = 00010 = 2
		// 'bbn' = 00100 = 4
		// 'bbndev' = 01000 = 8
		//  'admin' = 10000 = 16
		//  BUT the names are not the things. MySQL is a little strange in that you
		//  can use the numeric value OR the string name but you must use them in
		//  the correct context. Using the &amp; operator is a numeric
		//  operation and so "permissions &amp; 'admin'" results in a warning
		//  because the string is converted to a numeric value, which is out of
		//  range of the set, and a warning is issued.  However "permissions &amp;
		//  16 works just fine.  Note that binary "10000" is 2<sup>4</sup> which is
		//  16.  You can see that I used "permissions like '%admin%' which is MySQL
		//  wildcard and sort of regular expression matching.
		$query_str="SELECT username FROM accounts WHERE ";
		$query_str.="password=sha1('" . $_SERVER['PHP_AUTH_PW'] . "')";
		//$query_str.=" AND permissions like '%admin%'";
		$query_str.=" AND permission & 16";
		$rtn=mysql_query($query_str);
		if(!$rtn) {
			$err= "ERROR: $query_str\n" ;
			$err.="caused: MYSQL ERROR: " . mysql_error() ;
			$err.= "\n";
			error_log($err);
			mysql_close($mylink);
			return false;
		}
		if(mysql_num_rows($rtn) == 0) {
			mysql_close($mylink);
			error_log("NO MATCH: $query_str\n");
			return false;
		}
		$row=mysql_fetch_row($rtn);
		if($row[0] == $_SERVER['PHP_AUTH_USER'] ) {
			mysql_close($mylink);
			return true;
		}
	}
	else {
		/*
		 $buf="";
		foreach($_SERVER as $key => $value) {
		$buf.="$key => $value<br>";
		}
		error_log("INVALID REQUEST: page=site_admin_reg.php\n$buf\n");
		*/
		return false;
	}
}

/****************************************************************************/

if(($rtn=auth_validate("cina")) !== true )
{
	// not going to print the errors unless testing
	// as long as the php is correct all the other errors will
	// be hidden
	header('WWW-Authenticate: Basic realm="CINA_ADMIN"');
	echo "<html><head><title>Not Authorized</title></head><body>";
	echo "ERROR: 401 Authorization required<br><hr>";
	echo "</body></html>";
	exit(1);
}



?>

<!-- DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
     "http://www.w3.org/TR/html4/loose.dtd" -->

<html>
<head>
<title>CINA/BBN Registration</title>
<link rel="stylesheet" type="text/css" href="site_admin.css">
<META http-equiv="Content-Script-Type" content="text/javascript">
<script src="site_admin.js" type="text/javascript">
    </script>
</head>





<?php
  if( $_SERVER['REQUEST_METHOD'] != "POST") 
    echo '<body onload="closeme()"></body></html>';
  else echo '<body onresize="do_img_resize()" onload="do_img_resize()">'; 
?>
  <table id="toptable" border="1" align="center">
  <tr> 
    <td><img id="i1" src="left.png" > </td>
    <td><img id="i2" src="stars.png" > </td>
    <td><img id="i3" src="right.png" > </td>
  </tr>
  </table>
  <H2 >CINA/BBN Site Administration: User Registration</H2>
  <hr><hr>
    <?php
      if(array_key_exists('ADMIN_ACTION',$_POST)) {
	switch($_POST['ADMIN_ACTION']) {
	  case 'DO_EMAIL':
	    break;
	  case 'REGISTER':
	    echo 'You have completed the registration, if there were';
	    echo ' no errors you should now have a chance to send ';
	    echo ' the new user email.';
	    echo '<hr><hr>';
	    echo '<H2>Results</H2>';
	    echo '<div class="nav">';
	    echo '<form action="site_admin.php" method="POST">';
	    echo '<input type="submit" value="RETURN">';
	    echo '</form>';
	    break;
	  case 'VALIDATE':
	    echo 'The results of the password validation are below.';
	    echo '<hr><hr>';
	    echo '<H2>Results</H2>';
	    echo '<div class="nav">';
	    echo '<form action="site_admin.php" method="POST">';
	    echo '<input type="submit" value="RETURN">';
	    echo '</form>';
	    break;
	  case 'RESET':
	    echo 'The results of the password reset are below.';
	    echo '<hr><hr>';
	    echo '<H2>Results</H2>';
	    echo '<div class="nav">';
	    echo '<form action="site_admin.php" method="POST">';
	    echo '<input type="submit" value="RETURN">';
	    echo '</form>';
	    break;
	  default:
	  }
      }
      else {

	echo 'This is the page used primarily to register a new user. ';
	echo 'Please fill in';
	echo ' the form below and submit.  If registration is successful,';
	echo ' you can continue and send the new user an email.';
        echo '<hr><hr>';
	echo '<H2>Options</H2>';
	echo '<div class="nav">';
	echo '<form action="site_admin_reg_help.php" method="POST">';
	echo '<input type="submit" value="HELP">';
	echo '</form>';
      }
     ?>
    </div>

<?php
  /// This array stores all the formatting characters for the output
  $text_array =& $test_comp;

  if( array_key_exists('ADMIN_ACTION',$_POST)) {
    include_once('admin_functions.php');
    include_once('information.php');
    include_once('error_handler.php');
    include_once('site_admin_registration_handlers.php');
    /// The information I need to manipulate the databases

    switch($_POST['ADMIN_ACTION']) {
      case 'REGISTER':
	if(array_key_exists("DATABASE",$_POST)) {
	  $my_dbinfo =& get_db_info('cina');
	  if(! $my_dbinfo) {
	    $adminerr="ERROR: Could not get databse information for";
	    $adminerr.= $_POST['DATABASE'];
	    $adminerr.="{$text_array['eol']}";
	    trigger_error($adminerr,E_USER_ERROR);
	    return;
	    }
	  
	  }
	else {
	  $adminerr="ERROR: No DATABASE specified";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	  //echo "<br>Calling function<br>";
	echo '<div class="display">';
	$rtn=handle_registration($my_dbinfo,$text_array);
	if(is_array($rtn)) {
	  echo '<p class="header">';
	  echo "<br>The registration was successful. Use the ";
	  echo "button below to send an email to the new user. The<br>\n";
	  echo " email can be edited in-place if desired.";
	  echo "</p>\n";
	  echo '<form name="emailer" action="site_admin_reg.php" ';
	  echo 'method="POST"> ' . "\n";
	  echo '<table border="0" width="80%" align="center">';
	  echo '<tr><td>';
	  echo '<input type="submit" value="SEND EMAIL">' . "\n";
	  echo '<input type="hidden" name="ADMIN_ACTION" value="DO_EMAIL">' . "\n";
	  echo '<input type="hidden" name="DATABASE" value="';
	  echo "{$rtn['db']}\">\n";
	  echo '<input type="hidden" name="FULLNAME" value="';
	  echo "{$rtn['name']}\">\n";
	  echo '<input type="hidden" name="UNAME" value="';
	  echo "{$rtn['user']}\">\n";
	  echo '<input type="hidden" name="EMAILTO" value="';
	  echo "{$rtn['email']}\">\n";
	  echo '<input type="hidden" name="EMAILFROM" value="';
	  echo "{$rtn['from']}\">\n";
	  //echo 'coordinator@nucastrodata.org">' . "\n";
	  echo '<input type="hidden" name="CC" value="';
	  echo "{$rtn['cc']}\">\n";
	  //echo 'coordinator@nucastrodata.org">' . "\n";
	  echo '<input type="hidden" name="SUBJECT" value="';
	  echo "{$rtn['subject']}\">\n";
	  //echo 'NUCASTRODATA Registration">' . "\n";
	  echo '</td><td>';
	  echo '<textarea  name="BODY" rows="20" cols="80">';
	  echo $rtn['body'];
	  echo '</textarea>';
	  echo '</td></tr>';
	  echo '</form>';
	  }
	elseif(is_bool($rtn)){
	  echo "Sorry, an error occurred.  Please correct and try again.\n";
	  }
	elseif(is_int($rtn)) {
	}
	echo '</div>';
	break;
      case 'VALIDATE':
	if(!array_key_exists("DATABASE",$_POST)) {
	  $adminerr="ERROR: No DATABASE information in input stream";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	$my_dbinfo =& get_db_info($_POST['DATABASE']);
	if(! $my_dbinfo) {
	  $adminerr="ERROR: Could not get database information for {$ar[0]}";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	
	echo '<div class="display">';
	validate_password($my_dbinfo,$text_array);
	echo '</div>';
	break;
      case 'DO_EMAIL':
	/*
	echo "DOING THE EMAIL<br><hr>";
        foreach($_POST as $key => $value) {
	  echo "$key => $value<br>";
	  }
	echo "<hr>";
	*/
	if(!array_key_exists("DATABASE",$_POST)) {
	  $adminerr="ERROR: No DATABASE information in input stream";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	handle_email();
	// write a 'whole' document because its the correct thing to do
	echo '<div class="nav">' . "\n";
	echo '<form method="POST" action="site_admin.php">' . "\n";
	echo '<input type="submit" value="RETURN"></form>' . "\n";
	echo "</div>\n";
        echo "</body></html>";
	break;
      case 'RESET':
	if(!array_key_exists("DATABASE",$_POST)) {
	  $adminerr="ERROR: No DATABASE information in input stream";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	$my_dbinfo =& get_db_info($_POST['DATABASE']);
	if(! $my_dbinfo) {
	  $adminerr="ERROR: Could not get database information for {$ar[0]}";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	
	echo '<div class="display">';
	handle_pw_reset($my_dbinfo,$text_array);
	echo '</div>';
	break;
      default:
	$adminerr= "ERROR: Unknown function: {$_POST['ADMIN_ACTION']}";
	$adminerr.="{$text_array['eol']}";
	trigger_error($adminerr,E_USER_ERROR);
      }
    return;
    }
  else {
    // output a set of forms for the user to select options from
    echo '<div class="norm">';
    echo '<ul>'. "\n";
    echo '<li><h3>Register New User</h3>'. "\n";
    /*
    * the site registration form.  This allows a choice of database
    * the the user enters all the required info, on submission a javascript
    * will do a quick and dirty validation, bounce the passwords and then 
    * mangle them if they are the same.
    */
    echo '<form name="register" action="site_admin_reg.php" method="POST"';
    echo ' enctype="multipart/form-data" ';
    echo 'onsubmit="return reg_checkscript();">'. "\n";
    echo '<input type="hidden" value="REGISTER" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<input type="hidden" value="" 
    		name="ENCODE">'. "\n";
    echo '<input type="hidden" value="some value" 
    		name="DUMMY">'. "\n";
    echo '<table width="900" border="10">';
    echo '<caption>New User Registration</caption>';
    echo '<tr align="center" valign="top"><td colspan="4">';
    echo '<br>DATABASE to register: ';
    echo '<select  size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '</td>';
    echo "\n";
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank">FULL NAME: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="1" type="text" name="NAME" size="30"';
    echo ' class="req">' . "\n";
    echo '</td>';
    echo '<td rowspan="3" class="blank">ADDRESS:</td>';
    echo '<td rowspan="3" class="blank">';
    echo '<textarea tabindex="8" rows="4" cols="40" name="ADDRESS"';
    echo ' class="req">';
    echo '</textarea></td>'. "\n";
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank">USERNAME: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="2" type="text" name="USERNAME" class="req">'. "\n";
    echo '</td>';
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank">EMAIL: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="3" type="text" size="40" name="EMAIL"';
    echo ' class="req">'. "\n";
    echo '</td>';

    echo '<tr>';
    echo '<td class="blank">INSTITUTION: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="4" type="text" name="INSTITUTION" size="40"';
    echo ' class="req">'. "\n";
    echo '</td>';
    echo '<td rowspan="3" class="blank">RESEARCH: </td>';
    echo '<td rowspan="3" class="blank">';
    echo '<textarea tabindex="9" rows="4" cols="40" name="RESEARCH"';
    echo ' class="noreq">';
    echo '</textarea></td>'. "\n";
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank">COUNTRY: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="5" type="text" name="COUNTRY" class="noreq">'. "\n";
    echo '</td>';

    echo '<tr>';
    echo '<td class="blank">PASSWORD: </td>';
    echo '<td class="blank">';
    echo '<input tabindex="6" type="password" name="PW1" class="req">'. "\n";
    echo '</td>';
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank">PASSWORD (again): </td>';
    echo '<td class="blank">';
    echo '<input tabindex="7" type="password" name="PW2"  class="req">'. "\n";
    echo '</td>';
    echo '<td rowspan="2" class="blank">INFORMATION: </td>';
    echo '<td rowspan="2" class="blank">';
    echo '<textarea tabindex="10" rows="2" cols="40" name="INFORMATION"';
    echo ' class="noreq">';
    echo '</textarea></td>'. "\n";
    echo '</tr>';

    echo '<tr>';
    echo '<td class="blank" colspan="2">';
    echo 'Use this link to choose a lower-case UCAMS password:';
    echo '<a href="https://ucams.ornl.gov/cgi-bin/cgiwrap/ucams';
    echo '/genpw/ucamgen.cgi?page=INTRO" target="_blank">UCAMS</a>';
    echo '</td>';
    echo '</tr>';
    echo '<tr>';
    echo '<td class="blank" colspan="2" >';
    echo " Passwords must be at least 8 characters long.";
    echo '</td>';
    echo '<td rowspan="2" class="blank">HEAR of SUITE: </td>';
    echo '<td rowspan="2" class="blank">';
    echo '<textarea tabindex="11" rows="2" cols="40" name="HEAR"';
    echo ' class="noreq">';
    echo '</textarea></td>'. "\n";
    echo '</tr>';


    echo '<tr>';
    echo '<td class="blank" align="center" >';
    echo '<input tabindex="12" type="submit" value="Submit Registration">';
    echo '</td><td align="center" class="blank">';
    echo '<input  type="reset" >'. "\n";
    echo '</td>';
    echo '</tr>';


    echo '</table>';
    echo '</form>'. "\n";
    echo '<li><h3>Validate Password</h3>'. "\n";
    /*
    * on submission a javascript does 
    * a quick and dirty validation and mangles the password for sending.
    */
    echo '<form name="validate" action="site_admin_reg.php" method="POST"';
    echo ' enctype="multipart/form-data" ';
    echo 'onsubmit="return val_checkscript();">'. "\n";
    echo '<input type="hidden" value="VALIDATE" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<input type="hidden" value="" 
    		name="ENCODE">'. "\n";
    echo '<input type="hidden" value="some value" 
    		name="DUMMY">'. "\n";
    echo '<table width="700" border="10">';
    echo '<caption>Password Validation </caption>';
    echo '<tr align="center" valign="top"><td colspan="2">';
    echo '<br>DATABASE to check: ';
    echo '<select  size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '</td></tr>';

    echo '<tr><td>';
    echo 'PASSWORD:';
    echo '</td><td>';
    echo '<input  type="password" name="PW1">'. "\n";
    echo '</td></tr>';

    echo '<tr><td align="right">';
    echo '<input  type="submit" value="Check">'. "\n";
    echo '</td><td align="left">';
    echo '<input  type="reset" >'. "\n";
    echo '</td></tr>';


    echo '</table>';
    echo '</form>'. "\n";
    echo '<li><h3>Reset A Password</h3>'. "\n";
    /*
    * on submission a javascript does 
    * a quick and dirty validation and mangles the password for sending.
    */
    echo '<form name="reset" action="site_admin_reg.php" method="POST"';
    echo ' enctype="multipart/form-data" ';
    echo 'onsubmit="return val_resetscript();">'. "\n";
    echo '<input type="hidden" value="RESET" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<input type="hidden" value="" 
    		name="ENCODE">'. "\n";
    echo '<input type="hidden" value="some value" 
    		name="DUMMY">'. "\n";
    echo '<table width="700" border="10">';
    echo '<caption>Password Reset </caption>';
    echo '<tr align="center" valign="top"><td colspan="2">';
    echo '<br>DATABASE to use: ';
    echo '<select  size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '</td></tr>';

    echo '<tr><td>';
    echo 'USERNAME: ';
    echo '<input  type="text" name="USER">'. "\n";
    echo '</td><td>';
    echo 'NEW PASSWORD: ';
    echo '<input  type="password" name="PW1">'. "\n";
    echo '</td></tr>';

    echo '<tr><td align="right">';
    echo '<input  type="submit" value="Reset Password">'. "\n";
    echo '</td><td align="left">';
    echo '<input  type="reset" value="Clear Form" >'. "\n";
    echo '</td></tr>';


    echo '</table>';
    echo '</form>'. "\n";
    echo '</ul>'. "\n";
    echo '</div>';
    echo "<hr><hr><hr>\n";
    }


?>
  </body>
</html>

