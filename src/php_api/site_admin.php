<?php

/*****************************************************************************/
/*! \mainpage notitle 
*
* <img src="../dbinfo/stars.png">
*
* <br> <h1 align="center">CINA/BBN Site Administration Code Documentation</h1>
*
* \section intro Introduction
*
* This is the automatically (using doxygen) generated documentation of the
* PHP code used to do the administration of the CINA (nucastrodata.org) and
* BBN (bigbangonline.org) sites.  The main file is site_admin.php which then
* includes other files or uses them as targets for links.
*
*
* \section probs Potential Problems
*
* Right now this still very <b>beta</b> software. It should only be run from
* on the ORNL campus or from the VPN.  It could be accessed from outside and
* currently does not have any password protection.
*
* \section php PHP
*
* Like the Evaluation component, this is using PHP, MySQL and HTML.  We are
* trying to work toward strict HTML 4.0 compliant web pages but not sure
* that we are there yet.
*
* Please note that we are currently (08/22/2007) using PHP v. 4.3.9 and
* Apache v. 2.0.52 (with mod_ssl) and MySQL v. 5.0.27.  This means that
* there things documented in the PHP manual that ARE NOT avaiable in
* practice simply because the world has moved on this server is locked into
* an configuration.
*
* \section cgi CGI Information
*
* This section of the suite is intended to be accessed by a web browser. We
* are using the POST method to transmit data from forms.  In general you
* will that the PHP files, generate a page containing forms which have
* themselves as targets.  This cuts down on the number of files/pages and
* just makes sense to me.  Bu checking the values of the _POST array in the
* php, we can tell what to do with the page, process information or display
* a web page with forms.
*
* All output is via the HTTP connection by simply writing to standard
* output.  No special streams have to be opened.  Theoretically we should
* send back a Content-Type descriptor but at the moment do not. 
*
* \section msq MySQL
*
* The MySQL interface is through PHP compiled-in functions.  There are a
* number of these but primarily we use the <b>mysql_connect()</b>,
* <b>mysql_query()</b>, <b>mysql_close()</b> functions to directly access
* the selected database/tables and then use several functions to check the
* number of rows returned in the query and extract those values.  Error
* handling is done by always checking the function return values and using
* <b>mysql_errno()</b> and <b>mysql_error()</b> to test/recover the errors.
* It is possible to have a 'non-fatal' mysql error if no data is returned
* but the return value is no false. In which case mysql_errno() will return
* a non-zero value and mysql_error() will get the string representation.
*
* \section ge General Error Handling
*
* Error handling in PHP is similar to error handling in high-level languages
* such as C.  Return values and error codes are check.  Error reporting is
* somewhat different.  PHP uses <tt>/etc/php.ini</tt> to determine how to
* report errors, which ones to report and whether or not to log those
* errors. During the development phase I highly recommend that error logging
* in NOT used because a non-fatal parse error (like a mistake in a regular
* expression) can result in literally thousands of lines being written to
* the log file. But once the program reaches the production point, error
* logging should be turned on and error reporting turned off (to keep the
* clients from detecting/exploiting errors in your script).
*
* The PHP manual discusses all the options in the <tt>/etc/php.ini</tt>
* file.  In addition, you can change some of these on a per-script basis
* using the ini_set() function.  I have added an error_handler() function
* that is substituted for the PHP default error handler. This function is
* NOT called for all errors. Parse errors and Core errors occur BEFORE THE
* SCRIPT RUNS. These cannot be 'handled'.  Only the Warning, Notice and User
* errors can be handled.  This function can also either log the error OR
* echo it out. The reason for even using this functions is to have a
* consistent way to display user errors and make error messages easier to
* generate.
* 
* 
******************************************************************************/


/*! \file 
*
* \verbatim
* $Author: bucknerk $
* $Id: site_admin.php,v 1.13 2008/07/14 18:10:11 bucknerk Exp $
* $Date: 2008/07/14 18:10:11 $
*
* $Log: site_admin.php,v $
* Revision 1.13  2008/07/14 18:10:11  bucknerk
* Adding bbn to things to register for
*
* Revision 1.12  2008/07/08 15:54:26  bucknerk
* took out test files listing. files were for Caroline to test rate eval
*
* Revision 1.11  2008/04/08 14:38:22  bucknerk
* Been updating these for multiple typos and adding the notification parts
*
* Revision 1.10  2008/03/26 20:07:53  bucknerk
* I think I have the waiting point finder giving good responses now and
* that it runs correctly with zones like 01, 05 and so on.  Really just took data from the old cinad_main and put it in the cinad_eval then renamed the cinad_eval and moved old cinad_main to miscellaneous.
*
* Revision 1.9  2008/03/05 13:34:30  bucknerk
* minor changes
*
* Revision 1.8  2007/11/09 16:21:22  bucknerk
* added documentation, sql, get_accounts
*
* Revision 1.7  2007/10/15 19:06:56  bucknerk
* cleaned some things and fixed get_registrations and get CINAERROR_LOG
*
* Revision 1.6  2007/09/19 15:52:15  bucknerk
* Moved functions around, they were not actually the proper files. Also
* changed some of the display and added a check of username for uniqueness
* before registering a new user.
*
* Revision 1.5  2007/09/19 14:01:17  bucknerk
* added documentation
*
* Revision 1.4  2007/09/19 12:45:48  bucknerk
* reaction_handlers.php
*
* Revision 1.3  2007/09/07 13:12:18  bucknerk
* site_admin.css
*
* Revision 1.2  2007/09/06 12:02:11  bucknerk
* Changed html header, formalized it.  Changed to switch instead of if-else
* for option type. Changed html form to include several options.
*
* Revision 1.1  2007/08/23 18:08:54  bucknerk
* New files. Idea is to do straight web-based stuff with these, no Java.
*
*
* \endverbatim
* This is the entry point for some administrative operations on the 
* suite/databases
* that is outside of the <i fgcolor="red">nucastrodata.org</i> functionality
* It is meant to be purely web-based with no Java interface.
* \section adminincf Included Files
*
* The following files are always included:\n
* error_handler.php \n
* information.php \n
* admin_functions.php \n
* site_admin_handlers.php \n
*
*****************************************************************************/
?>

<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
     "http://www.w3.org/TR/html4/loose.dtd">

<html>
  <head>
    <title>BBN and NUCASTRODATA Site Administration</title>
    <link rel="stylesheet" type="text/css" href="site_admin.css">
    <META http-equiv="Content-Script-Type" content="text/javascript">
    <script src="site_admin.js" type="text/javascript">
    </script>
  </head>

  <body onresize="do_img_resize()" onload="do_img_resize()"> 
  <table id="toptable" border="1" align="center">
  <tr> 
    <td><img id="i1" src="left.png" > </td>
    <td><img id="i2" src="stars.png" > </td>
    <td><img id="i3" src="right.png" > </td>
  </tr>
  </table>
  <H2 >BBN & NUCASTRODATA Site/Database Administration</H2>
  <hr><hr>
    <?php
      if(array_key_exists('ADMIN_ACTION',$_POST)) {
	echo '<p align="center">(beta) Site Admin</p>';
	echo "<hr><hr>";
	echo "<H2>Results</H2>";
	echo '<div class="nav">';
	echo '<form action="site_admin.php" method="POST">';
	echo '<input type="submit" value="RETURN">';
	echo '</form>';
      }
      else {
	echo '<p align="center">This is a first pass (beta) of ';
	echo "administration options/functions for the";
	echo " site prompted by the incorporation of ";
	echo "the cina-eval portion of the suite.";
	echo "<hr><hr>";
	echo "<H2>Options</H2>";
	echo '<div class="nav">';
	echo '<table border="0" align="center">';
	echo '<tr><td>';
	echo '<form action="documentation.php" method="POST">';
	echo '<input type="submit" value="DOCUMENTATION">';
	echo '</form>';
	echo '</td><td>';
	echo '<form action="cina_eval_sql/sql.php" method="POST">';
	echo '<input type="submit" value="MySQL DB/TABLES">';
	echo '</form>';
	echo '</td><td>';
	echo '<form action="site_admin_help.php" method="POST">';
	echo '<input type="submit" value="HELP">';
	echo '</form>';
	echo '</tr></table>';
      }
     ?>
    </div>

<?php
  /// This array stores all the formatting characters for the output
  $text_array =& $test_comp;

  if( array_key_exists('ADMIN_ACTION',$_POST)) {
    include('admin_functions.php');
    include('information.php');
    include('error_handler.php');
    include('site_admin_handlers.php');
    /// The information I need to manipulate the databases

    switch($_POST['ADMIN_ACTION']) {
      case 'GET_PHPERROR_LOG':
	/*
	* The function called should also get any errors associated with
	* the database.
	*/
	$my_dbinfo =& get_db_info("cinad");
	if(! $my_dbinfo) {
	  $adminerr="ERROR: Could not get databse information for {$ar[0]}";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	
	echo '<div class="display">';
	handle_get_php_errorlog($my_dbinfo,$text_array);
	echo '<div>';
	break;
      case 'GET_CINAERROR_LOG':
	/*
	* The function called should also get any errors associated with
	* the database.
	*/

	$my_dbinfo =& get_db_info($_POST['DB']);
	if(! $my_dbinfo) {
	  $adminerr="ERROR: Could not get databse information for {$ar[0]}";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	
	echo '<div class="display">';
	handle_get_cina_errorlog($my_dbinfo,$text_array);
	echo '<div>';
	break;
      case 'GET_SESSIONS':
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
	handle_get_session_data($my_dbinfo,$text_array);
	echo '<div>';
	break;
      case 'GET_REGISTRATIONS':
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
	handle_get_registrations($my_dbinfo,$text_array);
	echo '<div>';
	break;
      case 'GET_ACCOUNTS':
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
	handle_get_account_data($my_dbinfo,$text_array);
	echo '<div>';
	break;
	/*
      case 'REGISTER':
	if(array_key_exists("DATABASE",$_POST)) {
	  $my_dbinfo =& get_db_info($_POST['DATABASE']);
	  if(! $my_dbinfo) {
	    $adminerr="ERROR: Could not get databse information for".
	    $adminerr.= $_POST['DATABASE'];
	    $adminerr.="{$text_array['eol']}";
	    trigger_error($adminerr,E_USER_ERROR);
	    return;
	    }
	  
	  }
	else { // this can't really happen
	  $adminerr="ERROR: No DATABASE specified";
	  $adminerr.="{$text_array['eol']}";
	  trigger_error($adminerr,E_USER_ERROR);
	  return;
	  }
	echo '<div class="display">';
	handle_registration($my_dbinfo,$text_array);
	echo '<div>';
	break;
	 */
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
	echo '<div>';
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
    echo ' enctype="multipart/form-data">' . "\n";
    echo '<input type="submit" value="Register New User">'. "\n";
    echo '</form>';
    echo '<li><h3>Assign Reaction Evaluation Tasks</h3>'. "\n";
    echo '<form name="eval_assignment" action="site_admin_eval.php" ';
    echo 'method="POST" enctype="multipart/form-data">' . "\n";
    echo '<input type="submit" value="Assign Tasks">'. "\n";
    echo '<br>For Database: <select size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '</select>' . "\n";
    echo '</form>';
    echo '<li><h3>View Current Sessions</h3>'. "\n";
    echo '<form name="sessions" action="site_admin.php" method="POST"
    	enctype="multipart/form-data">'. "\n";
    echo '<input type="hidden" value="GET_SESSIONS" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<br>DATABASE to view: <select size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '<input type="submit" value="Get Sessions">'. "\n";
    echo '</form>'. "\n";
    echo '<li><h3>View Accounts</h3>'. "\n";
    echo '<form name="accounts" action="site_admin.php" method="POST"
    	enctype="multipart/form-data">'. "\n";
    echo '<input type="hidden" value="GET_ACCOUNTS" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<br>DATABASE to view: <select size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '<input type="submit" value="Get Accounts">'. "\n";
    echo '</form>'. "\n";
    echo '<li><h3>View Raw Registration Data</h3>'. "\n";
    echo '<form name="regdata" action="site_admin.php" method="POST"
    	enctype="multipart/form-data">'. "\n";
    echo '<input type="hidden" value="GET_REGISTRATIONS" 
    		name="ADMIN_ACTION">'. "\n";
    echo '<br>DATABASE to view: <select size="1" name="DATABASE">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '<option>bbn</option>' . "\n";
    echo '<option>bbndev</option>' . "\n";
    echo '</select>' . "\n";
    echo '<input type="submit" value="Get Data">'. "\n";
    echo '</form>'. "\n";
    echo '<li><h3>View CINA Error Log</h3>'. "\n";
    echo '<form name="errors" action="site_admin.php" method="POST"
    	enctype="multipart/form-data">'. "\n";
    echo '<input type="hidden" value="GET_CINAERROR_LOG"
    		name="ADMIN_ACTION">'. "\n";
    echo '<br>Select number of days to view: ';
    echo '<select size="1" name="RANGE">' . "\n";
    echo '<option>2</option>' . "\n";
    echo '<option>30</option>' . "\n";
    echo '<option>60</option>' . "\n";
    echo '<option>90</option>' . "\n";
    echo '<option>120</option>' . "\n";
    echo '<option>all</option>' . "\n";
    echo '</select>';
    echo '<select size="1" name="DB">' . "\n";
    echo '<option>cina</option>' . "\n";
    echo '<option>cinad</option>' . "\n";
    echo '</select>';
    echo '<input type="submit" value="Get CINA Error Log">'. "\n";
    echo '</form>'. "\n";
    echo '<li><h3>View PHP Error Log</h3>'. "\n";
    echo '<form name="errors" action="site_admin.php" method="POST"
    	enctype="multipart/form-data">'. "\n";
    echo '<input type="hidden" value="GET_PHPERROR_LOG"
    		name="ADMIN_ACTION">'. "\n";
    echo '<input type="submit" value="Get PHP Error Log">'. "\n";
    echo '</form>'. "\n";
    echo '</ul>'. "\n";
    echo '</div>';
    echo "<hr><hr><hr>\n";
    /*
    echo "FILES FOR TESTING:";
    echo '<table width="100%" border="1">' . "\n";
    echo "<tr><th>Size</th><th>FILENAME</th></tr>";
    $dir=opendir("./testfiles");
    while(($fname=readdir($dir)) !==false) {
      if(preg_match("/^[\.]{1,2}$/",$fname)) {
	continue;
	}
      $sarr=stat("./testfiles/$fname");
      if($sarr['size'] > 1048576) 
	printf("<tr><td align=\"center\">%0.1f mbytes</td><td>",
	  $sarr['size']/1048576);
      else
	printf("<tr><td align=\"center\">%0.1f kbytes</td><td>",
	  $sarr['size']/1024);
      echo '<a href="./testfiles/' . $fname . '">';
      echo "$fname</a></td></tr>\n";
      }
    closedir($dir);
    echo "</table>\n";
     */
    }


?>
  </body>
</html>
