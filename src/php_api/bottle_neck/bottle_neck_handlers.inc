<?php
/****************************************************************************/
/*!
 * This function is called by the main cgi interface and
 * <ul>
 * <li> calls the function that creates the input file for the fortran
 * <li> causes the fortran code to run and captures the output
 * <li> validates and returns the output.
 * </ul>
 *
 *****************************************************************************/

function handle_get_bottle_necks(& $database_info, $t_array)
{
	if(array_key_exists('ID',$_POST)
	&& array_key_exists('PATH',$_POST)
	&& array_key_exists('ZONE',$_POST)
	&& array_key_exists('RANGE',$_POST)
	&& array_key_exists('TIME_STEP',$_POST)
	&& array_key_exists('LOW_THRESHOLD',$_POST)
	&& array_key_exists('HIGH_THRESHOLD',$_POST)
	&& array_key_exists('SEP_FACTOR',$_POST) )
	{
		$names=validate_session_id($database_info);
		if(! $names) {
			$err= "Could not validate session ID:";
			trigger_error($err,E_USER_ERROR);
			return;
		}
		// then we can start
		$ar=explode('/',$_POST['PATH']);
		$path_to_sim=$database_info->eval_path;
		// NEED TO FIX THIS, SORT OF HACKED UP BECAUSE THE GUI DOES NOT
		// RETURN A TRUE PATH WHEREAS THE TESTER I WROTE DOES.
		if($ar[1] == 'USER') {
			if(array_key_exists('OVERRIDE',$_POST)) {
				if($ar[3] != "em_sims") {
					$path_to_sim.='/USER/' . $ar[2] . '/em_sims/' . $ar[3];
				}
				else {
					$path_to_sim.='/USER/' . $ar[2] . '/em_sims/' . $ar[4];
				}
	  }
	  else {
	  	$path_to_sim.='/USER/' . $names['USERNAME'] . "/em_sims/" . $ar[2];
	  }
		}
		else {
			$path_to_sim.='/' . $ar[1] . "/em_sims/" . $ar[2];
		}
		if(! is_readable($path_to_sim)) {
			$err="Cannot read: $path_to_sim";
			trigger_error($err,E_USER_ERROR);
			return;
		}
		if(! is_readable($path_to_sim . "/data")) {
			$err="Cannot read: $path_to_sim/data";
	  trigger_error($err,E_USER_ERROR);
	  return;
		}
		if(! is_writable("./bottle_neck")) {
			$err="Cannot write to: ./bottle_neck";
	  trigger_error($err,E_USER_ERROR);
	  return;
		}
		$my_zone=find_zone($path_to_sim,$_POST['ZONE']);
		// want to change this somehow so that it is piped directly to the program
		// without having to create a file.
		$fp=fopen('./bottle_neck/parameter.dat','wb');
		if(!$fp) {
			$err="Could not create parameter file";
	  trigger_error($err,E_USER_ERROR);
	  return;
		}


		// for the following, if there is no value from the input we use the
		// default given by Tomomi/Michael.  The oddness is to force the input
		// values into the FORTRAN 13.6E format, with a capital E.  It seems as
		// though FORTRAN does not complain if the exponent is one or two digits,
		// has worked so far with no leading 0

		if(strlen($_POST['SEP_FACTOR']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['SEP_FACTOR']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-04\n");
		}
		if(strlen($_POST['HIGH_THRESHOLD']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['HIGH_THRESHOLD']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-05\n");
		}
		if(strlen($_POST['LOW_THRESHOLD']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['LOW_THRESHOLD']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-12\n");
		}
		if(strlen($_POST['SEP_FACTOR']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['SEP_FACTOR']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-04\n");
		}
		if(strlen($_POST['HIGH_THRESHOLD']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['HIGH_THRESHOLD']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-05\n");
		}
		if(strlen($_POST['LOW_THRESHOLD']) > 0) {
			$tmp=sprintf("%-13.7e\n",0.0+($_POST['LOW_THRESHOLD']));
			fwrite($fp,strtoupper($tmp));
		}
		else {
			fwrite($fp,"1.000000E-12\n");
		}
		fclose($fp);
		/*****************************************************************/
		// now do the same as the next function.
		// these are the isotope range min_Z,min_N,max_Z,max_N and the starting
		// and stopping step numbers and the zone.
		// These are for the 'create_input' code
		if(strlen($_POST['RANGE']) > 0) {
			$range=explode(',',$_POST['RANGE']);
			// $range[0]=explode(",",$range[0]);
			// $range[1]=explode(",",$range[1]);
		}
		else {
			$range[0]=1;
			$range[1]=110;
			// $range[1][0]=55;
			// $range[1][1]=55;
		}
		if(strlen($_POST['TIME_STEP']) > 0) {
			$steps=explode(',',$_POST['TIME_STEP']);
			//print_r($steps);
			//echo " -- " . count($steps) . "<br>";
			if(count($steps) < 2) {
				$steps[1]=0; // will force this to become "max"
			}
			elseif(is_float($steps[0]) || is_double($steps[0]) || $steps[0] < 0 ) {
				// assume that if negative they really mean time.
				// have to figure out the largest step number that is less than
				// or equal to
				// steps[0] and the smallest step number greater than or equal to
				// steps[1]
				$command="./bottle_neck/read_time_data " . $path_to_sim;
				$command.=" " . $my_zone;
				$command.=" " . $steps[0] . " " . $steps[1];
				// the backticks work the same as for shells
				$localsteps=`$command`;
				$temp=explode(",",$localsteps);
				$steps[0]=trim($temp[0]);
				$steps[1]=trim($temp[1]);
	  }
		}
		else {
			// default because the program will set the start at 0 and the end
			// at max
			$steps[0]=0;
			$steps[1]=0;
		}
		/*
		 * Here I will scan the selected directory/simulation and makesure that all
		 * the files I need are there. While I am at it I will see what zones are
		 * available and if the one the user chose is there.
		 */
		$flags_array=array("step" => 0,"flux" => 0,"fcnt" => 0,"time" => 0);
		$dir=opendir($path_to_sim);
		if($dir) {
			while (false !== ($fname=readdir($dir))) {
				if(preg_match("/^\.+$/",$fname)) {
					// skips the . and .. entries
					continue;
				}
				else {
					$files[]=$fname;
				}
			}
			closedir($dir);
			sort($files);
			if( in_array('isomap',$files) &&
	  in_array('reacmap',$files) &&
	  in_array('zonemap',$files) &&
	  in_array('data',$files)) {
	  	// I have the maps and the subdirectory

	  	foreach($files as $key => $value) {
	  		//echo "$value<br>";
	  		if(preg_match('/step_num[0-9]+/',$value)) {
	  			$flags_array['step']=1;
	    }
	    elseif(preg_match('/^flux[0-9]+$/',$value)) $flags_array['flux']=1;
	    elseif(preg_match('/timemap[0-9]+/',$value)) $flags_array['time']=1;
	    elseif(preg_match('/flux[0-9]+_cnt/',$value)) $flags_array['fcnt']=1;
	  	}
	  	$zones=explode(",",file_get_contents($path_to_sim . "/zonemap"));
	  	unset($tmparr);
	  	$tmparr=array_keys($flags_array,0);
	  	if(count($tmparr) > 0) {
	  		$err= "Checking files: " ;
	  		foreach($tmparr as $key => $value) {
	  			$err.= " Missing file $value:";
	    }
	    trigger_error($err,E_USER_ERROR);
	    return false;
	  	}
	  	$dir=opendir($path_to_sim . "/data");
	  	if($dir) {
	  		$netsu=false;
	  		while (false !== ($fname=readdir($dir))) {
	  			if($fname=="netsu") {
	      $netsu=true;
	      break;
	  			}
	    }
	    if(! $netsu) {
	    	$err= "Could not find simulation data/netsu file<br>";
	    	trigger_error($err,E_USER_ERROR);
	    	return false;
	    }
	  	}
	  	else {
	  		$err= " Could not open simulation 'data' directory<br>";
	  		trigger_error($err,E_USER_ERROR);
	  		return false;
	  	}
	  }
	  else {
	  	$err= " Simulation missing one of 'isomap' file";
	  	$err.= ", 'reacmap' file or 'data' directory <br>";
	  	trigger_error($err,E_USER_ERROR);
	  	return false;
	  }
		}
		else {
			$err= "Could not open simulation directory {$t_array['eol']}";
			trigger_error($err,E_USER_ERROR);
			return false;
		}

		$output_file="bn_" . date('YmdHms') . ".data";

		if($database_info->dbname == "cinad") {
			chdir('/var/www/html/cina_dev/bottle_neck');
		}
		else {
			chdir('/var/www/html/cina/bottle_neck');
		}
		$command="./create_input '$path_to_sim'";
		$command.=" " . $range[0] . " " . $range[1];
		//$command.=" " . $range[1][0] . " " . $range[1][1];
		$command.=" " . $steps[0] . " " . $steps[1];
		$command.=" " . $my_zone;
		$command.= " | ./find_bottle_necks";
		$desc[0]=array('pipe','r');
		$desc[1]=array('pipe','w');
		$desc[2]=array('pipe','w');

		$pfp=proc_open($command,$desc,$pipes);
		if(! is_resource($pfp)) {
			$err= "opening :$command";
			$err.="{$t_array['eol']}";
			trigger_error($err,E_USER_ERROR);
			return false;
		}

		$once=true;
		$buf=fgets($pipes[2]);
		if(!feof($pipes[2])) {
			echo "ERRORS=Returned from system:<br>";
			while(!feof($pipes[2])) {
				echo "$buf{$t_array['eol']}";
				$buf=fgets($pipes[2]);
			}
			print($t_array['footer']);
			proc_close($pfp);
			return false;
		}
		else {
			echo "RESULTS=";
			$buf=fgets($pipes[1]);
			while(!feof($pipes[1])) {
				echo $buf;
				$buf=fgets($pipes[1]);
			}
			print($t_array['footer']);
			proc_close($pfp);
		}
	}
	else {
		print_r($_POST);
		$err= "Missing one of ID, PATH, ZONE, RANGE,";
		$err.=" TIME_STEP, SEP_FACTOR, HIGH_THRESHOLD, LOW_THRESHOLD";
		$err.=" from the input stream:";
		trigger_error($err,E_USER_ERROR);
		return false;
	}
}


/****************************************************************************/
/*!
 * This function validates the input from the server and if correct causes the
 * only the create_input code to be run.  The output is redirected into a file
 * instead of another process.
 *
 * \param[in] $database_info, is a db_info object for the current path and
 * database.  This object is slightly modified from that used by the cina_eval
 * code because we don't need quite so much information.
 * \param[in] $t_array, is a text array for formatting the output.  This allows
 * the output format to change for testing (html-style) or production (text).
 * \output Echoes back the zone # and the output file name and asks the user to
 * verify the results.
 * \sideeffect Creates an output file in the \tbl{output_data} subdir that is
 * named the same as the OUTPUT_FILE $_POST variable
 *
 *
 * FUNCTION DESIGNED TO RUN FROM THE BOTTLE_NECK DIRECTORY
 *****************************************************************************/

function handle_create_input_data($t_array)
{
	if( //array_key_exists('ID',$_POST)
	array_key_exists('PATH',$_POST) // to simulation
	&& array_key_exists('ZONE',$_POST)
	&& array_key_exists('RANGE',$_POST) // z,n z,n
	&& array_key_exists('TIME_STEP',$_POST) //start,end integers or floats
	&& array_key_exists('OUTPUT_FILE',$_POST))
	{
		if($_POST['DB']=="CINA") {
			$path_to_sim="/var/www/cina_files/" . $_POST['PATH'];
		}
		else {
			$path_to_sim="/var/www/cina_files_dev/" . $_POST['PATH'];
		}
		if(! is_readable($path_to_sim)) {
			$err="Cannot read: $path_to_sim";
			trigger_error($err,E_USER_ERROR);
			return false;
		}
		if(! is_readable($path_to_sim . "/data")) {
			$err="Cannot read: $path_to_sim/data";
			trigger_error($err,E_USER_ERROR);
			return false;
		}
		$my_zone=find_zone($path_to_sim,$_POST['ZONE']);


		// these are the isotope range min_Z,min_N,max_Z,max_N and the starting
		// and stopping step numbers and the zone.
		// These are for the 'create_input' code
		if(strlen($_POST['RANGE']) > 0) {
			$range=explode(',',$_POST['RANGE']);
			//  $range[0]=explode(",",$range[0]);
			//  $range[1]=explode(",",$range[1]);
		}
		else {
			// write an error messag and return
			$range[0]=1;
			$range[1]=110;
			//  $range[1][0]=55;
			//  $range[1][1]=55;
		}
		if(strlen($_POST['TIME_STEP']) > 0) {
			$steps=explode(',',$_POST['TIME_STEP']);
			//print_r($steps);
			//echo " -- " . count($steps) . "<br>";
			if(count($steps) < 2) {
				$steps[1]=0; // will force this to become "max"
			}
			elseif(is_float($steps[0]) || is_double($steps[0]) || $steps[0] < 0 ) {
				// assume that if negative they really mean time.
				// have to figure out the largest step number that is less than
				// or equal to
				// steps[0] and the smallest step number greater than or equal to
				// steps[1]
				$command="./read_time_data " . $path_to_sim;
				$command.=" " . $my_zone;
				$command.=" " . $steps[0] . " " . $steps[1];
				// the backticks work the same as for shells
				$localsteps=`$command`;
				$temp=explode(",",$localsteps);
				$steps[0]=trim($temp[0]);
				$steps[1]=trim($temp[1]);
			}
		}
		else {
			// default because the program will set the start at 0 and the end
			// at max
			$steps[0]=0;
			$steps[1]=0;
		}
		/*
		 * Here I will scan the selected directory/simulation and makesure that all
		 * the files I need are there. While I am at it I will see what zones are
		 * available and if the one the user chose is there.
		 */
		$flags_array=array("step" => 0,"flux" => 0,"fcnt" => 0,"time" => 0);
		$dir=opendir($path_to_sim);
		if($dir) {
			while (false !== ($fname=readdir($dir))) {
				if(preg_match("/^\.+$/",$fname)) {
					// skips the . and .. entries
					continue;
				}
				else {
					$files[]=$fname;
				}
			}
			closedir($dir);
			sort($files);
			if( in_array('isomap',$files) &&
			in_array('reacmap',$files) &&
			in_array('zonemap',$files) &&
			in_array('data',$files)) {
				// I have the maps and the subdirectory

				foreach($files as $key => $value) {
					//echo "$value<br>";
					if(preg_match('/step_num[0-9]+/',$value)) {
						$flags_array['step']=1;
					}
					elseif(preg_match('/^flux[0-9]+$/',$value)) $flags_array['flux']=1;
					elseif(preg_match('/timemap[0-9]+/',$value)) $flags_array['time']=1;
					elseif(preg_match('/flux[0-9]+_cnt/',$value)) $flags_array['fcnt']=1;
				}
				$zones=explode(",",file_get_contents($path_to_sim . "/zonemap"));
				unset($tmparr);
				$tmparr=array_keys($flags_array,0);
				if(count($tmparr) > 0) {
					$err= "Checking files: " ;
					foreach($tmparr as $key => $value) {
						$err.= " Missing file $value:";
					}
					trigger_error($err,E_USER_ERROR);
					return false;
				}
				$dir=opendir($path_to_sim . "/data");
				if($dir) {
					$netsu=false;
					while (false !== ($fname=readdir($dir))) {
						if($fname=="netsu") {
							$netsu=true;
							break;
						}
					}
					if(! $netsu) {
						$err= "Could not find simulation data/netsu file<br>";
						trigger_error($err,E_USER_ERROR);
						return false;
					}
				}
				else {
					$err= " Could not open simulation 'data' directory<br>";
					trigger_error($err,E_USER_ERROR);
					return false;
				}
			}
			else {
				$err= " Simulation missing one of 'isomap' file";
				$err.= ", 'reacmap' file or 'data' directory <br>";
				trigger_error($err,E_USER_ERROR);
				return false;
			}
		}
		else {
			$err= "Could not open simulation directory";
			trigger_error($err,E_USER_ERROR);
			return false;
		}

		$output_file=$_POST['OUTPUT_FILE'];
		chdir('/home/bucknerk/working/NUCASTRODATA/cina_eval/bottle_neck');
		if(!file_exists("tmpdir")) {
			$rtn=mkdir("tmpdir");
			if(!$rtn) {
				echo "Could not create tmp directory<br>";
				return false;
			}
		}
		$command="./create_input $path_to_sim";
		$command.=" " . $range[0] . " " . $range[1];
		// $command.=" " . $range[1][0] . " " . $range[1][1];
		$command.=" " . $steps[0] . " " . $steps[1];
		$command.=" " . $my_zone;
		$command.= " > ./tmpdir/$output_file";
		//echo "<hr>$command<hr>";
		//return false;
		$desc[0]=array('pipe','r');
		$desc[2]=array('pipe','w');

		$pfp=proc_open($command,$desc,$pipes);
		if(! is_resource($pfp)) {
			$err= "opening :$command";
			trigger_error($err,E_USER_ERROR);
			return false;
		}

		//$buf=fgets($pfp);
		$once=true;
		$buf=fgets($pipes[2]);
		if(!feof($pipes[2])) {
			echo "ERRORS=Returned from system:<br>";
			while(!feof($pipes[2])) {
				echo "$buf{$t_array['eol']}";
				$buf=fgets($pipes[2]);
			}
			print($t_array['footer']);
			proc_close($pfp);
			return false;
		}
		else {
			$return_vals[]=$output_file;
			proc_close($pfp);
			system("chmod 0644 ./tmpdir/{$output_file}");
			return $return_vals;
		}
	}
	else {
		print_r($_POST);
		$err= "Missing one of  PATH, ZONE, RANGE,";
		$err.=" TIME_STEP, OUTPUT_FILE from the input stream:";
		trigger_error($err,E_USER_ERROR);
		return false;
	}
}

function find_zone($dir,$input)
{
	$map=explode(",",trim(file_get_contents("$dir/zonemap")));
	$rval=false;
	foreach($map as $key => $value) {
		if(trim($input) == trim($value)) {
			$rval=$key+1;
			break;
		}
	}
	if($rval ===false) {
		$rval=1;
	}
	if(count($map) > 9) {
		$tmp=sprintf("%02d",$rval);
	}
	else {
		$tmp=sprintf("%d",$rval);
	}
	return $tmp;
}