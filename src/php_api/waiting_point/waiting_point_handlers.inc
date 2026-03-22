<?php

function handle_get_waiting_points(& $database_info, $t_array)
  {
    if(array_key_exists('ID',$_POST)
      && array_key_exists('PATH',$_POST)
      && array_key_exists('ZONE',$_POST) 
      && array_key_exists('RANGE',$_POST) 
      && array_key_exists('TIME_STEP',$_POST) 
      && array_key_exists('MIN_ABUND',$_POST) 
      && array_key_exists('MIN_EFF_LIFE',$_POST) 
      && array_key_exists('RAT_EFF_LIFE',$_POST) )
    {
      
      $names=validate_session_id($database_info);
      if(! $names) {
	$err= "Could not validate session ID:{$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
	}
      // then we can start
      $ar=explode('/',$_POST['PATH']);
      $path_to_sim=$database_info->eval_path;
      if(($gross_idx=array_search('USER',$ar)) !== false) {
	if(($fine_idx=array_search('em_sims',$ar)) !== false) {
	  $path_to_sim.=$_POST['PATH'];
	  }
	else {
	  $path_to_sim.='/USER/' . $names['USERNAME'] . "/em_sims/" . $ar[2];
	  }
	}
      else {
	$path_to_sim.='/' . $ar[1] . "/em_sims/" . $ar[2];
	}
      if(! is_readable($path_to_sim)) {
	$err="Cannot read: $path_to_sim" . $t_array['eol'];
	trigger_error($err,E_USER_ERROR);
	return;
	}
      if(! is_readable($path_to_sim . "/data")) {
	$err="Cannot read: $path_to_sim/data" ;
	$err.=" - This may be an old simulation without the needed data files";
	$err.=$t_array['eol'];
	trigger_error($err,E_USER_ERROR);
	return;
	}
      if(! is_writable("./waiting_point")) {
	$err="Cannot write to: ./waiting_point" . $t_array['eol'];
	  trigger_error($err,E_USER_ERROR);
	return;
	}
       

      // want to change this somehow so that it is piped directly to the program
      // without having to create a file.
      $fp=fopen('./waiting_point/parameter.dat','wb');
      if(!$fp) {
	$err="Could not create parameter file{$t_array['eol']}";
	  trigger_error($err,E_USER_ERROR);
	return;
	}

      // default values and I don't know what these two represent
      // that being the case why are they here?
      fwrite($fp,"1.000000E-12\n1.000000E-00\n");

      // for the following, if there is no value from the input we use the
      // default given by Tomomi/Michael.  The oddness is to force the input
      // values into the FORTRAN 13.6E format, with a capital E.  It seems as
      // though FORTRAN does not complain if the exponent is one or two digits,
      // has worked so far with no leading 0

      if(strlen($_POST['MIN_EFF_LIFE']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['MIN_EFF_LIFE']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"2.000000E-01\n");
	}
      if(strlen($_POST['MIN_ABUND']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['MIN_ABUND']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"1.000000E-06\n");
	}
      if(strlen($_POST['RAT_EFF_LIFE']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['RAT_EFF_LIFE']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"5.000000E-01\n");
	}
      fclose($fp);
      // these are the isotope range min_Z,min_N,max_Z,max_N and the starting
      // and stopping step numbers and the zone.  
      // These are for the 'create_input' code
      if(strlen($_POST['RANGE']) > 0) {
	$range=explode(' ',$_POST['RANGE']);
	$range[0]=explode(",",$range[0]);
	$range[1]=explode(",",$range[1]);
	}
      else {
	$range[0][0]=1;
	$range[0][1]=0;
	$range[1][0]=55;
	$range[1][1]=55;
	}
      if(strlen($_POST['TIME_STEP']) > 0) {
	$steps=explode(',',$_POST['TIME_STEP']);
	if(count($steps) < 2) {
	  $steps[1]=$steps[0]+500;
	  }
	//print_r($steps);
	}
      else {
	$steps[0]=0;
	$steps[1]=5000;
	}
      /* 
      * Here I will scan the selected directory/simulation and makesure that all
      * the files I need are there. While I am at it I will see what zones are
      * available and if the one the user chose is there.
      */
      $flags_array=array("step" => 0,"flux" => 0,"fcnt" => 0,"time" => 0,
      		"abund" => 0,"acnt" => 0,"therm" => 0);
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
	    in_array('data',$files)) { 
	  // I have the maps and the subdirectory

	  foreach($files as $key => $value) {
	    //echo "$value<br>";
	    if(preg_match('/step_num[0-9]+/',$value)) {
	      $flags_array['step']=1;
	      $tmparr[]=preg_replace("/step_num/","",$value);
	      //print_r($tmparr);
	      // have to unset this or we just keep adding to it
	      unset($tmparr);
	      }
	    elseif(preg_match('/^flux[0-9]+$/',$value)) $flags_array['flux']=1;
	    elseif(preg_match('/^abund[0-9]+$/',$value)) $flags_array['abund']=1;
	    elseif(preg_match('/timemap[0-9]+/',$value)) $flags_array['time']=1;
	    elseif(preg_match('/thermo[0-9]+/',$value)) $flags_array['therm']=1;
	    elseif(preg_match('/flux[0-9]+_cnt/',$value)) $flags_array['fcnt']=1;
	    elseif(preg_match('/abund[0-9]+_cnt/',$value)) $flags_array['acnt']=1;
	    //else echo "COULD NOT MATCH: $value<br>";
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
	    return;
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
	      return;
	      }
	    }
	  else {
	    $err= " Could not open simulation 'data' directory<br>";
	    trigger_error($err,E_USER_ERROR);
	    return;
	    }
	  }
	else {
	  $err= " Simulation missing one of 'isomap' file";
	  $err.= ", 'reacmap' file or 'data' directory <br>";
	  trigger_error($err,E_USER_ERROR);
	  return;
	  }
	}
      else {
	$err= "Could not open simulation directory {$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
	}
      // doing this because array_search is unreliable for this particular
      // operation
      // NOTE: the 'zone' number used for the file suffixes is NOT the 'real'
      // zone but is the position in the list contained in the zonemap file of
      // the actual zone. So for every simulation that only has one zone the
      // suffix is 1 regardless of the actual zone number.
      $lzone=false;
      foreach($zones as $k => $v) {
	if(trim($v) == trim($_POST['ZONE'])) {
	  $lzone=$k;
	  break;
	}
      }
      //echo "<hr>";
      //echo "{$_POST['ZONE']}<br>";
      //print_r($zones);
      //echo "<br>lzone=$lzone<br>";
      //echo "<hr>";
      if($lzone !== false) {
	$zone=$lzone+1;
	//echo "<hr>FOUNDIT<hr>";
	}
      else {
	// just pick the first one
	$zone=1;
	}

      //it IS the zone from POST or it is not and we used the first one
      //either way the user gets a correct answer and there should be no
      //index out-of-bounds error.
      echo "ZONE={$zones[$zone-1]}{$t_array['eol']}";
      if($database_info->dbname =="cinad") {
	chdir('/var/www/html/cina_dev/waiting_point');
      }
      else {
	chdir('/var/www/html/cina/waiting_point');
      }
      //$path_to_sim=preg_replace('/ /','\\ ',$path_to_sim);
      $command="./create_input '$path_to_sim'";
      $command.=" " . $range[0][0] . " " . $range[0][1];
      $command.=" " . $range[1][0] . " " . $range[1][1];
      $command.=" " . $steps[0] . " " . $steps[1];
      $command.=" $zone";
      $command.= " | ./newcode";
      //echo $command . "<br>";
      //return;
      /*
      $flog=fopen("wait.log","a");
      if($flog) {
	fwrite($flog,date("Y/m/d H:m:s"));
        fwrite($flog,$command . "\n");
	fclose($flog);
	}
      */
      $desc[0]=array('pipe','r');
      $desc[1]=array('pipe','w');
      $desc[2]=array('pipe','w');

      $pfp=proc_open($command,$desc,$pipes);
      if(! is_resource($pfp)) {
	$err= "opening :$command";
	$err.="{$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
      }

      //$buf=fgets($pfp);
      $once=true;
      $buf=fgets($pipes[2]);
      if(!feof($pipes[2])) {
	while(!feof($pipes[2])) {
	  echo "$buf\n";
	  $buf=fgets($pipes[2]);
	  }
	print($t_array['footer']);
	proc_close($pfp);
	return;
	}
      else {
	$buf=fgets($pipes[1]);
	printf($t_array['header'],"WAITING_POINTS=");
	while(!feof($pipes[1])) {
	  $ar=preg_replace("/ +/",",",trim($buf));
	  //echo $ar[0] . "," . $ar[1] . " ";
	  echo "$ar ";
	  $buf=fgets($pipes[1]);
	  }
	print($t_array['footer']);
	proc_close($pfp);
	return;
	}
      }
    else {
      $err= "Missing one of ID, USER, PATH, ZONE, RANGE,";
      $err.=" TIME_STEP, MIN_ABUND, MIN_EFF_LIFE, or RAT_EFF_LIFE";
      $err.=" from the input stream:";
      $err.="{$t_array['eol']}";
      trigger_error($err,E_USER_ERROR);
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
*****************************************************************************/

function handle_create_input_data(& $database_info, $t_array)
  {
    if(array_key_exists('ID',$_POST)
      && array_key_exists('PATH',$_POST)
      && array_key_exists('ZONE',$_POST) 
      && array_key_exists('RANGE',$_POST) 
      && array_key_exists('TIME_STEP',$_POST) 
      && array_key_exists('MIN_ABUND',$_POST) 
      && array_key_exists('MIN_EFF_LIFE',$_POST) 
      && array_key_exists('RAT_EFF_LIFE',$_POST)
      && array_key_exists('OUTPUT_FILE',$_POST)
    )
    {
      
      $names=validate_session_id($database_info);
      if(! $names) {
	$err= "Could not validate session ID:{$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
	}
      // then we can start
      $ar=explode('/',$_POST['PATH']);
      $path_to_sim=$database_info->path;
      if($ar[1] == 'USER') {
	// a hack to allow local creation on from any directory
	if(array_key_exists("OVERRIDE",$_POST)) {
	  $path_to_sim.='/USER/' . $ar[2] . '/em_sims/' . $ar[3];
	  }
	else {
	    $path_to_sim.='/USER/' . $names['USERNAME'] . '/em_sims/' . $ar[2];
	  }
	}
      else {
	$path_to_sim.='/' . $ar[1] . '/em_sims/' . $ar[2];
	}
      //$path_to_sim.='/em_sims/' . $ar[2];
      // this does not work, unsure why
      //$path_to_sim=preg_replace('/ /','\\\\ ',$path_to_sim);
      if(! is_readable($path_to_sim)) {
	$err="Cannot read: $path_to_sim" . $t_array['eol'];
	trigger_error($err,E_USER_ERROR);
	return;
	}
      if(! is_readable($path_to_sim . "/data")) {
	$err="Cannot read: $path_to_sim/data" . $t_array['eol'];
	trigger_error($err,E_USER_ERROR);
	return;
	}
      if(! is_writable("./waiting_point")) {
	$err="Cannot write to: ./waiting_point" . $t_array['eol'];
	trigger_error($err,E_USER_ERROR);
	return;
	}
       

      // want to change this somehow so that it is piped directly to the program
      // without having to create a file.
      $fp=fopen('./waiting_point/parameter.dat','wb');
      if(!$fp) {
	$err="Could not create parameter file{$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
	}

      // default values and I don't know what these two represent
      // that being the case why are they here?
      fwrite($fp,"1.000000E-12\n1.000000E-00\n");

      // for the following, if there is no value from the input we use the
      // default given by Tomomi/Michael.  The oddness is to force the input
      // values into the FORTRAN 13.6E format, with a capital E.  It seems as
      // though FORTRAN does not complain if the exponent is one or two digits,
      // has worked so far with no leading 0

      if(strlen($_POST['MIN_EFF_LIFE']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['MIN_EFF_LIFE']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"2.000000E-01\n");
	}
      if(strlen($_POST['MIN_ABUND']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['MIN_ABUND']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"1.000000E-06\n");
	}
      if(strlen($_POST['RAT_EFF_LIFE']) > 0) {
	$tmp=sprintf("%-13.7e\n",0.0+($_POST['RAT_EFF_LIFE']));
	fwrite($fp,strtoupper($tmp));
	}
      else {
	fwrite($fp,"5.000000E-01\n");
	}
      fclose($fp);
      // these are the isotope range min_Z,min_N,max_Z,max_N and the starting
      // and stopping step numbers and the zone.  
      // These are for the 'create_input' code
      if(strlen($_POST['RANGE']) > 0) {
	$range=explode(' ',$_POST['RANGE']);
	$range[0]=explode(",",$range[0]);
	$range[1]=explode(",",$range[1]);
	}
      else {
	$range[0][0]=1;
	$range[0][1]=0;
	$range[1][0]=55;
	$range[1][1]=55;
	}
      if(strlen($_POST['TIME_STEP']) > 0) {
	$steps=explode(',',$_POST['TIME_STEP']);
	//print_r($steps);
	//echo " -- " . count($steps) . "<br>";
	if(count($steps) < 2) {
	  $steps[1]=$steps[0]+500;
	  }
	elseif(is_float($steps[0]) || is_double($steps[0]) || $steps[0] < 0 ) {
	    // assume that if negative they really mean time.
	    // have to figure out the largest step number that is less than or equal to 
	    // steps[0] and the smallest step number greater than or equal to
	    // steps[1]
	    $command="./waiting_point/read_time_data " . $path_to_sim . " " . $_POST['ZONE'];
	    $command.=" " . $steps[0] . " " . $steps[1];
	    // the backticks work the same as for shells
	    $localsteps=`$command`;
	    $temp=explode(",",$localsteps);
	    $steps[0]=trim($temp[0]);
	    $steps[1]=trim($temp[1]);
	    }
	//print_r($steps);
	}
      else {
	// stupid default
	$steps[0]=0;
	$steps[1]=5000;
	}
      /* 
      * Here I will scan the selected directory/simulation and makesure that all
      * the files I need are there. While I am at it I will see what zones are
      * available and if the one the user chose is there.
      */
      $flags_array=array("step" => 0,"flux" => 0,"fcnt" => 0,"time" => 0,
      		"abund" => 0,"acnt" => 0,"therm" => 0);
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
	      $tmparr[]=preg_replace("/step_num/","",$value);
	      //print_r($tmparr);
	      // have to unset this or we just keep adding to it
	      unset($tmparr);
	      }
	    elseif(preg_match('/^flux[0-9]+$/',$value)) $flags_array['flux']=1;
	    elseif(preg_match('/^abund[0-9]+$/',$value)) $flags_array['abund']=1;
	    elseif(preg_match('/timemap[0-9]+/',$value)) $flags_array['time']=1;
	    elseif(preg_match('/thermo[0-9]+/',$value)) $flags_array['therm']=1;
	    elseif(preg_match('/flux[0-9]+_cnt/',$value)) $flags_array['fcnt']=1;
	    elseif(preg_match('/abund[0-9]+_cnt/',$value)) $flags_array['acnt']=1;
	    //else echo "COULD NOT MATCH: $value<br>";
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
	    return;
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
	      return;
	      }
	    }
	  else {
	    $err= " Could not open simulation 'data' directory<br>";
	    trigger_error($err,E_USER_ERROR);
	    return;
	    }
	  }
	else {
	  $err= " Simulation missing one of 'isomap' file";
	  $err.= ", 'reacmap' file or 'data' directory <br>";
	  trigger_error($err,E_USER_ERROR);
	  return;
	  }
	}
      else {
	$err= "Could not open simulation directory {$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
	}
      $lzone=false;
      foreach($zones as $k => $v) {
	if(trim($v) == trim($_POST['ZONE'])) {
	  $lzone=$k;
	  break;
	}
      }
      if($lzone !== false) {
	$zone=$lzone+1;
	//echo "<hr>FOUNDIT<hr>";
	}
      else {
	// just pick the first one
	$zone=1;
	}


      chdir('/home/bucknerk/www/cina_eval/waiting_point');
      $command="./create_input $path_to_sim";
      $command.=" " . $range[0][0] . " " . $range[0][1];
      $command.=" " . $range[1][0] . " " . $range[1][1];
      $command.=" " . $steps[0] . " " . $steps[1];
      $command.=" " . trim($zone);
      $command.= " > output_data/" . $_POST["OUTPUT_FILE"];
      //echo "<br> COMMAND IS $command<br>";
      /*
      $flog=fopen("wait.log","a");
      if($flog) {
	fwrite($flog,date("Y/m/d H:m:s"));
        fwrite($flog,$command . "\n");
	fclose($flog);
	}
      */
      $desc[0]=array('pipe','r');
      $desc[2]=array('pipe','w');

      $pfp=proc_open($command,$desc,$pipes);
      if(! is_resource($pfp)) {
	$err= "opening :$command";
	$err.="{$t_array['eol']}";
	trigger_error($err,E_USER_ERROR);
	return;
      }

      //$buf=fgets($pfp);
      $once=true;
      $buf=fgets($pipes[2]);
      if(!feof($pipes[2])) {
	echo "ERRORS=Returned from system:{$t_array['eol']}";
	while(!feof($pipes[2])) {
	  echo "$buf{$t_array['eol']}";
	  $buf=fgets($pipes[2]);
	  }
	print($t_array['footer']);
	proc_close($pfp);
	return;
	}
      else {
	printf($t_array['header'],"ZONE= " . $zone . "{$t_array['eol']}DATA FILE " . $_POST["OUTPUT_FILE"] );
	echo "Should have been created, please verify {$t_array['eol']}";
	print($t_array['footer']);
	proc_close($pfp);
	return;
	}
      }
    else {
      $err= "Missing one of ID, USER, PATH, ZONE, RANGE,";
      $err.=" TIME_STEP, MIN_ABUND, MIN_EFF_LIFE, RAT_EFF_LIFE";
      $err.=" OUTPUT_FILE from the input stream:";
      $err.="{$t_array['eol']}";
      trigger_error($err,E_USER_ERROR);
      }
  }
	    
	    
	    
	    
	    




