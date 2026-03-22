<?php

class SimWorkflowRunUtils{
	
	public $id;
	public $username;
	public $pw;
	public $cgi_com;
	
	public function __construct($id, $username, $pw){
		$this->id = $id;
		$this->username = $username;
		$this->pw = $pw;
		$this->cgi_com = new CGICom();
	}
	
	public function get_sim_workflow_runs(){
	
	    $query = "SELECT sim_workflow_run_index, sim_workflow_index FROM sim_workflow_runs WHERE username=:username AND name<>''";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('username', $this->username);
	    $pdos->execute();

	    while($row = $pdos->fetch_assoc()){
	        
	        $sim_workflow_run_index =  $row["sim_workflow_run_index"];
	        
	        $sim_workflow_run = new SimWorkflowRun();
	        $sim_workflow_run->populate($sim_workflow_run_index);
	        $sim_workflow_run->print_all();
	        
	        $sim_workflow_index =  $row["sim_workflow_index"];
	        
	        $sim_workflow = new SimWorkflow();
	        $sim_workflow->populate($sim_workflow_index);
	        $sim_workflow->print_all2();

	    }
	}
	
	public function name_exists($name){
	    $query = "SELECT name FROM sim_workflow_runs WHERE username=:username";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_str_value('username', $this->username);
	    $pdos->execute();
	    
	    while($row = $pdos->fetch_assoc()){
	        if($name==$row['name']){
	            print("NAME_EXISTS=true\n");
	            exit();
	        }
	    }
	    print("NAME_EXISTS=false\n");
	}
	
	public function erase_sim_workflow_run($sim_workflow_run_index){
	    $sim_workflow_run = new SimWorkflowRun();
	    $sim_workflow_run->populate($sim_workflow_run_index);
	    $sim_workflow_run->erase();
	}
	
	public function save_sim_workflow_run($sim_workflow_run_index, $sim_name_base, $notes){
		
		$sim_workflow_run = new SimWorkflowRun();
		$sim_workflow_run->populate($sim_workflow_run_index);
		
		$sim_workflow_index = $sim_workflow_run->sim_workflow_index;
		
		$sim_workflow = new SimWorkflow();
		$sim_workflow->populate($sim_workflow_index);
		
		$sim_workflow_sim_array = $sim_workflow->populate_sim_workflow_sim_array($sim_workflow_index);
	
		$username = $this->username;
		$al26_type = $sim_workflow->al26_type;
		$sunet_path = $sim_workflow->sunet_path;
		$sim_type = $sim_workflow->sim_type;
		$init_abund_path = $sim_workflow->init_abund_path;
		$thermo_path = $sim_workflow->thermo_path;
		$max_timesteps = $sim_workflow->max_timesteps;
		$include_weak = $sim_workflow->include_weak;
		$include_screening = $sim_workflow->include_screening;
		$start_time = $sim_workflow->start_time;
		$stop_time = $sim_workflow->stop_time;
		$reaction_rate = $sim_workflow->reaction_rate;
		$lib_dir = $sim_workflow->lib_dir;
		$sim_workflow_mode = $sim_workflow->sim_workflow_mode;
		
		$looping_type = "";
		if($sim_workflow_mode==SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING){
			$looping_type = "Double Looping";
		}else if($sim_workflow_mode==SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING){
			$looping_type = "Single Looping";
		}

		switch($sim_workflow_mode){
				
			case SimWorkflowMode::SINGLE_STANDARD:
			    $this->save_sims_from_temp($sim_name_base, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::SINGLE_STANDARD_SENS:
			    $this->save_sims_from_temp_sens($sim_name_base, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::SINGLE_CUSTOM:
			    $this->save_sims_from_temp($sim_name_base, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::SINGLE_CUSTOM_SENS:
			    $this->save_sims_from_temp_sens($sim_name_base, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::DIR_STANDARD:
			    $this->save_sims_from_temp_lib_dir($sim_name_base, $lib_dir, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING:
			    $this->save_sims_from_temp_lib_dir($sim_name_base, $lib_dir, $sim_workflow_run_index);
				break;
			case SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING:
			    $this->save_sims_from_temp_lib_dir($sim_name_base, $lib_dir, $sim_workflow_run_index);
				break;
		
		}
		
		for($i=0; $i<count($sim_workflow_sim_array); $i++){
		
			$sim_workflow_sim = $sim_workflow_sim_array[$i];
			
			$library = $sim_workflow_sim->library;
			$library_path = $sim_workflow_sim->library_path;
			$params =  $sim_workflow_sim->params;
			$scale_factor = $sim_workflow_sim->scale_factor;
			$zones = $sim_workflow_sim->zones;
			$sim_name = "";
			
			switch($sim_workflow_mode){
					
				case SimWorkflowMode::SINGLE_STANDARD:
					$sim_name = $sim_name_base;
					break;
				case SimWorkflowMode::SINGLE_STANDARD_SENS:
					$sim_name = $sim_name_base."_".($i+1);
					break;
				case SimWorkflowMode::SINGLE_CUSTOM:
					$sim_name = $sim_name_base;
					break;
				case SimWorkflowMode::SINGLE_CUSTOM_SENS:
					$sim_name = $sim_name_base."_".($i+1);
					break;
				case SimWorkflowMode::DIR_STANDARD:
					$sim_name = $sim_name_base."_".$library;
					break;
				case SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING:
					$sim_name = $sim_name_base."_".$library;
					break;
				case SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING:
					$sim_name = $sim_name_base."_".$library;
					break;
			
			}
			
			$query = "INSERT INTO sims (username, 
										folder_type, 
										sim_name, 
										library_path, 
										min_iso, 
										max_iso, 
										al26_type,
										sunet_path,
										sim_type,
										notes, 
										init_abund_path,
										thermo_path,
										max_timesteps,
										include_weak,
										include_screening,
										start_time,
										stop_time,
										zones,
										creation_date,
										scale_factor,
										reaction_rate,
										params,
										lib_dir,
										looping_type
										) VALUES (
										:username, 
										'USER', 
										:sim_name, 
										:library_path, 
										'', 
										'', 
										:al26_type,
										:sunet_path,
										:sim_type,
										:notes, 
										:init_abund_path,
										:thermo_path,
										:max_timesteps,
										:include_weak,
										:include_screening,
										:start_time,
										:stop_time,
										:zones,
										NOW(),
										:scale_factor,
										:reaction_rate,
										:params,
										:lib_dir,
										:looping_type)";
			
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('username',			$username);
			$pdos->set_str_value('sim_name', 			$sim_name);
			$pdos->set_str_value('library_path', 		$library_path);
			$pdos->set_str_value('al26_type', 			$al26_type);
			$pdos->set_str_value('sunet_path', 			$sunet_path);
			$pdos->set_str_value('sim_type', 			$sim_type);
			$pdos->set_str_value('notes', 				$notes);
			$pdos->set_str_value('init_abund_path', 	$init_abund_path);
			$pdos->set_str_value('thermo_path', 		$thermo_path);
			$pdos->set_int_value('max_timesteps', 		$max_timesteps);
			$pdos->set_str_value('include_weak', 		$include_weak);
			$pdos->set_str_value('include_screening', 	$include_screening);
			$pdos->set_str_value('start_time', 			$start_time);
			$pdos->set_str_value('stop_time', 			$stop_time);
			$pdos->set_str_value('zones', 				$zones);
			$pdos->set_str_value('scale_factor', 		$scale_factor);
			$pdos->set_str_value('reaction_rate', 		$reaction_rate);
			$pdos->set_str_value('params', 				$params);
			$pdos->set_str_value('lib_dir', 			$lib_dir);
			$pdos->set_str_value('looping_type', 		$looping_type);
			$pdos->execute();
				
		}
		
		$save_sim_workflow_run_message = "";
		
		switch($sim_workflow_mode){
			
			case SimWorkflowMode::SINGLE_STANDARD:
			    $save_sim_workflow_run_message = "Simulation was saved to USER/$sim_name_base.";
				break;
			case SimWorkflowMode::SINGLE_STANDARD_SENS:
				$start_sim_name = $sim_name_base."_1";
				$end_sim_name = $sim_name_base."_".count($sim_workflow_sim_array);
				$save_sim_workflow_run_message = "Simulation was saved to USER/$start_sim_name through USER/$end_sim_name.";
				break;
			case SimWorkflowMode::SINGLE_CUSTOM:
			    $save_sim_workflow_run_message = "Simulation was saved to USER/$sim_name_base.";
				break;
			case SimWorkflowMode::SINGLE_CUSTOM_SENS:
				$start_sim_name = $sim_name_base."_1";
				$end_sim_name = $sim_name_base."_".count($sim_workflow_sim_array);
				$save_sim_workflow_run_message = "Simulation was saved to USER/$start_sim_name through USER/$end_sim_name.";
				break;
			case SimWorkflowMode::DIR_STANDARD:
				$start_sim_workflow_sim = $sim_workflow_sim_array[0];
				$end_sim_workflow_sim = $sim_workflow_sim_array[count($sim_workflow_sim_array)-1];
				$start_lib = $start_sim_workflow_sim->library;
				$end_lib = $end_sim_workflow_sim->library;
				$start_sim_name = $sim_name_base."_".$start_lib;
				$end_sim_name = $sim_name_base."_".$end_lib;
				$save_sim_workflow_run_message = "Simulation was saved to USER/$start_sim_name through USER/$end_sim_name.";
				break;
			case SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING:
				$start_sim_workflow_sim = $sim_workflow_sim_array[0];
				$end_sim_workflow_sim = $sim_workflow_sim_array[count($sim_workflow_sim_array)-1];
				$start_lib = $start_sim_workflow_sim->library;
				$end_lib = $end_sim_workflow_sim->library;
				$start_sim_name = $sim_name_base."_".$start_lib;
				$end_sim_name = $sim_name_base."_".$end_lib;
				$save_sim_workflow_run_message = "Simulation was saved to USER/$start_sim_name through USER/$end_sim_name.";
				break;
			case SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING:
				$start_sim_workflow_sim = $sim_workflow_sim_array[0];
				$end_sim_workflow_sim = $sim_workflow_sim_array[count($sim_workflow_sim_array)-1];
				$start_lib = $start_sim_workflow_sim->library;
				$end_lib = $end_sim_workflow_sim->library;
				$start_sim_name = $sim_name_base."_".$start_lib;
				$end_sim_name = $sim_name_base."_".$end_lib;
				$save_sim_workflow_run_message = "Simulation was saved to USER/$start_sim_name through USER/$end_sim_name.";
				break;
		
		}
		
		$sim_workflow_run->erase();
		
		print("SAVE_SIM_WORKFLOW_RUN_MESSAGE=".$save_sim_workflow_run_message."\n");
	
	}
	
	public function get_sim_workflow_run_status(){
		
		$sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
		
		$query = "SELECT * FROM sim_workflow_runs WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_run_index', $sim_workflow_run_index);
		$pdos->execute();
		
		$row = $pdos->fetch_assoc();
		
		$current_status = 		$row["current_status"];
		$current_lib = 			$row["current_lib"];
		$current_zone = 		$row["current_zone"];
		$current_scale_factor = $row["current_scale_factor"];
		$current_text = 		$row["current_text"];

		if($current_status!=""){
			
			print("CURRENT_STATUS=$current_status\n");
			
			if($current_status==SimWorkflowRunStatus::ERROR){
				$message = "An error has occurred during your simulation. "
								."The appropriate staff have been notified.";
				print("ERROR=$message\n");
				
			}
		}
		
		if($current_lib!=""){
			print("CURRENT_LIB=$current_lib\n");
		}
		
		if($current_zone!=""){
			print("CURRENT_ZONE=$current_zone\n");
		}
		
		if($current_scale_factor!=""){
			print("CURRENT_SCALE_FACTOR=$current_scale_factor\n");
		}
		
		if($current_text!=""){
			print("CURRENT_TEXT=$current_text\n");
		}
	
	}
	
	private function save_sims_from_temp($sim_name_base, $sim_workflow_run_index){
	
		$username = $this->username;
		
		$old_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims/tmp/$sim_workflow_run_index";
		$new_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims";
	
		system("rm -Rf $new_dir/$sim_name_base");
		$query = "DELETE FROM sims WHERE sim_name=:sim_name AND folder_type='USER' AND username=:username";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('sim_name', $sim_name_base);
		$pdos->set_str_value('username', $username);
		$pdos->execute();
	
		if (!file_exists($new_dir)){
			mkdir($new_dir, 0755);
		}
	
		$old_path = $old_dir . "/sim_1";
		if (file_exists($old_path)) {
			$new_path = "$new_dir/$sim_name_base";
			if (mkdir($new_path, 0755)) {
				system("mv $old_path/* $new_path");
				system("rm -Rf $old_path");
			}
		}
	
	}
	
	private function save_sims_from_temp_sens($sim_name_base, $sim_workflow_run_index){
		
		$username = $this->username;
		
		$old_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims/tmp/$sim_workflow_run_index";
		$new_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims";
		
		// delete all sims called PATH from users regular directory; cycle up to _9.
		for ($i = 1; $i <= 11; $i ++) {
			system("rm -Rf $new_dir/$sim_name_base"."_$i");
			$sim_name = $sim_name_base."_$i";
			$query = "DELETE FROM sims WHERE sim_name=:sim_name AND folder_type='USER' AND username=:username";
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('sim_name', $sim_name);
			$pdos->set_str_value('username', $username);
			$pdos->execute();
		}
		
		if (!file_exists($new_dir)){
			mkdir($new_dir, 0755);
		}
		
		for($i=1; $i<11; $i++){
			$old_path = $old_dir . "/sim_" . $i;
			if (file_exists($old_path)) {
				$new_path = $new_dir . "/" . $sim_name_base . "_" . $i;
				if (mkdir($new_path, 0755)) {
					system("mv $old_path/* $new_path");
					system("rm -Rf $old_path");
				}
			}
		}
	
	}
	
	private function save_sims_from_temp_lib_dir($sim_name_base, $lib_dir, $sim_workflow_run_index){
		
		$username = $this->username;
		
		$old_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims/tmp/$sim_workflow_run_index";
		$new_dir = PHPUtils::$DATA_PATH."/USER/$username/em_sims";
		
		$array = DataUtils::get_dir_children($old_dir);
		
		foreach($array as $temp_sim_name){
			
		    if(strpos($temp_sim_name, $lib_dir."_")!==false && $temp_sim_name!=""){
		        $library_name = substr($temp_sim_name, strpos($temp_sim_name, "sim_") + 4);
				system("rm -Rf " . $new_dir . "/" . $sim_name_base . "_" . $library_name);
				$sim_name = $sim_name_base."_$library_name";
				$query = "DELETE FROM sims WHERE sim_name=:sim_name AND folder_type='USER' AND username=:username";
				$pdos = new PDOSUtil($query);
				$pdos->set_str_value('sim_name', $sim_name);
				$pdos->set_str_value('username', $username);
				$pdos->execute();
			}
		}
		
		if (!file_exists($new_dir)){
			mkdir($new_dir, 0755);
		}
		
		$array = DataUtils::get_dir_children($old_dir);
		
		foreach($array as $temp_sim_name){
		    if(strpos($temp_sim_name, $lib_dir."_")!==false && $temp_sim_name!=""){
			    $library_name = substr($temp_sim_name, strpos($temp_sim_name, "sim_") + 4);
			    $old_path = $old_dir . "/" . $temp_sim_name;
				if (file_exists($old_path)) {
					$new_path = $new_dir . "/" . $sim_name_base . "_" . $library_name;
					if (mkdir($new_path, 0755)) {
						system("mv $old_path/* $new_path");
						system("rm -Rf $old_path");
					}
				}
			}
		}
	
	}
	
}

?>
