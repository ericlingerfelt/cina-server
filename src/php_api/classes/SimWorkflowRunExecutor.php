<?php

class SimWorkflowRunExecutor{
	
	public $id;
	public $username;
	public $pw;
	public $sim_workflow_index;
	public $sim_workflow_run_index;
	public $sim_workflow;
	public $sim_workflow_run;
	public $sim_workflow_sim_array;
	public $cgi_com;
	public $sleep_interval = 1;
	
	public function __construct($id, $username, $pw, $sim_workflow_run_index){
		
		$this->id = $id;
		$this->username = $username;
		$this->pw = $pw;
		$this->sim_workflow_run_index = $sim_workflow_run_index;
		
		$this->cgi_com = new CGICom();
		$this->sim_workflow_utils = new SimWorkflowUtils($this->id, $this->username, $this->pw);
		
		$this->sim_workflow_run = new SimWorkflowRun();
		$this->sim_workflow_run->populate($this->sim_workflow_run_index);
		
		$this->sim_workflow_index = $this->sim_workflow_run->sim_workflow_index;
		
		$this->sim_workflow = new SimWorkflow();
		$this->sim_workflow->populate($this->sim_workflow_index);
		
		$this->sim_workflow_sim_array = $this->sim_workflow->populate_sim_workflow_sim_array($this->sim_workflow_index);
		
	}
	
	public function abort_sim_workflow_run(){
		
		$this->sim_workflow_run->update_current_status(SimWorkflowRunStatus::ABORTED);
		$this->sim_workflow_run->set_end_date();
		
		if($this->sim_workflow->sim_workflow_mode==SimWorkflowMode::SINGLE_STANDARD_SENS
				|| $this->sim_workflow->sim_workflow_mode==SimWorkflowMode::SINGLE_CUSTOM_SENS){
			
			for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
				
			    $temp_lib="temp_".$this->sim_workflow_run_index."_$i";
				$this->delete_temp_lib($temp_lib);
				
			}
		}
		
		$this->delete_temp_sims();
		$this->sim_workflow_run->kill();
		
	}

	public function handle_sim_workflow_run_error($error_log_index, $id, $username, $pw, $sim_workflow_run_index){
		
	    $sim_workflow_run_executor = new SimWorkflowRunExecutor($id, $username, $pw, $sim_workflow_run_index);
	    $sim_workflow_run_executor->sim_workflow_run->set_error_log_index($error_log_index);
	    $sim_workflow_run_executor->sim_workflow_run->update_current_status(SimWorkflowRunStatus::ERROR);
	    $sim_workflow_run_executor->sim_workflow_run->set_end_date();	
		
	    if($sim_workflow_run_executor->sim_workflow->sim_workflow_mode==SimWorkflowMode::SINGLE_STANDARD_SENS
	        || $sim_workflow_run_executor->sim_workflow->sim_workflow_mode==SimWorkflowMode::SINGLE_CUSTOM_SENS){
			
            for($i=0; $i<count($sim_workflow_run_executor->sim_workflow_sim_array); $i++){
			
			    $temp_lib="temp_".$this->sim_workflow_run_index."_$i";
			    $sim_workflow_run_executor->delete_temp_lib($temp_lib);

			}
			
		}
		
		$sim_workflow_run_executor->delete_temp_sims();
		$sim_workflow_run_executor->sim_workflow_run->kill();
		
	}
	
	public function execute_sim_workflow_run(){
	
		$this->sim_workflow_run->set_start_date();
		$this->sim_workflow_run->update_current_status(SimWorkflowRunStatus::RUNNING);
		
		switch($this->sim_workflow->sim_workflow_mode){
		
			case SimWorkflowMode::SINGLE_STANDARD:
				$this->execute_SINGLE_STANDARD();
				break;
			case SimWorkflowMode::SINGLE_STANDARD_SENS:
				$this->execute_SINGLE_STANDARD_SENS();
				break;
			case SimWorkflowMode::SINGLE_CUSTOM:
				$this->execute_SINGLE_CUSTOM();
				break;
			case SimWorkflowMode::SINGLE_CUSTOM_SENS:
				$this->execute_SINGLE_CUSTOM_SENS();
				break;
			case SimWorkflowMode::DIR_STANDARD:
				$this->execute_DIR_STANDARD();
				break;
			case SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING:
				$this->execute_DIR_CUSTOM_DOUBLE_LOOPING();
				break;
			case SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING:
				$this->execute_DIR_CUSTOM_SINGLE_LOOPING();
				break;
		
		}
		
		$this->sim_workflow_run->set_end_date();
		$this->sim_workflow_run->update_current_status(SimWorkflowRunStatus::COMPLETE);
	
	}
	
	private function get_element_synthesis_update_text(){
	
		$element_synthesis_update_text = "";
		
		$redir_path = PHPUtils::$DATA_PATH."/tmp/".$this->sim_workflow_run_index."/em_syn/redir";
		$contents = file_get_contents($redir_path);
		$array = explode("\n", $contents);
		$array_length = count($array);
		if($array_length > 10){
			
			for($i=($array_length-10); $i<$array_length; $i++){
				$element_synthesis_update_text .= $array[$i]."|";
			}
			
		}else{
			
			for($i=0; $i<$array_length; $i++){
				$element_synthesis_update_text .= $array[$i]."|";
			}
		
		}
		
		return $element_synthesis_update_text;
		
	}
	
	//TESTED FOR EXECUTION FOR SINGLE AND MULTI ZONE
	private function execute_SINGLE_STANDARD(){
	
		$sim_workflow_sim = $this->sim_workflow_sim_array[0];
		$this->sim_workflow_run->update_current_lib($sim_workflow_sim->library);
		$this->sim_workflow_run->update_current_text("Creating reaction rate network for this simulation.");
		$this->sim_workflow_run->update_current_zone("Please wait...");
		
		//1.1 ELEMENT SYNTHESIZE SETUP
		$input_array = array();
		$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
		$input_array["LIBRARY"] = 			     $sim_workflow_sim->library;
		$input_array["TYPE"] = 				     $this->sim_workflow->al26_type;
		$input_array["SUNET_PATH"] = 		     $this->sim_workflow->sunet_path;
		$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);

		//1.2 SYNTHESIZE ELEMENTS
		$input_array = array();
		$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
		$input_array["NOTES"] = 			$this->sim_workflow->notes;
		$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
		$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
		$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
		$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
		$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
		$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
		$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
		$input_array["ZONES"] = 			$sim_workflow_sim->zones;
		$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
		
		//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
		do{
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
			$current_text = $this->get_element_synthesis_update_text();
			$current_zone = $output_array["ZONE"];
			$this->sim_workflow_run->update_current_zone($current_zone);
			$this->sim_workflow_run->update_current_text($current_text);
			sleep($this->sleep_interval);
		}
		while($output_array["SYNTHESIS"] == "RUNNING");
		
		//2. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		//3. MOVE SIM TO TEMP
		$temp_sim_name = "sim_1";
		$this->move_sim_to_temp($temp_sim_name);
	
	}
	
	//TESTED FOR EXECUTION
	private function execute_SINGLE_STANDARD_SENS(){
	
		$sim_workflow_sim = $this->sim_workflow_sim_array[0];
		$this->sim_workflow_run->update_current_lib($sim_workflow_sim->library);

		//1. CREATE UP TO TEN LIBRARIES BASED ON INPUT PARAMS LISTS
		$temp_lib_array = array();
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
		    $temp_lib_array[]="temp_".$this->sim_workflow_run_index."_$i";
		}
		
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
			
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$scale_factor = $sim_workflow_sim->scale_factor;
			$library = 		$sim_workflow_sim->library;
			$params = 		$sim_workflow_sim->params;
			$temp_lib = 	$temp_lib_array[$i];
			
			$this->sim_workflow_run->update_current_scale_factor($scale_factor);
			$this->sim_workflow_run->update_current_text("Creating reaction rate library for scale factor: $scale_factor");
			
			//MAKE A COPY OF THE BASE LIBRARY FOR EACH INPUT PARAMS SET
			$input_array = array();
			$input_array["SRC_LIB"] = 			$library;
			$input_array["DEST_LIB"] = 			$temp_lib;
			$input_array["DEST_GROUP"] = 		"USER";
			$input_array["DEL_SRC_LIB"] = 		"NO";
			$output_array = $this->cgi_com->execute(CGIComActions::MODIFY_RATE_LIBRARY, $this->id, $this->username, $this->pw, $input_array);
			
			//GET THE RATE INFO FOR ALL PROPERTIES
			$input_array = array();
			$input_array["RATES"] = 			$this->sim_workflow->reaction_rate_id;
			$output_array = $this->cgi_com->execute(CGIComActions::GET_RATE_INFO, $this->id, $this->username, $this->pw, $input_array);
			
			//MODIFY PROPERTIES FOR NEW PARAMS
			$current_properties = $output_array["PROPERTIES"];
			$current_properties_array = explode("\t", $current_properties);
			$new_properties = "";
			foreach($current_properties_array as $property){
			    $subarray = explode("=", $property);
			    if(trim($subarray[0])=="Parameters"){
			        $new_properties.= trim($subarray[0])." = ".$params."\t";
			    }else{
			        $new_properties.= $property."\t";
			    }
			}
			
			$new_properties = substr($new_properties, 0, -1);
			
			//APPLY THE SCALED PARAMS TO THE CORRECT RATE ID IN THE TEMP LIBRARY
			$input_array = array();
			$input_array["DEST_LIB"] = 			$temp_lib;
			$input_array["PROPERTIES"] = 		$new_properties;
			$output_array = $this->cgi_com->execute(CGIComActions::MODIFY_RATES, $this->id, $this->username, $this->pw, $input_array);
		
		}
		
		//2. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		//3. LOOP OVER NEW LIBRARIES 
		for($i=0; $i<count($temp_lib_array); $i++){
			
			$temp_lib = $temp_lib_array[$i];
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$scale_factor = $sim_workflow_sim->scale_factor;
			$this->sim_workflow_run->update_current_scale_factor($scale_factor);
			$this->sim_workflow_run->update_current_text("Creating reaction rate network for scale factor: $scale_factor");
			
			//3.1 ELEMENT SYNTHESIS SETUP
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["LIBRARY"] = 			$temp_lib;
			$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
			$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
			
			//3.2 SYNTHESIZE ELEMENTS
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["NOTES"] = 			$this->sim_workflow->notes;
			$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
			$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
			$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
			$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
			$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
			$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
			$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
			$input_array["ZONES"] = 			$sim_workflow_sim->zones;
			$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
			do{
				$input_array = array();
				$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
				$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
				$current_text = $this->get_element_synthesis_update_text();
				$current_zone = $output_array["ZONE"];
				$this->sim_workflow_run->update_current_zone($current_zone);
				$this->sim_workflow_run->update_current_text($current_text);
				sleep($this->sleep_interval);
			}
			while($output_array["SYNTHESIS"] == "RUNNING");
			
			//3.3 MOVE SIM TO TEMP
			$temp_sim_name = "sim_".($i+1);
			$this->move_sim_to_temp($temp_sim_name);
			
			//3.4 DELETE TEMP LIBRARY
			$this->delete_temp_lib($temp_lib);
			
		}
		
	}
	
	//TESTED FOR EXECUTION FOR SINGLE AND MULTI ZONE
	private function execute_SINGLE_CUSTOM(){
	
		$sim_workflow_sim = $this->sim_workflow_sim_array[0];
		$this->sim_workflow_run->update_current_lib($sim_workflow_sim->library);
		$this->sim_workflow_run->update_current_text("Creating reaction rate network for this simulation.");
		$this->sim_workflow_run->update_current_zone("Please wait...");
		
		//1.1 ELEMENT SYNTHESIZE SETUP
		$input_array = array();
		$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
		$input_array["LIBRARY"] = 			$sim_workflow_sim->library;
		$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
		$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
		$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
		
		//1.2 SYNTHESIZE ELEMENTS
		$input_array = array();
		$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
		$input_array["NOTES"] = 			$this->sim_workflow->notes;
		$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
		$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
		$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
		$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
		$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
		$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
		$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
		$input_array["ZONES"] = 			$sim_workflow_sim->zones;
		$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);

		//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
		do{
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
			$current_text = $this->get_element_synthesis_update_text();
			$current_zone = $output_array["ZONE"];
			$this->sim_workflow_run->update_current_zone($current_zone);
			$this->sim_workflow_run->update_current_text($current_text);
			sleep($this->sleep_interval);
		}
		while($output_array["SYNTHESIS"] == "RUNNING");
		
		//2. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		//3. MOVE SIM TO TEMP
		$temp_sim_name = "sim_1";
		$this->move_sim_to_temp($temp_sim_name);
		
	}
	
	//TESTED FOR EXECUTION
	private function execute_SINGLE_CUSTOM_SENS(){
	
		$sim_workflow_sim = $this->sim_workflow_sim_array[0];
		$this->sim_workflow_run->update_current_lib($sim_workflow_sim->library);

		//1. CREATE UP TO TEN LIBRARIES BASED ON INPUT PARAMS LISTS
		$temp_lib_array = array();
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
		    $temp_lib_array[]="temp_".$this->sim_workflow_run_index."_$i";
		}
		
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
			
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$scale_factor = $sim_workflow_sim->scale_factor;
			$library = 	$sim_workflow_sim->library;
			$params = 	$sim_workflow_sim->params;
			$temp_lib = $temp_lib_array[$i];
			
			$this->sim_workflow_run->update_current_scale_factor($scale_factor);
			$this->sim_workflow_run->update_current_text("Creating reaction rate library for scale factor: $scale_factor");
			
			//MAKE A COPY OF THE BASE LIBRARY FOR EACH INPUT PARAMS SET
			$input_array = array();
			$input_array["SRC_LIB"] = 			$library;
			$input_array["DEST_LIB"] = 			$temp_lib;
			$input_array["DEST_GROUP"] = 		"USER";
			$input_array["DEL_SRC_LIB"] = 		"NO";
			$output_array = $this->cgi_com->execute(CGIComActions::MODIFY_RATE_LIBRARY, $this->id, $this->username, $this->pw, $input_array);
			
			//GET THE RATE INFO FOR ALL PROPERTIES
			$input_array = array();
			$input_array["RATES"] = 			$this->sim_workflow->reaction_rate_id;
			$output_array = $this->cgi_com->execute(CGIComActions::GET_RATE_INFO, $this->id, $this->username, $this->pw, $input_array);
			
			//MODIFY PROPERTIES FOR NEW PARAMS
			$current_properties = $output_array["PROPERTIES"];
			$current_properties_array = explode("\t", $current_properties);
			$new_properties = "";
			foreach($current_properties_array as $property){
			    $subarray = explode("=", $property);
			    if(trim($subarray[0])=="Parameters"){
			        $new_properties.= trim($subarray[0])." = ".$params."\t";
			    }else{
			        $new_properties.= $property."\t";
			    }
			}
			
			$new_properties = substr($new_properties, 0, -1);
			
			//APPLY THE SCALED PARAMS TO THE CORRECT RATE ID IN THE TEMP LIBRARY
			$input_array = array();
			$input_array["DEST_LIB"] = 			$temp_lib;
			$input_array["PROPERTIES"] = 		$new_properties;
			$output_array = $this->cgi_com->execute(CGIComActions::MODIFY_RATES, $this->id, $this->username, $this->pw, $input_array);
			
		}
		
		//2. DELETE TEMP SIMS IN USER ACCOUNT
		$this->delete_temp_sims();
		
		//3. LOOP OVER NEW LIBRARIES 
		for($i=0; $i<count($temp_lib_array); $i++){
			
			$temp_lib = $temp_lib_array[$i];
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$scale_factor = $sim_workflow_sim->scale_factor;
			$this->sim_workflow_run->update_current_scale_factor($scale_factor);
			$this->sim_workflow_run->update_current_text("Creating reaction rate network for scale factor: $scale_factor");
			
			//3.1 ELEMENT SYNTHESIS SETUP
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["LIBRARY"] = 			$temp_lib;
			$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
			$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
			
			//3.2 SYNTHESIZE ELEMENTS
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["NOTES"] = 			$this->sim_workflow->notes;
			$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
			$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
			$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
			$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
			$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
			$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
			$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
			$input_array["ZONES"] = 			$sim_workflow_sim->zones;
			$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
			do{
				$input_array = array();
				$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
				$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
				$current_text = $this->get_element_synthesis_update_text();
				$current_zone = $output_array["ZONE"];
				$this->sim_workflow_run->update_current_zone($current_zone);
				$this->sim_workflow_run->update_current_text($current_text);
				sleep($this->sleep_interval);
			}
			while($output_array["SYNTHESIS"] == "RUNNING");
			
			//3.3 MOVE SIM TO TEMP
			$temp_sim_name = "sim_".($i+1);
			$this->move_sim_to_temp($temp_sim_name);
			
			//3.4 DELETE TEMP LIBRARY
			$this->delete_temp_lib($temp_lib);
			
		}
		
	}
	
	//TESTED FOR EXECUTION FOR SINGLE ZONE AND MULTIZONE
	private function execute_DIR_STANDARD(){
	
		//1. LOOP OVER LIBRARIES IN DIRECTORY
		$lib_dir = $this->sim_workflow->lib_dir;
		
		//1.1. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
			
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$library = $sim_workflow_sim->library;
			$this->sim_workflow_run->update_current_lib($library);
			$this->sim_workflow_run->update_current_text("Creating reaction rate network for reaction rate library: $library");
			$this->sim_workflow_run->update_current_zone("Please wait...");
			
			//1.2 ELEMENT SYNTHESIZE SETUP
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["LIBRARY"] = 			$library;
			$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
			$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 SYNTHESIZE ELEMENTS
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["NOTES"] = 			$this->sim_workflow->notes;
			$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
			$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
			$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
			$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
			$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
			$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
			$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
			$input_array["ZONES"] = 			$sim_workflow_sim->zones;
			$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
			do{
				$input_array = array();
				$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
				$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
				$current_text = $this->get_element_synthesis_update_text();
				$current_zone = $output_array["ZONE"];
				$this->sim_workflow_run->update_current_zone($current_zone);
				$this->sim_workflow_run->update_current_text($current_text);
				sleep($this->sleep_interval);
			}
			while($output_array["SYNTHESIS"] == "RUNNING");
			
			//1.4 MOVE SIM TO TEMP
			$temp_sim_name = "sim_".$library;
			$this->move_sim_to_temp($temp_sim_name);
			
		}
		
	}
	
	//TESTED FOR EXECUTION FOR SINGLE ZONE AND MULTIZONE
	private function execute_DIR_CUSTOM_DOUBLE_LOOPING(){
	
		//1. LOOP OVER LIBRARIES IN DIRECTORY
		$lib_dir = $this->sim_workflow->lib_dir;
		
		//1.1. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
			
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$library = $sim_workflow_sim->library;
			$this->sim_workflow_run->update_current_lib($library);
			$this->sim_workflow_run->update_current_text("Creating reaction rate network for reaction rate library: $library");
			$this->sim_workflow_run->update_current_zone("Please wait...");
			
			//1.2 ELEMENT SYNTHESIZE SETUP
			$input_array = array();
			$input_array["LIBRARY"] = 			$library;
			$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
			$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 SYNTHESIZE ELEMENTS
			$input_array = array();
			$input_array["NOTES"] = 			$this->sim_workflow->notes;
			$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
			$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
			$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
			$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
			$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
			$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
			$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
			$input_array["ZONES"] = 			$sim_workflow_sim->zones;
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
			do{
				$input_array = array();
				$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
				$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
				$current_text = $this->get_element_synthesis_update_text();
				$current_zone = $output_array["ZONE"];
				$this->sim_workflow_run->update_current_zone($current_zone);
				$this->sim_workflow_run->update_current_text($current_text);
				sleep($this->sleep_interval);
			}
			while($output_array["SYNTHESIS"] == "RUNNING");
			
			//1.4 MOVE SIM TO TEMP
			$temp_sim_name = "sim_".$library;
			$this->move_sim_to_temp($temp_sim_name);
		
		}
	
	}
	
	//TESTED FOR EXECUTION
	private function execute_DIR_CUSTOM_SINGLE_LOOPING(){
	
		//1. ONE-TO-ONE LOOP OVER LIBRARIES IN DIRECTORY AND CUSTOM THERMO PROFILES
		$lib_dir = $this->sim_workflow->lib_dir;

		//1.1. DELETE TEMP SIMS IN USER ACCOUNT FOR INITIALIZATION
		$this->delete_temp_sims();
		
		for($i=0; $i<count($this->sim_workflow_sim_array); $i++){
			
			$sim_workflow_sim = $this->sim_workflow_sim_array[$i];
			$library = $sim_workflow_sim->library;
			$this->sim_workflow_run->update_current_lib($library);
			$this->sim_workflow_run->update_current_text("Creating reaction rate network for reaction rate library: $library");
			$this->sim_workflow_run->update_current_zone("Please wait...");
			
			//1.2 ELEMENT SYNTHESIZE SETUP
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["LIBRARY"] = 			$library;
			$input_array["TYPE"] = 				$this->sim_workflow->al26_type;
			$input_array["SUNET_PATH"] = 		$this->sim_workflow->sunet_path;
			$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 SYNTHESIZE ELEMENTS
			$input_array = array();
			$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
			$input_array["NOTES"] = 			$this->sim_workflow->notes;
			$input_array["INIT_ABUND_PATH"] = 	$this->sim_workflow->init_abund_path;
			$input_array["THERMO_PATH"] = 		$this->sim_workflow->thermo_path;
			$input_array["MAX_TIMESTEPS"] = 	$this->sim_workflow->max_timesteps;
			$input_array["INCLUDE_WEAK"] = 		$this->sim_workflow->include_weak;
			$input_array["INCLUDE_SCREENING"] = $this->sim_workflow->include_screening;
			$input_array["START_TIME"] = 		$this->sim_workflow->start_time;
			$input_array["STOP_TIME"] = 		$this->sim_workflow->stop_time;
			$input_array["ZONES"] = 			$sim_workflow_sim->zones;
			$output_array = $this->cgi_com->execute(CGIComActions::SYNTHESIZE_ELEMENTS, $this->id, $this->username, $this->pw, $input_array);
			
			//1.3 CHECK FOR RUNNING SIMULATION BEFORE MOVING TO TEMP
			do{
				$input_array = array();
				$input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
				$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_UPDATE, $this->id, $this->username, $this->pw, $input_array);
				$current_text = $this->get_element_synthesis_update_text();
				$current_zone = $output_array["ZONE"];
				$this->sim_workflow_run->update_current_zone($current_zone);
				$this->sim_workflow_run->update_current_text($current_text);
				sleep($this->sleep_interval);
			}
			while($output_array["SYNTHESIS"] == "RUNNING");
			
			//1.4 MOVE SIM TO TEMP
			$temp_sim_name = "sim_".$library;
			$this->move_sim_to_temp($temp_sim_name);
		
		}

	}

	private function delete_temp_lib($temp_lib){
	
		$temp_lib_dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/rate_libs/$temp_lib";
		system("rm -Rf $temp_lib_dir");
		
	}
	
	private function delete_temp_sims(){
		
	    $dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/tmp/".$this->sim_workflow_run_index."/*";
		system("rm -Rf $dir");
		
	}
	
	private function move_sim_to_temp($sim_name){
		
	    $old_dir = PHPUtils::$DATA_PATH."/tmp/".$this->sim_workflow_run_index."/em_syn";
	    $new_dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/tmp/".$this->sim_workflow_run_index."/".$sim_name;
		if(!file_exists($new_dir)){
			mkdir($new_dir, 0755, true);
		}
		system("mv $old_dir/* $new_dir/");
		
	}
	
}

?>
