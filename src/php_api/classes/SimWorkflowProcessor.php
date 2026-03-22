<?php

class SimWorkflowProcessor{
	
    public $name;
	public $username;
	public $pw;
	public $sim_workflow;
	
	public function __construct($name, $username, $pw){
	    $this->name = $name;
		$this->username = $username;
		$this->pw = $pw;
		$this->sim_workflow = new SimWorkflow();
	}
	
	public function create_sim_workflow(){
	
		switch($_POST["SIM_WORKFLOW_MODE"]){
		
			case SimWorkflowMode::SINGLE_STANDARD:
				$this->create_SINGLE_STANDARD();
				break;
			case SimWorkflowMode::SINGLE_STANDARD_SENS:
				$this->create_SINGLE_STANDARD_SENS();
				break;
			case SimWorkflowMode::SINGLE_CUSTOM:
				$this->create_SINGLE_CUSTOM();
				break;
			case SimWorkflowMode::SINGLE_CUSTOM_SENS:
				$this->create_SINGLE_CUSTOM_SENS();
				break;
			case SimWorkflowMode::DIR_STANDARD:
				$this->create_DIR_STANDARD();
				break;
			case SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING:
				$this->create_DIR_CUSTOM_DOUBLE_LOOPING();
				break;
			case SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING:
				$this->create_DIR_CUSTOM_SINGLE_LOOPING();
				break;
		
		}
	
	}
	
	private function create_SINGLE_STANDARD(){

	    $name = 	            $this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::SINGLE_STANDARD;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		"";
		$reaction_rate_id = 	"";
		$lib_dir = 				"";
		
		$sim_workflow_index = $this->sim_workflow->create($name, 
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$library_path = 		$_POST["LIBRARY_PATH"];
		$zones = 				$_POST["ZONES"];
		$scale_factor = 		"";
		$params = 				"";
		
		$sim_workflow_sim = new SimWorkflowSim();
		$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");
		
	}
	
	private function create_SINGLE_STANDARD_SENS(){
	
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::SINGLE_STANDARD_SENS;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		$_POST["REACTION_RATE"];
		$reaction_rate_id = 	$_POST["REACTION_RATE_ID"];
		$lib_dir = 				"";
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$library_path = 		$_POST["LIBRARY_PATH"];
		$zones = 				$_POST["ZONES"];
		$scale_factors = 		$_POST["SCALE_FACTORS"];
		$scale_factor_array = explode(",", $scale_factors);

		$params = 				$_POST["PARAMS"];
		$params_array = explode(":", $params);

		for($i=0; $i<count($scale_factor_array); $i++){
			
			$scale_factor = $scale_factor_array[$i];
			$params = 		$params_array[$i];
			
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);
			
		}
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");
		
	}
	
	private function create_SINGLE_CUSTOM(){
	
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::SINGLE_CUSTOM;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		"";
		$reaction_rate_id = 	"";
		$lib_dir = 				"";
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$library_path = 		$_POST["LIBRARY_PATH"];
		$zones = 				$_POST["ZONES"];
		$scale_factor = 		"";
		$params = 				"";
		
		$sim_workflow_sim = new SimWorkflowSim();
		$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");
		
	}
	
	private function create_SINGLE_CUSTOM_SENS(){
		
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::SINGLE_CUSTOM_SENS;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		$_POST["REACTION_RATE"];
		$reaction_rate_id = 	$_POST["REACTION_RATE_ID"];
		$lib_dir = 				"";
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$library_path = 		$_POST["LIBRARY_PATH"];
		$zones = 				$_POST["ZONES"];
		
		$scale_factors = 		$_POST["SCALE_FACTORS"];
		$scale_factor_array = explode(",", $scale_factors);

		$params = 				$_POST["PARAMS"];
		$params_array = explode(":", $params);

		for($i=0; $i<count($scale_factor_array); $i++){
			
			$scale_factor = $scale_factor_array[$i];
			$params = 		$params_array[$i];
			
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);
			
		}
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");

	}
	
	private function create_DIR_STANDARD(){
		
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::DIR_STANDARD;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		"";
		$reaction_rate_id = 	"";
		$lib_dir = 				$_POST["LIB_DIR"];
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$zones = 				$_POST["ZONES"];
		$scale_factor = 		"";
		$params = 				"";

		$lib_array = LibDirUtils::get_lib_dir_libs_static($lib_dir, $this->username);
		
		foreach($lib_array as $lib){
		
			$library_path = "USER/$lib";
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);
		
		}
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");
		
	}
	
	private function create_DIR_CUSTOM_DOUBLE_LOOPING(){
		
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::DIR_CUSTOM_DOUBLE_LOOPING;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		"";
		$reaction_rate_id = 	"";
		$lib_dir = 				$_POST["LIB_DIR"];
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$zones = 				$_POST["ZONES"];
		$scale_factor = 		"";
		$params = 				"";
		
		$lib_array = LibDirUtils::get_lib_dir_libs_static($lib_dir, $this->username);
		
		foreach($lib_array as $lib){
		
			$library_path = "USER/$lib";
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);

		}
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");

	}
	
	private function create_DIR_CUSTOM_SINGLE_LOOPING(){
		
	    $name = 	$this->name;
		$username = 			$this->username;
		$sim_workflow_mode = 	SimWorkflowMode::DIR_CUSTOM_SINGLE_LOOPING;
		$al26_type = 			$_POST["AL26_TYPE"];
		$sunet_path = 			$_POST["SUNET_PATH"];
		$sim_type = 			$_POST["SIM_TYPE"];
		$notes = 				$_POST["NOTES"];
		$init_abund_path = 		$_POST["INIT_ABUND_PATH"];
		$thermo_path = 			$_POST["THERMO_PATH"];
		$max_timesteps = 		$_POST["MAX_TIMESTEPS"];
		$include_weak = 		$_POST["INCLUDE_WEAK"];
		$include_screening = 	$_POST["INCLUDE_SCREENING"];
		$start_time = 			$_POST["START_TIME"];
		$stop_time = 			$_POST["STOP_TIME"];
		$reaction_rate = 		"";
		$reaction_rate_id = 	"";
		$lib_dir = 				$_POST["LIB_DIR"];
		
		$sim_workflow_index = $this->sim_workflow->create($name,
		                                                $username, 
														$sim_workflow_mode,
														$al26_type,
														$sunet_path,
														$sim_type,
														$notes,
														$init_abund_path,
														$thermo_path,
														$max_timesteps,
														$include_weak,
														$include_screening,
														$start_time,
														$stop_time,
														$reaction_rate,
														$reaction_rate_id,
														$lib_dir);
		
		$zones = 				$_POST["ZONES"];
		$zones_array = explode(",", $zones);
		
		$scale_factor = 		"";
		$params = 				"";
		
		$lib_array = LibDirUtils::get_lib_dir_libs_static($lib_dir, $this->username);
		
		$num_sims = min(count($lib_array), count($zones_array));
		
		for($i=0; $i<$num_sims; $i++){
		
			$lib = $lib_array[$i];
			$zones = $zones_array[$i];
			$library_path = "USER/$lib";
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->create($sim_workflow_index,
											$library_path,
											$zones,
											$scale_factor,
											$params);

		}
		
		print("SIM_WORKFLOW_INDEX=$sim_workflow_index\n");
	
	}
	
}

?>