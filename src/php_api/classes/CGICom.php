<?php

class CGICom{

	public $header;
	public $id;
	public $action;
	public $user;
	public $pw;
	public $sim_workflow_run_index;
	public $library;
	public $isotope;
	public $type_database;
	public $rates;
	public $properties;
	public $src_lib;
	public $dest_lib;
	public $dest_group;
	public $chk_temp_behavior;
	public $chk_overflow;
	public $chk_inverse;
	public $del_src_lib;
	public $min_isotope;
	public $max_isotope;
	public $type;
	public $sunet_path;
	public $notes;
	public $init_abund_path;
	public $thermo_path;
	public $max_timesteps;
	public $include_weak;
	public $include_screening;
	public $start_time;
	public $stop_time;
	public $zones;
	public $make_inverse;
	
	public $input_array;
	public $output_array;
	public $cgi_input_array;
	public $cgi_output;
	
	public function __construct(){
		$this->initialize();
	}
	
	private function initialize(){
		
		$this->header = "2.8";
		$this->id = "";
		$this->action = "";
		$this->user = "";
		$this->pw = "";
		$this->sim_workflow_run_index = "";
		$this->library = "";
		$this->isotope = "";
		$this->type_database = "";
		$this->rates = "";
		$this->properties = "";
		$this->src_lib = "";
		$this->dest_lib = "";
		$this->dest_group = "";
		$this->chk_temp_behavior = "NO";
		$this->chk_overflow = "NO";
		$this->chk_inverse = "NO";
		$this->del_src_lib = "";
		$this->min_isotope = "";
		$this->max_isotope = "";
		$this->type = "";
		$this->sunet_path = "";
		$this->notes = "";
		$this->init_abund_path = "";
		$this->thermo_path = "";
		$this->max_timesteps = "";
		$this->include_weak = "";
		$this->include_screening = "";
		$this->start_time = "";
		$this->stop_time = "";
		$this->zones = "";
		$this->make_inverse = "NO";
		
		$this->input_array = array();
		$this->output_array = array();
		$this->cgi_input_array = array();
		$this->cgi_output = "";
		
	}
	
	public function execute($action, $id, $user, $pw, $input_array){
	
	    $this->initialize();
	    
		$this->action = 		$action;
		$this->id = 			$id;
		$this->user = 			$user;
		$this->pw = 			$pw;
		$this->input_array = 	$input_array;
		$this->output_array = 	array();
		
		$this->cgi_input_array = array();
		$this->cgi_input_array["HEADER"] = 	$this->header;
		$this->cgi_input_array["ID"] = 		$this->id;
		$this->cgi_input_array["ACTION"] = 	$this->action;
		$this->cgi_input_array["USER"] = 	$this->user;
		$this->cgi_input_array["PW"] = 		$this->pw;
		
		switch($action){
			case CGIComActions::GET_RATE_LIST:
				$this->execute_GET_RATE_LIST();
				break;
			case CGIComActions::GET_RATE_INFO:
				$this->execute_GET_RATE_INFO();
				break;
			case CGIComActions::MODIFY_RATE_LIBRARY:
				$this->execute_MODIFY_RATE_LIBRARY();
				break;
			case CGIComActions::MODIFY_RATES:
				$this->execute_MODIFY_RATES();
				break;
			case CGIComActions::ELEMENT_SYNTHESIS_SETUP:
				$this->execute_ELEMENT_SYNTHESIS_SETUP();
				break;
			case CGIComActions::SYNTHESIZE_ELEMENTS:
				$this->execute_SYNTHESIZE_ELEMENTS();
				break;
			case CGIComActions::ELEMENT_SYNTHESIS_UPDATE:
				$this->execute_ELEMENT_SYNTHESIS_UPDATE();
				break;
			case CGIComActions::SAVE_ELEMENT_SYNTHESIS_RUN:
				$this->execute_SAVE_ELEMENT_SYNTHESIS_RUN();
				break;
			
		}
		
		$this->check_for_error();
		
		switch($action){
			case CGIComActions::GET_RATE_LIST:
				$this->parse_GET_RATE_LIST();
				break;
			case CGIComActions::GET_RATE_INFO:
				$this->parse_GET_RATE_INFO();
				break;
			case CGIComActions::MODIFY_RATE_LIBRARY:
				$this->parse_MODIFY_RATE_LIBRARY();
				break;
			case CGIComActions::MODIFY_RATES:
				$this->parse_MODIFY_RATES();
				break;
			case CGIComActions::ELEMENT_SYNTHESIS_SETUP:
				$this->parse_ELEMENT_SYNTHESIS_SETUP();
				break;
			case CGIComActions::SYNTHESIZE_ELEMENTS:
				$this->parse_SYNTHESIZE_ELEMENTS();
				break;
			case CGIComActions::ELEMENT_SYNTHESIS_UPDATE:
				$this->parse_ELEMENT_SYNTHESIS_UPDATE();
				break;
			case CGIComActions::SAVE_ELEMENT_SYNTHESIS_RUN:
				$this->parse_SAVE_ELEMENT_SYNTHESIS_RUN();
				break;
					
		}
		
		return $this->output_array;
		
	}
	
	private function check_for_error(){
		
		if(substr($this->cgi_output, 0, 6) == "ERROR="){
			$error = substr($this->cgi_output, 6);
			Error::return_error($error, ErrorOutputType::PRIVATE_OUTPUT);
		}
		
	}
	
	private function execute_GET_RATE_LIST(){
	
		$this->library = 			$this->input_array["LIBRARY"];
		$this->isotope = 			$this->input_array["ISOTOPE"];
		
		$this->cgi_input_array["LIBRARY"] = 	$this->library;
		$this->cgi_input_array["ISOTOPE"] = 	$this->isotope;
		$this->cgi_input_array["TYPE"] = 		$this->type_database;
		
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
	
	}
	
	private function execute_GET_RATE_INFO(){
	
		$this->rates = 				$this->input_array["RATES"];
		
		$this->cgi_input_array["RATES"] = 		$this->rates;
		$this->cgi_input_array["PROPERTIES"] = 	$this->properties;
		
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
		
	}
	
	private function execute_MODIFY_RATE_LIBRARY(){
	
		$this->src_lib = 			$this->input_array["SRC_LIB"];
		$this->dest_lib = 			$this->input_array["DEST_LIB"];
		$this->dest_group = 		$this->input_array["DEST_GROUP"];
		$this->del_src_lib = 		$this->input_array["DEL_SRC_LIB"];
		
		$this->cgi_input_array["SRC_LIB"] = 			$this->src_lib;
		$this->cgi_input_array["DEST_LIB"] = 			$this->dest_lib;
		$this->cgi_input_array["DEST_GROUP"] = 			$this->dest_group;
		$this->cgi_input_array["CHK_TEMP_BEHAVIOR"] = 	$this->chk_temp_behavior;
		$this->cgi_input_array["CHK_OVERFLOW"] = 		$this->chk_overflow;
		$this->cgi_input_array["CHK_INVERSE"] = 		$this->chk_inverse;
		$this->cgi_input_array["DEL_SRC_LIB"] = 		$this->del_src_lib;
		
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
		
	}
	
	private function execute_MODIFY_RATES(){
	
		$this->dest_lib = 			$this->input_array["DEST_LIB"];
		$this->properties = 		$this->input_array["PROPERTIES"];
	
		$this->cgi_input_array["RATES"] = 			$this->rates;
		$this->cgi_input_array["DEST_LIB"] = 			$this->dest_lib;
		$this->cgi_input_array["CHK_TEMP_BEHAVIOR"] = $this->chk_temp_behavior;
		$this->cgi_input_array["CHK_OVERFLOW"] = 		$this->chk_overflow;
		$this->cgi_input_array["CHK_INVERSE"] = 		$this->chk_inverse;
		$this->cgi_input_array["PROPERTIES"] = 		$this->properties;
		$this->cgi_input_array["MAKE_INVERSE"] = 		$this->make_inverse;
	
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
	
	}
	
	private function execute_ELEMENT_SYNTHESIS_SETUP(){
		
	    $this->sim_workflow_run_index = $this->input_array["SIM_WORKFLOW_RUN_INDEX"];
		$this->library = 			$this->input_array["LIBRARY"];
		$this->sunet_path = 		$this->input_array["SUNET_PATH"];
		$this->type = 				$this->input_array["TYPE"];
		
		$this->cgi_input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
		$this->cgi_input_array["LIBRARY"] = 		$this->library;
		$this->cgi_input_array["MIN_ISOTOPE"] = 	$this->min_isotope;
		$this->cgi_input_array["MAX_ISOTOPE"] = 	$this->max_isotope;
		$this->cgi_input_array["TYPE"] = 			$this->type;
		$this->cgi_input_array["SUNET_PATH"] = 	$this->sunet_path;
		
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
	
	}
	
	private function execute_SYNTHESIZE_ELEMENTS(){
		
	    $this->sim_workflow_run_index = $this->input_array["SIM_WORKFLOW_RUN_INDEX"];
		$this->notes = 				$this->input_array["NOTES"];
		$this->init_abund_path = 	$this->input_array["INIT_ABUND_PATH"];
		$this->thermo_path = 		$this->input_array["THERMO_PATH"];
		$this->max_timesteps = 		$this->input_array["MAX_TIMESTEPS"];
		$this->include_weak = 		$this->input_array["INCLUDE_WEAK"];
		$this->include_screening = 	$this->input_array["INCLUDE_SCREENING"];
		$this->start_time = 		$this->input_array["START_TIME"];
		$this->stop_time = 			$this->input_array["STOP_TIME"];
		$this->zones = 				$this->input_array["ZONES"];
		
		$this->cgi_input_array["SIM_WORKFLOW_RUN_INDEX"] =   $this->sim_workflow_run_index;
		$this->cgi_input_array["NOTES"] = 			         $this->notes;
		$this->cgi_input_array["INIT_ABUND_PATH"] = 	     $this->init_abund_path;
		$this->cgi_input_array["THERMO_PATH"] = 		     $this->thermo_path;
		$this->cgi_input_array["MAX_TIMESTEPS"] = 	         $this->max_timesteps;
		$this->cgi_input_array["INCLUDE_WEAK"] = 		$this->include_weak;
		$this->cgi_input_array["INCLUDE_SCREENING"] = $this->include_screening;
		$this->cgi_input_array["START_TIME"] = 		$this->start_time;
		$this->cgi_input_array["STOP_TIME"] = 		$this->stop_time;
		$this->cgi_input_array["ZONES"] = 			$this->zones;
		
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
	
	}
	
	private function execute_ELEMENT_SYNTHESIS_UPDATE(){
	
	    $this->sim_workflow_run_index = $this->input_array["SIM_WORKFLOW_RUN_INDEX"];
	    
	    $this->cgi_input_array["SIM_WORKFLOW_RUN_INDEX"] = $this->sim_workflow_run_index;
	    
		$this->cgi_output = CGIUtils::execute_cgi_call($this->cgi_input_array);
		
	}
	
	private function parse_GET_RATE_LIST(){
		
		$this->output_array = explode("\n", $this->cgi_output);
		
	}
	
	private function parse_GET_RATE_INFO(){
		$position = strpos($this->cgi_output, "Reaction String");
		$this->cgi_output = substr($this->cgi_output, $position);
		$this->output_array["PROPERTIES"] = $this->cgi_output;
		
	}
	
	private function parse_MODIFY_RATE_LIBRARY(){
		
		$array = explode("\n", $this->cgi_output);
		foreach($array as $line){
			if($line!="" && strpos($line, "=") !== FALSE){
				$subarray = explode("=", $line);
				$this->output_array[$subarray[0]] = $subarray[1];
			}
		}
		
	}
	
	private function parse_MODIFY_RATES(){
	
		$array = explode("\n", $this->cgi_output);
		foreach($array as $line){
			if($line!="" && strpos($line, "=") !== FALSE){
				$subarray = explode("=", $line);
				$this->output_array[$subarray[0]] = $subarray[1];
			}
		}
	
	}
	
	private function parse_ELEMENT_SYNTHESIS_SETUP(){
	    
		$array = explode("\n", $this->cgi_output);
		foreach($array as $line){
			if($line!="" && strpos($line, "=") !== FALSE){
				$subarray = explode("=", $line);
				$this->output_array[$subarray[0]] = $subarray[1];
			}
		}
		
		if(array_key_exists("SETUP", $this->output_array) && $this->output_array["SETUP"] == "FAILURE"){
			Error::return_error($this->cgi_output, ErrorOutputType::PRIVATE_OUTPUT);
		}
		
	}
	
	private function parse_SYNTHESIZE_ELEMENTS(){
		
		$array = explode("\n", $this->cgi_output);
		foreach($array as $line){
			if($line!="" && strpos($line, "=") !== FALSE){
				$subarray = explode("=", $line);
				$this->output_array[$subarray[0]] = $subarray[1];
			}
		}
		
	}
	
	private function parse_ELEMENT_SYNTHESIS_UPDATE(){
		
		$array = explode("\n", $this->cgi_output);
		foreach($array as $line){
			if($line!="" && strpos($line, "=") !== FALSE){
				if(strpos($line, "TEXT=") !== FALSE){
					$text_contents = substr($line, 5, strlen($line));
					$this->output_array["TEXT"] = $text_contents;
				}else{
					$subarray = explode("=", $line);
					$this->output_array[$subarray[0]] = $subarray[1];
				}
			}
		}
		
	}
	
}

?>
