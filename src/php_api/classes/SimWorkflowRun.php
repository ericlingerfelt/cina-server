<?php

class SimWorkflowRun{

    public $sim_workflow_run_index;
	public $sim_workflow_index;
	public $name;
	public $current_lib;
	public $current_zone;
	public $current_scale_factor;
	public $current_status;
	public $current_text;
	public $start_date;
	public $end_date;
	public $error_log_index;
	public $process_id;
	public $username;
	
	public function create($sim_workflow_index, $username){
		
		$query = "INSERT INTO sim_workflow_runs (sim_workflow_index, username) VALUES (:sim_workflow_index, :username)";
		
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_index',	 $sim_workflow_index);
		$pdos->set_str_value('username',	         $username);
		$pdos->execute();
		
		$sim_workflow_run_index = $pdos->get_insert_id();
		
		print("SIM_WORKFLOW_RUN_INDEX=$sim_workflow_run_index\n");
	}
	
	public function name($sim_workflow_run_index, $name){
	    
	    $query = "UPDATE sim_workflow_runs SET name=:name WHERE sim_workflow_run_index=:sim_workflow_run_index";
	    
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_run_index',	  $sim_workflow_run_index);
	    $pdos->set_str_value('name',	                  $name);
	    $pdos->execute();

	}
	
	public function populate($sim_workflow_run_index){
		
		$query = "SELECT * FROM sim_workflow_runs WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_run_index', $sim_workflow_run_index);
		$pdos->execute();
		
		$row = $pdos->fetch_assoc();
		
		$this->sim_workflow_run_index = $sim_workflow_run_index;
		$this->sim_workflow_index = 	$row["sim_workflow_index"];
		$this->name = 			        $row["name"];
		$this->username = 			    $row["username"];
		$this->current_lib = 			$row["current_lib"];
		$this->current_zone = 			$row["current_zone"];
		$this->current_scale_factor =	$row["current_scale_factor"];
		$this->current_status =			$row["current_status"];
		$this->current_text =			$row["current_text"];
		$this->start_date =				$row["start_date"];
		$this->end_date =				$row["end_date"];
		$this->error_log_index =		$row["error_log_index"];
		$this->process_id =				$row["process_id"];
		
		return $this;
		
	}
	
	public function print_all(){
	    
	    print("NAME=$this->name\n");
	    print("SIM_WORKFLOW_RUN_INDEX=$this->sim_workflow_run_index\n");
	    print("USERNAME=$this->username\n");
	    print("CURRENT_LIB=$this->current_lib\n");
	    print("CURRENT_ZONE=$this->current_zone\n");
	    print("CURRENT_SCALE_FACTOR=$this->current_scale_factor\n");
	    print("CURRENT_STATUS=$this->current_status\n");
	    print("CURRENT_TEXT=$this->current_text\n");
	    print("START_DATE=$this->start_date\n");
	    print("END_DATE=$this->end_date\n");
	    print("ERROR_LOG_INDEX=$this->error_log_index\n");
	    print("PROCESS_ID=$this->process_id\n");
	    
	}
	
	public function update_current_lib($current_lib){
	
		$query = "UPDATE sim_workflow_runs SET current_lib=:current_lib WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('current_lib', $current_lib);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function update_current_zone($current_zone){
	
		$query = "UPDATE sim_workflow_runs SET current_zone=:current_zone WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('current_zone', $current_zone);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function update_current_scale_factor($current_scale_factor){
	
		$query = "UPDATE sim_workflow_runs SET current_scale_factor=:current_scale_factor WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('current_scale_factor', $current_scale_factor);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function update_current_status($current_status){
	
		$query = "UPDATE sim_workflow_runs SET current_status=:current_status WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('current_status', $current_status);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function update_current_text($current_text){
	
		$query = "UPDATE sim_workflow_runs SET current_text=:current_text WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('current_text', $current_text);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function set_start_date(){

		$query = "UPDATE sim_workflow_runs SET start_date=NOW() WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
		
	}
	
	public function set_end_date(){
	
		$query = "UPDATE sim_workflow_runs SET end_date=NOW() WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function set_process_id($process_id){
	
		$query = "UPDATE sim_workflow_runs SET process_id=:process_id WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('process_id', $process_id);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function set_error_log_index($error_log_index){
	
		$query = "UPDATE sim_workflow_runs SET error_log_index=:error_log_index WHERE sim_workflow_run_index=:sim_workflow_run_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('error_log_index', $error_log_index);
		$pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
		$pdos->execute();
	
	}
	
	public function erase(){
	    
	    $query = "DELETE FROM sim_workflow_runs WHERE sim_workflow_run_index=:sim_workflow_run_index";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_run_index', $this->sim_workflow_run_index);
	    $pdos->execute();
	
	}
	
	public function kill(){
	
		system("/bin/kill " . $this->process_id);
		
		$em_syn_temp_dir = PHPUtils::$DATA_PATH."/tmp/".$this->sim_workflow_run_index."/em_syn";
		system("rm -Rf $em_syn_temp_dir/*");
	
		$this->erase();
		
	}
	
}

?>