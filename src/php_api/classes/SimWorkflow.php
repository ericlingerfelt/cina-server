<?php

class SimWorkflow{

	public $sim_workflow_index;
	public $name;
	public $username;
	public $sim_workflow_mode;
	public $al26_type;
	public $sunet_path;
	public $sim_type;
	public $notes;
	public $init_abund_path;
	public $thermo_path;
	public $max_timesteps;
	public $include_weak;
	public $include_screening;
	public $start_time;
	public $stop_time;
	public $reaction_rate;
	public $reaction_rate_id;
	public $lib_dir;
	public $creation_date;
	public $folder_type;
	
	public function create($name,
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
							$lib_dir){
		
		$query = "INSERT INTO sim_workflows (name, 
                                                username, 
												sim_workflow_mode,
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
												reaction_rate,
												reaction_rate_id,
												lib_dir, 
                                                creation_date, 
                                                folder_type
												) VALUES (
                                                :name,
												:username, 
												:sim_workflow_mode,
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
												:reaction_rate,
												:reaction_rate_id,
												:lib_dir, 
                                                NOW(), 
                                                'USER')";
		
		
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('name',                $name);
		$pdos->set_str_value('username',			$username);
		$pdos->set_str_value('sim_workflow_mode', 	$sim_workflow_mode);
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
		$pdos->set_str_value('reaction_rate', 		$reaction_rate);
		$pdos->set_str_value('reaction_rate_id', 	$reaction_rate_id);
		$pdos->set_str_value('lib_dir', 			$lib_dir);
		$pdos->execute();
		
		return $pdos->get_insert_id();
	}
	
	public function print_small(){
	    
	    print("FOLDER_TYPE=$this->folder_type\n");
	    print("NAME=$this->name\n");
	    print("SIM_WORKFLOW_INDEX=$this->sim_workflow_index\n");
	    
	}
	
	public function print_all2(){
	    
	    print("SIM_WORKFLOW_INDEX=$this->sim_workflow_index\n");
	    print("FOLDER_TYPE=$this->folder_type\n");
	    print("SIM_WORKFLOW_=$this->name\n");
	    print("CREATION_DATE=$this->creation_date\n");
	    print("USERNAME=$this->username\n");
	    print("SIM_WORKFLOW_MODE=$this->sim_workflow_mode\n");
	    print("AL26_TYPE=$this->al26_type\n");
	    print("SUNET_PATH=$this->sunet_path\n");
	    print("SIM_TYPE=$this->sim_type\n");
	    print("NOTES=$this->notes\n");
	    print("INIT_ABUND_PATH=$this->init_abund_path\n");
	    print("THERMO_PATH=$this->thermo_path\n");
	    print("MAX_TIMESTEPS=$this->max_timesteps\n");
	    print("INCLUDE_WEAK=$this->include_weak\n");
	    print("INCLUDE_SCREENING=$this->include_screening\n");
	    print("START_TIME=$this->start_time\n");
	    print("STOP_TIME=$this->stop_time\n");
	    print("REACTION_RATE=$this->reaction_rate\n");
	    print("REACTION_RATE_ID=$this->reaction_rate_id\n");
	    print("LIB_DIR=$this->lib_dir\n");
	    
	    $query = "SELECT sim_workflow_sim_index FROM sim_workflow_sims WHERE sim_workflow_index=:sim_workflow_index";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	    $pdos->execute();
	    
	    $scale_factors = "";
	    $params = "";
	    $library_path = "";
	    
	    while($row = $pdos->fetch_assoc()){
	        $sim_workflow_sim = new SimWorkflowSim();
	        $sim_workflow_sim->populate($row['sim_workflow_sim_index']);
	        $sim_workflow_sim->print_all();
	    }
	    
	}
	
	public function print_all(){
	
	   print("FOLDER_TYPE=$this->folder_type\n");
	   print("NAME=$this->name\n");
	   print("SIM_WORKFLOW_INDEX=$this->sim_workflow_index\n");
	   print("CREATION_DATE=$this->creation_date\n");
	   print("USERNAME=$this->username\n");
	   print("SIM_WORKFLOW_MODE=$this->sim_workflow_mode\n");
	   print("AL26_TYPE=$this->al26_type\n");
	   print("SUNET_PATH=$this->sunet_path\n");
	   print("SIM_TYPE=$this->sim_type\n");
	   print("NOTES=$this->notes\n");
	   print("INIT_ABUND_PATH=$this->init_abund_path\n");
	   print("THERMO_PATH=$this->thermo_path\n");
	   print("MAX_TIMESTEPS=$this->max_timesteps\n");
	   print("INCLUDE_WEAK=$this->include_weak\n");
	   print("INCLUDE_SCREENING=$this->include_screening\n");
	   print("START_TIME=$this->start_time\n");
	   print("STOP_TIME=$this->stop_time\n");
	   print("REACTION_RATE=$this->reaction_rate\n");
	   print("REACTION_RATE_ID=$this->reaction_rate_id\n");
	   print("LIB_DIR=$this->lib_dir\n");
	  
	   $query = "SELECT sim_workflow_sim_index FROM sim_workflow_sims WHERE sim_workflow_index=:sim_workflow_index";
	   $pdos = new PDOSUtil($query);
	   $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	   $pdos->execute();
	   
	   $scale_factors = "";
	   $params = "";
	   $library_path = "";
	   
	   while($row = $pdos->fetch_assoc()){
	       $sim_workflow_sim = new SimWorkflowSim();
	       $sim_workflow_sim->populate($row['sim_workflow_sim_index']);
	       $sim_workflow_sim->print_all();
	   }
	    
	}
	
	public function populate($sim_workflow_index){
		
		$query = "SELECT * FROM sim_workflows WHERE sim_workflow_index=:sim_workflow_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_index', $sim_workflow_index);
		$pdos->execute();
		
		$row = $pdos->fetch_assoc();
		
		$this->sim_workflow_index = 	$sim_workflow_index;
		$this->name = 		            $row["name"];
		$this->username = 				$row["username"];
		$this->sim_workflow_mode = 		$row["sim_workflow_mode"];
		$this->al26_type = 				$row["al26_type"];
		$this->sunet_path = 			$row["sunet_path"];
		$this->sim_type = 				$row["sim_type"];
		$this->notes = 					$row["notes"];
		$this->init_abund_path = 		$row["init_abund_path"];
		$this->thermo_path = 			$row["thermo_path"];
		$this->max_timesteps = 			$row["max_timesteps"];
		$this->include_weak = 			$row["include_weak"];
		$this->include_screening = 		$row["include_screening"];
		$this->start_time = 			$row["start_time"];
		$this->stop_time = 				$row["stop_time"];
		$this->reaction_rate = 			$row["reaction_rate"];
		$this->reaction_rate_id = 		$row["reaction_rate_id"];
		$this->lib_dir = 				$row["lib_dir"];
		$this->creation_date =          $row["creation_date"];
		$this->folder_type =            $row["folder_type"];
		
		return $this;
		
	}
	
	public function copy(){
	
	    $query = "SELECT * FROM sim_workflows WHERE sim_workflow_index=:sim_workflow_index AND username=:username and folder_type='USER'";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	    $pdos->set_int_value('username', $this->username);
	    $pdos->execute();
	    
	    $row = $pdos->fetch_assoc();
	    
	    $query = "INSERT INTO sim_workflows (name,
                                                folder_type,
                                                username,
												sim_workflow_mode,
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
												reaction_rate,
												reaction_rate_id,
												lib_dir, 
                                                creation_date
												) VALUES (
                                                :name,
                                                :folder_type,
												:username,
												:sim_workflow_mode,
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
												:reaction_rate,
												:reaction_rate_id,
												:lib_dir, 
                                                NOW())";
	    
	    
	    $pdos = new PDOSUtil($query);
	    $pdos->set_str_value('name',                $row['name']);
	    $pdos->set_str_value('folder_type',         'SHARED');
	    $pdos->set_str_value('username',			'');
	    $pdos->set_str_value('sim_workflow_mode', 	$row['sim_workflow_mode']);
	    $pdos->set_str_value('al26_type', 			$row['al26_type']);
	    $pdos->set_str_value('sunet_path', 			$row['sunet_path']);
	    $pdos->set_str_value('sim_type', 			$row['sim_type']);
	    $pdos->set_str_value('notes', 				$row['notes']);
	    $pdos->set_str_value('init_abund_path', 	$row['init_abund_path']);
	    $pdos->set_str_value('thermo_path', 		$row['thermo_path']);
	    $pdos->set_int_value('max_timesteps', 		$row['max_timesteps']);
	    $pdos->set_str_value('include_weak', 		$row['include_weak']);
	    $pdos->set_str_value('include_screening', 	$row['include_screening']);
	    $pdos->set_str_value('start_time', 			$row['start_time']);
	    $pdos->set_str_value('stop_time', 			$row['stop_time']);
	    $pdos->set_str_value('reaction_rate', 		$row['reaction_rate']);
	    $pdos->set_str_value('reaction_rate_id', 	$row['reaction_rate_id']);
	    $pdos->set_str_value('lib_dir', 			$row['lib_dir']);
	    $pdos->execute();
	    
	    $new_sim_workflow_index = $pdos->get_insert_id();
	    
	    // May have to copy all other items to shared as well. Worry about later. 
	    
	    $query = "SELECT sim_workflow_sim_index FROM sim_workflow_sims WHERE sim_workflow_index=:sim_workflow_index";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	    $pdos->execute();
	   
	    while($row = $pdos->fetch_assoc()){
	        $sim_workflow_sim = new SimWorkflowSim();
	        $sim_workflow_sim->populate($row['sim_workflow_sim_index']);
	        $sim_workflow_sim->copy($row['sim_workflow_sim_index'], $new_sim_workflow_index);
	    }
	    
	}
	
	public function erase(){
	   
	    $query = "DELETE FROM sim_workflows WHERE sim_workflow_index=:sim_workflow_index AND username=:username and folder_type='USER'";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	    $pdos->set_int_value('username', $this->username);
	    $pdos->execute();
	
	    $query = "DELETE FROM sim_workflow_sims WHERE sim_workflow_index=:sim_workflow_index";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index', $this->sim_workflow_index);
	    $pdos->execute();
	    
	}
	
	public function populate_sim_workflow_sim_array($sim_workflow_index){
	
		$sim_workflow_sim_array = array();
		
		$query = "SELECT sim_workflow_sim_index FROM sim_workflow_sims WHERE sim_workflow_index=:sim_workflow_index ORDER BY sim_workflow_sim_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_index', $sim_workflow_index);
		$pdos->execute();
		
		while($row = $pdos->fetch_assoc()){
			$sim_workflow_sim = new SimWorkflowSim();
			$sim_workflow_sim->populate($row["sim_workflow_sim_index"]);
			$sim_workflow_sim_array []= $sim_workflow_sim;
		}
		
		return $sim_workflow_sim_array;
	
	}
	
}

?>