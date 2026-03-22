<?php

class SimWorkflowSim{

	public $sim_workflow_sim_index;
	public $sim_workflow_index;
	public $library_path;
	public $library;
	public $zones;
	public $scale_factor;
	public $params;
	
	public function create($sim_workflow_index, 
							$library_path, 
							$zones, 
							$scale_factor, 
							$params){
		
		$query = "INSERT INTO sim_workflow_sims (sim_workflow_index,
												library_path,
												zones,
												scale_factor,
												params
												) VALUES (
												:sim_workflow_index,
												:library_path,
												:zones,
												:scale_factor,
												:params)";
		
		
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_index',	$sim_workflow_index);
		$pdos->set_str_value('library_path', 		$library_path);
		$pdos->set_str_value('zones', 				$zones);
		$pdos->set_str_value('scale_factor', 		$scale_factor);
		$pdos->set_str_value('params', 				$params);
		$pdos->execute();
		
	}
	
	public function print_all(){
	    
	    print("SIM_WORKFLOW_SIM_INDEX=$this->sim_workflow_sim_index\n");
	    print("LIBRARY_PATH=$this->library_path\n");
	    print("ZONES=$this->zones\n");
	    print("SCALE_FACTOR=$this->scale_factor\n");
	    print("PARAMS=$this->params\n");
	
	}
	
	public function copy($sim_workflow_sim_index, $new_sim_workflow_index){
	
	    $query = "SELECT * FROM sim_workflow_sims WHERE sim_workflow_sim_index=:sim_workflow_sim_index";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_sim_index', $sim_workflow_sim_index);
	    $pdos->execute();
	    
	    $row = $pdos->fetch_assoc();
	    
	    $query = "INSERT INTO sim_workflow_sims (sim_workflow_index,
												library_path,
												zones,
												scale_factor,
												params
												) VALUES (
												:sim_workflow_index,
												:library_path,
												:zones,
												:scale_factor,
												:params)";
	    
	    
	    $pdos = new PDOSUtil($query);
	    $pdos->set_int_value('sim_workflow_index',	$new_sim_workflow_index);
	    $pdos->set_str_value('library_path', 		$row['library_path']);
	    $pdos->set_str_value('zones', 				$row['zones']);
	    $pdos->set_str_value('scale_factor', 		$row['scale_factor']);
	    $pdos->set_str_value('params', 				$row['params']);
	    $pdos->execute();
	
	}
	
	public function populate($sim_workflow_sim_index){
	
		$query = "SELECT * FROM sim_workflow_sims WHERE sim_workflow_sim_index=:sim_workflow_sim_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('sim_workflow_sim_index', $sim_workflow_sim_index);
		$pdos->execute();
		
		$row = $pdos->fetch_assoc();
		
		$this->sim_workflow_sim_index = $sim_workflow_sim_index;
		$this->sim_workflow_index = 	$row["sim_workflow_index"];
		$this->library_path = 			$row["library_path"];
		$this->zones = 					$row["zones"];
		$this->scale_factor = 			$row["scale_factor"];
		$this->params = 				$row["params"];
		
		$array = explode("/", $this->library_path);
		$this->library = $array[1];			
		
		return $this;
	
	}
	
}

?>
