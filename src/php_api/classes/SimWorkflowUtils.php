<?php

class SimWorkflowUtils{
	
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
	
	public function get_sim_workflows(){

	    $folder_type = $_POST["FOLDER_TYPE"];
	    
	    if($folder_type=="ALL"){
	        $this->print_sim_workflows("PUBLIC");
	        $this->print_sim_workflows("SHARED");
	        $this->print_sim_workflows("USER");
	    }else{
	        $this->print_sim_workflows($folder_type);
	    }
	
	}
	
	public function in_use($name){
	    $query = "SELECT COUNT(*) FROM sim_workflows INNER JOIN sim_workflow_runs 
                    ON sim_workflows.sim_workflow_index = sim_workflow_runs.sim_workflow_index 
                    WHERE sim_workflow_runs.username=:username AND sim_workflows.name=:name";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_str_value('username', $this->username);
	    $pdos->set_str_value('name', $name);
	    $pdos->execute();
	    
	    if($pdos->has_rows()){
	        print("IN_USE=true\n");
	    }else{
	       print("IN_USE=false\n");
	    }
	}
	
	public function name_exists($name){
	    $query = "SELECT COUNT(*) FROM sim_workflows WHERE username=:username AND name=:name";
	    $pdos = new PDOSUtil($query);
	    $pdos->set_str_value('username', $this->username);
	    $pdos->set_str_value('name', $name);
	    $pdos->execute();
	    
	    if($pdos->has_rows()){
           print("NAME_EXISTS=true\n");
	    }else{
	       print("NAME_EXISTS=false\n");
	    }
	}
	
	private function print_sim_workflows($folder_type){
	    
	    if ($folder_type=="USER"){
    	    
    	    $query = "SELECT sim_workflow_index FROM sim_workflows WHERE username=:username AND folder_type='USER' AND name<>''";
    	    $pdos = new PDOSUtil($query);
    	    $pdos->set_str_value('username', $this->username);
    	    $pdos->execute();
	    
	    }else{
	    
	        $query = "SELECT sim_workflow_index FROM sim_workflows WHERE folder_type=:folder_type";
	        $pdos = new PDOSUtil($query);
	        $pdos->set_str_value('folder_type', $folder_type);
	        $pdos->execute();
	        
	    }
	    
	    while($row = $pdos->fetch_assoc()){
	        
	        $sim_workflow_index =  $row["sim_workflow_index"];
	        
	        $sim_workflow = new SimWorkflow();
	        $sim_workflow->populate($sim_workflow_index);
	        $sim_workflow->print_small();
	        
	    }
	}
	
	public function get_sim_workflow_info($sim_workflow_indices){
	    
	    $sim_workflow_indices_array = explode(",", $sim_workflow_indices);
	    
	    foreach($sim_workflow_indices_array as $sim_workflow_index){
	        
	        $sim_workflow = new SimWorkflow();
	        $sim_workflow->populate($sim_workflow_index);
	        $sim_workflow->print_all();
	        
	    }
	    
	}
	
	public function copy_sim_workflows_to_shared($sim_workflow_indices){
	    
	    $sim_workflow_indices_array = explode(",", $sim_workflow_indices);
	    
	    foreach($sim_workflow_indices_array as $sim_workflow_index){
	        
	        $sim_workflow = new SimWorkflow();
	        $sim_workflow->populate($sim_workflow_index);
	        $sim_workflow->copy();
	        
	    }
	    
	}
	
	public function erase_sim_workflows($sim_workflow_indices){
	
	    $sim_workflow_indices_array = explode(",", $sim_workflow_indices);
	    
	    foreach($sim_workflow_indices_array as $sim_workflow_index){
	        
	        $sim_workflow = new SimWorkflow();
	        $sim_workflow->populate($sim_workflow_index);
	        $sim_workflow->erase();
	        
	    }
	
	}
	
	public function get_sim_workflow_types(){

		$query = "SELECT * FROM sim_workflow_types";
		$pdos = new PDOSUtil($query);
		$pdos->execute();
	
		while($row = $pdos->fetch_assoc()){
		
			print("CATEGORY=".$row["category"]."\n");
			print("TYPE=".$row["type"]."\n");
			print("TYPE_DESC=".$row["type_desc"]."\n");
			print("SUNET_PATH=".$row["sunet_path"]."\n");
			print("SUNET_DESC=".$row["sunet_desc"]."\n");
			print("INIT_ABUND_PATH=".$row["init_abund_path"]."\n");
			print("INIT_ABUND_DESC=".$row["init_abund_desc"]."\n");
			print("THERMO_PATH=".$row["thermo_path"]."\n");
			print("THERMO_DESC=".$row["thermo_desc"]."\n");
			print("ZONES=".$row["zones"]."\n");
			print("START_TIME=".$row["start_time"]."\n");
			print("STOP_TIME=".$row["stop_time"]."\n");
		
		}
	
	}
	
	public function get_sens_network_isotopes(){
		
		$input_array = array();
		$input_array["SIM_WORKFLOW_RUN_INDEX"] = rand();
		$input_array["LIBRARY"] = 			$_POST["LIBRARY"];
		$input_array["TYPE"] = 				$_POST["TYPE"];
		$input_array["SUNET_PATH"] = 		$_POST["SUNET_PATH"];
		$output_array = $this->cgi_com->execute(CGIComActions::ELEMENT_SYNTHESIS_SETUP, $this->id, $this->username, $this->pw, $input_array);
		
		$dir = PHPUtils::$DATA_PATH."/tmp/".$input_array["SIM_WORKFLOW_RUN_INDEX"]."/em_syn/data/netsu";
		
		print("SENS_SIM_WORKFLOW_RUN_INDEX=".$input_array["SIM_WORKFLOW_RUN_INDEX"]."\n");
		
		if(file_exists($dir)){
		
			if($netsu=file_get_contents($dir)){
					
				$array = explode("\n", $netsu);
				$temp_isotope_array = array();
				$reaction_type = "";
				foreach($array as $netsu_row){
					$first_char = substr($netsu_row, 0, 1);
					if(preg_match("/[0-8]/", $first_char)){
						if(preg_match("/[1-8]/", $first_char)){
							$reaction_type = $first_char;
						}else{
							$isotopes_for_reaction_array = $this->get_reactant_isotopes($netsu_row, $reaction_type);
							switch($reaction_type){
								case 1:
								case 2:
								case 3:
									$temp_isotope_array[]=$isotopes_for_reaction_array[0];
									break;
								case 4:
								case 5:
								case 6:
								case 7:
									$temp_isotope_array[]=Reaction::getBiggestIsotope($isotopes_for_reaction_array[0]
																						, $isotopes_for_reaction_array[1]);
									break;
								case 8:
									$isotope = Reaction::getBiggestIsotope($isotopes_for_reaction_array[0]
																				, $isotopes_for_reaction_array[1]);
									$temp_isotope_array[]=Reaction::getBiggestIsotope($isotope
																							, $isotopes_for_reaction_array[2]);
									break;
							}
						}
					}
				}
					
				$isotope_string_array = array();
				print("ISOTOPES=");
				foreach($temp_isotope_array as $isotope){
					if(!in_array($isotope->toString(), $isotope_string_array)){
						$isotope_string_array[]=$isotope->toString();
						print($isotope->z.",".$isotope->a."\t");
					}
				}
			}
		}
	}
	
	private function get_reactant_isotopes($netsu_row, $reaction_type){
		$isotope_array = array();
		$num_reactants = 0;
		switch($reaction_type){
			case 1:
			case 2:
			case 3:
				$num_reactants = 1;
				break;
			case 4:
			case 5:
			case 6:
			case 7:
				$num_reactants = 2;
				break;
			case 8:
				$num_reactants = 3;
				break;
		}
		for($i=5; $i<=$num_reactants*5; $i+=5){
			$isotope_string = substr($netsu_row, $i, 5);
			$isotope_string = trim($isotope_string);
			if($isotope_string!=""){
				$isotope_array[]=$this->get_isotope_object($isotope_string);
			}
		}
		return $isotope_array;
	}
	
	public function get_sens_network_reactions(){
	
		print("ISOTOPE=".$_POST['ISOTOPE']."\n");
		$array = explode(",",$_POST['ISOTOPE']);
		$isotope = new Isotope();
		$isotope->z = $array[0];
		$isotope->a = $array[1];
		$dir = PHPUtils::$DATA_PATH."/tmp/".$_POST["SENS_SIM_WORKFLOW_RUN_INDEX"]."/em_syn/data/netsu";
	
		$fake_index = 0;
		if(file_exists($dir)){
			if($netsu=file_get_contents($dir)){
				$array = explode("\n", $netsu);
				$reaction_names = array();
				$reaction_type = "";
				foreach($array as $netsu_row){
					$first_char = substr($netsu_row, 0, 1);
					if(preg_match("/[0-8]/", $first_char)){
						if(preg_match("/[1-8]/", $first_char)){
							$reaction_type = $first_char;
						}else if($this->is_isotope_in_netsu_row($netsu_row, $isotope)){
							$reaction = new Reaction($reaction_type, $this->get_decay($netsu_row), $this->get_isotopes($netsu_row));
							if(!in_array($reaction->getReactionName(), $reaction_names)){
								$main_isotope =& $reaction->main_isotope;
								if($main_isotope->compareTo($isotope)==0){
									print("REACTION_INDEX=".$fake_index."\n");
									print("REACTION_NAME=".$reaction->getReactionName()."\n");
									print("DECAY=".$reaction->decay."\n");
									print("REACTION_TYPE=".$reaction->reaction_type."\n");
									$reaction_names[]=$reaction->getReactionName();
									$fake_index++;
								}
							}
						}
					}
				}
			}
		}
	}
	
	private function get_decay($netsu_row){
		$decay_array = array("bet+", "bet-", "ec", "bec", "bkmo", "btyk", "mo92");
		foreach($decay_array as $decay){
			if(strstr($netsu_row, $decay)){
				return $decay;
			}
		}
		return "";
	}
	
	private function get_isotopes($netsu_row){
		$isotope_array = array();
		for($i=5; $i<=30; $i+=5){
			$isotope_string = substr($netsu_row, $i, 5);
			$isotope_string = trim($isotope_string);
			if($isotope_string!=""){
				$isotope_array[]=$this->get_isotope_object($isotope_string);
			}
		}
		return $isotope_array;
	}
	
	private function get_isotope_object($isotope_string){
		$element = "";
		$a = "";
		for($i=0; $i<strlen($isotope_string); $i++){
			$char = substr($isotope_string, $i, 1);
			if(preg_match("/[0-9]/", $char)){
				$a.=$char;
			}else{
				$element.=$char;
			}
		}
	
		$element = trim($element);
		$a = trim($a);
		$z = "";
	
		if(substr($element, 0, 3)=="al-"){
			$z = 13;
			$a = 26;
			$isAlMinus = true;
		}else if(substr($element, 0, 3)=="al*"){
			$z = 13;
			$a = 26;
			$isAlStar = true;
		}else if($a==""){
			switch($element){
				case "n":
					$z = 0;
					$a = 1;
					break;
				case "p":
					$z = 1;
					$a = 1;
					break;
				case "d":
					$z = 1;
					$a = 2;
					break;
				case "t":
					$z = 1;
					$a = 3;
					break;
			}
		}else{
			$z = array_search(ucfirst($element), Isotope::$symbolString);
		}
		$isotope = new Isotope();
		$isotope->z = $z;
		$isotope->a = $a;
		if(isset($isAlMinus) && $isAlMinus){
			$isotope->isAlMinus = true;
		}else if(isset($isAlStar) && $isAlStar){
			$isotope->isAlStar = true;
		}
		return $isotope;
	}
	
	private function is_isotope_in_netsu_row($netsu_row, $isotope){
		if($isotope->z==13 && $isotope->a==26){
			return strstr($netsu_row, "al26")
			|| strstr($netsu_row, "al-6")
			|| strstr($netsu_row, "al*6");
		}else{
			return strstr($netsu_row, $isotope->toNetsuString());
		}
	}
	
}

?>
