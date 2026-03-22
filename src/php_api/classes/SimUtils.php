<?php

class SimUtils{

	public $username;

	public function __construct($username){

		$this->username = $username;
		
	}
	
	public function get_element_synthesis_edot_values(){
	
	    $path = $_POST["PATH"];
	    $zones = $_POST["ZONES"];
	    
	    $path_comps = explode("/", $path);
	    $folder_type = $path_comps[0];
	    $sim_name = $path_comps[1];
        
        if($folder_type == "SHARED"){
            $dir = PHPUtils::$DATA_PATH."/SHARED/em_sims/$sim_name";
        }else if($folder_type == "USER"){
            $dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/$sim_name";
        }

        $ev_file_array = DataUtils::get_dir_children("$dir/ev*");
        $ev_zone_str = "";
        
        if(count($ev_file_array)>0){
       
            if(count($ev_file_array)==1){
            
                $ev_file = "ev1";
                $ev_zone_path = "$dir/$ev_file";
                $ev_zone_contents = file_get_contents($ev_zone_path);
                $ev_zone_line_array = explode("\n", $ev_zone_contents);
                foreach($ev_zone_line_array as $line){
                    $value = trim($line);
                    if($value != ""){
                        $ev_zone_str.= "$value,";
                    }
                }
                $ev_zone_str = substr($ev_zone_str, 0, count($ev_zone_str)-2);
                print("EDOT_VALUES=$ev_zone_str\n");
                
            }else{
            
                foreach($ev_file_array as $ev_file){
                    
                    $zone_str = substr(basename($ev_file), 2);
                    $zone = intval($zone_str);
                    
                    if($zone == $zones){
                    
                        $ev_zone_contents = file_get_contents($ev_file);
                        $ev_zone_line_array = explode("\n", $ev_zone_contents);
                        foreach($ev_zone_line_array as $line){
                            $value = trim($line);
                            if($value != ""){
                                $ev_zone_str.= "$value,";
                            }
                        }
                        $ev_zone_str = substr($ev_zone_str, 0, count($ev_zone_str)-2);
                        print("EDOT_VALUES=$ev_zone_str\n");
                        break;
                        
                    }
                    
                }
            
            }
        
        }else{
        
            print("EDOT_VALUES=\n");
            
        }
	
	}
	
	public function get_sims(){
		
		$folder_type = $_POST["FOLDER_TYPE"];
		
		if($folder_type=="ALL"){
			$this->print_sims("SHARED");
			$this->print_sims("USER");
		}else{
			$this->print_sims($folder_type);
		}

	}
	
	private function print_sims($folder_type){
		
	    print("FOLDER_TYPE=$folder_type\n");
	    
	    if($folder_type == "USER"){
	        $query = "SELECT sim_name FROM sims WHERE folder_type=:folder_type AND username=:username";
	        $pdos = new PDOSUtil($query);
	        $pdos->set_str_value('folder_type', $folder_type);
	        $pdos->set_str_value('username', $this->username);
	    }else{
	        $query = "SELECT sim_name FROM sims WHERE folder_type=:folder_type";
	        $pdos = new PDOSUtil($query);
	        $pdos->set_str_value('folder_type', $folder_type);
	    }
		$pdos->execute();
		
		while($row = $pdos->fetch_assoc()){
		    $sim_name = $row["sim_name"];
			print("NAME=$sim_name\n");
		}
	}
	
	public function get_sim_info(){
		
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
		
		foreach($path_array as $path){
	
			print("PATH=$path\n");
	
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$sim_name = $path_comps[1];
	
			if($folder_type == "USER"){
				$query = "SELECT * FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name AND username=:username";
				$pdos = new PDOSUtil($query);
				$pdos->set_str_value('folder_type', $folder_type);
				$pdos->set_str_value('sim_name', $sim_name);
				$pdos->set_str_value('username', $this->username);
			}else{
				$query = "SELECT * FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name";
				$pdos = new PDOSUtil($query);
				$pdos->set_str_value('folder_type', $folder_type);
				$pdos->set_str_value('sim_name', $sim_name);
			}
		
			$pdos->execute();
			
			$row = $pdos->fetch_assoc();
			foreach($row as $key=>$value){
				print(strtoupper($key)."=".$this->replace_newlines(trim($value))."\n");
			}
		}
	}
	
	private function replace_newlines($str){
		if(preg_match("/\n/", $str)){
			return preg_replace("/\n/", "<br>", $str);
		}else{
			return $str;
		}
	}
	
	public function copy_sims_to_shared(){
		
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
	
		foreach($path_array as $path){
	
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$sim_name = $path_comps[1];
			
			if($folder_type != "USER"){
				continue;
			}
			 
			$dir_old = PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/".$sim_name;
			$dir_new = PHPUtils::$DATA_PATH."/SHARED/em_sims/".$sim_name;
			 
			if(!file_exists($dir_new)){
			    ExecUtils::exec("/bin/mkdir $dir_new");
			    ExecUtils::exec("/bin/chmod 775 $dir_new");
			}else{
			    ExecUtils::exec("/bin/rm -Rf $dir_new/*");
			}
			
			ExecUtils::exec("/bin/cp -r $dir_old/* $dir_new");
		
			$query = "SELECT * FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name AND username=:username";
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('folder_type',  "USER");
			$pdos->set_str_value('sim_name',     $sim_name);
			$pdos->set_str_value('username',     $this->username);
			$pdos->execute();
			
			$row = $pdos->fetch_assoc();
			 
			$sim_name = 			$row["sim_name"];
			$zones = 				$row["zones"];
			$library_path = 		$row["library_path"];
			$min_iso = 				$row["min_iso"];
			$max_iso = 				$row["max_iso"];
			$al26_type = 			$row["al26_type"];
			$sunet_path = 			$row["sunet_path"];
			$sim_type = 			$row["sim_type"];
			$notes = 				$row["notes"];
			$init_abund_path = 		$row["init_abund_path"];
			$thermo_path = 			$row["thermo_path"];
			$max_timesteps = 		$row["max_timesteps"];
			$include_weak = 		$row["include_weak"];
			$include_screening = 	$row["include_screening"];
			$start_time = 			$row["start_time"];
			$stop_time = 			$row["stop_time"];
			$scale_factor = 		$row["scale_factor"];
			$reaction_rate = 		$row["reaction_rate"];
			$params = 				$row["params"];
			
			$query2 = "INSERT INTO sims (username, sim_name, folder_type, zones, library_path, min_iso, max_iso, al26_type, sunet_path, sim_type, notes
						, init_abund_path, thermo_path, max_timesteps, include_weak, include_screening, start_time
						, stop_time, scale_factor, reaction_rate, params, creation_date) VALUES (";
			$query2 .= ":username,";
			$query2 .= ":sim_name,";
			$query2 .= ":folder_type,";
			$query2 .= ":zones,";
			$query2 .= ":library_path,";
			$query2 .= ":min_iso,";
			$query2 .= ":max_iso,";
			$query2 .= ":al26_type,";
			$query2 .= ":sunet_path,";
			$query2 .= ":sim_type,";
			$query2 .= ":notes,";
			$query2 .= ":init_abund_path,";
			$query2 .= ":thermo_path,";
			$query2 .= ":max_timesteps,";
			$query2 .= ":include_weak,";
			$query2 .= ":include_screening,";
			$query2 .= ":start_time,";
			$query2 .= ":stop_time,";
			$query2 .= ":scale_factor,";
			$query2 .= ":reaction_rate,";
			$query2 .= ":params,";
			$query2 .= "NOW())";
			
			$pdos2 = new PDOSUtil($query2);
			$pdos2->set_str_value('username', 			$this->username);
			$pdos2->set_str_value('sim_name', 			$sim_name);
			$pdos2->set_str_value('folder_type', 		"SHARED");
			$pdos2->set_str_value('zones', 				$zones);
			$pdos2->set_str_value('library_path',		$library_path);
			$pdos2->set_str_value('min_iso', 			$min_iso);
			$pdos2->set_str_value('max_iso',		 	$max_iso);
			$pdos2->set_str_value('al26_type', 			$al26_type);
			$pdos2->set_str_value('sunet_path', 		$sunet_path);
			$pdos2->set_str_value('sim_type', 			$sim_type);
			$pdos2->set_str_value('notes', 				$notes);
			$pdos2->set_str_value('init_abund_path', 	$init_abund_path);
			$pdos2->set_str_value('thermo_path',		$thermo_path);
			$pdos2->set_str_value('max_timesteps', 		$max_timesteps);
			$pdos2->set_str_value('include_weak', 		$include_weak);
			$pdos2->set_str_value('include_screening', 	$include_screening);
			$pdos2->set_str_value('start_time', 		$start_time);
			$pdos2->set_str_value('stop_time', 			$stop_time);
			$pdos2->set_str_value('scale_factor', 		$scale_factor);
			$pdos2->set_str_value('reaction_rate', 		$reaction_rate);
			$pdos2->set_str_value('params', 			$params);
			
			$pdos2->execute();
			
		}
		
	}
	
	public function erase_sims(){
	
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
	
		foreach($path_array as $path){
	
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$sim_name = $path_comps[1];
			
			if($folder_type != "USER"){
				continue;
			}

			ExecUtils::exec("rm -Rf ".PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/".$sim_name);
			$query = "DELETE FROM sims WHERE folder_type='USER' AND sim_name=:sim_name AND username=:username";
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('sim_name', $sim_name);
			$pdos->set_str_value('username', $this->username);
			$pdos->execute();

		}
	}
	
	public function get_element_synthesis_zones(){
	
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
		
		foreach($path_array as $path){
		
			if(trim($path)!=""){
		
				$path_comps = explode("/", $path);
				$folder_type = $path_comps[0];
				$sim_name = $path_comps[1];
				
				if($folder_type != "USER"){
					
    				$query = "SELECT zones FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name";
    				$pdos = new PDOSUtil($query);
    				$pdos->set_str_value('folder_type', $folder_type);
    				$pdos->set_str_value('sim_name', $sim_name);
    				$pdos->execute();
				
				}else{
				
				    $query = "SELECT zones FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name AND username=:username";
				    $pdos = new PDOSUtil($query);
				    $pdos->set_str_value('folder_type', $folder_type);
				    $pdos->set_str_value('sim_name', $sim_name);
				    $pdos->set_str_value('username', $this->username);
				    $pdos->execute();
				
				}
					
				$row = $pdos->fetch_assoc();
				$zones = $row["zones"];
					
				if($zones==""){
					$zones="1";
				}
					
				print("PATH=$path\n");
				print("ZONES=$zones\n");
		
			}
		}
		
	}
	
	public function get_total_weights(){
	
		$path = $_POST["PATH"];
	
		$path_comps = explode("/", $path);
		$folder_type = $path_comps[0];
		$sim_name = $path_comps[1];
	
		if($folder_type=="USER"){
			$weights_path = PHPUtils::$DATA_PATH."/USER/".$this->username."/em_sims/$sim_name/weights";
		}else{
			$weights_path = PHPUtils::$DATA_PATH."/$type/em_sims/$sim_name/weights";
		}
	
		$contents = file_get_contents($weights_path);
	
		$array = explode("\n", $contents);
		$total_weights = 0.0;
	
		foreach($array as $line){
	
			if(trim($line)!=""){
	
				$line_array = explode(" ", $line);
				$total_weights += floatval($line_array[1]);
	
			}
	
		}
	
		print("TOTAL_WEIGHTS=$total_weights\n");
		
	}
	
	public function is_sim_sens(){

		$path = $_POST["PATH"];
	
		$path_comps = explode("/", $path);
		$folder_type = $path_comps[0];
		$sim_name = $path_comps[1];

		if($folder_type == "USER"){
			$query = "SELECT * FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name AND username=:username";
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('folder_type', $folder_type);
			$pdos->set_str_value('sim_name', $sim_name);
			$pdos->set_str_value('username', $this->username);
		}else{
			$query = "SELECT * FROM sims WHERE folder_type=:folder_type AND sim_name=:sim_name";
			$pdos = new PDOSUtil($query);
			$pdos->set_str_value('folder_type', $folder_type);
			$pdos->set_str_value('sim_name', $sim_name);
		}
		
		$pdos->execute();
			
		$row = $pdos->fetch_assoc();
		
		print("PATH=$path\n");
		$scale_factor = $row["scale_factor"];
		$reaction_rate = $row["reaction_rate"];
		
		if($reaction_rate != ""){
			print("IS_SIM_SENS=true\n");
			print("SCALE_FACTOR=$scale_factor\n");
			print("REACTION_RATE=$reaction_rate\n");
		}else{
			print("IS_SIM_SENS=false\n");
		}
		
	}

}

?>