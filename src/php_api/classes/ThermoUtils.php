<?php

class ThermoUtils{

	public $username;

	public function __construct($username){

		$this->username = $username;
		
	}
	
	public function get_thermo_profile_sets(){
	
		$folder_type = $_POST["FOLDER_TYPE"];
		
		if($folder_type=="ALL"){
			$this->print_thermo_profile_sets("PUBLIC");
			$this->print_thermo_profile_sets("SHARED");
			$this->print_thermo_profile_sets("USER");
		}else{
		    $this->print_thermo_profile_sets($folder_type);
		}
		
	}
	
	private function print_thermo_profile_sets($folder_type){
		
	    if($folder_type == "PUBLIC"){
			$dir = PHPUtils::$DATA_PATH."/PUBLIC/thermo";
	    }else if($folder_type == "SHARED"){
			$dir = PHPUtils::$DATA_PATH."/SHARED/thermo";
	    }else if($folder_type == "USER"){
			$dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo";
		}
	
		print("FOLDER_TYPE=$folder_type\n");
		$array = DataUtils::get_dir_children($dir);
		foreach($array as $name){
		    if($name !=""){
			     print("NAME=$name\n");
		    }
		}
	}
	
	public function get_thermo_profile_set_info(){
	
		$paths = $_POST['PATHS'];
		$path_array = explode(",", $paths);
		
		foreach($path_array as $path){
		
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$thermo_profile_set_name = $path_comps[1];
			
			if($folder_type == "PUBLIC"){
				$thermo_path = PHPUtils::$DATA_PATH."/PUBLIC/thermo/$thermo_profile_set_name";
				$query = "SELECT thermo_desc FROM sim_workflow_types WHERE thermo_path=:thermo_path";
				$pdos = new PDOSUtil($query);
				$pdos->set_str_value('thermo_path', $path);
				$pdos->execute();
				$row = $pdos->fetch_assoc();
				$desc = $row["thermo_desc"];
			}else if($folder_type == "SHARED"){
				$thermo_path = PHPUtils::$DATA_PATH."/SHARED/thermo/$thermo_profile_set_name";
				$desc = file_get_contents("$thermo_path/desc");
			}else if($folder_type == "USER"){
				$thermo_path = PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo/$thermo_profile_set_name";
				$desc = file_get_contents("$thermo_path/desc");
			}

			$start_time = $this->get_start_time($thermo_path);
			$stop_time = $this->get_stop_time($thermo_path);
			$num_profiles = $this->get_num_profiles($thermo_path);
			$creation_date = date("Y-m-d H:i:s", filectime($thermo_path));
			
			print("PATH=$path\n");
			print("NAME=$thermo_profile_set_name\n");
			print("TYPE=$folder_type\n");
			print("DESC=$desc\n");
			print("START_TIME=$start_time\n");
			print("STOP_TIME=$stop_time\n");
			print("NUM_PROFILES=$num_profiles\n");
			print("CREATION_DATE=$creation_date\n");
				
		}
		
	}
	
	public function copy_thermo_profile_sets_to_shared(){
		
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
		
		foreach($path_array as $path){
		
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$thermo_profile_set_name = $path_comps[1];
				
			if($folder_type != "USER"){
				continue;
			}
		
			$dir_old = PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo/$thermo_profile_set_name";
			$dir_new = PHPUtils::$DATA_PATH."/SHARED/thermo/$thermo_profile_set_name";
		
			if(!file_exists($dir_new)){
			    ExecUtils::exec("/bin/mkdir $dir_new");
				ExecUtils::exec("/bin/chmod 775 $dir_new");
			}else{
			    ExecUtils::exec("/bin/rm -Rf $dir_new/*");
			}
			
			ExecUtils::exec("/bin/cp -r $dir_old/* $dir_new");
				
		}
	
	}
	
	public function erase_thermo_profile_sets(){
	
		$paths = $_POST["PATHS"];
		$path_array = explode(",", $paths);
		
		foreach($path_array as $path){
		
			$path_comps = explode("/", $path);
			$folder_type = $path_comps[0];
			$thermo_profile_set_name = $path_comps[1];
				
			if($folder_type != "USER"){
				continue;
			}
		
			ExecUtils::exec("rm -Rf ".PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo/".$thermo_profile_set_name);
		
		}
		
	}
	
	public function import_thermo_profile_set(){
	
		$name = $_POST["NAME"];
		$desc = $_POST["DESC"];
		
		$dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo/$name";
		$temp_file_name = "temp.zip";
		
		ExecUtils::exec("/bin/rm -Rf $dir");
		ExecUtils::exec("/bin/mkdir -p $dir");
		file_put_contents("$dir/desc", "");
		move_uploaded_file($_FILES["cinafile"]["tmp_name"], "$dir/$temp_file_name");
		
		ExecUtils::exec("cd $dir && /usr/bin/unzip $temp_file_name");
		ExecUtils::exec("/bin/rm -Rf $dir/$temp_file_name");
		ExecUtils::exec("/bin/chgrp apache -R $dir");
		ExecUtils::exec("/bin/chmod 777 -R $dir");
		
		$desc_file = PHPUtils::$DATA_PATH."/USER/".$this->username."/thermo/$name/desc";
		file_put_contents($desc_file, $desc);
		
		$desc = file_get_contents($desc_file);
		$start_time = $this->get_start_time($dir);
		$stop_time = $this->get_stop_time($dir);
		$num_profiles = $this->get_num_profiles($dir);
		$creation_date = date("Y-m-d H:i:s", filectime($dir));
		
		print("START_TIME=$start_time\n");
		print("STOP_TIME=$stop_time\n");
		print("NUM_PROFILES=$num_profiles\n");
		print("CREATION_DATE=$creation_date\n");
		
	}
	
	private function get_start_time($thermo_path){
		$thermo_profile_contents = file_get_contents($thermo_path."/1");
		$thermo_profile_contents_array = explode("\n", $thermo_profile_contents);
		$line = $thermo_profile_contents_array[1];
		$start_time = str_ireplace("D", "E", trim(substr($line, 11, 16)));
		return $start_time;
	}
	
	private function get_stop_time($thermo_path){
		$thermo_profile_contents = file_get_contents($thermo_path."/1");
		$thermo_profile_contents_array = explode("\n", $thermo_profile_contents);
		$line = $thermo_profile_contents_array[1];
		$stop_time = str_ireplace("D", "E", trim(substr($line, 37, 16)));
		return $stop_time;
	}
	
	private function get_num_profiles($thermo_dir){
		$file_counter = 0;
		$handle = opendir($thermo_dir);
		if ($handle !== false) {
			while (false !== ($file = readdir($handle))) {
				if ($file != "." && $file != ".." && substr($file, 0, 1) != '.' && ctype_digit($file)) {
					$file_counter++;
				}
			}
			closedir($handle);
		}
		$num_profiles = (int)$file_counter;
		return $num_profiles;
	}
	
}


?>