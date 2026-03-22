<?php

class LibDirUtils{

	public $username;
	
	public function __construct($username){
	
		$this->username = $username;
	
	}
	
	public function erase_lib_dir(){
	
		$lib_dir = $_POST["LIB_DIR"];
		$library_dir = PHPUtils::$DATA_PATH."/USER/$this->username/rate_libs";
	
		ExecUtils::exec("/bin/rm -Rf $library_dir/$lib_dir"."_dir_*");
		
	}
	
	public function get_lib_dirs(){
		
	    //$folder_type = $_POST["FOLDER_TYPE"];
	    
	    //if($folder_type=="ALL"){
	        //$this->print_lib_dirs("PUBLIC");
	        //$this->print_lib_dirs("SHARED");
	        $this->print_lib_dirs("USER");
	    //}else{
	        //$this->print_lib_dirs($folder_type);
	    //}
	   
	}
	
	private function print_lib_dirs($folder_type){
	    
	    if($folder_type == "PUBLIC"){
	        //$dir = PHPUtils::$DATA_PATH."/PUBLIC/rate_libs";
	        return;
	    }else if($folder_type == "SHARED"){
	        //$dir = PHPUtils::$DATA_PATH."/SHARED/rate_libs";
	        return;
	    }else if($folder_type == "USER"){
	        $dir = PHPUtils::$DATA_PATH."/USER/".$this->username."/rate_libs";
	    }
	    
	    //print("FOLDER_TYPE=$folder_type\n");
	    $array = DataUtils::get_dir_children($dir);
	    $lib_dir_array = array();
	    foreach($array as $name){
	        
	        if($name != "" && strpos($name, "_dir_")!==false){
	            
	            $name_array = explode("_dir_", $name);
	            $lib_dir_name = $name_array[0];
	            if(!in_array($lib_dir_name, $lib_dir_array)){
	                $lib_dir_array []= $lib_dir_name;
	            }

	        }
	    }
	    foreach($lib_dir_array as $lib_dir_name){
	        print("LIB_DIR=$lib_dir_name\n");
	    }
	}
	
	public function get_lib_dir_info(){
	
	    $lib_dir = $_POST["LIB_DIR"];
	
	    print("NAME=".$lib_dir."\n");
	
		$lib_array = array();
		$rate_libs_path = PHPUtils::$DATA_PATH."/USER/".$this->username."/rate_libs";
		$array = DataUtils::get_dir_children($rate_libs_path);
		
		foreach($array as $library_name){
		    if(strpos($library_name, $lib_dir."_dir_")!==false){
				print("LIB=".$library_name."\n");
				$library_info_contents = file_get_contents(PHPUtils::$DATA_PATH."/USER/$this->username/rate_libs/".$library_name."/Library_Info");
				$library_info_contents_array = explode("\n", $library_info_contents);
				foreach($library_info_contents_array as $line){
					if(strpos($line, "Library Notes=")!==false){
						$sub_array = explode("=", $line);
						print("NOTES=".$sub_array[1]."\n");
					}
					if(strpos($line, "Creation Date=")!==false){
						$sub_array = explode("=", $line);
						print("CREATION_DATE=".$sub_array[1]."\n");
					}
				}
			}
		}
	
	}
	
	public function get_lib_dir_libs(){
	
	    $lib_dir = $_POST["LIB_DIR"];
	
		$lib_array = array();
		$rate_libs_path = PHPUtils::$DATA_PATH."/USER/".$this->username."/rate_libs";
		$array = DataUtils::get_dir_children($rate_libs_path);
		
		foreach($array as $library_name){
		    if(strpos($library_name, $lib_dir."_dir_")!==false){
				print("LIB_DIR_LIB=".$library_name."\n");
			}
		}
	
	}
	
	public static function get_lib_dir_libs_static($lib_dir, $username){
	
	    $lib_array = array();
	    $rate_libs_path = PHPUtils::$DATA_PATH."/USER/$username/rate_libs";
	    $array = DataUtils::get_dir_children($rate_libs_path);
		
		foreach($array as $library_name){
			if(strpos($library_name, $lib_dir."_dir_")!==false){
				$lib_array[]=$library_name;
			}
		}
		sort($lib_array);
		return $lib_array;
	
	}
	
	public function import_rate_library(){
	
		$library_name = $_POST["LIBRARY"];
		$library_notes = $_POST["NOTES"];
		$lib_dir_name = $_POST["LIB_DIR"];
	
		$library_path = PHPUtils::$DATA_PATH."/USER/".$this->username."/rate_libs/$library_name";
		ExecUtils::exec("/bin/rm -Rf $library_path");
		ExecUtils::exec("/bin/mkdir $library_path");
		
		move_uploaded_file($_FILES["cinafile"]["tmp_name"], "$library_path/$library_name.netsu");
		$this->generate_rate_library_from_netsu("$library_path/$library_name.netsu", $library_name, $library_notes, $library_path);
		
		ExecUtils::exec("/bin/rm -Rf $library_path/$library_name.netsu");
		ExecUtils::exec("/bin/chgrp apache -R $library_path");
		ExecUtils::exec("/bin/chmod 777 -R $library_path");
	
	}
	
	private function generate_rate_library_from_netsu($library_file, $library_name, $library_notes, $output_base_dir){

		//Split the file into an array of lines
		$reaction_input_array = explode("\n", file_get_contents($library_file));
	
		//An array holding the all elements lower cased
		$symbol_array_lowercase = array_map("strtolower", Isotope2::$symbol_array);
	
		//Filp the keys and values so an element returns an atomic number
		$symbol_array_flipped = array_flip($symbol_array_lowercase);
	
		//An array that will hold arrays of the three lines parsed for each REACLIB reaction
		$reaction_component_array = array();
	
		//A counter for cycling over three lines
		$reaction_component_counter = 0;
	
		//A counter for cycling over every line
		$counter = 0;
	
		//Fill the $reaction_component_array
		foreach($reaction_input_array as $line){
			$reaction_component_array[$reaction_component_counter][$counter] = $line;
			$counter++;
			if($counter==3){
				$counter = 0;
				$reaction_component_counter++;
			}
		}
	
		//Create a 4D array containing Reaction objects keyed on [z][a][reaction_type][rateid]
		$reaction_array = array();
	
		//A variable to hold the current reaction type
		$reaction_type = "";
	
		//The last Reaction object created
		$last_reaction = null;
	
		//Cycle over all three line componants parsed from REACLIB
		for($i=0; $i<count($reaction_component_array); $i++) {
	
			//Get the array holding the three lines for this reaction
			$comp_array = $reaction_component_array[$i];
	
			//If the array has less than three lines
			if(count($comp_array) < 3){
	
				//Go to the next array
				continue;
	
				//If any lines are blank
			}else if($comp_array[0]==""
					&& $comp_array[1]==""
					&& $comp_array[2]==""){
	
				//Go to the next array
				continue;
			}
	
			//If the first line can be cast to a number
			if(is_numeric(trim($comp_array[0]))){
	
				//Then its teh reaction type
				$reaction_type = trim($comp_array[0]);
	
				//Go to the next array
				continue;
			}
	
			//Create variables containing each line
			$reaction_line = $comp_array[0];
			$param_line_1 = $comp_array[1];
			$param_line_2 = $comp_array[2];
	
			//An array holding each item parsed from the first line
			$reaction_line_array = $this->parse_reaction_line($reaction_line);
	
			//An array of 7 parameters parsed from the second and third lines
			$param_array = $this->parse_param_lines($param_line_1, $param_line_2);
	
			//Get the biblio code
			$label = $reaction_line_array[6];
	
			//Set the resonant parameter to nr by default
			$resonant = "nr";
	
			//Change the resonant parameter to "r" only if "r" is found
			if($reaction_line_array[7]=="r"){
				$resonant = "r";
			}
	
			//Set the is_inverse flag to false by default
			$is_inverse = false;
	
			//Set is_inverse to true only if "v" is found
			if($reaction_line_array[8]=="v"){
				$is_inverse = true;
			}
	
			//Get the Q-value
			$q_value = $reaction_line_array[9];
	
			//Get an array of Isotope objects for this reaction
			$isotope_array = $this->get_isotope_array($reaction_line_array, $symbol_array_flipped);
	
			//Create a new Reaction object
			$reaction = new Reaction2($reaction_type, $label, $library_name, $isotope_array, $param_array, $is_inverse, $resonant, $q_value);
	
			//Get the main isotope
			$main_isotope = $reaction->main_isotope;
	
			//Raph said we only want reactions whoe main isotope is <= 85
			//He was wrong
			//if($main_isotope->z <= 85){
	
			//If the last Reaction object parsed is not null
			//and has the same rateid as teh current reaction
			if($last_reaction!=null && $last_reaction->rateid==$reaction->rateid){
	
				//Append the current reactions parameters to the last one
				$reaction->param_array = array_merge($last_reaction->param_array, $reaction->param_array);
	
				//Use the previous Reaction's Q-value. MSS decided to use the first one we found
				$reaction->q_value = $last_reaction->q_value;
	
				//Use the previous Reaction's biblio code. MSS decided to use the first one we found
				$reaction->source_label = $last_reaction->source_label;
	
				//Append the Reaction's resonant symbol with a comma. This format is used by CINA
				$reaction->resonant = $last_reaction->resonant.",".$reaction->resonant;
			}
	
			//Set the rateid
			$rateid = $reaction->rateid;
	
			//Add this Reaction to the 4D array
			$reaction_array[$main_isotope->z][$main_isotope->a][$reaction_type][$rateid]= $reaction;
	
			//Set the last reactio to the current one
			$last_reaction = $reaction;
			//}
	
		}
	
		//Create and write the Rate_Isotopes file
		$this->create_rate_isotopes_file($reaction_array, $output_base_dir);
	
		//Create and write the Library_Info file
		$this->create_library_info_file($output_base_dir, $library_notes);
	
		//Create and write the rid<z>_<a>.rate and iso<z>_<a>.rate files
		$this->create_rate_files($reaction_array, $output_base_dir);
	
	}
	
	/**
	 * Creates and writes to disk the rid<z>_<a>.rate and iso<z>_<a>.rate files.
	 *
	 * @param	$reaction_array	a 4D array containing Reaction objects keyed on [z][a][reaction_type][rateid]
	 * @param	$output_dir		the output directory
	 */
	private function create_rate_files($reaction_array, $output_dir){
		$z_keys = array_keys($reaction_array);
		asort($z_keys);
		foreach($z_keys as $z){
			mkdir("$output_dir/$z");
			$a_keys = array_keys($reaction_array[$z]);
			asort($a_keys);
			foreach($a_keys as $a){
				file_put_contents("$output_dir/$z/iso$z"."_"."$a.rate", $this->get_iso_file_contents($reaction_array[$z][$a]));
				file_put_contents("$output_dir/$z/rid$z"."_"."$a.rate", $this->get_rid_file_contents($reaction_array[$z][$a], "$output_dir/$z/iso$z"."_"."$a.rate"));
			}
		}
	}
	
	/**
	 * Creates and returns the contents of a rid<z>_<a>.rate file.
	 *
	 * @param	$array			a 2D array conatining Reaction objects keyed on [reaction_type][rateid]
	 * @param	$iso_file_path	the path to this file's associated iso<z>_<a>.rate file used to set the file pointer in ths rid<z>_<a>.rate file.
	 * @return	the contents of a rid<z>_<a>.rate file
	 */
	private function get_rid_file_contents($array, $iso_file_path){
		$str = "";
		$counter = 0;
		foreach($array as $subarray){
			$counter += count($subarray);
		}
		$str.= $counter."\n";
		$iso_file_contents = file_get_contents($iso_file_path);
		$last_position = 0;
		foreach($array as $subarray){
			foreach($subarray as $rateid => $reaction){
				$position = strpos($iso_file_contents, $rateid, $last_position);
				$pointer = sprintf("%9d", $position);
				$last_position = (int)$position + strlen($rateid);
				$str.= $pointer.$rateid."\n";
			}
		}
		return $str;
	}
	
	/**
	 * Creates and returns the contents of a iso<z>_<a>.rate file.
	 *
	 * @param	$array	a 2D array containing Reaction objects keyed on [reaction_type][rateid]
	 * @return	the contents of a iso<z>_<a>.rate file
	 */
	private function get_iso_file_contents($array){
		$str = "";
		foreach($array as $subarray){
			foreach($subarray as $rateid => $reaction){
				$str.= $rateid."\n";
				$str.= "Reaction String = ".$reaction->reaction_name."\n";
				$str.= "Reaction Type = ".$reaction->reaction_type;
				if($reaction->is_inverse){
					$str.= ",v";
				}
				if($reaction->source_label=="bet-"
						|| $reaction->source_label=="bet+"
						|| $reaction->source_label=="ec"){
					$str.= ",".$reaction->source_label;
				}
				$str.= "\n";
				$str.= "Biblio Code = ".$reaction->source_label."\n";
				$str.= "Q-value = ".$reaction->q_value."\n";
				$str.= "Number of Reactants = ".$reaction->num_reactants."\n";
				$str.= "Number of Products = ".$reaction->num_products."\n";
				$str.= "Resonant Components = ".$reaction->resonant."\n";
				$str.= "Number of Parameters = ".count($reaction->param_array)."\n";
				$str.= "Parameters = ".$reaction->param_array[0];
				for($i=1; $i<count($reaction->param_array); $i++){
					$str.= ",".$reaction->param_array[$i];
				}
				$str.= "\n";
				$str.= "Creation Date = ".date("m/d/Y H:i:s")."\n";
			}
		}
		return $str;
	}
	
	/**
	 * Creates and writes the Library_Info file.
	 * @param	$output_dir		the output directory
	 */
	private function create_library_info_file($output_dir, $library_notes){
		$str = "";
		$str.= "Library Notes=$library_notes\n";
		$str.= "Creation Date=".date("m/d/Y H:i:s")."\n";
		$str.= "Library Recipe=This library was imported by nucastrodata.org coordinator.\n";
		$str.= "All Inverses Present=YES\n";
		file_put_contents("$output_dir/Library_Info", $str);
	}
	
	/**
	 * Creates and writes the Rate_Isotopes file.
	 *
	 * @param	$reaction_array	an array of Reaction objects
	 * @param	$output_dir		the output directory
	 */
	private function create_rate_isotopes_file($reaction_array, $output_dir){
		$z_keys = array_keys($reaction_array);
		asort($z_keys);
		$str = "";
		foreach($z_keys as $z){
			$a_keys = array_keys($reaction_array[$z]);
			asort($a_keys);
			$str.= "$z=";
			foreach($a_keys as $a){
				$str.= "$a\t";
			}
			$str.= "\n";
		}
		file_put_contents("$output_dir/Rate_Isotopes", $str);
	}
	
	/**
	 * Returns an array of Isotope objects for a reaction.
	 *
	 * @param	$array	an array parsed with the parse_reaction_line method
	 * @return	an array of Isotope objects for a reaction
	 */
	private function get_isotope_array($array, $symbol_array_flipped){
		$isotope_array = array();
		for($i=0; $i<6; $i++){
			if($array[$i]!=""){
				$z_a_array = $this->get_isotope_z_a_array($array[$i], $symbol_array_flipped);
				$isotope = new Isotope2();
				$isotope->z = $z_a_array["z"];
				$isotope->a = $z_a_array["a"];
				$isotope->is_alminus = 0;
				if($array[$i]=="al-6"){
					$isotope->is_alminus = 1;
				}
				$isotope->is_alstar = 0;
				if($array[$i]=="al*6"){
					$isotope->is_alstar = 1;
				}
				$isotope_array []= $isotope;
			}
		}
		return $isotope_array;
	}
	
	/**
	 * Returns an associative array keyed on z and a for those
	 * values as parsed from a REACLIB isotope string.
	 *
	 * @param	$symbol		a REACLIB isotope string
	 * @return	an associative array keyed on z and a for those values
	 */
	private function get_isotope_z_a_array($symbol, $symbol_array_flipped){
	
		$element = "";
		$z = "";
		$a = "";
	
		if($symbol=="al*6" || $symbol=="al-6"){
	
			$a = "26";
			$z = "13";
	
		}else{
	
			$array = str_split($symbol);
			foreach($array as $char){
				if(is_numeric($char)){
					$a.=$char;
				}else{
					$element.=$char;
				}
			}
	
			if($a==""){
				if($element=="n"){
					$a = "1";
					$z = "0";
				}else if($element=="p"){
					$a = "1";
					$z = "1";
				}else if($element=="d"){
					$a = "2";
					$z = "1";
				}else if($element=="t"){
					$a = "3";
					$z = "1";
				}
			}else{
				$z = $this->get_z_from_element($element, $symbol_array_flipped);
			}
		}
	
		$z_a_array = array();
		$z_a_array["z"] = $z;
		$z_a_array["a"] = $a;
		return $z_a_array;
	}
	
	/**
	 * Returns the atomic number for a lower case element name.
	 *
	 * @param	$element	a lower case element name
	 * @return	the atomic number
	 */
	private function get_z_from_element($element, $symbol_array_flipped){
		return $symbol_array_flipped[$element];
	}
	
	/**
	 * Parses the first line into an array of 10 items
	 *
	 * @param	$line	the first line
	 * @return	an array of 10 items
	 */
	private function parse_reaction_line($line){
		$array = array();
		$array[0] = trim(substr($line, 5, 5));
		$array[1] = trim(substr($line, 10, 5));
		$array[2] = trim(substr($line, 15, 5));
		$array[3] = trim(substr($line, 20, 5));
		$array[4] = trim(substr($line, 25, 5));
		$array[5] = trim(substr($line, 30, 5));
		$array[6] = trim(substr($line, 43, 4));
		$array[7] = trim(substr($line, 47, 1));
		$array[8] = trim(substr($line, 48, 1));
		$array[9] = trim(substr($line, 52, 12));
		return $array;
	}
	
	/**
	 * Parses the second and third lines into an array of 7 parameters.
	 *
	 * @param	$line1	the second line
	 * @param	$line2 	the third line
	 * @return	an array of 7 parameters
	 */
	private function parse_param_lines($line1, $line2){
		$array = array();
		$array[0] = trim(substr($line1,  0, 13));
		$array[1] = trim(substr($line1, 13, 13));
		$array[2] = trim(substr($line1, 26, 13));
		$array[3] = trim(substr($line1, 39, 13));
		$array[4] = trim(substr($line2,  0, 13));
		$array[5] = trim(substr($line2, 13, 13));
		$array[6] = trim(substr($line2, 26, 13));
		return $array;
	}

}


?>