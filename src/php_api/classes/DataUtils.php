<?php

class DataUtils{

	public static function get_dir_children($file_path){
	    $file_list = glob($file_path);
	    if(count($file_list) == 0){
	        return array();
	    }
		$dir_children = trim(ExecUtils::exec_with_output("/bin/ls $file_path"));
		if($dir_children==""){
			return array();
		}
		return explode(" ", $dir_children);
	}

}

?>