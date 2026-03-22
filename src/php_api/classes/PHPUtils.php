<?php

class PHPUtils{
	
	public static $IS_DEV = false;
	
	public static $DB_PORT = "3306";
	public static $DB_NAME = "";
	public static $DB_USERNAME = "";
	public static $DB_PASSWORD = "";
	
	public static $CGI_URL = "";
	public static $DATA_PATH = "";
	
	public static function initialize($error_format){
		
		if(strstr($_SERVER['PHP_SELF'], "cina_dev")===FALSE){
			PHPUtils::$IS_DEV = false;
		}else{
			PHPUtils::$IS_DEV = true;
		}
		
		PHPUtils::$DB_NAME= "cina";
		PHPUtils::$DB_USERNAME = $_SERVER['DB_USERNAME'];
		PHPUtils::$DB_PASSWORD = $_SERVER['DB_PASSWORD'];
		PHPUtils::$CGI_URL = "https://nucastrodata2.ornl.gov/cgi-bin/cina";
		PHPUtils::$DATA_PATH = "/var/www/cina_files";
		if(PHPUtils::$IS_DEV){
			PHPUtils::set_dev();
		}
		set_include_path(get_include_path().PATH_SEPARATOR."/usr/share/pear/Mail".PATH_SEPARATOR.getcwd()."/classes");
		PHPUtils::initialize_error_handling($error_format);
	}
	
	private static function set_dev(){
		PHPUtils::$DB_NAME = "cinad";
		PHPUtils::$DB_USERNAME = $_SERVER['DB_USERNAME_DEV'];
		PHPUtils::$DB_PASSWORD = $_SERVER['DB_PASSWORD_DEV'];
		PHPUtils::$CGI_URL = "https://nucastrodata2.ornl.gov/cgi-bin/cinadev";
		PHPUtils::$DATA_PATH = "/var/www/cina_files_dev";
	}

	private static function initialize_error_handling($error_format){
		Error::$error_format = $error_format;
		set_error_handler(array("Error", "error_handler"));
	}
	
	public static function initialize_async(){
		
		if(strstr($_SERVER['PHP_SELF'], "cina_dev")===FALSE){
			PHPUtils::$IS_DEV = false;
		}else{
			PHPUtils::$IS_DEV = true;
		}
		
		PHPUtils::$DB_NAME= "cina";
		PHPUtils::$DB_USERNAME = "cina";
		PHPUtils::$DB_PASSWORD = "IhWfAbN!2";
		PHPUtils::$CGI_URL = "https://nucastrodata2.ornl.gov/cgi-bin/cina";
		PHPUtils::$DATA_PATH = "/var/www/cina_files";
		if(PHPUtils::$IS_DEV){
			PHPUtils::set_dev_async();
		}
		set_include_path(get_include_path().PATH_SEPARATOR."/usr/share/pear/Mail".PATH_SEPARATOR.getcwd()."/classes");
	}
	
	private static function set_dev_async(){
		PHPUtils::$DB_NAME = "cinad";
		PHPUtils::$DB_USERNAME = "cinad";
		PHPUtils::$DB_PASSWORD = "IhWfAbN!2d";
		PHPUtils::$CGI_URL = "https://nucastrodata2.ornl.gov/cgi-bin/cinadev";
		PHPUtils::$DATA_PATH = "/var/www/cina_files_dev";
	}
	
	public static function initialize_error_handling_async($error_format, $error_instance, $error_function, 
															$id, $username, $pw, $sim_workflow_run_index){
		Error::$error_format = 			$error_format;
		Error::$error_instance = 		$error_instance;
		Error::$error_function = 		$error_function;
		Error::$id = 					$id;
		Error::$username = 				$username;
		Error::$pw = 					$pw;
		Error::$sim_workflow_run_index = 	$sim_workflow_run_index;
		set_error_handler(array("Error", "error_handler"));
	}
	
	public static function autoload($class_name){
		if($class_name=="Mail_mime"){
			$class_name="mime";
		}
	    require_once $class_name . '.php';
	}

	public static function clean_input_value($value){
		if(get_magic_quotes_gpc()){
			$value = stripslashes($value);
		}
		return trim($value);
	}
	
	public static function clean_input_values(){
		foreach($_POST as $key=>$value){
			$_POST[$key] = PHPUtils::clean_input_value($value);
		}
	}
	
	public static function check_var_missing($var){
		if(!array_key_exists($var, $_POST)){
			Error::return_error("The variable ".$var." is missing from the action ".$_POST["ACTION"], ErrorOutputType::PRIVATE_OUTPUT);
		}
	}
	
	public static function check_var_empty($var){
		if($_POST[$var]==""){
			Error::return_error("The variable ".$var." is blank for the action ".$_POST["ACTION"], ErrorOutputType::PRIVATE_OUTPUT);
		}
	}
	
}
?>
