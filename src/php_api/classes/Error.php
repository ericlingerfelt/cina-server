<?php

class Error{
	
	public static $error_format;
	public static $error_instance;
	public static $error_function;
	public static $id;
	public static $username;
	public static $pw;
	public static $sim_workflow_run_index;
	
	public static function error_handler($errno, $errstr, $errfile, $errline){
		if($errno<=1024){
		    $error = "[ERROR CODE: $errno] $errstr in $errfile on line $errline";
		    Error::return_error($error, ErrorOutputType::PRIVATE_OUTPUT);
		}
	}

	public static function return_pdo_error($pdoe, $pdos){
		if($pdos==null){
			$error = "MESSAGE -> ". $pdoe->getMessage()."\nSTACKTRACE -> ".$pdoe->getTraceAsString();
		}else if($pdoe==null){
			$error = "QUERY -> ".$pdos->queryString;
		}else{
			$error = "MESSAGE -> ". $pdoe->getMessage()." QUERY -> ".$pdos->queryString."\nSTACKTRACE -> ".$pdoe->getTraceAsString();
		}
		Error::return_error($error, ErrorOutputType::PRIVATE_OUTPUT);
	}
	
	public static function return_error($error, $error_priv){
		$message = "";
		
		if($error_priv==ErrorOutputType::PUBLIC_OUTPUT){
			$message = $error;
		}else if($error_priv==ErrorOutputType::PRIVATE_OUTPUT){
			$message = "An error has occurred completing your request. "
						."The appropriate staff have been notified.";
		}else if($error_priv==ErrorOutputType::NO_OUTPUT){
			Error::log_error($error);
		}
		
		if(Error::$error_format==ErrorFormat::WEB_SERVICE){
			print("ERROR=".$message);
		}else if(Error::$error_format==ErrorFormat::HTML){
			print("<br><br><b><i>$message</i></b><br><br>");
		}

		Error::log_error($error);
	}
	
	public static function log_error($error){
		
		$id = Error::$id;
		$username = Error::$username;
		$args = "";
		foreach($_POST as $key => $value){
			$args.= "$key=$value:";
		}
		$args = rtrim($args, ':');

		$query = "INSERT INTO error_log (id, username, error, args, date) VALUES (:id, :username, :error, :args, NOW())";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('id', $id);
		$pdos->set_str_value('username', $username);
		$pdos->set_str_value('error', $error);
		$pdos->set_str_value('args', $args);
		$pdos->execute();
			
		$error_log_index = $pdos->get_insert_id();
		Email::send_php_error_email($error_log_index);
		if(Error::$error_function!=""){
			call_user_func_array(array(Error::$error_instance, Error::$error_function), 
									array($error_log_index, $id, $username, Error::$pw, Error::$sim_workflow_run_index));
		}
		die;
	}

}
?>
