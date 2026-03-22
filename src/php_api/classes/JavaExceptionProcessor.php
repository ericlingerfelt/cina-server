<?php

class JavaExceptionProcessor{

	public $id;
	public $username;

	public function __construct($id, $username){
		$this->id = $id;
		$this->username = $username;
		Error::$id = $this->id;
	}

	public function process_java_exception(){

		$stack_trace = $_POST["STACK_TRACE"];
		
		$query = "INSERT INTO exception_log (id, username, stack_trace, date) VALUES (:id, :username, :stack_trace, NOW())";
		$pdos = new PDOSUtil($query);
		$pdos->set_str_value('id', 			$this->id);
		$pdos->set_str_value('username', 	$this->username);
		$pdos->set_str_value('stack_trace', $stack_trace);
		$pdos->execute();
		
		if(!Email::send_java_exception_email($pdos->get_insert_id())){
			Error::return_error("Unable to send email for java exception.", ErrorOutputType::NO_OUTPUT);
		}

	}

}

?>