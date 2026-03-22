<?php

class PDOSUtil{
	
	private $pdos;
	private $pdo; 

	public function __construct($query){

		$host = "localhost";
		
		$port = PHPUtils::$DB_PORT;
		$username = PHPUtils::$DB_USERNAME;
		$password = PHPUtils::$DB_PASSWORD;
		$dbname = PHPUtils::$DB_NAME;

		$dsn = "mysql:dbname=$dbname;host=$host;port=$port";
	
		try{
			$this->pdo = new PDO($dsn, $username, $password);
			$this->pdo->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
			$this->pdo->setAttribute(PDO::ATTR_EMULATE_PREPARES, false);
			$this->pdos = $this->pdo->prepare($query);
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}

	public function set_str_value($name, $value){
		try{
			if($value=='NULL'){
				$this->pdos->bindValue(":".$name, null, PDO::PARAM_STR);
			}else{
				$this->pdos->bindValue(":".$name, $value, PDO::PARAM_STR);
			}
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}
	
	public function set_int_value($name, $value){
		try{
			$this->pdos->bindValue(":".$name, $value, PDO::PARAM_INT);
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}
	
	public function execute(){
		try{
			$this->pdos->execute();
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}
	
	public function has_rows(){
		if($this->pdos->fetchColumn() > 0){
			return TRUE;
		}
		return FALSE;
	}
	
	public function fetch_assoc(){
		try{
			return $this->pdos->fetch(PDO::FETCH_ASSOC);
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}
	
	public function fetch_row(){
		try{
			return $this->pdos->fetch(PDO::FETCH_NUM);
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}
	
	public function get_insert_id(){
		try{
			return $this->pdo->lastInsertId();
		}catch(PDOException $pdoe){
			Error::return_pdo_error($pdoe, $this->pdos);
		}
	}

}

?>
