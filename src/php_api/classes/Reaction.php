<?php
class Reaction{
	
	public $reaction_type, $decay, $main_isotope, $isotope_array, $reaction_name;
	
	function __construct($reaction_type, $decay, $isotope_array){
		$this->decay = $decay;
		$this->reaction_type = $reaction_type;
		$this->isotope_array = $isotope_array;
		$this->main_isotope = $this->getMainIsotope();
		$this->reaction_name = $this->getReactionName();
	}
	
	private function getMainIsotope(){
		switch($this->reaction_type){
			case 1:
			case 2:
			case 3:
				return $this->isotope_array[0];
				break;
			case 4:
			case 5:
			case 6:
			case 7:
				return $this->getBiggestIsotope($this->isotope_array[0], $this->isotope_array[1]);
				break;
			case 8:
				$isotope = $this->getBiggestIsotope($this->isotope_array[0], $this->isotope_array[1]);
				return $this->getBiggestIsotope($isotope, $this->isotope_array[2]);
				break;
		}
	}
	
	public static function getBiggestIsotope(Isotope $isotope1, Isotope $isotope2){
		$result = $isotope1->compareTo($isotope2);
		if($result==0){
			return $isotope1;
		}else if($result>0){
			return $isotope1;
		}else{
			return $isotope2;
		}
	}
	
	public function getReactionName(){
		$isotope_array = $this->isotope_array;
		switch($this->reaction_type){
			case 1:
				return $isotope_array[0]->toString()
						." --> "
						.$isotope_array[1]->toString();
			case 2:
				return $isotope_array[0]->toString()
						." --> "
						.$isotope_array[1]->toString()
						." + "
						.$isotope_array[2]->toString();
			case 3:
				return $isotope_array[0]->toString()
						." --> "
						.$isotope_array[1]->toString()
						." + "
						.$isotope_array[2]->toString()
						." + "
						.$isotope_array[3]->toString();
			case 4:
				return $isotope_array[0]->toString()
						." + "
						.$isotope_array[1]->toString()
						." --> "
						.$isotope_array[2]->toString();
			case 5:
				return $isotope_array[0]->toString()
						." + "
						.$isotope_array[1]->toString()
						." --> "
						.$isotope_array[2]->toString()
						." + "
						.$isotope_array[3]->toString();
			case 6:
				return $isotope_array[0]->toString()
						." + "
						.$isotope_array[1]->toString()
						." --> "
						.$isotope_array[2]->toString()
						." + "
						.$isotope_array[3]->toString()
						." + "
						.$isotope_array[4]->toString();
			case 7:
				return $isotope_array[0]->toString()
						." + "
						.$isotope_array[1]->toString()
						." --> "
						.$isotope_array[2]->toString()
						." + "
						.$isotope_array[3]->toString()
						." + "
						.$isotope_array[4]->toString()
						." + "
						.$isotope_array[5]->toString();
			case 8:
				if(count($isotope_array)==4){
					return $isotope_array[0]->toString()
							." + "
							.$isotope_array[1]->toString()
							." + "
							.$isotope_array[2]->toString()
							." --> "
							.$isotope_array[3]->toString();
				}else if(count($isotope_array)==5){
					return $isotope_array[0]->toString()
							." + "
							.$isotope_array[1]->toString()
							." + "
							.$isotope_array[2]->toString()
							." --> "
							.$isotope_array[3]->toString()
							." + "
							.$isotope_array[4]->toString();
				}
		}
	}
}
?>