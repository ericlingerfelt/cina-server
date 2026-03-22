<?php

/**
 * The Reaction class represents a thermonuclear reaction parsed from REACLIB.
 * 
 * @author		Eric J. Lingerfelt
 * @copyright	(c) 2012, Oak Ridge National Laboratory
 */
class Reaction2{
	
	//The reaction type (1-8) 
	public $reaction_type;
	
	//An array of isotopes containing each reactant and product
	//in the order they were parsed from the REACLIB reaction
	public $isotope_array;
	
	//The biblio code
	public $source_label;
	
	//A flag indicating an inverse reaction
	public $is_inverse;
	
	//A flag indicating a resonant reaction
	public $is_resonant;
	
	//An array of length 7 containing all of the parameters
	public $param_array;
	
	//The Q-value
	public $q_value;
	
	//The library name for this reaction
	public $library;
	
	//The number of reactants
	public $num_reactants;
	
	//The number of products
	public $num_products;
	
	//The rate id required by CINA
	public $rateid;
	
	//The main (i.e., key) reactant isotope used to classify this reaction 
	public $main_isotope;
	
	//The reaction name as seen in CINA
	public $reaction_name;
	
	//The resonant symbol
	public $resonant;
	
	/**
	 * The Constructor
	 */
	function __construct($reaction_type
							, $source_label
							, $library
							, $isotope_array
							, $param_array
							, $is_inverse
							, $resonant
							, $q_value){
		$this->reaction_type = $reaction_type;
		$this->source_label = $source_label;
		$this->library = $library;
		$this->isotope_array = $isotope_array;
		$this->param_array = $param_array;
		$this->is_inverse = $is_inverse;
		$this->resonant = $resonant;
		$this->q_value = $q_value;
		$this->num_reactants = $this->getNumReactants();
		$this->num_products = $this->getNumProducts();
		$this->rateid = $this->getRateID();
		$this->main_isotope = $this->getMainIsotope();
		$this->reaction_name = $this->getReactionName();
	}
	
	/**
	 * Returns the rate id as used in CINA.
	 * 
	 * @return	the rate id as used in CINA
	 */
	private function getRateID(){
		
		//Unicode \u0009
		$char09 = '\u0009';
		
		//Unicode \u00b
		$char0b = '\u000b';
		
		//Create the rate id and return it
		$rateid = "0".$this->reaction_type;
		$rateid.= sprintf("%03d", $this->getMainIsotope()->z);
		$rateid.= sprintf("%03d", $this->getMainIsotope()->a);
		$rateid.= $this->library;
		$rateid.= json_decode('"'.$char09.'"');
		$rateid.= $this->getReactionName();
		$rateid.= json_decode('"'.$char0b.'"');
		if($this->source_label=="bet-"
			|| $this->source_label=="bet+"
			|| $this->source_label=="ec"){
			$rateid.= $this->source_label;
		}
		return $rateid;
	}
	
	/**
	 * Returns the main (i.e., key) reactant isotope.
	 * 
	 * @return the main (i.e., key) reactant isotope
	 */
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
	
	/**
	 * Returns the "biggest" isotope as compared by the Isotope class's compareTo method.
	 * 
	 * @param	$isotope1	the first isotope to compare
	 * @param	$isotope2	the second isotope to compare
	 * @return	the "biggest" isotope as compared by the Isotope class's compareTo method
	 */
	private static function getBiggestIsotope(Isotope2 $isotope1, Isotope2 $isotope2){
		$result = $isotope1->compareTo($isotope2);
		if($result==0){
			return $isotope1;
		}else if($result>0){
			return $isotope1;
		}else{
			return $isotope2;
		}
	}
	
	/**
	 * Returns the number of reactants.
	 * 
	 * @return	the number of reactants
	 */
	private function getNumReactants(){
		switch($this->reaction_type){
			case 1:
			case 2:
			case 3:
				return 1;
			case 4:
			case 5:
			case 6:
			case 7:
				return 2;
			case 8:
				return 3;
		}
	}
	
	/**
	 * Returns the number of products.
	 * 
	 * @return	the number of products
	 */
	private function getNumProducts(){
		$isotope_array = $this->isotope_array;
		switch($this->reaction_type){
			case 1:
				return 1;
			case 2:
				return 2;
			case 3:
				return 3;
			case 4:
				return 1;
			case 5:
				return 2;
			case 6:
				return 3;
			case 7:
				return 4;
			case 8:
				if(count($isotope_array)==4){
					return 1;
				}else if(count($isotope_array)==5){
					return 2;
				}
		}
	}
	
	/**
	 * Returns the reaction name.
	 * 
	 * @return 	the reaction name
	 */
	private function getReactionName(){
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