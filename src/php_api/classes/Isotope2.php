<?php

/**
 * The Isotope class represents an isotope as parsed from REACLIB.
 * 
 * @author 		Eric J. Lingerfelt
 * @copyright	(c) 2012, Oak Ridge National Laboratory
 */
class Isotope2{
	
	//An array of element symbol keyed on proton number
	public static $symbol_array = array("", "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na"
										, "Mg", "Al", "Si", "P", "S", "Cl", "Ar", "K", "Ca", "Sc", "Ti", "V", "Cr", "Mn", "Fe" 
										, "Co", "Ni", "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", "Kr", "Rb", "Sr", "Y", "Zr", "Nb"
										, "Mo", "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", "Sn", "Sb", "Te", "I", "Xe", "Cs", "Ba"
										, "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", "Lu"
										, "Hf", "Ta", "W", "Re", "Os", "Ir", "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", "At", "Rn"
										, "Fr", "Ra", "Ac", "Th", "Pa", "U", "Np", "Pu", "Am", "Cm", "Bk", "Cf", "Es", "Fm", "Md"
										, "No", "Lr", "Rf", "Db", "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn", "Nh", "Fl", "Mc"
										, "Lv", "Ts", "Og", "Uue", "Ubn", "Ubu", "Ubb", "Ubt", "Ubq", "Ubp", "Ubh", "Ubs", "Ubo"
										, "Ube", "Utn", "Utu", "Utb", "Utt", "Utq", "Utp", "Uth", "Uts", "Uto", "Ute", "Uqn", "Uqu"
										, "Uqb", "Uqt", "Uqq", "Uqp", "Uqh", "Uqs", "Uqo", "Uqe", "Upn", "Upu", "Upb", "Upt", "Upq"
										, "Upp", "Uph", "Ups", "Upo", "Upe", "Uhn", "Uhu", "Uhb", "Uht", "Uhq", "Uhp", "Uhh", "Uhs"
										, "Uho", "Uhe", "Usn", "Usu", "Usb", "Ust", "Usq", "Usp");

	//The atomic number
	public $z;
	
	//The mass number
	public $a;
	
	//Flag indicating Al-6
	public $is_alminus;
	
	//Flag indicating Al*6
	public $is_alstar;
	
	/**
	 * Returns the string representation of this isotope.
	 * 
	 * @return	the string representation of this isotope
	 */
	public function toString(){
		if($this->z==0 && $this->a==1){
			return "n";
		}else if($this->z==1 && $this->a==1){
			return "p";
		}else if($this->z==1 && $this->a==2){
			return "d";
		}else if($this->z==1 && $this->a==3){
			return "t";
		}else if(isset($this->is_alminus) && $this->is_alminus){
			return "-6Al";
		}else if(isset($this->is_alstar) && $this->is_alstar){
			return "*6Al";
		}else{
			return $this->a.Isotope2::$symbol_array[$this->z];
		}
	}
	
	/**
	 * Compares this isotope to another by z and then a values.
	 * 
	 * @param	the isotope to compare to this isotope
	 * @return	0, if equal. A negative number if $temp is greater and a positive number if this isotope is greater
	 */
	public function compareTo(Isotope2 $temp){
		if($this->z!=$temp->z){
			return $this->z-$temp->z;
		}
		return $this->a-$temp->a;
	}
}
?>