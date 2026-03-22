<?php
class Isotope{
	
	public static $symbolString = array("", "H", "He", "Li", "Be", "B", "C", "N", "O", "F", "Ne", "Na"
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
					
	public $z, $a, $symbol, $isAlMinus, $isAlStar;
	
	public function toString(){
		if($this->z==0 && $this->a==1){
			return "n";
		}else if($this->z==1 && $this->a==1){
			return "p";
		}else if($this->z==1 && $this->a==2){
			return "d";
		}else if($this->z==1 && $this->a==3){
			return "t";
		}else if(isset($this->isAlMinus) && $this->isAlMinus){
			return "-6Al";
		}else if(isset($this->isAlStar) && $this->isAlStar){
			return "*6Al";
		}else{
			return $this->a.Isotope::$symbolString[$this->z];
		}
	}
	
	public function toNetsuString(){
		if($this->z==0 && $this->a==1){
			return "n";
		}else if($this->z==1 && $this->a==1){
			return "p";
		}else if($this->z==1 && $this->a==2){
			return "d";
		}else if($this->z==1 && $this->a==3){
			return "t";
		}else if(isset($this->isAlMinus) && $this->isAlMinus){
			return "al-6";
		}else if(isset($this->isAlStar) && $this->isAlStar){
			return "al*6";
		}else{
			return strtolower(Isotope::$symbolString[$this->z]).$this->a;
		}
	}
	
	public function compareTo(Isotope $temp){
		if($this->z!=$temp->z){
			return $this->z-$temp->z;
		}
		return $this->a-$temp->a;
	}
}
?>