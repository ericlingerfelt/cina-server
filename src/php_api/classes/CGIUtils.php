<?php

class CGIUtils{
	
	public static function execute_cgi_call($cgi_input_array){
		
        $cgi_input_str = "";
		foreach ($cgi_input_array as $key => $value){
			$cgi_input_str .= $key.'='.urlencode($value).'&';
		}
		$cgi_input_str = rtrim($cgi_input_str, '&');
		
		$ch = curl_init();
		
		curl_setopt($ch, CURLOPT_URL, PHPUtils::$CGI_URL);
		curl_setopt($ch, CURLOPT_POST, count($cgi_input_array));
		curl_setopt($ch, CURLOPT_POSTFIELDS, $cgi_input_str);
		curl_setopt($ch, CURLOPT_HEADER, 0);
		curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
		
		$cgi_output = curl_exec($ch);
		curl_close($ch);
		
		return $cgi_output;
		
	}
	
}

?>