<?php

class ExecUtils{
    
    public static function exec_async($command){
        
        $output_array = array();
        $return_value = "";
        $process_id = exec($command.' > /dev/null 2>&1 & echo $!;', $output_array, $return_value);
        
        if((int)$return_value!=0){
            
            $output = implode("\n", $output_array);
            Error::return_error($output, ErrorOutputType::PRIVATE_OUTPUT);
            
        }else{
        
        	return $process_id;
        
        }
        
    }
    
    public static function exec($command){
    
    	$output_array = array();
    	$return_value = "";
    	exec($command.' 2>&1', $output_array, $return_value);
    
    	if((int)$return_value!=0){
    
    		$output = implode("\n", $output_array);
    		Error::return_error($output, ErrorOutputType::PRIVATE_OUTPUT);
    
    	}

    }
    
    public static function exec_with_output($command){
    
    	$output_array = array();
    	$return_value = "";
    	exec($command.' 2>&1', $output_array, $return_value);
    
    	if((int)$return_value!=0){
    
    		$output = implode("\n", $output_array);
    		Error::return_error($output, ErrorOutputType::PRIVATE_OUTPUT);
    
    	}
    
    	$output_str = "";
    	foreach($output_array as $output){
    		$output_str.= "$output ";
    	}
    	return trim($output_str);
    }
    
}

?>
