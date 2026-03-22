<?php

header('Cache-Control: no-cache, no-store, must-revalidate');
header('Expires: Thur, 01 Jan 1970 00:00:01 GMT');
header('Pragma: no-cache');

include('error_handler.php');
$old_handler=set_error_handler('error_handler');

$ar = array();

if(strstr($_SERVER['PHP_SELF'], "cina_dev")===FALSE){
	$ar[0] = "cina";
}else{
	$ar[0] = "cina_dev";
}

include 'information.php';
include 'admin_functions.php';

$text_array =& $run_comp;

$username = log_php_exchange($ar);

function log_php_exchange($ar){
	$my_dbinfo =& get_db_info("$ar[0]");
	if(! $my_dbinfo) {
		$merr="ERROR: Could not get database information for {$ar[0]}\n";
		trigger_error($merr,E_USER_ERROR);
		return;
	}
	$rtn=validate_session_id($my_dbinfo);
	if(! $rtn) {
		$err="COULD NOT validate session ID";
		trigger_error($err,E_USER_ERROR);
		return;
	}
	$id = $_POST["ID"];
	$username = $rtn["USERNAME"];
	
	$args = "";
	foreach($_POST as $key => $value){
		$args.= "$key=$value:";
	}
	$args = rtrim($args, ':');
	
	$query = "INSERT INTO logs_access_php (id, username, args, date) VALUES ('$id', '$username', '$args', NOW())";
	$rtn = mysql_query($query);
	if(!$rtn){
		$err=mysql_error();
		trigger_error($err,E_USER_ERROR);
		return;
	}
	return $username;
}

if(array_key_exists('ACTION',$_POST)===FALSE || trim($_POST["ACTION"])==""){
	
	$merr="NO ACTION SPECIFIED:" . $text_array['eol'];
	trigger_error($merr,E_USER_ERROR);
	return;
	
}

if(array_key_exists('ACTION',$_POST)===TRUE){
	
	if($_POST["ACTION"]=="EXECUTE_SIM_WORKFLOW_RUN"
	        || $_POST["ACTION"]=="SIM_WORKFLOW_NAME_EXISTS"
	        || $_POST["ACTION"]=="SIM_WORKFLOW_IN_USE"
	        || $_POST["ACTION"]=="SIM_WORKFLOW_RUN_NAME_EXISTS"
			|| $_POST["ACTION"]=="CREATE_SIM_WORKFLOW"
	        || $_POST["ACTION"]=="ERASE_SIM_WORKFLOWS"
	        || $_POST["ACTION"]=="ERASE_SIM_WORKFLOW_RUN"
	        || $_POST["ACTION"]=="CREATE_SIM_WORKFLOW_RUN"
	        || $_POST["ACTION"]=="GET_SIM_WORKFLOW_RUNS"
	        || $_POST["ACTION"]=="GET_SIM_WORKFLOWS"
	        || $_POST["ACTION"]=="GET_SIM_WORKFLOW_INFO"
	        || $_POST["ACTION"]=="COPY_SIM_WORKFLOWS_TO_SHARED"
			|| $_POST["ACTION"]=="SAVE_SIM_WORKFLOW_RUN"
			|| $_POST["ACTION"]=="GET_SIM_WORKFLOW_RUN_STATUS"
			|| $_POST["ACTION"]=="GET_SIM_WORKFLOW_TYPES"
			|| $_POST["ACTION"]=="ABORT_SIM_WORKFLOW_RUN"
			|| $_POST["ACTION"]=="GET_SENS_NETWORK_ISOTOPES"
			|| $_POST["ACTION"]=="GET_SENS_NETWORK_REACTIONS"
			|| $_POST["ACTION"]=="LOG_JAVA_EXCEPTION"
			|| $_POST["ACTION"]=="GET_THERMO_PROFILE_SET_INFO"
			|| $_POST["ACTION"]=="COPY_THERMO_PROFILE_SETS_TO_SHARED"
			|| $_POST["ACTION"]=="GET_THERMO_PROFILE_SETS"
			|| $_POST["ACTION"]=="ERASE_THERMO_PROFILE_SETS"
			|| $_POST["ACTION"]=="IMPORT_THERMO_PROFILE_SET"
			|| $_POST["ACTION"]=="GET_SIM_INFO"
			|| $_POST["ACTION"]=="GET_SIMS"
			|| $_POST["ACTION"]=="ERASE_SIMS"
			|| $_POST["ACTION"]=="COPY_SIMS_TO_SHARED"
			|| $_POST["ACTION"]=="GET_TOTAL_WEIGHTS"
			|| $_POST["ACTION"]=="GET_ELEMENT_SYNTHESIS_ZONES"
			|| $_POST["ACTION"]=="IS_SIM_SENS"
			|| $_POST["ACTION"]=="IMPORT_RATE_LIBRARY"
			|| $_POST["ACTION"]=="GET_LIB_DIRS"
			|| $_POST["ACTION"]=="GET_LIB_DIR_LIBS"
			|| $_POST["ACTION"]=="ERASE_LIB_DIR"
			|| $_POST["ACTION"]=="GET_LIB_DIR_INFO"
	        || $_POST["ACTION"]=="GET_ELEMENT_SYNTHESIS_EDOT_VALUES"
	        || $_POST["ACTION"]=="NAME_SIM_WORKFLOW_RUN"){
	
		$my_dbinfo =& get_db_info("$ar[0]");
		if(! $my_dbinfo) {
			$merr="ERROR: Could not get database information for {$ar[0]}\n";
			trigger_error($merr,E_USER_ERROR);
			return;
		}
		$rtn = validate_session_id ($my_dbinfo);
		if (!$rtn) {
			$err = "COULD NOT validate session ID";
			trigger_error ( $err, E_USER_ERROR );
			return;
		}
		
		$pw = "";
		if(array_key_exists("PW", $_POST)){
			$pw = $_POST["PW"];
		}
		$id = $_POST["ID"];
		
		include(getcwd()."/classes/PHPUtils.php");
                
		function __autoload($class_name){
			PHPUtils::autoload ($class_name);
		}
		PHPUtils::initialize (ErrorFormat::WEB_SERVICE);
		
		switch($_POST['ACTION']) {
		    
		    case 'SIM_WORKFLOW_IN_USE':
		        
		        $name = $_POST["NAME"];
		        
		        $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
		        $sim_workflow_utils->in_use($name);
		        
		        break;
		    
		    case 'SIM_WORKFLOW_NAME_EXISTS':
		        
		        $name = $_POST["NAME"];
		        
		        $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
		        $sim_workflow_utils->name_exists($name);
		        
		        break;
		        
		    case 'SIM_WORKFLOW_RUN_NAME_EXISTS':
		        
		        $name = $_POST["NAME"];
		        
		        $sim_workflow_run_utils = new SimWorkflowRunUtils($id, $username, $pw);
		        $sim_workflow_run_utils->name_exists($name);
		        
		        break;
		    
		    case 'CREATE_SIM_WORKFLOW':
		        
		        $name = $_POST["NAME"];
		        
		        $sim_workflow_processor = new SimWorkflowProcessor($name, $username, $pw);
		        $sim_workflow_processor->create_sim_workflow();
		        
		        break;
		        
		    case 'CREATE_SIM_WORKFLOW_RUN':
		        
		        $sim_workflow_index = $_POST["SIM_WORKFLOW_INDEX"];
		        
		        $sim_workflow_run = new SimWorkflowRun();
		        $sim_workflow_run->create($sim_workflow_index, $username);
		        
		        break;
		        
		    case 'NAME_SIM_WORKFLOW_RUN':
		        
		        $sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
		        $name = $_POST["NAME"];
		        
		        $sim_workflow_run = new SimWorkflowRun();
		        $sim_workflow_run->name($sim_workflow_run_index, $name);
		        
		        break;
		        
		    case 'GET_SIM_WORKFLOW_INFO':
		        
		        $sim_workflow_indices = $_POST["SIM_WORKFLOW_INDICES"];
		        
		        $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
		        $sim_workflow_utils->get_sim_workflow_info($sim_workflow_indices);
		        
		        break;
		        
		    case 'COPY_SIM_WORKFLOWS_TO_SHARED':
		        
		        $sim_workflow_indices = $_POST["SIM_WORKFLOW_INDICES"];
		        
		        $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
		        $sim_workflow_utils->copy_sim_workflows_to_shared($sim_workflow_indices);
		        
		        break;
		        
		    case 'ERASE_SIM_WORKFLOWS':
		        
		        $sim_workflow_indices = $_POST["SIM_WORKFLOW_INDICES"];
		        
		        $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
		        $sim_workflow_utils->erase_sim_workflows($sim_workflow_indices);
		        
		        break;
		        
		    case 'ERASE_SIM_WORKFLOW_RUN':
		        
		        $sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
		        
		        $sim_workflow_run_utils = new SimWorkflowRunUtils($id, $username, $pw);
		        $sim_workflow_run_utils->erase_sim_workflow_run($sim_workflow_run_index);
		        
		        break;
		        
		    case 'GET_SIM_WORKFLOW_RUNS':
		        
		        $sim_workflow_run_utils = new SimWorkflowRunUtils($id, $username, $pw);
		        $sim_workflow_run_utils->get_sim_workflow_runs();
		        
		        break;

			case 'GET_SIM_WORKFLOWS':
			    
			    $sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
			    $sim_workflow_utils->get_sim_workflows();
			    
			    break;
	
			case 'EXECUTE_SIM_WORKFLOW_RUN':
					
				$sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
				$process_id = ExecUtils::exec_async(getcwd()."/classes/execute_sim_workflow_run.php $id $username $pw $sim_workflow_run_index");
				
				$sim_workflow_run = new SimWorkflowRun();
				$sim_workflow_run->populate($sim_workflow_run_index);
				$sim_workflow_run->set_process_id($process_id);

				break;
			
			case 'ABORT_SIM_WORKFLOW_RUN':
			    
			    $sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
			    
			    $sim_workflow_run_executor = new SimWorkflowRunExecutor($id, $username, $pw, $sim_workflow_run_index);
			    $sim_workflow_run_executor->abort_sim_workflow_run();
			    
			    break;
				
			case 'SAVE_SIM_WORKFLOW_RUN':
					
			    $sim_workflow_run_index = $_POST["SIM_WORKFLOW_RUN_INDEX"];
			    $sim_name_base = $_POST["SIM_NAME"];
			    $notes = $_POST["NOTES"];
			    
			    $sim_workflow_run_utils = new SimWorkflowRunUtils($id, $username, $pw);
			    $sim_workflow_run_utils->save_sim_workflow_run($sim_workflow_run_index, $sim_name_base, $notes);
					
				break;
			
			case 'GET_SENS_NETWORK_ISOTOPES':
			
				$sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
				$sim_workflow_utils->get_sens_network_isotopes();
			
				break;
				
			case 'GET_SIM_WORKFLOW_RUN_STATUS':
					
				$sim_workflow_run_utils = new SimWorkflowRunUtils($id, $username, $pw);
				$sim_workflow_run_utils->get_sim_workflow_run_status();
					
				break;
				
			case 'GET_SIM_WORKFLOW_TYPES':
					
				$sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
				$sim_workflow_utils->get_sim_workflow_types();
					
				break;
			
			case 'GET_SENS_NETWORK_REACTIONS':
				
				$sim_workflow_utils = new SimWorkflowUtils($id, $username, $pw);
				$sim_workflow_utils->get_sens_network_reactions();
				
				break;
			
			case 'LOG_JAVA_EXCEPTION':
			
				$java_exception_processor = new JavaExceptionProcessor($id, $username);
				$java_exception_processor->process_java_exception();
	
				break;
				
			case 'GET_THERMO_PROFILE_SET_INFO':
					
				$thermo_utils = new ThermoUtils($username);
				$thermo_utils->get_thermo_profile_set_info();
				break;
			
			case 'COPY_THERMO_PROFILE_SETS_TO_SHARED':
					
				$thermo_utils = new ThermoUtils($username);
				$thermo_utils->copy_thermo_profile_sets_to_shared();
				break;
			
			case 'GET_THERMO_PROFILE_SETS':
					
				$thermo_utils = new ThermoUtils($username);
				$thermo_utils->get_thermo_profile_sets();
				break;
					
			case 'ERASE_THERMO_PROFILE_SETS':
			
				$thermo_utils = new ThermoUtils($username);
				$thermo_utils->erase_thermo_profile_sets();
				break;
			
			case 'IMPORT_THERMO_PROFILE_SET':
			
				$thermo_utils = new ThermoUtils($username);
				$thermo_utils->import_thermo_profile_set();
				break;
				
			case 'GET_SIM_INFO':
					
				$sim_utils = new SimUtils($username);
				$sim_utils->get_sim_info();
				break;
			
			case 'GET_SIMS':
					
				$sim_utils = new SimUtils($username);
				$sim_utils->get_sims();
				break;
					
			case 'ERASE_SIMS':
					
				$sim_utils = new SimUtils($username);
				$sim_utils->erase_sims();
				break;
					
			case 'COPY_SIMS_TO_SHARED':
					
				$sim_utils = new SimUtils($username);
				$sim_utils->copy_sims_to_shared($username);
				break;
				
			case 'GET_TOTAL_WEIGHTS':
					
				$sim_utils = new SimUtils($username);
				$sim_utils->get_total_weights();
				break;
			
			case 'GET_ELEMENT_SYNTHESIS_ZONES':
			
				$sim_utils = new SimUtils($username);
				$sim_utils->get_element_synthesis_zones();
				break;
			
			case 'IS_SIM_SENS':
			
				$sim_utils = new SimUtils($username);
				$sim_utils->is_sim_sens();
				break;
				
			case 'IMPORT_RATE_LIBRARY':
					
				$lib_dir_utils = new LibDirUtils($username);
				$lib_dir_utils->import_rate_library();
				break;
				
			case 'GET_LIB_DIRS':
					
				$lib_dir_utils = new LibDirUtils($username);
				$lib_dir_utils->get_lib_dirs();
				break;
			
			case 'GET_LIB_DIR_LIBS':
					
				$lib_dir_utils = new LibDirUtils($username);
				$lib_dir_utils->get_lib_dir_libs();
				break;
			
			case 'ERASE_LIB_DIR':
			
				$lib_dir_utils = new LibDirUtils($username);
				$lib_dir_utils->erase_lib_dir();
				break;
			
			case 'GET_LIB_DIR_INFO':
					
				$lib_dir_utils = new LibDirUtils($username);
				$lib_dir_utils->get_lib_dir_info();
				break;
				
			case 'GET_ELEMENT_SYNTHESIS_EDOT_VALUES':
			    
			    $sim_utils = new SimUtils($username);
			    $sim_utils->get_element_synthesis_edot_values();
			    break;
				
			default:
					
				$merr="ILLEGAL ACTION: {$_POST['ACTION']}" . $text_array['eol'];
				trigger_error($merr,E_USER_ERROR);
				return;
				
		}
		
	}else{

		$my_dbinfo =& get_db_info("$ar[0]");
		if(! $my_dbinfo) {
			$merr="ERROR: Could not get database information for {$ar[0]}\n";
			trigger_error($merr,E_USER_ERROR);
			return;
		}
		
		switch($_POST['ACTION']) {
					
			case 'GET_WAITING_POINTS':
					
				include('waiting_point/waiting_point_handlers.php');
				handle_get_waiting_points($my_dbinfo,$text_array);
				break;
					
			case 'GET_BOTTLENECK_REACTIONS':
					
				include('bottle_neck/bottle_neck_handlers.php');
				handle_get_bottle_necks($my_dbinfo,$text_array);
				break;
					
			case 'IS_MASTER_USER':
					
				include('master_user_handlers.php');
				handle_is_master_user($my_dbinfo,$text_array,$username);
				break;
		
			default:
					
				$merr="ILLEGAL ACTION: {$_POST['ACTION']}" . $text_array['eol'];
				trigger_error($merr,E_USER_ERROR);
				return;
				
		}
		
	}
	
}
?>

