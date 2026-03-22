#!/usr/bin/php

<?php

$id 				= $argv[1];
$username 			= $argv[2];
$pw 				= $argv[3];
$sim_workflow_run_index = $argv[4];

include("PHPUtils.php");            
function __autoload($class_name){
	PHPUtils::autoload ($class_name);
}
PHPUtils::initialize_async();
$sim_workflow_run_executor = new SimWorkflowRunExecutor($id, $username, $pw, $sim_workflow_run_index);
PHPUtils::initialize_error_handling_async(ErrorFormat::WEB_SERVICE, 
											$sim_workflow_run_executor, 
											"handle_sim_workflow_run_error", 
											$id, 
											$username, 
											$pw, 
											$sim_workflow_run_index);
$sim_workflow_run_executor->execute_sim_workflow_run();
?>