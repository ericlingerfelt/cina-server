<?php

function handle_is_master_user(& $database_info, $t_array, $username){
    
    $query_str = "SELECT is_master_user FROM accounts WHERE username='" . $username . "'";
    $rtn = mysql_query($query_str);
    if (! $rtn) {
        $err = mysql_error();
        trigger_error($err, E_USER_ERROR);
        return;
    }
    $row = mysql_fetch_assoc($rtn);
    if ($row['is_master_user'] == "1") {
        print("IS_MASTER_USER=true\n");
    }else{
        print("IS_MASTER_USER=false\n");
    }
}

?>