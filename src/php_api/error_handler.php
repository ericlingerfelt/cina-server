<?php
function error_handler($errno, $errmsg, $filename, $linenum, $vars)
{


  // define an assoc array of error string
  // E_ERROR, E_PARSE and the COMPILE and CORE errors CANNOT
  // be handled.
  $errortype = array (
    //E_WARNING => 'ERROR=PHP Warning',
    //E_NOTICE => 'ERROR=PHP Notice',
    E_USER_ERROR => 'ERROR=User Error'
    //E_USER_WARNING => 'ERROR=User Warning',
    //E_USER_NOTICE => 'ERROR=User Notice',
    );
  // put together the error string
  $err=$errortype[$errno];
  $err .= ": " .  $errmsg ;
  $err .= " in " .  $filename ;
  $err .= " at " .  $linenum .  "\n"; 

  if(array_key_exists('log',$GLOBALS) && $GLOBALS['log']) {
    // save to the error log for production only
    error_log($err);
    echo "<hr>Error logged</hr>";
  }
  echo "\n$err";
}



