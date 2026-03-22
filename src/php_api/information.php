<?php
$database_obj=false;
class DB_info
{
	var $dbname; ///< main database
	var $reac_dbname;  ///< reaction database
	var $eval_path; ///< filesystem path to reaction folders
	var $temp_path; ///< filesystem path to reaction folders
	var $password; ///< password
	var $username; ///< username associated with password
	var $suffix; ///<"_dev" or blank

	function DB_info($in_name='',$altin_name='',$in_pw='',$in_user='',
	$in_path='',$in_tpath='', $suffix='')
	{
		$this->dbname=$in_name;
		$this->reac_dbname=$altin_name;
		$this->password=$in_pw;
		$this->username=$in_user;
		$this->eval_path=$in_path;
		$this->temp_path=$in_tpath;
		$this->suffix=$suffix;
	}

}

function & get_db_info($suite_name )
{
	switch($suite_name) {
		case 'cina': // cina_eval
			$database_obj=new DB_info('cina','reactions','IhWfAbN!2','cina','/var/www/cina_folders','/tmp', '');
			break;
		case 'cina_dev': // cina_eval_dev
			$database_obj=new DB_info('cinad','reactions_dev','IhWfAbN!2d','cinad','/var/www/cina_folders_dev','/tmp', '_dev');
			break;
		case 'cina_wp_bn': // waiting point and bottle neck
			$database_obj=new DB_info('cina','dummy','IhWfAbN!2','cina','/var/www/cina_files', '/tmp', '');
			break;
		case 'cina_dev_wp_bn': // waiting point and bottle neck for dev
			$database_obj=new DB_info('cinad','dummy','IhWfAbN!2d','cinad','/var/www/cina_files_dev', '/tmp', '_dev');
			break;
		case 'cina_repository': // repository info
			$database_obj=new DB_info('cina','cina_repository','IhWfAbN!2','cina','/var/www/cina_repository_folders','/tmp', '');
			break;
		case 'cina_dev_repository': // repository info for dev
			$database_obj=new DB_info('cinad','cina_repository_dev','IhWfAbN!2d','cinad','/var/www/cina_repository_folders_dev','/tmp', '_dev');
			break;
	}
	return $database_obj;

}

$UPLOAD_ERRORS = array('UPLOAD_ERR_OK',
'UPLOAD_ERR_INI_SIZE',
'UPLOAD_ERR_FORM_SIZE',
'UPLOAD_ERR_PARTIAL',
'UPLOAD_ERR_NO_FILE',
'UPLOAD_ERR_NO_TMP_DIR',
'UPLOAD_ERR_CANT_WRITE',
'UPLOAD_ERR_EXTENSION');

$test_comp = array(
  'header' => "<html><head>\n<title>TEST OUTPUT</title></head>\n<body><hr><hr> %s <br><a href=\"cina_test.html\">Return</a><hr><hr>\n",
  'footer' => '</body></html>',
  'eol' => ' \N' . "<br>\n",
  'tab' => ' \T<br>');

$run_comp = array(
'header' => '%s',
  'footer' => "\n",
  'eol' => "\n",
  'tab' => "\t");

?>
