<?php

class Email{
	
	private static $CINA_DEV_EMAIL = "eric@pandiasoftware.com";
	
	public static function send_php_error_email($error_log_index){
	
		$query = "SELECT * FROM error_log WHERE error_log_index=:error_log_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('error_log_index', $error_log_index);
		$pdos->execute();
		$row = $pdos->fetch_assoc();
		$error = $row["error"];
		$time = $row["date"];
	
		$text = "A php error has been thrown by CINA.\n";
		$text.= "ERROR_LOG_INDEX=$error_log_index\n";
		$text.= "ERROR=$error\n";
		$text.= "TIME=$time\n";
	
		$html = "<html><body>A php error has been thrown by CINA.<br>";
		$html.= "ERROR_LOG_INDEX=$error_log_index<br>";
		$html.= "ERROR=$error<br>";
		$html.= "TIME=$time<br>";
		$html.= "</body><html>";
	
		$subject = "CINA PHP Error";
		return Email::send_email(Email::$CINA_DEV_EMAIL, $subject, $text, $html);
	}
	
	public static function send_java_exception_email($exception_index){
	
		$query = "SELECT * FROM exception_log WHERE exception_index=:exception_index";
		$pdos = new PDOSUtil($query);
		$pdos->set_int_value('exception_index', $exception_index);
		$pdos->execute();
		$row = $pdos->fetch_assoc();
		$stack_trace = $row["stack_trace"];
		$time = $row["date"];
	
		$text = "A java exception has been thrown by CINA.\n";
		$text.= "EXCEPTION_INDEX=$exception_index\n";
		$text.= "STACK_TRACE=$stack_trace\n";
		$text.= "TIME=$time\n";
	
		$html = "<html><body>A java exception has been thrown by CINA.<br>";
		$html.= "EXCEPTION_INDEX=$exception_index<br>";
		$html.= "STACK_TRACE=$stack_trace<br>";
		$html.= "TIME=$time<br>";
		$html.= "</body><html>";

		$subject = "CINA Java Exception";
		return Email::send_email(Email::$CINA_DEV_EMAIL, $subject, $text, $html);
	}
	
	public static function send_email($to, $subject, $text, $html, $file_array=array()){
		
		$headers["From"] = $to;
		$headers["To"] = $to;
		$headers["Subject"] = $subject;
		$headers["Date"] = date("r");
		$headers["X-Mailer"] = 'PHP/'.phpversion();
		$headers["Message-id"] = sha1(mt_rand());
		$headers["Reply-To"] = $to;
		
		$mime = new Mail_mime("\n");
		$mime->setTXTBody($text);
		$mime->setHTMLBody($html);
		foreach($file_array as $file){
			$mime->addAttachment($file);
		}
		$body = $mime->get();
		$headers = $mime->headers($headers);
		
		$mail =& Mail::factory('mail');
		return $mail->send($to, $headers, $body);

	}
}
?>
