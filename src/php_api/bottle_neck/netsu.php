#!/usr/bin/php -q
<?php
/****************************************************************************/
/*! \file netsu.php
 * \verbatim
 *
 * $Author: bucknerk $
 * $Date: 2008/03/25 18:09:30 $
 * $Id: netsu.php,v 1.2 2008/03/25 18:09:30 bucknerk Exp $
 *
 * $Log: netsu.php,v $
 * Revision 1.2  2008/03/25 18:09:30  bucknerk
 * Modified to create a 'reverse' version, that is the fluxes are for the
 * reactions that become the mass not that start from the mass
 *
 * Revision 1.1  2008/03/20 18:52:03  bucknerk
 * new files
 *
 * Revision 1.3  2008/02/25 20:48:06  bucknerk
 * minor changes
 *
 * Revision 1.2  2007/12/03 17:48:50  bucknerk
 * merged with branch cina-rel-1-0
 *
 * Revision 1.1.2.2  2007/12/03 13:30:18  bucknerk
 * finally fixed the segfault.  typically was freeing already freed memory. 
 * Cut and paste strikes again. Also updated some documentation.
 *
 * Revision 1.1.2.1  2007/11/28 18:35:25  bucknerk
 * Renamed craz_list list_crazy.dat, updated the makefile for the create_input
 * files and added all those create_input files.
 *
 *
 * \endverbatim
 *
 * This file is used to open the netsu file created by the element synthesis
 * suite.  I am using php because is the much easier to write string parsing
 * code than with C. 
 *
 * For now, we are skipping the metastable form of Al26.  It actually has
 * completely different reactions than the stable isotope.  The problem is that
 * it is still Z=13,N=13.  Tomomi's code is not prepared to handle it yet.
 * Eventually we will probably change this to Al99 or something.
 *
 * \param[in] argv[1],the base directory in which to find the netsu file.
 *
 * \output The type 1,2,4 and 5 reactions from the netsu file in a slightly
 * different format for the create_input program .
 ****************************************************************************/

$fp=fopen($argv[1] . "/netsu","rb");
if(!$fp) {
  echo "ERROR=Could not open " . $argv[1] .  "/netsu\n";
  exit(1);
  }
$line_num=1;
$line=fgets($fp,128); // 
$line=fgets($fp,128); // 
$index=0;
while($line) {
  if(strlen(trim($line)) != 0) {
    switch ($line_num%3) {
      case 1:
	if($line[0] == '0') {

	  $newtype=false;
	  $current_type[$index]=$this_type;
	  for($i=0;$i<7;$i++) {
	    $iso[$index][$i] = substr($line,$i*5+6,5);
	    }
	  $ref[$index]=substr($line,43,4);
	  // I could skip the next two
	  $res[$index]=$line[47];
	  $inv[$index]=$line[48];
	  $q_val[$index]=substr($line,52,12);
	  //echo "\t$index: {$q_val[$index]}\n";
	  }
	elseif($line[0] > 0) {
	  $tmparr=explode(" ",$line);
	  $current_type[$index]=$tmparr[0];
	  $this_type=$tmparr[0];
	  $newtype=true;
	  //echo "Set current type to {$current_type[$index]}\n";
	  }
	else { 
	  echo "ERROR=line $line_num: Unknown line format: $line\n";
	  $newtype=true;
	  }
	break;
      case 2:
	if($newtype) break;
	for($i=0;$i<4;$i++) {
	  $coeff[$index][$i]=substr($line,$i*13,13);
	  //echo "$index:$i:{$coeff[$index][$i]}\n";
	  }
	break;
      case 0:
	if($newtype) break;
	for($i=4;$i<7;$i++) {
	  $coeff[$index][$i]=substr($line,($i-4)*13,13);
	  //echo "$index:$i:{$coeff[$index][$i]}\n";
	  }
	$index++;
	break;
	}
      $line_num++;
      }
    $line=fgets($fp,128);
  }
fclose($fp);
//echo $line_num . "::" . $index . "\n";
//exit(0);
// NOTE: the continue '2' means break out of the second level of enclosing
// block. This is necessay because unlike C where 'continue' is only meaningful
// in a loop context, php's 'continue' acts as a 'break' from within a switch.
$previous="";
for($i=0;$i<$index;$i++) {
  if(isset($current_type[$i])) {
  switch($current_type[$i]) {
    case 1: // a --> b
      //echo $ref[$i] . "\n";
      if($ref[$i]=="bet+" || trim($ref[$i])=="bec" ||
          trim($ref[$i])=="ec" || $ref[$i] == "btyk") {
	$a=trim($iso[$i][0]);
	if($a=="al*6") continue 2; // the metastable version
	if($a=="al-6") $a="al26";
	$b=trim($iso[$i][1]);
	if($b=="al*6") continue 2; // the metastable version
	if($b=="al-6") $b="al26";
	if(strlen($a)> 1) {
	  $reac_str=ucfirst($a);
	  $prime=$reac_str;
	  }
	else {
	  $reac_str=$a;
	  $prime=$reac_str;
	  }
	$reac_str.= " --> ";
	if(strlen($b)> 1) {
	  $reac_str.=ucfirst($b);
	  }
	else {
	  $reac_str.=$b;
	  }
	}
      else { // if not beta + rate skip it
	continue 2;
	}
      break;
    case 2: // a --> b + c
      $a=trim($iso[$i][0]);
      if($a=="al*6") continue 2; // the metastable version
      if($a=="al-6") $a="al26";
      $b=trim($iso[$i][1]);
      if($b != "p" && $b != "he4") continue 2;
      if($b == "p") {
	$current_type[$i]="2p";
	}
      else {
	$current_type[$i]="2a";
	}
      $c=trim($iso[$i][2]);
      if($c=="al*6") continue 2;
      if($c=="al-6") $c="al26";
      if(strlen($a)> 1) {
	$reac_str=ucfirst($a);
	$prime=$reac_str;
	}
      else {
	$reac_str=$a;
	$prime=$reac_str;
	}
      $reac_str.= " --> ";
      if(strlen($b)> 1) {
	$reac_str.=ucfirst($b);
	}
      else {
	$reac_str.=$b;
	}
      if(strlen($c)> 1) {
	$reac_str.= " + " . ucfirst($c);
	}
      else {
	$reac_str.= " + $c";
	}
      break;
    case 4: // a + b --> c
      $a=trim($iso[$i][0]);
      if($a != "p" && $a != "he4") continue 2;
      if($a == "p") {
	$current_type[$i]="4p";
	}
      else {
	$current_type[$i]="4a";
	}
      $b=trim($iso[$i][1]);
      if($b=="al*6") continue 2;
      if($b=="al-6") $b="al26";
      $c=trim($iso[$i][2]);
      if($c=="al*6") continue 2;
      if($c=="al-6") $c="al26";
      if(strlen($a)> 1) {
	$reac_str=ucfirst($a);
	}
      else {
	$reac_str=$a;
	}
      if(strlen($b)> 1) {
	$reac_str.= " + " . ucfirst($b);
	$prime=ucfirst($b);
	}
      else {
	$reac_str.= " + $b";
	$prime=$b;
	}
      $reac_str.= " --> ";
      if(strlen($c)> 1) {
	$reac_str.=ucfirst($c);
	}
      else {
	$reac_str.=$c;
	}
      break;
    case 5: // a + b --> c + d
      $a=trim($iso[$i][0]);
      $c=trim($iso[$i][2]);
      if($a == "p" && $c == "he4") {
	$current_type[$i]="5p";
	}
      elseif($a == "he4" && $c == "p") {
	$current_type[$i]="5a";
	}
      else continue 2;
      $b=trim($iso[$i][1]);
      if($b=="al*6")  continue 2;
      if($b=="al-6") $b="al26";
      $d=trim($iso[$i][3]);
      if($d=="al*6") continue 2;
      if($d=="al-6") $d="al26";
      if(strlen($a)> 1) {
	$reac_str=ucfirst($a);
	}
      else {
	$reac_str=$a;
	}
      if(strlen($b)> 1) {
	$reac_str.= " + " . ucfirst($b);
	$prime=ucfirst($b);
	}
      else {
	$reac_str.= " + $b";
	$prime=$b;
	}
      $reac_str.= " --> ";
      if(strlen($c)> 1) {
	$reac_str.=ucfirst($c);
	}
      else {
	$reac_str.=$c;
	}
      if(strlen($d)> 1) {
	$reac_str.= " + " . ucfirst($d);
	}
      else {
	$reac_str.= " + $d";
	}
      break;
    default:
      continue 2;
    }
  // should skip duplicates
  if($previous != $reac_str ) {
    $previous=$reac_str;
    echo "\n"; // means a new reaction is starting
    echo $current_type[$i] . "\n";
    echo "$reac_str\n";
    echo "$prime\n";
    echo $q_val[$i] . "\n";
    echo trim($ref[$i]) . "\n";
    }
    echo implode(",",$coeff[$i]) . "\n";
  }
}
exit(0);
?>

  

  






