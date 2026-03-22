/*! /file
 * \verbatim
 * $Author: bucknerk $
 * $Date: 2007/11/09 16:42:08 $
 * $Id: mysha1.java,v 1.1 2007/11/09 16:42:08 bucknerk Exp $
 *
 * $Log: mysha1.java,v $
 * Revision 1.1  2007/11/09 16:42:08  bucknerk
 * finally adding it
 *
 *
 * \endverbatim
 *
 * This was taken almost completely from the web, found it in several places so
 * I don't really know who came up with it.  But it works an gives a consistent
 * output that also works with Eric's code (go figure).
 * I simply compile it and hang it in the main cina directory where the
 * site_admin code can find it.
 */
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
 
 
/*!
 * The main class validates the arguments and passes the first one to the sha1
 * computation class
 */
public class mysha1 {

  public static void main(String args[]) throws IOException {
    if(args.length != 1) {
      System.out.println("ERROR");
      return;
    }
    try {
       System.out.print( SHA1(args[0]).trim()); }
    catch (NoSuchAlgorithmException e) {
       e.printStackTrace(); }
    catch (UnsupportedEncodingException e) {
       e.printStackTrace();
       }
     }

 
  /*!
   * This class only takes the message digest output and makes it readable.
   * This is because the output of the messagedigest is not encoded as 'normal'
   * password would be.
   */
   
  private static String convertToHex(byte[] data) {
     StringBuffer buf = new StringBuffer();
     for (int i = 0; i < data.length; i++) {
	 int halfbyte = (data[i] >>> 4) & 0x0F;
	 int two_halfs = 0;
       do {
	if ((0 <= halfbyte) && (halfbyte <= 9))
	  buf.append((char) ('0' + halfbyte));
	else
	  buf.append((char) ('a' + (halfbyte - 10)));
	  halfbyte = data[i] & 0x0F;
	}
       while(two_halfs++ < 1);
       }
      return buf.toString();
  }
   
  /*!
   * This does the encoded by using the sha1 algorithm and treating the input
   * text as though it were an email message, hence the name 
   */
  public static String SHA1(String text) 
	throws NoSuchAlgorithmException,
       UnsupportedEncodingException  
  {
    MessageDigest md;
    md = MessageDigest.getInstance("SHA-1");
    byte[] sha1hash = new byte[40];
    md.update(text.getBytes("iso-8859-1"), 0, text.length());
    sha1hash = md.digest();
    return convertToHex(sha1hash);
    }
}
