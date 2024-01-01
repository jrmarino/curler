with Ada.Text_IO;

package body curl_header is

   package TIO renames Ada.Text_IO;

   --------------------------
   --  set_curl_option #1  --
   --------------------------
   procedure set_curl_option (curlobj : CURLX; option : OptionString; optvalue : String)
   is
      result    : CURLcode;
      coptvalue : IC.Strings.chars_ptr;
   begin
      coptvalue := IC.Strings.New_String (optvalue);
      result    := curl_setopt_string (curlobj, option , coptvalue);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set " & option'Img & " (" & optvalue & ")");
      end case;
      IC.Strings.Free (coptvalue);
   end set_curl_option;


   --------------------------
   --  set_curl_option #2  --
   --------------------------
   procedure set_curl_option (curlobj : CURLX; option : OptionLong; optvalue : Long_Integer)
   is
      result    : CURLcode;
      coptvalue : IC.long;
   begin
      coptvalue := IC.long (optvalue);
      result    := curl_setopt_long (curlobj, option, coptvalue);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set " & option'Img & " (" & optvalue'Img & ")");
      end case;
   end set_curl_option;


   --------------------------
   --  set_curl_option #3  --
   --------------------------
   procedure set_curl_option (curlobj : CURLX; option : OptionBool; optvalue : Boolean)
   is
      result : CURLcode;
      coptvalue : IC.long := 0;
   begin
      if optvalue then
         coptvalue := 1;
      end if;
      result := curl_setopt_bool (curlobj, option, coptvalue);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set " & option'Img & " (" & optvalue'Img & ")");
      end case;
   end set_curl_option;


   --------------------------
   --  set_curl_option #4  --
   --------------------------
   procedure set_curl_option (curlobj : CURLX; option : OptionPointer; optvalue : Void_Ptr)
   is
      result : CURLcode;
   begin
      result := curl_setopt_pointer (curlobj, option, optvalue);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set " & option'Img);
      end case;
   end set_curl_option;


   --------------------
   --  execute_curl  --
   --------------------
   procedure execute_curl (curlobj : CURLX)
   is
      result : CURLcode;
   begin
      result := curl_easy_perform (curlobj);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to execute curl");
      end case;
   end execute_curl;


   --------------------------
   --  set_write_callback  --
   --------------------------
   procedure set_write_callback (curlobj : CURLX; callback : write_callback)
   is
      result : CURLcode;
   begin
      result := curl_setopt_write_callback (curlobj, CURLOPT_WRITEFUNCTION, callback);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set set_write_callback");
      end case;
   end set_write_callback;


   -------------------------
   --  set_read_callback  --
   -------------------------
   procedure set_read_callback (curlobj : CURLX; callback : write_callback)
   is
      result : CURLcode;
   begin
      result := curl_setopt_write_callback (curlobj, CURLOPT_READFUNCTION, callback);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set set_read_callback");
      end case;
   end set_read_callback;


   ---------------------------
   --  set_header_callback  --
   ---------------------------
   procedure set_header_callback (curlobj : CURLX; callback : write_callback)
   is
      result : CURLcode;
   begin
      result := curl_setopt_write_callback (curlobj, CURLOPT_HEADERFUNCTION, callback);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set set_header_callback");
      end case;
   end set_header_callback;


   --------------------------
   --  set_debug_callback  --
   --------------------------
   procedure set_debug_callback (curlobj : CURLX; callback : debug_callback)
   is
      result : CURLcode;
   begin
      result := curl_setopt_debug_callback (curlobj, CURLOPT_DEBUGFUNCTION, callback);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set set_debug_callback");
      end case;
   end set_debug_callback;


   -----------------------------
   --  set_progress_callback  --
   -----------------------------
   procedure set_progress_callback (curlobj : CURLX; callback : progress_callback)
   is
      result : CURLcode;
   begin
      result := curl_setopt_progress_callback (curlobj, CURLOPT_XFERINFOFUNCTION, callback);
      case result is
         when CURLE_OK => null;
         when others => TIO.Put_Line ("Failed to set set_progress_callback");
      end case;
   end set_progress_callback;

end curl_header;
