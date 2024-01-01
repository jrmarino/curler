with Ada.Command_Line;
with Ada.Text_IO;
with curl_header;

procedure Curler is

   package CLI renames Ada.Command_Line;
   package TIO renames Ada.Text_IO;

   curlobj : curl_header.CURLX;
begin

   if CLI.Argument_Count < 1 then
      TIO.Put_Line ("pass a URL as an argument");
      return;
   end if;

   curlobj := curl_header.curl_easy_init;
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, True);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, CLI.Argument (1));
   curl_header.execute_curl (curlobj);


   curl_header.curl_easy_cleanup (curlobj);
end Curler;
