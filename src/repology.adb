with Ada.Text_IO;
with System;
with curl_header;
with curl_callbacks;

procedure repology
is
   package CAL renames curl_callbacks;
   package TIO renames Ada.Text_IO;

   latest : constant String := "https://raw.githubusercontent.com/Ravenports/Ravenports/" &
                               "master/Mk/Misc/repology.json";

   data : CAL.curldata;
   curlobj : curl_header.CURLX;

begin

   data.totalsize := 0;
   CAL.SIO.Create (data.file_handle, CAL.SIO.In_File, "/tmp/repology.json");

   curlobj := curl_header.curl_easy_init;
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_VERBOSE, False);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_NOPROGRESS, True);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_URL, latest);
   curl_header.set_curl_option (curlobj, curl_header.CURLOPT_WRITEDATA, data'Address);
   curl_header.set_write_callback (curlobj, CAL.write_file'Access);
   curl_header.execute_curl (curlobj);

   CAL.SIO.Close (data.file_handle);

end repology;
