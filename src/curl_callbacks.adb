with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with Ada.Real_Time;
with Ada.Direct_IO;
with Ada.Directories;
with Unix;

package body curl_callbacks is

   package RT  renames Ada.Real_Time;
   package STM renames Ada.Streams;
   package FIX renames Ada.Strings.Fixed;

   ------------------
   --  write_file  --
   ------------------
   function write_file (ptr      : IC.Strings.chars_ptr;
                        size     : IC.size_t;
                        nmemb    : IC.size_t;
                        userdata : System.Address) return IC.size_t
   is
      bytes_passed : constant Natural := Natural (nmemb);
      zdata : curldata;
      for zdata'Address use userdata;
      pragma Import (Ada, zdata);
   begin
      if bytes_passed = 0 then
         return IC.size_t (0);
      end if;

      declare
         subtype cdatatype is IC.char_array (1 .. nmemb);
         subtype adatatype is STM.Stream_Element_Array (1 .. STM.Stream_Element_Offset (nmemb));

         function char_to_stream is new Ada.Unchecked_Conversion
           (Source => cdatatype,
            Target => adatatype);

         cdata : cdatatype := IC.Strings.Value (ptr, nmemb);
         adata : adatatype := char_to_stream (cdata);

      begin
         SIO.Write (zdata.file_handle, adata);
      end;

      zdata.totalsize := zdata.totalsize + bytes_passed;
      return nmemb;

   end write_file;


   ----------------------
   --  process_header  --
   ----------------------
   function process_header
     (ptr      : IC.Strings.chars_ptr;
      size     : IC.size_t;
      nmemb    : IC.size_t;
      userdata : System.Address) return IC.size_t
   is
      zdata : curldata;
      for zdata'Address use userdata;
      pragma Import (Ada, zdata);
   begin
      declare
         subtype cdatatype is IC.char_array (1 .. nmemb);

         cdata : constant String := IC.Strings.Value (ptr, nmemb);
         delim : Natural;
         equalsign : Natural;
      begin
         --  Ada.Text_IO.Put_Line (cdata (cdata'First .. cdata'Last - 2));
         delim := FIX.index (cdata, ":");
         if delim > 0 then
            if cdata (cdata'First .. delim) = "Cache-Control:" then
               equalsign := FIX.index (cdata, "=", delim);
               if equalsign > 0 then
                  if cdata (cdata'First .. equalsign) = "Cache-Control: max-age=" then
                     begin
                        zdata.max_age := Natural'Value (cdata (equalsign + 1 .. cdata'Last - 2));
                        Ada.Text_IO.Put_Line ("max age =" & zdata.max_age'Img);
                     exception
                        when others => null;
                     end;
                  end if;
               end if;
            elsif cdata (cdata'First .. delim) = "ETag:" then
               write_etag_file (ASU.To_String (zdata.etag_file), extract_etag (cdata));
            end if;
         end if;
      end;

      return nmemb;
   end process_header;


   --------------------
   --  extract_etag  --
   --------------------
   function extract_etag (raw_etag : String) return String
   is
      myquote : constant String (1 .. 1) := (1 => '"');
      first_quote : Natural;
      second_quote : Natural;
   begin
      first_quote := FIX.index (raw_etag, myquote);
      if first_quote > 0 then
         second_quote := FIX.index (raw_etag, myquote, first_quote + 1);
         if second_quote > 0 then
            return raw_etag (first_quote + 1 .. second_quote - 1);
         end if;
      end if;
      return "";
   end extract_etag;


   -----------------------
   --  write_etag_file  --
   -----------------------
   procedure write_etag_file (filename : String; etag : String)
   is
      File_Size : constant Natural := etag'Length;

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      file_handle : File_String_IO.File_Type;
   begin
      File_String_IO.Create (File => file_handle,
                             Mode => File_String_IO.Out_File,
                             Name => filename);
      File_String_IO.Write  (File => file_handle,
                             Item => etag);
      File_String_IO.Close  (file_handle);
   exception
      when Storage_Error =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
      when others =>
         if File_String_IO.Is_Open (file_handle) then
            File_String_IO.Close (file_handle);
         end if;
   end write_etag_file;


   -----------------------
   --  found_etag_file  --
   -----------------------
   function found_etag_file (filename : String) return Boolean
   is
      filetc : Unix.Time_Characteristics;
   begin
      filetc := Unix.get_time_characteristics (filename);
      case filetc.ftype is
         when Unix.regular =>
            return True;
         when others =>
            null;
      end case;
      return False;
   end found_etag_file;


   --------------------------
   --  target_file_cached  --
   --------------------------
   function target_file_cached (target_file : String; etag_file : String) return Boolean
   is
      target : Unix.Time_Characteristics;
      filetc : Unix.Time_Characteristics;
   begin
      target := Unix.get_time_characteristics (target_file);
      case target.ftype is
         when Unix.regular =>
            filetc := Unix.get_time_characteristics (etag_file);
            case filetc.ftype is
               when Unix.regular =>
                  return not Unix.tag_expired (filetc.mtime);
               when others =>
                  null;
            end case;
         when others =>
            null;
      end case;
      return False;
   end target_file_cached;


   ------------------
   --  saved_etag  --
   ------------------
   function saved_etag (filename : String) return String
   is
      filesize : constant Natural := Natural (Ada.Directories.Size (filename));
   begin
      declare
         subtype File_String    is String (1 .. filesize);
         package File_String_IO is new Ada.Direct_IO (File_String);
         File     : File_String_IO.File_Type;
         Contents : File_String;
      begin
         File_String_IO.Open (File => File,
                              Mode => File_String_IO.In_File,
                              Name => filename);
         File_String_IO.Read (File => File,
                              Item => Contents);
         File_String_IO.Close (File);
         return Contents;
      exception
         when others =>
            if File_String_IO.Is_Open (File) then
               File_String_IO.Close (File);
            end if;
            return "";
      end;
   end saved_etag;


   ---------------------------
   --  set_expiration_time  --
   ---------------------------
   function set_expiration_time (path : String; max_age : Natural) return Boolean
   is
      use type Unix.filetime;

      filetc : Unix.Time_Characteristics;
      rc     : Unix.metadata_rc;
   begin
      filetc := Unix.get_time_characteristics (path);
      case filetc.ftype is
         when Unix.regular =>
            rc := Unix.adjust_metadata
              (path         => path,
               reset_owngrp => False,
               reset_perms  => False,
               reset_mtime  => True,
               type_of_file => filetc.ftype,
               new_uid      => 0,
               new_gid      => 0,
               new_perms    => 0,
               new_m_secs   => filetc.mtime + Unix.filetime (max_age),
               new_m_nano   => filetc.mnsec);
            case rc is
               when 0 => return True;
               when others => null;
            end case;
         when others => null;
      end case;
      return False;
   end set_expiration_time;


   ----------------------------------
   --  randomized_download_target  --
   ----------------------------------
   function randomized_download_target (true_path : String) return String
   is
      right_now : constant RT.Time := RT.Clock;
      seconds   : RT.Seconds_Count;
      nanospan  : RT.Time_Span;
      nduration : Duration;
   begin
      RT.Split (right_now, seconds, nanospan);
      nduration := RT.To_Duration (nanospan);
      declare
         durstr : constant String := nduration'Img;
      begin
         --  durstr in format " 0.xxxxxxxxx" (leading space)
         return true_path & durstr (durstr'First + 2 .. durstr'Last);
      end;
   end randomized_download_target;


   -----------------------------
   --  rename_temporary_file  --
   -----------------------------
   procedure rename_temporary_file (true_path, temporary_path : String) is
   begin
      if Ada.Directories.Exists (true_path) then
         begin
            Ada.Directories.Delete_File (true_path);
         exception
            when others => return;
         end;
      end if;
      if Ada.Directories.Exists (temporary_path) then
         begin
            Ada.Directories.Rename (temporary_path, true_path);
         exception
            when others => return;
         end;
      end if;
   end rename_temporary_file;

end curl_callbacks;
