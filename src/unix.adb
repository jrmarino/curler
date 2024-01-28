package body Unix is

   ------------------------------
   --  file_modification_time  --
   ------------------------------
   function file_modification_time (sb : struct_stat_Access) return time_specification
   is
      cres : timespec;
      result : time_specification;
   begin
      cres := arc_get_mtime (sb);
      result.epoch := filetime (cres.tv_sec);
      result.nsecs := nanoseconds (cres.tv_nsec);
      return result;
   end file_modification_time;


   ---------------
   --  success  --
   ---------------
   function success (rc : IC.int) return Boolean
   is
      use type IC.int;
   begin
      return (rc = IC.int (0));
   end success;


   ---------------
   --  stat_ok  --
   ---------------
   function stat_ok (path : String; sb : struct_stat_Access) return Boolean
   is
      c_path : IC.Strings.chars_ptr;
      res    : IC.int;
   begin
      c_path := IC.Strings.New_String (path);
      res := arc_stat (c_path, sb);
      IC.Strings.Free (c_path);
      return success (res);
   end stat_ok;


   --------------------
   --  type_of_file  --
   --------------------
   function type_of_file (sb : struct_stat_Access) return file_type
   is
      res : constant IC.unsigned_char := arc_get_file_type (sb);
   begin
      case res is
         when 1 => return directory;
         when 2 => return regular;
         when 3 => return symlink;
         when 4 => return hardlink;
         when 5 => return fifo;
         when others => return unsupported;
      end case;
   end type_of_file;


   ------------------------------
   --  get_time_charactistics  --
   ------------------------------
   function get_time_characteristics (path : String) return Time_Characteristics
   is
      result : Time_Characteristics;
      sb     : aliased Unix.struct_stat;
      tspec  : time_specification;
   begin
      result.ftype := unsupported;
      result.mtime := 0;
      result.mnsec := 0;
      result.error := True;
      begin
         if Unix.stat_ok (path, sb'Unchecked_Access) then
            tspec := file_modification_time (sb'Unchecked_Access);
            result.ftype := type_of_file (sb'Unchecked_Access);
            result.mtime := tspec.epoch;
            result.mnsec := tspec.nsecs;
            result.error := False;
         end if;
      exception
         when others =>
            null;
      end;
      return result;
   end get_time_characteristics;


   -------------------
   --  tag_expired  --
   -------------------
   function tag_expired (mtime : filetime) return Boolean
   is
      current_time : filetime := arc_time (null);
   begin
      return current_time > mtime;
   end tag_expired;


   -----------------------
   --  adjust_metadata  --
   -----------------------
   function adjust_metadata
     (path         : String;
      reset_owngrp : Boolean;
      reset_perms  : Boolean;
      reset_mtime  : Boolean;
      type_of_file : file_type;
      new_uid      : owngrp_id;
      new_gid      : owngrp_id;
      new_perms    : permissions;
      new_m_secs   : filetime;
      new_m_nano   : nanoseconds) return metadata_rc
   is
      cpath     : constant IC.char_array := IC.To_C (path);
      do_owner  : IC.unsigned_char := 0;
      do_perms  : IC.unsigned_char := 0;
      do_mtime  : IC.unsigned_char := 0;
      rescode   : IC.unsigned_char := 0;
      c_uid     : IC.unsigned := 0;
      c_gid     : IC.unsigned := 0;
      c_perms   : IC.short := 0;
   begin
      if reset_owngrp then
         do_owner := 1;
         c_uid := IC.unsigned (new_uid);
         c_gid := IC.unsigned (new_gid);
      end if;
      if reset_perms then
         do_perms := 1;
         c_perms := IC.short (new_perms);
      end if;
      if reset_mtime then
         do_mtime := 1;
      end if;
      case type_of_file is
         when regular | directory =>
            rescode := set_metadata
              (path              => cpath,
               reset_modtime     => do_mtime,
               reset_ownership   => do_owner,
               reset_permissions => do_perms,
               new_user_id       => c_uid,
               new_group_id      => c_gid,
               new_permissions   => c_perms,
               new_mtime_epoch   => time_t (new_m_secs),
               new_mtime_nsecs   => IC.long (new_m_nano));
         when unsupported | hardlink | fifo | symlink =>
            --  Skip hardlinks.  This is set on the target.
            --  Doing it again is redundant
            null;
      end case;
      return metadata_rc (rescode);
   end adjust_metadata;


end Unix;
