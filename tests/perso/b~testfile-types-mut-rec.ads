pragma Ada_95;
with System;
package ada_main is
   pragma Warnings (Off);

   gnat_argc : Integer;
   gnat_argv : System.Address;
   gnat_envp : System.Address;

   pragma Import (C, gnat_argc);
   pragma Import (C, gnat_argv);
   pragma Import (C, gnat_envp);

   gnat_exit_status : Integer;
   pragma Import (C, gnat_exit_status);

   GNAT_Version : constant String :=
                    "GNAT Version: 4.9.3" & ASCII.NUL;
   pragma Export (C, GNAT_Version, "__gnat_version");

   Ada_Main_Program_Name : constant String := "_ada_p" & ASCII.NUL;
   pragma Export (C, Ada_Main_Program_Name, "__gnat_ada_main_program_name");

   procedure adainit;
   pragma Export (C, adainit, "adainit");

   procedure adafinal;
   pragma Export (C, adafinal, "adafinal");

   function main
     (argc : Integer;
      argv : System.Address;
      envp : System.Address)
      return Integer;
   pragma Export (C, main, "main");

   type Version_32 is mod 2 ** 32;
   u00001 : constant Version_32 := 16#983d3fcb#;
   pragma Export (C, u00001, "pB");
   u00002 : constant Version_32 := 16#a964624f#;
   pragma Export (C, u00002, "system__standard_libraryB");
   u00003 : constant Version_32 := 16#c15e0628#;
   pragma Export (C, u00003, "system__standard_libraryS");
   u00004 : constant Version_32 := 16#3ffc8e18#;
   pragma Export (C, u00004, "adaS");
   u00005 : constant Version_32 := 16#9a3f0a9b#;
   pragma Export (C, u00005, "ada__text_ioB");
   u00006 : constant Version_32 := 16#27578d68#;
   pragma Export (C, u00006, "ada__text_ioS");
   u00007 : constant Version_32 := 16#e9502879#;
   pragma Export (C, u00007, "ada__exceptionsB");
   u00008 : constant Version_32 := 16#e1be92cd#;
   pragma Export (C, u00008, "ada__exceptionsS");
   u00009 : constant Version_32 := 16#51ba2b93#;
   pragma Export (C, u00009, "ada__exceptions__last_chance_handlerB");
   u00010 : constant Version_32 := 16#909606f8#;
   pragma Export (C, u00010, "ada__exceptions__last_chance_handlerS");
   u00011 : constant Version_32 := 16#90249111#;
   pragma Export (C, u00011, "systemS");
   u00012 : constant Version_32 := 16#77a147eb#;
   pragma Export (C, u00012, "system__soft_linksB");
   u00013 : constant Version_32 := 16#6144caac#;
   pragma Export (C, u00013, "system__soft_linksS");
   u00014 : constant Version_32 := 16#65bb1402#;
   pragma Export (C, u00014, "system__parametersB");
   u00015 : constant Version_32 := 16#3ba8257b#;
   pragma Export (C, u00015, "system__parametersS");
   u00016 : constant Version_32 := 16#643ddf46#;
   pragma Export (C, u00016, "system__secondary_stackB");
   u00017 : constant Version_32 := 16#3b455e78#;
   pragma Export (C, u00017, "system__secondary_stackS");
   u00018 : constant Version_32 := 16#39a03df9#;
   pragma Export (C, u00018, "system__storage_elementsB");
   u00019 : constant Version_32 := 16#bde7db15#;
   pragma Export (C, u00019, "system__storage_elementsS");
   u00020 : constant Version_32 := 16#41837d1e#;
   pragma Export (C, u00020, "system__stack_checkingB");
   u00021 : constant Version_32 := 16#1e9bfaf9#;
   pragma Export (C, u00021, "system__stack_checkingS");
   u00022 : constant Version_32 := 16#7ff7f3a3#;
   pragma Export (C, u00022, "system__exception_tableB");
   u00023 : constant Version_32 := 16#6ca6d92c#;
   pragma Export (C, u00023, "system__exception_tableS");
   u00024 : constant Version_32 := 16#c3335bfd#;
   pragma Export (C, u00024, "system__htableB");
   u00025 : constant Version_32 := 16#14e622fb#;
   pragma Export (C, u00025, "system__htableS");
   u00026 : constant Version_32 := 16#089f5cd0#;
   pragma Export (C, u00026, "system__string_hashB");
   u00027 : constant Version_32 := 16#b6b84985#;
   pragma Export (C, u00027, "system__string_hashS");
   u00028 : constant Version_32 := 16#9e373b74#;
   pragma Export (C, u00028, "system__exceptionsB");
   u00029 : constant Version_32 := 16#a83a37b3#;
   pragma Export (C, u00029, "system__exceptionsS");
   u00030 : constant Version_32 := 16#cd9019cf#;
   pragma Export (C, u00030, "system__exceptions__machineS");
   u00031 : constant Version_32 := 16#a2eb6533#;
   pragma Export (C, u00031, "system__exceptions_debugB");
   u00032 : constant Version_32 := 16#6b2380a9#;
   pragma Export (C, u00032, "system__exceptions_debugS");
   u00033 : constant Version_32 := 16#570325c8#;
   pragma Export (C, u00033, "system__img_intB");
   u00034 : constant Version_32 := 16#92ff71d3#;
   pragma Export (C, u00034, "system__img_intS");
   u00035 : constant Version_32 := 16#ff5c7695#;
   pragma Export (C, u00035, "system__tracebackB");
   u00036 : constant Version_32 := 16#b8200e4c#;
   pragma Export (C, u00036, "system__tracebackS");
   u00037 : constant Version_32 := 16#8c33a517#;
   pragma Export (C, u00037, "system__wch_conB");
   u00038 : constant Version_32 := 16#8b59b3c3#;
   pragma Export (C, u00038, "system__wch_conS");
   u00039 : constant Version_32 := 16#9721e840#;
   pragma Export (C, u00039, "system__wch_stwB");
   u00040 : constant Version_32 := 16#a6489fc2#;
   pragma Export (C, u00040, "system__wch_stwS");
   u00041 : constant Version_32 := 16#9b29844d#;
   pragma Export (C, u00041, "system__wch_cnvB");
   u00042 : constant Version_32 := 16#84ee0930#;
   pragma Export (C, u00042, "system__wch_cnvS");
   u00043 : constant Version_32 := 16#69adb1b9#;
   pragma Export (C, u00043, "interfacesS");
   u00044 : constant Version_32 := 16#ece6fdb6#;
   pragma Export (C, u00044, "system__wch_jisB");
   u00045 : constant Version_32 := 16#049e1011#;
   pragma Export (C, u00045, "system__wch_jisS");
   u00046 : constant Version_32 := 16#8cb17bcd#;
   pragma Export (C, u00046, "system__traceback_entriesB");
   u00047 : constant Version_32 := 16#2535f183#;
   pragma Export (C, u00047, "system__traceback_entriesS");
   u00048 : constant Version_32 := 16#1b5643e2#;
   pragma Export (C, u00048, "ada__streamsB");
   u00049 : constant Version_32 := 16#2564c958#;
   pragma Export (C, u00049, "ada__streamsS");
   u00050 : constant Version_32 := 16#db5c917c#;
   pragma Export (C, u00050, "ada__io_exceptionsS");
   u00051 : constant Version_32 := 16#08ba48f3#;
   pragma Export (C, u00051, "ada__tagsB");
   u00052 : constant Version_32 := 16#ee1e5cdd#;
   pragma Export (C, u00052, "ada__tagsS");
   u00053 : constant Version_32 := 16#c12203be#;
   pragma Export (C, u00053, "system__unsigned_typesS");
   u00054 : constant Version_32 := 16#1e25d3f1#;
   pragma Export (C, u00054, "system__val_lluB");
   u00055 : constant Version_32 := 16#bbd054cc#;
   pragma Export (C, u00055, "system__val_lluS");
   u00056 : constant Version_32 := 16#27b600b2#;
   pragma Export (C, u00056, "system__val_utilB");
   u00057 : constant Version_32 := 16#3c8427ef#;
   pragma Export (C, u00057, "system__val_utilS");
   u00058 : constant Version_32 := 16#d1060688#;
   pragma Export (C, u00058, "system__case_utilB");
   u00059 : constant Version_32 := 16#b42df8c6#;
   pragma Export (C, u00059, "system__case_utilS");
   u00060 : constant Version_32 := 16#e0b7a7e8#;
   pragma Export (C, u00060, "interfaces__c_streamsB");
   u00061 : constant Version_32 := 16#6c8b8ac5#;
   pragma Export (C, u00061, "interfaces__c_streamsS");
   u00062 : constant Version_32 := 16#bfa64d6c#;
   pragma Export (C, u00062, "system__crtlS");
   u00063 : constant Version_32 := 16#ec6e1273#;
   pragma Export (C, u00063, "system__file_ioB");
   u00064 : constant Version_32 := 16#0165f036#;
   pragma Export (C, u00064, "system__file_ioS");
   u00065 : constant Version_32 := 16#8cbe6205#;
   pragma Export (C, u00065, "ada__finalizationB");
   u00066 : constant Version_32 := 16#22e22193#;
   pragma Export (C, u00066, "ada__finalizationS");
   u00067 : constant Version_32 := 16#95817ed8#;
   pragma Export (C, u00067, "system__finalization_rootB");
   u00068 : constant Version_32 := 16#dfd6e281#;
   pragma Export (C, u00068, "system__finalization_rootS");
   u00069 : constant Version_32 := 16#769e25e6#;
   pragma Export (C, u00069, "interfaces__cB");
   u00070 : constant Version_32 := 16#96001448#;
   pragma Export (C, u00070, "interfaces__cS");
   u00071 : constant Version_32 := 16#d0432c8d#;
   pragma Export (C, u00071, "system__img_enum_newB");
   u00072 : constant Version_32 := 16#f16897d1#;
   pragma Export (C, u00072, "system__img_enum_newS");
   u00073 : constant Version_32 := 16#eec8f81b#;
   pragma Export (C, u00073, "system__os_libB");
   u00074 : constant Version_32 := 16#fc501b71#;
   pragma Export (C, u00074, "system__os_libS");
   u00075 : constant Version_32 := 16#1a817b8e#;
   pragma Export (C, u00075, "system__stringsB");
   u00076 : constant Version_32 := 16#ee9b8077#;
   pragma Export (C, u00076, "system__stringsS");
   u00077 : constant Version_32 := 16#da3df06e#;
   pragma Export (C, u00077, "system__file_control_blockS");
   u00078 : constant Version_32 := 16#1f8826cb#;
   pragma Export (C, u00078, "system__finalization_mastersB");
   u00079 : constant Version_32 := 16#df27fd08#;
   pragma Export (C, u00079, "system__finalization_mastersS");
   u00080 : constant Version_32 := 16#57a37a42#;
   pragma Export (C, u00080, "system__address_imageB");
   u00081 : constant Version_32 := 16#31c80c2b#;
   pragma Export (C, u00081, "system__address_imageS");
   u00082 : constant Version_32 := 16#7268f812#;
   pragma Export (C, u00082, "system__img_boolB");
   u00083 : constant Version_32 := 16#65fde0fa#;
   pragma Export (C, u00083, "system__img_boolS");
   u00084 : constant Version_32 := 16#d7aac20c#;
   pragma Export (C, u00084, "system__ioB");
   u00085 : constant Version_32 := 16#0e66665e#;
   pragma Export (C, u00085, "system__ioS");
   u00086 : constant Version_32 := 16#6d4d969a#;
   pragma Export (C, u00086, "system__storage_poolsB");
   u00087 : constant Version_32 := 16#5e6a53cc#;
   pragma Export (C, u00087, "system__storage_poolsS");
   u00088 : constant Version_32 := 16#e34550ca#;
   pragma Export (C, u00088, "system__pool_globalB");
   u00089 : constant Version_32 := 16#c88d2d16#;
   pragma Export (C, u00089, "system__pool_globalS");
   u00090 : constant Version_32 := 16#6810466c#;
   pragma Export (C, u00090, "system__memoryB");
   u00091 : constant Version_32 := 16#c959f725#;
   pragma Export (C, u00091, "system__memoryS");
   u00092 : constant Version_32 := 16#1220f12d#;
   pragma Export (C, u00092, "system__storage_pools__subpoolsB");
   u00093 : constant Version_32 := 16#b0e8cddc#;
   pragma Export (C, u00093, "system__storage_pools__subpoolsS");
   u00094 : constant Version_32 := 16#aba9f469#;
   pragma Export (C, u00094, "system__storage_pools__subpools__finalizationB");
   u00095 : constant Version_32 := 16#9662cb63#;
   pragma Export (C, u00095, "system__storage_pools__subpools__finalizationS");
   --  BEGIN ELABORATION ORDER
   --  ada%s
   --  interfaces%s
   --  system%s
   --  system.case_util%s
   --  system.case_util%b
   --  system.htable%s
   --  system.img_bool%s
   --  system.img_bool%b
   --  system.img_enum_new%s
   --  system.img_enum_new%b
   --  system.img_int%s
   --  system.img_int%b
   --  system.io%s
   --  system.io%b
   --  system.parameters%s
   --  system.parameters%b
   --  system.crtl%s
   --  interfaces.c_streams%s
   --  interfaces.c_streams%b
   --  system.standard_library%s
   --  system.exceptions_debug%s
   --  system.exceptions_debug%b
   --  system.storage_elements%s
   --  system.storage_elements%b
   --  system.stack_checking%s
   --  system.stack_checking%b
   --  system.string_hash%s
   --  system.string_hash%b
   --  system.htable%b
   --  system.strings%s
   --  system.strings%b
   --  system.os_lib%s
   --  system.traceback_entries%s
   --  system.traceback_entries%b
   --  ada.exceptions%s
   --  system.soft_links%s
   --  system.unsigned_types%s
   --  system.val_llu%s
   --  system.val_util%s
   --  system.val_util%b
   --  system.val_llu%b
   --  system.wch_con%s
   --  system.wch_con%b
   --  system.wch_cnv%s
   --  system.wch_jis%s
   --  system.wch_jis%b
   --  system.wch_cnv%b
   --  system.wch_stw%s
   --  system.wch_stw%b
   --  ada.exceptions.last_chance_handler%s
   --  ada.exceptions.last_chance_handler%b
   --  system.address_image%s
   --  system.exception_table%s
   --  system.exception_table%b
   --  ada.io_exceptions%s
   --  ada.tags%s
   --  ada.streams%s
   --  ada.streams%b
   --  interfaces.c%s
   --  system.exceptions%s
   --  system.exceptions%b
   --  system.exceptions.machine%s
   --  system.finalization_root%s
   --  system.finalization_root%b
   --  ada.finalization%s
   --  ada.finalization%b
   --  system.storage_pools%s
   --  system.storage_pools%b
   --  system.finalization_masters%s
   --  system.storage_pools.subpools%s
   --  system.storage_pools.subpools.finalization%s
   --  system.storage_pools.subpools.finalization%b
   --  system.memory%s
   --  system.memory%b
   --  system.standard_library%b
   --  system.pool_global%s
   --  system.pool_global%b
   --  system.file_control_block%s
   --  system.file_io%s
   --  system.secondary_stack%s
   --  system.file_io%b
   --  system.storage_pools.subpools%b
   --  system.finalization_masters%b
   --  interfaces.c%b
   --  ada.tags%b
   --  system.soft_links%b
   --  system.os_lib%b
   --  system.secondary_stack%b
   --  system.address_image%b
   --  system.traceback%s
   --  ada.exceptions%b
   --  system.traceback%b
   --  ada.text_io%s
   --  ada.text_io%b
   --  p%b
   --  END ELABORATION ORDER


end ada_main;
