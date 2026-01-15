prog entry @plist_free_4253248;

proc @plist_free_4253248 () -> ()
  { .name = "plist_free"; .address = 0x40e640; .returnBlock = "plist_free_basil_return_1" }
[
  block %plist_free_entry {.address = 0x40e640} [
    goto(%plist_free_31, %plist_free_17);
  ];
  block %plist_free_17 {.address = 0x40e660} [
    goto(%plist_free_23, %plist_free_basil_return_1);
  ];
  block %plist_free_23 {.address = 0x40e674} [
    goto(%plist_free_17, %plist_free_31);
  ];
  block %plist_free_31 {.address = 0x40e684} [
    goto(%plist_free_23);
  ];
  block %plist_free_basil_return_1 [
    return ();
  ]
];
