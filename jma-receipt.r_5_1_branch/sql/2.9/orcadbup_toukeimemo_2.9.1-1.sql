delete from tbl_syskanri where kanricd = '0044' and kbncd in ('1014');
COPY "tbl_syskanri" FROM stdin;
0044	1014    	00000000	99999999	高額日計表（高額請求添付資料）                                                                                                                                  ORCBG008                診療年月            YM        処理区分            KBN       検索内容            NAIYO     入外区分            PSN1                                                                                                                                                                                          1111			20041006	20060523	181425
\.
delete from tbl_toukeimemo where pgid = 'ORCBG008' and kanricd = '0000' and kbncd = '000';
COPY "tbl_toukeimemo" FROM stdin;
ORCBG008	0000	000     	00000000	99999999	診療年月　診療年月を入力します。\n処理区分　検索対象区分を入力します。\n　　　　　１：レセプトの請求点数が指定した点数以上となる場合を対象\n　　　　　２：指定した患者を対象\n検索内容　点数指定のとき、点数を入力します。\n　　　　　患者指定のとき、患者番号を入力します。\n入外区分　０：入外分\n　　　　　１：入院分\n　　　　　２：外来分			00000000	00000000	000000
\.
