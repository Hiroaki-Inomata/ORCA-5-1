      *******************************************************************
      * Project code name "ORCA"
      * 日医標準レセプトソフト（JMA standard receipt software）
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGC11.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 手術歴登録（Ｃ１１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/03/03    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    04/04/09  登録後の一覧の保険名称修正
      *  01.00.02    NACL-藤原    05/03/03  訂正日指定対応
      *
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.07.02    NACL-藤原    06/01/16  年齢表示を追加
      *
      *  03.00.01    NACL-藤原    06/06/19   C06,C07,C08 削除
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/10/26  公務災害対応
      *
      *  04.08.01    NACL-藤原    18/06/18  更新時のOPID 設定
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    病名登録用共通パラメタ
           COPY    "C01COMMON-SPA".
      *
      *    病名登録用画面ＳＰＡ
           COPY    "C02SCR-SPA".
           02  SPA-C02-PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING  //PTBYO-//
                                   BY         //SPA-C02-PTBYO-//.
           02  WRK-C02-HAISIYMD-OCC.
               03  WRK-C02-HAISIYMD-O  PIC 9(08)   OCCURS  6.
           02  WRK-C02-PTBYOMEI-REC    OCCURS  6.
           COPY    "CPPTBYOMEI.INC"
                                   REPLACING   //PTBYO-//
                                   BY          //WRK-C02-PTBYO-//.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-OPE-ROW         PIC 9(01).
           03  FLG-COM-ROW         PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-CIDMSG          PIC X(80).
           03  WRK-DSPYM           PIC 9(01).
           03  WRK-YMD             PIC 9(08).
           03  WRK-YMD-R           REDEFINES   WRK-YMD.
               05  WRK-YM          PIC X(06).
               05  WRK-DD          PIC 9(02).
           03  WRK-ORC-DBYMD       PIC 9(08).
           03  WRK-STR             PIC 9(05).
           03  WRK-NUM             PIC ZZZ.
           03  WRK-BANGO           PIC 9(05).
           03  WRK-RENNUM          PIC 9(03).
      *
           03  WRK-OPELIST         PIC X(200).
      *
           03  WRK-HKNCOMBIMEI     PIC X(100).
           03  WRK-HKNKBNMEI       PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-C11-OPE-ROW     PIC S9(09). 
           03  WRK-C11-COM-ROW     PIC S9(09). 
      *
           03  WRK-KBN-HENSYU-AREA.
               05  WRK-KBN         PIC X(03).
               05  WRK-KBNMEI      PIC X(20).
               05  WRK-HKNID       PIC 9(10).
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
      *
      *    年齢表示
           03  WRK-NENREI-G.
               05  WRK-NENREI-Z    PIC ZZ9.
               05  WRK-NENREI-MEI  PIC X(04).
           03  WRK-NENREI-GR       REDEFINES   WRK-NENREI-G.
               05  WRK-NENREI-ZN   PIC Z9.
               05  WRK-NENREI-MEIN PIC X(04).
               05  WRK-NENREI-FIL  PIC X(01).
      *
      *    日付変換用
           03  WRK-SYMD            PIC X(08).
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    ＭＡＸ行
       01  WRK-CONS-C11-MAX        PIC 9(03)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                    SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA22SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
                                   MCPAREA
                                   SPAAREA
                                   LINKAREA
                                   SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS  ALSO    MCP-EVENT
               WHEN    "LINK"      ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"      ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN    OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END         =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-C01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-C02FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG      =   "CERR"
               MOVE    SPACE           TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
               IF      FLG-END         =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE  SPACE             TO  MCP-PUTTYPE.
           MOVE   "C11"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "CID1"
                   PERFORM 320-CID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPA-SET-SEC
      *
           MOVE    1               TO  SPA-GMN-C11-CUR
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *     確認画面よりの処理
      *****************************************************************
       320-CID1-SET-SEC           SECTION.
      *
           EVALUATE    SPA-CIDCD
           WHEN    "0101"
      *        入院歴登録の確認
               IF      SPA-CID1-FLG    =   "OK"
                   PERFORM 4120-TOUROKU-SEC
               END-IF
           WHEN    "0102"
      *        入院歴削除の確認
               IF      SPA-CID1-FLG    =   "OK"
                   PERFORM 480-SAKUJYO-SEC
               END-IF
           END-EVALUATE
      * 
           MOVE    SPACE           TO  SPA-CIDCD
                                       SPA-CID1-FLG
      *
           .
       320-CID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-C11-AREA
      *
      *    システム日付
           MOVE    SPA-NAI-C02-SYSYMD  TO  SPA-NAI-C11-SYSYMD
           MOVE    SPA-GMN-C02-SYSYMD  TO  SPA-GMN-C11-SYSYMD
      *
      *    表示期間の開始年月取得
           MOVE    SPA-NAI-C11-SYSYMD  TO  SPA-NAI-C11-DSPYMSTL (1)
           MOVE    ZERO                TO  SPA-NAI-C11-DSPYMSTL (6)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C11-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C11-DSPYMSTL (2)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C11-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -2                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C11-DSPYMSTL (3)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C11-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -3                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C11-DSPYMSTL (4)
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-C11-SYSYMD  TO  LNK-DAY6-KIJUN 
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -6                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-C11-DSPYMSTL (5)
      *
           MOVE    "1"                 TO  SPA-GMN-C11-NYUGAIKBNL    (1)
           MOVE    "入院"              TO  SPA-GMN-C11-NYUGAIKBNMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-C11-NYUGAIKBNL    (2)
           MOVE    "入院外"            TO  SPA-GMN-C11-NYUGAIKBNMEIL (2)
      *
           MOVE    "1"                 TO  SPA-GMN-C11-DSPYML    (1)
           MOVE    "当月分のみ"        TO  SPA-GMN-C11-DSPYMMEIL (1)
           MOVE    "2"                 TO  SPA-GMN-C11-DSPYML    (2)
           MOVE    "１ヶ月前迄"        TO  SPA-GMN-C11-DSPYMMEIL (2)
           MOVE    "3"                 TO  SPA-GMN-C11-DSPYML    (3)
           MOVE    "２ヶ月前迄"        TO  SPA-GMN-C11-DSPYMMEIL (3)
           MOVE    "4"                 TO  SPA-GMN-C11-DSPYML    (4)
           MOVE    "３ヶ月前迄"        TO  SPA-GMN-C11-DSPYMMEIL (4)
           MOVE    "5"                 TO  SPA-GMN-C11-DSPYML    (5)
           MOVE    "半年前迄"          TO  SPA-GMN-C11-DSPYMMEIL (5)
           MOVE    "6"                 TO  SPA-GMN-C11-DSPYML    (6)
           MOVE    "制限なし"          TO  SPA-GMN-C11-DSPYMMEIL (6)
           MOVE    6                   TO  SPA-GMN-C11-DSPYM-MAX
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C02-SRYKA-MAX
               MOVE    SPA-GMN-C02-SRYKALST (IDX)
                                       TO  SPA-GMN-C11-SRYKALST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-SRYKA-MAX   TO  SPA-GMN-C11-SRYKA-MAX
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C02-BSRYKA-MAX
               MOVE    SPA-GMN-C02-BSRYKALST (IDX)
                                       TO  SPA-GMN-C11-BSRYKALST (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C02-BSRYKA-MAX  TO  SPA-GMN-C11-BSRYKA-MAX
      *
           MOVE    SPA-GMN-C11-SRYKALST  (1)
                                       TO  SPA-GMN-C11-SRYKA-G
           MOVE    SPA-GMN-C11-DSPYMLST  (2)
                                       TO  SPA-GMN-C11-DSPYM-G
           MOVE    SPA-NAI-C11-DSPYMSTL  (2)
                                       TO  SPA-NAI-C11-DSPYM
      *
           IF      SPA-NAI-C02-PTID    >   ZERO
               MOVE    SPA-NAI-C02-PTID    TO  SPA-NAI-C11-PTID
               MOVE    SPA-GMN-C02-PTNUM   TO  SPA-GMN-C11-PTNUM
               MOVE    SPA-GMN-C02-NAME    TO  SPA-GMN-C11-NAME
               MOVE    SPA-GMN-C02-KANANAME
                                           TO  SPA-GMN-C11-KANANAME
               MOVE    SPA-GMN-C02-SEX     TO  SPA-GMN-C11-SEX
               MOVE    SPA-GMN-C02-BIRTHDAY
                                           TO  SPA-GMN-C11-BIRTHDAY
               MOVE    SPA-GMN-C02-AGE     TO  SPA-GMN-C11-AGE
      *
      *        一覧表示
               MOVE    1                   TO  SPA-GMN-C11-OPE-PAGE
                                               SPA-GMN-C11-COM-PAGE    
               PERFORM 3101-OPELIST-HENSYU-SEC
               PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
               MOVE    4                   TO  SPA-GMN-C11-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-C11-CUR
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    過去手術一覧編集処理
      *****************************************************************
       3101-OPELIST-HENSYU-SEC     SECTION.
      *                              
            INITIALIZE                 SPA-GMN-C11-OPELIST-TBL
                                       SPA-NAI-C11-OPELIST-TBL
                                       SPA-GMN-C11-OPE-MAX
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-C11-OPE-MAX
      *
      *    指定頁に表示する開始件数
           COMPUTE WRK-STR         = ( SPA-GMN-C11-OPE-PAGE  -  1 )
                                   *   WRK-CONS-C11-MAX
                                   +   1
      *
           INITIALIZE                  SRYACCT-REC
      *****MOVE    SPA-HOSPID          TO  ACCT-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  ACCT-PTID
           IF      SPA-GMN-C11-SRYKA   =   ZERO
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C11-SRYKA   TO  ACCT-SRYKA
           END-IF
           MOVE    SPA-NAI-C11-SYSYMD (1:6)
                                       TO  ACCT-EDSRYYM
           MOVE    SPA-NAI-C11-DSPYM   TO  ACCT-STSRYYM
           MOVE    "50"                TO  ACCT-SRYKBN       
      *
           IF      SPA-GMN-C11-SRYKA   =   ZERO
               MOVE    "tbl_sryacct"       TO  WRK-MCP-TABLE
               MOVE    "key34"             TO  WRK-MCP-PATHNAME
           ELSE
               MOVE    "tbl_sryacct"       TO  WRK-MCP-TABLE
               MOVE    "key35"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           PERFORM UNTIL ( IDX             >=  WRK-CONS-C11-MAX )
                   OR    ( FLG-SRYACCT     =   1                )
               PERFORM 31011-SRYACT-HENSYU-SEC
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-C11-MAX )
           AND   ( FLG-SRYACCT          =  ZERO             )
               MOVE    1                   TO  SPA-GMN-C11-OPE-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C11-OPE-NEXT
           END-IF
      *
      *    手術日による並びかえ処理
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >=      SPA-GMN-C11-OPE-MAX
               COMPUTE IDX1    =   IDX     +   1
               PERFORM VARYING IDY FROM    IDX1    BY  1
                       UNTIL   IDY >       SPA-GMN-C11-OPE-MAX
                   IF      SPA-NAI-C11-OPE-TSRYYMD (IDX)
                                       <   SPA-NAI-C11-OPE-TSRYYMD (IDY)
                       MOVE    SPA-GMN-C11-OPELIST-OCC (IDX)
                                       TO  WRK-OPELIST
                       MOVE    SPA-GMN-C11-OPELIST-OCC (IDY)
                                       TO  SPA-GMN-C11-OPELIST-OCC (IDX)
                       MOVE    WRK-OPELIST
                                       TO  SPA-GMN-C11-OPELIST-OCC (IDY)
                       MOVE    SPA-NAI-C11-OPELIST-OCC (IDX)
                                       TO  WRK-OPELIST
                       MOVE    SPA-NAI-C11-OPELIST-OCC (IDY)
                                       TO  SPA-NAI-C11-OPELIST-OCC (IDX)
                       MOVE    WRK-OPELIST
                                       TO  SPA-NAI-C11-OPELIST-OCC (IDY)
                   END-IF
               END-PERFORM
           END-PERFORM            
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C11-OPE-MAX
               MOVE    IDX             TO  SPA-GMN-C11-OPE-TNUM (IDX)
           END-PERFORM
      *
           MOVE    1                   TO  FLG-OPE-ROW
           .
       3101-OPELIST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の診療行為データ編集処理
      *****************************************************************
       31011-SRYACT-HENSYU-SEC         SECTION.
      *
      *    最終受診日取得
           MOVE    ZERO                TO  WRK-YMD
           MOVE    ACCT-SRYYM          TO  WRK-YM
           PERFORM VARYING IDZ FROM    31  BY  -1
                   UNTIL   IDZ <       1
               IF      ACCT-DAY (1 IDZ)    >   ZERO
                   MOVE    IDZ                 TO  WRK-DD
                   MOVE    1                   TO  IDZ
               END-IF
           END-PERFORM
           MOVE    WRK-YMD             TO  WRK-ORC-DBYMD
      *            
           INITIALIZE                      SRYACT-REC
      *****MOVE    ACCT-HOSPID         TO  SRY-HOSPID
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           PERFORM UNTIL ( IDX         >=  WRK-CONS-C11-MAX )
                   OR    ( FLG-SRYACT  =   1                )
      *        データ区分＝１だけ対象とする
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       5
                       OR      SRY-SRYCD(IDY)  =   SPACE
                       OR      IDX             >=  WRK-CONS-C11-MAX
      *
                   IF    ( SRY-SRYCD (IDY) (1:1)   =   "1"   )
                   AND   ( SRY-SRYSYUKBN       NOT =   "510" )    
                       INITIALIZE                      TNS-TENSU-REC
                       MOVE    SRY-SRYCD (IDY)         TO  TNS-SRYCD
                       MOVE    WRK-ORC-DBYMD           TO  TNS-YUKOSTYMD
                                                           TNS-YUKOEDYMD
                       MOVE    SPA-HOSPNUM             TO  TNS-HOSPNUM
                       MOVE    TNS-TENSU-REC           TO  MCPDATA-REC
                       PERFORM 900-TENSU-READ-SEC
                       IF      TNS-DATAKBN             =   "1"
                           PERFORM 310111-SRYACT-SPA-HENSYU-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       31011-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴編集処理
      *****************************************************************
       310111-SRYACT-SPA-HENSYU-SEC   SECTION.
      *         
      *    保険組合せ名称
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           MOVE    SPACE               TO  WRK-HKNKBNMEI
      *
           INITIALIZE                      HKNCOMBI-REC
      *****MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *        労災の時
               IF      COMB-HKNNUM         =   "971"   OR  "973"
                   INITIALIZE                      PTRSIINF-REC
      *************MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
                   MOVE    SPA-NAI-C11-PTID    TO  PTRSI-PTID
                   MOVE    COMB-HKNID          TO  PTRSI-HKNID
                   PERFORM 900-PTRSIINF-READ-SEC
                   EVALUATE    PTRSI-HKNKBN
                       WHEN    "1"
                           MOVE    "短"        TO  WRK-HKNKBNMEI
                       WHEN    "2"
                           MOVE    "傷"        TO  WRK-HKNKBNMEI
                       WHEN    "3"
                           MOVE    "ア"        TO  WRK-HKNKBNMEI
                       WHEN    "5"
                           MOVE    "公"        TO  WRK-HKNKBNMEI
                   END-EVALUATE
                   IF      PTRSI-COMMENT       =   SPACE
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               WRK-HKNKBNMEI       DELIMITED  BY  SPACE
                               INTO                WRK-HKNCOMBIMEI
                       END-STRING
                   ELSE
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               WRK-HKNKBNMEI      DELIMITED  BY  SPACE
                               "（"                DELIMITED  BY  SIZE
                               PTRSI-COMMENT       DELIMITED  BY  SPACE
                               "）"                DELIMITED  BY  SIZE
                               INTO                WRK-HKNCOMBIMEI
                       END-STRING
                   END-IF
               ELSE
                   IF       COMB-SYU-TANSEIDONAME   NOT =   SPACE                    
                       STRING  COMB-SYU-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH1-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH2-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH3-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH4-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                                     INTO          WRK-HKNCOMBIMEI
                       END-STRING
                   ELSE
                       STRING  COMB-KOH1-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH2-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH3-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                               " "                 DELIMITED  BY  SIZE
                               COMB-KOH4-TANSEIDONAME
                                                   DELIMITED  BY  SPACE
                                     INTO          WRK-HKNCOMBIMEI
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
      *    診療科名(全科表示のみ)
           MOVE    SPACE               TO  WRK-SRYKAMEI                          
           MOVE    ACCT-SRYKA          TO  WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  WRK-SRYKAMEI
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       31
                   OR      IDX             >=  WRK-CONS-C11-MAX
               IF      ACCT-DAY (1 IDZ)        >   ZERO
      *
      *            該当頁に表示するデータかどうか
                   ADD     1                   TO  IDX2
                   IF      IDX2                <   WRK-STR
                       GO  TO  310111-SRYACT-SPA-HENSYU-EXT
                   END-IF
      *
                   ADD     1                   TO  IDX
                   IF      IDX             >   WRK-CONS-C11-MAX
                       GO  TO  310111-SRYACT-SPA-HENSYU-EXT
                   END-IF
      *
                   MOVE    TNS-NAME        TO
                                       SPA-GMN-C11-OPE-TOPENAME  (IDX)
                   MOVE    WRK-SRYKAMEI    TO
                                       SPA-GMN-C11-OPE-TSRYKA    (IDX)
                   MOVE    ACCT-SRYKA      TO
                                       SPA-NAI-C11-OPE-TSRYKA    (IDX)
                   MOVE    WRK-HKNCOMBIMEI TO
                                       SPA-GMN-C11-OPE-THKNCOMBI (IDX)
                   MOVE    ACCT-HKNCOMBI   TO
                                       SPA-NAI-C11-OPE-THKNCOMBI (IDX)
                   MOVE    ACCT-SRYYM      TO  WRK-YM
                   MOVE    IDZ             TO  WRK-DD
                   MOVE    WRK-YMD         TO
                                       SPA-NAI-C11-OPE-TSRYYMD   (IDX)
                                       WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO
                                       SPA-GMN-C11-OPE-TSRYYMD   (IDX)
                   MOVE    SRY-SRYCD (IDY) TO  
                                       SPA-NAI-C11-OPE-TSRYCD    (IDX)
                   MOVE    ACCT-NYUGAIKBN  TO  
                                       SPA-NAI-C11-OPE-TNYUGAIKBN(IDX)
      *
                   MOVE    IDX             TO  SPA-GMN-C11-OPE-MAX
               END-IF
           END-PERFORM
           .
       310111-SRYACT-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト適用欄記載一覧処理
      *****************************************************************
       3102-COMMENTLIST-HENSYU-SEC      SECTION.
      *
            INITIALIZE                 SPA-GMN-C11-COMMENTLIST-TBL
                                       SPA-NAI-C11-COMMENTLIST-TBL
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
      *
      *    指定頁に表示する開始件数
           COMPUTE WRK-STR         = ( SPA-GMN-C11-COM-PAGE  -  1 )
                                   *   WRK-CONS-C11-MAX
                                   +   1
      *
           INITIALIZE                  OPECOM-REC
      *****MOVE    SPA-HOSPID          TO  OPECOM-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  OPECOM-PTID
           MOVE    "tbl_opecom"        TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
      *
           MOVE    SPA-HOSPNUM         TO  OPECOM-HOSPNUM
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           PERFORM UNTIL ( IDX             >=  WRK-CONS-C11-MAX )
                   OR    ( FLG-OPECOM      =   1                )
               PERFORM 32011-OPECOM-HENSYU-SEC
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-C11-MAX )
           AND   ( FLG-OPECOM           =  ZERO             )
               MOVE    1                   TO  SPA-GMN-C11-COM-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C11-COM-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-COM-ROW
           .
       3102-COMMENTLIST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴編集処理
      *****************************************************************
       32011-OPECOM-HENSYU-SEC         SECTION.
      *
      *    該当頁に表示するデータかどうか
           ADD     1                   TO  IDX2
           IF      IDX2                <   WRK-STR
               GO  TO  32011-OPECOM-HENSYU-EXT
           END-IF
      *
           ADD     1                   TO  IDX
           IF      IDX             >   WRK-CONS-C11-MAX
               GO  TO  32011-OPECOM-HENSYU-EXT
           END-IF
      *
           MOVE    IDX2                TO
                                       SPA-GMN-C11-COM-TNUM      (IDX)
           MOVE    OPECOM-OPENAME      TO
                                       SPA-GMN-C11-COM-TOPENAME  (IDX)
           MOVE    OPECOM-SRYCD        TO
                                       SPA-NAI-C11-COM-TSRYCD    (IDX)
           MOVE    OPECOM-RENNUM       TO
                                       SPA-NAI-C11-COM-TRENNUM   (IDX)
           MOVE    OPECOM-SRYYMD       TO
                                       SPA-NAI-C11-COM-TSRYYMD   (IDX)
                                       WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO
                                       SPA-GMN-C11-COM-TSRYYMD   (IDX)
           MOVE    OPECOM-NYUGAIKBN    TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-C11-COM-TNYUGAIKBN (IDX)
           MOVE    OPECOM-SRYKA        TO  WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO
                                       SPA-GMN-C11-COM-TSRYKA    (IDX)
           IF      OPECOM-HKNNUM   NOT =   SPACE    
               IF      OPECOM-HKNNUM       =   "971"   OR  "973"
                   INITIALIZE                      PTRSIINF-REC
      *************MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
                   MOVE    SPA-NAI-C11-PTID    TO  PTRSI-PTID
                   MOVE    OPECOM-HKNID        TO  PTRSI-HKNID
                   PERFORM 900-PTRSIINF-READ-SEC
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                                                   WRK-HKNKBNMEI
                   EVALUATE    PTRSI-HKNKBN
                       WHEN    "1"
                           MOVE    "短"        TO  WRK-HKNKBNMEI
                       WHEN    "2"
                           MOVE    "傷"        TO  WRK-HKNKBNMEI
                       WHEN    "3"
                           MOVE    "ア"        TO  WRK-HKNKBNMEI
                       WHEN    "5"
                           MOVE    "公"        TO  WRK-HKNKBNMEI
                   END-EVALUATE
                   EVALUATE    OPECOM-HKNNUM
                       WHEN    "971"
                          MOVE    "労災保険"   TO  WRK-HKNCOMBIMEI
                       WHEN    "973"
                          MOVE    "自陪責"     TO  WRK-HKNCOMBIMEI
                   END-EVALUATE
                   STRING  WRK-HKNCOMBIMEI     DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-HKNKBNMEI       DELIMITED  BY  SPACE
                           INTO            SPA-GMN-C11-COM-THKNNUM (IDX)
                   END-STRING
               ELSE
                   INITIALIZE                  HKNNUM-REC
      *************MOVE    SPA-HOSPID          TO  HKN-HOSPID
                   MOVE    OPECOM-HKNNUM       TO  HKN-HKNNUM
                   MOVE    OPECOM-SRYYMD       TO  HKN-TEKSTYMD
                                                   HKN-TEKEDYMD
                   PERFORM 900-HKNNUM-READ-SEC
                   MOVE    HKN-TANSEIDONAME    TO
                                       SPA-GMN-C11-COM-THKNNUM   (IDX)
               END-IF
           END-IF
           MOVE    OPECOM-TEKSTYM      TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO
                                       SPA-GMN-C11-COM-TTEKYM (IDX)(1:6)
           IF      OPECOM-TEKEDYM      =   ALL "9"
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    OPECOM-TEKEDYM      TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2)
           END-IF  
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO
                                       SPA-GMN-C11-COM-TTEKYM(IDX)(11:6)
           MOVE    "〜"                TO
                                       SPA-GMN-C11-COM-TTEKYM(IDX)(8:2)
      *
           MOVE    IDX                 TO  SPA-GMN-C11-COM-MAX
           .
       32011-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 460-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 470-ATOGMN-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-SAKUJYO-GID-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 4120-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C11-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
      *            患者番号
                   PERFORM 4101-PTNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
      *            診療科
                   PERFORM 4103-SRYKA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "DSPYM"
      *            表示期間
                   PERFORM 4102-DSPYM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
      *            選択番号
                   PERFORM 4104-SELNUM1-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
      *            選択番号
                   PERFORM 4105-SELNUM2-CHK-SEC
               WHEN    ANY             ALSO    "OPELIST"
      *            過去手術一覧
                   PERFORM 4106-OPELISTSEL-SEC
               WHEN    ANY             ALSO    "COMMENTLIST"
      *            適用欄記載一覧
                   PERFORM 4107-COMMENTLISTSEL-SEC
               WHEN    OTHER
                   PERFORM 4100-INPUT-CHK-SEC
      *            入力チェック処理
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    1                   TO  SPA-GMN-C11-CUR
      *
           INITIALIZE                  SPA-GMN-C11-HEAD
                                       SPA-NAI-C11-HEAD
                                       SPA-GMN-C11-BODY
                                       SPA-NAI-C11-BODY
                                       SPA-GMN-C11-OPELIST-TBL
                                       SPA-NAI-C11-OPELIST-TBL
                                       SPA-GMN-C11-COMMENTLIST-TBL
                                       SPA-NAI-C11-COMMENTLIST-TBL
      *
           MOVE    SPA-GMN-C11-DSPYMLST  (2)
                                       TO  SPA-GMN-C11-DSPYM-G
           MOVE    SPA-NAI-C11-DSPYMSTL  (2)
                                       TO  SPA-NAI-C11-DSPYM
           MOVE    SPA-GMN-C11-SRYKALST (1)
                                       TO  SPA-GMN-C11-SRYKA-G
      *
           MOVE    C11-PTNUM           TO  SPA-GMN-C11-PTNUM
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-C11-PTNUM   TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM    TO  SPA-GMN-C11-PTNUM
                   MOVE    SPTNUM-PTID     TO  SPA-NAI-C11-PTID
               WHEN    OTHER
                   MOVE    "0001"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
      *        患者マスター存在チェック
      *********MOVE    SPA-HOSPID          TO  PTINF-HOSPID
               MOVE    SPA-NAI-C11-PTID    TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF           NOT =   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
               ELSE
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO      4101-PTNUM-CHK-EXT
                   END-IF
      *
                   MOVE    PTINF-NAME          TO  SPA-GMN-C11-NAME
                   MOVE    PTINF-KANANAME      TO  SPA-GMN-C11-KANANAME
                   EVALUATE    PTINF-SEX
                       WHEN    "1" 
                           MOVE    "男"            TO  SPA-GMN-C11-SEX
                       WHEN    "2"
                           MOVE    "女"            TO  SPA-GMN-C11-SEX
                   END-EVALUATE
                   MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-C11-BIRTHDAY
      *            年齢計算
                   INITIALIZE                ORCSAGECHKAREA
grpsys             MOVE    SPA-HOSPNUM       TO  AGECHK-HOSPNUM
                   MOVE    SPA-HOSPNUM       TO  AGECHK-HOSPNUM
                   MOVE    SPA-NAI-C11-PTID  TO  AGECHK-PTID
                   MOVE    SPA-NAI-C02-SYSYMD
                                             TO  AGECHK-SRYYMD
                   MOVE    PTINF-BIRTHDAY    TO  AGECHK-BIRTHDAY
                   CALL    "ORCSAGECHK"        USING
                                               ORCSAGECHKAREA
grpsys                                         SPA-AREA
                   MOVE    SPACE             TO  WRK-NENREI-G
                   IF      AGECHK-NENREI2-YY   =   ZERO
                       MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
                       MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
                   ELSE
                       MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
                       MOVE    "才"                TO  WRK-NENREI-MEI
                   END-IF
                   MOVE    WRK-NENREI-G        TO  SPA-GMN-C11-AGE
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD           =   SPACE
      *        一覧表示
               MOVE    1                   TO  SPA-GMN-C11-OPE-PAGE
                                               SPA-GMN-C11-COM-PAGE    
               PERFORM 3101-OPELIST-HENSYU-SEC
               PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
               MOVE    4                   TO  SPA-GMN-C11-CUR
           END-IF    
           . 
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    表示期間チェック処理
      *****************************************************************
       4102-DSPYM-CHK-SEC              SECTION.
      *
           MOVE    2                   TO  SPA-GMN-C11-CUR
      *
           INITIALIZE                      SPA-GMN-C11-BODY
                                           SPA-NAI-C11-BODY
      *
           MOVE    ZERO                TO  SPA-NAI-C11-DSPYM
      *
           MOVE    C11-DSPYM           TO  SPA-GMN-C11-DSPYM-G
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-C11-DSPYM-MAX
               IF      SPA-GMN-C11-DSPYM   =   SPA-GMN-C11-DSPYML (IDX)
                   MOVE    SPA-GMN-C11-DSPYMLST (IDX)
                                           TO  SPA-GMN-C11-DSPYM-G
                   MOVE    SPA-NAI-C11-DSPYMSTL  (IDX)
                                           TO  SPA-NAI-C11-DSPYM
                   MOVE    SPA-GMN-C11-DSPYM-MAX
                                           TO  IDX
               END-IF
           END-PERFORM         
           IF      SPA-GMN-C11-DSPYMMEI    =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-C11-PTID    >   ZERO  )
               AND   ( SPA-GMN-C11-SRYKAMEI
                                       NOT =   SPACE )
      *            一覧表示
                   MOVE    1                   TO  SPA-GMN-C11-OPE-PAGE
                   PERFORM 3101-OPELIST-HENSYU-SEC
                   MOVE    4                   TO  SPA-GMN-C11-CUR 
               END-IF  
           END-IF    
           .
       4102-DSPYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       4103-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-GMN-C11-CUR
      *
           INITIALIZE                      SPA-GMN-C11-BODY
                                           SPA-NAI-C11-BODY
      *
           MOVE    C11-SRYKA           TO  SPA-GMN-C11-SRYKA-G
      *
           MOVE    SPA-GMN-C11-SRYKA   TO  WRK-KBN
           PERFORM 5001-SRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-SRYKAMEI
           IF      WRK-KBNMEI          =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               IF    ( SPA-NAI-C11-PTID    >   ZERO  )
               AND   ( SPA-GMN-C11-DSPYMMEI
                                       NOT =   SPACE )
               AND   ( SPA-GMN-C11-SRYKAMEI
                                       NOT =   SPACE )
      *            一覧表示
                   MOVE    1                   TO  SPA-GMN-C11-OPE-PAGE
                   PERFORM 3101-OPELIST-HENSYU-SEC
                   MOVE    4                   TO  SPA-GMN-C11-CUR 
               END-IF  
           END-IF    
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号（手術歴）入力処理
      *****************************************************************
       4104-SELNUM1-CHK-SEC         SECTION.
      *
           MOVE    4                   TO  SPA-GMN-C11-CUR
      *
           INITIALIZE                      SPA-GMN-C11-BODY
                                           SPA-NAI-C11-BODY
      *
           MOVE    C11-SELNUM1         TO  SPA-GMN-C11-SELNUM1
      *
           IF      SPA-GMN-C11-SELNUM1 =   ZERO
               MOVE    5                   TO  SPA-GMN-C11-CUR
           ELSE
               COMPUTE WRK-BANGO   =   SPA-GMN-C11-SELNUM1
                                   - ( SPA-GMN-C11-OPE-PAGE    -   1 )
                                   *   WRK-CONS-C11-MAX
      * 
               IF    ( WRK-BANGO           >   SPA-GMN-C11-OPE-MAX )
               OR    ( WRK-BANGO           <   1                   )
                   MOVE    ZERO                TO  SPA-GMN-C11-SELNUM1
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO           TO  SPA-NAI-C11-SELNUM1
                   PERFORM 41041-OPELIST-SELECT-SEC
      *
                   MOVE    SPA-NAI-C11-SELNUM1 TO  C11-OPE-ROW
               END-IF
           END-IF    
      *
           .
       4104-SELNUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名処理
      *****************************************************************
       41041-OPELIST-SELECT-SEC        SECTION.
      *
           MOVE    SPA-GMN-C11-OPE-TOPENAME (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-OPENAME
           MOVE    SPA-GMN-C11-OPE-TSRYYMD  (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-SRYYMD
           MOVE    SPA-NAI-C11-OPE-TSRYYMD (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-NAI-C11-SRYYMD
           MOVE    SPA-NAI-C11-OPE-TSRYKA  (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-BSRYKA
           MOVE    SPA-GMN-C11-OPE-TSRYKA  (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-BSRYKAMEI
           MOVE    SPA-NAI-C11-OPE-THKNCOMBI (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-NAI-C11-HKNCOMBI
           MOVE    SPA-NAI-C11-OPE-TSRYCD  (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-NAI-C11-SRYCD
           MOVE    SPA-NAI-C11-OPE-TNYUGAIKBN (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-NYUGAIKBN
                                           WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-NYUGAIKBNMEI
           MOVE    SPA-GMN-C11-OPE-TSRYYMD (SPA-NAI-C11-SELNUM1) (1:6)
                                       TO  SPA-GMN-C11-TEKSTYM
           MOVE    SPA-NAI-C11-OPE-TSRYYMD (SPA-NAI-C11-SELNUM1) (1:6)
                                       TO  SPA-NAI-C11-TEKSTYM
           MOVE    "999999"            TO  SPA-NAI-C11-TEKEDYM
                                           SPA-GMN-C11-TEKEDYM
      *
      *    保険・公費
           PERFORM 410411-HKNNUM-HENSYU-SEC
      *
           MOVE    5                   TO  SPA-GMN-C11-CUR
           .
       41041-OPELIST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費番号編集処理
      *****************************************************************
       410411-HKNNUM-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      HKNCOMBI-REC
      *****MOVE    SPA-HOSPID          TO  COMB-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  COMB-PTID
           MOVE    SPA-NAI-C11-HKNCOMBI
                                       TO  COMB-HKNCOMBINUM
           PERFORM 900-HKNCOMBI-READ-SEC
           IF      COMB-HKNNUM     NOT =   SPACE
               ADD     1               TO  IDX
               MOVE    COMB-HKNNUM     TO  SPA-GMN-C11-HKNNUML   (IDX)
               MOVE    COMB-HKNID      TO  SPA-NAI-C11-HKNIDL    (IDX)
               IF      COMB-HKNNUM         =   "971"   OR  "973"
                   INITIALIZE                      PTRSIINF-REC
      *************MOVE    SPA-HOSPID          TO  PTRSI-HOSPID
                   MOVE    SPA-NAI-C11-PTID    TO  PTRSI-PTID
                   MOVE    COMB-HKNID          TO  PTRSI-HKNID
                   PERFORM 900-PTRSIINF-READ-SEC
                   MOVE    SPACE               TO  WRK-HKNCOMBIMEI
                                                   WRK-HKNKBNMEI
                   EVALUATE    PTRSI-HKNKBN
                       WHEN    "1"
                           MOVE    "短"        TO  WRK-HKNKBNMEI
                       WHEN    "2"
                           MOVE    "傷"        TO  WRK-HKNKBNMEI
                       WHEN    "3"
                           MOVE    "ア"        TO  WRK-HKNKBNMEI
                       WHEN    "5"
                           MOVE    "公"        TO  WRK-HKNKBNMEI
                   END-EVALUATE
                   STRING  COMB-SYU-TANSEIDONAME
                                               DELIMITED  BY  SPACE
                           " "                 DELIMITED  BY  SIZE
                           WRK-HKNKBNMEI       DELIMITED  BY  SPACE
                           INTO                WRK-HKNCOMBIMEI
                   END-STRING
                   MOVE    WRK-HKNCOMBIMEI     TO
                                           SPA-GMN-C11-HKNNUMMEIL(IDX)
                   MOVE    PTRSI-HKNKBN        TO
                                           SPA-NAI-C11-HKNKBNL   (IDX)
               ELSE
                   MOVE    COMB-SYU-TANSEIDONAME
                                       TO  SPA-GMN-C11-HKNNUMMEIL(IDX)
               END-IF
           END-IF
           IF      COMB-KOHHKNNUM (1)
                                   NOT =   SPACE
               ADD     1               TO  IDX
               MOVE    COMB-KOHHKNNUM (1)
                                       TO  SPA-GMN-C11-HKNNUML   (IDX)
               MOVE    COMB-KOH1-TANSEIDONAME
                                       TO  SPA-GMN-C11-HKNNUMMEIL(IDX)
               MOVE    COMB-KOHID     (1)
                                       TO  SPA-NAI-C11-HKNIDL    (IDX)
           END-IF
           IF      COMB-KOHHKNNUM (2)
                                   NOT =   SPACE
               ADD     1               TO  IDX
               MOVE    COMB-KOHHKNNUM (2)
                                       TO  SPA-GMN-C11-HKNNUML   (IDX)
               MOVE    COMB-KOH2-TANSEIDONAME
                                       TO  SPA-GMN-C11-HKNNUMMEIL(IDX)
               MOVE    COMB-KOHID     (2)
                                       TO  SPA-NAI-C11-HKNIDL    (IDX)
           END-IF
           IF      COMB-KOHHKNNUM (3)
                                   NOT =   SPACE
               ADD     1               TO  IDX
               MOVE    COMB-KOHHKNNUM (3)
                                       TO  SPA-GMN-C11-HKNNUML   (IDX)
               MOVE    COMB-KOH3-TANSEIDONAME
                                       TO  SPA-GMN-C11-HKNNUMMEIL(IDX)
               MOVE    COMB-KOHID     (3)
                                       TO  SPA-NAI-C11-HKNIDL    (IDX)
           END-IF
           IF      COMB-KOHHKNNUM (4)
                                   NOT =   SPACE
               ADD     1               TO  IDX
               MOVE    COMB-KOHHKNNUM (4)
                                       TO  SPA-GMN-C11-HKNNUML   (IDX)
               MOVE    COMB-KOH4-TANSEIDONAME
                                       TO  SPA-GMN-C11-HKNNUMMEIL(IDX)
               MOVE    COMB-KOHID     (4)
                                       TO  SPA-NAI-C11-HKNIDL    (IDX)
           END-IF
           MOVE    IDX                 TO  SPA-GMN-C11-HKNNUM-MAX
      *
      *    労災・自賠責のときは表示
           IF    ( SPA-GMN-C11-HKNNUM-MAX
                                        =   1                 )
           AND   ( SPA-GMN-C11-HKNNUML (1)
                                        =   "971"   OR  "973" )
               MOVE    SPA-GMN-C11-HKNNUMLST (1)
                                           TO  SPA-GMN-C11-HKNNUM-G
           END-IF     
           .
       410411-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号（適用欄記載）入力処理
      *****************************************************************
       4105-SELNUM2-CHK-SEC         SECTION.
      *
           MOVE    20                  TO  SPA-GMN-C11-CUR
      *
           INITIALIZE                      SPA-GMN-C11-BODY
                                           SPA-NAI-C11-BODY
      *
           MOVE    C11-SELNUM2         TO  SPA-GMN-C11-SELNUM2
      *
           IF      SPA-GMN-C11-SELNUM2 =   ZERO
               MOVE    5                   TO  SPA-GMN-C11-CUR
           ELSE
               COMPUTE WRK-BANGO   =   SPA-GMN-C11-SELNUM2
                                   - ( SPA-GMN-C11-COM-PAGE    -   1 )
                                   *   WRK-CONS-C11-MAX
      *
               IF    ( WRK-BANGO           >   SPA-GMN-C11-COM-MAX )
               OR    ( WRK-BANGO           <   1               )
                   MOVE    ZERO                TO  SPA-GMN-C11-SELNUM2
                   MOVE    "0004"              TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO           TO  SPA-NAI-C11-SELNUM2
                   PERFORM 41051-COMMENTLIST-SELECT-SEC
      *
                   MOVE    SPA-NAI-C11-SELNUM2 TO  C11-COM-ROW
               END-IF
           END-IF
      *
           .
       4105-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択適用欄記載編集処理
      *****************************************************************
       41051-COMMENTLIST-SELECT-SEC    SECTION.
      *
           INITIALIZE                      OPECOM-REC
      *****MOVE    SPA-HOSPID          TO  OPECOM-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  OPECOM-PTID
           MOVE    SPA-NAI-C11-COM-TSRYYMD (SPA-NAI-C11-SELNUM2)
                                       TO  OPECOM-SRYYMD
           MOVE    SPA-NAI-C11-COM-TSRYCD  (SPA-NAI-C11-SELNUM2)
                                       TO  OPECOM-SRYCD
           MOVE    SPA-NAI-C11-COM-TRENNUM (SPA-NAI-C11-SELNUM2)
                                       TO  OPECOM-RENNUM
           MOVE    "tbl_opecom"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
           
           MOVE    OPECOM-OPENAME      TO  SPA-GMN-C11-OPENAME
           MOVE    OPECOM-OPECOMMENT   TO  SPA-GMN-C11-OPECOMMENT
           MOVE    OPECOM-SRYYMD       TO  SPA-NAI-C11-SRYYMD
           MOVE    SPA-GMN-C11-COM-TSRYYMD (SPA-NAI-C11-SELNUM2)
                                       TO  SPA-GMN-C11-SRYYMD
           MOVE    OPECOM-SRYCD        TO  SPA-NAI-C11-SRYCD
           MOVE    OPECOM-RENNUM       TO  SPA-NAI-C11-RENNUM
           MOVE    OPECOM-NYUGAIKBN    TO  SPA-GMN-C11-NYUGAIKBN
           MOVE    SPA-GMN-C11-COM-TNYUGAIKBN  (SPA-NAI-C11-SELNUM2)
                                       TO  SPA-GMN-C11-NYUGAIKBNMEI
           MOVE    OPECOM-SRYKA        TO  SPA-GMN-C11-BSRYKA
           MOVE    SPA-GMN-C11-COM-TSRYKA      (SPA-NAI-C11-SELNUM2)
                                       TO  SPA-GMN-C11-BSRYKAMEI
           MOVE    OPECOM-HKNNUM       TO  SPA-GMN-C11-HKNNUM
           MOVE    OPECOM-HKNKBN       TO  SPA-NAI-C11-HKNKBN
           MOVE    OPECOM-HKNID        TO  SPA-NAI-C11-HKNID
           MOVE    OPECOM-HKNCOMBI     TO  SPA-NAI-C11-HKNCOMBI
           MOVE    OPECOM-TEKSTYM      TO  SPA-NAI-C11-TEKSTYM
                                           WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)   TO  SPA-GMN-C11-TEKSTYM
           MOVE    OPECOM-TEKEDYM      TO  SPA-NAI-C11-TEKEDYM
           IF      OPECOM-TEKEDYM      =   ALL "9"
               MOVE    ALL "9"             TO  WRK-SYMD
           ELSE
               MOVE    OPECOM-TEKEDYM      TO  WRK-SYMD (1:6)
               MOVE    "01"                TO  WRK-SYMD (7:2) 
           END-IF
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG (1:6)       TO  SPA-GMN-C11-TEKEDYM
      *
      *
      *    保険・公費
           PERFORM 410411-HKNNUM-HENSYU-SEC
      *
           MOVE    SPA-GMN-C11-HKNNUM  TO  WRK-KBN
           PERFORM 5001-HKNNUM-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-HKNNUMMEI
           MOVE    WRK-HKNID           TO  SPA-NAI-C11-HKNID
      *
           MOVE    5                   TO  SPA-GMN-C11-CUR
           .
       41051-COMMENTLIST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択（手術歴）処理
      *****************************************************************
       4106-OPELISTSEL-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-BANGO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-C11-OPE-MAX
               IF      C11-OPE-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C11-SELNUM1
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-BANGO
                       MOVE    SPA-GMN-C11-OPE-MAX TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-C11-BODY1
                                       SPA-NAI-C11-BODY1
                                       SPA-GMN-C11-SELNUM2
                                       SPA-NAI-C11-SELNUM2
      *
           IF      WRK-BANGO           >   ZERO
               MOVE    WRK-BANGO           TO  SPA-NAI-C11-SELNUM1
           END-IF
      *
           MOVE    SPA-GMN-C11-OPE-TNUM (SPA-NAI-C11-SELNUM1)
                                       TO  SPA-GMN-C11-SELNUM1
      *                                 
           PERFORM 41041-OPELIST-SELECT-SEC
           .
       4106-OPELISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択（適用欄記載）処理
      *****************************************************************
       4107-COMMENTLISTSEL-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-BANGO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-C11-COM-MAX
               IF      C11-COM-SELECT (IDX)    =   "T"
                   IF      IDX             =   SPA-NAI-C11-SELNUM2
                       CONTINUE
                   ELSE        
                       MOVE    IDX                 TO  WRK-BANGO
                       MOVE    SPA-GMN-C11-COM-MAX TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-C11-BODY1
                                       SPA-NAI-C11-BODY1
                                       SPA-GMN-C11-SELNUM1
                                       SPA-NAI-C11-SELNUM1
      *
           IF      WRK-BANGO           >   ZERO
               MOVE    WRK-BANGO           TO  SPA-NAI-C11-SELNUM2
           END-IF
      *
           MOVE    SPA-GMN-C11-COM-TNUM (SPA-NAI-C11-SELNUM2)
                                       TO  SPA-GMN-C11-SELNUM2
      *                                 
           PERFORM 41051-COMMENTLIST-SELECT-SEC
           .
       4107-COMMENTLISTSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4100-INPUT-CHK-SEC              SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4100-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       4100-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    C11-OPENAME          TO  SPA-GMN-C11-OPENAME
           MOVE    C11-OPECOMMENT       TO  SPA-GMN-C11-OPECOMMENT
           MOVE    C11-NYUGAIKBN        TO  SPA-GMN-C11-NYUGAIKBN-G
           MOVE    C11-HKNNUM           TO  SPA-GMN-C11-HKNNUM-G
           MOVE    C11-BSRYKA           TO  SPA-GMN-C11-BSRYKA-G
           MOVE    C11-TEKSTYM          TO  SPA-GMN-C11-TEKSTYM
           MOVE    C11-TEKEDYM          TO  SPA-GMN-C11-TEKEDYM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-OPENAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41022-OPECOMMENT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41025-TEKSTYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41026-TEKEDYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41023-NYUGAIKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41027-HKNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41024-BSRYKA-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術名入力処理
      *****************************************************************
       41021-OPENAME-CHK-SEC           SECTION.
      * 
           IF      SPA-GMN-C11-OPENAME =   SPACE  
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-C11-CUR
           ELSE
      *        半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C11-OPENAME TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-C11-OPENAME
               ELSE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF
           .
       41021-OPENAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント入力処理
      *****************************************************************
       41022-OPECOMMENT-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-C11-OPECOMMENT
                                   NOT =   SPACE  
      *        半角チェック
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C11-OPECOMMENT
                                           TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO
                                               SPA-GMN-C11-OPECOMMENT
               ELSE
                   MOVE    "0006"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF
           .
       41022-OPECOMMENT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分入力処理
      *****************************************************************
       41023-NYUGAIKBN-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C11-NYUGAIKBN
                                       TO  WRK-KBN
           PERFORM 5001-NYUGAIKBN-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-NYUGAIKBNMEI
           IF      SPA-GMN-C11-NYUGAIKBN  
                                       =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C11-NYUGAIKBNMEI
                                           =   SPACE    
                  MOVE    "0009"               TO  SPA-ERRCD
                   MOVE    9                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF    
           .
       41023-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41024-BSRYKA-CHK-SEC                SECTION.
      *
           MOVE    SPA-GMN-C11-BSRYKA    TO  WRK-KBN
           PERFORM 5001-BSRYKA-HENSYU-SEC
           MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-BSRYKAMEI
           IF      SPA-GMN-C11-BSRYKA  =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C11-BSRYKAMEI
                                           =   SPACE    
                  MOVE    "0010"               TO  SPA-ERRCD
                   MOVE    11                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF    
           .
       41024-BSRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効期間開始年月入力処理
      *****************************************************************
       41025-TEKSTYM-CHK-SEC           SECTION.
      *
           IF     SPA-GMN-C11-TEKSTYM  =   SPACE
               MOVE    SPA-NAI-C11-SYSYMD (1:6)
                                           TO  SPA-NAI-C11-TEKSTYM
               MOVE    SPA-GMN-C11-SYSYMD (1:6)
                                           TO  SPA-GMN-C11-TEKSTYM
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-GMN-C11-TEKSTYM TO  SGDAY-INDAY 
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY  (1:6)  TO  SPA-GMN-C11-TEKSTYM
                   MOVE    SGDAY-SDAY   (1:6)  TO  SPA-NAI-C11-TEKSTYM
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF    
           .
       41025-TEKSTYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    有効期間終了年月入力処理
      *****************************************************************
       41026-TEKEDYM-CHK-SEC           SECTION.
      *
           IF     SPA-GMN-C11-TEKEDYM  =   SPACE   OR  ALL "9"
               MOVE    ALL "9"             TO  SPA-NAI-C11-TEKEDYM
                                               SPA-GMN-C11-TEKEDYM
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-GMN-C11-TEKEDYM TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY  (1:6)  TO  SPA-GMN-C11-TEKEDYM
                   MOVE    SGDAY-SDAY   (1:6)  TO  SPA-NAI-C11-TEKEDYM
               ELSE
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-C11-CUR
               END-IF
           END-IF    
           .
       41026-TEKEDYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       41027-HKNNUM-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-C11-HKNID
      *
           IF      SPA-GMN-C11-HKNNUM  =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C11-HKNNUM  TO  WRK-KBN
               PERFORM 5001-HKNNUM-HENSYU-SEC
               MOVE    WRK-KBNMEI          TO  SPA-GMN-C11-HKNNUMMEI
               MOVE    WRK-HKNID           TO  SPA-NAI-C11-HKNID
           END-IF
           IF      SPA-GMN-C11-HKNNUM   =   SPACE
               CONTINUE
           ELSE
               IF      SPA-GMN-C11-HKNNUMMEI   =   SPACE    
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    10                  TO  SPA-GMN-C11-CUR
               END-IF
           END-IF    
           .
       41027-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
           IF      SPA-NAI-C11-TEKSTYM >   SPA-NAI-C11-TEKEDYM
               MOVE    "0050"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-C11-CUR
           END-IF
           .
        4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       4120-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 4100-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               GO  TO  4120-KAKUTEI-EXT
           END-IF
      *
           PERFORM 4120-TOUROKU-SEC
           .
       4120-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4120-TOUROKU-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    入院歴登録
      *
           IF      SPA-NAI-C11-SELNUM2 NOT =   ZERO
               INITIALIZE                      OPECOM-REC
      *********MOVE    SPA-HOSPID              TO  OPECOM-HOSPID
               MOVE    SPA-NAI-C11-PTID        TO  OPECOM-PTID
               MOVE    SPA-NAI-C11-SRYYMD      TO  OPECOM-SRYYMD
               MOVE    SPA-NAI-C11-SRYCD       TO  OPECOM-SRYCD
               MOVE    SPA-NAI-C11-RENNUM      TO  OPECOM-RENNUM   
               MOVE    "tbl_opecom"            TO  WRK-MCP-TABLE
               MOVE    "key"                   TO  WRK-MCP-PATHNAME
               PERFORM 900-OPECOM-READ-SEC
               IF      FLG-OPECOM          =   1
                   MOVE    "1003"              TO  SPA-ERRCD
                   DISPLAY "READ OPECOM-KEY =>" OPECOM-KEY
                   GO  TO  4120-TOUROKU-EXT
               END-IF
           ELSE   
               MOVE    ZERO                TO  WRK-RENNUM 
               INITIALIZE                      OPECOM-REC
      *********MOVE    SPA-HOSPID          TO  OPECOM-HOSPID
               MOVE    SPA-NAI-C11-PTID    TO  OPECOM-PTID
               MOVE    SPA-NAI-C11-SRYYMD  TO  OPECOM-SRYYMD
               MOVE    SPA-NAI-C11-SRYCD   TO  OPECOM-SRYCD
               MOVE    "tbl_opecom"        TO  WRK-MCP-TABLE
               MOVE    "key3"              TO  WRK-MCP-PATHNAME
               PERFORM 900-OPECOM-READ-SEC
               IF      FLG-OPECOM          =   ZERO
                   MOVE    OPECOM-RENNUM       TO  WRK-RENNUM
               END-IF
               ADD     1                   TO  WRK-RENNUM 
      *           
               INITIALIZE                      OPECOM-REC
      *********MOVE    SPA-HOSPID          TO  OPECOM-HOSPID
               MOVE    SPA-NAI-C11-PTID    TO  OPECOM-PTID
               MOVE    SPA-NAI-C11-SRYYMD  TO  OPECOM-SRYYMD
               MOVE    SPA-NAI-C11-SRYCD   TO  OPECOM-SRYCD
               MOVE    WRK-RENNUM          TO  OPECOM-RENNUM
           END-IF
           MOVE    SPA-GMN-C11-OPENAME TO  OPECOM-OPENAME                  
           MOVE    SPA-GMN-C11-OPECOMMENT
                                       TO  OPECOM-OPECOMMENT                  
           MOVE    SPA-GMN-C11-NYUGAIKBN
                                       TO  OPECOM-NYUGAIKBN                  
           MOVE    SPA-GMN-C11-BSRYKA  TO  OPECOM-SRYKA                  
           MOVE    SPA-GMN-C11-HKNNUM  TO  OPECOM-HKNNUM                  
           MOVE    SPA-NAI-C11-HKNKBN  TO  OPECOM-HKNKBN                  
           MOVE    SPA-NAI-C11-HKNID   TO  OPECOM-HKNID                  
           MOVE    SPA-NAI-C11-HKNCOMBI
                                       TO  OPECOM-HKNCOMBI                  
           MOVE    SPA-NAI-C11-TEKSTYM TO  OPECOM-TEKSTYM                  
           MOVE    SPA-NAI-C11-TEKEDYM TO  OPECOM-TEKEDYM                  
      *
           IF      SPA-NAI-C11-SELNUM2 =   ZERO
               MOVE    SMCNDATE-YMD        TO  OPECOM-CREYMD
               MOVE    SMCNDATE-YMD        TO  OPECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  OPECOM-UPHMS
      *
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD        TO  OPECOM-UPYMD
               MOVE    SMCNDATE-HMS        TO  OPECOM-UPHMS
      *
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF
           MOVE    SPA-OPID            TO  OPECOM-OPID
           MOVE    SPA-HOSPNUM         TO  OPECOM-HOSPNUM
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "tbl_opecom"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "OPECOM-KEY =>" OPECOM-KEY " " MCP-FUNC  
               GO  TO  4120-TOUROKU-EXT
           ELSE
               PERFORM 41211-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    一覧表示
           IF      SPA-NAI-C11-SELNUM2 =   ZERO
               MOVE    1                   TO  SPA-GMN-C11-COM-PAGE
           END-IF
      *
           PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
           IF      SPA-NAI-C11-SELNUM2 >   ZERO
               MOVE    ZERO                TO  FLG-COM-ROW
           END-IF
      *
           INITIALIZE               SPA-GMN-C11-BODY
                                    SPA-NAI-C11-BODY
      *
           MOVE    4                   TO  SPA-GMN-C11-CUR
           .
       4120-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       41211-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "1"                 TO  LNK-UPD-SYORI
      *****MOVE    SPA-HOSPID          TO  LNK-UPD-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
grpsys                                          SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
       41211-RECEUPD-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "C02"           TO  SPA-SAKIPG
           MOVE    "C11"           TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"        TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG      TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *    
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           IF    ( SPA-NAI-C11-SELNUM1 =   ZERO  )    
           AND   ( SPA-NAI-C11-SELNUM2 =   ZERO  )
           AND   ( SPA-GMN-C11-OPENAME =   SPACE )   
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               INITIALIZE                  SPA-GMN-C11-HEAD
                                           SPA-NAI-C11-HEAD
      *                              
                                           SPA-GMN-C11-OPELIST-TBL
                                           SPA-NAI-C11-OPELIST-TBL
                                           SPA-GMN-C11-COMMENTLIST-TBL
                                           SPA-NAI-C11-COMMENTLIST-TBL
               MOVE    SPA-GMN-C11-SRYKALST  (1)
                                           TO  SPA-GMN-C11-SRYKA-G
               MOVE    SPA-GMN-C11-DSPYMLST  (2)
                                           TO  SPA-GMN-C11-DSPYM-G
               MOVE    SPA-NAI-C11-DSPYMSTL  (2)
                                           TO  SPA-NAI-C11-DSPYM
      *
               MOVE    1                   TO  SPA-GMN-C11-CUR
           ELSE
               MOVE    4                   TO  SPA-GMN-C11-CUR
           END-IF
      *
           INITIALIZE               SPA-GMN-C11-BODY
                                    SPA-NAI-C11-BODY
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C11-COM-PAGE    =   1
               GO      TO      460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C11-COM-PAGE    =   SPA-GMN-C11-COM-PAGE    -
                                               1
      *    検索処理
           PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-C11-COM-NEXT    =   ZERO
               GO      TO      470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C11-COM-PAGE
      *    検索処理
           PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除ガイドメッセージ処理
      *****************************************************************
       480-SAKUJYO-GID-SEC             SECTION.
      *
           IF      SPA-NAI-C11-SELNUM2 =   ZERO
               MOVE    "0051"              TO  SPA-ERRCD
               GO  TO  480-SAKUJYO-GID-EXT
           END-IF
      *
           MOVE    "0102"             TO  SPA-CIDCD
          .
       480-SAKUJYO-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除　処理
      *****************************************************************
       480-SAKUJYO-SEC             SECTION.
      *
           INITIALIZE                      OPECOM-REC
      *****MOVE    SPA-HOSPID          TO  OPECOM-HOSPID
           MOVE    SPA-NAI-C11-PTID    TO  OPECOM-PTID
           MOVE    SPA-NAI-C11-SRYYMD  TO  OPECOM-SRYYMD
           MOVE    SPA-NAI-C11-SRYCD   TO  OPECOM-SRYCD
           MOVE    SPA-NAI-C11-RENNUM  TO  OPECOM-RENNUM
           MOVE    SPA-HOSPNUM         TO  OPECOM-HOSPNUM
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_opecom"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               DISPLAY "OPECOM-KEY =>" OPECOM-KEY 
               GO  TO  480-SAKUJYO-EXT
           ELSE
               PERFORM 41211-RECEUPD-UPDATE-SEC
           END-IF
      *
           COMPUTE C11-COM-ROW =   SPA-NAI-C11-SELNUM2 -   1    
      *
           INITIALIZE                  SPA-GMN-C11-BODY
                                       SPA-NAI-C11-BODY
      *    検索処理
           MOVE    1                   TO  SPA-GMN-C11-COM-PAGE    
           PERFORM 3102-COMMENTLIST-HENSYU-SEC
      *
           MOVE    4                   TO  SPA-GMN-C11-CUR
           .
       480-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-CIDCD       NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE           TO  LINKAREA
      *
           MOVE    "CURRENT"       TO  MCP-PUTTYPE
           MOVE    "C11"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-OPE-ROW         =   1
               MOVE    ZERO                TO  WRK-C11-OPE-ROW
           ELSE
               MOVE    C11-OPE-ROW         TO  WRK-C11-OPE-ROW
           END-IF
           IF      FLG-COM-ROW         =   1
               MOVE    ZERO                TO  WRK-C11-COM-ROW
           ELSE
               MOVE    C11-COM-ROW         TO  WRK-C11-COM-ROW
           END-IF
      *
           MOVE    SPACE                   TO  C11
           INITIALIZE                          C11
           MOVE    SPA-GMN-C11-PTNUM       TO  C11-PTNUM
           MOVE    SPA-GMN-C11-SYSYMD      TO  C11-SYSYMD
           MOVE    SPA-GMN-C11-KANANAME    TO  C11-KANANAME
           MOVE    SPA-GMN-C11-NAME        TO  C11-NAME
           MOVE    SPA-GMN-C11-SEX         TO  C11-SEX
           MOVE    SPA-GMN-C11-BIRTHDAY    TO  C11-BIRTHDAY
           MOVE    SPA-GMN-C11-AGE         TO  C11-AGE
           MOVE    SPA-GMN-C11-SRYKA-G     TO  C11-SRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-C11-SRYKA-MAX
               MOVE    SPA-GMN-C11-SRYKALST (IDX)
                                               TO  C11-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C11-SRYKA-MAX   TO  C11-SRYKA-COUNT
           MOVE    SPA-GMN-C11-DSPYM-G     TO  C11-DSPYM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-C11-DSPYM-MAX
               MOVE    SPA-GMN-C11-DSPYMLST (IDX)
                                               TO  C11-DSPYM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C11-DSPYM-MAX   TO  C11-DSPYM-COUNT
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >  SPA-GMN-C11-OPE-MAX
               MOVE    SPA-GMN-C11-OPE-TNUM (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C11-OPE-TNUM  (IDX)
               IF      SPA-GMN-C11-SRYKA   =   ZERO
                   MOVE    SPA-GMN-C11-OPE-TSRYKA     (IDX)
                                           TO  C11-OPE-TSRYKA    (IDX)
               END-IF
               MOVE    SPA-GMN-C11-OPE-TSRYYMD    (IDX)
                                           TO  C11-OPE-TSRYYMD   (IDX)
               MOVE    SPA-GMN-C11-OPE-TOPENAME   (IDX)
                                           TO  C11-OPE-TOPENAME  (IDX)
               MOVE    SPA-GMN-C11-OPE-THKNCOMBI   (IDX)
                                           TO  C11-OPE-THKNCOMBI (IDX)
      *         
               IF      IDX                 =   SPA-NAI-C11-SELNUM1
                   MOVE    "T"                 TO  C11-OPE-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  C11-OPE-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C11-OPE-MAX TO  C11-OPE-COUNT
           MOVE    ZERO                    TO  C11-OPE-ROW
           IF      WRK-C11-OPE-ROW         >   ZERO
               MOVE    WRK-C11-OPE-ROW         TO  C11-OPE-ROW
           END-IF
      *
           MOVE    SPA-GMN-C11-SELNUM1     TO  C11-SELNUM1
           MOVE    SPA-GMN-C11-SRYYMD      TO  C11-SRYYMD
           MOVE    SPA-GMN-C11-OPENAME     TO  C11-OPENAME
           MOVE    SPA-GMN-C11-OPECOMMENT  TO  C11-OPECOMMENT
           MOVE    SPA-GMN-C11-TEKSTYM     TO  C11-TEKSTYM
           MOVE    SPA-GMN-C11-TEKEDYM     TO  C11-TEKEDYM
      *
           MOVE    SPA-GMN-C11-NYUGAIKBN-G TO  C11-NYUGAIKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       2
                   OR      SPA-GMN-C11-NYUGAIKBNL  (IDX)  =   SPACE
               MOVE    SPA-GMN-C11-NYUGAIKBNLST (IDX)
                                           TO  C11-NYUGAIKBN-ITEM (IDX)
               MOVE    IDX                 TO  C11-NYUGAIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-C11-BSRYKA-G   TO  C11-BSRYKA
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-C11-BSRYKA-MAX
               MOVE    SPA-GMN-C11-BSRYKALST (IDX)
                                               TO  C11-BSRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C11-BSRYKA-MAX   TO  C11-BSRYKA-COUNT
      *
           MOVE    SPA-GMN-C11-HKNNUM-G    TO  C11-HKNNUM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-C11-HKNNUM-MAX
               MOVE    SPA-GMN-C11-HKNNUMLST (IDX)
                                           TO  C11-HKNNUM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-C11-HKNNUM-MAX  TO  C11-HKNNUM-COUNT
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX         >   SPA-GMN-C11-COM-MAX
               MOVE    SPA-GMN-C11-COM-TNUM (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C11-COM-TNUM      (IDX)
               MOVE    SPA-GMN-C11-COM-TSRYKA     (IDX)
                                           TO  C11-COM-TSRYKA    (IDX)
               MOVE    SPA-GMN-C11-COM-TSRYYMD    (IDX)
                                           TO  C11-COM-TSRYYMD   (IDX)
               MOVE    SPA-GMN-C11-COM-TOPENAME   (IDX)
                                           TO  C11-COM-TOPENAME  (IDX)
               MOVE    SPA-GMN-C11-COM-THKNNUM    (IDX)
                                           TO  C11-COM-THKNNUM   (IDX)
               MOVE    SPA-GMN-C11-COM-TNYUGAIKBN (IDX)
                                           TO  C11-COM-TNYUGAIKBN(IDX)
               MOVE    SPA-GMN-C11-COM-TTEKYM     (IDX)
                                           TO  C11-COM-TYUKYM    (IDX)
      *         
               IF      IDX                 =   SPA-NAI-C11-SELNUM2
                   MOVE    "T"                 TO  C11-COM-SELECT (IDX)
               ELSE
                   MOVE    "F"                 TO  C11-COM-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C11-COM-MAX     TO  C11-COM-COUNT
           MOVE    ZERO                    TO  C11-COM-ROW
           IF      WRK-C11-COM-ROW         >   ZERO
               MOVE    WRK-C11-COM-ROW         TO  C11-COM-ROW
           END-IF
      *
           MOVE    SPA-GMN-C11-SELNUM2     TO  C11-SELNUM2
      *
      *    カーソルセット
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD           =   SPACE)
             AND ( SPA-GMN-C11-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
         EVALUATE    SPA-GMN-C11-CUR
             WHEN    1
                     MOVE  "PTNUM"      TO  MCP-WIDGET
             WHEN    2
                     MOVE  "DSPYM"      TO  MCP-WIDGET
             WHEN    3
                     MOVE  "SRYKA"      TO  MCP-WIDGET
             WHEN    4
                     MOVE  "SELNUM1"    TO  MCP-WIDGET
             WHEN    5
                     MOVE  "OPENAME"    TO  MCP-WIDGET
             WHEN    6
                     MOVE  "OPECOMMENT" TO  MCP-WIDGET
             WHEN    7
                     MOVE  "TEKSTYM"    TO  MCP-WIDGET
             WHEN    8
                     MOVE  "TEKEDYM"    TO  MCP-WIDGET
             WHEN    9
                     MOVE  "NYUGAIKBN"  TO  MCP-WIDGET
             WHEN    10
                     MOVE  "HKNNUM"     TO  MCP-WIDGET
             WHEN    11
                     MOVE  "BSRYKA"     TO  MCP-WIDGET
             WHEN    12
                     MOVE  "SELNUM2"    TO  MCP-WIDGET
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       5011-INPUT-CUR-SEC              SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "PTNUM"
                   MOVE    4                   TO  SPA-GMN-C11-CUR
               WHEN    "DSPYM"
                   MOVE    4                   TO  SPA-GMN-C11-CUR
               WHEN    "SRYKA"
                   MOVE    4                   TO  SPA-GMN-C11-CUR
               WHEN    "SELNUM1"
                   MOVE    5                   TO  SPA-GMN-C11-CUR
               WHEN    "OPENAME"
                   MOVE    6                   TO  SPA-GMN-C11-CUR
               WHEN    "OPECOMMENT"
                   MOVE    7                   TO  SPA-GMN-C11-CUR
               WHEN    "TEKSTYM"
                   MOVE    8                   TO  SPA-GMN-C11-CUR
               WHEN    "TEKEDYM"
                   MOVE    9                   TO  SPA-GMN-C11-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    10                  TO  SPA-GMN-C11-CUR
               WHEN    "HKNNUM"
                   MOVE    11                  TO  SPA-GMN-C11-CUR
               WHEN    "BSRYKA"
                   MOVE    5                   TO  SPA-GMN-C11-CUR
               WHEN    "SELNUM2"
                   MOVE    5                   TO  SPA-GMN-C11-CUR
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C11-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の患者が存在しません"
                                                   TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "表示期間入力エラー"    TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療科目入力エラー"    TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "選択番号入力エラー"    TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "手術名が未入力です"    TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "全角入力して下さい"    TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "有効期間開始入力エラー"
                                                   TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "有効期間終了入力エラー"
                                                   TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "入外区分入力エラー"    TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "保険番号入力エラー"    TO  SPA-ERRMSG
      *
               WHEN    "0050"
                   MOVE    "有効期間終了が開始以前になっています"
                                                   TO  SPA-ERRMSG
               WHEN    "0051"
                   MOVE    "記載一覧からの選択時のみ削除処理ができます"
                                                   TO  SPA-ERRMSG
      *
               WHEN    "1001"
                   MOVE    "更新処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理でエラーになりました"
                                                   TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "手術歴マスタが存在しません"
                                                   TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"  TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG               TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE               TO  CERR
           MOVE    SPA-ERRCD           TO  CERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  CERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C11"               TO  SPA-MOTOPG
           MOVE    "CERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-CIDCD
           WHEN    "0101"
               MOVE    "更新します。よろしいですか？"
                                       TO  WRK-CIDMSG
           WHEN    "0102"
               MOVE    "削除します。よろしいですか？"
                                       TO  WRK-CIDMSG
           WHEN    OTHER
               MOVE    SPA-ERRCD       TO  WRK-CIDMSG
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-CID1-FLG
      *
           MOVE    SPACE           TO  CID1
           INITIALIZE                  CID1
           MOVE    SPA-CIDCD       TO  CID1-ID1CODE
           MOVE    WRK-CIDMSG      TO  CID1-ID1MSG
           EVALUATE    SPA-CIDCD
               WHEN    "0102"
                   MOVE    "B01"           TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"           TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "C11"           TO  SPA-MOTOPG
           MOVE    "CID1"          TO  SPA-SAKIPG
      *
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "CID1"          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-SRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDXX    FROM   1   BY  1
                       UNTIL   IDXX    >      SPA-GMN-C11-SRYKA-MAX
                   IF      WRK-KBN    =   SPA-GMN-C11-SRYKAL (IDXX)
                       MOVE    SPA-GMN-C11-SRYKAMEIL (IDXX)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C11-SRYKA-MAX
                                          TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-SRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       5001-BSRYKA-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE               TO  WRK-KBNMEI
      *     
           IF      WRK-KBN             =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDXX    FROM   1   BY  1
                       UNTIL   IDXX    >      SPA-GMN-C11-BSRYKA-MAX
                   IF      WRK-KBN     =   SPA-GMN-C11-BSRYKAL (IDXX)
                       MOVE    SPA-GMN-C11-BSRYKAMEIL (IDXX)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-GMN-C11-BSRYKA-MAX
                                          TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-BSRYKA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分編集処理
      *****************************************************************
       5001-NYUGAIKBN-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDXX    FROM   1   BY  1
                       UNTIL   IDXX    >      2
                   IF      WRK-KBN    =   SPA-GMN-C11-NYUGAIKBNL (IDXX)
                       MOVE    SPA-GMN-C11-NYUGAIKBNMEIL (IDXX)
                                          TO  WRK-KBNMEI
                       MOVE    2          TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-NYUGAIKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号編集処理
      *****************************************************************
       5001-HKNNUM-HENSYU-SEC       SECTION.
      * 
           MOVE    SPACE              TO  WRK-KBNMEI
           MOVE    ZERO               TO  WRK-HKNID 
      *     
           IF      WRK-KBN            =   SPACE   
               CONTINUE
           ELSE
               PERFORM VARYING IDXX    FROM   1   BY  1
                       UNTIL   IDXX    >      SPA-GMN-C11-HKNNUM-MAX
                   IF      WRK-KBN        =   SPA-GMN-C11-HKNNUML (IDXX)
                       MOVE    SPA-GMN-C11-HKNNUMMEIL (IDXX)
                                          TO  WRK-KBNMEI
                       MOVE    SPA-NAI-C11-HKNIDL (IDXX)
                                          TO  WRK-HKNID
                       MOVE    SPA-GMN-C11-HKNNUM-MAX
                                          TO  IDXX
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       5001-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9" OR  SPACE
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC           SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNCOMBI-REC
                   MOVE    ZERO                TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1                   TO  FLG-HKNCOMBI
                   INITIALIZE                      HKNCOMBI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
               INITIALIZE                      HKNCOMBI-REC
           END-IF
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険マスター読込
      *****************************************************************
       900-PTRSIINF-READ-SEC           SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTRSIINF-REC
                   MOVE    ZERO                TO  FLG-PTRSIINF
               ELSE
                   MOVE    1                   TO  FLG-PTRSIINF
                   INITIALIZE                      PTRSIINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTRSIINF
               INITIALIZE                      PTRSIINF-REC
           END-IF
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTRSIINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC             SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  HKNNUM-REC
                   MOVE    ZERO                TO  FLG-HKNNUM
               ELSE
                   MOVE    1                   TO  FLG-HKNNUM
                   INITIALIZE                      HKNNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
               INITIALIZE                      HKNNUM-REC
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-SEC             SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                      TNS-TENSU-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       900-SRYACCT-READ-N-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-N-EXT.
           EXIT.
      *
     *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-READ-N-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  OPECOM-HOSPNUM
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
     *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-NAI-C02-PTID    TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBFETCH-SEC                SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルアクセス処理
      *****************************************************************
       900-ORCDBMAIN-SEC               SECTION.
      *
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-ORCDBMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
