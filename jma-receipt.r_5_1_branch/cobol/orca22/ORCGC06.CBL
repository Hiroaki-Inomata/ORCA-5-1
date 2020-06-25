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
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGC06.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 病名登録
      *  コンポーネント名  : 病名セット登録（Ｃ０６）
      *  管理者            : 
      *  13/07/26    NACL−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/11/22  Warning 対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *   
           COPY    "ENUM-VALUE".
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
           03  FLG-USERBYOMEI      PIC 9(01).
           03  FLG-BYOMEI          PIC 9(01).
           03  FLG-BYOMEIINPUTSET  PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-CHK             PIC 9(01).   
           03  FLG-ROW             PIC 9(01).  
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDW                 PIC 9(04).
           03  IDX1                PIC 9(04). 
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZ1                PIC 9(04).
           03  IDZ2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM             PIC ZZ9.
           03  WRK-SELNUM          PIC 9(03). 
           03  WRK-BANGO           PIC S9(05).
      *                             
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      * 
           03  WRK-C06-SETCDLST-ROW
                                   PIC S9(09). 
      *
           03  WRK-CIDMSG          PIC X(80).
      *
           03  WRK-STR             PIC 9(05).
      *
           03  WRK-ERR-BYOMEI      PIC X(80).
      *
           03  WRK-LENGTH          PIC 9(02).
      *
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEI-O    PIC X(100)  OCCURS  6.
               05  WRK-BYOMEI      PIC X(100).
      *
       01  WRK-CONS-AREA.
      *    一覧行数
           03  WRK-CONS-TBL-MAX    PIC 9(03)   VALUE   100.
      *
      *    未コード化傷病名コード
           03  WRK-CONS-BYOMEICD   PIC X(07)   VALUE   "0000999". 
      *
      *    病名システム予約コードテーブル
           COPY    "CMBYOMEICD.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    病名
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    自院病名
       01  USERBYOMEI-REC.
           COPY    "CPUSERBYOMEI.INC".
      *
      *    病名入力セット
       01  BYOMEIINPUTSET-REC.
           COPY    "CPBYOMEIINPUTSET.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    数字変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    MCPAREA.
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
           INITIALIZE                  WRK-AREA
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
               WHEN     "PUTG"     ALSO    "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
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
      *        初期画面セット
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
           MOVE    "NEW"           TO  MCP-PUTTYPE
           MOVE    "C06"           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1               TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF      SPA-MOTOPG      =   "CID4"
      *        確認画面
               IF      SPA-CID4-FLG    =   "OK"
                   EVALUATE    SPA-CIDCD4
                       WHEN    "0101"
                           PERFORM 410021-SETCD-HENSYU-SEC
                       WHEN    "0104"
      *                    削除処理
                           PERFORM 2801-SAKUJYO-SEC
                   END-EVALUATE
               ELSE
                   EVALUATE    SPA-CIDCD4
                       WHEN    "0101"
                           INITIALIZE          SPA-GMN-C06-INPUTCD-G
                           PERFORM 410021-SETCD-HENSYU-SEC
                   END-EVALUATE
               END-IF
               INITIALIZE                  SPA-CIDCD-AREA
                                           SPA-CID1-AREA
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           IF      SPA-MOTOPG       =   "C04"
      *        自院病名セット処理
               PERFORM 320-C04SPASET-SEC
               GO  TO  300-SCREEN-EXT
           END-IF
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初画面編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-C06
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               IF      SPA-C01-TBYOMEICD(IDX)
                               NOT =   SPACE
                   MOVE    SPA-C01-TBYOMEICD (IDX)
                                       TO  SPA-GMN-C06-INPUTCD   (IDX)
                   MOVE    SPA-C01-TBYOMEISBT (IDX)  TO
                                           SPA-NAI-C06-BYOMEISBT (IDX)
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       6
                       IF      SPA-C01-TBYOMEICD-INDEX (IDX)
                                                   =   IDZ
                           MOVE    SPA-NAI-C02-BYOMEI  (IDZ)
                                           TO  SPA-GMN-C06-BYOMEI (IDX)
                           PERFORM VARYING IDX3    FROM    1   BY  1
                                   UNTIL   IDX3    >       21
                                   OR      SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                                                   =   SPACE
                                   OR      SPA-NAI-C02-BYOMEICD-O
                                                          (IDZ IDX3)
                                                   =   ALL "9"
                               MOVE    SPA-NAI-C02-BYOMEICD-O (IDZ IDX3)
                                           TO  SPA-NAI-C06-BYOMEICD-O
                                                              (IDX IDX3)
                               MOVE    SPA-NAI-C02-BYOMEI-O   (IDZ IDX3)
                                           TO  SPA-NAI-C06-BYOMEI-O
                                                              (IDX IDX3)
                           END-PERFORM    
                           MOVE    6       TO  IDZ
                       END-IF
                   END-PERFORM 
               END-IF            
           END-PERFORM
      *
      *    複数病名の先頭フラグ編集処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
      *    病名編集処理
           PERFORM 3102-BYOMEI-HENSYU-SEC
      *
      *    病名セット一覧
           MOVE    1                   TO  SPA-GMN-C06-PAGE
           PERFORM 3103-BYOMEIINPUTSET-LST-SEC
      *
           IF      SPA-GMN-C06-INPUTCD-G
                                       =   SPACE
               MOVE    1                   TO  SPA-GMN-C06-CUR
           ELSE
               MOVE    2                   TO  SPA-GMN-C06-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数病名の先頭フラグ編集処理
      *****************************************************************
       3101-BYOMEITOP-HENSYU-SEC      SECTION.
      *
           IF      SPA-GMN-C06-INPUTCD (IDX) 
                                       =   SPACE
               MOVE    SPACE           TO  SPA-NAI-C06-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
           IF      IDX                 =   1  
               MOVE    "1"             TO  SPA-NAI-C06-BYOMEITOP(IDX)
               GO  TO  3101-BYOMEITOP-HENSYU-EXT
           END-IF
      *
      *   自院病名の部位が入力されている場合は複数病名にはしない
          PERFORM VARYING IDW FROM    1   BY  1
                  UNTIL   IDW >       6
               IF      SPA-NAI-C06-BYOMEISBT(IDW)
                                           =   "1"
                   MOVE    SPACE   TO  SPA-NAI-C06-BYOMEITOP(IDX)
                   GO  TO  3101-BYOMEITOP-HENSYU-EXT
               END-IF
           END-PERFORM
      *
           COMPUTE IDW     =   IDX     -   1
           EVALUATE    SPA-NAI-C06-BYOMEISBT(IDX)
               WHEN    "8"
                   MOVE    SPACE   TO  SPA-NAI-C06-BYOMEITOP(IDX)
               WHEN    "7"
               WHEN    "0"
                   EVALUATE    SPA-NAI-C06-BYOMEISBT (IDW)
                       WHEN    "7"
                       WHEN    "1"
                           MOVE    SPACE   TO
                                           SPA-NAI-C06-BYOMEITOP(IDX)
                       WHEN    "8"
                       WHEN    "0"
                           MOVE    "1"     TO
                                           SPA-NAI-C06-BYOMEITOP(IDX)
                   END-EVALUATE
               WHEN    "1"
                   MOVE    SPACE   TO  SPA-NAI-C06-BYOMEITOP(IDX)
           END-EVALUATE
           .
       3101-BYOMEITOP-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       3102-BYOMEI-HENSYU-SEC      SECTION.
      *
      *    病名
           MOVE    SPACE                   TO  WRK-BYOMEI-AREA
           MOVE    ZERO                    TO  IDY
      *
           MOVE    SPA-GMN-C06-BYOMEI (1)  TO  WRK-BYOMEI
      *     
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C06-INPUTCD (IDX)
                                   NOT =   SPACE
                   IF      SPA-NAI-C06-BYOMEITOP (IDX) =   "1"
                       IF      WRK-BYOMEI  NOT =   SPACE
                           ADD     1           TO  IDY
                           MOVE    WRK-BYOMEI  TO  WRK-BYOMEI-O (IDY)
                       END-IF
                       MOVE    SPACE           TO  WRK-BYOMEI
                   END-IF
                   STRING  WRK-BYOMEI              DELIMITED   BY  SPACE
                           SPA-GMN-C06-BYOMEI(IDX) DELIMITED   BY  SPACE
                                                   INTO    WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           IF      WRK-BYOMEI      NOT =   SPACE
               ADD     1               TO  IDY
               MOVE    WRK-BYOMEI      TO  WRK-BYOMEI-O (IDY)
           END-IF
      *
           MOVE   WRK-BYOMEI-O (1)     TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
                   OR      WRK-BYOMEI-O (IDX)  =   SPACE     
               STRING  WRK-BYOMEI      DELIMITED  BY  SPACE
                   "／"                DELIMITED  BY  SIZE
                   WRK-BYOMEI-O (IDX)  DELIMITED  BY  SPACE
                                 INTO             WRK-BYOMEI
               END-STRING
           END-PERFORM
      *****MOVE    WRK-BYOMEI          TO  SPA-GMN-C06-BYOMEI
           .
       3102-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セット一覧編集処理
      *****************************************************************
       3103-BYOMEIINPUTSET-LST-SEC     SECTION.
      *
           INITIALIZE                      SPA-GMN-C06-SETCD-TBL
           MOVE    ZERO                TO  SPA-GMN-C06-MAX
      *
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  WRK-MCP-TABLE
           MOVE    "key2"              TO  WRK-MCP-PATHNAME
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-C06-PAGE  -  1 )
                                   *   WRK-CONS-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
           PERFORM UNTIL ( IDX                 >=  WRK-CONS-TBL-MAX )
                   OR    ( FLG-BYOMEIINPUTSET  =   1           )
               ADD     1                   TO  IDX2
               IF      IDX2                >=  WRK-STR
                   ADD     1                   TO  IDX
                   IF      IDX                 <=  WRK-CONS-TBL-MAX
                       MOVE    IDX2            TO
                                   SPA-GMN-C06-TSELNUM  (IDX)
                       MOVE    BYOSET-BYOMEIINPUTSET
                                               TO
                                   SPA-GMN-C06-TBYOMEIINPUTSET     (IDX)
                       MOVE    BYOSET-BYOMEIINPUTSETNAME
                                               TO
                                   SPA-GMN-C06-TBYOMEIINPUTSETNAME (IDX)
                       MOVE    IDX             TO  SPA-GMN-C06-MAX
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-TBL-MAX )
           AND   ( FLG-USERBYOMEI      =   ZERO        )
               MOVE    1                   TO  SPA-GMN-C06-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-C06-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
           .
      *
       3103-BYOMEIINPUTSET-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    検索画面より自院病名セット処理
      *****************************************************************
       320-C04SPASET-SEC          SECTION.
      *
           IF      SPA-C01KYOUTU       =   SPACE
               GO  TO  320-C04SPASET-EXT
           END-IF
      *
           INITIALIZE               SPA-GMN-C06-INPUTCD-G
                                    SPA-NAI-C06-INPUTCD-G
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       6
               IF      SPA-C01-TBYOMEICD (IDZ)
                                       =   SPACE
                   CONTINUE
               ELSE
                   MOVE    SPA-C01-TBYOMEICD (IDZ)
                                           TO  SPA-GMN-C06-INPUTCD (IDZ)
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-GMN-C06-INPUTCD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
      *            自院病名編集処理
                   PERFORM 4102211-INPUTCD-HENSYU-SEC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 3102-BYOMEI-HENSYU-SEC
      *
           MOVE    4                   TO  SPA-GMN-C06-CUR
           .
       320-C04SPASET-EXT.
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
                   MOVE    SPACE           TO  SPA-C01KYOUTU 
                   MOVE    SPA-C01KYOUTU   TO  SPA-WORK
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *        自院病名検索
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 240-USERBYOMEI-KENSAKU-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 260-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 270-ATOGMN-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 280-SAKUJYO-GID-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 250-KAKUTEI-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-C06-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41001-SELNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SETCD"
                   PERFORM 41002-SETCD-CHK-SEC
               WHEN    ANY             ALSO    "SETCDLST"
                   PERFORM 41020-SETCDSEL-SEC
               WHEN    OTHER
                   PERFORM 400-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    番号入力処理
      *****************************************************************
       41001-SELNUM-CHK-SEC            SECTION.
      *
      *    クリア処理
           PERFORM 220-CLEAR-SEC
      *
           MOVE    ZERO                TO  SPA-NAI-C06-SELNUM
           MOVE    C06-SELNUM          TO  SPA-GMN-C06-SELNUM
      *
           IF      SPA-GMN-C06-SELNUM  =   ZERO
               MOVE    2                   TO  SPA-GMN-C06-CUR
               GO  TO  41001-SELNUM-CHK-EXT
           END-IF
      *
           COMPUTE WRK-SELNUM      =   SPA-GMN-C06-SELNUM
                                   - ( SPA-GMN-C06-PAGE    -   1 )
                                   *   WRK-CONS-TBL-MAX
      *
           IF    ( WRK-SELNUM          >   SPA-GMN-C06-MAX )
           OR    ( WRK-SELNUM          <   1               )
               MOVE    "0012"              TO  SPA-ERRCD
               GO  TO  41001-SELNUM-CHK-EXT
           END-IF
      *
           MOVE    WRK-SELNUM          TO  SPA-NAI-C06-SELNUM
                                           C06-SETCDLST-ROW
      *
      *    選択病名編集処理
           PERFORM 410011-SETCD-SELECT-SEC
      *
           .
       41001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択病名編集処理
      *****************************************************************
       410011-SETCD-SELECT-SEC         SECTION.
      *      
           MOVE    SPA-GMN-C06-TBYOMEIINPUTSET   (SPA-NAI-C06-SELNUM)
                                       TO
                                       SPA-GMN-C06-BYOMEIINPUTSET
                                       SPA-NAI-C06-BYOMEIINPUTSET
                                       SPA-NAI-C06-BYOMEIINPUTSET-OLD
      *
      *    病名セットコード編集処理
            PERFORM 4100111-BYOMEIINPUTSET-HEN-SEC
      *
           .
       410011-SETCD-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セットコード編集処理
      *****************************************************************
       4100111-BYOMEIINPUTSET-HEN-SEC  SECTION.
      *
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                       TO   BYOSET-BYOMEIINPUTSET
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
           PERFORM 900-BYOMEIINPUTSET-READ-SEC
           IF      FLG-BYOMEIINPUTSET  =   ZERO
               MOVE    BYOSET-BYOMEIINPUTSETNAME
                                       TO
                                       SPA-GMN-C06-BYOMEIINPUTSETNAME
               PERFORM VARYING IDZ     FROM    1   BY  1
                       UNTIL   IDZ     >       6
                       OR      BYOSET-BYOMEIINPUTCD (IDZ)
                                       =   SPACE 
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM     TO  USBYO-HOSPNUM
                   MOVE    BYOSET-BYOMEIINPUTCD (IDZ)
                                           TO  USBYO-BYOMEIINPUTCD
                                               SPA-GMN-C06-INPUTCD (IDZ)
      *            自院病名編集処理
                   PERFORM 4102211-INPUTCD-HENSYU-SEC
                   MOVE    SPACE            TO  SPA-ERRCD
               END-PERFORM
           END-IF
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       6
               PERFORM 3101-BYOMEITOP-HENSYU-SEC
           END-PERFORM
           PERFORM 3102-BYOMEI-HENSYU-SEC
      *
           MOVE    3                   TO  SPA-GMN-C06-CUR
      *
           .
       4100111-BYOMEIINPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セットコードチェック処理
      *****************************************************************
       41002-SETCD-CHK-SEC             SECTION.
      *
           MOVE    2                   TO  SPA-GMN-C06-CUR
      *
           MOVE    SPACE               TO  SPA-NAI-C06-BYOMEIINPUTSET
      *
           MOVE    C06-SETCD           TO  SPA-GMN-C06-BYOMEIINPUTSET
      *
           IF      SPA-GMN-C06-BYOMEIINPUTSET
                                       =   SPACE
               MOVE    "0006"              TO  SPA-ERRCD
           ELSE
      *        半角チェック
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                           TO  KANACHK-MAE-INPUT
               MOVE    10                  TO  KANACHK-MAX-CNT 
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-RC2         =   ZERO )
                   MOVE    KANACHK-OUT-INPUT   TO
                                           SPA-GMN-C06-BYOMEIINPUTSET
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        コードの桁数
               MOVE    ZERO                TO  WRK-LENGTH
               PERFORM VARYING IDX FROM    10  BY  -1
                       UNTIL   IDX <       1
                   IF      SPA-GMN-C06-BYOMEIINPUTSET (IDX:1)
                                       NOT =   SPACE
                       MOVE    IDX             TO  WRK-LENGTH
                       MOVE    1               TO  IDX
                   END-IF
               END-PERFORM                                    
      *
      *        「.」「／」で始まる又は「+」「-」で終わるコードは入力不可
               IF      KANACHK-SYUBETU  =   "2"
                   COMPUTE IDX     =   WRK-LENGTH  -   1
                   IF      SPA-GMN-C06-BYOMEIINPUTSET (IDX:2)
                                           =   "＋"    OR  "−"
                       MOVE    "0036"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   IF      SPA-GMN-C06-BYOMEIINPUTSET (1:1)
                                           =   "." OR  "/"
                       MOVE    "0030"              TO  SPA-ERRCD
                   ELSE
                       IF      SPA-GMN-C06-BYOMEIINPUTSET (WRK-LENGTH:1)
                                               =   "-" OR  "+"
                           MOVE    "0036"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *            ７桁のとき
      *            数字のみ又は病名マスタ・自院病名マスタ・予約コードに存在するか
                   IF    ( SPA-ERRCD           =   SPACE ) 
                   AND   ( WRK-LENGTH          =   7     )
                       INITIALIZE                  ORCSNUMAREA
                       MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                                   TO  SNUM-INX
                       CALL    "ORCSNUM"           USING
                                                   ORCSNUMAREA
                       IF      SNUM-RC             =   ZERO
                           MOVE    "0001"              TO  SPA-ERRCD
                       ELSE
                           MOVE    ZERO        TO  FLG-BYOMEI
                           IF  (   SPA-GMN-C06-BYOMEIINPUTSET (1:3)
                                                       =   "ZZZ"    )
                           AND ( ( SPA-GMN-C06-BYOMEIINPUTSET (4:1)
                                                       <   "1" ) OR 
                                 ( SPA-GMN-C06-BYOMEIINPUTSET (4:1)
                                                       =   "C" )    )
      *                        予約病名コード検索
                               SET     TBL-BYOMEICD-IDX  TO  1
                               SEARCH  TBL-BYOMEICD-OCC
                                               VARYING TBL-BYOMEICD-IDX
                                   AT  END
                                       MOVE    1   TO  FLG-BYOMEI
                                   WHEN   SPA-GMN-C06-BYOMEIINPUTSET
                                                       =
                                               TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                      MOVE     ZERO    TO  FLG-BYOMEI
                               END-SEARCH
                           ELSE
      *                        病名マスタ検索
                               MOVE    SPA-GMN-C06-BYOMEIINPUTSET 
                                                       TO  BYO-BYOMEICD
                               PERFORM 900-BYOMEI-READ-SEC
                           END-IF
                           IF      FLG-BYOMEI          =   ZERO
                               MOVE    "0031"          TO  SPA-ERRCD
                           END-IF
                       END-IF
                   END-IF
               END-IF    
      *
               IF      SPA-ERRCD           =   SPACE
      *            自院病名マスタ検索
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
                   MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                               TO  USBYO-BYOMEIINPUTCD
                   PERFORM 900-USBYO-READ-SEC
                   IF      FLG-USERBYOMEI      =   ZERO
                       MOVE    "0031"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD    NOT =   SPACE     
      ***      MOVE    SPACE               TO
      ***                              SPA-GMN-C06-BYOMEIINPUTSETNAME
               GO  TO  41002-SETCD-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-C06-BYOMEIINPUTSET
                                           =
                                       SPA-NAI-C06-BYOMEIINPUTSET-OLD
               CONTINUE
           ELSE
               MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                           TO
      *********                        SPA-NAI-C06-BYOMEIINPUTSET-OLD
                                       SPA-NAI-C06-BYOMEIINPUTSET
      *
      *        登録済かどうかのチェック
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE     SPA-GMN-C06-BYOMEIINPUTSET
                                           TO  BYOSET-BYOMEIINPUTSET
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               PERFORM 900-BYOMEIINPUTSET-READ-SEC
               IF      FLG-BYOMEIINPUTSET  =   ZERO
                   IF      SPA-GMN-C06-INPUTCD-G
                                               NOT =   SPACE
                       MOVE    "0101"              TO  SPA-CIDCD4
                       GO  TO  41002-SETCD-CHK-EXT
                   ELSE
                       INITIALIZE          SPA-GMN-C06-INPUTCD-G
                       PERFORM 410021-SETCD-HENSYU-SEC
                   END-IF
               ELSE
                   INITIALIZE          SPA-GMN-C06-SELNUM
                                       SPA-NAI-C06-SELNUM
                                       C06-SETCDLST-ROW
      *********                        SPA-NAI-C06-BYOMEIINPUTSET-OLD
               END-IF
           END-IF    
      *
           MOVE    3                   TO  SPA-GMN-C06-CUR
      *
           .
       41002-SETCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名セットコード入力後処理
      *****************************************************************
       410021-SETCD-HENSYU-SEC         SECTION.
      *          
           IF      SPA-CID4-FLG    =   "OK"
               CONTINUE
           ELSE
      *        病名セットコード編集処理
               PERFORM 4100111-BYOMEIINPUTSET-HEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                   TO  SPA-NAI-C06-BYOMEIINPUTSET-OLD
      *
      *    一覧にあれば行選択する
           PERFORM VARYING     IDX FROM    1   BY  1
                   UNTIL       IDX >   SPA-GMN-C06-MAX
               IF      SPA-GMN-C06-BYOMEIINPUTSET  =
                                   SPA-GMN-C06-TBYOMEIINPUTSET (IDX)
                   MOVE    IDX                 TO  SPA-NAI-C06-SELNUM
                   MOVE    SPA-GMN-C06-TSELNUM  (SPA-NAI-C06-SELNUM)
                                               TO  SPA-GMN-C06-SELNUM
                   MOVE    SPA-NAI-C06-SELNUM  TO  C06-SETCDLST-ROW
                   MOVE    SPA-GMN-C06-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    3                   TO  SPA-GMN-C06-CUR
      *
           .
       410021-SETCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       41020-SETCDSEL-SEC              SECTION.
      *          
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-C06-MAX
               IF      C06-SETCDLST-SELECT (IDX)
                                       =   "T"
                   IF      IDX             =   SPA-NAI-C06-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-C06-MAX TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
      *    クリア処理
           PERFORM 220-CLEAR-SEC
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-C06-SELNUM
                                               C06-SETCDLST-ROW
           END-IF
      *
           MOVE    SPA-GMN-C06-TSELNUM (SPA-NAI-C06-SELNUM)
                                       TO  SPA-GMN-C06-SELNUM
      *
      *    選択病名編集処理
           PERFORM 410011-SETCD-SELECT-SEC
           .
       41020-SETCDSEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       400-INPUT-CHK-SEC               SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  400-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前チェック
               PERFORM 4105-KEIKOKU-CHK-SEC
           END-IF
      *
           .
       400-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC            SECTION.
      *
           MOVE    C06-SETNAME         TO
                                       SPA-GMN-C06-BYOMEIINPUTSETNAME
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6  
               MOVE    C06-INPUTCD (IDX)   TO  SPA-GMN-C06-INPUTCD (IDX)
           END-PERFORM
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 41021-BYOMEIINPUTSETNM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 41022-INPUTCD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名チェック処理
      *****************************************************************
       41021-BYOMEIINPUTSETNM-CHK-SEC  SECTION.
      *
      *    半角チェック
           IF      SPA-GMN-C06-BYOMEIINPUTSETNAME
                                       =   SPACE
               MOVE    "0007"              TO  SPA-ERRCD
           ELSE    
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C06-BYOMEIINPUTSETNAME
                                           TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   MOVE    KANACHK-OUT-INPUT   TO
                                       SPA-GMN-C06-BYOMEIINPUTSETNAME
               ELSE
                   MOVE    "0071"              TO  SPA-ERRCD
               END-IF
           END-IF    
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    3                   TO  SPA-GMN-C06-CUR
           ELSE
               IF      WRK-MCP-WIDGET      =   "SETNAME"
      *            未入力の入力コード欄があれば遷移
                   PERFORM VARYING IDZ FROM    1   BY  1
                           UNTIL   IDZ >       6
                       IF      SPA-GMN-C06-INPUTCD (IDZ)
                                                   =   SPACE
                           COMPUTE SPA-GMN-C06-CUR =   IDZ     +   3
                           MOVE    6                   TO  IDZ
                       END-IF
                   END-PERFORM
               END-IF 
           END-IF
           .
       41021-BYOMEIINPUTSETNM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名コードチェック処理
      *****************************************************************
       41022-INPUTCD-CHK-SEC           SECTION.
      *
           PERFORM VARYING IDZ FROM    1   BY  1
                   UNTIL   IDZ >       6
                   OR      SPA-ERRCD   NOT =   SPACE
               PERFORM 410221-INPUTCD-CHECK-SEC
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       6
                   PERFORM 3101-BYOMEITOP-HENSYU-SEC
               END-PERFORM
               PERFORM 3102-BYOMEI-HENSYU-SEC
           END-IF
           .
       41022-INPUTCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名コードチェック処理
      *****************************************************************
       410221-INPUTCD-CHECK-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-C06-BYOMEI (IDZ)
      *
      *    半角チェック
           IF      SPA-GMN-C06-INPUTCD (IDZ)
                                       =   SPACE
               CONTINUE
           ELSE    
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI 
               MOVE    SPA-GMN-C06-INPUTCD (IDZ)
                                           TO  KANACHK-MAE-INPUT
               MOVE    10                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-RC2         =   ZERO )
                   MOVE    KANACHK-OUT-INPUT   TO
                                               SPA-GMN-C06-INPUTCD (IDZ)
      *
                   INITIALIZE                      USERBYOMEI-REC
                   MOVE    SPA-GMN-C06-INPUTCD (IDZ)
                                               TO  USBYO-BYOMEIINPUTCD
      *            自院病名編集処理
                   PERFORM 4102211-INPUTCD-HENSYU-SEC
               ELSE
                   MOVE    "0072"              TO  SPA-ERRCD
               END-IF
      *
               IF      SPA-ERRCD   NOT =   SPACE
                   COMPUTE SPA-GMN-C06-CUR =    IDZ +   3
               END-IF
           END-IF    
           .
       410221-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名編集処理
      *****************************************************************
       4102211-INPUTCD-HENSYU-SEC                SECTION.
      *
           IF      USBYO-BYOMEIINPUTCD =   SPACE
               INITIALIZE                  SPA-GMN-C06-BYOMEI-OCC (IDZ)
                                           SPA-NAI-C06-BYOMEI-OCC (IDZ)
               GO  TO  4102211-INPUTCD-HENSYU-EXT
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           PERFORM 900-USBYO-READ-SEC
           IF      FLG-USERBYOMEI      =   ZERO    
               MOVE    USBYO-BYOMEI        TO  SPA-GMN-C06-BYOMEI  (IDZ)
               MOVE    USBYO-BYOMEISBT     TO  SPA-NAI-C06-BYOMEISBT
                                                                   (IDZ)
      *
               PERFORM VARYING IDXX    FROM    1   BY  1
                       UNTIL   IDXX    >       21
                       OR      USBYO-BYOMEICD (IDXX)   =   SPACE
                       OR      USBYO-BYOMEICD (IDXX)   =   ALL "9"
                   MOVE    USBYO-BYOMEICD (IDXX)
                                       TO
                                       SPA-NAI-C06-BYOMEICD-O (IDZ IDXX)
                   IF      SPA-NAI-C06-BYOMEICD-O (IDZ IDXX)
                                       =   WRK-CONS-BYOMEICD
                       MOVE    SPA-GMN-C06-BYOMEI   (IDZ)
                                           TO
                                       SPA-NAI-C06-BYOMEI-O (IDZ IDXX)
                   ELSE                           
                       IF  (   SPA-NAI-C06-BYOMEICD-O(IDZ IDXX)(1:3)
                                               =   "ZZZ"    )
                       AND ( ( SPA-NAI-C06-BYOMEICD-O(IDZ IDXX)(4:1)
                                               <   "1" ) OR 
                             ( SPA-NAI-C06-BYOMEICD-O(IDZ IDXX)(4:1)
                                               =   "C" )    )
      *                    予約病名コード検索
                           SET     TBL-BYOMEICD-IDX  TO  1
                           SEARCH  TBL-BYOMEICD-OCC
                                           VARYING TBL-BYOMEICD-IDX
                               AT  END
                                   MOVE    "■■■"    TO
                                         SPA-NAI-C06-BYOMEI-O (IDZ IDXX)
                               WHEN    SPA-NAI-C06-BYOMEICD-O
                                                          (IDZ IDXX)
                                               =   TBL-YYK-BYOMEICD
                                                      (TBL-BYOMEICD-IDX)
                                   MOVE    TBL-YYK-BYOMEI 
                                                      (TBL-BYOMEICD-IDX)
                                                   TO
                                         SPA-NAI-C06-BYOMEI-O (IDZ IDXX)
                           END-SEARCH
                       ELSE
                           MOVE    SPA-NAI-C06-BYOMEICD-O (IDZ IDXX)
                                                   TO   BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      FLG-BYOMEI      =   ZERO
                               MOVE    BYO-BYOMEI      TO
                                       SPA-NAI-C06-BYOMEI-O  (IDZ IDXX)
                           ELSE
                               MOVE    "■■■"        TO
                                       SPA-NAI-C06-BYOMEI-O  (IDZ IDXX)
                           END-IF
                       END-IF 
                   END-IF    
               END-PERFORM
           ELSE
               MOVE    "■■■"            TO  SPA-GMN-C06-BYOMEI  (IDZ)
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
           .
       4102211-INPUTCD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       4105-KEIKOKU-CHK-SEC                SECTION.
      *
      *    セットコードのチェック
           IF      SPA-NAI-C06-BYOMEIINPUTSET
                                       =   SPACE
               IF      SPA-GMN-C06-BYOMEIINPUTSET
                                           =   SPACE
                   MOVE    "0006"              TO  SPA-ERRCD
               ELSE
                   MOVE    "0032"              TO  SPA-ERRCD
               END-IF
               MOVE    2                   TO  SPA-GMN-C06-CUR
               GO  TO  4105-KEIKOKU-CHK-EXT
           END-IF
      *
      *    自院コード入力チェック
           IF      SPA-GMN-C06-INPUTCD-G
                                   =   SPACE
               MOVE    "0034"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-C06-CUR
               GO  TO  4105-KEIKOKU-CHK-EXT
           END-IF
      *
      *    病名重複チェック
           MOVE    SPACE                   TO  WRK-BYOMEI-AREA
           MOVE    ZERO                    TO  IDY
      *
           MOVE    SPA-GMN-C06-BYOMEI (1)  TO  WRK-BYOMEI
           PERFORM VARYING IDX FROM    2   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C06-INPUTCD (IDX)
                                   NOT =   SPACE
                   IF      SPA-NAI-C06-BYOMEITOP (IDX) =   "1"
                       IF      WRK-BYOMEI  NOT =   SPACE
                           ADD     1           TO  IDY
                           MOVE    WRK-BYOMEI  TO  WRK-BYOMEI-O(IDY)
                       END-IF
                       MOVE    SPACE           TO  WRK-BYOMEI
                   END-IF
                   STRING  WRK-BYOMEI          DELIMITED   BY  SPACE
                           SPA-GMN-C06-BYOMEI(IDX)
                                               DELIMITED   BY  SPACE
                                               INTO    WRK-BYOMEI
                   END-STRING
               END-IF    
           END-PERFORM    
           IF      WRK-BYOMEI      NOT =   SPACE
               ADD     1               TO  IDY
               MOVE    WRK-BYOMEI      TO  WRK-BYOMEI-O (IDY)
           END-IF
      *
           MOVE    SPACE               TO  WRK-ERR-BYOMEI
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
                   OR      WRK-BYOMEI-O (IDX)  =   SPACE
               PERFORM VARYING IDY FROM    IDX  BY  1
                       UNTIL   IDY >       6
                       OR      WRK-BYOMEI-O (IDY)  =   SPACE
                   IF      IDY             >   IDX
                       IF      WRK-BYOMEI-O (IDX)  =
                                                   WRK-BYOMEI-O (IDY)
                           MOVE    "0002"      TO  SPA-ERRCD
                           MOVE    WRK-BYOMEI-O (IDX)
                                               TO  WRK-ERR-BYOMEI
                           COMPUTE SPA-GMN-C06-CUR     =   IDX     +   3
                           GO  TO  4105-KEIKOKU-CHK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4105-KEIKOKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "C06"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           INITIALIZE               SPA-GMN-C06-KOUMOKU
                                    SPA-NAI-C06-KOUMOKU
      *                                   
           MOVE    1                TO  SPA-GMN-C06-CUR
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名検索処理
      *****************************************************************
       240-USERBYOMEI-KENSAKU-SEC      SECTION.
      *
      *    病名のチェック
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF    ( SPA-GMN-C06-INPUTCD (IDX)
                                           NOT =   SPACE )
               AND   ( SPA-GMN-C06-BYOMEI (IDX)(1:2)
                                               =   "■"  )
                   MOVE    "0035"              TO  SPA-ERRCD
                   COMPUTE SPA-GMN-C06-CUR =   IDX     +   3
                   GO  TO  240-USERBYOMEI-KENSAKU-EXT
               END-IF
           END-PERFORM
      *
      *
           INITIALIZE                      SPA-C01KYOUTU
           MOVE    "C06"               TO  SPA-C01-MOTOPG
      *
      *    補足コメント入力不可
            MOVE    1                   TO  SPA-C01-B10-FLG
      *    
           MOVE    ZERO                TO  IDY
      * 
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       6
               IF      SPA-GMN-C06-INPUTCD (IDX)  NOT =   SPACE
                   ADD     1               TO  IDY              
                   MOVE    SPA-GMN-C06-INPUTCD  (IDX)
                                           TO  SPA-C01-TBYOMEICD (IDY)
                   MOVE    SPA-NAI-C06-BYOMEITOP (IDX)
                                           TO  SPA-C01-TBYOMEITOP (IDY)
                   MOVE    SPA-NAI-C06-BYOMEISBT (IDX)
                                           TO  SPA-C01-TBYOMEISBT (IDY)
                   MOVE    "1"             TO  SPA-C01-TUSERBYOMEIFLG
                                                                  (IDY)
                   MOVE    IDX             TO  SPA-C01-TBYOMEICD-INDEX
                                                                  (IDY)
               END-IF                              
           END-PERFORM
           MOVE    "BYOMEICD"          TO  SPA-C01-WIDGET
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-C01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-C02FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGC04"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-C01KYOUTU
           MOVE    SPA-FREE        TO  SPA-C02FREE
      *
           MOVE    "C06"                TO  SPA-MOTOPG
           MOVE    "C04"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "C04"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       240-USERBYOMEI-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       260-MAEGMN-SEC                  SECTION.
      *
           IF      SPA-GMN-C06-PAGE    =   1
               GO  TO  260-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-C06-PAGE    =   SPA-GMN-C06-PAGE    -   1
      *
           PERFORM 3103-BYOMEIINPUTSET-LST-SEC
      *
           MOVE    1                   TO  SPA-GMN-C06-CUR     
      *
           .
       260-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       270-ATOGMN-SEC                 SECTION.
      *
           IF      SPA-GMN-C06-NEXT    =   ZERO
               GO  TO  270-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-C06-PAGE
      *
           PERFORM 3103-BYOMEIINPUTSET-LST-SEC
      *
           MOVE    1                   TO  SPA-GMN-C06-CUR     
      *
           .
       270-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認処理
      *****************************************************************
       280-SAKUJYO-GID-SEC             SECTION.
      *
      *    セットコード未入力のとき
           IF      SPA-GMN-C06-BYOMEIINPUTSET
                                       =   SPACE
               MOVE    2                   TO  SPA-GMN-C06-CUR
               GO  TO  280-SAKUJYO-GID-EXT
           END-IF
      *
      *    追加のとき
           IF      SPA-NAI-C06-BYOMEIINPUTSET-OLD
                                       =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C06-CUR
               GO  TO  280-SAKUJYO-GID-EXT
           END-IF
      *
      *    セットコードのチェック
           IF    ( SPA-GMN-C06-BYOMEIINPUTSET
                                   NOT =   SPACE )
           AND   ( SPA-NAI-C06-BYOMEIINPUTSET
                                       =   SPACE )
               MOVE    "0033"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-C06-CUR
               GO  TO  280-SAKUJYO-GID-EXT
           END-IF
      *
           MOVE    "0104"              TO  SPA-CIDCD4
      *
          .
       280-SAKUJYO-GID-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       2801-SAKUJYO-SEC            SECTION.
      *
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                       TO  BYOSET-BYOMEIINPUTSET
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC      NOT =   ZERO 
               MOVE    "1002"          TO  SPA-ERRCD
               DISPLAY "BYOSET-KEY =>" BYOSET-KEY
               GO  TO   2801-SAKUJYO-EXT
            END-IF
      *
      *    病名セット一覧
           PERFORM 220-CLEAR-SEC
      *
           PERFORM 3103-BYOMEIINPUTSET-LST-SEC
      *
           MOVE    1                   TO  SPA-GMN-C06-CUR
          .
       2801-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定処理
      *****************************************************************
       250-KAKUTEI-SEC                 SECTION.
      *
      *    入力チェック
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 400-INPUT-CHK-SEC
           IF    ( SPA-ERRCD       NOT =   SPACE )
      *****OR    ( SPA-CIDCD       NOT =   SPACE )  
               GO  TO  250-KAKUTEI-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           INITIALIZE                      BYOMEIINPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
           MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                       TO   BYOSET-BYOMEIINPUTSET
           MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
           PERFORM 900-BYOMEIINPUTSET-READ-SEC
           IF      FLG-BYOMEIINPUTSET  =   1
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                           TO  BYOSET-BYOMEIINPUTSET
           END-IF
           MOVE    SPA-GMN-C06-BYOMEIINPUTSETNAME
                                       TO  BYOSET-BYOMEIINPUTSETNAME
           INITIALIZE                      BYOSET-BYOMEIINPUTCD-G
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDZ     FROM    1   BY  1
                   UNTIL   IDZ     >       6
               IF      SPA-GMN-C06-INPUTCD (IDZ)
                                   NOT =   SPACE
                   ADD     1                   TO  IDX
                   MOVE    SPA-GMN-C06-INPUTCD (IDZ)
                                               TO
                                           BYOSET-BYOMEIINPUTCD (IDX)
               END-IF
           END-PERFORM
      *
           IF      FLG-BYOMEIINPUTSET  =   1
               MOVE    SMCNDATE-YMD        TO  BYOSET-CREYMD
               MOVE    SMCNDATE-YMD        TO  BYOSET-UPYMD
               MOVE    SMCNDATE-HMS        TO  BYOSET-UPHMS
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    SMCNDATE-YMD        TO  USBYO-UPYMD
               MOVE    SMCNDATE-HMS        TO  USBYO-UPHMS
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1004"              TO  SPA-ERRCD
               DISPLAY MCP-FUNC "BYOSET-KEY =>" USBYO-KEY 
               GO  TO  250-KAKUTEI-EXT
           END-IF
      *
      *    変更前のコード削除処理
           IF    ( SPA-NAI-C06-BYOMEIINPUTSET-OLD
                                   NOT =   SPACE                      )
           AND   ( SPA-NAI-C06-BYOMEIINPUTSET-OLD
                                   NOT =   SPA-GMN-C06-BYOMEIINPUTSET )
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  BYOSET-HOSPNUM
               MOVE    SPA-NAI-C06-BYOMEIINPUTSET-OLD
                                           TO  BYOSET-BYOMEIINPUTSET
               MOVE    BYOMEIINPUTSET-REC  TO  MCPDATA-REC
      *
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               CALL    "ORCDBMAIN"         USING
                                               MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC      NOT =   ZERO 
                   MOVE    "1002"          TO  SPA-ERRCD
                   DISPLAY "BYOSET-KEY =>" BYOSET-KEY
                   GO  TO  250-KAKUTEI-EXT
                END-IF
           END-IF
      *
      *    病名セット一覧
           PERFORM 220-CLEAR-SEC
      *
           PERFORM 3103-BYOMEIINPUTSET-LST-SEC
      *
           MOVE    1                   TO  SPA-GMN-C06-CUR
           .
       250-KAKUTEI-EXT.
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
           IF      SPA-CIDCD4      NOT =   SPACE
               PERFORM 520-CIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "C06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           IF      FLG-ROW             =   1
               MOVE    ZERO                TO  WRK-C06-SETCDLST-ROW
           ELSE
               MOVE    C06-SETCDLST-ROW    TO  WRK-C06-SETCDLST-ROW
           END-IF
      * 
           INITIALIZE                  C06
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-C06-MAX
               MOVE    SPA-GMN-C06-TSELNUM  (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  C06-TRENNUM  (IDX)
               MOVE    SPA-GMN-C06-TBYOMEIINPUTSET(IDX)
                                           TO  C06-TSETCD   (IDX)
               MOVE    SPA-GMN-C06-TBYOMEIINPUTSETNAME (IDX)
                                           TO  C06-TSETNAME (IDX)
      *         
               IF      IDX             =   SPA-NAI-C06-SELNUM
                   MOVE    "T"             TO  C06-SETCDLST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  C06-SETCDLST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-C06-MAX     TO  C06-SETCDLST-COUNT
           MOVE    ZERO                TO  C06-SETCDLST-ROW
           IF      WRK-C06-SETCDLST-ROW
                                       >   ZERO
               MOVE    WRK-C06-SETCDLST-ROW
                                           TO  C06-SETCDLST-ROW
           END-IF
           IF      SPA-NAI-C06-SELNUM  >   ZERO
               MOVE    2                   TO  C06-SETCDLST-ROWATTR
           END-IF
      *
           MOVE    SPA-GMN-C06-SELNUM  TO  C06-SELNUM
           MOVE    SPA-GMN-C06-BYOMEIINPUTSET
                                       TO  C06-SETCD
           MOVE    SPA-GMN-C06-BYOMEIINPUTSETNAME
                                       TO  C06-SETNAME
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >      6
               MOVE    SPA-GMN-C06-INPUTCD(IDX)   TO  C06-INPUTCD (IDX)
               MOVE    SPA-GMN-C06-BYOMEI (IDX)   TO  C06-BYOMEI  (IDX)
               IF      SPA-NAI-C06-BYOMEITOP (IDX)
                                           =   "1"
                   MOVE    "◎"                TO  C06-BYOMEITOP (IDX)
               END-IF
           END-PERFORM
      *
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
             AND ( SPA-GMN-C06-CUR     =   ZERO )
               PERFORM 5011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-C06-CUR
               WHEN    1
                   MOVE  "SELNUM"          TO  MCP-WIDGET
               WHEN    2
                   MOVE  "SETCD"           TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SETNAME"         TO  MCP-WIDGET
               WHEN    4
                   MOVE  "INPUTCD1"        TO  MCP-WIDGET
               WHEN    5
                   MOVE  "INPUTCD2"        TO  MCP-WIDGET
               WHEN    6
                   MOVE  "INPUTCD3"        TO  MCP-WIDGET
               WHEN    7
                   MOVE  "INPUTCD4"        TO  MCP-WIDGET
               WHEN    8
                   MOVE  "INPUTCD5"        TO  MCP-WIDGET
               WHEN    9
                   MOVE  "INPUTCD6"        TO  MCP-WIDGET
               WHEN    99
                   MOVE  "B12"             TO  MCP-WIDGET
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
               WHEN    "SELNUM"
                   MOVE    2                   TO  SPA-GMN-C06-CUR
               WHEN    "SETCD"
                   MOVE    3                   TO  SPA-GMN-C06-CUR
               WHEN    "SETNAME"
                   MOVE    4                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD1"
                   MOVE    5                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD2"
                   MOVE    6                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD3"
                   MOVE    7                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD4"
                   MOVE    8                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD5"
                   MOVE    9                   TO  SPA-GMN-C06-CUR
               WHEN    "INPUTCD6"
                   MOVE    99                  TO  SPA-GMN-C06-CUR
               WHEN    "B12"
                   MOVE    99                  TO  SPA-GMN-C06-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー画面表示処理
      *****************************************************************
       510-ERRSET-SEC                  SECTION.
      *
           MOVE    SPACE               TO  CERR
           MOVE    SPA-ERRCD           TO  CERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE
                       "数字のみ７桁での入力はできません"
                                                       TO  CERR-ERRMSG
               WHEN    "0002"
                   STRING   WRK-ERR-BYOMEI     DELIMITED   BY  SPACE
                            "が複数入力されています"
                                               DELIMITED   BY  SIZE 
                                               INTO        CERR-ERRMSG
                   END-STRING
               WHEN    "0003"
                   MOVE    "対象のデータを選択してありません"
                                                       TO  CERR-ERRMSG
               WHEN    "0004"
                   MOVE    "自院病名マスタが存在しません"
                                                       TO  CERR-ERRMSG
               WHEN    "0006"
                   MOVE    "セットコード入力エラー"    TO  CERR-ERRMSG
               WHEN    "0007"
                   MOVE    "セット名入力エラー"        TO  CERR-ERRMSG
               WHEN    "0012"
                   MOVE    "選択番号入力エラー"        TO  CERR-ERRMSG
               WHEN    "0030"
                   MOVE
                      "先頭の文字が入力できない文字です"
                                                       TO  CERR-ERRMSG
               WHEN    "0031"
                   STRING   "病名マスタ、自院病名マスタ"
                                               DELIMITED   BY  SIZE
                            "又は予約病名コードに存在するコードです"
                                               DELIMITED   BY  SIZE 
                                               INTO        CERR-ERRMSG
                   END-STRING
               WHEN    "0032"
                   MOVE
                       "登録できないセットコードです"  TO  CERR-ERRMSG
               WHEN    "0033"
                   MOVE    "セットコード入力エラー"    TO  CERR-ERRMSG
               WHEN    "0034"
                   MOVE    "入力コードが未入力です"    TO  CERR-ERRMSG
               WHEN    "0035"
                   MOVE    "不明の入力コードが入力されています"
                                                       TO  CERR-ERRMSG
               WHEN    "0036"
                   MOVE
                      "最後の文字が入力できない文字です"
                                                       TO  CERR-ERRMSG
               WHEN    "0071"
                   MOVE    "全角入力して下さい"        TO  CERR-ERRMSG
               WHEN    "0072"
                   MOVE    "全角または半角のみで入力して下さい" 
                                                       TO  CERR-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  WRK-ERR-BYOMEI
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "C06"                TO  SPA-MOTOPG
           MOVE    "CERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "CERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面表示処理
      *****************************************************************
       520-CIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-CID4-FLG
      *
           MOVE    SPACE               TO  CID4
           INITIALIZE                      CID4
           MOVE    SPA-CIDCD4          TO  CID4-ID1CODE
           EVALUATE    SPA-CIDCD4
               WHEN    "0101"
                   STRING   "セットコードが登録済ですが、"
                                           DELIMITED   BY  SIZE
                            "表示の入力コードに変更しますか？"
                                            DELIMITED   BY  SIZE
                                            INTO    CID4-ID1MSG
                   END-STRING
               WHEN    "0104"
                   MOVE    "セットコードを削除します。よろしいですか？"
                                       TO  CID4-ID1MSG
           END-EVALUATE
      *
           EVALUATE    SPA-CIDCD4
               WHEN    "0104"
                   MOVE    "B01"               TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"               TO  MCP-WIDGET
           END-EVALUATE
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "C06"               TO  SPA-MOTOPG
           MOVE    "CID4"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "CID4"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-CIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自院病名読込
      *****************************************************************
       900-USBYO-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  USBYO-HOSPNUM
           MOVE    USERBYOMEI-REC      TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_userbyomei"    TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  USERBYOMEI-REC
                   MOVE    ZERO                TO  FLG-USERBYOMEI
               ELSE
                   INITIALIZE                  USERBYOMEI-REC
                   MOVE    1                   TO  FLG-USERBYOMEI
               END-IF
           ELSE
               MOVE    1               TO  FLG-USERBYOMEI
               INITIALIZE                  USERBYOMEI-REC
           END-IF
      *
           MOVE    "tbl_userbyomei"    TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-USBYO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-SEC     SECTION.
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomeiinputset"
                                           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-BYOMEIINPUTSET-READ-N-SEC
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           MOVE    "tbl_byomeiinputset"
                                       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEIINPUTSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名入力セット読込
      *****************************************************************
       900-BYOMEIINPUTSET-READ-N-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BYOMEIINPUTSET-REC
               MOVE    ZERO                TO  FLG-BYOMEIINPUTSET
           ELSE
               INITIALIZE                      BYOMEIINPUTSET-REC
               MOVE    1                   TO  FLG-BYOMEIINPUTSET
           END-IF
      *
           .
       900-BYOMEIINPUTSET-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                      BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                      BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           PERFORM 900-ORCDBMAIN-SEC
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
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
