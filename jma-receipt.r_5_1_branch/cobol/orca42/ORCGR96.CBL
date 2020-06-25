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
      *
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCGR96.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト作成−未請求患者設定
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/12/05    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *  04.01.02    NACL-藤原    07/01/21  月途中の負担者番号変更対応
      *                                     （生保の公費単独のみ） 
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/08/22  公費４種までの併用の記録対応
      *                                     レセプト種別はレセプト種別
      *                                     （紙レセプト）から表示する
      *
      *  04.04.01    NACL-藤原    08/12/02  自費レセプトの種別表示の変更
      *
      *  04.08.01    NACL-藤原    16/10/10  個別作成の最大を２０００に変更
      *  04.08.02    NACL-藤原    18/03/28  平成３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "R01COMMON-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    画面用ＳＰＡ
           COPY    "R01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYUKA           PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-READ            PIC 9(01).
      *     
           03  FLG-MISEIKYU-ROW    PIC 9(01).
           03  FLG-KAIJYO-ROW      PIC 9(01).
           03  FLG-SEIKYU-UMU      PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(05).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDXX                PIC 9(04).
      *
           03  IDX1                PIC 9(04). 
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-NUM             PIC ZZ9.
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG1        PIC X(09).
      *
           03  WRK-MISEIKYULST-ROW PIC S9(9)   BINARY.
           03  WRK-KAIJYOLST-ROW   PIC S9(9)   BINARY.
      *
           03  WRK-RECEERR         PIC X(200).
           03  WRK-RIDMSG          PIC X(70).
      *
           03  WRK-NAI-SRYYM-MAE   PIC 9(06).
           03  WRK-GMN-NYUGAIKBN-MAE
                                   PIC X(01).
           03  WRK-MOTOPG          PIC X(08).
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"
                                   REPLACING  //SPA-//
                                   BY         //WRK-SPA-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-LINE-MISEIKYU-MAX
                                   PIC 9(03)   VALUE   500.
           03  WRK-CONS-LINE-KAIJYO-MAX
                                   PIC 9(03)   VALUE   500.
           03  WRK-CONS-LINE-PTNUM-MAX
                                   PIC 9(03)   VALUE   100.
      *
           03  WRK-CONS-SRYYM-200610
                                   PIC 9(06)   VALUE   200610.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
           COPY    "CPRCF010.INC".
      *
           COPY    "CPKOHSKY.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      * 01  ORCSSYUACCAREA.
      *     COPY    "CPORCSSYUACC.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    レセプト種別取得パラメタ
           COPY    "CPORCSSYUBETU.INC".
      *     
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA42SCRAREA.INC".
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
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
      *****MOVE    SPA-FREE        TO  SPA-R01FREE
           MOVE    SPA-FREE        TO  SPA-R96-AREA
           MOVE    SPA-WORK        TO  SPA-R01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"         ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-R01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *****MOVE    SPA-R01FREE     TO  SPA-FREE
           MOVE    SPA-R96-AREA    TO  SPA-FREE
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "RERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "R96"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =  "P97"  
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-R96-AREA
                   MOVE    SPA-WORK        TO  SPA-R01KYOUTU
                   PERFORM 320-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *         
               MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "RID1"
                   PERFORM 330-RID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-R96-AREA
      *
      *    対象診療年月
           IF      SPA-SYSYMD(7:2)     >   "10"
               MOVE    SPA-SYSYMD(1:6)     TO  SPA-NAI-R96-SRYYM
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE  "61"                  TO  LNK-DAY6-IRAI
               MOVE  SPA-SYSYMD            TO  LNK-DAY6-KIJUN
               MOVE  "2"                   TO  LNK-DAY6-ZOGENPTN
               MOVE  -1                    TO  LNK-DAY6-ZOGEN
               CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN(1:6)
                                           TO  SPA-NAI-R96-SRYYM
           END-IF
           MOVE    SPA-NAI-R96-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-R96-SRYYM
      *
      *    入外区分
           IF      SPA-BEDSU       >   ZERO
               MOVE    "1"             TO  SPA-GMN-R96-NYUGAIKBNL    (1)
               MOVE    "入院"          TO  SPA-GMN-R96-NYUGAIKBNMEIL (1)
               MOVE    "2"             TO  SPA-GMN-R96-NYUGAIKBNL    (2)
               MOVE    "入院外"        TO  SPA-GMN-R96-NYUGAIKBNMEIL (2)
               MOVE    2               TO  SPA-GMN-R96-NYUGAIKBN-MAX
           ELSE
               MOVE    "2"             TO  SPA-GMN-R96-NYUGAIKBNL    (1)
               MOVE    "入院外"        TO  SPA-GMN-R96-NYUGAIKBNMEIL (1)
               MOVE    1               TO  SPA-GMN-R96-NYUGAIKBN-MAX
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-R96-NYUGAIKBN-MAX
               IF      SPA-R01-NYUGAIKBN 
                                       =   SPA-GMN-R96-NYUGAIKBNL (IDX)
                   MOVE    SPA-GMN-R96-NYUGAIKBNLST (IDX)
                                           TO  SPA-GMN-R96-NYUGAIKBN-G
                   MOVE    SPA-GMN-R96-NYUGAIKBN-MAX
                                           TO  IDX
               END-IF
           END-PERFORM
      *
      *    未請求解除患者一覧編集処理
           PERFORM 3100-KAIJYO-HENSYU-SEC
      *
      *    未請求患者一覧編集処理
           PERFORM 3101-MISEIKYU-HENSYU-SEC
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求解除患者一覧編集処理
      *****************************************************************
       3100-KAIJYO-HENSYU-SEC      SECTION.
      *
           INITIALIZE                  SPA-GMN-R96-KAIJYO-TABLE
                                       SPA-NAI-R96-KAIJYO-TABLE
      *
           MOVE    ZERO            TO  IDXX
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-GMN-R96-NYUGAIKBN  
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key14"         TO  WRK-MCP-PATHNAME
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           PERFORM UNTIL   FLG-SYUKA   =   1
                   OR      IDXX    >=  WRK-CONS-LINE-KAIJYO-MAX
               ADD     1           TO  IDXX
               MOVE    IDXX        TO  SPA-GMN-R96-KAIJYO-TRENNUM (IDXX)
      *
               MOVE    SYUKA-SRYYM TO  SPA-NAI-R96-KAIJYO-TSRYYM  (IDXX)
               MOVE    SYUKA-SRYYM TO  WRK-SYMD (1:6)
               MOVE    "01"        TO  WRK-SYMD (7:2) 
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG1(1:6)
                                   TO  SPA-GMN-R96-KAIJYO-TSRYYM  (IDXX)
      *
               MOVE    SYUKA-PTID  TO  SPA-NAI-R96-KAIJYO-TPTID   (IDXX)
               INITIALIZE              ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
               MOVE    SYUKA-PTID  TO  SPTID-PTID
               CALL    "ORCSPTID"  USING   ORCSPTIDAREA
                                           SPA-AREA
               MOVE    SPTID-PTNUM TO  SPA-GMN-R96-KAIJYO-TPTNUM  (IDXX)
      *
               MOVE    SYUKA-NYUGAIKBN
                                   TO  SPA-NAI-R96-KAIJYO-TNYUGAIKBN
                                                                  (IDXX)
      *
grpsys         MOVE    SPA-HOSPNUM TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-R96-KAIJYO-TPTID   (IDXX)
                                   TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME  TO  SPA-GMN-R96-KAIJYO-TNAME   (IDXX)
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDXX            TO  SPA-GMN-R96-KAIJYO-TBL-MAX
      *
           MOVE    1               TO  FLG-KAIJYO-ROW
           .
       3100-KAIJYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求患者一覧編集処理
      *****************************************************************
       3101-MISEIKYU-HENSYU-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-R96-MISEIKYU-TABLE
                                       SPA-GMN-R96-PTNUM-TABLE
                                       SPA-NAI-R96-MISEIKYU-TABLE
                                       SPA-GMN-R96-KOUMOKU
                                       SPA-NAI-R96-KOUMOKU
      *
           MOVE    ZERO            TO  IDXX
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-R96-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-R96-NYUGAIKBN  
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "01"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key13"         TO  WRK-MCP-PATHNAME
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           PERFORM UNTIL   FLG-SYUKA   =   1
                   OR      IDXX        >=  WRK-CONS-LINE-MISEIKYU-MAX
               ADD     1           TO  IDXX
               MOVE    IDXX        TO  SPA-GMN-R96-MISEIKYU-TRENNUM
                                                                  (IDXX)
      *
               MOVE    SYUKA-PTID  TO  SPA-NAI-R96-MISEIKYU-TPTID (IDXX)
      *
               INITIALIZE              ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM TO  SPTID-HOSPNUM
               MOVE    SPA-NAI-R96-MISEIKYU-TPTID (IDXX)
                                   TO  SPTID-PTID
               CALL    "ORCSPTID"  USING   ORCSPTIDAREA
                                           SPA-AREA
               MOVE    SPTID-PTNUM TO  SPA-GMN-R96-MISEIKYU-TPTNUM
                                                                  (IDXX)
      *
grpsys         MOVE    SPA-HOSPNUM TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-R96-MISEIKYU-TPTID (IDXX)
                                   TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME  TO  SPA-GMN-R96-MISEIKYU-TNAME (IDXX)
               MOVE    PTINF-BIRTHDAY
                                   TO  SPA-NAI-R96-MISEIKYU-TBIRTHDAY
                                                                  (IDXX)
      *
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME
                                   TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDXX            TO  SPA-GMN-R96-MISEIKYU-TBL-MAX    
      *
           MOVE    1               TO  FLG-MISEIKYU-ROW
                                       FLG-KAIJYO-ROW
      *
           IF      SPA-GMN-R96-MISEIKYU-TBL-MAX
                                   =   ZERO
               MOVE    4               TO  SPA-GMN-R96-CUR
           ELSE
               MOVE    3               TO  SPA-GMN-R96-CUR
           END-IF
           MOVE    4               TO  SPA-GMN-R96-CUR
           .
       3101-MISEIKYU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       320-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM   NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM   TO  R96-PTNUM
               PERFORM 41024-PTNUM-CHK-SEC
               MOVE    SPACE               TO  SPA-ERRCD
      *****    MOVE    5                   TO  SPA-GMN-R96-CUR
           END-IF
           .
       320-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面ＯＫ処理
      *****************************************************************
       330-RID1-SET-SEC            SECTION.
      *
           IF      SPA-RID1-FLG    =   "OK"
               EVALUATE    SPA-RIDCD
                   WHEN    "1001"
                       PERFORM 490-SETTEI-SEC
                   WHEN    "1002"
                       PERFORM 440-KAIJYO-SEC
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE           TO  SPA-RID1-FLG
           MOVE    SPACE           TO  SPA-RIDCD      
           .
       330-RID1-SET-EXT.
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
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *       クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        未請求解除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-KAIJYO-MAE-SEC
      *        前月分 
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-BACK-MONTH-SEC
      *        次月分 
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-NEXT-MONTH-SEC
      *        氏名検索 
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 490-KENSAKU-SEC
      *        未請求設定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-SETTEI-MAE-SEC
      *        入外区分表示切替え処理
               WHEN    "CLICKED"       ALSO    "B23"
                   PERFORM 490-NYUGAIKBN-CHANGE-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 41023-SELNUM-CHK-SEC
      *
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41024-PTNUM-CHK-SEC
      *
               WHEN    ANY             ALSO    "MISEIKYULST"
                   PERFORM 400-MISEIKYULST-SEL-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-R96-CUR
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF    ( SPA-NAI-R96-SRYYM
                                   NOT =   WRK-NAI-SRYYM-MAE     )
               OR    ( SPA-GMN-R96-NYUGAIKBN
                                   NOT =   WRK-GMN-NYUGAIKBN-MAE )
      *            該当診療年月の情報編集処理
                   PERFORM 3101-MISEIKYU-HENSYU-SEC
                   IF      SPA-GMN-R96-NYUGAIKBN
                                   NOT =   WRK-GMN-NYUGAIKBN-MAE
      *                未請求解除患者一覧編集処理
                       PERFORM 3100-KAIJYO-HENSYU-SEC
                   END-IF
               END-IF
           ELSE
               IF      SPA-ERRCD       =   "0103"
                   CONTINUE
               ELSE
                   INITIALIZE              SPA-GMN-R96-MISEIKYU-TABLE
                                           SPA-GMN-R96-PTNUM-TABLE
                                           SPA-NAI-R96-MISEIKYU-TABLE
               END-IF
           END-IF   
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *   入力チェック
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "CLICKED"       ALSO    "B12"
                   CONTINUE
               WHEN    OTHER
                   IF      SPA-NAI-R96-PTID    >   ZERO
                       MOVE    "0103"              TO  SPA-ERRCD
                       IF      SPA-NAI-R96-SELNUM  >   ZERO
                           MOVE    91              TO  SPA-GMN-R96-CUR
                       ELSE
                           MOVE    99              TO  SPA-GMN-R96-CUR
                       END-IF
                       GO  TO  410-INPUT-CHK-EXT
                   END-IF
           END-EVALUATE        
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4101-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
           MOVE    SPA-NAI-R96-SRYYM   TO  WRK-NAI-SRYYM-MAE
           MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  WRK-GMN-NYUGAIKBN-MAE
      *
           MOVE    R96-SRYYM           TO  SPA-GMN-R96-SRYYM
           MOVE    R96-NYUGAIKBN       TO  SPA-GMN-R96-NYUGAIKBN-G
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
           PERFORM 41021-SRYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
           PERFORM 41022-NYUGAIKBN-CHK-SEC
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月入力処理
      *****************************************************************
       41021-SRYYM-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-R96-SRYYM
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-GMN-R96-SRYYM   TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-GMN-R96-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-NAI-R96-SRYYM
               IF      SPA-NAI-R96-SRYYM   <   WRK-CONS-SRYYM-200610
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  SPA-GMN-R96-CUR
           END-IF
           .
       41021-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分  入力処理
      *****************************************************************
       41022-NYUGAIKBN-CHK-SEC                SECTION.
      *
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       SPA-GMN-R96-NYUGAIKBN-MAX
               IF      SPA-GMN-R96-NYUGAIKBN 
                               =   SPA-GMN-R96-NYUGAIKBNL (IDY)
                   MOVE    SPA-GMN-R96-NYUGAIKBNLST (IDY)
                                      TO  SPA-GMN-R96-NYUGAIKBN-G
                   MOVE    SPA-GMN-R96-NYUGAIKBN-MAX
                                      TO  IDY
               END-IF
           END-PERFORM
           IF      SPA-GMN-R96-NYUGAIKBNMEI
                                       =   SPACE
               MOVE    "0008"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2               TO  SPA-GMN-R96-CUR
           END-IF
      *             
           .
       41022-NYUGAIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括取得処理前チェック処理
      *****************************************************************
       4101-KAKUTEI-CHK-SEC        SECTION.
      *
      *
           .
       4101-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       41023-SELNUM-CHK-SEC            SECTION.
      *
           MOVE    3               TO  SPA-GMN-R96-CUR
      *
           INITIALIZE                  SPA-GMN-R96-KOUMOKU
                                       SPA-NAI-R96-KOUMOKU
                                       SPA-GMN-R96-PTNUM-TABLE
      *
           MOVE    R96-SELNUM      TO  SPA-GMN-R96-SELNUM
      *
      *    選択番号チェック
           IF      SPA-GMN-R96-SELNUM  =   ZERO
               MOVE    4               TO  SPA-GMN-R96-CUR 
           ELSE
               IF    ( SPA-GMN-R96-SELNUM  >
                                       SPA-GMN-R96-MISEIKYU-TBL-MAX )
               OR    ( SPA-GMN-R96-SELNUM  <   1                    )
                   MOVE    "0002"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-R96-SELNUM  TO  SPA-NAI-R96-SELNUM
      *
                   MOVE    SPA-GMN-R96-MISEIKYU-TPTNUM
                                               (SPA-NAI-R96-SELNUM)
                                           TO  SPA-GMN-R96-PTNUM
                   MOVE    SPA-NAI-R96-MISEIKYU-TPTID
                                               (SPA-NAI-R96-SELNUM)
                                           TO  SPA-NAI-R96-PTID
                   MOVE    SPA-GMN-R96-MISEIKYU-TNAME
                                               (SPA-NAI-R96-SELNUM)
                                           TO  SPA-GMN-R96-NAME
                   MOVE    SPA-NAI-R96-MISEIKYU-TBIRTHDAY
                                               (SPA-NAI-R96-SELNUM)
                                           TO  SPA-NAI-R96-BIRTHDAY
      *
      *            レセプト一覧編集処理
                   PERFORM 410231-PTNUMLST-HENSYU-SEC
      *
                   MOVE    SPA-NAI-R96-SELNUM  TO  R96-MISEIKYULST-ROW
               END-IF
           END-IF
      *
           .
       41023-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト一覧編集処理
      *****************************************************************
       410231-PTNUMLST-HENSYU-SEC       SECTION.
      *
           IF    SPA-GMN-R96-NYUGAIKBN =   "2"
               INITIALIZE                      JYURRK-REC
grpsys         MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-NAI-R96-SRYYM   TO  JYURRK-SRYYMD(1:6)
               MOVE    "% "                TO  JYURRK-SRYYMD(7:)
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                           TO  JYURRK-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID    TO  JYURRK-PTID
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  WRK-MCP-TABLE
               MOVE    "key40"             TO  WRK-MCP-PATHNAME
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
grpsys         MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
               MOVE    SPA-NAI-R96-PTID    TO  PTNYUINRRK-PTID
               MOVE    SPA-NAI-R96-SRYYM   TO  PTNYUINRRK-TENSTUYMD(1:6)
               MOVE    "01"                TO  PTNYUINRRK-TENSTUYMD(7:)
               MOVE    SPA-NAI-R96-SRYYM   TO  PTNYUINRRK-TENNYUYMD(1:6)
               MOVE    "31"                TO  PTNYUINRRK-TENNYUYMD(7:)
               MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
               MOVE    "tbl_ptnyuinrrk"    TO  WRK-MCP-TABLE
               MOVE    "key46"             TO  WRK-MCP-PATHNAME
           END-IF
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
           ELSE
               MOVE    1                   TO  FLG-READ
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    ZERO                TO  IDX1
      *
           IF      FLG-READ            =   ZERO
               INITIALIZE                  ORCSSYUBETUAREA
grpsys         MOVE    SPA-HOSPNUM     TO  LNK-SYUBETU-HOSPNUM
               MOVE    SPA-PREFNUM     TO  LNK-SYUBETU-PREFNUM
               MOVE    SPA-NAI-R96-SRYYM
                                       TO  LNK-SYUBETU-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  LNK-SYUBETU-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID    TO  LNK-SYUBETU-PTID
               MOVE    SPA-NAI-R96-BIRTHDAY
                                       TO  LNK-SYUBETU-BIRTHDAY
               CALL    "ORCSSYUBETU"   USING   ORCSSYUBETUAREA
                                               SPA-AREA
               IF      LNK-SYUBETU-RC  =   ZERO
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       100
                           OR      LNK-SYUBETU-TEISYUTUSAKI (IDY)
                                                       =   ZERO
                     IF      LNK-SYUBETU-TEISYUTUSAKI (IDY)
                                                       =   1   OR  2
                                                               OR  6  
                       ADD     1               TO  IDX1
      *
                       EVALUATE    LNK-SYUBETU-TEISYUTUSAKI (IDY)
                           WHEN    "1"
                               MOVE    "社保"  TO  
                                   SPA-GMN-R96-PTNUM-TTEISYUTUSAKI(IDX1)
                           WHEN    "2"
                               MOVE    "国保"  TO  
                                   SPA-GMN-R96-PTNUM-TTEISYUTUSAKI(IDX1)
                           WHEN    "6"
                               MOVE    "広域"  TO  
                                   SPA-GMN-R96-PTNUM-TTEISYUTUSAKI(IDX1)
                       END-EVALUATE
                       EVALUATE    LNK-SYUBETU-RECESYUBETU-Y  (IDY)
                           WHEN    "0000"
                               MOVE    "不明分"
                                   TO  SPA-GMN-R96-PTNUM-TRECESYUBETU
                                                              (IDX1)
                           WHEN    "6666"
                               MOVE    "特別療養費"
                                   TO  SPA-GMN-R96-PTNUM-TRECESYUBETU
                                                              (IDX1)
                           WHEN    "8888"
                               MOVE    "自費"
                                   TO  SPA-GMN-R96-PTNUM-TRECESYUBETU
                                                              (IDX1)
                           WHEN    OTHER
                               MOVE    LNK-SYUBETU-RECESYUBETU-Y  (IDY)
                                   TO  SPA-GMN-R96-PTNUM-TRECESYUBETU
                                                              (IDX1)
                       END-EVALUATE
                       MOVE    LNK-SYUBETU-HKNJANUM     (IDY)
                                   TO  SPA-GMN-R96-PTNUM-THKNJANUM
                                                              (IDX1)
                       EVALUATE    LNK-SYUBETU-HKNNUM (IDY)
                           WHEN    "002"
                           WHEN    "031"  THRU  "034"
                               EVALUATE    LNK-SYUBETU-HOJOKBN (IDY)
                                   WHEN    "1"
                                   WHEN    "A"
                                   WHEN    "D"
                                   WHEN    "G"
                                       MOVE    "職務上"            TO
                                           SPA-GMN-R96-PTNUM-THOJOKBN
                                                               (IDX1)
                                   WHEN    "2"
                                   WHEN    "B"
                                   WHEN    "E"
                                   WHEN    "H"
                                       MOVE    "下船後３ヶ月以内"  TO
                                           SPA-GMN-R96-PTNUM-THOJOKBN
                                                               (IDX1)
                                   WHEN    "3"
                                   WHEN    "C"
                                   WHEN    "F"
                                   WHEN    "I"
                                       MOVE    "通勤災害"          TO
                                           SPA-GMN-R96-PTNUM-THOJOKBN
                                                               (IDX1)
                               END-EVALUATE
                       END-EVALUATE
                     END-IF 
                   END-PERFORM
      *            医保分の請求がないとき
                   IF    ( LNK-SYUBETU-TEISYUTUSAKI (1)   >   ZERO )
                   AND   ( IDX1                           =   ZERO )
                   AND   ( SPA-NAI-R96-SELNUM             =   ZERO )
                       MOVE    "0010"              TO  SPA-ERRCD
                       GO  TO  410231-PTNUMLST-HENSYU-EXT
                   END-IF
               END-IF
           END-IF
      *
           MOVE    IDX1            TO  SPA-GMN-R96-PTNUM-TBL-MAX
      *
           IF      SPA-NAI-R96-SELNUM  >   ZERO
               MOVE    91              TO  SPA-GMN-R96-CUR
           ELSE
               MOVE    99              TO  SPA-GMN-R96-CUR
           END-IF
           .
       410231-PTNUMLST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       41024-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    4                   TO  SPA-GMN-R96-CUR
      *
           IF      SPA-NAI-R96-SELNUM      >   ZERO
               IF      SPA-GMN-R96-PTNUM   NOT =   R96-PTNUM
                   MOVE   "0004"               TO  SPA-ERRCD
               ELSE
                   MOVE    99                  TO  SPA-GMN-R96-CUR
               END-IF
               GO  TO  41024-PTNUM-CHK-EXT
           END-IF
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  41024-PTNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                      SPA-GMN-R96-KOUMOKU
                                           SPA-NAI-R96-KOUMOKU
                                           SPA-GMN-R96-PTNUM-TABLE
      *
           MOVE    R96-PTNUM           TO  SPA-GMN-R96-PTNUM
      *
           IF      SPA-GMN-R96-PTNUM       =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               INITIALIZE                      ORCSPTIDAREA
grpsys         MOVE    SPA-HOSPNUM             TO  SPTID-HOSPNUM
               MOVE    SPA-GMN-R96-PTNUM       TO  SPTID-PTNUM
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC        NOT =   ZERO
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    SPTID-PTNUM         TO  SPA-GMN-R96-PTNUM
                   MOVE    SPTID-PTID          TO  SPA-NAI-R96-PTID
      *
      *            患者マスター存在チェック
grpsys             MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-NAI-R96-PTID    TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF       NOT =   ZERO
                       MOVE    "0001"          TO  SPA-ERRCD
                   ELSE
                       MOVE    PTINF-NAME      TO  SPA-GMN-R96-NAME
                       MOVE    PTINF-BIRTHDAY  TO  SPA-NAI-R96-BIRTHDAY
                       MOVE    PTINF-TSTPTNUMKBN
                                               TO
                                               SPA-NAI-R96-TSTPTNUMKBN
                       IF      SPA-NAI-R96-TSTPTNUMKBN =   "1"
                           MOVE    "0006"              TO  SPA-ERRCD
                       END-IF 
                   END-IF
               END-IF
           END-IF
      *
      *
           IF      SPA-ERRCD           =   SPACE
      *        未請求設定一覧をチェック
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-R96-MISEIKYU-TBL-MAX
                   IF      SPA-NAI-R96-PTID
                                   =   SPA-NAI-R96-MISEIKYU-TPTID (IDX)
                       MOVE    IDX         TO  SPA-NAI-R96-SELNUM
                       MOVE    SPA-GMN-R96-MISEIKYU-TRENNUM
                                                   (SPA-NAI-R96-SELNUM)
                                           TO  SPA-GMN-R96-SELNUM
                       MOVE    SPA-NAI-R96-SELNUM
                                           TO  R96-MISEIKYULST-ROW
                       MOVE    SPA-GMN-R96-MISEIKYU-TBL-MAX
                                               TO  IDX
                   END-IF
               END-PERFORM
      *
      *        未請求解除一覧をチェック
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-R96-KAIJYO-TBL-MAX
                   IF    ( SPA-NAI-R96-PTID
                                   =   SPA-NAI-R96-KAIJYO-TPTID (IDX) )
                   AND   ( SPA-NAI-R96-SRYYM
                                   =   SPA-NAI-R96-KAIJYO-TSRYYM(IDX) )
                       MOVE    IDX     TO   SPA-NAI-R96-KAIJYO-SELNUM
                       MOVE    SPA-NAI-R96-KAIJYO-SELNUM
                                       TO  R96-KAIJYOLST-ROW
                       MOVE    SPA-GMN-R96-KAIJYO-TBL-MAX
                                       TO  IDX
                   END-IF
               END-PERFORM
      *
      *        レセプト一覧編集処理
               PERFORM 410231-PTNUMLST-HENSYU-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    ZERO                TO  SPA-NAI-R96-PTID
                   MOVE    4                   TO  SPA-GMN-R96-CUR
               END-IF
           END-IF
           .
       41024-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定一覧選択処理
      *****************************************************************
       400-MISEIKYULST-SEL-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-R96-MISEIKYU-TBL-MAX
               IF      R96-MISEIKYULST-SELECT (IDX)    =   "T"
                   IF      IDX                 =   SPA-NAI-R96-SELNUM
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  WRK-SELNUM
                       MOVE    SPA-GMN-R96-MISEIKYU-TBL-MAX
                                                   TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-R96-SELNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-R96-KOUMOKU
                                       SPA-GMN-R96-PTNUM-TABLE
      *
           MOVE    SPA-GMN-R96-MISEIKYU-TRENNUM (SPA-NAI-R96-SELNUM)
                                   TO  SPA-GMN-R96-SELNUM
      *
           MOVE    SPA-GMN-R96-MISEIKYU-TPTNUM     (SPA-NAI-R96-SELNUM)
                                   TO  SPA-GMN-R96-PTNUM
           MOVE    SPA-NAI-R96-MISEIKYU-TPTID      (SPA-NAI-R96-SELNUM)
                                   TO  SPA-NAI-R96-PTID
           MOVE    SPA-GMN-R96-MISEIKYU-TNAME      (SPA-NAI-R96-SELNUM)
                                   TO  SPA-GMN-R96-NAME
           MOVE    SPA-NAI-R96-MISEIKYU-TBIRTHDAY  (SPA-NAI-R96-SELNUM)
                                   TO  SPA-NAI-R96-BIRTHDAY
      *
      *    レセプト一覧編集処理
           PERFORM 410231-PTNUMLST-HENSYU-SEC
           .
       400-MISEIKYULST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前月分処理
      *****************************************************************
       450-BACK-MONTH-SEC               SECTION.
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-BACK-MONTH-EXT
           END-IF
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-R96-SRYYM       TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                TO  LNK-DAY6-KIJUN (7:2)  
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)
                                       TO  SPA-NAI-R96-SRYYM
      *
           MOVE    SPA-NAI-R96-SRYYM   TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-R96-SRYYM
      *
           MOVE    1                   TO  SPA-GMN-R96-PAGE
      *     
      *    該当診療年月の情報編集処理
           PERFORM 3101-MISEIKYU-HENSYU-SEC
           .
      *
       450-BACK-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    次月分処理
      *****************************************************************
       480-NEXT-MONTH-SEC               SECTION.
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  480-NEXT-MONTH-EXT
           END-IF
      *
      *    次月診療年月
           INITIALIZE                     STS-AREA-DAY
           INITIALIZE                     LNK-DAY6-AREA
           MOVE    "61"                   TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-R96-SRYYM      TO  LNK-DAY6-KIJUN (1:6)
           MOVE    "01"                   TO  LNK-DAY6-KIJUN (7:2)  
           MOVE    "2"                    TO  LNK-DAY6-ZOGENPTN
           MOVE    1                      TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"              USING   STS-AREA-DAY
                                                  LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN(1:6)   TO  SPA-NAI-R96-SRYYM
      *
           MOVE    SPA-NAI-R96-SRYYM       TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG1(1:6)   TO  SPA-GMN-R96-SRYYM
      *
           MOVE    1                   TO  SPA-GMN-R96-PAGE
      *
      *    該当診療年月の情報編集処理
           PERFORM 3101-MISEIKYU-HENSYU-SEC
           .
      *
       480-NEXT-MONTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索
      *****************************************************************
       490-KENSAKU-SEC             SECTION.
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-KENSAKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-R96-AREA        TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "R96"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
          .
      *
       490-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-R96-KOUMOKU
                                       SPA-NAI-R96-KOUMOKU
                                       SPA-GMN-R96-PTNUM-TABLE
      *
           MOVE    1               TO  FLG-MISEIKYU-ROW
                                       FLG-KAIJYO-ROW
      *
           MOVE    4               TO  SPA-GMN-R96-CUR
          .
      *
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定解除前処理
      *****************************************************************
       440-KAIJYO-MAE-SEC          SECTION.
      *
           IF      SPA-NAI-R96-PTID    =   ZERO
               MOVE    "0100"              TO  SPA-ERRCD
               GO  TO  440-KAIJYO-MAE-EXT
           END-IF
      *
           IF      SPA-NAI-R96-TSTPTNUMKBN =   "1"
               MOVE    "0006"              TO  SPA-ERRCD
               GO  TO  440-KAIJYO-MAE-EXT
           END-IF 
      *
           IF      SPA-NAI-R96-SELNUM  =   ZERO
               MOVE    "0101"              TO  SPA-ERRCD
               GO  TO  440-KAIJYO-MAE-EXT
           END-IF
      *
           IF    ( SPA-NAI-R96-KAIJYO-SELNUM
                                   =   ZERO                     )
           AND   ( SPA-GMN-R96-KAIJYO-TBL-MAX
                                   =   WRK-CONS-LINE-KAIJYO-MAX )
               MOVE    "0104"              TO  SPA-ERRCD
               GO  TO  440-KAIJYO-MAE-EXT
           END-IF
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-R96-PTID    TO  SYUKA-PTID
           MOVE    SPA-NAI-R96-SRYYM   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-R96-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key15"         TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   1
               MOVE    "0101"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-R96-CUR
           ELSE
               IF      SYUKA-MISEIKYU-FLG  =   "01"
      *****        MOVE    "1002"          TO  SPA-RIDCD
                   PERFORM 440-KAIJYO-SEC
               ELSE
                   MOVE    "0101"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       440-KAIJYO-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定解除処理
      *****************************************************************
       440-KAIJYO-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM     TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-R96-PTID
                                   TO  SYUKA-PTID
           MOVE    SPA-NAI-R96-SRYYM
                                   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-R96-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    "02"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SPA-TERMID      TO  SYUKA-TERMID
           MOVE    SPA-OPID        TO  SYUKA-OPID
           MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
           MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
           MOVE    SYUKA-REC       TO  MCPDATA-REC
      *
           MOVE    "DBUPDATE"      TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "upd4"          TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "SYUKA KEY=" 
                                   MCP-FUNC " " SYUKA-KEY
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
      *        未請求解除患者一覧編集処理
               PERFORM 3100-KAIJYO-HENSYU-SEC
      *
      *        未請求患者一覧編集処理
               PERFORM 3101-MISEIKYU-HENSYU-SEC
           END-IF
           .
      *
       440-KAIJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定前処理
      *****************************************************************
       490-SETTEI-MAE-SEC                SECTION.
      *
      *    入力チェック
           MOVE    0               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-SETTEI-MAE-EXT
           END-IF
      *
           IF      SPA-NAI-R96-TSTPTNUMKBN =   "1"
               MOVE    "0006"              TO  SPA-ERRCD
               GO  TO  490-SETTEI-MAE-EXT
           END-IF 
      *
           IF      SPA-NAI-R96-PTID        =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO  TO  490-SETTEI-MAE-EXT
           END-IF 
      *
      *    請求管理テーブルの存在チェック
           INITIALIZE                  RECE10-REC
grpsys     MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    SPA-NAI-R96-SRYYM
                                   TO  RECE10-SRYYM
           MOVE    SPA-GMN-R96-NYUGAIKBN
                                   TO  RECE10-NYUGAIKBN
           MOVE    SPA-NAI-R96-PTID
                                   TO  RECE10-PTID
           PERFORM 900-SEIKYU-READ-SEC
           IF      FLG-SEIKYU-UMU  =   1
               MOVE    "1001"          TO  SPA-RIDCD
           ELSE
      *        未請求設定処理
               MOVE    SPACE           TO  SPA-RIDCD     
               PERFORM 490-SETTEI-SEC
           END-IF
           .
       490-SETTEI-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    未請求設定処理
      *****************************************************************
       490-SETTEI-SEC             SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-RIDCD      =   "1001"
      *        請求管理削除
               INITIALIZE                  RECE10-REC
grpsys         MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
               MOVE    SPA-NAI-R96-SRYYM
                                       TO  RECE10-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  RECE10-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID
                                       TO  RECE10-PTID
               MOVE    1               TO  RECE10-TEISYUTUSAKI
               MOVE    2               TO  RECE10-XXTEISYUTUSAKI 
               MOVE    RECE10-REC      TO  MCPDATA-REC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "del7"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "SEIKYU KEY=" RECE10-KEY
                   MOVE    "1002"          TO  SPA-ERRCD
                   GO  TO  490-SETTEI-EXT
               END-IF
      *
               INITIALIZE                  RECE10-REC
               MOVE    SPA-HOSPNUM     TO  RECE10-HOSPNUM
               MOVE    SPA-NAI-R96-SRYYM
                                       TO  RECE10-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  RECE10-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID
                                       TO  RECE10-PTID
               MOVE    6               TO  RECE10-TEISYUTUSAKI
               MOVE    6               TO  RECE10-XXTEISYUTUSAKI 
               MOVE    RECE10-REC      TO  MCPDATA-REC
               MOVE    "tbl_seikyu"    TO  MCP-TABLE
               MOVE    "del7"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "SEIKYU KEY=" RECE10-KEY
                   MOVE    "1002"          TO  SPA-ERRCD
                   GO  TO  490-SETTEI-EXT
               END-IF
      *
      *        公費請求書削除
               INITIALIZE                  KOHSKY-REC
grpsys         MOVE    SPA-HOSPNUM     TO  KOHSKY-HOSPNUM
               MOVE    SPA-NAI-R96-SRYYM
                                       TO  KOHSKY-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  KOHSKY-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID
                                       TO  KOHSKY-PTID
               MOVE    KOHSKY-REC      TO  MCPDATA-REC
               MOVE    "tbl_kohsky"    TO  MCP-TABLE
               MOVE    "del4"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "KOHKSY KEY=" KOHSKY-KEY
                   MOVE    "1003"          TO  SPA-ERRCD
                   GO  TO  490-SETTEI-EXT
               END-IF
      *
      *        レセ電削除
               INITIALIZE                  RECEDEN-REC
grpsys         MOVE    SPA-HOSPNUM     TO  RECEDEN-HOSPNUM
               MOVE    SPA-NAI-R96-SRYYM
                                       TO  RECEDEN-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  RECEDEN-NYUGAIKBN
               MOVE    SPA-NAI-R96-PTID
                                       TO  RECEDEN-PTID
               MOVE    RECEDEN-REC     TO  MCPDATA-REC
               MOVE    "tbl_receden"   TO  MCP-TABLE
               MOVE    "del2"          TO  MCP-PATHNAME
               MOVE    "DBDELETE"      TO  MCP-FUNC
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   DISPLAY "RECEDEN KEY=" RECEDEN-KEY
                   MOVE    "1004"          TO  SPA-ERRCD
                   GO  TO  490-SETTEI-EXT
               END-IF
           END-IF
      *
           INITIALIZE                  SYUKA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
           MOVE    SPA-NAI-R96-PTID    TO  SYUKA-PTID
           MOVE    SPA-NAI-R96-SRYYM   TO  SYUKA-SRYYM
           MOVE    SPA-GMN-R96-NYUGAIKBN
                                   TO  SYUKA-NYUGAIKBN
           MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           MOVE    SYUKA-REC       TO  MCPDATA-REC
           MOVE    "tbl_syuka"     TO  WRK-MCP-TABLE
           MOVE    "key15"         TO  WRK-MCP-PATHNAME
           PERFORM 900-SYUKA-READ-SEC
           IF      FLG-SYUKA       =   1
               INITIALIZE                  SYUKA-REC
grpsys         MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
               MOVE    SPA-NAI-R96-PTID    TO  SYUKA-PTID
               MOVE    SPA-NAI-R96-SRYYM   TO  SYUKA-SRYYM
               MOVE    SPA-GMN-R96-NYUGAIKBN
                                       TO  SYUKA-NYUGAIKBN
               MOVE    "YYYY"          TO  SYUKA-RECESYUBETU
           END-IF
      *
           MOVE    "01"            TO  SYUKA-MISEIKYU-FLG
           MOVE    SPA-TERMID      TO  SYUKA-TERMID
           MOVE    SPA-OPID        TO  SYUKA-OPID
      *
           IF      FLG-SYUKA       =   1
               MOVE    SMCNDATE-YMD        TO  SYUKA-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
           ELSE
               MOVE    SMCNDATE-YMD        TO  SYUKA-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUKA-UPHMS
           END-IF
           MOVE    SYUKA-REC       TO  MCPDATA-REC
      *
           IF      FLG-SYUKA       =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "upd4"          TO  MCP-PATHNAME
           ELSE
               MOVE    "DBINSERT"      TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
           END-IF
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               DISPLAY "SYUKA KEY=" 
                                   MCP-FUNC " " SYUKA-KEY
               MOVE    "1001"          TO  SPA-ERRCD
           ELSE
      *        未請求解除患者一覧編集処理
               PERFORM 3100-KAIJYO-HENSYU-SEC
      *
      *        未請求患者一覧編集処理
               PERFORM 3101-MISEIKYU-HENSYU-SEC
           END-IF
           .
      *
       490-SETTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分表示切替処理
      *****************************************************************
       490-NYUGAIKBN-CHANGE-SEC          SECTION.
      *
           PERFORM 400-ENTRY-SYORI-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  490-NYUGAIKBN-CHANGE-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-R96-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"         TO  R96-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"         TO  R96-NYUGAIKBN
           END-EVALUATE
      *
           PERFORM 400-ENTRY-SYORI-SEC
           .
       490-NYUGAIKBN-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *    
           MOVE    SPACE               TO  SPA-R01-GMNKBN
      *
           MOVE    SPA-R01KYOUTU       TO  SPA-WORK       
      *
           MOVE    SPA-R01-MOTOPG      TO  SPA-SAKIPG
           MOVE    "R96"               TO  SPA-MOTOPG
      *
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
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG1
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG1
      *        MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               
           END-IF
           .
       5002-HIZUKE-HEN-EXT.
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
           IF      SPA-RIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "R96    "           TO  MCP-WINDOW
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
           IF      FLG-MISEIKYU-ROW    =   1
               MOVE    ZERO                TO  WRK-MISEIKYULST-ROW
           ELSE
               MOVE    R96-MISEIKYULST-ROW TO  WRK-MISEIKYULST-ROW
           END-IF
           IF      FLG-KAIJYO-ROW      =   1
               MOVE    ZERO                TO  WRK-KAIJYOLST-ROW
           ELSE
           MOVE    R96-KAIJYOLST-ROW       TO  WRK-KAIJYOLST-ROW
           END-IF
      * 
           INITIALIZE                      R96
      *
           MOVE    SPA-GMN-R96-SRYYM       TO  R96-SRYYM
      *
           MOVE    SPA-GMN-R96-NYUGAIKBN-G TO  R96-NYUGAIKBN
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-R96-NYUGAIKBN-MAX
               MOVE    SPA-GMN-R96-NYUGAIKBNLST (IDX)
                                       TO  R96-NYUGAIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-R96-NYUGAIKBN-MAX
                                       TO  R96-NYUGAIKBN-COUNT
      *
           MOVE    SPA-GMN-R96-SELNUM      TO  R96-SELNUM
           MOVE    SPA-GMN-R96-PTNUM       TO  R96-PTNUM
           MOVE    SPA-GMN-R96-NAME        TO  R96-NAME
      *          
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-R96-MISEIKYU-TBL-MAX
               MOVE    SPA-GMN-R96-MISEIKYU-TRENNUM (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  R96-MISEIKYU-TNUM    (IDX)
               MOVE    SPA-GMN-R96-MISEIKYU-TPTNUM  (IDX)
                                       TO  R96-MISEIKYU-TPTNUM  (IDX)
               MOVE    SPA-GMN-R96-MISEIKYU-TNAME   (IDX)
                                       TO  R96-MISEIKYU-TNAME   (IDX)
      *
               IF      IDX         =   SPA-NAI-R96-SELNUM
                   MOVE    "T"         TO  R96-MISEIKYULST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  R96-MISEIKYULST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-R96-MISEIKYU-TBL-MAX
                                       TO  R96-MISEIKYULST-COUNT
           MOVE    ZERO                TO  R96-MISEIKYULST-ROW
           IF      WRK-MISEIKYULST-ROW >   ZERO
               MOVE    WRK-MISEIKYULST-ROW TO  R96-MISEIKYULST-ROW
           END-IF
      *          
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-R96-PTNUM-TBL-MAX
               MOVE    SPA-GMN-R96-PTNUM-TTEISYUTUSAKI (IDX)
                                       TO  R96-PTNUM-TTEISYUTUSAKI (IDX)
               MOVE    SPA-GMN-R96-PTNUM-TRECESYUBETU(IDX)
                                       TO  R96-PTNUM-TRECESYUBETU  (IDX)
               MOVE    SPA-GMN-R96-PTNUM-THKNJANUM   (IDX)
                                       TO  R96-PTNUM-THKNJANUM     (IDX)
               MOVE    SPA-GMN-R96-PTNUM-THOJOKBN    (IDX)
                                       TO  R96-PTNUM-THOJOKBN      (IDX)
           END-PERFORM
           MOVE    SPA-GMN-R96-PTNUM-TBL-MAX
                                       TO  R96-PTNUMLST-COUNT
      *          
           PERFORM VARYING     IDX FROM    1   BY  1
                   UNTIL       IDX >       SPA-GMN-R96-KAIJYO-TBL-MAX
               MOVE    SPA-GMN-R96-KAIJYO-TRENNUM (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  R96-KAIJYO-TNUM    (IDX)
               MOVE    SPA-GMN-R96-KAIJYO-TSRYYM  (IDX)
                                       TO  R96-KAIJYO-TSRYYM  (IDX)
               MOVE    SPA-GMN-R96-KAIJYO-TPTNUM  (IDX)
                                       TO  R96-KAIJYO-TPTNUM  (IDX)
               MOVE    SPA-GMN-R96-KAIJYO-TNAME   (IDX)
                                       TO  R96-KAIJYO-TNAME   (IDX)
      *
               IF      IDX         =   SPA-NAI-R96-KAIJYO-SELNUM
                   MOVE    "T"         TO  R96-KAIJYOLST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  R96-KAIJYOLST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-R96-KAIJYO-TBL-MAX
                                       TO  R96-KAIJYOLST-COUNT
           MOVE    ZERO                TO  R96-KAIJYOLST-ROW
           IF      WRK-KAIJYOLST-ROW   >   ZERO
               MOVE    WRK-KAIJYOLST-ROW   TO  R96-KAIJYOLST-ROW
           END-IF
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
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-GMN-R96-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-R96-CUR
               WHEN    1
                   MOVE  "SRYYM"       TO  MCP-WIDGET
               WHEN    2
                   MOVE  "NYUGAIKBN"   TO  MCP-WIDGET
               WHEN    3
                   MOVE  "SELNUM"      TO  MCP-WIDGET
               WHEN    4
                   MOVE  "PTNUM"       TO  MCP-WIDGET
               WHEN    91 
                   MOVE  "B04"         TO  MCP-WIDGET   
               WHEN    98 
                   MOVE  "B01"         TO  MCP-WIDGET   
               WHEN    99
                   MOVE  "B12"         TO  MCP-WIDGET   
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SRYYM"
                   MOVE    2           TO  SPA-GMN-R96-CUR
               WHEN    "NYUGAIKBN"
                   MOVE    3           TO  SPA-GMN-R96-CUR
               WHEN    "SELNUM"
                   MOVE    4           TO  SPA-GMN-R96-CUR
               WHEN    "PTNUM"
                   MOVE    99          TO  SPA-GMN-R96-CUR
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "患者番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "選択番号入力エラー"        TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "該当の患者が存在しません"  TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "他の患者番号を選択中です"  TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "テスト患者の設定はできません"
                                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "診療年月入力エラー"        TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "入外区分入力エラー"        TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "平成１８年１０月以降のみ設定可能です"
                                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "医保分の診療がないため未請求設定できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "正しい患者を入力して下さい"
                                                       TO  SPA-ERRMSG
      *
               WHEN    "0100"
                   MOVE    "患者番号が未入力です"      TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE
                       "未請求設定が行われていませんので解除できません"
                                                       TO  SPA-ERRMSG
               WHEN    "0103"
                   MOVE    "患者番号入力中です"        TO  SPA-ERRMSG
               WHEN    "0104"
                   STRING  "登録の上限に達しました。一度解除した"
                                           DELIMITED   BY  SIZE 
                           "患者で個別処理を行って下さい"
                                           DELIMITED   BY  SIZE 
                                                       INTO   SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1001"
                   MOVE    "未請求設定ができませんでした"
                                                       TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "未請求設定の解除ができませんでした"
                                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RERR
           MOVE    SPA-ERRCD            TO  RERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  RERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "R96"                TO  SPA-MOTOPG
           MOVE    "RERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RERR"               TO  MCP-WINDOW
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
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-RIDMSG
      *
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE
                      "請求データを削除して設定します。よろしいですか。"
                                               TO  WRK-RIDMSG
               WHEN    "1002"
                   MOVE    "未請求設定を解除します。よろしいですか。"
                                               TO  WRK-RIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  RID1
           INITIALIZE                       RID1
           MOVE    SPA-RIDCD            TO  RID1-ID1CODE
           MOVE    WRK-RIDMSG           TO  RID1-ID1MSG
           MOVE    "ＮＯ"               TO  RID1-B01-LABEL
           EVALUATE    SPA-RIDCD
               WHEN    "1001"
                   MOVE    "B01"                TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE    "B12"                TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "R96"                TO  SPA-MOTOPG
           MOVE    "RID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "RID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
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
      *    主科情報読込
      *****************************************************************
       900-SYUKA-READ-SEC         SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 910-SYUKA-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUKA
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科情報読込
      *****************************************************************
       910-SYUKA-NEXT-SEC         SECTION.
      * 
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC               =   ZERO
               MOVE    ZERO                 TO  FLG-SYUKA
               MOVE    MCPDATA-REC          TO  SYUKA-REC
           ELSE
               MOVE    1                    TO  FLG-SYUKA
               INITIALIZE                       SYUKA-REC
           END-IF
      *
           .
       910-SYUKA-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（医保の対象データの存在チェック）
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SEIKYU-UMU
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           PERFORM                 UNTIL   FLG-SEIKYU      =   1
                                   OR      FLG-SEIKYU-UMU  =   1
               IF      RECE10-TEISYUTUSAKI =   1   OR  2   OR  6
                   MOVE    1               TO  FLG-SEIKYU-UMU
               END-IF
      * 
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key59"             TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key59"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
               MOVE    MCPDATA-REC     TO  RECE10-REC
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-NEXT-EXT.
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
