      ********************************************************************
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
       PROGRAM-ID.             ORCGH03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 返戻データ取込一覧
      *  管理者            : 
      *  作成日付    作業者        記述
      *  15/09/14    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *  04.08.03    NACL-藤原    18/03/28  平成３０年４月改正対応
      *****************************************************************
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理スパ領域
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HENREI-K        PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-HENREI-B-CHK    PIC 9(01).
      *
           03  FLG-KANRI-ROW       PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDXY                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDZZ                PIC 9(04).
      *
           03  IDWW                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-SELNO           PIC 9(05).
      *
           03 WRK-ZZ9              PIC ZZ9.
      *
           03  WRK-H03-BODY-ROW    PIC S9(09).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(02).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-H03-KANRI-ROW   PIC S9(09).
      *
           03  WRK-RECEIPT-NO      PIC X(06).
      *
           01  CHK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //CHK-HENREI-B-//.
      *
           COPY    "CPSK1001.INC"  REPLACING   //SYS-1001-//
                                   BY          //CHK-1001-//.
      *
       01  WRK-HEN-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-HENYMDG.
               05  WRK-HENYM       PIC X(06).
               05  WRK-HENYM-FIL   PIC X(03).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-CONS-AREA.
           03  WRK-CONS-KANRI-LINE-MAX
                                   PIC 9(04)   VALUE   100.
           03  WRK-CONS-BODY-LINE-MAX
                                   PIC 9(04)   VALUE   100.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HENREI-K-REC.
           COPY    "CPHENREI-KANRI.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
           COPY    "CPSK1001.INC".
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
           COPY    "CPORCSPTNUM.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "H01.INC".
           COPY    "H02.INC".
           COPY    "H03.INC".
           COPY    "H99.INC".
           COPY    "HERR.INC".
           COPY    "HID1.INC".
      *
       PROCEDURE                       DIVISION    USING
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
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
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
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           IF      SPA-MOTOPG          =   "HERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "H03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
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
           MOVE    SPACE           TO  SPA-H03-AREA
           INITIALIZE                  SPA-H03-AREA
      *
           INITIALIZE                  HENREI-K-REC
           MOVE   SPA-HOSPNUM      TO  HENREI-K-HOSPNUM
           MOVE    HENREI-K-REC    TO  MCPDATA-REC
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_kanri"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-KANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-K
               INITIALIZE                  HENREI-K-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM         UNTIL   FLG-HENREI-K
                                           =   1
                           OR      IDX     >   WRK-CONS-KANRI-LINE-MAX
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-H03-K-TSEQNUM   (IDX)
               MOVE    HENREI-K-SYORIYM
                                       TO  SPA-NAI-H03-K-TSYORIYM  (IDX)
                                           WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG (1:6)
                                       TO  SPA-GMN-H03-K-TSYORIYM  (IDX)
               MOVE    HENREI-K-TEISYUTUSAKI
                                       TO  SPA-NAI-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
               EVALUATE    HENREI-K-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"  TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                   WHEN    "2"
                       MOVE    "国保"  TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
                   WHEN    "3"
                       MOVE    "労災"  TO  SPA-GMN-H03-K-TTEISYUTUSAKI
                                                                   (IDX)
               END-EVALUATE
               MOVE    HENREI-K-TEISYUTUSAKI2
                                       TO  SPA-NAI-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
               EVALUATE    HENREI-K-TEISYUTUSAKI2
                   WHEN    "0"
                       MOVE    "　"    TO  SPA-GMN-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
                   WHEN    "1"
                       MOVE    "保"    TO  SPA-GMN-H03-K-TTEISYUTUSAKI2
                                                                   (IDX)
               END-EVALUATE
               MOVE    HENREI-K-RECV-PATIENT-CNT
                                       TO  SPA-GMN-H03-K-TCNT      (IDX)
               MOVE    HENREI-K-CREYMD TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-H03-K-TCREYMD   (IDX)
               MOVE    HENREI-K-HOSPCD TO  SPA-NAI-H03-K-THOSPCD   (IDX)
      *??
           DISPLAY "HENREI-K-HOSPCD=" IDX "#" HENREI-K-HOSPCD "#"
      *??
               INITIALIZE                  SYS-1001-REC
               MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
               MOVE    "1001"          TO  SYS-1001-KANRICD
               MOVE    "*"             TO  SYS-1001-KBNCD
               MOVE    HENREI-K-SYORIYM
                                       TO  SYS-1001-STYUKYMD (1:6)
               MOVE    "01"            TO  SYS-1001-STYUKYMD (7:2)
               MOVE    SYS-1001-STYUKYMD
                                       TO  SYS-1001-EDYUKYMD
               MOVE    SYS-1001-REC    TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI    =   ZERO
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
      *??
           DISPLAY "SYS-1001-HOSPCD=" SYS-1001-HOSPCD "#"
      *??
                   IF      HENREI-K-HOSPCD NOT =   SPACE
                       IF      SYS-1001-HOSPCD
                                           NOT =   HENREI-K-HOSPCD
                           MOVE    "旧"            TO
                                             SPA-GMN-H03-K-THOSPCD (IDX)
                       END-IF
                   ELSE
                       MOVE    SYS-1001-REC    TO  CHK-1001-REC
      *
                       INITIALIZE                  CHK-HENREI-B-REC
                       MOVE    SPA-HOSPNUM     TO  CHK-HENREI-B-HOSPNUM
                       MOVE    HENREI-K-SYORIYM
                                               TO  CHK-HENREI-B-SYORIYM
                       MOVE    HENREI-K-TEISYUTUSAKI
                                               TO
                                           CHK-HENREI-B-TEISYUTUSAKI
                       MOVE    HENREI-K-TEISYUTUSAKI2
                                               TO
                                           CHK-HENREI-B-TEISYUTUSAKI2
                       MOVE    HENREI-K-HOSPCD TO CHK-HENREI-B-HOSPCD
                       MOVE    CHK-HENREI-B-REC
                                               TO  MCPDATA-REC
                       MOVE    "key4"          TO  WRK-MCP-PATHNAME
                       PERFORM 900-HENREI-BODY-READ-SEC
                       IF      FLG-HENREI-B-CHK    =   ZERO
                           INITIALIZE                  SYS-1001-REC
                           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
                           MOVE    "1001"          TO  SYS-1001-KANRICD
                           MOVE    "*"             TO  SYS-1001-KBNCD
                           MOVE    CHK-HENREI-B-SRYYM  TO
                                               SYS-1001-STYUKYMD (1:6)
                           MOVE    "01"            TO
                                               SYS-1001-STYUKYMD (7:2)
                           MOVE    SYS-1001-STYUKYMD
                                                   TO  SYS-1001-EDYUKYMD
                           MOVE    SYS-1001-REC    TO  MCPDATA-REC
                           PERFORM 900-SYSKANRI-READ-SEC
                           IF      FLG-SYSKANRI    =   ZERO
                               MOVE    MCPDATA-REC     TO  SYS-1001-REC
      *??
           DISPLAY "BODY SYS-1001-HOSPCD=" SYS-1001-HOSPCD "#"
      *??
                               IF      SYS-1001-HOSPCD
                                               NOT =   CHK-1001-HOSPCD
                                   MOVE    "旧"            TO
                                           SPA-GMN-H03-K-THOSPCD (IDX)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *??
           DISPLAY "THOSPCD=" SPA-GMN-H03-K-THOSPCD (IDX) "#"
      *??
      *
               INITIALIZE                      CHK-HENREI-B-REC
               MOVE    SPA-HOSPNUM         TO  CHK-HENREI-B-HOSPNUM
               MOVE    HENREI-K-SYORIYM    TO  CHK-HENREI-B-SYORIYM
               MOVE    HENREI-K-TEISYUTUSAKI
                                           TO  CHK-HENREI-B-TEISYUTUSAKI
               MOVE    HENREI-K-TEISYUTUSAKI2
                                           TO CHK-HENREI-B-TEISYUTUSAKI2
               MOVE    HENREI-K-HOSPCD     TO CHK-HENREI-B-HOSPCD
               MOVE    CHK-HENREI-B-REC    TO  MCPDATA-REC
               MOVE    "key6"              TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      FLG-HENREI-B-CHK    =   ZERO
                   MOVE  CHK-HENREI-B-ERRCD    TO
                                       SPA-GMN-H03-K-TERRCD    (IDX)
               END-IF
     *
               MOVE    "tbl_henrei_kanri"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-KANRI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_kanri"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           MOVE    IDX             TO  SPA-GMN-H03-KANRI-MAX
      *
           MOVE    "　記号・番号　"
                                   TO  SPA-GMN-H03-B-HYODAI
      *
           MOVE   1                TO  FLG-KANRI-ROW
      *
           MOVE    1               TO  SPA-GMN-H03-CUR
      *
           .
       310-SPASET-EXT.
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
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
                WHEN   "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
                WHEN   ANY             ALSO    "KANRI_LIST"
                   PERFORM 450-KANRILST-SELECT-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-H03-CUR
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-GMN-H03-KOUMOKU
                                           SPA-NAI-H03-KOUMOKU
      *
           MOVE    H03-SELNUM          TO  SPA-GMN-H03-SELNUM
      *
           IF      SPA-GMN-H03-SELNUM  =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-GMN-H03-SELNUM  >   SPA-GMN-H03-KANRI-MAX
                   MOVE    "0001"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-H03-SELNUM  TO  SPA-NAI-H03-SELNUM
                                               H03-KANRI-LIST-ROW
      *        返戻取り込みデータ明細情報編集処理
               PERFORM 4101-HENREI-BODY-HENSYU-SEC
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   返戻取り込みデータ明細情報編集処理
      *****************************************************************
       4101-HENREI-BODY-HENSYU-SEC            SECTION.
      *
           INITIALIZE                      SPA-GMN-H03-BODY-TBL
                                           SPA-NAI-H03-BODY-TBL
      *
           IF      SPA-NAI-H03-K-TTEISYUTUSAKI (SPA-NAI-H03-SELNUM)
                                       =   "3"
               MOVE    "年金証書番号等"    TO  SPA-GMN-H03-B-HYODAI
           ELSE
               MOVE    "　記号・番号　"    TO  SPA-GMN-H03-B-HYODAI
           END-IF
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    SPA-NAI-H03-K-TSYORIYM       (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-SYORIYM
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI  (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI
           MOVE    SPA-NAI-H03-K-TTEISYUTUSAKI2 (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-TEISYUTUSAKI2
           MOVE    SPA-NAI-H03-K-THOSPCD        (SPA-NAI-H03-SELNUM)
                                       TO  HENREI-B-HOSPCD
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM         UNTIL   FLG-HENREI-B    =   1
                           OR      IDX             >=
                                               WRK-CONS-BODY-LINE-MAX
               ADD     1               TO  IDX
               MOVE    IDX             TO  SPA-GMN-H03-BODY-MAX
                                           SPA-GMN-H03-B-TSEQNUM   (IDX)
      *
               MOVE    HENREI-B-NYUGAIKBN
                                       TO  SPA-NAI-H03-B-TNYUGAIKBN(IDX)
               MOVE    HENREI-B-PTID   TO  SPA-NAI-H03-B-TPTID     (IDX)
               MOVE    HENREI-B-PTNUM  TO  SPA-GMN-H03-B-TPTNUM    (IDX)
               MOVE    HENREI-B-NAME   TO  SPA-GMN-H03-B-TNAME     (IDX)
               MOVE    HENREI-B-SRYYM  TO  WRK-SYMD (1:6)
               MOVE    "01"            TO  WRK-SYMD (7:2)
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG  (1:6)
                                       TO  SPA-GMN-H03-B-TSRYYM    (IDX)
               MOVE    HENREI-B-RECESYUBETU
                                       TO  SPA-NAI-H03-B-TRECESYUBETU
                                                                   (IDX)
               MOVE    HENREI-B-HKNJANUM
                                       TO  SPA-GMN-H03-B-THKNJANUM (IDX)
               MOVE    HENREI-B-KIGO   TO  SPA-GMN-H03-B-TKIGO     (IDX)
               MOVE    HENREI-B-RECEIPT-KBN
                                       TO  SPA-NAI-H03-B-TRECEIPT-KBN
                                                                   (IDX)
               MOVE    HENREI-B-DISASTER
                                       TO  SPA-NAI-H03-B-TDISASTER (IDX)
               MOVE    HENREI-B-HR-MSG TO  SPA-GMN-H03-B-TERR      (IDX)
      *
               MOVE    HENREI-B-REC        TO  MCPDATA-REC
               MOVE    "key5"              TO  WRK-MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
               IF      FLG-HENREI-B-CHK    =   ZERO
                   MOVE  CHK-HENREI-B-ERRCD    TO
                                           SPA-GMN-H03-B-TERRCD    (IDX)
               END-IF
      *
      *        患者ＩＤチェック
               IF      HENREI-B-PTID   >   ZERO
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
                   MOVE    HENREI-B-PTNUM  TO  SPTID-PTNUM
                   CALL    "ORCSPTID"      USING   ORCSPTIDAREA
                                                   SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    1               TO  SPA-NAI-H03-B-TERRFLG
                                                                   (IDX)
                   ELSE
                       IF      SPTID-PTID      NOT =   HENREI-B-PTID
                           MOVE    2           TO  SPA-NAI-H03-B-TERRFLG
                                                                   (IDX)
                       END-IF
                   END-IF
               END-IF
      *
               PERFORM 900-HENREI-BODY-READ-KEY4-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       4101-HENREI-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理情報一覧選択処理
      *****************************************************************
       450-KANRILST-SELECT-SEC       SECTION.
      *
           INITIALIZE                      SPA-GMN-H03-KOUMOKU
                                           SPA-NAI-H03-KOUMOKU
      *
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-KANRI-MAX
               IF      H03-KANRI-LIST-SELECT (IDX)
                                       =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-GMN-H03-KANRI-MAX
                                           TO  IDX
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNO           >   ZERO
               MOVE    WRK-SELNO           TO  SPA-GMN-H03-SELNUM
                                               SPA-NAI-H03-SELNUM
                                               H03-KANRI-LIST-ROW
      *        明細情報編集処理
               PERFORM 4101-HENREI-BODY-HENSYU-SEC
           END-IF
           .
       450-KANRILST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "H01"           TO  SPA-SAKIPG
           MOVE    "H03"           TO  SPA-MOTOPG
      *
           MOVE    "JOIN"          TO  MCP-PUTTYPE
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
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO   WRK-HENYMDG
           ELSE
               IF      WRK-SYMD            =   ALL "9"
                   MOVE    SPACE               TO  WRK-HGO
                   MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
                   MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
                   MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
                   MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               ELSE
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG3
               END-IF
           END-IF
      *
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
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H03"               TO  MCP-WINDOW
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
           IF      FLG-KANRI-ROW       =   1
               MOVE    ZERO                TO  WRK-H03-KANRI-ROW
           ELSE
               MOVE    H03-KANRI-LIST-ROW  TO  WRK-H03-KANRI-ROW
           END-IF
      *
           INITIALIZE                  H03
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-KANRI-MAX
               MOVE    SPA-GMN-H03-K-TSEQNUM     (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9            TO  H03-KANRI-TSEQNUM    (IDX)
               MOVE    SPA-GMN-H03-K-TSYORIYM    (IDX)
                                           TO  H03-KANRI-TSYORIYM  (IDX)
               MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI    (IDX)
                                           TO  H03-KANRI-TTEISYUTUSAKI
                                                                   (IDX)
               MOVE    SPA-GMN-H03-K-TTEISYUTUSAKI2   (IDX)
                                           TO  H03-KANRI-TTEISYUTUSAKI2
                                                                   (IDX)
               MOVE    SPA-GMN-H03-K-TCNT      (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H03-KANRI-TCNT      (IDX)
               MOVE    SPA-GMN-H03-K-TERRCD    (IDX)
                                           TO  H03-KANRI-TERRCD    (IDX)
               MOVE    SPA-GMN-H03-K-TCREYMD (IDX)
                                           TO  H03-KANRI-TCREYMD   (IDX)
               MOVE    SPA-GMN-H03-K-THOSPCD   (IDX)
                                           TO  H03-KANRI-THOSPCD   (IDX)
      *         
               IF      IDX                 =   SPA-GMN-H03-SELNUM
                   MOVE    "T"                 TO  H03-KANRI-LIST-SELECT
                                                                   (IDX)
               ELSE
                   MOVE    "F"                 TO  H03-KANRI-LIST-SELECT
                                                                   (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H03-KANRI-MAX
                                       TO  H03-KANRI-LIST-COUNT
           MOVE    ZERO                TO  H03-KANRI-LIST-ROW
           IF      WRK-H03-KANRI-ROW   >   ZERO
               MOVE    WRK-H03-KANRI-ROW   TO  H03-KANRI-LIST-ROW
           END-IF
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-GMN-H03-BODY-MAX
               MOVE    SPA-GMN-H03-B-TSEQNUM    (IDX)
                                           TO  WRK-ZZ9
               MOVE    WRK-ZZ9             TO  H03-BODY-TSEQNUM    (IDX)
               EVALUATE    SPA-NAI-H03-B-TNYUGAIKBN (IDX)
                   WHEN    "1"
                       MOVE    "入"        TO  H03-BODY-TNYUGAIKBN (IDX)
                   WHEN    "2"
                       MOVE    "外"        TO  H03-BODY-TNYUGAIKBN (IDX)
               END-EVALUATE
               MOVE    SPA-GMN-H03-B-TPTNUM     (IDX)
                                           TO  H03-BODY-TPTNUM     (IDX)
               MOVE    SPA-GMN-H03-B-TNAME      (IDX)
                                           TO  H03-BODY-TNAME      (IDX)
               MOVE    SPA-GMN-H03-B-TSRYYM     (IDX)
                                           TO  H03-BODY-TSRYYM     (IDX)
               EVALUATE    SPA-NAI-H03-B-TRECESYUBETU (IDX)
                   WHEN    "7101"
                   WHEN    "7091"
                       MOVE    "傷病・業務"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7102"
                   WHEN    "7092"
                       MOVE    "傷病・通勤"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7071"
                   WHEN    "7081"
                       MOVE    "短期・業務"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    "7072"
                   WHEN    "7082"
                       MOVE    "短期・通勤"    TO
                                           H03-BODY-TRECESYUBETU (IDX)
                   WHEN    OTHER
                       MOVE    SPA-NAI-H03-B-TRECESYUBETU (IDX)
                                                       TO
                                           H03-BODY-TRECESYUBETU (IDX)
               END-EVALUATE
               IF     SPA-GMN-H03-B-THKNJANUM  (IDX)
                                   NOT =   "00000000"
                   MOVE    SPA-GMN-H03-B-THKNJANUM  (IDX)
                                           TO  H03-BODY-THKNJANUM (IDX)
               END-IF
               MOVE    SPA-GMN-H03-B-TKIGO      (IDX)
                                           TO  H03-BODY-TKIGO     (IDX)
               EVALUATE    SPA-NAI-H03-B-TRECEIPT-KBN (IDX)(1:1)
                   WHEN    "1"
                       MOVE    "老併"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "2"
                       MOVE    "老健"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "3"
                       MOVE    "医併"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
                   WHEN    "4"
                       MOVE    "医療"          TO  H03-BODY-TRECEIPT-KBN
                                                                  (IDX)
               END-EVALUATE
               EVALUATE    SPA-NAI-H03-B-TDISASTER (IDX)
                   WHEN    "00"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "災"            TO  H03-BODY-TDISASTER
                                                                  (IDX)
               END-EVALUATE
               MOVE    SPA-GMN-H03-B-TERR      (IDX)
                                           TO  H03-BODY-TERR      (IDX)
               MOVE    SPA-GMN-H03-B-TERRCD    (IDX)
                                           TO  H03-BODY-TERRCD    (IDX)
      *
               IF      SPA-H01NAI-PTID     >   ZERO
                   IF      SPA-H01NAI-PTID     =
                                               SPA-NAI-H03-B-TPTID (IDX)
                       MOVE    "#FFCC99"       TO
                                           H03-BODY-LIST-BGCOLOR (IDX)
                   END-IF
               END-IF
      *
               IF      SPA-GMN-H03-B-TERRCD (IDX)
                                        NOT =   SPACE
                   MOVE    "#4169E1"       TO
                                           H03-BODY-LIST-FGCOLOR (IDX)
               ELSE
                   IF      SPA-NAI-H03-B-TERRFLG (IDX)
                                               >   0
                       MOVE    "#FF0000"       TO
                                           H03-BODY-LIST-FGCOLOR (IDX)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-H03-BODY-MAX
                                           TO  H03-BODY-LIST-COUNT
      *
           MOVE    SPA-GMN-H03-B-HYODAI    TO  H03-BODY-LIST-HYODAI 
      *
           MOVE    SPA-GMN-H03-SELNUM      TO  H03-SELNUM
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
           IF    ( SPA-ERRCD           =   SPACE )
           AND   ( SPA-GMN-H03-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-H03-CUR
               WHEN    1
                   MOVE  "SELNUM"          TO  MCP-WIDGET
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
       50011-INPUT-CUR-SEC             SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    1               TO  SPA-GMN-H03-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"        TO  HERR-ERRMSG
           END-EVALUATE
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H03"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
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
      *    オンライン返戻管理読み込み処理
      *****************************************************************
       900-HENREI-KANRI-READ-SEC     SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE   ZERO                 TO  FLG-HENREI-K
               MOVE   MCPDATA-REC          TO  HENREI-K-REC
           ELSE
               INITIALIZE                      HENREI-K-REC
               MOVE   1                    TO  FLG-HENREI-K
           END-IF
           .
       900-HENREI-KANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE  MCPDATA-REC           TO  CHK-HENREI-B-REC
                   MOVE  ZERO                  TO  FLG-HENREI-B-CHK
               ELSE
                   INITIALIZE                      CHK-HENREI-B-REC
                   MOVE  1                     TO  FLG-HENREI-B-CHK
               END-IF
           ELSE
               INITIALIZE                      CHK-HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B-CHK
           END-IF
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-KEY4-SEC   SECTION.
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-KEY4-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
               CALL    "coblog" USING "SELECT ERR table=" MCP-TABLE
                       " pathname="        MCP-PATHNAME
           END-IF
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
      *    ＰＵＴ処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
