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
       PROGRAM-ID.             ORCGN01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 保険者情報設定
      *  コンポーネント名  : 保険者情報
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.00.02    MCC-藤原     01/10/24  削除ができるように修正 
      *  01.00.03    NACL-藤原    02/09/05  保険番号のコラムリストを
      *                                     １００に変更
      *  02.00.00    NACL-多々納  03/07/07  異動情報追加
      *                                     一括入力へ変更
      *  02.01.01    NACL-森脇    04/07/14  給付割合を追加
      *  02.01.02    NACL-多々納  04/07/27  検証番号チェックの修正
      *  04.06.00    NACL-多々納  11/03/22  LD104 ,LD105,LD106,LD108,LD109　統合
      *  04.07.00    NACL-多々納  14/02/03  介護保険対応
      *  05.01.00    NACL-多々納  17/10/XX  ginbee対応（マスター分離）
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
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    システム管理情報設定用共通パラメタ
           COPY    "N01COMMON-SPA".
      *
      *    画面用ＳＰＡ
       01  SPA-N01.
           03  SPA-N01-AREA.
               05  SPA-GMN-PAGE        PIC 9(02).
               05  SPA-GMN-LINE        PIC 9(02).
               05  SPA-GMN-MAX         PIC 9(03).
               05  SPA-GMN-CUR         PIC 9(02).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-HKNJANUM       PIC X(08).
                   07  SPA-GMN-HKNNUMLST      OCCURS  100.
                       09  SPA-GMN-HKNNUML    PIC X(03).
                       09  SPA-GMN-F1L        PIC X(01).
                       09  SPA-GMN-HKNNUMNML  PIC X(20).
                   07  SPA-GMN-HKNIDX         PIC 9(03).
      *                 
                   07  SPA-GMN-KOUMOKU.
                       09  SPA-GMN-HKNJANAME      PIC X(200).
                       09  SPA-GMN-HKNJANAMETAN1  PIC X(100).
                       09  SPA-GMN-HKNJANAMETAN2  PIC X(40).
                       09  SPA-GMN-HKNJANAMETAN3  PIC X(40).
                       09  SPA-GMN-POST           PIC X(07).
                       09  SPA-GMN-ADRS           PIC X(100).
                       09  SPA-GMN-BANTI          PIC X(100).
                       09  SPA-GMN-TEL            PIC X(15).
                       09  SPA-GMN-KIGO           PIC X(80).
      *
                       09  SPA-GMN-HKNNUM-G.
                           11  SPA-GMN-HKNNUM     PIC X(03).
                           11  SPA-GMN-FL         PIC X(01).
                           11  SPA-GMN-HKNNUMNM   PIC X(20).
      *            異動内容
                   07  SPA-GMN-IDOKBN-G.
                       09  SPA-GMN-IDOKBN         PIC X(01).
                       09  SPA-GMN-F2L            PIC X(01).
                       09  SPA-GMN-IDOKBNMEI      PIC X(20).
                   07  SPA-GMN-IDOKBNLST       OCCURS  10.
                       09  SPA-GMN-IDOKBNL        PIC X(01).
                       09  SPA-GMN-F1L            PIC X(01).
                       09  SPA-GMN-IDOKBNMEIL     PIC X(20).
                   07  SPA-GMN-IDOMAX         PIC 9(03).
      *            異動年月日
                   07  SPA-GMN-IDOYMD         PIC X(10).
      *----(02.01.01)--ADD-START---
      *            本人外来給付割合
                   07  SPA-GMN-HGKYURATE-G.
                       09  SPA-GMN-HGKYURATE      PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-HGKYURATEMEI   PIC X(20).
                   07  SPA-GMN-HGKYURATELST   OCCURS  4.
                       09  SPA-GMN-HGKYURATEL     PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-HGKYURATEMEIL  PIC X(20).
                   07  SPA-GMN-HGKYURATEMAX       PIC 9(03).
      *            本人入院給付割合
                   07  SPA-GMN-HNKYURATE-G.
                       09  SPA-GMN-HNKYURATE      PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-HNKYURATEMEI   PIC X(20).
                   07  SPA-GMN-HNKYURATELST   OCCURS  4.
                       09  SPA-GMN-HNKYURATEL     PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-HNKYURATEMEIL  PIC X(20).
                   07  SPA-GMN-HNKYURATEMAX       PIC 9(03).
      *            家族外来給付割合
                   07  SPA-GMN-KGKYURATE-G.
                       09  SPA-GMN-KGKYURATE      PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-KGKYURATEMEI   PIC X(20).
                   07  SPA-GMN-KGKYURATELST   OCCURS  4.
                       09  SPA-GMN-KGKYURATEL     PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-KGKYURATEMEIL  PIC X(20).
                   07  SPA-GMN-KGKYURATEMAX       PIC 9(03).
      *            家族入院給付割合
                   07  SPA-GMN-KNKYURATE-G.
                       09  SPA-GMN-KNKYURATE      PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-KNKYURATEMEI   PIC X(20).
                   07  SPA-GMN-KNKYURATELST   OCCURS  4.
                       09  SPA-GMN-KNKYURATEL     PIC X(01).
                       09  FILLER                 PIC X(01).
                       09  SPA-GMN-KNKYURATEMEIL  PIC X(20).
                   07  SPA-GMN-KNKYURATEMAX       PIC 9(03).
      *----(02.01.01)--ADD-END-----
      *
               05  SPA-NAI-AREA.
                   07  SPA-NAI-UPDFLG     PIC 9(01).
                   07  SPA-HKNJANUM-LEN   PIC 9(01).
                   07  SPA-NAI-HKNNUML    OCCURS  100.
                       09  SPA-NAI-THKNKOHSBTKBN   PIC X(01).
                       09  SPA-NAI-TKENSNUMCHKKBN  PIC X(01).
      *            異動年月日
                   07  SPA-NAI-IDOYMD         PIC X(08).
      *
                   07  SPA-NAI-HKNKOHSBTKBN   PIC X(01).
                   07  SPA-NAI-KENSNUMCHKKBN  PIC X(01).
      *H29.10
      *            ユーザー
                   07  SPA-NAI-USER-FLG       PIC 9(01).
                   07  SPA-NAI-MAST-FLG       PIC 9(01).
      *            処理区分
                   07  SPA-NAI-SYORIKBN       PIC 9(01).
      *        前回コード
               05  SPA-MAE-HKNJANUM           PIC X(08).
      *
           COPY    "ENUM-VALUE".
      *
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01). 
           03  FLG-SPATMP          PIC 9(01). 
      *
           03  FLG-TOUROKU         PIC 9(01). 
           03  FLG-ERR             PIC 9(01). 
           03  FLG-OK              PIC 9(01). 
           03  FLG-CHK             PIC 9(01). 
      *
      *    添字領域
      *
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
      *
       01  WRK-AREA.
           03  WRK-FILEMEI         PIC X(20).
           03  WRK-PATH-TABLE      PIC X(64).
           03  WRK-PATH-PATHNAME   PIC X(64).
           03  WRK-HENFLG          PIC 9(01).
           03  WRK-NIDMSG          PIC X(80). 
           03  WRK-YMD             PIC X(09). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-HKNJANUM        PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-ATO-INPUT       PIC X(400).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
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
           COPY    "CPLNKSYS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  HKNJAINF-REC.
           COPY  "CPHKNJAINF.INC".
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    保険番号マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *   チェックデジット
           COPY    "CPORCCHKDGT.INC".
      *
      *    日付サブルーチン領域
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPDATA2.INC".
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
      *orca104
           COPY    "V01.INC".
           COPY    "V02.INC".
           COPY    "V03.INC".
           COPY    "VERR.INC".
           COPY    "VID1.INC".
      *orca105
           COPY    "N01.INC".
           COPY    "NERR.INC".
           COPY    "NID1.INC".
      *orca106
           COPY    "N11.INC".
           COPY    "N11ERR.INC".
           COPY    "N11ID1.INC".
      *orca108
           COPY    "N31.INC".
           COPY    "N31ERR.INC".
           COPY    "N31ID1.INC".
      *orca109
           COPY    "N41.INC".
           COPY    "N41ERR.INC".
           COPY    "N41ID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-WORK        TO  SPA-N01KYOUTU
           MOVE    SPA-FREE        TO  SPA-N01
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
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
           MOVE    SPA-N01         TO  SPA-FREE
           MOVE    SPA-N01KYOUTU   TO  SPA-WORK 
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "NERR"
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
      ***  MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "N01"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               IF    ( SPA-MOTOPG      =   "W01" )
                   PERFORM 800-SPA-INPUT-SEC
                   GO  TO  300-SCREEN-EXT
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "NID1"
                   PERFORM 330-NID1-SET-SEC
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
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-NID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-NIDCD
               WHEN    "1001"
      *        登録
                   IF      SPA-NID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
               WHEN    "1002"
      *        削除
                   IF      SPA-NID1-FLG    =   "OK"
                       PERFORM 4031-DEL-SEC
                   END-IF
               WHEN    "1010"
      *        ユーザ登録
                   IF      SPA-NID1-FLG    =   "OK"
                       PERFORM 490-KOUSHIN-SEC
                   END-IF
               WHEN    "1005"
      *        ユーザ複写
                   IF      SPA-NID1-FLG    =   "OK"
      *                新規とする
                       MOVE    1               TO  SPA-NAI-USER-FLG
                       MOVE    ZERO            TO  SPA-NAI-UPDFLG
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-NID1-FLG
           MOVE    SPACE           TO  SPA-NIDCD
           .
       330-NID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE   SPA-N01-AREA
                        SPA-N01KYOUTU
      *
      *    保険番号　全件編集（適用範囲分）
           INITIALIZE                  HKNNUM-REC
      *****MOVE    SPA-HOSPID          TO  HKN-HOSPID
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL       FLG-HKNNUM  =   1
                               OR      SPA-GMN-HKNIDX
                                           =   100
               IF      SPA-GMN-HKNIDX  >   ZERO
               AND     HKN-HKNNUM      = 
                                      SPA-GMN-HKNNUML (SPA-GMN-HKNIDX)
                   CONTINUE
               ELSE    
                   ADD     1               TO  SPA-GMN-HKNIDX
                   MOVE    HKN-HKNNUM      TO  SPA-GMN-HKNNUML
                                                      (SPA-GMN-HKNIDX)
                   IF      HKN-HKNNUM      =   "027"
                       MOVE    "老人保健"      TO  SPA-GMN-HKNNUMNML
                                                      (SPA-GMN-HKNIDX)
                   ELSE
                       MOVE    HKN-TANSEIDONAME
                                               TO  SPA-GMN-HKNNUMNML
                                                      (SPA-GMN-HKNIDX)
                   END-IF                                       
                   MOVE    HKN-HKNKOHSBTKBN    TO
                                               SPA-NAI-THKNKOHSBTKBN
                                                      (SPA-GMN-HKNIDX)
                   MOVE    HKN-KENSNUMCHKKBN    TO
                                               SPA-NAI-TKENSNUMCHKKBN
                                                      (SPA-GMN-HKNIDX)
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           END-PERFORM
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF     SPA-GMN-HKNIDX       <   100
               ADD    1                    TO  SPA-GMN-HKNIDX
           ELSE
               MOVE   100                  TO  SPA-GMN-HKNIDX
           END-IF
           MOVE   "C00"                TO  SPA-GMN-HKNNUML
                                                      (SPA-GMN-HKNIDX)
           MOVE    "介護保険"          TO  SPA-GMN-HKNNUMNML
                                                      (SPA-GMN-HKNIDX)
      *
      *    異動内容
           MOVE    "1:廃止"            TO  SPA-GMN-IDOKBNLST (1)
           MOVE    "2:支部廃止"        TO  SPA-GMN-IDOKBNLST (2)
           MOVE    "3:解散"            TO  SPA-GMN-IDOKBNLST (3)
           MOVE    "4:合併後消滅"      TO  SPA-GMN-IDOKBNLST (4)
           MOVE    4                   TO  SPA-GMN-IDOMAX
      *----(02.01.01)--ADD-START---
      *    給付割合
           MOVE    "0:７割"            TO  SPA-GMN-HGKYURATELST (1)
                                           SPA-GMN-HNKYURATELST (1)
                                           SPA-GMN-KGKYURATELST (1)
                                           SPA-GMN-KNKYURATELST (1)
           MOVE    "1:８割"            TO  SPA-GMN-HGKYURATELST (2)
                                           SPA-GMN-HNKYURATELST (2)
                                           SPA-GMN-KGKYURATELST (2)
                                           SPA-GMN-KNKYURATELST (2)
           MOVE    "2:９割"            TO  SPA-GMN-HGKYURATELST (3)
                                           SPA-GMN-HNKYURATELST (3)
                                           SPA-GMN-KGKYURATELST (3)
                                           SPA-GMN-KNKYURATELST (3)
           MOVE    "3:１０割"          TO  SPA-GMN-HGKYURATELST (4)
                                           SPA-GMN-HNKYURATELST (4)
                                           SPA-GMN-KGKYURATELST (4)
                                           SPA-GMN-KNKYURATELST (4)
           MOVE    4                   TO  SPA-GMN-HGKYURATEMAX
                                           SPA-GMN-HNKYURATEMAX
                                           SPA-GMN-KGKYURATEMAX
                                           SPA-GMN-KNKYURATEMAX
      *----(02.01.01)--ADD-END-----
      *
           MOVE    1                   TO  SPA-GMN-CUR
           . 
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM  402-CLEAR-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *!
      *        前回ＣＤ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM  404-MAECD-SYORI-SEC
      *        ユーザー複写
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  410-USER-SYORI-SEC
      *!
      *        付加設定（システム管理）
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-W65-SENI-SEC
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "HKNJANUM"
      *            保険者番号入力
                   PERFORM 4100-HKNJANUM-CHK-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      410-INPUT-CHK-EXT
               END-IF
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号チェック
      *****************************************************************
       4100-HKNJANUM-CHK-SEC       SECTION.
      *
           MOVE    1                TO  SPA-GMN-CUR
      *
           MOVE    N01-HKNJANUM     TO  SPA-GMN-HKNJANUM
      *
           MOVE    ZERO             TO  SPA-HKNJANUM-LEN
           PERFORM    VARYING   IDX   FROM    1   BY  1
                      UNTIL    (IDX      >    8  )
                      OR       (SPA-GMN-HKNJANUM(IDX:1)  =   SPACE)
               ADD     1                 TO  SPA-HKNJANUM-LEN
           END-PERFORM
      *H29.10
           MOVE    ZERO                TO  SPA-NAI-USER-FLG
           MOVE    ZERO                TO  SPA-NAI-SYORIKBN
           MOVE    ZERO                TO  SPA-NAI-MAST-FLG
      *    ユーザー登録チェック
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
      *
           MOVE    "tbl_hknjainf_user" TO  WRK-PATH-TABLE
           PERFORM 900-HKNJAINF-INV-SEC
           IF      FLG-HKNJAINF        =   ZERO
      *        ユーザー登録あり
               MOVE    1                   TO  SPA-NAI-USER-FLG
               MOVE    ZERO                TO  SPA-NAI-MAST-FLG
               MOVE    ZERO                TO  SPA-NAI-SYORIKBN
               PERFORM 41001-HKNJA-HEN-SEC
           END-IF
      *    マスター登録チェック
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           PERFORM 900-HKNJAINF-MAST-INV-SEC
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    1                   TO  SPA-NAI-MAST-FLG
      *        ユーザーなし
               IF      SPA-NAI-USER-FLG    =   ZERO
                   MOVE    ZERO                TO  SPA-NAI-USER-FLG
                   MOVE    ZERO                TO  SPA-NAI-SYORIKBN
                   PERFORM 41001-HKNJA-HEN-SEC
               END-IF
           ELSE
      *        ユーザーなし
               IF      SPA-NAI-USER-FLG    =   ZERO
      *            新規追加
                   MOVE    1                   TO  SPA-NAI-USER-FLG
                   MOVE    1                   TO  SPA-NAI-SYORIKBN
                   MOVE    ZERO                TO  SPA-NAI-MAST-FLG
                   PERFORM 41002-HKNJA-ADD-SEC
               END-IF
           END-IF
      *    存在チェック
      *    MOVE    SPACE               TO  HKNJAINF-REC
      *    INITIALIZE                      HKNJAINF-REC
      *    MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
      *    MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
      *    MOVE    HKNJAINF-REC        TO  MCPDATA-REC
      *    PERFORM 900-HKNJAINF-INV-SEC
      *    IF      FLG-HKNJAINF        =   ZERO
      *        PERFORM 41001-HKNJA-HEN-SEC
      *    ELSE
      *        PERFORM 41002-HKNJA-ADD-SEC
      **** END-IF
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41003-HKNJANUM-INIT-SEC
      *        前回ＣＤへ保存
               MOVE    SPA-GMN-HKNJANUM    TO  SPA-MAE-HKNJANUM
      *
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       4100-HKNJANUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号更新編集処理
      *****************************************************************
       41001-HKNJA-HEN-SEC       SECTION.
      *
           MOVE    1                   TO  SPA-NAI-UPDFLG 
           MOVE    HKNJA-HKNJANAME     TO  SPA-GMN-HKNJANAME
           MOVE    HKNJA-HKNJANAME-TAN1
                                           TO  SPA-GMN-HKNJANAMETAN1
           MOVE    HKNJA-HKNJANAME-TAN2
                                           TO  SPA-GMN-HKNJANAMETAN2
           MOVE    HKNJA-HKNJANAME-TAN3
                                           TO  SPA-GMN-HKNJANAMETAN3
           MOVE    HKNJA-HKNNUM        TO  SPA-GMN-HKNNUM
           MOVE    HKNJA-POST          TO  SPA-GMN-POST
           MOVE    HKNJA-ADRS          TO  SPA-GMN-ADRS
           MOVE    HKNJA-BANTI         TO  SPA-GMN-BANTI
           MOVE    HKNJA-TEL           TO  SPA-GMN-TEL
           MOVE    HKNJA-KIGO          TO  SPA-GMN-KIGO
           MOVE    HKNJA-IDOKBN        TO  SPA-GMN-IDOKBN
      *
           MOVE    HKNJA-IDOYMD        TO  SPA-NAI-IDOYMD
           MOVE    SPA-NAI-IDOYMD      TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-IDOYMD
      *----(02.01.01)--ADD-START---
           IF        ( HKNJA-HKNNUM    =   "060" )
               AND   ( SPA-HKNJANUM-LEN    =   6 )
               AND   ( SPA-GMN-HKNJANUM(3:1)   =   "3" )
      *        本人外来給付割合
               EVALUATE    HKNJA-HON-GAIKYURATE
                   WHEN    080
                       MOVE    "1"         TO  SPA-GMN-HGKYURATE
                   WHEN    090
                       MOVE    "2"         TO  SPA-GMN-HGKYURATE
                   WHEN    100
                       MOVE    "3"         TO  SPA-GMN-HGKYURATE
                   WHEN    OTHER
                       MOVE    "0"         TO  SPA-GMN-HGKYURATE
               END-EVALUATE
      *        本人入院給付割合
               EVALUATE    HKNJA-HON-NYUKYURATE
                   WHEN    080
                       MOVE    "1"         TO  SPA-GMN-HNKYURATE
                   WHEN    090
                       MOVE    "2"         TO  SPA-GMN-HNKYURATE
                   WHEN    100
                       MOVE    "3"         TO  SPA-GMN-HNKYURATE
                   WHEN    OTHER
                       MOVE    "0"         TO  SPA-GMN-HNKYURATE
               END-EVALUATE
      *        家族外来給付割合
               EVALUATE    HKNJA-KZK-GAIKYURATE
                   WHEN    080
                       MOVE    "1"         TO  SPA-GMN-KGKYURATE
                   WHEN    090
                       MOVE    "2"         TO  SPA-GMN-KGKYURATE
                   WHEN    100
                       MOVE    "3"         TO  SPA-GMN-KGKYURATE
                   WHEN    OTHER
                       MOVE    "0"         TO  SPA-GMN-KGKYURATE
               END-EVALUATE
      *        家族入院給付割合
               EVALUATE    HKNJA-KZK-NYUKYURATE
                   WHEN    080
                       MOVE    "1"         TO  SPA-GMN-KNKYURATE
                   WHEN    090
                       MOVE    "2"         TO  SPA-GMN-KNKYURATE
                   WHEN    100
                       MOVE    "3"         TO  SPA-GMN-KNKYURATE
                   WHEN    OTHER
                       MOVE    "0"         TO  SPA-GMN-KNKYURATE
               END-EVALUATE
           END-IF
      *----(02.01.019--ADD-END-----
           .
      *
       41001-HKNJA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号新規チェック処理
      *****************************************************************
       41002-HKNJA-ADD-SEC       SECTION.
      *
           IF      SPA-HKNJANUM-LEN =   4   OR  6  OR  8
               CONTINUE
           ELSE         
               MOVE    "0001"          TO  SPA-ERRCD
               GO  TO               41002-HKNJA-ADD-EXT
           END-IF
      *
           IF  SPA-GMN-HKNJANUM (1:SPA-HKNJANUM-LEN)
                                      IS  NOT NUMERIC
               MOVE    "0001"         TO  SPA-ERRCD
               GO  TO                 41002-HKNJA-ADD-EXT
           END-IF
      *
      *    入力された保険者番号をセットすれば、サブプロ内で
      *    検証してくれる (処理区分:2)
           IF      SPA-HKNJANUM-LEN =   4
               CONTINUE
           ELSE
               INITIALIZE               LNK-CHKDGT-AREA
               MOVE    2                TO  LNK-I-SYORIKBN
               MOVE    SPA-GMN-HKNJANUM TO  LNK-I-CODE
               CALL    "ORCSCHKDGT"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD      NOT =   ZERO
                   MOVE    "0002"       TO  SPA-ERRCD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-UPDFLG
           MOVE    SPACE               TO  SPA-GMN-KOUMOKU
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKNJANUM-LEN
                   WHEN    4
                       MOVE    "001"           TO  SPA-GMN-HKNNUM
                   WHEN    6
                       MOVE    "060"           TO  SPA-GMN-HKNNUM
                       IF      SPA-GMN-HKNJANUM(3:1)   =   "3"
                           MOVE    "0:７割"    TO  SPA-GMN-HGKYURATE-G
                                                   SPA-GMN-HNKYURATE-G
                                                   SPA-GMN-KGKYURATE-G
                                                   SPA-GMN-KNKYURATE-G
                       END-IF
                   WHEN    8
                       IF      SPA-GMN-HKNJANUM (1:2)
                                               =   "27"
                           MOVE    "027"           TO  SPA-GMN-HKNNUM
                           MOVE    "老人保健"      TO  SPA-GMN-HKNNUMNM
                           MOVE    2               TO  SPA-GMN-CUR
      ******************** GO  TO  4100-HKNJANUM-CHK-EXT
                       ELSE
                          INITIALIZE                  HKNNUM-REC
      ********************MOVE    SPA-HOSPID          TO  HKN-HOSPID
                          MOVE    SPA-GMN-HKNJANUM(1:2)
                                                      TO  HKN-HBTNUM
                          MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
                          MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
                          MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                          MOVE    HKNNUM-REC          TO  MCPDATA-REC
                          MOVE    "tbl_hknnum"     TO  WRK-PATH-TABLE
                          MOVE    "key4"           TO  WRK-PATH-PATHNAME
                          PERFORM 900-HKNNUM-INV-SEC
                          IF      FLG-HKNNUM          =   ZERO
                              MOVE    HKN-HKNNUM      TO  SPA-GMN-HKNNUM
                          END-IF      
                       END-IF       
               END-EVALUATE
           END-IF
           .
       41002-HKNJA-ADD-EXT.
           EXIT.
      *****************************************************************
      *    保険者番号共通編集処理
      *****************************************************************
       41003-HKNJANUM-INIT-SEC       SECTION.
      *
      *    保険番号
           IF      SPA-GMN-HKNNUM      NOT =   SPACE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-HKNIDX
                   IF      SPA-GMN-HKNNUM      =   SPA-GMN-HKNNUML (IDX)
                       MOVE    SPA-GMN-HKNNUMLST (IDX)
                                               TO  SPA-GMN-HKNNUM-G
                       MOVE    SPA-NAI-THKNKOHSBTKBN(IDX)
                                               TO  SPA-NAI-HKNKOHSBTKBN
                       MOVE    SPA-NAI-TKENSNUMCHKKBN(IDX)
                                               TO  SPA-NAI-KENSNUMCHKKBN
                       MOVE    SPA-GMN-HKNIDX  TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *    異動内容
           IF      SPA-GMN-IDOKBN    =   SPACE
               MOVE    SPACE           TO  SPA-GMN-IDOKBN-G
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-IDOMAX
                   IF      SPA-GMN-IDOKBN    =   SPA-GMN-IDOKBNL
                                                               (IDX)
                       MOVE    SPA-GMN-IDOKBNLST (IDX)
                                               TO  SPA-GMN-IDOKBN-G
                       MOVE    SPA-GMN-IDOMAX  TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *----(02.01.01)--ADD-START---
      *    本人外来給付割合
           IF      SPA-GMN-HGKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HGKYURATE-G
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-HGKYURATEMAX
                   IF      SPA-GMN-HGKYURATE   =   SPA-GMN-HGKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-HGKYURATELST (IDX)
                                               TO  SPA-GMN-HGKYURATE-G
                       MOVE    SPA-GMN-HGKYURATEMAX   TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *
      *    本人入院給付割合
           IF      SPA-GMN-HNKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HNKYURATE-G
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-HNKYURATEMAX
                   IF      SPA-GMN-HNKYURATE   =   SPA-GMN-HNKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-HNKYURATELST (IDX)
                                               TO  SPA-GMN-HNKYURATE-G
                       MOVE    SPA-GMN-HNKYURATEMAX   TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *
      *    家族外来給付割合
           IF      SPA-GMN-KGKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-KGKYURATE-G
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-KGKYURATEMAX
                   IF      SPA-GMN-KGKYURATE   =   SPA-GMN-KGKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-KGKYURATELST (IDX)
                                               TO  SPA-GMN-KGKYURATE-G
                       MOVE    SPA-GMN-KGKYURATEMAX   TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *
      *    家族入院給付割合
           IF      SPA-GMN-KNKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-KNKYURATE-G
           ELSE
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-KNKYURATEMAX
                   IF      SPA-GMN-KNKYURATE   =   SPA-GMN-KNKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-KNKYURATELST (IDX)
                                               TO  SPA-GMN-KNKYURATE-G
                       MOVE    SPA-GMN-KNKYURATEMAX   TO  IDX
                   END-IF
               END-PERFORM
           END-IF       
      *----(02.01.01)--ADD-END-----
      *
           .
      *
       41003-HKNJANUM-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    N01-HKNJANAME       TO  SPA-GMN-HKNJANAME
           MOVE    N01-HKNJANAMETAN1   TO  SPA-GMN-HKNJANAMETAN1
           MOVE    N01-HKNJANAMETAN2   TO  SPA-GMN-HKNJANAMETAN2
           MOVE    N01-HKNJANAMETAN3   TO  SPA-GMN-HKNJANAMETAN3
           MOVE    N01-HKNNUM          TO  SPA-GMN-HKNNUM-G
           MOVE    N01-POST            TO  SPA-GMN-POST
           MOVE    N01-ADRS            TO  SPA-GMN-ADRS
           MOVE    N01-BANTI           TO  SPA-GMN-BANTI
           MOVE    N01-TEL             TO  SPA-GMN-TEL
           MOVE    N01-KIGO            TO  SPA-GMN-KIGO
      *
           MOVE    N01-IDOKBN          TO  SPA-GMN-IDOKBN-G
           MOVE    N01-IDOYMD          TO  SPA-GMN-IDOYMD
      *
      *----(02.01.01)--ADD-START---
           MOVE    N01-HGKYURATE       TO  SPA-GMN-HGKYURATE-G
           MOVE    N01-HNKYURATE       TO  SPA-GMN-HNKYURATE-G
           MOVE    N01-KGKYURATE       TO  SPA-GMN-KGKYURATE-G
           MOVE    N01-KNKYURATE       TO  SPA-GMN-KNKYURATE-G
      *----(02.01.01)--ADD-END-----
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    保険者名称チェック
           PERFORM 4101-HKNJANAME-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険者名称（短縮１）チェック
           PERFORM 4102-HKNJANAMETAN1-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険者名称（短縮２）チェック
           PERFORM  4103-HKNJANAMETAN2-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険者名称（短縮３）チェック
           PERFORM 4104-HKNJANAMETAN3-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    保険番号チェック
           PERFORM 4105-HKNNUM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    郵便番号チェック
           PERFORM 4106-POST-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    住所チェック
           PERFORM 4107-ADRS-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    番地・方書チェック
           PERFORM 4108-BANTI-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    記号
           PERFORM 4110-KIGO-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    異動内容
           PERFORM 4111-IDOKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    異動年月日
           PERFORM 4111-IDOYMD-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *----(02.01.01)--ADD-START---
      *    本人外来給付割合
           PERFORM 4112-HGKYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    本人入院給付割合
           PERFORM 4113-HNKYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    家族外来給付割合
           PERFORM 4114-KGKYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *    家族入院給付割合
           PERFORM 4115-KNKYURATE-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *----(02.01.01)--ADD-END-----
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者名称チェック
      *****************************************************************
       4101-HKNJANAME-CHK-SEC       SECTION.
      *
           MOVE    SPA-GMN-HKNJANAME   TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-HKNJANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4101-HKNJANAME-CHK-EXT
           END-IF
      *
           IF     (WRK-MCP-WIDGET      =   "HKNJANAME" )  AND
                  (SPA-GMN-HKNJANAMETAN1
                                       =   SPACE       )
               MOVE    SPA-GMN-HKNJANAME   TO  SPA-GMN-HKNJANAMETAN1
           END-IF                                      
      *
           .
       4101-HKNJANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者名称短縮１チェック
      *****************************************************************
       4102-HKNJANAMETAN1-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-HKNJANAMETAN1   TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-HKNJANAMETAN1
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4102-HKNJANAMETAN1-CHK-EXT
           END-IF
      *
           IF     (WRK-MCP-WIDGET      =   "HKNJANAMETAN1" )  AND
                  (SPA-GMN-HKNJANAMETAN2
                                       =   SPACE       )
               MOVE    SPA-GMN-HKNJANAMETAN1
                                       TO  SPA-GMN-HKNJANAMETAN2
           END-IF 
      *                           
           .
       4102-HKNJANAMETAN1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者名称短縮２チェック
      *****************************************************************
       4103-HKNJANAMETAN2-CHK-SEC     SECTION.
      *
           MOVE    SPA-GMN-HKNJANAMETAN2   TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-HKNJANAMETAN2
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4103-HKNJANAMETAN2-CHK-EXT
           END-IF
      *
           IF     (WRK-MCP-WIDGET      =   "HKNJANAMETAN2" )  AND
                  (SPA-GMN-HKNJANAMETAN3
                                       =   SPACE       )
               MOVE    SPA-GMN-HKNJANAMETAN2
                                       TO  SPA-GMN-HKNJANAMETAN3
           END-IF 
      *
           .
       4103-HKNJANAMETAN2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者名称短縮３チェック
      *****************************************************************
       4104-HKNJANAMETAN3-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-HKNJANAMETAN3   TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-HKNJANAMETAN3
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
               GO      TO      4104-HKNJANAMETAN3-CHK-EXT
           END-IF
      *
           .
       4104-HKNJANAMETAN3-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号チェック
      *****************************************************************
       4105-HKNNUM-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-HKNNUM      =   SPACE
               MOVE    SPACE               TO  SPA-NAI-HKNKOHSBTKBN
               MOVE    SPACE               TO  SPA-NAI-KENSNUMCHKKBN
           ELSE
               MOVE    ZERO               TO  FLG-OK
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL  (IDY >      SPA-GMN-HKNIDX )
                       OR     (FLG-OK     =   1  )
                   IF      SPA-GMN-HKNNUM =   SPA-GMN-HKNNUML (IDY)
                       MOVE    SPA-GMN-HKNNUMLST (IDY)
                                              TO  SPA-GMN-HKNNUM-G
                       MOVE    1              TO  FLG-OK
                       MOVE    SPA-NAI-THKNKOHSBTKBN(IDY)
                                               TO  SPA-NAI-HKNKOHSBTKBN
                       MOVE    SPA-NAI-TKENSNUMCHKKBN(IDY)
                                               TO  SPA-NAI-KENSNUMCHKKBN
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
                   GO  TO  4105-HKNNUM-CHK-EXT
               END-IF
      *
               EVALUATE    SPA-GMN-HKNNUM
                   WHEN    "001"
                       IF      SPA-HKNJANUM-LEN NOT =   4
                           MOVE    "0005"         TO  SPA-ERRCD
                       END-IF                       
                   WHEN    "060"
                       IF      SPA-HKNJANUM-LEN NOT =   6
                           MOVE    "0005"         TO  SPA-ERRCD
                       END-IF
      *----(02.01.01)--ADD-START---
                       IF        ( SPA-HKNJANUM-LEN   =   6 )
                           AND   ( SPA-GMN-HKNJANUM(3:1)  =   "3" )
                           IF      SPA-GMN-HGKYURATE  =   SPACE
                               MOVE    "0"     TO  SPA-GMN-HGKYURATE
                           END-IF
                           IF      SPA-GMN-HNKYURATE  =   SPACE
                               MOVE    "0"     TO  SPA-GMN-HNKYURATE
                           END-IF
                           IF      SPA-GMN-KGKYURATE  =   SPACE
                               MOVE    "0"     TO  SPA-GMN-KGKYURATE
                           END-IF
                           IF      SPA-GMN-KNKYURATE  =   SPACE
                               MOVE    "0"     TO  SPA-GMN-KNKYURATE
                           END-IF
                       END-IF
      *----(02.01.01)--ADD-END-----
      *!!!
                   WHEN    "C00"
                       IF      SPA-HKNJANUM-LEN NOT =   6
                           MOVE    "0005"         TO  SPA-ERRCD
                       END-IF
      *!!!
                   WHEN    OTHER
                       IF      SPA-HKNJANUM-LEN NOT =   8
                           MOVE    "0005"         TO  SPA-ERRCD
                       ELSE
                          INITIALIZE                  HKNNUM-REC
      ********************MOVE    SPA-HOSPID          TO  HKN-HOSPID
                          MOVE    SPA-GMN-HKNNUM      TO  HKN-HKNNUM
                          MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
                          MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
                          MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                          MOVE    HKNNUM-REC          TO  MCPDATA-REC
                          MOVE    "tbl_hknnum"    TO  WRK-PATH-TABLE
                          MOVE    "key5"          TO  WRK-PATH-PATHNAME
                          PERFORM 900-HKNNUM-INV-SEC
                          IF      FLG-HKNNUM          =   ZERO
      *                        法別番号チェックありの時、チェック
                               IF      (HKN-HBTNUMCHKKBN   =  "1" )  AND
                                       (HKN-HBTNUM     NOT =
                                              SPA-GMN-HKNJANUM (1:2))
                                  MOVE    "0005"         TO  SPA-ERRCD
                              END-IF
                           ELSE 
                               MOVE    "0005"         TO  SPA-ERRCD
                           END-IF      
                       END-IF 
               END-EVALUATE
           END-IF        
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6           TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-HKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    郵便番号チェック
      *****************************************************************
       4106-POST-CHK-SEC        SECTION.
      *
           IF     (WRK-MCP-WIDGET      =   "POST" )  AND
                  (SPA-GMN-POST    NOT =   SPACE  )
               MOVE    SPACE               TO  ADRS-REC
               INITIALIZE                  ADRS-REC
               MOVE    SPA-GMN-POST        TO  ADRS-POST
               MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
               MOVE    ADRS-REC            TO  MCPDATA-REC
               PERFORM 900-ADRS-INV-SEC
               IF      FLG-ADRS            =   ZERO
                   MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN-ADRS
      ******** ELSE
      ********     MOVE    SPACE               TO  SPA-GMN-ADRS
               END-IF
           END-IF
      *
           .
       4106-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック
      *****************************************************************
       4107-ADRS-CHK-SEC            SECTION.
      *
           MOVE    SPA-GMN-ADRS        TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ADRS
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4107-ADRS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    番地方書チェック
      *****************************************************************
       4108-BANTI-CHK-SEC        SECTION.
      *
           MOVE    SPA-GMN-BANTI      TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-BANTI
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    9                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4108-BANTI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    記号チェック
      *****************************************************************
       4110-KIGO-CHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-KIGO        TO  WRK-MAE-INPUT
           PERFORM 800-KANACHK-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KIGO
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4110-KIGO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    異動内容チェック
      *****************************************************************
       4111-IDOKBN-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-IDOKBN    =   SPACE
               MOVE    SPACE           TO  SPA-GMN-IDOKBN-G
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-IDOMAX
                   IF      SPA-GMN-IDOKBN    =   SPA-GMN-IDOKBNL
                                                               (IDX)
                       MOVE    SPA-GMN-IDOKBNLST (IDX)
                                               TO  SPA-GMN-IDOKBN-G
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-IDOMAX  TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4111-IDOKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    異動年月日チェック
      *****************************************************************
       4111-IDOYMD-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-IDOYMD      =   SPACE
               MOVE    SPACE               TO  SPA-NAI-IDOYMD
           ELSE
      *
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-IDOYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-IDOYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-IDOYMD
               ELSE
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    13                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4111-IDOYMD-CHK-EXT.
           EXIT.
      *
      *----(02.01.01)--ADD-START---
      *****************************************************************
      *    本人外来給付割合チェック
      *****************************************************************
       4112-HGKYURATE-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-HGKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HGKYURATE-G
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-HGKYURATEMAX
                   IF      SPA-GMN-HGKYURATE   =   SPA-GMN-HGKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-HGKYURATELST (IDX)
                                               TO  SPA-GMN-HGKYURATE-G
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-HGKYURATEMAX    TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    14              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4112-HGKYURATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人入院給付割合チェック
      *****************************************************************
       4113-HNKYURATE-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-HNKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HNKYURATE-G
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-HNKYURATEMAX
                   IF      SPA-GMN-HNKYURATE   =   SPA-GMN-HNKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-HNKYURATELST (IDX)
                                               TO  SPA-GMN-HNKYURATE-G
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-HNKYURATEMAX  TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    15              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4113-HNKYURATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    家族外来給付割合チェック
      *****************************************************************
       4114-KGKYURATE-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KGKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-KGKYURATE-G
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-KGKYURATEMAX
                   IF      SPA-GMN-KGKYURATE   =   SPA-GMN-KGKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-KGKYURATELST (IDX)
                                               TO  SPA-GMN-KGKYURATE-G
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-KGKYURATEMAX    TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    16              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4114-KGKYURATE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    家族入院給付割合チェック
      *****************************************************************
       4115-KNKYURATE-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KNKYURATE   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-KNKYURATE-G
           ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       SPA-GMN-KNKYURATEMAX
                   IF      SPA-GMN-KNKYURATE   =   SPA-GMN-KNKYURATEL
                                                               (IDX)
                       MOVE    SPA-GMN-KNKYURATELST (IDX)
                                               TO  SPA-GMN-KNKYURATE-G
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-GMN-KNKYURATEMAX  TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0009"          TO  SPA-ERRCD
                   MOVE    17              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4115-KNKYURATE-CHK-EXT.
           EXIT.
      *
      *----(02.01.01)--ADD-END-----
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-HKNJANUM    =   SPACE
               MOVE    "0008"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-GMN-HKNNUM      =   SPACE
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    6               TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-UPDFLG      =   ZERO
               PERFORM 41091-HKNJANUM-CHK-SEC
           END-IF
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    必須入力チェック処理
      *****************************************************************
       41091-HKNJANUM-CHK-SEC             SECTION.
      *
           IF      SPA-HKNJANUM-LEN    =   4   OR  6  OR  8
               CONTINUE
           ELSE
               MOVE    "0001"          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-HKNJANUM (1:SPA-HKNJANUM-LEN)
                                      IS  NOT NUMERIC      )
               MOVE    "0001"         TO  SPA-ERRCD
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-HKNKOHSBTKBN    =  "1"  OR  "4"
                                               OR  "8" OR  "9"
                                               OR  SPACE
      *            保険
                   IF      SPA-HKNJANUM-LEN    =   4
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-CHK
                   END-IF
               ELSE
      *            公費
                   IF      SPA-NAI-KENSNUMCHKKBN   =   "1"
                      MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
           END-IF
      *    入力された保険者番号をセットすれば、サブプロ内で
      *    検証してくれる (処理区分:2)
           IF      FLG-CHK             =   1
               INITIALIZE               LNK-CHKDGT-AREA
               MOVE    2                TO  LNK-I-SYORIKBN
               MOVE    SPA-GMN-HKNJANUM TO  LNK-I-CODE
               CALL    "ORCSCHKDGT"       USING   LNK-CHKDGT-AREA
               IF      LNK-O-ERRCD      NOT =   ZERO
                   MOVE    "0002"       TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       800-KANACHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
           .
      *
       800-KANACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "M02"               TO  SPA-SAKIPG
           MOVE    "N01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
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
      *    削除確認画面表示処理
      *****************************************************************
       402-CLEAR-SEC                               SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-KOUMOKU
                                           SPA-GMN-HKNJANUM
           INITIALIZE                      SPA-GMN-IDOKBN-G
           INITIALIZE                      SPA-GMN-IDOYMD
           INITIALIZE                      SPA-NAI-IDOYMD
      *
           MOVE    ZERO                TO  SPA-NAI-UPDFLG
      *H29.10
           MOVE    ZERO                TO  SPA-NAI-USER-FLG
           MOVE    ZERO                TO  SPA-NAI-MAST-FLG
           MOVE    ZERO                TO  SPA-NAI-SYORIKBN
      *          
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .    
       402-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-NAI-USER-FLG    =   ZERO
               GO      TO      403-DEL-EXT
           END-IF
      *
           IF      SPA-NAI-UPDFLG      =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
           ELSE    
               MOVE    "1002"              TO  SPA-NIDCD
           END-IF    
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                              SECTION.
      *
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
     ****  MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           IF      MCP-RC              NOT =   ZERO 
               MOVE    "1002"                  TO  SPA-ERRCD
               GO  TO  4031-DEL-EXT
           END-IF
      *
      *    前回ＣＤへ保存
           MOVE    SPA-GMN-HKNJANUM    TO  WRK-HKNJANUM
      *
           PERFORM 310-SPASET-SEC
      *    前回ＣＤへ保存
           MOVE    WRK-HKNJANUM        TO  SPA-MAE-HKNJANUM
           .
       4031-DEL-EXT.
           EXIT.
      *
      *H29.10
      *****************************************************************
      *       前回ＣＤ処理
      *****************************************************************
       404-MAECD-SYORI-SEC              SECTION.
      *
           PERFORM 402-CLEAR-SEC
      *
           IF      SPA-MAE-HKNJANUM    NOT =   SPACE
               MOVE    SPA-MAE-HKNJANUM        TO  N01-HKNJANUM
               PERFORM 4100-HKNJANUM-CHK-SEC
           END-IF
           .
       404-MAECD-SYORI-EXT.
           EXIT.
      *H29.10
      *****************************************************************
      *     ユーザー複写処理
      *****************************************************************
       410-USER-SYORI-SEC              SECTION.
      *
           IF     (SPA-GMN-HKNJANUM    NOT =   SPACE )
              AND (SPA-NAI-UPDFLG          =   1     )
              AND (SPA-NAI-USER-FLG        =   ZERO  )
               MOVE    "1005"              TO  SPA-NIDCD
     **        MOVE    1                   TO  SPA-NAI-USER-FLG
      *        新規とする
      **       MOVE    ZERO                TO  SPA-NAI-UPDFLG
           END-IF
      *
           .
       410-USER-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *       登録前処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
           IF      SPA-NAI-USER-FLG    =   ZERO
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           MOVE    "1001"          TO  SPA-NIDCD
      *H29.10
      *    ユーザー登録へ複写追加
           IF     (SPA-NAI-USER-FLG    =   1    )
             AND  (SPA-NAI-SYORIKBN    =   ZERO )
             AND  (SPA-NAI-UPDFLG      =   ZERO )
               MOVE    "1010"          TO  SPA-NIDCD
               GO      TO   490-TOUROKU-EXT
           END-IF
      *
      *----(02.01.01)--ADD-START---
      *    本人給付割合チェック
           EVALUATE    SPA-GMN-HGKYURATE    ALSO     SPA-GMN-HNKYURATE
             WHEN      "3"     ALSO    "3"
             WHEN      "2"     ALSO    "2"
             WHEN      "1"     ALSO    "1"
             WHEN      "0"     ALSO    "0"
             WHEN      "0"     ALSO    "1"
             WHEN      "1"     ALSO    "2"
             WHEN      "2"     ALSO    "3"
             WHEN      SPACE   ALSO    SPACE
               CONTINUE
             WHEN      OTHER
               MOVE    "1011"          TO  SPA-NIDCD
           END-EVALUATE
      *
      *    家族給付割合チェック
           EVALUATE    SPA-GMN-KGKYURATE    ALSO     SPA-GMN-KNKYURATE
             WHEN      "3"     ALSO    "3"
             WHEN      "2"     ALSO    "2"
             WHEN      "1"     ALSO    "1"
             WHEN      "0"     ALSO    "0"
             WHEN      "0"     ALSO    "1"
             WHEN      "1"     ALSO    "2"
             WHEN      "2"     ALSO    "3"
             WHEN      SPACE   ALSO    SPACE
               CONTINUE
             WHEN      OTHER
               MOVE    "1011"          TO  SPA-NIDCD
           END-EVALUATE
      *----(02.01.01)--ADD-END-----
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *       登録処理
      *****************************************************************
       490-KOUSHIN-SEC           SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           MOVE    SPACE               TO  HKNJAINF-REC
           INITIALIZE                      HKNJAINF-REC
      *****MOVE    SPA-HOSPID          TO  HKNJA-HOSPID
           MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf_user" TO  WRK-PATH-TABLE
           PERFORM 900-HKNJAINF-INV-SEC
      *
           IF      FLG-HKNJAINF        =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  HKNJAINF-REC
               INITIALIZE                      HKNJAINF-REC
      *********MOVE    SPA-HOSPID          TO  HKNJA-HOSPID
               MOVE    SPA-GMN-HKNJANUM    TO  HKNJA-HKNJANUM
           END-IF
      *
           MOVE    SPA-GMN-HKNJANAME   TO  HKNJA-HKNJANAME 
           MOVE    SPA-GMN-HKNJANAMETAN1
                                       TO  HKNJA-HKNJANAME-TAN1 
           MOVE    SPA-GMN-HKNJANAMETAN2
                                       TO  HKNJA-HKNJANAME-TAN2 
           MOVE    SPA-GMN-HKNJANAMETAN3
                                       TO  HKNJA-HKNJANAME-TAN3 
           MOVE    SPA-GMN-HKNNUM      TO  HKNJA-HKNNUM
      *----(02.01.01)--ADD-START---
      *    本人外来給付割合
           EVALUATE    SPA-GMN-HGKYURATE
           WHEN    "1"
               MOVE    80              TO  HKNJA-HON-GAIKYURATE
           WHEN    "2"
               MOVE    90              TO  HKNJA-HON-GAIKYURATE
           WHEN    "3"
               MOVE    100             TO  HKNJA-HON-GAIKYURATE
           WHEN    OTHER
               MOVE    ZERO            TO  HKNJA-HON-GAIKYURATE
           END-EVALUATE
      *    本人入院給付割合
           EVALUATE    SPA-GMN-HNKYURATE
           WHEN    "1"
               MOVE    80              TO  HKNJA-HON-NYUKYURATE
           WHEN    "2"
               MOVE    90              TO  HKNJA-HON-NYUKYURATE
           WHEN    "3"
               MOVE    100             TO  HKNJA-HON-NYUKYURATE
           WHEN    OTHER
               MOVE    ZERO            TO  HKNJA-HON-NYUKYURATE
           END-EVALUATE
      *    家族外来給付割合
           EVALUATE    SPA-GMN-KGKYURATE
           WHEN    "1"
               MOVE    80              TO  HKNJA-KZK-GAIKYURATE
           WHEN    "2"
               MOVE    90              TO  HKNJA-KZK-GAIKYURATE
           WHEN    "3"
               MOVE    100             TO  HKNJA-KZK-GAIKYURATE
           WHEN    OTHER
               MOVE    ZERO            TO  HKNJA-KZK-GAIKYURATE
           END-EVALUATE
      *    家族入院給付割合
           EVALUATE    SPA-GMN-KNKYURATE
           WHEN    "1"
               MOVE    80              TO  HKNJA-KZK-NYUKYURATE
           WHEN    "2"
               MOVE    90              TO  HKNJA-KZK-NYUKYURATE
           WHEN    "3"
               MOVE    100             TO  HKNJA-KZK-NYUKYURATE
           WHEN    OTHER
               MOVE    ZERO            TO  HKNJA-KZK-NYUKYURATE
           END-EVALUATE
      *----(02.01.01)--ADD-END-----
           MOVE    SPA-GMN-POST        TO  HKNJA-POST 
           MOVE    SPA-GMN-ADRS        TO  HKNJA-ADRS 
           MOVE    SPA-GMN-BANTI       TO  HKNJA-BANTI 
           MOVE    SPA-GMN-TEL         TO  HKNJA-TEL 
           MOVE    SPA-GMN-KIGO        TO  HKNJA-KIGO 
      *
           MOVE    SPA-GMN-IDOKBN      TO  HKNJA-IDOKBN
           MOVE    SPA-NAI-IDOYMD      TO  HKNJA-IDOYMD
      *
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
      *
           MOVE    SPA-OPID            TO  HKNJA-OPID
           MOVE    SMCNDATE-YMD        TO  HKNJA-UPYMD
           MOVE    SMCNDATE-HMS        TO  HKNJA-UPHMS
           IF      FLG-HKNJAINF        =   1
               MOVE    SMCNDATE-YMD        TO  HKNJA-CREYMD
           END-IF
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
      **** MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "tbl_hknjainf_user" TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           ELSE
	       MOVE    "DBINSERT"      TO  MCP-FUNC
           END-IF
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              NOT =   ZERO
               DISPLAY "N01 HKNJAINF ERR:" MCP-FUNC
                       ",MCP-RC:" MCP-RC
      *****************",KYE:" HKNJA-HOSPID
                       ",KYE:" HKNJA-HOSPNUM
                       ","   HKNJA-HKNJANUM
      *
               MOVE    "1001"                  TO  SPA-ERRCD
           END-IF
      *
      *    前回ＣＤへ保存
           MOVE    SPA-GMN-HKNJANUM    TO  WRK-HKNJANUM
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 310-SPASET-SEC 
           END-IF
      *    前回ＣＤへ保存
           MOVE    WRK-HKNJANUM        TO  SPA-MAE-HKNJANUM
           .
       490-KOUSHIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加設定（システム管理）へ
      *****************************************************************
       450-W65-SENI-SEC                SECTION.
      *
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           MOVE    "W01"               TO  SPA-SAKIPG
           MOVE    "N01"               TO  SPA-MOTOPG
      *
           PERFORM 800-SPA-OUTPUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
                                       LNKSYS-AREA
           MOVE    SPA-GMN-HKNJANUM    TO  LNKSYS-HKNJANUM
           MOVE    LNKSYS-AREA         TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-W65-SENI-EXT.
           EXIT.
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
           IF      SPA-NIDCD       NOT =   SPACE
               PERFORM 520-WIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "N01    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE                 TO  N01
           INITIALIZE                        N01
      *
           MOVE    SPA-GMN-HKNJANUM      TO  N01-HKNJANUM
           MOVE    SPA-GMN-HKNJANAME     TO  N01-HKNJANAME
           MOVE    SPA-GMN-HKNJANAMETAN1 TO  N01-HKNJANAMETAN1
           MOVE    SPA-GMN-HKNJANAMETAN2 TO  N01-HKNJANAMETAN2
           MOVE    SPA-GMN-HKNJANAMETAN3 TO  N01-HKNJANAMETAN3
           MOVE    SPA-GMN-POST          TO  N01-POST
           MOVE    SPA-GMN-ADRS          TO  N01-ADRS
           MOVE    SPA-GMN-BANTI         TO  N01-BANTI  
           MOVE    SPA-GMN-TEL           TO  N01-TEL
           MOVE    SPA-GMN-KIGO          TO  N01-KIGO
           MOVE    SPA-GMN-IDOKBN-G      TO  N01-IDOKBN
           MOVE    SPA-GMN-IDOYMD        TO  N01-IDOYMD
      *
           MOVE    SPA-GMN-HKNNUM-G      TO  N01-HKNNUM
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   SPA-GMN-HKNIDX
               MOVE    SPA-GMN-HKNNUMLST (IDX) TO  N01-HKNNUM-ITEM (IDX)
               MOVE    IDX                     TO  N01-HKNNUM-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-IDOMAX
               MOVE    SPA-GMN-IDOKBNLST (IDX)
                                           TO  N01-IDOKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-IDOMAX      TO  N01-IDOKBN-COUNT
      *
      *----(02.01.01)--ADD-START---
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-HGKYURATEMAX
               MOVE    SPA-GMN-HGKYURATELST (IDX)
                                           TO  N01-HGKYURATE-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HGKYURATEMAX    TO  N01-HGKYURATE-COUNT
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-HNKYURATEMAX
               MOVE    SPA-GMN-HNKYURATELST (IDX)
                                           TO  N01-HNKYURATE-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HNKYURATEMAX    TO  N01-HNKYURATE-COUNT
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-KGKYURATEMAX
               MOVE    SPA-GMN-KGKYURATELST (IDX)
                                           TO  N01-KGKYURATE-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-KGKYURATEMAX    TO  N01-KGKYURATE-COUNT
      *
           PERFORM VARYING     IDX     FROM    1       BY  1
                   UNTIL       IDX     >   SPA-GMN-KNKYURATEMAX
               MOVE    SPA-GMN-KNKYURATELST (IDX)
                                           TO  N01-KNKYURATE-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-KNKYURATEMAX    TO  N01-KNKYURATE-COUNT
      *
           IF        ( SPA-GMN-HKNNUM  =   "060" )
               AND   ( SPA-HKNJANUM-LEN    =   6 )
               AND   ( SPA-GMN-HKNJANUM(3:1)   =   "3" )
               AND   ( SPA-ERRCD       =   SPACE )
               MOVE    SPA-GMN-HGKYURATE-G TO  N01-HGKYURATE
               MOVE    SPA-GMN-HNKYURATE-G TO  N01-HNKYURATE
               MOVE    SPA-GMN-KGKYURATE-G TO  N01-KGKYURATE
               MOVE    SPA-GMN-KNKYURATE-G TO  N01-KNKYURATE
               MOVE    WIDGET-NORMAL       TO  N01-HGKYURATE-STATE
                                               N01-HNKYURATE-STATE
                                               N01-KGKYURATE-STATE
                                               N01-KNKYURATE-STATE
           ELSE
               MOVE    SPACE               TO  N01-HGKYURATE
                                               N01-HNKYURATE
                                               N01-KGKYURATE
                                               N01-KNKYURATE
               MOVE    SPACE               TO  SPA-GMN-HGKYURATE
                                               SPA-GMN-HNKYURATE
                                               SPA-GMN-KGKYURATE
                                               SPA-GMN-KNKYURATE
               MOVE    WIDGET-INSENSITIVE  TO  N01-HGKYURATE-STATE
                                               N01-HNKYURATE-STATE
                                               N01-KGKYURATE-STATE
                                               N01-KNKYURATE-STATE
           END-IF
      *----(02.01.01)--ADD-END-----
      *H29.10
      *    ユーザー判定
           IF      SPA-NAI-USER-FLG    =   ZERO
      *        マスター登録
               MOVE    SPACE               TO  N01-MIDASHI-DATA
               MOVE    SPACE               TO  N01-MIDASHI-STYLE
               MOVE    WIDGET-INSENSITIVE  TO  N01-B03-STATE
               MOVE    WIDGET-INSENSITIVE  TO  N01-B12-STATE
           ELSE
      *        ユーザー登録
               MOVE    "【ユーザ―管理】"  TO  N01-MIDASHI-DATA
               MOVE    "red"               TO  N01-MIDASHI-STYLE
               MOVE    WIDGET-NORMAL       TO  N01-B03-STATE
               MOVE    WIDGET-NORMAL       TO  N01-B12-STATE
      *        マスター登録あり
               IF      SPA-NAI-MAST-FLG    =   1
                   MOVE    "【提供マスター有】"
                                           TO  N01-MIDASHI02-DATA
                   MOVE    "blue"          TO  N01-MIDASHI02-STYLE
               END-IF
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "保険者番号入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "検証番号チェックエラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "全角で入力して下さい。"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "保険番号がありません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "保険番号と保険者番号の構成が違います。"
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "異動内容がありません。"
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "異動年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "保険者番号を入力して下さい"
                                               TO  SPA-ERRMSG
      *----(02.01.01)--ADD-START---
               WHEN    "0009"
                   MOVE    "給付割合がありません。"
                                               TO  SPA-ERRMSG
      *----(02.01.01)--ADD-END-----
               WHEN    "0010"
                   MOVE    "未登録のデータなので削除できません"
                                               TO  SPA-ERRMSG
               WHEN    "0099"
                   MOVE    "未入力の項目があります"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "更新処理ができませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "削除処理ができませんでした"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  NERR
           MOVE    SPA-ERRCD            TO  NERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  NERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "N01"                TO  SPA-MOTOPG
           MOVE    "NERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "NERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ表示
      *****************************************************************
       520-WIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-NIDMSG
      *
           EVALUATE    SPA-NIDCD
               WHEN    "1001"
      ****         MOVE    "登録処理をします。"    TO  WRK-NIDMSG
                   STRING  "登録処理をします。"
                           "よろしいですか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-NIDMSG
                   END-STRING
      *----(02.01.01)--ADD-START---
               WHEN    "1011"
                   MOVE    "想定外の給付割合です。登録処理をしますか？"
                                                 TO  WRK-NIDMSG
      *----(02.01.01)--ADD-END-----
               WHEN    "1002"
      ****         MOVE    "削除処理をします。"    TO  WRK-NIDMSG
                   STRING  "削除処理をします。"
                           "よろしいですか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-NIDMSG
                   END-STRING
      *
               WHEN    "1005"
                   STRING  "マスターをユーザーへ複写します。"
                           "よろしいですか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-NIDMSG
                   END-STRING
               WHEN    "1010"
                   STRING  "ユーザー管理となりますので、"
                           "マスター更新の対象外となります。"
                           "登録処理をしますか？"
                                                   DELIMITED BY  SIZE
                                               INTO    WRK-NIDMSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  NID1
           INITIALIZE                       NID1
           MOVE    SPA-NIDCD            TO  NID1-ID1CODE
           MOVE    WRK-NIDMSG           TO  NID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "N01"                TO  SPA-MOTOPG
           MOVE    "NID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "NID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-WIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理    
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          IF       SPA-GMN-CUR         =   ZERO
               PERFORM 50011-GMNCUR-SET-SEC
          END-IF
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE    "HKNJANUM"          TO   MCP-WIDGET
                WHEN    02
                    MOVE    "HKNJANAME"         TO   MCP-WIDGET
                WHEN    03
                    MOVE    "HKNJANAMETAN1"     TO   MCP-WIDGET
                WHEN    04
                    MOVE    "HKNJANAMETAN2"     TO   MCP-WIDGET
                WHEN    05
                    MOVE    "HKNJANAMETAN3"     TO   MCP-WIDGET
                WHEN    06
                    MOVE    "HKNNUM"            TO   MCP-WIDGET
                WHEN    07
                    MOVE    "POST"              TO   MCP-WIDGET
                WHEN    08
                    MOVE    "ADRS"              TO   MCP-WIDGET
                WHEN    09
                    MOVE    "BANTI"             TO   MCP-WIDGET
                WHEN    10
                    MOVE    "TEL"               TO   MCP-WIDGET
                WHEN    11
                    MOVE    "KIGO"              TO   MCP-WIDGET
                WHEN    12
                    MOVE    "IDOKBN"            TO   MCP-WIDGET
                WHEN    13
                    MOVE    "IDOYMD"            TO   MCP-WIDGET
      *----(02.01.01)--ADD-START---
                WHEN    14
                    MOVE    "HGKYURATE"         TO   MCP-WIDGET
                WHEN    15
                    MOVE    "HNKYURATE"         TO   MCP-WIDGET
                WHEN    16
                    MOVE    "KGKYURATE"         TO   MCP-WIDGET
                WHEN    17
                    MOVE    "KNKYURATE"         TO   MCP-WIDGET
      *----(02.01.01)--ADD-END-----
                WHEN    99
                    MOVE    "B12"               TO   MCP-WIDGET
            END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル設定処理
      *****************************************************************
       50011-GMNCUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "HKNJANUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "HKNJANAME"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "HKNJANAMETAN1"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "HKNJANAMETAN2"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "HKNJANAMETAN3"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "HKNNUM"
      *----(02.01.01)--UPD-START---
                   IF        ( SPA-GMN-HKNNUM  =   "060" )
                       AND   ( SPA-HKNJANUM-LEN    =   6 )
                       AND   ( SPA-GMN-HKNJANUM(3:1)   =   "3" )
                       MOVE    14             TO  SPA-GMN-CUR
                   ELSE
                       MOVE    7              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HGKYURATE"
                   MOVE    15              TO  SPA-GMN-CUR
               WHEN    "HNKYURATE"
                   MOVE    16              TO  SPA-GMN-CUR
               WHEN    "KGKYURATE"
                   MOVE    17              TO  SPA-GMN-CUR
               WHEN    "KNKYURATE"
                   MOVE    7               TO  SPA-GMN-CUR
      *----(02.01.01)--UPD-END-----
               WHEN    "POST"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "ADRS"
                   MOVE    9               TO  SPA-GMN-CUR
               WHEN    "BANTI"
                   MOVE    10              TO  SPA-GMN-CUR
               WHEN    "TEL"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    "KIGO"
                   MOVE    12              TO  SPA-GMN-CUR
               WHEN    "IDOKBN"
                   MOVE    13              TO  SPA-GMN-CUR
               WHEN    "IDOYMD"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-GMNCUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HENYMDG
               GO      TO      5002-HIZUKE-HEN-EXT
           END-IF
      *
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
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＣＲＳＰＡ読込処理
      *****************************************************************
       800-SPA-INPUT-SEC               SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-INPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-INPUT-SEC
      *
           .
       800-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ読込処理
      *****************************************************************
       8001-SPA-INPUT-SEC               SECTION.
      *
           PERFORM 900-SPATMP-KEY-SEL-SEC
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    SPATMP-DATA-REC
                                   TO  SPA-N01
           ELSE
               MOVE    SPATMP-DATA-REC
                                   TO  SPA-N01KYOUTU
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBDELETE-SEC
      *
           .
       8001-SPA-INPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ出力処理
      *****************************************************************
       800-SPA-OUTPUT-SEC              SECTION.
      *
           MOVE    "SCREEN"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-OUTPUT-SEC
      *
           MOVE    "COMMON"        TO  WRK-FILEMEI
           PERFORM 8001-SPA-OUTPUT-SEC
      *
           .
       800-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    ＳＣＲＳＰＡ出力処理
      *****************************************************************
       8001-SPA-OUTPUT-SEC             SECTION.
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           IF    ( WRK-FILEMEI     =   "SCREEN" )
               MOVE    SPA-N01
                                   TO  SPATMP-DATA-REC
           ELSE
               MOVE    SPA-N01KYOUTU
                                   TO  SPATMP-DATA-REC
           END-IF
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBINSERT-SEC
      *
           .
       8001-SPA-OUTPUT-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-INV-SEC         SECTION.
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBMAIN-SELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBMAIN-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
       900-HKNJAINF-INV-EXT.
           EXIT.
      *****************************************************************
      *    管理マスター読込（マスター）
      *****************************************************************
       900-HKNJAINF-MAST-INV-SEC         SECTION.
      *
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_hknjainf_master"   TO  WRK-PATH-TABLE
           PERFORM 900-HKNJAINF-INV-SEC
      *
           .
       900-HKNJAINF-MAST-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所マスター読込
      *****************************************************************
       900-ADRS-INV-SEC         SECTION.
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBMAIN-SELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBMAIN-FETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-ADRS
                   MOVE    MCPDATA-REC         TO  ADRS-REC
               ELSE
                   MOVE    1                   TO  FLG-ADRS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-ADRS
           END-IF
      *
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
       900-ADRS-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           PERFORM 910-DBMAIN-SELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
               MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           PERFORM 990-DBLOSEC-SEC
      *
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNMST-NEXT-SEC              SECTION.
      *
           PERFORM 920-DBMAIN-FETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNMST-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBMAIN-SELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBMAIN-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBMAIN-FETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBMAIN-FETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       900-SPATMP-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-SPATMP
      *
           INITIALIZE                  SPATMP-REC
      *
           PERFORM 9001-SPATMP-KEY-EDIT-SEC
      *
           MOVE    SPATMP-REC      TO  MCPDATA2-REC
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBSELECT-SEC
           IF    ( FLG-SPATMP      =   ZERO )
               MOVE    MCPDATA2-REC TO SPATMP-REC
           ELSE
               MOVE    1           TO  FLG-SPATMP
               INITIALIZE              SPATMP-REC
           END-IF
      *
           MOVE    "tbl_spa_tmp"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 920-DBCLOSECURSOR-SEC
      *
           .
       900-SPATMP-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索条件編集処理
      *****************************************************************
       9001-SPATMP-KEY-EDIT-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM     TO  SPATMP-HOSPNUM
           MOVE    "N01"           TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID      TO  SPATMP-TERMID
           MOVE    WRK-FILEMEI     TO  SPATMP-FILEMEI
           MOVE    1               TO  SPATMP-EDANUM
      *
           .
       9001-SPATMP-KEY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       920-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 920-DBFETCH-SEC
           END-IF
      *
           .
      *
       920-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       920-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       920-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       920-DBDELETE-SEC                SECTION.
      *
           MOVE    "DBDELETE"      TO  MCP-FUNC
           PERFORM 920-ORCDBSPATMP-SEC
      *
           .
      *
       920-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＯＲＣＳＰＡＴＭＰ呼出処理
      *****************************************************************
       920-ORCDBSPATMP-SEC                SECTION.
      *
           CALL    "ORCDBSPATMP"       USING
                                       MCPAREA
                                       MCPDATA2-REC
                                       SPA-AREA
      *
           .
      *
       920-ORCDBSPATMP-EXT.
           EXIT.
