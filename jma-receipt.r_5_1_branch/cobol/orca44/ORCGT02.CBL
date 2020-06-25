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
       PROGRAM-ID.             ORCGT02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 総括表・公費請求書
      *  コンポーネント名  : 公費請求書個別発行
      *  管理者            : 
      *  04/10/11    NaCl−藤原   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  02.07.01    NACL-藤原    05/10/31  MONFUNC対応
      *  02.09.01    NACL-藤原    06/05/10  処理中のメッセージ表示
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.07.01    NACL-藤原    12/08/01  レセ電データチェック機能対応
      *  04.07.02    NACL-藤原    13/04/10  個別作成時のプレビュー画面表示
      *
      *  04.08.01    NACL-藤原    16/01/18  地方公費処理スクリプトの存在
      *                                     チェック対応
      *  04.08.02    NACL-藤原    16/06/27  医療機関コード変更時のレセ電
      *                                     データ等の作成対応
      *                                      （１日からの変更のみ）
      *
      *  04.09.01    NACL-藤原    17/02/24  レポジトリ名設定時は地方公費
      *                                     処理スクリプトの存在チェック
      *                                     を行わない
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    
      *    処理スクリプトチェック用ワークファイル
           SELECT  WRK-FILE        ASSIGN  WRKPARA
                                   FILE    STATUS  IS  STS-WRK.
      *
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    処理スクリプトチェック用ワークファイル
       FD  WRK-FILE.
       01  WRK-R                   PIC X(10).
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    レセプト作成指示共通パラメタ
           COPY    "T01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "T01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-WRK             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
      *
           03  FLG-ROW             PIC 9(01).  
           03  FLG-CHK             PIC 9(01).  
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX2                PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-RECEERR         PIC X(200).
           03  WRK-TIDMSG          PIC X(80).
      * 
           03  WRK-T02-PARALST-ROW
                                   PIC S9(09). 
      *
           03  WRK-JOBID           PIC 9(03).
           03  WRK-STR             PIC 9(05).
           03  WRK-SELNUM          PIC S9(05).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-NUM             PIC ZZ9.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-CHK-AREA.
               05  WRK-CHK-CNT     PIC 9(02).
               05  WRK-CHK-SHELLID PIC X(20).
               05  WRK-CHK-SHORIMSG
                                   PIC X(40).
           03  WRK-Z9              PIC Z9.
      *
       01  WRKPARA                 PIC X(200).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMDG3        PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING  //SPA-//
                                   BY         //WRK-SPA-//.
      *     
           COPY    "ORCA-DBMETA".
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "044".
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "SOKATU".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *     
      *    ジョブ管理ＤＢのキー値（レセプト）  
           03  WRK-CONS-JOB-SHELLID-RECEPT1
                                   PIC X(08)   VALUE   "RECEPT1".
           03  WRK-CONS-JOB-JOBID-RECEPT1
                                   PIC 9(07)   VALUE   1.
      *
      *    最大行
           03  WRK-CONS-PARALST-MAX
                                   PIC 9(03)   VALUE   100.
           03  WRK-CONS-SHELLIDLST-MAX
                                   PIC 9(03)   VALUE   25.
      *
      *    エラーファイルＩＤ
           03  WRK-CONS-FILE-ID    PIC X(08)   VALUE   "OR044ERR".
      *
      *    県単独事業用シェル
       01  WRK-SHELL-KENTAN.
           03  FILLER             PIC X(15) VALUE
                                             "scripts/kentan/".
           03  WRK-SHELL-SHELL    PIC X(30).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK3003.INC".
      *
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".          
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
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
           COPY    "ORCA44SCRAREA.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
                                       WRK-AREA
      *                                 
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-T01FREE
           MOVE    SPA-WORK        TO  SPA-T01KYOUTU
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
           MOVE    SPA-T01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-T01FREE     TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "TERR"
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
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "T02    "           TO  MCP-WINDOW
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
           EVALUATE    SPA-MOTOPG
               WHEN    "TID1"
                   PERFORM 3002-TID1-SET-SEC
                   GO  TO  300-SCREEN-EXT
               WHEN    "TID2"
                   PERFORM 3003-TID2-SET-SEC
                   GO  TO  300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ１）ＯＫ処理
      *****************************************************************
       3002-TID1-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"
               EVALUATE    SPA-TIDCD
                   WHEN    "6666"
                       MOVE    SPACE           TO  SPA-TID1-FLG
                       MOVE    SPACE           TO  SPA-TIDCD
                       PERFORM 210-BACK
                       GO  TO  3002-TID1-SET-EXT
               END-EVALUATE        
           END-IF
      *
           MOVE    1               TO  SPA-GMN-T02-CUR
      *
           MOVE    SPACE           TO  SPA-TID1-FLG
           MOVE    SPACE           TO  SPA-TIDCD
           .
       3002-TID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＴＩＤ２）ＯＫ処理
      *****************************************************************
       3003-TID2-SET-SEC            SECTION.
      *
           IF      SPA-TID1-FLG    =   "OK"    OR  "NO"    OR  "PV"
      *        ジョブ管理チェック処理
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD      =   SPACE
                   PERFORM 30031-BTPARA-JOBID-UPDATE-SEC
                   IF      SPA-ERRCD      =   SPACE  
                       PERFORM 210-BACK
                       GO  TO  3003-TID2-SET-EXT
                   END-IF 
               END-IF
           END-IF
      *
           MOVE    1               TO  SPA-GMN-T02-CUR
      *
           MOVE    SPACE           TO  SPA-TID1-FLG
           MOVE    SPACE           TO  SPA-TIDCD
           .
       3003-TID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ更新（ＪＯＢＩＤ）処理
      *****************************************************************
       30031-BTPARA-JOBID-UPDATE-SEC    SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理
           MOVE    ZERO                TO  WRK-JOBID
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T02-SHELLID-MAX
               IF      SPA-GMN-T02-TSYORIFLG (IDX) =   "1"
                   ADD     1                   TO  WRK-JOBID
                   MOVE    WRK-JOBID           TO
                                               SPA-NAI-T02-TJOBID (IDX)
      * 
                   INITIALIZE                      BTPARA-REC
                   MOVE    WRK-CONS-JOB-SHELLID
                                               TO  BTPARA-SHELLID
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                               TO  BTPARA-SCRIPTID
                   MOVE    WRK-JOBID           TO  BTPARA-JOBID
                   MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
                   MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
                   MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
                   MOVE    BTPARA-REC          TO  MCPDATA-REC
                   MOVE    "tbl_btpara"        TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC      NOT =   ZERO
                       MOVE    "1006"          TO  SPA-ERRCD
                   END-IF
              END-IF
          END-PERFORM 
      *
          .
       30031-BTPARA-JOBID-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-GMN-T02-TSYORIFLG-TBL
                                       SPA-GMN-T02-PARA-TBL
                                       SPA-NAI-T02-PARA-TBL
                                       SPA-GMN-T02-KOUMOKU
                                       SPA-NAI-T02-KOUMOKU 
           MOVE    1               TO  SPA-GMN-T02-CUR
                                       SPA-GMN-T02-PAGE
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
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK-MAE-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-SAKUJYO-SEC
      *    処理開始
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GID-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
                   PERFORM 41001-SELNUM1-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   PERFORM 41002-SELNUM2-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "PARA"
                   PERFORM 41003-PARA-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SYORIFLG"
                   PERFORM 41004-SYORIFLG-CHK-SEC
      *        病名選択
               WHEN    ANY             ALSO    "PARALST"
                   PERFORM 41020-PARALST-SEC
      *        分類選択
               WHEN    ANY             ALSO    "SHELLIDLST"
                   PERFORM 41021-SHELLIDLST-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費一覧選択処理
      *****************************************************************
       41001-SELNUM1-CHK-SEC            SECTION.
      *   
      *    パラメタに入力途中の項目があるかチェック
           IF    ( SPA-GMN-T02-SELNUM2  >   ZERO  )
           OR    ( SPA-GMN-T02-PARA NOT =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-T02-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-T02-CUR
      *
               INITIALIZE                      SPA-GMN-T02-KOUMOKU
                                               SPA-NAI-T02-KOUMOKU
                                               SPA-GMN-T02-PARA-TBL
                                               SPA-NAI-T02-PARA-TBL
      *
               MOVE    T02-SELNUM1         TO  SPA-GMN-T02-SELNUM1
      *
               IF      SPA-GMN-T02-SELNUM1 =   ZERO
                   MOVE    99                  TO  SPA-GMN-T02-CUR
               ELSE
                   MOVE    ZERO                TO  FLG-CHK  
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       SPA-GMN-T02-SHELLID-MAX
                           OR      FLG-CHK     =   1
                       IF      SPA-GMN-T02-SELNUM1 =
                                           SPA-GMN-T02-TRENNUM1 (IDX)
                           MOVE    IDX             TO
                                                   SPA-NAI-T02-SELNUM1
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-PERFORM
                   IF      FLG-CHK             =   ZERO                           
                       MOVE    "0001"          TO  SPA-ERRCD
                   ELSE
                       PERFORM 410011-SHELLIDLST-SELECT-SEC
                   END-IF
               END-IF
           END-IF    
           .
       41001-SELNUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費一覧選択処理
      *****************************************************************
       410011-SHELLIDLST-SELECT-SEC    SECTION.
      *                                     
           INITIALIZE                  SPA-GMN-T02-PARA-TBL
                                       SPA-NAI-T02-PARA-TBL
                                       SPA-GMN-T02-KOUMOKU2 
                                       SPA-NAI-T02-KOUMOKU2
      *
           EVALUATE    SPA-GMN-T02-TSYORIFLG (SPA-NAI-T02-SELNUM1)
               WHEN    SPACE
                   MOVE    SPACE           TO  SPA-GMN-T02-SYORIFLG-G
               WHEN    "1"
                   MOVE    SPA-GMN-T02-SYORIFLGLST (2)
                                           TO  SPA-GMN-T02-SYORIFLG-G
           END-EVALUATE            
           MOVE    SPA-GMN-T02-SYORIFLGLST (2)
                                   TO  SPA-GMN-T02-SYORIFLG-G
      *
           MOVE    SPA-GMN-T02-TSHORIMSG (SPA-NAI-T02-SELNUM1)
                                       TO  SPA-GMN-T02-SHORIMSG
           EVALUATE    SPA-NAI-T02-TPARAKBN (SPA-NAI-T02-SELNUM1)
               WHEN    "1"
                   MOVE    "保険者番号"    TO  SPA-GMN-T02-TITLE1
                   MOVE    "保険者名"      TO  SPA-GMN-T02-TITLE2 
               WHEN    "2"
                   MOVE    "患者番号"      TO  SPA-GMN-T02-TITLE1
                   MOVE    "氏    名"      TO  SPA-GMN-T02-TITLE2
           END-EVALUATE 
      *
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-SCRIPTID
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 4100111-SHELLID-LST-SEC
      *
           MOVE    2                   TO  SPA-GMN-T02-CUR
           .
       410011-SHELLIDLST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費パラメタ一覧編集処理
      *****************************************************************
       4100111-SHELLID-LST-SEC         SECTION.
      *
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               INITIALIZE                  BTPARA-REC
               MOVE    1               TO  FLG-BTPARA
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-T02-PAGE  -  1 )
                                   *   WRK-CONS-PARALST-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
           PERFORM UNTIL ( IDX             >=  WRK-CONS-PARALST-MAX ) OR
                         ( FLG-BTPARA       =   1                   )
               ADD     1                   TO  IDX2
               IF      IDX2                    >=  WRK-STR
                   ADD     1               TO  IDX
                   IF      IDX             <=  WRK-CONS-PARALST-MAX
                       MOVE    BTPARA-RENNUM
                                           TO  SPA-GMN-T02-TRENNUM2(IDX)
                       EVALUATE    SPA-NAI-T02-TPARAKBN
                                           (SPA-NAI-T02-SELNUM1)
                           WHEN    "1"    
                               MOVE    BTPARA-INFO-PARA TO
                                               SPA-GMN-T02-TPARA (IDX)
                               INITIALIZE          HKNJAINF-REC
                               MOVE    SPA-HOSPNUM TO HKNJA-HOSPNUM
                               MOVE   SPA-GMN-T02-TPARA (IDX)
                                                   TO HKNJA-HKNJANUM
                               MOVE    HKNJAINF-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-HKNJA-READ-SEC
                               IF      FLG-HKNJAINF    =   ZERO
                                   MOVE    HKNJA-HKNJANAME  TO
                                               SPA-GMN-T02-TNAME (IDX)
                               END-IF
                           WHEN    "2"
                               MOVE    BTPARA-INFO-PTNUM   TO
                                               SPA-GMN-T02-TPARA (IDX)
                               MOVE    BTPARA-INFO-PTID    TO 
                                               SPA-NAI-T02-TPTID (IDX)
                               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                               MOVE    SPA-NAI-T02-TPTID (IDX)
                                                       TO  PTINF-PTID
                               PERFORM 900-PTINF-READ-SEC
                               IF      FLG-PTINF       =   ZERO
                                   MOVE    PTINF-NAME  TO
                                               SPA-GMN-T02-TNAME (IDX)
                               END-IF
                       END-EVALUATE  
      *      
                       MOVE    IDX         TO  SPA-GMN-T02-PARA-MAX
                   END-IF
               END-IF
      *
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           END-PERFORM
      *
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF    ( IDX                 >=  WRK-CONS-PARALST-MAX )  AND
                 ( FLG-BTPARA          =   ZERO                 )
               MOVE    1                   TO  SPA-GMN-T02-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-T02-NEXT
           END-IF
      *
           MOVE    1                   TO  FLG-ROW
           .
       4100111-SHELLID-LST-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ選択処理
      *****************************************************************
       41002-SELNUM2-CHK-SEC            SECTION.
      *
           IF      SPA-NAI-T02-SELNUM1  =  ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-T02-CUR
               GO  TO  41002-SELNUM2-CHK-EXT
           END-IF
      *                                     
           MOVE    3                   TO  SPA-GMN-T02-CUR
      *
           INITIALIZE                      SPA-GMN-T02-KOUMOKU2
                                           SPA-NAI-T02-KOUMOKU2
      *
           MOVE    T02-SELNUM2         TO  SPA-GMN-T02-SELNUM2
      *
           IF      SPA-GMN-T02-SELNUM2 =   ZERO
               MOVE    4                   TO  SPA-GMN-T02-CUR
           ELSE  
               PERFORM VARYING IDX    FROM    1   BY  1
                       UNTIL   IDX    >   WRK-CONS-PARALST-MAX
                   IF      SPA-GMN-T02-SELNUM2
                                      =   SPA-GMN-T02-TRENNUM2 (IDX)
                       MOVE    IDX             TO  SPA-NAI-T02-SELNUM2
                       PERFORM 410021-PARALST-SELECT-SEC
                   END-IF
               END-PERFORM
               IF          SPA-NAI-T02-SELNUM2 =   ZERO
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       41002-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ一覧選択処理
      *****************************************************************
       410021-PARALST-SELECT-SEC        SECTION.
      *
           MOVE    SPA-GMN-T02-TPARA (SPA-NAI-T02-SELNUM2)
                                       TO  SPA-GMN-T02-PARA
           MOVE    SPA-GMN-T02-TNAME (SPA-NAI-T02-SELNUM2)
                                       TO  SPA-GMN-T02-NAME
           MOVE    SPA-NAI-T02-TPTID (SPA-NAI-T02-SELNUM2)
                                       TO  SPA-NAI-T02-PTID
      *
           MOVE    4                   TO  SPA-GMN-T02-CUR
           .
       410021-PARALST-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタチェック処理
      *****************************************************************
       41003-PARA-CHK-SEC          SECTION.
      *
           IF      SPA-NAI-T02-SELNUM1  =  ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-T02-CUR
               GO  TO  41003-PARA-CHK-EXT
           END-IF
      *
           MOVE    4               TO  SPA-GMN-T02-CUR
      *
           INITIALIZE                  SPA-NAI-T02-PTID
                                       SPA-GMN-T02-NAME
      *
           MOVE    T02-PARA        TO  SPA-GMN-T02-PARA
      *
           IF      SPA-GMN-T02-PARA    =   SPACE
               IF      SPA-GMN-T02-SELNUM2 =   ZERO
                   MOVE    1               TO  SPA-GMN-T02-CUR
               ELSE
                   EVALUATE    SPA-NAI-T02-TPARAKBN(SPA-NAI-T02-SELNUM1)
                       WHEN    "1"    
                           MOVE    "0016"          TO  SPA-ERRCD
                       WHEN    "2"    
                           MOVE    "0015"          TO  SPA-ERRCD
                   END-EVALUATE 
               END-IF
           ELSE
               EVALUATE    SPA-NAI-T02-TPARAKBN (SPA-NAI-T02-SELNUM1)
                   WHEN    "1"    
                       IF      SPA-GMN-T02-PARA (9:)
                                           NOT =   SPACE    
                           MOVE    "0012"          TO  SPA-ERRCD
                       ELSE
                           INITIALIZE                  HKNJAINF-REC
                           MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
                           MOVE   SPA-GMN-T02-PARA TO  HKNJA-HKNJANUM
                           MOVE    HKNJAINF-REC    TO  MCPDATA-REC
                           PERFORM 900-HKNJA-READ-SEC
                           IF      FLG-HKNJAINF    =   ZERO
                               MOVE    HKNJA-HKNJANAME
                                                   TO  SPA-GMN-T02-NAME
                           ELSE
                               MOVE    "0012"      TO  SPA-ERRCD
                           END-IF
                       END-IF
                   WHEN    "2"
                       INITIALIZE                  ORCSPTIDAREA
                       MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                       MOVE    SPA-GMN-T02-PARA    TO  SPTID-PTNUM
                       CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                           SPA-AREA
                       IF      SPTID-RC        =   ZERO
                           MOVE    SPTID-PTNUM     TO  SPA-GMN-T02-PARA
                           MOVE    SPTID-PTID      TO  SPA-NAI-T02-PTID
                           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                           MOVE    SPA-NAI-T02-PTID
                                                   TO  PTINF-PTID
                           PERFORM 900-PTINF-READ-SEC
                           IF      FLG-PTINF       =   ZERO
                               MOVE    PTINF-NAME  TO  SPA-GMN-T02-NAME
                           ELSE
                               MOVE    "0003"      TO  SPA-ERRCD
                           END-IF
                       ELSE
                           MOVE    "0003"          TO  SPA-ERRCD
                       END-IF
                   WHEN    OTHER
                       MOVE    "0011"      TO  SPA-ERRCD
               END-EVALUATE
      *
      *        パラメタ重複チェック
               IF      SPA-ERRCD           =   SPACE
                   INITIALIZE                      BTPARA-REC
                   MOVE    WRK-CONS-JOB-SHELLID
                                               TO  BTPARA-SHELLID
                   MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                               TO  BTPARA-SCRIPTID
                   EVALUATE    SPA-NAI-T02-TPARAKBN(SPA-NAI-T02-SELNUM1)
                       WHEN    "1"
                           MOVE    SPA-GMN-T02-PARA
                                               TO  BTPARA-INFO-PARA
                       WHEN    "2"
                           MOVE    SPA-NAI-T02-PTID
                                               TO  BTPARA-INFO-PTID
                           MOVE    SPA-GMN-T02-PARA 
                                               TO  BTPARA-INFO-PTNUM
                   END-EVALUATE
                   MOVE    "tbl_btpara"        TO  WRK-MCP-TABLE
                   MOVE    "key2"              TO  WRK-MCP-PATHNAME
                   PERFORM 900-BTPARA-READ-SEC
                   IF      FLG-BTPARA          =   ZERO
                       IF    ( SPA-NAI-T02-SELNUM2 >   ZERO            )
                       AND   ( BTPARA-RENNUM       =
                                               SPA-GMN-T02-TRENNUM2
                                                 (SPA-NAI-T02-SELNUM2) )
                           CONTINUE
                       ELSE
                           MOVE    "0007"          TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
      *        更新処理 
               IF      SPA-ERRCD           =   SPACE         
                   IF      SPA-GMN-T02-SELNUM2 =   ZERO
                       PERFORM 410031-TUIKA-SEC
                   ELSE
                       PERFORM 410032-HENKOU-SEC
                   END-IF
               END-IF     
           END-IF
           .
       41003-PARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加処理
      *****************************************************************
       410031-TUIKA-SEC                SECTION.
      *
      *    最大登録数チェック
           IF      SPA-GMN-T02-PARA-MAX
                                       =   100
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      410031-TUIKA-EXT
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *    追加処理    
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-SCRIPTID
           MOVE    "tbl_btpara"        TO  WRK-MCP-TABLE
           MOVE    "key6"              TO  WRK-MCP-PATHNAME
           PERFORM 900-BTPARA-READ-SEC
           COMPUTE WRK-RENNUM  =   BTPARA-RENNUM   +   1
      *
           INITIALIZE                      BTPARA-REC
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    SPA-NAI-T02-TKBNCD (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-DSP-RENNUM
           MOVE    WRK-RENNUM          TO  BTPARA-RENNUM
           MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-SCRIPTID
           MOVE    SPA-NAI-T02-TPARAKBN (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-INFO-KBN
           EVALUATE    SPA-NAI-T02-TPARAKBN (SPA-NAI-T02-SELNUM1)
               WHEN    "1"
                   MOVE    SPA-GMN-T02-PARA    TO  BTPARA-INFO-PARA
               WHEN    "2"
                   MOVE    SPA-NAI-T02-PTID    TO  BTPARA-INFO-PTID
                   MOVE    SPA-GMN-T02-PARA    TO  BTPARA-INFO-PTNUM
           END-EVALUATE
           MOVE    SMCNDATE-YMD        TO  BTPARA-CREYMD
           MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
           MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1002"          TO  SPA-ERRCD
           ELSE
               ADD     1                TO  SPA-GMN-T02-PARA-MAX
               MOVE    WRK-RENNUM       TO  SPA-GMN-T02-TRENNUM2
                                            (SPA-GMN-T02-PARA-MAX)
               MOVE    SPA-GMN-T02-PARA-MAX
                                        TO  IDX
               PERFORM 4100311-INTBL-SET-SEC
      *
               MOVE    SPA-GMN-T02-PARA-MAX
                                        TO  T02-PARALST-ROW
      *
               MOVE    4                TO  SPA-GMN-T02-CUR  
           END-IF
      *
           .
       410031-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4100311-INTBL-SET-SEC              SECTION.
      *
           MOVE    SPA-GMN-T02-PARA    TO  SPA-GMN-T02-TPARA  (IDX)
           MOVE    SPA-NAI-T02-PTID    TO  SPA-NAI-T02-TPTID  (IDX)
           MOVE    SPA-GMN-T02-NAME    TO  SPA-GMN-T02-TNAME  (IDX)
      *
           INITIALIZE                   SPA-GMN-T02-KOUMOKU2
                                        SPA-NAI-T02-KOUMOKU2
      *
          .
       4100311-INTBL-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    変更　処理
      *****************************************************************
       410032-HENKOU-SEC              SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新処理    
           INITIALIZE                      BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  BTPARA-SHELLID
           MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                       TO  BTPARA-SCRIPTID
           MOVE    SPA-GMN-T02-TRENNUM2 (SPA-NAI-T02-SELNUM2)
                                       TO  BTPARA-RENNUM
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-BTPARA-READ-SEC
           IF      FLG-BTPARA          =   ZERO
               EVALUATE    SPA-NAI-T02-TPARAKBN(SPA-NAI-T02-SELNUM1)
                   WHEN    "1"
                       MOVE    SPA-GMN-T02-PARA    TO  BTPARA-INFO-PARA
                   WHEN    "2"
                       MOVE    SPA-NAI-T02-PTID    TO  BTPARA-INFO-PTID
                       MOVE    SPA-GMN-T02-PARA    TO  BTPARA-INFO-PTNUM
               END-EVALUATE
               MOVE    SMCNDATE-YMD        TO  BTPARA-UPYMD
               MOVE    SMCNDATE-HMS        TO  BTPARA-UPHMS
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBUPDATE"          TO  MCP-FUNC
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC      NOT =   ZERO
                   MOVE    "1003"          TO  SPA-ERRCD
               END-IF
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
           END-IF
      * 
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-NAI-T02-SELNUM2 TO  IDX
               PERFORM 4100311-INTBL-SET-SEC
      *
               MOVE    4                   TO  SPA-GMN-T02-CUR
           END-IF  
      *
          .
       410032-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理区分チェック処理
      *****************************************************************
       41004-SYORIFLG-CHK-SEC          SECTION.
      *
           MOVE    2                   TO  SPA-GMN-T02-CUR
      *
           IF      SPA-NAI-T02-SELNUM1 =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-T02-CUR
           END-IF
      *         
           IF      SPA-ERRCD          =   SPACE
               MOVE    T02-SYORIFLG        TO  SPA-GMN-T02-SYORIFLG-G
      *
               EVALUATE    SPA-GMN-T02-SYORIFLG
                   WHEN    SPACE
                   WHEN    ZERO
                       MOVE    SPA-GMN-T02-SYORIFLGLST (1)
                                           TO  SPA-GMN-T02-SYORIFLG-G
                       MOVE    SPACE       TO  SPA-GMN-T02-TSYORIFLG
                                                   (SPA-NAI-T02-SELNUM1)
                   WHEN    "1"
                       MOVE    SPA-GMN-T02-SYORIFLGLST (2)
                                           TO  SPA-GMN-T02-SYORIFLG-G
                                               SPA-GMN-T02-TSYORIFLG
                                                   (SPA-NAI-T02-SELNUM1)
                   WHEN    OTHER
                       MOVE    "0008"          TO  SPA-ERRCD
               END-EVALUATE            
      *
               IF      SPA-ERRCD          =   SPACE
                   IF    ( SPA-GMN-T02-SYORIFLG   =   SPACE   OR  ZERO )
                   OR    ( SPA-GMN-T02-TPARAKBN (SPA-NAI-T02-SELNUM1   )
                                                  =   SPACE )       
                       INITIALIZE                 SPA-GMN-T02-KOUMOKU
                                                  SPA-NAI-T02-KOUMOKU
                                                  SPA-GMN-T02-PARA-TBL
                                                  SPA-NAI-T02-PARA-TBL 
                       MOVE    1                  TO  SPA-GMN-T02-CUR
                   ELSE
                       MOVE    4                  TO  SPA-GMN-T02-CUR
                   END-IF
               END-IF
           END-IF
           .
       41004-SYORIFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ一覧選択処理
      *****************************************************************
       41020-PARALST-SEC       SECTION.
      *          
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T02-PARA-MAX
               IF      T02-PARALST-SELECT (IDX)     =   "T"
                   IF      IDX             =   SPA-NAI-T02-SELNUM2
                       CONTINUE
                   ELSE        
                       MOVE    IDX                     TO  WRK-SELNUM
                       MOVE    SPA-GMN-T02-PARA-MAX TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM           >   ZERO
               MOVE    WRK-SELNUM           TO  SPA-NAI-T02-SELNUM2
               MOVE    SPA-GMN-T02-TRENNUM2  (SPA-NAI-T02-SELNUM2) 
                                           TO SPA-GMN-T02-SELNUM2
           END-IF
      *
           PERFORM 410021-PARALST-SELECT-SEC
           .
       41020-PARALST-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象公費一覧選択処理
      *****************************************************************
       41021-SHELLIDLST-SEC       SECTION.
      *   
      *    パラメタに入力途中の項目があるかチェック
           IF    ( SPA-GMN-T02-SELNUM2  >   ZERO  )
           OR    ( SPA-GMN-T02-PARA NOT =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-T02-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *          
      *        行選択
               MOVE    ZERO                TO  WRK-SELNUM
      *     
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-GMN-T02-SHELLID-MAX
                   IF      T02-SHELLIDLST-SELECT (IDX) =   "T"
                       IF      IDX             =   SPA-NAI-T02-SELNUM1
                           CONTINUE
                       ELSE        
                           MOVE    IDX                 TO  WRK-SELNUM
                           MOVE    SPA-GMN-T02-SHELLID-MAX
                                                       TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
      *
               IF      WRK-SELNUM          >   ZERO
                   INITIALIZE                      SPA-GMN-T02-KOUMOKU
                                                   SPA-NAI-T02-KOUMOKU
                                                   SPA-GMN-T02-PARA-TBL
                                                   SPA-NAI-T02-PARA-TBL
      *
                   MOVE    WRK-SELNUM          TO  SPA-NAI-T02-SELNUM1
                   MOVE    SPA-GMN-T02-TRENNUM1 (SPA-NAI-T02-SELNUM1) 
                                               TO SPA-GMN-T02-SELNUM1
               END-IF
      *
               PERFORM 410011-SHELLIDLST-SELECT-SEC
           END-IF
           .
       41021-BUNRUISEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る前処理
      *****************************************************************
       210-BACK-MAE-SEC                 SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   SPA-GMN-T02-SHELLID-MAX
               IF      SPA-GMN-T02-TSYORIFLG (IDX)  =   "1"
                   MOVE    "6666"          TO  SPA-TIDCD
                   GO  TO 210-BACK-MAE-EXT
               END-IF
           END-PERFORM
      *
           PERFORM 210-BACK
           .
       210-BACK-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
      *
           MOVE    "T01"               TO  SPA-SAKIPG
           MOVE    "T02"               TO  SPA-MOTOPG
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
      *    削除処理
      *****************************************************************
       440-SAKUJYO-SEC             SECTION.
      *
           IF      SPA-NAI-T02-SELNUM1  =  ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *
           IF      SPA-NAI-T02-SELNUM2 =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               GO  TO  440-SAKUJYO-EXT
           END-IF
      *
      *    削除処理    
           INITIALIZE                  BTPARA-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  BTPARA-SHELLID
           MOVE    SPA-GMN-T02-TRENNUM2 (SPA-NAI-T02-SELNUM2)
                                   TO  BTPARA-RENNUM
           MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                   TO  BTPARA-SCRIPTID
           MOVE    SPA-HOSPNUM     TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"      TO  MCP-FUNC
           MOVE    "tbl_btpara"    TO  MCP-TABLE
           MOVE    "del2"          TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC      NOT =   ZERO
               MOVE    "1005"          TO  SPA-ERRCD
           ELSE
      *
               INITIALIZE                   SPA-GMN-T02-KOUMOKU2
                                            SPA-NAI-T02-KOUMOKU2
                                            SPA-GMN-T02-PARA-TBL
                                            SPA-NAI-T02-PARA-TBL
      *
               INITIALIZE                      BTPARA-REC
               MOVE    WRK-CONS-JOB-SHELLID
                                           TO  BTPARA-SHELLID
               MOVE    SPA-NAI-T02-TSHELLID (SPA-NAI-T02-SELNUM1)
                                           TO  BTPARA-SCRIPTID
               MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
               MOVE    BTPARA-REC          TO  MCPDATA-REC
               MOVE    "tbl_btpara"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 4100111-SHELLID-LST-SEC
      *
               MOVE    4                   TO  SPA-GMN-T02-CUR
           END-IF
      *
          .
       440-SAKUJYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁処理
      *****************************************************************
       460-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T02-PAGE        =   1
               GO  TO  460-MAEGMN-EXT
           END-IF
      *
           COMPUTE SPA-GMN-T02-PAGE    =   SPA-GMN-T02-PAGE    -   1
      *
           MOVE    1                   TO  SPA-GMN-T02-CUR     
      *
           .
       460-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       470-ATOGMN-SEC             SECTION.
      *
           IF      SPA-GMN-T02-NEXT    =   ZERO
               GO  TO  470-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-T02-PAGE
      *
           MOVE    1                   TO  SPA-GMN-T02-CUR     
      *
           .
       470-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-PRINT-GID-SEC                SECTION.
      *   
      *    入力途中の項目があるかチェック
           IF    ( SPA-GMN-T02-SELNUM2  >   ZERO  )
           OR    ( SPA-GMN-T02-PARA NOT =   SPACE )
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-T02-CUR
           END-IF
      *
      *    処理対象のチェック
           IF      SPA-ERRCD       =   SPACE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX         >   SPA-GMN-T02-SHELLID-MAX
                       OR      FLG-CHK     =   1
                   IF      SPA-GMN-T02-TSYORIFLG (IDX)
                                               =   "1"
                       MOVE    1                   TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-T02-CUR
               END-IF
           END-IF
      *       
      *    処理スクリプトの存在チェック
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4502-SCRIPT-CHECK-SEC
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
               PERFORM 4501-JOBKANRI-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
      *            ガイドメッセージセット
                   MOVE    "1001"              TO  SPA-TIDCD
               END-IF
           END-IF
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ管理チェック処理
      *****************************************************************
       4501-JOBKANRI-CHK-SEC       SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRCD
      *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-NAI-SKYYM       TO  SYS-3003-STYUKYMD (1:6)
           MOVE    "01"                TO  SYS-3003-STYUKYMD (7:2)
           MOVE    SYS-3003-STYUKYMD   TO  SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC 
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SYS-3003-JOBID
      *            ジョブ管理チェック処理
                   MOVE    "CHK"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    IDX             TO  JOB-JOBID
                   MOVE    WRK-CONS-JOB-SHELLID
                                           TO  JOB-SHELLID
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "7777"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  WRK-RECEERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE      
                                                   INTO  WRK-RECEERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
      *
      *    ジョブ管理チェック（レセプト）
           IF      SPA-ERRCD           =   SPACE         
               MOVE    "CHK"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-CONS-JOB-JOBID-RECEPT1
                                       TO  JOB-JOBID
               MOVE    WRK-CONS-JOB-SHELLID-RECEPT1
                                       TO  JOB-SHELLID
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
              IF      SJOBKANRI-RETURN
                                       =   ZERO
                   IF      JOB-ENDYMD      =   SPACE
                       MOVE    "7777"          TO  SPA-ERRCD
                       MOVE    SPACE             TO  WRK-RECEERR
                       STRING  "レセプト作成処理中です【"
                                                 DELIMITED  BY  SIZE
                               JOB-SHELLMSG      DELIMITED  BY  SPACE
                               "】"              DELIMITED  BY  SIZE      
                                                 INTO  WRK-RECEERR
                       END-STRING
                   END-IF
               END-IF    
           END-IF    
           .
       4501-JOBKANRI-CHK-EXT.
           EXIT.
      *     
      *****************************************************************
      *    処理スクリプトの存在チェック
      *****************************************************************
       4502-SCRIPT-CHECK-SEC              SECTION.
      *
           INITIALIZE              WRK-CHK-AREA
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T02-SHELLID-MAX
               IF      SPA-GMN-T02-TSYORIFLG (IDX)
                                               =   "1"
                 IF    ( SPA-MW      =   SPA-GINBEE )
                 AND   ( SPA-NAI-T02-TNAME   (IDX)
                                 NOT =   SPACE      )
                   CONTINUE
                 ELSE
       *            ファイルデイレクトリ位置指定
                   MOVE    SPA-NAI-T02-TSHELLID (IDX)
                                           TO  WRK-SHELL-SHELL
                   INITIALIZE                  ORCSMKPASSAREA
                   MOVE    WRK-SHELL-KENTAN
                                           TO  MKPASS-IN-01
                   CALL    "ORCSMKPASSSITE"    USING   ORCSMKPASSAREA
                   MOVE    MKPASS-OT-01    TO  WRKPARA
      *
                   OPEN    INPUT               WRK-FILE
                   IF      STS-WRK         =   "00"
                       CLOSE   WRK-FILE
                   ELSE
                       ADD     1               TO  WRK-CHK-CNT
                       IF      SPA-ERRCD       =   SPACE
                           MOVE    "0018"          TO  SPA-ERRCD
                           MOVE    SPA-NAI-T02-TSHELLID  (IDX)
                                                   TO  WRK-CHK-SHELLID
                           MOVE    SPA-GMN-T02-TSHORIMSG (IDX)
                                                   TO  WRK-CHK-SHORIMSG
                       END-IF
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
           .
       4502-SCRIPT-CHECK-EXT.
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
           IF      SPA-TIDCD       NOT =   SPACE
               EVALUATE    SPA-TIDCD
                   WHEN    "1001"
                       PERFORM 530-TID2SET-SEC
                       GO  TO  500-SET-SCREEN-EXT
                   WHEN    OTHER
                       PERFORM 520-TIDSET-SEC
                       GO  TO  500-SET-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "T02    "           TO  MCP-WINDOW
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
               MOVE    ZERO                TO  WRK-T02-PARALST-ROW
           ELSE
               MOVE    T02-PARALST-ROW     TO  WRK-T02-PARALST-ROW
           END-IF
      * 
           INITIALIZE                  T02
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T02-SYORIFLG-MAX
               MOVE    SPA-GMN-T02-SYORIFLGLST (IDX)
                                           TO  T02-SYORIFLG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-T02-SYORIFLG-MAX
                                       TO  T02-SYORIFLG-COUNT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-T02-SHELLID-MAX
               MOVE    SPA-GMN-T02-TRENNUM1  (IDX)
                                       TO  WRK-NUM
               MOVE    WRK-NUM         TO  T02-SHELLIDLST-RENNUM  (IDX)
               IF      SPA-GMN-T02-TSYORIFLG (IDX)
                                       =   "1"
                   MOVE    "◯"        TO  T02-SHELLIDLST-SYORIFLG(IDX)
               END-IF
               MOVE    SPA-GMN-T02-TSHORIMSG (IDX)
                                       TO  T02-SHELLIDLST-SHORIMSG(IDX)
               MOVE    SPA-GMN-T02-TPARAKBN  (IDX)
                                       TO  T02-SHELLIDLST-PARAKBN (IDX)
               IF      IDX             =   SPA-NAI-T02-SELNUM1
                   MOVE    "T"         TO  T02-SHELLIDLST-SELECT  (IDX)
               ELSE
                   MOVE    "F"         TO  T02-SHELLIDLST-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-T02-SHELLID-MAX
                                       TO  T02-SHELLIDLST-COUNT
           MOVE    ZERO                TO  T02-SHELLIDLST-ROW
      *
           MOVE    SPA-GMN-T02-TITLE1  TO  T02-TITLE1
           MOVE    SPA-GMN-T02-TITLE2  TO  T02-TITLE2
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-GMN-T02-PARA-MAX
               MOVE    SPA-GMN-T02-TRENNUM2 (IDX)
                                           TO  WRK-NUM
               MOVE    WRK-NUM             TO  T02-PARALST-RENNUM (IDX)
               MOVE    SPA-GMN-T02-TPARA(IDX)
                                           TO  T02-PARALST-PARA   (IDX)
               MOVE    SPA-GMN-T02-TNAME(IDX)
                                           TO  T02-PARALST-NAME   (IDX)
               IF      IDX             =   SPA-NAI-T02-SELNUM2
                   MOVE    "T"             TO  T02-PARALST-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  T02-PARALST-SELECT (IDX)
               END-IF
           END-PERFORM    
           MOVE    SPA-GMN-T02-PARA-MAX    TO  T02-PARALST-COUNT
           MOVE    ZERO                    TO  T02-PARALST-ROW
           IF      WRK-T02-PARALST-ROW     >   ZERO
               MOVE    WRK-T02-PARALST-ROW     TO  T02-PARALST-ROW
           END-IF
      *
           MOVE    SPA-GMN-T02-SELNUM1 TO  T02-SELNUM1
           MOVE    SPA-GMN-T02-SYORIFLG-G
                                       TO  T02-SYORIFLG
           MOVE    SPA-GMN-T02-SHORIMSG
                                       TO  T02-SHORIMSG  
           MOVE    SPA-GMN-T02-SELNUM2 TO  T02-SELNUM2
           MOVE    SPA-GMN-T02-PARA    TO  T02-PARA
           MOVE    SPA-GMN-T02-NAME    TO  T02-NAME
           IF    ( SPA-NAI-T02-SELNUM1 >   ZERO        )
           AND   ( SPA-NAI-T02-TPARAKBN (SPA-NAI-T02-SELNUM1)
                                   NOT =   "1" AND "2" )
               MOVE    WIDGET-INSENSITIVE  TO  T02-SELNUM2-STATE
                                               T02-PARA-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  T02-SELNUM2-STATE
                                               T02-PARA-STATE
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
         EVALUATE    SPA-GMN-T02-CUR
             WHEN    1
                     MOVE  "SELNUM1"    TO  MCP-WIDGET
             WHEN    2
                     MOVE  "SYORIFLG"   TO  MCP-WIDGET
             WHEN    3
                     MOVE  "SELNUM2"    TO  MCP-WIDGET
             WHEN    4
                     MOVE  "PARA"       TO  MCP-WIDGET
             WHEN    99
                     MOVE  "B12"        TO  MCP-WIDGET
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           MOVE    SPACE                TO  TERR
           MOVE    SPA-ERRCD            TO  TERR-ERRCODE
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号エラー"            TO  TERR-ERRMSG
               WHEN    "0002"
                   MOVE    "これ以上追加できません"    TO  TERR-ERRMSG
               WHEN    "0003"
                   MOVE
                       "該当の患者が存在しません"      TO  TERR-ERRMSG
               WHEN    "0004"
                   MOVE    "対象のデータを選択してありません" 
                                                       TO  TERR-ERRMSG
               WHEN    "0006"
                   MOVE    "対象の処理が選択してありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0005"
                   MOVE    "対象のデータが選択してあります"
                                                       TO  TERR-ERRMSG
               WHEN    "0007"
                   MOVE    "既に入力済です"            TO  TERR-ERRMSG
               WHEN    "0008"
                   MOVE    "処理区分入力エラー"        TO  TERR-ERRMSG
               WHEN    "0009"
                   MOVE    "選択番号エラー"            TO  TERR-ERRMSG
               WHEN    "0011"
                   MOVE
                       "パラメタ区分が指定してないので入力できません"
                                                       TO  TERR-ERRMSG
               WHEN    "0012"
                   MOVE    "該当の保険者番号が存在しません"
                                                       TO  TERR-ERRMSG
               WHEN    "0013"
                   MOVE
                       "対象となる処理が選択してありません"
                                                       TO  TERR-ERRMSG
               WHEN    "0014"
                   MOVE
                       "パラメタの入力中です"          TO  TERR-ERRMSG
               WHEN    "0015" 
                   MOVE
                       "患者番号が未入力です"          TO  TERR-ERRMSG
               WHEN    "0016" 
                   MOVE
                       "保険者番号が未入力です"        TO  TERR-ERRMSG
               WHEN    "0018"
                   IF      WRK-CHK-CNT         =   1
                       STRING  WRK-CHK-SHORIMSG 
                                               DELIMITED   BY  SPACE
                               "の処理スクリプトが存在しません"
                                               DELIMITED   BY  SIZE
                               "、システム管理で確認して下さい"
                                               DELIMITED   BY  SIZE
                                               INTO    TERR-ERRMSG
                       END-STRING
                   ELSE
                       MOVE    WRK-CHK-CNT     TO  WRK-Z9
                       STRING  WRK-CHK-SHORIMSG
                                               DELIMITED   BY  SPACE
                               "を含む"        DELIMITED   BY  SIZE
                               WRK-Z9          DELIMITED   BY  SIZE
                               "個の処理スクリプトが存在しません"
                                               DELIMITED   BY  SIZE
                                               INTO    TERR-ERRMSG
                       END-STRING
                   END-IF
               WHEN    "1002"
                   MOVE    "パラメタＤＢの追加処理でエラーとなりました" 
                                                       TO  TERR-ERRMSG
               WHEN    "1003"
                   MOVE    "パラメタＤＢの更新処理でエラーとなりました" 
                                                       TO  TERR-ERRMSG
               WHEN    "1004"
                   MOVE    "対象のパラメタＤＢが存在しません" 
                                                       TO  TERR-ERRMSG
               WHEN    "1005"
                   MOVE    "パラメタＤＢの削除処理でエラーとなりました" 
                                                       TO  TERR-ERRMSG
               WHEN    "1006"
                   MOVE
                  "パラメタＤＢのＪＯＢＩＤ更新処理でエラーとなりました"
                                                       TO  TERR-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR                 TO  TERR-ERRMSG
           END-EVALUATE
      *
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "T02"                TO  SPA-MOTOPG
           MOVE    "TERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TERR"               TO  MCP-WINDOW
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
      *    エラーメッセージ表示理
      *****************************************************************
       520-TIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "6666"
                   MOVE    "入力途中です。処理を終了しますか"
                                            TO  WRK-TIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TID1
           INITIALIZE                       TID1
           MOVE    SPA-TIDCD            TO  TID1-ID1CODE
           MOVE    WRK-TIDMSG           TO  TID1-ID1MSG
           MOVE    "戻る"               TO  TID1-B01-LABEL
           MOVE    "ＯＫ"               TO  TID1-B12-LABEL
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "T02"                TO  SPA-MOTOPG
           MOVE    "TID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       530-TID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-TIDMSG
      *
           EVALUATE    SPA-TIDCD
               WHEN    "1001"
                   MOVE    "請求書の印刷処理を行います"
                                       TO  WRK-TIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  TID2
           INITIALIZE                       TID2
           MOVE    SPA-TIDCD            TO  TID2-ID1CODE
           MOVE    WRK-TIDMSG           TO  TID2-ID1MSG
           MOVE    "T"                  TO  TID2-B10-VISIBLE
           MOVE    "F10:プレビュー"     TO  TID2-B10-LABEL
           MOVE    "T"                  TO  TID2-B11-VISIBLE
           MOVE    "F11:印刷しない"     TO  TID2-B11-LABEL
           MOVE    "F12:印刷する"       TO  TID2-B12-LABEL
           MOVE    "black"              TO  TID2-ID1MSG-STYLE
           MOVE    "blue"               TO  TID2-LABEL-STYLE
      *
           IF      SPA-GMN-SRYYMKBN-MAX
                                       >   ZERO
               STRING   "　【"         DELIMITED  BY  SPACE
                        SPA-NAI-SRYYMKBN-ST-PRT(SPA-NAI-SRYYMKBN-SELNUM)
                                       DELIMITED  BY  SPACE
                        "〜"           DELIMITED  BY  SPACE
                        SPA-NAI-SRYYMKBN-ED-PRT(SPA-NAI-SRYYMKBN-SELNUM)
                                       DELIMITED  BY  SPACE
                        "】"           DELIMITED  BY  SPACE
                                       INTO       TID2-LABEL
               END-STRING
           END-IF
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "T02"                TO  SPA-MOTOPG
           MOVE    "TID2"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "TID2"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-TID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
      *    保険者マスター読込
      *****************************************************************
       900-HKNJA-READ-SEC         SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               ELSE
                   MOVE    1                   TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HKNJA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-BTPARA-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BTPARA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-N-SEC          SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  BTPARA-REC
               MOVE    ZERO                TO  FLG-BTPARA
           ELSE
               MOVE    1                   TO  FLG-BTPARA
               INITIALIZE                      BTPARA-REC
           END-IF
      *
           .
       900-BTPARA-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
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
      *    ジョブ管理ＤＢ制御処理
      *****************************************************************
       900-CALL-ORCSJOB-SEC            SECTION.
      *
grpsys     MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
           .
       900-CALL-ORCSJOB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "SELECT ERR table=" MCP-TABLE
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBFETCH-EXT.
           EXIT.
      *
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
      *
