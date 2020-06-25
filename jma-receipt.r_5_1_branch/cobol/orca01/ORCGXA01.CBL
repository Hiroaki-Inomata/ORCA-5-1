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
       PROGRAM-ID.                 ORCGXA01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 再印刷
      *  コンポーネント名  : 再印刷指定（ＸＡ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/09/12  ディレクトリをサブプロへ
      *  01.00.02    NACL-藤原    02/10/18  確定のとき自動的に情報確認画面を表示
      *  01.00.03    NACL-藤原    03/03/03  レセプト再印刷の追加
      *  01.00.04    NACL-藤原    03/04/12  印刷開始・終了頁の編集の修正
      *  01.00.05    NACL-藤原    03/05/19  未印刷のときのダイアログを止める
      *　プレビュー機能追加
      *  01.01.01    NACL-藤原    03/07/07  プレビュー機能追加
      *  01.01.02    NACL-太田    04/06/06  情報削除画面遷移追加
      *  02.07.00    NACL-竹田    05/10/24  MONFUNC 対応
      *
      *  04.05.01    NACL-藤原    10/04/30  削除後の画面表示の修正
      *
      *  04.06.01    NACL-藤原    10/08/18  一括入力対応
      *  04.06.02    NACL-竹田    10/11/02  クライアント印刷対応
      *
      *  05.00.01    NACL-藤原    19/08/28  ＨＡＯＲＩ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    再印刷処理共通パラメタ
           COPY    "XA01COMMON-SPA".
      *
      *    再印刷画面用ＳＰＡ
           COPY    "XA01SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "CMJOBNAME.INC".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-LOOPEND         PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MONPE           PIC 9(05).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  SYS-TIME            PIC X(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDL                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PRTKANRI-TBL-GROUP
                                   PIC X(14).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-RECEERR         PIC X(200).
           03  WRK-XAIDMSG         PIC X(70).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-PAGE            PIC ZZZZZZZZZ9.   
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-XA01-MOTOPG     PIC X(08).
           03  WRK-SELNUM          PIC 9(03). 
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-SHORI-RENNUM    PIC 9(04).
      *
           03  WRK-ZZ              PIC ZZ9.
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE-G.
               05  WRK-EDPAGE      PIC 9(05).
      *  
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-TITLE           PIC X(200).
      *
           03  WRK-SHELLTBL-ARG-X.
               05  WRK-SHELLTBL-ARG-9          PIC 9(10).
      *
           03  WRK-PARAM.
               05  WRK-PARAM-ORCA-UID          PIC X(36).
               05  WRK-PARAM-KARTE-UID         PIC X(36).
      *
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
           03  WRK-HENTIME.
               05  WRK-HH          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-MM          PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-SS          PIC X(02).
      *
       01  WRK-ROW.
           05  WRK-LIST-ROW        PIC S9(9)   BINARY.
      *                             
      *
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".   
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *              
      *
       01  WRK-CONS-AREA.
      *    システム管理ＤＢ（３００３）のキー値  
           03  WRK-CONS-3003-KBNCD PIC X(08)   VALUE   "001".
      *    印刷管理ＤＢのキー値  
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "REPRINT".
           03  WRK-CONS-PRTKANRI-TBL-GROUP
                                   PIC X(14)   VALUE   SPACE.
      *    ジョブ管理ＤＢのキー値  
           03  WRK-CONS-JOB-SHELLID
                                   PIC X(08)   VALUE   "REPRINT".
           03  WRK-CONS-JOB-JOBID  PIC 9(07)   VALUE   ZERO.
      *
      *
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.     
      *
      **** COPY    "XC01COMMON-SPA".
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1031.INC".
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
      *    印刷制御マスタ
       01  PRTCTRL-REC.
           COPY    "CPPRTCTRL.INC".
      *
           COPY    "MONBLOB".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *     COPY    "CPORCMCP.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
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
           COPY    "ORCA01SCRAREA.INC".
      *
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XA01
           MOVE    SPA-WORK        TO  SPA-XA01KYOUTU 
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
           MOVE    SPA-XA01KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XA01        TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                   SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "XAERR"
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
      *****MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "XA01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
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
           MOVE    SPACE           TO  WRK-MOTOPG
      *
           EVALUATE    SPA-MOTOPG
      *    作表処理
               WHEN    "XA99" 
                   PERFORM 340-XA99-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "XAID1"
                   PERFORM 330-XAID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
               WHEN    "XAID2"
                   PERFORM 330-XAID2-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               EVALUATE    WRK-MOTOPG
               WHEN    "XB01"
                   MOVE    SPA-WORK        TO   WRK-XA01-MOTOPG
                   INITIALIZE                   SPA-XA01-AREA
                                                SPA-XA01KYOUTU
                   MOVE    WRK-XA01-MOTOPG TO   SPA-XA01-MOTOPG
                   PERFORM 310-SPASET-SEC
                   GO  TO  300-SCREEN-EXT
      *
               WHEN    "XC01"
                   INITIALIZE              SPA-XA01-AREA
                                           SPA-XA01KYOUTU
                   MOVE    SPA-WORK        TO  LNK-XC01-AREA
                   MOVE    LNK-XC01-MOTOPG TO  SPA-XA01-MOTOPG
                   PERFORM 310-SPASET-SEC
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
      *
           INITIALIZE              SPA-XA01-AREA
                                   SPA-XA01KYOUTU
      *
           MOVE    WRK-MOTOPG      TO  SPA-XA01-MOTOPG
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
           EVALUATE    SPA-PRTKANRI-TBL-KEY
               WHEN    "SOKATU"
                   MOVE    "診療年月"     TO  SPA-GMN-TITLE1
                   MOVE    "請求年月日"   TO  SPA-GMN-TITLE2
               WHEN    "DAILY"
               WHEN    "MONTHLY"
                   MOVE    SPACE          TO  SPA-GMN-TITLE1
                   MOVE    "作成年月日"   TO  SPA-GMN-TITLE2
               WHEN    "RECEPT2"
                   MOVE    "診療年月"     TO  SPA-GMN-TITLE1
                   MOVE    "作成年月日"   TO  SPA-GMN-TITLE2
               WHEN    OTHER
                   MOVE    "年月"         TO  SPA-GMN-TITLE1
                   MOVE    "年月日"       TO  SPA-GMN-TITLE2
           END-EVALUATE
      *
           INITIALIZE                  PRTKANRI-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-PRTKANRI    =   1
                               OR      IDX             =   100
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN-TNO    (IDX)
               MOVE    PRTKANRI-TBL-KEY    TO
                                           SPA-NAI-TTBL-KEY   (IDX)
               MOVE    PRTKANRI-TBL-GROUP  TO
                                           SPA-NAI-TTBL-GROUP (IDX)
               MOVE    PRTKANRI-SRYYM      TO  SPA-NAI-TSRYYM (IDX)
               MOVE    PRTKANRI-SKYYMD     TO  SPA-NAI-TSKYYMD(IDX)
               MOVE    PRTKANRI-CREYMD     TO  SPA-NAI-TCREYMD(IDX)
               IF      PRTKANRI-TBL-GROUP
                                       NOT =   WRK-PRTKANRI-TBL-GROUP
                   MOVE    PRTKANRI-TBL-GROUP  TO
                                               SPA-GMN-TTBL-GROUP (IDX)
                                               WRK-PRTKANRI-TBL-GROUP
                   IF      PRTKANRI-SRYYM      NOT =   SPACE   AND ZERO
                       MOVE    PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
                       MOVE    "01"            TO  WRK-SYMD (7:2)
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG(1:6)
                                               TO  SPA-GMN-TSRYYM (IDX)
                   END-IF
                   IF      PRTKANRI-SKYYMD     NOT =   SPACE   AND ZERO
                       MOVE    PRTKANRI-SKYYMD TO  WRK-SYMD 
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG     TO  SPA-GMN-TSKYYMD(IDX)
                   END-IF
                   SET     TBL-IDX             TO  1
                   SEARCH      MNAME-TBL-OCC      VARYING   TBL-IDX
                       AT   END
                           MOVE    SPACE       TO
                                           SPA-GMN-TSHORI-TITLE (IDX)
                       WHEN    PRTKANRI-TBL-KEY    =   MNAME-SHELLID
                                                          (TBL-IDX)
                           MOVE    MNAME-JOBNAME (TBL-IDX)
                                                TO  SPA-GMN-TSHORI-TITLE
                                                           (IDX)
                   END-SEARCH
      *
      *                                                                                          
                   IF    ( SPA-XA01-MOTOPG     =   "X91" )
                    AND  ( SPA-PRT-GROUP   NOT =   SPACE )
                       IF    ( PRTKANRI-TBL-GROUP
                                               =   SPA-PRT-GROUP )
                           MOVE    PRTKANRI-TBL-GROUP
                                               TO  SPA-GMN-TBL-GROUP
                       END-IF
                   END-IF
      *
               END-IF                                 
               MOVE    PRTKANRI-RENNUM     TO  SPA-GMN-TRENNUM    (IDX)
               MOVE    PRTKANRI-SHORI-RENNUM
                                           TO  SPA-GMN-TSHORI-RENNUM
                                                                  (IDX)
               MOVE    PRTKANRI-PAGE       TO  SPA-GMN-TPAGE      (IDX)
               MOVE    PRTKANRI-TITLE      TO  SPA-GMN-TPRT-TITLE (IDX)
               MOVE    PRTKANRI-PRTFLG     TO  SPA-NAI-TPRTFLG    (IDX)
               IF      SPA-NAI-TPRTFLG (IDX)   =   SPACE
                   MOVE    "未"            TO  SPA-GMN-TPRTFLG (IDX)
               END-IF    
               IF    ( PRTKANRI-SHELLID    =   "API" OR "API_01" )
                   MOVE    "1"             TO  SPA-NAI-TAPI       (IDX)
               END-IF
      *                                                                                          
               PERFORM 900-PRTKANRI-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-PRTKANRI    =   ZERO
               MOVE    1               TO  SPA-NAI-NEXT
           END-IF
      *
           MOVE    IDX                 TO  SPA-GMN-MAX
      *                                 
           MOVE    1                   TO  FLG-LIST-ROW
      *
           IF    ( SPA-GMN-TBL-GROUP   =   SPACE )
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    99              TO  SPA-GMN-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-XAID1-SET-SEC            SECTION.
      *
           IF      SPA-XAID1-FLG    =   "OK"
               EVALUATE    SPA-XAIDCD
                   WHEN    "6666"
                       MOVE    "JOIN"          TO  MCP-PUTTYPE
                       PERFORM 210-BACK-SYORI-SEC
                   WHEN    "1002"
                   WHEN    "1003"
                       PERFORM 430-DELETE-SEC
                   WHEN    "1001"
      *************WHEN    "1004"
                       PERFORM 450-PRINT-SEC
               END-EVALUATE        
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           MOVE    SPACE           TO  SPA-XAID1-FLG
                                       SPA-XA99-FLG
           MOVE    SPACE           TO  SPA-XAIDCD
           MOVE    SPACE           TO  SPA-XAIDCD2      
           .
       330-XAID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面２ ＯＫ処理
      *****************************************************************
       330-XAID2-SET-SEC            SECTION.
      *
           IF      SPA-XAID1-FLG    NOT =   SPACE
               EVALUATE    SPA-XAIDCD2
                   WHEN    "1001"
      *************WHEN    "1004"
                   WHEN    "1005"
                       PERFORM 450-PRINT-SEC
               END-EVALUATE
           ELSE        
               MOVE    SPACE           TO  SPA-XAID1-FLG
           END-IF
      *
           MOVE    SPACE           TO  SPA-XA99-FLG
           MOVE    SPACE           TO  SPA-XAIDCD
           MOVE    SPACE           TO  SPA-XAIDCD2      
           .
       330-XAID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理表示画面戻り処理
      *****************************************************************
       340-XA99-SET-SEC            SECTION.
      *
           IF      SPA-XA99-FLG     =   "OK"
               PERFORM 4501-NEXT-GMN-SEC
           END-IF    
      *
           MOVE    SPACE           TO  SPA-XAID1-FLG
                                       SPA-XA99-FLG
           MOVE    SPACE           TO  SPA-XAIDCD
           MOVE    SPACE           TO  SPA-XAIDCD2      
           .
       340-XA99-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4501-NEXT-GMN-SEC            SECTION.       
      *
      *****MOVE    SPACE               TO  LINKAREA
      *****MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *****MOVE    SPA-XA01            TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "XA01"              TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    SPA-GMN-RENNUM      TO  LNK-XC01-RENNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  LNK-XC01-SHORI-RENNUM
           MOVE    SPA-GMN-STPAGE      TO  LNK-XC01-ST-PAGE
           MOVE    SPA-GMN-EDPAGE      TO  LNK-XC01-ED-PAGE
           MOVE    SPA-XA01-MOTOPG     TO  LNK-XC01-MOTOPG
           MOVE    SPACE               TO  LNK-XC01-MODE
      *
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4501-NEXT-GMN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *     終了
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE   "CHANGE"         TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *     クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *     削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-DELETE-GID-SEC
      *     情報削除
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-JYOUHOU-DEL-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-PRINT-GID-SEC
      *     処理確認
               WHEN    "CLICKED"       ALSO    "B13"
                   PERFORM 490-KAKUNIN-SEC
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
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 201-SELNUM-SEC
               WHEN    "ACTIVATE"      ALSO    "RENNUM"
                   PERFORM 203-RENNUM-SEC
               WHEN    "ACTIVATE"      ALSO    "SHORI_RENNUM"
                   PERFORM 204-SHORI-RENNUM-SEC
      *        行選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 203-SELECT-SEC
               WHEN    OTHER
                   MOVE    ZERO                TO  FLG-TOUROKU
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
           IF      XA01-RENNUM NOT =   SPA-GMN-RENNUM
               MOVE    "RENNUM"        TO  MCP-WIDGET
               PERFORM 203-RENNUM-SEC
           ELSE
               IF      XA01-SHORI-RENNUM
                                   NOT =   SPA-GMN-SHORI-RENNUM
                   MOVE    "SHORI_RENNUM"  TO  MCP-WIDGET
                   PERFORM 204-SHORI-RENNUM-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    入力箇所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
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
           IF    ( SPA-NAI-API     NOT =   "1" )
               MOVE    XA01-STPAGE         TO  SPA-GMN-STPAGE
               MOVE    XA01-EDPAGE         TO  SPA-GMN-EDPAGE
           END-IF
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
           PERFORM 205-STPAGE-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  4102-KIHON-CHK-EXT
           END-IF
           PERFORM 206-EDPAGE-SEC
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       201-SELNUM-SEC                 SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           INITIALIZE                  SPA-GMN-BODY
                                       SPA-NAI-BODY
      *      
           MOVE    XA01-SELNUM         TO  SPA-GMN-SELNUM
      *     
           IF      SPA-GMN-SELNUM      =   ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-SELNUM  >   SPA-GMN-MAX
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-SELNUM  TO  SPA-NAI-SELNUM     
                   PERFORM 2011-SELECT-HENSYU-SEC
               END-IF    
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF    
           .
       201-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       2011-SELECT-HENSYU-SEC        SECTION.
      *
           MOVE    SPA-GMN-SELNUM      TO  IDY
      *     
           MOVE    SPA-NAI-TTBL-GROUP  (IDY)   TO  SPA-GMN-TBL-GROUP
           MOVE    SPA-GMN-TRENNUM     (IDY)   TO  SPA-GMN-RENNUM
           MOVE    SPA-GMN-TSHORI-RENNUM (IDY) TO
                                               SPA-GMN-SHORI-RENNUM
           MOVE    1                           TO  SPA-GMN-STPAGE
           MOVE    SPA-GMN-TPAGE       (IDY)   TO  SPA-GMN-EDPAGE 
           MOVE    SPA-NAI-TAPI (IDY)          TO  SPA-NAI-API
           MOVE    SPA-NAI-TTBL-GROUP  (IDY)   TO  SPA-GMN-TBL-GROUP
           
           .
       2011-SELECT-HENSYU-EXT.
           EXIT.
           .
      *     
      *****************************************************************
      *    連番入力処理
      *****************************************************************
       203-RENNUM-SEC               SECTION.
      *
           MOVE    3                   TO  SPA-GMN-CUR
      *     
           MOVE    XA01-RENNUM         TO  SPA-GMN-RENNUM
      *     
           MOVE    ZERO                TO  SPA-GMN-STPAGE
                                           SPA-GMN-EDPAGE
                                           SPA-GMN-SELNUM
                                           SPA-NAI-SELNUM      
           IF      SPA-GMN-RENNUM      =   ZERO
               MOVE    ZERO                TO  SPA-GMN-SHORI-RENNUM
               MOVE    99                  TO  SPA-GMN-CUR
               GO  TO  203-RENNUM-EXT                                
           END-IF
      *     
           IF      SPA-GMN-TBL-GROUP   =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                       OR      SPA-GMN-TBL-GROUP
                                           >   SPA-NAI-TTBL-GROUP (IDX)
                   IF      SPA-GMN-TBL-GROUP
                                           =   SPA-NAI-TTBL-GROUP (IDX)
                   AND     SPA-GMN-RENNUM  =   SPA-GMN-TRENNUM    (IDX)
                       MOVE    IDX             TO  IDY
                       IF      SPA-GMN-SHORI-RENNUM
                                           NOT =   ZERO
                       AND     SPA-GMN-SHORI-RENNUM
                                       =   SPA-GMN-TSHORI-RENNUM(IDX)
                           MOVE    IDX             TO  SPA-GMN-SELNUM
                                                       SPA-NAI-SELNUM
                           MOVE    1               TO  SPA-GMN-STPAGE
                           MOVE    SPA-GMN-TPAGE (IDX)
                                                   TO  SPA-GMN-EDPAGE                                 
                           MOVE    SPA-GMN-MAX     TO  IDX
                       END-IF    
                   END-IF
               END-PERFORM
               IF      IDY                 =   ZERO
                   MOVE    "0006"              TO  SPA-ERRCD
               END-IF
           END-IF        
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
     *
           .
       203-RENNUM-EXT.
           EXIT.
      *     
      *****************************************************************
      *    処理内連番入力処理
      *****************************************************************
       204-SHORI-RENNUM-SEC               SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *     
           MOVE    XA01-SHORI-RENNUM   TO  SPA-GMN-SHORI-RENNUM
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
                                           SPA-NAI-SELNUM      
      *
           IF      SPA-GMN-SHORI-RENNUM    =   ZERO
               MOVE    ZERO                TO  SPA-GMN-STPAGE
                                               SPA-GMN-EDPAGE
               MOVE    99                  TO  SPA-GMN-CUR
               GO  TO  204-SHORI-RENNUM-EXT                                
           END-IF
      *     
           IF      SPA-GMN-TBL-GROUP   =   SPACE
           OR      SPA-GMN-RENNUM      =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                   IF      SPA-GMN-TBL-GROUP
                                       =   SPA-NAI-TTBL-GROUP (IDX)
                   AND     SPA-GMN-RENNUM
                                       =   SPA-GMN-TRENNUM    (IDX)
                   AND     SPA-GMN-SHORI-RENNUM
                                       =   SPA-GMN-TSHORI-RENNUM(IDX)
                       MOVE    IDX             TO  IDY
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-PERFORM
               IF      IDY                 =   ZERO
                   MOVE    "0006"              TO  SPA-ERRCD
               ELSE
                   MOVE    IDY                 TO  SPA-GMN-SELNUM
                                                   SPA-NAI-SELNUM
                   MOVE    1                   TO  SPA-GMN-STPAGE
                   MOVE    SPA-GMN-TPAGE (IDY) TO  SPA-GMN-EDPAGE                                 
               END-IF
           END-IF        
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    5                   TO  SPA-GMN-CUR
           ELSE    
               MOVE    ZERO                TO  SPA-GMN-STPAGE
                                               SPA-GMN-EDPAGE
           END-IF                                                  
      *
           .
       204-SHORI-RENNUM-EXT.
           EXIT.
      *     
      *****************************************************************
      *    開始頁入力処理
      *****************************************************************
       205-STPAGE-SEC               SECTION.
      *
           IF      SPA-GMN-STPAGE      >   ZERO
               IF      SPA-GMN-TBL-GROUP       =   SPACE
               OR      SPA-GMN-RENNUM          =   ZERO
               OR      SPA-GMN-SHORI-RENNUM    =   ZERO
               OR      SPA-GMN-SELNUM          =   ZERO
                   IF      SPA-GMN-SELNUM          =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                   END-IF
               ELSE    
                   IF    ( SPA-GMN-STPAGE      >
                                       SPA-GMN-TPAGE (SPA-GMN-SELNUM) )
                       MOVE    "0002"              TO  SPA-ERRCD
                   END-IF                                    
               END-IF
           ELSE
               IF      SPA-GMN-TBL-GROUP   NOT =   SPACE
               AND     SPA-GMN-RENNUM          >   ZERO
               AND     SPA-GMN-SHORI-RENNUM    >   ZERO
               AND     SPA-GMN-SELNUM          >   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF                                                  
      *
           IF      SPA-ERRCD       NOT  =   SPACE
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF    
      *
           .
       205-STPAGE-EXT.
           EXIT.
      *     
      *****************************************************************
      *    終了頁入力処理
      *****************************************************************
       206-EDPAGE-SEC               SECTION.
      *
           IF      SPA-GMN-EDPAGE      >   ZERO
               IF      SPA-GMN-TBL-GROUP       =   SPACE
               OR      SPA-GMN-RENNUM          =   ZERO
               OR      SPA-GMN-SHORI-RENNUM    =   ZERO
               OR      SPA-GMN-SELNUM          =   ZERO
                   IF      SPA-GMN-SELNUM          =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                   END-IF
               ELSE    
                   IF      SPA-GMN-EDPAGE      >
                                       SPA-GMN-TPAGE (SPA-GMN-SELNUM) 
                       MOVE    "0002"              TO  SPA-ERRCD
                   ELSE    
                       IF      SPA-GMN-EDPAGE      <   SPA-GMN-STPAGE 
                           MOVE    "0002"              TO  SPA-ERRCD
                       END-IF
                   END-IF                                        
               END-IF
           ELSE
               IF      SPA-GMN-TBL-GROUP   NOT =   SPACE
               AND     SPA-GMN-RENNUM          >   ZERO
               AND     SPA-GMN-SHORI-RENNUM    >   ZERO
               AND     SPA-GMN-SELNUM          >   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
               END-IF
           END-IF                                                  
      *
           IF      SPA-ERRCD       NOT  =   SPACE
               MOVE    6                    TO  SPA-GMN-CUR
           END-IF    
      *
           .
       206-EDPAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       203-SELECT-SEC               SECTION.
      *          
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF      XA01-SELECT (IDX)   =   "T"
                   IF      IDX             =   SPA-NAI-SELNUM
                       CONTINUE
                   ELSE        
                       MOVE    IDX             TO  WRK-SELNUM
                       MOVE    SPA-GMN-MAX     TO  IDX
                   END-IF
               END-IF    
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           INITIALIZE                  SPA-GMN-BODY
      *
           MOVE    SPA-NAI-SELNUM      TO  SPA-GMN-SELNUM
      *                                 
           PERFORM 2011-SELECT-HENSYU-SEC
      *
           MOVE    99                  TO  SPA-GMN-CUR         
           .
       203-SELECT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
           IF      SPA-GMN-TBL-GROUP   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               GO  TO  4103-KAKUTEI-CHK-EXT
           END-IF                                
      *
      *    印刷時のみチェック
           IF     MCP-WIDGET           =   "B12"
               IF      SPA-GMN-SELNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      SPA-GMN-STPAGE      >
                                   SPA-GMN-TPAGE (SPA-GMN-SELNUM)
                   OR      SPA-GMN-EDPAGE      >
                                   SPA-GMN-TPAGE (SPA-GMN-SELNUM)
                       MOVE    "0002"              TO  SPA-ERRCD
                   ELSE
                       IF      SPA-GMN-EDPAGE      <   SPA-GMN-STPAGE
                           MOVE    "0002"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK                    SECTION.
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN
                                           =   ZERO
                       IF      JOB-ENDYMD      =   SPACE
      *********        AND     JOB-TERMID      =   SPA-TERMID
                           MOVE    "6666"          TO  SPA-XAIDCD
                           GO  TO  210-BACK-EXT
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 210-BACK-SYORI-SEC         
           .
       210-BACK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC              SECTION.
      *
           MOVE    SPA-XA01-MOTOPG     TO  SPA-SAKIPG
           MOVE    "XA01"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                  SPA-KYOTUKEY
                                       SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                SECTION.
      *
           INITIALIZE               SPA-GMN-BODY
                                    SPA-NAI-SELNUM
      *
           MOVE    1                TO  SPA-GMN-CUR        
      *
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  430-DELETE-GID-EXT               
           END-IF
      *         
      *    帳票管理ＤＢのチェック
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-GMN-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET2-SEC
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *     
      *    指定のグループが存在しないとき
           IF      FLG-PRTKANRI        =   1
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    PRTKANRI-TBL-KEY    TO  SPA-WRK-TBL-KEY    
      *        ガイドメッセージセット
               MOVE    "1002"              TO  SPA-XAIDCD
      *        未処理の帳票があるとき
               PERFORM                     UNTIL   FLG-PRTKANRI    =   1
                   IF      PRTKANRI-PRTFLG         =   SPACE
                       MOVE    "1003"          TO  SPA-XAIDCD
                       MOVE    1               TO  FLG-PRTKANRI
                   END-IF
                   PERFORM 900-PRTKANRI-FET2-SEC
               END-PERFORM
           END-IF
      *                  
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       430-DELETE-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       430-DELETE-SEC                SECTION.
      *         
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPA-WRK-TBL-KEY     TO  PRTKANRI-TBL-KEY
           MOVE    SPA-GMN-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-WRK-TBL-KEY     TO  PRTDATA-TBL-KEY
           MOVE    SPA-GMN-TBL-GROUP   TO  PRTDATA-TBL-GROUP
           MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "delmb1"            TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           INITIALIZE              SPA-XA01-AREA
           PERFORM 310-SPASET-SEC
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *    MOVE    SPACE               TO  LINKAREA
      *    MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *    MOVE    SPA-XA01            TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "XA01"              TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-XA01-MOTOPG     TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    作表処理
      *****************************************************************
       450-PRINT-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-PRINT-GID-EXT
           END-IF
     *
      *    ジョブ管理チェック        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
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
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  450-PRINT-GID-EXT               
           END-IF
      *         
      *    帳票管理ＤＢのチェック
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-GMN-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET2-SEC
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *     
      *    指定のグループが存在しないとき
           IF      FLG-PRTKANRI        =   1
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    PRTKANRI-TBL-KEY    TO  SPA-WRK-TBL-KEY    
      *     
      *        ガイドメッセージセット
               MOVE    "1001"       TO  SPA-XAIDCD2
      *
      *        未処理の帳票があるとき
      *****    PERFORM                     UNTIL   FLG-PRTKANRI    =   1
      *            IF      PRTKANRI-PRTFLG         =   SPACE
      *                MOVE    "1004"          TO  SPA-XAIDCD2
      *                MOVE    1               TO  FLG-PRTKANRI
      *            END-IF
      *            PERFORM 900-PRTKANRI-FET2-SEC
      *****    END-PERFORM                   
           END-IF
      *                 
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       450-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       450-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      SPA-XA01-TBL-GROUP
                                           SPA-XA01-JOB-JOBID
      * 
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
               MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE                                 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  450-PRINT-EXT
               END-IF
           END-IF                 
      * 
      *    ジョブ管理削除処理
           MOVE    "ALL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING   ORCSJOBKANRIAREA  
                                           JOBKANRI-REC
                                           SPA-AREA
           IF      SJOBKANRI-RETURN    =   ZERO
               CONTINUE
           ELSE
               MOVE    "1004"              TO  SPA-ERRCD
               GO  TO  450-PRINT-EXT
           END-IF             
      *
      *    monblobapi処理
           IF      SPA-XA01-JOB-PARAM
                               NOT =   SPACE
      *        Ｄａｔａ＿Ｉｄ取得
               MOVE    "MONBLOBNEW"        TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "monblob"           TO  MCP-TABLE
               CALL    "MONFUNC"           USING
                                               MCPAREA
                                               MONBLOB
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "E88"               TO  SPA-ERRCD
                   GO  TO  450-PRINT-EXT
               END-IF
               MOVE    MONBLOB-ID          TO
                                       SPA-XA01-MONBLOB-PRINT-ID (1)
           END-IF
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
      *     
           MOVE    SPA-HOSPNUM         TO  RECEERR-HOSPNUM
           MOVE    "OR001ERR"          TO  RECEERR-FILE-ID
           MOVE    SPA-TERMID          TO  RECEERR-TERMID
           MOVE    1                   TO  RECEERR-SYOKBN1
      *
      *    印刷処理
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *                                   
      *****MOVE    RECEERR             TO  SHELL-ERRFILE
      *
           MOVE    SPA-GMN-STPAGE      TO  WRK-STPAGE
           IF      SPA-GMN-EDPAGE      =   ZERO
               MOVE    ALL "9"             TO  WRK-EDPAGE-G
           ELSE
               MOVE    SPA-GMN-EDPAGE      TO  WRK-EDPAGE
           END-IF                            
      *
           MOVE    SPA-GMN-TBL-GROUP   TO  SPA-XA01-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-XA01-JOB-JOBID
      *
      *             
           MOVE    SPACE               TO  SHELLTBL
      *****MOVE    "/usr/local/orca/scripts/allways/print.sh"
      *****                            TO  SHELLTBL-NAME
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *     
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    SPA-WRK-TBL-KEY     TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    SPA-GMN-TBL-GROUP   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  LNK-PRTKANRI-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "print_prv.sh"      TO  LNK-PRTKANRI-SHELLID
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1       
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2     
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3       
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4       
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5       
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6       
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7       
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8      
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9       
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10      
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11      
      *                 
      *                 
           MOVE    RECEERR-BASENAME    TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
           EVALUATE    SPA-XAID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "CP"
                   MOVE    "5"             TO  SHELLTBL-ARG17
               WHEN    "NO"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "OK"
                 IF    SPA-CLIENT-PRT-FLG  =  2
                   MOVE    "5"             TO  SHELLTBL-ARG17
                 ELSE
                   MOVE    "2"             TO  SHELLTBL-ARG17
                 END-IF
           END-EVALUATE
           IF      SPA-XA01-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "7"                 TO  SHELLTBL-ARG17
           END-IF
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    SPA-CLIENT-SHOWDIALOG
                                       TO  SHELLTBL-ARG22
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    monblobapi処理
           IF      SPA-XA01-JOB-PARAM
                               NOT =   SPACE
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 45013-MONBLOBAPI-SEC
               END-IF
           END-IF
      *
      *    ＪＯＢＩＤ更新処理
      *
           PERFORM 45012-JOBID-UPD-SEC 
      *     
      *    情報確認画面を表示                                                   
           PERFORM 490-KAKUNIN-SEC
           .
       450-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ジョブ開始セット処理
      *****************************************************************
       45011-JOBKANRI-HENSYU-SEC       SECTION.
      *
           ADD     1               TO  WRK-CONS-JOB-JOBID
      *       
           MOVE    "DEL"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           MOVE    "JBS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    WRK-CONS-JOB-JOBID
                                   TO  JOB-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                   TO  JOB-SHELLID
           MOVE    SPA-TERMID      TO  JOB-TERMID                        
           MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
           MOVE    SPA-XA01-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-XA01-JOB-ORCA-UID
                                   TO  JOB-ORCA-UID
           CALL    "ORCSJOB"       USING
                                   ORCSJOBKANRIAREA 
                                   JOBKANRI-REC
                                   SPA-AREA
      *
           .
       45011-JOBKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＪＯＢＩＤ更新処理
      *****************************************************************
       45012-JOBID-UPD-SEC    SECTION.
      *
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
      *       
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    ZERO                TO  SYS-3003-STYUKYMD
           MOVE    ALL "9"             TO  SYS-3003-EDYUKYMD
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SYS-3003-TBL-KEY
           MOVE    WRK-CONS-PRTKANRI-TBL-GROUP
                                       TO  SYS-3003-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SYS-3003-JOBID                                      
      *
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           IF      FLG-SYSKANRI        =   ZERO     
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           ELSE
               MOVE    "DBINSERT"          TO  MCP-FUNC
           END-IF         
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "SYSKANRI 3003  ERR KEY = ["
                                  MCP-RC  " " SYS-3003-KEY  "]"   
      *****    MOVE    "1010"          TO  SPA-ERRCD
           END-IF
      *
           .
       45012-JOBID-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    MONBLOBAPI処理
      *****************************************************************
       45013-MONBLOBAPI-SEC        SECTION.
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "haori_monblobapi.sh"
                                       TO  SHELLTBL-NAME
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-HOSPNUM
           MOVE    WRK-CONS-JOB-JOBID
                                       TO  SHELLTBL-JOBID
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-SHELLID
      *
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    SPA-WRK-TBL-KEY     TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    SPA-XA01-TBL-GROUP  TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    ZERO                TO  LNK-PRTKANRI-RENNUM
           MOVE    ZERO                TO  LNK-PRTKANRI-SHORI-RENNUM
           MOVE    "000000"            TO  LNK-PRTKANRI-SRYYM
           MOVE    SPA-SYSYMD          TO  LNK-PRTKANRI-SKYYMD
           MOVE    "haori_monblobapi.sh"
                                       TO  LNK-PRTKANRI-SHELLID
           MOVE    ZERO                TO  LNK-PRTKANRI-PRIORITY
           MOVE    SPA-TERMID          TO  LNK-PRTKANRI-TERMID
           MOVE    SPA-OPID            TO  LNK-PRTKANRI-OPID
      *
           MOVE    LNK-PRTKANRI-RENNUM TO  SHELLTBL-ARG1
           MOVE    LNK-PRTKANRI-TBL-KEY
                                       TO  SHELLTBL-ARG2
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                       TO  SHELLTBL-ARG3
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                       TO  SHELLTBL-ARG4
           MOVE    LNK-PRTKANRI-SRYYM  TO  SHELLTBL-ARG5
           MOVE    LNK-PRTKANRI-SKYYMD TO  SHELLTBL-ARG6
           MOVE    LNK-PRTKANRI-SHELLID
                                       TO  SHELLTBL-ARG7
           MOVE    LNK-PRTKANRI-PRIORITY
                                       TO  SHELLTBL-ARG8
           MOVE    LNK-PRTKANRI-TERMID TO  SHELLTBL-ARG9
           MOVE    LNK-PRTKANRI-OPID   TO  SHELLTBL-ARG10
           MOVE    LNK-PRTKANRI-PRTNM  TO  SHELLTBL-ARG11
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG13
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG14
           MOVE    RECEERR-BASENAME    TO  SHELLTBL-ARG15
      *
           MOVE    SPA-XA01-MONBLOB-PRINT-ID (1)
                                       TO  SHELLTBL-ARG16
           MOVE    SPA-XA01-PUSHBA01-EVENT
                                       TO  SHELLTBL-ARG17
           MOVE    SPA-XA01-JOB-JOBID  TO  WRK-SHELLTBL-ARG-9
           MOVE    WRK-SHELLTBL-ARG-X  TO  SHELLTBL-ARG18
      *
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       45013-MONBLOBAPI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理確認処理
      *****************************************************************
       490-KAKUNIN-SEC            SECTION.
      *
           MOVE    SPACE               TO  XA99
           INITIALIZE                      XA99
           MOVE    ZERO                TO  XA99-COUNT
           MOVE    WIDGET-INSENSITIVE  TO  XA99-B12-STATE
      *        
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
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
                   MOVE    SPA-HOSPNUM     TO  JOB-HOSPNUM
                   CALL    "ORCSJOB"       USING
                                           ORCSJOBKANRIAREA  
                                           JOBKANRI-REC 
                                           SPA-AREA
                   IF      SJOBKANRI-RETURN    =   ZERO
                       MOVE    JOB-JOBID       TO  XA99-RENNUM   (IDX)
                       MOVE    JOB-SHELLMSG    TO  XA99-SHORINM  (IDX)
                       MOVE    JOB-STARTTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-STARTTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-STARTTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  XA99-STARTTIME(IDX)
                       MOVE    JOB-ENDTIME(1:2)
                                               TO  WRK-HH
                       MOVE    JOB-ENDTIME(3:2)
                                               TO  WRK-MM
                       MOVE    JOB-ENDTIME(5:2)
                                               TO  WRK-SS
                       MOVE    WRK-HENTIME     TO  XA99-ENDTIME  (IDX)
                       IF    ( JOB-UPDCNT  NOT =   ZERO )
                           MOVE    JOB-UPDCNT      TO  WRK-PAGE
                           MOVE    WRK-PAGE (5:6)  TO  XA99-PAGE (IDX)
                       END-IF
                       MOVE    JOB-YOBI        TO  XA99-ERRMSG   (IDX)
                       IF      JOB-ENDYMD      =   SPACE
                           MOVE    "処理中です"    TO  XA99-MSG
                       ELSE
                           IF      JOB-ERRCD       =   SPACE
                               CONTINUE
                           ELSE
                               STRING  JOB-YOBI    DELIMITED  BY  SPACE
                                       "【"        DELIMITED  BY  SIZE
                                      JOB-SHELLMSG DELIMITED  BY  SPACE
                                      "】"         DELIMITED  BY  SIZE      
                                                   INTO  XA99-MSG
                               END-STRING
                           END-IF                          
                       END-IF    
                       MOVE    IDX                     TO  XA99-COUNT    
      *
                   END-IF
               END-PERFORM
           END-IF
           MOVE    "B01"                TO  MCP-WIDGET
           IF      XA99-MSG              =   SPACE
               IF      XA99-COUNT            >   ZERO    
                   MOVE    "処理は正常に終了しました"
                                                TO  XA99-MSG
                   IF      SPA-XAID1-FLG    =   "PV"
                       MOVE    WIDGET-NORMAL    TO  XA99-B12-STATE
                       MOVE    "B12"            TO  MCP-WIDGET
                   ELSE
                       MOVE    "B01"            TO  MCP-WIDGET
                   END-IF
               ELSE
                   MOVE    "該当する処理はありません"
                                                TO  XA99-MSG
               END-IF                                 
           END-IF                                 
      *
           MOVE    "XA01"                TO  SPA-MOTOPG
           MOVE    "XA99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
      *
           MOVE    "XA99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       490-KAKUNIN-EXT.
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
           IF      SPA-XAIDCD       NOT =   SPACE
               PERFORM 520-TIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-XAIDCD2      NOT =   SPACE
               PERFORM 530-XAID2SET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XA01    "          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面内容編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    XA01-ROW            TO  WRK-LIST-ROW
      *
           INITIALIZE  XA01
      *
           EVALUATE    FLG-LIST-ROW
           WHEN    1
               MOVE    ZERO            TO  XA01-ROW
               MOVE    ZERO            TO  XA01-ROWATTR
           WHEN    OTHER
               MOVE    WRK-LIST-ROW    TO  XA01-ROW
               MOVE    ZERO            TO  XA01-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-TITLE1      TO  XA01-HEAD1
           MOVE    SPA-GMN-TITLE2      TO  XA01-HEAD2
      *     
           MOVE    SPA-GMN-SELNUM      TO  XA01-SELNUM
           MOVE    SPA-GMN-TBL-GROUP   TO  XA01-TBL-GROUP
           MOVE    SPA-GMN-RENNUM      TO  XA01-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM 
                                       TO  XA01-SHORI-RENNUM
           IF    ( SPA-NAI-API     NOT =   "1" )
               MOVE    SPA-GMN-STPAGE  TO  XA01-STPAGE
               MOVE    SPA-GMN-EDPAGE  TO  XA01-EDPAGE                            
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  XA01-TNO       (IDX)
               MOVE    SPA-GMN-TTBL-GROUP(IDX) TO  XA01-TTBL-GROUP(IDX)
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  XA01-TRENNUM   (IDX)
               MOVE    SPA-GMN-TSHORI-RENNUM(IDX)
                                               TO
                                               XA01-TSHORI-RENNUM (IDX)
               IF    ( SPA-NAI-TAPI (IDX)   NOT =   "1" )
                   MOVE    SPA-GMN-TPAGE     (IDX) TO  WRK-PAGE
                   MOVE    WRK-PAGE(6:5)       TO  XA01-TPAGE     (IDX)
               END-IF
               MOVE    SPA-GMN-TSRYYM    (IDX) TO  XA01-TSRYYM    (IDX)
               MOVE    SPA-GMN-TSKYYMD   (IDX) TO  XA01-TSKYYMD   (IDX)
               MOVE    SPA-GMN-TSHORI-TITLE (IDX)  TO
                                               XA01-TSHORI-TITLE  (IDX)
               MOVE    SPA-GMN-TPRT-TITLE(IDX) TO  XA01-TPRT-TITLE(IDX)
               MOVE    SPA-GMN-TPRTFLG   (IDX) TO  XA01-TPRTFLG   (IDX)
      *       
               IF      IDX             =   SPA-NAI-SELNUM
                   MOVE    "T"             TO  XA01-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  XA01-SELECT (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-MAX         TO  XA01-COUNT
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
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    1
                   MOVE    "SELNUM"       TO  MCP-WIDGET
               WHEN    3
                   MOVE    "RENNUM"       TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SHORI_RENNUM" TO  MCP-WIDGET
               WHEN    5
                   MOVE    "STPAGE"       TO  MCP-WIDGET
               WHEN    6
                   MOVE    "EDPAGE"       TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"          TO  MCP-WIDGET
           END-EVALUATE
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
               WHEN    "STPAGE"
                   MOVE    6          TO  SPA-GMN-CUR
               WHEN    "EDPAGE"
                   MOVE    99         TO  SPA-GMN-CUR
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の選択番号はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "未入力の項目があります" 
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "頁の入力が違います" 
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "該当の帳票はありません" 
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "帳票管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
               WHEN    "1003"
               WHEN    "1006"
                   MOVE    "帳票ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "ジョブ管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE
                   "システム管理ＤＢ（３００３）が削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "7777"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-RECEERR         TO  SPA-ERRMSG
     *
               WHEN    "E88"
                   MOVE    "Ｄａｔａ＿Ｉｄが取得できません。"
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XAERR
           MOVE    SPA-ERRCD            TO  XAERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  XAERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "XA01"                TO  SPA-MOTOPG
           MOVE    "XAERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                 TO  MCP-PUTTYPE
           MOVE    "XAERR"               TO  MCP-WINDOW
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
           MOVE    SPACE               TO  WRK-XAIDMSG
      *
           EVALUATE    SPA-XAIDCD
               WHEN    "1002"
                   MOVE    "該当のグループの削除処理を行います"
                                       TO  WRK-XAIDMSG
               WHEN    "1003"
                   MOVE
                   "処理中の可能性がありますが削除処理を行いますか"
                                       TO  WRK-XAIDMSG
               WHEN    "1005"
                   MOVE    "プレビュー処理を行います"
                                       TO  WRK-XAIDMSG
               WHEN    "6666"
                   MOVE    "印刷処理中です"
                                            TO  WRK-XAIDMSG
               WHEN    "8888"
                   MOVE    "処理が正常終了しました"
                                            TO  WRK-XAIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XAID1
           INITIALIZE                       XAID1
           MOVE    SPA-XAIDCD           TO  XAID1-ID1CODE
           MOVE    WRK-XAIDMSG          TO  XAID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "XA01"               TO  SPA-MOTOPG
           MOVE    "XAID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XAID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-TIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ガイドメッセージ３表示処理
      *****************************************************************
       530-XAID2SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-XAIDMSG
      *
           EVALUATE    SPA-XAIDCD2
               WHEN    "1001"
                   MOVE    "印刷処理を行います"
                                       TO  WRK-XAIDMSG
      *********WHEN    "1004"
      **           MOVE
      **           "処理中の可能性がありますが印刷処理を行いますか"
      *********                        TO  WRK-XAIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  XAID2
           INITIALIZE                       XAID2
      *
           MOVE    ZERO                 TO  CNT-MONPE
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL ( IDX     >   SPA-GMN-MAX )
      *
               MOVE    ZERO            TO  WRK-RENNUM
                                           WRK-SHORI-RENNUM
               IF    ( SPA-GMN-RENNUM  NOT =   ZERO )
                   MOVE    SPA-GMN-TRENNUM (IDX)
                                       TO  WRK-RENNUM
               END-IF
      *
               IF    ( SPA-GMN-SHORI-RENNUM
                                       NOT =   ZERO )
                   MOVE    SPA-GMN-TSHORI-RENNUM (IDX)
                                       TO  WRK-SHORI-RENNUM
               END-IF
      *
               IF    ( SPA-NAI-TTBL-GROUP (IDX)
                                       =   SPA-GMN-TBL-GROUP )
                 AND ( WRK-RENNUM  =   SPA-GMN-RENNUM )
                 AND ( WRK-SHORI-RENNUM
                                       =   SPA-GMN-SHORI-RENNUM )
                 AND ( SPA-NAI-TAPI (IDX)    NOT =   "1" )
                    COMPUTE CNT-MONPE = CNT-MONPE   +   1
               END-IF
           END-PERFORM
      *
           IF    ( CNT-MONPE        >   ZERO )
               MOVE    "T"          TO  XAID2-B10-VISIBLE
           ELSE
               MOVE    "F"          TO  XAID2-B10-VISIBLE
           END-IF
      *
           MOVE    SPA-XAIDCD2          TO  XAID2-ID1CODE
           MOVE    WRK-XAIDMSG          TO  XAID2-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "XA01"               TO  SPA-MOTOPG
           MOVE    "XAID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "XAID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       530-XAID2SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み（ｋｅｙ２）
      *****************************************************************
       900-PRTKANRI-FET2-SEC            SECTION.
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               IF      SPA-GMN-RENNUM      =   ZERO
                   CONTINUE
               ELSE
                   IF      SPA-GMN-RENNUM   NOT = 
                                           PRTKANRI-RENNUM
                       GO  TO  900-PRTKANRI-FET2-SEC
                   END-IF
               END-IF                                             
               IF      SPA-GMN-SHORI-RENNUM   =   ZERO
                   CONTINUE
               ELSE
                   IF      SPA-GMN-SHORI-RENNUM   NOT = 
                                           PRTKANRI-SHORI-RENNUM
                       GO  TO  900-PRTKANRI-FET2-SEC
                   END-IF
               END-IF                                             
               MOVE    ZERO                TO  FLG-PRTKANRI
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
           .
       900-PRTKANRI-FET2-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷管理ＤＢ読み込み
      *****************************************************************
       900-PRTKANRI-FET-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-PRTKANRI    NOT =   ZERO )
                    OR   ( FLG-LOOPEND     NOT =   ZERO )
      *
               MOVE    "tbl_prtkanri"      TO  MCP-TABLE
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PRTKANRI-REC
                   MOVE    ZERO            TO  FLG-PRTKANRI
      *
                   MOVE    1               TO  FLG-LOOPEND
      *
                   IF    ( SPA-XA01-MOTOPG     =   "X91" )
                    AND  ( SPA-PRT-GROUP   NOT =   SPACE )
                       IF    ( PRTKANRI-TBL-GROUP
                                               =   SPA-PRT-GROUP )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-PRTKANRI
               END-IF
      *
           END-PERFORM
      *
           .
       900-PRTKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理マスター読込（キー）
      *****************************************************************
       900-SYSKANRI-INV-SEC         SECTION.
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
       900-SYSKANRI-INV-EXT.
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
      *    ＤＢＦＥＴＣＨ処理
      *****************************************************************
       900-DBINSERT-SEC               SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBINSERT-EXT.
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
