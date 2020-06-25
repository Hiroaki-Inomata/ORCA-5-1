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
       PROGRAM-ID.                 ORCGXA02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 再印刷(オンライン帳票)
      *  コンポーネント名  : 再印刷指定（ＸＡ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/07/00    NACL−竹田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-藤原    11/06/27  ２頁以降の頁チェックの修正
      *
      *  04.06.01    NACL-藤原    10/08/18  一括入力対応
      *
      *  05.00.01    NACL-藤原    10/08/18  帳票削除対応
      *  05.00.02    NACL-藤原    19/08/28  ＨＡＯＲＩ対応
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
                                   BY         //XA02ERR//.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    再印刷処理共通パラメタ
           COPY    "XA02COMMON-SPA".
      *
           COPY    "XA02SCR-SPA".
      *
           COPY    "ENUM-VALUE".
      *
           COPY    "CMJOBNAME.INC".
      *
           COPY    "CPLOCKCONST.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-XA02ERR         PIC X(02).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PRTKANRI        PIC 9(05).
           03  CNT-MONPE           PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PRTKANRI        PIC 9(01).
           03  FLG-PRTKANRI2       PIC 9(01).
           03  FLG-PRTDATA         PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-LIST-ROW        PIC 9(01).
           03  FLG-LDMAP           PIC 9(01).
           03  FLG-READ-SKIP       PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PRTKANRI-TBL-GROUP
                                   PIC X(14).
           03  WRK-PRTKANRI-RENNUM PIC 9(04).
           03  WRK-XA02ERR         PIC X(200).
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
           03  WRK-XA02-MOTOPG     PIC X(08).
           03  WRK-SELNUM          PIC 9(03). 
      *
           03  WRK-ZZ              PIC ZZ9.
           03  WRK-YMD             PIC X(10).
           03  WRK-ZOGENPTN        PIC X(01).
           03  WRK-ZOGEN           PIC S9(05).
           03  WRK-BANGO           PIC S9(05).
           03  WRK-CREYMD          PIC X(08).
      *
           03  WRK-STR             PIC 9(05).
      *  
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *  
           03  WRK-STPAGE          PIC 9(05).
           03  WRK-EDPAGE-G.
               05  WRK-EDPAGE      PIC 9(05).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(03).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-SHORI-RENNUM    PIC 9(04).
      *
           03  WRK-SHELLTBL-ARG-X.
               05  WRK-SHELLTBL-ARG-9          PIC 9(10).
      *
           03  WRK-PARAM.
               05  WRK-PARAM-ORCA-UID          PIC X(36).
               05  WRK-PARAM-KARTE-UID         PIC X(36).
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
           COPY    "CPSHELLTBL.INC".
      *
       01  WRK-LNK-AREA.
           COPY    "CPORCSPRTLNK.INC".   
      *     
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
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
           03  WRK-CONS-LINE-MAX   PIC 9(03)   VALUE   100.
      *
           03  WRK-CONS-TBL-MAX    PIC 9(03)   VALUE   300.
      *    一括削除の印刷管理ＤＢのキー値  
           03  WRK-CONS-DEL-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "ONLINE".
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
           COPY    "CPPRVKANRI.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    LDMAP情報（詳細）
           COPY  "CPLDMAP.INC".
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-XA02FREE
           MOVE    SPA-WORK        TO  SPA-XA02KYOUTU 
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
           MOVE    SPA-XA02KYOUTU  TO  SPA-WORK  
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-XA02FREE    TO  SPA-FREE
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
           MOVE    "XA02"              TO  MCP-WINDOW
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
               WHEN    "XA03"
                   PERFORM 330-XA03-SET-SEC
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
                   INITIALIZE                   SPA-XA02KYOUTU
                   MOVE    WRK-XA02-MOTOPG TO   SPA-XA02-MOTOPG
                   MOVE    LNK-SCRSPA      TO   SPA-XA02FREE
                   MOVE    SPA-NAI-SELNUM  TO  XA02-ROW
                   GO  TO  300-SCREEN-EXT
      *
               WHEN    "XC01"
                   INITIALIZE                  SPA-XA02KYOUTU
                   MOVE    WRK-XA02-MOTOPG TO  SPA-XA02-MOTOPG
                   MOVE    LNK-SCRSPA      TO  SPA-XA02FREE
                   MOVE    SPA-NAI-SELNUM  TO  XA02-ROW
                   GO  TO  300-SCREEN-EXT
               END-EVALUATE
           END-IF
      *
      *
           INITIALIZE              SPA-XA02-AREA
                                   SPA-XA02KYOUTU
           PERFORM 310-SPASET-SEC
      *
           MOVE    WRK-MOTOPG              TO  SPA-XA02-MOTOPG
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
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
          MOVE    SMCNDATE-YMD            TO  SPA-NAI-SMCNDATE-YMD
          MOVE    SMCNDATE-YMD            TO  WRK-SYMD 
          PERFORM 5002-HIZUKE-HEN-SEC
          MOVE    WRK-HENYMDG             TO  SPA-GMN-SMCNDATE-YMD
      *
          MOVE    SPA-NAI-SMCNDATE-YMD    TO  SPA-NAI-CREYMD
                                              SPA-NAI-CREYMD-OLD
          MOVE    SPA-GMN-SMCNDATE-YMD    TO  SPA-GMN-CREYMD
      *
           MOVE    1                      TO  SPA-GMN-PAGE
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
                   MOVE    "業務名"       TO  SPA-GMN-TITLE1
                   MOVE    "患者番号"     TO  SPA-GMN-TITLE2
           END-EVALUATE
      *
      *                                 
           MOVE    1                   TO  FLG-LIST-ROW
      *                                 
           MOVE    ZERO                TO  SPA-GMN-CUR
      *                                 
           MOVE   "00 全業務"          TO  SPA-GMN-GYOMU-G
                                           SPA-GMN-GYOMULST (1)
           MOVE    1                   TO  SPA-GMN-GYOMULST-MAX
      *
grpsys     MOVE    SPA-HOSPNUM         TO  LDMAP-HOSPNUM
           MOVE    TBL-LDMAP           TO  MCPDATA-REC
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
           ELSE
               MOVE    1               TO  FLG-LDMAP
           END-IF
      *
           PERFORM             UNTIL   FLG-LDMAP   =   1
                               OR      SPA-GMN-GYOMULST-MAX
                                                   >=  99
      *       病床管理は対象としない
               IF      LDMAP-LDMSG (1:2)   =   "35"
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-GYOMULST-MAX
      *
                   MOVE    LDMAP-LDMSG     TO  SPA-GMN-GYOMULST
                                               (SPA-GMN-GYOMULST-MAX)
               END-IF
      *
               MOVE    "tbl_ldmap"     TO  MCP-TABLE
               MOVE    "key4"          TO  MCP-PATHNAME
               PERFORM 900-TBL-LDMAP-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ldmap"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           ADD     1                   TO  SPA-GMN-GYOMULST-MAX
           MOVE    "99 その他"         TO  SPA-GMN-GYOMULST
                                           (SPA-GMN-GYOMULST-MAX)
      *
      *    コラムリストの編集
           MOVE   SPA-GMN-GYOMULST (1) TO  SPA-GMN-GYOMU-G
                                           SPA-GMN-GYOMU-OLD-G
           PERFORM 3101-SPA-LIST-HEN-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コラムリストの編集
      *****************************************************************
       3101-SPA-LIST-HEN-SEC           SECTION.
      *
           INITIALIZE              SPA-GMN-TBL
                                   SPA-NAI-TBL
           MOVE    SPACE           TO  WRK-PRTKANRI-TBL-GROUP
      *
           IF      SPA-GMN-GYOMU-CD    =   "00"
                   MOVE "key11"        TO  WRK-MCP-PATHNAME 
           ELSE
                   MOVE "key12"        TO  WRK-MCP-PATHNAME 
           END-IF
      *
           INITIALIZE                  PRTKANRI-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-NAI-CREYMD      TO  PRTKANRI-CREYMD
           MOVE    SPA-GMN-GYOMU-CD    TO  PRTKANRI-GYOUMU-CD
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC             =   ZERO
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *
           COMPUTE WRK-STR         = ( SPA-GMN-PAGE  -  1 )
                                   *   WRK-CONS-TBL-MAX
                                   +   1
      *
           MOVE    ZERO                TO  IDX
                                           IDX2
                                           SPA-GMN-MAX
           PERFORM UNTIL ( IDX             >=  WRK-CONS-TBL-MAX )
                   OR    ( FLG-PRTKANRI    =   1                )
      *
               ADD     1               TO  IDX2
               IF      IDX2            >=  WRK-STR
                   PERFORM 31011-PRTKANRI-SPA-SEC
               END-IF
      *
               PERFORM 900-PRTKANRI-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *                                 
           IF    ( IDX                 >=  WRK-CONS-TBL-MAX )
           AND   ( FLG-PRTKANRI        =   ZERO             )
               MOVE    1                   TO  SPA-GMN-TBL-NEXT
           ELSE
               MOVE    ZERO                TO  SPA-GMN-TBL-NEXT
           END-IF
      *                                 
           IF      SPA-GMN-MAX         >   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-SPA-LIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票管理情報 編集処理
      *****************************************************************
       31011-PRTKANRI-SPA-SEC     SECTION.
      *
           ADD     1                   TO  IDX
      *  
           IF      IDX                 <=  WRK-CONS-TBL-MAX
               MOVE    IDX2                TO  SPA-GMN-TNO        (IDX)
               MOVE    PRTKANRI-TBL-KEY    TO  SPA-NAI-TTBL-KEY   (IDX)
               MOVE    PRTKANRI-TBL-GROUP  TO  SPA-NAI-TTBL-GROUP (IDX)
               MOVE    PRTKANRI-TBL-UUID   TO  SPA-NAI-TTBL-UUID  (IDX)
               MOVE    PRTKANRI-SKYYMD     TO  SPA-NAI-TSKYYMD-9  (IDX)
               IF      PRTKANRI-SKYYMD NOT =   SPACE   AND ZERO
                   MOVE    PRTKANRI-SKYYMD     TO  WRK-SYMD 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-NAI-TSKYYMD(IDX)
               END-IF
               IF      WRK-PRTKANRI-TBL-GROUP
                                           NOT =   PRTKANRI-TBL-GROUP
                   MOVE    PRTKANRI-TBL-GROUP
                                           TO  SPA-GMN-TTBL-GROUP (IDX)
                                               WRK-PRTKANRI-TBL-GROUP
                   IF      PRTKANRI-SRYYM  NOT =   SPACE   AND ZERO
                       MOVE    PRTKANRI-SRYYM  TO  WRK-SYMD (1:6)
                       MOVE    "01"            TO  WRK-SYMD (7:2)
                       PERFORM 5002-HIZUKE-HEN-SEC
                       MOVE    WRK-HENYMDG(1:6)
                                               TO  SPA-GMN-TSRYYM (IDX)
                   END-IF
                   MOVE    SPA-NAI-TSKYYMD(IDX)
                                               TO  SPA-GMN-TSKYYMD(IDX)
      *
                   SET     TBLLOCK-IDX             TO  1
                   SEARCH  ORC-LOCKCONST-TX    VARYING  TBLLOCK-IDX
                       AT   END
                           MOVE    "その他"        TO  SPA-GMN-TGYOUMU
                                                       (IDX)
                       WHEN   PRTKANRI-GYOUMU-CD   =
                                      ORC-LOCKCONST-GNUM (TBLLOCK-IDX)
                          MOVE    ORC-LOCKCONST-NAME (TBLLOCK-IDX)
                                                   TO  SPA-GMN-TGYOUMU
                                                       (IDX)
                   END-SEARCH
               END-IF                                 
               MOVE    PRTKANRI-RENNUM     TO  SPA-GMN-TRENNUM    (IDX)
               MOVE    PRTKANRI-SHORI-RENNUM
                                           TO  SPA-GMN-TSHORI-RENNUM
                                                                  (IDX)
               MOVE    PRTKANRI-PAGE       TO  SPA-GMN-TPAGE      (IDX)
               MOVE    PRTKANRI-TITLE      TO  SPA-GMN-TPRT-TITLE (IDX)
               MOVE    PRTKANRI-PRTFLG     TO  SPA-NAI-TPRTFLG    (IDX)
      *
               IF    ( PRTKANRI-SHELLID    =   "API" OR "API_01" )
                   MOVE    "1"             TO  SPA-NAI-TAPI       (IDX)
               END-IF
               MOVE    PRTKANRI-API-ID     TO  SPA-NAI-TAPI-ID    (IDX)
      *
               PERFORM 3101-SPA-PTINFO-SEC
      *
               MOVE    IDX                 TO  SPA-GMN-MAX
           END-IF
           .
       31011-PRTKANRI-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *     患者情報取得
      *****************************************************************
       3101-SPA-PTINFO-SEC          SECTION.
      *
           INITIALIZE                      PRTDATA-REC
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
           MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
           MOVE    PRTKANRI-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
           MOVE    PRTKANRI-TBL-UUID   TO  PRTDATA-TBL-UUID
           MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
           MOVE    1                   TO  PRTDATA-PAGE
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           PERFORM 900-PRTDATA-INV-SEC
           IF      FLG-PRTDATA         =   ZERO
               MOVE    PRTDATA-PTID        TO  SPA-NAI-LPTID  (IDX)
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    PRTDATA-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM     TO  SPA-GMN-TLPTNUM  (IDX)
               END-IF
      *
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    PRTDATA-PTID        TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-NAME          TO  SPA-GMN-TLPTNAME (IDX)
           ELSE
               DISPLAY "XA02 PRT FETCH ERR !!"
           END-IF
           .
       3101-SPA-PTINFO-EXT.
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
                       PERFORM 460-PRINT-SEC
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
                       PERFORM 460-PRINT-SEC
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
      *    一括削除処理
      *****************************************************************
       330-XA03-SET-SEC            SECTION.
      *
           IF      SPA-XAID1-FLG    =   "OK"
               PERFORM 430-DELETE-DAY-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-XAID1-FLG
           MOVE    SPACE           TO  SPA-XA99-FLG
           MOVE    SPACE           TO  SPA-XAIDCD
           MOVE    SPACE           TO  SPA-XAIDCD2
           .
       330-XA03-SET-EXT.
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
      *    画面移行用のパラメタ変更
           MOVE    "XA02"              TO  SPA-MOTOPG
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    SPA-GMN-RENNUM      TO  LNK-XC01-RENNUM
           MOVE    SPA-NAI-TBL-GROUP   TO  LNK-XC01-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  LNK-XC01-TBL-UUID
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  LNK-XC01-SHORI-RENNUM
           MOVE    SPA-GMN-STPAGE      TO  LNK-XC01-ST-PAGE
           MOVE    SPA-GMN-EDPAGE      TO  LNK-XC01-ED-PAGE
           MOVE    SPACE               TO  LNK-XC01-MODE
      *          
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-XA02FREE        TO  LNK-SCRSPA
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
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
      *     前日
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-BEFORE-SEC
      *     次日
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-NEXT-SEC
      *     帳票削除
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM  430-DELETE-DAY-GID-SEC
      *     前頁
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 440-BACK-PAGE-SEC
      *     次頁
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 450-NEXT-PAGE-SEC
      *     一覧更新
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 470-REFRESH-SEC
      *     印刷
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 460-PRINT-GID-SEC
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
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 207-PTNUM-SEC
               WHEN    "ACTIVATE"      ALSO    "GYOMUCD"
                   PERFORM 208-GYOMU-SEC
               WHEN    "ACTIVATE"      ALSO    "CREYMD"
                   PERFORM 209-CREYMD-SEC
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
           IF      XA02-RENNUM NOT =   SPA-GMN-RENNUM
               MOVE    "RENNUM"        TO  MCP-WIDGET
               PERFORM 203-RENNUM-SEC
           ELSE
               IF      XA02-SHORI-RENNUM
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
               MOVE    XA02-STPAGE     TO  SPA-GMN-STPAGE
               MOVE    XA02-EDPAGE     TO  SPA-GMN-EDPAGE
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
           MOVE    XA02-SELNUM         TO  SPA-GMN-SELNUM
      *     
           IF      SPA-GMN-SELNUM      =   ZERO
               CONTINUE
           ELSE
               COMPUTE WRK-BANGO       =   SPA-GMN-SELNUM
                                       - ( SPA-GMN-PAGE    -   1 )
                                           *   WRK-CONS-TBL-MAX
      *
               IF    ( WRK-BANGO       >   SPA-GMN-MAX )
               OR    ( WRK-BANGO       <   1           )
                   MOVE    "0001"          TO  SPA-ERRCD
               ELSE
                   MOVE    WRK-BANGO       TO  SPA-NAI-SELNUM     
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
           MOVE    SPA-NAI-SELNUM      TO  IDY
      *     
           MOVE    SPA-NAI-TTBL-GROUP  (IDY)   TO  SPA-NAI-TBL-GROUP
           MOVE    SPA-NAI-TSKYYMD     (IDY)   TO  SPA-GMN-SKYYMD
           MOVE    SPA-NAI-TTBL-UUID   (IDY)   TO  SPA-GMN-TBL-UUID 
           MOVE    SPA-GMN-TRENNUM     (IDY)   TO  SPA-GMN-RENNUM
           MOVE    SPA-GMN-TSHORI-RENNUM (IDY) TO  SPA-GMN-SHORI-RENNUM
           MOVE    1                           TO  SPA-GMN-STPAGE
           MOVE    SPA-GMN-TPAGE       (IDY)   TO  SPA-GMN-EDPAGE 
           MOVE    SPA-GMN-TLPTNUM     (IDY)   TO  SPA-GMN-PTNUM  
           MOVE    SPA-GMN-TLPTNAME    (IDY)   TO  SPA-GMN-NAME
           MOVE    SPA-NAI-LPTID       (IDY)   TO  SPA-NAI-PTID
           MOVE    SPA-GMN-TPRT-TITLE  (IDY)   TO  SPA-GMN-PRT-TITLE
           MOVE    SPA-NAI-TAPI (IDY)          TO  SPA-NAI-API
           MOVE    SPA-NAI-TAPI-ID (IDY)       TO  SPA-NAI-API-ID
      *
           MOVE    SPA-NAI-SELNUM      TO  XA02-ROW
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-GMN-MAX
                   OR          IDX     =       IDY 
               IF      SPA-NAI-PTID        =   SPA-NAI-LPTID  (IDX)
                   MOVE    IDX                 TO  XA02-ROW
                   MOVE    SPA-GMN-MAX         TO  IDX
               END-IF         
           END-PERFORM
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
           MOVE    XA02-RENNUM         TO  SPA-GMN-RENNUM
      *     
           MOVE    ZERO                TO  SPA-GMN-STPAGE
                                           SPA-GMN-EDPAGE
                                           SPA-GMN-SELNUM
                                           SPA-NAI-SELNUM
           MOVE    SPACE               TO  SPA-GMN-PRT-TITLE      
           IF      SPA-GMN-RENNUM      =   ZERO
               MOVE    ZERO                TO  SPA-GMN-SHORI-RENNUM
               MOVE    99                  TO  SPA-GMN-CUR
               GO  TO  203-RENNUM-EXT                                
           END-IF
      *     
           IF      SPA-NAI-TBL-GROUP   =   SPACE
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                       OR      SPA-NAI-TBL-GROUP
                                           >   SPA-NAI-TTBL-GROUP (IDX)
                   IF      SPA-NAI-TBL-GROUP
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
                           MOVE    SPA-GMN-TPRT-TITLE (IDX)
                                                   TO  SPA-GMN-PRT-TITLE                                 
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
           MOVE    XA02-SHORI-RENNUM   TO  SPA-GMN-SHORI-RENNUM
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
                                           SPA-NAI-SELNUM      
      *
           IF      SPA-GMN-SHORI-RENNUM    =   ZERO
               MOVE    ZERO                TO  SPA-GMN-STPAGE
                                               SPA-GMN-EDPAGE
               MOVE    SPACE               TO  SPA-GMN-PRT-TITLE      
               MOVE    99                  TO  SPA-GMN-CUR
               GO  TO  204-SHORI-RENNUM-EXT                                
           END-IF
      *     
           IF      SPA-NAI-TBL-GROUP   =   SPACE
           OR      SPA-GMN-RENNUM      =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       SPA-GMN-MAX
                   IF      SPA-NAI-TBL-GROUP
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
                   MOVE    SPA-GMN-TPRT-TITLE (IDX)
                                               TO  SPA-GMN-PRT-TITLE                                 
               END-IF
           END-IF        
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    5                   TO  SPA-GMN-CUR
           ELSE    
               MOVE    ZERO                TO  SPA-GMN-STPAGE
                                               SPA-GMN-EDPAGE
               MOVE    SPACE               TO  SPA-GMN-PRT-TITLE      
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
               IF      SPA-NAI-TBL-GROUP       =   SPACE
               OR      SPA-GMN-RENNUM          =   ZERO
               OR      SPA-GMN-SHORI-RENNUM    =   ZERO
               OR      SPA-NAI-SELNUM          =   ZERO
                   IF      SPA-NAI-SELNUM          =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                   END-IF
               ELSE    
                   IF      SPA-GMN-STPAGE      >
                                       SPA-GMN-TPAGE (SPA-NAI-SELNUM) 
                       MOVE    "0002"              TO  SPA-ERRCD
                   END-IF                                    
               END-IF
           ELSE
               IF      SPA-NAI-TBL-GROUP   NOT =   SPACE
               AND     SPA-GMN-RENNUM          >   ZERO
               AND     SPA-GMN-SHORI-RENNUM    >   ZERO
               AND     SPA-NAI-SELNUM          >   ZERO
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
               IF      SPA-NAI-TBL-GROUP       =   SPACE
               OR      SPA-GMN-RENNUM          =   ZERO
               OR      SPA-GMN-SHORI-RENNUM    =   ZERO
               OR      SPA-NAI-SELNUM          =   ZERO
                   IF      SPA-NAI-SELNUM          =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                   END-IF
               ELSE    
                   IF      SPA-GMN-EDPAGE      >
                                       SPA-GMN-TPAGE (SPA-NAI-SELNUM) 
                       MOVE    "0002"              TO  SPA-ERRCD
                   ELSE    
                       IF      SPA-GMN-EDPAGE      <   SPA-GMN-STPAGE 
                           MOVE    "0002"              TO  SPA-ERRCD
                       END-IF
                   END-IF                                        
               END-IF
           ELSE
               IF      SPA-NAI-TBL-GROUP   NOT =   SPACE
               AND     SPA-GMN-RENNUM          >   ZERO
               AND     SPA-GMN-SHORI-RENNUM    >   ZERO
               AND     SPA-NAI-SELNUM          >   ZERO
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
      *    患者番号入力処理
      *****************************************************************
       207-PTNUM-SEC                SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
      *     
           MOVE    XA02-PTNUM          TO  SPA-GMN-PTNUM
      *
           MOVE    SPACE               TO  SPA-GMN-NAME
           MOVE    ZERO                TO  SPA-NAI-PTID 
      *
           INITIALIZE                  ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    SPA-GMN-PTNUM       TO  SPTID-PTNUM
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    "0008"          TO  SPA-ERRCD
           ELSE
               MOVE    SPTID-PTNUM         TO  SPA-GMN-PTNUM
               MOVE    SPTID-PTID          TO  SPA-NAI-PTID
      *
      *        患者マスター存在チェック
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    SPA-NAI-PTID        TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF       NOT =   ZERO
                   MOVE    "0008"              TO  SPA-ERRCD
               ELSE
                   MOVE    PTINF-NAME          TO  SPA-GMN-NAME
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >       SPA-GMN-MAX
                   IF      SPA-NAI-PTID        =   SPA-NAI-LPTID  (IDX)
                       MOVE    IDX                 TO  SPA-NAI-SELNUM
                       INITIALIZE                      SPA-GMN-BODY
                       MOVE    SPA-GMN-TNO (IDX)   TO  SPA-GMN-SELNUM
                       PERFORM 2011-SELECT-HENSYU-SEC
                       MOVE    99                  TO  SPA-GMN-CUR
      *
                       MOVE    SPA-GMN-MAX         TO  IDX
                   END-IF         
               END-PERFORM
      *
               IF      SPA-NAI-SELNUM      =   ZERO
                   INITIALIZE                  SPA-GMN-BODY1
                                               SPA-NAI-BODY1
                   MOVE    "0009"              TO  SPA-ERRCD
               END-IF
           END-IF
           .
       207-PTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    業務コード入力処理
      *****************************************************************
       208-GYOMU-SEC                SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           MOVE    XA02-G-VALUE        TO  SPA-GMN-GYOMU-G
      *
           MOVE    SPACE           TO  SPA-GMN-GYOMU-NAME
           PERFORM VARYING IDX FROM   1   BY  1
                   UNTIL   IDX >      SPA-GMN-GYOMULST-MAX
               IF      SPA-GMN-GYOMU-CD   =  SPA-GMN-ITEM-CD (IDX)
                   MOVE    SPA-GMN-GYOMULST (IDX)
                                             TO  SPA-GMN-GYOMU-G
                   MOVE    SPA-GMN-GYOMULST-MAX
                                             TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-GMN-GYOMU-NAME  =  SPACE
               MOVE    "0007"              TO  SPA-ERRCD
               PERFORM 420-CLEAR-SEC
               INITIALIZE                      SPA-GMN-TBL
                                               SPA-NAI-TBL
                                               SPA-GMN-MAX
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-GYOMU-CD-OLD
                                       NOT =   SPA-GMN-GYOMU-CD
                   MOVE    SPA-GMN-GYOMU-G TO  SPA-GMN-GYOMU-OLD-G
                   PERFORM 420-CLEAR-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
      *            コラムリストの編集
                   PERFORM 3101-SPA-LIST-HEN-SEC
               ELSE
                   IF      SPA-GMN-MAX     >   ZERO
                       MOVE    1           TO  SPA-GMN-CUR
                   END-IF
               END-IF
           ELSE
               MOVE   SPACE            TO  SPA-GMN-GYOMU-OLD-G
           END-IF  
     *
           .
       208-GYOMU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月日入力処理
      *****************************************************************
       209-CREYMD-SEC               SECTION.
      *
           MOVE    8                   TO  SPA-GMN-CUR
      *     
           MOVE    XA02-CREYMD         TO  SPA-GMN-CREYMD
      *  
           MOVE    SPACE               TO  SPA-NAI-CREYMD
           IF      SPA-GMN-CREYMD      =   SPACE
               MOVE    SPA-NAI-SMCNDATE-YMD    TO  SPA-NAI-CREYMD
               MOVE    SPA-GMN-SMCNDATE-YMD    TO  SPA-GMN-CREYMD
           ELSE
      *        日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-CREYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-CREYMD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-CREYMD
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-CREYMD-OLD
                                       NOT =   SPA-NAI-CREYMD
                   MOVE    SPA-NAI-CREYMD      TO  SPA-NAI-CREYMD-OLD
                   PERFORM 420-CLEAR-SEC
                   MOVE    1               TO  SPA-GMN-PAGE
      *            コラムリストの編集
                   PERFORM 3101-SPA-LIST-HEN-SEC
               ELSE
                   IF      SPA-GMN-MAX     >   ZERO
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    2               TO  SPA-GMN-CUR 
                   END-IF
               END-IF
           ELSE
               MOVE   SPACE            TO  SPA-NAI-CREYMD-OLD
           END-IF  
      *
           .
       209-CREYMD-EXT.
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
               IF      XA02-SELECT (IDX)   =   "T"
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
           MOVE    SPA-GMN-TNO (SPA-NAI-SELNUM)
                                       TO  SPA-GMN-SELNUM
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
           IF      SPA-NAI-TBL-GROUP   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               GO  TO  4103-KAKUTEI-CHK-EXT
           END-IF                                
      *
      *    印刷時のみチェック
           IF     MCP-WIDGET           =   "B12"
               IF      SPA-NAI-SELNUM  =   ZERO
                   CONTINUE
               ELSE
                   IF      SPA-GMN-STPAGE      >
                                   SPA-GMN-TPAGE (SPA-NAI-SELNUM)
                   OR      SPA-GMN-EDPAGE      >
                                   SPA-GMN-TPAGE (SPA-NAI-SELNUM)
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
           MOVE    SPA-XA02-MOTOPG     TO  SPA-SAKIPG
           MOVE    "XA02"              TO  SPA-MOTOPG
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
                                    SPA-NAI-BODY
      *
           MOVE    2                TO  SPA-GMN-CUR        
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
           MOVE    SPA-NAI-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  PRTKANRI-TBL-UUID 
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  PRTKANRI-SHORI-RENNUM
           MOVE    SPA-GMN-RENNUM      TO  PRTKANRI-RENNUM
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
               MOVE    "1002"              TO  SPA-XAIDCD
      *
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
           MOVE    SPA-NAI-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  PRTKANRI-TBL-UUID 
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  PRTKANRI-SHORI-RENNUM
           MOVE    SPA-GMN-RENNUM      TO  PRTKANRI-RENNUM
grpsys     MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "del14"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1002"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *         
           INITIALIZE                      PRTDATA-REC
           MOVE    SPA-WRK-TBL-KEY     TO  PRTDATA-TBL-KEY
           MOVE    SPA-NAI-TBL-GROUP   TO  PRTDATA-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  PRTDATA-TBL-UUID
           MOVE    SPA-GMN-SHORI-RENNUM
                                       TO  PRTDATA-SHORI-RENNUM
           MOVE    SPA-GMN-RENNUM      TO  PRTDATA-RENNUM
grpsys     MOVE    SPA-HOSPNUM         TO  PRTDATA-HOSPNUM
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    PRTDATA-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "delmb2"            TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "del14"             TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC         NOT  =   ZERO
               MOVE    "1003"              TO  SPA-ERRCD
               GO  TO  430-DELETE-EXT
           END-IF
      *
           PERFORM 470-REFRESH-SEC
           .
       430-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除日数入力処理
      *****************************************************************
        430-DELETE-DAY-GID-SEC               SECTION.
      *
           IF      SPA-NAI-SELNUM      >   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               GO  TO  430-DELETE-DAY-GID-EXT
           END-IF
      *
           MOVE    SPACE               TO  SPA-XAID1-FLG
      *    
           MOVE    SPA-XA02KYOUTU      TO  SPA-WORK  
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XA02FREE        TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGXA03"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-XA02FREE
           MOVE    SPA-WORK            TO  SPA-XA02KYOUTU 
      *
           MOVE    "DAY"               TO  MCP-WIDGET
      *
           MOVE    "XA02"              TO  SPA-MOTOPG
           MOVE    "XA03"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XA03"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
        430-DELETE-DAY-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括削除処理
      *****************************************************************
       430-DELETE-DAY-SEC              SECTION.
      *
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
      *
           INITIALIZE                      PRTKANRI-REC
           MOVE    WRK-CONS-DEL-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-NAI-XA03-CREYMD TO  PRTKANRI-CREYMD
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PRTKANRI-FET-SEC
           ELSE 
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
      *
           PERFORM     UNTIL   FLG-PRTKANRI    =   1
                       OR      SPA-ERRCD   NOT =   SPACE
               INITIALIZE                      PRTDATA-REC
               MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
               MOVE    PRTKANRI-RENNUM     TO  PRTDATA-RENNUM
               MOVE    PRTKANRI-TBL-GROUP  TO  PRTDATA-TBL-GROUP
               MOVE    PRTKANRI-TBL-UUID   TO  PRTDATA-TBL-UUID
               MOVE    PRTKANRI-TBL-KEY    TO  PRTDATA-TBL-KEY
               MOVE    PRTKANRI-SHORI-RENNUM
                                           TO  PRTDATA-SHORI-RENNUM
               MOVE    PRTKANRI-HOSPNUM    TO  PRTDATA-HOSPNUM
               MOVE    PRTDATA-REC         TO  MCPDATA-REC
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 900-PRTDATA-FET-SEC
               ELSE 
                   MOVE    1                   TO  FLG-PRTDATA
               END-IF
      *
               PERFORM     UNTIL   FLG-PRTDATA     =   1
                           OR      SPA-ERRCD   NOT =   SPACE
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_prtdata"       TO  MCP-TABLE
                   MOVE    "delmb5"            TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1008"              TO  SPA-ERRCD
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "tbl_prtdata"       TO  MCP-TABLE
                       MOVE    "del18"             TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    "1009"              TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
                       INITIALIZE                      PRVKANRI-REC
                       MOVE    PRTDATA-TBL-KEY     TO  PRVKANRI-TBL-KEY
                       MOVE    PRTDATA-RENNUM      TO  PRVKANRI-RENNUM
                       MOVE    PRTDATA-TBL-GROUP   TO
                                                   PRVKANRI-TBL-GROUP
                       MOVE    PRTDATA-TBL-UUID    TO  PRVKANRI-TBL-UUID
                       MOVE    PRTDATA-TBL-KEY     TO  PRVKANRI-TBL-KEY
                       MOVE    PRTDATA-SHORI-RENNUM
                                                   TO
                                                   PRVKANRI-SHORI-RENNUM
                       MOVE    PRTDATA-PAGE        TO  PRVKANRI-PAGE
                       MOVE    SPA-HOSPNUM         TO  PRVKANRI-HOSPNUM
                       MOVE    PRVKANRI-REC        TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_prvkanri"      TO  MCP-TABLE
                       MOVE    "del16"             TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC          NOT =   ZERO
                           MOVE    "1010"              TO  SPA-ERRCD
                       END-IF
                   END-IF
      *
                   MOVE    "key4"             TO  MCP-PATHNAME
                   PERFORM 900-PRTDATA-FET-SEC
               END-PERFORM
      *
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    PRTKANRI-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_prtkanri"      TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC         NOT  =   ZERO
                       MOVE    "1007"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-PRTKANRI-FET-SEC
           END-PERFORM
      *
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           PERFORM 470-REFRESH-SEC
           .
       430-DELETE-DAY-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日処理
      *****************************************************************
       440-BEFORE-SEC                  SECTION.
      *
          INITIALIZE               SPA-GMN-BODY
                                   SPA-GMN-TBL
                                   SPA-NAI-BODY
                                   SPA-NAI-TBL
           MOVE    1   TO          SPA-GMN-PAGE
           MOVE    SPA-NAI-CREYMD      TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    -1                  TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-CREYMD
                                           SPA-NAI-CREYMD-OLD
      *    コラムリストの編集
           PERFORM 3101-SPA-LIST-HEN-SEC
      *
           PERFORM 470-EDIT-CREYMD-SEC
      *
           .
       440-BEFORE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次日処理
      *****************************************************************
       450-NEXT-SEC                    SECTION.
      *
          INITIALIZE               SPA-GMN-BODY
                                   SPA-GMN-TBL
                                   SPA-NAI-BODY
                                   SPA-NAI-TBL
           MOVE    1   TO          SPA-GMN-PAGE
           MOVE    SPA-NAI-CREYMD         TO  WRK-YMD
           MOVE    "1"                 TO  WRK-ZOGENPTN
           MOVE    1                   TO  WRK-ZOGEN
           PERFORM 800-CALENDAR-SEC
           MOVE    LNK-DAY6-KEISAN     TO  SPA-NAI-CREYMD
                                           SPA-NAI-CREYMD-OLD
      *    コラムリストの編集
           PERFORM 3101-SPA-LIST-HEN-SEC
      *
           PERFORM 470-EDIT-CREYMD-SEC
      *
           .
       450-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       440-BACK-PAGE-SEC               SECTION.
      *
           IF    ( SPA-GMN-PAGE    >   1 )
               INITIALIZE              SPA-GMN-BODY
                                       SPA-GMN-TBL
                                       SPA-NAI-BODY
                                       SPA-NAI-TBL
               COMPUTE     SPA-GMN-PAGE
                   =       SPA-GMN-PAGE
                   -       1
      *        コラムリストの編集
               PERFORM 3101-SPA-LIST-HEN-SEC
           END-IF
      *
           .
       440-BACK-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁処理
      *****************************************************************
       450-NEXT-PAGE-SEC           SECTION.
      *
           IF    ( SPA-GMN-TBL-NEXT    =   1 )
               INITIALIZE              SPA-GMN-BODY
                                       SPA-GMN-TBL
                                       SPA-NAI-BODY
                                       SPA-NAI-TBL
               COMPUTE     SPA-GMN-PAGE
                   =       SPA-GMN-PAGE
                   +       1
      *        コラムリストの編集
               PERFORM 3101-SPA-LIST-HEN-SEC
           END-IF
      *
           .
       450-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    リフレッシュ処理
      *****************************************************************
       470-REFRESH-SEC                 SECTION.
      *
          INITIALIZE               SPA-GMN-BODY
                                   SPA-GMN-TBL
                                   SPA-NAI-BODY
                                   SPA-NAI-TBL
           PERFORM 3101-SPA-LIST-HEN-SEC
      *
           .
       470-REFRESH-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日編集処理
      *****************************************************************
       470-EDIT-CREYMD-SEC             SECTION.
      *
           MOVE    SPA-NAI-CREYMD  TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-CREYMD
      *
           .
       470-EDIT-CREYMD-EXT.
           EXIT.
      *****************************************************************
      *    情報削除画面へ
      *****************************************************************
       490-JYOUHOU-DEL-SEC             SECTION.
      *
      *    画面移行用のパラメタ変更
           MOVE    "XA02"              TO  SPA-MOTOPG
           MOVE    "XB01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SPA-JOB-SHELLID
      *
           MOVE    SPA-XA02KYOUTU      TO  SPA-WORK
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
           MOVE    SPA-XA02FREE        TO  LNK-SCRSPA
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE   "XB01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-JYOUHOU-DEL-EXT.
           EXIT.
      *****************************************************************
      *    作表処理
      *****************************************************************
       460-PRINT-GID-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  460-PRINT-GID-EXT
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
                           MOVE    SPACE           TO  WRK-XA02ERR
                           STRING  "処理中です【"  DELIMITED  BY  SIZE
                                   JOB-SHELLMSG    DELIMITED  BY  SPACE
                                   "】"            DELIMITED  BY  SIZE      
                                                   INTO  WRK-XA02ERR
                           END-STRING
                           MOVE    SYS-3003-JOBID  TO  IDX
                       END-IF
                   END-IF    
               END-PERFORM
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE                 
               GO  TO  460-PRINT-GID-EXT               
           END-IF
      *         
      *    帳票管理ＤＢのチェック
           INITIALIZE                      PRTKANRI-REC
           MOVE    SPA-PRTKANRI-TBL-KEY
                                       TO  PRTKANRI-TBL-KEY
           MOVE    SPA-NAI-TBL-GROUP   TO  PRTKANRI-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  PRTKANRI-TBL-UUID
           MOVE    SPA-HOSPNUM         TO  PRTKANRI-HOSPNUM
           MOVE    PRTKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
       460-PRINT-GID-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       460-PRINT-SEC                   SECTION.
      *
           INITIALIZE                      SPA-XA02-TBL-GROUP
                                           SPA-XA02-JOB-JOBID
      *
      *    ＪＯＢＩＤ（３００４）削除処理
           MOVE    SPACE               TO  SYS-3003-REC 
           INITIALIZE                      SYS-3003-REC
           MOVE    "3003"              TO  SYS-3003-KANRICD
           MOVE    WRK-CONS-3003-KBNCD TO  SYS-3003-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-3003-STYUKYMD
                                           SYS-3003-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3003-HOSPNUM
      *    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SYS-3003-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-3003-REC
      *        MOVE    SPA-HOSPNUM         TO  -HOSPNUM
               MOVE    SYS-3003-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC              =   ZERO
                   CONTINUE
               ELSE                                 
                   MOVE    "1005"              TO  SPA-ERRCD
                   GO  TO  460-PRINT-EXT
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
               GO  TO  460-PRINT-EXT
           END-IF             
      *
      *    monblobapi処理
           IF      SPA-XA02-JOB-PARAM
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
                   GO  TO  460-PRINT-EXT
               END-IF
               MOVE    MONBLOB-ID          TO
                                       SPA-XA02-MONBLOB-PRINT-ID (1)
           END-IF
      *
      *
           MOVE    ZERO                TO  WRK-CONS-JOB-JOBID     
                                           WRK-PRTKANRI-RENNUM
           MOVE    SPACE               TO  WRK-CONS-PRTKANRI-TBL-GROUP                                
      *     
           MOVE    SPA-HOSPNUM         TO  XA02ERR-HOSPNUM
           MOVE    "OR001ERR"          TO  XA02ERR-FILE-ID
           MOVE    SPA-TERMID          TO  XA02ERR-TERMID
           MOVE    2                   TO  XA02ERR-SYOKBN1
      *
      *    印刷処理
      *
           PERFORM 45011-JOBKANRI-HENSYU-SEC
      *
           MOVE    SPA-GMN-STPAGE      TO  WRK-STPAGE
           IF      SPA-GMN-EDPAGE      =   ZERO
               MOVE    ALL "9"             TO  WRK-EDPAGE-G
           ELSE
               MOVE    SPA-GMN-EDPAGE      TO  WRK-EDPAGE
           END-IF                            
      *
           MOVE    SPA-NAI-TBL-GROUP   TO  SPA-XA02-TBL-GROUP
           MOVE    WRK-CONS-JOB-JOBID  TO  SPA-XA02-JOB-JOBID
      *             
           MOVE    SPACE               TO  SHELLTBL
           MOVE    "print_prv.sh"      TO  SHELLTBL-NAME
      *     
           MOVE    SPACE               TO  LNK-PRTKANRI-AREA
           INITIALIZE                      LNK-PRTKANRI-AREA
           MOVE    SPA-WRK-TBL-KEY     TO  LNK-PRTKANRI-TBL-KEY  
           MOVE    SPA-NAI-TBL-GROUP   TO  LNK-PRTKANRI-TBL-GROUP
           MOVE    SPA-GMN-TBL-UUID    TO  LNK-PRTKANRI-TBL-UUID
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
           MOVE    XA02ERR-BASENAME    TO  SHELLTBL-ARG12
           MOVE    WRK-CONS-JOB-JOBID  TO  SHELLTBL-ARG13
           MOVE    WRK-CONS-JOB-SHELLID
                                       TO  SHELLTBL-ARG14
           MOVE    WRK-STPAGE          TO  SHELLTBL-ARG15
           MOVE    WRK-EDPAGE          TO  SHELLTBL-ARG16
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG18
           MOVE    LNK-PRTKANRI-TBL-UUID 
                                       TO  SHELLTBL-ARG19      
           MOVE    LNK-PRTKANRI-GYOUMU-CD
                                       TO  SHELLTBL-ARG20      
           EVALUATE    SPA-XAID1-FLG
               WHEN    "PV"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "NO"
                   MOVE    "3"             TO  SHELLTBL-ARG17
               WHEN    "OK"
                   MOVE    "2"             TO  SHELLTBL-ARG17
           END-EVALUATE
           IF      SPA-XA02-JOB-PARAM
                                   NOT =   SPACE
               MOVE    "7"                 TO  SHELLTBL-ARG17
           END-IF
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
      *
      *    monblobapi処理
           IF      SPA-XA02-JOB-PARAM
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
       460-PRINT-EXT.
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
           MOVE    SPA-XA02-JOB-KARTE-UID
                                   TO  JOB-KARTE-UID
           MOVE    SPA-XA02-JOB-ORCA-UID
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
           MOVE    SPA-XA02-TBL-GROUP  TO  LNK-PRTKANRI-TBL-GROUP
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
           MOVE    XA02ERR-BASENAME    TO  SHELLTBL-ARG15
      *
           MOVE    SPA-XA02-MONBLOB-PRINT-ID (1)
                                       TO  SHELLTBL-ARG16
           MOVE    SPA-XA02-PUSHBA01-EVENT
                                       TO  SHELLTBL-ARG17
           MOVE    SPA-XA02-JOB-JOBID  TO  WRK-SHELLTBL-ARG-9
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
                           MOVE    WRK-PAGE(5:6)   TO  XA99-PAGE (IDX)
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
           MOVE    "XA02"                TO  SPA-MOTOPG
           MOVE    "XA99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
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
      **** MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XA02    "          TO  MCP-WINDOW.
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
           MOVE    XA02-ROW            TO  WRK-LIST-ROW
      *
           INITIALIZE  XA02
      *
           EVALUATE    FLG-LIST-ROW
           WHEN    1
               MOVE    ZERO            TO  XA02-ROW
               MOVE    ZERO            TO  XA02-ROWATTR
           WHEN    OTHER
               MOVE    WRK-LIST-ROW    TO  XA02-ROW
               MOVE    ZERO            TO  XA02-ROWATTR
           END-EVALUATE
      *
           MOVE    SPA-GMN-TITLE1      TO  XA02-HEAD1
           MOVE    SPA-GMN-TITLE2      TO  XA02-HEAD2
      *     
           MOVE    SPA-GMN-SELNUM      TO  XA02-SELNUM
           MOVE    SPA-GMN-SKYYMD      TO  XA02-SKYYMD
           MOVE    SPA-GMN-RENNUM      TO  XA02-RENNUM
           MOVE    SPA-GMN-SHORI-RENNUM 
                                       TO  XA02-SHORI-RENNUM
           IF    ( SPA-NAI-API     NOT =   "1" )
               MOVE    SPA-GMN-STPAGE  TO  XA02-STPAGE
               MOVE    SPA-GMN-EDPAGE  TO  XA02-EDPAGE
           END-IF
           MOVE    SPA-GMN-PRT-TITLE   TO  XA02-PRT-TITLE         
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX
      *
               MOVE    SPA-GMN-TNO       (IDX) TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  XA02-TNO    (IDX)
               IF      SPA-GMN-TTBL-GROUP (IDX) NOT = SPACE
                   MOVE    SPA-GMN-TTBL-GROUP (IDX) (1:8)
                                           TO  WRK-SYMD
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG     TO  XA02-SAKU-YMD(IDX)
                   STRING  SPA-GMN-TTBL-GROUP(IDX)(9:2)
                                               DELIMITED  BY  SIZE
                           ":"                 DELIMITED  BY  SIZE
                           SPA-GMN-TTBL-GROUP(IDX)(11:2)
                                               DELIMITED  BY  SIZE
                           ":"                 DELIMITED  BY  SIZE
                           SPA-GMN-TTBL-GROUP(IDX)(13:2)
                                               DELIMITED  BY  SIZE
                                       INTO    XA02-SAKU-TIME(IDX)
                   END-STRING
               END-IF
               MOVE    SPA-GMN-TRENNUM   (IDX) TO  XA02-TRENNUM   (IDX)
               MOVE    SPA-GMN-TSHORI-RENNUM(IDX)
                                               TO
                                               XA02-TSHORI-RENNUM (IDX)
               IF    ( SPA-NAI-TAPI (IDX)   NOT =   "1" )
                   MOVE    SPA-GMN-TPAGE     (IDX) TO  WRK-PAGE
                   MOVE    WRK-PAGE(6:5)       TO  XA02-TPAGE     (IDX)
               END-IF
      *
               MOVE    SPA-GMN-TGYOUMU   (IDX) TO  XA02-GYOMUNM   (IDX)
      *
               MOVE    SPA-GMN-TSKYYMD   (IDX) TO  XA02-TSKYYMD   (IDX)
               MOVE    SPA-GMN-TLPTNUM   (IDX) TO  XA02-LPTNUM    (IDX)
               MOVE    SPA-GMN-TPRT-TITLE(IDX) TO  XA02-TPRT-TITLE(IDX)
               MOVE    SPA-GMN-TLPTNAME   (IDX) TO  XA02-PTNAME   (IDX)
      *       
               IF      IDX             =   SPA-NAI-SELNUM
                   MOVE    "T"             TO  XA02-SELECT (IDX)
               ELSE
                   MOVE    "F"             TO  XA02-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  XA02-COUNT
      *
           MOVE    SPA-GMN-GYOMU-G     TO  XA02-G-VALUE
           MOVE    SPA-GMN-GYOMULST-MAX
                                       TO  XA02-G-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-GYOMULST-MAX
               MOVE    SPA-GMN-GYOMULST (IDX)  TO  XA02-G-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-PTNUM       TO  XA02-PTNUM 
           MOVE    "blue"              TO  XA02-PTNAME-STYLE
           MOVE    SPA-GMN-NAME        TO  XA02-PTNAME-VALUE
      *
           MOVE    SPA-GMN-CREYMD      TO  XA02-CREYMD
      *
           MOVE    WIDGET-NORMAL       TO  XA02-B06S-STATE
                                           XA02-B07S-STATE
      *
           IF    ( SPA-GMN-PAGE        =   1 )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XA02-B06S-STATE
           END-IF
      *
           IF    ( SPA-GMN-TBL-NEXT    =   ZERO )
               MOVE    WIDGET-INSENSITIVE
                                       TO  XA02-B07S-STATE
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
           AND   ( SPA-GMN-CUR     =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    2
                   MOVE    "GYOMUCD"      TO  MCP-WIDGET
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
               WHEN    7
                   MOVE    "PTNUM"        TO  MCP-WIDGET
               WHEN    8
                   MOVE    "CREYMD"       TO  MCP-WIDGET
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
      *    エラーメッセージ表示
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
               WHEN    "0007"
                   MOVE    "業務コードの入力に誤りがあります" 
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "該当患者の帳票はありません"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "診療年月日入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "帳票を選択中です"
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "帳票管理ＤＢが削除できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
               WHEN    "1003"
               WHEN    "1006"
               WHEN    "1007"
               WHEN    "1008"
               WHEN    "1009"
               WHEN    "1010"
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
                   MOVE    WRK-XA02ERR         TO  SPA-ERRMSG
               WHEN    "9999"
                   MOVE    WRK-XA02ERR         TO  SPA-ERRMSG
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
           MOVE    "XA02"                TO  SPA-MOTOPG
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
           MOVE    "XA02"               TO  SPA-MOTOPG
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
           MOVE    ZERO                TO  CNT-MONPE
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
                                       =   SPA-NAI-TBL-GROUP )
                 AND ( SPA-NAI-TTBL-UUID (IDX)
                                       =   SPA-GMN-TBL-UUID )
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
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "XA02"               TO  SPA-MOTOPG
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
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
      *    増減パターン 1:日／2:月／3:年
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
      *    MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
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
           MOVE    "tbl_prtkanri"      TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTKANRI-REC
               MOVE    ZERO                TO  FLG-PRTKANRI
           ELSE
               MOVE    1                   TO  FLG-PRTKANRI
           END-IF
           .
       900-PRTKANRI-FET-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷ＤＢ読み込み
      *****************************************************************
       900-PRTDATA-FET-SEC             SECTION.
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           PERFORM 900-DBFETCH-SEC
      *                                  
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PRTDATA-REC
               MOVE    ZERO                TO  FLG-PRTDATA
           ELSE
               MOVE    1                   TO  FLG-PRTDATA
           END-IF
           .
       900-PRTDATA-FET-EXT.
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
      *    排他制御マスター読込（キー）
      *****************************************************************
       900-TBL-LDMAP-READ-SEC        SECTION.
      *
           MOVE    1                   TO  FLG-READ-SKIP
      *
           PERFORM UNTIL     ( FLG-READ-SKIP   =   ZERO )
                   OR        ( FLG-LDMAP   NOT =   ZERO )
               MOVE    "tbl_ldmap"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF    ( MCP-RC              =   ZERO )
                     MOVE    ZERO          TO  FLG-LDMAP
                     MOVE    MCPDATA-REC   TO  TBL-LDMAP
      *
      *            無床診療所の場合、入院情報は読み飛ばす
                   IF    ( SPA-BEDSU       <=  ZERO )
                     AND ( LDMAP-GROUPNUM   =  2 )
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  FLG-READ-SKIP
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-LDMAP
               END-IF
           END-PERFORM
      *
           .
       900-TBL-LDMAP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
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
      *    印刷データ読込
      *****************************************************************
       900-PRTDATA-INV-SEC         SECTION.
      *
           MOVE    "tbl_prtdata"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_prtdata"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-PRTDATA
                   MOVE    MCPDATA-REC         TO  PRTDATA-REC 
               ELSE
                   MOVE    1                   TO  FLG-PRTDATA
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PRTDATA
           END-IF
      *
           MOVE    "tbl_prtdata"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PRTDATA-INV-EXT.
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
