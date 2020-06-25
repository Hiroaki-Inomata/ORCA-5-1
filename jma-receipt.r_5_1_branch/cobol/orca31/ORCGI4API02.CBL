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
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCGI4API02.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 入院会計登録
      *  管理者            :
      *  作成日付    作業者        記述
      *  14/04/09    NACL−太田　新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT        SECTION.
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
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-NYUINACCT           PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-ORCSNUM             PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDX8                    PIC 9(05).
           03  IDX9                    PIC 9(05).
           03  IDXA                    PIC 9(05).
           03  IDXD                    PIC 9(05).
           03  IDXK                    PIC 9(05).
           03  IDXE                    PIC 9(05).
           03  IDXH                    PIC 9(05).
           03  IDX-ITM                 PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ERRCD               PIC X(04).
           03  WRK-ERRMSG              PIC X(100).
           03  WRK-YMD                 PIC X(10). 
           03  WRK-ZOGENPTN            PIC X(01).
           03  WRK-ZOGEN               PIC S9(05).
           03  WRK-KEISANYMD           PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-EDTYMD1             PIC X(09).
           03  WRK-DATE.
               05  WRK-DATE-YY         PIC 9(04).
               05  WRK-DATE-FL1        PIC X(01).
               05  WRK-DATE-MM         PIC 9(02).
               05  WRK-DATE-FL2        PIC X(01).
               05  WRK-DATE-DD         PIC 9(02).
           03  WRK-HMS.
               05  WRK-HMS-HH          PIC 9(02).
               05  WRK-HMS-MM          PIC 9(02).
               05  WRK-HMS-SS          PIC 9(02).
           03  WRK-TIME.
               05  WRK-TIME-HH         PIC 9(02).
               05  WRK-TIME-FL1        PIC X(01).
               05  WRK-TIME-MM         PIC 9(02).
               05  WRK-TIME-FL2        PIC X(01).
               05  WRK-TIME-SS         PIC 9(02).
           03  WRK-SRYYMD.
               05  WRK-SRYYMD-YM       PIC X(06).
               05  WRK-SRYYMD-DD       PIC 9(02).
           03  WRK-SAGAKU              PIC 9(08).
           03  WRK-SNUM-INX            PIC X(26).
           03  WRK-SNUM-OUTNUM         PIC 9(12).
           03  WRK-GAIHAKUKBN-G.
               05  WRK-GAIHAKUKBN      PIC 9(02).
      *
       01  API-AREA.
           03  API-SAVEREQ             PIC X(01).
           03  API-NYUINYMD            PIC X(08).
           03  API-ORDERID             PIC X(04).
           03  API-SAGAKU              PIC X(07).
           03  API-SAGAKUCD            PIC X(02).
           03  API-SRYYMD              PIC X(08).
           03  API-ENDYMD              PIC X(08).
           03  API-GAIHAKUKBN          PIC X(02).
           03  API-SKJ-AREA.
               05  API-SKJ-OCC         OCCURS 3.
                   07  API-SKJTIME     PIC X(02).
                   07  API-SKJTYPE     PIC X(01).
                   07  API-NISSU       PIC X(100).
                   07  API-SAME-AFTER  PIC X(1).
           03  API-ERR-MAX             PIC 9(03).
           03  API-ERR-OCC             OCCURS  10.
               05  API-ERRCD           PIC X(04).
               03  API-ERRMSG          PIC X(100).
      *
       01  CONST-AREA.
           03  CONST-H300401           PIC X(08)   VALUE "20180401".
           03  CONST-H280401           PIC X(08)   VALUE "20160401".
           03  CONST-H240401           PIC X(08)   VALUE "20120401".
           03  CONST-ERR-MAX           PIC 9(03)   VALUE 10.
           03  CONST-ODRITEM-MAX       PIC 9(05)   VALUE 30.
      *
           COPY    "COMMON-SPA"        REPLACING   //SPA-//
                                       BY          //TSPA-//.
      *
      *    オーダエラーコード
           COPY    "CPNCLAIMERRCD.INC".
      *
      *    オーダ項目ＩＤ
           COPY    "CPNCLAIMID.INC".
      *
      *    オーダＩＤ
           COPY    "CPNORDERID.INC".
      *
       01  TRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING  //PTNYUINRRK-//
                                       BY         //TRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    算定可能入院料取得
           COPY    "CPORCSNYUINRYO.INC".
      *
      *    入院オーダーエラーメッセージ取得
           COPY    "CPORCSODRS03.INC".
      *
      *    入院異動チェックサブ
           COPY    "CPORCSODR.INC".
       01  LNK-ODRITEM-REC.
           02  LNK-ODRITEM-MAX         PIC 9(03).
           02  LNK-ODRITEM-OCC         OCCURS   30.
           COPY    "CPODRITEM.INC"     REPLACING  //ODRITEM-//
                                       BY         //LNK-ODRITEM-//.
      *
      *    入院基本情報取得サブ（外泊等）
           COPY    "CPORCSNINF03.INC".
      *
      *    ＵＩＤ取得用エリア
       01  SUID-AREA.
           03  SUID-RET                PIC X(2).
           03  SUID-UID                PIC X(36).
       01  SUID-ST                     PIC 9(2).
      *
      *    API XML読み込み用定義体
           COPY    "CPHSACCTMV2REQ.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPHSACCTMV2RES.INC".
      *
      *ver.4.7
      *    API XML読み込み共通定義
           COPY    "CPAPIV2REQ.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1010.INC".
           COPY    "CPSK5113.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    オーダーテーブル
       01  ORDER-REC.
           COPY    "CPORDER.INC".
      *
      *    オーダ項目
       01  ODRITEM-REC.
           COPY    "CPODRITEM.INC".
      *
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
            COPY    "MCPAREA".
            COPY    "ORCA-SPA".
            COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA31SCRAREA.INC".
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
       000-MAIN-SEC                SECTION.
      *
           DISPLAY   "***************"
           DISPLAY   "* accept start*"
           DISPLAY   "***************"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  API-AREA
           INITIALIZE                  SPA-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( API-ERR-MAX         =   ZERO )
               PERFORM 200-MAIN-SEC
           ELSE
               IF    ( API-SAVEREQ     =   "1" )
                   PERFORM 290-ORDER-INSERT-SEC
               END-IF
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
           DISPLAY   "***************"
           DISPLAY   "* accept end  *"
           DISPLAY   "***************"
           .
       000-MAIN-EXIT.
           EXIT    PROGRAM.
      *
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  XML-HSACCTMRES
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    MCP-USER            TO  SPA-OPID
      *
           MOVE    SMCNDATE-YMD        TO  SPA-SYSYMD
      *
           INITIALIZE                  SUID-AREA
           CALL    "orcuidset" USING   SUID-ST
                                       SUID-AREA
      *
      *    医療機関識別番号セット 
           MOVE    "API"               TO  SPA-MOTOPG
           CALL    "ORCSHOSPNUM"       USING
                                       MCPAREA
                                       SPA-AREA
           IF    ( SPA-ERRCD   NOT =   SPACE )
               MOVE   "8099"       TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    ＳＰＡ共通設定
           INITIALIZE                  SYS-1010-REC
           INITIALIZE                  ORCSCOMMONAREA
           MOVE    "M00"               TO  ORCSCOMMON-PG
           CALL    "ORCSCOMMON"        USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSCOMMONAREA
                                       SYS-1010-REC
           IF    ( SPA-ERRCD       NOT =   SPACE )
               EVALUATE    SPA-ERRCD
               WHEN    "1001"
                   MOVE    "8901"      TO  WRK-ERRCD
               WHEN    "1002"
                   MOVE    "8902"      TO  WRK-ERRCD
               WHEN    "1003"
                   MOVE    "8903"      TO  WRK-ERRCD
               WHEN    "1005"
                   MOVE    "8905"      TO  WRK-ERRCD
               WHEN    "1015"
                   MOVE    "8915"      TO  WRK-ERRCD
               WHEN    OTHER
                   MOVE    "8900"      TO  WRK-ERRCD
               END-EVALUATE
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
      *    XML情報取り出し
           PERFORM 900-XML-READ-SEC
           IF    ( WRK-ERRCD           NOT =   SPACE )
               PERFORM 990-ERROR-EDIT-SEC
               PERFORM 990-EXIT-PROGRAM-SEC
           END-IF
      *
           EVALUATE    HSACCTMREQ-REQNUM
           WHEN    "2"
               PERFORM 1001-GAIHAKU-SEC
               MOVE    NORDERID-GPTSTID    TO  API-ORDERID
           WHEN    "3"
               PERFORM 1001-SAGAKU-SEC
               MOVE    NORDERID-SAGAKUID   TO  API-ORDERID
           WHEN    "4"
               PERFORM 1001-SHOKUJI-SEC
               MOVE    NORDERID-FOODID     TO  API-ORDERID
           WHEN    "9"
               PERFORM 1001-CRENACCT-SEC
               MOVE    NORDERID-CRENACCTID TO  API-ORDERID
           WHEN    OTHER
               MOVE    "8003"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    電文項目チェック処理
      *****************************************************************
       1001-INIT-CHECK-SEC         SECTION.
      *
           MOVE    HSACCTMREQ-SAVEREQ
                                       TO  API-SAVEREQ
      *
           PERFORM 10011-SRYYMD-CHECK-SEC
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
      *
           .
       1001-INIT-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（食事）
      *****************************************************************
       1001-SHOKUJI-SEC            SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
               MOVE    HSACCTMREQ-SKJTIME (IDX2)
                                           TO  API-SKJTIME (IDX2)
               MOVE    HSACCTMREQ-SKJTYPE (IDX2)
                                           TO  API-SKJTYPE (IDX2)
               MOVE    HSACCTMREQ-NISSU  (IDX2)
                                           TO  API-NISSU (IDX2)
               MOVE    HSACCTMREQ-SAME-AFTER (IDX2)
                                           TO  API-SAME-AFTER (IDX2)
           END-PERFORM
      *
           .
       1001-SHOKUJI-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（室料差額）
      *****************************************************************
       1001-SAGAKU-SEC                 SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           PERFORM 10011-SAGAKU-CHECK-SEC
      *
           PERFORM 10011-ENDYMD-CHECK-SEC
      *
           .
       1001-SAGAKU-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（外泊）
      *****************************************************************
       1001-GAIHAKU-SEC            SECTION.
      *
           PERFORM 1001-INIT-CHECK-SEC
      *
           PERFORM 10011-GAIHAKUKBN-CHECK-SEC
      *
           PERFORM 10011-ENDYMD-CHECK-SEC
      *
           .
       1001-GAIHAKU-EXT.
           EXIT.
      *****************************************************************
      *    電文項目退避処理（入院会計作成）
      *****************************************************************
       1001-CRENACCT-SEC            SECTION.
      *
           MOVE    HSACCTMREQ-SAVEREQ
                                       TO  API-SAVEREQ
      *
           PERFORM 10011-PTNUM-CHECK-SEC
      *
           PERFORM 10011-NYUINYMD-CHECK-SEC
      *
           .
       1001-CRENACCT-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       10011-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( HSACCTMREQ-NYUINYMD =   SPACE )
               MOVE    "8001"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               MOVE    HSACCTMREQ-NYUINYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "8001"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  API-NYUINYMD
                   IF    ( HSACCTMREQ-REQNUM   =   "9" )
                       MOVE    WRK-SYMD    TO  API-SRYYMD
                   END-IF
               END-IF
      *
           END-IF
      *
           PERFORM 100111-NYUINYMD-CHECK-SEC
      *
           .
       10011-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    入院日チェック処理
      *****************************************************************
       100111-NYUINYMD-CHECK-SEC       SECTION.
      *
           IF    ( WRK-ERRCD           =   SPACE )
               PERFORM 900-PTNYUINRRK-KEY42-SEL-SEC
               IF    ( FLG-PTNYUINRRK       =   ZERO )
                   MOVE    PTNYUINRRK-REC   TO  TRRK-REC
                   MOVE    TRRK-NYUINYMD   TO  WRK-YMD
                   MOVE    "1"             TO  WRK-ZOGENPTN
                   MOVE    -1              TO  WRK-ZOGEN
                   PERFORM 800-CALENDAR-SEC
                   MOVE    WRK-KEISANYMD   TO  WRK-SRYYMD
                   PERFORM 900-PTNYUINRRK-KEY39-SEL-SEC
                   IF    ( FLG-PTNYUINRRK      =   ZERO )
                       EVALUATE    TRUE
                       WHEN  ( PTNYUINRRK-JTIKBN   NOT =   "5" )
                        AND  ( PTNYUINRRK-JTIKBN   NOT =   "6" )
                        AND  ( PTNYUINRRK-NYUINYMD =   TRRK-NYUINYMD )
                       WHEN  ( PTNYUINRRK-JTIKBN   =   "6" )
                        AND  ( PTNYUINRRK-TAIINYMD =   WRK-SRYYMD )
                           CONTINUE
                       WHEN    OTHER
                           MOVE    "8005"      TO  WRK-ERRCD
                           PERFORM 990-ERROR-EDIT-SEC
                       END-EVALUATE
                   ELSE
                       MOVE    "8005"      TO  WRK-ERRCD
                       PERFORM 990-ERROR-EDIT-SEC
                   END-IF
               ELSE
                   IF    ( HSACCTMREQ-REQNUM   =   "9" )
                       MOVE    "8005"  TO  WRK-ERRCD
                   ELSE
                       MOVE    "8010"  TO  WRK-ERRCD
                   END-IF
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
           END-IF
      *
           .
       100111-NYUINYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    診療日チェック処理
      *****************************************************************
       10011-SRYYMD-CHECK-SEC          SECTION.
      *
           IF    ( HSACCTMREQ-SRYYMD =   SPACE )
               MOVE    SMCNDATE-YMD    TO  API-SRYYMD
           ELSE
               MOVE    HSACCTMREQ-SRYYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "8009"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  API-SRYYMD
               END-IF
           END-IF
      *
           .
       10011-SRYYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       10011-PTNUM-CHECK-SEC       SECTION.
      *
           IF    ( HSACCTMREQ-PTNUM    =   SPACE )
               MOVE    "0004"            TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           ELSE
               INITIALIZE                  ORCSPTIDAREA
               MOVE    SPA-HOSPNUM     TO  SPTID-HOSPNUM
               MOVE    HSACCTMREQ-PTNUM   TO  SPTID-PTNUM
               CALL    "ORCSPTID"      USING
                                       ORCSPTIDAREA
                                       SPA-AREA
               IF    ( SPTID-RC        NOT =   ZERO )
                   MOVE    "0004"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               END-IF
      *
               MOVE    SPTID-PTNUM     TO  SPA-PTNUM
               MOVE    SPTID-PTID      TO  SPA-PTID
           END-IF
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
           IF    ( FLG-PTINF       NOT =   ZERO )
               MOVE    "0004"          TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           .
       10011-PTNUM-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    外泊区分チェック処理
      *****************************************************************
       10011-GAIHAKUKBN-CHECK-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
      *
           MOVE    HSACCTMREQ-GAIHAKUKBN
                                       TO  WRK-SNUM-INX
           PERFORM 800-ORCSNUM-SEC
           IF    ( FLG-ORCSNUM         =   ZERO )
      *
               EVALUATE    TRUE
               WHEN  ( API-SRYYMD       >=   CONST-H300401 )
                   EVALUATE    WRK-SNUM-OUTNUM
                   WHEN    0   THRU    6
                   WHEN    8   THRU    9
                   WHEN    13  THRU    17
                   WHEN    99
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "8011"  TO  WRK-ERRCD
                   END-EVALUATE
               WHEN  ( API-SRYYMD      >=   CONST-H280401 )
                   IF    ( WRK-SNUM-OUTNUM =   ZERO OR 1 OR 2  OR 3 OR 4
                                            OR 5 OR 6  OR 8
                                            OR 9 OR 13 OR 99 )
                       CONTINUE
                   ELSE
                       MOVE    "8011"  TO  WRK-ERRCD
                   END-IF
               WHEN    OTHER
                   IF    ( WRK-SNUM-OUTNUM =   ZERO OR 1 OR 2  OR 3 OR 4
                                                OR 5 OR 6  OR 7 OR 8
                                                OR 9 OR 13 OR 99 )
                       CONTINUE
                   ELSE
                       MOVE    "8011"  TO  WRK-ERRCD
                   END-IF
               END-EVALUATE
           ELSE
               MOVE    "8011"          TO  WRK-ERRCD
           END-IF
      *
           IF    ( WRK-ERRCD           =   SPACE )
               MOVE    WRK-SNUM-OUTNUM TO  WRK-GAIHAKUKBN
               MOVE    WRK-GAIHAKUKBN-G TO API-GAIHAKUKBN
           ELSE
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           .
       10011-GAIHAKUKBN-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    室料差額チェック処理
      *****************************************************************
       10011-SAGAKU-CHECK-SEC          SECTION.
      *
           MOVE    SPACE           TO  API-SAGAKUCD
           MOVE    HSACCTMREQ-SAGAKU
                                   TO  API-SAGAKU
      *
           IF    ( HSACCTMREQ-SAGAKU   =   SPACE )
               MOVE    "0024"      TO  WRK-ERRCD
           ELSE
               MOVE    HSACCTMREQ-SAGAKU
                                   TO  WRK-SNUM-INX
               PERFORM 800-ORCSNUM-SEC
               IF    ( FLG-ORCSNUM =   ZERO )
                   IF    ( WRK-SNUM-OUTNUM =   ZERO )
                       MOVE    "00"    TO  API-SAGAKUCD
                   END-IF
               ELSE
                   MOVE    "0024"      TO  WRK-ERRCD
               END-IF
      *
               IF    ( WRK-ERRCD       =   SPACE )
                AND  ( API-SAGAKUCD    =   SPACE )
                   INITIALIZE              SYSKANRI-REC
                   MOVE    "5113"      TO  SYS-KANRICD
                   MOVE    API-SRYYMD  TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
                   PERFORM 900-SYSKANRI-KEY2-SEL-SEC
                   MOVE    SYSKANRI-REC TO SYS-5113-REC
                   PERFORM UNTIL ( FLG-SYSKANRI    NOT =   ZERO )
                            OR   ( API-SAGAKUCD    NOT =   SPACE )
                       IF    ( WRK-SNUM-OUTNUM
                                   =   SYS-5113-BRM-SAGAKU-NM )
                           MOVE    SYS-5113-KBNCD
                                       TO  API-SAGAKUCD
                       END-IF
      *
                       PERFORM 900-SYSKANRI-KEY2-FET-SEC
                       MOVE    SYSKANRI-REC
                                       TO  SYS-5113-REC
                   END-PERFORM
      *
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key2"          TO  MCP-PATHNAME
                   PERFORM 910-DBCLOSECURSOR-SEC
               END-IF
           END-IF
      *
           IF    ( WRK-ERRCD       =   SPACE )
               IF    ( API-SAGAKUCD    =   SPACE )
                   MOVE     "8047" TO  WRK-ERRCD
               END-IF
           END-IF
      *
          IF    (  WRK-ERRCD       =   SPACE )
              CONTINUE
          ELSE
               PERFORM 990-ERROR-EDIT-SEC
          END-IF
      *
           .
       10011-SAGAKU-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    期間終了日チェック処理
      *****************************************************************
       10011-ENDYMD-CHECK-SEC          SECTION.
      *
           MOVE    SPACE           TO  API-ENDYMD
      *
           IF    ( HSACCTMREQ-ENDYMD   NOT =   SPACE )
               MOVE    HSACCTMREQ-ENDYMD
                                       TO  WRK-DATE
               PERFORM 800-SYMD-SEC
               PERFORM 800-HIZUKE-SEC
               IF    ( WRK-EDTYMD1     =   SPACE )
                   MOVE    "8013"      TO  WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
               ELSE
                   MOVE    WRK-SYMD    TO  API-ENDYMD
                   IF    ( API-SRYYMD (1:6)    NOT =   API-ENDYMD (1:6))
                       MOVE    "8014"  TO  WRK-ERRCD
                       PERFORM 990-ERROR-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       10011-ENDYMD-CHECK-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           INITIALIZE                  ORDER-REC
           INITIALIZE                  ODRITEM-REC
           INITIALIZE                  SODR-AREA
           INITIALIZE                  LNK-ODRITEM-REC
      *
           EVALUATE    HSACCTMREQ-REQNUM
           WHEN    "2"
               PERFORM 2001-GAIHAKU-SEC
           WHEN    "3"
               PERFORM 2001-SAGAKU-SEC
           WHEN    "4"
               PERFORM 2001-SHOKUJI-SEC
           WHEN    "9"
               PERFORM 2001-CRENACCT-SEC
           END-EVALUATE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外泊等処理
      *****************************************************************
       2001-GAIHAKU-SEC                SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    API-SRYYMD          TO  SODR-SRYYMD
           MOVE    API-ORDERID         TO  SODR-ORDERID
      *
           MOVE    API-SRYYMD          TO  SPA-SRYYMD
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM 2300-GAIHAKUKBN-SET-SEC
      *
           PERFORM 2300-ENDYMD-SET-SEC
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR03"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-GAIHAKU-EXT.
           EXIT.
      *****************************************************************
      *    室料差額処理
      *****************************************************************
       2001-SAGAKU-SEC                 SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    API-SRYYMD          TO  SODR-SRYYMD
           MOVE    API-ORDERID         TO  SODR-ORDERID
      *
           MOVE    API-SRYYMD          TO  SPA-SRYYMD
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM 2300-SAGAKU-SET-SEC
      *
           PERFORM 2300-ENDYMD-SET-SEC
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR04"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-SAGAKU-EXT.
           EXIT.
      *****************************************************************
      *    食事処理
      *****************************************************************
       2001-SHOKUJI-SEC                SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    API-SRYYMD          TO  SODR-SRYYMD
           MOVE    API-ORDERID         TO  SODR-ORDERID
      *
           MOVE    API-SRYYMD          TO  SPA-SRYYMD
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   3 )
      *
               PERFORM 2300-SKJTIME-SET-SEC
      *
               PERFORM 2300-SKJTYPE-SET-SEC
      *
               PERFORM 2300-NISSU-SET-SEC
      *
               PERFORM 2300-SAME-AFTER-SET-SEC
      *
           END-PERFORM
      *
           MOVE   IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR04"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2001-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-SHOKUJI-EXT.
           EXIT.
      *****************************************************************
      *    入院会計作成処理
      *****************************************************************
       2001-CRENACCT-SEC               SECTION.
      *
           PERFORM 2300-SODR-COMMON-SET-SEC
      *
           MOVE    API-SRYYMD          TO  SODR-SRYYMD
           MOVE    API-ORDERID         TO  SODR-ORDERID
      *
           MOVE    API-SRYYMD          TO  SPA-SRYYMD
      *
           MOVE    ZERO                TO  IDX-ITM
      *
           MOVE    IDX-ITM          TO  LNK-ODRITEM-MAX
      *
           CALL   "ORCSODR04"      USING   SODR-AREA
                                           LNK-ODRITEM-REC
                                           MCPAREA
                                           SPA-AREA
           EVALUATE    TRUE
           WHEN  ( SODR-RC         =   ZERO )
            AND  ( SODR-ERRFLG     =   ZERO )
               PERFORM 2002-RESPONSE-EDIT-SEC
           WHEN  ( SODR-RC     NOT =   ZERO )
               MOVE    "8051"      TO  WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           WHEN  ( SODR-ERRFLG  NOT =   ZERO )
               PERFORM 2400-ORDER-ERROR-EDIT-SEC
           END-EVALUATE
      *
           .
       2001-CRENACCT-EXT.
           EXIT.
      *****************************************************************
      *    食事時間区分設定処理
      *****************************************************************
       2300-SKJTIME-SET-SEC            SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
           MOVE    NCLAIMID-SKJTIMEJMA
                                       TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-SKJTIME (IDX2)  TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-SKJTIME-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事の種類区分設定処理
      *****************************************************************
       2300-SKJTYPE-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
           MOVE    NCLAIMID-SKJTYPEJMA TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-SKJTYPE (IDX2)  TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-SKJTYPE-SET-EXT.
           EXIT.
      *****************************************************************
      *    一括入力設定処理
      *****************************************************************
       2300-NISSU-SET-SEC              SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
           MOVE    NCLAIMID-NISSUJMA   TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-NISSU (IDX2)    TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-NISSU-SET-EXT.
           EXIT.
      *****************************************************************
      *    食事継続区分設定処理
      *****************************************************************
       2300-SAME-AFTER-SET-SEC         SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
           MOVE    NCLAIMID-SAME-AFTERJMA
                                       TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-SAME-AFTER (IDX2)
                                       TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-NISSU-SET-EXT.
           EXIT.
      *****************************************************************
      *    外泊区分設定処理
      *****************************************************************
       2300-GAIHAKUKBN-SET-SEC         SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-GAIHAKUKBNJMA
                                       TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-GAIHAKUKBN      TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-GAIHAKUKBN-SET-EXT.
           EXIT.
      *****************************************************************
      *    終了日設定処理
      *****************************************************************
       2300-ENDYMD-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-ENDYMDJMA
                                       TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-ENDYMD          TO  LNK-ODRITEM-ITEM (IDX-ITM)
      *
           .
       2300-ENDYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    室料差額設定処理
      *****************************************************************
       2300-SAGAKU-SET-SEC             SECTION.
      *
           PERFORM 2300-ODRITEM-COMMON-SET-SEC
      *
           MOVE    NCLAIMID-SAGAKUJMA  TO  LNK-ODRITEM-ITEMID (IDX-ITM)
           MOVE    API-SAGAKUCD        TO  LNK-ODRITEM-ITEM (IDX-ITM)
           MOVE    API-SAGAKU          TO  LNK-ODRITEM-MEMO (IDX-ITM)
      *
           .
       2300-SAGAKU-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-SODR-COMMON-SET-SEC       SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SODR-HOSPNUM
           MOVE    SUID-UID            TO  SODR-KARTE-KEY
           MOVE    1                   TO  SODR-ORDERNUM
           MOVE    "1"                 TO  SODR-NYUGAIKBN
           MOVE    SPA-PTNUM           TO  SODR-PTNUM
           MOVE    SPA-PTID            TO  SODR-PTID
           MOVE    SMCNDATE-YMD        TO  SODR-UKEYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UKEHMS
           IF    ( API-SAVEREQ         =   "1" )
               MOVE    "0"             TO  SODR-PROCKBN
           ELSE
               MOVE    "2"             TO  SODR-PROCKBN
           END-IF
           MOVE    2                   TO  SODR-KEYGENERATE
           MOVE    99                  TO  SODR-INF-DIALOG
           MOVE    "1"                 TO  SODR-SRCKBN
      *
           MOVE    "api_"              TO  SODR-OPID
           MOVE    SPA-OPID            TO  SODR-OPID(5:)
           MOVE    SMCNDATE-YMD        TO  SODR-UPYMD
           MOVE    SMCNDATE-HMS        TO  SODR-UPHMS
      *
           .
       2300-SODR-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    共通項目設定処理
      *****************************************************************
       2300-ODRITEM-COMMON-SET-SEC       SECTION.
      *
           COMPUTE    IDX-ITM  =   IDX-ITM +   1
      *
           MOVE    SPA-HOSPNUM     TO  LNK-ODRITEM-HOSPNUM  (IDX-ITM)
           MOVE    SUID-UID        TO  LNK-ODRITEM-KARTE-KEY(IDX-ITM)
           MOVE    1               TO  LNK-ODRITEM-ORDERNUM(IDX-ITM)
           MOVE    IDX-ITM         TO  LNK-ODRITEM-ITEMNUM (IDX-ITM)
      *
           .
       2300-ODRITEM-COMMON-SET-EXT.
           EXIT.
      *****************************************************************
      *    入院オーダーエラー編集処理
      *****************************************************************
       2400-ORDER-ERROR-EDIT-SEC         SECTION.
      *
           IF    ( SODR-ERRCD   NOT =  SPACE )
               MOVE    SODR-ERRCD       TO WRK-ERRCD
               PERFORM 990-ERROR-EDIT-SEC
           END-IF
      *
           PERFORM VARYING IDX9    FROM    1   BY  1
                   UNTIL ( IDX9    >   LNK-ODRITEM-MAX )
      *
               MOVE    LNK-ODRITEM-OCC (IDX9)  TO  ODRITEM-REC
               IF    ( ODRITEM-ERRCD   NOT =   SPACE )
                   MOVE    ODRITEM-ERRCD
                                        TO WRK-ERRCD
                   PERFORM 990-ERROR-EDIT-SEC
                END-IF
      *
           END-PERFORM
      *
           .
       2400-ORDER-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2001-RESPONSE-EDIT-SEC          SECTION.
      *
           INITIALIZE                  SNINF03-AREA
           MOVE    API-SRYYMD(1:6) TO  SNINF03-IN-SRYYM
           CALL    "ORCSNINF03"    USING
                                   SNINF03-AREA
                                   SPA-AREA
      *
           MOVE    SNINF03-PTNUM       TO  HSACCTMRES-PTNUM
           MOVE    SNINF03-NAME        TO  HSACCTMRES-NAME
           MOVE    SNINF03-KANANAME    TO  HSACCTMRES-KANANAME
           MOVE    SNINF03-BIRTHDAY    TO  HSACCTMRES-BIRTHDAY
           MOVE    SNINF03-SEX         TO  HSACCTMRES-SEX
      *
           PERFORM VARYING IDXA    FROM    1   BY  1
                   UNTIL ( IDXA    >   5 )
               MOVE    SNINF03-NYUINYMD (IDXA)
                                       TO  HSACCTMRES-NYUINYMD (IDXA)
               MOVE    SNINF03-TAIINYMD (IDXA)
                                       TO  HSACCTMRES-TAIINYMD (IDXA)
           END-PERFORM
      *
           MOVE    SNINF03-SRYYM       TO  HSACCTMRES-SRYYM
      *
           PERFORM VARYING IDXD    FROM    1   BY  1
                   UNTIL ( IDXD    >   31 )
      *
               MOVE    SNINF03-SRYYMD (IDXD)
                                   TO  HSACCTMRES-SRYYMD (IDXD)
               MOVE    SNINF03-SRYKA-LBL (IDXD)
                                   TO  HSACCTMRES-SRYKA-LBL (IDXD)
               MOVE    SNINF03-SRYKA-DATA(IDXD)
                                   TO  HSACCTMRES-SRYKA-DATA(IDXD)
               MOVE    SNINF03-SRYKA-NAME (IDXD)
                                   TO  HSACCTMRES-SRYKA-NAME (IDXD)
      *
               MOVE    SNINF03-BTUNUM-LBL (IDXD)
                                   TO  HSACCTMRES-BTUNUM-LBL (IDXD)
               MOVE    SNINF03-BTUNUM-DATA (IDXD)
                                   TO  HSACCTMRES-BTUNUM-DATA (IDXD)
      *
               MOVE    SNINF03-BTUNAME-LBL (IDXD)
                                   TO  HSACCTMRES-BTUNAME-LBL (IDXD)
               MOVE    SNINF03-BTUNAME-DATA (IDXD)
                                   TO  HSACCTMRES-BTUNAME-DATA (IDXD)
      *
               MOVE    SNINF03-BRMNUM-LBL (IDXD)
                                   TO  HSACCTMRES-BRMNUM-LBL (IDXD)
               MOVE    SNINF03-BRMNUM-DATA (IDXD)
                                   TO  HSACCTMRES-BRMNUM-DATA (IDXD)
      *
               MOVE    SNINF03-HKNCOMBI (IDXD)
                                   TO  HSACCTMRES-HKNCOMBI (IDXD)
      *
               MOVE    SNINF03-PTSTATUS-LBL (IDXD)
                                   TO  HSACCTMRES-PTSTATUS-LBL (IDXD)
               MOVE    SNINF03-PTSTATUS-DATA (IDXD)
                                   TO  HSACCTMRES-PTSTATUS-DATA (IDXD)
               MOVE    SNINF03-PTSTATUS-NAME (IDXD)
                                   TO  HSACCTMRES-PTSTATUS-NAME (IDXD)
      *
               MOVE    SNINF03-SKJ-ASA-LBL (IDXD)
                                   TO  HSACCTMRES-SKJ-ASA-LBL (IDXD)
               MOVE    SNINF03-SKJ-ASA-DATA (IDXD)
                                   TO  HSACCTMRES-SKJ-ASA-DATA (IDXD)
               MOVE    SNINF03-SKJ-ASA-NAME (IDXD)
                                   TO  HSACCTMRES-SKJ-ASA-NAME (IDXD)
      *
               MOVE    SNINF03-SKJ-HIRU-LBL (IDXD)
                                   TO  HSACCTMRES-SKJ-HIRU-LBL (IDXD)
               MOVE    SNINF03-SKJ-HIRU-DATA (IDXD)
                                   TO  HSACCTMRES-SKJ-HIRU-DATA (IDXD)
               MOVE    SNINF03-SKJ-HIRU-NAME (IDXD)
                                   TO  HSACCTMRES-SKJ-HIRU-NAME (IDXD)
      *
               MOVE    SNINF03-SKJ-YU-LBL (IDXD)
                                   TO  HSACCTMRES-SKJ-YU-LBL (IDXD)
               MOVE    SNINF03-SKJ-YU-DATA (IDXD)
                                   TO  HSACCTMRES-SKJ-YU-DATA (IDXD)
               MOVE    SNINF03-SKJ-YU-NAME (IDXD)
                                   TO  HSACCTMRES-SKJ-YU-NAME (IDXD)
      *
               MOVE    SNINF03-SAGAKU-LBL (IDXD)
                                   TO  HSACCTMRES-SAGAKU-LBL (IDXD)
               MOVE    SNINF03-SAGAKU-DATA (IDXD)
                                   TO  HSACCTMRES-SAGAKU-DATA (IDXD)
               MOVE    SNINF03-SAGAKU-NAME (IDXD)
                                   TO  HSACCTMRES-SAGAKU-NAME (IDXD)
      *
           END-PERFORM
      *
           PERFORM VARYING IDXH    FROM    1   BY  1
                   UNTIL ( IDXH    >   10 )
      *
               MOVE    SNINF03-HKNCOMBI-KEY (IDXH)
                                   TO  HSACCTMRES-HKNCOMBI-KEY (IDXH)
               MOVE    SNINF03-HKNNUM (IDXH)
                                   TO  HSACCTMRES-HKNNUM (IDXH)
               MOVE    SNINF03-HKNJANUM (IDXH)
                                   TO  HSACCTMRES-HKNJANUM (IDXH)
               MOVE    SNINF03-HKNJANAME (IDXH)
                                   TO  HSACCTMRES-HKNJANAME (IDXH)
               MOVE    SNINF03-HKIGO (IDXH)
                                   TO  HSACCTMRES-HKIGO (IDXH)
               MOVE    SNINF03-HNUM (IDXH)
                                   TO  HSACCTMRES-HNUM (IDXH)
      *
               PERFORM VARYING IDXK    FROM    1   BY  1
                       UNTIL ( IDXK    >   4 )
      *
                   MOVE    SNINF03-KOHNUM (IDXH IDXK)
                                   TO  HSACCTMRES-KOHNUM (IDXH IDXK)
                   MOVE    SNINF03-KOHNAME (IDXH IDXK)
                                   TO  HSACCTMRES-KOHNAME (IDXH IDXK)
                   MOVE    SNINF03-FTNJANUM (IDXH IDXK)
                                   TO  HSACCTMRES-FTNJANUM (IDXH IDXK)
                   MOVE    SNINF03-JKYSNUM (IDXH IDXK)
                                   TO  HSACCTMRES-JKYSNUM (IDXH IDXK)
      *
               END-PERFORM
      *
           END-PERFORM
      *
      *
           .
       2001-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       2002-RESPONSE-EDIT-SEC          SECTION.
      *
           PERFORM 900-PTNUM-KEY-SEL-SEC
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           PERFORM 900-NYUINACCT-KEY58-SEL-SEC
      *
           MOVE    PTNUM-PTNUM         TO  HSACCTMRES-PTNUM
           MOVE    PTINF-NAME          TO  HSACCTMRES-NAME
           MOVE    PTINF-KANANAME      TO  HSACCTMRES-KANANAME
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACCTMRES-BIRTHDAY
           MOVE    PTINF-SEX           TO  HSACCTMRES-SEX
      *
           MOVE    NACCT-SRYYM         TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACCTMRES-SRYYM
      *
           .
       2002-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    オーダー追加処理
      *****************************************************************
       290-ORDER-INSERT-SEC          SECTION.
      *
           INITIALIZE                      ORDER-REC
           MOVE    SPA-HOSPNUM         TO  ORDER-HOSPNUM
           MOVE    SUID-UID            TO  ORDER-KARTE-KEY
           MOVE    1                   TO  ORDER-ORDERNUM
           MOVE    "1"                 TO  ORDER-NYUGAIKBN
           MOVE    SPA-PTNUM           TO  ORDER-PTNUM
           MOVE    SPA-PTID            TO  ORDER-PTID
           MOVE    API-SRYYMD          TO  ORDER-SRYYMD
           MOVE    SMCNDATE-YMD        TO  ORDER-UKEYMD
           MOVE    SMCNDATE-HMS        TO  ORDER-UKEHMS
           MOVE    API-ORDERID         TO  ORDER-ORDERID
      *
           MOVE    API-ERRCD (1)       TO  ORDER-ERRCD1
      *    MOVE    API-ERRCD (2)       TO  ORDER-ERRCD2
      *
           MOVE    1                   TO  ORDER-STATUS
      *
           MOVE    2                   TO  ORDER-KEYGENERATE
           MOVE    SPACE               TO  ORDER-TERMID
           MOVE    "api_"              TO  ORDER-OPID
           MOVE    SPA-OPID            TO  ORDER-OPID(5:)
           MOVE    SMCNDATE-YMD        TO  ORDER-CREYMD
                                           ORDER-UPYMD
           MOVE    SMCNDATE-YMD        TO  ORDER-UPHMS
      *
           MOVE    ORDER-REC           TO  MCPDATA-REC
           MOVE    "tbl_order"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
      *
           .
       290-ORDER-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACCTMRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSACCTMRES-INFORMATION-TIME
      *
           IF    ( API-ERR-MAX         =   ZERO )
               MOVE    1               TO  API-ERR-MAX
               MOVE    "00"            TO  API-ERRCD (1)
           END-IF
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   API-ERR-MAX )
      *
               MOVE    API-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD   TO  HSACCTMRES-API-RESULT    (IDX1)
               MOVE    WRK-ERRMSG  TO  HSACCTMRES-API-RESULT-MSG(IDX1)
      *
           END-PERFORM
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-DATE-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-DATE
      *
           IF    ( WRK-SYMD        NOT  =   SPACE )
               INITIALIZE                  WRK-DATE
               MOVE    WRK-SYY             TO  WRK-DATE-YY
               MOVE    WRK-SMM             TO  WRK-DATE-MM
               MOVE    WRK-SDD             TO  WRK-DATE-DD
               MOVE    "-"                 TO  WRK-DATE-FL1
               MOVE    "-"                 TO  WRK-DATE-FL2
      *
               EVALUATE    WRK-SYMD
               WHEN    "00000000"
                   MOVE    01              TO  WRK-DATE-MM
                   MOVE    01              TO  WRK-DATE-DD
               WHEN    "99999999"
                   MOVE    12              TO  WRK-DATE-MM
                   MOVE    31              TO  WRK-DATE-DD
               END-EVALUATE
      *
           END-IF
      *
           .
       800-DATE-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-SYMD-SEC                    SECTION.
      *
           MOVE    SPACE               TO  WRK-SYMD
      *
           IF    ( WRK-DATE        NOT  =   SPACE )
               MOVE    WRK-DATE-YY     TO  WRK-SYY
               MOVE    WRK-DATE-MM     TO  WRK-SMM
               MOVE    WRK-DATE-DD     TO  WRK-SDD
           END-IF
      *
           .
       800-SYMD-EXT.
           EXIT.
      *****************************************************************
      *    カレンダー処理
      *****************************************************************
       800-CALENDAR-SEC                SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
           MOVE    WRK-ZOGENPTN        TO  LNK-DAY6-ZOGENPTN
           MOVE    WRK-ZOGEN           TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-KEISANYMD
      *
           .
       800-CALENDAR-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       800-HIZUKE-SEC                      SECTION.
      *
           MOVE    SPACE           TO  WRK-EDTYMD1
      *
           IF  ( WRK-SYMD          =   ALL "9"  OR   ZERO )
               MOVE    WRK-SYMD (1:8)
                                   TO  WRK-EDTYMD1
           ELSE
               INITIALIZE              STS-AREA-DAY
               INITIALIZE              LNK-DAY2-AREA
               MOVE    "21"        TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD    TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"       USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
               IF    ( STS-DAY-RC1     =   ZERO )
                   MOVE    LNK-DAY2-EDTYMD1
                                   TO  WRK-EDTYMD1
               END-IF
           END-IF
      *
           .
       800-HIZUKE-EXT.
           EXIT.
      *****************************************************************
      *    時刻編集処理
      *****************************************************************
       800-TIME-SEC                    SECTION.
      *
           INITIALIZE                      WRK-TIME
           MOVE    WRK-HMS-HH          TO  WRK-TIME-HH
           MOVE    WRK-HMS-MM          TO  WRK-TIME-MM
           MOVE    WRK-HMS-SS          TO  WRK-TIME-SS
           MOVE    ":"                 TO  WRK-TIME-FL1
           MOVE    ":"                 TO  WRK-TIME-FL2
      *
           .
       800-TIME-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値変換処理
      *****************************************************************
       800-ORCSNUM-SEC                 SECTION.
      *
           MOVE    ZERO                TO  FLG-ORCSNUM
           MOVE    ZERO                TO  WRK-SNUM-OUTNUM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SNUM-INX        TO  SNUM-INX
           CALL    "ORCSNUM"           USING   ORCSNUMAREA
           IF    ( SNUM-RC         NOT =   ZERO  )
            OR   ( SNUM-MINKBN     NOT =   SPACE )
            OR   ( SNUM-SYOKBN     NOT =   SPACE )
               MOVE    1               TO  FLG-ORCSNUM
           ELSE
               MOVE    SNUM-OUTNUM     TO  WRK-SNUM-OUTNUM
           END-IF
      *
           .
      *
       800-ORCSNUM-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           INITIALIZE                      SODRS03-AREA
           MOVE    WRK-ERRCD           TO  SODRS03-ERRCD
           CALL    "ORCSODRS03"            USING
                                           SODRS03-AREA
           MOVE    SODRS03-ERRCD       TO  WRK-ERRCD
           MOVE    SODRS03-ERRMSG      TO  WRK-ERRMSG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *****************************************************************
      *    XML情報から内容を取り出す
      *****************************************************************
       900-XML-READ-SEC             SECTION.
      *
           IF    ( I4API02-BODY     NOT =   LOW-VALUE )
               DISPLAY "hsacctmodv2 object is not low_value"
           END-IF
      *
           INITIALIZE                      XML-APIREQ
           MOVE    "xml_hsacctmodv2req"
                                           TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    I4API02-BODY            TO  APIREQ-OBJECT
           MOVE    ZERO                    TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           MOVE    "XMLREAD"               TO  MCP-FUNC
           PERFORM 910-XMLREAD-SEC
           IF    ( MCP-RC              =   ZERO  OR  1 )
               PERFORM 9001-XML-FILTER-SEC
           ELSE
               DISPLAY "XMLREAD failure = " MCP-RC
               MOVE   "8098"            TO  WRK-ERRCD
           END-IF
      *
           MOVE    LOW-VALUE               TO  I4API02-BODY
      *
           .
       900-XML-READ-EXT.
           EXIT.
      *****************************************************************
      *    XMLフィルタリング処理
      *****************************************************************
       9001-XML-FILTER-SEC             SECTION.
      *
           MOVE    APIREQ-PATIENTINFOREQ
                                       TO  HSACCTMREQ-PRIVATE-OBJECTS
      *
           .
       9001-XML-FILTER-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    "リクエスト番号"
                               TO  HSACCTMRES-REQNUM-LBL
      *
           MOVE    HSACCTMREQ-REQNUM
                               TO  HSACCTMRES-REQNUM-DATA
      *
           EVALUATE    HSACCTMREQ-REQNUM
           WHEN    "2"
               MOVE    "外泊"
                               TO  HSACCTMRES-REQNUM-NAME
           WHEN    "3"
               MOVE    "室料差額"
                               TO  HSACCTMRES-REQNUM-NAME
           WHEN    "4"
               MOVE    "食事"
                               TO  HSACCTMRES-REQNUM-NAME
           WHEN    "9"
               MOVE    "入院会計作成"
                               TO  HSACCTMRES-REQNUM-NAME
           END-EVALUATE
      *
           PERFORM 9001-XML-WRITE-SEC
      *
           .
       900-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       9001-XML-WRITE-SEC             SECTION.
      *
           INITIALIZE                          XML-APIREQ
           MOVE    "xml_hsacctmodv2res"    TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           MOVE    HSACCTMRES-PRIVATE-OBJECTS TO APIREQ-PATIENTINFOREQ
           MOVE    1                       TO  APIREQ-MODE
           MOVE    "private_objects"       TO  APIREQ-RECORDNAME
           PERFORM 910-XMLWRITE-SEC
           IF     (MCP-RC              =   ZERO  OR  1  )
               MOVE    APIREQ-OBJECT       TO  I4API02-BODY
               MOVE    "application/xml"   TO  I4API02-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       9001-XML-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYSKANRI-KEY10-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理
      *****************************************************************
       900-SYSKANRI-KEY2-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理FETCH処理
      *****************************************************************
       900-SYSKANRI-KEY2-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者番号取得処理
      *****************************************************************
       900-PTNUM-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNUM
      *
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-PTID            TO  PTNUM-PTID
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNUM-REC
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNUM-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者情報取得処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTINF
      *
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTINF-REC
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY39-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    API-NYUINYMD        TO  PTNYUINRRK-NYUINYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key39"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY39-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY42-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
           MOVE    API-SRYYMD          TO  PTNYUINRRK-TENNYUYMD
                                           PTNYUINRRK-TENSTUYMD
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
      *
       900-PTNYUINRRK-KEY42-SEL-EXT.
           EXIT.
      *****************************************************************
      *    入院会計検索処理
      *****************************************************************
       900-NYUINACCT-KEY58-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-NYUINACCT
      *
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    SPA-PTID            TO  NACCT-PTID
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
           ELSE
               MOVE    1               TO  FLG-NYUINACCT
               INITIALIZE                  NYUINACCT-REC
           END-IF
      *
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key58"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-NYUINACCT-KEY58-SEL-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF    ( MCP-RC          =   ZERO )
               PERFORM 910-DBFETCH-SEC
           END-IF
      *
           .
      *
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FETCHはしない）
      *****************************************************************
       911-DBSELECT-SEC               SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       911-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DBCLOSECURSOR-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ読込処理
      *****************************************************************
       910-XMLREAD-SEC                 SECTION.
      *
           MOVE    "XMLREAD"       TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLREAD-EXT.
           EXIT.
      *****************************************************************
      *    ＸＭＬ書込処理
      *****************************************************************
       910-XMLWRITE-SEC                SECTION.
      *
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-APIREQ
      *
           .
      *
       910-XMLWRITE-EXT.
           EXIT.
      *****************************************************************
      *    エラー編集処理
      *****************************************************************
       990-ERROR-EDIT-SEC              SECTION.
      *
           IF    ( API-ERR-MAX     <   CONST-ERR-MAX )
      *
               PERFORM VARYING IDX8    FROM    1   BY  1
                       UNTIL ( IDX8    >   CONST-ERR-MAX )
                        OR   ( API-ERRCD  (IDX8)
                                       =   WRK-ERRCD )
      *
                   IF    ( API-ERRCD  (IDX8)   =   SPACE )
                       MOVE    WRK-ERRCD       TO  API-ERRCD  (IDX8)
                       MOVE    IDX8            TO  API-ERR-MAX
                       MOVE    CONST-ERR-MAX   TO  IDX8
                   END-IF
      *
               END-PERFORM
      *
           END-IF
      *
           .
       990-ERROR-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           IF    ( WRK-ERRCD           =   "8099" )
               MOVE    404             TO  I4API02-HTTPSTATUS
           ELSE
               PERFORM 9901-EXIT-PROGRAM-SEC
           END-IF
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
      *****************************************************************
      *    プログラム終了処理（エラー時）
      *****************************************************************
       9901-EXIT-PROGRAM-SEC            SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE            TO  HSACCTMRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS        TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME            TO  HSACCTMRES-INFORMATION-TIME
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   API-ERR-MAX )
      *
               MOVE    API-ERRCD  (IDX1)
                                       TO  WRK-ERRCD
               PERFORM 890-ERRCD-MSG-SEC
               MOVE    WRK-ERRCD   TO  HSACCTMRES-API-RESULT    (IDX1)
               MOVE    WRK-ERRMSG  TO  HSACCTMRES-API-RESULT-MSG(IDX1)
      *
           END-PERFORM
      *
           PERFORM 900-XML-WRITE-SEC
      *
           .
       9901-EXIT-PROGRAM-EXT.
           EXIT.
