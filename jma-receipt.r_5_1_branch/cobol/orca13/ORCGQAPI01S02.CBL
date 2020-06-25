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
       PROGRAM-ID.         ORCGQAPI01S02.
      *****************************************************************
      *  システム名        : 日医標準レセプトソフト
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 患者照会
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/10/31    NACL−太田　新規作成
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
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "Q01SCR-SPA".
      *
       01  FLG-AREA.
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-ENTRYCHK            PIC 9(01).
           03  FLG-WILD-CARD           PIC 9(01).
           03  FLG-KIKANOVER           PIC 9(01).
           03  FLG-APIPARA             PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-PTSRH               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTPUBLIC            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDX1                    PIC 9(05).
           03  IDX2                    PIC 9(05).
           03  IDXL                    PIC 9(05).
      *
       01  CNT-AREA.
           03  CNT-SEQNUM              PIC 9(07).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG              PIC X(08).
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
           03  WRK-PTNUM               PIC X(20).
           03  WRK-PTNUM-WK            PIC X(20).
           03  WRK-PTNUM-LENGTH        PIC 9(05).
           03  WRK-RANGE-FROM          PIC 9(07).
           03  WRK-RANGE-OFFSET        PIC 9(07).
           03  WRK-RANGE-TO            PIC 9(07).
           03  WRK-RANGE-LIMIT         PIC 9(07).
           03  WRK-HKNCOMBINUM         PIC X(04).
           03  WRK-NUMZ                PIC Z(14)9.
      *
       01  RES-AREA.
           03  RES-RESNUM              PIC X(02).
           03  RES-COUNT               PIC X(07).
      *
      *    固定値領域
       01  CONST-AREA.
           03  CONST-LIMIT             PIC 9(05)   VALUE 200.
      *
       01  RESULT-REC.
           COPY    "CPPTSRH.INC"       REPLACING   //PTSRH-//
                                       BY          //RESULT-//.
      *
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY   //TMPSPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
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
      *    保険組合せセット領域
           COPY    "CPORCSAPIHKN.INC".
      *
      *    API XML書き込み用定義体
           COPY    "CPFINDV3RES.INC".
      *
      *    最終受診日取得
           COPY    "CPORCSLASTYMD.INC".
      *
      *    保険組合せ編集サブ
           COPY    "CPORCSHKNSET.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK5001.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    ＡＰＩパラメタ
       01  APIPARA-REC.
           COPY    "CPAPIPARA.INC".
           COPY    "CPPARA013.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者地域連携
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    患者照会
       01  PTSRH-REC.
           COPY    "CPPTSRH.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
           COPY    "CPAPIV2REQ.INC".
      *
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                 SECTION.
      *
       01  SCRAREA.
           COPY    "ORCA13SCRAREA.INC".
      *
           COPY    "CPFINDV3REQ.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                   DIVISION    USING
                                               SCRAREA
                                               XML-FINDV3REQ
                                               SPA-AREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-MAIN-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  RES-AREA
      *
           MOVE    SPACE           TO  SPA-Q01FREE
           INITIALIZE                  SPA-Q01FREE
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *
      *    主処理
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 200-MAIN-SEC
           END-IF
      *
      *    返却領域編集
           PERFORM 300-END-SEC
      *
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
           INITIALIZE                  XML-FINDV3RES
      *
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-STATE-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2001-INPUT-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2002-KANREN-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
               PERFORM 2003-RESPONSE-EDIT-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    状態確認処理
      *****************************************************************
       2001-STATE-CHK-SEC              SECTION.
      *
           IF    ( FINDREQ-ORCA-UID   =    SPACE )
               MOVE    "0205"              TO  SPA-ERRCD
               MOVE    "01"                TO  RES-RESNUM
           END-IF
      *
           IF    ( FINDREQ-KARTE-UID   =   SPACE )
               MOVE    "0203"              TO  SPA-ERRCD
               MOVE    "01"                TO  RES-RESNUM
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-APIPARA-KEY5-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   EVALUATE    PARA013-STATE
                   WHEN    "01"
                       MOVE    "1040"      TO  SPA-ERRCD
                   WHEN    "02"
                       CONTINUE
                   WHEN    OTHER
                       MOVE    "0206"      TO  SPA-ERRCD
                       MOVE    "01"        TO  RES-RESNUM
                   END-EVALUATE
               ELSE
                   MOVE    "0206"          TO  SPA-ERRCD
                   MOVE    "01"            TO  RES-RESNUM
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-APIPARA-KEY7-SEL-SEC
               MOVE    APIPARA-DATA-REC    TO  PARA013-REC
               IF    ( FLG-APIPARA         =   ZERO )
                   MOVE    "1038"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               PERFORM 900-PTSRH-KEY2-SEL-SEC
               MOVE    PTSRH-REC           TO  RESULT-REC
               IF    ( RESULT-OUTKBN       =   "3" )
                   MOVE    RESULT-CNT-SRYCD
                                       TO  WRK-NUMZ
                   MOVE    FUNCTION TRIM (WRK-NUMZ)
                                       TO  RES-COUNT
               ELSE
                   MOVE    RESULT-CNT-ALL  TO  WRK-NUMZ
                   MOVE    FUNCTION TRIM (WRK-NUMZ)
                                       TO  RES-COUNT
               END-IF
      *
               IF    ( RESULT-DBERR    NOT =   ZERO )
                   MOVE    "1049"      TO  SPA-ERRCD
                   MOVE    "01"        TO  RES-RESNUM
               END-IF
      *
               IF    ( SPA-ERRCD           =   SPACE )
                   IF    ( RESULT-CNT-ALL      =   ZERO )
                       MOVE    "1041"          TO  SPA-ERRCD
                       MOVE    "01"            TO  RES-RESNUM
                   END-IF
               END-IF
           END-IF
      *
           .
       2001-STATE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入力項目チェック処理
      *****************************************************************
       2001-INPUT-CHK-SEC              SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
              PERFORM 2001-RANGE-FROM-CHK-SEC
           END-IF
      *
           IF    ( SPA-ERRCD       =   SPACE )
              PERFORM 2001-RANGE-TO-CHK-SEC
           END-IF
      *
           .
       2001-INPUT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    開始件数チェック処理
      *****************************************************************
       2001-RANGE-FROM-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-RANGE-FROM  =   SPACE )
               MOVE    0               TO  WRK-RANGE-OFFSET
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    FINDREQ-RANGE-FROM
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "1042"      TO  SPA-ERRCD
               ELSE
                   IF    ( SNUM-OUTNUM     <= 1 )
                       MOVE    0       TO  WRK-RANGE-OFFSET
                   ELSE
                       COMPUTE WRK-RANGE-OFFSET
                           =   SNUM-OUTNUM
                           -   1
                   END-IF
               END-IF
           END-IF
      *
           IF    ( SPA-ERRCD           =   SPACE )
               IF  ( WRK-RANGE-OFFSET >=   RESULT-CNT-ALL )
                   MOVE    "1044"      TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-RANGE-FROM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    終了件数チェック処理
      *****************************************************************
       2001-RANGE-TO-CHK-SEC         SECTION.
      *
           IF    ( FINDREQ-RANGE-TO    =   SPACE )
               MOVE    CONST-LIMIT     TO  WRK-RANGE-LIMIT
           ELSE
               INITIALIZE                  ORCSNUMAREA
               MOVE    FINDREQ-RANGE-TO
                                       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF  ( SNUM-RC       NOT =   ZERO  )
                OR ( SNUM-MINKBN   NOT =   SPACE )
                OR ( SNUM-SYOKBN   NOT =   SPACE )
                   MOVE    "1043"      TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTNUM TO  WRK-RANGE-TO
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   IF    ( WRK-RANGE-TO 
                                       <   WRK-RANGE-OFFSET + 1 )
                       MOVE    "1046"  TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               IF    ( SPA-ERRCD       =   SPACE )
                   COMPUTE WRK-RANGE-LIMIT
                       =   WRK-RANGE-TO
                       -   WRK-RANGE-OFFSET
                   IF    ( CONST-LIMIT <   WRK-RANGE-LIMIT )
                       MOVE    CONST-LIMIT TO  WRK-RANGE-LIMIT
                   END-IF
               END-IF
           END-IF
      *
           COMPUTE WRK-RANGE-FROM = WRK-RANGE-OFFSET + 1
      *
           .
       2001-RANGE-TO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       2002-KANREN-CHK-SEC             SECTION.
      *
      *
           .
       2002-KANREN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    検索処理編集処理
      *****************************************************************
       2003-RESPONSE-EDIT-SEC          SECTION.
      *
           MOVE    WRK-RANGE-FROM      TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                       TO  FINDRES-RANGE-FROM
      *
           COMPUTE CNT-SEQNUM = WRK-RANGE-FROM
      *
           PERFORM 900-PTSRH-ODR-SEL-SEC
      *
           MOVE    ZERO                TO  IDXL
           PERFORM UNTIL ( FLG-PTSRH       NOT =   ZERO )
                    OR   ( IDXL        >=  CONST-LIMIT )
      *
               EVALUATE    RESULT-OUTKBN
               WHEN    "2"
                   PERFORM 20032-RESPONSE-SEC
               WHEN    "3"
                   PERFORM 20033-RESPONSE-SEC
               WHEN    OTHER
                   PERFORM 20031-RESPONSE-SEC
               END-EVALUATE
      *
               PERFORM 900-PTSRH-ODR-FET-SEC
      *
               COMPUTE CNT-SEQNUM  =  CNT-SEQNUM   +   1
      *
           END-PERFORM
      *
           MOVE    "tbl_ptsrh"     TO  MCP-TABLE
           MOVE    "odr"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2003-RESPONSE-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理
      *****************************************************************
       20031-RESPONSE-SEC                  SECTION.
      *
           COMPUTE IDXL  =   IDXL  +   1
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           PERFORM 900-PTNUM-PUBLIC-KEY-SEL-SEC
      *
           MOVE    CNT-SEQNUM      TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  FINDRES-SEQNUM (IDXL)
                                       FINDRES-RANGE-TO
      *
           MOVE    PTSRH-PTNUM     TO  FINDRES-PTNUM (IDXL)
           MOVE    PTSRH-KANANAME  TO  FINDRES-KANANAME (IDXL)
           MOVE    PTINF-NAME      TO  FINDRES-NAME (IDXL)
           MOVE    PTSRH-SEX       TO  FINDRES-SEX (IDXL)
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDRES-BIRTHDAY (IDXL)
      *
           MOVE    SPACE           TO  WRK-HKNCOMBINUM
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    PTINF-PTID      TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
           IF    ( ORCSLASTYMD-HKNCOMBI    NOT =   SPACE )
               MOVE    ORCSLASTYMD-HKNCOMBI
                                       TO  WRK-HKNCOMBINUM
               PERFORM 200311-HOKEN-EDIT-SEC
           ELSE
               INITIALIZE              ORCSHKNSETAREA
               MOVE    SPA-AREA        TO  TMPSPA-AREA
      *
               MOVE    SPACE           TO  TMPSPA-SRYKA
               MOVE    "2"             TO  TMPSPA-NYUGAIKBN
               MOVE    PTINF-PTID      TO  TMPSPA-PTID
               MOVE    PTINF-BIRTHDAY  TO  TMPSPA-BIRTHDAY
               MOVE    SPACE           TO  TMPSPA-HKNCOMBINUM
               MOVE    "1"             TO  ORCSHKN-KBN
               CALL    "ORCSHKNSET"    USING
                                       TMPSPA-AREA
                                       ORCSHKNSETAREA
               IF    ( ORCSHKN-ERRCD       =   ZERO )
                AND  ( ORCSHKN-IDX     NOT =   ZERO )
                   MOVE    ORCSHKN-GMN-HKNCOMBINUM (ORCSHKN-IDX)
                                       TO  WRK-HKNCOMBINUM
                   PERFORM 200311-HOKEN-EDIT-SEC
               END-IF
           END-IF
      *
           IF    ( ORCSLASTYMD-BRMNUM  NOT =   SPACE )
      *
               MOVE    ORCSLASTYMD-BRMNUM (1:2)
                                   TO  FINDRES-BTUNUM (IDXL)
               MOVE    ORCSLASTYMD-BRMNUM (4:)
                                   TO  FINDRES-BRMNUM (IDXL)
      *
               INITIALIZE              SYSKANRI-REC
               MOVE    SPA-HOSPNUM TO  SYS-HOSPNUM
               MOVE    "5001"      TO  SYS-KANRICD
               MOVE    ORCSLASTYMD-BRMNUM (1:2)
                                   TO  SYS-KBNCD
               MOVE    SPA-SYSYMD  TO  SYS-STYUKYMD
                                       SYS-EDYUKYMD
               PERFORM 900-SYSKANRI-KEY10-SEL-SEC
               MOVE    SYSKANRI-REC TO SYS-5001-REC
               MOVE    SYS-5001-BTU-NAME
                                   TO  FINDRES-BTUNAME (IDXL)
      *
           ELSE
               MOVE    ORCSLASTYMD-LASTYMD
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  FINDRES-LASTYMD (IDXL)
           END-IF
      *
           MOVE    PTINF-HOME-TEL1 TO  FINDRES-TEL (IDXL)
      *
           MOVE    PTINF-HOME-POST
                                   TO  FINDRES-ZIPCODE (IDXL)
      *
           MOVE    PTINF-HOME-ADRS TO  FINDRES-ADD1 (IDXL)
      *
           MOVE    PTINF-HOME-BANTI
                                   TO  FINDRES-ADD2 (IDXL)
      *
      *    地域連携ＩＤ
           MOVE    PTPUBLIC-PATIENT-ID1
                                   TO  FINDRES-PATIENT-ID1(IDXL)
      *
           .
       20031-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（点数検索）
      *****************************************************************
       20032-RESPONSE-SEC                  SECTION.
      *
           COMPUTE IDXL  =   IDXL  +   1
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    CNT-SEQNUM      TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  FINDRES-SEQNUM (IDXL)
                                       FINDRES-RANGE-TO
      *
           MOVE    PTSRH-PTNUM     TO  FINDRES-PTNUM (IDXL)
           MOVE    PTSRH-KANANAME  TO  FINDRES-KANANAME (IDXL)
           MOVE    PTINF-NAME      TO  FINDRES-NAME (IDXL)
           MOVE    PTSRH-SEX       TO  FINDRES-SEX (IDXL)
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDRES-BIRTHDAY (IDXL)
      *
           IF    ( PTSRH-NYUGAIKBN =   "1" )
               MOVE    "I"         TO  FINDRES-NYUGAIKBN (IDXL)
           ELSE
               MOVE    "O"         TO  FINDRES-NYUGAIKBN (IDXL)
           END-IF
      *
           IF    ( PTSRH-HKNID     NOT =   ZERO )
               PERFORM 200312-HOKEN-EDIT-SEC
           END-IF
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   4 )
                    OR   ( PTSRH-KOHID (IDX2)  =   ZERO )
               PERFORM 200312-KOHI-EDIT-SEC
           END-PERFORM
      *
           PERFORM VARYING IDX2    FROM    1   BY  1
                   UNTIL ( IDX2    >   5 )
                    OR   ( PTSRH-TOKKI (IDX2)  =   ZERO )
               MOVE    PTSRH-TOKKI   (IDX2)
                                   TO  FINDRES-TOKKI (IDXL IDX2)
               MOVE    PTSRH-TOKKINM (IDX2)
                                   TO  FINDRES-TOKKINM (IDXL IDX2)
           END-PERFORM
      *
           MOVE    PTSRH-TOTALTEN  TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  FINDRES-TOTALTEN (IDXL)
      *
           .
       20032-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    レスポンス編集処理（診療行為検索）
      *****************************************************************
       20033-RESPONSE-SEC              SECTION.
      *
           COMPUTE IDXL  =   IDXL  +   1
      *
           PERFORM 900-PTINF-KEY-SEL-SEC
      *
           MOVE    CNT-SEQNUM      TO  WRK-NUMZ
           MOVE    FUNCTION TRIM (WRK-NUMZ)
                                   TO  FINDRES-SEQNUM (IDXL)
                                       FINDRES-RANGE-TO
      *
           MOVE    PTSRH-PTNUM     TO  FINDRES-PTNUM (IDXL)
           MOVE    PTSRH-KANANAME  TO  FINDRES-KANANAME (IDXL)
           MOVE    PTINF-NAME      TO  FINDRES-NAME (IDXL)
           MOVE    PTSRH-SEX       TO  FINDRES-SEX (IDXL)
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDRES-BIRTHDAY (IDXL)
      *
           MOVE    PTSRH-SRYCD     TO  FINDRES-SRYCD (IDXL)
           MOVE    PTSRH-SRYNAME   TO  FINDRES-SRYNAME (IDXL)
           MOVE    PTSRH-R4-KAISUTOTAL
                                   TO  FINDRES-KAISUTOTAL (IDXL)
           MOVE    PTSRH-R4-SURYOTOTAL
                                   TO  FINDRES-SURYOTOTAL (IDXL)
           IF    ( PTSRH-R4-SURYO-EDIT =   1 )
               MOVE    PTSRH-R4-TANICD
                                   TO  FINDRES-TANICD (IDXL)
               MOVE    PTSRH-R4-TANINAME
                                   TO  FINDRES-TANINAME (IDXL)
           END-IF
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   10 )
                    OR   ( PTSRH-R4-SRYYMD (IDX0)
                                   =   SPACE )
      *
               MOVE    PTSRH-R4-SRYYMD (IDX0)
                                   TO  WRK-SYMD
               PERFORM 800-DATE-SEC
               MOVE    WRK-DATE    TO  FINDRES-SRYYMD (IDXL IDX0)
               IF    ( PTSRH-R4-SURYO-EDIT =   1 )
                   MOVE    FUNCTION TRIM(PTSRH-R4-SRYSURYO (IDX0))
                                   TO  FINDRES-SRYSURYO (IDXL IDX0)
               ELSE
                   MOVE    "1"     TO  FINDRES-SRYSURYO (IDXL IDX0)
               END-IF
               MOVE    FUNCTION TRIM(PTSRH-R4-ZAIKAISU (IDX0))
                                   TO  FINDRES-ZAIKAISU (IDXL IDX0)
      *
           END-PERFORM
      *
           .
       20033-RESPONSE-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       200311-HOKEN-EDIT-SEC           SECTION.
      *
           INITIALIZE                  ORCSAPIHKNAREA
           MOVE    PTSRH-PTID      TO  SPA-PTID
           MOVE    WRK-HKNCOMBINUM TO  ORCSAPIHKN-IN-HKNCOMBI
           MOVE    "2"             TO  ORCSAPIHKN-KBN
           CALL    "ORCSAPIHKN"    USING
                                   SPA-AREA
                                   ORCSAPIHKNAREA
           MOVE    ZERO            TO  SPA-PTID
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   20 )
                     OR  ( ORCSAPIHKN-THKNCOMBI (IDX1)
                                   =   SPACE  )
               IF    ( WRK-HKNCOMBINUM
                                   =   ORCSAPIHKN-THKNCOMBI (IDX1))
      *
                   MOVE    ORCSAPIHKN-HKNNUM (IDX1)
                                   TO  FINDRES-HKNNUM (IDXL)
                   MOVE    ORCSAPIHKN-HKNJANUM (IDX1)
                                   TO  FINDRES-HKNJANUM (IDXL)
                   MOVE    ORCSAPIHKN-HKNNUM-NAME (IDX1)
                                   TO  FINDRES-HKNNAME (IDXL)
                   MOVE    ORCSAPIHKN-CONTKBN  (IDX1)
                                   TO  FINDRES-CONTKBN (IDXL)
                   MOVE    ORCSAPIHKN-HOJOKBN  (IDX1)
                                   TO  FINDRES-HOJOKBN (IDXL)
                   MOVE    ORCSAPIHKN-HONKZKKBN  (IDX1)
                                   TO  FINDRES-HONKZKKBN (IDXL)
                   MOVE    ORCSAPIHKN-HIHKNJANAME  (IDX1)
                                   TO  FINDRES-HIHKNJANAME (IDXL)
                   MOVE    ORCSAPIHKN-TEKSTYMD  (IDX1)
                                   TO  FINDRES-TEKSTYMD (IDXL)
                   MOVE    ORCSAPIHKN-TEKEDYMD  (IDX1)
                                   TO  FINDRES-TEKEDYMD (IDXL)
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL     ( IDX2    >   4 )
                       MOVE    ORCSAPIHKN-KOHNUM (IDX1 IDX2)
                                   TO  FINDRES-KOHNUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOHNUM-NAME (IDX1 IDX2)
                                   TO  FINDRES-KOHNAME (IDXL IDX2)
                       MOVE    ORCSAPIHKN-FTNJANUM (IDX1 IDX2)
                                   TO  FINDRES-FTNJANUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-JKYSNUM (IDX1 IDX2)
                                   TO  FINDRES-JKYSNUM (IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOH-TEKSTYMD (IDX1 IDX2)
                                   TO  FINDRES-KOH-TEKSTYMD (IDXL IDX2)
                       MOVE    ORCSAPIHKN-KOH-TEKEDYMD (IDX1 IDX2)
                                   TO  FINDRES-KOH-TEKEDYMD (IDXL IDX2)
                   END-PERFORM
      *
               END-IF
      *
           END-PERFORM
      *
           .
       200311-HOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       200312-HOKEN-EDIT-SEC           SECTION.
      *
           PERFORM 900-PTHKNINF-KEY-SEL-SEC
      *
           INITIALIZE                  HKNNUM-REC
           MOVE   PTHKN-HOSPNUM    TO  HKN-HOSPNUM
           MOVE   PTHKN-HKNNUM     TO  HKN-HKNNUM
           MOVE   "00"             TO  HKN-PAYKBN
           MOVE   PTHKN-TEKSTYMD   TO  HKN-TEKSTYMD
                                       HKN-TEKEDYMD
           PERFORM 900-HKNNUM-KEY-SEL-SEC
      *
           MOVE    PTHKN-HKNNUM
                                   TO  FINDRES-HKNNUM (IDXL)
           MOVE    PTHKN-HKNJANUM
                                   TO  FINDRES-HKNJANUM (IDXL)
           MOVE    HKN-TANSEIDONAME
                                   TO  FINDRES-HKNNAME (IDXL)
           MOVE    PTHKN-CONTKBN
                                   TO  FINDRES-CONTKBN (IDXL)
           MOVE    PTHKN-HOJOKBN
                                   TO  FINDRES-HOJOKBN (IDXL)
           MOVE    PTHKN-HONKZKKBN
                                   TO  FINDRES-HONKZKKBN (IDXL)
           MOVE    PTHKN-HIHKNJANAME
                                   TO  FINDRES-HIHKNJANAME (IDXL)
           MOVE    PTHKN-TEKSTYMD
                                   TO  FINDRES-TEKSTYMD (IDXL)
           MOVE    PTHKN-TEKEDYMD
                                   TO  FINDRES-TEKEDYMD (IDXL)
      *
           .
       200312-HOKEN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       200312-KOHI-EDIT-SEC            SECTION.
      *
           PERFORM 900-PTKOHINF-KEY-SEL-SEC
      *
           INITIALIZE                  HKNNUM-REC
           MOVE   PTKOH-HOSPNUM    TO  HKN-HOSPNUM
           MOVE   PTKOH-KOHNUM     TO  HKN-HKNNUM
           MOVE   PTKOH-PAYKBN     TO  HKN-PAYKBN
           MOVE   PTKOH-TEKSTYMD   TO  HKN-TEKSTYMD
                                       HKN-TEKEDYMD
           PERFORM 900-HKNNUM-KEY-SEL-SEC
      *
           MOVE    PTKOH-KOHNUM
                                   TO  FINDRES-KOHNUM (IDXL IDX2)
           MOVE    HKN-TANSEIDONAME
                                   TO  FINDRES-KOHNAME (IDXL IDX2)
           MOVE    PTKOH-FTNJANUM
                                   TO  FINDRES-FTNJANUM (IDXL IDX2)
           MOVE    PTKOH-JKYSNUM
                                   TO  FINDRES-JKYSNUM (IDXL IDX2)
           MOVE    PTKOH-TEKSTYMD
                                   TO  FINDRES-KOH-TEKSTYMD (IDXL IDX2)
           MOVE    PTKOH-TEKEDYMD
                                   TO  FINDRES-KOH-TEKEDYMD (IDXL IDX2)
      *
           .
       200312-KOHI-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
               MOVE    "0000"      TO  SPA-ERRCD
           END-IF
      *
           PERFORM 900-XML-WRITE-SEC
      *
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
           CALL  "ORCSDAY"             USING   STS-AREA-DAY
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
           EXIT.
      *****************************************************************
      *    エラーメッセージ編集処理
      *****************************************************************
       890-ERRCD-MSG-SEC            SECTION.
      *
           MOVE    SPA-MOTOPG      TO  WRK-MOTOPG
           MOVE    "API"           TO  SPA-MOTOPG
           CALL    "ORCGQERRMSG"   USING
                                   SPA-AREA
                                   SPA-Q01FREE
           MOVE    WRK-MOTOPG      TO  SPA-MOTOPG
      *
           .
       890-ERRCD-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    XML情報を書き出す
      *****************************************************************
       900-XML-WRITE-SEC             SECTION.
      *
           MOVE    FINDREQ-REQNUM      TO  FINDRES-REQNUM
           IF    ( RES-RESNUM          =   SPACE )
               MOVE    "02"            TO  FINDRES-RESNUM
           ELSE
               MOVE    "01"            TO  FINDRES-RESNUM
           END-IF
           MOVE    FINDREQ-KARTE-UID   TO  FINDRES-KARTE-UID
           MOVE    FINDREQ-ORCA-UID    TO  FINDRES-ORCA-UID
      *
           MOVE    SMCNDATE-YMD    TO  WRK-SYMD
           PERFORM 800-DATE-SEC
           MOVE    WRK-DATE        TO  FINDRES-INFORMATION-DATE
      *
           MOVE    SMCNDATE-HMS    TO  WRK-HMS
           PERFORM 800-TIME-SEC
           MOVE    WRK-TIME        TO  FINDRES-INFORMATION-TIME
      *
           PERFORM 890-ERRCD-MSG-SEC
           IF    ( SPA-ERRCD       =   ZERO )
               MOVE    ZERO        TO  FINDRES-API-RESULT
           ELSE
               MOVE    "E"         TO  FINDRES-API-RESULT
               MOVE    SPA-ERRCD   TO  FINDRES-API-RESULT(2:)
           END-IF
           MOVE    SPA-Q01-ERRMSG  TO  FINDRES-API-RESULT-MSG
      *
           MOVE    RES-COUNT       TO  FINDRES-COUNT
      *
           MOVE    "xml_findv3res"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    1                   TO  FINDRES-MODE
           MOVE    "findv3res"         TO  FINDRES-RECORDNAME
           MOVE    "XMLWRITE"          TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XML-FINDV3RES
           IF     (MCP-RC          =   ZERO  OR  1  )
               MOVE    FINDRES-OBJECT      TO  QAPI01-BODY
               MOVE    "application/xml"   TO  QAPI01-CONTENT-TYPE
           ELSE
               DISPLAY "XMLWRITE failure = " MCP-RC
           END-IF
      *
           .
       900-XML-WRITE-EXT.
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
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY5-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api013"            TO  APIPARA-GYOUMUID
           MOVE    FINDREQ-ORCA-UID    TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    FINDREQ-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY5-SEL-EXT.
           EXIT.
      *****************************************************************
      *    APIパラメタ検索処理
      *****************************************************************
       900-APIPARA-KEY7-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-APIPARA
      *
           INITIALIZE                      APIPARA-REC
           MOVE    SPA-HOSPNUM         TO  APIPARA-HOSPNUM
           MOVE    "api013"            TO  APIPARA-GYOUMUID
           MOVE    FINDREQ-ORCA-UID    TO  APIPARA-TERMID
           MOVE    SPA-OPID            TO  APIPARA-FILEMEI
           MOVE    1                   TO  APIPARA-EDANUM
           MOVE    FINDREQ-KARTE-UID   TO  APIPARA-KARTE-UID
           MOVE    APIPARA-REC         TO  MCPDATA-REC
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC     TO  APIPARA-REC
           ELSE
               MOVE    1               TO  FLG-APIPARA
               INITIALIZE                  APIPARA-REC
           END-IF
      *
           MOVE    "tbl_api_para"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-APIPARA-KEY7-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-KEY2-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    FINDREQ-ORCA-UID    TO  PTSRH-TERMID
           MOVE    "0"                 TO  PTSRH-DATAKBN
           MOVE    "%"                 TO  PTSRH-UPYMD
           MOVE    "%"                 TO  PTSRH-UPHMS
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTSRH-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会検索処理
      *****************************************************************
       900-PTSRH-ODR-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           INITIALIZE                      PTSRH-REC
           MOVE    SPA-HOSPNUM         TO  PTSRH-HOSPNUM
           MOVE    FINDREQ-ORCA-UID    TO  PTSRH-TERMID
           MOVE    "1"                 TO  PTSRH-DATAKBN
           MOVE    "%"                 TO  PTSRH-UPYMD
           MOVE    "%"                 TO  PTSRH-UPHMS
           MOVE    FUNCTION CONCATENATE (
                       FUNCTION TRIM(RESULT-ORDER)
                       " LIMIT "
                       WRK-RANGE-LIMIT
                       " OFFSET "
                       WRK-RANGE-OFFSET
                       )               TO  PTSRH-SQL
           MOVE    PTSRH-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者照会FETCH処理
      *****************************************************************
       900-PTSRH-ODR-FET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-PTSRH
      *
           MOVE    "tbl_ptsrh"         TO  MCP-TABLE
           MOVE    "odr"               TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    ZERO            TO  FLG-PTINF
               MOVE    MCPDATA-REC     TO  PTSRH-REC
           ELSE
               MOVE    1               TO  FLG-PTSRH
               INITIALIZE                  PTSRH-REC
           END-IF
      *
           .
       900-PTSRH-ODR-FET-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       900-PTINF-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTINF
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTINF-PTID
           MOVE    PTINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTINF-REC
           ELSE
               INITIALIZE              PTINF-REC
               MOVE    1           TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携検索処理
      *****************************************************************
       900-PTNUM-PUBLIC-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO            TO  FLG-PTPUBLIC
      *
           INITIALIZE                  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM     TO  PTPUBLIC-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTPUBLIC-PTID
           MOVE    PTPUBLIC-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTPUBLIC-REC
           ELSE
               INITIALIZE              PTPUBLIC-REC
               MOVE    1           TO  FLG-PTPUBLIC
           END-IF
      *
           MOVE    "tbl_ptnum_public" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNUM-PUBLIC-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者保険情報検索処理
      *****************************************************************
       900-PTHKNINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTHKNINF
      *
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM     TO  PTHKN-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTHKN-PTID
           MOVE    PTSRH-HKNID     TO  PTHKN-HKNID
           MOVE    PTHKNINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTHKNINF-REC
           ELSE
               INITIALIZE              PTHKNINF-REC
               MOVE    1           TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    "tbl_pthkninf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
           
      *
           .
       900-PTHKNINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者公費情報検索処理
      *****************************************************************
       900-PTKOHINF-KEY-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-PTKOHINF
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
           MOVE    PTSRH-PTID      TO  PTKOH-PTID
           MOVE    PTSRH-KOHID (IDX2)
                                   TO  PTKOH-KOHID
           MOVE    PTKOHINF-REC    TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  PTKOHINF-REC
           ELSE
               INITIALIZE              PTKOHINF-REC
               MOVE    1           TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTKOHINF-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    保険番号検索処理
      *****************************************************************
       900-HKNNUM-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-HKNNUM
      *
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  HKNNUM-REC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1           TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-HKNNUM-KEY-SEL-EXT.
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
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
