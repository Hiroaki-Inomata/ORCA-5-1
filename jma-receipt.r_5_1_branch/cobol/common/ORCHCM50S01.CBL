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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCHCM50S01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 収納
      *  コンポーネント名  : 自費内訳明細書データ取得
      *  管理者            : 
      *  作成日付    作業者        記述
      *  06/12/21    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07.05.29 グループ診療対応
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
      *    診療行為ファイル
           SELECT  HCM50SRY-FILE   ASSIGN   HCM50F01
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  HCM50SRY-ISM-KEY
                                   FILE    STATUS  IS  STS-HCM50F01.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *    診療行為ファイル
       FD  HCM50SRY-FILE.
           COPY "CPORCHCM50SRY.INC".
      *
       WORKING-STORAGE                 SECTION.
      *
           COPY    "CPASGNPARA.INC"    REPLACING   //ASGNPARA//
                                       BY          //HCM50F01//.
      *
       01  STS-AREA.
           03  STS-HCM50F01            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-HCM50SRY            PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-PTNYUINRRK          PIC 9(01).
           03  FLG-LOOPEND             PIC 9(01).
      *
      *    カウント領域
      *01  CNT-AREA.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
           03  IDXN                    PIC 9(05).
           03  IDXZ                    PIC 9(05).
           03  IDX-NACCT               PIC 9(05).
           03  IDX-NSRY                PIC 9(05).
           03  IDX-SRY                 PIC 9(05).
           03  IDX-DAY                 PIC 9(05).
           03  IDXE                    PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM           PIC X(06).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-SRYKBN              PIC X(03).
           03  WRK-SRYKBN2             PIC X(03).
           03  WRK-TENTTLKBN           PIC 9(03).
           03  WRK-KANANAME            PIC X(80).
           03  WRK-SURYO               PIC S9(11).
           03  WRK-KINGAKU             PIC S9(11).
           03  WRK-SRYCD               PIC X(09).
           03  WRK-STSRYYMD.
               05  WRK-STSRYYM         PIC X(06).
               05  WRK-STSRYDD         PIC X(02).
           03  WRK-EDSRYYMD.
               05  WRK-EDSRYYM         PIC X(06).
               05  WRK-EDSRYDD         PIC X(02).
           03  WRK-HKNCOMBI.
               05  WRK-HKNCOMBI9       PIC 9(04).
           03  WRK-ZAIKAISU            PIC 9(10).
           03  WRK-KANRICD             PIC X(04).
           03  WRK-KBNCD               PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-SKBT-BRMSAGAKU    PIC X(01)   VALUE   "3".
           03  CONST-SRYCD-BRMSAGAKU   PIC X(09)   VALUE   "099999912".
           03  CONST-NSRY-MAX          PIC 9(03)   VALUE   20.
           03  CONST-SRY-MAX           PIC 9(03)   VALUE   5.
      *
           COPY    "CPTTNS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報−入院基本
           COPY    "CPSK5000.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    収納マスタ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスタ
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴マスタ
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCHCM31S01.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    入院会計取得
           COPY    "CPORCHCM34S01.INC".
      *
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                         SECTION.
      *
           COPY    "CPORCHCM31.INC".
           COPY    "CPORCHCM50S01.INC".
      *
           COPY    "COMMON-SPA".
      *
      ******************************************************************
      *
       PROCEDURE                       DIVISION
                                       USING   ORCHCM31AREA
                                               HCM50S01-AREA
                                               SPA-AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END     =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  TTNS-AREA
      *
           MOVE    HCM50S01-HCM50F01   TO  HCM50F01
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           OPEN    I-O                 HCM50SRY-FILE
      *
           PERFORM 900-SYUNOU-KEY65-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
      *
               IF    ( SYU-NYUGAIKBN   =   "1" )
      *
                   PERFORM 2001-NYUIN-SEC
      *
               ELSE
                   PERFORM 20011-GAI-SRY-HEN-SEC
               END-IF
      *
               PERFORM 900-SYUNOU-KEY65-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key65"         TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           CLOSE   HCM50SRY-FILE
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    入院編集処理
      *****************************************************************
       2001-NYUIN-SEC                    SECTION.
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
      *
           INITIALIZE                  SS006-AREA
           MOVE    SYU-SKYSTYMD    TO  SS006-SKYSTYMD
           MOVE    SYU-SKYEDYMD    TO  SS006-SKYEDYMD
           IF    ( SYU-CREATEKBN       =   "1" )
               IF    ( SYU-SKYEDYMD    =   PTNYUINRRK-TAIINYMD )
                   MOVE    "1"     TO  SS006-SKYKBN
               ELSE
                   MOVE    "2"     TO  SS006-SKYKBN
               END-IF
           ELSE
               MOVE    SYU-CREATEKBN   TO  SS006-SKYKBN
           END-IF
      *
           CALL    "ORCSS006"              USING
                                           SS006-AREA
                                           SPA-AREA
      *
           PERFORM 20011-NYU-NSRY-HEN-SEC
           PERFORM 20011-NYU-SRY-HEN-SEC
      *
           .
       2001-NYUIN-EXT.
           EXIT.
      *****************************************************************
      *    入院診療行為編集処理
      *****************************************************************
       20011-NYU-NSRY-HEN-SEC          SECTION.
      *
           INITIALIZE                      HCM34S01-AREA
      *
           MOVE    "5000"              TO  WRK-KANRICD
           MOVE    "*"                 TO  WRK-KBNCD
           MOVE    SYU-SRYYMD(1:6)     TO  WRK-SRYYMD
           PERFORM 900-SYSKANRI-KEY10-SEL-SEC
           MOVE    SYSKANRI-REC        TO  SYS-5000-REC
      *
           INITIALIZE                      HCM34S01-AREA
           MOVE    SYU-HOSPNUM          TO  HCM34S01-HOSPNUM
           MOVE    SYU-PTID            TO  HCM34S01-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  HCM34S01-SANTEIYM
           MOVE    "1"                 TO  HCM34S01-KIKANKBN
           MOVE    SYU-SKYSTYMD        TO  HCM34S01-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  HCM34S01-SKYEDYMD
           MOVE    SYU-HKNCOMBINUM     TO  HCM34S01-HKNCOMBI
           MOVE    SYU-SYUHKNNUM       TO  HCM34S01-HKNNUM
           MOVE    SS006-DOJITUFLG-ST  TO  HCM34S01-DOJITUFLG-ST
           MOVE    SS006-TIMEKBN-ST    TO  HCM34S01-TIMEKBN-ST
           MOVE    SS006-DOJITUFLG-ED  TO  HCM34S01-DOJITUFLG-ED
           MOVE    SS006-TIMEKBN-ED    TO  HCM34S01-TIMEKBN-ED
      *
           IF    ( SYU-SYUHKNNUM       =   "971" )
                                       OR  "973"
               CALL    "ORCHCM34S02"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           ELSE
               CALL    "ORCHCM34S01"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           END-IF
      *
           PERFORM VARYING IDX-NACCT   FROM    1   BY  1
                   UNTIL ( IDX-NACCT   >   HCM34S01-HENKYAKU-MAX )
               IF    ( HCM34S01-ZAISKBKBN(IDX-NACCT)
                                       =   CONST-SKBT-BRMSAGAKU )
                   PERFORM VARYING IDX-NSRY    FROM    1   BY  1
                           UNTIL ( IDX-NSRY    >   CONST-NSRY-MAX )
      *
                       IF    ( HCM34S01-SRYCD (IDX-NACCT IDX-NSRY)
                                       =   CONST-SRYCD-BRMSAGAKU )
      *
                           MOVE    SYU-SRYYMD      TO  WRK-SRYYMD
                           MOVE    SYU-SKYSTYMD    TO  WRK-STSRYYMD
                           MOVE    SYU-SKYEDYMD    TO  WRK-EDSRYYMD
                           MOVE    "1"         TO  WRK-NYUGAIKBN
                           MOVE    HCM34S01-SRYCD (IDX-NACCT IDX-NSRY)
                                               TO  WRK-SRYCD
                           MOVE    HCM34S01-SURYO (IDX-NACCT IDX-NSRY)
                                               TO  WRK-SURYO
                           MOVE    HCM34S01-ZAITEN (IDX-NACCT)
                                               TO  WRK-KINGAKU
                           PERFORM 900-TENSU-KEY-SEL-SEC
                           MOVE    TNS-KANANAME TO WRK-KANANAME
                           MOVE    "90"         TO WRK-SRYKBN
                           IF    ( SYS-5000-BRMSGK-TAXKBN  =   "1" )
                               MOVE    "96"     TO WRK-SRYKBN2
                           ELSE
                               MOVE    "95"     TO WRK-SRYKBN2
                           END-IF
                           MOVE    ZERO         TO WRK-TENTTLKBN
                           PERFORM 200111-HCM50SRY-KEY-HEN-SEC
                           PERFORM 900-HCM50SRY-DYN-READ-SEC
                           IF    ( FLG-HCM50SRY    =   ZERO )
                               COMPUTE HCM50SRY-ZAIKAISU
                                   =   HCM50SRY-ZAIKAISU
                                   +   HCM34S01-ZAIKAISU (IDX-NACCT)
                               REWRITE HCM50SRY-REC
                           ELSE
                               PERFORM 200111-HCM50SRY-KEY-HEN-SEC
                               COMPUTE HCM50SRY-ZAIKAISU
                                   =   HCM34S01-ZAIKAISU (IDX-NACCT)
                               MOVE    TNS-NAME
                                               TO  HCM50SRY-NAME
                               WRITE   HCM50SRY-REC
                           END-IF
                       END-IF
      *
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       20011-NYU-NSRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイルキー項目編集処理
      *****************************************************************
       200111-HCM50SRY-KEY-HEN-SEC     SECTION.
      *
           INITIALIZE                      HCM50SRY-REC
           MOVE    WRK-NYUGAIKBN       TO  HCM50SRY-NYUGAIKBN
           IF    ( WRK-NYUGAIKBN       =   "1" )
               IF    ( HCM50S01-SUMKBN =   "1" )
                   MOVE    WRK-STSRYYM     TO  HCM50SRY-SKYSTYMD
                                               HCM50SRY-SKYEDYMD
               ELSE
                   MOVE    WRK-STSRYYMD    TO  HCM50SRY-SKYSTYMD
                   MOVE    WRK-EDSRYYMD    TO  HCM50SRY-SKYEDYMD
               END-IF
           ELSE
               IF    ( HCM50S01-SUMKBN =   "1" )
                   MOVE    WRK-SRYYM       TO  HCM50SRY-SKYSTYMD
                                               HCM50SRY-SKYEDYMD
               ELSE
                   MOVE    WRK-SRYYMD      TO  HCM50SRY-SKYSTYMD
                                               HCM50SRY-SKYEDYMD
               END-IF
           END-IF
           MOVE    WRK-KANANAME        TO  HCM50SRY-KANANAME
           MOVE    WRK-SRYKBN          TO  HCM50SRY-SRYKBN
           MOVE    WRK-SRYKBN2         TO  HCM50SRY-SRYKBN2
           MOVE    WRK-TENTTLKBN       TO  HCM50SRY-TENTTLKBN
           MOVE    WRK-SRYCD           TO  HCM50SRY-SRYCD
           MOVE    WRK-SURYO           TO  HCM50SRY-SURYO
           MOVE    WRK-KINGAKU         TO  HCM50SRY-KINGAKU
      *
           .
       200111-HCM50SRY-KEY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20011-NYU-SRY-HEN-SEC           SECTION.
      *
           MOVE    2                   TO  IDXN
           IF    ( SS006-DOJITUFLG-ST  =   "1" )
               MOVE    3               TO  IDXN
           END-IF
      *
           MOVE    SYU-SKYSTYMD        TO  WRK-STSRYYMD
           MOVE    SYU-SKYEDYMD        TO  WRK-EDSRYYMD
           MOVE    "1"                 TO  WRK-NYUGAIKBN
           PERFORM 900-SRYACCT-KEY49-SEL-SEC
      *
           PERFORM UNTIL ( FLG-SRYACCT NOT =   ZERO )
      *
               MOVE    ACCT-HKNCOMBI   TO  WRK-HKNCOMBI9
               IF    ( SYU-HKNCOMBINUM =   WRK-HKNCOMBI )
                   PERFORM 200111-SRY-HEN-SEC
               END-IF
      *
               PERFORM 900-SRYACCT-KEY49-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-NYU-SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       20011-GAI-SRY-HEN-SEC           SECTION.
      *
           PERFORM 900-JYURRK-KEY47-SEL-SEC
      *
           PERFORM UNTIL ( FLG-JYURRK  NOT =   ZERO )
      *
               MOVE    SYU-SRYYMD      TO  WRK-STSRYYMD
                                           WRK-EDSRYYMD
               COMPUTE IDXN  = JYURRK-RENNUM   +   1
      *
               PERFORM VARYING IDXZ    FROM    1   BY  1
                       UNTIL ( IDXZ    >   15 )
      *
                   IF    ( JYURRK-ZAINUM (IDXZ)  NOT =   ZERO)
                       PERFORM 900-SRYACCT-KEY57-SEL-SEC
                       IF    ( FLG-SRYACCT     =   ZERO )
                        AND  ( ACCT-SRYKBN     =   "95" OR "96" )
                        AND  ( ACCT-HKNCOMBI   NOT = "9999" )
                        AND  ( ACCT-JIHOKBN    NOT = "1" )
                           PERFORM 200111-SRY-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
               PERFORM 900-JYURRK-KEY47-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       20011-GAI-SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為編集処理
      *****************************************************************
       200111-SRY-HEN-SEC              SECTION.
      *
           PERFORM 900-SRYACT-KEY2-SEL-SEC
      *
           PERFORM     UNTIL ( FLG-SRYACT  NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRY FROM    1   BY  1
                       UNTIL ( IDX-SRY >   CONST-SRY-MAX )
      *
                   IF    ( SRY-SRYCD (IDX-SRY) (1:3)
                                       =   "095" OR "096" )
                    AND  ( SRY-JIHIMONEY (IDX-SRY)
                                       >   ZERO )
                       PERFORM 200111-JIHISRY-HEN-SEC
                   END-IF
      *
               END-PERFORM
      *
               PERFORM 900-SRYACT-KEY2-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       200111-SRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    自費診療行為編集処理
      *****************************************************************
       200111-JIHISRY-HEN-SEC          SECTION.
      *
           MOVE    SRY-SRYCD (IDX-SRY) TO  WRK-SRYCD
           MOVE    SYU-SRYYMD          TO  WRK-SRYYMD
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
           MOVE    SPACE               TO  WRK-SRYKBN
           MOVE    ZERO                TO  WRK-TENTTLKBN
      *
           EVALUATE    SRY-SRYCD (IDX-SRY) (4:2)
           WHEN    "00"
           WHEN    "91"
           WHEN    "92"
           WHEN    "93"
           WHEN    "94"
               MOVE    SRY-SRYKBN      TO  WRK-SRYKBN
               IF    ( SRY-NYUGAIKBN   =   "1" )
                   MOVE    TNS-NYUTENTTLKBN    TO  WRK-TENTTLKBN
               ELSE
                   MOVE    TNS-GAITENTTLKBN    TO  WRK-TENTTLKBN
               END-IF
           WHEN    OTHER
               EVALUATE    SRY-SRYCD (IDX-SRY) (4:2)
               WHEN    "11"
                   MOVE    "12"        TO  WRK-SRYKBN
               WHEN    "12"
                   MOVE    "11"        TO  WRK-SRYKBN
               WHEN    "13"
                   MOVE    "13"        TO  WRK-SRYKBN
               WHEN    "14"
                   MOVE    "14"        TO  WRK-SRYKBN
               WHEN   "21"   THRU  "27"
               WHEN   "82"
                   MOVE    "21"        TO  WRK-SRYKBN
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "31"        TO  WRK-SRYKBN
               WHEN   "40"
                   MOVE    "40"        TO  WRK-SRYKBN
               WHEN   "50"
                   MOVE    "50"        TO  WRK-SRYKBN
               WHEN   "54"
                   MOVE    "54"        TO  WRK-SRYKBN
               WHEN   "60"
                   MOVE    "60"        TO  WRK-SRYKBN
               WHEN   "64"
                   MOVE    "64"        TO  WRK-SRYKBN
               WHEN   "70"
                   MOVE    "70"        TO  WRK-SRYKBN
               WHEN   "80"
                   MOVE    "80"        TO  WRK-SRYKBN
               WHEN   "83"
                   MOVE    "83"        TO  WRK-SRYKBN
               WHEN   "84"
                   MOVE    "84"        TO  WRK-SRYKBN
               WHEN   "85"
                   MOVE    "85"        TO  WRK-SRYKBN
               WHEN   "89"
               WHEN   "90"
                   MOVE    "90"        TO  WRK-SRYKBN
               WHEN   OTHER
                   MOVE    "80"        TO  WRK-SRYKBN
               END-EVALUATE
           END-EVALUATE
      *
           IF    ( WRK-SRYKBN      NOT =   SPACE )
      *
               MOVE    ACCT-SRYYM      TO  WRK-SRYYM
               MOVE    ZERO            TO  WRK-SRYDD
               PERFORM VARYING IDX-DAY FROM    1   BY  1
                       UNTIL ( IDX-DAY >   31 )
      *
                   MOVE    IDX-DAY     TO  WRK-SRYDD
                   IF    ( WRK-SRYYMD  >=  WRK-STSRYYMD )
                    AND  ( WRK-SRYYMD  <=  WRK-EDSRYYMD )
      *
                       IF    (  ACCT-DAY (IDXN IDX-DAY)   >   ZERO )
                           MOVE    SRY-NYUGAIKBN   TO  WRK-NYUGAIKBN
                           MOVE    SRY-SRYKBN      TO  WRK-SRYKBN2
                           MOVE    SRY-SRYCD    (IDX-SRY)
                                                   TO  WRK-SRYCD
                           MOVE    SRY-SRYSURYO (IDX-SRY)
                                                   TO  WRK-SURYO
                           MOVE    SRY-JIHIMONEY(IDX-SRY)
                                                   TO  WRK-KINGAKU
                           MOVE    TNS-KANANAME    TO  WRK-KANANAME
                           PERFORM 200111-HCM50SRY-KEY-HEN-SEC
                           PERFORM 900-HCM50SRY-DYN-READ-SEC
                           IF    ( FLG-HCM50SRY    =   ZERO )
                               COMPUTE HCM50SRY-ZAIKAISU
                               =   HCM50SRY-ZAIKAISU
                               +   ACCT-DAY (IDXN IDX-DAY)
                               REWRITE HCM50SRY-REC
                           ELSE
                               PERFORM 200111-HCM50SRY-KEY-HEN-SEC
                               COMPUTE HCM50SRY-ZAIKAISU
                                   =   ACCT-DAY (IDXN IDX-DAY)
                               MOVE    TNS-NAME    TO  HCM50SRY-NAME
                               WRITE   HCM50SRY-REC
                           END-IF
                       END-IF
      *
                   END-IF
      *
               END-PERFORM
           END-IF
      *
           .
       200111-JIHISRY-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *    PERFORM 800-DISPLAY-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    返却値表示
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
           DISPLAY "==IN====================="
           DISPLAY "HOSPNUM    " HCM50S01-HOSPNUM
           DISPLAY "NYUGAIKBN " HCM50S01-NYUGAIKBN
           DISPLAY "PTID      " HCM50S01-PTID
           DISPLAY "SUMKBN    " HCM50S01-SUMKBN
           DISPLAY "SRYYMD    " HCM50S01-SRYYMD
           DISPLAY "STYMD     " HCM50S01-STYMD
           DISPLAY "EDYMD     " HCM50S01-EDYMD
           DISPLAY "HCM50F01  " HCM50S01-HCM50F01
      *
           OPEN    INPUT               HCM50SRY-FILE
           MOVE    ZERO            TO  FLG-HCM50SRY
           READ    HCM50SRY-FILE   NEXT
           AT  END
               MOVE    1           TO  FLG-HCM50SRY
           END-READ
      *
           MOVE    ZERO            TO  IDX0
           DISPLAY "==IN====================="
           DISPLAY " "
           DISPLAY "==OT===================="
           PERFORM UNTIL ( FLG-HCM50SRY    NOT =   ZERO )
      *
               COMPUTE IDX0    =   IDX0    +   1
               DISPLAY IDX0 ":NYUGAIKBN " HCM50SRY-NYUGAIKBN
               DISPLAY IDX0 ":SKYSTYMD  " HCM50SRY-SKYSTYMD
               DISPLAY IDX0 ":SKYEDYMD  " HCM50SRY-SKYEDYMD
               DISPLAY IDX0 ":SRYKBN    " HCM50SRY-SRYKBN
               DISPLAY IDX0 ":TENTTLKBN " HCM50SRY-TENTTLKBN
               DISPLAY IDX0 ":KANANAME  " HCM50SRY-KANANAME
               DISPLAY IDX0 ":SURYO     " HCM50SRY-SURYO
               DISPLAY IDX0 ":KINGAKU   " HCM50SRY-KINGAKU
               DISPLAY IDX0 ":SRYCD     " HCM50SRY-SRYCD
               DISPLAY IDX0 ":ZAIKAISU  " HCM50SRY-ZAIKAISU
               DISPLAY IDX0 ":NAME      " HCM50SRY-NAME
      *
               READ    HCM50SRY-FILE   NEXT
               AT  END
                   MOVE    1           TO  FLG-HCM50SRY
               NOT AT  END
                   DISPLAY "--"
               END-READ
           END-PERFORM
           DISPLAY "==OT===================="
      *
           CLOSE   HCM50SRY-FILE
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    診療行為ファイル読込処理
      *****************************************************************
       900-HCM50SRY-DYN-READ-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-HCM50SRY
      *
           READ    HCM50SRY-FILE
           INVALID
               MOVE    1           TO  FLG-HCM50SRY
           END-READ
      *
           .
       900-HCM50SRY-DYN-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY65-SEL-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    HCM50S01-HOSPNUM TO  SYU-HOSPNUM
           EVALUATE    ORCHCM31-NYUGAI
               WHEN    "0"
                   MOVE    "%"         TO  SYU-NYUGAIKBN
               WHEN    "1"
                   MOVE    "1"         TO  SYU-NYUGAIKBN
               WHEN    "2"
                   MOVE    "2"         TO  SYU-NYUGAIKBN
           END-EVALUATE
           MOVE    HCM50S01-PTID   TO  SYU-PTID
           MOVE    HCM50S01-STYMD  TO  SYU-SKYSTYMD
           MOVE    HCM50S01-EDYMD  TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC      TO  MCPDATA-REC
           MOVE    "tbl_syunou"    TO  MCP-TABLE
           MOVE    "key65"         TO  MCP-PATHNAME
           PERFORM 911-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               PERFORM 900-SYUNOU-KEY65-FET-SEC
           END-IF
      *
           .
       900-SYUNOU-KEY65-SEL-EXT.
           EXIT.
      *****************************************************************
      *    収納FETCH処理
      *****************************************************************
       900-SYUNOU-KEY65-FET-SEC        SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
           MOVE    ZERO            TO  FLG-LOOPEND
      *
           PERFORM UNTIL ( FLG-SYUNOU  NOT =   ZERO )
                    OR   ( FLG-LOOPEND NOT =   ZERO )
      *
               MOVE    "tbl_syunou"    TO  MCP-TABLE
               MOVE    "key65"         TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SYUNOU-REC
      *
                   MOVE    1           TO  FLG-LOOPEND
      *
                   IF    ( SYU-DENPJTIKBN  =   "3" )
                       MOVE    ZERO    TO  FLG-LOOPEND
                   END-IF
      *
                   IF    ( ORCHCM31-DSPKBN     =   "3" )
                        IF    ( SYU-SYUHKNNUM (1:2) =   "98" )
                         OR   ( SYU-SYUHKNNUM       =   "971" )
                         OR   ( SYU-SYUHKNNUM       =   "973" )
                         OR   ( SYU-SYUHKNNUM       =   "975" )
                            MOVE    ZERO    TO  FLG-LOOPEND
                        END-IF
                   END-IF
      *
                   IF    ( ORCHCM31-KIKANKBN   =   "1" )
                       IF    ( ORCHCM31-SUMKBN =   "2" )
                           IF    ( SYU-SRYYMD >=   ORCHCM31-STRSRYYMD )
                            AND  ( SYU-SRYYMD <=   ORCHCM31-ENDSRYYMD )
                               CONTINUE
                           ELSE
                               MOVE    ZERO    TO  FLG-LOOPEND
                           END-IF
                       END-IF
                   END-IF
      *
                   IF    ( FLG-LOOPEND =    1 )
                    AND  ( ORCHCM31-DSPKBN     =   "2" OR "3" OR "4" )
      *
                       INITIALIZE                  HCM31S01-AREA
      *
                       IF    ( ORCHCM31-SYORIKBN   =   "1" )
                           MOVE    "1"         TO  HCM31S01-PROCKBN
                       ELSE
                           MOVE    "2"         TO  HCM31S01-PROCKBN
                       END-IF
      *
                       MOVE    SYU-HOSPNUM     TO  HCM31S01-HOSPNUM
                       MOVE    SYU-NYUGAIKBN   TO  HCM31S01-NYUGAIKBN
                       MOVE    SYU-PTID        TO  HCM31S01-PTID
                       MOVE    SYU-DENPNUM     TO  HCM31S01-DENPNUM
                       MOVE    SYU-SRYYMD      TO  HCM31S01-SRYYMD
                       MOVE    ORCHCM31-STRSRYYMD
                                               TO  HCM31S01-STYMD
                       MOVE    ORCHCM31-ENDSRYYMD
                                               TO  HCM31S01-EDYMD
                       MOVE    ORCHCM31-SUMKBN
                                               TO  HCM31S01-SUMKBN
                       CALL    "ORCHCM31S01"       USING
                                                   HCM31S01-AREA
                                                   SPA-AREA
                       IF    ( HCM31S01-KINGAKU (1)        =   ZERO )
                            MOVE    ZERO    TO  FLG-LOOPEND
                       END-IF
                   END-IF
      *
               ELSE
                   MOVE    1           TO  FLG-SYUNOU
                   INITIALIZE              SYUNOU-REC
               END-IF
      *
           END-PERFORM
      *
           .
       900-SYUNOU-KEY65-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY49-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    HCM50S01-HOSPNUM     TO  ACCT-HOSPNUM
           MOVE    WRK-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    HCM50S01-PTID       TO  ACCT-PTID
           MOVE    WRK-STSRYYM         TO  ACCT-STSRYYM
           MOVE    WRK-EDSRYYM         TO  ACCT-EDSRYYM
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY49-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療会計FETCH処理
      *****************************************************************
       900-SRYACCT-KEY49-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key49"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACCT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACCT
               INITIALIZE              SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY49-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       900-SRYACT-KEY2-SEL-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM    TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN  TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID       TO  SRY-PTID
           MOVE    ACCT-SRYKA      TO  SRY-SRYKA
           MOVE    ACCT-SRYYM      TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM     TO  SRY-ZAINUM
           MOVE    SRYACT-REC      TO  MCPDATA-REC
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY2-SEL-EXT.
           EXIT.
      *****************************************************************
      *    診療行為FETCH処理
      *****************************************************************
       900-SRYACT-KEY2-FET-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SRYACT
      *
           MOVE    "tbl_sryact"     TO  MCP-TABLE
           MOVE    "key2"           TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          =   ZERO )
               MOVE    MCPDATA-REC TO  SRYACT-REC
           ELSE
               MOVE    1           TO  FLG-SRYACT
               INITIALIZE              SRYACT-REC
           END-IF
      *
           .
       900-SRYACT-KEY2-FET-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *
           MOVE    WRK-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    WRK-SRYYMD          TO  TTNS-CHK-YMD
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    HCM50S01-HOSPNUM    TO  TNS-HOSPNUM
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC              =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
                ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
      *
           END-IF
      *
           .
       900-TENSU-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
      *
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
      *
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴検索処理
      *****************************************************************
       900-JYURRK-KEY47-SEL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           INITIALIZE                      JYURRK-REC
           MOVE    SYU-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SYU-PTID            TO  JYURRK-PTID
           MOVE    SYU-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SYU-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SYU-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           MOVE    SYU-DENPNUM         TO  JYURRK-DENPNUM
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       900-JYURRK-KEY47-SEL-EXT.
           EXIT.
      *****************************************************************
      *    受信履歴FETCH処理
      *****************************************************************
       900-JYURRK-KEY47-FET-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  JYURRK-REC
           ELSE
               MOVE    1               TO  FLG-JYURRK
               INITIALIZE                  JYURRK-REC
           END-IF
      *
           .
       900-JYURRK-KEY47-FET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計検索処理
      *****************************************************************
       900-SRYACCT-KEY57-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-ZAINUM (IDXZ)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key57"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SRYACCT-KEY57-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者入院履歴検索処理
      *****************************************************************
       900-PTNYUINRRK-KEY-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-PTNYUINRRK
      *
           INITIALIZE                      PTNYUINRRK-REC
           MOVE    SYU-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SYU-PTID            TO  PTNYUINRRK-PTID
           MOVE    SYU-NYUIN-RRKNUM    TO  PTNYUINRRK-RRKNUM
           MOVE    1                   TO  PTNYUINRRK-RRKEDANUM
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-PTNYUINRRK-KEY-SEL-EXT.
           EXIT.
      *****************************************************************
      *    システム管理検索処理(KEY10)
      *****************************************************************
       900-SYSKANRI-KEY10-SEL-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-SYSKANRI
      *
           INITIALIZE                      SYSKANRI-REC
      *
           MOVE    HCM50S01-HOSPNUM    TO  SYS-HOSPNUM
           MOVE    WRK-KANRICD         TO  SYS-KANRICD
           MOVE    WRK-KBNCD           TO  SYS-KBNCD
           MOVE    WRK-SRYYMD          TO  SYS-STYUKYMD
                                           SYS-EDYUKYMD
      *
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
      *    ＤＢ検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理
      *****************************************************************
       911-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DB-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       910-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
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
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBCLOSECURSOR-EXT.
           EXIT.
