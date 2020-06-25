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
       PROGRAM-ID.             ORCSS003.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 日次統計
      *  コンポーネント名  : 初診／再診カウントサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/04/22    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.05.00    NACL-太田    07/06/05  グループ診療対応
      *
      *  04.06.01    NACL-門脇    13/02/18  労災レセ電対応
      *                                     （新診療行為コード対応）
      *****************************************************************
      *
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
      *FILE                            SECTION.
      *
       WORKING-STORAGE                 SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYUNOU                      PIC 9(01).
           03  FLG-JYURRK                      PIC 9(01).
           03  FLG-SRYACCT                     PIC 9(01).
           03  FLG-SRYACT                      PIC 9(01).
           03  FLG-PTNYUINRRK                  PIC 9(01).
           03  FLG-TENSU                       PIC 9(01).
           03  FLG-END                         PIC 9(01).
           03  FLG-OK                          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX0                            PIC 9(05).
           03  IDX-ZAINUM                      PIC 9(05).
           03  IDX-SRYCD                       PIC 9(05).
           03  IDX-KAISU                       PIC 9(05).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SRYYMD.
               05  WRK-SRYYM                   PIC X(06).
               05  WRK-SRYDAY                  PIC 9(02).
           03  WRK-JNISSU-KBN-G.
               05  WRK-SYOSAISIN-KBN           PIC X(01).
               05  WRK-JNISSUGEN-KBN           PIC X(01).
               05  WRK-JIKANGAI-KBN            PIC X(01).
           03  WRK-DAY                         PIC S9(05).
      *
       01  CONST-AREA.
           03  CONST-ZAINUM-MAX            PIC 9(05)   VALUE 15.
           03  CONST-SRYCD-MAX             PIC 9(05)   VALUE 5.
           03  CONST-H180401               PIC X(08)   VALUE "20060401".
      *
      *
           COPY    "CPTTNS.INC".
      *
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    点数マスタ−
           COPY    "CPTENSU.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    入院会計作成パラ
           COPY    "CPORCHCM34S01.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "MCPAREA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                        SECTION.
      *
      *    初診／再診カウントサブ
           COPY    "CPORCSS003.INC".
           COPY    "COMMON-SPA".
      *
       PROCEDURE                       DIVISION    USING
           SS003-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *    INITIALIZE                      CNT-AREA
      *
           INITIALIZE                      SS003-OT
      *
           INITIALIZE                      SGETTEMP-AREA
           CALL    "ORCSGETTEMP"           USING
                                           SGETTEMP-AREA
      *
      *    初期処理
           PERFORM 100-INIT-SEC
      *    主処理
           PERFORM 200-MAIN-SEC
      *    終了処理
           PERFORM 300-END-SEC
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           MOVE    ZERO            TO  FLG-END
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( SS003-SYORIKBN      =   "0" OR "2" )
            OR   ( SGETTEMP-MW         =   SPA-GINBEE )
               INITIALIZE              TTNS-AREA
           END-IF
      *
           IF    ( SS003-SYORIKBN      =   "1" OR "2" )
               IF    ( SS003-NYUGAIKBN =   "1" )
                   PERFORM 2001-SYUNOU-CHK-SEC
               ELSE
                   PERFORM 2001-JYURRK-CHK-SEC
                   MOVE    WRK-SYOSAISIN-KBN   TO  SS003-SYOSAISIN-KBN
                   MOVE    WRK-JNISSUGEN-KBN   TO  SS003-JNISSUGEN-KBN
                   MOVE    WRK-JIKANGAI-KBN    TO  SS003-JIKANGAI-KBN
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納チェック処理
      *****************************************************************
       2001-SYUNOU-CHK-SEC             SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
           PERFORM 900-PTNYUINRRK-KEY-SEL-SEC
      *
           INITIALIZE                      SS006-AREA
           MOVE    SYU-SKYSTYMD        TO  SS006-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  SS006-SKYEDYMD
           IF    ( SYU-CREATEKBN       =   "1" )
               IF    ( SYU-SKYEDYMD    =   PTNYUINRRK-TAIINYMD )
                   MOVE    "1"         TO  SS006-SKYKBN
               ELSE
                   MOVE    "2"         TO  SS006-SKYKBN
               END-IF
           ELSE
               MOVE    SYU-CREATEKBN   TO  SS006-SKYKBN
           END-IF
           CALL    "ORCSS006"              USING
                                           SS006-AREA
                                           SPA-AREA
      *
           PERFORM 2002-SRYACCT-CHK-SEC
      *
           PERFORM 2002-NYUINACCT-CHK-SEC
      *
           .
       2001-SYUNOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴チェック処理
      *****************************************************************
       2001-JYURRK-CHK-SEC             SECTION.
      *
           PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
      *    受診履歴検索処理
           MOVE    ZERO                TO  FLG-JYURRK
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
           PERFORM UNTIL ( FLG-JYURRK  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM VARYING IDX-ZAINUM  FROM    1   BY  1
                       UNTIL ( IDX-ZAINUM  >  CONST-ZAINUM-MAX )
                        OR   ( FLG-END     NOT =   ZERO )
      *
                   IF    ( JYURRK-SRYYMD(7:2)  >=  "01" )
                    AND  ( JYURRK-SRYYMD(7:2)  <=  "31" )
                       IF    ( JYURRK-ZAINUM (IDX-ZAINUM)  NOT =   ZERO)
                           PERFORM 2001-SRYACCT-CHK-SEC
                       END-IF
                   END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key47"             TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  JYURRK-REC
               ELSE
                   MOVE    1           TO  FLG-JYURRK
                   INITIALIZE              JYURRK-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key47"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-JYURRK-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       2001-SRYACCT-CHK-SEC            SECTION.
      *
           MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
           COMPUTE IDX-KAISU   =   JYURRK-RENNUM   +   1
      *
      *    診療会計検索
           MOVE    ZERO                TO  FLG-SRYACCT
           INITIALIZE                      SRYACCT-REC
           MOVE    JYURRK-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  ACCT-PTID
           MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
           MOVE    JYURRK-ZAINUM (IDX-ZAINUM)
                                       TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           PERFORM 20011-SRYACCT-CHK-SEC
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key31"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           IF    ( WRK-SYOSAISIN-KBN NOT =   SPACE )
               MOVE    WRK-SRYYMD  TO  SS003-JNISSU-YMD
                                                (WRK-SRYDAY)
           END-IF
           IF    ( WRK-JNISSUGEN-KBN NOT =   SPACE )
               MOVE    WRK-SRYYMD  TO  SS003-JNISSU-YMD
                                                (WRK-SRYDAY)
               MOVE    1           TO  SS003-JNISSU-GEN
                                                (WRK-SRYDAY)
           END-IF
      *
           .
       2001-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       2002-SRYACCT-CHK-SEC            SECTION.
      *
           PERFORM 900-SRYACCT-KEY22-SEL-SEC
      *
           MOVE    ACCT-SRYYM          TO  WRK-SRYYM
           PERFORM VARYING WRK-SRYDAY  FROM    1   BY  1
                   UNTIL ( WRK-SRYDAY  >   31 )
      *
               MOVE    1               TO  IDX-KAISU
      *
               IF    ( WRK-SRYYMD      >=  SYU-SKYSTYMD )
                AND  ( WRK-SRYYMD      <=  SYU-SKYEDYMD )
                   IF    ( SS006-DOJITUFLG-ST  =   "1" )
                    AND  ( WRK-SRYYMD          =   SYU-SKYSTYMD )
                       CONTINUE
                   ELSE
                       PERFORM 20011-SRYACCT-CHK-SEC
                       IF    ( WRK-SYOSAISIN-KBN NOT =   SPACE )
                           MOVE    WRK-SRYYMD  TO  SS003-JNISSU-YMD
                                                            (WRK-SRYDAY)
                       END-IF
                       IF    ( WRK-JNISSUGEN-KBN NOT =   SPACE )
                           MOVE    WRK-SRYYMD  TO  SS003-JNISSU-YMD
                                                            (WRK-SRYDAY)
                           MOVE    1           TO  SS003-JNISSU-GEN
                                                            (WRK-SRYDAY)
                       END-IF
                       IF    ( ACCT-ZAITEN    NOT =    ZERO)
                        AND  ( ACCT-ZAIKAISU  NOT =    ZERO)
                            IF   ACCT-SRYKBN  =   "95" OR  "96"
                                CONTINUE
                            ELSE
                                MOVE    WRK-SRYYMD
                                               TO SS003-JNISSU-YMD
                                                            (WRK-SRYDAY)
                            END-IF
                       END-IF
                       INITIALIZE          WRK-JNISSU-KBN-G
                   END-IF
               END-IF
      *
               PERFORM 900-SRYACCT-KEY22-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2002-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入院会計チェック処理
      *****************************************************************
       2002-NYUINACCT-CHK-SEC          SECTION.
      *
           INITIALIZE                      HCM34S01-AREA
           MOVE    SYU-HOSPNUM         TO  HCM34S01-HOSPNUM
           MOVE    SYU-PTID            TO  HCM34S01-PTID
           MOVE    SYU-SRYYMD(1:6)     TO  HCM34S01-SANTEIYM
           MOVE    "1"                 TO  HCM34S01-KIKANKBN
           MOVE    SYU-SKYSTYMD        TO  HCM34S01-SKYSTYMD
           MOVE    SYU-SKYEDYMD        TO  HCM34S01-SKYEDYMD
           IF    ( SYU-SKYSTYMD        <   CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST  TO  HCM34S01-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST    TO  HCM34S01-TIMEKBN-ST
           END-IF
           IF    ( SYU-SKYEDYMD        <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED  TO  HCM34S01-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED    TO  HCM34S01-TIMEKBN-ED
           END-IF
           MOVE    SYU-HKNCOMBINUM     TO  HCM34S01-HKNCOMBI
           MOVE    SYU-SYUHKNNUM       TO  HCM34S01-HKNNUM
      *
           IF      SYU-SYUHKNNUM       =   "971"
                                       OR  "973"
               CALL    "ORCHCM34S02"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           ELSE
               CALL    "ORCHCM34S01"       USING
                                           HCM34S01-AREA
                                           SPA-AREA
           END-IF
           MOVE    SYU-SKYSTYMD        TO  WRK-SRYYM
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   HCM34S01-HENKYAKU-MAX )
               IF    ( HCM34S01-ZAISKBKBN (IDX0)
                                   =   "1" OR  "2" )
                   PERFORM VARYING WRK-SRYDAY  FROM    1   BY  1
                           UNTIL ( WRK-SRYDAY  >   31 )
                       IF    ( HCM34S01-DAY (IDX0 WRK-SRYDAY)
                                       >   ZERO )
                           MOVE    WRK-SRYYMD
                                       TO  SS003-JNISSU-YMD
                                                    (WRK-SRYDAY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       2002-NYUINACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       20011-SRYACCT-CHK-SEC            SECTION.
      *
           EVALUATE    SS003-SYORIKBN
           WHEN    "1"
               PERFORM 200111-SRYACCT-CHK-SEC
               IF    ( FLG-OK     =    1 )
                   PERFORM 2001-SRYACT-CHK-SEC
               END-IF
           WHEN    "2"
               PERFORM 2001-SRYACT-CHK-SEC
           END-EVALUATE
      *
           .
       20011-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療会計チェック処理
      *****************************************************************
       200111-SRYACCT-CHK-SEC            SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
      *
           IF   (( ACCT-ZAITEN     NOT =   ZERO )
             AND ( ACCT-DAY (IDX-KAISU WRK-SRYDAY)
                                   NOT =   ZERO )
             AND ( ACCT-SRYKBN         =   "11"
                                       OR  "12" 
                                       OR  "13"
                                       OR  "14" ))
           OR   (( ACCT-ZAITEN         =   ZERO )
             AND ( ACCT-DAY (IDX-KAISU WRK-SRYDAY)
                                   NOT =   ZERO )
             AND(  ACCT-SRYKBN         =   "11"
                                       OR  "12"
                                       OR  "99" ))
      *
               MOVE    1           TO  FLG-OK
      *
           END-IF
      *
           .
       200111-SRYACCT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療行為チェック処理
      *****************************************************************
       2001-SRYACT-CHK-SEC             SECTION.
      *
      *    診療行為検索
           MOVE    ZERO                TO  FLG-SRYACT
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SRYACT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACT
               INITIALIZE                  SRYACT-REC
           END-IF
      *
           PERFORM UNTIL ( FLG-SRYACT  NOT =   ZERO )
                    OR   ( FLG-END     NOT =   ZERO )
      *
               PERFORM VARYING IDX-SRYCD   FROM    1   BY  1
                       UNTIL ( IDX-SRYCD   >   CONST-SRYCD-MAX )
                        OR   ( SRY-SRYCD (IDX-SRYCD)   =   SPACE)
                        OR   ( FLG-END             NOT =   ZERO )
      *
                   PERFORM 200111-SRYACCT-CHK-SEC
                   IF    ( FLG-OK          =   1 )
                       PERFORM 2001-SYOSAISIN-CHK-SEC
                   END-IF
      *
                   PERFORM VARYING IDX0    FROM    1   BY  1
                           UNTIL ( IDX0    >   100 )
      *
                       IF    ( SRY-SRYCD (IDX-SRYCD)
                                           =   SS003-SRYCD (IDX0) )
                           COMPUTE SS003-SRYCD-KAISU (IDX0)
                               =   SS003-SRYCD-KAISU (IDX0)
                               +   ACCT-DAY (IDX-KAISU WRK-SRYDAY)
                       END-IF
      *
                   END-PERFORM
      *
               END-PERFORM
      *
               MOVE    SRYACT-REC      TO  MCPDATA-REC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 910-DBFETCH-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC TO  SRYACT-REC
               ELSE
                   MOVE    1           TO  FLG-SRYACT
                   INITIALIZE              SRYACT-REC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"    TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       2001-SRYACT-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初再診チェック処理
      *****************************************************************
       2001-SYOSAISIN-CHK-SEC          SECTION.
      *
           MOVE    SRY-SRYCD (IDX-SRYCD)   TO  TTNS-CHK-SRYCD
           MOVE    JYURRK-SRYYMD           TO  TTNS-CHK-YMD
      *
      *    点数マスタ検索
           PERFORM 900-TENSU-KEY-SEL-SEC
      *
      *    初診チェック
           IF   (( TNS-JITUDAY             =   2 )
             AND ( TNS-DAYCNT              =   1 ))
            OR  (  SRY-SRYCD (IDX-SRYCD)   =   "099110001"
                                            OR "111011810"
                                            OR "111012810"
                                            OR "111012610"
                                            OR "101110040" )
               MOVE    "1"                 TO  WRK-SYOSAISIN-KBN
           END-IF
      *
      *    再診チェック
           IF   (( TNS-JITUDAY             =   2          ) AND
                 ( TNS-DAYCNT              =   2          ))
           OR   (  SRY-SRYCD (IDX-SRYCD)   =   "112700850"
                                           OR  "099120001"
                                           OR  "099409901"
                                           OR  "099999902"
                                           OR  "112008350"
                                           OR  "112008350"
                                           OR  "112008850"
                                           OR  "112008850"
                                           OR  "112010150"
                                           OR  "112010150"
                                           OR  "112010650"
                                           OR  "112010650"
                                           OR  "112011710"
                                           OR  "112011710"
                                           OR  "112703050"
                                           OR  "112703450"
                                           OR  "112704450"
                                           OR  "112704850"
                                           OR  "112705850"
                                           OR  "113013910"
                                           OR  "113014010"
                                           OR  "113014110"
                                           OR  "112015810"
                                           OR  "112015950"
                                           OR  "112016210"
                                           OR  "112017610"
                                           OR  "112016410"
                                           OR  "112016550"
                                           OR  "113015810"
                                           OR  "113018410"
      *
                                           OR  "112016850"
                                           OR  "112016950"
                                           OR  "112017010"
                                           OR  "112017150"
                                           OR  "112017450"
      *
                                           OR  "113024810"
                                           OR  "113025710"
                                           OR  "112023210"
                                           OR  "112023450"
                                           OR  "112023750"
                                           OR  "112023550"
                                           OR  "112023850"
      *
                                           OR  "113031310"
                                           OR  "113031410"
                                                           )
           OR   (( SRY-SRYCD (IDX-SRYCD)   =   "101120100") AND
                 ( SRY-SRYYM              <=   "201306"   ))
           OR   (( SRY-SRYCD (IDX-SRYCD)   =   "101120040"
                                           OR  "101120050"
                                           OR  "101120060"
                                           OR  "101120070"
                                           OR  "101120080"
                                                          ) AND
                 ( SRY-SRYYM              >=   "201307"   ))
               IF    ( WRK-SYOSAISIN-KBN   =   "1" )
                   CONTINUE
               ELSE
                   MOVE    "2"             TO  WRK-SYOSAISIN-KBN
               END-IF
           END-IF
      *
           IF    ( SRY-SRYCD (IDX-SRYCD)   =  "099409903" )
                   MOVE    "1"             TO  WRK-JNISSUGEN-KBN
           END-IF
      *
           IF   (( TNS-DATAKBN             =  "2" )
             AND ( TNS-SRYKBN              =  "11" OR "12" OR "13" )
             AND ( TNS-TIMEKSNKBN      NOT =  ZERO ))
            OR  (  SRY-SRYCD (IDX-SRYCD)   =  "113005570"
                                           OR "113007770"
                                           OR "113009870"
                                           OR "113007870"
                                           OR "113007670"
                                           OR "112006070"
                                           OR "113007470"
                                           OR "113009770"
                                           OR "113007570"
                                           OR "113007370"
                                           OR "111010770"
                                           OR "113009670"
                                           OR "113007070"
                                           OR "113007170"
                                           OR "113007270" )
                   MOVE    "1"             TO  WRK-JIKANGAI-KBN
           END-IF
      *
           .
       2001-SYOSAISIN-CHK-EXT.
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
      *    DISPLAY 処理
      *****************************************************************
       800-DISPLAY-SEC                 SECTION.
      *
           DISPLAY "SS003-SYOSAISIN-KBN = " 
                    SS003-SYOSAISIN-KBN
           DISPLAY "SS003-JNISSUGEN-KBN = " 
                    SS003-JNISSUGEN-KBN
           DISPLAY "SS003-JIKANGAI-KBN  = " 
                    SS003-JIKANGAI-KBN
           DISPLAY "SS003-SRYCD-KAISU(1)= " 
                    SS003-SRYCD-KAISU(1)
           DISPLAY "SS003-SRYCD-KAISU(2)= " 
                    SS003-SRYCD-KAISU(2)
           DISPLAY "SS003-SRYCD-KAISU(3)= " 
                    SS003-SRYCD-KAISU(3)
           DISPLAY "SS003-SRYCD-KAISU(4)= " 
                    SS003-SRYCD-KAISU(4)
           DISPLAY "SS003-SRYCD-KAISU(5)= " 
                    SS003-SRYCD-KAISU(5)
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   31 )
      *
               DISPLAY IDX0 ":JYMD[" SS003-JNISSU-YMD(IDX0) "]"
                            " JGEN[" SS003-JNISSU-GEN(IDX0) "]"
           END-PERFORM
      *
           .
       800-DISPLAY-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ検索処理(KEY1)
      *****************************************************************
       900-TENSU-KEY-SEL-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD      TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD        TO  TNS-YUKOSTYMD
                                               TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF    ( MCP-RC          =   ZERO )
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
      *
      *            点数マスタ退避ワークに編集する
                   PERFORM 9001-TTNS-HEN-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSU
                   INITIALIZE                  TNS-TENSU-REC
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBCLOSECURSOR-SEC
           END-IF
      *
           .
      *
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
      *    収納マスタ検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SS003-HOSPNUM       TO  SYU-HOSPNUM
           MOVE    SS003-PTID          TO  SYU-PTID
           MOVE    SS003-NYUGAIKBN     TO  SYU-NYUGAIKBN
           MOVE    SS003-DENPNUM       TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           .
       900-SYUNOU-KEY-SEL-EXT.
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
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
           ELSE
                   INITIALIZE              PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
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
      *    会計照会検索処理
      *****************************************************************
       900-SRYACCT-KEY22-SEL-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           INITIALIZE                      SRYACCT-REC
           MOVE    SS003-HOSPNUM       TO  ACCT-HOSPNUM
           MOVE    SS003-PTID          TO  ACCT-PTID
           MOVE    SS003-NYUGAIKBN     TO  ACCT-NYUGAIKBN
           MOVE    SYU-SKYSTYMD(1:6)   TO  ACCT-SRYYM
           MOVE    SYU-HKNCOMBINUM     TO  ACCT-HKNCOMBI
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC        NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY22-SEL-EXT.
           EXIT.
      *****************************************************************
      *    会計照会FETCH処理
      *****************************************************************
       900-SRYACCT-KEY22-FET-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-SRYACCT
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key22"             TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC          NOT =   ZERO )
               MOVE    1               TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           END-IF
      *
           .
       900-SRYACCT-KEY22-FET-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHも行う)
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
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ検索処理（FHETCHは行わない)
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
       911-DBSELECT-EXT.
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
