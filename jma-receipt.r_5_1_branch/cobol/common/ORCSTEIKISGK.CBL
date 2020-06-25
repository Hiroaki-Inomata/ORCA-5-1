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
       PROGRAM-ID.                     ORCSTEIKISGK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 共通
      *  コンポーネント名  : 定期請求差額取得サブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  09/12/14    NACL−太田    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-SYUNOU              PIC 9(01).
           03  FLG-SYUDAY              PIC 9(01).
           03  FLG-PTTEIKIRRK          PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX-KBT                 PIC 9(05).
           03  IDX-SYU                 PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DENPNUM             PIC 9(07).
           03  WRK-UPDNYUGAIKBN        PIC X(01).
           03  WRK-NYUGAIKBN           PIC X(01).
           03  WRK-SKYKBN              PIC X(01).
           03  WRK-SKYSTYMD            PIC X(08).
           03  WRK-SKYEDYMD            PIC X(08).
      *
       01  CONST-AREA.
           03  CONST-H180401           PIC X(08)   VALUE "20060401".
           03  CONST-DENPNUM           PIC 9(07)   VALUE 9999001.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
       01  SYUDAY-REC.
           COPY    "CPSYUDAY.INC".
      *
       01  PTTEIKIRRK-REC.
           COPY    "CPPTTEIKIRRK.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    負担金計算サブ
       01  LNK-ORCS02-REC.
           COPY  "CPORCS02.INC".
       01  LNK-SYUNOU-REC-T.
         02 LNK-SYUNOU-REC     OCCURS 10.
           COPY  "CPSYUNOU.INC"    REPLACING  //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUDAY-REC-T.
         02 LNK-SYUDAY-REC     OCCURS 10.
           COPY  "CPSYUDAY.INC"  REPLACING  //SDAY-// BY //LNK-SDAY-//.
      *
      *    分娩入院チェックサブ
           COPY    "CPORCSBUNBENCHK.INC".
      *
      *    入院請求期間取得サブ
           COPY    "CPORCSS006.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "MCPAREA".
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSTEIKISGK.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           STEIKISGK-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           IF    ( FLG-END         =   ZERO )
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           MOVE    CONST-DENPNUM   TO  WRK-DENPNUM
           MOVE    ZERO            TO  STEIKISGK-RC
                                       STEIKISGK-S02-RC
      *
           PERFORM 900-SYUNOU-DEL6-SEC
           IF    ( FLG-SYUNOU     NOT  =   ZERO )
               MOVE    -3         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           PERFORM 900-SYUDAY-DEL6-SEC
           IF    ( FLG-SYUDAY     NOT  =   ZERO )
               MOVE    -4         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           IF    ( STEIKISGK-RC    =   ZERO )
               PERFORM 2001-SYUNOU-TAIHI-SEC
           END-IF
      *
           IF    ( STEIKISGK-RC    =   ZERO )
               PERFORM 2001-SYUNOU-DUMMY-EDIT-SEC
           END-IF
      *
           IF    ( STEIKISGK-RC    =   ZERO )
               IF    ( STEIKISGK-TAIIN-SKYSTYMD    NOT =   SPACE )
                   PERFORM 2001-SYUNOU-TAIIN-EDIT-SEC
               END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    収納データ退避処理
      *****************************************************************
       2001-SYUNOU-TAIHI-SEC           SECTION.
      *
           INITIALIZE                  STEIKISGK-KBT-G
      *
           PERFORM 900-PTTEIKIRRK-KEY9-SEL-SEC
      *
           PERFORM UNTIL ( FLG-PTTEIKIRRK  NOT =   ZERO )
      *
               PERFORM 900-SYUNOU-KEY-SEL-SEC
      *
               MOVE    ZERO                TO  IDX-KBT
      *
               EVALUATE    PTTEIKIRRK-KBT
               WHEN    "1"
                   MOVE    1               TO  IDX-KBT
               WHEN    "2"
                   MOVE    2               TO  IDX-KBT
               WHEN    "3"
                   MOVE    3               TO  IDX-KBT
               END-EVALUATE
      *
               IF    ( IDX-KBT            >=  1 )
                AND  ( IDX-KBT            <=  3 )
      *
                   MOVE    PTTEIKIRRK-SKYSTYMD
                                       TO  STEIKISGK-SKYSTYMD (IDX-KBT)
                   MOVE    PTTEIKIRRK-SKYEDYMD
                                       TO  STEIKISGK-SKYEDYMD (IDX-KBT)
                   COMPUTE STEIKISGK-SYU-SKYMONEY (IDX-KBT)
                       =   STEIKISGK-SYU-SKYMONEY (IDX-KBT)
                       +   SYU-SKYMONEY
      *
               END-IF
      *
               PERFORM 900-PTTEIKIRRK-KEY9-FET-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBCLOSECURSOR-SEC
      *
           MOVE    "3"                TO   WRK-UPDNYUGAIKBN
           MOVE    "1"                TO   WRK-NYUGAIKBN
      *
           PERFORM 900-SYUNOU-UPD1-SEC
           IF    ( FLG-SYUNOU     NOT  =   ZERO )
               MOVE    -1         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           PERFORM 900-SYUDAY-UPD1-SEC
           IF    ( FLG-SYUDAY     NOT  =   ZERO )
               MOVE    -2         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           .
       2001-SYUNOU-TAIHI-EXT.
           EXIT.
      *****************************************************************
      *    ダミー収納データ作成処理
      *****************************************************************
       2001-SYUNOU-DUMMY-EDIT-SEC      SECTION.
      *
           PERFORM VARYING IDX-KBT    FROM    1   BY  1
                   UNTIL ( IDX-KBT    >   3 )
      *
               IF    ( STEIKISGK-SKYSTYMD (IDX-KBT) NOT =   SPACE )
      *
                   MOVE    IDX-KBT         TO   STEIKISGK-KBT-MAX
      *
                   MOVE    "2"             TO   WRK-SKYKBN
                   MOVE    STEIKISGK-SKYSTYMD (IDX-KBT)
                                           TO   WRK-SKYSTYMD
                   MOVE    STEIKISGK-SKYEDYMD (IDX-KBT)
                                           TO   WRK-SKYEDYMD
                   PERFORM 20011-NYUFTN-CALC-SEC
                   IF    ( LNK-S02-RC      =   ZERO )
                       PERFORM VARYING IDX-SYU FROM    1   BY  1
                               UNTIL ( IDX-SYU >   10 )
                                OR   ( LNK-SYU-HOSPNUM (IDX-SYU)
                                               =   ZERO )
      *
                           PERFORM 20011-SYUNOU-DUMMY-EDIT-SEC
      *
                           PERFORM 20011-SYUDAY-DUMMY-EDIT-SEC
      *
                           COMPUTE WRK-DENPNUM =   WRK-DENPNUM +   1
      *
                       END-PERFORM
                   ELSE
                       MOVE    1           TO  STEIKISGK-RC
                       MOVE    LNK-S02-RC  TO  STEIKISGK-S02-RC
                       GO  TO  2001-SYUNOU-DUMMY-EDIT-EXT
                   END-IF
      *
                   COMPUTE STEIKISGK-SAGAKU (IDX-KBT)
                       =   STEIKISGK-FTN-SKYMONEY (IDX-KBT)
                       -   STEIKISGK-SYU-SKYMONEY (IDX-KBT)
      *
               END-IF
      *
           END-PERFORM
      *
           .
       2001-SYUNOU-DUMMY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    退院収納計算処理
      *****************************************************************
       2001-SYUNOU-TAIIN-EDIT-SEC      SECTION.
      *
           MOVE    "1"             TO   WRK-SKYKBN
           MOVE    STEIKISGK-TAIIN-SKYSTYMD
                                   TO   WRK-SKYSTYMD
           MOVE    STEIKISGK-TAIIN-SKYEDYMD
                                   TO   WRK-SKYEDYMD
           PERFORM 20011-NYUFTN-CALC-SEC
           IF    ( LNK-S02-RC      =   ZERO )
               PERFORM VARYING IDX-SYU FROM    1   BY  1
                       UNTIL ( IDX-SYU >   10 )
                        OR   ( LNK-SYU-HOSPNUM (IDX-SYU)
                                   =   ZERO )
      *
                   IF    ( STEIKISGK-SRYYMD (1:6)
                                   =   LNK-SYU-SRYYMD (IDX-SYU)(1:6))
                       COMPUTE STEIKISGK-TAIIN-SKYMONEY
                            =  STEIKISGK-TAIIN-SKYMONEY
                            +  LNK-SYU-SKYMONEY (IDX-SYU)
                   END-IF
      *
                END-PERFORM
           ELSE
               MOVE    2           TO  STEIKISGK-RC
               MOVE    LNK-S02-RC  TO  STEIKISGK-S02-RC
           END-IF
      *
           .
       2001-SYUNOU-TAIIN-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    負担金計算処理
      *****************************************************************
       20011-NYUFTN-CALC-SEC           SECTION.
      *
           INITIALIZE              LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
           INITIALIZE                  SBUNBENCHK-AREA
           MOVE    STEIKISGK-PTID  TO  SBUNBENCHK-PTID
           MOVE    STEIKISGK-RRKNUM
                                   TO  SBUNBENCHK-RRKNUM
           CALL    "ORCSBUNBENCHK" USING
                                   SBUNBENCHK-AREA
                                   SPA-AREA
      *
           INITIALIZE                  SS006-AREA
           MOVE    STEIKISGK-PTID  TO  SPA-PTID
           MOVE    WRK-SKYSTYMD    TO  SS006-SKYSTYMD
           MOVE    WRK-SKYEDYMD    TO  SS006-SKYEDYMD
           MOVE    WRK-SKYKBN      TO  SS006-SKYKBN
           CALL    "ORCSS006"          USING
                                       SS006-AREA
                                       SPA-AREA
      *
           MOVE    SPA-HOSPNUM     TO  LNK-S02-HOSPNUM
           MOVE    "1"             TO  LNK-S02-NYUGAIKBN
           MOVE    STEIKISGK-PTID  TO  LNK-S02-PTID
      *
           MOVE    WRK-SKYSTYMD    TO  LNK-S02-SKYSTYMD
           MOVE    WRK-SKYEDYMD    TO  LNK-S02-SKYEDYMD
           IF    ( LNK-S02-SKYSTYMD    <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ST
                                   TO  LNK-S02-DOJITUFLG-ST
               MOVE    SS006-TIMEKBN-ST
                                   TO  LNK-S02-TIMEKBN-ST
           END-IF
           IF    ( LNK-S02-SKYEDYMD    <  CONST-H180401 )
               CONTINUE
           ELSE
               MOVE    SS006-DOJITUFLG-ED
                                   TO  LNK-S02-DOJITUFLG-ED
               MOVE    SS006-TIMEKBN-ED
                                   TO  LNK-S02-TIMEKBN-ED
           END-IF
           MOVE    SPA-SYSYMD      TO  LNK-S02-SYSYMD
           IF    ( WRK-SKYKBN          =   "1" )
               IF    ( SBUNBENCHK-BUNBEN   =   ZERO )
                   MOVE    "1"     TO  LNK-S02-SYORI-KBN
               ELSE
                   MOVE    "8"     TO  LNK-S02-SYORI-KBN
               END-IF
           ELSE
               MOVE    "A"         TO  LNK-S02-TEIKI-KBN
               IF    ( SBUNBENCHK-BUNBEN   =   ZERO )
                   MOVE    "2"     TO  LNK-S02-SYORI-KBN
               ELSE
                   MOVE    "9"     TO  LNK-S02-SYORI-KBN
               END-IF
           END-IF
           CALL    "ORCSNYUFTN"    USING
                                   SPA-AREA
                                   LNK-ORCS02-REC
                                   LNK-SYUNOU-REC-T
                                   LNK-SYUDAY-REC-T
      *
           .
       20011-NYUFTN-CALC-EXT.
           EXIT.
      *****************************************************************
      *    収納ダミーデータ編集処理
      *****************************************************************
       20011-SYUNOU-DUMMY-EDIT-SEC     SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDX-SYU)
                                           TO  SYUNOU-REC
           MOVE    WRK-DENPNUM             TO  SYU-DENPNUM
           MOVE    STEIKISGK-SKYSTYMD (IDX-KBT)
                                           TO  SYU-SKYSTYMD
           MOVE    STEIKISGK-SKYEDYMD (IDX-KBT)
                                           TO  SYU-SKYEDYMD
           MOVE    SYUNOU-REC              TO  MCPDATA-REC
           MOVE    "tbl_syunou"            TO  MCP-TABLE
           MOVE    "key"                   TO  MCP-PATHNAME
           PERFORM 910-DBINSERT-SEC
           IF    ( MCP-RC              NOT =   ZERO )
               MOVE    -5                  TO  STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           COMPUTE STEIKISGK-FTN-SKYMONEY (IDX-KBT)
               =   STEIKISGK-FTN-SKYMONEY (IDX-KBT)
               +   SYU-SKYMONEY
      *
           .
       20011-SYUNOU-DUMMY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    日別収納ダミーデータ編集処理
      *****************************************************************
       20011-SYUDAY-DUMMY-EDIT-SEC     SECTION.
      *
           MOVE    LNK-SYUDAY-REC (IDX-SYU)
                                       TO  SYUDAY-REC
      *
           IF    ( SDAY-DAYINFFLG      =   "1" )
      *
               MOVE    WRK-DENPNUM     TO  SDAY-DENPNUM
               MOVE    SYUDAY-REC      TO  MCPDATA-REC
               MOVE    "tbl_syuday"    TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 910-DBINSERT-SEC
               IF    ( MCP-RC      NOT =   ZERO )
                   MOVE    -6          TO  STEIKISGK-RC
                   PERFORM 800-ABORT-SEC
               END-IF
           END-IF
      *
           .
       20011-SYUDAY-DUMMY-EDIT-EXT.
           EXIT.
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                    SECTION.
      *
           PERFORM 900-SYUNOU-DEL6-SEC
           IF    ( FLG-SYUNOU     NOT  =   ZERO )
               MOVE    -7         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           PERFORM 900-SYUDAY-DEL6-SEC
           IF    ( FLG-SYUDAY     NOT  =   ZERO )
               MOVE    -8         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           MOVE    "1"            TO   WRK-UPDNYUGAIKBN
           MOVE    "3"            TO   WRK-NYUGAIKBN
      *
           PERFORM 900-SYUNOU-UPD1-SEC
           IF    ( FLG-SYUNOU     NOT  =   ZERO )
               MOVE    -9         TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           PERFORM 900-SYUDAY-UPD1-SEC
           IF    ( FLG-SYUDAY     NOT  =   ZERO )
               MOVE    -10        TO   STEIKISGK-RC
               PERFORM 800-ABORT-SEC
           END-IF
      *
           IF    ( STEIKISGK-RC   =    1 )
               INITIALIZE              STEIKISGK-KBT-G
           END-IF
      *
           .
       300-END-EXT.
           EXIT.
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       800-ABORT-SEC                   SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "abort"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           PERFORM 990-EXIT-PROGRAM-SEC
      *
           .
       800-ABORT-EXT.
           EXIT.
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       900-SYUNOU-UPD1-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM    TO   SYU-HOSPNUM
           MOVE    WRK-UPDNYUGAIKBN
                                  TO   SYU-NYUGAIKBN
           MOVE    WRK-NYUGAIKBN  TO   SYU-SELNYUGAIKBN
           MOVE    STEIKISGK-PTID TO   SYU-PTID
           MOVE    "2"            TO   SYU-CREATEKBN
           MOVE    STEIKISGK-SRYYMD(1:6)
                                  TO   SYU-SRYYMD(1:6)
           MOVE    "%"            TO   SYU-SRYYMD(7:1)
           MOVE    SYUNOU-REC     TO   MCPDATA-REC
           MOVE    "tbl_syunou"   TO   MCP-TABLE
           MOVE    "upd1"         TO   MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    1          TO   FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-UPD1-EXT.
           EXIT.
      *****************************************************************
      *    日別収納更新処理
      *****************************************************************
       900-SYUDAY-UPD1-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUDAY
      *
           INITIALIZE                  SYUDAY-REC
           MOVE    SPA-HOSPNUM    TO   SDAY-HOSPNUM
           MOVE    WRK-UPDNYUGAIKBN
                                  TO   SDAY-NYUGAIKBN
           MOVE    WRK-NYUGAIKBN  TO   SDAY-DAYINFFLG
           MOVE    STEIKISGK-PTID TO   SDAY-PTID
           MOVE    "2"            TO   SDAY-CREATEKBN
           MOVE    STEIKISGK-SRYYMD
                                  TO   SDAY-SKYYM
           MOVE    SYUDAY-REC     TO   MCPDATA-REC
           MOVE    "tbl_syuday"   TO   MCP-TABLE
           MOVE    "upd1"         TO   MCP-PATHNAME
           PERFORM 910-DBUPDATE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    1          TO  FLG-SYUDAY
           END-IF
      *
      *
           .
      *
       900-SYUDAY-UPD1-EXT.
           EXIT.
      *****************************************************************
      *    収納削除処理
      *****************************************************************
       900-SYUNOU-DEL6-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUNOU
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM    TO   SYU-HOSPNUM
           MOVE    "1"            TO   SYU-NYUGAIKBN
           MOVE    STEIKISGK-PTID TO   SYU-PTID
           MOVE    CONST-DENPNUM  TO   SYU-DENPNUM
           MOVE    SYUNOU-REC     TO   MCPDATA-REC
           MOVE    "tbl_syunou"   TO   MCP-TABLE
           MOVE    "del6"         TO   MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    1          TO   FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-DEL6-EXT.
           EXIT.
      *****************************************************************
      *    日別収納削除処理
      *****************************************************************
       900-SYUDAY-DEL6-SEC             SECTION.
      *
           MOVE    ZERO            TO  FLG-SYUDAY
      *
           INITIALIZE                  SYUDAY-REC
           MOVE    SPA-HOSPNUM    TO   SDAY-HOSPNUM
           MOVE    "1"            TO   SDAY-NYUGAIKBN
           MOVE    STEIKISGK-PTID TO   SDAY-PTID
           MOVE    CONST-DENPNUM  TO   SDAY-DENPNUM
           MOVE    SYUDAY-REC     TO   MCPDATA-REC
           MOVE    "tbl_syuday"   TO   MCP-TABLE
           MOVE    "del6"         TO   MCP-PATHNAME
           PERFORM 910-DBDELETE-SEC
           IF    ( MCP-RC         NOT  =   ZERO )
               MOVE    1          TO  FLG-SYUDAY
           END-IF
      *
           .
      *
       900-SYUDAY-DEL6-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY9-SEL-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           INITIALIZE                      PTTEIKIRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTTEIKIRRK-HOSPNUM
           MOVE    STEIKISGK-PTID      TO  PTTEIKIRRK-PTID
           MOVE    STEIKISGK-SRYYMD(1:6)
                                       TO  PTTEIKIRRK-SRYYM
           MOVE    STEIKISGK-RRKNUM    TO  PTTEIKIRRK-RRKNUM
           MOVE    PTTEIKIRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY9-SEL-EXT.
           EXIT.
      *****************************************************************
      *    患者定期請求履歴検索処理
      *****************************************************************
       900-PTTEIKIRRK-KEY9-FET-SEC     SECTION.
      *
           MOVE    ZERO                TO  FLG-PTTEIKIRRK
      *
           MOVE    "tbl_ptteikirrk"    TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBFETCH-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  PTTEIKIRRK-REC
           ELSE
               MOVE    1               TO  FLG-PTTEIKIRRK
               INITIALIZE                  PTTEIKIRRK-REC
           END-IF
      *
           .
       900-PTTEIKIRRK-KEY9-FET-EXT.
           EXIT.
      *****************************************************************
      *    収納検索処理
      *****************************************************************
       900-SYUNOU-KEY-SEL-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUNOU
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    PTTEIKIRRK-PTID     TO  SYU-PTID
           MOVE    PTTEIKIRRK-DENPNUM  TO  SYU-DENPNUM
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF    ( MCP-RC              =   ZERO )
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE                  SYUNOU-REC
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
       910-DBCLOSECURSOR-SEC                 SECTION.
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
      *****************************************************************
      *    ＤＢＤＥＬＥＴＥ処理
      *****************************************************************
       910-DBDELETE-SEC                 SECTION.
      *
           MOVE    "DBDELETE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBDELETE-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       910-DBINSERT-SEC                 SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
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
      *    ＤＢＵＰＤＡＴＥ処理
      *****************************************************************
       910-DBUPDATE-SEC                 SECTION.
      *
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       910-DBUPDATE-EXT.
           EXIT.
      *****************************************************************
      *    EXIT PROGRAM
      *****************************************************************
       990-EXIT-PROGRAM-SEC            SECTION.
      *
           DISPLAY "STEIKISGK-RC = "   STEIKISGK-RC
      *
           EXIT PROGRAM
      *
           .
       990-EXIT-PROGRAM-EXT.
           EXIT.
