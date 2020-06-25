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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION      DIVISION.
       PROGRAM-ID.         ORCSSYUNOUCHK.
      ****************************************************************
      *    OS           LINUX
      *
      *    PROGRAM      ORCSSYUNOUCHK  /  一括再計算該当者チェックサブ
      *  管理者            :
      *  作成日付    作業者        記述
      *  16/11/01    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.08.01    NACL-門脇    17/12/15  第三者行為現物給付に関わる
      *                                     負担金計算対応
      *  05.00.01    NACL-門脇    19/10/01  マル長対応
      ****************************************************************
       ENVIRONMENT         DIVISION.
       CONFIGURATION       SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE SECTION.
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(05).
           03  IDX2                        PIC 9(05).
           03  IDX3                        PIC 9(05).
           03  IDX4                        PIC 9(05).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-SYUNOU                  PIC 9(01).
           03  FLG-SYUMEI                  PIC 9(01).
           03  FLG-ARI                     PIC 9(01).
           03  FLG-ARI2                    PIC 9(01).
           03  FLG-GENBUTU                 PIC 9(01).
           03  FLG-GENBUTU2                PIC 9(01).
           03  FLG-GENBUTU3                PIC 9(01).
           03  FLG-GENBUTU4                PIC 9(01).
           03  FLG-SYORI-KBN               PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-SRYACT                  PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-ZAIARI                  PIC 9(01).
           03  FLG-CYOKI                   PIC 9(01).
           03  FLG-CYOKI2                  PIC 9(01).
           03  FLG-CYOKI3                  PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-PTID-TABLE.
             04  WRK-PTID-OCC              OCCURS   10000.
               05  WRK-PTID                PIC 9(10).
               05  WRK-GAITOKBN            PIC 9(02).
               05  WRK-SKYMONEY-FLG        PIC 9(01).
               05  WRK-TAISYOKBN-A         PIC 9(01).
               05  WRK-TAISYOKBN-B         PIC 9(01).
               05  WRK-CHKNGDATA           PIC 9(01).
      *
           03  WRK-PTID-TABLE2.
             04  WRK-PTID-OCC2             OCCURS   10000.
               05  WRK-PTID2               PIC 9(10).
               05  WRK-GAITOKBN2           PIC 9(02).
               05  WRK-SKYMONEY2-FLG       PIC 9(01).
               05  WRK-TAISYOKBN2-A        PIC 9(01).
               05  WRK-TAISYOKBN2-B        PIC 9(01).
               05  WRK-CHKNGDATA2          PIC 9(01).
      *
           03  WRK-PTID-TABLE3.
             04  WRK-PTID-OCC3             OCCURS   10000.
               05  WRK-PTID3               PIC 9(10).
               05  WRK-GAITOKBN3           PIC 9(02).
               05  WRK-SKYMONEY3-FLG       PIC 9(01).
               05  WRK-TAISYOKBN3-A        PIC 9(01).
               05  WRK-TAISYOKBN3-B        PIC 9(01).
               05  WRK-CHKNGDATA3          PIC 9(01).
      *
           03  WRK-SKS-TABLE.
             04  WRK-SKS-OCC               OCCURS   100.
               05  WRK-SKS-DENPNUM-END     PIC 9(07).
               05  WRK-SKS-SYUDATA         PIC X(2500).
               05  WRK-SKS-KOREIFLG        PIC X(01).
      *
           03  WRK-CHECKFLG-TABLE.
             04  WRK-CHECKFLG-OCC          OCCURS   6.
               05  WRK-CHECKFLG            PIC 9(01).
      *
           03  WRK-KOREIFLG                PIC X(01).
           03  WRK-IKTSORTKBN              PIC X(01).
      *
           03  WRK-IKT-TABLE               PIC X(64).
           03  WRK-IKT-PATHNAME            PIC X(64).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    収納データ
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    システム管理
           COPY    "CPSYSKANRI.INC".
           COPY    "CPSK1039.INC".
           COPY    "CPSK2010.INC".
      *
      *    ワーク収納データ
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING  //SYU-// BY //WRK-SYU-//.
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    負担金再計算用サブ
           COPY    "CPORCSSAIKEISAN.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "CPORCSSYUNOUCHK.INC".
           COPY  "COMMON-SPA".
      /
       PROCEDURE           DIVISION  USING
                                     LNK-ORCSSYUNOUCHK-REC
                                     SPA-AREA
                                     .
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
           INITIALIZE         LNK-ORCSSYUNOUCHK-OUT-AREA
           MOVE   ZERO   TO   LNK-ORCSSYUNOUCHK-RC
      *
           INITIALIZE         IDX-AREA
                              FLG-AREA
                              WRK-AREA
      *
           INITIALIZE         WRK-PTID-TABLE
           INITIALIZE         WRK-PTID-TABLE2
           INITIALIZE         WRK-PTID-TABLE3
      *
      *    該当診療年月の収納がある方を抽出
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key96"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key96"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
                 MOVE     ZERO     TO  FLG-ARI
                 PERFORM  VARYING  IDX1  FROM  1  BY  1
                           UNTIL  (IDX1            >  10000)
                            OR    (WRK-PTID(IDX1)  =  ZERO )
                            OR    (FLG-ARI         =   1   )
                   IF  SYU-PTID  =  WRK-PTID(IDX1)
                     MOVE    1     TO  FLG-ARI
                   END-IF
                 END-PERFORM
                 IF    FLG-ARI   =  ZERO
                 AND   IDX1     <=  10000
                   MOVE  SYU-PTID  TO  WRK-PTID(IDX1)
                 END-IF
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key96"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           ELSE
             MOVE        1         TO  LNK-ORCSSYUNOUCHK-RC
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key96"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
      *    システム管理１０３９を検索
           INITIALIZE                            SYS-1039-REC
           MOVE   "1039"                     TO  SYS-1039-KANRICD
           MOVE   "*"                        TO  SYS-1039-KBNCD
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM  TO  SYS-1039-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-1039-STYUKYMD(1:6)
           MOVE   "01"                       TO  SYS-1039-STYUKYMD(7:2)
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-1039-EDYUKYMD(1:6)
           MOVE   "01"                       TO  SYS-1039-EDYUKYMD(7:2)
           MOVE   SYS-1039-REC               TO  MCPDATA-REC
           MOVE   "DBSELECT"                 TO  MCP-FUNC
           MOVE   "tbl_syskanri"             TO  MCP-TABLE
           MOVE   "key10"                    TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"             USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF      MCP-RC  =  ZERO
               MOVE    "tbl_syskanri"        TO  MCP-TABLE
               MOVE    "key10"               TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               MOVE     SYSKANRI-REC         TO  SYS-1039-REC
           ELSE
               INITIALIZE                        SYS-1039-REC
           END-IF
           MOVE        "tbl_syskanri"        TO  MCP-TABLE
           MOVE        "key10"               TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
      *    システム管理２０１０を検索
           IF     LNK-ORCSSYUNOUCHK-SRYYM  < "202001"
             INITIALIZE                          SYS-2010-REC
           ELSE
             INITIALIZE                          SYS-2010-REC
             MOVE "2010"                     TO  SYS-2010-KANRICD
             MOVE "972"                      TO  SYS-2010-KBNCD
             MOVE LNK-ORCSSYUNOUCHK-HOSPNUM  TO  SYS-2010-HOSPNUM
             MOVE LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-2010-STYUKYMD(1:6)
             MOVE "01"                       TO  SYS-2010-STYUKYMD(7:2)
             MOVE LNK-ORCSSYUNOUCHK-SRYYM    TO  SYS-2010-EDYUKYMD(1:6)
             MOVE "01"                       TO  SYS-2010-EDYUKYMD(7:2)
             MOVE SYS-2010-REC               TO  MCPDATA-REC
             MOVE "DBSELECT"                 TO  MCP-FUNC
             MOVE "tbl_syskanri"             TO  MCP-TABLE
             MOVE "key10"                    TO  MCP-PATHNAME
             CALL "ORCDBMAIN"             USING  MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
             IF    MCP-RC  =  ZERO
               MOVE    "tbl_syskanri"        TO  MCP-TABLE
               MOVE    "key10"               TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               MOVE     SYSKANRI-REC         TO  SYS-2010-REC
             ELSE
               INITIALIZE                        SYS-2010-REC
             END-IF
             MOVE      "tbl_syskanri"        TO  MCP-TABLE
             MOVE      "key10"               TO  MCP-PATHNAME
             PERFORM    910-DB-CLOSE-SEC
           END-IF
      *
      *    一括再計算該当者チェック
           MOVE     1             TO   FLG-SYORI-KBN
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                     UNTIL  (IDX1            >  10000)
                      OR    (WRK-PTID(IDX1)  =  ZERO )
             PERFORM  100-IKATUSAIKEI-SEC
           END-PERFORM
      *
      *    返却値セット
           MOVE       ZERO      TO   IDX2
           MOVE       ZERO      TO   IDX3
           MOVE       ZERO      TO   IDX4
           PERFORM  VARYING  IDX1  FROM  1  BY  1
                     UNTIL  (IDX1            >  10000)
                      OR    (WRK-PTID(IDX1)  =  ZERO )
             IF     WRK-CHKNGDATA(IDX1)  =  ZERO
               PERFORM  200-HENKYAKU-SET-SEC
             ELSE
               PERFORM  201-HENKYAKU-SET2-SEC
             END-IF
           END-PERFORM
      *
           IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
      *      患者請求額の変更がない方：自動で一括再計算
             INITIALIZE                  WRK-PTID-TABLE
             MOVE  WRK-PTID-TABLE2  TO   WRK-PTID-TABLE
             MOVE     ZERO          TO   FLG-SYORI-KBN
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
               PERFORM  100-IKATUSAIKEI-SEC
             END-PERFORM
           ELSE
      *      患者請求額の変更がない方：該当者チェック抽出分の収納更新
             INITIALIZE                  WRK-PTID-TABLE
             MOVE  WRK-PTID-TABLE2  TO   WRK-PTID-TABLE
             PERFORM  VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
               PERFORM  300-SYUNOU-KOUSIN-SEC
             END-PERFORM
           END-IF
      *
      *    患者請求額の変更がある方：該当者チェック抽出分の収納更新
           INITIALIZE                    WRK-PTID-TABLE
           MOVE    WRK-PTID-TABLE3  TO   WRK-PTID-TABLE
           PERFORM    VARYING  IDX1  FROM  1  BY  1
                       UNTIL  (IDX1            >  10000)
                        OR    (WRK-PTID(IDX1)  =  ZERO )
             PERFORM    300-SYUNOU-KOUSIN-SEC
           END-PERFORM
      *
      *****
      *
      *    収納明細更新（入返金日、日報キーの修復）
           PERFORM      400-SYUMEI-KOUSIN-SEC
      *
           .
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      **************************************************************
      *    一括再計算
      **************************************************************
       100-IKATUSAIKEI-SEC           SECTION.
      *
           MOVE   SPACE   TO  SPA-ERRCD
      *
           MOVE   ZERO    TO  IDX2
           INITIALIZE         WRK-SKS-TABLE
           INITIALIZE         WRK-CHECKFLG-TABLE
      *
           PERFORM  1002-SRYACT-KENSAKU-SEC
      *
           MOVE   ZERO    TO  FLG-GENBUTU
           MOVE   ZERO    TO  FLG-GENBUTU2
           MOVE   ZERO    TO  FLG-GENBUTU3
           MOVE   ZERO    TO  FLG-GENBUTU4
           MOVE   ZERO    TO  FLG-CYOKI
           MOVE   ZERO    TO  FLG-CYOKI2
           MOVE   ZERO    TO  FLG-CYOKI3
      *
           MOVE   SYS-1039-IKTSORTKBN
                          TO  WRK-IKTSORTKBN
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key35"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key35"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
      ***        IF   SYU-SAIKEISANKBN     = "1"
                 IF  (SYU-SYUHKNNUM    NOT = SPACE   )
                 AND (SYU-KOH1HKNNUM       = SPACE   )
                 AND (SYU-KOH2HKNNUM       = SPACE   )
                 AND (SYU-KOH3HKNNUM       = SPACE   )
                 AND (SYU-KOH4HKNNUM       = SPACE   )
                 AND (SYU-SRYYMD(1:6)     >= "201204")
                    MOVE   1   TO  FLG-GENBUTU2
                 END-IF
                 IF  (SYU-SYUHKNNUM    NOT = SPACE)
                 AND (SYU-KOH1HKNNUM   NOT = SPACE)
                 AND (SYU-KOH1HKNNUM   NOT = "970")
                    MOVE   1   TO  FLG-GENBUTU3
                 END-IF
                 IF   SYU-KOH1HKNNUM       = "970"
                    MOVE   1   TO  FLG-GENBUTU4
                 END-IF
                 IF  (SYU-KOH1HKNNUM    =   "972" OR "974")
                 AND (SYU-KOH2HKNNUM    =   SPACE         )
                 AND (SYU-KOH3HKNNUM    =   SPACE         )
                 AND (SYU-KOH4HKNNUM    =   SPACE         )
                 AND (SYU-SRYYMD(1:6)  >=   "202001"      )
                    MOVE   1   TO  FLG-CYOKI2
                 END-IF
                 IF  (SYU-KOH2HKNNUM    =   "972" OR "974")
                 OR  (SYU-KOH3HKNNUM    =   "972" OR "974")
                 OR  (SYU-KOH4HKNNUM    =   "972" OR "974")
                    MOVE   1   TO  FLG-CYOKI3
                 END-IF
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key35"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           IF     FLG-GENBUTU4  =   1
             IF   FLG-ZAIARI    =   1
             AND  FLG-GENBUTU2  =   1
             AND  FLG-GENBUTU3  =  ZERO
                    MOVE    3     TO  FLG-GENBUTU
             ELSE
                    MOVE    2     TO  FLG-GENBUTU
             END-IF
           ELSE
             IF  (FLG-GENBUTU2  =   1)
             AND (FLG-GENBUTU3  =   1)
                    MOVE    1     TO  FLG-GENBUTU
             END-IF
           END-IF
           IF    (FLG-CYOKI2    =   1)
           AND   (FLG-CYOKI3    =   1)
                    MOVE    1     TO  FLG-CYOKI
           END-IF
      *
           IF   (FLG-CYOKI              =  1 )
      ***  AND  (SYS-2010-CYOKI-FTNKBN  = "1")
             MOVE    "1"          TO  WRK-IKTSORTKBN
           END-IF
      *
           EVALUATE   FLG-GENBUTU
             WHEN        0
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key35"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key69"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
             WHEN        1
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key82"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key83"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
             WHEN        2
      *        通常診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key88"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key89"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
      *        第三者行為診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
             WHEN        3
      *        第三者行為診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key90"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key91"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
      *        通常診療分の計算
               IF   WRK-IKTSORTKBN  =  "0" OR SPACE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key88"        TO  WRK-IKT-PATHNAME
               ELSE
                  MOVE    "tbl_syunou"   TO  WRK-IKT-TABLE
                  MOVE    "key89"        TO  WRK-IKT-PATHNAME
               END-IF
               PERFORM  1001-IKATUSAIKEI2-SEC
           END-EVALUATE
      *
           EVALUATE   TRUE
      *      第三者行為現物給付（第三者行為分の優先計算該当）
             WHEN   WRK-CHECKFLG(1) =  1
               MOVE   7   TO   WRK-GAITOKBN(IDX1)
      *      第三者行為現物給付（通常診療分の優先計算該当）
             WHEN   WRK-CHECKFLG(2) =  1
               MOVE   1   TO   WRK-GAITOKBN(IDX1)
      *      ７０歳未満現物給付
             WHEN   WRK-CHECKFLG(3) =  1
               MOVE   2   TO   WRK-GAITOKBN(IDX1)
      *      高齢者現物給付
             WHEN   WRK-CHECKFLG(4) =  1
               MOVE   3   TO   WRK-GAITOKBN(IDX1)
      *      マル長計算
             WHEN   WRK-CHECKFLG(5) =  1
               MOVE   8   TO   WRK-GAITOKBN(IDX1)
      *      収納更新未処理
             WHEN   WRK-CHECKFLG(6) =  1
               MOVE   4   TO   WRK-GAITOKBN(IDX1)
      *      該当者チェック抽出分
             WHEN   OTHER
               MOVE   5   TO   WRK-GAITOKBN(IDX1)
           END-EVALUATE
      *
           .
       100-IKATUSAIKEI-EXT.
           EXIT.
      *
      **************************************************************
      *    一括再計算
      **************************************************************
       1001-IKATUSAIKEI2-SEC           SECTION.
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   WRK-IKT-TABLE                TO  MCP-TABLE
           MOVE   WRK-IKT-PATHNAME             TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      WRK-IKT-TABLE           TO  MCP-TABLE
             MOVE      WRK-IKT-PATHNAME        TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               MOVE     ZERO     TO  FLG-ARI
               PERFORM  VARYING  IDX3  FROM  1  BY  1
                         UNTIL  (IDX3                   >  100 )
                          OR    (WRK-SKS-DENPNUM-END(IDX3)
                                                        =  ZERO)
                          OR    (FLG-ARI                =   1  )
                 IF  SYU-DENPNUM  =  WRK-SKS-DENPNUM-END(IDX3)
                   MOVE    1     TO  FLG-ARI
                   IF   FLG-SYORI-KBN  =  1
                     MOVE   WRK-SKS-SYUDATA(IDX3)
                                 TO  WRK-SYUNOU-REC
                     MOVE   WRK-SKS-KOREIFLG(IDX3)
                                 TO  WRK-KOREIFLG
                     PERFORM    10011-SYUNOU-DATA-CHECK-SEC
                   END-IF
                 END-IF
               END-PERFORM
      *
               IF  (SYU-DENPJTIKBN  NOT = "3" )
               AND (FLG-ARI             = ZERO)
                 INITIALIZE                 LNK-ORCSSAIKEISAN-REC
                 MOVE   SYU-HOSPNUM    TO   LNK-ORCSSKS-HOSPNUM
                 MOVE   SYU-NYUGAIKBN  TO   LNK-ORCSSKS-NYUGAIKBN
                 MOVE   SYU-PTID       TO   LNK-ORCSSKS-PTID
                 MOVE   SYU-DENPNUM    TO   LNK-ORCSSKS-DENPNUM
                 MOVE   SYU-SRYKA      TO   LNK-ORCSSKS-SRYKA
                 MOVE   WRK-IKTSORTKBN TO   LNK-ORCSSKS-IKTSORTKBN
                 MOVE   SYS-1039-IKTKEISANKBN
                                       TO   LNK-ORCSSKS-IKTKEISANKBN
                 EVALUATE   FLG-GENBUTU
                   WHEN        1
                   WHEN        2
                        MOVE   "1"     TO   LNK-ORCSSKS-IKTKEISANKBN
                        MOVE   "1"     TO   LNK-ORCSSKS-IKTGENKEIFLG
                   WHEN        3
                        MOVE   "1"     TO   LNK-ORCSSKS-IKTKEISANKBN
                        MOVE   "2"     TO   LNK-ORCSSKS-IKTGENKEIFLG
                 END-EVALUATE
                 IF    FLG-CYOKI  =  1
                    MOVE  "1"          TO   LNK-ORCSSKS-IKTKEISANKBN
                    IF  SYS-2010-CYOKI-FTNKBN  = "1"
                        MOVE   "1"     TO   LNK-ORCSSKS-IKTCYOKIFLG
                    END-IF
                 END-IF
                 MOVE     "1"          TO   LNK-ORCSSKS-IKTJISSIFLG
      *
                 IF     FLG-SYORI-KBN  =  1
                   MOVE   "1"          TO   LNK-ORCSSKS-IKTCHECKFLG
                 END-IF
      *
                 CALL    "ORCSSAIKEISAN"     USING
                                             SPA-AREA
                                             LNK-ORCSSAIKEISAN-REC
      *
                 PERFORM  VARYING  IDX3  FROM  1  BY  1
                           UNTIL  (IDX3                      >  15  )
                            OR    (LNK-ORCSSKS-DENPNUM-END(IDX3)
                                                             =  ZERO)
                   ADD    1        TO   IDX2
                   IF    IDX2     <=  100
                     MOVE   LNK-ORCSSKS-DENPNUM-END(IDX3)
                                   TO   WRK-SKS-DENPNUM-END(IDX2)
                     IF   FLG-SYORI-KBN  =  1
                       MOVE   LNK-ORCSSKS-SYUNOU-DATA(IDX3)
                                   TO   WRK-SKS-SYUDATA(IDX2)
                       MOVE   LNK-ORCSSKS-KOREIFLG(IDX3)
                                   TO   WRK-SKS-KOREIFLG(IDX2)
                     END-IF
                   END-IF
                 END-PERFORM
      *
                 IF   FLG-SYORI-KBN  =  1
                   MOVE     ZERO     TO  FLG-ARI2
                   PERFORM  VARYING  IDX3  FROM  1  BY  1
                             UNTIL  (IDX3                      >  15  )
                              OR    (LNK-ORCSSKS-DENPNUM-END(IDX3)
                                                               =  ZERO)
                              OR    (FLG-ARI2                  =   1  )
                     IF  SYU-DENPNUM  =  LNK-ORCSSKS-DENPNUM-END(IDX3)
                       MOVE    1     TO  FLG-ARI2
                       MOVE   LNK-ORCSSKS-SYUNOU-DATA(IDX3)
                                     TO  WRK-SYUNOU-REC
                       MOVE   LNK-ORCSSKS-KOREIFLG(IDX3)
                                     TO  WRK-KOREIFLG
                       PERFORM    10011-SYUNOU-DATA-CHECK-SEC
                     END-IF
                   END-PERFORM
                   IF    FLG-ARI2  =  ZERO
                     MOVE      1     TO  WRK-CHKNGDATA(IDX1)
                   END-IF
                 END-IF
               END-IF
      *
               MOVE    WRK-IKT-TABLE           TO  MCP-TABLE
               MOVE    WRK-IKT-PATHNAME        TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        WRK-IKT-TABLE           TO  MCP-TABLE
           MOVE        WRK-IKT-PATHNAME        TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
       1001-IKATUSAIKEI2-EXT.
           EXIT.
      *
      **************************************************************
      *    収納データチェック
      **************************************************************
       10011-SYUNOU-DATA-CHECK-SEC           SECTION.
      *
           IF     WRK-SYU-SKJ-FTNGAKU(2) NOT = ZERO
             EVALUATE   FLG-GENBUTU
      *        第三者行為現物給付（第三者行為分の優先計算該当）
               WHEN       3
                   MOVE     1    TO   WRK-CHECKFLG(1)
      *        第三者行為現物給付（通常診療分の優先計算該当）
               WHEN       2
                   MOVE     1    TO   WRK-CHECKFLG(2)
      *        ７０歳未満現物給付
      *        高齢者現物給付
               WHEN       1
                 IF   WRK-KOREIFLG  =  SPACE
                   MOVE     1    TO   WRK-CHECKFLG(3)
                 ELSE
                   MOVE     1    TO   WRK-CHECKFLG(4)
                 END-IF
             END-EVALUATE
           END-IF
      *    マル長計算
           IF   (FLG-CYOKI              =  1 )
      ***  AND  (SYS-2010-CYOKI-FTNKBN  = "1")
             MOVE    1    TO   WRK-CHECKFLG(5)
           END-IF
      *    収納更新未処理
           IF    SYU-ACCT-UPDKBN  NOT =  SPACE
             MOVE    1    TO   WRK-CHECKFLG(6)
           END-IF
      *
           IF    SYU-SKYMONEY      NOT =  WRK-SYU-SKYMONEY
      *      患者請求額の変更がある方
             MOVE       1     TO    WRK-SKYMONEY-FLG(IDX1)
             MOVE       1     TO    WRK-TAISYOKBN-A(IDX1)
           ELSE
      *      患者請求額の変更がない方（データ項目値が変更となる収納あり）
             IF (SYU-TOTAL-HKNTEN      NOT = WRK-SYU-TOTAL-HKNTEN      )
             OR (SYU-RSISHOSHIN-MONEY  NOT = WRK-SYU-RSISHOSHIN-MONEY  )
             OR (SYU-RSISAISHIN-MONEY  NOT = WRK-SYU-RSISAISHIN-MONEY  )
             OR (SYU-RSISDO-MONEY      NOT = WRK-SYU-RSISDO-MONEY      )
             OR (SYU-RSIETC-MONEY      NOT = WRK-SYU-RSIETC-MONEY      )
      *
             OR (SYU-HKNTEKMONEY       NOT = WRK-SYU-HKNTEKMONEY       )
             OR (SYU-SYUCOMPFTN        NOT = WRK-SYU-SYUCOMPFTN        )
             OR (SYU-SYUCOMPFTN-ENTANI NOT = WRK-SYU-SYUCOMPFTN-ENTANI )
             OR (SYU-KOH-FTN           NOT = WRK-SYU-KOH-FTN           )
             OR (SYU-KOH-FTN-ENTANI    NOT = WRK-SYU-KOH-FTN-ENTANI    )
             OR (SYU-KOH1HKNFTNMONEY   NOT = WRK-SYU-KOH1HKNFTNMONEY   )
             OR (SYU-KOH1COMPFTN       NOT = WRK-SYU-KOH1COMPFTN       )
             OR (SYU-KOH1COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH1COMPFTN-ENTANI)
             OR (SYU-KOH2HKNFTNMONEY   NOT = WRK-SYU-KOH2HKNFTNMONEY   )
             OR (SYU-KOH2COMPFTN       NOT = WRK-SYU-KOH2COMPFTN       )
             OR (SYU-KOH2COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH2COMPFTN-ENTANI)
             OR (SYU-KOH3HKNFTNMONEY   NOT = WRK-SYU-KOH3HKNFTNMONEY   )
             OR (SYU-KOH3COMPFTN       NOT = WRK-SYU-KOH3COMPFTN       )
             OR (SYU-KOH3COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH3COMPFTN-ENTANI)
             OR (SYU-KOH4HKNFTNMONEY   NOT = WRK-SYU-KOH4HKNFTNMONEY   )
             OR (SYU-KOH4COMPFTN       NOT = WRK-SYU-KOH4COMPFTN       )
             OR (SYU-KOH4COMPFTN-ENTANI
                                       NOT = WRK-SYU-KOH4COMPFTN-ENTANI)
      *
             OR (SYU-RSIJIBAI-SKYMONEY NOT = WRK-SYU-RSIJIBAI-SKYMONEY )
             OR (SYU-SKJ-FTNGAKU(1)    NOT = WRK-SYU-SKJ-FTNGAKU(1)    )
             OR (SYU-SKJ-FTNGAKU(2)    NOT = WRK-SYU-SKJ-FTNGAKU(2)    )
             OR (SYU-SKJ-FTNGAKU(3)    NOT = WRK-SYU-SKJ-FTNGAKU(3)    )
             OR (SYU-SKJ-FTNDAY(1)     NOT = WRK-SYU-SKJ-FTNDAY(1)     )
             OR (SYU-SKJ-FTNDAY(2)     NOT = WRK-SYU-SKJ-FTNDAY(2)     )
             OR (SYU-SKJ-FTNDAY(3)     NOT = WRK-SYU-SKJ-FTNDAY(3)     )
             OR (SYU-YKZKENNUM         NOT = WRK-SYU-YKZKENNUM         )
             OR (SYU-KOHTAIYKZKBN      NOT = WRK-SYU-KOHTAIYKZKBN      )
             OR (SYU-NYNSHOKAISU       NOT = WRK-SYU-NYNSHOKAISU       )
             OR (SYU-SAIKEISANKBN      NOT = WRK-SYU-SAIKEISANKBN      )
      *
             OR (SYU-SECMONEY          NOT = ZERO AND 9                )
             OR (SYU-ACCT-UPDKBN       NOT = SPACE                     )
               MOVE     1     TO    WRK-TAISYOKBN-B(IDX1)
             END-IF
           END-IF
      *
           .
       10011-SYUNOU-DATA-CHECK-EXT.
           EXIT.
      *
      **************************************************************
      *    診療行為情報検索
      **************************************************************
       1002-SRYACT-KENSAKU-SEC           SECTION.
      *
           MOVE  ZERO             TO   FLG-ZAIARI
      *
           INITIALIZE                  SRYACT-REC
           MOVE  LNK-ORCSSYUNOUCHK-HOSPNUM
                                  TO   SRY-HOSPNUM
           MOVE  LNK-ORCSSYUNOUCHK-NYUGAIKBN
                                  TO   SRY-NYUGAIKBN
           MOVE  WRK-PTID(IDX1)   TO   SRY-PTID
           MOVE  LNK-ORCSSYUNOUCHK-SRYYM
                                  TO   SRY-SRYYM
           MOVE  "099999931"      TO   SRY-SRYCD(1)
           MOVE  "099999931"      TO   SRY-SRYCD(2)
           MOVE  "099999931"      TO   SRY-SRYCD(3)
           MOVE  "099999931"      TO   SRY-SRYCD(4)
           MOVE  "099999931"      TO   SRY-SRYCD(5)
           MOVE  SRYACT-REC       TO   MCPDATA-REC
           MOVE  "DBSELECT"       TO   MCP-FUNC
           MOVE  "tbl_sryact"     TO   MCP-TABLE
           MOVE  "key7"           TO   MCP-PATHNAME
           CALL  "ORCDBMAIN"   USING   MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           IF     MCP-RC  =  ZERO
             MOVE    "tbl_sryact"     TO   MCP-TABLE
             MOVE    "key7"           TO   MCP-PATHNAME
             PERFORM  900-SRYACT-READ-SEC
             PERFORM  UNTIL  FLG-SRYACT  =  1
                       OR    FLG-ZAIARI  =  1
               INITIALIZE                  SRYACCT-REC
               MOVE  SRY-HOSPNUM      TO   ACCT-HOSPNUM
               MOVE  SRY-NYUGAIKBN    TO   ACCT-NYUGAIKBN
               MOVE  SRY-SRYKA        TO   ACCT-SRYKA
               MOVE  SRY-PTID         TO   ACCT-PTID
               MOVE  SRY-SRYYM        TO   ACCT-SRYYM
               MOVE  SRY-ZAINUM       TO   ACCT-ZAINUM
               MOVE  SRYACCT-REC      TO   MCPDATA-REC
               MOVE  "DBSELECT"       TO   MCP-FUNC
               MOVE  "tbl_sryacct"    TO   MCP-TABLE
               MOVE  "key31"          TO   MCP-PATHNAME
               CALL  "ORCDBMAIN"   USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
               IF     MCP-RC  =  ZERO
                 MOVE    "tbl_sryacct"     TO   MCP-TABLE
                 MOVE    "key31"           TO   MCP-PATHNAME
                 PERFORM  900-SRYACCT-READ-SEC
                 IF    FLG-SRYACCT  =  ZERO
                   IF  ACCT-ZAIKAISU  NOT =  ZERO
                     INITIALIZE                 HKNCOMBI-REC
                     MOVE  ACCT-HOSPNUM    TO   COMB-HOSPNUM
                     MOVE  ACCT-PTID       TO   COMB-PTID
                     MOVE  ACCT-HKNCOMBI   TO   COMB-HKNCOMBINUM
                     MOVE  HKNCOMBI-REC    TO   MCPDATA-REC
                     MOVE  "DBSELECT"      TO   MCP-FUNC
                     MOVE  "tbl_hkncombi"  TO   MCP-TABLE
                     MOVE  "key"           TO   MCP-PATHNAME
                     CALL  "ORCDBMAIN"  USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
                     IF      MCP-RC  =  ZERO
                       MOVE    "tbl_hkncombi"   TO   MCP-TABLE
                       MOVE    "key"            TO   MCP-PATHNAME
                       PERFORM  900-HKNCOMBI-READ-SEC
                       IF    FLG-HKNCOMBI  =  ZERO
                         IF  COMB-KOH1HKNNUM  =  "970"
                           MOVE     1           TO   FLG-ZAIARI
                         END-IF
                       END-IF
                     END-IF
                     MOVE    "tbl_hkncombi"     TO   MCP-TABLE
                     MOVE    "key"              TO   MCP-PATHNAME
                     PERFORM  910-DB-CLOSE-SEC
                   END-IF
                 END-IF
               END-IF
               MOVE    "tbl_sryacct"            TO   MCP-TABLE
               MOVE    "key31"                  TO   MCP-PATHNAME
               PERFORM  910-DB-CLOSE-SEC
      *
               MOVE    "tbl_sryact"             TO   MCP-TABLE
               MOVE    "key7"                   TO   MCP-PATHNAME
               PERFORM  900-SRYACT-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_sryact"             TO   MCP-TABLE
           MOVE        "key7"                   TO   MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
      *
       1002-SRYACT-KENSAKU-EXT.
           EXIT.
      *
      **************************************************************
      *    返却値セット１
      **************************************************************
       200-HENKYAKU-SET-SEC           SECTION.
      *
           IF     WRK-GAITOKBN(IDX1)   NOT =  ZERO
             IF  (WRK-TAISYOKBN-A(IDX1)    =  ZERO)
             AND (WRK-TAISYOKBN-B(IDX1)    =  ZERO)
               CONTINUE
             ELSE
               ADD        1     TO   IDX2
               MOVE  WRK-PTID(IDX1)
                                TO   LNK-ORCSSYUNOUCHK-PTID(IDX2)
               IF    WRK-SKYMONEY-FLG(IDX1)  =  ZERO
                 IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
                   MOVE   8     TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
                 ELSE
                   MOVE   7     TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
                 END-IF
               ELSE
                 MOVE    ZERO   TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
               END-IF
               MOVE  WRK-GAITOKBN(IDX1)
                                TO   LNK-ORCSSYUNOUCHK-GAITOKBN(IDX2)
      *
               IF    WRK-SKYMONEY-FLG(IDX1)  =  ZERO
                 IF  LNK-ORCSSYUNOUCHK-OPTION1  =  "1"
                     ADD    1     TO   IDX3
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID2(IDX3)
                 ELSE
                   IF  WRK-GAITOKBN(IDX1) = 1 OR 2 OR 3 OR 4 OR
                                            5 OR 7 OR 8
                     ADD    1     TO   IDX3
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID2(IDX3)
                     MOVE   WRK-GAITOKBN(IDX1)
                                  TO   WRK-GAITOKBN2(IDX3)
                   END-IF
                 END-IF
               ELSE
                 IF  WRK-GAITOKBN(IDX1) = 1 OR 2 OR 3 OR 4 OR
                                          5 OR 7 OR 8
                     ADD    1     TO   IDX4
                     MOVE   WRK-PTID(IDX1)
                                  TO   WRK-PTID3(IDX4)
                     MOVE   WRK-GAITOKBN(IDX1)
                                  TO   WRK-GAITOKBN3(IDX4)
                 END-IF
               END-IF
             END-IF
           END-IF
      *
           .
       200-HENKYAKU-SET-EXT.
           EXIT.
      *
      **************************************************************
      *    返却値セット２
      **************************************************************
       201-HENKYAKU-SET2-SEC           SECTION.
      *
           ADD        1     TO   IDX2
           MOVE  WRK-PTID(IDX1)
                            TO   LNK-ORCSSYUNOUCHK-PTID(IDX2)
           MOVE       9     TO   LNK-ORCSSYUNOUCHK-JTIFLG(IDX2)
           MOVE       6     TO   LNK-ORCSSYUNOUCHK-GAITOKBN(IDX2)
      *
           .
       201-HENKYAKU-SET2-EXT.
           EXIT.
      *
      **************************************************************
      *    収納データ更新
      **************************************************************
       300-SYUNOU-KOUSIN-SEC           SECTION.
      *
           INITIALIZE                              SYUNOU-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SYU-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SYU-NYUGAIKBN
           MOVE   WRK-PTID(IDX1)               TO  SYU-PTID
           MOVE   LNK-ORCSSYUNOUCHK-SRYYM      TO  SYU-SRYYMD(1:6)
           MOVE   "__"                         TO  SYU-SRYYMD(7:2)
           MOVE   SYUNOU-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syunou"                 TO  MCP-TABLE
           MOVE   "key35"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syunou"            TO  MCP-TABLE
             MOVE      "key35"                 TO  MCP-PATHNAME
             PERFORM    900-SYUNOU-READ-SEC
             PERFORM    UNTIL  FLG-SYUNOU  =  1
               IF  SYU-DENPJTIKBN  NOT = "3"
                 EVALUATE  WRK-GAITOKBN(IDX1)
                   WHEN       1
                     IF (SYU-SYUHKNNUM  =  SPACE OR
                                           "971" OR "973" OR "975" OR
                                           "068" OR "069" OR "040"  )
                     OR (SYU-SYUHKNNUM(1:2)  =  "98" OR "90" OR "91")
                        MOVE   ZERO  TO  SYU-SECMONEY
                     ELSE
                        MOVE    4    TO  SYU-SECMONEY
                     END-IF
                   WHEN       2
                     IF (SYU-SYUHKNNUM  =  SPACE OR
                                           "971" OR "973" OR "975" OR
                                           "068" OR "069" OR "040"  )
                     OR (SYU-SYUHKNNUM(1:2)  =  "98" OR "90" OR "91")
                        MOVE   ZERO  TO  SYU-SECMONEY
                     ELSE
                        MOVE    1    TO  SYU-SECMONEY
                     END-IF
                   WHEN       3
                     IF (SYU-SYUHKNNUM  =  SPACE OR
                                           "971" OR "973" OR "975" OR
                                           "068" OR "069" OR "040"  )
                     OR (SYU-SYUHKNNUM(1:2)  =  "98" OR "90" OR "91")
                        MOVE   ZERO  TO  SYU-SECMONEY
                     ELSE
                        MOVE    2    TO  SYU-SECMONEY
                     END-IF
                   WHEN       4
                   WHEN       5
                        MOVE    7    TO  SYU-SECMONEY
                   WHEN       7
                     IF (SYU-SYUHKNNUM  =  SPACE OR
                                           "971" OR "973" OR "975" OR
                                           "068" OR "069" OR "040"  )
                     OR (SYU-SYUHKNNUM(1:2)  =  "98" OR "90" OR "91")
                        MOVE   ZERO  TO  SYU-SECMONEY
                     ELSE
                        MOVE    6    TO  SYU-SECMONEY
                     END-IF
                   WHEN       8
                     IF (SYU-SYUHKNNUM  =  SPACE OR
                                           "971" OR "973" OR "975" OR
                                           "068" OR "069" OR "040"  )
                     OR (SYU-SYUHKNNUM(1:2)  =  "98" OR "90" OR "91")
                        MOVE   ZERO  TO  SYU-SECMONEY
                     ELSE
                        MOVE    5    TO  SYU-SECMONEY
                     END-IF
                 END-EVALUATE
      *
                 MOVE   SYUNOU-REC             TO  MCPDATA-REC
                 MOVE  "DBUPDATE"              TO  MCP-FUNC
                 MOVE  "tbl_syunou"            TO  MCP-TABLE
                 MOVE  "key"                   TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"          USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               END-IF
               MOVE    "tbl_syunou"            TO  MCP-TABLE
               MOVE    "key35"                 TO  MCP-PATHNAME
               PERFORM  900-SYUNOU-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syunou"            TO  MCP-TABLE
           MOVE        "key35"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
       300-SYUNOU-KOUSIN-EXT.
           EXIT.
      *
      **************************************************************
      *    収納明細更新
      **************************************************************
       400-SYUMEI-KOUSIN-SEC           SECTION.
      *
           INITIALIZE                              SYUMEI-REC
           MOVE   LNK-ORCSSYUNOUCHK-HOSPNUM    TO  SMEI-HOSPNUM
           MOVE   LNK-ORCSSYUNOUCHK-NYUGAIKBN  TO  SMEI-NYUGAIKBN
           MOVE   SYUMEI-REC                   TO  MCPDATA-REC
           MOVE   "DBSELECT"                   TO  MCP-FUNC
           MOVE   "tbl_syumei"                 TO  MCP-TABLE
           MOVE   "key18"                      TO  MCP-PATHNAME
           CALL   "ORCDBMAIN"               USING  MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC  =  ZERO
             MOVE      "tbl_syumei"            TO  MCP-TABLE
             MOVE      "key18"                 TO  MCP-PATHNAME
             PERFORM    900-SYUMEI-READ-SEC
             PERFORM    UNTIL  FLG-SYUMEI  =  1
               IF  SMEI-NYUHEN-YMD  =  SPACE
                 DISPLAY "<SMEI> PTID=" SMEI-PTID
                         " DENPNUM=" SMEI-DENPNUM
                         " DENPEDANUM=" SMEI-DENPEDANUM
      *
                 MOVE   SMEI-CREYMD       TO  SMEI-NYUHEN-YMD
                 MOVE   SMEI-CREYMD       TO  SMEI-DAILYKEY(1:8)
                 MOVE   SMEI-CREHMS(1:4)  TO  SMEI-DAILYKEY(9:4)
      *
                 MOVE   SYUMEI-REC        TO  MCPDATA-REC
                 MOVE  "DBUPDATE"         TO  MCP-FUNC
                 MOVE  "tbl_syumei"       TO  MCP-TABLE
                 MOVE  "key"              TO  MCP-PATHNAME
                 CALL  "ORCDBMAIN"     USING  MCPAREA
                                              MCPDATA-REC
                                              SPA-AREA
               END-IF
               MOVE    "tbl_syumei"            TO  MCP-TABLE
               MOVE    "key18"                 TO  MCP-PATHNAME
               PERFORM  900-SYUMEI-READ-SEC
             END-PERFORM
           END-IF
           MOVE        "tbl_syumei"            TO  MCP-TABLE
           MOVE        "key18"                 TO  MCP-PATHNAME
           PERFORM      910-DB-CLOSE-SEC
      *
           .
       400-SYUMEI-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納データ読込
      *****************************************************************
       900-SYUNOU-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
      *
       900-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細読込
      *****************************************************************
       900-SYUMEI-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUMEI-REC
               MOVE    ZERO            TO  FLG-SYUMEI
           ELSE
               INITIALIZE                  SYUMEI-REC
               MOVE    1               TO  FLG-SYUMEI
           END-IF
      *
           .
      *
       900-SYUMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスタ−読込
      *****************************************************************
       900-SRYACT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
      *
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ−読込
      *****************************************************************
       900-SRYACCT-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
               MOVE    ZERO            TO  FLG-SRYACCT
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
      *
           .
      *
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
      *
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       910-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       910-DB-CLOSE-EXT.
           EXIT.
      *
