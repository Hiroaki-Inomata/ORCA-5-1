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
       PROGRAM-ID.             ORCSKOREIFTNCHK.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 
      *  コンポーネント名  : 高齢者の一部負担金額記載に関するチェック
      *  管理者            : 
      *  作成日付    作業者        記述
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  03.02.01    NACL-門脇    06/08/10  高齢者上限額経過措置対応
      *  03.03.01    NACL-門脇    06/09/29  Ｈ１８年１０月改正に伴う一部
      *                                     負担金編集対応
      *  03.03.02    NACL-門脇    06/10/10  老齢福祉年金についての対応
      *  03.03.03    NACL-門脇    06/11/02  減額・免除・支払猶予対応
      *  03.04.01    NACL-門脇    06/11/20  高額４回目以降対応
      *  03.04.02    NACL-門脇    07/03/14  保険欄一部負担金記載修正
      *  03.04.03    NACL-門脇    07/04/01  現物給付対応
      *  03.04.04    NACL-門脇    07/06/13  現物給付高額４回目公費併用時対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  03.05.02    NACL-門脇    07/09/03  減額減免全国公費対応
      *  03.05.03    NACL-門脇    07/12/03  北海道特定疾患（道単独事業）対応
      *  04.02.01    NACL-門脇    08/04/01  平成２０年４月改正対応
      *  04.02.02    NACL-門脇    08/05/01  後期高齢者月途中保険者変更対応
      *
      *  04.03.01    NACL-門脇    09/01/05  平成２１年１月改正対応
      *  04.03.02    NACL-門脇    09/02/26  長崎県地方公費対応
      *
      *  04.04.01    NACL-門脇    09/04/13  平成２１年５月改正対応
      *  04.04.02    NACL-門脇    09/06/01  平成２１年５月改正対応
      *                                     （東京都１５１）
      *  04.04.03    NACL-門脇    09/09/07  愛知県単独５１対応
      *
      *  04.05.01    NACL-門脇    11/10/05  岡山県西粟倉村退職者国保２割対応
      *                                     修正（平成２３年９月まで）
      *  04.05.02    NACL-門脇    12/05/16  愛知・三重県地方公費対応
      *
      *  04.06.01    NACL-門脇    11/01/14  保険別請求チェック表の本体取込み
      *  04.06.02    NACL-門脇    13/04/10  公費を含む場合の主保険の一部負担額
      *                                     （１円単位）（判定用）の編集追加
      *                                     公費を含む場合の主保険の一部負担額
      *                                     の編集修正
      *
      *  04.07.01    NACL-門脇    13/07/10  自賠責（第三者行為）対応
      *  04.07.02    NACL-門脇    13/11/11  減額（円）の対応
      *  04.08.01    NACL-門脇    16/11/11  減額（円超）の対応
      *  04.08.02    NACL-門脇    17/06/02  減免（減額（割））の負担金計算・
      *                                     レセプト対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-PTINF             PIC 9(01).
           03  FLG-HKNCOMBI          PIC 9(01).
           03  FLG-SYUNOU            PIC 9(01).
           03  FLG-PTHKNINF          PIC 9(01).
           03  FLG-TOKUREI-HKNJYA    PIC 9(01).
           03  FLG-CYOKINOMI         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX1                  PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-HKNCOMBINUM-TBL.
             04  WRK-HKNCOMBINUM-OCC    OCCURS  50.
               05  WRK-HKNCOMBINUM   PIC 9(04).
               05  WRK-KYURATE       PIC 9(03).
           03  WRK-FTNKEI            PIC 9(09).
           03  WRK-FTNKEI2           PIC 9(09).
           03  WRK-FTNKEI-XXX        PIC 9(09).
           03  WRK-KINGAKU           PIC 9(09).
           03  WRK-FTN-KOH-XXX       PIC 9(09).
           03  WRK-FTN-KOH-YYY       PIC 9(09).
           03  WRK-GENGAKU-RITUX     PIC 9(03).
           03  WRK-FTNKEI-GENGAKU    PIC 9(09).
           03  WRK-GENGAKU-KIN       PIC 9(07).
      *
           03  WRK-CYOKI-HKNCOMBINUM PIC 9(04).
           03  WRK-KOHID-CYOKI       PIC 9(10).
      *
      *    共通領域
           COPY    "MCPAREA".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "CPORCSKOREIFTNCHK.INC".
      *
grpsys     COPY    "COMMON-SPA".
      *
       PROCEDURE                    DIVISION    USING
           ORCSKOREIFTNCHKAREA
grpsys     SPA-AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                 SECTION.
      *
           MOVE    ZERO                TO  LNK-KOREIFTNCHK-RC
           INITIALIZE                      LNK-KOREIFTNCHK-OUT-AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    パラメータチェック
           PERFORM   100-PRM-CHK-SEC
      *
           IF    LNK-KOREIFTNCHK-RC    =   ZERO
               PERFORM   200-MAIN-SEC
           END-IF
      *
           .
       000-PROC-EXT.
      *
           EXIT PROGRAM
           .
      *
      *****************************************************************
      *    パラメータチェック
      *****************************************************************
       100-PRM-CHK-SEC                     SECTION.
      *
      *    患者マスター検索
           INITIALIZE                          PTINF-REC
grpsys     MOVE    LNK-KOREIFTNCHK-HOSPNUM TO  PTINF-HOSPNUM
           MOVE    LNK-KOREIFTNCHK-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
           END-IF
      *
           IF    LNK-KOREIFTNCHK-HKNNUM    = SPACE
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
           END-IF
      *
           IF    LNK-KOREIFTNCHK-HKNID     =  ZERO
               MOVE    10              TO  LNK-KOREIFTNCHK-RC
           END-IF
      *
           .
       100-PRM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主取得
      *****************************************************************
       200-MAIN-SEC            SECTION.
      *
           PERFORM    250-PTHKNINF-KENSAKU-SEC
      *
           MOVE    ZERO                    TO  IDX1
      *    対象の保険組合せ番号取得
           INITIALIZE                          HKNCOMBI-REC
grpsys     MOVE    LNK-KOREIFTNCHK-HOSPNUM TO  COMB-HOSPNUM
           MOVE    LNK-KOREIFTNCHK-PTID    TO  COMB-PTID
           MOVE    LNK-KOREIFTNCHK-HKNID   TO  COMB-HKNID
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key5"                  TO  MCP-PATHNAME
           PERFORM 900-HKNCOMBI-SELECT-SEC
           IF      FLG-HKNCOMBI            NOT =   ZERO
               MOVE    01                  TO  LNK-KOREIFTNCHK-RC
               GO  TO  200-MAIN-EXT
           ELSE
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
               PERFORM
                    UNTIL   (IDX1             >=    50 )   OR
                            (FLG-HKNCOMBI  NOT =   ZERO)
      *
                 IF   COMB-KOH1HKNNUM  =  "970"
                   CONTINUE
                 ELSE
                   ADD     1     TO     IDX1
                   MOVE    COMB-HKNCOMBINUM
                                 TO     WRK-HKNCOMBINUM(IDX1)
                   PERFORM   210-KYURATE-HENSYU-SEC
                 END-IF
      *
                 MOVE   "tbl_hkncombi"   TO  MCP-TABLE
                 MOVE   "key5"           TO  MCP-PATHNAME
                 PERFORM 910-HKNCOMBI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE     "tbl_hkncombi"   TO  MCP-TABLE
           MOVE     "key5"           TO  MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
           MOVE    LNK-KOREIFTNCHK-GENGAKU-KIN  TO  WRK-GENGAKU-KIN
      *    収納マスタから一部負担額チェック等
           MOVE       ZERO      TO    FLG-CYOKINOMI
           MOVE       ZERO      TO    WRK-CYOKI-HKNCOMBINUM
           IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
               PERFORM       220-NYUIN-SYUKEI-SEC
           ELSE
               IF  LNK-KOREIFTNCHK-RECEKA     =  SPACE
                   PERFORM   230-GAIRAI-SYUKEI-SEC
               END-IF
           END-IF
      *
           MOVE       ZERO      TO    WRK-KOHID-CYOKI
           IF     LNK-KOREIFTNCHK-RC         =   ZERO
             IF   LNK-KOREIFTNCHK-CYOKINOMI  =   ZERO
                CONTINUE
             ELSE
                IF    FLG-CYOKINOMI              =    1
                AND   WRK-CYOKI-HKNCOMBINUM  NOT =   ZERO
                  INITIALIZE                       HKNCOMBI-REC
                  MOVE LNK-KOREIFTNCHK-HOSPNUM TO  COMB-HOSPNUM
                  MOVE LNK-KOREIFTNCHK-PTID    TO  COMB-PTID
                  MOVE WRK-CYOKI-HKNCOMBINUM   TO  COMB-HKNCOMBINUM
                  MOVE "tbl_hkncombi"          TO  MCP-TABLE
                  MOVE "key"                   TO  MCP-PATHNAME
                  PERFORM 900-HKNCOMBI-SELECT-SEC
                  IF   FLG-HKNCOMBI            =   ZERO
                    MOVE "tbl_hkncombi"        TO  MCP-TABLE
                    MOVE "key"                 TO  MCP-PATHNAME
                    PERFORM 910-HKNCOMBI-READ-SEC
                    IF FLG-HKNCOMBI            =   ZERO
                      IF  COMB-KOH1HKNNUM = "972" OR "974"
                       MOVE   COMB-KOH1ID      TO  WRK-KOHID-CYOKI
                      END-IF
                    END-IF
                  END-IF
                  MOVE  "tbl_hkncombi"         TO  MCP-TABLE
                  MOVE  "key"                  TO  MCP-PATHNAME
                  PERFORM   920-DB-CLOSE-SEC
      *
                  IF  LNK-KOREIFTNCHK-KOHID-CYOKI  NOT = ZERO
                  AND LNK-KOREIFTNCHK-KOHID-CYOKI   =
                                              WRK-KOHID-CYOKI
                    CONTINUE
                  ELSE
                    INITIALIZE     LNK-KOREIFTNCHK-OUT-AREA
                  END-IF
                ELSE
                    INITIALIZE     LNK-KOREIFTNCHK-OUT-AREA
                END-IF
             END-IF
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      ***************************************************************
      *    患者保険情報検索
      ***************************************************************
       250-PTHKNINF-KENSAKU-SEC           SECTION.
      *
           MOVE       ZERO               TO    FLG-TOKUREI-HKNJYA
      *
           INITIALIZE                          PTHKNINF-REC
grpsys     MOVE   LNK-KOREIFTNCHK-HOSPNUM
                                         TO    PTHKN-HOSPNUM
           MOVE   LNK-KOREIFTNCHK-PTID   TO    PTHKN-PTID
           MOVE   LNK-KOREIFTNCHK-HKNID  TO    PTHKN-HKNID
           MOVE   PTHKNINF-REC           TO    MCPDATA-REC
           MOVE   "DBSELECT"             TO    MCP-FUNC
           MOVE   "tbl_pthkninf"         TO    MCP-TABLE
           MOVE   "key"                  TO    MCP-PATHNAME
           CALL   "ORCDBMAIN"            USING MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC            =     ZERO
               MOVE    "tbl_pthkninf"    TO    MCP-TABLE
               MOVE    "key"             TO    MCP-PATHNAME
               PERFORM  900-PTHKNINF-READ-SEC
               IF     FLG-PTHKNINF   =     ZERO
                  IF   PTHKN-HKNJANUM  = "67330753"
                      MOVE    1          TO    FLG-TOKUREI-HKNJYA
                  END-IF
               END-IF
           END-IF
      *
           MOVE     "tbl_pthkninf"       TO    MCP-TABLE
           MOVE     "key"                TO    MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
           .
      *
       250-PTHKNINF-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険の負担割合編集
      *****************************************************************
       210-KYURATE-HENSYU-SEC            SECTION.
      *
           EVALUATE  LNK-KOREIFTNCHK-NENREIKBN
             WHEN     "1"
              IF  COMB-KOH1-HKNKOHSBTKBN   =   "3"
                EVALUATE   COMB-KOH1PAYKBN
                   WHEN   "01"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "02"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "03"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
              ELSE
               IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
                EVALUATE   COMB-HOJOKBN
                   WHEN   "1"
                   WHEN   "5"
                   WHEN   "9"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "2"
                   WHEN   "6"
                   WHEN   "8"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "3"
                   WHEN   "7"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
               ELSE
                EVALUATE   COMB-HOJOKBN
                   WHEN   "1"
                   WHEN   "4"
                   WHEN   "9"
                       MOVE   10     TO   WRK-KYURATE(IDX1)
                   WHEN   "2"
                   WHEN   "5"
                   WHEN   "8"
                       MOVE   20     TO   WRK-KYURATE(IDX1)
                   WHEN   "3"
                   WHEN   "6"
                   WHEN   "7"
                       MOVE   30     TO   WRK-KYURATE(IDX1)
                END-EVALUATE
               END-IF
              END-IF
             WHEN     "2"
              IF      LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
               EVALUATE    COMB-HKNNUM
                 WHEN    "060"
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                       IF    COMB-HOJOKBN = "0" OR "1" OR "4" OR "5"
                          EVALUATE   COMB-HOJOKBN
                            WHEN   "0"
                            WHEN   "4"
                              MOVE   ZERO    TO   WRK-KYURATE(IDX1)
                            WHEN   "1"
                            WHEN   "5"
                              MOVE   10      TO   WRK-KYURATE(IDX1)
                          END-EVALUATE
                       ELSE
                          MOVE       20      TO   WRK-KYURATE(IDX1)
                       END-IF
                    ELSE
                       EVALUATE   COMB-HOJOKBN
                         WHEN   "0"
                         WHEN   "4"
                            MOVE   ZERO      TO   WRK-KYURATE(IDX1)
                         WHEN   "1"
                         WHEN   "5"
                            MOVE   10        TO   WRK-KYURATE(IDX1)
                         WHEN   "2"
                         WHEN   "6"
                            MOVE   20        TO   WRK-KYURATE(IDX1)
                         WHEN   "3"
                            MOVE   30        TO   WRK-KYURATE(IDX1)
                       END-EVALUATE
                    END-IF
                 WHEN    OTHER
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                    ELSE
                       IF   COMB-HOJOKBN  =  SPACE
                         IF  (FLG-TOKUREI-HKNJYA       =   1      )
                         AND (LNK-KOREIFTNCHK-SRYYM   <=  "201109")
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                         ELSE
                           MOVE     30       TO   WRK-KYURATE(IDX1)
                         END-IF
                       END-IF
                    END-IF
               END-EVALUATE
              ELSE
               EVALUATE    COMB-HKNNUM
                 WHEN    "060"
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                       IF    COMB-HOJOKBN = "0" OR "1" OR "4"
                          EVALUATE   COMB-HOJOKBN
                            WHEN   "0"
                              MOVE   ZERO    TO   WRK-KYURATE(IDX1)
                            WHEN   "1"
                            WHEN   "4"
                              MOVE   10      TO   WRK-KYURATE(IDX1)
                          END-EVALUATE
                       ELSE
                          MOVE       20      TO   WRK-KYURATE(IDX1)
                       END-IF
                    ELSE
                       EVALUATE   COMB-HOJOKBN
                         WHEN   "0"
                            MOVE   ZERO      TO   WRK-KYURATE(IDX1)
                         WHEN   "1"
                         WHEN   "4"
                            MOVE   10        TO   WRK-KYURATE(IDX1)
                         WHEN   "2"
                         WHEN   "5"
                            MOVE   20        TO   WRK-KYURATE(IDX1)
                         WHEN   "3"
                         WHEN   "6"
                            MOVE   30        TO   WRK-KYURATE(IDX1)
                       END-EVALUATE
                    END-IF
                 WHEN    OTHER
                    IF ((LNK-KOREIFTNCHK-SRYYM   <=  "200803") AND
                        (LNK-KOREIFTNCHK-AGE          <   3  ))
                    OR ((LNK-KOREIFTNCHK-SRYYM   >=  "200804") AND
                        (LNK-KOREIFTNCHK-SYUGAKUKBN   =   1  ))
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                    ELSE
                       IF   COMB-HOJOKBN  =  SPACE
                         IF  (FLG-TOKUREI-HKNJYA       =   1      )
                         AND (LNK-KOREIFTNCHK-SRYYM   <=  "201109")
                           MOVE     20       TO   WRK-KYURATE(IDX1)
                         ELSE
                           MOVE     30       TO   WRK-KYURATE(IDX1)
                         END-IF
                       END-IF
                    END-IF
               END-EVALUATE
              END-IF
           END-EVALUATE
      *
           .
       210-KYURATE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（入院）
      *****************************************************************
       220-NYUIN-SYUKEI-SEC            SECTION.
      *
      *    一部負担額チェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              INITIALIZE                            SYUNOU-REC
grpsys        MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE    "tbl_syunou"              TO  MCP-TABLE
              MOVE    "key19"                   TO  MCP-PATHNAME
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  220-NYUIN-SYUKEI-EXT
              ELSE
                  MOVE   "tbl_syunou"           TO  MCP-TABLE
                  MOVE   "key19"                TO  MCP-PATHNAME
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
      *
                   IF     SYU-KOH-HKNNUM(1) =  "972" OR "974"
                      IF    FLG-CYOKINOMI   =  ZERO
                         MOVE     1     TO    FLG-CYOKINOMI
                         MOVE   WRK-HKNCOMBINUM(IDX1)
                                        TO    WRK-CYOKI-HKNCOMBINUM
                      END-IF
      *
                      EVALUATE   LNK-KOREIFTNCHK-NENREIKBN
                        WHEN       "1"
                          PERFORM     2201-NYUIN-KOREI-SYUKEI-SEC
                        WHEN       "2"
                          PERFORM     2202-NYUGAI-WAKODO-SYUKEI-SEC
                      END-EVALUATE
                   ELSE
                      MOVE        9     TO    FLG-CYOKINOMI
                      MOVE      ZERO    TO    WRK-CYOKI-HKNCOMBINUM
                   END-IF
                   MOVE   "tbl_syunou"          TO  MCP-TABLE
                   MOVE   "key19"               TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_syunou"             TO  MCP-TABLE
              MOVE     "key19"                  TO  MCP-PATHNAME
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       220-NYUIN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（入院）（高齢者）
      *****************************************************************
       2201-NYUIN-KOREI-SYUKEI-SEC            SECTION.
      *
           ADD    SYU-KOH-FTN      TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
           COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                               * 10
                               * WRK-KYURATE(IDX1)
                               / 100
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
      *
           IF     SYU-KOH-HKNNUM(1) = "972"
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE  5000   TO   WRK-KINGAKU
               ELSE
                  MOVE  10000  TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
           END-IF
           IF     SYU-KOH-HKNNUM(1) = "974"
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE  10000  TO   WRK-KINGAKU
               ELSE
                  MOVE  20000  TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
           END-IF
      *
           .
       2201-NYUIN-KOREI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（入外）（若人）
      *****************************************************************
       2202-NYUGAI-WAKODO-SYUKEI-SEC            SECTION.
      *
           ADD    SYU-KOH-FTN      TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
           COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                               * 10
                               * WRK-KYURATE(IDX1)
                               / 100
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
           IF   LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
             CONTINUE
           ELSE
             COMPUTE  WRK-FTNKEI = WRK-FTNKEI
                                 + SYU-GRP-SGKMONEY
           END-IF
      *
           IF     SYU-KOH-HKNNUM(1) = "972"
               IF  LNK-KOREIFTNCHK-TOKU75 = 3
                  MOVE  5000   TO   WRK-KINGAKU
               ELSE
                  MOVE  10000  TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
           END-IF
           IF     SYU-KOH-HKNNUM(1) = "974"
               IF  LNK-KOREIFTNCHK-TOKU75 = 3
                  MOVE  10000  TO   WRK-KINGAKU
               ELSE
                  MOVE  20000  TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
           END-IF
      *
           .
       2202-NYUGAI-WAKODO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタから一部負担額チェック等（外来）
      *****************************************************************
       230-GAIRAI-SYUKEI-SEC            SECTION.
      *
      *    一部負担額チェック
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                 UNTIL   (IDX1                  >    50 )   OR
                         (WRK-HKNCOMBINUM(IDX1) =   ZERO)
      *
              INITIALIZE                            SYUNOU-REC
grpsys        MOVE    LNK-KOREIFTNCHK-HOSPNUM   TO  SYU-HOSPNUM
              MOVE    LNK-KOREIFTNCHK-NYUGAIKBN TO  SYU-NYUGAIKBN
              MOVE    LNK-KOREIFTNCHK-PTID      TO  SYU-PTID
              MOVE    LNK-KOREIFTNCHK-SRYYM     TO  SYU-SRYYMD(1:6)
              MOVE    "__"                      TO  SYU-SRYYMD(7:2)
              MOVE    WRK-HKNCOMBINUM(IDX1)     TO  SYU-HKNCOMBINUM
              MOVE   "tbl_syunou"               TO  MCP-TABLE
              MOVE   "key41"                    TO  MCP-PATHNAME
              PERFORM 900-SYUNOU-SELECT-SEC
              IF      FLG-SYUNOU            NOT =   ZERO
                  MOVE    01                    TO  LNK-KOREIFTNCHK-RC
                  GO  TO  230-GAIRAI-SYUKEI-EXT
              ELSE
                  MOVE   "tbl_syunou"           TO  MCP-TABLE
                  MOVE   "key41"                TO  MCP-PATHNAME
                  PERFORM 910-SYUNOU-READ-SEC
                  PERFORM  UNTIL
                             (FLG-SYUNOU        =    1)
      *
                   IF     SYU-KOH-HKNNUM(1) =  "972" OR "974"
                      IF    FLG-CYOKINOMI   =  ZERO
                         MOVE     1     TO    FLG-CYOKINOMI
                         MOVE   WRK-HKNCOMBINUM(IDX1)
                                        TO    WRK-CYOKI-HKNCOMBINUM
                      END-IF
      *
                      EVALUATE   LNK-KOREIFTNCHK-NENREIKBN
                        WHEN       "1"
                          PERFORM     2301-GAIRAI-KOREI-SYUKEI-SEC
                        WHEN       "2"
                          PERFORM     2202-NYUGAI-WAKODO-SYUKEI-SEC
                      END-EVALUATE
                   ELSE
                      MOVE        9     TO    FLG-CYOKINOMI
                      MOVE      ZERO    TO    WRK-CYOKI-HKNCOMBINUM
                   END-IF
                   MOVE   "tbl_syunou"          TO  MCP-TABLE
                   MOVE   "key41"               TO  MCP-PATHNAME
                   PERFORM 910-SYUNOU-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE     "tbl_syunou"             TO  MCP-TABLE
              MOVE     "key41"                  TO  MCP-PATHNAME
              PERFORM   920-DB-CLOSE-SEC
      *
           END-PERFORM
      *
           .
       230-GAIRAI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額集計（外来）（高齢者）
      *****************************************************************
       2301-GAIRAI-KOREI-SYUKEI-SEC            SECTION.
      *
           ADD    SYU-KOH-FTN      TO  LNK-KOREIFTNCHK-KOHFTNGAKU
      *
           COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                               * 10
                               * WRK-KYURATE(IDX1)
                               / 100
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
           COMPUTE  WRK-FTNKEI = WRK-FTNKEI
                               + SYU-GRP-SGKMONEY
      *
           IF     SYU-KOH-HKNNUM(1) = "972"
             IF   LNK-KOREIFTNCHK-TEISYOTOKU  NOT =  SPACE
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE  4000     TO   WRK-KINGAKU
               ELSE
                  MOVE  8000     TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
             ELSE
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE  5000     TO   WRK-KINGAKU
               ELSE
                  MOVE  10000    TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
             END-IF
           END-IF
           IF     SYU-KOH-HKNNUM(1) = "974"
             IF   LNK-KOREIFTNCHK-KEIKASOCHI =  SPACE
               IF  LNK-KOREIFTNCHK-TOKU75 = 1 OR 2
                  MOVE  10000    TO   WRK-KINGAKU
               ELSE
                  MOVE  20000    TO   WRK-KINGAKU
               END-IF
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
             ELSE
               MOVE     12000    TO   WRK-KINGAKU
               PERFORM  270-ICHIBUFTN-GEN-SEC
               PERFORM  260-ICHIBUFTN-KOH-SET-SEC
             END-IF
           END-IF
      *
           .
       2301-GAIRAI-KOREI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険欄の一部負担金額セット（公費含む場合）
      *****************************************************************
       260-ICHIBUFTN-KOH-SET-SEC               SECTION.
      *
           IF  WRK-FTN-KOH-XXX   >   WRK-KINGAKU
              MOVE  WRK-KINGAKU      TO   WRK-FTN-KOH-XXX
           END-IF
           COMPUTE  WRK-FTN-KOH-YYY  =  WRK-KINGAKU - WRK-FTN-KOH-XXX
           IF      WRK-FTNKEI    >   WRK-FTN-KOH-YYY
              ADD   WRK-FTN-KOH-YYY  TO  LNK-KOREIFTNCHK-ICHIBUGAKU
                                         WRK-FTN-KOH-XXX
           ELSE
              ADD   WRK-FTNKEI       TO  LNK-KOREIFTNCHK-ICHIBUGAKU
                                         WRK-FTN-KOH-XXX
           END-IF
      *
           COMPUTE  WRK-FTNKEI2  = SYU-TOTAL-HKNTEN
                                 * 10
                                 * WRK-KYURATE(IDX1)
                                 / 100
           ADD      WRK-FTNKEI2      TO  LNK-KOREIFTNCHK-ICHIBUGAKU2
      *
           .
       260-ICHIBUFTN-KOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額の減額
      *****************************************************************
       270-ICHIBUFTN-GEN-SEC               SECTION.
      *
           IF   LNK-KOREIFTNCHK-GENGAKUKBN   NOT =   ZERO
      *
               COMPUTE  WRK-FTNKEI = SYU-TOTAL-HKNTEN
                                   * 10
                                   * WRK-KYURATE(IDX1)
                                   / 100
      *
               EVALUATE   LNK-KOREIFTNCHK-GENGAKUKBN
                 WHEN        1
                   IF  LNK-KOREIFTNCHK-GENGAKUFLG  =  2
                     IF  WRK-KYURATE(IDX1)  <=
                                       LNK-KOREIFTNCHK-GENGAKU-RITU
                         MOVE   ZERO     TO   WRK-FTNKEI
                     ELSE
                         COMPUTE  WRK-FTNKEI-XXX               =
                                  SYU-TOTAL-HKNTEN             *
                                  10                           *
                                  LNK-KOREIFTNCHK-GENGAKU-RITU /
                                  100
                         IF  WRK-FTNKEI > WRK-FTNKEI-XXX
                            COMPUTE  WRK-FTNKEI         =
                                     WRK-FTNKEI         -
                                     WRK-FTNKEI-XXX
                         ELSE
                           MOVE ZERO     TO   WRK-FTNKEI
                         END-IF
                     END-IF
                   ELSE
                     IF  LNK-KOREIFTNCHK-GENGAKU-RITU = 100
                         MOVE   ZERO     TO   WRK-FTNKEI
                     ELSE
                         COMPUTE  WRK-GENGAKU-RITUX =
                                  100 - LNK-KOREIFTNCHK-GENGAKU-RITU
                         IF    WRK-FTNKEI  NOT = ZERO
                            COMPUTE  WRK-FTNKEI         =
                                     WRK-FTNKEI         *
                                     WRK-GENGAKU-RITUX  /
                                     100
                         END-IF
                     END-IF
                   END-IF
                 WHEN        2
                   MOVE     WRK-FTNKEI   TO   WRK-FTNKEI-GENGAKU
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU +  5
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU /  10
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU *  10
      *
                   IF  WRK-FTNKEI < WRK-GENGAKU-KIN
                     MOVE   ZERO   TO   WRK-FTNKEI
                   ELSE
                     COMPUTE  WRK-FTNKEI     =
                              WRK-FTNKEI     -
                              WRK-GENGAKU-KIN
                   END-IF
      *
                   IF  WRK-GENGAKU-KIN
                                   <=  WRK-FTNKEI-GENGAKU
                     MOVE   ZERO   TO   WRK-GENGAKU-KIN
                   ELSE
                     COMPUTE  WRK-GENGAKU-KIN   =
                              WRK-GENGAKU-KIN   -
                              WRK-FTNKEI-GENGAKU
                   END-IF
                 WHEN        3
                 WHEN        4
                       MOVE   ZERO     TO   WRK-FTNKEI
                 WHEN        5
                   MOVE     WRK-FTNKEI   TO   WRK-FTNKEI-GENGAKU
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU +  5
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU /  10
                   COMPUTE  WRK-FTNKEI-GENGAKU =
                            WRK-FTNKEI-GENGAKU *  10
      *
                   IF  WRK-FTNKEI > WRK-GENGAKU-KIN
                     COMPUTE  WRK-FTNKEI     =
                              WRK-GENGAKU-KIN
                   END-IF
      *
                   IF  WRK-GENGAKU-KIN
                                   <=  WRK-FTNKEI-GENGAKU
                     MOVE   ZERO   TO   WRK-GENGAKU-KIN
                   ELSE
                     COMPUTE  WRK-GENGAKU-KIN   =
                              WRK-GENGAKU-KIN   -
                              WRK-FTNKEI-GENGAKU
                   END-IF
               END-EVALUATE
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI +  5
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI / 10
               COMPUTE  WRK-FTNKEI = WRK-FTNKEI * 10
               IF   LNK-KOREIFTNCHK-NYUGAIKBN  =  "1"
                 CONTINUE
               ELSE
                 COMPUTE  WRK-FTNKEI = WRK-FTNKEI
                                     + SYU-GRP-SGKMONEY
               END-IF
      *
           END-IF
      *
           IF   LNK-KOREIFTNCHK-GENGAKUKBN   NOT =   ZERO
           AND  LNK-KOREIFTNCHK-GENGAKUFLG       =   ZERO
      *
               EVALUATE   LNK-KOREIFTNCHK-GENGAKUKBN
                 WHEN        1
                   IF  LNK-KOREIFTNCHK-GENGAKU-RITU = 100
                       MOVE   ZERO     TO   WRK-KINGAKU
                   ELSE
                       COMPUTE  WRK-GENGAKU-RITUX =
                                100 - LNK-KOREIFTNCHK-GENGAKU-RITU
                       IF    WRK-KINGAKU  NOT = ZERO
                          COMPUTE  WRK-KINGAKU        =
                                   WRK-KINGAKU        *
                                   WRK-GENGAKU-RITUX  /
                                   100
                       END-IF
                   END-IF
                 WHEN        2
                   IF  WRK-KINGAKU < LNK-KOREIFTNCHK-GENGAKU-KIN
                     MOVE   ZERO   TO   WRK-KINGAKU
                   ELSE
                     COMPUTE  WRK-KINGAKU    =
                              WRK-KINGAKU    -
                              LNK-KOREIFTNCHK-GENGAKU-KIN
                   END-IF
                 WHEN        3
                 WHEN        4
                       MOVE   ZERO     TO   WRK-KINGAKU
                 WHEN        5
                   IF  WRK-KINGAKU > LNK-KOREIFTNCHK-GENGAKU-KIN
                     COMPUTE  WRK-KINGAKU    =
                              LNK-KOREIFTNCHK-GENGAKU-KIN
                   END-IF
               END-EVALUATE
               COMPUTE  WRK-KINGAKU = WRK-KINGAKU +  5
               COMPUTE  WRK-KINGAKU = WRK-KINGAKU / 10
               COMPUTE  WRK-KINGAKU = WRK-KINGAKU * 10
      *
           END-IF
      *
           .
       270-ICHIBUFTN-GEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"           TO  MCP-FUNC
           MOVE    "tbl_ptinf"          TO  MCP-TABLE
           MOVE    "key"                TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"          USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
           IF      MCP-RC               =   ZERO
               MOVE    "DBFETCH"        TO  MCP-FUNC
               MOVE    "tbl_ptinf"      TO  MCP-TABLE
               MOVE    "key"            TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"      USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF      MCP-RC           =   ZERO
                   MOVE    MCPDATA-REC  TO  PTINF-REC
                   MOVE    ZERO         TO  FLG-PTINF
               ELSE
                   INITIALIZE               PTINF-REC
                   MOVE    1            TO  FLG-PTINF
               END-IF
           ELSE
               INITIALIZE                   PTINF-REC
               MOVE    1                TO  FLG-PTINF
           END-IF
      *
           MOVE     "tbl_ptinf"         TO  MCP-TABLE
           MOVE     "key"               TO  MCP-PATHNAME
           PERFORM   920-DB-CLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター検索
      *****************************************************************
       900-HKNCOMBI-SELECT-SEC        SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-HKNCOMBI
           ELSE
               MOVE    1                  TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
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
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター検索
      *****************************************************************
       900-SYUNOU-SELECT-SEC        SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    ZERO               TO  FLG-SYUNOU
           ELSE
               MOVE    1                  TO  FLG-SYUNOU
           END-IF
      *
           .
       900-SYUNOU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       910-SYUNOU-READ-SEC         SECTION.
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
       910-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報読込
      *****************************************************************
       900-PTHKNINF-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       920-DB-CLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DB-CLOSE-EXT.
           EXIT.
