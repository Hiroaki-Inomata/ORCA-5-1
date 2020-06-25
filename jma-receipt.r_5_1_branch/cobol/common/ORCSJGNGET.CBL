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
       PROGRAM-ID.         ORCSJGNGET.
      ****************************************************************
      *    OS           LINUX                                         
      *                                                               
      *    PROGRAM      ORCSJGNGET  /  公費月上限額取得サブ           
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/10/28    NACL−門脇    新規作成
      ****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    06/03/20  自立支援法対応
      *  01.00.02    NACL-門脇    06/10/30  障害児施設医療・療養介護医療
      *                                     対応
      *  03.04.01    NACL-門脇    07/02/14  福島県飯館村対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/21  肝炎治療対応
      *
      *  04.05.01    NACL-門脇    09/07/28  所得者情報の公費入力機能向上
      *  04.05.02    NACL-門脇    12/05/16  愛知・三重県地方公費対応
      *
      *  04.06.01    NACL-門脇    13/05/01  大阪府地方公費対応
      *
      *  04.07.01    NACL-門脇    11/09/01  システム管理２０１０機能追加対応
      *  04.07.02    NACL-門脇    14/11/04  平成２７年１月改正対応
      *                                     （０５４、０５２対応）
      *  04.07.03    NACL-門脇    15/01/26  平成２７年１月改正対応
      *                                     （都道府県独自難病対応）
      *
      *  05.00.01    NACL-門脇    18/11/07  肝がん・重度肝硬変治療研究促進
      *                                     事業医療対応
      *  05.00.02    NACL-門脇    20/03/26  小児慢性の実施機関番号追加対応
      *                                     （７１０・８１０）
      *****************************************************************
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
      *    添え字領域
       01  IDX-AREA.
           03  IDX1                        PIC 9(03).
           03  IDX2                        PIC 9(03).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI                PIC 9(01).
           03  FLG-PTKOHFTN                PIC 9(01).
           03  FLG-PTKOHFTNETC             PIC 9(01).
           03  FLG-HKNCOMBI                PIC 9(01).
           03  FLG-NYUINACCT               PIC 9(01).
           03  FLG-SRYACCT                 PIC 9(01).
           03  FLG-ARI                     PIC 9(01).
           03  FLG-TAISYO                  PIC 9(01).
      *
      *    ワーク領域
       01  WRK-AREA.
      *
           03  WRK-ED-S.
              05  WRK-ED                   PIC 9(02).
      *
           03  WRK-SKYEDD-S.
              05  WRK-SKYEDD               PIC 9(02).
      *
           03  WRK-GAIRAI-DAY              PIC 9(02).
      *
           03  WRK-RUIKEI-DAY              PIC 9(02).
      *
           03  WRK-KEISAN                  PIC S9(02).
           03  WRK-KEISAN2                 PIC 9(07)V9(05).
      *
           03  WRK-RUIKEI-GAI              PIC 9(07)V9(05).
           03  WRK-RUIKEI-NYU              PIC 9(07)V9(05).
           03  WRK-RUIKEI-GAI2             PIC 9(07).
           03  WRK-RUIKEI-NYU2             PIC 9(07).
      *
           03  WRK-HIWARIKBN               PIC X(01).
      *
           03  WRK-HKNCOMBINUM-TABLE.
              05 WRK-HKNCOMBINUM           PIC 9(04)   OCCURS   50.
      *
           03  WRK-NYUIN-TABLE.
              05 WRK-NYUIN-NISUU           PIC 9(02).
              05 WRK-NYUIN-DAY             PIC 9(01)   OCCURS   31.
      *
           03  WRK-KOHFTN-TABLE.
              05 WRK-KOHFTN-OCC            OCCURS     31.
                07 WRK-KOHFTN-NISUU        PIC 9(02).
                07 WRK-KOHFTN-TEKSTYMD     PIC X(08).
                07 WRK-KOHFTN-TEKEDYMD     PIC X(08).
                07 WRK-KOHFTN-GAIJGNGAK    PIC 9(07).
                07 WRK-KOHFTN-NYUJGNGAK    PIC 9(07).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY                  PIC 9(02).
               05  SYS-MM                  PIC 9(02).
               05  SYS-DD                  PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY               PIC 9(04).
               05  WRK-SYSMM               PIC 9(02).
               05  WRK-SYSDD               PIC 9(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    患者公費負担他院データ
       01  PTKOHFTNETC-REC.
           COPY    "CPPTKOHFTNETC.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    入院会計
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    システム管理
           COPY    "CPSK1001.INC".
           COPY    "CPSK2010.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      ***  COPY    "CPORCMCP.INC".
      *
      /
      ***
       LINKAGE             SECTION.
           COPY  "CPORCSJGNGET.INC".
           COPY  "COMMON-SPA".
      /
       PROCEDURE           DIVISION  USING
                                     LNK-ORCSJGNGET-REC
                                     SPA-AREA
                                     .
      ******************************************************************
      *        依頼制御処理
      ******************************************************************
       MAIN-RTN            SECTION.
       MAIN-000.
      *
           INITIALIZE             LNK-ORCSJGNGET-OUT-AREA
           MOVE     ZERO    TO    LNK-ORCSJGNGET-RC
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
      *    当該地方公費の対象チェック
           MOVE       ZERO          TO   FLG-TAISYO
           EVALUATE   LNK-ORCSJGNGET-HENFLG
             WHEN     SPACE
                IF    LNK-ORCSJGNGET-KOHHKNNUM(1:1) >=  "1" AND <=  "8"
                   IF    LNK-ORCSJGNGET-SETKBN  NOT = ZERO
                     MOVE   LNK-ORCSJGNGET-SETKBN
                                      TO   LNK-ORCSJGNGET-TAISYOKBN
                     MOVE   LNK-ORCSJGNGET-SETKBN2
                                      TO   LNK-ORCSJGNGET-TEKIYOFLG
                   ELSE
                     MOVE       1     TO   LNK-ORCSJGNGET-TAISYOKBN
                   END-IF
                ELSE
                   MOVE         9     TO   LNK-ORCSJGNGET-TAISYOKBN
                END-IF
                MOVE            1     TO   FLG-TAISYO
             WHEN     "1"
                IF    LNK-ORCSJGNGET-KOHHKNNUM(1:1) >=  "1" AND <=  "8"
                   PERFORM  300-TAISYO-CHECK-SEC
                   MOVE       ZERO    TO   FLG-TAISYO
                ELSE
                   MOVE        21     TO   LNK-ORCSJGNGET-RC
                END-IF
             WHEN     "2"
                IF    LNK-ORCSJGNGET-KOHHKNNUM(1:1) >=  "1" AND <=  "8"
                   PERFORM  300-TAISYO-CHECK-SEC
                   IF    LNK-ORCSJGNGET-TAISYOKBN  =  1  OR  2
                        MOVE    1     TO   FLG-TAISYO
                   END-IF
                ELSE
                   MOVE        21     TO   LNK-ORCSJGNGET-RC
                END-IF
           END-EVALUATE
      *
           IF   (LNK-ORCSJGNGET-RC  =  ZERO)
           AND  (FLG-TAISYO         =   1  )
      *
            IF     LNK-ORCSJGNGET-SRYYM    >=   "200604"
             PERFORM    100-KEISAN1-SEC
            ELSE
      *      更正・育成計算区分の取得
             INITIALIZE                         SYS-1001-REC
             MOVE    "1001"               TO    SYS-1001-KANRICD
             MOVE    "*"                  TO    SYS-1001-KBNCD
             MOVE    LNK-ORCSJGNGET-HOSPNUM
                                          TO    SYS-1001-HOSPNUM
             MOVE    LNK-ORCSJGNGET-SRYYM TO    SYS-1001-STYUKYMD(1:6)
             MOVE    "01"                 TO    SYS-1001-STYUKYMD(7:2)
             MOVE    LNK-ORCSJGNGET-SRYYM TO    SYS-1001-EDYUKYMD(1:6)
             MOVE    "01"                 TO    SYS-1001-EDYUKYMD(7:2)
             MOVE    SYS-1001-REC         TO    MCPDATA-REC
             MOVE    "DBSELECT"           TO    MCP-FUNC
             MOVE    "tbl_syskanri"       TO    MCP-TABLE
             MOVE    "key10"              TO    MCP-PATHNAME
             CALL    "ORCDBMAIN"          USING   MCPAREA
                                                  MCPDATA-REC
                                                  SPA-AREA
             IF      MCP-RC               =     ZERO
                 MOVE    "tbl_syskanri"       TO    MCP-TABLE
                 MOVE    "key10"              TO    MCP-PATHNAME
                 PERFORM   900-SYSKANRI-READ-SEC
                 IF    FLG-SYSKANRI   =   ZERO
                    MOVE  SYS-1001-HIWARIKBN  TO    WRK-HIWARIKBN
                    IF    WRK-HIWARIKBN  =   SPACE
                        MOVE    "1"           TO    WRK-HIWARIKBN
                    END-IF
                 ELSE
                    MOVE     11           TO    LNK-ORCSJGNGET-RC
                 END-IF
             ELSE
                    MOVE     11           TO    LNK-ORCSJGNGET-RC
             END-IF
      *
             MOVE     "tbl_syskanri"      TO    MCP-TABLE
             MOVE     "key10"             TO    MCP-PATHNAME
             PERFORM   910-DB-CLOSE-SEC
      *
             IF    LNK-ORCSJGNGET-RC   =   ZERO
                 EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                    WHEN     "015"
                    WHEN     "016"
                       EVALUATE   WRK-HIWARIKBN
                          WHEN     "1"
                            PERFORM    100-KEISAN1-SEC
                          WHEN     "2"
                            PERFORM    200-KEISAN2-SEC
                       END-EVALUATE
                    WHEN     OTHER
                            PERFORM    100-KEISAN1-SEC
                 END-EVALUATE
             END-IF
            END-IF
      *
           END-IF
      *
           .
      *
       MAIN-EXT.
           EXIT PROGRAM
           STOP RUN.
      *
      *****************************************************************
      *    日割り計算しない場合の公費月上限額取得
      *****************************************************************
       100-KEISAN1-SEC         SECTION.
      *
           INITIALIZE                              PTKOHFTN-REC
           MOVE    LNK-ORCSJGNGET-HOSPNUM  TO      PTKOHFTN-HOSPNUM
           MOVE    LNK-ORCSJGNGET-PTID     TO      PTKOHFTN-PTID
           MOVE    LNK-ORCSJGNGET-KOHID    TO      PTKOHFTN-KOHID
           MOVE    PTKOHFTN-REC            TO      MCPDATA-REC
           MOVE    "DBSELECT"              TO      MCP-FUNC
           MOVE    "tbl_ptkohftn"          TO      MCP-TABLE
           MOVE    "key5"                  TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_ptkohftn"        TO      MCP-TABLE
             MOVE    "key5"                TO      MCP-PATHNAME
             PERFORM  900-PTKOHFTN-READ-SEC
             PERFORM  UNTIL     FLG-PTKOHFTN      =     1
               IF   (PTKOHFTN-TEKSTYMD(1:6) <=  LNK-ORCSJGNGET-SRYYM  )
               AND  (LNK-ORCSJGNGET-SRYYM   <=  PTKOHFTN-TEKEDYMD(1:6))
      *
                 ADD   PTKOHFTN-GAIJGNGAK  TO  LNK-ORCSJGNGET-GAIJGNGAK
                 ADD   PTKOHFTN-NYUJGNGAK  TO  LNK-ORCSJGNGET-NYUJGNGAK
      *
               END-IF
               MOVE    "tbl_ptkohftn"      TO      MCP-TABLE
               MOVE    "key5"              TO      MCP-PATHNAME
               PERFORM   900-PTKOHFTN-READ-SEC
             END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptkohftn"          TO      MCP-TABLE
           MOVE    "key5"                  TO      MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           IF     LNK-ORCSJGNGET-SRYYM   >=   "200604"
               EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN     "015"
                  WHEN     "016"
                  WHEN     "021"
                     MOVE   LNK-ORCSJGNGET-NYUJGNGAK
                                TO  LNK-ORCSJGNGET-TAFTNRUIKEI
                     MOVE   LNK-ORCSJGNGET-GAIJGNGAK
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
      *
                     PERFORM 1001-TAFTNRUIKEI-HENSYU-SEC
               END-EVALUATE
           END-IF
           IF     LNK-ORCSJGNGET-SRYYM   >=   "200610"
               EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN     "024"
                  WHEN     "079"
                     MOVE   LNK-ORCSJGNGET-NYUJGNGAK
                                TO  LNK-ORCSJGNGET-TAFTNRUIKEI
                     MOVE   LNK-ORCSJGNGET-GAIJGNGAK
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
               END-EVALUATE
           END-IF
      *
           IF     LNK-ORCSJGNGET-SRYYM   >=   "200804"
               EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN     "038"
                     MOVE   LNK-ORCSJGNGET-NYUJGNGAK
                                TO  LNK-ORCSJGNGET-TAFTNRUIKEI
                     MOVE   LNK-ORCSJGNGET-GAIJGNGAK
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
      *
                     PERFORM 1001-TAFTNRUIKEI-HENSYU-SEC
               END-EVALUATE
           END-IF
      *
           IF     LNK-ORCSJGNGET-SRYYM   >=   "201501"
               EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN     "052"
                  WHEN     "054"
                     MOVE   LNK-ORCSJGNGET-NYUJGNGAK
                                TO  LNK-ORCSJGNGET-TAFTNRUIKEI
                     MOVE   LNK-ORCSJGNGET-GAIJGNGAK
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
      *
                     PERFORM 1001-TAFTNRUIKEI-HENSYU-SEC
               END-EVALUATE
           END-IF
      *
           IF     LNK-ORCSJGNGET-SRYYM   >=   "201812"
               EVALUATE   LNK-ORCSJGNGET-KOHHKNNUM
                  WHEN     "092"
                     MOVE   ZERO
                                TO  LNK-ORCSJGNGET-GAIJGNGAK
                     IF     LNK-ORCSJGNGET-NYUJGNGAK  =  ZERO
                                                     OR  999999
                                                     OR  9999999
                        MOVE   9999999
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
                     END-IF
               END-EVALUATE
           END-IF
      *
      *    地方公費の入外上限額・他一部負担累計入力分
           IF    (LNK-ORCSJGNGET-KOHHKNNUM(1:1) >=  "1" AND <=  "8")
           AND   (LNK-ORCSJGNGET-TAISYOKBN       =   2             )
                     MOVE   LNK-ORCSJGNGET-NYUJGNGAK
                                TO  LNK-ORCSJGNGET-TAFTNRUIKEI
                     MOVE   LNK-ORCSJGNGET-GAIJGNGAK
                                TO  LNK-ORCSJGNGET-NYUJGNGAK
      *
                     PERFORM 1001-TAFTNRUIKEI-HENSYU-SEC
           END-IF
      *
           .
      *
       100-KEISAN1-EXT.
           EXIT.
      *
      *****************************************************************
      *    他一部負担金額集計
      *****************************************************************
       1001-TAFTNRUIKEI-HENSYU-SEC         SECTION.
      *
           INITIALIZE                        PTKOHFTNETC-REC
           MOVE  LNK-ORCSJGNGET-HOSPNUM  TO  PTKOHFTNETC-HOSPNUM
           MOVE  LNK-ORCSJGNGET-PTID     TO  PTKOHFTNETC-PTID
           MOVE  LNK-ORCSJGNGET-KOHID    TO  PTKOHFTNETC-KOHID
           MOVE  LNK-ORCSJGNGET-SRYYM    TO  PTKOHFTNETC-SRYYMD(1:6)
           MOVE  "__"                    TO  PTKOHFTNETC-SRYYMD(7:2)
           MOVE  PTKOHFTNETC-REC         TO  MCPDATA-REC
           MOVE  "DBSELECT"              TO  MCP-FUNC
           MOVE  "tbl_ptkohftnetc"       TO  MCP-TABLE
           MOVE  "key2"                  TO  MCP-PATHNAME
           CALL  "ORCDBMAIN"             USING   MCPAREA
                                                 MCPDATA-REC
                                                 SPA-AREA
           IF     MCP-RC   =    ZERO
             MOVE    "tbl_ptkohftnetc"   TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM  900-PTKOHFTNETC-READ-SEC
             IF       FLG-PTKOHFTNETC = ZERO
                 MOVE     1    TO   LNK-ORCSJGNGET-TAFTNFLG
             END-IF
           END-IF
           MOVE      "tbl_ptkohftnetc"   TO  MCP-TABLE
           MOVE      "key2"              TO  MCP-PATHNAME
           PERFORM    910-DB-CLOSE-SEC
      *
           .
      *
       1001-TAFTNRUIKEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日割り計算する場合の公費月上限額取得（更正・育成）
      *****************************************************************
       200-KEISAN2-SEC         SECTION.
      *
      *    対象の保険組合せ番号取得
           PERFORM      2001-HKNCOMBINUM-GET-SEC
      *
           IF    LNK-ORCSJGNGET-RC   =   ZERO
      *      入院日数取得
             PERFORM    2002-NYUIN-NISUU-GET-SEC
           END-IF
      *
           IF    LNK-ORCSJGNGET-RC   =   ZERO
      *      月末日の取得
             PERFORM    2003-GETUMATU-GET-SEC
           END-IF
      *
           IF    LNK-ORCSJGNGET-RC   =   ZERO
      *      患者公費負担情報期間毎の日数算出
             PERFORM    2004-KOHFTN-GET-SEC
           END-IF
      *
           IF    LNK-ORCSJGNGET-RC   =   ZERO
      *      月上限額算出
             PERFORM    2005-GENDOGAKU-GET-SEC
           END-IF
      *
           .
      *
       200-KEISAN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の保険組合せ番号取得
      *****************************************************************
       2001-HKNCOMBINUM-GET-SEC.
      *
           INITIALIZE                          HKNCOMBI-REC
           MOVE    LNK-ORCSJGNGET-HOSPNUM  TO  COMB-HOSPNUM
           MOVE    LNK-ORCSJGNGET-PTID     TO  COMB-PTID
           MOVE    LNK-ORCSJGNGET-KOHID    TO  COMB-KOHID(1)
           MOVE    LNK-ORCSJGNGET-KOHID    TO  COMB-KOHID(2)
           MOVE    LNK-ORCSJGNGET-KOHID    TO  COMB-KOHID(3)
           MOVE    LNK-ORCSJGNGET-KOHID    TO  COMB-KOHID(4)
           MOVE    HKNCOMBI-REC            TO  MCPDATA-REC
           MOVE    "DBSELECT"              TO  MCP-FUNC
           MOVE    "tbl_hkncombi"          TO  MCP-TABLE
           MOVE    "key4"                  TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC               =       ZERO
              MOVE    "tbl_hkncombi"       TO  MCP-TABLE
              MOVE    "key4"               TO  MCP-PATHNAME
              PERFORM  900-HKNCOMBI-READ-SEC
              PERFORM   VARYING   IDX1   FROM  1  BY  1
                UNTIL   (IDX1              >    50 )   OR
                        (FLG-HKNCOMBI      =     1 )
                  MOVE     COMB-HKNCOMBINUM  TO
                                       WRK-HKNCOMBINUM(IDX1)
                  MOVE    "tbl_hkncombi"   TO  MCP-TABLE
                  MOVE    "key4"           TO  MCP-PATHNAME
                  PERFORM  900-HKNCOMBI-READ-SEC
              END-PERFORM
           END-IF
      *
           MOVE    "tbl_hkncombi"   TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           IF   WRK-HKNCOMBINUM(1)  =  ZERO
                  MOVE     12            TO  LNK-ORCSJGNGET-RC
           END-IF
      *
           .
      *
       2001-HKNCOMBINUM-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    入院日数取得
      ******************************************************************
       2002-NYUIN-NISUU-GET-SEC         SECTION.
      *
      *    診療会計処理
           INITIALIZE                            SRYACCT-REC
           MOVE    LNK-ORCSJGNGET-HOSPNUM   TO   ACCT-HOSPNUM
           MOVE    "1"                      TO   ACCT-NYUGAIKBN
           MOVE    LNK-ORCSJGNGET-PTID      TO   ACCT-PTID
           MOVE    LNK-ORCSJGNGET-SRYYM     TO   ACCT-SRYYM
           MOVE    SRYACCT-REC              TO   MCPDATA-REC
           MOVE    "DBSELECT"               TO   MCP-FUNC
           MOVE    "tbl_sryacct"            TO   MCP-TABLE
           MOVE    "key33"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_sryacct"        TO   MCP-TABLE
               MOVE    "key33"              TO   MCP-PATHNAME
               PERFORM  900-SRYACCT-READ-SEC
               PERFORM  UNTIL   FLG-SRYACCT  =    1
      *
                  PERFORM    20021-SRYACCT-NISUU-GET-SEC
      *
                  MOVE    "tbl_sryacct"     TO   MCP-TABLE
                  MOVE    "key33"           TO   MCP-PATHNAME
                  PERFORM    900-SRYACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_sryacct"     TO   MCP-TABLE
           MOVE    "key33"           TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    入院会計処理
           INITIALIZE                            NYUINACCT-REC
           MOVE    LNK-ORCSJGNGET-HOSPNUM   TO   NACCT-HOSPNUM
           MOVE    LNK-ORCSJGNGET-PTID      TO   NACCT-PTID
           MOVE    LNK-ORCSJGNGET-SRYYM     TO   NACCT-SRYYM
           MOVE    "5"                      TO   NACCT-ZAISKBKBN
           MOVE    NYUINACCT-REC            TO   MCPDATA-REC
           MOVE    "DBSELECT"               TO   MCP-FUNC
           MOVE    "tbl_nyuinacct"          TO   MCP-TABLE
           MOVE    "key17"                  TO   MCP-PATHNAME
           CALL    "ORCDBMAIN"              USING   MCPAREA
                                                    MCPDATA-REC
                                                    SPA-AREA
           IF      MCP-RC             =       ZERO
               MOVE    "tbl_nyuinacct"      TO   MCP-TABLE
               MOVE    "key17"              TO   MCP-PATHNAME
               PERFORM  900-NYUINACCT-READ-SEC
               PERFORM  UNTIL  FLG-NYUINACCT  =    1
      *
                 PERFORM   20022-NYUINACCT-NISUU-GET-SEC
      *
                 MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
                 MOVE    "key17"            TO   MCP-PATHNAME
                 PERFORM   900-NYUINACCT-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_nyuinacct"    TO   MCP-TABLE
           MOVE    "key17"            TO   MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
           .
      *
       2002-NYUIN-NISUU-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    診療会計より
      ******************************************************************
       20021-SRYACCT-NISUU-GET-SEC         SECTION.
      *
           IF   LNK-ORCSJGNGET-NYUGAIKBN   =  "1"
                MOVE     LNK-ORCSJGNGET-SKYEDYMD(7:2)  TO  WRK-SKYEDD-S
                MOVE     ZERO       TO      FLG-ARI
                PERFORM   VARYING   IDX1   FROM  1  BY  1
                      UNTIL  (IDX1                  >  50 ) OR
                             (WRK-HKNCOMBINUM(IDX1) = ZERO) OR
                             (FLG-ARI               =   1 )
                    IF   ACCT-HKNCOMBI  =  WRK-HKNCOMBINUM(IDX1)
                       MOVE    1    TO      FLG-ARI
                       PERFORM    VARYING   IDX2   FROM   1   BY   1
                                   UNTIL    IDX2    >    WRK-SKYEDD
                         IF     ACCT-DAY(1 IDX2)   NOT =  ZERO
                            IF  WRK-NYUIN-DAY(IDX2) =  ZERO
                               MOVE    1    TO    WRK-NYUIN-DAY(IDX2)
                               ADD     1    TO    WRK-NYUIN-NISUU
                            END-IF
                         END-IF
                       END-PERFORM
                    END-IF
                END-PERFORM
           ELSE
                MOVE     ZERO       TO      FLG-ARI
                PERFORM   VARYING   IDX1   FROM  1  BY  1
                      UNTIL  (IDX1                  >  50 ) OR
                             (WRK-HKNCOMBINUM(IDX1) = ZERO) OR
                             (FLG-ARI               =   1 )
                    IF   ACCT-HKNCOMBI  =  WRK-HKNCOMBINUM(IDX1)
                       MOVE    1    TO      FLG-ARI
                       PERFORM    VARYING   IDX2   FROM   1   BY   1
                                   UNTIL    IDX2    >    31
                         IF     ACCT-DAY(1 IDX2)   NOT =  ZERO
                            IF  WRK-NYUIN-DAY(IDX2) =  ZERO
                               MOVE    1    TO    WRK-NYUIN-DAY(IDX2)
                               ADD     1    TO    WRK-NYUIN-NISUU
                            END-IF
                         END-IF
                       END-PERFORM
                    END-IF
                END-PERFORM
           END-IF
      *
           .
      *
       20021-SRYACCT-NISUU-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    入院会計より
      ******************************************************************
       20022-NYUINACCT-NISUU-GET-SEC         SECTION.
      *
           IF   LNK-ORCSJGNGET-NYUGAIKBN   =  "1"
                MOVE     LNK-ORCSJGNGET-SKYEDYMD(7:2)  TO  WRK-SKYEDD-S
                PERFORM    VARYING   IDX1   FROM   1   BY   1
                            UNTIL    IDX1    >    WRK-SKYEDD
                  IF   NACCT-DAY(IDX1)   NOT =  ZERO
                    MOVE     ZERO       TO      FLG-ARI
                    PERFORM   VARYING   IDX2   FROM  1  BY  1
                        UNTIL  (IDX2                  >  50 ) OR
                               (WRK-HKNCOMBINUM(IDX2) = ZERO) OR
                               (FLG-ARI               =   1 )
                       IF   NACCT-DAY(IDX1)  =  WRK-HKNCOMBINUM(IDX2)
                          MOVE    1     TO      FLG-ARI
                          IF  WRK-NYUIN-DAY(IDX1) =  ZERO
                                 MOVE    1    TO    WRK-NYUIN-DAY(IDX1)
                                 ADD     1    TO    WRK-NYUIN-NISUU
                          END-IF
                       END-IF
                    END-PERFORM
                  END-IF
                END-PERFORM
           ELSE
                PERFORM    VARYING   IDX1   FROM   1   BY   1
                            UNTIL    IDX1    >    31
                  IF   NACCT-DAY(IDX1)   NOT =  ZERO
                    MOVE     ZERO       TO      FLG-ARI
                    PERFORM   VARYING   IDX2   FROM  1  BY  1
                        UNTIL  (IDX2                  >  50 ) OR
                               (WRK-HKNCOMBINUM(IDX2) = ZERO) OR
                               (FLG-ARI               =   1 )
                       IF   NACCT-DAY(IDX1)  =  WRK-HKNCOMBINUM(IDX2)
                          MOVE    1     TO      FLG-ARI
                          IF  WRK-NYUIN-DAY(IDX1) =  ZERO
                                 MOVE    1    TO    WRK-NYUIN-DAY(IDX1)
                                 ADD     1    TO    WRK-NYUIN-NISUU
                          END-IF
                       END-IF
                    END-PERFORM
                  END-IF
                END-PERFORM
           END-IF
      *
           .
      *
       20022-NYUINACCT-NISUU-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    月末日の取得
      ******************************************************************
       2003-GETUMATU-GET-SEC         SECTION.
      *
           INITIALIZE                               STS-AREA-DAY
           INITIALIZE                               LNK-DAY7-AREA
           MOVE          "71"                   TO  LNK-DAY7-IRAI
           MOVE          LNK-ORCSJGNGET-SRYYM   TO  LNK-DAY7-YM
           CALL          "ORCSDAY"          USING   STS-AREA-DAY
                                                    LNK-DAY7-AREA
           IF      STS-DAY-RC1    =   ZERO
              MOVE       LNK-DAY7-KOYOMI(7:2)   TO  WRK-ED-S
           ELSE
              MOVE          13            TO  LNK-ORCSJGNGET-RC
           END-IF
      *
           .
      *
       2003-GETUMATU-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    患者公費負担情報期間毎の日数算出
      ******************************************************************
       2004-KOHFTN-GET-SEC         SECTION.
      *
      *    患者公費負担情報退避
           MOVE         ZERO               TO      IDX1
      *
           INITIALIZE                              PTKOHFTN-REC
           MOVE    LNK-ORCSJGNGET-HOSPNUM  TO      PTKOHFTN-HOSPNUM
           MOVE    LNK-ORCSJGNGET-PTID     TO      PTKOHFTN-PTID
           MOVE    LNK-ORCSJGNGET-KOHID    TO      PTKOHFTN-KOHID
           MOVE    PTKOHFTN-REC            TO      MCPDATA-REC
           MOVE    "DBSELECT"              TO      MCP-FUNC
           MOVE    "tbl_ptkohftn"          TO      MCP-TABLE
           MOVE    "key5"                  TO      MCP-PATHNAME
           CALL    "ORCDBMAIN"             USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC               =       ZERO
             MOVE    "tbl_ptkohftn"        TO      MCP-TABLE
             MOVE    "key5"                TO      MCP-PATHNAME
             PERFORM  900-PTKOHFTN-READ-SEC
             PERFORM  UNTIL   (IDX1             >=    31)  OR
                              (FLG-PTKOHFTN      =     1)
               IF   (PTKOHFTN-TEKSTYMD(1:6) <=  LNK-ORCSJGNGET-SRYYM  )
               AND  (LNK-ORCSJGNGET-SRYYM   <=  PTKOHFTN-TEKEDYMD(1:6))
                 ADD    1         TO  IDX1
                 IF    PTKOHFTN-TEKSTYMD(1:6)  <  LNK-ORCSJGNGET-SRYYM
                   MOVE  LNK-ORCSJGNGET-SRYYM
                                  TO   WRK-KOHFTN-TEKSTYMD(IDX1)(1:6)
                   MOVE  "01"     TO   WRK-KOHFTN-TEKSTYMD(IDX1)(7:2)
                 ELSE
                   MOVE  PTKOHFTN-TEKSTYMD
                                  TO   WRK-KOHFTN-TEKSTYMD(IDX1)
                 END-IF
                 IF    LNK-ORCSJGNGET-SRYYM  <  PTKOHFTN-TEKEDYMD(1:6)
                   MOVE  LNK-ORCSJGNGET-SRYYM
                                  TO   WRK-KOHFTN-TEKEDYMD(IDX1)(1:6)
                   MOVE  WRK-ED-S TO   WRK-KOHFTN-TEKEDYMD(IDX1)(7:2)
                 ELSE
                   MOVE  PTKOHFTN-TEKEDYMD
                                  TO   WRK-KOHFTN-TEKEDYMD(IDX1)
                 END-IF
                 MOVE  PTKOHFTN-GAIJGNGAK TO  WRK-KOHFTN-GAIJGNGAK(IDX1)
                 MOVE  PTKOHFTN-NYUJGNGAK TO  WRK-KOHFTN-NYUJGNGAK(IDX1)
               END-IF
               MOVE    "tbl_ptkohftn"     TO      MCP-TABLE
               MOVE    "key5"             TO      MCP-PATHNAME
               PERFORM   900-PTKOHFTN-READ-SEC
             END-PERFORM
           END-IF
      *
           MOVE    "tbl_ptkohftn"     TO      MCP-TABLE
           MOVE    "key5"             TO      MCP-PATHNAME
           PERFORM  910-DB-CLOSE-SEC
      *
      *    期間毎の日数算出
           PERFORM   VARYING   IDX1   FROM  1  BY  1
                      UNTIL   (IDX1                      >   31 ) OR
                              (WRK-KOHFTN-TEKSTYMD(IDX1) = SPACE)
      *
             INITIALIZE                   STS-AREA-DAY
             INITIALIZE                   LNK-DAY5-AREA
             MOVE    "51"                 TO  LNK-DAY5-IRAI
             MOVE    WRK-KOHFTN-TEKSTYMD(IDX1)
                                          TO  LNK-DAY5-START
             MOVE    WRK-KOHFTN-TEKEDYMD(IDX1)
                                          TO  LNK-DAY5-END
             CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                  LNK-DAY5-AREA
             IF      STS-DAY-RC1    =   ZERO
                 MOVE    LNK-DAY5-NISSU2  TO  WRK-KOHFTN-NISUU(IDX1)
      *
                 COMPUTE WRK-RUIKEI-DAY   =   WRK-RUIKEI-DAY
                                          +   WRK-KOHFTN-NISUU(IDX1)
             ELSE
                 MOVE       14            TO  LNK-ORCSJGNGET-RC
             END-IF
      *
           END-PERFORM
      *
           .
      *
       2004-KOHFTN-GET-EXT.
           EXIT.
      *
      ******************************************************************
      *    月上限額算出
      ******************************************************************
       2005-GENDOGAKU-GET-SEC         SECTION.
      *
      *    該当月に入院がある場合
           IF    WRK-NYUIN-NISUU    NOT  =   ZERO
      *
      *       外来分の日数
              COMPUTE   WRK-KEISAN   =   WRK-RUIKEI-DAY
                                     -   WRK-NYUIN-NISUU
              IF        WRK-KEISAN   <   ZERO
                 MOVE      ZERO           TO  WRK-GAIRAI-DAY
              ELSE
                 MOVE      WRK-KEISAN     TO  WRK-GAIRAI-DAY
              END-IF
      *
              PERFORM   VARYING   IDX1   FROM  1  BY  1
                         UNTIL   (IDX1                      >   31 ) OR
                                 (WRK-KOHFTN-TEKSTYMD(IDX1) = SPACE)
      *
                COMPUTE  WRK-KEISAN2     =
                 (WRK-KOHFTN-GAIJGNGAK(IDX1)  *  WRK-KOHFTN-NISUU(IDX1)
                                              *  WRK-GAIRAI-DAY       )
                                              /
                 (WRK-ED                      *  WRK-RUIKEI-DAY       )
                COMPUTE  WRK-RUIKEI-GAI  =  WRK-RUIKEI-GAI
                                         +  WRK-KEISAN2
      *
                COMPUTE  WRK-KEISAN2     =
                 (WRK-KOHFTN-NYUJGNGAK(IDX1)  *  WRK-KOHFTN-NISUU(IDX1)
                                              *  WRK-NYUIN-NISUU      )
                                              /
                 (WRK-ED                      *  WRK-RUIKEI-DAY       )
                COMPUTE  WRK-RUIKEI-NYU  =  WRK-RUIKEI-NYU
                                         +  WRK-KEISAN2
      *
              END-PERFORM
      *
              COMPUTE     WRK-RUIKEI-GAI2   =   WRK-RUIKEI-GAI   /  10
              COMPUTE     WRK-RUIKEI-GAI2   =   WRK-RUIKEI-GAI2  *  10
      *
              COMPUTE     WRK-RUIKEI-NYU2   =   WRK-RUIKEI-NYU   /  10
              COMPUTE     WRK-RUIKEI-NYU2   =   WRK-RUIKEI-NYU2  *  10
      *
              MOVE   WRK-RUIKEI-GAI2     TO   LNK-ORCSJGNGET-GAIJGNGAK
              MOVE   WRK-RUIKEI-NYU2     TO   LNK-ORCSJGNGET-NYUJGNGAK
      *
           ELSE
      *
              PERFORM   VARYING   IDX1   FROM  1  BY  1
                         UNTIL   (IDX1                      >   31 ) OR
                                 (WRK-KOHFTN-TEKSTYMD(IDX1) = SPACE)
      *
                COMPUTE  WRK-KEISAN2     =
                 (WRK-KOHFTN-GAIJGNGAK(IDX1)  *  WRK-KOHFTN-NISUU(IDX1))
                                              /  WRK-ED
                COMPUTE  WRK-RUIKEI-GAI  =  WRK-RUIKEI-GAI
                                         +  WRK-KEISAN2
      *
                COMPUTE  WRK-KEISAN2     =
                 (WRK-KOHFTN-NYUJGNGAK(IDX1)  *  WRK-KOHFTN-NISUU(IDX1))
                                              /  WRK-ED
                COMPUTE  WRK-RUIKEI-NYU  =  WRK-RUIKEI-NYU
                                         +  WRK-KEISAN2
      *
              END-PERFORM
      *
              COMPUTE     WRK-RUIKEI-GAI2   =   WRK-RUIKEI-GAI   /  10
              COMPUTE     WRK-RUIKEI-GAI2   =   WRK-RUIKEI-GAI2  *  10
      *
              COMPUTE     WRK-RUIKEI-NYU2   =   WRK-RUIKEI-NYU   /  10
              COMPUTE     WRK-RUIKEI-NYU2   =   WRK-RUIKEI-NYU2  *  10
      *
              MOVE   WRK-RUIKEI-GAI2     TO   LNK-ORCSJGNGET-GAIJGNGAK
      ****    MOVE   WRK-RUIKEI-NYU2     TO   LNK-ORCSJGNGET-NYUJGNGAK
              MOVE   ZERO                TO   LNK-ORCSJGNGET-NYUJGNGAK
      *
           END-IF
      *
           .
      *
       2005-GENDOGAKU-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    当該地方公費の対象チェック２
      *****************************************************************
       300-TAISYO-CHECK-SEC         SECTION.
      *
           IF  LNK-ORCSJGNGET-TEKSTYMD  =  SPACE
                   MOVE    22     TO   LNK-ORCSJGNGET-RC
           END-IF
           IF  LNK-ORCSJGNGET-PREFNUM   =  SPACE
                   MOVE    23     TO   LNK-ORCSJGNGET-RC
           END-IF
      *
           IF  LNK-ORCSJGNGET-RC  =  ZERO
      *
             EVALUATE   LNK-ORCSJGNGET-PREFNUM
               WHEN       "01"
                 IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "183"   )
                 AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200810")
                   MOVE     1     TO   LNK-ORCSJGNGET-TAISYOKBN
                 END-IF
               WHEN       "13"
                 IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "151"   )
                 AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200310")
                   MOVE     1     TO   LNK-ORCSJGNGET-TAISYOKBN
                 END-IF
               WHEN       "23"
                 IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "191"   )
                 AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200910")
                   MOVE     1     TO   LNK-ORCSJGNGET-TAISYOKBN
                 END-IF
                 IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                 AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                   MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                 END-IF
               WHEN       "24"
                 IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                 AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                   MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                 END-IF
             END-EVALUATE
      *
             IF  ( LNK-ORCSJGNGET-TAISYOKBN      =  ZERO        )
             OR  ((LNK-ORCSJGNGET-PREFNUM        = "23" OR "24") AND
                  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"       ) AND
                  (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104"    ))
               INITIALIZE                   SYS-2010-REC
               MOVE   "2010"            TO  SYS-2010-KANRICD
               MOVE   LNK-ORCSJGNGET-KOHHKNNUM
                                        TO  SYS-2010-KBNCD
               MOVE   LNK-ORCSJGNGET-HOSPNUM
                                        TO  SYS-2010-HOSPNUM
               MOVE   LNK-ORCSJGNGET-TEKSTYMD(1:6)
                                        TO  SYS-2010-STYUKYMD(1:6)
               MOVE   "01"              TO  SYS-2010-STYUKYMD(7:2)
               MOVE   LNK-ORCSJGNGET-TEKSTYMD(1:6)
                                        TO  SYS-2010-EDYUKYMD(1:6)
               MOVE   "01"              TO  SYS-2010-EDYUKYMD(7:2)
               MOVE   SYS-2010-REC      TO  MCPDATA-REC
               MOVE   "DBSELECT"        TO  MCP-FUNC
               MOVE   "tbl_syskanri"    TO  MCP-TABLE
               MOVE   "key10"           TO  MCP-PATHNAME
               CALL   "ORCDBMAIN"       USING   MCPAREA
                                                MCPDATA-REC
                                                SPA-AREA
               IF     MCP-RC   =   ZERO
                  MOVE  "tbl_syskanri"  TO  MCP-TABLE
                  MOVE  "key10"         TO  MCP-PATHNAME
                  PERFORM   900-SYSKANRI-READ-SEC
               ELSE
                  INITIALIZE                SYS-2010-REC
               END-IF
               EVALUATE  SYS-2010-PTJGNKBN
                 WHEN      SPACE
                    MOVE   ZERO   TO   LNK-ORCSJGNGET-TAISYOKBN
                    EVALUATE   LNK-ORCSJGNGET-PREFNUM
                     WHEN       "07"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "281"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200702")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "08"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200904")
                         MOVE     1     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "23"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "24"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "27"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "192"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201211")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "28"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "186"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "200310")
                         MOVE     1     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                    END-EVALUATE
                 WHEN      "0"
                    MOVE   ZERO   TO   LNK-ORCSJGNGET-TAISYOKBN
                    EVALUATE   LNK-ORCSJGNGET-PREFNUM
                     WHEN       "23"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                     WHEN       "24"
                       IF  (LNK-ORCSJGNGET-KOHHKNNUM      = "152"   )
                       AND (LNK-ORCSJGNGET-TEKSTYMD(1:6) >= "201104")
                         MOVE     2     TO   LNK-ORCSJGNGET-TAISYOKBN
                       END-IF
                    END-EVALUATE
                 WHEN      "1"
                    IF  ((LNK-ORCSJGNGET-PREFNUM       = "01"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "183"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200810"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "13"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "151"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200310"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "23"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "191"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200910"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "23"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201104"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "24"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201104"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "07"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "281"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200702"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "08"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200904"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "27"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "192"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201211"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "28"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "186"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200310"))
                         MOVE   ZERO    TO   LNK-ORCSJGNGET-TAISYOKBN
                    ELSE
                         MOVE    1      TO   LNK-ORCSJGNGET-TAISYOKBN
                    END-IF
                 WHEN      "2"
                    IF  ((LNK-ORCSJGNGET-PREFNUM       = "01"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "183"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200810"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "13"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "151"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200310"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "23"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "191"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200910"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "23"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201104"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "24"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201104"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "07"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "281"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200702"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "08"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "152"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200904"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "27"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "192"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "201211"))
                    OR  ((LNK-ORCSJGNGET-PREFNUM       = "28"    ) AND
                         (LNK-ORCSJGNGET-KOHHKNNUM     = "186"   ) AND
                         (LNK-ORCSJGNGET-TEKSTYMD(1:6) < "200310"))
                         MOVE   ZERO    TO   LNK-ORCSJGNGET-TAISYOKBN
                    ELSE
                         MOVE    2      TO   LNK-ORCSJGNGET-TAISYOKBN
                    END-IF
                 WHEN      "3"
                         MOVE    2      TO   LNK-ORCSJGNGET-TAISYOKBN
                         MOVE    1      TO   LNK-ORCSJGNGET-TEKIYOFLG
               END-EVALUATE
               MOVE     "tbl_syskanri"  TO  MCP-TABLE
               MOVE     "key10"         TO  MCP-PATHNAME
               PERFORM   910-DB-CLOSE-SEC
             END-IF
      *
           END-IF
      *
           .
      *
       300-TAISYO-CHECK-EXT.
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
               MOVE    MCPDATA-REC     TO  SYS-1001-REC
                                           SYS-2010-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               INITIALIZE                  SYS-1001-REC
                                           SYS-2010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
      *
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担マスター読込
      *****************************************************************
       900-PTKOHFTN-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTN-REC
               MOVE    ZERO            TO  FLG-PTKOHFTN
           ELSE
               INITIALIZE                  PTKOHFTN-REC
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           .
      *
       900-PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費負担他院データマスター読込
      *****************************************************************
       900-PTKOHFTNETC-READ-SEC          SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTNETC-REC
               MOVE    ZERO            TO  FLG-PTKOHFTNETC
           ELSE
               INITIALIZE                  PTKOHFTNETC-REC
               MOVE    1               TO  FLG-PTKOHFTNETC
           END-IF
      *
           .
      *
       900-PTKOHFTNETC-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
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
      *    入院会計マスター読込
      *****************************************************************
       900-NYUINACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  NYUINACCT-REC
               MOVE    ZERO            TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
      *
           .
      *
       900-NYUINACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
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
