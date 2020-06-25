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
       PROGRAM-ID.                 ORCGK081.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 入院指示せん印刷画面（Ｋ０８１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/02/XX    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/11/17  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.03.00    NACL-多々納  08/06/XX  ドクター表示件数変更
      *  04.03.00    NACL-多々納  08/10/09  ユーザプログラム対応
      *  04.07.00    NACL-多々納  12/01/12  クライアント印刷対応
      *  04.08.00    NACL-多々納  14/08/11  クライアント印刷変更
      *  05.00.00    NACL-多々納  17/10/06  帳票API対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "K081SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SYOHOU-ARI      PIC 9(01).
           03  FLG-CHUSYA-ARI      PIC 9(01).
           03  FLG-SIJISEN-ARI     PIC 9(01).
           03  FLG-SYOHOU-CHK      PIC 9(01).
           03  FLG-CHUSYA-CHK      PIC 9(01).
           03  FLG-SIJISEN-CHK     PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *    クライアント印刷有
           03  FLG-CLIENT-OK           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
           03  ID1                 PIC 9(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-DRCD            PIC X(04).
           03  WRK-VALUE           PIC X(01).
           03  WRK-CHKKBN          PIC 9(01).
      *
           03  WRK-PATH            PIC X(64).
           03  WRK-MCP-WIDGET      PIC X(64).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
           03  WRK-CHUSYA-PG           PIC X(20).
           03  WRK-SIJISEN-PG          PIC X(20).
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
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    ドクター
           COPY  "CPSK1010.INC".
      *
      *    入院指示せん設定情報
           COPY  "CPSK5007.INC".
      *    帳票ＰＧ
           COPY    "CPSK1034.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *    患者入院履歴ワーク
       01  HZN-PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC"  REPLACING
                               //PTNYUINRRK-// BY //HZN-PTNRRK-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    ドクター編集処理パラメタ
           COPY    "CPORCSDRSET.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *    入院処方箋発行サブ
           COPY    "CPORCHC501.INC".
      *    注射処方箋発行サブ
           COPY    "CPORCHC502.INC".
      *    入院指示箋発行サブ
           COPY    "CPORCHC503.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-K081        TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   "NEW"                TO  MCP-PUTTYPE
               MOVE   "K081"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    SPACE               TO  SPA-KIDCD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM                     310-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           INITIALIZE                  SPA-K081GMN-AREA
      *
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-5007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-5007-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYS-5007-REC
           END-IF
      *
           IF      SPA-NAI-SIJISENKBN  NOT =   "1"
      *        指示せん発行なし
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYS-5007-REC
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               EVALUATE    SYS-5007-KBNCD
                   WHEN    "01"
      *                入院処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK1(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK1FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-SYOHOU-ARI
                           END-IF
                       END-PERFORM
                   WHEN    "03"
      *                注射処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK2(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK2FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-CHUSYA-ARI
                           END-IF
                       END-PERFORM
                   WHEN    "05"
      *                指示箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK3(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK3FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-SIJISEN-ARI
                           END-IF
                       END-PERFORM
               END-EVALUATE
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-5007-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院処方箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-SYOHOU-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-SYOHOU-LST(2)
           MOVE    2                   TO  SPA-K081-SYOHOU-MAX
           IF      FLG-SYOHOU-ARI      =   ZERO
               MOVE    1                   TO  SPA-K081-SYOHOU-MAX
           END-IF
           IF     (FLG-SYOHOU-ARI      =   1   )  AND
                  (SPA-NAI-SYOHOU      =   1   )
               MOVE    SPA-K081GMN-SYOHOU-LST(2)
                                       TO  SPA-K081GMN-SYOHOU-G
           ELSE
               MOVE    SPA-K081GMN-SYOHOU-LST(1)
                                       TO  SPA-K081GMN-SYOHOU-G
           END-IF
           PERFORM 3102-SYOHOU-CHK-SEC
      *    注射処方箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-CHUSYA-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-CHUSYA-LST(2)
           MOVE    2                   TO  SPA-K081-CHUSYA-MAX
           IF      FLG-CHUSYA-ARI      =   ZERO
               MOVE    1                   TO  SPA-K081-CHUSYA-MAX
           END-IF
           IF     (FLG-CHUSYA-ARI      =   1   )  AND
                  (SPA-NAI-CHUSYA      =   1   )
               MOVE    SPA-K081GMN-CHUSYA-LST(2)
                                       TO  SPA-K081GMN-CHUSYA-G
           ELSE
               MOVE    SPA-K081GMN-CHUSYA-LST(1)
                                       TO  SPA-K081GMN-CHUSYA-G
           END-IF
           PERFORM 3103-CHUSYA-CHK-SEC
      *    指示箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-SIJISEN-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-SIJISEN-LST(2)
           MOVE    2                   TO  SPA-K081-SIJISEN-MAX
           IF      FLG-SIJISEN-ARI     =   ZERO
               MOVE    1                   TO  SPA-K081-SIJISEN-MAX
           END-IF
           IF     (FLG-SIJISEN-ARI     =   1   )  AND
                  (SPA-NAI-SIJISEN     =   1   )
               MOVE    SPA-K081GMN-SIJISEN-LST(2)
                                       TO  SPA-K081GMN-SIJISEN-G
           ELSE
               MOVE    SPA-K081GMN-SIJISEN-LST(1)
                                       TO  SPA-K081GMN-SIJISEN-G
           END-IF
           PERFORM 3104-SIJISEN-CHK-SEC
      *
      *    ドクター編集
      *    担当医を編集
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           INITIALIZE                  HZN-PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key36"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD    <=  SPA-SRYYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                         ((PTNYUINRRK-TENSTUYMD    NOT =   SPACE ) AND
                          (PTNYUINRRK-TENSTUYMD    >=  SPA-SRYYMD))
                       MOVE    PTNYUINRRK-REC  TO  HZN-PTNYUINRRK-REC
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key36"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  SPA-K081-DRCD-MAX
           INITIALIZE                  SPA-K081GMN-DRCDLST-G
           IF      FLG-OK              =   1
               IF      HZN-PTNRRK-DRCD1    NOT =   SPACE
                   MOVE    HZN-PTNRRK-DRCD1(2:4)   TO  WRK-DRCD
                   PERFORM 3105-DRCD-ADD-SEC
               END-IF
               IF      HZN-PTNRRK-DRCD2    NOT =   SPACE
                   MOVE    HZN-PTNRRK-DRCD2(2:4)   TO  WRK-DRCD
                   PERFORM 3105-DRCD-ADD-SEC
               END-IF
               IF      HZN-PTNRRK-DRCD3    NOT =   SPACE
                   MOVE    HZN-PTNRRK-DRCD3(2:4)   TO  WRK-DRCD
                   PERFORM 3105-DRCD-ADD-SEC
               END-IF
           END-IF
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSDR-DRCD-MAX )  OR
                              (SPA-K081-DRCD-MAX  >=   99  )
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDZ     FROM   1   BY  1
                       UNTIL      (IDZ     >   SPA-K081-DRCD-MAX )  OR
                                  (FLG-CHK =   1    )
                   IF      ORCSDR-DRCD(IDX)
                                           =   SPA-K081GMN-DRCDL(IDZ)
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   ADD     1               TO  SPA-K081-DRCD-MAX
                   MOVE    ORCSDR-DRCD-TBL(IDX)
                                           TO  SPA-K081GMN-DRCDLST
                                                   (SPA-K081-DRCD-MAX)
               END-IF
           END-PERFORM
           MOVE    SPA-K081GMN-DRCDLST(1)  TO  SPA-K081GMN-DRCD-G
      *
      *    ユーザプログラム起動初期処理
           IF      SPA-ETC-MOTOPG      =   "K02N"
      *v4.8
      *        IF      SPA-MW              =   SPA-GINBEE
      *            INITIALIZE                  SPA-K081GMN-USERPG-AREA
      *        ELSE
                   PERFORM 3101-USERPG-SET-SEC
      **       END-IF
           ELSE
               INITIALIZE                  SPA-K081GMN-USERPG-AREA
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    指示せん発行初期処理
      *****************************************************************
       3101-SPA-HENSYU-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                      SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-5007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-5007-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
               INITIALIZE                      SYS-5007-REC
           END-IF
           PERFORM
                   UNTIL       FLG-SYSKANRI    =   1
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
               EVALUATE    SYS-5007-KBNCD
                   WHEN    "01"
      *                入院処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK1(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK1FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-SYOHOU-ARI
                           END-IF
                       END-PERFORM
                   WHEN    "03"
      *                注射処方箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK2(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK2FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-CHUSYA-ARI
                           END-IF
                       END-PERFORM
                   WHEN    "05"
      *                指示箋
                       PERFORM VARYING IDX     FROM    1   BY  1
                               UNTIL   IDX     >   4
                           MOVE    SYS-5007-SIJISEN-HEAD(IDX)
                                           TO  SPA-K081GMN-CHK3(IDX)
                           IF      SYS-5007-SIJISEN-PRT (IDX)   >   ZERO
                               MOVE    1       TO  SPA-K081GMN-CHK3FLG
                                                                (IDX)
                               MOVE    1           TO  FLG-SIJISEN-ARI
                           END-IF
                       END-PERFORM
               END-EVALUATE
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-5007-REC
                   MOVE    ZERO                TO  FLG-SYSKANRI
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院処方箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-SYOHOU-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-SYOHOU-LST(2)
           MOVE    2                   TO  SPA-K081-SYOHOU-MAX
           IF      FLG-SYOHOU-ARI      =   ZERO
               MOVE    1                   TO  SPA-K081-SYOHOU-MAX
           END-IF
           IF     (FLG-SYOHOU-ARI      =   1   )  AND
                  (SPA-NAI-SYOHOU      =   1   )
               MOVE    SPA-K081GMN-SYOHOU-LST(2)
                                       TO  SPA-K081GMN-SYOHOU-G
           ELSE
               MOVE    SPA-K081GMN-SYOHOU-LST(1)
                                       TO  SPA-K081GMN-SYOHOU-G
           END-IF
           PERFORM 3102-SYOHOU-CHK-SEC
      *    注射処方箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-CHUSYA-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-CHUSYA-LST(2)
           MOVE    2                   TO  SPA-K081-CHUSYA-MAX
           IF      FLG-CHUSYA-ARI      =   ZERO
               MOVE    1                   TO  SPA-K081-CHUSYA-MAX
           END-IF
           IF     (FLG-CHUSYA-ARI      =   1   )  AND
                  (SPA-NAI-CHUSYA      =   1   )
               MOVE    SPA-K081GMN-CHUSYA-LST(2)
                                       TO  SPA-K081GMN-CHUSYA-G
           ELSE
               MOVE    SPA-K081GMN-CHUSYA-LST(1)
                                       TO  SPA-K081GMN-CHUSYA-G
           END-IF
           PERFORM 3103-CHUSYA-CHK-SEC
      *    指示箋
           MOVE    "0 発行しない"      TO  SPA-K081GMN-SIJISEN-LST(1)
           MOVE    "1 発行する"        TO  SPA-K081GMN-SIJISEN-LST(2)
           MOVE    2                   TO  SPA-K081-SIJISEN-MAX
           IF      FLG-SIJISEN-ARI     =   ZERO
               MOVE    1                   TO  SPA-K081-SIJISEN-MAX
           END-IF
           IF     (FLG-SIJISEN-ARI     =   1   )  AND
                  (SPA-NAI-SIJISEN     =   1   )
               MOVE    SPA-K081GMN-SIJISEN-LST(2)
                                       TO  SPA-K081GMN-SIJISEN-G
           ELSE
               MOVE    SPA-K081GMN-SIJISEN-LST(1)
                                       TO  SPA-K081GMN-SIJISEN-G
           END-IF
           PERFORM 3104-SIJISEN-CHK-SEC
      *
           .
       3101-SPA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *     ユーザプログラム起動初期処理
      *****************************************************************
       3101-USERPG-SET-SEC              SECTION.
      *
           MOVE    "0 Ｕ・Ｐ指示なし"      TO  SPA-K081GMN-USERPG-LIST
                                                                  (1)
           IF      SPA-K081NAI-GYOUMU-ARI  =   "1"
      *        実行するプログラムがある時
               MOVE    "1 Ｕ・Ｐ指示あり"  TO  SPA-K081GMN-USERPG-LIST
                                                                  (2)
               MOVE    2                   TO  SPA-K081GMN-USERPG-MAX
               IF      SPA-K081NAI-GYOUMU-FLG  =   "1"
                   MOVE    SPA-K081GMN-USERPG-LIST (2)
                                               TO  SPA-K081GMN-USERPG-G
               ELSE
                   MOVE    SPA-K081GMN-USERPG-LIST (1)
                                               TO  SPA-K081GMN-USERPG-G
               END-IF
           ELSE
      *        実行するプログラムがない時
               MOVE    1                   TO  SPA-K081GMN-USERPG-MAX
               MOVE    SPA-K081GMN-USERPG-LIST(1)
                                               TO  SPA-K081GMN-USERPG-G
           END-IF
           .
       3101-USERPG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       3105-DRCD-ADD-SEC                   SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               ADD     1                   TO  SPA-K081-DRCD-MAX
               MOVE    WRK-DRCD            TO  SPA-K081GMN-DRCDL
                                                (SPA-K081-DRCD-MAX)
               MOVE    SYS-1010-NAME       TO  SPA-K081GMN-DRCDMEIL
                                                (SPA-K081-DRCD-MAX)
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3105-DRCD-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方箋チェック区分設定
      *****************************************************************
       3102-SYOHOU-CHK-SEC                   SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   4
               IF     (SPA-K081GMN-SYOHOU  =   "1"  )  AND
                      (SPA-K081GMN-CHK1FLG(IDX)    =   1 )
                   MOVE    "T"             TO  SPA-K081GMN-CHK1-VALUE
                                                                 (IDX)
                   MOVE    1               TO  SPA-K081GMN-CHK1KBN
                                                                 (IDX)
               ELSE
                   MOVE    "F"             TO  SPA-K081GMN-CHK1-VALUE
                                                                 (IDX)
                   MOVE    ZERO            TO  SPA-K081GMN-CHK1KBN
                                                                 (IDX)
               END-IF
           END-PERFORM
     *
           .
       3102-SYOHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    注射箋チェック区分設定
      *****************************************************************
       3103-CHUSYA-CHK-SEC                   SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   4
               IF     (SPA-K081GMN-CHUSYA  =   "1"  )  AND
                      (SPA-K081GMN-CHK2FLG(IDX)  =   1 )
                   MOVE    "T"             TO  SPA-K081GMN-CHK2-VALUE
                                                                 (IDX)
                   MOVE    1               TO  SPA-K081GMN-CHK2KBN
                                                                 (IDX)
               ELSE
                   MOVE    "F"             TO  SPA-K081GMN-CHK2-VALUE
                                                                 (IDX)
                   MOVE    ZERO            TO  SPA-K081GMN-CHK2KBN
                                                                 (IDX)
               END-IF
           END-PERFORM
     *
           .
       3103-CHUSYA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    指示せんチェック区分設定
      *****************************************************************
       3104-SIJISEN-CHK-SEC                   SECTION.
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   4
               IF     (SPA-K081GMN-SIJISEN =   "1"  )  AND
                      (SPA-K081GMN-CHK3FLG(IDX)    =   1 )
                   MOVE    "T"             TO  SPA-K081GMN-CHK3-VALUE
                                                                 (IDX)
                   MOVE    1               TO  SPA-K081GMN-CHK3KBN
                                                                 (IDX)
               ELSE
                   MOVE    "F"             TO  SPA-K081GMN-CHK3-VALUE
                                                                 (IDX)
                   MOVE    ZERO            TO  SPA-K081GMN-CHK3KBN
                                                                 (IDX)
               END-IF
           END-PERFORM
     *
           .
       3104-SIJISEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    ZERO            TO  SPA-K081-OKFLG
                   PERFORM 210-BACK
      *        頭書
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-HEAD-PRINT-SEC
      *        発行
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    1               TO  SPA-K081-OKFLG
                   PERFORM 450-TOROKU-SEC
      *        発行のみ
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    2               TO  SPA-K081-OKFLG
                   PERFORM 450-TOROKU-SEC
      *
               WHEN    "CLICKED"       ALSO    "CHK11"
               WHEN    "CLICKED"       ALSO    "CHK12"
               WHEN    "CLICKED"       ALSO    "CHK13"
               WHEN    "CLICKED"       ALSO    "CHK14"
               WHEN    "CLICKED"       ALSO    "CHK21"
               WHEN    "CLICKED"       ALSO    "CHK22"
               WHEN    "CLICKED"       ALSO    "CHK23"
               WHEN    "CLICKED"       ALSO    "CHK24"
               WHEN    "CLICKED"       ALSO    "CHK31"
               WHEN    "CLICKED"       ALSO    "CHK32"
               WHEN    "CLICKED"       ALSO    "CHK33"
               WHEN    "CLICKED"       ALSO    "CHK34"
                   MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
                   PERFORM 420-CHK-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-K081GMN-CUR
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC        SECTION.
      *
           IF      K081-SYOHOU(1:1)    =   SPA-K081GMN-SYOHOU
               MOVE    ZERO                TO  FLG-SYOHOU-CHK
           ELSE
               MOVE    1                   TO  FLG-SYOHOU-CHK
           END-IF
           IF      K081-CHUSYA(1:1)    =   SPA-K081GMN-CHUSYA
               MOVE    ZERO                TO  FLG-CHUSYA-CHK
           ELSE
               MOVE    1                   TO  FLG-CHUSYA-CHK
           END-IF
           IF      K081-SIJISEN(1:1)   =   SPA-K081GMN-SIJISEN
               MOVE    ZERO                TO  FLG-SIJISEN-CHK
           ELSE
               MOVE    1                   TO  FLG-SIJISEN-CHK
           END-IF
           MOVE    K081-SYOHOU         TO  SPA-K081GMN-SYOHOU-G
           MOVE    K081-CHUSYA         TO  SPA-K081GMN-CHUSYA-G
           MOVE    K081-SIJISEN        TO  SPA-K081GMN-SIJISEN-G
           MOVE    K081-DRCD           TO  SPA-K081GMN-DRCD-G
      *
           MOVE    K081-USERPG         TO  SPA-K081GMN-USERPG-G
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
      *    入院処方箋
           PERFORM 4100-SYOHOU-CHK-SEC
      *    注射処方箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-CHUSYA-CHK-SEC
           END-IF
      *    指示箋
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-SIJISEN-CHK-SEC
           END-IF
      *    ドクター
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-DRCD-CHK-SEC
           END-IF
      *    ユーザプログラム
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-USERPG-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院処方箋チェック処理
      *****************************************************************
       4100-SYOHOU-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K081-SYOHOU-MAX
               IF      SPA-K081GMN-SYOHOU  =   SPA-K081GMN-SYOHOU-LST
                                                   (IDX)(1:1)
                   MOVE    SPA-K081GMN-SYOHOU-LST(IDX)
                                           TO  SPA-K081GMN-SYOHOU-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-K081GMN-CUR
           END-IF
      *    変更時の設定
           IF     (FLG-SYOHOU-CHK      =   1  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        発行しないの時、すべて解除
               IF      SPA-K081GMN-SYOHOU  =   "0"
                   MOVE    "F"             TO  WRK-VALUE
                   MOVE    ZERO            TO  WRK-CHKKBN
               ELSE
                   MOVE    "T"             TO  WRK-VALUE
                   MOVE    1               TO  WRK-CHKKBN
               END-IF
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      SPA-K081GMN-CHK1FLG(IDX)    =   1
                       MOVE    WRK-VALUE   TO  SPA-K081GMN-CHK1-VALUE
                                                                 (IDX)
                       MOVE    WRK-CHKKBN  TO  SPA-K081GMN-CHK1KBN
                                                                 (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4100-SYOHOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    注射処方箋チェック処理
      *****************************************************************
       4101-CHUSYA-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K081-CHUSYA-MAX
               IF      SPA-K081GMN-CHUSYA  =   SPA-K081GMN-CHUSYA-LST
                                                   (IDX)(1:1)
                   MOVE    SPA-K081GMN-CHUSYA-LST(IDX)
                                           TO  SPA-K081GMN-CHUSYA-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K081GMN-CUR
           END-IF
      *    変更時の設定
           IF     (FLG-CHUSYA-CHK      =   1  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        発行しないの時、すべて解除
               IF      SPA-K081GMN-CHUSYA  =   "0"
                   MOVE    "F"             TO  WRK-VALUE
                   MOVE    ZERO            TO  WRK-CHKKBN
               ELSE
                   MOVE    "T"             TO  WRK-VALUE
                   MOVE    1               TO  WRK-CHKKBN
               END-IF
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      SPA-K081GMN-CHK2FLG(IDX)    =   1
                       MOVE    WRK-VALUE   TO  SPA-K081GMN-CHK2-VALUE
                                                                 (IDX)
                       MOVE    WRK-CHKKBN  TO  SPA-K081GMN-CHK2KBN
                                                                 (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4101-CHUSYA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   指示箋チェック処理
      *****************************************************************
       4102-SIJISEN-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K081-SIJISEN-MAX
               IF      SPA-K081GMN-SIJISEN =   SPA-K081GMN-SIJISEN-LST
                                                   (IDX)(1:1)
                   MOVE    SPA-K081GMN-SIJISEN-LST(IDX)
                                           TO  SPA-K081GMN-SIJISEN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-K081GMN-CUR
           END-IF
      *    変更時の設定
           IF     (FLG-SIJISEN-CHK     =   1  )  AND
                  (SPA-ERRCD           =   SPACE )
      *        発行しないの時、すべて解除
               IF      SPA-K081GMN-SIJISEN =   "0"
                   MOVE    "F"             TO  WRK-VALUE
                   MOVE    ZERO            TO  WRK-CHKKBN
               ELSE
                   MOVE    "T"             TO  WRK-VALUE
                   MOVE    1               TO  WRK-CHKKBN
               END-IF
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >   4
                   IF      SPA-K081GMN-CHK3FLG(IDX)    =   1
                       MOVE    WRK-VALUE   TO  SPA-K081GMN-CHK3-VALUE
                                                                 (IDX)
                       MOVE    WRK-CHKKBN  TO  SPA-K081GMN-CHK3KBN
                                                                 (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       4102-SIJISEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       4103-DRCD-CHK-SEC              SECTION.
      *
           IF      SPA-K081GMN-DRCD    =   SPACE
               MOVE    SPACE           TO  SPA-K081GMN-DRCD-G
           ELSE
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    SPA-K081GMN-DRCD-G  TO  ORCSDR-IN-DRCD-G
           MOVE    SPA-K081-DRCD-MAX   TO  ORCSDR-DRCD-MAX
           MOVE    SPA-K081GMN-DRCDLST-G
                                       TO  ORCSDR-DRCD-TBL-G
           CALL   "ORCSDRSET"          USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-K081GMN-DRCDLST-G
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-K081GMN-DRCD-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-K081-DRCD-MAX
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-K081GMN-CUR
           END-IF
      *
           END-IF
      *
           .
       4103-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム処理
      *****************************************************************
       4103-USERPG-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K081GMN-USERPG-MAX
               IF      SPA-K081GMN-USERPG  =   SPA-K081GMN-USERPG-LIST
                                                         (IDX)(1:1)
                   MOVE    SPA-K081GMN-USERPG-LIST(IDX)
                                           TO  SPA-K081GMN-USERPG-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               IF     (SPA-K081NAI-GYOUMU-ARI  =   "1"   )  AND
                      (SPA-K081NAI-GYOUMU-FLG  =   "1"   )
                   MOVE    SPA-K081GMN-USERPG-LIST (2)
                                           TO  SPA-K081GMN-USERPG-G
               ELSE
                   MOVE    SPA-K081GMN-USERPG-LIST (1)
                                           TO  SPA-K081GMN-USERPG-G
               END-IF
           END-IF
           .
       4103-USERPG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック選択処理
      *****************************************************************
       420-CHK-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-K081GMN-CUR
           EVALUATE    WRK-MCP-WIDGET(1:4)
               WHEN    "CHK1"
                   MOVE    WRK-MCP-WIDGET(5:1) TO  ID1
                   MOVE    K081-CHK1-VALUE (ID1)
                                           TO  SPA-K081GMN-CHK1-VALUE
                                                             (ID1)
                   IF      SPA-K081GMN-CHK1FLG (ID1)   =   ZERO
                       MOVE    "F"         TO  SPA-K081GMN-CHK1-VALUE
                                                             (ID1)
                   END-IF
               WHEN    "CHK2"
                   MOVE    WRK-MCP-WIDGET(5:1) TO  ID1
                   MOVE    K081-CHK2-VALUE (ID1)
                                           TO  SPA-K081GMN-CHK2-VALUE
                                                             (ID1)
                   IF      SPA-K081GMN-CHK2FLG (ID1)   =   ZERO
                       MOVE    "F"         TO  SPA-K081GMN-CHK2-VALUE
                                                             (ID1)
                   END-IF
               WHEN    "CHK3"
                   MOVE    WRK-MCP-WIDGET(5:1) TO  ID1
                   MOVE    K081-CHK3-VALUE (ID1)
                                           TO  SPA-K081GMN-CHK3-VALUE
                                                             (ID1)
                   IF      SPA-K081GMN-CHK3FLG (ID1)   =   ZERO
                       MOVE    "F"         TO  SPA-K081GMN-CHK3-VALUE
                                                             (ID1)
                   END-IF
           END-EVALUATE
           MOVE    ZERO            TO  FLG-SYOHOU-ARI
           MOVE    ZERO            TO  FLG-CHUSYA-ARI
           MOVE    ZERO            TO  FLG-SIJISEN-ARI
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >   4
               IF     SPA-K081GMN-CHK1-VALUE(IDX)  =   "T"
                   MOVE    1               TO  SPA-K081GMN-CHK1KBN(IDX)
                   MOVE    1               TO  FLG-SYOHOU-ARI
               ELSE
                   MOVE    ZERO            TO  SPA-K081GMN-CHK1KBN(IDX)
               END-IF
      *
               IF     SPA-K081GMN-CHK2-VALUE(IDX)  =   "T"
                   MOVE    1               TO  SPA-K081GMN-CHK2KBN(IDX)
                   MOVE    1               TO  FLG-CHUSYA-ARI
               ELSE
                   MOVE    ZERO            TO  SPA-K081GMN-CHK2KBN(IDX)
               END-IF
      *
               IF     SPA-K081GMN-CHK3-VALUE(IDX)  =   "T"
                   MOVE    1               TO  SPA-K081GMN-CHK3KBN(IDX)
                   MOVE    1               TO  FLG-SIJISEN-ARI
               ELSE
                   MOVE    ZERO            TO  SPA-K081GMN-CHK3KBN(IDX)
               END-IF
           END-PERFORM
      *
           IF      FLG-SYOHOU-ARI      =   1
               IF      SPA-K081GMN-SYOHOU  =   "0"
                   MOVE    SPA-K081GMN-SYOHOU-LST (2)
                                               TO  SPA-K081GMN-SYOHOU-G
               END-IF
           ELSE
               IF      SPA-K081GMN-SYOHOU  =   "1"
                   MOVE    SPA-K081GMN-SYOHOU-LST (1)
                                               TO  SPA-K081GMN-SYOHOU-G
               END-IF
           END-IF
      *
           IF      FLG-CHUSYA-ARI      =   1
               IF      SPA-K081GMN-CHUSYA  =   "0"
                   MOVE    SPA-K081GMN-CHUSYA-LST (2)
                                               TO  SPA-K081GMN-CHUSYA-G
               END-IF
           ELSE
               IF      SPA-K081GMN-CHUSYA  =   "1"
                   MOVE    SPA-K081GMN-CHUSYA-LST (1)
                                               TO  SPA-K081GMN-CHUSYA-G
               END-IF
           END-IF
      *
           IF      FLG-SIJISEN-ARI      =   1
               IF      SPA-K081GMN-SIJISEN  =   "0"
                   MOVE    SPA-K081GMN-SIJISEN-LST (2)
                                               TO  SPA-K081GMN-SIJISEN-G
               END-IF
           ELSE
               IF      SPA-K081GMN-SIJISEN  =   "1"
                   MOVE    SPA-K081GMN-SIJISEN-LST (1)
                                               TO  SPA-K081GMN-SIJISEN-G
               END-IF
           END-IF
           .
       420-CHK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    SPA-ETC-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K081"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 210-BACK
           END-IF
      *
           .
       450-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    頭書印刷処理
      *****************************************************************
       440-HEAD-PRINT-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *        帳票プログラム取得
               PERFORM 459001-PRTNUM-HEN-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *
      *        印刷制御情報取得サブ
               CALL    "ORCSONPRTSET"      USING   SPA-AREA
               IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                 OR  ( SPA-PRTCONF         =   "1" )
      *            クライアント印刷制御情報取得処理
                   PERFORM 4901-CLIENT-PRT-SEC
               END-IF
      *
               INITIALIZE              ORCSMCNDATEAREA
               CALL    "ORCSMCNDATE"   USING
                                       ORCSMCNDATEAREA
      *
               IF       SPA-K081GMN-SYOHOU     =   "1"
                   PERFORM 4401-HC501-PRINT-SEC
               END-IF
               IF       SPA-K081GMN-CHUSYA     =   "1"
                   PERFORM 4402-HC502-PRINT-SEC
               END-IF
               IF       SPA-K081GMN-SIJISEN     =   "1"
                   PERFORM 4403-HC503-PRINT-SEC
               END-IF
      *
               IF      (SPA-CLIENT-PRT-FLG =   1  )
      *            印刷ダミー処理
                   MOVE    "99"                TO  SPA-PRT-FLG
                   CALL    "ORCHCDUMMY"    USING   SPA-AREA
                   MOVE    SPACE               TO  SPA-PRT-FLG
               END-IF
               MOVE    ZERO                    TO  SPA-CLIENT-TEMP-FLG
           END-IF
      *
           .
       440-HEAD-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       459001-PRTNUM-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           MOVE    SPACE               TO  WRK-CHUSYA-PG
           MOVE    SPACE               TO  WRK-SIJISEN-PG
      *    入院処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1202"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
           END-IF
      *    注射処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1203"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-CHUSYA-PG
           END-IF
      *    指示せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1204"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SIJISEN-PG
           END-IF
           .
       459001-PRTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    入院処方箋頭書印刷処理
      *****************************************************************
       4401-HC501-PRINT-SEC             SECTION.
      *
           INITIALIZE              ORCHC501AREA
           MOVE    1                   TO  ORCHC501-SYORIKBN
           MOVE    SPA-PTNUM           TO  ORCHC501-PTNUM
           MOVE    SPA-SRYYMD          TO  ORCHC501-SRYYMD
           MOVE    SPA-SYSYMD          TO  ORCHC501-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCHC501-SRYKA
           MOVE    SMCNDATE-YMD        TO  ORCHC501-PRINTYMD
           MOVE    SMCNDATE-HMS        TO  ORCHC501-PRINTHMS
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCHC501-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  ORCHC501-DRCD(2:4)
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
               IF      SPA-K081GMN-CHK1KBN (IDX)   =   "1"
                   MOVE    1               TO  ORCHC501-CHKPRT(IDX)
               END-IF
           END-PERFORM
      *
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "01"                TO  SPA-PRT-FLG
               MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           CALL    WRK-SYOHOU-PG       USING
                                       SPA-AREA
                                       ORCHC501AREA
           .
       4401-HC501-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    注射処方箋頭書印刷処理
      *****************************************************************
       4402-HC502-PRINT-SEC             SECTION.
      *
           INITIALIZE              ORCHC502AREA
           MOVE    1                   TO  ORCHC502-SYORIKBN
           MOVE    SPA-PTNUM           TO  ORCHC502-PTNUM
           MOVE    SPA-SRYYMD          TO  ORCHC502-SRYYMD
           MOVE    SPA-SYSYMD          TO  ORCHC502-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCHC502-SRYKA
           MOVE    SMCNDATE-YMD        TO  ORCHC502-PRINTYMD
           MOVE    SMCNDATE-HMS        TO  ORCHC502-PRINTHMS
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCHC502-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  ORCHC502-DRCD(2:4)
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
               IF      SPA-K081GMN-CHK2KBN (IDX)   =   "1"
                   MOVE    1               TO  ORCHC502-CHKPRT(IDX)
               END-IF
           END-PERFORM
      *
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "02"                TO  SPA-PRT-FLG
               MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           CALL    WRK-CHUSYA-PG       USING
                                       SPA-AREA
                                       ORCHC502AREA
           .
       4402-HC502-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    指示箋頭書印刷処理
      *****************************************************************
       4403-HC503-PRINT-SEC             SECTION.
      *
           INITIALIZE              ORCHC503AREA
           MOVE    1                   TO  ORCHC503-SYORIKBN
           MOVE    SPA-PTNUM           TO  ORCHC503-PTNUM
           MOVE    SPA-SRYYMD          TO  ORCHC503-SRYYMD
           MOVE    SPA-SYSYMD          TO  ORCHC503-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCHC503-SRYKA
           MOVE    SMCNDATE-YMD        TO  ORCHC503-PRINTYMD
           MOVE    SMCNDATE-HMS        TO  ORCHC503-PRINTHMS
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCHC503-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  ORCHC503-DRCD(2:4)
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
               IF      SPA-K081GMN-CHK3KBN (IDX)   =   "1"
                   MOVE    1               TO  ORCHC503-CHKPRT(IDX)
               END-IF
           END-PERFORM
      *
      *    IF      FLG-CLIENT-OK       =   1
               MOVE    "03"                TO  SPA-PRT-FLG
               MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *    END-IF
      *
           CALL    WRK-SIJISEN-PG      USING
                                       SPA-AREA
                                       ORCHC503AREA
           .
       4403-HC503-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      * 再印刷なし
      *
      *    INITIALIZE                  KDMY01
      *    MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      ***  MOVE    "入院処方箋"        TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "注射処方箋"        TO  ORCSPRTCTRL-TITLE-I(02)
      ***  MOVE    "入院指示箋"        TO  ORCSPRTCTRL-TITLE-I(03)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      **** MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
           MOVE    1                   TO  FLG-CLIENT-OK
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    元の画面へ
      *****************************************************************
       4502-BACK-SYORI-SEC             SECTION.
      *
           MOVE    SPA-ETC-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K081"              TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4502-BACK-SYORI-EXT.
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
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "K081"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  K081
      *
           INITIALIZE                      K081
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K081-SYOHOU-MAX
               MOVE    SPA-K081GMN-SYOHOU-LST (IDX)
                                           TO  K081-SYOHOU-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-K081-SYOHOU-MAX TO  K081-SYOHOU-COUNT
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K081-CHUSYA-MAX
               MOVE    SPA-K081GMN-CHUSYA-LST (IDX)
                                           TO  K081-CHUSYA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-K081-CHUSYA-MAX     TO  K081-CHUSYA-COUNT
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K081-SIJISEN-MAX
               MOVE    SPA-K081GMN-SIJISEN-LST (IDX)
                                           TO  K081-SIJISEN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-K081-SIJISEN-MAX    TO  K081-SIJISEN-COUNT
      *
           MOVE    SPA-K081GMN-SYOHOU-G    TO  K081-SYOHOU
           MOVE    SPA-K081GMN-CHUSYA-G    TO  K081-CHUSYA
           MOVE    SPA-K081GMN-SIJISEN-G   TO  K081-SIJISEN
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K081-DRCD-MAX
               MOVE    SPA-K081GMN-DRCDLST (IDX)
                                           TO  K081-DRCD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-K081-DRCD-MAX   TO  K081-DRCD-COUNT
           MOVE    SPA-K081GMN-DRCD-G  TO  K081-DRCD
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   4
               MOVE    SPA-K081GMN-CHK1 (IDX)
                                           TO  K081-CHK1 (IDX)
               MOVE    SPA-K081GMN-CHK1-VALUE (IDX)
                                           TO  K081-CHK1-VALUE (IDX)
               IF      SPA-K081GMN-CHK1FLG (IDX)   =   ZERO
                   MOVE    WIDGET-INSENSITIVE
                                           TO  K081-CHK1-STATE  (IDX)
               ELSE
                   MOVE    WIDGET-NORMAL
                                           TO  K081-CHK1-STATE  (IDX)
               END-IF
               IF      SPA-K081GMN-CHK1KBN (IDX)   =   1
                   MOVE    "black"         TO  K081-CHK1-STYLE (IDX)
               ELSE
      ************ MOVE    "blue"          TO  K081-CHK1-STYLE (IDX)
                   MOVE    "black"         TO  K081-CHK1-STYLE (IDX)
               END-IF
           END-PERFORM
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   4
               MOVE    SPA-K081GMN-CHK2 (IDX)
                                           TO  K081-CHK2 (IDX)
               MOVE    SPA-K081GMN-CHK2-VALUE (IDX)
                                           TO  K081-CHK2-VALUE (IDX)
               IF      SPA-K081GMN-CHK2FLG (IDX)   =   ZERO
                   MOVE    WIDGET-INSENSITIVE
                                           TO  K081-CHK2-STATE  (IDX)
               ELSE
                   MOVE    WIDGET-NORMAL
                                           TO  K081-CHK2-STATE  (IDX)
               END-IF
               IF      SPA-K081GMN-CHK2KBN (IDX)   =   1
                   MOVE    "black"         TO  K081-CHK2-STYLE (IDX)
               ELSE
      **********   MOVE    "blue"          TO  K081-CHK2-STYLE (IDX)
                   MOVE    "black"         TO  K081-CHK2-STYLE (IDX)
               END-IF
           END-PERFORM
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   4
               MOVE    SPA-K081GMN-CHK3 (IDX)
                                           TO  K081-CHK3 (IDX)
               MOVE    SPA-K081GMN-CHK3-VALUE (IDX)
                                           TO  K081-CHK3-VALUE (IDX)
               IF      SPA-K081GMN-CHK3FLG (IDX)   =   ZERO
                   MOVE    WIDGET-INSENSITIVE
                                           TO  K081-CHK3-STATE  (IDX)
               ELSE
                   MOVE    WIDGET-NORMAL
                                           TO  K081-CHK3-STATE  (IDX)
               END-IF
               IF      SPA-K081GMN-CHK3KBN (IDX)   =   1
                   MOVE    "black"         TO  K081-CHK3-STYLE (IDX)
               ELSE
      *********    MOVE    "blue"          TO  K081-CHK3-STYLE (IDX)
                   MOVE    "black"         TO  K081-CHK3-STYLE (IDX)
               END-IF
           END-PERFORM
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K081GMN-USERPG-MAX
               MOVE    SPA-K081GMN-USERPG-LIST (IDX)
                                           TO  K081-USERPG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-K081GMN-USERPG-MAX  TO  K081-USERPG-COUNT
           MOVE    SPA-K081GMN-USERPG-G    TO  K081-USERPG
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    WIDGET-INSENSITIVE  TO  K081-USERPG-STATE
      *    ELSE
               MOVE    WIDGET-NORMAL       TO  K081-USERPG-STATE
      **   END-IF
      *
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF      SPA-K081GMN-CUR     =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-K081GMN-CUR
               WHEN    01
                   MOVE    "SYOHOU"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "CHUSYA"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SIJISEN"         TO  MCP-WIDGET
               WHEN    04
                   MOVE    "DRCD"            TO  MCP-WIDGET
               WHEN    11
                   MOVE    "CHK11"           TO  MCP-WIDGET
               WHEN    12
                   MOVE    "CHK12"           TO  MCP-WIDGET
               WHEN    13
                   MOVE    "CHK13"           TO  MCP-WIDGET
               WHEN    14
                   MOVE    "CHK14"           TO  MCP-WIDGET
               WHEN    21
                   MOVE    "CHK21"           TO  MCP-WIDGET
               WHEN    22
                   MOVE    "CHK22"           TO  MCP-WIDGET
               WHEN    23
                   MOVE    "CHK23"           TO  MCP-WIDGET
               WHEN    24
                   MOVE    "CHK24"           TO  MCP-WIDGET
               WHEN    31
                   MOVE    "CHK31"           TO  MCP-WIDGET
               WHEN    32
                   MOVE    "CHK32"           TO  MCP-WIDGET
               WHEN    33
                   MOVE    "CHK33"           TO  MCP-WIDGET
               WHEN    34
                   MOVE    "CHK34"           TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SYOHOU"
                   MOVE    11          TO  SPA-K081GMN-CUR
               WHEN    "CHK11"
                   MOVE    12          TO  SPA-K081GMN-CUR
               WHEN    "CHK12"
                   MOVE    13          TO  SPA-K081GMN-CUR
               WHEN    "CHK13"
                   MOVE    14          TO  SPA-K081GMN-CUR
               WHEN    "CHK14"
                   MOVE    02          TO  SPA-K081GMN-CUR
      *
               WHEN    "CHUSYA"
                   MOVE    21          TO  SPA-K081GMN-CUR
               WHEN    "CHK21"
                   MOVE    22          TO  SPA-K081GMN-CUR
               WHEN    "CHK22"
                   MOVE    23          TO  SPA-K081GMN-CUR
               WHEN    "CHK23"
                   MOVE    24          TO  SPA-K081GMN-CUR
               WHEN    "CHK24"
                   MOVE    03          TO  SPA-K081GMN-CUR
      *
               WHEN    "SIJISEN"
                   MOVE    31          TO  SPA-K081GMN-CUR
               WHEN    "CHK31"
                   MOVE    32          TO  SPA-K081GMN-CUR
               WHEN    "CHK32"
                   MOVE    33          TO  SPA-K081GMN-CUR
               WHEN    "CHK33"
                   MOVE    34          TO  SPA-K081GMN-CUR
               WHEN    "CHK34"
                   MOVE    04          TO  SPA-K081GMN-CUR
      *
               WHEN    "DRCD"
                   MOVE    99          TO  SPA-K081GMN-CUR
               WHEN    "USERPG"
                   MOVE    99          TO  SPA-K081GMN-CUR
           END-EVALUATE
      *
           IF      SPA-K081GMN-CUR     >=  11  AND  <=  14
               COMPUTE IDX             =   SPA-K081GMN-CUR  -  10
               PERFORM
                       UNTIL   (IDX        >   4   )  OR
                               (SPA-K081GMN-CHK1FLG(IDX) =   1)
                   ADD     1               TO  IDX
               END-PERFORM
               IF      IDX             >   4
                   MOVE    02          TO  SPA-K081GMN-CUR
               ELSE
                   COMPUTE SPA-K081GMN-CUR =   IDX   +  10
               END-IF
           END-IF
      *
           IF      SPA-K081GMN-CUR     >=  21  AND  <=  24
               COMPUTE IDX             =   SPA-K081GMN-CUR  -  20
               PERFORM
                       UNTIL   (IDX        >   4   )  OR
                               (SPA-K081GMN-CHK2FLG(IDX) =   1)
                   ADD     1               TO  IDX
               END-PERFORM
               IF      IDX             >   4
                   MOVE    03          TO  SPA-K081GMN-CUR
               ELSE
                   COMPUTE SPA-K081GMN-CUR =   IDX   +  20
               END-IF
           END-IF
      *
           IF      SPA-K081GMN-CUR     >=  31  AND  <=  34
               COMPUTE IDX             =   SPA-K081GMN-CUR  -  30
               PERFORM
                       UNTIL   (IDX        >   4   )  OR
                               (SPA-K081GMN-CHK3FLG(IDX) =   1)
                   ADD     1               TO  IDX
               END-PERFORM
               IF      IDX             >   4
                   MOVE    04          TO  SPA-K081GMN-CUR
               ELSE
                   COMPUTE SPA-K081GMN-CUR =   IDX   +  30
               END-IF
           END-IF
      *
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入院処方入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "注射処方入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "指示せん入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "ドクター入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "1202"
                   MOVE    "入院処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1203"
                   MOVE    "注射処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1204"
                   MOVE    "指示箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    OTHER
                   MOVE    SPA-ERRCD
                                               TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K081"              TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTNYUINRRK-REC
               MOVE    ZERO                TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                      PTNYUINRRK-REC
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
