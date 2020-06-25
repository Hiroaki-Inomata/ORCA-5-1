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
       PROGRAM-ID.                 ORCGXD01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : ユーザプログラム起動
      *  コンポーネント名  : ユーザプログラム実行（ＸＤ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/10/05    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/02/14  MONFUNC 対応
      *  04.08.00    NACL-多々納  16/06/22  伝票番号追加対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
      *FILE-CONTROL.
      *
      *
       DATA                        DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "XD01SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDX-T               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-ORC-DBPATH          PIC X(64).
           03  WRK-MCP-WIDGET          PIC X(60).
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
       01  CONST-AREA.
           03  CONST-NYU-SYORIKBN-VAL.
      *
               05  CONST-NYUIN-G.
                   07  CONST-NYUIN
                           PIC 9(02)   VALUE   01.
                   07  CONST-NYUINMEI
                           PIC X(30)   VALUE   "入院登録".
      *
               05  CONST-TAIIN-G.
                   07  CONST-TAIIN
                           PIC 9(02)   VALUE   02.
                   07  CONST-TAIINMEI
                           PIC X(30)   VALUE   "退院登録".
      *
               05  CONST-HENKOU-G.
                   07  CONST-HENKOU
                           PIC 9(02)   VALUE   03.
                   07  CONST-HENKOUMEI
                           PIC X(30)   VALUE   "変更".
      *
               05  CONST-SYOUKAI-G.
                   07  CONST-SYOUKAI
                           PIC 9(02)   VALUE   04.
                   07  CONST-SYOUKAIFLMEI 
                           PIC X(30)   VALUE   "照会".
      *
               05  CONST-NYUINTORIKESHI-G.
                   07  CONST-NYUINTORIKESHI
                           PIC 9(02)   VALUE   05.
                   07  CONST-NYUINTORIKESHIMEI
                           PIC X(30)   VALUE   "入院取消".
      *
               05  CONST-NYUINTORIKESHIALL-G.
                   07  CONST-NYUINTORIKESHIALL
                           PIC 9(02)   VALUE   06.
                   07  CONST-NYUINTORIKESHIALLMEI
                           PIC X(30)   VALUE "入院取消（会計含む）".
      *
               05  CONST-TAIINTORIKESHI-G.
                   07  CONST-TAIINTORIKESHI
                           PIC 9(02)   VALUE   07.
                   07  CONST-TAIINTORIKESHIMEI
                           PIC X(30)   VALUE   "退院取消".
      *
               05  CONST-IDO-G.
                   07  CONST-IDO
                           PIC 9(02)    VALUE  08.
                   07  CONST-IDOMEI
                           PIC X(30)    VALUE   "転科　転棟　転室".
      *
               05  CONST-IDOTORIKESHI-G.
                   07  CONST-IDOTORIKESHI
                           PIC 9(02)    VALUE  09.
                   07  CONST-IDOTORIKESHIMEI
                           PIC X(30)   VALUE   "異動取消".
      *
               05  CONST-SAIKEISAN-G.
                   07  CONST-SAIKEISAN
                           PIC 9(02)   VALUE   10.
                   07  CONST-SAIKEISANMEI
                           PIC X(30)   VALUE   "退院再計算".
      *
               05  CONST-TAIINSRYHZN-G.
                   07  CONST-TAIINSRYHZN
                           PIC 9(02)   VALUE   11.
                   07  CONST-TAIINSRYHZNMEI
                           PIC X(30)   VALUE "退院登録（診療保存）".
           03  CONST-NYU-SYORIKBN-R    REDEFINES
                                       CONST-NYU-SYORIKBN-VAL.
               05  CONST-NYU-SYORIKBN-OCC      OCCURS  11.
                   07  CONST-NYU-SYORIKBN      PIC 9(02).
                   07  CONST-NYU-SYORIKBNMEI   PIC X(30).
           03  CONST-NYU-SYORIKBN-MAX          PIC 9(03)   VALUE   11.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9700.INC".
      *    ユーザプログラム起動
           COPY  "CPSK9701.INC".
           COPY  "CPSK3103.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *H28.6
      *    診療行為用共通パラメタ（伝票番号用）
           COPY    "K01COMMON-SPA"  REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *    全角・半角チェック
           COPY    "CPORCXKANACONV.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERPG.INC".
      *
      *    課金チェック
           COPY    "CPORCSBILLINGCHECK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      **** COPY    "ORCA-DBPATH".
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
           COPY    "XD01.INC".
           COPY    "XDERR.INC".
           COPY    "XDID1.INC".
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
      *****MOVE    SPA-WORK            TO  SPA-XD01KYOUTU
           MOVE    SPA-FREE            TO  SPA-XD01
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
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      **** MOVE    SPA-XD01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-XD01            TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "XDERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "XD01"               TO  MCP-WINDOW
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
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-AREA
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "XDID1"
      *            確認画面より
                   PERFORM 3002-XDID1-SET-SEC
               WHEN    OTHER
                   PERFORM 3001-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-XD01
      *
           MOVE    SPA-MOTOPG          TO  SPA-MAE-MOTOPG
      *
           IF    ( SPA-MW              =   SPA-GINBEE )
               INITIALIZE                          SBC-AREA
               CALL    "ORCSBILLINGCHECK"  USING   SBC-AREA
                                                   SPA-AREA
               IF    ( SBC-BILLINGUSER =   "1" )
                   MOVE    "1"         TO  SPA-NAI-CUSTOM-ENABLED
               END-IF
           ELSE
               MOVE    "1"             TO  SPA-NAI-CUSTOM-ENABLED
           END-IF
      *
      *    処理区分
           EVALUATE    SPA-MAE-MOTOPG
           WHEN    "I01"
           WHEN    "I04"
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL ( IDX >   CONST-NYU-SYORIKBN-MAX )
                          OR ( SPA-GMN-SYORIMEI    NOT =   SPACE )
      *
                   IF    ( SPA-SYORI-FLG   =   CONST-NYU-SYORIKBN (IDX))
                       STRING  "【　"          DELIMITED   BY  SIZE
                               CONST-NYU-SYORIKBNMEI (IDX)
                                               DELIMITED   BY  SPACE
                               "　】"          DELIMITED   BY  SIZE
                       INTO    SPA-GMN-SYORIMEI
                       END-STRING
                   END-IF
      *
               END-PERFORM
           WHEN    OTHER
               EVALUATE    SPA-SYORI-FLG
               WHEN    1
                   MOVE    "【　登録処理　】"      TO  SPA-GMN-SYORIMEI
               WHEN    2
                   MOVE    "【　更新処理　】"      TO  SPA-GMN-SYORIMEI
               WHEN    3
                   MOVE    "【　削除処理　】"      TO  SPA-GMN-SYORIMEI
               END-EVALUATE
           END-EVALUATE
      *
      *    診療日
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYMD
           ELSE
               MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-SRYYMD
           END-IF
      *    管理コード決定
           PERFORM 30011-KANRICD-SEC
      *
           EVALUATE    SPA-MAE-MOTOPG
           WHEN    "I01"
           WHEN    "I04"
               PERFORM 30011-PTINF-HEN-SEC
           WHEN    OTHER
               EVALUATE    SPA-SYORI-FLG
               WHEN    1
               WHEN    2
                   PERFORM 30011-PTINF-HEN-SEC
               WHEN    3
                   PERFORM 30012-SPA-HEN-SEC
               END-EVALUATE
           END-EVALUATE
      *H28.6
      *    伝票番号（Ｋ０３）のみ
           IF      SPA-MAE-MOTOPG      =   "K03"
               MOVE    SPA-WORK            TO  WRK-SPA-K01KYOUTU
               MOVE    WRK-SPA-DENPNUM     TO  SPA-NAI-DENPNUM
           END-IF
      *
      *    診療科初期編集
           MOVE    SPACE               TO  SYS-KEY
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
      *
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   40   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKAL   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKAMEIL(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    初期表示
           PERFORM 3001-SPA-INIT-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       3001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       30011-PTINF-HEN-SEC          SECTION.
      *
      *    患者情報検索
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
      *
      *    保険組合せ
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    SPACE               TO  ORCSHKN-KBN2
      *    中途終了・入院は包括も
           IF      SPA-MAE-MOTOPG      =   "K02"
                                       OR  "K02N"
                                       OR  "K08"
               MOVE    "1"                 TO  ORCSHKN-KBN2
           END-IF
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKNCOMBI-MAX
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )
                           OR (IDX     >   30   )
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX)
               MOVE    IDX             TO  SPA-HKNCOMBI-MAX
           END-PERFORM
           .
       30011-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報ＳＰＡ編集処理
      *****************************************************************
       30012-SPA-HEN-SEC          SECTION.
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
      *
           .
       30012-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期表示処理
      *****************************************************************
       3001-SPA-INIT-SEC          SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT
           INITIALIZE                  SPA-GMN-TBLG
           INITIALIZE                  SPA-NAI-TBLG
      *    診療科初期表示
           IF      SPA-SRYKA           =   SPACE
               MOVE    SPA-GMN-SRYKAL (1)      TO  SPA-GMN-SRYKA
           ELSE
               MOVE    SPA-SRYKA               TO  SPA-GMN-SRYKA
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           PERFORM 30011-SRYKA-TBLHEN-SEC
      *
      *    職員情報をＳＰＡにセット
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-LISTG
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           IF      SPA-DRCD            =   SPACE
               MOVE    SPA-GMN-DRCD-LIST(01) TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    SPA-DRCD(2:4)       TO  SPA-GMN-DRCD
               PERFORM 30012-DRCD-TBLHEN-SEC
           END-IF
      *
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    SPA-GMN-THKNCOMBINUM(01)
                                           TO  SPA-GMN-HKNCOMBI
           ELSE
               MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBI
           END-IF
           PERFORM 30013-HKNCOMBI-TBLHEN-SEC
      *
      *    処理・帳票名
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                  SYS-9701-REC
           MOVE    SPA-NAI-KANRICD     TO  SYS-9701-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-9701-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-9701-REC
               INITIALIZE                  SYS-9701-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL       (IDX            >=  10   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC             TO  SYS-9701-REC
      *
               PERFORM 900-SYS-3103-READ-SEC
      *
               IF      SPA-MW                  =   SPA-GINBEE
                AND    SPA-NAI-CUSTOM-ENABLED  =   SPACE
                AND    SYS-3103-REPOS-NAME NOT =   SPACE
                   CONTINUE
               ELSE
                   IF      SYS-9701-EXECUTE-SW     =   "1"
                       PERFORM 30012-SYORINM-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SYORINML(1) TO  SPA-GMN-SYORINM
           MOVE    SPA-NAI-KBNCDL (1)  TO  SPA-NAI-KBNCD
           MOVE    SPA-NAI-PRTNML (1)  TO  SPA-GMN-PRTNM
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       3001-SPA-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    管理コード決定処理
      *****************************************************************
       30011-KANRICD-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYS-9700-REC
           INITIALIZE                      SYS-9700-REC
           MOVE    "9700"              TO  SYS-9700-KANRICD
           MOVE    "1%"                TO  SYS-9700-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-9700-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9700-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9700-HOSPNUM
           MOVE    SYS-9700-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           MOVE    SPACE               TO  SPA-NAI-KANRICD
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9700-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   25
                   IF      SYS-9700-GYOUMUID (IDX)
                                           =   SPA-MOTOPG
                       MOVE    SYS-9700-GYOUMU-KANRICD (IDX)
                                               TO  SPA-NAI-KANRICD
                       MOVE    25              TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           .
       30011-KANRICD-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理・帳票名テーブル編集処理
      *****************************************************************
       30012-SYORINM-HEN-SEC          SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    SYS-9701-SYORINM    TO  SPA-GMN-SYORINML(IDX)
      *
           MOVE    SYS-9701-PRTNM      TO  SPA-NAI-PRTNML  (IDX)
           MOVE    SYS-9701-KBNCD      TO  SPA-NAI-KBNCDL  (IDX)
      *    実行形式
           IF      SYS-9701-EXECUTE-YTPE   =   "2"
               ADD     1                   TO  SPA-TBL-MAX
               ADD     1                   TO  IDY
               MOVE    SPA-TBL-MAX         TO  SPA-GMN-TNUM (IDY)
               MOVE    SYS-9701-SYORINM    TO  SPA-GMN-TSYORINM (IDY)
               MOVE    SYS-9701-PRTNM      TO  SPA-GMN-TPRTNM   (IDY)
      *
               MOVE    SPA-SRYKA           TO  SPA-NAI-TSRYKA   (IDY)
               IF      SPA-DRCD            =   SPACE
                   MOVE    SPACE           TO  SPA-NAI-TDRCD    (IDY)
               ELSE
                   MOVE    SPA-DRCD(2:4)   TO  SPA-NAI-TDRCD    (IDY)
               END-IF
               MOVE    SPA-HKNCOMBINUM     TO  SPA-NAI-THKNCOMBI(IDY)
               MOVE    SYS-9701-KBNCD      TO  SPA-NAI-TKBNCD   (IDY)
               PERFORM 300121-TBL-MEIHEN-SEC
           END-IF
           MOVE    IDX                 TO  SPA-SYORINM-MAX
      *
           .
       30012-SYORINM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル名称編集処理
      *****************************************************************
       300121-TBL-MEIHEN-SEC          SECTION.
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-SRYKA-MAX
               IF      SPA-SRYKA       =   SPA-GMN-SRYKAL (IDX-T)
                   MOVE    SPA-GMN-SRYKAMEIL (IDX-T)
                                           TO  SPA-GMN-TSRYKA(IDY)
                   MOVE    SPA-SRYKA-MAX   TO  IDX-T
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-DRCD-MAX
               IF      SPA-DRCD(2:4)   =   SPA-GMN-DRCDL (IDX-T)
                   MOVE    SPA-GMN-DRCDMEIL (IDX-T)
                                          TO  SPA-GMN-TDRCD (IDY)
                   MOVE    SPA-DRCD-MAX   TO  IDX-T
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-HKNCOMBI-MAX
               IF      SPA-HKNCOMBINUM =   SPA-GMN-THKNCOMBINUM(IDX-T)
                   MOVE    SPA-GMN-THKNCOMBIMEI (IDX-T)
                                           TO  SPA-GMN-THKNCOMBI (IDY)
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDX-T
               END-IF
           END-PERFORM
      *
           .
       300121-TBL-MEIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       30011-SRYKA-TBLHEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           .
       30011-SRYKA-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       30012-DRCD-TBLHEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               IF      SPA-GMN-DRCD    =   SPA-GMN-DRCDL (IDX)
                   MOVE    SPA-GMN-DRCD-LIST (IDX)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    SPA-DRCD-MAX    TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
      *        一覧に無い場合、追加
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
               MOVE    "2"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               MOVE    SPA-GMN-DRCD-LISTG  TO  ORCSDR-DRCD-TBL-G
               MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
               MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-LISTG
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
               IF      ORCSDR-ERRCD    NOT =   ZERO
                   MOVE    "0004"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       30012-DRCD-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       30013-HKNCOMBI-TBLHEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBI-MAX
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM(IDX)
                   MOVE    SPA-GMN-HKNCOMBI-LIST (IDX)
                                              TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-HKNCOMBI-MAX   TO  IDX
                   MOVE    1                  TO  FLG-CHK
               END-IF
           END-PERFORM
           .
       30013-HKNCOMBI-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       3002-XDID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-XDID1CD
      *        実行確認
               WHEN    "0101"
                   IF      SPA-XDID1-FLG        =   "OK"
                       PERFORM 4120-TOROKU-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-XDID1CD
           END-EVALUATE
      *
           .
       3002-XDID1-SET-EXT.
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
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                    PERFORM 402-CLREA-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 403-DELETE-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B08"
                    PERFORM 408-KAKUTEI-SEC
      *        実行
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 412-TOROKU-SEC
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
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4001-SELNUM-CHK-SEC
      *        選択
               WHEN    ANY             ALSO    "MLIST"
                   PERFORM 4003-LIST-SEL-SEC
      *
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-KOUSIN
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号チェック処理
      *****************************************************************
       4001-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    XD01-SELNUM         TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-TBL-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SELNUM      =   SPA-GMN-TNUM (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               IF      SPA-GMN-SELNUM      >   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               PERFORM 40011-GMN-SET-SEC
           END-IF
      *
           .
       4001-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       40011-GMN-SET-SEC          SECTION.
      *
           MOVE    SPA-GMN-TSYORINM (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-SYORINM
           MOVE    SPA-NAI-TKBNCD     (SPA-GMN-SELNUM)
                                       TO  SPA-NAI-KBNCD
           MOVE    SPA-GMN-TPRTNM (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-PRTNM
      *
           MOVE    SPA-NAI-TSRYKA (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-SRYKA
           MOVE    SPA-NAI-TDRCD  (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-DRCD
           MOVE    SPA-NAI-THKNCOMBI (SPA-GMN-SELNUM)
                                       TO  SPA-GMN-HKNCOMBI
      *
           IF      SPA-GMN-SRYKA       =   SPACE
               MOVE    SPACE               TO  SPA-GMN-SRYKA-G
           ELSE
               PERFORM 30011-SRYKA-TBLHEN-SEC
           END-IF
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
           ELSE
               PERFORM 30012-DRCD-TBLHEN-SEC
           END-IF
      *
           IF      SPA-GMN-HKNCOMBI    =   SPACE
               MOVE    SPACE               TO  SPA-GMN-HKNCOMBI-G
           ELSE
               PERFORM 30013-HKNCOMBI-TBLHEN-SEC
           END-IF
      *
           .
       40011-GMN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細リスト選択処理
      *****************************************************************
       4003-LIST-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               IF      XD01-MLIST-SELECT (IDX) =   "T"
                   MOVE    SPA-GMN-TNUM  (IDX)     TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-GMN-SELNUM)
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM
               MOVE    SPA-GMN-SELNUM      TO  XD01-SELNUM
               PERFORM 4001-SELNUM-CHK-SEC
           END-IF
      *
           .
       4003-LIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
      *    入力がプリンタ名の時、確定へ
           IF      SPA-ERRCD           =   SPACE
               IF      WRK-MCP-WIDGET      =   "PRTNM"
                   PERFORM 4081-KAKUTEI-SYORI-SEC
               END-IF
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    XD01-SYORINM        TO  SPA-GMN-SYORINM
           MOVE    XD01-SRYKA          TO  SPA-GMN-SRYKA-G
           MOVE    XD01-DRCD           TO  SPA-GMN-DRCD-G
           MOVE    XD01-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
           MOVE    XD01-PRTNM          TO  SPA-GMN-PRTNM
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    処理・帳票名
           PERFORM 4101-SYORINM-CHK-SEC
      *    診療科
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-SRYKA-CHK-SEC
           END-IF
      *    ドクター
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-DRCD-CHK-SEC
           END-IF
      *    保険組合せ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4104-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理・帳票名チェック処理
      *****************************************************************
       4101-SYORINM-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-SYORINM-MAX)
                           OR (FLG-OK          =   1     )
               IF      SPA-GMN-SYORINM    =   SPA-GMN-SYORINML(IDX)
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-NAI-KBNCDL(IDX) TO  SPA-NAI-KBNCD
                   IF      WRK-MCP-WIDGET      =   "SYORINM"
                       MOVE    SPA-NAI-PRTNML(IDX) TO  SPA-GMN-PRTNM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           .
       4101-SYORINM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       4102-SRYKA-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-SRYKA-MAX)
                           OR (FLG-OK      =   1     )
               IF      SPA-GMN-SRYKA       =   SPA-GMN-SRYKAL(IDX)
                   MOVE    SPA-GMN-SRYKA-LIST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-GMN-CUR
           ELSE
      *        変更がある時、ドクター変更
               IF      SPA-GMN-SRYKA   NOT =   SPA-MAE-SRYKA
      *            職員情報をＳＰＡにセット
                   INITIALIZE                  ORCSDRSETAREA
                   MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
                   MOVE    "1"                 TO  ORCSDR-KBN
                   MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
                   CALL    "ORCSDRSET"         USING
                                               SPA-AREA
                                               ORCSDRSETAREA
                   MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-LISTG
                   MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
                   MOVE    SPA-GMN-DRCD-LIST(01) TO  SPA-GMN-DRCD-G
               END-IF
               MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           END-IF
           .
       4102-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       4103-DRCD-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE           TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-DRCD-MAX)
                               OR (FLG-OK      =   1     )
                   IF      SPA-GMN-DRCD        =   SPA-GMN-DRCDL(IDX)
                       MOVE    SPA-GMN-DRCD-LIST (IDX)
                                               TO  SPA-GMN-DRCD-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0004"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4103-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       4104-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-HKNCOMBI    =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HKNCOMBI-G
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-HKNCOMBI-MAX)
                               OR (FLG-OK      =   1     )
                   IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                                (IDX)
                       MOVE    SPA-GMN-HKNCOMBI-LIST (IDX)
                                               TO  SPA-GMN-HKNCOMBI-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0005"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4104-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    LNK-COMMON          TO  SPA-KYOUTU
      *
           IF      SPA-MOTOPG          =   SPA-SAKIPG
               MOVE    "XD01"              TO  SPA-MOTOPG
           END-IF
      *
           IF    ( SPA-MOTOPG          =   "I04" )
               MOVE    "XD01"          TO  SPA-MOTOPG
           END-IF
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC       SECTION.
      *
      *    初期表示
           PERFORM 3001-SPA-INIT-SEC
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       402-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    削除処理
      *****************************************************************
       403-DELETE-SEC       SECTION.
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               INITIALIZE                  SPA-GMN-TBL (SPA-GMN-SELNUM)
               INITIALIZE                  SPA-NAI-TBL (SPA-GMN-SELNUM)
               PERFORM VARYING    IDX  FROM    1  BY  1
                       UNTIL      IDX  >   10
                   IF     (SPA-GMN-TSYORINM (IDX)  =   SPACE ) AND
                          (IDX             <   10  )
                       MOVE    SPA-GMN-TBL (IDX + 1)
                                               TO  SPA-GMN-TBL (IDX)
                       MOVE    SPA-NAI-TBL (IDX + 1)
                                               TO  SPA-NAI-TBL (IDX)
                       INITIALIZE              SPA-GMN-TBL (IDX + 1)
                       INITIALIZE              SPA-NAI-TBL (IDX + 1)
                   END-IF
               END-PERFORM
               MOVE    ZERO            TO  SPA-TBL-MAX
               PERFORM VARYING    IDX  FROM    1  BY  1
                       UNTIL      IDX  >   10
                   IF      SPA-GMN-TSYORINM (IDX)  NOT =   SPACE
                       ADD     1               TO  SPA-TBL-MAX
                       MOVE    SPA-TBL-MAX     TO  SPA-GMN-TNUM
                                                        (IDX)
                   END-IF
               END-PERFORM
      *        クリア
               PERFORM 4031-INPUT-CLREA-SEC
           END-IF
      *
           .
       403-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力行クリア処理
      *****************************************************************
       4031-INPUT-CLREA-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-INPUT
      *    診療科初期表示
           IF      SPA-SRYKA           =   SPACE
               MOVE    SPA-GMN-SRYKAL (1)      TO  SPA-GMN-SRYKA
           ELSE
               MOVE    SPA-SRYKA               TO  SPA-GMN-SRYKA
           END-IF
           PERFORM 30011-SRYKA-TBLHEN-SEC
      *    職員情報をＳＰＡにセット
           IF      SPA-GMN-SRYKA   NOT =   SPA-MAE-SRYKA
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-LISTG
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           END-IF
           IF      SPA-DRCD            =   SPACE
               MOVE    SPA-GMN-DRCD-LIST(01) TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    SPA-DRCD (2:4)      TO  SPA-GMN-DRCD
               PERFORM 30012-DRCD-TBLHEN-SEC
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
      *
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    SPA-GMN-THKNCOMBINUM(01)
                                           TO  SPA-GMN-HKNCOMBI
           ELSE
               MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBI
           END-IF
           PERFORM 30013-HKNCOMBI-TBLHEN-SEC
      *
           MOVE    SPA-GMN-SYORINML(1) TO  SPA-GMN-SYORINM
           MOVE    SPA-NAI-KBNCDL (1)  TO  SPA-NAI-KBNCD
           MOVE    SPA-NAI-PRTNML (1)  TO  SPA-GMN-PRTNM
      *
           MOVE    2                   TO  SPA-GMN-CUR
      *
           .
       4031-INPUT-CLREA-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       408-KAKUTEI-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4081-KAKUTEI-SYORI-SEC
           END-IF
           .
       408-KAKUTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4081-KAKUTEI-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO
      *        追加
               IF      SPA-TBL-MAX     >=  10
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
                   ADD     1                   TO  SPA-TBL-MAX
                   MOVE    SPA-TBL-MAX         TO  IDX
               END-IF
           ELSE
               MOVE    SPA-GMN-SELNUM      TO  IDX
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    IDX             TO  SPA-GMN-TNUM (IDX)
      *
               MOVE    SPA-GMN-SYORINM     TO  SPA-GMN-TSYORINM(IDX)
               MOVE    SPA-GMN-SRYKA       TO  SPA-NAI-TSRYKA  (IDX)
               MOVE    SPA-GMN-SRYKAMEI    TO  SPA-GMN-TSRYKA  (IDX)
               MOVE    SPA-GMN-DRCD        TO  SPA-NAI-TDRCD   (IDX)
               MOVE    SPA-GMN-DRCDMEI     TO  SPA-GMN-TDRCD   (IDX)
               MOVE    SPA-GMN-HKNCOMBI    TO  SPA-NAI-THKNCOMBI(IDX)
               MOVE    SPA-GMN-HKNCOMBIMEI
                                           TO  SPA-GMN-THKNCOMBI(IDX)
               MOVE    SPA-GMN-PRTNM       TO  SPA-GMN-TPRTNM  (IDX)
               MOVE    SPA-NAI-KBNCD       TO  SPA-NAI-TKBNCD  (IDX)
      *        クリア
               PERFORM 4031-INPUT-CLREA-SEC
           END-IF
           .
       4081-KAKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       412-TOROKU-SEC             SECTION.
      *
           IF      SPA-GMN-SELNUM      >   ZERO
               MOVE    "0012"          TO  SPA-ERRCD
           ELSE
               IF      SPA-TBL-MAX     =   ZERO
                   MOVE    "0013"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"          TO  SPA-XDID1CD
           END-IF
      *
           .
       412-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    シェル実行処理
      *****************************************************************
       4120-TOROKU-SYORI-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TBL-MAX
               PERFORM 4120-SHELL-SYORI-SEC
           END-PERFORM
           .
      *
       420-TOROKU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    シェル実行処理
      *****************************************************************
       4120-SHELL-SYORI-SEC             SECTION.
      *
      *    処理・帳票名
           MOVE    SPACE               TO  SYS-9701-REC
           INITIALIZE                  SYS-9701-REC
           MOVE    SPA-NAI-KANRICD     TO  SYS-9701-KANRICD
           STRING  SPA-NAI-TKBNCD (IDX)
                                       DELIMITED  BY  SPACE
                   "%"                 DELIMITED  BY  SIZE
                                  INTO     SYS-9701-KBNCD
           END-STRING
           MOVE    SPA-SRYYMD          TO  SYS-9701-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9701-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-9701-HOSPNUM
           MOVE    SYS-9701-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-9701-REC
               INITIALIZE                  SYS-9701-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI       =   ZERO
               MOVE    MCPDATA-REC             TO  SYS-9701-REC
               PERFORM 900-SYS-3103-READ-SEC
               IF      SPA-MW                  =   SPA-GINBEE
                AND    SPA-NAI-CUSTOM-ENABLED  =   SPACE
                AND    SYS-3103-REPOS-NAME NOT =   SPACE
                   CONTINUE
               ELSE
                   IF     (SYS-9701-KBNCD      =   SPA-NAI-TKBNCD (IDX))
                      AND (SYS-9701-EXECUTE-SW =   "1"    )
      *                ユーザプログラム起動処理
                       PERFORM 41201-SHELL-CALL-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4120-SHELL-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       41201-SHELL-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSUSERPGAREA
      *    処理ＰＧ
           MOVE    SPA-MAE-MOTOPG      TO  ORCSUSERPG-GMNPG
      *    処理区分
           MOVE    SPA-SYORI-FLG       TO  ORCSUSERPG-SYORI-FLG
      *    システム日付西暦
           MOVE    SPA-SYSYMD          TO  ORCSUSERPG-SYSYMD
      *    診療日
           MOVE    SPA-SRYYMD          TO  ORCSUSERPG-SRYYMD
      *    患者ＩＤ
           MOVE    SPA-PTID            TO  ORCSUSERPG-PTID
      *    患者番号
           MOVE    SPA-PTNUM           TO  ORCSUSERPG-PTNUM
      *    診療科
           MOVE    SPA-NAI-TSRYKA(IDX) TO  ORCSUSERPG-SRYKA
      *    ドクター
           IF      SPA-NAI-TDRCD (IDX) =   SPACE
               MOVE    SPACE               TO  ORCSUSERPG-DRCD
           ELSE
               MOVE    "1"                 TO  ORCSUSERPG-DRCD(1:1)
               MOVE    SPA-NAI-TDRCD (IDX) TO  ORCSUSERPG-DRCD(2:4)
           END-IF
      *    保険組合せ
           MOVE    SPA-NAI-THKNCOMBI (IDX)
                                       TO  ORCSUSERPG-HKNCOMBI
      *    処理・帳票名
           MOVE    SPA-GMN-TSYORINM (IDX)
                                       TO  ORCSUSERPG-SYORINM
      *    実行
           MOVE    SYS-9701-SHELL      TO  ORCSUSERPG-SHELL
      *    プリンタ名
           MOVE    SPA-GMN-TPRTNM (IDX)
                                       TO  ORCSUSERPG-PRTNM
      *H28.6
      *    伝票番号
           IF      SPA-MAE-MOTOPG      =   "K03"
               MOVE    SPA-NAI-DENPNUM TO  ORCSUSERPG-DENPNUM
           END-IF
      *
      *    リポジトリ名
           IF      SPA-MW                  =   SPA-GINBEE
            AND    SPA-NAI-CUSTOM-ENABLED  =   "1"
               MOVE    SYS-3103-REPOS-NAME
                                       TO  ORCSUSERPG-REPOS-NAME
           END-IF
      *
           CALL    "ORCSUSERPG"        USING
                                       ORCSUSERPGAREA
                                       SPA-AREA
      *
           .
       41201-SHELL-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
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
           .
       800-SEIWA-HEN-EXT.
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
           IF      SPA-XDID1CD       NOT =   SPACE
               PERFORM 520-XD01IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "XD01"               TO  MCP-WINDOW.
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
           MOVE    SPACE               TO  XD01
      *
           INITIALIZE                      XD01
           MOVE    SPA-GMN-PTNUM       TO  XD01-PTNUM
           MOVE    SPA-GMN-NAME        TO  XD01-NAME
           MOVE    SPA-GMN-SEX         TO  XD01-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  XD01-BIRTHDAY
           MOVE    SPA-GMN-SRYYMD      TO  XD01-SRYYMD
           MOVE    SPA-GMN-SYORIMEI    TO  XD01-SYORIMEI
      *
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX     >   SPA-TBL-MAX
               MOVE    SPA-GMN-TNUM (IDX)  TO  XD01-TNUM   (IDX)
               MOVE    SPA-GMN-TSYORINM(IDX)
                                           TO  XD01-TSYORINM (IDX)
               MOVE    SPA-GMN-TSRYKA(IDX) TO  XD01-TSRYKA   (IDX)
               MOVE    SPA-GMN-TDRCD (IDX) TO  XD01-TDRCD   (IDX)
               MOVE    SPA-GMN-THKNCOMBI(IDX)
                                           TO  XD01-THKNCOMBI (IDX)
               MOVE    SPA-GMN-TPRTNM(IDX) TO  XD01-TPRTNM   (IDX)
               IF      SPA-GMN-TNUM   (IDX)  =   SPA-GMN-SELNUM
                   MOVE    "T"             TO  XD01-MLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  XD01-MLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-TBL-MAX         TO  XD01-MLIST-COUNT
      *
           MOVE    SPA-GMN-SYORINM     TO  XD01-SYORINM
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-SYORINM-MAX
               MOVE   SPA-GMN-SYORINML(IDX)   TO  XD01-SYORINM-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-SYORINM-MAX     TO  XD01-SYORINM-COUNT
      *
           MOVE    SPA-GMN-SRYKA-G     TO  XD01-SRYKA
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST(IDX)   TO  XD01-SRYKA-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  XD01-SRYKA-COUNT
      *
           MOVE    SPA-GMN-DRCD-G      TO  XD01-DRCD
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCD-LIST(IDX)  TO  XD01-DRCD-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  XD01-DRCD-COUNT
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  XD01-HKNCOMBI
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-HKNCOMBI-MAX
               MOVE    SPA-GMN-HKNCOMBI-LIST(IDX)
                                           TO  XD01-HKNCOMBI-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-HKNCOMBI-MAX    TO  XD01-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-PRTNM       TO  XD01-PRTNM
           MOVE    SPA-GMN-SELNUM      TO  XD01-SELNUM
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SYORINM"         TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SRYKA"           TO  MCP-WIDGET
               WHEN    04
                   MOVE    "DRCD"            TO  MCP-WIDGET
               WHEN    05
                   MOVE    "HKNCOMBI"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "PRTNM"           TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B08"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SYORINM"
                   MOVE    3               TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "DRCD"
                   MOVE    5               TO  SPA-GMN-CUR
               WHEN    "HKNCOMBI"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "PRTNM"
                   MOVE    99              TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "選択番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "処理・帳票名入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "診療科入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "ドクター入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "保険組合せ入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "処理数は１０件までです。"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "選択中です。追加登録を押下して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "実行すべき処理がありません。"
                                       TO  SPA-ERRMSG
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  XDERR
           MOVE    SPA-ERRCD           TO  XDERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  XDERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "XD01"               TO  SPA-MOTOPG
           MOVE    "XDERR"            TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XDERR"            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-XD01IDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  XDID1
           INITIALIZE                      XDID1
           EVALUATE    SPA-XDID1CD
               WHEN    "0101"
                   MOVE    "処理を実行します。よろしいですか？"
                                       TO  XDID1-ID1MSG
               WHEN    OTHER
                   MOVE    SPA-XDID1CD
                                       TO  XDID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-XDID1CD         TO  XDID1-ID1CODE
      *
           MOVE    SPACE               TO  SPA-XDID1-FLG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "XD01"              TO  SPA-MOTOPG
           MOVE    "XDID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "XDID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-XD01IDSET-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYS-3103-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           MOVE    "3103"              TO  SYS-3103-KANRICD
           MOVE    SYS-9701-KANRICD    TO  SYS-3103-KBNCD
           MOVE    "0"                 TO  SYS-3103-KBNCD(5:1)
           MOVE    SYS-9701-KBNCD(1:2) TO  SYS-3103-KBNCD(6:2)
           MOVE    SPA-SYSYMD          TO  SYS-3103-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-3103-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-3103-HOSPNUM
           MOVE    SYS-3103-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           MOVE    SPACE               TO  SYS-3103-REC
           INITIALIZE                      SYS-3103-REC
           IF  MCP-RC              =   ZERO
               PERFORM 900-SYSKANRI-READ-SEC
               IF    ( FLG-SYSKANRI    =   ZERO )
                   MOVE    MCPDATA-REC TO  SYS-3103-REC
               END-IF
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYS-3103-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"     USING
                                       MCPAREA
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
           CALL    "ORCDBMAIN"         USING
                                           MCPAREA
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
           CALL   "ORCDBMAIN"          USING
                                           MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
