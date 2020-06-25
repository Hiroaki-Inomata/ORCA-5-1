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
       PROGRAM-ID.                 ORCGK10.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 中途終了一覧　（Ｋ１０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/05/25    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.02    NACL-多々納  04/10/12  ワーク診療行為レイアウト変更
      *  01.00.03    NACL-多々納  05/11/16  患者番号表示
      *                                     MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.07.00    NACL-多々納  11/08/08  明細２００件対応
      *  04.07.00    NACL-多々納  11/10/XX  剤番＝１なし対応
      *                                     同日再入院対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面用共通スパ領域
           COPY    "K02SCR-SPA".
      *
      *    画面スパ領域
      *01  SPA-K10.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    保険組合せ
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-SELNUM          PIC 9(03).
      *
           03  WRK-KIDMSG          PIC X(80).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-ZZ              PIC ZZZ.
           03  WRK-ORDER           PIC 9(01).
      *
           03  WRK-ERRMSG          PIC X(40).
      *
      *    コラムリスト位置
           03  WRK-K10-ROW             PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
           03  WRK-KEY-AREA.
               05  WRK-KEY-PTID        PIC 9(10).
               05  WRK-KEY-SRYYMD      PIC X(08).
               05  WRK-KEY-SRYKA       PIC X(02).
               05  WRK-KEY-HKNCOMBI    PIC X(04).
               05  WRK-KEY-DOUJITSUKBN    PIC X(01).
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
           COPY    "CPSK1010.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    保険組合せマスタ−
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
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
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
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
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
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
               MOVE   "K10"                TO  MCP-WINDOW
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
                   INITIALIZE                  SPA-K10GMN-AREA
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
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
      *                中途終了分削除
                       PERFORM                 30011-WKSRYCD-DEL-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPACE           TO  SPA-K10-GMN-SELNUM
                       MOVE    ZERO            TO  SPA-K10-SELNUM
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "0102"
                   IF      SPA-KID1-FLG        =   "OK"
      *                確定処理
                       PERFORM 4510-KAKUTEI-SYORI-SEC
                       IF      SPA-ERRCD           =   SPACE
                            MOVE    ZERO            TO  SPA-K10-SYORI
                            PERFORM 4502-BACK-SYORI-SEC
                       END-IF
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
           MOVE    ZERO                TO  SPA-K10-NEXT
           INITIALIZE                  SPA-K10-LIST
           MOVE    ZERO                TO  SPA-K10-MAX
      *
      *    ワーク診療行為を検索する
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-GMN-PTID        TO  WKSRY-PTID
           IF      SPA-GMN-PTID        =   ZERO
               IF      SPA-WKSRYHYOFLG     =   ZERO
                   MOVE    "key8"              TO  WRK-PATH
               ELSE
                   MOVE    "key9"              TO  WRK-PATH
               END-IF
           ELSE
               IF      SPA-WKSRYHYOFLG         =   ZERO
                   MOVE    "key4"              TO  WRK-PATH
               ELSE
                   MOVE    "key10"             TO  WRK-PATH
               END-IF
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 940-WKSINKOUI-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           INITIALIZE                  WRK-KEY-AREA
           PERFORM
               UNTIL   (FLG-WKSRYACT   =   1 )  OR
                       (IDX            >   200 )
               IF      (WKSRY-PTID         =   WRK-KEY-PTID  )  AND
                       (WKSRY-SRYKA        =   WRK-KEY-SRYKA )  AND
                       (WKSRY-SRYYMD       =   WRK-KEY-SRYYMD)  AND
                       (WKSRY-HKNCOMBI     =   WRK-KEY-HKNCOMBI)  AND
                       (WKSRY-DOUJITSUKBN  =   WRK-KEY-DOUJITSUKBN)
                   MOVE    1                   TO  FLG-CHK
               ELSE
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
               IF     ((WKSRY-ZAINUM       =   1   )  OR
                       (FLG-CHK            =   ZERO ))  AND
                      ( WKSRY-RENNUM       =   1   )
                   ADD     1                   TO  IDX
                   IF      IDX                 >   200
                       MOVE    1                   TO  SPA-K10-NEXT
                   ELSE
                       PERFORM 3101-TBL-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    WKSRY-PTID          TO  WRK-KEY-PTID
               MOVE    WKSRY-SRYKA         TO  WRK-KEY-SRYKA
               MOVE    WKSRY-SRYYMD        TO  WRK-KEY-SRYYMD
               MOVE    WKSRY-HKNCOMBI      TO  WRK-KEY-HKNCOMBI
               MOVE    WKSRY-DOUJITSUKBN   TO  WRK-KEY-DOUJITSUKBN
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 940-WKSINKOUI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集用処理
      *****************************************************************
       3101-TBL-HENSYU-SEC             SECTION.
      *
           MOVE    IDX                 TO  SPA-K10-MAX
           MOVE    IDX                 TO  SPA-K10-SEQ    (IDX)
           MOVE    WKSRY-PTID          TO  SPA-K10-PTID   (IDX)
           MOVE    WKSRY-SRYKA         TO  SPA-K10-SRYKA  (IDX)
           MOVE    WKSRY-SRYYMD        TO  SPA-K10-SRYYMD  (IDX)
           MOVE    WKSRY-HKNCOMBI      TO  SPA-K10-HKNCOMBI(IDX)
      *
      *    患者マスター読み込み
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    WKSRY-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-NAME          TO  SPA-K10-GMN-NAME  (IDX)
      *    患者番号
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    WKSRY-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                               SPA-AREA
           MOVE    SPTID-PTNUM         TO  SPA-K10-PTNUM  (IDX)
      *
      *    診療科
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1  BY  1
                   UNTIL       IDY     >   SPA-SRYKA-MAX
               IF      WKSRY-SRYKA         =   SPA-GMN-SRYKAL (IDY)
                   MOVE    SPA-GMN-SRYKAMEIL (IDY)
                                           TO  SPA-K10-GMN-SRYKA
                                                               (IDX)
                   MOVE    SPA-SRYKA-MAX   TO  IDY
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO )
             AND  (WKSRY-SRYKA     NOT =   "99" )
               MOVE    WKSRY-SRYKA         TO  SPA-K10-GMN-SRYKA
                                                               (IDX)
           END-IF
      *
      *    診察日
           MOVE    WKSRY-SRYYMD        TO  SPA-K10-SRYYMD(IDX)
           MOVE    WKSRY-SRYYMD        TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K10-GMN-SRYYMD (IDX)
      *
      *    展開
           IF      WKSRY-MOD-FLG       =   ZERO
               MOVE    SPACE           TO  SPA-K10-SYORIKBN (IDX)
           ELSE
               MOVE    "◎"            TO  SPA-K10-SYORIKBN (IDX)
           END-IF
      *    ドクター
           IF     (WKSRY-DRCD          =   ZERO  OR  SPACE )
      *        入院は表示しない
             OR   (WKSRY-NYUGAIKBN     =   "1"             )
               CONTINUE
           ELSE
               PERFORM 31012-DRCD-NAME-SEC
           END-IF
      *ver.4.7
           MOVE    WKSRY-DOUJITSUKBN   TO  SPA-K10-DOUKBN (IDX)
      *
      *    保険組合せ
           IF      WKSRY-HKNCOMBI      =   "9999"
               MOVE    "包括分入力"    TO  SPA-K10-GMN-HKNCOMBI (IDX)
               GO      TO      3101-TBL-HENSYU-EXT
           END-IF
      *
      *    保険組合せ
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    WKSRY-PTID          TO  COMB-PTID
           MOVE    WKSRY-HKNCOMBI      TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-HKNCOMBI        =   1
               MOVE    WKSRY-HKNCOMBI     TO  SPA-K10-GMN-HKNCOMBI
                                                              (IDX)
           ELSE
               IF      COMB-HKNNUM         =   "971"
                                           OR  "973"
                   MOVE    "1"             TO  SPA-K10-HKNKBN (IDX)
               END-IF
               IF      COMB-HKNNUM         =   "975"
                   MOVE    "2"             TO  SPA-K10-HKNKBN (IDX)
               END-IF
               PERFORM 31011-HKNCOMBI-HEN-SEC
               MOVE    WRK-HKNCOMBI        TO  SPA-K10-GMN-HKNCOMBI
                                                          (IDX)
           END-IF
      *
           .
       3101-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       31011-HKNCOMBI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
      *
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   " "                     DELIMITED  BY  SIZE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
           END-STRING
      *    主保険がない時
           IF      COMB-SYU-TANSEIDONAME   =   SPACE
               MOVE    SPACE               TO  WRK-HKNCOMBI
               STRING  COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                       " "                     DELIMITED  BY  SIZE
                       COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBI
               END-STRING
           END-IF
      *
           .
       31011-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター名称編集処理
      *****************************************************************
       31012-DRCD-NAME-SEC              SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WKSRY-DRCD          TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  SPA-K10-GMN-DRCD-NAME
                                                               (IDX)
           ELSE
               MOVE    WKSRY-DRCD          TO  SPA-K10-GMN-DRCD-NAME
                                                               (IDX)
           END-IF
           .
       31012-DRCD-NAME-EXT.
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
      *        削除
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 440-DELETE-SEC
      *        追加
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 460-TUIKA-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 450-TOROKU-SEC
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
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 410-SELNUM-CHK-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "LIST"
                   PERFORM 420-LIST-CHK-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択番号入力チェック処理
      *****************************************************************
       410-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-K10GMN-CUR
      *
           MOVE    K10-SELNUM          TO  SPA-K10-GMN-SELNUM
           IF      K10-SELNUM          =   SPACE
               MOVE    ZERO                TO  SPA-K10-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K10-SELNUM          TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0001"              TO  SPA-ERRCD
                   GO      TO      410-SELNUM-CHK-EXT
               ELSE
                   MOVE    SNUM-OUTNUM       TO  SPA-K10-SELNUM
               END-IF
           END-IF
           MOVE    SPA-K10-SELNUM      TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-K10-GMN-SELNUM
      *
           IF      SPA-K10-SELNUM      <   01   OR
                                       >   SPA-K10-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    2                   TO  SPA-K10GMN-CUR
      *        追加へ
               IF     (SPA-K10-SRYKA (SPA-K10-SELNUM)  =   "99" )
                   MOVE    3               TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *
           .
       410-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       420-LIST-CHK-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-K10-MAX
              IF      K10-SELECT (IDX)        =  "T"
                  IF      IDX                 =   SPA-K10-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-K10-SEQ(IDX)    TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    WRK-SELNUM          TO  SPA-K10-SELNUM
           END-IF
      *
           IF      SPA-K10-SELNUM      <   01   OR
                                       >   SPA-K10-MAX
               CONTINUE
           ELSE
               MOVE    SPA-K10-SELNUM      TO  WRK-ZZ
               MOVE    WRK-ZZ              TO  SPA-K10-GMN-SELNUM
               MOVE    2                   TO  SPA-K10GMN-CUR
      *        追加へ
               IF     (SPA-K10-SRYKA (SPA-K10-SELNUM)  =   "99" )
                   MOVE    3                   TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *
           .
       420-LIST-CHK-SEC-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           MOVE    ZERO                TO  SPA-K10-SELNUM
           MOVE    ZERO                TO  SPA-K10-SYORI
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K10"               TO  SPA-MOTOPG
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
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
           IF      SPA-K10-SELNUM      >   SPA-K10-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-K10-SELNUM      >   ZERO
                   PERFORM 4512-KAKUTEI-CHK-SEC
                   IF     (SPA-ERRCD           =   SPACE )  AND
                          (SPA-KIDCD           =   SPACE )
                       PERFORM 4510-KAKUTEI-SYORI-SEC
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               MOVE    ZERO            TO  SPA-K10-SYORI
               PERFORM 4502-BACK-SYORI-SEC
           END-IF
      *
           .
       450-TOROKU-EXT.
           EXIT.
      *****************************************************************
      *    元の画面へ
      *****************************************************************
       4502-BACK-SYORI-SEC             SECTION.
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K10"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    確定チェック処理
      *****************************************************************
       4512-KAKUTEI-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-K10-MAX )  OR
                             (FLG-OK   =   1       )
               IF      SPA-K10-SYORIKBN (IDX)  NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *    診療科判断
           IF     (SPA-K10-SRYKA (SPA-K10-SELNUM)  =   "99" )
               IF      SPA-GMN-PTID        =   ZERO
                   MOVE    "0011"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-K10GMN-CUR
               ELSE
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    3                   TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *
      *H28.10
      *    排他中判定
           IF     (SPA-GMN-PTID    NOT =   ZERO )
              AND (SPA-ERRCD           =   SPACE)
      *        排他中
               IF      SPA-LOCK        NOT  =  ZERO
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-PTID        NOT =   ZERO
                   IF      FLG-OK              =   1
                       MOVE    "0102"          TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
           .
       4512-KAKUTEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4510-KAKUTEI-SYORI-SEC             SECTION.
      *
           MOVE    SPA-K10-SELNUM      TO  IDX
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4510-KAKUTEI-SYORI-EXT
           END-IF
      *
           MOVE    SPA-K10-PTID (IDX)  TO  SPA-PTID
           MOVE    SPA-K10-SRYKA(IDX)  TO  SPA-SRYKA
           MOVE    SPA-K10-SRYYMD(IDX) TO  SPA-SRYYMD
           MOVE    SPA-K10-GMN-SRYYMD(IDX)
                                       TO  SPA-SRYYMDW
           MOVE    SPA-K10-HKNCOMBI(IDX)
                                       TO  SPA-HKNCOMBINUM
           MOVE    SPA-K10-PTNUM  (IDX)
                                       TO  SPA-PTNUM
      *ver.4.7
           MOVE    SPA-K10-DOUKBN (IDX)
                                       TO  SPA-WKSRY-DOUKBN
      *
           .
       4510-KAKUTEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    追加　処理
      *****************************************************************
       460-TUIKA-SEC             SECTION.
      *
           IF      SPA-K10-SELNUM      >   SPA-K10-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-K10-SELNUM      >   ZERO
                   PERFORM 4610-TUIKA-SYORI-SEC
               END-IF
           END-IF
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-K10GMN-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  SPA-K10-SYORI
               PERFORM 4502-BACK-SYORI-SEC
           END-IF
           .
       460-TUIKA-EXT.
           EXIT.
      *****************************************************************
      *    追加チェック処理
      *****************************************************************
       4610-TUIKA-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-K10GMN-CUR
           ELSE
               IF      SPA-K10-SRYYMD(SPA-K10-SELNUM)
                                       NOT =   SPA-SRYYMD
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *H28.10
      *    排他中判定
           IF     (SPA-GMN-PTID    NOT =   ZERO )
              AND (SPA-ERRCD           =   SPACE)
      *        排他中
               IF      SPA-LOCK        NOT  =  ZERO
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-K10GMN-CUR
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4610-TUIKA-SYORI-EXT
           END-IF
      *
           IF     (SPA-HKNCOMBINUM     =   "9999")  OR
                  (SPA-HKNKBN          =   1     )
              OR  (SPA-HKNKBN          =   2     )
               IF      SPA-K10-HKNCOMBI(SPA-K10-SELNUM)
                                           =   SPA-HKNCOMBINUM
                   CONTINUE
               ELSE
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-K10GMN-CUR
               END-IF
           ELSE
      ****     IF     (SPA-K10-HKNCOMBI(SPA-K10-SELNUM)
      **                                   =   "9999"  ) OR
               IF     (SPA-K10-HKNKBN (SPA-K10-SELNUM)
                                           =   "1"
                                           OR  "2"     )
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-K10GMN-CUR
               END-IF
           END-IF
      *    ６５歳以上で老人公費のある時
           IF     (SPA-NAI-ROUJIN2     =   1 )  AND
                  (SPA-ERRCD           =   SPACE )
             IF    (SPA-K10-HKNCOMBI(SPA-K10-SELNUM)
                                         =   "9999"  OR
                                             "0000"  )
               CONTINUE
             ELSE
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               MOVE    SPA-K10-HKNCOMBI(SPA-K10-SELNUM)
                                           TO  AGECHK-HKNCOMBINUM
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      SPA-NAI-ROUJIN  NOT =   AGECHK-ROUJIN
                   MOVE    "0007"          TO  SPA-ERRCD
                   MOVE    2               TO  SPA-K10GMN-CUR
               END-IF
             END-IF
           END-IF
      *    選択中でないこと
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K10-SYORIKBN(SPA-K10-SELNUM)   NOT =   SPACE)
               MOVE    "0004"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-K10GMN-CUR
           END-IF
      *
           .
       4610-TUIKA-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    削除　処理
      *****************************************************************
       440-DELETE-SEC             SECTION.
      *
           IF      SPA-K10-SELNUM      >   SPA-K10-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               IF      SPA-K10-SELNUM      >   ZERO
      *            選択中でないこと
                   IF     (SPA-K10-SYORIKBN(SPA-K10-SELNUM)
                                           NOT =   SPACE ) AND
                          (SPA-PTID            =   ZERO  )
                       MOVE    "0008"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "0101"          TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       440-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    中途終了分削除処理
      *****************************************************************
        30011-WKSRYCD-DEL-SEC             SECTION.
      *
           MOVE    SPA-K10-SELNUM      TO  IDX
      *    ワーク診療行為マスタ検索
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-K10-PTID (IDX)  TO  WKSRY-PTID
           MOVE    SPA-K10-SRYKA(IDX)  TO  WKSRY-SRYKA
           MOVE    SPA-K10-SRYYMD(IDX) TO  WKSRY-SRYYMD
           MOVE    SPA-K10-HKNCOMBI(IDX)
                                       TO  WKSRY-HKNCOMBI
      *ver.4.7
           MOVE    SPA-K10-DOUKBN (IDX)
                                       TO  WKSRY-DOUJITSUKBN
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSINKOUI-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSINKOUI-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    テーブル再編集
           PERFORM 310-SPA-SET-SEC
           MOVE    ZERO                TO  FLG-GMN-INIT
      ***  IF      SPA-K10-NEXT        =   1
      *        テーブル再編集
      *        PERFORM 310-SPA-SET-SEC
      *    ELSE
      *        行削除
      *        MOVE    SPA-K10-SELNUM      TO  IDY
      *        PERFORM VARYING    IDX  FROM    SPA-K10-SELNUM   BY  1
      *                UNTIL      IDX      >   SPA-K10-MAX
      *            IF      IDX             =   SPA-K10-SELNUM
      *                INITIALIZE                  SPA-K10-LIST-OCC(IDX)
      *            ELSE
      *                MOVE    IDY         TO  SPA-K10-SEQ (IDX)
      *                MOVE    SPA-K10-LIST-OCC(IDX)
      *                                    TO  SPA-K10-LIST-OCC(IDX - 1)
      *                INITIALIZE                  SPA-K10-LIST-OCC(IDX)
      *                ADD     1           TO  IDY
      *            END-IF
      *        END-PERFORM
      *        COMPUTE SPA-K10-MAX         =   SPA-K10-MAX   -   1
      **** END-IF
           MOVE    SPACE               TO  SPA-K10-GMN-SELNUM
           MOVE    ZERO                TO  SPA-K10-SELNUM
           MOVE    1                   TO  SPA-K10GMN-CUR
      *
           .
        30011-WKSRYCD-DEL-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
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
       520-SEIWA-HEN-EXT.
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
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K10"               TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-K10-MAX         =   ZERO)
               MOVE    ZERO                TO  WRK-K10-ROW
           ELSE
               MOVE    K10-ROW             TO  WRK-K10-ROW
           END-IF
      *
           MOVE    SPACE               TO  K10
      *
      *    コラムリストの位置指定
           INITIALIZE                      K10
           MOVE    ZERO                TO  K10-ROW
           MOVE    ZERO                TO  K10-ROWATTR
           IF      WRK-K10-ROW     NOT =   ZERO
               MOVE    WRK-K10-ROW         TO  K10-ROW
           END-IF
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K10-MAX
               MOVE    SPA-K10-SEQ    (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K10-NO  (IDX)
               MOVE    SPA-K10-PTNUM      (IDX)
                                               TO  K10-PTNUM (IDX)
               MOVE    SPA-K10-GMN-NAME   (IDX)
                                               TO  K10-NAME  (IDX)
               MOVE    SPA-K10-GMN-SRYKA  (IDX)
                                               TO  K10-SRYKA (IDX)
               MOVE    SPA-K10-GMN-SRYYMD (IDX)
                                               TO  K10-SRYYMD(IDX)
      *TEST
               IF      SPA-K10-DOUKBN (IDX)    =   "1"
                   MOVE    "(2)"               TO  K10-SRYYMD(IDX)
                                                           (10:3)
               END-IF
      *TEST
               MOVE    SPA-K10-GMN-HKNCOMBI (IDX)
                                               TO  K10-HKNCOMBI(IDX)
               MOVE    SPA-K10-SYORIKBN (IDX)
                                               TO  K10-SYORIKBN(IDX)
               MOVE    SPA-K10-GMN-DRCD-NAME (IDX)
                                               TO  K10-DRCD-NAME(IDX)
               IF      SPA-K10-SEQ(IDX)    =   SPA-K10-SELNUM
                   MOVE    "T"             TO  K10-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K10-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K10-MAX         TO  K10-COUNT
      *
           MOVE    SPA-K10-GMN-SELNUM  TO  K10-SELNUM
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
           EVALUATE    SPA-K10GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "B12"             TO  MCP-WIDGET
               WHEN    03
                   MOVE    "B10"             TO  MCP-WIDGET
               WHEN    04
                   MOVE    "B01"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
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
                   MOVE    "番号選択入力エラー"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "患者が選択されていません。"
                                               TO  SPA-ERRMSG
                   MOVE    "追加はできません。"
                                               TO  SPA-ERRMSG(27:)
               WHEN    "0003"
                   MOVE    "登録中の診療日と違います。"
                                               TO  SPA-ERRMSG
                   MOVE    "追加はできません。"
                                               TO  SPA-ERRMSG(27:)
               WHEN    "0004"
                   MOVE    "既に展開中です。"
                                               TO  SPA-ERRMSG
                   MOVE    "追加はできません。"
                                               TO  SPA-ERRMSG(15:)
               WHEN    "0008"
                   MOVE    "現在、他で展開中です。"
                                               TO  SPA-ERRMSG
                   MOVE    "削除はできません。"
                                               TO  SPA-ERRMSG(23:)
               WHEN    "0006"
                   MOVE    "労災・自賠責、包括分入力の"
                                               TO  SPA-ERRMSG
                   MOVE    "複数保険入力はできません。"
                                               TO  SPA-ERRMSG(27:)
               WHEN    "0007"
                   MOVE    "主保険と老人・一般の判定が違います。"
                                               TO  SPA-ERRMSG
                   MOVE    "複数保険入力はできません。"
                                               TO  SPA-ERRMSG(37:)
               WHEN    "0005"
                   MOVE    "入院です。追加はできません。"
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   STRING  "診療科がありません。" 
                           "追加をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "診療科がありません。" 
                           "患者を選択後、追加をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9998"
                   STRING  "他端末で使用中です。" 
                                               DELIMITED  BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
           END-EVALUATE
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K10"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
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
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE
                   "対象のデータを削除します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0102"
                   MOVE    "既に診療内容が展開中です。"
                                       TO  WRK-KIDMSG
                   MOVE
                   "現在の内容はクリアされます。よろしいですか？"
                                       TO  WRK-KIDMSG(29:)
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K10"               TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
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
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
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
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
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
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSINKOUI-READ-SEC          SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSINKOUI-READ-EXT.
           EXIT.
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
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
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
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-K10-PTID (IDX)  TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
