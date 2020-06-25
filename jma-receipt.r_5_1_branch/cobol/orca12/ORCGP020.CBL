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
       PROGRAM-ID.             ORCGP020.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 受付情報（Ｐ０２０）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/10  受付完了チェック追加
      *  01.00.02    MCC-太田     01/05/10  受付完了時初期化追加
      *  01.00.03    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  02.00.00    MCC-多々納   02/07/04  インタフェース変更
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  02/11/26  診療科・ドクター件数修正
      *  02.00.03    NACL-多々納  05/11/11  MONFUNC 対応 他
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/06/11  氏名変更の予約・受付対応
      *  04.08.00    NACL-多々納  17/04/20  受付取消の表示対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-HKNMST          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-INIT            PIC 9(01).
           03  FLG-OK              PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-IN-SELNUM       PIC 9(01).
           03  FLG-IN-HKNKUMI      PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX-LINE            PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
           03  WRK-WSYSYMD.
               05  WRK-WSYSYY      PIC 9(04).
               05  WRK-WSYSMM      PIC 9(02).
               05  WRK-WSYSDD      PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  SPA-GMN20-LINE        PIC 9(04).
           03  WRK-RENNUM9         PIC ZZZ9.
           03  WRK-RENNUMZ         PIC ZZZZ.
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-ST              PIC 9(04).
           03  WRK-ED              PIC 9(04).
      *
           03  WRK-EDANUM          PIC 9(04).
           03  WRK-SELNUM          PIC 9(04).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
      *P02SCR-SPAのP020(受付）と同じレイアウトとする
       01  WRK-SORT-AREA.
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO            OCCURS   10.
                   07  WRK-GMN-YBANGOU     PIC 9(04).
                   07  WRK-GMN-TYYKTIME    PIC X(05).
                   07  WRK-GMN-TUKETIME    PIC X(05).
                   07  WRK-GMN-TSRYKA      PIC X(10).
                   07  WRK-GMN-TDRCD       PIC X(24).
                   07  WRK-GMN-TSRYNAI     PIC X(10).
                   07  WRK-GMN-TKARUTE     PIC X(02).
                   07  WRK-GMN-TSYOHOU     PIC X(02).
                   07  WRK-GMN-TKOHBAN     PIC X(04).
                   07  WRK-GMN-THKNCOMBIMEI    PIC X(60).
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO            OCCURS   10.
                   07  WRK-NAI-KEYYYKTIME  PIC 9(04).
                   07  WRK-NAI-UKEID       PIC 9(05).
                   07  WRK-NAI-UKETIME     PIC X(06).
                   07  WRK-NAI-SRYKA       PIC X(02).
                   07  WRK-NAI-DRCD        PIC X(04).
                   07  WRK-NAI-SRYNAI      PIC X(02).
                   07  WRK-NAI-YYKID       PIC 9(02).
                   07  WRK-NAI-YYKTIME     PIC 9(04).
      *H29.4
                   07  WRK-NAI20-DELKBN      PIC 9(01).
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
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
           03  WRK-SPACE           PIC X(02)   VALUE  "　".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療内容情報
           COPY    "CPSK1012.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
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
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    ＤＢ検索
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
           COPY    "ORCA12SCRAREA.INC".
      *    COPY    "P02.INC".
      *    COPY    "P02A.INC".
      *    COPY    "P02D.INC".
      *    COPY    "P02E.INC".
      *    COPY    "P02F.INC".
      *    COPY    "P014.INC".
      *    COPY    "P015.INC".
      *    COPY    "P98.INC".
      *    COPY    "P981.INC".
      *    COPY    "P0201.INC".
      *    COPY    "P982.INC".
      *    COPY    "P100.INC".
      *    COPY    "P02X.INC".
      *    COPY    "P02S.INC".
      *    COPY    "P02Q.INC".
      *    COPY    "P02Q1.INC".
      *    COPY    "P02Y.INC".
      *    COPY    "P02Y1.INC".
      *    COPY    "P02G.INC".
      *    COPY    "P02G2.INC".
      *    COPY    "P02K.INC".
      *    COPY    "P02H.INC".
      *    COPY    "P02T.INC".
      *    COPY    "PERR.INC".
      *    COPY    "PID1.INC".
      *    COPY    "PDMY01.INC".
      *    COPY    "CPAPIMOD.INC".
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
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
      *
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
           IF      SPA-FLG-END             =   ZERO
      ******** PERFORM 500-GMNHEN-SEC
               PERFORM 5001-MAPCUR-SEC
           END-IF
      *
           MOVE    SPA-P02KYOUTU     TO  SPA-WORK
           MOVE    SPA-AREA          TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA    TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面編集
           PERFORM 300-SCREEN-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           IF      SPA-MOTOPG          =   "P0201"
      *        受付未登録より
               PERFORM 3001-UKEPAR-SET-SEC
           ELSE
      *        初期画面編集
               PERFORM 310-SPASET-SEC
           END-IF
      *
           MOVE    1               TO  SPA-GMN20-CUR
      *
           .
       3OO-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    パラメータファイル内容をＳＰＡにセット
           MOVE    ZERO                TO  FLG-INIT
           PERFORM 314-PARAFILE-KENSAKU-SPASET-SEC
      *
      *    予約編集
           PERFORM 3101-YYKSPA-HEN-SEC
      *
           .
       31O-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメータファイル検索  スパ初期編集処理
      *****************************************************************
       314-PARAFILE-KENSAKU-SPASET-SEC              SECTION.
      *
      *    組合せ表示　初期設定処理
      *****PERFORM 3210-COMB-INIT-SET-SEC
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-NAI20-UKEMAX
           INITIALIZE                      SPA-GMN20-TBL
           INITIALIZE                      SPA-NAI20-TBL
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "UKETUKE"           TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  UKETUKE-REC
      *        受付編集
               PERFORM 3101-UKETU-HEN-SEC
               MOVE    PARA-EDANUM         TO  SPA-NAI20-UKEMAX
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       314-PARAFILE-KENSAKU-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約，受付編集処理
      *****************************************************************
       3101-UKETU-HEN-SEC              SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  SPA-GMN20-YBANGOU (IDX)
           MOVE    UKE-UKEID           TO  SPA-NAI20-UKEID (IDX)
           MOVE    UKE-UKETIME         TO  SPA-NAI20-UKETIME (IDX)
           MOVE    UKE-SRYKA           TO  SPA-NAI20-SRYKA   (IDX)
           MOVE    UKE-SRYNAIYO        TO  SPA-NAI20-SRYNAI  (IDX)
           MOVE    UKE-DRCD     (2:4)  TO  SPA-NAI20-DRCD    (IDX)
      *    保険組合せ
           IF      UKE-HKNCOMBI        =   ZERO
              MOVE    SPACE            TO  SPA-GMN20-THKNCOMBI(IDX)
           ELSE
              MOVE    UKE-HKNCOMBI     TO  SPA-GMN20-THKNCOMBI(IDX)
           END-IF
           MOVE    UKE-KEYYYKTIME      TO  SPA-NAI20-KEYYYKTIME(IDX)
           MOVE    UKE-YYKTIME         TO  SPA-NAI20-YYKTIME (IDX)
           MOVE    UKE-YYKID           TO  SPA-NAI20-YYKID   (IDX)
           IF      UKE-UKETIME         =   SPACE  OR  ZERO
               CONTINUE
           ELSE
               MOVE    UKE-UKETIME (1:2)
                                       TO  SPA-GMN20-TUKETIME(IDX)(1:2)
               MOVE    ":"             TO  SPA-GMN20-TUKETIME(IDX)(3:1)
               MOVE    UKE-UKETIME (3:2)
                                       TO  SPA-GMN20-TUKETIME(IDX)(4:2)
           END-IF
           IF      UKE-YYKTIME         =   SPACE   OR  ZERO
               CONTINUE
           ELSE
               MOVE    UKE-YYKTIME (1:2)
                                       TO  SPA-GMN20-TYYKTIME(IDX)(1:2)
               MOVE    ":"             TO  SPA-GMN20-TYYKTIME(IDX)(3:1)
               MOVE    UKE-YYKTIME (3:2)
                                       TO  SPA-GMN20-TYYKTIME(IDX)(4:2)
           END-IF
      *    診療科　内容編集
           IF      SPA-NAI20-SRYKA (IDX)     NOT =   SPACE
               PERFORM 31012-SRYKA-HEN-SEC
           END-IF
      *    担当ドクター編集
           IF  SPA-NAI20-DRCD (IDX)    NOT =   SPACE
               PERFORM 31013-DRCD-HEN-SEC
           END-IF
      *    診療行為内容編集
           IF  SPA-NAI20-SRYNAI (IDX)    NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY               >   10)
                           OR       (SPA-SRYNAI-CD(IDY) =   SPACE)
                   IF  SPA-NAI20-SRYNAI (IDX)   =   SPA-SRYNAI-CD(IDY)
                       MOVE    SPA-SRYNAI-MEI (IDY)
                                             TO  SPA-GMN20-TSRYNAI(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *    保険組合せ編集
           IF  SPA-GMN20-THKNCOMBI(IDX)    NOT =   SPACE
               MOVE    SPA-GMN20-THKNCOMBI(IDX)
                                         TO  SPA-GMN20-THKNCOMBIMEI
                                                                (IDX)
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY    >   SPA-HKNCOMBI-MAX)
                   IF      SPA-GMN20-THKNCOMBI(IDX)
                                       =   SPA-GMN-THKNCOMBINUM(IDY)
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDY)
                                           TO  SPA-GMN20-THKNCOMBIMEI
                                                                (IDX)
                       MOVE    SPA-HKNCOMBI-MAX    TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *H29.4
      *    削除分
           IF      UKE-UPYMD       =   "99999999"
               MOVE    1                TO  SPA-NAI20-DELKBN (IDX)
           END-IF
      *
           .
       3101-UKETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科　内容編集処理
      *****************************************************************
       31012-SRYKA-HEN-SEC             SECTION.
      *
           PERFORM     VARYING   IDY   FROM    1   BY  1
                       UNTIL    (IDY           >   SPA-SRYKA-MAX)
                       OR       (SPA-GMN-SRYKA-LST(IDY) =   SPACE)
               IF      SPA-NAI20-SRYKA (IDX) =   SPA-GMN-SRYKA-LST(IDY)
                                                                  (1:2)
                   MOVE    SPA-GMN-SRYKA-LST(IDY)(4:)
                                             TO  SPA-GMN20-TSRYKA(IDX)
               END-IF
           END-PERFORM
      *
           .
       31012-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター内容編集処理
      *****************************************************************
       31013-DRCD-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-NAI20-SRYKA (IDX)
                                       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN20-DRCDLST-G TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-NAI20-DRCD (IDX)
                                       TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN20-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-OUT-DRCDMEI  TO  SPA-GMN20-TDRCD(IDX)
      *
           .
       31013-DRCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約編集処理
      *****************************************************************
       3101-YYKSPA-HEN-SEC             SECTION.
      *
           MOVE    SPA-NAI20-UKEMAX    TO  IDX
      *
           IF      SPA-GMN-PTID        =   ZERO
               GO      TO      3101-YYKSPA-HEN-EXT
           END-IF
      *
      *    予約マスターより
           MOVE    SPACE               TO  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  YYK-PTID
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-YYK
           END-IF
           PERFORM
                   UNTIL     (FLG-YYK      =   1    )
                   OR        (IDX          >=  10   )
               IF     (YYK-YYKID       NOT =   ZERO       ) AND
                      (YYK-RAIINZUMI       =   SPACE      )
      *            受付済みでないもの
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   IDX
                       IF     (YYK-YYKID       =   SPA-NAI20-YYKID(IDY))
                       AND    (YYK-SRYNAIYO    =   SPA-NAI20-SRYNAI
                                                                  (IDY))
                       AND    (YYK-DRCD (2:4)  =   SPA-NAI20-DRCD (IDY))
                       AND    (YYK-SRYKA       =   SPA-NAI20-SRYKA(IDY))
                       AND    (YYK-KEYYYKTIME  =   SPA-NAI20-KEYYYKTIME
                                                                  (IDY))
                           MOVE    1               TO  FLG-ARI
                           MOVE    IDX             TO  IDY
                       END-IF
                   END-PERFORM
                   IF      FLG-ARI         =   ZERO
                        PERFORM 31011-YYKUKE-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YYK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-NAI20-UKEMAX
           .
       3101-YYKSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約受付一覧表示編集理
      *****************************************************************
       31011-YYKUKE-HENSYU-SEC             SECTION.
      *
           ADD     1               TO  IDX
           MOVE    IDX             TO  SPA-GMN20-YBANGOU (IDX)
           MOVE    YYK-SRYNAIYO    TO  SPA-NAI20-SRYNAI  (IDX)
           MOVE    YYK-DRCD (2:4)  TO  SPA-NAI20-DRCD    (IDX)
           MOVE    YYK-SRYKA       TO  SPA-NAI20-SRYKA   (IDX)
           MOVE    YYK-KEYYYKTIME  TO  SPA-NAI20-KEYYYKTIME
                                                         (IDX)
           MOVE    YYK-YYKID       TO  SPA-NAI20-YYKID   (IDX)
           MOVE    YYK-YYKTIME     TO  SPA-NAI20-YYKTIME (IDX)
           MOVE    YYK-YYKTIME(1:2)
                                    TO  SPA-GMN20-TYYKTIME(IDX)(1:2)
           MOVE    ":"              TO  SPA-GMN20-TYYKTIME(IDX)(3:1)
           MOVE    YYK-YYKTIME(3:2)
                                    TO  SPA-GMN20-TYYKTIME(IDX)(4:2)
      *    診療科
           IF      YYK-SRYKA       NOT =   SPACE
               PERFORM 31012-SRYKA-HEN-SEC
           END-IF
      *    担当ドクター名編集
           IF      YYK-DRCD           NOT =   SPACE
               PERFORM 31013-DRCD-HEN-SEC
           END-IF
      *    診療行為内容編集
           IF      YYK-SRYNAIYO       NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY           >   10)
                           OR       (SPA-SRYNAI-CD(IDY)
                                                   =   SPACE)
                   IF      YYK-SRYNAIYO      =   SPA-SRYNAI-CD(IDY)
                       MOVE    SPA-SRYNAI-MEI (IDY)
                                             TO  SPA-GMN20-TSRYNAI(IDX)
                   END-IF
               END-PERFORM
           END-IF
           .
       31011-YYKUKE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦表示編集理
      *****************************************************************
       31011-WAREKIHEN-SEC             SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
           .
       31011-WAREKIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未登録画面より、再編集
      *****************************************************************
       3001-UKEPAR-SET-SEC             SECTION.
      *
           MOVE    UKEPAR-REC          TO  UKETUKE-REC
           IF  UKETUKE-REC         NOT =   SPACE
               IF      SPA-NAME            =   SPACE
                   MOVE    UKE-NAME        TO  SPA-GMN-NAMEH
                   MOVE    UKE-NAME        TO  SPA-GMN-NAME
                   MOVE    UKE-NAME        TO  P02-NAMEH
                   MOVE    UKE-NAME        TO  P02-P02-NAME
               END-IF
               MOVE    UKE-UKEID           TO  SPA-P02-UKEID
               MOVE    SPA-NAI20-UKEMAX    TO  IDX
               PERFORM 3101-UKETU-HEN-SEC
               MOVE    IDX                 TO  SPA-NAI20-UKEMAX
      *
               MOVE    SPACE               TO  UKEPAR-REC
           END-IF
      *
           .
       3001-UKEPAR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    未登録画面より、再編集
      *****************************************************************
       3001-YYKSET-SEC             SECTION.
      *
           ADD     1                   TO  SPA-NAI20-UKEMAX
           MOVE    SPA-NAI20-UKEMAX    TO  IDX
           MOVE    IDX                 TO  SPA-GMN20-YBANGOU (IDX)
           MOVE    YYK-SRYNAIYO        TO  SPA-NAI20-SRYNAI  (IDX)
           MOVE    YYK-DRCD (2:4)      TO  SPA-NAI20-DRCD    (IDX)
           MOVE    YYK-SRYKA           TO  SPA-NAI20-SRYKA   (IDX)
           MOVE    YYK-KEYYYKTIME      TO  SPA-NAI20-KEYYYKTIME(IDX)
           MOVE    YYK-YYKID           TO  SPA-NAI20-YYKID   (IDX)
           MOVE    YYK-YYKTIME         TO  SPA-NAI20-YYKTIME (IDX)
           MOVE    YYK-YYKTIME(1:2)
                                       TO  SPA-GMN20-TYYKTIME(IDX)(1:2)
           MOVE    ":"                 TO  SPA-GMN20-TYYKTIME(IDX)(3:1)
           MOVE    YYK-YYKTIME(3:2)
                                       TO  SPA-GMN20-TYYKTIME(IDX)(4:2)
      *     診療科　内容編集
           IF      YYK-SRYKA         NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY       >    SPA-SRYKA-MAX )
                   IF      YYK-SRYKA       =   SPA-GMN-SRYKA-LST(IDY)
                                                            (1:2)
                       MOVE    SPA-GMN-SRYKA-LST (IDY)(4:)
                                          TO  SPA-GMN20-TSRYKA(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *            担当ドクター編集
           IF      YYK-DRCD          NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY               >   10)
                           OR       (SPA-DRCD-CD(IDY) =   SPACE)
                   IF      YYK-DRCD (2:4)     =   SPA-DRCD-CD(IDY)
                       MOVE    SPA-DRCD-MEI (IDY)
                                              TO  SPA-GMN20-TDRCD(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *            診療行為内容編集
           IF      YYK-SRYNAIYO       NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL    (IDY               >   10)
                           OR       (SPA-SRYNAI-CD(IDY) =   SPACE)
                   IF      YYK-SRYNAIYO      =   SPA-SRYNAI-CD(IDY)
                       MOVE    SPA-SRYNAI-MEI (IDY)
                                             TO  SPA-GMN20-TSRYNAI(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPA-NAI20-UKEMAX         TO  SPA-GMN20-YSELNUM
           PERFORM 4101-YSELNUM-CHK-SEC
      *
           .
       3001-YYKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
      *
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
           IF      SPA-GMN20-CUR       =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
               ADD     1               TO  SPA-GMN20-CUR
           END-IF
      *
           EVALUATE    SPA-GMN20-CUR
              WHEN    01
                  MOVE    "P020_SELNUM"    TO  MCP-WIDGET
              WHEN    02
                  MOVE    "P020_SRYKA"     TO  MCP-WIDGET
              WHEN    03
                  MOVE    "P020_DRNAME"    TO  MCP-WIDGET
              WHEN    04
                  MOVE    "P020_SRYNAIYO"  TO  MCP-WIDGET
              WHEN    05
                  MOVE    "P020_KARUTE"    TO  MCP-WIDGET
              WHEN    06
                  MOVE    "P020_SYOHOU"    TO  MCP-WIDGET
              WHEN    07
                  MOVE    "P020_HKNKUMI"   TO  MCP-WIDGET
      *        受付完了へ
              WHEN    08
                  MOVE    "BY1"            TO  MCP-WIDGET
      *        受付へ
              WHEN    09
                  MOVE    "BY2"            TO  MCP-WIDGET
      *        確定
              WHEN    10
                  MOVE    "B12"            TO  MCP-WIDGET
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
               WHEN    "P020_SELNUM"
                   IF      SPA-GMN20-YSELNUM   =   SPACE
                       MOVE    1               TO  SPA-GMN20-CUR
                   ELSE
                       MOVE    08              TO  SPA-GMN20-CUR
                   END-IF
               WHEN    "P020_SRYKA"
                   MOVE    02              TO  SPA-GMN20-CUR
               WHEN    "P020_DRNAME"
                   MOVE    03              TO  SPA-GMN20-CUR
               WHEN    "P020_SRYNAIYO"
                   MOVE    04              TO  SPA-GMN20-CUR
               WHEN    "P020_KARUTE"
                   MOVE    05              TO  SPA-GMN20-CUR
               WHEN    "P020_SYOHOU"
                   MOVE    06              TO  SPA-GMN20-CUR
               WHEN    "P020_HKNKUMI"
                   MOVE    07              TO  SPA-GMN20-CUR
           END-EVALUATE
      *
           .
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    登録
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    未登録画面へ
               WHEN    "CLICKED"       ALSO    "BM1"
                   PERFORM 420-MITOROKU-SEC
      *    受付完了
               WHEN    "CLICKED"       ALSO    "BY1"
                   PERFORM 421-UKETUKE-SEC
      *    受付取り消し
               WHEN    "CLICKED"       ALSO    "BY2"
                   PERFORM 422-UKETUDEL-SEC
      *    [前↑]押下
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-ZEN-GMNHYOUJI-SEC
      *    [次↓]押下
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-TUGI-GMNHYOUJI-SEC
      *
      *     以外はエラー
               WHEN    "CLICKED"       ALSO     ANY
                   MOVE    "0110"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-GMN20-CUR
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    前表示（組合せ情報）処理
      *****************************************************************
       440-ZEN-GMNHYOUJI-SEC           SECTION.
      *
           .
       440-ZEN-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次表示（組合せ情報）処理
      *****************************************************************
       450-TUGI-GMNHYOUJI-SEC          SECTION.
      *
           .
       450-TUGI-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN20-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *       予約リスト選択
               WHEN    ANY             ALSO    "P020_UKETUKE_LIST"
                   PERFORM 4101-YBANG-CHK-SEC
      *    保険組合せ選択
               WHEN    ANY             ALSO    "P020_HKNKUMI_LIST"
                   PERFORM 41013-HKNKUMI-LIST-SEC
           END-EVALUATE
           .
      *
       400-GMN-INPUT-EXT.
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
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           IF      P02-P020-SELNUM     =   SPA-GMN20-YSELNUM
               MOVE    ZERO            TO  FLG-IN-SELNUM
           ELSE
               MOVE    1               TO  FLG-IN-SELNUM
           END-IF
           MOVE    P02-P020-SELNUM     TO  SPA-GMN20-YSELNUM
      *
           MOVE    P02-P020-SRYKA      TO  SPA-GMN20-SRYKA-G
           MOVE    P02-P020-DRNAME     TO  SPA-GMN20-DRCD
           MOVE    P02-P020-SRYNAIYO   TO  SPA-GMN20-SRYNAI
           MOVE    P02-P020-KARUTE     TO  SPA-GMN20-KARUTE-G
           MOVE    P02-P020-SYOHOU     TO  SPA-GMN20-SYOHOU-G
      *
           IF      P02-P020-HKNKUMI    =   SPA-GMN20-HKNKUMI
               MOVE    1               TO  FLG-IN-HKNKUMI
           ELSE
               MOVE    ZERO            TO  FLG-IN-HKNKUMI
           END-IF
           MOVE    P02-P020-HKNKUMI    TO  SPA-GMN20-HKNKUMI
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    予約番号
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-YSELNUM-CHK-SEC
           END-IF
      *    診療科
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-SRYKA-CHK-SEC
           END-IF
      *    ドクター
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-DRNAME-CHK-SEC
           END-IF
      *    診療内容
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-SRYNAIYO-CHK-SEC
           END-IF
      *
      *    カルテ発行
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-KARUTE-CHK-SEC
           END-IF
      *
      *    処方発行
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-SYOHOU-CHK-SEC
           END-IF
      *
      *    保険組合せ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-HKNKUMI-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付番号選択処理
      *****************************************************************
       4101-YSELNUM-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-YSELNUM   =   SPACE
               MOVE    ZERO                TO  SPA-NAI20-YSELNUM
               GO      TO      4101-YSELNUM-CHK-EXT
           END-IF
      *
           INITIALIZE                          ORCSNUMAREA
           MOVE    SPA-GMN20-YSELNUM       TO  SNUM-INX
           CALL    "ORCSNUM"                   USING
                                               ORCSNUMAREA
           IF     (SNUM-RC             NOT =   ZERO  )   OR
                  (SNUM-MINKBN         NOT =   SPACE )   OR
                  (SNUM-SYOKBN         NOT =   SPACE )
               MOVE    "2001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN20-CUR
               GO      TO      4101-YSELNUM-CHK-EXT
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-NAI20-YSELNUM
               MOVE    SPA-NAI20-YSELNUM   TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ         TO  SPA-GMN20-YSELNUM
           END-IF
      *
           IF      SPA-NAI20-YSELNUM       >=  01
                                      AND  <=  SPA-NAI20-UKEMAX
               CONTINUE
           ELSE
      *        対象外
               MOVE    "2001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN20-CUR
           END-IF
      *
           IF      (SPA-ERRCD          =   SPACE )  AND
                   (FLG-IN-SELNUM      =   1     )
      *        入力ありで、対象あり
               MOVE    SPA-NAI20-YSELNUM       TO  IDX
               MOVE    SPA-NAI20-SRYKA   (IDX) TO  SPA-GMN20-SRYKA
               MOVE    SPACE                   TO  SPA-GMN20-SRYKA1
               MOVE    SPA-GMN20-TSRYKA  (IDX) TO  SPA-GMN20-SRYKAMEI
               MOVE    SPA-NAI20-DRCD    (IDX) TO  SPA-GMN20-DRCDCD
               MOVE    SPACE                   TO  SPA-GMN20-TANT1
               MOVE    SPA-GMN20-TDRCD   (IDX) TO  SPA-GMN20-DRCDMEI
               MOVE    SPA-NAI20-SRYNAI  (IDX) TO  SPA-GMN20-SRYNAICD
               MOVE    SPACE                   TO  SPA-GMN20-SRYNA1
               MOVE    SPA-GMN20-TSRYNAI (IDX) TO  SPA-GMN20-SRYNAIMEI
               MOVE    SPA-GMN20-TKARUTE (IDX) TO  SPA-GMN20-KARUTE
               MOVE    SPA-GMN20-TSYOHOU (IDX) TO  SPA-GMN20-SYOHOU
               MOVE    SPA-GMN20-THKNCOMBI(IDX) TO  SPA-GMN20-HKNKUMI
               IF      SPA-GMN20-SYOHOU        =   SPACE
                   MOVE    SPA-GMN20-SYOHOU-LST(1)
                                       TO  SPA-GMN20-SYOHOU-G
               END-IF
               IF      SPA-GMN20-KARUTE        =   SPACE
                   MOVE    SPA-GMN20-KARUTE-LST(1)
                                       TO  SPA-GMN20-KARUTE-G
               END-IF
               MOVE    SPA-GMN20-SRYKA     TO  SPA-GMN20-MAE-SRYKA
      *        ドクター一覧
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN20-SRYKA     TO  ORCSDR-SRYKA
               MOVE    SPA-GMN20-DRCD      TO  ORCSDR-IN-DRCD-G
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN20-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
      *        選択分が受付無しの時
               IF      SPA-NAI20-UKETIME(IDX)  =   SPACE
                   MOVE     8                  TO  SPA-GMN20-CUR
               ELSE
                   MOVE     9                  TO  SPA-GMN20-CUR
               END-IF
           END-IF
      *
           .
       4101-YSELNUM-CHK-EXT.
           EXIT.
           .
      *****************************************************************
      *    診療科選択処理
      *****************************************************************
       4101-SRYKA-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-SRYKA-G   =   SPACE
               GO      TO      4101-SRYKA-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   SPA-SRYKA-MAX
               IF      SPA-GMN20-SRYKA     =   SPA-GMN-SRYKA-LST (IDX)
                                                                (1:2)
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0038"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN20-CUR
               GO      TO      4101-SRYKA-CHK-EXT
           END-IF
      *
           IF      SPA-GMN20-SRYKA     NOT =   SPA-GMN20-MAE-SRYKA
      *        診療科が変わった時、ドクターを再編集する
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "3"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN20-SRYKA     TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN20-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDX       FROM    1   BY  1
                       UNTIL     IDX           >   SPA-DRCD-MAX 
                   IF      SPA-GMN20-DRCDCD    =   SPA-DRCD-CD(IDX)
                       MOVE    SPA-GMN20-DRCDLST(IDX)
                                               TO  SPA-GMN20-DRCD
                       MOVE    1               TO  FLG-CHK
                       MOVE    SPA-DRCD-MAX    TO  IDX
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   IF      ORCSDR-IN-DRCD  NOT =   SPACE
                       MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN20-DRCD
                   ELSE
                       MOVE    SPA-GMN20-DRCDLST(1)
                                                   TO  SPA-GMN20-DRCD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN20-SRYKA     TO  SPA-GMN20-MAE-SRYKA
           .
      *
       4101-SRYKA-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクター択処理
      *****************************************************************
       4101-DRNAME-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-DRCD      =   SPACE
               GO      TO      4101-DRNAME-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN20-SRYKA     TO  ORCSDR-SRYKA
           MOVE    SPA-GMN20-DRCDLST-G TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN20-DRCD      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN20-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN20-DRCD
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "2012"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN20-CUR
           END-IF
      *
      **** MOVE    ZERO                TO  FLG-CHK
      *    PERFORM VARYING   IDX       FROM    1   BY  1
      *            UNTIL    (IDX           >   SPA-DRCD-MAX )  OR
      *                     (SPA-GMN20-DRCDLST(IDX) 
      *                                    =   SPACE  )
      *        IF      SPA-GMN20-DRCDCD    =   SPA-GMN20-DRCDLST (IDX)
      *                                                         (1:4)
      *            MOVE    1               TO  FLG-CHK
      *        END-IF
      *    END-PERFORM
      *    IF      FLG-CHK             =   ZERO
      *        MOVE    "2012"              TO  SPA-ERRCD
      *        MOVE    3                   TO  SPA-GMN20-CUR
      *****END-IF
           .
      *
       4101-DRNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療内容チェック処理
      *****************************************************************
       4101-SRYNAIYO-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-SRYNAICD      =   SPACE
               GO      TO      4101-SRYNAIYO-CHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX           >   SPA-GMN20-SRYNAI-MAX ) OR
                            (SPA-GMN20-SRYNAILST(IDX) 
                                           =   SPACE  )
               IF      SPA-GMN20-SRYNAICD  =   SPA-GMN20-SRYNAILST(IDX)
                                                                (1:2)
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "2013"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN20-CUR
           END-IF
           .
      *
       4101-SRYNAIYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カルテ選択処理
      *****************************************************************
       4101-KARUTE-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-KARUTE      =   "0"    OR  "1"
                                         OR  "2"
                                         OR  "3"
      *        保険組合せが１件の時、１へ
               IF     (SPA-GMN20-KARUTE      =   "1"  )  AND
                      (SPA-HKNCOMBI-MAX      =   1    )
                   MOVE    SPA-GMN-THKNCOMBINUM (1)
                                             TO  SPA-NAI20-HKNKUMI
                   MOVE    SPA-GMN-THKNCOMBIMEI (1)
                                             TO  SPA-NAI20-HKNCOMBIMEI
                   MOVE    SPA-GMN-THKNCOMBINUM (1)
                                             TO  SPA-GMN20-HKNKUMI
               END-IF
      *
           ELSE
               MOVE    "2003"            TO  SPA-ERRCD
               MOVE    5                 TO  SPA-GMN20-CUR
           END-IF
           .
      *
       4101-KARUTE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処方箋選択処理
      *****************************************************************
       4101-SYOHOU-CHK-SEC            SECTION.
      *
           IF      SPA-GMN20-SYOHOU      =   "0"    OR  "1"
      *        保険組合せが１件の時、１へ
               IF     (SPA-HKNCOMBI-MAX      =   1  )  AND
                      (SPA-GMN20-SYOHOU      =   "1" )
                   MOVE    SPA-GMN-THKNCOMBINUM (1)
                                             TO  SPA-NAI20-HKNKUMI
                   MOVE    SPA-GMN-THKNCOMBINUM (1)
                                             TO  SPA-GMN20-HKNKUMI
               END-IF
           ELSE
               MOVE    "2005"            TO  SPA-ERRCD
               MOVE    6                 TO  SPA-GMN20-CUR
           END-IF
           .
      *
       4101-SYOHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       4101-HKNKUMI-CHK-SEC           SECTION.
      *
           IF      SPA-GMN20-HKNKUMI   =   SPACE
               MOVE    ZERO            TO  SPA-NAI20-HKNKUMI
               MOVE    SPACE           TO  SPA-NAI20-HKNCOMBIMEI
               GO      TO                  4101-HKNKUMI-CHK-EXT
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN20-HKNKUMI   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "2002"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN20-CUR
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-NAI20-HKNKUMI
               IF      SPA-NAI20-HKNKUMI   =   ZERO
                   MOVE    SPACE           TO  SPA-NAI20-HKNCOMBIMEI
                   MOVE    SPA-NAI20-HKNKUMI   TO  SPA-GMN20-HKNKUMI
                   MOVE    8                   TO  SPA-GMN20-CUR
               ELSE
                   MOVE    SPA-NAI20-HKNKUMI   TO  SPA-GMN20-HKNKUMI
               END-IF
           END-IF
      *
           IF      (SPA-ERRCD          =   SPACE )  AND
                   (SPA-NAI20-HKNKUMI  NOT =   ZERO)
                MOVE    ZERO               TO  FLG-OK
                PERFORM VARYING     IDX    FROM    1   BY  1
                    UNTIL      (IDX    >   30         )
                    OR         (IDX    >   SPA-HKNCOMBI-MAX)
                   IF      SPA-NAI20-HKNKUMI
                                       =   SPA-GMN-THKNCOMBINUM(IDX)
                       MOVE    SPA-GMN-THKNCOMBIMEI (IDX)
                                           TO  SPA-NAI20-HKNCOMBIMEI
                       MOVE    1           TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "2002"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN20-CUR
              END-IF
          END-IF
      *
           .
       4101-HKNKUMI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付選択処理
      *****************************************************************
       4101-YBANG-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      P02-P020-UKETUKE-LIST-SELECT(IDX)  =   "T"
                   IF      IDX             =   SPA-NAI20-YSELNUM
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN20-YBANGOU  (IDX)
                                               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          NOT =    ZERO
               MOVE     WRK-SELNUM         TO  SPA-NAI20-YSELNUM
           END-IF
           IF      SPA-NAI20-YSELNUM   NOT =   ZERO
               MOVE     SPA-NAI20-YSELNUM  TO  P02-P020-SELNUM
      *        受付取消へ
               MOVE     9                  TO  SPA-GMN20-CUR
           END-IF
           .
      *
       4101-YBANG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       41013-HKNKUMI-LIST-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
      ************ UNTIL       IDX     >   10
                   UNTIL       IDX     >   30
               IF      P02-P020-HKNCOMBI-SELECT(IDX)  =   "T"
                   IF      SPA-GMN-THKNCOMBINUM (IDX)
                                                =   SPA-NAI20-HKNKUMI
                       CONTINUE
                   ELSE
                       MOVE    SPA-GMN-THKNCOMBINUM (IDX)
                                               TO  WRK-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM          TO  SPA-NAI20-HKNKUMI
           END-IF
      *
           IF      SPA-NAI20-HKNKUMI   NOT =   ZERO
               MOVE    SPA-NAI20-HKNKUMI   TO  P02-P020-HKNKUMI
      *        受付完了へ
               MOVE    8                   TO  SPA-GMN20-CUR
           END-IF
           .
      *
       41013-HKNKUMI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付完了
      *****************************************************************
       421-UKETUKE-SEC            SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO              421-UKETUKE-EXT
           END-IF
      *
           IF       SPA-GMN20-SRYKA    =   SPACE
               MOVE    "2010"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN20-CUR
               GO      TO              421-UKETUKE-EXT
           END-IF
      *
           IF      SPA-GMN20-SRYNAICD      =   SPACE
               MOVE    "2013"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN20-CUR
               GO      TO              421-UKETUKE-EXT
           END-IF
      *
           IF      SPA-NAI20-YSELNUM   =   ZERO
               ADD    1                TO  SPA-NAI20-UKEMAX
      *
               IF  SPA-NAI20-UKEMAX    >   10
                   MOVE   "2008"       TO  SPA-ERRCD
                   COMPUTE SPA-NAI20-UKEMAX
                                       =   SPA-NAI20-UKEMAX  -  1
                   GO     TO           421-UKETUKE-EXT
               END-IF
               MOVE   SPA-NAI20-UKEMAX TO  SPA-NAI20-YSELNUM
      *----(01.00.02) LINE ADD START -------------------------------
               MOVE    SPACE           TO  SPA-GMN20-TBLO
                                               (SPA-NAI20-UKEMAX)
               MOVE    SPACE           TO  SPA-NAI20-TBLO
                                               (SPA-NAI20-UKEMAX)
               INITIALIZE                  SPA-GMN20-TBLO
                                               (SPA-NAI20-UKEMAX)
               INITIALIZE                  SPA-NAI20-TBLO
                                               (SPA-NAI20-UKEMAX)
      *----(01.00.02) LINE ADD END   -------------------------------
           END-IF
           MOVE    SPA-NAI20-YSELNUM   TO  IDX
           MOVE    SPA-NAI20-YSELNUM   TO  SPA-GMN20-YBANGOU (IDX)
           IF      SPA-NAI20-UKETIME (IDX)   =   SPACE  OR  ZERO
               ACCEPT  SYS-TIME        FROM    TIME
               MOVE    SYS-TIME(1:6)   TO  SPA-NAI20-UKETIME (IDX)
           END-IF
           MOVE    SPA-NAI20-UKETIME (IDX)(1:2)
                                       TO  SPA-GMN20-TUKETIME(IDX)(1:2)
           MOVE    ":"                 TO  SPA-GMN20-TUKETIME(IDX)(3:1)
           MOVE    SPA-NAI20-UKETIME (IDX)(3:2)
                                       TO  SPA-GMN20-TUKETIME(IDX)(4:2)
      *
           MOVE    SPA-GMN20-SRYKA     TO  SPA-NAI20-SRYKA   (IDX)
           MOVE    SPA-GMN20-SRYKAMEI  TO  SPA-GMN20-TSRYKA  (IDX)
           MOVE    SPA-GMN20-DRCDCD    TO  SPA-NAI20-DRCD    (IDX)
           MOVE    SPA-GMN20-DRCDMEI   TO  SPA-GMN20-TDRCD   (IDX)
           MOVE    SPA-GMN20-SRYNAICD  TO  SPA-NAI20-SRYNAI  (IDX)
           MOVE    SPA-GMN20-SRYNAIMEI TO  SPA-GMN20-TSRYNAI (IDX)
           MOVE    SPA-GMN20-KARUTE    TO  SPA-GMN20-TKARUTE (IDX)
           MOVE    SPA-GMN20-SYOHOU    TO  SPA-GMN20-TSYOHOU (IDX)
           MOVE    SPA-GMN20-HKNKUMI   TO  SPA-GMN20-THKNCOMBI(IDX)
           MOVE    SPA-NAI20-HKNCOMBIMEI
                                       TO  SPA-GMN20-THKNCOMBIMEI(IDX)
      *H29.4
      *    削除解除
           MOVE    ZERO                TO  SPA-NAI20-DELKBN (IDX)
      *
           MOVE    SPACE               TO  SPA-GMN20-YSELNUM
           MOVE    ZERO                TO  SPA-NAI20-YSELNUM
           MOVE    SPACE               TO  SPA-GMN20-KARUTE-G
           MOVE    SPACE               TO  SPA-GMN20-SYOHOU-G
           MOVE    SPACE               TO  SPA-GMN20-HKNKUMI 
           MOVE    ZERO                TO  SPA-NAI20-HKNKUMI 
      *
           IF      SPA-GMN-SRYKA-LST (1)     NOT =   SPACE
               MOVE    SPA-GMN-SRYKA-LST(1)  TO  SPA-GMN20-SRYKA-G
           END-IF
           IF  SPA-SRYNAI-CD (1)    NOT =   SPACE
               MOVE    SPA-GMN20-SRYNAILST(1) TO  SPA-GMN20-SRYNAI
           END-IF
           IF  SPA-DRCD-CD (1)      NOT =   SPACE
               MOVE    SPA-GMN20-DRCDLST(1)   TO  SPA-GMN20-DRCD
           END-IF
           MOVE    SPA-GMN20-KARUTE-LST(1)
                                       TO  SPA-GMN20-KARUTE-G
           MOVE    SPA-GMN20-SYOHOU-LST(1)
                                       TO  SPA-GMN20-SYOHOU-G
      *
           MOVE    1                    TO  SPA-GMN20-CUR
           .
       421-UKETUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付取り消し
      *****************************************************************
       422-UKETUDEL-SEC           SECTION.
      *
           IF       SPA-NAI20-YSELNUM    =   ZERO
               GO      TO                  422-UKETUDEL-EXT
           END-IF
      *
      *    予約分は受付時間のクリア
           IF  SPA-GMN20-TYYKTIME (SPA-NAI20-YSELNUM)
                                         =   SPACE OR ZERO
               CONTINUE
           ELSE
               MOVE    SPACE           TO  SPA-GMN20-TUKETIME
                                                   (SPA-NAI20-YSELNUM)
               MOVE    SPACE           TO  SPA-NAI20-UKETIME
                                                   (SPA-NAI20-YSELNUM)
      *H29.4
      *        受付済みは削除区分設定
               IF      SPA-NAI20-UKEID(SPA-NAI20-YSELNUM)
                                                   =    ZERO
                                                   OR   99999
                   CONTINUE
               ELSE
                   MOVE    1           TO  SPA-NAI20-DELKBN
                                                   (SPA-NAI20-YSELNUM)
               END-IF
      *
               MOVE    SPACE           TO  SPA-GMN20-YSELNUM
               MOVE    ZERO            TO  SPA-NAI20-YSELNUM
               MOVE    SPA-GMN20-KARUTE-LST(1)
                                       TO  SPA-GMN20-KARUTE-G
               MOVE    SPA-GMN20-SYOHOU-LST(1)
                                       TO  SPA-GMN20-SYOHOU-G
               MOVE    SPACE           TO  SPA-GMN20-HKNKUMI 
               MOVE    ZERO            TO  SPA-NAI20-HKNKUMI 
               GO      TO                  422-UKETUDEL-EXT
           END-IF
      *
      *H29.4
           IF      SPA-NAI20-UKEID(SPA-NAI20-YSELNUM)  =    ZERO
                                                       OR   99999
      *新規登録分
      *
      *    １行取り消し
           INITIALIZE                SPA-GMN20-TBLO(SPA-NAI20-YSELNUM)
           INITIALIZE                SPA-NAI20-TBLO(SPA-NAI20-YSELNUM)
           MOVE    SPA-GMN20-TBL       TO  WRK-GMN-TBL
           MOVE    SPA-NAI20-TBL       TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN20-TBL
           INITIALIZE                  SPA-NAI20-TBL
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   10
               IF      WRK-GMN-YBANGOU(IDX)  NOT =   ZERO
                   ADD     1                 TO  IDY
                   MOVE    WRK-GMN-TBLO(IDX) TO  SPA-GMN20-TBLO (IDY)
                   MOVE    WRK-NAI-TBLO(IDX) TO  SPA-NAI20-TBLO   (IDY)
                   MOVE    IDY               TO  SPA-GMN20-YBANGOU(IDY)
               END-IF
           END-PERFORM
      *
           COMPUTE SPA-NAI20-UKEMAX    =   SPA-NAI20-UKEMAX   -   1
      *
      *H29.4
           ELSE
      *        受付済みは削除区分設定
               MOVE    1               TO  SPA-NAI20-DELKBN
                                                   (SPA-NAI20-YSELNUM)
           END-IF
      *
      *
           MOVE    SPACE               TO  SPA-GMN20-YSELNUM
           MOVE    ZERO                TO  SPA-NAI20-YSELNUM
           MOVE    SPACE               TO  SPA-GMN20-KARUTE-G
           MOVE    SPACE               TO  SPA-GMN20-SYOHOU-G
           MOVE    SPACE               TO  SPA-GMN20-HKNKUMI 
           MOVE    ZERO                TO  SPA-NAI20-HKNKUMI 
      *
           IF      SPA-GMN-SRYKA-LST (1)    NOT =   SPACE
               MOVE    SPA-GMN-SRYKA-LST(1)    TO  SPA-GMN20-SRYKA-G
           END-IF
           IF      SPA-SRYNAI-CD (1)   NOT =   SPACE
               MOVE    SPA-GMN20-SRYNAILST(1)  TO  SPA-GMN20-SRYNAI
           END-IF
           IF      SPA-DRCD-CD (1)     NOT =   SPACE
               MOVE    SPA-GMN20-DRCDLST(1)    TO  SPA-GMN20-DRCD
           END-IF
           MOVE    SPA-GMN20-KARUTE-LST(1)
                                       TO  SPA-GMN20-KARUTE-G
           MOVE    SPA-GMN20-SYOHOU-LST(1)
                                       TO  SPA-GMN20-SYOHOU-G
           .
       422-UKETUDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    未登録画面へ
      *****************************************************************
       420-MITOROKU-SEC            SECTION.
      *
           INITIALIZE                       UKETUKE-REC
           INITIALIZE                       P0201
           INITIALIZE                       SPA-NAI201-AREA
      *
           MOVE  SPA-HOSPNUM                TO  UKE-HOSPNUM.
           MOVE  SPA-SYSYMD                 TO  UKE-UKEYMD.
           MOVE  ZERO                       TO  UKE-PTID.
           MOVE  UKETUKE-REC                TO  MCPDATA-REC.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF  MCP-RC                       =  ZERO
               MOVE  ZERO                   TO  IDX
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               PERFORM  UNTIL    ( IDX          >   19   )
                        OR       ( MCP-RC   NOT =   ZERO )
                   MOVE  MCPDATA-REC        TO  UKETUKE-REC
                   COMPUTE  IDX             =  IDX  +  1
                   COMPUTE  IDX2            = ((IDX - 1) * 5 ) + 1
                   MOVE  UKE-UKEID          TO  SPA-WORK(IDX2:5)
                   MOVE  IDX                TO  P0201-NUM(IDX)
                   MOVE  UKE-UKETIME(1:2)   TO  P0201-YYKTIME(IDX)(1:2)
                   MOVE  ":"                TO  P0201-YYKTIME(IDX)(3:1)
                   MOVE  UKE-UKETIME(3:2)   TO  P0201-YYKTIME(IDX)(4:2)
                   MOVE  UKE-NAME           TO  P0201-NAME   (IDX)
                   MOVE  UKE-UKEID          TO  SPA-NAI201-UKEID  (IDX)
      *
                   PERFORM   VARYING   IDY   FROM    1   BY  1
                             UNTIL   ( IDY      >   SPA-SRYKA-MAX )
                             OR      ( P0201-SRYKA(IDX)  NOT =  SPACE )
                     IF  UKE-SRYKA          =    SPA-GMN-SRYKA-LST (IDY)
                                                            (1:2)
                         MOVE    SPA-GMN-SRYKA-LST(IDY)(4:)
                                            TO  P0201-SRYKA(IDX)
                     END-IF
                   END-PERFORM
      *
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
               END-PERFORM
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE  IDX                    TO  SPA-GMN201-MAX
      *
           ELSE
               MOVE  ZERO                   TO  SPA-GMN201-MAX
           END-IF.
           MOVE  SPA-GMN201-MAX             TO  P0201-COUNT.
      *
           IF  SPA-GMN201-MAX               >   ZERO
	       MOVE  "P0201"                TO  SPA-SAKIPG
	       MOVE  "P02"                  TO  SPA-MOTOPG
	       MOVE  SPACE                  TO  LNK-KYOUTU
	       MOVE  "NEW"                  TO  MCP-PUTTYPE
               MOVE  SPA-SAKIPG             TO  MCP-WINDOW
               MOVE  "SELNUM"               TO  MCP-WIDGET
               PERFORM 900-PUT-WINDOW
               MOVE    1                    TO  SPA-FLG-END
           ELSE
               MOVE    "2004"               TO  SPA-ERRCD
           END-IF
      *
           .
       420-MITOROKU-EXT.
           EXIT.
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    SPA-NAI20-TBL       TO  WRK-NAI-TBL
      *
      *    受診内容をパラメタへ保存
           PERFORM 500-SPA-PARA-SEC
      *
           MOVE    WRK-NAI-TBL         TO  SPA-NAI20-TBL
      *
          .
       490-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    画面の内容をパラメタへ保存
      *****************************************************************
       500-SPA-PARA-SEC        SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "UKETUKE"           TO  PARA-FILEMEI
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *    新規で受付指定が無い場合、対象としない）
           IF     (SPA-GMN-PTID        =   ZERO )  AND
                  (SPA-P02-UKEID       =   ZERO )
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           MOVE    ZERO                TO  WRK-EDANUM
           PERFORM
               UNTIL   FLG-UKETUKE     =   1
             IF        UKE-UKEID           =   ZERO
                   CONTINUE
             ELSE
               MOVE    ZERO            TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   10  )
                        OR        (SPA-GMN20-YBANGOU(IDX)  =   ZERO)
                        OR        (FLG-OK                  =   1   )
                   IF     (SPA-NAI20-UKETIME(IDX)  =   SPACE   OR  ZERO)
      *H30.8
                   AND    (SPA-NAI20-DELKBN (IDX)  =   ZERO  )
                       CONTINUE
                   ELSE
                       IF      SPA-NAI20-UKEID (IDX) =   UKE-UKEID
                           PERFORM 5001-UKETUKE-HEN-SEC
                           PERFORM 5002-UKE-PARA-SEC
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-IF
               END-PERFORM
      *H29.4
      *        存在なしデータ
               IF      FLG-OK          =   ZERO
                   IF     (SPA-GMN-PTID        =   ZERO )  AND
                          (SPA-P02-UKEID   NOT =   UKE-UKEID)
                       CONTINUE
                   ELSE
      *H29.4
      ******           MOVE    "99999999"         TO  UKE-UPYMD
      *                他で登録？
                       PERFORM 5002-UKE-PARA-SEC
                   END-IF
               END-IF
      *
              END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    新規分出力
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-GMN20-YBANGOU(IDX)    =   ZERO )
               IF      SPA-NAI20-UKETIME(IDX)    =   SPACE   OR  ZERO
                   CONTINUE
               ELSE
                   IF      SPA-NAI20-UKEID (IDX)  NOT =   ZERO
                       INITIALIZE                      UKETUKE-REC
                       MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
                       MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
                       MOVE    SPA-GMN-NAME        TO  UKE-NAME
                       MOVE    SPA-NAI20-UKEID(IDX)
                                                   TO  UKE-UKEID
                   ELSE
                       INITIALIZE                      UKETUKE-REC
                       MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
                       MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
                       MOVE    99999               TO  UKE-UKEID
                       MOVE    SPA-GMN-NAME        TO  UKE-NAME
                   END-IF 
                   PERFORM 5001-UKETUKE-HEN-SEC
                   PERFORM 5002-UKE-PARA-SEC
               END-IF
           END-PERFORM
      *
           .
      *
       500-SPA-PARA-EXT.
           EXIT.
      *****************************************************************
      *    受付編集
      *****************************************************************
       5001-UKETUKE-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-PTID              TO  UKE-PTID
           MOVE    SPA-NAI20-UKETIME(IDX)    TO  UKE-UKETIME
           MOVE    SPA-NAI20-SRYKA  (IDX)    TO  UKE-SRYKA
           IF      SPA-NAI20-DRCD (IDX)      =   SPACE
               MOVE    SPACE                     TO  UKE-DRCD
           ELSE
               MOVE    "1"                       TO  UKE-DRCD (1:)
               MOVE    SPA-NAI20-DRCD (IDX)      TO  UKE-DRCD (2:)
           END-IF
           MOVE    SPA-NAI20-SRYNAI (IDX)    TO  UKE-SRYNAIYO 
           MOVE    SPA-NAI20-KEYYYKTIME(IDX) TO  UKE-KEYYYKTIME
           MOVE    SPA-NAI20-YYKTIME(IDX)    TO  UKE-YYKTIME
           MOVE    SPA-NAI20-YYKID  (IDX)    TO  UKE-YYKID
      *    保険組合せ
           IF      SPA-GMN20-THKNCOMBI(IDX)  =   SPACE
               MOVE    ZERO                      TO  UKE-HKNCOMBI
           ELSE
               MOVE    SPA-GMN20-THKNCOMBI(IDX)  TO  UKE-HKNCOMBI
           END-IF
      *
      *    受付氏名は患者氏名を編集する
      *D   IF      SPA-P02-UKEID   NOT =   UKE-UKEID
               MOVE    SPA-GMN-NAME        TO  UKE-NAME
      *D   END-IF
      *H29.4
      *    削除分
           IF      SPA-NAI20-DELKBN (IDX)  =   1
               MOVE    "99999999"          TO  UKE-UPYMD
           ELSE
               MOVE    SPACE               TO  UKE-UPYMD
           END-IF
      *
      *    パラメタ編集済み
           MOVE    SPACE               TO  SPA-NAI20-UKETIME(IDX)
           .
      *
       5001-UKETUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付パラメタ出力
      *****************************************************************
       5002-UKE-PARA-SEC              SECTION.
      *
           ADD     1                   TO  WRK-EDANUM
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "UKETUKE"           TO  PARA-FILEMEI
           MOVE    WRK-EDANUM          TO  PARA-EDANUM
           MOVE    UKETUKE-REC         TO  PARA-DATA-REC
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
          IF      MCP-RC          NOT =   ZERO
               DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
           END-IF
           .
      *
       5002-UKE-PARA-EXT.
           EXIT.
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRMSG-SEC              SECTION.
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診内容マスター読み込み
      *****************************************************************
       900-UKETUKE-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読み込み
      *****************************************************************
       910-YYK-READ-SEC         SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  YYK-REC
               MOVE    ZERO            TO  FLG-YYK
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
           .
       910-YYK-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
