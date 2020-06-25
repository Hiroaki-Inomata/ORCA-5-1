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
       PROGRAM-ID.             ORCGY05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 患者予約状況一覧（Ｙ０５）　
      *                     （Ｙ０１のサブ）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/09/21    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  06/05/15  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.01.00    NACL-多々納  08/01/30  予約内容テーブル対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *    スパ領域
       01  SPA-Y05.
           03  SPA-Y05-AREA.
               05  SPA-GMN-PAGE            PIC 9(02).
               05  SPA-GMN-MAX             PIC 9(04).
               05  SPA-GMN-CUR             PIC 9(02).
      *
               05  SPA-NAI-NEXT            PIC 9(01).
               05  SPA-GMN-SYORI           PIC 9(01).
      *
               05  SPA-GMN-PTNUM           PIC X(20).
               05  SPA-GMN-NAME            PIC X(50).
               05  SPA-GMN-SEX             PIC X(02).
               05  SPA-GMN-BIRTHDAY        PIC X(09).
      *
               05  SPA-GMN-AREA.
                   07  SPA-GMN-TBL.
                       09  SPA-GMN-TBLO    OCCURS  100.
                           11  SPA-GMN-TBANGO      PIC 9(03).
                           11  SPA-GMN-TYYKYMD-G.
                               13  SPA-GMN-TYYKYMD     PIC X(09).
                               13  SPA-GMN-TF1         PIC X(01).
                               13  SPA-GMN-TYOBI       PIC X(04).
                           11  SPA-GMN-TYYKTIME    PIC X(04).
                           11  SPA-GMN-TSRYKA      PIC X(20).
                           11  SPA-GMN-TSINRYO     PIC X(20).
                           11  SPA-GMN-TDRNAME     PIC X(50).
                           11  SPA-GMN-TYYKKBN     PIC X(30).
                           11  SPA-GMN-TRENRAKU    PIC X(20).
                           11  SPA-GMN-TRAIIN      PIC X(04).
               05  SPA-NAIB-AREA.
                   07  SPA-NAI-PTID                PIC 9(10).
                   07  SPA-NAI-SEX                 PIC X(01).
                   07  SPA-NAI-BIRTHDAY            PIC X(08).
      *
                   07  SPA-NAI-TBL.
                       09  SPA-NAI-TBLO    OCCURS  100.
                           11  SPA-NAI-TSRYKA      PIC X(02).
                           11  SPA-NAI-TKYYKTIME   PIC X(04). 
                           11  SPA-NAI-TSRYNAIYO   PIC X(02).
                           11  SPA-NAI-TDRCD       PIC X(05).
                           11  SPA-NAI-TKAKREN     PIC X(01).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX-G               PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-STR             PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENNYOBI        PIC X(02).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
           03  WRK-HENTIME.
               05  WRK-HTIME1      PIC X(02).
               05  WRK-HK1         PIC X(01).
               05  WRK-HTIME2      PIC X(02).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    診療内容情報
           COPY    "CPSK1012.INC".
      *    診療科
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    予約確認
           COPY    "CPSK0051.INC".
      *    予約内容
           COPY    "CPSK1028.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付サブルーチン領域
      *
           COPY  "CPORCSDAY.INC".
      *
           COPY  "CPORCSLNK.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
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
           COPY    "Y01.INC".
           COPY    "Y011.INC".
           COPY    "Y012.INC".
           COPY    "Y03.INC".
           COPY    "Y04.INC".
           COPY    "Y05.INC".
           COPY    "YERR.INC".
           COPY    "YID1.INC".
           COPY    "CPAPIMODV2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-Y05
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU  
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 210-GMNSENI
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
           MOVE    SPA-Y01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-Y05         TO  SPA-FREE
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
           IF      SPA-MOTOPG          =   "YERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               PERFORM 500-GMNHEN-SEC
           END-IF
      *
      *    画面編集
           MOVE    SPACE                TO  LNK-KYOUTU
      *
           MOVE    SPACE                TO  MCP-PUTTYPE
           MOVE    "Y05"                TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                    TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-Y05-AREA
      *
           IF      LNK-KYOUTU         NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           IF      (SPA-MOTOPG         =   "P97")
               MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
               IF      SPA-PTID        NOT =   ZERO
                   MOVE    SPA-PTID        TO  SPA-NAI-PTID
                   MOVE    SPA-PTNUM       TO  SPA-GMN-PTNUM
               END-IF
           ELSE
               MOVE    SPA-WORK            TO  SPA-Y01KYOUTU
      *
               MOVE    SPA-Y01-PTNUM       TO  SPA-GMN-PTNUM
               MOVE    SPA-Y01-PTID        TO  SPA-NAI-PTID
               MOVE    SPA-Y01-YYKNAME     TO  SPA-GMN-NAME
               MOVE    SPA-Y01-SEX         TO  SPA-GMN-SEX
               MOVE    SPA-Y01-BIRTHDAYH   TO  SPA-GMN-BIRTHDAY
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-SYORI
           IF      SPA-NAI-PTID    NOT =   ZERO
               PERFORM 4101-PTINF-HEN-SEC
               PERFORM 310-SPASET-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      * 
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタ編集処理
      *****************************************************************
       310-SPASET-SEC           SECTION.
      *
      *    開始行設定
           COMPUTE WRK-STR             =   (SPA-GMN-PAGE  -  1)
                                       *    100
                                       +    1
      *
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
      *    予約一覧編集
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  YYK-PTID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-G
           MOVE    ZERO                TO  SPA-NAI-NEXT
           MOVE    ZERO                TO  SPA-GMN-MAX
           PERFORM
                   UNTIL      (FLG-YYK     =   1   )
                         OR   (IDX         >   100 )
               MOVE    ZERO                TO  FLG-SYORI
               IF      YYK-YYKID           =   ZERO
                   MOVE    1               TO  FLG-SYORI
               END-IF
      *
               IF      FLG-SYORI           =   ZERO
      *            開始行読み飛ばし
                   ADD     1               TO  IDX-G
                   IF      IDX-G           <   WRK-STR
                       CONTINUE
                   ELSE
                       ADD     1           TO  IDX
                       IF      IDX         >   100
                           MOVE    1           TO  SPA-NAI-NEXT
                       ELSE
                           PERFORM 3101-YOYAKU-HEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスタ編集処理
      *****************************************************************
       3101-YOYAKU-HEN-SEC           SECTION.
      *
           MOVE    IDX             TO  SPA-GMN-MAX
           MOVE    IDX-G           TO  SPA-GMN-TBANGO  (IDX)
           MOVE    YYK-KEYYYKTIME  TO  SPA-NAI-TKYYKTIME (IDX)
           MOVE    YYK-YYKYMD      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG     TO  SPA-GMN-TYYKYMD (IDX)
           MOVE    "("             TO  SPA-GMN-TYOBI   (IDX)(1:1)
           MOVE    WRK-HENNYOBI    TO  SPA-GMN-TYOBI   (IDX)(2:2)
           MOVE    ")"             TO  SPA-GMN-TYOBI   (IDX)(4:1)
           MOVE    YYK-YYKTIME     TO  SPA-GMN-TYYKTIME(IDX)
           MOVE    YYK-SRYKA       TO  SPA-NAI-TSRYKA  (IDX)
           MOVE    YYK-SRYNAIYO    TO  SPA-NAI-TSRYNAIYO (IDX)
           MOVE    YYK-DRCD        TO  SPA-NAI-TDRCD    (IDX)
           MOVE    YYK-KAKREN      TO  SPA-NAI-TKAKREN  (IDX)
      *
           IF      YYK-RAIINZUMI       =   "1"
               MOVE    "来院"          TO  SPA-GMN-TRAIIN(IDX)
           END-IF
      ***  EVALUATE    YYK-YYKKBN
      *        WHEN    "00"
      *            MOVE    SPACE               TO  SPA-GMN-TYYKKBN (IDX)
      *        WHEN    "01"
      *            MOVE    "患者による予約"    TO  SPA-GMN-TYYKKBN (IDX)
      *        WHEN    "02"
      *            MOVE    "医師による予約"    TO  SPA-GMN-TYYKKBN (IDX)
      **** END-EVALUATE
      *    ドクター
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >=  SPA-GMN-MAX ) OR
                              (FLG-OK  =   1           )
               IF     YYK-DRCD             =   SPA-NAI-TDRCD(IDX-D)
                   MOVE    SPA-GMN-TDRNAME(IDX-D)
                                           TO  SPA-GMN-TDRNAME (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     FLG-OK               =   ZERO
               PERFORM 31011-DRCD-HEN-SEC
           END-IF
      *
      *    診療内容
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >=  SPA-GMN-MAX ) OR
                              (FLG-OK  =   1           )
               IF     YYK-SRYNAIYO         =   SPA-NAI-TSRYNAIYO(IDX-D)
                   MOVE    SPA-GMN-TSINRYO(IDX-D)
                                           TO  SPA-GMN-TSINRYO (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF     FLG-OK               =   ZERO
               PERFORM 31011-SRYNAIYO-HEN-SEC
           END-IF
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    YYK-SRYKA           TO  SYS-1005-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1005-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1005-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-SRYKANAME  TO  SPA-GMN-TSRYKA(IDX)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約確認連絡コード
           MOVE    SPACE               TO  SYS-0051-REC
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    YYK-KAKREN          TO  SYS-0051-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-0051-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-0051-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0051-HOSPNUM
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-0051-REC
               MOVE    SYS-0051-YYKKAKNAME TO  SPA-GMN-TRENRAKU(IDX)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約内容コード
           MOVE    SPACE               TO  SYS-1028-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
           MOVE    "1028"              TO  SYS-1028-KANRICD
           MOVE    YYK-YYKKBN          TO  SYS-1028-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1028-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1028-EDYUKYMD
      *
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1028-REC
               MOVE    SYS-1028-YYKKBNNAME TO  SPA-GMN-TYYKKBN (IDX)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3101-YOYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       31011-DRCD-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    YYK-DRCD            TO  SYS-1010-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1010-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1010-EDYUKYMD
      *
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
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  SPA-GMN-TDRNAME (IDX)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31011-DRCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療内容編集処理
      *****************************************************************
       31011-SRYNAIYO-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    YYK-SRYNAIYO        TO  SYS-1012-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1012-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1012-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               MOVE    SYS-1012-SRYNAIYODSP
                                           TO  SPA-GMN-TSINRYO (IDX)
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       31011-DRCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  Y05
           INITIALIZE                      Y05
      *
           MOVE    SPA-GMN-PTNUM       TO  Y05-PTNUM
           MOVE    SPA-GMN-NAME        TO  Y05-NAME
           MOVE    SPA-GMN-SEX         TO  Y05-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  Y05-BIRTHDAY
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >  SPA-GMN-MAX
               MOVE    SPA-GMN-TBANGO (IDX)
                                           TO  Y05-BANGO  (IDX)
               MOVE    SPA-GMN-TYYKYMD-G(IDX)
                                           TO  Y05-YYKYMD (IDX)
               MOVE    SPA-GMN-TYYKTIME(IDX)(1:2)
                                           TO  Y05-YYKTIME(IDX)(1:2)
               MOVE    ":"                 TO  Y05-YYKTIME(IDX)(3:1)
               MOVE    SPA-GMN-TYYKTIME(IDX)(3:2)
                                           TO  Y05-YYKTIME(IDX)(4:2)
               MOVE    SPA-GMN-TSRYKA (IDX)
                                           TO  Y05-SRYKA  (IDX)
               MOVE    SPA-GMN-TSINRYO (IDX)
                                           TO  Y05-SINRYO (IDX)
               MOVE    SPA-GMN-TDRNAME(IDX)
                                           TO  Y05-DRNAME (IDX)
               MOVE    SPA-GMN-TYYKKBN(IDX)
                                           TO  Y05-YYKKBN (IDX)
               MOVE    SPA-GMN-TRENRAKU(IDX)
                                           TO  Y05-RENRAKU (IDX)
               MOVE    SPA-GMN-TRAIIN (IDX)
                                           TO  Y05-RAIIN (IDX)
           END-PERFORM
           MOVE    SPA-GMN-MAX         TO  Y05-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       210-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 220-CLEAR-SEC
      *    前ページ表示
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次ページ表示
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   INITIALIZE                  SPA-KYOTUKEY
                   PERFORM 470-P97-SYORI-SEC
           END-EVALUATE
      *
           .
       210-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "Y05"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "Y01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理へ
      *****************************************************************
       220-CLEAR-SEC             SECTION.
      *
           INITIALIZE                  SPA-Y05
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-P97-SYORI-SEC            SECTION.
      *
           MOVE    "Y05"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P97    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-P97-SYORI-EXT.
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
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LNK-KYOUTU
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Y05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 4100-PTNUM-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       4100-PTNUM-CHK-SEC             SECTION.
      *
           MOVE    Y05-PTNUM           TO  SPA-GMN-PTNUM
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    10
               WHEN    99
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               WHEN    20
      *            氏名検索へ
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   INITIALIZE                  SPA-KYOTUKEY
      *            氏名編集
                   MOVE    SPA-GMN-PTNUM       TO  SPA-NAME
                   PERFORM 470-P97-SYORI-SEC
               WHEN    00
      *            患者番号決定
                   MOVE    SPTNUM-PTID         TO  SPA-NAI-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   PERFORM 4101-PTINF-HEN-SEC
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 310-SPASET-SEC
                   MOVE    2                   TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4100-PTNUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報検索処理
      *****************************************************************
       4101-PTINF-HEN-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-NAI-PTID        TO  PTINF-PTID
           PERFORM 920-PTINF-READ-SEC
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
      *
           .
       4101-PTINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE   -   1
               PERFORM 310-SPASET-SEC
           END-IF
      *
           .
       430-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       440-ATOGMN-SEC             SECTION.
      *
           IF      SPA-NAI-NEXT        =   ZERO
               MOVE    "0003"          TO  SPA-ERRCD
           ELSE
               COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE   +   1
               PERFORM 310-SPASET-SEC
           END-IF
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       500-SEIWA-HEN-SEC             SECTION.
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
      *
      *    曜日
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE    "31"                TO  LNK-DAY3-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY3-AREA
           MOVE  LNK-DAY3-YOBIMEI      TO  WRK-HENNYOBI
      *
           .
       3101-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE   "PTNUM"      TO   MCP-WIDGET
                WHEN    02
                    MOVE  "B01"         TO   MCP-WIDGET
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
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "該当の患者番号がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "前ページは有りません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "次ページは有りません"
                                       TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  YERR
           MOVE    SPA-ERRCD            TO  YERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  YERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "Y05"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
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
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       920-PTINF-READ-SEC         SECTION.
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
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理
      *****************************************************************
       900-YYK-READ-SEC         SECTION.
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
       900-YYK-READ-EXT.
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
       990-DBCLOSE-SEC         SECTION.
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
           MOVE    "PUTWINDOW"       TO  MCP-FUNC.
           CALL    "ORCDBMAIN"       USING
                                         MCPAREA
                                         MCPDATA-REC
                                         SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
