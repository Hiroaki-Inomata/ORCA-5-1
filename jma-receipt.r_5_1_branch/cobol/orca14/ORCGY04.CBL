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
       PROGRAM-ID.             ORCGY04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約患者一覧（Ｙ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01.00.01    NACL-多々納  02/09/11  ドクター編集の修正
      *  01.00.02    NACL-多々納  03/03/07  ドクター件数の修正
      *  01.00.05    NACL-多々納  06/05/12  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.05.00    NACL-多々納  09/06/10  カスタマイズ帳票対応
      *  04.08.00    NACL-森脇    13/10/10  クライアント印刷対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *
       DATA                    DIVISION.
      *FILE                        SECTION.
      *
      *
       WORKING-STORAGE             SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    予約用ワークスパ領域
           COPY    "Y01COMMON-SPA".
      *
      *    画面ＳＰＡ領域
           COPY    "Y04SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-YOYAKU          PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
      *----(04.08.00)--ADD-START---
      *    クライアント印刷有
           03  FLG-CLIENT-OK       PIC 9(01).
      *----(04.08.00)--ADD-END-----
      *
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
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
      *
           03  IDX-G               PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-STR-YMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-SYMD.
               05  WRK-SSYY        PIC 9(04).
               05  WRK-SSMM        PIC 9(02).
               05  WRK-SSDD        PIC 9(02).
      *
           03  WRK-PATH                PIC X(64).
      *
           03  WRK-SINRYO-MAX          PIC 9(04).
           03  WRK-DRNCD-MAX           PIC 9(04).
      *    予約患者一覧ＰＧ名
           03  WRK-YYKICH-PG           PIC X(20).
      *
           03  WRK-GMN-DRCD            PIC X(04).
      *
      *    ＳＯＲＴ用ワーク
       01  WRK-SORT-AREA.
           03  WRK-YYKTIME                 PIC X(04).
           03  WRK-IDX                     PIC 9(04).
           03  WRK-GMN-TBL.
               05  WRK-GMN-TBLO    OCCURS  6.
                   07  WRK-GMN-YYKYMD      PIC X(20).
                   07  WRK-GMN-TBLO2   OCCURS  200.
                       09  WRK-GMN-TSINRYO     PIC X(02).
                       09  WRK-GMN-TYYKTIME    PIC X(05).
                       09  WRK-GMN-TKNJNAME    PIC X(20).
                   07  WRK-GMN-MAX         PIC 9(04).
      *
           03  WRK-NAI-TBL.
               05  WRK-NAI-TBLO    OCCURS  6.
                   07  WRK-NAI-YYKYMD      PIC X(08).
                   07  WRK-NAI-TBLO2   OCCURS  200.
                       09  WRK-NAI-TBANGO      PIC 9(03).
                       09  WRK-NAI-TYYKID      PIC 9(03).
                       09  WRK-NAI-TKYYKTIME   PIC 9(04).
                       09  WRK-NAI-TYYKTIME    PIC 9(04).
                       09  WRK-NAI-TSINRYO     PIC X(10).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENNYOBI        PIC X(02).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
           03  WRK-HENTIME.
               05  WRK-HTIME1      PIC X(02).
               05  FILLER          PIC X(01)   VALUE   ":".
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
      *    帳票ＰＧ
           COPY  "CPSK1032.INC".
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
      *
      *    予約一覧パラメタ領域
           COPY    "CPORCHC15.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *----(04.08.00)--ADD-START---
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *----(04.08.00)--ADD-END-----
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
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-Y04
           MOVE    SPA-WORK        TO  SPA-Y01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
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
           MOVE    SPA-Y04         TO  SPA-FREE
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *
           INITIALIZE                  FLG-AREA
      **    INITIALIZE                  STS-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
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
      *
      *D     PERFORM 510-ERRSET-SEC
      *
      *    MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPACE               TO  MCP-PUTTYPE.
           MOVE    "Y04"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           INITIALIZE              SPA-Y04
      *
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *
           MOVE    SPA-WORK                TO  SPA-Y01KYOUTU
      *
      *    診療内容をＳＰＡにセット
      *    全体を１行へ
           MOVE    1                   TO  SPA-SINRYO-MAX
           MOVE    "00"                TO  SPA-GMN-SINRYOLST
                                                   (SPA-SINRYO-MAX)
                                                   (1:2)
           MOVE    SPACE               TO  SPA-GMN-SINRYOLST
                                                   (SPA-SINRYO-MAX)
                                                   (3:1)
           MOVE    "全体"              TO  SPA-GMN-SINRYOLST
                                                   (SPA-SINRYO-MAX)
                                                   (4:)
     *
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-Y01-YYKYMDS     TO  SYS-1012-STYUKYMD
           MOVE    SPA-Y01-YYKYMDS     TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM
                   UNTIL      (SPA-SINRYO-MAX  >=  100 )   OR
                              (FLG-SYSKANRI    =   1   )
      *
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               ADD      1                  TO  SPA-SINRYO-MAX
               MOVE    SYS-1012-KBNCD      TO  SPA-GMN-SINRYOCD
               MOVE    SPACE               TO  SPA-GMN-SK1
               MOVE    SYS-1012-SRYNAIYODSP
                                           TO  SPA-GMN-SINRYOMEI
               MOVE    SPA-GMN-SINRYO      TO  SPA-GMN-SINRYOLST
                                                  (SPA-SINRYO-MAX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    職員情報をＳＰＡにセット
      *    全体を１件目
           MOVE    1                   TO  SPA-DRCD-MAX
           MOVE    "9999 指定なし"     TO  SPA-GMN-DRNAMELST
                                                (SPA-DRCD-MAX)
      *
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-Y01-YYKYMDS     TO  SYS-1010-STYUKYMD
           MOVE    SPA-Y01-YYKYMDS     TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM
                   UNTIL      (SPA-DRCD-MAX      >=  100 )   OR
                              (FLG-SYSKANRI      =   1   )
      *
               MOVE    MCPDATA-REC      TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
      *H27.12  非表示
                AND   (SYS-1010-HYOJIFLG   NOT =   "1" )
                   ADD     1                   TO  SPA-DRCD-MAX
                   MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-DRNCD
                   MOVE    SPACE               TO  SPA-GMN-D1
                   MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMEMEI
                   MOVE    SPA-GMN-DRNAME      TO  SPA-GMN-DRNAMELST
                                                       (SPA-DRCD-MAX)
               END-IF
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    初期表示
           IF      SPA-Y01-YYKINIT-FLG =   "1"
      *        全体
               MOVE    SPA-GMN-SINRYOLST (1)   TO  SPA-GMN-SINRYO
               MOVE    SPA-GMN-DRNAMELST (1)   TO  SPA-GMN-DRNAME
               MOVE    "1"                     TO  SPA-NAI-DRNCD(1:1)
               MOVE    SPA-GMN-DRNCD           TO  SPA-NAI-DRNCD(2:4)
           ELSE
               MOVE    SPA-Y01-SRYNAIYOG   TO  SPA-GMN-SINRYO
               MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
               MOVE    SPA-Y01-DRCD        TO  SPA-NAI-DRNCD
           END-IF
      *
           MOVE    SPA-Y01-YYKYMDS     TO  WRK-STR-YMD
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-YYKHEN-SEC
      *
           MOVE    1               TO  SPA-GMN-CUR
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約状況編集処理
      *****************************************************************
       310-YYKHEN-SEC              SECTION.
      *
      *    開始行設定
           COMPUTE IDX-STR             =   (SPA-GMN-PAGE  -  1)
                                       *    200
                                       +    1
      *
           INITIALIZE              SPA-GMN-TBL
           INITIALIZE              SPA-NAI-TBL
      *
           MOVE    ZERO                TO  SPA-NAI-NEXT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   6
               MOVE    WRK-STR-YMD     TO  SPA-NAI-YYKYMD (IDX)
      *
               MOVE    WRK-STR-YMD      TO  WRK-SYMD
               PERFORM 500-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-YYKYMD (IDX)
               MOVE    "("             TO  SPA-GMN-YYKYMD (IDX)
                                                       (11:1)
               MOVE    WRK-HENNYOBI    TO  SPA-GMN-YYKYMD (IDX)
                                                       (12:2)
               MOVE    ")"             TO  SPA-GMN-YYKYMD (IDX)
                                                       (14:)
      *
      *        予約マスター検索
               INITIALIZE                  YYK-REC
               MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
               MOVE    SPA-GMN-SINRYOCD    TO  YYK-SRYNAIYO
               MOVE    SPA-NAI-DRNCD       TO  YYK-DRCD
               MOVE    SPA-NAI-YYKYMD(IDX) TO  YYK-YYKYMD
               IF     (SPA-GMN-SINRYOCD    =   "00"  )  OR
                      (SPA-NAI-DRNCD(2:4)  =   "9999")
                   MOVE    "key5"          TO  WRK-PATH
               ELSE
                   MOVE    "key3"          TO  WRK-PATH
               END-IF
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    WRK-PATH            TO  MCP-PATHNAME
                   PERFORM 900-YOYAKU-READ-SEC
               ELSE
                   INITIALIZE                  YYK-REC
                   MOVE    1               TO  FLG-YOYAKU
               END-IF
      *
               MOVE    ZERO                TO  IDY
               MOVE    ZERO                TO  IDX-G
               PERFORM 
                       UNTIL       FLG-YOYAKU      =   1
                   MOVE    ZERO                TO  FLG-SYORI
                   IF     (SPA-GMN-SINRYOCD    =   "00"  )  OR
                          (SPA-NAI-DRNCD(2:4)  =   "9999")
                       IF      SPA-GMN-SINRYOCD    NOT =   "00"
                           IF      SPA-GMN-SINRYOCD    =  YYK-SRYNAIYO
                               MOVE    1           TO  FLG-SYORI
                           END-IF
                       END-IF
                       IF      SPA-NAI-DRNCD(2:4)  NOT =   "9999"
                           IF      SPA-NAI-DRNCD       =  YYK-DRCD
                               MOVE    1           TO  FLG-SYORI
                           END-IF
                       END-IF
                       IF     (SPA-GMN-SINRYOCD    =   "00"  )  AND
                              (SPA-NAI-DRNCD(2:4)  =   "9999")
                           MOVE    1           TO  FLG-SYORI
                       END-IF
                   ELSE
                       MOVE    1               TO  FLG-SYORI
                   END-IF
                   IF      FLG-SYORI           =   1
                     AND  (YYK-YYKID           >   ZERO )
                       PERFORM 3101-YOYAKU-HEN-SEC
                   END-IF
      *
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    WRK-PATH            TO  MCP-PATHNAME
                   PERFORM 900-YOYAKU-READ-SEC
               END-PERFORM
      *        カーソルクロース
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
      *        日付を＋１する
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    SPA-NAI-YYKYMD(IDX) TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN 
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
               MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           END-PERFORM
      *
      ***** PERFORM 3102-TBLSORT-SEC
      *
           .
       310-YYKHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター編集処理
      *****************************************************************
       3101-YOYAKU-HEN-SEC              SECTION.
      *
      *    対象外のページを読み飛ばし
           ADD     1               TO  IDX-G
           IF      IDX-G           <   IDX-STR
               CONTINUE
           ELSE
               ADD     1               TO  IDY
               IF      IDY             >   200
                   MOVE    1               TO  SPA-NAI-NEXT
               ELSE
                   PERFORM 41002-SRYNAIYO-SET-SEC
                   MOVE    YYK-KEYYYKTIME  TO  
                                           SPA-NAI-TKYYKTIME(IDX IDY)
                   MOVE    YYK-YYKTIME     TO  SPA-NAI-TYYKTIME(IDX IDY)
                   MOVE    YYK-YYKTIME(1:2)
                                           TO  WRK-HTIME1
                   MOVE    YYK-YYKTIME(3:2)
                                           TO  WRK-HTIME2
                   MOVE    WRK-HENTIME     TO  SPA-GMN-TYYKTIME(IDX IDY)
                   MOVE    YYK-NAME        TO  SPA-GMN-TKNJNAME(IDX IDY)
                   MOVE    YYK-YYKID       TO  SPA-NAI-TYYKID  (IDX IDY)
                   MOVE    IDY             TO  SPA-NAI-TBANGO  (IDX IDY)
                   MOVE    IDY             TO  SPA-GMN-MAX     (IDX)
               END-IF
           END-IF
           .
       3101-YOYAKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容設定処理
      *****************************************************************
       41002-SRYNAIYO-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    YYK-SRYNAIYO        TO  SYS-1012-KBNCD
           MOVE    YYK-YYKYMD          TO  SYS-1012-STYUKYMD
           MOVE    YYK-YYKYMD          TO  SYS-1012-EDYUKYMD
      *    システム管理検索
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
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               MOVE    SYS-1012-SRYNAIYODSP 
                                       TO  SPA-GMN-TSINRYO(IDX IDY)
               MOVE    SYS-1012-SRYNAIYODSP 
                                       TO  SPA-NAI-TSINRYO(IDX IDY)
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       41002-SRYNAIYO-SET-EXT.
           EXIT.
      *       
      *****************************************************************
      *    ＳＯＲＴ編集処理
      *****************************************************************
       3102-TBLSORT-SEC              SECTION.
      *
           INITIALIZE                  WRK-SORT-AREA
           MOVE    SPA-GMN-TBL         TO  WRK-GMN-TBL
           MOVE    SPA-NAI-TBL         TO  WRK-NAI-TBL
           INITIALIZE                  SPA-GMN-TBL
           INITIALIZE                  SPA-NAI-TBL
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   6
               MOVE    WRK-GMN-YYKYMD (IDX1)   TO  SPA-GMN-YYKYMD (IDX1)
               MOVE    WRK-NAI-YYKYMD (IDX1)   TO  SPA-NAI-YYKYMD (IDX1)
               MOVE    WRK-GMN-MAX    (IDX1)   TO  SPA-GMN-MAX    (IDX1)
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   WRK-GMN-MAX (IDX1)
                   MOVE    HIGH-VALUE          TO  WRK-YYKTIME
                   MOVE    ZERO                TO  WRK-IDX
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   WRK-GMN-MAX (IDX1)
                       IF      WRK-NAI-TBANGO (IDX1 IDY)  =   ZERO
                           CONTINUE
                       ELSE
                           IF      WRK-GMN-TYYKTIME (IDX1 IDY)
                                                   <  WRK-YYKTIME
                               MOVE    WRK-GMN-TYYKTIME (IDX1 IDY)
                                                   TO  WRK-YYKTIME
                               MOVE    IDY         TO  WRK-IDX
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-IDX         >   ZERO
                       MOVE    WRK-GMN-TBLO2(IDX1 WRK-IDX)
                                          TO  SPA-GMN-TBLO2(IDX1 IDX)
                       MOVE    WRK-NAI-TBLO2(IDX1 WRK-IDX)
                                          TO  SPA-NAI-TBLO2(IDX1 IDX)
                       INITIALIZE         WRK-GMN-TBLO2 (IDX1 WRK-IDX)
                       INITIALIZE         WRK-NAI-TBLO2 (IDX1 WRK-IDX)
                   END-IF
               END-PERFORM
           END-PERFORM
           .
       3102-TBLSORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  Y04
           INITIALIZE                      Y04
      *
           MOVE    SPA-GMN-SINRYO      TO  Y04-SRYNAIYO
           MOVE    SPA-GMN-DRNAME      TO  Y04-DRNAME
      *
           MOVE    ZERO                TO  Y04-S1-COUNT
                                           Y04-S2-COUNT
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-SINRYO-MAX
               MOVE    SPA-GMN-SINRYOLST (IDX)
                                           TO  Y04-SRYNAIYOLST(IDX)
           END-PERFORM
            MOVE    SPA-SINRYO-MAX     TO   Y04-S2-COUNT
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >  SPA-DRCD-MAX
               MOVE    SPA-GMN-DRNAMELST (IDX)
                                           TO  Y04-DRNAMELST  (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO   Y04-S1-COUNT
      *
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX             >   6
      *        MOVE    SPA-GMN-YYKYMD (IDX)
      *                                TO  Y04-TYYKYMD  (IDX)
      *    END-PERFORM
      *
              MOVE    SPA-GMN-YYKYMD (1)
                                      TO  Y04-TYYKYMD1
              MOVE    SPA-GMN-YYKYMD (2)
                                      TO  Y04-TYYKYMD2
              MOVE    SPA-GMN-YYKYMD (3)
                                      TO  Y04-TYYKYMD3
              MOVE    SPA-GMN-YYKYMD (4)
                                      TO  Y04-TYYKYMD4
              MOVE    SPA-GMN-YYKYMD (5)
                                      TO  Y04-TYYKYMD5
              MOVE    SPA-GMN-YYKYMD (6)
                                      TO  Y04-TYYKYMD6
           MOVE    ZERO               TO  Y04-LIST1-COUNT
           MOVE    ZERO               TO  Y04-LIST2-COUNT
           MOVE    ZERO               TO  Y04-LIST3-COUNT
           MOVE    ZERO               TO  Y04-LIST4-COUNT
           MOVE    ZERO               TO  Y04-LIST5-COUNT
           MOVE    ZERO               TO  Y04-LIST6-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(1)
                   OR          SPA-GMN-TKNJNAME (1 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (1 IDX) 
                                           TO  Y04-LIST1-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(1 IDX)
                                           TO  Y04-LIST1-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(1 IDX)
                                           TO  Y04-LIST1-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST1-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(2)
                   OR          SPA-GMN-TKNJNAME (2 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (2 IDX) 
                                           TO  Y04-LIST2-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(2 IDX)
                                           TO  Y04-LIST2-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(2 IDX)
                                           TO  Y04-LIST2-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST2-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(3)
                   OR          SPA-GMN-TKNJNAME (3 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (3 IDX) 
                                           TO  Y04-LIST3-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(3 IDX)
                                           TO  Y04-LIST3-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(3 IDX)
                                           TO  Y04-LIST3-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST3-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(4)
                   OR          SPA-GMN-TKNJNAME (4 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (4 IDX) 
                                           TO  Y04-LIST4-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(4 IDX)
                                           TO  Y04-LIST4-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(4 IDX)
                                           TO  Y04-LIST4-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST4-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(5)
                   OR          SPA-GMN-TKNJNAME (5 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (5 IDX) 
                                           TO  Y04-LIST5-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(5 IDX)
                                           TO  Y04-LIST5-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(5 IDX)
                                           TO  Y04-LIST5-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST5-COUNT
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-GMN-MAX(6)
                   OR          SPA-GMN-TKNJNAME (6 IDX)  =   SPACE 
               MOVE    SPA-GMN-TSINRYO (6 IDX) 
                                           TO  Y04-LIST6-SRYNAIYO(IDX)
               MOVE    SPA-GMN-TYYKTIME(6 IDX)
                                           TO  Y04-LIST6-YYKTIME (IDX)
               MOVE    SPA-GMN-TKNJNAME(6 IDX)
                                           TO  Y04-LIST6-NAME    (IDX)
               MOVE    IDX                 TO  Y04-LIST6-COUNT
           END-PERFORM
      *
           PERFORM 5001-MAPCUR-SEC
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
          EVALUATE    SPA-GMN-CUR
                WHEN    01
                    MOVE  "SRYNAIYO"    TO   MCP-WIDGET
                WHEN    02
                    MOVE  "DRNAME"      TO   MCP-WIDGET
            END-EVALUATE
            .
       5001-MAPCUR-EXT.
      *     EXIT.
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
      *    印刷
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 440-PRINT-SEC
      *    前日
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 420-MAEHI-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 430-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 440-ATOGMN-SEC
      *    次日
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 430-ATOHI-SEC
      *    全体
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 430-ALL-HEN-SEC
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
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "Y04"               TO  SPA-MOTOPG
      *
            MOVE    SPA-SAKIPG         TO  MCP-WINDOW
            MOVE   "CHANGE"            TO  MCP-PUTTYPE
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
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *       ドクター入力
               WHEN    "ACTIVATE"      ALSO    "DRNAME"
               WHEN    "CHANGED"       ALSO    "DRNAME"
                   MOVE    02          TO  SPA-GMN-CUR
                   PERFORM 41012-DRNAME-SEC
      *        診療内容入力
               WHEN    "ACTIVATE"      ALSO    "SRYNAIYO"
               WHEN    "CHANGED"       ALSO    "SRYNAIYO"
                   MOVE    01          TO  SPA-GMN-CUR
                   PERFORM 41011-SINRYO-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *****************************************************************
      *    診療内容入力
      *****************************************************************
       41011-SINRYO-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX            >   SPA-SINRYO-MAX
                   IF     Y04-SRYNAIYO(1:2)
                                       =   SPA-GMN-SINRYOLST(IDX)(1:2)
                      MOVE  SPA-GMN-SINRYOLST(IDX)
                                               TO  Y04-SRYNAIYO
                      MOVE  IDX                TO  IDY
                      MOVE  SPA-SINRYO-MAX     TO  IDX
                   END-IF
           END-PERFORM
           IF      IDY                 =   ZERO
               MOVE    "0001"          TO  SPA-ERRCD
           ELSE
               IF      Y04-SRYNAIYO(1:2)   =   SPA-GMN-SINRYOCD
                   MOVE    Y04-SRYNAIYO        TO  SPA-GMN-SINRYO
               ELSE
                   MOVE    Y04-SRYNAIYO        TO  SPA-GMN-SINRYO
                   MOVE    SPA-NAI-YYKYMD (01) TO  WRK-STR-YMD
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 310-YYKHEN-SEC
               END-IF
               ADD     1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       41011-SINRYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力
      *****************************************************************
       41012-DRNAME-SEC            SECTION.
      *
      *H29.12
      *    コード変換
           MOVE    Y04-DRNAME(1:4)     TO  WRK-GMN-DRCD
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-GMN-DRCD        TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   4     )   OR
                  (SNUM-SYOKBN         =   "1"   )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0002"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(12:4)   TO  WRK-GMN-DRCD
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-DRCD-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
      ****     IF     Y04-DRNAME(1:4)
               IF     WRK-GMN-DRCD
                                       =   SPA-GMN-DRNAMELST(IDX)(1:4)
                   MOVE    SPA-GMN-DRNAMELST(IDX)
                                           TO  Y04-DRNAME
                   MOVE    IDX             TO  IDY
                   MOVE    SPA-DRCD-MAX    TO  IDX
               END-IF
           END-PERFORM
      *H27.11
      *    対象なし
           IF      IDY                 =   ZERO
               PERFORM 310112-DRCD-CHK-SEC
           END-IF
           IF      IDY                 =   ZERO
               MOVE    "0002"          TO  SPA-ERRCD
           ELSE
               IF      Y04-DRNAME(1:4)    =   SPA-GMN-DRNCD
                   MOVE    Y04-DRNAME         TO  SPA-GMN-DRNAME
               ELSE
                   MOVE    Y04-DRNAME          TO  SPA-GMN-DRNAME
                   MOVE    "1"                 TO  SPA-NAI-DRNCD (1:1)
                   MOVE    SPA-GMN-DRNCD       TO  SPA-NAI-DRNCD (2:4)
                   MOVE    SPA-NAI-YYKYMD (01) TO  WRK-STR-YMD
                   MOVE    1                   TO  SPA-GMN-PAGE
                   PERFORM 310-YYKHEN-SEC
               END-IF
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
       41012-DRNAME-EXT.
           EXIT.
      *****************************************************************
      *    システム管理テーブル編集処理
      *****************************************************************
       310112-DRCD-CHK-SEC           SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    "1"                 TO  SYS-1010-KBNCD(1:1)
      **** MOVE    Y04-DRNAME(1:4)     TO  SYS-1010-KBNCD(2:4)
           MOVE    WRK-GMN-DRCD        TO  SYS-1010-KBNCD(2:4)
           MOVE    SPA-NAI-YYKYMD(1)   TO  SYS-1010-STYUKYMD
           MOVE    SPA-NAI-YYKYMD(6)   TO  SYS-1010-EDYUKYMD
      *
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
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
      *        最後の行へ編集する
               IF      SPA-DRCD-MAX        >=   100
                   MOVE    100             TO  SPA-DRCD-MAX
               ELSE
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPACE               TO  SPA-GMN-DRNAMELST
                                                      (SPA-DRCD-MAX)
               MOVE    SYS-1010-KBNCD(2:4) TO  SPA-GMN-DRNAMELST
                                                      (SPA-DRCD-MAX)
                                                            (1:4)
               MOVE    SYS-1010-NAME       TO  SPA-GMN-DRNAMELST
                                                      (SPA-DRCD-MAX)
                                                            (6:)
      *
               MOVE    SPA-GMN-DRNAMELST (SPA-DRCD-MAX)
                                           TO  Y04-DRNAME
               MOVE    SPA-DRCD-MAX        TO  IDY
           END-IF
      *
           .
       310112-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日　処理
      *****************************************************************
       420-MAEHI-SEC              SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YYKYMD (01) TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN 
           CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KEISAN     TO  WRK-STR-YMD
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-YYKHEN-SEC
      *
           .
       420-MAEHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前日　処理
      *****************************************************************
       430-ATOHI-SEC              SECTION.
      *
           MOVE    SPA-NAI-YYKYMD (02) TO  WRK-STR-YMD
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-YYKHEN-SEC
      *
           .
       430-AOTHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       430-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               CONTINUE
           ELSE
               COMPUTE SPA-GMN-PAGE        =   SPA-GMN-PAGE   -   1
               MOVE    SPA-NAI-YYKYMD (01) TO  WRK-STR-YMD
               PERFORM 310-YYKHEN-SEC
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
               MOVE    SPA-NAI-YYKYMD (01) TO  WRK-STR-YMD
               PERFORM 310-YYKHEN-SEC
           END-IF
      *
           .
       440-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全体　処理
      *****************************************************************
       430-ALL-HEN-SEC              SECTION.
      *
           IF     (SPA-GMN-SINRYOCD    =   "00"  ) AND
                  (SPA-GMN-DRNCD       =   "9999")
               MOVE    SPA-Y01-SRYNAIYOG   TO  SPA-GMN-SINRYO
               MOVE    SPA-Y01-DRNAME      TO  SPA-GMN-DRNAME
               MOVE    SPA-Y01-DRCD        TO  SPA-NAI-DRNCD
           ELSE
               MOVE    SPA-GMN-SINRYOLST(01)
                                           TO  SPA-GMN-SINRYO
               MOVE    SPA-GMN-DRNAMELST(01)
                                           TO  SPA-GMN-DRNAME
               MOVE    "1"                 TO  SPA-NAI-DRNCD(1:1)
               MOVE    SPA-GMN-DRNCD       TO  SPA-NAI-DRNCD(2:4)
           END-IF
      *
           MOVE    SPA-NAI-YYKYMD (01) TO  WRK-STR-YMD
           MOVE    1                   TO  SPA-GMN-PAGE
           PERFORM 310-YYKHEN-SEC
      *
           .
       430-ALL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表印刷　処理
      *****************************************************************
       440-PRINT-SEC             SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得サブ
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    予約患者一覧出力
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000012"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-YYKICH-PG
           END-IF
      *
           INITIALIZE                  ORCHC15AREA
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX       >   6
               MOVE    SPA-NAI-YYKYMD (IDX)    TO  ORCHC15-YYKYMD (IDX)
               MOVE    SPA-GMN-YYKYMD (IDX)    TO  ORCHC15-GMN-YYKYMD
                                                                 (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SINRYOCD    TO  ORCHC15-SRYNAIYO
           MOVE    SPA-GMN-SINRYOMEI   TO  ORCHC15-SRYNAIYO-MEI
           MOVE    SPA-NAI-DRNCD       TO  ORCHC15-DRCD-G
           MOVE    SPA-GMN-DRNAMEMEI   TO  ORCHC15-DRCD-NAME
           MOVE    SPA-GMN-PAGE        TO  ORCHC15-PAGE
      *
           IF      WRK-YYKICH-PG   NOT =   SPACE
      *        クライアント印刷
               MOVE    "01"                TO  SPA-PRT-FLG
      *
               MOVE    ORCHC15AREA         TO  ONPRTDATA-DATA
               CALL    WRK-YYKICH-PG       USING
                                           SPA-AREA
                                           ONPRTDATA
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                    TO  SPA-CLIENT-TEMP-FLG
      *
           .
       440-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC          SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           .
       4901-CLIENT-PRT-EXT.
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
                   MOVE    "診療内容コードの入力に誤りがあります"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "ドクターコードの入力に誤りがあります"
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
           MOVE    "Y04"                TO  SPA-MOTOPG
           MOVE    "YERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "YERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       510-ERRMSG-EXT.
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
      *    MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Y04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
      *    予約マスター読込
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YOYAKU
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YOYAKU
           END-IF
      *
           .
       900-YOYAKU-READ-EXT.
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
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
           CALL   "ORCDBMAIN"       USING
                                        MCPAREA
                                        MCPDATA-REC
                                        SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
