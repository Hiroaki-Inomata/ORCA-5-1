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
       PROGRAM-ID.             ORCHC15.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 予約登録
      *  コンポーネント名  : 予約一覧表　印刷（ＨＣ１５）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/10/26    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     02.03.18  帳票ファイル名の変更
      *  01.00.03    NACL-多々納  02.09.13  印刷サプの追加
      *  01.00.04    NACL-太田    03.06.12  フォームをDIAからMONPEに変更
      *  01.00.05    NACL-多々納  03/06/27  出力先プリンタサブ追加
      *  01.00.06    NACL-多々納  06/05/15  MONFUNC 対応 他
      *  04.05.00    NACL-多々納  09/06/XX  カスタマイズ帳票対応
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  04.08.00    NACL-多々納  16/10/19  API(GINBEE)対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
           COPY    "HCM15.INC".
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-YYKEND          PIC 9(01).
      *API
           03  FLG-APISYORI        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-D               PIC 9(04).
           03  IDX-L               PIC 9(04).
           03  IDX-C               PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-API             PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WYY         PIC X(03).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-SYSYMDG         PIC X(22).
           03  WRK-ALLPAGE         PIC 9(03).
      *
           03  WRK-YYKYMD          PIC X(08).
           03  WRK-PAGE            PIC 9(03).
           03  WRK-LINE            PIC 9(04).
      *
           03  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-PTNUM               PIC X(20).
      *
           03  WRK-PATH                PIC X(64).
      *
      *    予約テーブル
       01  TBL-YYK-AREA.
           03  TBL-YYK-OCC             OCCURS  6.
               05  TBL-YYK-OCC2        OCCURS  330.
                   07  TBL-SRYNAIYO-MEI    PIC X(10).
                   07  TBL-YYKTIME         PIC X(05).
                   07  TBL-YYKNAME         PIC X(20).
      *
       01  TBL-YYKEND-AREA.
           03  TBL-YYKEND-OCC          OCCURS   6.
               05  TBL-CNTMAX              PIC 9(04).
               05  TBL-YYKEND              PIC 9(01).
      *
      *
      *    診療内容テーブル
       01  TBL-SRYNAIYO-AREA.
           03  TBL-SRYNAIYO-G          OCCURS   99.
               05  TBL-SRYNAIYO        PIC X(02).
               05  TBL-SRYNAIYOMEI     PIC X(30).
           03  TBL-SRYNAI-MAX          PIC 9(02).
      *
      *H28.10
      *    API対応用
      *    ドクターテーブル
       01  TBL-DRCD-AREA.
           03  TBL-DRCD-G              OCCURS   200.
               05  TBL-DRCD            PIC X(05).
               05  TBL-DRCDNAME        PIC X(50).
               05  TBL-DRCDKANANAME    PIC X(50).
           03  TBL-DRCD-MAX            PIC 9(03).
      *
      *    診療科テーブル
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-G             OCCURS   99.
               05  TBL-SRYKA-CD        PIC X(02).
               05  TBL-SRYKANAME       PIC X(30).
           03  TBL-SRYKA-MAX           PIC 9(02).
      *    連絡先テーブル
       01  TBL-RENRAK-AREA.
           03  TBL-RENRAK-G            OCCURS   99.
               05  TBL-RENRAK-CD       PIC X(01).
               05  TBL-YYKKAKNAME      PIC X(30).
           03  TBL-RENRAK-MAX          PIC 9(02).
      *    予約内容テーブル
       01  TBL-YYKKBN-AREA.
           03  TBL-YYKKBN-G            OCCURS   99.
               05  TBL-YYKKBN-CD       PIC X(02).
               05  TBL-YYKKBNNAME      PIC X(30).
           03  TBL-YYKKBN-MAX          PIC 9(02).
      *
       01  WRK-API-AREA.
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
      *    帳票最大行
       01  LINE-MAX                    PIC 9(03)   VALUE   33.
      *    テーブル最大行
       01  TBL-MAX                     PIC 9(03)   VALUE   330.
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENTIME.
               05  WRK-HTIME1          PIC X(02).
               05  FILLER              PIC X(01)   VALUE   ":".
               05  WRK-HTIME2          PIC X(02).
      *
      *H28.10  API
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-API-DATA           PIC X(01).
           03  PARA-API-CUSTOM-ID      PIC X(30).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(100).
           03  WRK-PARA-MAX            PIC 9(04).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *
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
           COPY    "CPSYSKANRI.INC".
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
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    予約患者一覧パラメタ領域
           COPY    "CPORCHC15.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "MCPAREA".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *H28.10
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      * API対応
      *    一覧表構造体
           COPY    "CPFMYYKPTLIST.INC".
      *
           COPY    "CPORCRPMAIN.INC".
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "CPONPRTDATA.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ONPRTDATA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期編集処理
           PERFORM 100-INIT-SYORI-SEC
      *
      *    一覧印刷処理
           PERFORM 200-MAIN-SYORI-SEC
                   UNTIL           FLG-YYKEND  =   1
      *
           .
       000-PROC-EXT.
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-INIT-SYORI-SEC             SECTION.
      *
           MOVE    ONPRTDATA-DATA      TO  ORCHC15AREA
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  TBL-YYK-AREA
           INITIALIZE                  TBL-YYKEND-AREA
      *
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *H28.10
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    各ＤＢ読込
           PERFORM 100-SYORI-SEC
           .
       100-INIT-SYORI-EXT.
           EXIT.
      *
      *H28.10
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"          TO  WRK-PARA-NAME
           MOVE    1                   TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-API-DATA
           IF     (PARA-API-DATA       =   "1"       )
               CONTINUE
           ELSE
      *        API送信なし
               MOVE    "0"             TO  PARA-API-DATA
           END-IF
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           MOVE    30                  TO  WRK-PARA-MAX
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:WRK-PARA-MAX)
                                       TO  PARA-API-CUSTOM-ID
      *
           .
       100-OPTION-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC             SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC15"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
               IF      WRK-PARA-MAX        >   ZERO
                    MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                                   TO  WRK-PARA
               END-IF
           ELSE
      *        全角判定
               IF      WRK-PARA-MAX        >   ZERO
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    ORCSPRGOPTION-OT-PARA(1:WRK-PARA-MAX)
                                               TO  KANACHK-MAE-INPUT
                   MOVE    WRK-PARA-MAX        TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  WRK-PARA
               END-IF
           END-IF
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票印刷開始処理
      *****************************************************************
       400-SYOKI-PRINT-SEC             SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC15"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
      *
           ADD     1                   TO  WRK-ALLPAGE
           MOVE    WRK-ALLPAGE         TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT               PRT-FILE
      *
           .
       400-SYOKI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       400-PRINT-SEC                SECTION.
      *
           PERFORM 400-SYOKI-PRINT-SEC
      *
      *    印刷用データ出力
           WRITE  HCM15
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"           TO  MKPRT-ID
           MOVE    "HCM15.red"         TO  MKPRT-DIA
           MOVE    SPACE               TO  MKPRT-DEF
      **** MOVE    HC01PARA            TO  MKPRT-DAT
           MOVE    ASGNPARA            TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(19)  TO  MKPRT-PRTNM
           MOVE    "予約患者一覧"      TO  MKPRT-INFO
           CALL    "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期編集処理
      *****************************************************************
       100-SYORI-SEC                SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1031-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1031-REC
      *    予約一覧の出力先設定がないときは、受付一覧（元の予約一覧）
           IF      SYS-1031-PRTNM(19)  =   SPACE
               MOVE    SYS-1031-PRTNM(7)   TO  SYS-1031-PRTNM(19)
           END-IF
      *
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  WRK-SYSYMDG
      *
           .
       100-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    システム管理テーブル編集処理
      *****************************************************************
       1001-SYSKANRI-TBL-SEC           SECTION.
      *
           INITIALIZE                  TBL-SRYNAIYO-AREA
      *    診療内容
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    WRK-YYKYMD          TO  SYS-1012-STYUKYMD
           MOVE    WRK-YYKYMD          TO  SYS-1012-EDYUKYMD
      *
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX-D
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX-D          >=  99  )
               MOVE    MCPDATA-REC         TO  SYS-1012-REC
               ADD     1                   TO  IDX-D
               MOVE    SYS-1012-KBNCD(1:2) TO  TBL-SRYNAIYO  (IDX-D)
               MOVE    SYS-1012-SRYNAIYODSP
                                           TO  TBL-SRYNAIYOMEI (IDX-D)
               MOVE    IDX-D               TO  TBL-SRYNAI-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.10
      *API対応
           IF     (PARA-API-DATA        =   "1")
               PERFORM 10011-APISYSKANRI-TBL-SEC
           END-IF
      *
           .
       1001-SYSKANRI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理テーブル編集処理（API）
      *****************************************************************
       10011-APISYSKANRI-TBL-SEC       SECTION.
      *
           INITIALIZE                  TBL-DRCD-AREA
           INITIALIZE                  TBL-SRYKA-AREA
           INITIALIZE                  TBL-RENRAK-AREA
           INITIALIZE                  TBL-YYKKBN-AREA
      *
      *    職員情報
           MOVE    SPACE               TO  SYS-1010-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-YYKYMD          TO  SYS-1010-STYUKYMD
           MOVE    WRK-YYKYMD          TO  SYS-1010-EDYUKYMD
      *
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX-D
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX-D          >=  200 )
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-KBNCD(1:1) =   "1"
                   ADD     1                   TO  IDX-D
                   MOVE    SYS-1010-KBNCD(1:5) TO  TBL-DRCD  (IDX-D)
                   MOVE    SYS-1010-NAME       TO  TBL-DRCDNAME (IDX-D)
                   MOVE    SYS-1010-KANANAME   TO  TBL-DRCDKANANAME
                                                               (IDX-D)
                   MOVE    IDX-D               TO  TBL-DRCD-MAX
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診療科
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-YYKYMD          TO  SYS-1005-STYUKYMD
           MOVE    WRK-YYKYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX-D
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX-D          >=  99  )
               ADD     1                   TO  IDX-D
               MOVE    MCPDATA-REC         TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD(1:2) TO  TBL-SRYKA-CD  (IDX-D)
               MOVE    SYS-1005-SRYKANAME1 TO  TBL-SRYKANAME (IDX-D)
               MOVE    IDX-D               TO  TBL-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約確認連絡コード
           MOVE    SPACE               TO  SYS-0051-REC
           MOVE    "0051"              TO  SYS-0051-KANRICD
           MOVE    WRK-YYKYMD          TO  SYS-0051-STYUKYMD
           MOVE    WRK-YYKYMD          TO  SYS-0051-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0051-HOSPNUM
           MOVE    SYS-0051-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX-D
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX-D          >=  99  )
               ADD     1                   TO  IDX-D
               MOVE    MCPDATA-REC         TO  SYS-0051-REC
               MOVE    SYS-0051-KBNCD(1:2) TO  TBL-RENRAK-CD (IDX-D)
               MOVE    SYS-0051-YYKKAKNAME TO  TBL-YYKKAKNAME (IDX-D)
               MOVE    IDX-D               TO  TBL-RENRAK-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    予約内容コード
           MOVE    SPACE               TO  SYS-1028-REC
           MOVE    SPA-HOSPNUM         TO  SYS-1028-HOSPNUM
           MOVE    "1028"              TO  SYS-1028-KANRICD
           MOVE    WRK-YYKYMD          TO  SYS-1028-STYUKYMD
           MOVE    WRK-YYKYMD          TO  SYS-1028-EDYUKYMD
      *
           MOVE    SYS-1028-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  IDX-D
           PERFORM UNTIL   (FLG-SYSKANRI   =   1   )
                       OR  (IDX-D          >=  99  )
               ADD     1                   TO  IDX-D
               MOVE    MCPDATA-REC         TO  SYS-1028-REC
               MOVE    SYS-1028-KBNCD(1:2) TO  TBL-YYKKBN-CD (IDX-D)
               MOVE    SYS-1028-YYKKBNNAME
                                           TO  TBL-YYKKBNNAME (IDX-D)
               MOVE    IDX-D               TO  TBL-YYKKBN-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1001-SYSKANRI-TBL-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約患者一覧処理
      *****************************************************************
       200-MAIN-SYORI-SEC              SECTION.
      *
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-D
           INITIALIZE                  TBL-YYK-AREA
      *    予約内容編集処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   6   )   OR
                              (ORCHC15-YYKYMD (IDX)    =   SPACE )
               IF      TBL-YYKEND  (IDX)       =   ZERO
                   MOVE    TBL-CNTMAX (IDX)        TO  IDX-STR
                   MOVE    ORCHC15-YYKYMD (IDX)    TO  WRK-YYKYMD
      *            診療内容テーブル編集
                   PERFORM 1001-SYSKANRI-TBL-SEC
      *            予約テーブル編集
                   PERFORM 2001-YYK-SYORI-SEC
               END-IF
           END-PERFORM
      *
      *H28.10
           IF     (PARA-API-DATA        =   "1")
      *        ＡＰＩ送信
               PERFORM 20091-API-SYORI-SEC
           ELSE
      *
      *    印刷処理
           PERFORM 2010-HC15-SYORI-SEC
      *
           END-IF
      *
      *    処理終了判定
           IF     (TBL-YYKEND  (1)     =   1  )  AND
                  (TBL-YYKEND  (2)     =   1  )  AND
                  (TBL-YYKEND  (3)     =   1  )  AND
                  (TBL-YYKEND  (4)     =   1  )  AND
                  (TBL-YYKEND  (5)     =   1  )  AND
                  (TBL-YYKEND  (6)     =   1  )
               MOVE    1                   TO  FLG-YYKEND
           END-IF
           .
       200-MAIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約テーブ編集処理
      *****************************************************************
       2001-YYK-SYORI-SEC              SECTION.
      *
      *    予約マスター検索
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    ORCHC15-SRYNAIYO    TO  YYK-SRYNAIYO
           MOVE    ORCHC15-DRCD-G      TO  YYK-DRCD
           MOVE    ORCHC15-YYKYMD(IDX) TO  YYK-YYKYMD
           IF     (ORCHC15-SRYNAIYO    =   "00"  )  OR
                  (ORCHC15-DRCD        =   "9999")
               MOVE    "key5"              TO  WRK-PATH
           ELSE
               MOVE    "key3"              TO  WRK-PATH
           END-IF
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 900-YYK-READ-SEC
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           MOVE    ZERO                TO  TBL-CNTMAX (IDX)
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDX-C
      *API
      *H28.10
           MOVE    ZERO                TO  IDX-API
           IF      PARA-API-DATA        =   "1"
      *        ＡＰＩ送信内容初期編集処理
               PERFORM 20091-API-INITHENSYU-SEC
           END-IF
           PERFORM
                   UNTIL       FLG-YYK         =   1
               MOVE    ZERO                TO  FLG-SYORI
      *        選択内容判定
               IF     (ORCHC15-SRYNAIYO    =   "00"  )  OR
                      (ORCHC15-DRCD        =   "9999")
                   IF      ORCHC15-SRYNAIYO    NOT =   "00"
                       IF      ORCHC15-SRYNAIYO    =  YYK-SRYNAIYO
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   END-IF
                   IF      ORCHC15-DRCD        NOT =   "9999"
                       IF      ORCHC15-DRCD-G      =  YYK-DRCD
                           MOVE    1               TO  FLG-SYORI
                       END-IF
                   END-IF
                   IF     (ORCHC15-SRYNAIYO    =   "00"  )  AND
                          (ORCHC15-DRCD        =   "9999")
                       MOVE    1               TO  FLG-SYORI
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
      *        キーレコード判定
               IF      YYK-YYKID           =   ZERO
                   MOVE    ZERO            TO  FLG-SYORI
               END-IF
               IF      FLG-SYORI           =   1
      *H28.10
                 IF      PARA-API-DATA        =   "1"
      *              ＡＰＩ送信内容編集処理
                     PERFORM 2009-API-HENSYU-SEC
                 ELSE
      *!
                   ADD     1                   TO  IDX-C
      *            前回の読み飛ばし
                   IF      IDX-C               <   IDX-STR
                       CONTINUE
                   ELSE
                       PERFORM 2001-YYKTBL-HEN-SEC
                   END-IF
      *!
                 END-IF
               END-IF
      *
               IF      FLG-YYK             =   ZERO
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    WRK-PATH            TO  MCP-PATHNAME
                   PERFORM 900-YYK-READ-SEC
               END-IF
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    次ページなし
           IF      TBL-CNTMAX (IDX)    =   ZERO
               MOVE    1               TO  TBL-YYKEND (IDX)
           END-IF
           .
       2001-YYK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約テーブル内容編集処理
      *****************************************************************
       2001-YYKTBL-HEN-SEC                SECTION.
      *
           ADD     1                   TO  IDY
           IF      IDY                 >   TBL-MAX
      *        テーブルオーバー
               MOVE    IDX-C           TO  TBL-CNTMAX (IDX)
               MOVE    1               TO  FLG-YYK
           ELSE
      *        診療内容
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   99   )
                   IF     YYK-SRYNAIYO     =   TBL-SRYNAIYO (IDX-D)
                       MOVE    TBL-SRYNAIYOMEI (IDX-D)
                                               TO  TBL-SRYNAIYO-MEI
                                                            (IDX IDY)
                       MOVE    99              TO  IDX-D
                   END-IF
               END-PERFORM
      *        予約時間
               MOVE    YYK-YYKTIME(1:2)    TO  WRK-HTIME1
               MOVE    YYK-YYKTIME(3:2)    TO  WRK-HTIME2
               MOVE    WRK-HENTIME         TO  TBL-YYKTIME (IDX IDY)
      *        予約氏名
               MOVE    YYK-NAME            TO  TBL-YYKNAME (IDX IDY)
           END-IF
           .
       2001-YYKTBL-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2010-HC15-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-LINE
           MOVE    ZERO                TO  IDX-L
           INITIALIZE                  HCM15
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   TBL-MAX
               ADD     1               TO  IDX-L
               IF      IDX-L           >   LINE-MAX
      *            帳票印刷開始処理
                   PERFORM 200-HC15-PRINT-SEC
                   MOVE    1               TO  IDX-L
               END-IF
               MOVE    ZERO                TO  FLG-SYORI
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   6
                   IF      TBL-YYKNAME (IDY IDX)
                                           NOT =   SPACE
                       MOVE    1               TO  FLG-SYORI
      *                明細編集処理
                       PERFORM 300-HC15-HEN-SEC
                   END-IF
               END-PERFORM
      *        明細終了
               IF      FLG-SYORI           =   ZERO
                   MOVE    TBL-MAX             TO  IDX
               END-IF
           END-PERFORM
      *
           IF      IDX-L           >   ZERO
      *        帳票印刷開始処理
               PERFORM 200-HC15-PRINT-SEC
           END-IF
      *
           .
       2010-HC15-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       200-HC15-PRINT-SEC                SECTION.
      *
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM15-PAGE 
      *
           MOVE    "予約患者一覧"      TO  HCM15-TITLE
      *
      *    診療日
           MOVE    WRK-SYSYMDG         TO  HCM15-HAKYMD
      *    ドクター
           MOVE    ORCHC15-DRCD-NAME   TO  HCM15-DRCD
      *
      *    診察内容
           IF      ORCHC15-SRYNAIYO    =   "00"
               MOVE    ORCHC15-SRYNAIYO-MEI    TO  HCM15-TITLE2
           END-IF
      *
           MOVE    ORCHC15-GMN-YYKYMD (01) TO  HCM15-YYKYMD (01)
           MOVE    ORCHC15-GMN-YYKYMD (02) TO  HCM15-YYKYMD (02)
           MOVE    ORCHC15-GMN-YYKYMD (03) TO  HCM15-YYKYMD (03)
           MOVE    ORCHC15-GMN-YYKYMD (04) TO  HCM15-YYKYMD (04)
           MOVE    ORCHC15-GMN-YYKYMD (05) TO  HCM15-YYKYMD (05)
           MOVE    ORCHC15-GMN-YYKYMD (06) TO  HCM15-YYKYMD (06)
      *
           PERFORM 400-PRINT-SEC
      *
           INITIALIZE                  HCM15
           MOVE    ZERO                TO  IDX-L
           .
       200-HC15-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       300-HC15-HEN-SEC                SECTION.
      *
           MOVE    TBL-SRYNAIYO-MEI (IDY IDX)
                                       TO  HCM15-SRYNAIYO (IDY IDX-L)
           MOVE    TBL-YYKTIME    (IDY IDX)
                                       TO  HCM15-YYKTIME  (IDY IDX-L)
           MOVE    TBL-YYKNAME    (IDY IDX)
                                       TO  HCM15-NAME     (IDY IDX-L)
      *
           .
       310-HC15-HEN-EXT.
           EXIT.
      *
      *H28.10
      *****************************************************************
      *    ＡＰＩ送信内容編集処理
      *****************************************************************
       2009-API-HENSYU-SEC           SECTION.
      *
           ADD    1                   TO  IDX-API
           IF     IDX-API             >   1000
               GO      TO      2009-API-HENSYU-EXT
           END-IF
      *
      *    予約時間
           MOVE    YYK-YYKTIME(1:2)    TO  WRK-HTIME1
           MOVE    YYK-YYKTIME(3:2)    TO  WRK-HTIME2
           MOVE    WRK-HENTIME         TO  FMYPL-TYYKTIME(IDX IDX-API)
      *    診療科
           MOVE    YYK-SRYKA           TO  FMYPL-TSRYKA (IDX IDX-API)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   TBL-SRYKA-MAX  )
               IF     YYK-SRYKA            =   TBL-SRYKA-CD (IDX-D)
                   MOVE    TBL-SRYKANAME (IDX-D)
                                           TO  FMYPL-TSRYKANAME
                                                         (IDX IDX-API)
                   MOVE    TBL-SRYKA-MAX   TO  IDX-D
               END-IF
           END-PERFORM
      *    診療内容
           MOVE    YYK-SRYNAIYO        TO  FMYPL-TSRYNAIYO(IDX IDX-API)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   TBL-SRYNAI-MAX  )
               IF      YYK-SRYNAIYO    =   TBL-SRYNAIYO (IDX-D)
                   MOVE    TBL-SRYNAIYOMEI (IDX-D)
                                               TO  FMYPL-TSRYNAIYONAME
                                                           (IDX IDX-API)
                   MOVE    TBL-SRYNAI-MAX  TO  IDX-D
               END-IF
           END-PERFORM
      *    ドクター
           MOVE    YYK-DRCD            TO  FMYPL-TDR-CODE(IDX IDX-API)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   TBL-DRCD-MAX )
                          OR  (TBL-DRCD (IDX-D)    =   SPACE)
               IF     YYK-DRCD             =   TBL-DRCD (IDX-D)
                   MOVE    TBL-DRCDNAME (IDX-D)
                                               TO  FMYPL-TDR-NAME
                                                           (IDX IDX-API)
                  MOVE    TBL-DRCDKANANAME (IDX-D)
                                               TO  FMYPL-TDR-KANANAME
                                                           (IDX IDX-API)
                   MOVE    TBL-DRCD-MAX    TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    患者番号
           IF      YYK-PTID            =   ZERO
               MOVE    SPACE               TO  FMYPL-TPTNUM(IDX IDX-API)
           ELSE
               PERFORM 3001-KNJNO-HENSYU-SEC
               MOVE    WRK-PTNUM           TO  FMYPL-TPTNUM(IDX IDX-API)
      *        患者情報
               INITIALIZE                      PTINF-REC
               MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
               MOVE    YYK-PTID            TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               MOVE    PTINF-KANANAME      TO  FMYPL-TKANANAME
                                                           (IDX IDX-API)
               MOVE    PTINF-NAME          TO  FMYPL-TNAME (IDX IDX-API)
               MOVE    PTINF-SEX           TO  FMYPL-TSEX  (IDX IDX-API)
      *        生年月日
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  FMYPL-TBIRTHDAY 
                                                           (IDX IDX-API)
           END-IF
      *    予約患者名称
           MOVE    YYK-NAME            TO  FMYPL-TYYKNAME (IDX IDX-API)
      *
      *    予約確認連絡コード
           MOVE    YYK-KAKREN          TO  FMYPL-TKAKREN (IDX IDX-API)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   TBL-RENRAK-MAX )
               IF     YYK-KAKREN            =   TBL-RENRAK-CD (IDX-D)
                   MOVE    TBL-YYKKAKNAME (IDX-D)
                                           TO  FMYPL-TKAKRENNAME
                                                         (IDX IDX-API)
                   MOVE    TBL-RENRAK-MAX  TO  IDX-D
               END-IF
           END-PERFORM
      *
      *    予約内容コード
           MOVE    YYK-YYKKBN          TO  FMYPL-TYYKKBN (IDX IDX-API)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL      (IDX-D   >   TBL-YYKKBN-MAX   )
               IF     YYK-YYKKBN            =   TBL-YYKKBN-CD (IDX-D)
                   MOVE    TBL-YYKKBNNAME (IDX-D)
                                           TO  FMYPL-TYYKKBNNAME
                                                         (IDX IDX-API)
                   MOVE    TBL-YYKKBN-MAX  TO  IDX-D
               END-IF
           END-PERFORM
      *    来院済み
           IF      YYK-RAIINZUMI       =   "1"
               MOVE    "1"             TO  FMYPL-TRAIINZUMI
                                                         (IDX IDX-API)
           END-IF
      *
           .
       2009-API-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ初期編集処理
      *****************************************************************
       20091-API-INITHENSYU-SEC         SECTION.
      *
           IF      IDX                 =   1
               PERFORM 20090-API-INIT-SEC
           END-IF
      *    予約日
           MOVE    ORCHC15-YYKYMD(IDX) TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMYPL-TSRYYMD (IDX)
      *
           .
       20091-API-INITHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ初期編集処理
      *****************************************************************
       20090-API-INIT-SEC         SECTION.
      *
           INITIALIZE                   FMYYKPTLIST-REC
      *
           MOVE    "yoyakukanjalist"   TO  FMYPL-FORM-ID
           MOVE    SYS-1031-PRTNM(19)  TO  FMYPL-PRINTER
      *    発行日
           MOVE    SPA-SYSYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMYPL-PRTYMD
      *    診療内容
           MOVE    ORCHC15-SRYNAIYO    TO  FMYPL-SRYNAIYO
           IF      ORCHC15-SRYNAIYO    =   "00"
               MOVE    ORCHC15-SRYNAIYO-MEI    TO  FMYPL-SRYNAIYONAME
           ELSE
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   TBL-SRYNAI-MAX  )
                   IF      ORCHC15-SRYNAIYO    =   TBL-SRYNAIYO (IDX-D)
                       MOVE    TBL-SRYNAIYOMEI (IDX-D)
                                               TO  FMYPL-SRYNAIYONAME
                       MOVE    TBL-SRYNAI-MAX  TO  IDX-D
                   END-IF
               END-PERFORM
           END-IF
      *    ドクター
           MOVE    ORCHC15-DRCD-G        TO  FMYPL-DR-CODE
           IF      ORCHC15-DRCD        =   "9999"
               MOVE    ORCHC15-DRCD-NAME   TO  FMYPL-DR-NAME
           ELSE
               PERFORM VARYING     IDX-D   FROM    1   BY  1
                       UNTIL      (IDX-D   >   TBL-DRCD-MAX )
                   IF      ORCHC15-DRCD    =   TBL-DRCD (IDX-D)(2:4)
                       MOVE    TBL-DRCDNAME (IDX-D)
                                               TO  FMYPL-DR-NAME
                       MOVE    TBL-DRCDKANANAME (IDX-D)
                                               TO  FMYPL-DR-KANANAME
                       MOVE    TBL-DRCD-MAX    TO  IDX-D
                   END-IF
               END-PERFORM
           END-IF
           .
       20090-API-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       20091-API-SYORI-SEC         SECTION.
      *
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMYPL-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "yoyakukanjalist"   TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "予約患者一覧"      TO  RPMAIN-TITLE
           MOVE    "fm_appointment_list_patient"
                                       TO  MCP-TABLE
           MOVE    FMYPL-DATA          TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       20091-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       3001-KNJNO-HENSYU-SEC           SECTION.
      *
      *    患者番号変換より、患者IDより患者NOを取得する
      *
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    YYK-PTID            TO  SPTID-PTID
      *
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           .
       3001-KNJNO-HENSYU-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付編集処理（API編集）
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込
      *****************************************************************
       900-YYK-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-YYK
               MOVE    MCPDATA-REC         TO  YYK-REC
           ELSE
               MOVE    1                   TO  FLG-YYK
           END-IF
      *
           .
       900-YYK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       910-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           .
       910-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者マスター読み込み
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
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   MOVE    1               TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
           END-IF
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
      *
