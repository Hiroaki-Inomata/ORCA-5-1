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
       PROGRAM-ID.             ORCHCN01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 入院登録
      *  コンポーネント名  : 入院カルテ（診療録）印刷
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/03/13    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-太田    03.05.16  病名編集をコメント化
      *  01.00.02    NACL-太田    03.07.01  出力先プリンタサブ追加
      *  03.05.00    NACL-多々納  07/05/09  グループ診療対応
      *  04.04.00    NACL-太田    08/12/25  プログラムオプション
      *  04.06.00    NACL=多々納  10/11/25  病名補足コメント対応
      *  04.06.00    NACL=多々納  11/02/03  保険適応病名判定対応
      *  04.07.00    NACL-多々納  13/10/08  第三者行為対応
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  05.00.00    NACL-太田    17/04/29  退院日編集対応
      *  05.00.00    NACL-森脇    17/07/11  API(GINBEE)対応
      *  05.00.01    NACL-太田    18/07/26  API印刷対応
      *  05.00.00    NACL-多々納  19/03/XX  元号編集対応
      *  05.01.01    NMED-太田    19/09/15  API-PDF印刷対応
      *  05.00.00    NMED-多々納  20/04/08  被保険者証の枝番対応
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
       01  PRT-REC                 PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      *     印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    カルテ
           COPY    "HCN01.INC".
      *    続紙
           COPY    "HCN011.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-LOOP-END        PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-MEI             PIC 9(06).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
           03  SYS-TIME            PIC 9(08).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDX-H                   PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-WSYSYMD.
               05  WRK-WGO             PIC X(01).
               05  WRK-WYY             PIC 9(02).
               05  WRK-WMM             PIC 9(02).
               05  WRK-WDD             PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY           PIC 9(04).
               05  WRK-SYSMM           PIC 9(02).
               05  WRK-SYSDD           PIC 9(02).
      *
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PRT-CNT             PIC 9(03).
      *
           03  WRK-SRYKA-MEI           PIC X(20).
           03  WRK-DRCD-G.
               05  WRK-DRCD-MEI        PIC X(50)  OCCURS  3.
      *----API-ADD-START---
               05  WRK-DRCD-CODE       PIC X(05)  OCCURS  3.
               05  WRK-DRCD-KANA       PIC X(50)  OCCURS  3.
      *    状態
           03  WRK-CONDITION-MEI       PIC X(20).
           03  WRK-CONDITION           PIC X(08).
           03  WRK-KANRICD             PIC X(04).
      *
           03  WRK-KEIMEI              PIC X(02).
           03  WRK-SIKUMEI             PIC X(02).
      *----API-ADD-END-----
      *
           03  WRK-ADRS                PIC X(200).
           03  WRK-NENREI-X.
               05  WRK-NENREI          PIC 9(03).
      *
      *    保険情報
           03  WRK-HKN-AREA.
               05  WRK-HKNJA           PIC X(08).
               05  WRK-KIGONUM         PIC X(80).
               05  WRK-NUM             PIC X(80).
               05  WRK-HPTNUM          PIC X(08). 
               05  WRK-FTNSNO          PIC X(20).
               05  WRK-JKYSNO          PIC X(20).
      *
               05  WRK-HKNJA-G         PIC X(16).
               05  WRK-HEN-KIGONUM     PIC X(80).
      *R02.4
      *        枝番編集
               05  WRK-EDABAN          PIC X(04).
      *
           03  WRK-MAE-INPUT           PIC X(200).
           03  WRK-OUT-INPUT           PIC X(200).
      *
           03  WRK-ZZZ9                PIC ZZZ9.
           03  WRK-Z3                  PIC ZZ9.
      *
           03  WRK-COMB-HKNNUM         PIC X(03).
           03  WRK-COMB-KOHID          PIC 9(10).
      *
           03  WRK-COMMENT             PIC X(40).
      *----API-ADD-START---
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *----API-ADD-END-----
      *
      *    保険組合せテーブル
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOMBI-OCC        OCCURS   100.
               05  TBL-HKNCOMBINUM     PIC X(04).
               05  TBL-HKNNUM          PIC X(03).
               05  TBL-KOHID           PIC 9(10).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC X(02).
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC X(02).
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
      *
      *明細最大行
       01  MAX-LINE1               PIC 9(02)   VALUE   10.
       01  MAX-LINE2               PIC 9(02)   VALUE   22.
      *
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-BYOMEIKBN          PIC X(01).
           03  PARA-SRYKAKBN           PIC X(01).
           03  PARA-UTAGAIKBN          PIC X(01).
           03  PARA-NYUGAIKBN          PIC X(01).
           03  PARA-CHARTKBN           PIC X(01).
           03  PARA-HKNCOMBIKBN        PIC X(01).
           03  PARA-DAISANKBN          PIC X(01).
           03  PARA-TAIINYMDKBN        PIC X(01).
      *----API-ADD-START---
           03  PARA-API-DATA           PIC X(01).
           03  PARA-API-CUSTOM-ID      PIC X(20).
      *----API-ADD-END-----
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    患者病名マスタ−
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1034.INC".
      *    ドクター
           COPY  "CPSK1010.INC".
      *    診療科
           COPY  "CPSK1005.INC".
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    患者登録機能制御情報
           COPY  "CPSK1017.INC".
      *----API-ADD-START---
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1018.INC".
      *
      *    入院診療録api
           COPY    "CPFMKARTE1N.INC".
      *----API-ADD-END-----
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
           COPY  "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY  "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    病名編集サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *D   COPY    "CPORCMCP.INC".
      *----API-ADD-START---
      *    保険年齢パラメタ
           COPY    "CPORCSAGECHK.INC".
      *
           COPY    "CPORCRPMAIN.INC".
      *----API-ADD-END-----
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *    入院カルテ出力パラメタ
           COPY    "CPORCHCN01.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHCN01AREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
      *    初期　処理
           PERFORM 100-INIT-SEC
      *
           EVALUATE    ORCHCN01-SYORIKBN
      *        頭書のみ
               WHEN    "1"
                   PERFORM 200-HEAD-PRT-SEC
      *        すべて
               WHEN    "2"
                   PERFORM 300-ALL-PRT-SEC
           END-EVALUATE
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
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  CNT-AREA
      *
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    ZERO                TO  WRK-PAGE
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
      *
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                  SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1017-REC
           ELSE
               INITIALIZE                  SYS-1017-REC
           END-IF
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
           MOVE    "0"             TO  PARA-BYOMEIKBN
           MOVE    "0"             TO  PARA-SRYKAKBN
           MOVE    "1"             TO  PARA-NYUGAIKBN
           MOVE    "1"             TO  PARA-UTAGAIKBN
           MOVE    "1"             TO  PARA-CHARTKBN
           MOVE    "0"             TO  PARA-TAIINYMDKBN
      *
      *    病名編集
           IF    ( SYS-1017-KARUTE-BYOMEI
                                       =   "0" OR "1" OR "2" )
               MOVE    SYS-1017-KARUTE-BYOMEI
                                       TO  PARA-BYOMEIKBN
           END-IF
           MOVE    "BYOMEIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" OR "2" )
               MOVE    WRK-PARA(1:1)   TO  PARA-BYOMEIKBN
           END-IF
      *
      *    診療科
           MOVE    "SRYKAKBN"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-SRYKAKBN
           END-IF
      *    病名疑い区分
           MOVE    "UTAGAIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-UTAGAIKBN
           END-IF
      *    入外区分
           MOVE    "NYUGAIKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-NYUGAIKBN
           END-IF
      *    カルテ病名疑い区分
           MOVE    "CHARTKBN"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-CHARTKBN
           END-IF
      *    保険適応病名区分
           MOVE    "HKNCOMBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HKNCOMBIKBN
      *
      *    第三者編集区分
           MOVE    "DAISANKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-DAISANKBN
      *
      *    退院日区分
           MOVE    "TAIINYMDKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "0" OR "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-TAIINYMDKBN
           END-IF
      *----API-ADD-START---
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-API-DATA
           IF     (PARA-API-DATA       =   "1"       )
               CONTINUE
           ELSE
      *        API送信なし
               MOVE    "0"             TO  PARA-API-DATA
           END-IF
      *
           IF     ( SPA-API-FLG    NOT =   ZERO )
               IF    ( SPA-PRTCONF     =   "2" OR "3" )
                   MOVE    ZERO        TO  PARA-API-DATA
               ELSE
                   MOVE    "1"         TO  PARA-API-DATA
               END-IF
           END-IF
      *
           IF    ( SPA-API-FLG     =   2 OR 3 )
               IF    ( SPA-SRYKA   =   SPACE )
                   MOVE    "1"     TO  PARA-SRYKAKBN
               END-IF
           END-IF
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-API-CUSTOM-ID
      *----API-ADD-END-----
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
           MOVE    "ORCHCN01"          TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
      *----API-UPD-START---
           EVALUATE    WRK-PARA-NAME
           WHEN  "API_CUSTOM_ID"
               MOVE    ORCSPRGOPTION-OT-PARA
                                   TO  WRK-PARA
           WHEN    OTHER
               IF      ORCSPRGOPTION-OT-TYPE   =   1
      *            半角で１文字目を値とする
                   MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
               END-IF
           END-EVALUATE
      *----API-UPD-END-----
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *****************************************************************
      *    頭書のみ印刷処理
      *****************************************************************
       200-HEAD-PRT-SEC                SECTION.
      *
      *　   共通印刷項目編集
            PERFORM 2001-HEAD-HENSYU-SEC
      *----API-ADD-START---
           IF      PARA-API-DATA       =   "1"
               PERFORM 710-HEAD-API-SEC
               GO  TO  200-HEAD-PRT-EXT
           END-IF
      *----API-ADD-END-----
      *     カルテ印刷
            IF      ORCHCN01-PRTKBN    =   "1"  OR  "3"
                 PERFORM 2002-HCN01-HENSYU-SEC
                 PERFORM 2003-HCN01-PRINT-SEC
            END-IF
      *
      *     続紙印刷
            IF      ORCHCN01-PRTKBN    =   "2"  OR  "3"
                 PERFORM 2004-HCN011-HENSYU-SEC
                 PERFORM 2005-HCN011-PRINT-SEC
            END-IF
      *
           .
       200-HEAD-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通印刷項目編集処理
      *****************************************************************
       2001-HEAD-HENSYU-SEC                SECTION.
      *
      *    患者マスター検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               DISPLAY "*** PTINF  INVALIE:"   PTINF-KEY
               GO      TO      2001-HEAD-HENSYU-EXT
           END-IF
      *
      *    診療科
           IF      SPA-SRYKA           =   SPACE
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    SPA-SRYKA           TO  SYS-1005-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME1
                                               TO  WRK-SRYKA-MEI
               ELSE
                   MOVE    SPACE               TO  WRK-SRYKA-MEI
               END-IF
           END-IF
      *
      *    ドクター
           INITIALIZE                  WRK-DRCD-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      ORCHCN01-DR (IDX)   NOT =   SPACE
                   MOVE    SPACE               TO  SYS-1010-REC
                   INITIALIZE                  SYS-1010-REC
                   MOVE    "1010"              TO  SYS-1010-KANRICD
                   MOVE    ORCHCN01-DR (IDX)   TO  SYS-1010-KBNCD
                   MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
                   MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
                   MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
                   MOVE    SYS-1010-REC        TO  MCPDATA-REC
                   PERFORM 900-SYSKANRI-READ-SEC
                   IF      FLG-SYSKANRI        =   ZERO
                       MOVE    SYSKANRI-REC        TO  SYS-1010-REC
                       MOVE    SYS-1010-NAME       TO  WRK-DRCD-MEI
                                                                (IDX)
      *----API-ADD-START---
                       MOVE    SYS-1010-KBNCD  TO  WRK-DRCD-CODE(IDX)
                       MOVE    SYS-1010-KANANAME
                                               TO  WRK-DRCD-KANA(IDX)
      *----API-ADD-END-----
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       2001-HEAD-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    カルテ編集処理
      *****************************************************************
       2002-HCN01-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  HCN01
           INITIALIZE                      HCN01
      *
           MOVE    SPA-PTNUM           TO  HCN01-PTNUM
      *    患者マスターより
      *    患者氏名
           MOVE    PTINF-KANANAME      TO  HCN01-KANANAME
           IF      PTINF-NAME(31:)     =   SPACE
               MOVE    PTINF-NAME          TO  HCN01-NAME
           ELSE
               MOVE    PTINF-NAME          TO  HCN01-NAME2
           END-IF
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "○"                TO  HCN01-SEX (1)
               WHEN    "2"
                   MOVE    "○"                TO  HCN01-SEX (2)
           END-EVALUATE
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:4)    TO  HCN01-BIRTHGG
           MOVE    WRK-HYY             TO  HCN01-BIRTHYY
           MOVE    WRK-HMM             TO  HCN01-BIRTHMM
           MOVE    WRK-HDD             TO  HCN01-BIRTHDD
      *　　年齢
           MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           COMPUTE WRK-NENREI          =   WRK-SYSYY
                                       -   WRK-SYY
           IF      SPA-SYSYMD(5:4)     <   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI      =   WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI          TO  WRK-Z3
           MOVE    WRK-Z3              TO  HCN01-NENREI
      *
      *    診療科
           MOVE    WRK-SRYKA-MEI       TO  HCN01-SINRYOKA
      *    ドクター
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    WRK-DRCD-MEI (IDX)  TO  HCN01-DR  (IDX)
           END-PERFORM
      *
      *    自宅−住所
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-HOME-ADRS     DELIMITED   BY  SPACE
                   PTINF-HOME-BANTI    DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HCN01-ADRS
      *    自宅−昼電話番号
           MOVE    PTINF-HOME-TEL1     TO  HCN01-TEL
      *
      *    連絡先
           MOVE    PTINF-RENRAKU-NAME  TO  HCN01-RENNAME
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-RENRAKU-ADRS  DELIMITED   BY  SPACE
                   PTINF-RENRAKU-BANTI DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HCN01-RENADRS
           MOVE    PTINF-RENRAKU-TEL1  TO  HCN01-HIRUTEL
           MOVE    PTINF-RENRAKU-TEL2  TO  HCN01-YORUTEL
      *
      *    勤務先
           MOVE    PTINF-OFFICE-NAME   TO  HCN01-KINNAME
           MOVE    SPACE               TO  WRK-ADRS
           STRING  PTINF-OFFICE-ADRS   DELIMITED   BY  SPACE
                   PTINF-OFFICE-BANTI  DELIMITED   BY  SPACE
                   INTO                    WRK-ADRS
           END-STRING
           MOVE    WRK-ADRS            TO  HCN01-KINADRS
           MOVE    PTINF-OFFICE-TEL    TO  HCN01-KINTEL
      *
      *    入院日
           IF      ORCHCN01-NYUINYMD   NOT =   SPACE
               MOVE    ORCHCN01-NYUINYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HCN01-NYUINGG
               MOVE    WRK-HYY             TO  HCN01-NYUINYY
               MOVE    WRK-HMM             TO  HCN01-NYUINMM
               MOVE    WRK-HDD             TO  HCN01-NYUINDD
           END-IF
      *
      *    退院日
           IF      PARA-TAIINYMDKBN        =   "1"
            AND    ORCHCN01-TAIINYMD   NOT =   SPACE
               MOVE    ORCHCN01-TAIINYMD   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HCN01-TAIINGG
               MOVE    WRK-HYY             TO  HCN01-TAIINYY
               MOVE    WRK-HMM             TO  HCN01-TAIINMM
               MOVE    WRK-HDD             TO  HCN01-TAIINDD
           END-IF
      *
           .
       2002-HCN01-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ印刷処理
      *****************************************************************
       2003-HCN01-PRINT-SEC                SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCN01               TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCN01.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ***** MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(1)   TO  MKPRT-PRTNM
            MOVE   "入院診療録"
                                      TO  MKPRT-INFO
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2003-HCN01-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    続紙編集処理
      *****************************************************************
       2004-HCN011-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  HCN011
           INITIALIZE                      HCN011
      *
           MOVE    SPA-PTNUM           TO  HCN011-PTNUM
      *    患者マスターより
      *    患者氏名
           IF      PTINF-NAME(31:)     =   SPACE
               MOVE    PTINF-NAME          TO  HCN011-NAME
           ELSE
               MOVE    PTINF-NAME          TO  HCN011-NAME2
           END-IF
      *
           .
       2004-HCN011-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    続紙印刷処理
      *****************************************************************
       2005-HCN011-PRINT-SEC                SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCN011              TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCN011.red"        TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ***** MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(1)   TO  MKPRT-PRTNM
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2005-HCN011-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    すべての印刷処理
      *****************************************************************
       300-ALL-PRT-SEC                SECTION.
      *
      *　  共通印刷項目編集
           PERFORM 2001-HEAD-HENSYU-SEC
      *----API-ADD-START---
           IF      PARA-API-DATA       =   "1"
               PERFORM 710-ALL-API-SEC
               GO  TO  300-ALL-PRT-EXT
           END-IF
      *----API-ADD-END-----
           PERFORM 2002-HCN01-HENSYU-SEC
      *    保険項目編集
           PERFORM 3001-HCN01-ALLHENSYU-SEC
      *
           IF   (  PARA-BYOMEIKBN          NOT =   "2" )
      *        病名を印刷する
               PERFORM 3002-BYOMEI-PRT-SEC
           ELSE
      *        カルテ印刷
               PERFORM 2003-HCN01-PRINT-SEC
           END-IF
      *
           .
       300-ALL-PRT-EXT.
           EXIT.
      *****************************************************************
      *    カルテ保険情報編集処理
      *****************************************************************
       3001-HCN01-ALLHENSYU-SEC        SECTION.
      *
      *    保険情報検索
           PERFORM 30011-HKNCOMBI-HEN-SEC
      *
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  HCN01-ZOKUGARA
      *
           EVALUATE    COMB-HONKZKKBN
           WHEN    "1"
               MOVE    "本人"          TO  HCN01-HONKZK
           WHEN    "2"
               MOVE    "家族"          TO  HCN01-HONKZK
           END-EVALUATE
      *
      *    保険者番号
           MOVE    WRK-HKNJA-G         TO  HCN01-HPTNUM
      *    記号番号
           MOVE    WRK-HEN-KIGONUM     TO  HCN01-KIGONUM
      *R02.4
      *    枝番編集
           MOVE    WRK-EDABAN          TO  HCN01-EDABAN
      *    被保険者名
           MOVE    PTHKN-HIHKNJANAME   TO  HCN01-HIHKNNAME
      *    資格取得年月日
           IF      PTHKN-SKKGETYMD     =   SPACE
               CONTINUE
           ELSE
               MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HCN01-SYUTOKGG
               MOVE    WRK-HYY             TO  HCN01-SYUTOKYY
               MOVE    WRK-HMM             TO  HCN01-SYUTOKMM
               MOVE    WRK-HDD             TO  HCN01-SYUTOKDD
           END-IF
      *    有効期限（適用終了年月日）
           IF      PTHKN-TEKEDYMD      =   SPACE  OR  ALL "9"
               CONTINUE
           ELSE
               MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG(1:4)    TO  HCN01-YUKOGG
               MOVE    WRK-HYY             TO  HCN01-YUKOYY
               MOVE    WRK-HMM             TO  HCN01-YUKOMM
               MOVE    WRK-HDD             TO  HCN01-YUKODD
           END-IF
      *
      *    事業所名
           MOVE    HKNJA-HKNJANAME     TO  HCN01-JIGYONAME
      *    事業所住所
           STRING  HKNJA-ADRS          DELIMITED  BY  SPACE
                   HKNJA-BANTI         DELIMITED  BY  SPACE
                   INTO                    HCN01-JIGYOADRS
           END-STRING
      *
      *    公費情報
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   2
               IF      COMB-KOHID (IDY)    NOT =   SPACE
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
                   MOVE    SPA-PTID            TO  PTKOH-PTID
                   MOVE    COMB-KOHID (IDY)    TO  PTKOH-KOHID
                   PERFORM 940-PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF          =   ZERO
                       PERFORM 30012-PTKOHINF-HEN-SEC
                       IF      WRK-FTNSNO(17:)     =   SPACE
                           MOVE    WRK-FTNSNO      TO  HCN01-FTNSNO
                                                                (IDY)
                       ELSE
                           MOVE    WRK-FTNSNO      TO  HCN01-FTNSNON
                                                                (IDY)
                       END-IF
                       IF      WRK-JKYSNO(15:)     =   SPACE
                           MOVE    WRK-JKYSNO      TO  HCN01-JKYSNO
                                                                (IDY)
                       ELSE
                           MOVE    WRK-JKYSNO      TO  HCN01-JKYSNON
                                                                (IDY)
                       END-IF
                   END-IF
      *            終了年月日
                   IF      PTKOH-TEKEDYMD      =   SPACE  OR  ALL "9"
                       CONTINUE
                   ELSE
                       MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
                       PERFORM 31012-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG(1:4)    TO  HCN01-KOHENDGG
                                                                (IDY)
                       MOVE    WRK-HYY             TO  HCN01-KOHENDYY
                                                                (IDY)
                       MOVE    WRK-HMM             TO  HCN01-KOHENDMM
                                                                (IDY)
                       MOVE    WRK-HDD             TO  HCN01-KOHENDDD
                                                                (IDY)
                   END-IF
               END-IF
           END-PERFORM
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-PTRSIINF        =   ZERO
      *            保険者番号へ記号（労災区分名）
                   MOVE    WRK-HEN-KIGONUM     TO  HCN01-HPTNUM
      *            記号へ患者交付番号
                   IF    ( PTRSI-KOFUNUM       =   SPACE )
                       MOVE    SPACE           TO  HCN01-KIGONUM
                   ELSE
                       MOVE    PTRSI-KOFUNUM   TO  WRK-MAE-INPUT
                       PERFORM 2009-HENKAN-SEC
                       MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HCN01-KIGONUM
                   END-IF
      *
               END-IF
           END-IF
           .
       3001-HCN01-ALLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       30011-HKNCOMBI-HEN-SEC          SECTION.
      *
           INITIALIZE                      WRK-HKN-AREA
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
           PERFORM 910-HKNCOMBI-READ-SEC
           IF      FLG-HKNCOMBI        =   1
               INITIALIZE                  HKNCOMBI-REC
           END-IF
      *
      *    患者保険情報検索
      *    主保険−ＩＤ
           INITIALIZE                      PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-PTID            TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 920-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF      NOT =   ZERO
               INITIALIZE              PTHKNINF-REC
           END-IF
      *
      *    保険者番号変換処理（右づめ）
           MOVE    SPACE               TO  WRK-HKNJA
           MOVE    9                   TO  IDY
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    8   BY  -1
                   UNTIL       IDX     <   1
               IF      PTHKN-HKNJANUM(IDX:1)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
               END-IF
               IF      FLG-OK              =   1
                   COMPUTE IDY             =   IDY     -   1
                   MOVE    PTHKN-HKNJANUM(IDX:1)   TO  WRK-HKNJA (IDY:1)
               END-IF
           END-PERFORM
           MOVE    WRK-HKNJA           TO  WRK-MAE-INPUT
           PERFORM 2009-HENKAN-SEC
           MOVE    WRK-OUT-INPUT(1:16) TO  WRK-HKNJA-G
      *
           IF      COMB-HKNNUM         =  "975"
               MOVE    COMB-SYU-TANSEIDONAME   TO  WRK-HKNJA-G
           END-IF
      *
      *    記号・番号
      *    全角変換
           IF      PTHKN-KIGO          =   SPACE
               MOVE    SPACE               TO  WRK-KIGONUM
           ELSE
               MOVE    PTHKN-KIGO          TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KIGONUM
           END-IF
           IF      PTHKN-NUM           =   SPACE
               MOVE    SPACE               TO  WRK-NUM
           ELSE
               MOVE    PTHKN-NUM           TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-NUM
           END-IF
           IF      (WRK-KIGONUM        =   SPACE )  AND
                   (WRK-NUM            =   SPACE )
               MOVE    SPACE               TO  WRK-HEN-KIGONUM
           ELSE
               STRING  WRK-KIGONUM         DELIMITED   BY  SPACE
                       "・"                DELIMITED   BY  SIZE
                       WRK-NUM             DELIMITED   BY  SPACE
                   INTO                    WRK-HEN-KIGONUM
               END-STRING
           END-IF
      *R02.4
      *    枝番編集
           IF      PTHKN-EDABAN        =   SPACE
               MOVE    SPACE               TO  WRK-EDABAN
           ELSE
               MOVE    PTHKN-EDABAN        TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                          TO  WRK-EDABAN
           END-IF
      *
      *    保険番号マスター検索
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    COMB-TEKSTYMD       TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM      NOT =   ZERO
               INITIALIZE              HKNNUM-REC
           END-IF
      *
      *    保険者番号　検索チェック
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM         TO  HKNJA-HOSPNUM
           MOVE    PTHKN-HKNJA-G       TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       30011-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者公費変換処理
      *****************************************************************
       30012-PTKOHINF-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-FTNSNO
           MOVE    SPACE               TO  WRK-JKYSNO
      *    負担者番号
           IF      PTKOH-FTNJANUM      =   SPACE
               MOVE    SPACE               TO  WRK-FTNSNO
           ELSE
               MOVE    PTKOH-FTNJANUM      TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-FTNSNO
           END-IF
      *    受給者番号１
           IF      PTKOH-JKYSNUM       =   SPACE
               MOVE    SPACE               TO  WRK-JKYSNO
           ELSE
               MOVE    PTKOH-JKYSNUM       TO  WRK-MAE-INPUT
               PERFORM 2009-HENKAN-SEC
               MOVE    WRK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-JKYSNO
           END-IF
      *ver.4.7
           IF      COMB-KOH1HKNNUM     =   "970"
      *        第三者行為
               MOVE    COMB-KOH1-TANSEIDONAME
                                           TO  WRK-FTNSNO
           END-IF
      *ver.4.7
           IF      COMB-KOH1HKNNUM     =   "970"
      *        第三者行為
               IF      PARA-DAISANKBN      =   "1"
      *            負担者番号へ
                   MOVE    COMB-KOH1-TANSEIDONAME
                                               TO  WRK-FTNSNO
                   MOVE    SPACE               TO  WRK-JKYSNO
                   IF      PTKOH-JKYSNUM   NOT =   SPACE
                       STRING  "（"
                               PTKOH-JKYSNUM
                               "）"            DELIMITED  BY  SPACE
                                               INTO    WRK-JKYSNO
                       END-STRING
                   END-IF
               ELSE
      *            備考へ
                   MOVE    SPACE               TO  WRK-FTNSNO
                   MOVE    SPACE               TO  WRK-JKYSNO
                   MOVE    SPACE               TO  WRK-COMMENT
                   IF      PTKOH-JKYSNUM       =   SPACE
                       MOVE    COMB-KOH1-TANSEIDONAME  TO  WRK-COMMENT
                   ELSE
                       STRING  COMB-KOH1-TANSEIDONAME
                               "（"
                               PTKOH-JKYSNUM
                               "）"            DELIMITED  BY  SPACE
                                           INTO    WRK-COMMENT
                       END-STRING
                   END-IF
                   MOVE    WRK-COMMENT     TO  HCN01-COMMENT
               END-IF
           END-IF
      *
      *
           .
       30012-PTKOHINF-HEN-EXT.
           EXIT.
      *****************************************************************
      *    病名印刷編集処理
      *****************************************************************
       3002-BYOMEI-PRT-SEC        SECTION.
      *
      *    保険組合せテーブル編集
           INITIALIZE                  TBL-HKNCOMBI-AREA
           IF      SPA-HKNCOMBINUM     =   ZERO
               MOVE    SPACE               TO  WRK-COMB-HKNNUM
           ELSE
               MOVE    COMB-HKNNUM         TO  WRK-COMB-HKNNUM
      *ver.4.7
      *        第三者
               IF      COMB-KOH1HKNNUM     =   "970"
                   MOVE    COMB-KOH1HKNNUM TO  WRK-COMB-HKNNUM
                   MOVE    COMB-KOH1ID     TO  WRK-COMB-KOHID
               END-IF
           END-IF
           IF     (PARA-HKNCOMBIKBN    =   "1"
                                       OR  "2" )
               PERFORM 2009-HKNCOMBI-HEN-SEC
           END-IF
      *
      *    患者病名マスタより編集
           MOVE    MAX-LINE1           TO  CNT-MEI
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PRT-CNT
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM 
                   UNTIL   FLG-PTBYOMEI    NOT =   ZERO
      *
               ADD     1               TO  IDX
               IF      WRK-PRT-CNT     =   1
                   IF      IDX             >   MAX-LINE1
      *                カルテ印刷
                       PERFORM 2003-HCN01-PRINT-SEC
      *                続紙初期編集
                       PERFORM 2004-HCN011-HENSYU-SEC
                       ADD     1               TO  WRK-PRT-CNT
                       MOVE    1               TO  IDX
                   END-IF
               ELSE
      *            続紙印刷
                   IF      IDX             >   MAX-LINE2
      *                続紙印刷
                       PERFORM 2005-HCN011-PRINT-SEC
      *                続紙初期編集
                       PERFORM 2004-HCN011-HENSYU-SEC
                       ADD     1               TO  WRK-PRT-CNT
                       MOVE    1               TO  IDX
                   END-IF
               END-IF
      *
               IF      WRK-PRT-CNT         =   1
      *            カルテ病名編集処理
                   PERFORM 3103-HCN01-BYOMEI-SET-SEC
               ELSE
      *            続紙病名編集処理
                   PERFORM 3104-HCN011-BYOMEI-SET-SEC
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-PRT-CNT     =   1
      *            カルテ印刷
               PERFORM 2003-HCN01-PRINT-SEC
           ELSE
               IF      IDX             >   ZERO
      *            続紙印刷
                   PERFORM 2005-HCN011-PRINT-SEC
               END-IF
           END-IF
      *
           .
       3002-BYOMEI-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せテーブル編集処理
      *****************************************************************
       2009-HKNCOMBI-HEN-SEC        SECTION.
      *
           INITIALIZE                      TBL-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX-H
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
      *H28.10
      *    全件対象とする
      *    MOVE    SPA-SYSYMD          TO  COMB-TEKSTYMD
      *    MOVE    SPA-SYSYMD          TO  COMB-TEKEDYMD
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      **** MOVE    "key6"              TO  MCP-PATHNAME
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"               TO  MCP-PATHNAME
               PERFORM 909-HKNCOMBI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1  )
                       OR  (IDX-H          >=  100 )
      ****     IF      COMB-DELKBN         =   SPACE
      *H28.10
      *        労災・自賠責・第三者のみすべて編集
               IF     (COMB-HKNNUM         =   "971"
                                           OR  "973"
                                           OR  "970" )
                 OR   (COMB-KOH1HKNNUM     =   "970" )
                   ADD     1                   TO  IDX-H
                   MOVE    COMB-HKNCOMBINUM    TO  TBL-HKNCOMBINUM
                                                               (IDX-H)
                   MOVE    COMB-HKNNUM         TO  TBL-HKNNUM
                                                               (IDX-H)
      *ver.4.7
      *            第三者
                   IF      COMB-KOH1HKNNUM     =   "970"
                       MOVE    COMB-KOH1HKNNUM TO  TBL-HKNNUM (IDX-H)
                       MOVE    COMB-KOH1ID     TO  TBL-KOHID  (IDX-H)
                   END-IF
               END-IF
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 909-HKNCOMBI-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2009-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カルテ病名編集処理
      *****************************************************************
       3103-HCN01-BYOMEI-SET-SEC          SECTION.
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
      *    補足コメント
           MOVE    PTBYO-HOSOKU-COMMENT    TO  SMKBYO-HOSOKU-COMMENT
           IF    ( PARA-UTAGAIKBN      =   "1" )
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
           END-IF
           CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
      *
      *    カルテ病名がある時はカルテ病名
           IF    ( PTBYO-CHARTBYOMEI   NOT =   SPACE )
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-CHARTBYOMEI   TO  SMKBYO-BYOMEI
               IF    ( PARA-CHARTKBN       =   "1" )
                   MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               END-IF
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
           END-IF
           MOVE    SMKBYO-BYOMEI       TO  HCN01-BYOMEI (IDX)
      *
      *    開始年月日（診療年月日）
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *H31.3
      *    元号
           MOVE    WRK-HGO             TO  HCN01-STRGG  (IDX)
           MOVE    WRK-HYY             TO  HCN01-STRYY  (IDX)
           MOVE    WRK-HMM             TO  HCN01-STRMM  (IDX)
           MOVE    WRK-HDD             TO  HCN01-STRDD  (IDX)
      *
      *    終了年月日（転帰年月日）
           IF      PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *H31.3
      *        元号
               MOVE    WRK-HGO             TO  HCN01-ENDGG  (IDX)
               MOVE    WRK-HYY             TO  HCN01-ENDYY  (IDX)
               MOVE    WRK-HMM             TO  HCN01-ENDMM  (IDX)
               MOVE    WRK-HDD             TO  HCN01-ENDDD  (IDX)
           END-IF
      *
           EVALUATE    PTBYO-TENKIKBN
           WHEN    "1"
               MOVE    "○"            TO  HCN01-TENKI(IDX 1)
           WHEN    "2"
               MOVE    "○"            TO  HCN01-TENKI(IDX 3)
               MOVE    "死"            TO  HCN01-TENKIMSG(IDX)
           WHEN    "3"
               MOVE    "○"            TO  HCN01-TENKI(IDX 2)
           WHEN    "8"
               MOVE    "○"            TO  HCN01-TENKI(IDX 3)
               MOVE    "移"            TO  HCN01-TENKIMSG(IDX)
           END-EVALUATE
      *
           .
       3103-HCN01-BYOMEI-SET-EXT.
           EXIT.
      *****************************************************************
      *    続紙病名編集処理
      *****************************************************************
       3104-HCN011-BYOMEI-SET-SEC          SECTION.
      *
           ADD     1                   TO  CNT-MEI
           MOVE    CNT-MEI             TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  HCN011-CD (IDX)
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
      *
      *    補足コメント
           MOVE    PTBYO-HOSOKU-COMMENT    TO  SMKBYO-HOSOKU-COMMENT
           IF    ( PARA-UTAGAIKBN      =   "1" )
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
           END-IF
           CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
      *    カルテ病名がある時はカルテ病名
           IF    ( PTBYO-CHARTBYOMEI   NOT =   SPACE )
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               MOVE    PTBYO-CHARTBYOMEI   TO  SMKBYO-BYOMEI
               IF    ( PARA-CHARTKBN   =   "1" )
                   CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
               END-IF
           END-IF
           MOVE    SMKBYO-BYOMEI       TO  HCN011-BYOMEI (IDX)
      *    開始年月日（診療年月日）
           MOVE    PTBYO-SRYYMD        TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
      *H31.3
      *    元号
           MOVE    WRK-HGO             TO  HCN011-STRGG  (IDX)
           MOVE    WRK-HYY             TO  HCN011-STRYY  (IDX)
           MOVE    WRK-HMM             TO  HCN011-STRMM  (IDX)
           MOVE    WRK-HDD             TO  HCN011-STRDD  (IDX)
      *
      *    終了年月日（転帰年月日）
           IF      PTBYO-TENKIYMD  NOT =   SPACE
               MOVE    PTBYO-TENKIYMD      TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
      *H31.3
      *        元号
               MOVE    WRK-HGO             TO  HCN011-ENDGG  (IDX)
               MOVE    WRK-HYY             TO  HCN011-ENDYY  (IDX)
               MOVE    WRK-HMM             TO  HCN011-ENDMM  (IDX)
               MOVE    WRK-HDD             TO  HCN011-ENDDD  (IDX)
           END-IF
      *
           EVALUATE    PTBYO-TENKIKBN
           WHEN    "1"
               MOVE    "○"            TO  HCN011-TENKI(IDX 1)
           WHEN    "2"
               MOVE    "○"            TO  HCN011-TENKI(IDX 3)
               MOVE    "死"            TO  HCN011-TENKIMSG(IDX)
           WHEN    "3"
               MOVE    "○"            TO  HCN011-TENKI(IDX 2)
           WHEN    "8"
               MOVE    "○"            TO  HCN011-TENKI(IDX 3)
               MOVE    "移"            TO  HCN011-TENKIMSG(IDX)
           END-EVALUATE
      *
           .
       3104-HCN011-BYOMEI-SET-EXT.
           EXIT.
      *----API-ADD-START---
      *****************************************************************
      *    ＡＰＩ処理
      *****************************************************************
       710-HEAD-API-SEC            SECTION.
      *
           PERFORM 710-API-HEAD-SYORI-SEC
      *
           PERFORM 790-API-CALL-SEC
           .
       710-HEAD-API-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ処理
      *****************************************************************
       710-ALL-API-SEC            SECTION.
      *
           PERFORM 710-API-HEAD-SYORI-SEC
           PERFORM 710-API-PTINF-SYORI-SEC
      *
           IF      SPA-HKNCOMBINUM         NOT =  SPACE
               PERFORM 710-API-PTHKN-SYORI-SEC
           END-IF
      *
           IF   (  PARA-BYOMEIKBN          NOT =   "2" )
      *        病名を印刷する
               PERFORM 710-API-BYOMEIHEN-SEC
           END-IF
      *
           PERFORM 790-API-CALL-SEC
           .
       710-ALL-API-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ処理
      *****************************************************************
       710-API-HEAD-SYORI-SEC     SECTION.
      *
           INITIALIZE                      FMKARTE1N-REC
      *    印刷処理区分
           MOVE    ORCHCN01-PRTKBN     TO  FMKARTE1N-HAKKOKBN
           MOVE    "karte_no1_n"       TO  FMKARTE1N-FORM-ID
           MOVE    SYS-1034-PRTNM(1)   TO  FMKARTE1N-PRINTER
      *
      *    医療機関名
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                      SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                      SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPNAME   TO  FMKARTE1N-HOSPNAME
      *
      *    患者マスターより
           MOVE    SPA-PTNUM           TO  FMKARTE1N-PTNUM
           MOVE    PTINF-KANANAME      TO  FMKARTE1N-KANANAME
           MOVE    PTINF-NAME          TO  FMKARTE1N-NAME
           MOVE    PTINF-SEX           TO  FMKARTE1N-SEX
      *
      *    生年月日
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMKARTE1N-BIRTHDAY
      *
           .
       710-API-HEAD-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ処理
      *****************************************************************
       710-API-PTINF-SYORI-SEC     SECTION.
      *
      *    年齢
           MOVE    SPA-SYSYMD          TO  WRK-SYSYMD
           COMPUTE WRK-NENREI          =   WRK-SYSYY
                                       -   WRK-SYY
           IF      SPA-SYSYMD(5:4)     <   WRK-SYMD(5:4)
               COMPUTE WRK-NENREI      =   WRK-NENREI   -   1
           END-IF
           MOVE    WRK-NENREI          TO  FMKARTE1N-HOKEN-AGE
      *
      *    診療科
           MOVE    SPA-SRYKA           TO  FMKARTE1N-SRYKA
           MOVE    WRK-SRYKA-MEI       TO  FMKARTE1N-SRYKANAME
      *    ドクター
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               MOVE    WRK-DRCD-CODE(IDX)  TO  FMKARTE1N-DR-CODE(IDX)
               MOVE    WRK-DRCD-MEI(IDX)   TO  FMKARTE1N-DR-NAME(IDX)
               MOVE    WRK-DRCD-KANA(IDX)
                                       TO  FMKARTE1N-DR-KANANAME(IDX)
           END-PERFORM
      *
      *    自宅−住所
           MOVE    PTINF-HOME-POST     TO  FMKARTE1N-HOME-POST
           MOVE    PTINF-HOME-ADRS     TO  FMKARTE1N-HOME-ADRS(1)
           MOVE    PTINF-HOME-BANTI    TO  FMKARTE1N-HOME-ADRS(2)
           MOVE    PTINF-HOME-TEL1     TO  FMKARTE1N-HOME-TEL1
           MOVE    PTINF-HOME-TEL2     TO  FMKARTE1N-HOME-TEL2
      *
      *    状態名称１〜３
           MOVE    PTINF-CONDITION-1   TO  WRK-CONDITION
           MOVE    "1018"              TO  WRK-KANRICD
           PERFORM 200190-API-CONDITION-SEC
           MOVE    WRK-CONDITION-MEI   TO  FMKARTE1N-CONDITION(1)
           MOVE    PTINF-CONDITION-2   TO  WRK-CONDITION
           MOVE    "1019"              TO  WRK-KANRICD
           PERFORM 200190-API-CONDITION-SEC
           MOVE    WRK-CONDITION-MEI   TO  FMKARTE1N-CONDITION(2)
           MOVE    PTINF-CONDITION-3   TO  WRK-CONDITION
           MOVE    "1020"              TO  WRK-KANRICD
           PERFORM 200190-API-CONDITION-SEC
           MOVE    WRK-CONDITION-MEI   TO  FMKARTE1N-CONDITION(3)
      *
      *    勤務先
           MOVE    PTINF-OFFICE-NAME   TO  FMKARTE1N-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  FMKARTE1N-OFFICE-POST
           MOVE    PTINF-OFFICE-ADRS   TO  FMKARTE1N-OFFICE-ADRS(1)
           MOVE    PTINF-OFFICE-BANTI  TO  FMKARTE1N-OFFICE-ADRS(2)
           MOVE    PTINF-OFFICE-TEL    TO  FMKARTE1N-OFFICE-TEL
      *    連絡先
           MOVE    PTINF-RENRAKU-NAME  TO  FMKARTE1N-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  FMKARTE1N-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  FMKARTE1N-RENRAKU-POST
           MOVE    PTINF-RENRAKU-ADRS  TO  FMKARTE1N-RENRAKU-ADRS(1)
           MOVE    PTINF-RENRAKU-BANTI TO  FMKARTE1N-RENRAKU-ADRS(2)
           MOVE    PTINF-RENRAKU-TEL1  TO  FMKARTE1N-RENRAKU-TEL1
           MOVE    PTINF-RENRAKU-TEL2  TO  FMKARTE1N-RENRAKU-TEL2
      *    帰省先
           MOVE    PTINF-KISEI-NAME    TO  FMKARTE1N-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  FMKARTE1N-KISEI-POST
           MOVE    PTINF-KISEI-ADRS    TO  FMKARTE1N-KISEI-ADRS(1)
           MOVE    PTINF-KISEI-BANTI   TO  FMKARTE1N-KISEI-ADRS(2)
           MOVE    PTINF-KISEI-TEL     TO  FMKARTE1N-KISEI-TEL
      *
      *    続柄
           MOVE    PTINF-ZOKUGARA      TO  FMKARTE1N-ZOKUGARA
      *    職業
           MOVE    PTINF-JOB           TO  FMKARTE1N-JOB
      *    世帯主
           MOVE    PTINF-SETAINUSI     TO  FMKARTE1N-SETAINUSI
      *    登録日
           MOVE    PTINF-CREYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMKARTE1N-CREYMD
      *
      *    備考禁忌編集
           MOVE    PTINF-TABOO-1       TO  FMKARTE1N-TABOO(1)
           MOVE    PTINF-TABOO-2       TO  FMKARTE1N-TABOO(2)
      *    備考アレルギー編集
           MOVE    PTINF-ALLERGY-1     TO  FMKARTE1N-ALLERGY(1)
           MOVE    PTINF-ALLERGY-2     TO  FMKARTE1N-ALLERGY(2)
      *    備考感染症編集
           MOVE    PTINF-KANSENSYO-1   TO  FMKARTE1N-KANSENSYO(1)
           MOVE    PTINF-KANSENSYO-2   TO  FMKARTE1N-KANSENSYO(2)
      *    備考コメント編集
           MOVE    PTINF-COMMENT-1     TO  FMKARTE1N-COMMENT(1)
           MOVE    PTINF-COMMENT-2     TO  FMKARTE1N-COMMENT(2)
      *
      *    入院日
           IF      ORCHCN01-NYUINYMD   NOT =   SPACE
               MOVE    ORCHCN01-NYUINYMD   TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  FMKARTE1N-NYUINYMD
           END-IF
      *    退院日
           IF      PARA-TAIINYMDKBN        =   "1"
            AND    ORCHCN01-TAIINYMD   NOT =   SPACE
               MOVE    ORCHCN01-TAIINYMD   TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE        TO  FMKARTE1N-TAIINYMD
           END-IF
           .
       710-API-PTINF-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ処理
      *****************************************************************
       710-API-PTHKN-SYORI-SEC     SECTION.
      *
      *    保険情報検索
           PERFORM 30011-HKNCOMBI-HEN-SEC
      *
           MOVE    SPA-HKNCOMBINUM     TO  FMKARTE1N-HKNCOMBI
      *    負担割合
           MOVE    COMB-GAI-KFTNRATE   TO  FMKARTE1N-GAI-KFTNRATE
           MOVE    COMB-NYU-KFTNRATE   TO  FMKARTE1N-NYU-KFTNRATE
      *    保険年齢
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    "1"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-FTNRATE  NOT =   ZERO
      *        負担率
               MOVE    AGECHK-FTNRATE      TO  FMKARTE1N-HOKEN-RATE
           END-IF
      *    年齢
           MOVE    AGECHK-NENREI       TO  FMKARTE1N-HOKEN-AGE
      *
      *    保険情報
           MOVE    COMB-HKNNUM         TO  FMKARTE1N-HKNNUM
      *    短縮制度名
           MOVE    COMB-SYU-TANSEIDONAME
                                       TO  FMKARTE1N-HKNNAME
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    COMB-HKNNUM         TO  HKN-HKNNUM
           MOVE    "00"                TO  HKN-PAYKBN
           MOVE    PTHKN-TEKSTYMD      TO  HKN-TEKSTYMD
           PERFORM 930-HKNNUM-READ-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    HKN-TANSEIDONAME
                                       TO  FMKARTE1N-HKNNAME
           END-IF
      *    保険者番号
           MOVE    PTHKN-HKNJANUM      TO  FMKARTE1N-HKNJANUM
      *    記号・番号
           MOVE    PTHKN-KIGO          TO  FMKARTE1N-KIGO
           MOVE    PTHKN-NUM           TO  FMKARTE1N-NUM
      *R02.4
      *    枝番
           MOVE    PTHKN-EDABAN        TO  FMKARTE1N-EDABAN
      *    継続区分
           MOVE    PTHKN-CONTKBN       TO  FMKARTE1N-CONTKBN
      *    補助区分
           MOVE    PTHKN-HOJOKBN       TO  FMKARTE1N-HOJOKBN
      *    本人家族区分
           MOVE    PTHKN-HONKZKKBN     TO  FMKARTE1N-HONKZKKBN
      *    資格取得日
           MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMKARTE1N-SKKGETYMD
      *    適用開始年月日
           MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMKARTE1N-TEKSTYMD
      *    適用終了年月日
           MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMKARTE1N-TEKEDYMD
      *    保険者情報
           MOVE    HKNJA-HKNJANAME     TO  FMKARTE1N-HKNJANAME
           MOVE    HKNJA-ADRS          TO  FMKARTE1N-HKNJA-ADRS(1)
           MOVE    HKNJA-BANTI         TO  FMKARTE1N-HKNJA-ADRS(2)
           MOVE    HKNJA-TEL           TO  FMKARTE1N-HKNJA-TEL
      *    被保険者名
           MOVE    PTHKN-HIHKNJANAME   TO  FMKARTE1N-HIHKNJANAME
      *
      *    労災の時、保険者番号・記号の変更
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    SPA-PTID            TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-PTRSIINF        =   ZERO
                   MOVE    PTRSI-HKNKBN    TO  FMKARTE1N-RSI-HKNKBN
      *            患者交付番号・年金証書番号・健康管理手帳番号
                   MOVE    PTRSI-KOFUNUM   TO  FMKARTE1N-RSI-KOFUNUM
      *            事業所名称
                   MOVE    PTRSI-JIGYOUNAME
                                           TO  FMKARTE1N-RSI-JIGYOUNAME
      *            所在地都道府県名
                   PERFORM 200191-RSI-PREFNAME-SEC
      *            所在地都道府県名
                   STRING  PTRSI-PREFNAME
                           WRK-KEIMEI      DELIMITED  BY  SPACE
                                           INTO    FMKARTE1N-PREFNAME
                   END-STRING
      *            所在地郡市区名
                   STRING  PTRSI-CITYNAME
                           WRK-SIKUMEI     DELIMITED  BY  SPACE
                                           INTO    FMKARTE1N-CITYNAME
                   END-STRING
      *            傷病年月日
                   MOVE    PTRSI-SHOBYOYMD     TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE
                                       TO  FMKARTE1N-RSI-SHOBYOYMD
                   IF      PTRSI-HKNKBN    =   "3"
      *            アフターケア
      *                療養開始日
                       MOVE    PTRSI-SINSATUYMD    TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  FMKARTE1N-RSI-RYOSTYMD
      *                療養終了日
                       MOVE    PTRSI-SINSATUEND    TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  FMKARTE1N-RSI-RYOEDYMD
                   ELSE
      *                療養開始日
                       MOVE    PTRSI-RYOSTYMD      TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  FMKARTE1N-RSI-RYOSTYMD
      *                療養終了日
                       MOVE    PTRSI-RYOEDYMD      TO  WRK-SYMD
                       PERFORM 801-DAYHEN01-SEC
                       MOVE    WRK-HEN-DATE
                                           TO  FMKARTE1N-RSI-RYOEDYMD
                   END-IF
               END-IF
           END-IF
      *
      *    公費情報
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   2
               IF      COMB-KOHID(IDY)     NOT =   SPACE
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
                   MOVE    SPA-PTID            TO  PTKOH-PTID
                   MOVE    COMB-KOHID(IDY)     TO  PTKOH-KOHID
                   PERFORM 940-PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF          =   ZERO
                       MOVE    COMB-KOHHKNNUM(IDY)
                                   TO  FMKARTE1N-KOH-HKNNUM(IDY)
                       MOVE    COMB-KOH-TANSEIDONAME(IDY)
                                   TO  FMKARTE1N-KOH-TANSEIDONAME(IDY)
                       MOVE    PTKOH-FTNJANUM 
                                   TO  FMKARTE1N-KOH-FTNJANUM(IDY)
                       MOVE    PTKOH-JKYSNUM 
                                   TO  FMKARTE1N-KOH-JKYSNUM(IDY)
                   END-IF
      *            開始年月日
                   MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE
                                   TO  FMKARTE1N-KOH-TEKSTYMD(IDY)
      *            終了年月日
                   MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE
                                   TO  FMKARTE1N-KOH-TEKEDYMD(IDY)
               END-IF
           END-PERFORM
           .
       710-API-PTHKN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ病名編集処理
      *****************************************************************
       710-API-BYOMEIHEN-SEC         SECTION.
      *
      *    保険組合せテーブル編集
           INITIALIZE                  TBL-HKNCOMBI-AREA
           IF      SPA-HKNCOMBINUM     =   ZERO
               MOVE    SPACE               TO  WRK-COMB-HKNNUM
           ELSE
               MOVE    COMB-HKNNUM         TO  WRK-COMB-HKNNUM
      *ver.4.7
      *        第三者
               IF      COMB-KOH1HKNNUM     =   "970"
                   MOVE    COMB-KOH1HKNNUM TO  WRK-COMB-HKNNUM
                   MOVE    COMB-KOH1ID     TO  WRK-COMB-KOHID
               END-IF
           END-IF
           IF     (PARA-HKNCOMBIKBN    =   "1"
                                       OR  "2" )
               PERFORM 2009-HKNCOMBI-HEN-SEC
           END-IF
      *
      *    患者病名マスタより編集
           MOVE    MAX-LINE1           TO  CNT-MEI
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PRT-CNT
      *
           INITIALIZE                      PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
           END-IF
           PERFORM 
                   UNTIL   FLG-PTBYOMEI    NOT =   ZERO
      *
               ADD     1               TO  IDX
      *
      *        病名編集
               INITIALIZE                  ORCSMKBYOMEIAREA
               MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
      *        補足コメント
               MOVE    PTBYO-HOSOKU-COMMENT
                                           TO  SMKBYO-HOSOKU-COMMENT
               IF    ( PARA-UTAGAIKBN      =   "1" )
                   MOVE    PTBYO-UTAGAIFLG     TO  SMKBYO-UTAGAIFLG
               END-IF
               CALL    "ORCSMKBYOMEI"  USING   ORCSMKBYOMEIAREA
      *        カルテ病名がある時はカルテ病名
               IF    ( PTBYO-CHARTBYOMEI   NOT =   SPACE )
                   INITIALIZE                      ORCSMKBYOMEIAREA
                   MOVE    PTBYO-CHARTBYOMEI   TO  SMKBYO-BYOMEI
                   IF    ( PARA-CHARTKBN       =   "1" )
                       MOVE    PTBYO-UTAGAIFLG TO  SMKBYO-UTAGAIFLG
                   END-IF
                   CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
               END-IF
               MOVE    SMKBYO-BYOMEI   TO  FMKARTE1N-BYOMEI (IDX)
      *
      *        主病区分
               MOVE    PTBYO-SYUBYOFLG TO  FMKARTE1N-SYUBYOFLG(IDX)
      *        疑い区分
               MOVE    PTBYO-UTAGAIFLG TO  FMKARTE1N-UTAGAIFLG(IDX)
      *
      *        開始年月日（診療年月日）
               MOVE    PTBYO-SRYYMD    TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  FMKARTE1N-SRYYMD(IDX)
      *        転帰日付
               MOVE    PTBYO-TENKIYMD  TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  FMKARTE1N-TENKIYMD(IDX)
      *        転帰区分
               MOVE    PTBYO-TENKIKBN  TO  FMKARTE1N-TENKIKBN(IDX)
      *
               MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
               MOVE    "key3"          TO  MCP-PATHNAME
               PERFORM 950-PTBYOMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"  TO  MCP-TABLE
           MOVE    "key3"          TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       710-API-BYOMEIHEN-EXT.
           EXIT.
      *****************************************************************
      *    状態名称編集処理
      *****************************************************************
       200190-API-CONDITION-SEC        SECTION.
      *
           IF      WRK-CONDITION       =   SPACE
                                       OR  ZERO
               MOVE    "該当なし"          TO  WRK-CONDITION-MEI
           ELSE
               MOVE    SPACE               TO  SYS-1018-REC
               INITIALIZE                      SYS-1018-REC
               MOVE    WRK-KANRICD         TO  SYS-1018-KANRICD
               MOVE    WRK-CONDITION       TO  SYS-1018-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1018-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1018-EDYUKYMD
               MOVE    SPA-HOSPNUM         TO  SYS-1018-HOSPNUM
               MOVE    SYS-1018-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    SYSKANRI-REC        TO  SYS-1018-REC
                   MOVE    SYS-1018-CONDITION-BODY
                                               TO  WRK-CONDITION-MEI
               ELSE
                   MOVE    "該当なし"          TO  WRK-CONDITION-MEI
               END-IF
           END-IF
           .
       200190-API-CONDITION-EXT.
           EXIT.
      *****************************************************************
      *    都道府県名編集処理
      *****************************************************************
       200191-RSI-PREFNAME-SEC        SECTION.
      *
           EVALUATE    PTRSI-PREFKBN
               WHEN    "1"
                   MOVE    "都"            TO  WRK-KEIMEI
               WHEN    "2"
                   MOVE    "道"            TO  WRK-KEIMEI
               WHEN    "3"
                   MOVE    "府"            TO  WRK-KEIMEI
               WHEN    "4"
                   MOVE    "県"            TO  WRK-KEIMEI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-KEIMEI
           END-EVALUATE
      *
           EVALUATE    PTRSI-CITYKBN
               WHEN    "1"
                   MOVE    "郡"            TO  WRK-SIKUMEI
               WHEN    "2"
                   MOVE    "市"            TO  WRK-SIKUMEI
               WHEN    "3"
                   MOVE    "区"            TO  WRK-SIKUMEI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-SIKUMEI
           END-EVALUATE
           .
       200191-RSI-PREFNAME-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       790-API-CALL-SEC         SECTION.
      *
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMKARTE1N-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "karte_no1_n"       TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "カルテ１号紙（入院）"
                                       TO  RPMAIN-TITLE
           MOVE    "fm_karte_no1_h"
                                       TO  MCP-TABLE
           MOVE    FMKARTE1N-DATA      TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       790-API-CALL-EXT.
           EXIT.
      *****************************************************************
      *    日付変換処理
      *****************************************************************
       801-DAYHEN01-SEC            SECTION.
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
           .
       801-DAYHEN01-EXT.
           EXIT.
      *----API-ADD-END-----
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
      *
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全角変換処理
      *****************************************************************
       2009-HENKAN-SEC                 SECTION.
     *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC          =   ZERO )  AND
                  (KANACHK-MAX         >   ZERO )
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-OUT-INPUT
           ELSE
               MOVE    WRK-MAE-INPUT       TO  WRK-OUT-INPUT
           END-IF
      *
           .
       2009-HENKAN-EXT.
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
      *    保険組合せマスター読み込み
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
                   MOVE    ZERO            TO  FLG-HKNCOMBI
               ELSE
                   MOVE    1               TO  FLG-HKNCOMBI
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
     *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       920-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTHKNINF-REC
                   MOVE    ZERO            TO  FLG-PTHKNINF
               ELSE
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       920-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読み込み
      *****************************************************************
       930-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  HKNNUM-REC
                   MOVE    ZERO            TO  FLG-HKNNUM
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
     *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       930-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費マスター読み込み
      *****************************************************************
       940-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
     *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名マスター読み込み
      *****************************************************************
       950-PTBYOMEI-READ-SEC         SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-LOOP-END
      *
           PERFORM UNTIL ( FLG-PTBYOMEI    NOT =   ZERO )
                    OR   ( FLG-LOOP-END    NOT =   ZERO )
      *
               MOVE    "DBFETCH"           TO  MCP-FUNC
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
                   MOVE    ZERO            TO  FLG-PTBYOMEI
      *
                   MOVE    1               TO  FLG-LOOP-END
      *
                   EVALUATE    PARA-BYOMEIKBN
                   WHEN    "0"
                       CONTINUE
                   WHEN    "1"
                       IF    ( PTBYO-TENKIYMD  =   SPACE)
                           CONTINUE
                       ELSE
                           MOVE    ZERO        TO  FLG-LOOP-END
                       END-IF
                   WHEN    "2"
                           MOVE    ZERO        TO  FLG-LOOP-END
                   END-EVALUATE
      *
                   EVALUATE    PARA-SRYKAKBN
                   WHEN    "0"
                       IF    ( SPA-SRYKA   =  PTBYO-SRYKA )
                           CONTINUE
                       ELSE
                           MOVE    ZERO    TO  FLG-LOOP-END
                       END-IF
                   WHEN    "1"
                       CONTINUE
                   END-EVALUATE
      *
                   EVALUATE    PARA-NYUGAIKBN
                   WHEN    "0"
                       CONTINUE
                   WHEN    "1"
                       IF    ( PTBYO-NYUGAIKBN =   SPACE OR "1" )
                           CONTINUE
                       ELSE
                           MOVE    ZERO        TO  FLG-LOOP-END
                       END-IF
                   END-EVALUATE
      *
      *            保険適応病名判定
                   IF     (FLG-LOOP-END        =   1     )  AND
                          (PARA-HKNCOMBIKBN    =   "1"  OR  "2")
                       PERFORM 20024-HKNCOMBI-CHK-SEC
                   END-IF
      *
               ELSE
                   MOVE    1               TO  FLG-PTBYOMEI
               END-IF
      *
      *
           END-PERFORM
      *
      *
           .
       950-PTBYOMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    保険適応病名判定処理
      *****************************************************************
       20024-HKNCOMBI-CHK-SEC          SECTION.
      *
           EVALUATE    PARA-HKNCOMBIKBN
           WHEN    "1"
      *        労災・自賠責とその他保険
               IF      WRK-COMB-HKNNUM         =   "971"
                                               OR  "973"
                                               OR  "970"
      *            保険組合せ一致のみ
                   IF      SPA-HKNCOMBINUM     =   PTBYO-HKNCOMBI
                       CONTINUE
                   ELSE
                       MOVE    ZERO            TO  FLG-LOOP-END
                   END-IF
      *ver.47
      *            第三者行為は、公費ＩＤ一致
                   IF      WRK-COMB-HKNNUM     =   "970"
                       MOVE    ZERO                TO  FLG-LOOP-END
                       IF      PTBYO-KOHID         =   WRK-COMB-KOHID
                           MOVE    1                   TO  FLG-LOOP-END
                       END-IF
                   END-IF
               ELSE
      *            労災・自賠責は対象外
                   PERFORM VARYING IDX-H   FROM    1   BY  1
                           UNTIL   (IDX-H          >   100 )  OR
                                   (TBL-HKNCOMBINUM (IDX-H)  =   SPACE)
                       IF      PTBYO-HKNCOMBI      =   TBL-HKNCOMBINUM
                                                               (IDX-H)
                           IF      TBL-HKNNUM (IDX-H)  =   "971"
                                                       OR  "973"
                                                       OR  "970"
                               MOVE    ZERO            TO  FLG-LOOP-END
                           END-IF
                           MOVE    100                 TO  IDX-H
                       END-IF
                   END-PERFORM
      *ver.47
      *            第三者行為分は対象外
                   IF      PTBYO-KOHID     NOT =   ZERO
                       MOVE    ZERO            TO  FLG-LOOP-END
                   END-IF
               END-IF
           WHEN    "2"
      *        保険組合せ一致のみ
               IF      SPA-HKNCOMBINUM     =   PTBYO-HKNCOMBI
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-LOOP-END
               END-IF
      *ver.47
      *        第三者行為は、公費ＩＤ一致
               IF      WRK-COMB-HKNNUM     =   "970"
                   MOVE    ZERO            TO  FLG-LOOP-END
                   IF      PTBYO-KOHID         =   WRK-COMB-KOHID
                       MOVE    1               TO  FLG-LOOP-END
                   END-IF
               END-IF
           END-EVALUATE
           .
       20024-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主キー管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC           SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO            TO  FLG-HKNJAINF
           ELSE
               INITIALIZE              HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTRSIINF-REC
               MOVE    ZERO            TO  FLG-PTRSIINF
           ELSE
               INITIALIZE                  PTRSIINF-REC
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
      *
           .
       993-PTRSIINF-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せマスター読み込み
      *****************************************************************
       909-HKNCOMBI-NEXT-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           .
       909-HKNCOMBI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
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
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
