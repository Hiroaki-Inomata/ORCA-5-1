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
       PROGRAM-ID.             ORCHC33.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会
      *  コンポーネント名  : カルテ３号様式（診療録）印刷（外来）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/07/04    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     NACL-多々納  04/12/15  追記印刷・日集計追加他
      * 01.00.02     NACL-多々納  06/05/15  MONFUNC 対応
      * 03.04.01     NACL-森脇    07/02/08  平成１９年４月自費消費税表示変更
      * 03.04.02     NACL-多々納  07/02/28  包括診療追加対応
      * 03.05.00     NACL-多々納  07/04/XX  グループ診療対応
      * 04.01.00     NACL-多々納  07/10/23  公務災害対応
      * 04.07.00     NACL-多々納  12/09/25  プログラムオプション保険対応
      * 04.07.01     NACL-門脇    13/09/30  自賠責（第三者行為）対応
      * 04.07.00     NACL-多々納  14/02/13  労災金額％加算対応
      * 04.08.00     NACL-多々納  14/07/30  一時ディレクトリ対応
      * 05.00.00     NACL-太田    17/11/24  API対応
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
      *    カルテ３号様式
           COPY    "HCM33.INC".
      *
      *    カルテ３号様式（続き）
           COPY    "HCM331.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARI-RSI         PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-HKNKBN              PIC 9(01).
      *
           03  FLG-GENTEN              PIC 9(01).
           03  FLG-HC331               PIC 9(01).
           03  FLG-HC331-END           PIC 9(01).
           03  FLG-TUKI-END            PIC 9(01).
      *
           03  FLG-NEXT-YY             PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
      *
           03  FLG-KAGEN           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(06).
           03  CNT-MEI             PIC 9(06).
           03  CNT-API             PIC 9(06).
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
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-T1              PIC 9(04).
           03  IDX-T2              PIC 9(04).
      *
           03  IDXH                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
           03  IDX-RSI             PIC 9(04).
      *
           03  IDZ                 PIC 9(04).
           03  IDX-D               PIC 9(02).
      *
           03  IDX-C               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
      *
           03  WRK-YMDS1.
               05  WRK-YYS1            PIC 9(04).
               05  WRK-MMS1            PIC 9(02).
               05  WRK-DDS1            PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2            PIC 9(04).
               05  WRK-MMS2            PIC 9(02).
               05  WRK-DDS2            PIC 9(02).
           03  WRK-NENREI-G.
               05  WRK-NENREI-YY       PIC 9(03).
               05  WRK-NENREI-MM       PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
           03  WRK-SRYYM-END.
               05  WRK-SRYYY-END       PIC 9(04).
               05  WRK-SRYMM-END       PIC 9(02).
           03  WRK-BIRTHDAY            PIC X(30).
      *
           03  WRK-SRYYMD              PIC X(08).
      *
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z9                  PIC Z9.
           03  WRK-Z88-X.
               05  WRK-Z59-FILLER              PIC X(03).
               05  WRK-Z59-X.
                   07  WRK-Z59                 PIC ZZZZZ.
                   07  WRK-Z59-R               REDEFINES   WRK-Z59
                                               PIC -----.
           03  WRK-Z89-X.
               05  WRK-Z89                 PIC ZZZZZZZZ.
               05  WRK-Z79S                REDEFINES   WRK-Z89
                                           PIC --------.
      *
           03  WRK-SRYYM-N             PIC X(40).
           03  WRK-SRYYM-N1            PIC X(40).
           03  WRK-SRYYM-N2            PIC X(40).
      *
           03  WRK-HKNCOMBI-AREA.
               05  WRK-HKNCOMBIMEI         PIC X(120).
               05  WRK-HEN-HKNCOMBIMEI     PIC X(120).
               05  WRK-MEI-1               PIC X(02).
               05  WRK-MEI-2               PIC X(02).
               05  WRK-MEI-3               PIC X(02).
               05  WRK-MEI-4               PIC X(02).
               05  WRK-MEI-5               PIC X(02).
               05  WRK-HONKZKKBN           PIC X(06).
      *
           03  WRK-ORC-DBPATH          PIC X(64).
           03  WRK-SRYKA-MEI           PIC X(20).
      *
           03  WRK-SYMD.
               05  WRK-SYY             PIC X(4).
               05  WRK-SMM             PIC X(2).
               05  WRK-SDD             PIC X(2).
           03  WRK-HEN-DAY.
               05  WRK-H-MM            PIC Z9.
               05  WRK-H-P             PIC X(01).
               05  WRK-H-DD            PIC Z9.
           03  WRK-DAY                 PIC 9(02).
      *     集計
           03  WRK-GOK-ZAITEN          PIC S9(07).
           03  WRK-GOK-JIHIKEI         PIC 9(07).
           03  WRK-JIHIKEI             PIC 9(07).
           03  WRK-SKYMONEY            PIC S9(07).
           03  WRK-GOK-SKYMONEY        PIC S9(07).
           03  WRK-GOK-RSI-MONEY       PIC S9(07).
      *
           03  WRK-SUM-TEN             PIC 9(07).
           03  WRK-ZAITEN              PIC S9(07).
           03  WRK-RSI-MONEY           PIC 9(07).
      *
           03  WRK-TUK-ZAITEN          PIC 9(07).
           03  WRK-TUK-SKYMONEY        PIC S9(07).
           03  WRK-TUK-JIHIKEI         PIC S9(07).
           03  WRK-TUK-RSI-MONEY       PIC 9(07).
      *
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PRT-CNT             PIC 9(03).
      *H24.9
           03  WRK-PARA-HKNNUM         PIC X(03).
           03  WRK-CHK-HKNCOMBI        PIC X(04).
           03  WRK-ENDPAGE             PIC 9(03).
      *
      *    集計テーブル
       01  TBL-AREA.
      *?   03  TBL-OCC-1               OCCURS   10.
           03  TBL-OCC-1               OCCURS   11.
      *        剤点数
               05  TBL-OCC-2           OCCURS   400.
                   07  TBL-ZAITEN          PIC S9(07).
                   07  TBL-OCC-3           OCCURS   31.
                       09  TBL-DAY         PIC 9(03).
      *    集計テーブル（労災円）
       01  TBL-RSI-AREA.
           03  TBL-RSI-OCC-1               OCCURS   11.
      *        剤点数
               05  TBL-RSI-OCC-2               OCCURS   400.
                   07  TBL-RSI-ZAITEN          PIC S9(09).
                   07  TBL-RSI-OCC-3           OCCURS   31.
                       09  TBL-RSI-DAY         PIC 9(03).
      *
      *
      *    保険テーブル
       01  TBL-HKNCOMBI-AREA.
           03  TBL-HKNCOMBI-OCC        OCCURS   20.
               05  TBL-HKNCOMBI            PIC X(04).
               05  TBL-HKNCOMBIMEI         PIC X(100).
               05  TBL-HONKZKKBN           PIC X(04).
               05  TBL-HKNKBN              PIC 9(01).
      *
      *    日毎集計テーブル
       01  TBL-DAY-AREA.
           03  TBL-DAY-OCC               OCCURS   10.
      *        剤点数
               05  TBL-DAY-ZAITEN          PIC S9(09).
      *        労災円
               05  TBL-DAY-RSI-ZAITEN      PIC S9(09).
      *
      *    帳票編集テーブル
       01  TBL-HEN-AREA.
           03  TBL-HEN-OCC            OCCURS   400.
               05  TBL-HEN-OCC2-G.
                   07  TBL-HEN-OCC2            OCCURS   10.
                       09  TBL-NAIYO           PIC X(08).
               05  TBL-MEISYO              PIC X(12).
      *
      *    対象保険組合せテーブル
       01  TBL-ALL-HKNCOMBI-AREA.
           03  TBL-ALL-HKNCOMBI-OCC        OCCURS   99.
               05  TBL-ALL-HKNCOMBINUM     PIC X(04).
               05  TBL-ALL-HKNNUM          PIC X(03).
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
      *----(03.04.01)--ADD-START---
       01  CONTS-AREA.
           03  CONST-H190401       PIC X(08)   VALUE   "20070401".
      *----(03.04.01)--ADD-END-----
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-HKNCOMBIKBN        PIC X(01).
           03  PARA-API-DATA           PIC X(1).
           03  PARA-API-CUSTOM-ID      PIC X(30).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
      *
       01  API-AREA.
           03  API-ROW.
               05  API-SRYYMD.
                   07  API-SRYYM       PIC X(06).
                   07  API-SRYDD       PIC X(02).
               05  API-NAIYO           PIC X(08)
                                       OCCURS 10.
               05  API-BIKOU           PIC X(08).
               05  API-ENDPAGE-G.
                   07  API-ENDPAGE     PIC ZZ9.
               05  API-ENDLINE-G.
                   07  API-ENDLINE     PIC Z9.
      *
       01  CONST-AREA.
           03  CONST-API-BODY-MAX      PIC 9(05) VALUE 400.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    出力先プリンタ名割り当て情報
           COPY  "CPSK1031.INC".
      *    ドクター
           COPY  "CPSK1010.INC".
      *    診療科
           COPY  "CPSK1005.INC".
      *
      *    患者番号構成管理情報
           COPY  "CPSK1009.INC".
      *
      *    患者マスタ
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    保険組合せ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
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
      *
      *    患者番号変換サブ
           COPY  "CPORCSPTID.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *    API対応
      *    外来カルテ３号紙情報構造体
           COPY    "CPFMKARTE3.INC".
           COPY    "CPORCRPMAIN.INC".
      *
      *一括用
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "COMMON-SPA".
      *    カルテ３号様式出力パラメタ
           COPY    "CPORCHC33.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCHC33AREA
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
      *    メイン処理
           IF      FLG-END             =   ZERO
               PERFORM 200-MAIN-SEC
           END-IF
      *v4.8
           IF     (CNT-PAGE            =   ZERO )
              AND (ORCHC33-RC          =   ZERO )
      *        印刷なし
               MOVE    1                   TO  ORCHC33-RC
           END-IF
      *
           .
       000-PROC-EXT.
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
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL-HKNCOMBI-AREA
           INITIALIZE                  TBL-HEN-AREA
           INITIALIZE                  FMKARTE3-REC
      *
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    ZERO                TO  WRK-PAGE
      *
           EVALUATE    ORCHC33-KBN
               WHEN    "1"
      *            オンライン
      *            出力先プリンタ名編集処理
                   PERFORM 110-SYS1031-HENSYU-SEC
               WHEN    "2"
      *            バッチ
                   MOVE    1               TO  ORCHC33-STRDD
                   MOVE    ZERO            TO  ORCHC33-ENDPAGE
                   MOVE    ZERO            TO  ORCHC33-ENDLINE
           END-EVALUATE
      *
      *    パラメタ編集処理
           PERFORM 120-PARA-HENSYU-SEC
      *
      *    プログラムオプション取込み
           PERFORM 100-OPTION-SELECT-SEC
      *
      *    パラメタ内容チェック
           IF     (ORCHC33-PRINT-FLG   NOT =   1   )
            AND   (ORCHC33-RC              =   ZERO )
               PERFORM 1002-OPTION-CHEK-SEC
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプション取込み処理
      *****************************************************************
       100-OPTION-SELECT-SEC             SECTION.
      *
           INITIALIZE                  PARA-AREA
      *    保険区分
           MOVE    "HKNCOMBIKBN"       TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-HKNCOMBIKBN
      *
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"          TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)       =   "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-API-DATA
           END-IF
      *
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
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA            TO  PARA-API-CUSTOM-ID
      *
      *    バッチは保険区分なし、ＡＰＩ処理なし
           IF      ORCHC33-KBN         =   "2"
               MOVE    "0"             TO  PARA-HKNCOMBIKBN
               MOVE    "0"             TO  PARA-API-DATA
               MOVE    SPACE           TO  PARA-API-CUSTOM-ID
           END-IF
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
           MOVE    "ORCHC33"           TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME       TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING   SPA-AREA
                                               ORCSPRGOPTIONAREA
           MOVE    SPACE               TO  WRK-PARA
           EVALUATE    WRK-PARA-NAME
           WHEN    "API_CUSTOM_ID"
               MOVE    ORCSPRGOPTION-OT-PARA (1:30)
                                       TO  WRK-PARA
           WHEN    OTHER
               IF      ORCSPRGOPTION-OT-TYPE   =   1
      *            半角で１文字目を値とする
                   MOVE    ORCSPRGOPTION-OT-PARA(1:1)
                                       TO  WRK-PARA(1:1)
               END-IF
           END-EVALUATE
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"             TO  KANACHK-SYORI
           MOVE    WRK-PARA        TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT
                                   TO  WRK-PARA
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ内容チェック処理
      *****************************************************************
       1002-OPTION-CHEK-SEC                     SECTION.
      *
           EVALUATE    PARA-HKNCOMBIKBN
           WHEN    "1"
      *        労災・自賠責・自費
               IF      (ORCHC33-HKNCOMBI    =   "9999" )
                   MOVE    8               TO  ORCHC33-RC
                   MOVE    1               TO  FLG-END
               END-IF
           WHEN    "2"
      *        保険組合せ毎
               IF     (ORCHC33-HKNCOMBI    =   "0000"
                                           OR  "9999"
                                           OR  SPACE )
                   MOVE    8               TO  ORCHC33-RC
                   MOVE    1               TO  FLG-END
               END-IF
           END-EVALUATE
           .
       1002-OPTION-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名編集処理
      *****************************************************************
       110-SYS1031-HENSYU-SEC                   SECTION.
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
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       110-SYS1031-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       120-PARA-HENSYU-SEC                   SECTION.
      *
      *    患者ＩＤ
           IF      ORCHC33-PTID        =   ZERO
               MOVE    9               TO  ORCHC33-RC
               MOVE    1               TO  FLG-END
           END-IF
      *
      *    患者番号
           IF      ORCHC33-PTNUM       =   SPACE
               INITIALIZE                      ORCSPTIDAREA
               MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
               MOVE    ORCHC33-PTID        TO  SPTID-PTID
               CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                   SPA-AREA
               IF      SPTID-RC            =   ZERO
                   MOVE    SPTID-PTNUM     TO  ORCHC33-PTNUM
               ELSE
                   MOVE    9               TO  ORCHC33-RC
                   MOVE    1               TO  FLG-END
               END-IF
           END-IF
      *
      *    診療科
           IF      ORCHC33-SRYKA       =   SPACE  OR  ZERO
               MOVE    SPACE               TO  WRK-SRYKA-MEI
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    ORCHC33-SRYKA       TO  SYS-1005-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
               MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *
               MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME2
                                               TO  WRK-SRYKA-MEI
               ELSE
                   MOVE    SPACE               TO  WRK-SRYKA-MEI
               END-IF
           END-IF
      *
      *    診療日
           MOVE    SPACE               TO  WRK-SRYYM-N
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    ORCHC33-SRYYM       TO  LNK-DAY2-YMD(1:6)
           MOVE    "01"                TO  LNK-DAY2-YMD(7:2)
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               STRING      LNK-DAY2-EDTYMD2-G   DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-Y   DELIMITED  BY  SIZE
                           "年"                 DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-M   DELIMITED  BY  SIZE
                           "月"                 DELIMITED  BY  SIZE
                                       INTO    WRK-SRYYM-N1
               END-STRING
           ELSE
               MOVE    SPACE               TO  WRK-SRYYM-N1
           END-IF
      *    診療日（終了）
           IF     (ORCHC33-SRYYM-END   =   SPACE )  OR
                  (ORCHC33-SRYYM-END   =   ORCHC33-SRYYM)
               MOVE    SPACE               TO  WRK-SRYYM-N2
           ELSE
               INITIALIZE                  LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    ORCHC33-SRYYM-END   TO  LNK-DAY2-YMD(1:6)
               MOVE    "01"                TO  LNK-DAY2-YMD(7:2)
               CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                                  LNK-DAY2-AREA
               IF      STS-DAY-RC1         =   ZERO
                   STRING      LNK-DAY2-EDTYMD2-G  DELIMITED  BY  SIZE
                               LNK-DAY2-EDTYMD2-Y  DELIMITED  BY  SIZE
                               "年"                DELIMITED  BY  SIZE
                               LNK-DAY2-EDTYMD2-M  DELIMITED  BY  SIZE
                               "月"                DELIMITED  BY  SIZE
                                           INTO    WRK-SRYYM-N2
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-SRYYM-N2
               END-IF
           END-IF
           IF      WRK-SRYYM-N2        =   SPACE
               STRING      WRK-SRYYM-N1    DELIMITED  BY  SPACE
      *************        "分"            DELIMITED  BY  SIZE
                           "〜"            DELIMITED  BY  SIZE
                                       INTO    WRK-SRYYM-N
               END-STRING
           ELSE
               STRING      WRK-SRYYM-N1    DELIMITED  BY  SPACE
                           "〜"            DELIMITED  BY  SIZE
                           WRK-SRYYM-N2    DELIMITED  BY  SPACE
                           "分"            DELIMITED  BY  SIZE
                                       INTO    WRK-SRYYM-N
               END-STRING
           END-IF
      *
      *    年齢計算（末日時点の年齢とする）
           MOVE    ORCHC33-BIRTHDAY    TO  WRK-YMDS1
           MOVE    ORCHC33-SRYYM       TO  WRK-YMDS2
           MOVE    31                  TO  WRK-DDS2
           COMPUTE WRK-NENREI-YY       =   WRK-YYS2
                                       -   WRK-YYS1
           IF      WRK-YMDS1(5:4)      >   WRK-YMDS2(5:4)
               COMPUTE WRK-NENREI-YY   =   WRK-NENREI-YY
                                           -   1
               COMPUTE WRK-NENREI-MM   =  (WRK-MMS2  +  12 )
                                           -   WRK-MMS1
           ELSE
               COMPUTE WRK-NENREI-MM   =   WRK-MMS2
                                       -   WRK-MMS1
           END-IF
      *    生年月日
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    ORCHC33-BIRTHDAY    TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               STRING      LNK-DAY2-EDTYMD2-G  DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-Y  DELIMITED  BY  SIZE
                           "年"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-M  DELIMITED  BY  SIZE
                           "月"                DELIMITED  BY  SIZE
                           LNK-DAY2-EDTYMD2-D  DELIMITED  BY  SIZE
                           "日生"              DELIMITED  BY  SIZE
                                       INTO    WRK-BIRTHDAY
               END-STRING
           ELSE
               MOVE    SPACE               TO  WRK-BIRTHDAY
           END-IF
           .
       120-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン処理
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
      *    最終印刷頁
           MOVE    ORCHC33-ENDPAGE     TO  ORCHC33-PAGE
           IF    ( ORCHC33-ENDPAGE     =   ZERO )
            AND  ( ORCHC33-ENDLINE     >   ZERO )
               MOVE    1               TO  ORCHC33-PAGE
           END-IF
      *    最終印行
           IF      ORCHC33-ENDLINE     =   ZERO
               MOVE    ZERO            TO  FLG-HC331
           ELSE
               MOVE    1               TO  FLG-HC331
           END-IF
      *
           EVALUATE    ORCHC33-PRINT-FLG
              WHEN     1
      *           頭書き
                  PERFORM 2001-HEAD-PRINT-SEC
              WHEN     OTHER
      *           明細印刷
                  PERFORM 2000-ALL-PRINT-SEC
           END-EVALUATE
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    頭書き印刷処理
      *****************************************************************
       2001-HEAD-PRINT-SEC            SECTION.
      *
           IF    ( PARA-API-DATA       =   "1" )
      *       印刷処理
              PERFORM 7009-API-CALLSYORI-SEC
           ELSE
      *
      *       見出し編集
              PERFORM 20031-HEAD-HEN-SEC
      *       年度、ページクリア
              MOVE    SPACE            TO  HCM33-PAGE
              MOVE    SPACE            TO  HCM33-SRYYM
      *       印刷処理
              PERFORM 2003-HC33-PRINT-SEC
           END-IF
           .
       2001-HEAD-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    メイン印刷処理
      *****************************************************************
       2000-ALL-PRINT-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-PARA-HKNNUM
      *    保険組合せテーブル編集
           IF     (PARA-HKNCOMBIKBN    =   "1"
                                       OR  "2" )
               PERFORM 2009-HKNCOMBI-HEN-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-MEI
           MOVE    SPACE               TO  HCM33
      *
      *    追加印刷初期処理
           IF      FLG-HC331           =   1
               MOVE    SPACE               TO  HCM331
               INITIALIZE                      HCM331
      *        最終印刷行
               MOVE    ORCHC33-ENDLINE     TO  CNT-MEI
           ELSE
               MOVE    ZERO                TO  CNT-MEI
           END-IF
      *
           MOVE    ORCHC33-SRYYM       TO  WRK-SRYYM
           IF      ORCHC33-SRYYM-END   =   SPACE
               MOVE    WRK-SRYYM           TO  WRK-SRYYM-END
           ELSE
               MOVE    ORCHC33-SRYYM-END   TO  WRK-SRYYM-END
           END-IF
           PERFORM UNTIL       WRK-SRYYM   >   WRK-SRYYM-END
      *        集計処理
               PERFORM 2001-SYUKEI-SYORI-SEC
      *        印刷処理
               IF      FLG-PRINT           =   1
                   PERFORM 2002-PRINT-SYORI-SEC
               END-IF
      *        診療年月＋１
               COMPUTE WRK-SRYMM           =   WRK-SRYMM    +   1
               IF      WRK-SRYMM           >   12
                   COMPUTE WRK-SRYYY       =   WRK-SRYYY    +   1
                   MOVE    1               TO  WRK-SRYMM
               END-IF
           END-PERFORM
      *
           IF  PARA-API-DATA           =   "1"
               IF      CNT-API         >   ZERO
                   PERFORM 7009-API-CALLSYORI-SEC
               END-IF
           ELSE
               IF      CNT-MEI             >   ZERO
                   PERFORM 2003-HC33-PRINT-SEC
               END-IF
           END-IF
      *
           .
       2000-ALL-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せテーブル編集処理
      *****************************************************************
       2009-HKNCOMBI-HEN-SEC        SECTION.
      *
           INITIALIZE                      TBL-ALL-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX-C
           MOVE    SPACE               TO  WRK-PARA-HKNNUM
      *
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCHC33-PTID        TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           PERFORM UNTIL   (FLG-HKNCOMBI   =   1  )
                       OR  (IDX-C           >=  99 )
               MOVE    ZERO                TO  FLG-HKNCOMBICHK
               IF     (COMB-DELKBN         =   SPACE )
      *        診療年月の有効チェック
               AND   ((ORCHC33-SRYYM       >=  COMB-TEKSTYMD(1:6))
                  AND (ORCHC33-SRYYM       <=  COMB-TEKEDYMD(1:6)))
                   MOVE    1                   TO  FLG-HKNCOMBICHK
               END-IF
      *        終了日の有効チェック
               IF    ( COMB-DELKBN         =   SPACE   )    AND
                     ( ORCHC33-SRYYM-END   NOT =   SPACE )  AND
                     ((ORCHC33-SRYYM-END       >=  COMB-TEKSTYMD(1:6))
                 AND  (ORCHC33-SRYYM-END       <=  COMB-TEKEDYMD(1:6)))
                   MOVE    1                   TO  FLG-HKNCOMBICHK
               END-IF
      *
               IF      FLG-HKNCOMBICHK     =   1
                   ADD     1                   TO  IDX-C
                   MOVE    COMB-HKNCOMBINUM    TO  TBL-ALL-HKNCOMBINUM
                                                               (IDX-C)
                   MOVE    COMB-HKNNUM         TO  TBL-ALL-HKNNUM
                                                               (IDX-C)
                   IF      ORCHC33-HKNCOMBI    =   COMB-HKNCOMBINUM
                       MOVE    COMB-HKNNUM     TO  WRK-PARA-HKNNUM
                   END-IF
               END-IF
      *
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2009-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-PRINT
      *    集計テーブル
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL-RSI-AREA
           MOVE    ZERO                TO  WRK-TUK-ZAITEN
           MOVE    ZERO                TO  WRK-TUK-SKYMONEY
           MOVE    ZERO                TO  WRK-TUK-RSI-MONEY
      *
      *    診療会計マスターを編集
           INITIALIZE                      SRYACCT-REC 
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    ORCHC33-NYUGAIKBN   TO  ACCT-NYUGAIKBN
           MOVE    ORCHC33-PTID        TO  ACCT-PTID
           MOVE    WRK-SRYYM           TO  ACCT-SRYYM
           IF      ORCHC33-SRYKA       =   ZERO
               MOVE    "key19"             TO  WRK-ORC-DBPATH
           ELSE
               MOVE    ORCHC33-SRYKA       TO  ACCT-SRYKA
               MOVE    "key18"             TO  WRK-ORC-DBPATH
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           ELSE
               INITIALIZE                  SRYACCT-REC
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL      (FLG-SRYACCT     =   1   )
      *        包括まとめ入力対象外
               IF     (ACCT-HKNCOMBI       =   "9999" )
                  OR  (ACCT-JIHOKBN        =   "1"    )
                   CONTINUE
               ELSE
      *            保険編集
                   PERFORM 20013-HKNCOMBI-HEN-SEC
      *!!          IF      ACCT-ZAITEN         >   ZERO
                   IF      FLG-SYORI           =   1
                       MOVE    1                   TO  FLG-PRINT
                       PERFORM 20011-SRYACCT-HEN-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
               PERFORM 900-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    WRK-ORC-DBPATH      TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2001-SYUKEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       20011-SRYACCT-HEN-SEC                SECTION.
      *
           EVALUATE    ACCT-SRYKBN(1:1)
      *        診察
               WHEN    "1"
                   MOVE    1               TO  IDX1
      *        投薬
               WHEN    "2"
                   MOVE    2               TO  IDX1
      *        注射
               WHEN    "3"
                   MOVE    3               TO  IDX1
      *        処置・手術
               WHEN    "4"
               WHEN    "5"
                   MOVE    4               TO  IDX1
      *        検査
               WHEN    "6"
                   MOVE    5               TO  IDX1
      *        画像診断
               WHEN    "7"
                   MOVE    6               TO  IDX1
      *        その他
               WHEN    "8"
                   MOVE    7               TO  IDX1
      *        その他
               WHEN    OTHER
                   IF      ACCT-SRYKBN     =   "95"  OR  "96"
                       MOVE    11              TO  IDX1
                   ELSE
                       MOVE    ZERO            TO  IDX1
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   400 )  OR
                              (IDX1    =   ZERO)
               IF      TBL-ZAITEN (IDX1 IDX)   =   ZERO
                   MOVE    IDX             TO  IDX2
                   MOVE    400             TO  IDX
               END-IF
           END-PERFORM
           IF     (IDX2                =   ZERO ) AND
                  (IDX1                >   ZERO )
               DISPLAY "** ORCHC33 ZAI TBL 400 OVER **"
           END-IF
      *
           IF     (IDX2                =   ZERO ) OR
                  (IDX1                =   ZERO )
               CONTINUE
           ELSE
               PERFORM 200110-SRYACCT-HENSYU-SEC
           END-IF
           .
       20011-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       200110-SRYACCT-HENSYU-SEC                SECTION.
      *
           MOVE    ZERO    TO    FLG-ARI2
      *    労災・自賠責の時、円分を減算する
           IF  ((FLG-HKNKBN      =   1                           ) AND
                (ACCT-SRYKBN     =  "11" OR "12" OR "13" OR "80" ) AND
                (ACCT-ZAIREQFLG  =  "1"                          ))
           OR  ((FLG-HKNKBN      =   2                           ) AND
                (ACCT-SRYKBN     =  "80"                         ) AND
                (ACCT-ZAIREQFLG  =  "1"                          ))
               PERFORM 200111-RSI-ZAITEN-SEC
           END-IF
      *
           IF    FLG-ARI2   =  ZERO
      *        逓減点数を判定する
               MOVE    ZERO                TO  FLG-GENTEN
               IF      ACCT-ZAIREQFLG      =   "1"
                   PERFORM 950-SRYACCTPLUS-READ-SEC
                   IF      ACCTP-PLUSKBN       =   1
                       MOVE    1               TO  FLG-GENTEN
                   END-IF
               ELSE
                   IF     (ACCT-SRYKBN         =   "21" )  AND
                          (ACCT-ZAITEN         >   ZERO )  AND
                          (ACCT-YKZTEN         =   ZERO )
                       PERFORM 200112-GEN-TENSU-SEC
                   END-IF
               END-IF
               MOVE    ACCT-ZAITEN         TO  WRK-ZAITEN
               IF      FLG-GENTEN          =   1
                   COMPUTE WRK-ZAITEN      =   WRK-ZAITEN
                                           *   -1
               END-IF
           END-IF
      *
           IF      WRK-ZAITEN      NOT =   ZERO
      *        点数編集
               MOVE    WRK-ZAITEN          TO  TBL-ZAITEN (IDX1 IDX2)
      *        回数編集
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   31
                   MOVE    ACCT-DAY(1  IDX)    TO  TBL-DAY
                                                       (IDX1 IDX2 IDX)
               END-PERFORM
           END-IF
      *
           .
       200110-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災・自賠責点数減算処理
      *****************************************************************
       200111-RSI-ZAITEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-SUM-TEN
           MOVE    ZERO                TO  WRK-RSI-MONEY
      *    労災金額％加算 
           MOVE    ZERO                TO  FLG-KAGEN
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    "01"                TO  WRK-SRYYMD(7:2)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYYMD(7:2)
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL       FLG-SRYACT      =   1
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5        )  OR
                                  (SRY-SRYCD (IDZ) =   SPACE)
      *            点数マスタ検索
                   IF      (SRY-SRYCD (IDZ)(1:3)   =   "101"  )  OR
                           (SRY-SRYCD (IDZ)(1:5)   =   "09591" OR
                                                       "09592" OR
                                                       "09593" )
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                労災の金額算定分の時、点数をゼロとする
                       IF     (TNS-TENSIKIBETU     =   1  )  OR
                              (TNS-SRYCD (1:5)     =   "09591" OR
                                                       "09592" OR
                                                       "09593" )
      *                自賠責金額入力を点数とする
                           IF     (SRY-SRYSYUKBN       =   "809")
                              MOVE    SRY-JIHIMONEY(IDZ)   TO  TNS-TEN
                           END-IF
                           COMPUTE WRK-SUM-TEN     =   WRK-SUM-TEN
                                                   +  (TNS-TEN
                                                   /   10  )
                           COMPUTE WRK-RSI-MONEY   =   WRK-RSI-MONEY
                                                   +   TNS-TEN
                       END-IF
      *                ％加算・減算あり
                       IF     (TNS-TENSIKIBETU     =   5
                                                   OR  6  )
                           MOVE    1               TO  FLG-KAGEN
                       END-IF
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      ACCT-ZAITEN         >=  WRK-SUM-TEN
               COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                           -   WRK-SUM-TEN
           ELSE
               MOVE    ZERO                TO  WRK-ZAITEN
           END-IF
      *
      *    労災金額％加算対応
           IF     (ACCT-ZAIREQFLG      =   "1" )
             AND  (FLG-KAGEN           =   1   )
             AND  (WRK-RSI-MONEY       >   ZERO)
               PERFORM 950-SRYACCTPLUS-READ-SEC
               IF     (ACCTP-RSIKINGAKU    =   ZERO )
                  OR  (ACCT-SRYKBN         =   "11"
                                           OR  "12" )
                   CONTINUE
               ELSE
      *            金額のみの剤である
                   MOVE    ACCTP-RSIKINGAKU    TO  WRK-RSI-MONEY
                   MOVE    ZERO                TO  WRK-ZAITEN
               END-IF
           END-IF
      *
      *    労災円分
           IF      WRK-RSI-MONEY       >   ZERO
               MOVE      1                 TO  FLG-ARI2
               PERFORM 2001111-RSI-TBLHENSYU-SEC
           END-IF
      *
           .
       200111-RSI-ZAITEN-EXT.
           EXIT.
      *****************************************************************
      *    労災円分集計処理
      *****************************************************************
       2001111-RSI-TBLHENSYU-SEC                SECTION.
      *
           MOVE    ZERO                TO  IDX3
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   400
               IF      TBL-RSI-ZAITEN (IDX1 IDX) =   ZERO
                   MOVE    IDX             TO  IDX3
                   MOVE    400             TO  IDX
               END-IF
           END-PERFORM
           IF      IDX3                >   ZERO
      *        点数編集
               ADD     WRK-RSI-MONEY       TO  TBL-RSI-ZAITEN(IDX1 IDX3)
      *        回数編集
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   31
                   ADD     ACCT-DAY(1  IDX)    TO  TBL-RSI-DAY
                                                       (IDX1 IDX3  IDX)
               END-PERFORM
           END-IF
      *
           .
       2001111-RSI-TBLHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200112-GEN-TENSU-SEC                SECTION.
      *
      *    診療行為マスター読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *    点数検索用日付編集
           MOVE    ACCT-SRYYM          TO  WRK-SRYYMD(1:6)
           MOVE    "01"                TO  WRK-SRYYMD(7:2)
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  NOT =   ZERO
                   MOVE    IDX-D           TO  WRK-SRYYMD(7:2)
                   MOVE    31              TO  IDX-D
               END-IF
           END-PERFORM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *    減算分は１剤１明細とする
           IF      FLG-SRYACT          =   ZERO
      *        薬剤料逓減　を減算
               IF     (ACCT-SRYKBN         =   "21" )  AND
                      (SRY-SRYCD (01)      =   "630010002")
                   MOVE    1               TO  FLG-GENTEN
               END-IF
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200112-GEN-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       20013-HKNCOMBI-HEN-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    保険組合せ対象チェック
           IF      (PARA-HKNCOMBIKBN    =   "1"  OR  "2")
               MOVE    ACCT-HKNCOMBI        TO  WRK-CHK-HKNCOMBI
               PERFORM 200139-HKNCOMBI-CHK-SEC
               IF      FLG-SYORI            =   ZERO
                   GO      TO      20013-HKNCOMBI-HEN-EXT
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYORI
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   20
               IF      TBL-HKNCOMBI (IDX)  =   SPACE
                   MOVE    ACCT-HKNCOMBI       TO  TBL-HKNCOMBI (IDX)
                   PERFORM 200131-HKNCOMBI-MEI-SEC
                   MOVE    WRK-HKNCOMBIMEI     TO  TBL-HKNCOMBIMEI(IDX)
                   MOVE    TBL-HKNKBN (IDX)    TO  FLG-HKNKBN
                   MOVE    20                  TO  IDX
               ELSE
                   IF      TBL-HKNCOMBI (IDX)  =   ACCT-HKNCOMBI
                       MOVE    TBL-HKNKBN (IDX)    TO  FLG-HKNKBN
                       MOVE    20              TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20013-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       200131-HKNCOMBI-MEI-SEC                SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    ACCT-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    ACCT-PTID           TO  COMB-PTID
           MOVE    ACCT-HKNCOMBI       TO  COMB-HKNCOMBINUM
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
           INITIALIZE                  WRK-HKNCOMBI-AREA
           IF      COMB-HKNNUM         =   "971"
                                       OR  "973"
      *        労災保険名称
               PERFORM 2001312-RSIMEI-HEN-SEC
           ELSE
      *        保険名称
               PERFORM 2001311-HKNMEI-HEN-SEC
           END-IF
      *
      *    本人家族区分
           EVALUATE    COMB-HONKZKKBN
               WHEN    "1"
                   MOVE    "本人"          TO  TBL-HONKZKKBN (IDX)
               WHEN    "2"
                   MOVE    "家族"          TO  TBL-HONKZKKBN (IDX)
           END-EVALUATE
      *
           IF      COMB-HKNNUM         =   "971"  OR  "973"
               MOVE    1               TO  TBL-HKNKBN (IDX)
           ELSE
      *      第三者行為
             IF    COMB-KOH1HKNNUM     =   "970"
               MOVE    2               TO  TBL-HKNKBN (IDX)
             ELSE
               MOVE    ZERO            TO  TBL-HKNKBN (IDX)
             END-IF
           END-IF
           .
       200131-HKNCOMBI-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     保険名称編集処理
      *****************************************************************
       2001311-HKNMEI-HEN-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           IF      COMB-SYU-TANSEIDONAME   NOT =   SPACE
               MOVE    "・"                TO  WRK-MEI-1
           END-IF
           IF      COMB-KOH1-TANSEIDONAME  NOT =   SPACE
               MOVE    "・"                TO  WRK-MEI-2
           END-IF
           IF      COMB-KOH2-TANSEIDONAME  NOT =   SPACE
               MOVE    "・"                TO  WRK-MEI-3
           END-IF
           IF      COMB-KOH3-TANSEIDONAME  NOT =   SPACE
               MOVE    "・"                TO  WRK-MEI-4
           END-IF
           IF      COMB-KOH4-TANSEIDONAME  NOT =   SPACE
               MOVE    "・"                TO  WRK-MEI-5
           END-IF
           STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                   WRK-MEI-1               DELIMITED  BY  SPACE
                   COMB-KOH1-TANSEIDONAME  DELIMITED  BY  SPACE
                   WRK-MEI-2               DELIMITED  BY  SPACE
                   COMB-KOH2-TANSEIDONAME  DELIMITED  BY  SPACE
                   WRK-MEI-3               DELIMITED  BY  SPACE
                   COMB-KOH3-TANSEIDONAME  DELIMITED  BY  SPACE
                   WRK-MEI-5               DELIMITED  BY  SPACE
                   COMB-KOH4-TANSEIDONAME  DELIMITED  BY  SPACE
                         INTO              WRK-HKNCOMBIMEI
           END-STRING
      *    最後の’・’をはずす
           PERFORM VARYING    IDX-H    FROM    99  BY  -2
                   UNTIL      IDX-H    <=  1
               IF      WRK-HKNCOMBIMEI(IDX-H:2)     =   "・"
                   MOVE    SPACE           TO  WRK-HKNCOMBIMEI(IDX-H:2)
                   MOVE    2               TO  IDX-H
               END-IF
           END-PERFORM
           .
       2001311-HKNMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *     労災保険名称編集処理
      *****************************************************************
       2001312-RSIMEI-HEN-SEC                SECTION.
      *    保険検索
           MOVE    SPACE               TO  WRK-HKNCOMBIMEI
           INITIALIZE                      PTHKNINF-REC
           MOVE    COMB-HOSPNUM        TO  PTHKN-HOSPNUM
           MOVE    COMB-PTID           TO  PTHKN-PTID
           MOVE    COMB-HKNID          TO  PTHKN-HKNID
           PERFORM 950-PTHKNINF-READ-SEC
           IF      FLG-PTHKNINF        =   ZERO
              MOVE    PTHKN-KIGO           TO  WRK-HKNCOMBIMEI
           END-IF
           .
       2001312-RSIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険適応判定処理
      *****************************************************************
       200139-HKNCOMBI-CHK-SEC          SECTION.
      *
           EVALUATE    PARA-HKNCOMBIKBN
           WHEN    "1"
      *        労災・自賠責・自費保険とその他保険
               IF     (WRK-PARA-HKNNUM         =   "971"
                                               OR  "973" )
                  OR  (WRK-PARA-HKNNUM(1:2)    =   "98"  )
      *            保険組合せ一致のみ
                   IF      ORCHC33-HKNCOMBI    =   WRK-CHK-HKNCOMBI
                       MOVE    1                   TO  FLG-SYORI
                   END-IF
               ELSE
      *            労災・自賠責は対象外
                   PERFORM VARYING IDX-C   FROM    1   BY  1
                           UNTIL   (IDX-C          >   99  )  OR
                                   (TBL-ALL-HKNCOMBINUM (IDX-C)
                                                   =   SPACE)
                       IF      WRK-CHK-HKNCOMBI
                                           =   TBL-ALL-HKNCOMBINUM
                                                               (IDX-C)
                           IF     (TBL-ALL-HKNNUM (IDX-C)  =   "971"
                                                           OR  "973")
                           OR     (TBL-ALL-HKNNUM (IDX-C)(1:2)
                                                           =   "98")
                               MOVE    ZERO                TO  FLG-SYORI
                           ELSE
                               MOVE    1                   TO  FLG-SYORI
                           END-IF
                           MOVE    99                  TO  IDX-C
                       END-IF
                   END-PERFORM
               END-IF
           WHEN    "2"
      *        保険組合せ一致のみ
               IF      ORCHC33-HKNCOMBI    =   WRK-CHK-HKNCOMBI
                   MOVE    1                   TO  FLG-SYORI
               END-IF
           END-EVALUATE
           .
       200139-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     印刷処理
      *****************************************************************
       2002-PRINT-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  WRK-TUK-ZAITEN
           MOVE    ZERO                TO  WRK-TUK-SKYMONEY
           MOVE    ZERO                TO  WRK-TUK-RSI-MONEY
           MOVE    ZERO                TO  WRK-TUK-JIHIKEI
           MOVE    ZERO                TO  FLG-HC331-END
      *
      *    印刷開始日から対象とする
           IF      ORCHC33-STRDD       =   ZERO
               MOVE    1               TO  ORCHC33-STRDD
           END-IF
      *
      **** 追加印刷初期処理
      *    IF      FLG-HC331           =   1
      *        MOVE    SPACE               TO  HCM331
      *        INITIALIZE                      HCM331
      *        最終印刷行
      *        MOVE    ORCHC33-ENDLINE     TO  CNT-MEI
      *    ELSE
      *        MOVE    ZERO                TO  CNT-MEI
      *****END-IF
      *
      *H30.1
      *    バッチ印刷の保険組合せ編集
           IF     (ORCHC33-KBN         =   "2" )
             AND  (CNT-MEI             <   40  )
               PERFORM 200311-HKNSYUBETU-HEN-SEC
               MOVE    WRK-HEN-HKNCOMBIMEI     TO  HCM33-HKNSYUBETU
           END-IF
      *
           PERFORM VARYING     IDX     FROM    ORCHC33-STRDD  BY  1
                   UNTIL       IDX     >   31
               MOVE    ZERO                TO  FLG-ARI
               MOVE    ZERO                TO  FLG-ARI-RSI
               MOVE    ZERO                TO  WRK-GOK-ZAITEN
               MOVE    ZERO                TO  WRK-GOK-RSI-MONEY
               INITIALIZE                  TBL-DAY-AREA
      *        点数
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL       IDX1    >   10
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   400
                       IF      TBL-DAY (IDX1 IDX2 IDX) >   ZERO
      *                    印刷形態
                           IF      ORCHC33-KARUTE3KBN  =   "1"
      *                        日毎
                               PERFORM 20022-TBLDAY-ADD-SEC
                           ELSE
      *                        剤毎
                               PERFORM 20021-TBL-HENSYU-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
      *        労災円
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL       IDX1    >   10
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   400
                       IF      TBL-RSI-DAY (IDX1 IDX2 IDX) >   ZERO
      *                    労災円 印刷形態
                           IF      ORCHC33-KARUTE3KBN  =   "1"
      *                        日毎
                               PERFORM 20024-RSI-TBLDAY-ADD-SEC
                           ELSE
      *                        剤毎
                               PERFORM 20023-RSI-TBL-HENSYU-SEC
                           END-IF
                       END-IF
                   END-PERFORM
               END-PERFORM
      *        自費コードのみの判定
               IF      FLG-ARI             =   ZERO
                   IF      ORCHC33-KARUTE3KBN  =   "1"
                       CONTINUE
                   ELSE
                       INITIALIZE                  TBL-HEN-AREA
                       MOVE    ZERO                TO  IDX-T1
                       MOVE    ZERO                TO  IDX-MAX
                   END-IF
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   400
                       IF      TBL-DAY (11 IDX2 IDX)   >   ZERO
                           MOVE    1               TO  FLG-ARI
                           MOVE    400             TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
               IF      FLG-ARI             =   1
                   IF      ORCHC33-KARUTE3KBN  =   "1"
      *                日毎のテーブル編集
                       PERFORM 20023-TBL-HENSYU2-SEC
                   ELSE
      *                収納より負担金編集
                       MOVE    IDX                 TO  WRK-DAY
                       PERFORM 200231-SYUNOU-HEN-SEC
                   END-IF
      *            日ごとに編集する
                   PERFORM 20023-HC33-DAYHEN-SEC
               END-IF
           END-PERFORM
      *
           IF     (PARA-API-DATA       =   "1" )
               PERFORM 7003-API-TUKIKEI-SEC
               GO  TO  2002-PRINT-SYORI-EXT
           END-IF
      *
      *    月計印刷
           IF     (FLG-HC331           =   1   )  AND
                  (FLG-HC331-END       =   ZERO)
      *        月合計印刷（ＨＣ３３１）
               PERFORM 20042-HC331-TUKIKEI-SEC
               MOVE    1                   TO  FLG-TUKI-END
               MOVE    ZERO                TO  CNT-MEI
           END-IF
      *    月計印刷（ＨＣ３３）
           IF      FLG-TUKI-END        =   ZERO
               PERFORM 20024-TUKIKEI-SEC
           END-IF
      *
           .
       2002-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集テーブル編集処理（剤毎）
      *****************************************************************
       20021-TBL-HENSYU-SEC                SECTION.
      *
           IF      FLG-ARI             =   ZERO
               INITIALIZE                  TBL-HEN-AREA
               MOVE    ZERO                TO  IDX-T1
               MOVE    ZERO                TO  IDX-MAX
           END-IF
           MOVE    1                   TO  FLG-ARI
      *    最終行を求める
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   400
               IF      TBL-NAIYO (IDY IDX1)    =   SPACE
                   MOVE    IDY             TO  IDX-T1
                   MOVE    400             TO  IDY
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-Z88-X
           IF      TBL-ZAITEN (IDX1 IDX2)  >   ZERO
               MOVE    TBL-ZAITEN (IDX1 IDX2)  TO  WRK-Z59
           ELSE
               MOVE    TBL-ZAITEN (IDX1 IDX2)  TO  WRK-Z59-R
           END-IF
           COMPUTE WRK-GOK-ZAITEN      =   WRK-GOK-ZAITEN
                                       +  (TBL-ZAITEN (IDX1 IDX2)
                                       *   TBL-DAY (IDX1 IDX2 IDX) )
      *
           IF     (IDX1                =   2  OR  3 )  OR
                  (TBL-DAY (IDX1 IDX2 IDX) >  1     )
      *        回数あり
               MOVE    TBL-DAY (IDX1 IDX2 IDX) TO  WRK-Z9
               MOVE    WRK-Z59-X       TO  TBL-NAIYO(IDX-T1 IDX1)(1:5)
               MOVE    "x"             TO  TBL-NAIYO(IDX-T1 IDX1)(6:1)
               MOVE    WRK-Z9          TO  TBL-NAIYO(IDX-T1 IDX1)(7:2)
           ELSE
      *        点数のみ
               MOVE    WRK-Z88-X       TO  TBL-NAIYO(IDX-T1 IDX1)
           END-IF
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *
           .
       20021-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    日毎集計処理
      *****************************************************************
       20022-TBLDAY-ADD-SEC                SECTION.
      *
           MOVE    1                   TO  FLG-ARI
      *    日毎集計
           COMPUTE WRK-GOK-ZAITEN      =  (TBL-ZAITEN (IDX1 IDX2)
                                       *   TBL-DAY (IDX1 IDX2 IDX) )
           ADD     WRK-GOK-ZAITEN      TO  TBL-DAY-ZAITEN(IDX1)
      *
           .
       20022-TBLDAY-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災円帳票編集テーブル編集処理（剤毎）
      *****************************************************************
       20023-RSI-TBL-HENSYU-SEC                SECTION.
      *
           IF      FLG-ARI             =   ZERO
               INITIALIZE                  TBL-HEN-AREA
               MOVE    ZERO                TO  IDX-T1
               MOVE    ZERO                TO  IDX-MAX
           END-IF
           MOVE    1                   TO  FLG-ARI
      *    労災分開始位置
           IF      FLG-ARI-RSI         =   ZERO
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   400
                   IF      TBL-HEN-OCC2-G (IDY)    =   SPACE
                       MOVE    IDY             TO  IDX-T1
                       MOVE    400             TO  IDY
                   END-IF
               END-PERFORM
               MOVE    IDX-T1              TO  IDX-RSI
               MOVE    1                   TO  FLG-ARI-RSI
           ELSE
      *    最終行を求める
               PERFORM VARYING     IDY     FROM    IDX-RSI  BY  1
                       UNTIL       IDY     >   400
                   IF      TBL-NAIYO (IDY IDX1)    =   SPACE
                       MOVE    IDY             TO  IDX-T1
                       MOVE    400             TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    SPACE               TO  WRK-Z88-X
           IF      TBL-RSI-ZAITEN (IDX1 IDX2)  >   ZERO
               MOVE    TBL-RSI-ZAITEN (IDX1 IDX2)  TO  WRK-Z59
           ELSE
               MOVE    TBL-RSI-ZAITEN (IDX1 IDX2)  TO  WRK-Z59-R
           END-IF
           COMPUTE WRK-GOK-RSI-MONEY   =   WRK-GOK-RSI-MONEY
                                       +  (TBL-RSI-ZAITEN (IDX1 IDX2)
                                       *   TBL-RSI-DAY (IDX1 IDX2 IDX))
      *
           IF     (IDX1                =   2  OR  3 )  OR
                  (TBL-RSI-DAY (IDX1 IDX2 IDX) >  1     )
      *        回数あり
               MOVE    TBL-RSI-DAY (IDX1 IDX2 IDX) TO  WRK-Z9
               MOVE    WRK-Z59-X       TO  TBL-NAIYO(IDX-T1 IDX1)(1:5)
               MOVE    "x"             TO  TBL-NAIYO(IDX-T1 IDX1)(6:1)
               MOVE    WRK-Z9          TO  TBL-NAIYO(IDX-T1 IDX1)(7:2)
           ELSE
      *        点数のみ
               MOVE    WRK-Z88-X       TO  TBL-NAIYO(IDX-T1 IDX1)
           END-IF
           IF      TBL-NAIYO(IDX-T1 IDX1)(1:1) =   SPACE
               MOVE    "*"             TO  TBL-NAIYO(IDX-T1 IDX1)(1:1)
           END-IF
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *
           .
       20023-RSI-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災円 日毎集計処理
      *****************************************************************
       20024-RSI-TBLDAY-ADD-SEC                SECTION.
      *
           MOVE    1                   TO  FLG-ARI
      *    日毎集計
           COMPUTE WRK-GOK-ZAITEN      =  (TBL-RSI-ZAITEN (IDX1 IDX2)
                                       *   TBL-RSI-DAY (IDX1 IDX2 IDX) )
           ADD     WRK-GOK-ZAITEN      TO  TBL-DAY-RSI-ZAITEN(IDX1)
      *
           .
       20024-RSI-TBLDAY-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集テーブル編集処理（日毎）
      *****************************************************************
       20023-TBL-HENSYU2-SEC                SECTION.
      *
           INITIALIZE                  TBL-HEN-AREA
           MOVE    1                   TO  IDX-T1
           MOVE    1                   TO  IDX-MAX
           MOVE    ZERO                TO  WRK-GOK-ZAITEN
           MOVE    ZERO                TO  WRK-GOK-RSI-MONEY
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   9
               IF      TBL-DAY-ZAITEN(IDX1)    >   ZERO
                   MOVE    TBL-DAY-ZAITEN(IDX1)    TO  WRK-Z89
               ELSE
                   MOVE    TBL-DAY-ZAITEN(IDX1)    TO  WRK-Z79S
               END-IF
      *
               COMPUTE WRK-GOK-ZAITEN      =   WRK-GOK-ZAITEN
                                           +   TBL-DAY-ZAITEN(IDX1)
      *        点数のみ
               IF      TBL-DAY-ZAITEN(IDX1)    NOT =   ZERO
                   MOVE    WRK-Z89-X       TO  TBL-NAIYO(IDX-T1 IDX1)
               END-IF
           END-PERFORM
      *
      *    労災円集計
           MOVE    ZERO                TO  FLG-ARI-RSI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   9
               IF      TBL-DAY-RSI-ZAITEN(IDX1)    NOT =   ZERO
                   IF      FLG-ARI-RSI         =   ZERO
                       ADD     1                   TO  IDX-T1
                       MOVE    IDX-T1              TO  IDX-RSI
                   END-IF
                   MOVE    1                   TO  FLG-ARI-RSI
                   MOVE    TBL-DAY-RSI-ZAITEN(IDX1)    TO  WRK-Z89
                   MOVE    WRK-Z89-X       TO  TBL-NAIYO(IDX-T1 IDX1)
                   MOVE    "*"             TO  TBL-NAIYO(IDX-T1 IDX1)
                                                                (1:1)
                   COMPUTE WRK-GOK-RSI-MONEY
                                           =   WRK-GOK-RSI-MONEY
                                           +   TBL-DAY-RSI-ZAITEN(IDX1)
               END-IF
           END-PERFORM
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *    収納より負担金編集
           MOVE    IDX                 TO  WRK-DAY
           PERFORM 200232-SYUNOU-DAYHEN-SEC
      *
           .
       20023-TBL-HENSYU2-EXT.
           EXIT.
      *****************************************************************
      *    日ごとに編集処理
      *****************************************************************
       20023-HC33-DAYHEN-SEC           SECTION.
      *
      *    診療日
           MOVE    WRK-SRYMM           TO  WRK-H-MM
           MOVE    "."                 TO  WRK-H-P
           MOVE    WRK-DAY             TO  WRK-H-DD
      *
      *    点数合計
           IF      WRK-GOK-ZAITEN      >   ZERO
               MOVE    WRK-GOK-ZAITEN      TO  WRK-Z89
               MOVE    WRK-Z89             TO  TBL-NAIYO(01 08)
           ELSE
               MOVE    WRK-GOK-ZAITEN      TO  WRK-Z79S
               MOVE    WRK-Z79S            TO  TBL-NAIYO(01 08)
           END-IF
      *    労災円合計
           IF      WRK-GOK-RSI-MONEY   >   ZERO
               MOVE    WRK-GOK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  TBL-NAIYO(IDX-RSI 08)
               MOVE    "*"                 TO  TBL-NAIYO(IDX-RSI 08)
                                                                 (1:1)
           END-IF
      *
      *    月点数合計
           ADD     WRK-GOK-ZAITEN      TO  WRK-TUK-ZAITEN
           ADD     WRK-GOK-RSI-MONEY   TO  WRK-TUK-RSI-MONEY
      *
           IF    ( PARA-API-DATA       =   "1" )
               PERFORM 7002-API-BODY-SEC
               GO  TO  20023-HC33-DAYHEN-EXT
           END-IF
      *
      *    追加印刷
           IF     (FLG-HC331           =   1   )  AND
                  (FLG-HC331-END       =   ZERO)
               PERFORM 2004-HC331-HENSYU-SEC
           ELSE
               MOVE    1               TO  IDX-STR
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    IDX-STR  BY  1
                   UNTIL       IDX1    >   IDX-MAX
               ADD     1                   TO  CNT-MEI
               IF      CNT-MEI             >   40
      *            印刷処理
                   PERFORM 2003-HC33-PRINT-SEC
                   MOVE    1                   TO  CNT-MEI
               END-IF
               IF      CNT-MEI             =   1
                   PERFORM 20031-HEAD-HEN-SEC
               END-IF
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM33-NUM (CNT-MEI)
      *        日付
               IF     (IDX1                =   1 )  OR
                      (CNT-MEI             =   1 )
                   MOVE    WRK-HEN-DAY     TO  HCM33-SRYMD (CNT-MEI)
               END-IF
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   10
                   MOVE    TBL-NAIYO (IDX1 IDX2)   TO  HCM33-NAIYO
                                                          (CNT-MEI IDX2)
               END-PERFORM
               MOVE    TBL-MEISYO (IDX1)   TO  HCM33-BIKOU (CNT-MEI)
           END-PERFORM
      *
           .
       20023-HC33-DAYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       200231-SYUNOU-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-T1
           MOVE    ZERO                TO  WRK-GOK-JIHIKEI
      *
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    ORCHC33-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    ORCHC33-PTID        TO  SYU-PTID
           MOVE    WRK-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE    WRK-DAY             TO  SYU-SRYYMD(7:2)
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           PERFORM UNTIL    FLG-SYUNOU     =   1
      *H24.9
               MOVE    ZERO                TO  FLG-SYORI
               IF      (ORCHC33-SRYKA  NOT =   ZERO  AND  SPACE) AND
                       (ORCHC33-SRYKA  NOT =   SYU-SRYKA)
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
      *        保険組合せチェック
               IF      (PARA-HKNCOMBIKBN    =   "1"  OR  "2")
                 AND   (FLG-SYORI           =   1           )
                   MOVE    ZERO                 TO  FLG-SYORI
                   MOVE    SYU-HKNCOMBINUM      TO  WRK-CHK-HKNCOMBI
                   PERFORM 200139-HKNCOMBI-CHK-SEC
               END-IF
      *
               IF      FLG-SYORI           =   1
      *----(03.04.01)--UPD-START---
      *            自費計
                   COMPUTE WRK-JIHIKEI     =   SYU-JIHI-TOTAL
                                           +   SYU-JIHI-TOTAL-TAX
                                           +   SYU-TOTAL-TGMONEY
                                           +   SYU-TOTAL-TGMONEY-TAX
                   IF      SYU-SRYYMD      <   CONST-H190401
                       COMPUTE WRK-JIHIKEI     =   WRK-JIHIKEI
                                               +   SYU-JIHI-TAX
                   END-IF
      *----(03.04.01)--UPD-END-----
                   ADD     WRK-JIHIKEI         TO  WRK-GOK-JIHIKEI
                   ADD     WRK-JIHIKEI         TO  WRK-TUK-JIHIKEI
      *            請求金額−自費計
                   COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY
                                           -   WRK-JIHIKEI
      *            負担額
                   IF      WRK-SKYMONEY    NOT =   ZERO
      *H25.11
                   ADD     1                   TO  IDX-T1
                   MOVE    WRK-SKYMONEY        TO  WRK-Z79S
                   MOVE    WRK-Z79S            TO  TBL-NAIYO (IDX-T1 9)
      *
                   END-IF
      *            月計負担額
                   ADD     WRK-SKYMONEY        TO  WRK-TUK-SKYMONEY
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *
      *    自費
           IF      WRK-GOK-JIHIKEI     NOT =   ZERO
               MOVE    WRK-GOK-JIHIKEI     TO  WRK-Z89
      *        １行目へ
               MOVE    WRK-Z89             TO  TBL-NAIYO  (1  10)
               IF      IDX-MAX             =   ZERO
                   MOVE    1               TO  IDX-MAX
               END-IF
           END-IF
      *
           .
       200231-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    収納編集処理
      *****************************************************************
       200232-SYUNOU-DAYHEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  IDX-T1
           MOVE    ZERO                TO  WRK-GOK-JIHIKEI
           MOVE    ZERO                TO  WRK-GOK-SKYMONEY
      *
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    ORCHC33-NYUGAIKBN   TO  SYU-NYUGAIKBN
           MOVE    ORCHC33-PTID        TO  SYU-PTID
           MOVE    WRK-SRYYM           TO  SYU-SRYYMD(1:6)
           MOVE    WRK-DAY             TO  SYU-SRYYMD(7:2)
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           PERFORM UNTIL    FLG-SYUNOU     =   1
      *H24.9
               MOVE    ZERO                TO  FLG-SYORI
               IF      (ORCHC33-SRYKA  NOT =   ZERO  AND  SPACE) AND
                       (ORCHC33-SRYKA  NOT =   SYU-SRYKA)
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SYORI
               END-IF
      *        保険組合せチェック
               IF      (PARA-HKNCOMBIKBN    =   "1"  OR  "2")
                 AND   (FLG-SYORI           =   1           )
                   MOVE    ZERO                 TO  FLG-SYORI
                   MOVE    SYU-HKNCOMBINUM      TO  WRK-CHK-HKNCOMBI
                   PERFORM 200139-HKNCOMBI-CHK-SEC
               END-IF
      *
               IF      FLG-SYORI           =   1
      *            自費計
                   COMPUTE WRK-JIHIKEI     =   SYU-JIHI-TOTAL
                                           +   SYU-JIHI-TOTAL-TAX
                                           +   SYU-TOTAL-TGMONEY
                                           +   SYU-TOTAL-TGMONEY-TAX
                   IF      SYU-SRYYMD      <   CONST-H190401
                       COMPUTE WRK-JIHIKEI     =   WRK-JIHIKEI
                                               +   SYU-JIHI-TAX
                   END-IF
      *----(03.04.01)--UPD-END-----
                   ADD     WRK-JIHIKEI         TO  WRK-GOK-JIHIKEI
                   ADD     WRK-JIHIKEI         TO  WRK-TUK-JIHIKEI
      *            請求金額−自費計
                   COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY
                                           -   WRK-JIHIKEI
      *            負担額
                   ADD     WRK-SKYMONEY        TO  WRK-GOK-SKYMONEY
      *            月計負担額
                   ADD     WRK-SKYMONEY        TO  WRK-TUK-SKYMONEY
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    テーブル編集
           ADD     1                   TO  IDX-T1
           MOVE    WRK-GOK-SKYMONEY    TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  TBL-NAIYO (IDX-T1 9)
      *
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *
      *    自費
           IF      WRK-GOK-JIHIKEI     NOT =   ZERO
               MOVE    WRK-GOK-JIHIKEI     TO  WRK-Z89
      *        １行目へ
               MOVE    WRK-Z89             TO  TBL-NAIYO  (1  10)
           END-IF
      *
           .
       200232-SYUNOU-DAYHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    月計印刷処理
      *****************************************************************
       20024-TUKIKEI-SEC                SECTION.
      *
           ADD     1                   TO  CNT-MEI
           IF      CNT-MEI             >   40
      *        印刷処理
               PERFORM 2003-HC33-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
           END-IF
           IF      CNT-MEI             =   1
               PERFORM 20031-HEAD-HEN-SEC
           END-IF
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM33-NUM (CNT-MEI)
      *
           MOVE    "合計"              TO  HCM33-BIKOU (CNT-MEI)
           MOVE    WRK-TUK-ZAITEN      TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33-NAIYO (CNT-MEI 8)
           MOVE    WRK-TUK-SKYMONEY    TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  HCM33-NAIYO (CNT-MEI 9)
           MOVE    WRK-TUK-JIHIKEI     TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  HCM33-NAIYO (CNT-MEI 10)
      *
           IF      WRK-TUK-RSI-MONEY   =   ZERO
               CONTINUE
           ELSE
      *        労災・自賠責金額計処理
               PERFORM 200241-TUKIKEI-ROUSEI-SEC
           END-IF
           .
       20024-TUKIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    月計印刷処理（労災・自賠責）
      *****************************************************************
       200241-TUKIKEI-ROUSEI-SEC                SECTION.
      *
      *    労災・自賠責金額計処理
           ADD     1                   TO  CNT-MEI
           IF      CNT-MEI             >   40
               PERFORM 2003-HC33-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
           END-IF
           IF      CNT-MEI             =   1
               PERFORM 20031-HEAD-HEN-SEC
           END-IF
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM33-NUM (CNT-MEI)
      *
           MOVE    "※労・自"          TO  HCM33-BIKOU (CNT-MEI)
           MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33-NAIYO (CNT-MEI 8)
           MOVE    "*"                 TO  HCM33-NAIYO (CNT-MEI 8)(1:1)
      *
           .
       200241-TUKIKEI-ROUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ見出し編集処理
      *****************************************************************
       20031-HEAD-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  HCM33
           INITIALIZE                      HCM33
      *
           ADD     1                   TO  ORCHC33-PAGE
           MOVE    ORCHC33-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM33-PAGE
      *
           MOVE    ORCHC33-PTNUM       TO  HCM33-PTNUM
      *    患者マスターより
      *    患者氏名
           MOVE    ORCHC33-NAME        TO  HCM33-NAME
      *    性別
           EVALUATE    ORCHC33-SEX(1:1)
               WHEN    "1"
                   MOVE    "男"                TO  HCM33-SEX
               WHEN    "2"
                   MOVE    "女"                TO  HCM33-SEX
               WHEN    OTHER
                   MOVE    ORCHC33-SEX         TO  HCM33-SEX
           END-EVALUATE
      *
           MOVE    WRK-BIRTHDAY        TO  HCM33-TANJYOBI
      **** 年齢
      *    IF      WRK-NENREI-YY       >   ZERO
      *        MOVE    WRK-NENREI-YY       TO  WRK-ZZ9
      *        MOVE    WRK-ZZ9             TO  HCM33-NENREI
      *        MOVE    "才"                TO  HCM33-NENREIMEI
      *    ELSE
      *        MOVE    WRK-NENREI-MM       TO  WRK-ZZ9
      *        MOVE    WRK-ZZ9             TO  HCM33-NENREI
      *        MOVE    "ヶ月"              TO  HCM33-NENREIMEI
      **** END-IF
      *    診療科
           MOVE    WRK-SRYKA-MEI       TO  HCM33-SRYKA
      *    期間
           MOVE    WRK-SRYYM-N         TO  HCM33-SRYYM
      *
           PERFORM 200311-HKNSYUBETU-HEN-SEC
      *
           MOVE    WRK-HEN-HKNCOMBIMEI     TO  HCM33-HKNSYUBETU
           .
       20031-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険名称編集処理
      *****************************************************************
       200311-HKNSYUBETU-HEN-SEC                SECTION.
      *
      *    保険名称
           MOVE    SPACE               TO  WRK-HEN-HKNCOMBIMEI
           PERFORM VARYING     IDX-H   FROM    1   BY  1
                   UNTIL      (IDX-H   >   20  )  OR
                              (TBL-HKNCOMBI (IDX-H)  =   SPACE )
               MOVE    WRK-HEN-HKNCOMBIMEI     TO  WRK-HKNCOMBIMEI
               MOVE    SPACE                   TO  WRK-HEN-HKNCOMBIMEI
               MOVE    SPACE                   TO  WRK-MEI-1
               MOVE    TBL-HONKZKKBN (IDX-H)   TO  WRK-HONKZKKBN
               MOVE    "　"                    TO  WRK-HONKZKKBN(5:2)
               IF      IDX-H               >   1
                   MOVE    "／"                TO  WRK-MEI-1
                   IF      TBL-HONKZKKBN (IDX-H)   =
                                               TBL-HONKZKKBN (IDX-H - 1)
                       MOVE    SPACE           TO  WRK-HONKZKKBN
                   END-IF
               END-IF
               STRING  WRK-HKNCOMBIMEI         DELIMITED  BY  SPACE
                       WRK-MEI-1               DELIMITED  BY  SPACE
                       WRK-HONKZKKBN           DELIMITED  BY  SPACE
                       TBL-HKNCOMBIMEI(IDX-H)  DELIMITED  BY  SPACE
                                           INTO    WRK-HEN-HKNCOMBIMEI
               END-STRING
           END-PERFORM
      *
           .
       200311-HKNSYUBETU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    カルテ印刷処理
      *****************************************************************
       2003-HC33-PRINT-SEC                SECTION.
      *
           EVALUATE    ORCHC33-KBN
      *        オンライン印刷
               WHEN    "1"
                   PERFORM 20031-HC33-ONL-SEC
      *        バッチ印刷
               WHEN    "2"
                   PERFORM 20032-HC33-BAT-SEC
           END-EVALUATE
      *
      *v4.8
           ADD     1                   TO  CNT-PAGE
           .
       2003-HC33-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ印刷処理（オンライン）
      *****************************************************************
       20031-HC33-ONL-SEC                SECTION.
      *
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
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
           MOVE    HCM33               TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCM33.red"         TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ******MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1031-PRTNM(10)  TO  MKPRT-PRTNM
            MOVE   "カルテ３号紙"      TO  MKPRT-INFO
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       20031-HC33-ONL-EXT.
           EXIT.
      *****************************************************************
      *    カルテ印刷処理（バッチ）
      *****************************************************************
       20032-HC33-BAT-SEC         SECTION.
      *
           MOVE    ORCHC33-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    HCM33               TO  SPRT-PRTDATA
           MOVE    "1"                 TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    9               TO  ORCHC33-RC
           END-IF
      *
           MOVE    ORCSPRTAREA        TO  ORCHC33-PRTAREA
      *
           .
       20032-HC33-BAT-EXT.
           EXIT.
      *****************************************************************
      *    追加印刷処理
      *****************************************************************
       2004-HC331-HENSYU-SEC           SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1    BY  1
                   UNTIL      (IDX1        >   IDX-MAX )  OR
                              (CNT-MEI     >=  40      )
               ADD     1                   TO  CNT-MEI
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM331-NUM (CNT-MEI)
      *        日付
               IF      IDX1                =   1
                   MOVE    WRK-HEN-DAY     TO  HCM331-SRYMD (CNT-MEI)
               END-IF
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   10
                   MOVE    TBL-NAIYO (IDX1 IDX2)   TO  HCM331-NAIYO
                                                          (CNT-MEI IDX2)
               END-PERFORM
               MOVE    TBL-MEISYO (IDX1)   TO  HCM331-BIKOU (CNT-MEI)
               MOVE    IDX1                TO  IDX-STR
           END-PERFORM
      *
           IF      CNT-MEI             >=  40
      *        印刷処理
               PERFORM 2004-HC331-PRINT-SEC
               MOVE    ZERO                TO  CNT-MEI
               MOVE    1                   TO  FLG-HC331-END
           END-IF
           ADD     1                   TO  IDX-STR
      *
           .
       2004-HC331-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理
      *****************************************************************
       20042-HC331-TUKIKEI-SEC                SECTION.
      *
           ADD     1                   TO  CNT-MEI
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM331-NUM (CNT-MEI)
      *
           MOVE    "合計"              TO  HCM331-BIKOU (CNT-MEI)
           MOVE    WRK-TUK-ZAITEN      TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM331-NAIYO (CNT-MEI 8)
           MOVE    WRK-TUK-SKYMONEY    TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  HCM331-NAIYO (CNT-MEI 9)
           MOVE    WRK-TUK-JIHIKEI     TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  HCM331-NAIYO (CNT-MEI 10)
      *
           IF      WRK-TUK-RSI-MONEY   =   ZERO
      *        印刷処理
               PERFORM 2004-HC331-PRINT-SEC
           ELSE
      *        労災・自賠責金額計処理
               PERFORM 200421-HC331-ROUSAI-SEC
           END-IF
           .
       20042-HC331-TUKIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理（労災・自賠責）
      *****************************************************************
       200421-HC331-ROUSAI-SEC                SECTION.
      *
      *    労災・自賠責金額計処理
           ADD     1                   TO  CNT-MEI
           IF      CNT-MEI             >   40
               PERFORM 2004-HC331-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
      *        ＨＣ３３の印刷
               PERFORM 20031-HEAD-HEN-SEC
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM33-NUM (CNT-MEI)
      *
               MOVE    "※労・自"          TO  HCM33-BIKOU (CNT-MEI)
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  HCM33-NAIYO (CNT-MEI 8)
               MOVE    "*"                 TO  HCM33-NAIYO (CNT-MEI 8)
                                                                (1:1)
      *        印刷処理
               PERFORM 2003-HC33-PRINT-SEC
           ELSE
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM331-NUM (CNT-MEI)
      *
               MOVE    "※労・自"          TO  HCM331-BIKOU (CNT-MEI)
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  HCM331-NAIYO (CNT-MEI 8)
               MOVE    "*"                 TO  HCM331-NAIYO (CNT-MEI 8)
                                                                (1:1)
      *        印刷処理
               PERFORM 2004-HC331-PRINT-SEC
           END-IF
      *
           .
       200421-HC331-ROUSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理
      *****************************************************************
       2004-HC331-PRINT-SEC                SECTION.
      *
      *    オンラインのみ
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *
      *v4.8
           ADD     1                   TO  CNT-PAGE
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
           MOVE    HCM331              TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE   "HC01.sh"            TO  MKPRT-ID
           MOVE   "HCM331.red"         TO  MKPRT-DIA
           MOVE   SPACE                TO  MKPRT-DEF
      **** MOVE   HC01PARA             TO  MKPRT-DAT
           MOVE   ASGNPARA             TO  MKPRT-DAT
           MOVE   SYS-1031-PRTNM(13)   TO  MKPRT-PRTNM
           IF     SYS-1031-PRTNM(13)   =   SPACE
                MOVE   SYS-1031-PRTNM(10)  TO  MKPRT-PRTNM
           END-IF
           MOVE   "カルテ３号紙（追記）"   TO  MKPRT-INFO
           CALL   "ORCSMKPRT"          USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2004-HC331-PRINT-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ見出し編集処理
      *****************************************************************
       7001-API-HEAD-HEN-SEC           SECTION.
      *
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
           MOVE    ORCHC33-PTID    TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    "karte_no3"     TO  FMKARTE3-FORM-ID
           MOVE    PARA-API-CUSTOM-ID 
                                   TO  FMKARTE3-CUSTOM-ID
           MOVE   SYS-1031-PRTNM(10)
                                   TO  FMKARTE3-PRINTER
           IF    ( SYS-1031-PRTNM(13)  NOT =   SPACE )
               MOVE   SYS-1031-PRTNM(13)
                                   TO  FMKARTE3-PRINTER-ZOKUSHI
           ELSE
               MOVE   SYS-1031-PRTNM(10)
                                   TO  FMKARTE3-PRINTER-ZOKUSHI
           END-IF
      *
           MOVE    ORCHC33-ENDPAGE TO  WRK-ENDPAGE
           IF    ( ORCHC33-ENDLINE =   ZERO )
               COMPUTE WRK-ENDPAGE
                   =   ORCHC33-ENDPAGE     +   1
           ELSE
               IF    ( ORCHC33-ENDPAGE     =   ZERO )
                   MOVE    1       TO  WRK-ENDPAGE
               END-IF
           END-IF
           MOVE    WRK-ENDPAGE     TO  API-ENDPAGE
           MOVE    API-ENDPAGE-G   TO  FMKARTE3-ENDPAGE
      *
           MOVE    ORCHC33-ENDLINE TO  API-ENDLINE
           MOVE    API-ENDLINE-G   TO  FMKARTE3-ENDLINE
      *
           MOVE    ORCHC33-SRYYM   TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:)
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE(1:7)
                                   TO  FMKARTE3-SRYYM
      *
           IF  ( ORCHC33-PRINT-FLG =   1 )
               MOVE    "2"         TO  FMKARTE3-HAKKOKBN
           ELSE
               MOVE    "1"         TO  FMKARTE3-HAKKOKBN
           END-IF
      *
           IF    ( ORCHC33-SRYKA   =   SPACE OR ZERO )
               CONTINUE
           ELSE
               MOVE    ORCHC33-SRYKA
                                   TO  FMKARTE3-SRYKA
           END-IF
      *
           MOVE    WRK-SRYKA-MEI   TO  FMKARTE3-SRYKANAME
      *
           PERFORM 200311-HKNSYUBETU-HEN-SEC
           MOVE    WRK-HEN-HKNCOMBIMEI
                                   TO  FMKARTE3-HKNSYUBETU
      *
           MOVE    ORCHC33-PTNUM   TO  FMKARTE3-PTNUM
           MOVE    PTINF-NAME      TO  FMKARTE3-NAME
           MOVE    PTINF-KANANAME  TO  FMKARTE3-KANANAME
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  FMKARTE3-BIRTHDAY
           MOVE    PTINF-SEX       TO  FMKARTE3-SEX
      *
           .
       7001-API-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ明細編集処理
      *****************************************************************
       7002-API-BODY-SEC               SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   IDX-MAX
      *
               INITIALIZE                  API-AREA
      *
               MOVE    WRK-SRYYM       TO  API-SRYYM
               MOVE    WRK-DAY         TO  API-SRYDD
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   10
                   MOVE    TBL-NAIYO (IDX1 IDX2)
                                       TO  API-NAIYO (IDX2)
               END-PERFORM
               MOVE    TBL-MEISYO (IDX1)
                                       TO  API-BIKOU
               PERFORM 70021-API-BODY-SEC
      *
           END-PERFORM
      *
           .
       7002-API-BODY-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ明細編集処理
      *****************************************************************
       70021-API-BODY-SEC              SECTION.
      *
           IF    ( CNT-API         <   CONST-API-BODY-MAX )
      *
               COMPUTE CNT-API =   CNT-API     +   1
      *
               MOVE    API-SRYYMD      TO  WRK-SYMD
               PERFORM 801-DAYHEN01-SEC
               MOVE    WRK-HEN-DATE    TO  FMKARTE3-SRYYMD(CNT-API)
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   10
                   MOVE    API-NAIYO (IDX2)
                                       TO  FMKARTE3-NAIYO(CNT-API IDX2)
               END-PERFORM
               MOVE    API-BIKOU       TO  FMKARTE3-BIKOU (CNT-API)
      *
           END-IF
      *
           .
       70021-API-BODY-EXT.
           EXIT.
      *****************************************************************
      *    ＡＰＩ明細編集処理
      *****************************************************************
       7003-API-TUKIKEI-SEC               SECTION.
      *
           INITIALIZE                      API-AREA
      *
           MOVE    "合計"              TO  API-BIKOU
           MOVE    WRK-TUK-ZAITEN      TO  WRK-Z89
           MOVE    WRK-Z89             TO  API-NAIYO (8)
           MOVE    WRK-TUK-SKYMONEY    TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  API-NAIYO (9)
           MOVE    WRK-TUK-JIHIKEI     TO  WRK-Z79S
           MOVE    WRK-Z79S            TO  API-NAIYO (10)
           PERFORM 70021-API-BODY-SEC
      *
           IF      WRK-TUK-RSI-MONEY   NOT =   ZERO
      *
               INITIALIZE                      API-AREA
               MOVE    "※労・自"          TO  API-BIKOU
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  API-NAIYO (8)
               MOVE    "*"                 TO  API-NAIYO (8) (1:1)
               PERFORM 70021-API-BODY-SEC
           END-IF
      *
           .
       7003-API-TUKIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       7009-API-CALLSYORI-SEC         SECTION.
      *
           PERFORM 7001-API-HEAD-HEN-SEC
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    FMKARTE3-FORM-ID    TO  RPMAIN-ID
           MOVE    FMKARTE3-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "カルテ３号紙"      TO  RPMAIN-TITLE
           MOVE    "fm_karte_no3"      TO  MCP-TABLE
           MOVE    FMKARTE3-DATA       TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           MOVE    1                   TO   CNT-PAGE
      *
           .
       7009-API-CALLSYORI-EXT.
           EXIT.
      *****************************************************************
      *    日付編集処理(API）
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
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       900-SRYACCT-READ-EXT.
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
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-RAED-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       950-PTHKNINF-READ-SEC         SECTION.
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
                   INITIALIZE                  PTHKNINF-REC
                   MOVE    1               TO  FLG-PTHKNINF
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTHKNINF
               INITIALIZE                      PTHKNINF-REC
           END-IF
     *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTHKNINF-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       950-SRYACCTPLUS-READ-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
           MOVE    1                   TO  ACCTP-RENNUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   MOVE    1                   TO  FLG-SRYACCTPLUS
                   INITIALIZE                  SRYACCTPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
               INITIALIZE                  SRYACCTPLUS-REC
           END-IF
      *
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-SRYACCTPLUS-READ-EXT.
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
