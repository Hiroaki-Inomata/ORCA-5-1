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
       PROGRAM-ID.             ORCHCN33.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 会計照会
      *  コンポーネント名  : カルテ３号様式（診療録）印刷（入院）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  04/12/16    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.00    NACL-多々納  06/02/02  MONFUNC 対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/10/23  公務災害対応
      *  04.04.00    NACL-多々納  08/12/24  入院料包括対応
      *  04.07.00    NACL-多々納  12/09/25  プログラムオプション保険対応
      *  04.07.01    NACL-門脇    13/09/30  自賠責（第三者行為）対応
      *  04.07.00    NACL-多々納  14/02/13  労災金額％加算対応
      *  04.08.00    NACL-多々納  14/08/01  一時ディレクトリ対応
      *  04.08.00    NACL-多々納  16/11/XX  点滴薬剤計算オプション対応
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
      *    カルテ３号様式（入院）
           COPY    "HCM33N.INC".
      *
      *    カルテ３号様式（続き）
           COPY    "HCM331N.INC".
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
           03  FLG-NYUINACCT       PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-SYORI           PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-ARI-RSI         PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-HKNKBN          PIC 9(01).
      *
           03  FLG-GENTEN          PIC 9(01).
           03  FLG-HCN331          PIC 9(01).
           03  FLG-HCN331-END      PIC 9(01).
           03  FLG-TUKI-END        PIC 9(01).
           03  FLG-PRINT-OK        PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
      *    労災金額％加算 
           03  FLG-KAGEN           PIC 9(01).
      *H28.11
      *    点滴点数編集 
           03  FLG-TENTEKI         PIC 9(01).
           03  FLG-TENTEKI-ARI     PIC 9(01).
           03  FLG-HAIKI           PIC 9(01).
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
           03  IDX-T               PIC 9(04).
           03  IDX-T1              PIC 9(04).
           03  IDX-RSI             PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
           03  IDX-MAX             PIC 9(04).
           03  IDX-STR             PIC 9(04).
      *
           03  IDZ                 PIC 9(04).
           03  IDX-D               PIC 9(02).
           03  IDXH                PIC 9(04).
      *
           03  IDX-C               PIC 9(04).
      *
           03  IDX-END             PIC 9(02).
      *H28.11
      *    点滴点数編集 
           03  IDX-T2              PIC 9(04).
           03  IDX-T3              PIC 9(04).
           03  IDX-TBL             PIC 9(04).
           03  IDXA                PIC 9(04).
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
           03  WRK-HKNCOMBI-G.
               05  WRK-HKNCOMBI-9      PIC 9(04).
      *
           03  WRK-ZZ9                 PIC ZZ9.
           03  WRK-Z9                  PIC Z9.
           03  WRK-Z59-X.
               05  WRK-Z59                 PIC ZZZZ9.
               05  WRK-Z59-R               REDEFINES   WRK-Z59
                                           PIC ----9.
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
      *    集計
           03  WRK-GOK-ZAITEN          PIC S9(07).
           03  WRK-GOK-JIHIKEI         PIC 9(07).
           03  WRK-GOK-SYOKUJI         PIC S9(07).
           03  WRK-JIHIKEI             PIC 9(07).
           03  WRK-GOK-RSI-MONEY       PIC S9(07).
      *
           03  WRK-SUM-TEN             PIC 9(07).
           03  WRK-ZAITEN              PIC S9(07).
           03  WRK-RSI-MONEY           PIC 9(07).
      *
           03  WRK-TUK-ZAITEN          PIC 9(07).
           03  WRK-TUK-SYOKUJI         PIC S9(07).
           03  WRK-TUK-JIHIKEI         PIC S9(07).
           03  WRK-TUK-RSI-MONEY       PIC 9(07).
      *
           03  WRK-PAGE                PIC 9(03).
           03  WRK-PRT-CNT             PIC 9(03).
      *H24.9
           03  WRK-PARA-HKNNUM         PIC X(03).
           03  WRK-CHK-HKNCOMBI        PIC X(04).
      *H28.11
      *    点滴点数編集 
           03  WRK-TENSU               PIC 9(07)V9(04).
           03  WRK-ZAITEN-TEN          PIC 9(09).
           03  WRK-HAIKI-SURYO         PIC 9(07)V9(05).
           03  WRK-SURYO-9             PIC 9(07).
           03  WRK-SURYO               PIC 9(07)V9(05).
           03  WRK-SYOKEIS2            PIC 9(09)V9(06).
           03  WRK-TENSU-KEI           PIC 9(08)V9(06).
           03  WRK-TENSU-GOK           PIC 9(08)V9(06).
           03  WRK-ENDPAGE             PIC 9(03).
      *
      *    集計テーブル
       01  TBL-AREA.
           03  TBL-OCC-1               OCCURS   11.
      *        剤点数
               05  TBL-OCC-2           OCCURS   400.
                   07  TBL-ZAITEN          PIC S9(09).
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
      *H28.11
      *    点滴数量計用
       01  TBL-TENTEKI-AREA.
           03  TBL-TENTEKI-OCC         OCCURS   100.
               05  TBL-TEN-SRYCD           PIC X(09).
               05  TBL-TEN-TEN             PIC 9(07)V9(5).
               05  TBL-TEN-DAY-AREA        OCCURS   31.
                   07  TBL-TEN-DAY             PIC 9(03).
                   07  TBL-TEN-SURYO           PIC 9(05)V9(05).
       01  TBL-TENTEKI-AREA2.
           03  TBL-TENTEKI2-DAY-AREA    OCCURS   31.
               07  TBL-TEN2-TENSU          PIC S9(09).
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
           03  TBL-DAY-OCC               OCCURS   11.
      *        剤点数
               05  TBL-DAY-ZAITEN          PIC S9(09).
      *        労災円
               05  TBL-DAY-RSI-ZAITEN      PIC S9(09).
      *
      *    帳票編集テーブル
       01  TBL-HEN-AREA.
           03  TBL-HEN-OCC            OCCURS   400.
               05  TBL-HEN-OCC2            OCCURS   11.
                   07  TBL-NAIYO           PIC X(08).
               05  TBL-MEISYO              PIC X(12).
      *
      *    対象保険組合せテーブル
       01  TBL-ALL-HKNCOMBI-AREA.
           03  TBL-ALL-HKNCOMBI-OCC        OCCURS   99.
               05  TBL-ALL-HKNCOMBINUM     PIC X(04).
               05  TBL-ALL-HKNNUM          PIC X(03).
      *
      *H28.11
      *    診療行為マスター
       01  TBL-SRYACT-AREA.
           02  TBL-SRYACT-REC         OCCURS  10.
           COPY    "CPSRYACT.INC"     REPLACING
                                     //SRY-// BY //TBL-SRY-//.
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
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-HKNCOMBIKBN        PIC X(01).
      *H28.11
           03  PARA-TENTEKI            PIC X(01).
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
                                       OCCURS 11.
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
           COPY  "CPSK1034.INC".
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
      *    入院会計マスタ−
       01  NYUINACCT-REC.
           COPY    "CPNYUINACCT.INC".
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
           COPY    "CPFMKARTE3N.INC".
           COPY    "CPORCRPMAIN.INC".
      *
      *一括用
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    入院会計取得パラメタ
           COPY    "CPORCHCN33SUB.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *
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
           INITIALIZE                  TBL-RSI-AREA
           INITIALIZE                  TBL-HKNCOMBI-AREA
           INITIALIZE                  TBL-HEN-AREA
      *    点滴数量計用
           INITIALIZE                  TBL-TENTEKI-AREA
           INITIALIZE                  TBL-TENTEKI-AREA2
      *
           INITIALIZE                  FMKARTE3N-REC
      *
           ACCEPT  SYS-TIME            FROM    TIME
           MOVE    ZERO                TO  WRK-PAGE
      *
           EVALUATE    ORCHC33-KBN
               WHEN    "1"
      *            オンライン
      *            出力先プリンタ名編集処理
                   PERFORM 110-SYS1034-HENSYU-SEC
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
      *H28.11
      *    点滴点数集計
           MOVE    "TENTEKI"           TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)       TO  PARA-TENTEKI
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
           MOVE    "ORCHCN33"          TO  ORCSPRGOPTION-PRGID
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
       110-SYS1034-HENSYU-SEC                   SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                      SYS-1034-REC
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "2"                 TO  ORCSPRTNM-KBN
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "1"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       110-SYS1034-HENSYU-EXT.
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
               MOVE    ZERO            TO  FLG-HCN331
           ELSE
               MOVE    1               TO  FLG-HCN331
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
              MOVE    SPACE               TO  HCM33N-PAGE
              MOVE    SPACE               TO  HCM33N-SRYYM
      *       印刷処理
              PERFORM 2003-HCN33-PRINT-SEC
           END-IF
      *
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
           MOVE    SPACE               TO  HCM33N
      *
      *    追加印刷初期処理
           IF      FLG-HCN331           =   1
               MOVE    SPACE               TO  HCM331N
               INITIALIZE                      HCM331N
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
               PERFORM 2002-PRINT-SYORI-SEC
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
                   PERFORM 2003-HCN33-PRINT-SEC
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
               IF    ( ORCHC33-SRYYM-END   NOT =   SPACE )  AND
                     ((ORCHC33-SRYYM-END       <   COMB-TEKSTYMD(1:6))
                  OR  (ORCHC33-SRYYM-END       >   COMB-TEKEDYMD(1:6)))
                   MOVE    ZERO            TO  FLG-HKNCOMBICHK
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
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       2001-SYUKEI-SYORI-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-SYORI
      *    集計テーブル
           INITIALIZE                  TBL-AREA
           INITIALIZE                  TBL-RSI-AREA
           MOVE    ZERO                TO  WRK-TUK-ZAITEN
           MOVE    ZERO                TO  WRK-TUK-SYOKUJI
           MOVE    ZERO                TO  WRK-TUK-JIHIKEI
           MOVE    ZERO                TO  WRK-TUK-RSI-MONEY
      *    点滴数量計用
           INITIALIZE                  TBL-TENTEKI-AREA
           INITIALIZE                  TBL-TENTEKI-AREA2
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
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
      ***          IF      ACCT-ZAITEN         >   ZERO
                   IF      FLG-OK              =   1
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
      *!!!!
      *H28.11
      *    点滴薬剤日毎集計
           IF      PARA-TENTEKI        =   "1"
      *        点滴点数再計算
               PERFORM 20019-TENTEKI-SAIKEI-SEC
           END-IF
      *
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
                   EVALUATE    ACCT-SRYKBN
                   WHEN    "95"
                   WHEN    "96"
                       MOVE    11              TO  IDX1
                   WHEN    OTHER
                       MOVE    ZERO            TO  IDX1
                   END-EVALUATE
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
               DISPLAY "** ORCHCN33 ZAI TBL 400 OVER **"
           END-IF
      *
           IF     (IDX2                =   ZERO ) OR
                  (IDX1                =   ZERO )
               CONTINUE
           ELSE
               IF      ACCT-SRYKBN     =   "95"  OR  "96"
      *            保険外集計
                   PERFORM 200111-SRYACCT-JIHI-SEC
               ELSE
                   PERFORM 200110-SRYACCT-HENSYU-SEC
               END-IF
           END-IF
           .
       20011-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       200110-SRYACCT-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-ARI2
           MOVE    ZERO                TO  WRK-RSI-MONEY
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
                   IF     (ACCT-SRYKBN         =   "21")   AND
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
      *!!!!
      *H28.11
      *    点滴薬剤日毎集計
           IF      PARA-TENTEKI        =   "1"
               IF     (ACCT-SRYKBN     =   "33" )
                 AND  (ACCT-ZAITEN     >   ZERO )
                 AND  (ACCT-YKZTEN     >   ZERO )
                   PERFORM 2001109-TENTEKI-SYORI-SEC
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
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
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
                                                       "09593" OR
                                                       "09594" )
                       MOVE    SRY-SRYCD (IDZ)     TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                労災の金額算定分の時、点数をゼロとする
                       IF     (TNS-TENSIKIBETU     =   1  )  OR
                              (TNS-SRYCD (1:5)     =   "09591" OR
                                                       "09592" OR
                                                       "09593" OR
                                                       "09594" )
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
               PERFORM 930-SRYACT-RAED-SEC
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
      *****************************************************************
      *    保険外集計処理
      *****************************************************************
       200111-SRYACCT-JIHI-SEC                SECTION.
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    自費金額
           IF      SRY-JIHIMONEYTOTAL  NOT =   ZERO
      *        点数編集
               MOVE    SRY-JIHIMONEYTOTAL  TO  TBL-ZAITEN (IDX1 IDX2)
      *        回数編集
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   31
                   MOVE    ACCT-DAY(1  IDX)    TO  TBL-DAY
                                                       (IDX1 IDX2 IDX)
               END-PERFORM
           END-IF
      *
           .
       200111-SRYACCT-JIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    逓減点数編集処理
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
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
           MOVE    ZERO                TO  FLG-OK
      *    保険組合せ対象チェック
           IF      (PARA-HKNCOMBIKBN    =   "1"  OR  "2")
               MOVE    ACCT-HKNCOMBI        TO  WRK-CHK-HKNCOMBI
               PERFORM 200139-HKNCOMBI-CHK-SEC
               IF      FLG-OK               =   ZERO
                   GO      TO      20013-HKNCOMBI-HEN-EXT
               END-IF
           ELSE
               MOVE    1                   TO  FLG-OK
           END-IF
      *
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
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    ORCHC33-PTID        TO  COMB-PTID
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
             IF    COMB-KOH1HKNNUM     =   "970"
               MOVE    2               TO  TBL-HKNKBN (IDX)
             ELSE
               MOVE    ZERO            TO  TBL-HKNKBN (IDX)
             END-IF
           END-IF
           .
       200131-HKNCOMBI-MEI-EXT.
           EXIT.
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
      *
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
                       MOVE    1                   TO  FLG-OK
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
                               MOVE    ZERO                TO  FLG-OK
                           ELSE
                               MOVE    1                   TO  FLG-OK
                           END-IF
                           MOVE    99                  TO  IDX-C
                       END-IF
                   END-PERFORM
               END-IF
           WHEN    "2"
      *        保険組合せ一致のみ
               IF      ORCHC33-HKNCOMBI    =   WRK-CHK-HKNCOMBI
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-EVALUATE
           .
       200139-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *H28.11
      *****************************************************************
      *     点滴金額集計処理
      *****************************************************************
       2001109-TENTEKI-SYORI-SEC                SECTION.
      *
           INITIALIZE                  TBL-SRYACT-AREA
           MOVE    ZERO                TO  IDX-T
           MOVE    ZERO                TO  FLG-TENTEKI-ARI
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
               PERFORM 930-SRYACT-RAED-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM   UNTIL     FLG-SRYACT      =   1
               ADD     1                   TO  IDX-T
               IF      IDX-T               <=  10
                   MOVE    SRYACT-REC      TO  TBL-SRYACT-REC (IDX-T)
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-RAED-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX-T2  FROM    1   BY  1
                   UNTIL      (IDX-T2      >   IDX-T  )
               MOVE    TBL-SRYACT-REC (IDX-T2)     TO  SRYACT-REC
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2) =   SPACE )
      *            点滴・中心静脈薬剤
                   IF     (SRY-SRYSYUKBN           =   "330"
                                                   OR  "331"
                                                   OR  "350"
                                                   OR  "332"
                                                   OR  "352")
      *                薬剤
                       IF     (SRY-SRYCD (IDX2)(1:1)   =   "6" )
                           MOVE    1           TO  FLG-TENTEKI-ARI
                           PERFORM 2001109-TBL-HENSYU-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    集計点数（薬剤点数を除く）
           IF      FLG-TENTEKI-ARI     =   1
               COMPUTE WRK-ZAITEN          =   ACCT-ZAITEN
                                           -   ACCT-YKZTEN
           END-IF
      *
           .
       2001109-TENTEKI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル編集処理
      *****************************************************************
       2001109-TBL-HENSYU-SEC            SECTION.
      *
      *    同一コードチェック
           MOVE    ZERO                TO  IDX-TBL
           PERFORM VARYING IDX-T3      FROM    1   BY  1
                   UNTIL  (IDX-T3      >   100 )
                      OR  (IDX-TBL     >   ZERO)
               IF      SRY-SRYCD (IDX2)    =   TBL-TEN-SRYCD (IDX-T3)
                   MOVE    IDX-T3              TO  IDX-TBL
               ELSE
                   IF      TBL-TEN-SRYCD (IDX-T3)  =   SPACE
                       MOVE    IDX-T3              TO  IDX-TBL
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      IDX-TBL             =   ZERO
      *        テーブルオーバー
               GO      TO      2001109-TBL-HENSYU-EXT
           END-IF
      *
           IF      TBL-TEN-SRYCD (IDX-TBL) =   SPACE
               MOVE    SRY-SRYCD (IDX2)    TO  TBL-TEN-SRYCD (IDX-TBL)
      *        点数マスタ
               MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF      FLG-TENSU           =   ZERO
      *            点数
                   MOVE    TNS-TEN         TO  TBL-TEN-TEN  (IDX-TBL)
               END-IF
           END-IF
      *    数量集計
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               IF      ACCT-DAY (1 IDX-D)  >   ZERO
                   ADD     ACCT-DAY (1 IDX-D)  TO  TBL-TEN-DAY
                                                      (IDX-TBL IDX-D)
      *            数量計算
                   MOVE    ACCT-DAY (1 IDX-D)  TO  WRK-DAY
                   PERFORM 2001109-SURYO-KEI-SEC
      *            数量
                   ADD     WRK-SURYO            TO  TBL-TEN-SURYO
                                                      (IDX-TBL IDX-D)
               END-IF
           END-PERFORM
      *
           .
       2001109-TBL-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    数量計算編集処理
      *****************************************************************
       2001109-SURYO-KEI-SEC           SECTION.
      *
      *    残量廃棄計算
           MOVE    ZERO                TO  FLG-HAIKI
           MOVE    ZERO                TO  WRK-SURYO
           MOVE    ZERO                TO  WRK-HAIKI-SURYO
      *
           IF     (IDX2                <   5  )  AND
                  (SRY-SRYCD (IDX2 + 1)    =   "099309901")
               MOVE    1                   TO  FLG-HAIKI
           END-IF
           IF     (IDX2                =   5  )  AND
                  (IDX-T2              <   IDX-T )
               COMPUTE IDXA            =   IDX-T2   +   1
               IF      TBL-SRY-SRYCD (IDXA  1)
                                           =   "099309901"
                   MOVE    1                   TO  FLG-HAIKI
               END-IF
           END-IF
      *    数量
           IF      FLG-HAIKI           =   ZERO
               COMPUTE WRK-SURYO           =   SRY-SRYSURYO(IDX2)
                                           *   WRK-DAY
           ELSE
               COMPUTE WRK-SURYO-9         =   SRY-SRYSURYO(IDX2)
                                           +   0.99999
               COMPUTE WRK-SURYO           =   WRK-SURYO-9
                                           *   WRK-DAY
      *        廃棄数量
               COMPUTE WRK-HAIKI-SURYO =  (WRK-SURYO-9
                                           -   SRY-SRYSURYO(IDX2))
                                           *   WRK-DAY
           END-IF
      *
           .
       2001109-SURYO-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *     点滴点数再計算処理
      *****************************************************************
       20019-TENTEKI-SAIKEI-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
               MOVE    ZERO            TO  IDX
               MOVE    ZERO            TO  WRK-TENSU-GOK
               MOVE    ZERO            TO  WRK-ZAITEN-TEN
               MOVE    ZERO                TO  FLG-TENTEKI
               PERFORM VARYING     IDX3    FROM    1   BY  1
                       UNTIL      (IDX3        >   100   )
      *            コード点数編集処理
                   PERFORM 2001-TBL-CHEK-SEC
               END-PERFORM
      *        点数計算
               IF      FLG-TENTEKI         =   1
                   IF      WRK-TENSU-GOK       <=  15
                       MOVE    1               TO  WRK-SYOKEIS2
                   ELSE
                       COMPUTE WRK-SYOKEIS2    =  ((WRK-TENSU-GOK
                                               -  15)
                                               /    10 )
                                               +   1
                   END-IF
                   COMPUTE WRK-TENSU           =   WRK-SYOKEIS2
                                               +   0.99999
                   MOVE    WRK-TENSU           TO  WRK-ZAITEN-TEN
               END-IF
      *
               IF      WRK-ZAITEN-TEN  NOT =   ZERO
      *            点数編集
                   ADD     WRK-ZAITEN-TEN      TO  TBL-TEN2-TENSU(IDX-D)
               END-IF
           END-PERFORM
           .
       20019-TENTEKI-SAIKEI-EXT.
           EXIT.
      *****************************************************************
      *    点数編集処理
      *****************************************************************
       2001-TBL-CHEK-SEC            SECTION.
      *
           IF      TBL-TEN-SURYO (IDX3  IDX-D) >   ZERO
               MOVE    1                   TO  FLG-TENTEKI
               COMPUTE WRK-TENSU-KEI       =   TBL-TEN-TEN  (IDX3)
                                           *   TBL-TEN-SURYO
                                                         (IDX3 IDX-D)
               COMPUTE WRK-TENSU-GOK       =   WRK-TENSU-GOK 
                                           +   WRK-TENSU-KEI
           END-IF
      *
           .
       2001-TBL-CHEK-EXT.
           EXIT.
      *!!!!
      *
      *
      *****************************************************************
      *     印刷処理
      *****************************************************************
       2002-PRINT-SYORI-SEC                SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-TUK-ZAITEN
           MOVE    ZERO                TO  WRK-TUK-SYOKUJI
           MOVE    ZERO                TO  WRK-TUK-JIHIKEI
           MOVE    ZERO                TO  WRK-TUK-RSI-MONEY
           MOVE    ZERO                TO  FLG-HCN331-END
      *
      *    印刷開始日から対象とする
           IF      ORCHC33-STRDD       =   ZERO
               MOVE    1               TO  ORCHC33-STRDD
           END-IF
      *    システム日付を終了日とする
           IF      SPA-SYSYMD(1:6)     =   WRK-SRYYM
               MOVE    SPA-SYSYMD(7:2)     TO  IDX-END
           ELSE
               MOVE    31                  TO  IDX-END
           END-IF
      *
      *    入院料の算定
           PERFORM 200221-NYUIN-SYORI-SEC
      *
      *H30.1
      *    バッチ印刷の保険組合せ編集
           IF     (ORCHC33-KBN         =   "2" )
             AND  (CNT-MEI             <   40  )
               PERFORM 200311-HKNSYUBETU-HEN-SEC
               MOVE    WRK-HEN-HKNCOMBIMEI     TO  HCM33N-HKNSYUBETU
           END-IF
      *
           PERFORM VARYING     IDX     FROM    ORCHC33-STRDD  BY  1
                   UNTIL       IDX     >   IDX-END
               MOVE    ZERO                TO  FLG-ARI
               MOVE    ZERO                TO  WRK-GOK-ZAITEN
               INITIALIZE                  TBL-DAY-AREA
      *        点数
               PERFORM VARYING     IDX1    FROM    1   BY  1
                       UNTIL       IDX1    >   11
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   400
                       IF      TBL-DAY (IDX1 IDX2 IDX) >   ZERO
      *                    日毎に集計する
                           PERFORM 20022-TBLDAY-ADD-SEC
                       END-IF
                       IF      TBL-RSI-DAY (IDX1 IDX2 IDX) >   ZERO
      *                    日毎に集計する
                           PERFORM 20023-RSI-TBLDAY-ADD-SEC
                       END-IF
                   END-PERFORM
      *H28.11
      *            点滴薬剤点数
                   IF     (TBL-TEN2-TENSU (IDX)    >   ZERO )
                     AND  (IDX1                    =   3    )
                       ADD     TBL-TEN2-TENSU (IDX) TO  TBL-DAY-ZAITEN
                                                             (IDX1)
                   END-IF
      *
               END-PERFORM
               IF     (FLG-ARI             =   1 )  OR
                      (LNK-ORCHCN33SUB-TEN(IDX)    NOT =   ZERO) OR
                      (LNK-ORCHCN33SUB-SKJ(IDX)    NOT =   ZERO)
                   MOVE    1                   TO  FLG-SYORI
                   MOVE    IDX                 TO  WRK-DAY
      *            日毎のテーブル編集
                   PERFORM 20022-TBL-HENSYU-SEC
      *            日ごとに編集する
                   PERFORM 20023-HCN33-DAYHEN-SEC
               END-IF
           END-PERFORM
      *
           IF     (FLG-SYORI           =   1   )  AND
                  (PARA-API-DATA       =   "1" )
               PERFORM 7003-API-TUKIKEI-SEC
               GO  TO  2002-PRINT-SYORI-EXT
           END-IF
      *
      *    月計印刷
           IF     (FLG-SYORI           =   1   )  AND
                  (FLG-HCN331          =   1   )  AND
                  (FLG-HCN331-END      =   ZERO)
      *        月合計印刷（ＨＣ３３１）
               PERFORM 20042-HCN331-TUKIKEI-SEC
               MOVE    1                   TO  FLG-TUKI-END
               MOVE    ZERO                TO  CNT-MEI
           END-IF
      *    月計印刷（ＨＣ３３）
           IF     (FLG-SYORI           =   1   )  AND
                  (FLG-TUKI-END        =   ZERO)
               PERFORM 20024-TUKIKEI-SEC
           END-IF
      *
           .
       2002-PRINT-SYORI-EXT.
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
      *****************************************************************
      *    労災日毎集計処理
      *****************************************************************
       20023-RSI-TBLDAY-ADD-SEC                SECTION.
      *
           MOVE    1                   TO  FLG-ARI
      *    日毎集計
           COMPUTE WRK-GOK-ZAITEN      =  (TBL-RSI-ZAITEN (IDX1 IDX2)
                                       *   TBL-RSI-DAY (IDX1 IDX2 IDX) )
           ADD     WRK-GOK-ZAITEN      TO  TBL-DAY-RSI-ZAITEN(IDX1)
      *
           .
       20023-RSI-TBLDAY-ADD-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集テーブル編集処理（日毎）
      *****************************************************************
       20022-TBL-HENSYU-SEC                SECTION.
      *
           INITIALIZE                  TBL-HEN-AREA
           MOVE    1                   TO  IDX-T1
           MOVE    1                   TO  IDX-MAX
           MOVE    ZERO                TO  WRK-GOK-ZAITEN
           MOVE    ZERO                TO  WRK-GOK-JIHIKEI
           MOVE    ZERO                TO  WRK-GOK-SYOKUJI
           MOVE    ZERO                TO  WRK-GOK-RSI-MONEY
      *
      *    入院料点数
           ADD     LNK-ORCHCN33SUB-TEN(IDX)    TO  TBL-DAY-ZAITEN(08)
      *    食事療養
           MOVE    LNK-ORCHCN33SUB-SKJ(IDX)    TO  TBL-DAY-ZAITEN(10)
      *    その他点数（療養担当手当）
           ADD     LNK-ORCHCN33SUB-ETCTEN(IDX) TO  TBL-DAY-ZAITEN(07)
      *
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   11
               IF      TBL-DAY-ZAITEN(IDX1)    >   ZERO
                   MOVE    TBL-DAY-ZAITEN(IDX1)    TO  WRK-Z89
               ELSE
                   MOVE    TBL-DAY-ZAITEN(IDX1)    TO  WRK-Z79S
               END-IF
      *
               EVALUATE    IDX1
                   WHEN    10
      *                食事
                       COMPUTE WRK-GOK-SYOKUJI =   WRK-GOK-SYOKUJI
                                               +   TBL-DAY-ZAITEN(IDX1)
                   WHEN    11
      *                自費
                       COMPUTE WRK-GOK-JIHIKEI =   WRK-GOK-JIHIKEI
                                               +   TBL-DAY-ZAITEN(IDX1)
                   WHEN    OTHER
                       COMPUTE WRK-GOK-ZAITEN  =   WRK-GOK-ZAITEN
                                               +   TBL-DAY-ZAITEN(IDX1)
               END-EVALUATE
      *        点数のみ
               IF      TBL-DAY-ZAITEN(IDX1)    NOT =   ZERO
                   MOVE    WRK-Z89-X       TO  TBL-NAIYO(IDX-T1 IDX1)
               END-IF
           END-PERFORM
      *
      *    労災円集計
           MOVE    ZERO                TO  FLG-ARI-RSI
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   7
               IF      TBL-DAY-RSI-ZAITEN(IDX1)    NOT =   ZERO
                   IF      FLG-ARI-RSI         =   ZERO
                       ADD     1                   TO  IDX-T1
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
      *    労災円合計
           IF      WRK-GOK-RSI-MONEY   >   ZERO
               MOVE    WRK-GOK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  TBL-NAIYO(IDX-T1 09)
               MOVE    "*"                 TO  TBL-NAIYO(IDX-T1 09)
                                                                 (1:1)
           END-IF
      *
      *    最大行編集
           IF      IDX-T1              >   IDX-MAX
               MOVE    IDX-T1          TO  IDX-MAX
           END-IF
      *
           .
       20022-TBL-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *     入院料の算定処理
      *****************************************************************
       200221-NYUIN-SYORI-SEC         SECTION.
      *
           INITIALIZE                  ORCHCN33SUB-AREA
      *    医療機関ＩＤ
           MOVE    SPA-HOSPNUM         TO  LNK-ORCHCN33SUB-HOSPNUM
      *    患者ＩＤ
           MOVE    ORCHC33-PTID        TO  LNK-ORCHCN33SUB-PTID
      *    算定年月
           MOVE    WRK-SRYYM           TO  LNK-ORCHCN33SUB-SANTEIYM
      *    診療科
           MOVE    ORCHC33-SRYKA       TO  LNK-ORCHCN33SUB-SRYKA
           CALL   "ORCHCN33SUB"        USING
                                       ORCHCN33SUB-AREA
                                       SPA-AREA
           IF      LNK-ORCHCN33SUB-RC  NOT =   ZERO
               DISPLAY "ORCHCN33 ORCHCN33SUB ERR:"  LNK-ORCHCN33SUB-RC
           END-IF
      *
      *    入院の保険組合せを設定する
           INITIALIZE                      NYUINACCT-REC
           MOVE    SPA-HOSPNUM         TO  NACCT-HOSPNUM
           MOVE    ORCHC33-NYUGAIKBN   TO  NACCT-NYUGAIKBN
           MOVE    ORCHC33-PTID        TO  NACCT-PTID
           MOVE    WRK-SRYYM           TO  NACCT-SRYYM
           MOVE    "5"                 TO  NACCT-ZAISKBKBN
      *
           MOVE    NYUINACCT-REC       TO  MCPDATA-REC
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
               MOVE    "key17"             TO  MCP-PATHNAME
               PERFORM 900-NYUINACCT-READ-SEC
           ELSE
               INITIALIZE                  NYUINACCT-REC
               MOVE    1               TO  FLG-NYUINACCT
           END-IF
           MOVE    "tbl_nyuinacct"     TO  MCP-TABLE
           MOVE    "key17"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDXH    FROM    ORCHC33-STRDD  BY  1
                   UNTIL       IDXH    >   IDX-END
               IF      NACCT-DAY (IDXH)    NOT =   ZERO
                   MOVE    ZERO                TO  WRK-HKNCOMBI-9
                   MOVE    NACCT-DAY(IDXH)     TO  WRK-HKNCOMBI-9
                   MOVE    WRK-HKNCOMBI-G      TO  ACCT-HKNCOMBI
                   PERFORM 20013-HKNCOMBI-HEN-SEC
      *H24.9
      *            対象外
                   IF      FLG-OK              =   ZERO
                       MOVE    ZERO        TO  LNK-ORCHCN33SUB-TEN
                                                                 (IDXH)
                       MOVE    ZERO        TO  LNK-ORCHCN33SUB-ETCTEN
                                                                 (IDXH)
                       MOVE    ZERO        TO  LNK-ORCHCN33SUB-SKJ
                                                                 (IDXH)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       200221-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日ごとに編集処理
      *****************************************************************
       20023-HCN33-DAYHEN-SEC           SECTION.
      *
      *    診療日
           MOVE    WRK-SRYMM           TO  WRK-H-MM
           MOVE    "."                 TO  WRK-H-P
           MOVE    WRK-DAY             TO  WRK-H-DD
      *
      *    点数合計
           IF      WRK-GOK-ZAITEN      >   ZERO
               MOVE    WRK-GOK-ZAITEN      TO  WRK-Z89
               MOVE    WRK-Z89             TO  TBL-NAIYO(01 09)
           ELSE
               MOVE    WRK-GOK-ZAITEN      TO  WRK-Z79S
               MOVE    WRK-Z79S            TO  TBL-NAIYO(01 09)
           END-IF
      *    月点数合計
           ADD     WRK-GOK-ZAITEN      TO  WRK-TUK-ZAITEN
           ADD     WRK-GOK-JIHIKEI     TO  WRK-TUK-JIHIKEI
           ADD     WRK-GOK-SYOKUJI     TO  WRK-TUK-SYOKUJI
           ADD     WRK-GOK-RSI-MONEY   TO  WRK-TUK-RSI-MONEY
      *
           IF    ( PARA-API-DATA       =   "1" )
               PERFORM 7002-API-BODY-SEC
               GO  TO  20023-HCN33-DAYHEN-EXT
           END-IF
      *
      *    追加印刷
           IF     (FLG-HCN331           =   1   )  AND
                  (FLG-HCN331-END       =   ZERO)
               PERFORM 2004-HCN331-HENSYU-SEC
           ELSE
               MOVE    1               TO  IDX-STR
           END-IF
      *
           PERFORM VARYING     IDX1    FROM    IDX-STR  BY  1
                   UNTIL       IDX1    >   IDX-MAX
               ADD     1                   TO  CNT-MEI
               IF      CNT-MEI             >   40
      *            印刷処理
                   PERFORM 2003-HCN33-PRINT-SEC
                   MOVE    1                   TO  CNT-MEI
               END-IF
               IF      CNT-MEI             =   1
                   PERFORM 20031-HEAD-HEN-SEC
               END-IF
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM33N-NUM (CNT-MEI)
      *        日付
               IF     (IDX1                =   1 )  OR
                      (CNT-MEI             =   1 )
                   MOVE    WRK-HEN-DAY     TO  HCM33N-SRYMD (CNT-MEI)
               END-IF
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   11
                   MOVE    TBL-NAIYO (IDX1 IDX2)   TO  HCM33N-NAIYO
                                                          (CNT-MEI IDX2)
               END-PERFORM
           END-PERFORM
      *
           .
       20023-HCN33-DAYHEN-EXT.
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
               PERFORM 2003-HCN33-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
           END-IF
           IF      CNT-MEI             =   1
               PERFORM 20031-HEAD-HEN-SEC
           END-IF
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM33N-NUM (CNT-MEI)
      *
           MOVE    WRK-TUK-ZAITEN      TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33N-NAIYO (CNT-MEI 9)
           MOVE    WRK-TUK-SYOKUJI     TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33N-NAIYO (CNT-MEI 10)
           MOVE    WRK-TUK-JIHIKEI     TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33N-NAIYO (CNT-MEI 11)
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
               PERFORM 2003-HCN33-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
           END-IF
           IF      CNT-MEI             =   1
               PERFORM 20031-HEAD-HEN-SEC
           END-IF
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM33N-NUM (CNT-MEI)
      *
           MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM33N-NAIYO (CNT-MEI 9)
           MOVE    "*"                 TO  HCM33N-NAIYO (CNT-MEI 9)
                                                               (1:1)
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
           MOVE    SPACE               TO  HCM33N
           INITIALIZE                      HCM33N
      *
           ADD     1                   TO  ORCHC33-PAGE
           MOVE    ORCHC33-PAGE        TO  WRK-ZZ9
           MOVE    WRK-ZZ9             TO  HCM33N-PAGE
      *
           MOVE    ORCHC33-PTNUM       TO  HCM33N-PTNUM
      *    患者マスターより
      *    患者氏名
           MOVE    ORCHC33-NAME        TO  HCM33N-NAME
      *    性別
           EVALUATE    ORCHC33-SEX(1:1)
               WHEN    "1"
                   MOVE    "男"                TO  HCM33N-SEX
               WHEN    "2"
                   MOVE    "女"                TO  HCM33N-SEX
               WHEN    OTHER
                   MOVE    ORCHC33-SEX         TO  HCM33N-SEX
           END-EVALUATE
      *
           MOVE    WRK-BIRTHDAY        TO  HCM33N-TANJYOBI
      **** 年齢
      *    IF      WRK-NENREI-YY       >   ZERO
      *        MOVE    WRK-NENREI-YY       TO  WRK-ZZ9
      *        MOVE    WRK-ZZ9             TO  HCM33N-NENREI
      *        MOVE    "才"                TO  HCM33N-NENREIMEI
      *    ELSE
      *        MOVE    WRK-NENREI-MM       TO  WRK-ZZ9
      *        MOVE    WRK-ZZ9             TO  HCM33N-NENREI
      *        MOVE    "ヶ月"              TO  HCM33N-NENREIMEI
      *****END-IF
      *    診療科
           MOVE    WRK-SRYKA-MEI       TO  HCM33N-SRYKA
      *    期間
           MOVE    WRK-SRYYM-N         TO  HCM33N-SRYYM
      *
           PERFORM 200311-HKNSYUBETU-HEN-SEC
      *
           MOVE    WRK-HEN-HKNCOMBIMEI     TO  HCM33N-HKNSYUBETU
           .
       20031-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険名称編集処理
      *****************************************************************
       200311-HKNSYUBETU-HEN-SEC                SECTION.
      *
      *
      *    保険名称
           MOVE    SPACE                   TO  WRK-HEN-HKNCOMBIMEI
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
      *
      *****************************************************************
      *    カルテ印刷処理
      *****************************************************************
       2003-HCN33-PRINT-SEC                SECTION.
      *
           EVALUATE    ORCHC33-KBN
      *        オンライン印刷
               WHEN    "1"
                   PERFORM 20031-HCN33-ONL-SEC
      *        バッチ印刷
               WHEN    "2"
                   PERFORM 20032-HCN33-BAT-SEC
           END-EVALUATE
      *
      *v4.8
           ADD     1                   TO  CNT-PAGE
           .
       2003-HCN33-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ印刷処理（オンライン）
      *****************************************************************
       20031-HCN33-ONL-SEC                SECTION.
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
           MOVE    HCM33N               TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
            INITIALIZE                 ORCSMKPRTAREA
            MOVE   "HC01.sh"           TO  MKPRT-ID
            MOVE   "HCM33N.red"        TO  MKPRT-DIA
            MOVE   SPACE               TO  MKPRT-DEF
      ******MOVE   HC01PARA            TO  MKPRT-DAT
            MOVE   ASGNPARA            TO  MKPRT-DAT
            MOVE   SYS-1034-PRTNM(04)  TO  MKPRT-PRTNM
            MOVE   "カルテ３号紙"      TO  MKPRT-INFO
            CALL   "ORCSMKPRT"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       20031-HCN33-ONL-EXT.
           EXIT.
      *****************************************************************
      *    カルテ印刷処理（バッチ）
      *****************************************************************
       20032-HCN33-BAT-SEC         SECTION.
      *
           MOVE    ORCHC33-PRTAREA     TO  ORCSPRTAREA
      *
           MOVE    HCM33N              TO  SPRT-PRTDATA
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
       20032-HCN33-BAT-EXT.
           EXIT.
      *****************************************************************
      *    追加印刷処理
      *****************************************************************
       2004-HCN331-HENSYU-SEC           SECTION.
      *
           PERFORM VARYING     IDX1    FROM    1    BY  1
                   UNTIL      (IDX1        >   IDX-MAX )  OR
                              (CNT-MEI     >=  40      )
               ADD     1                   TO  CNT-MEI
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM331N-NUM (CNT-MEI)
      *        日付
               IF      IDX1                =   1
                   MOVE    WRK-HEN-DAY     TO  HCM331N-SRYMD (CNT-MEI)
               END-IF
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   11
                   MOVE    TBL-NAIYO (IDX1 IDX2)   TO  HCM331N-NAIYO
                                                          (CNT-MEI IDX2)
               END-PERFORM
               MOVE    IDX1                TO  IDX-STR
           END-PERFORM
      *
           IF      CNT-MEI             >=  40
      *        印刷処理
               PERFORM 2004-HCN331-PRINT-SEC
               MOVE    ZERO                TO  CNT-MEI
               MOVE    1                   TO  FLG-HCN331-END
           END-IF
           ADD     1                   TO  IDX-STR
      *
           .
       2004-HCN331-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理
      *****************************************************************
       20042-HCN331-TUKIKEI-SEC                SECTION.
      *
           ADD     1                   TO  CNT-MEI
      *    連番
           MOVE    CNT-MEI             TO  WRK-Z9
           MOVE    WRK-Z9              TO  HCM331N-NUM (CNT-MEI)
      *
           MOVE    WRK-TUK-ZAITEN      TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM331N-NAIYO (CNT-MEI 9)
           MOVE    WRK-TUK-SYOKUJI     TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM331N-NAIYO (CNT-MEI 10)
           MOVE    WRK-TUK-JIHIKEI    TO  WRK-Z89
           MOVE    WRK-Z89             TO  HCM331N-NAIYO (CNT-MEI 11)
      *
           IF      WRK-TUK-RSI-MONEY   =   ZERO
      *        印刷処理
               PERFORM 2004-HCN331-PRINT-SEC
           ELSE
      *        労災・自賠責金額計処理
               PERFORM 200421-HCN331-ROUSAI-SEC
           END-IF
           .
       20042-HCN331-TUKIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理（労災・自賠責）
      *****************************************************************
       200421-HCN331-ROUSAI-SEC                SECTION.
      *
      *    労災・自賠責金額計処理
           ADD     1                   TO  CNT-MEI
           IF      CNT-MEI             >   40
               PERFORM 2004-HCN331-PRINT-SEC
               MOVE    1                   TO  CNT-MEI
      *        ＨＣ３３の印刷
               PERFORM 20031-HEAD-HEN-SEC
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM33N-NUM (CNT-MEI)
      *
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  HCM33N-NAIYO (CNT-MEI 9)
               MOVE    "*"                 TO  HCM33N-NAIYO (CNT-MEI 9)
                                                                (1:1)
      *        印刷処理
               PERFORM 2003-HCN33-PRINT-SEC
           ELSE
      *        連番
               MOVE    CNT-MEI             TO  WRK-Z9
               MOVE    WRK-Z9              TO  HCM331N-NUM (CNT-MEI)
      *
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  HCM331N-NAIYO(CNT-MEI 9)
               MOVE    "*"                 TO  HCM331N-NAIYO(CNT-MEI 9)
                                                                (1:1)
      *        印刷処理
               PERFORM 2004-HCN331-PRINT-SEC
           END-IF
      *
           .
       200421-HCN331-ROUSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    追加カルテ印刷処理
      *****************************************************************
       2004-HCN331-PRINT-SEC                SECTION.
      *
      *    オンラインのみ
      *    ファイルＯＰＥＮ
           MOVE    SPA-HOSPNUM         TO  HC01PARA-HOSPNUM
           MOVE    "HC01"              TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID          TO  HC01PARA-TERMID
           MOVE    SYS-TIME            TO  HC01PARA-SYOHMS
           ADD     1                   TO  WRK-PAGE
           MOVE    WRK-PAGE            TO  HC01PARA-CNT
      *v4.8
           ADD     1                   TO  CNT-PAGE
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
      *    印刷用データ出力
           MOVE    HCM331N              TO  PRT-REC
           WRITE   PRT-REC
      *
           CLOSE                       PRT-FILE
      *
           INITIALIZE                  ORCSMKPRTAREA
           MOVE   "HC01.sh"            TO  MKPRT-ID
           MOVE   "HCM331N.red"        TO  MKPRT-DIA
           MOVE   SPACE                TO  MKPRT-DEF
      **** MOVE   HC01PARA             TO  MKPRT-DAT
           MOVE   ASGNPARA             TO  MKPRT-DAT
           MOVE   SYS-1034-PRTNM(05)   TO  MKPRT-PRTNM
           IF     SYS-1034-PRTNM(05)   =   SPACE
                MOVE   SYS-1034-PRTNM(04)  TO  MKPRT-PRTNM
           END-IF
           MOVE   "カルテ３号紙（追記）"   TO  MKPRT-INFO
           CALL   "ORCSMKPRT"          USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       2004-HCN331-PRINT-EXT.
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
           MOVE    "karte_no3_n"   TO  FMKARTE3N-FORM-ID
           MOVE    PARA-API-CUSTOM-ID 
                                   TO  FMKARTE3N-CUSTOM-ID
           MOVE   SYS-1034-PRTNM(04)
                                   TO  FMKARTE3N-PRINTER
           IF    ( SYS-1034-PRTNM(05)  NOT =   SPACE )
               MOVE   SYS-1034-PRTNM(05)
                                   TO  FMKARTE3N-PRINTER-ZOKUSHI
           ELSE
               MOVE   SYS-1034-PRTNM(04)
                                   TO  FMKARTE3N-PRINTER-ZOKUSHI
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
           MOVE    API-ENDPAGE-G   TO  FMKARTE3N-ENDPAGE
      *
           MOVE    ORCHC33-ENDLINE TO  API-ENDLINE
           MOVE    API-ENDLINE-G   TO  FMKARTE3N-ENDLINE
      *
           MOVE    ORCHC33-SRYYM   TO  WRK-SYMD
           MOVE    "01"            TO  WRK-SYMD(7:)
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE(1:7)
                                   TO  FMKARTE3N-SRYYM
      *
           IF  ( ORCHC33-PRINT-FLG =   1 )
               MOVE    "2"         TO  FMKARTE3N-HAKKOKBN
           ELSE
               MOVE    "1"         TO  FMKARTE3N-HAKKOKBN
           END-IF
      *
           IF    ( ORCHC33-SRYKA   =   SPACE OR ZERO )
               CONTINUE
           ELSE
               MOVE    ORCHC33-SRYKA
                                   TO  FMKARTE3N-SRYKA
           END-IF
      *
           MOVE    WRK-SRYKA-MEI   TO  FMKARTE3N-SRYKANAME
      *
           PERFORM 200311-HKNSYUBETU-HEN-SEC
           MOVE    WRK-HEN-HKNCOMBIMEI
                                   TO  FMKARTE3N-HKNSYUBETU
      *
           MOVE    ORCHC33-PTNUM   TO  FMKARTE3N-PTNUM
           MOVE    PTINF-NAME      TO  FMKARTE3N-NAME
           MOVE    PTINF-KANANAME  TO  FMKARTE3N-KANANAME
           MOVE    PTINF-BIRTHDAY  TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE    TO  FMKARTE3N-BIRTHDAY
           MOVE    PTINF-SEX       TO  FMKARTE3N-SEX
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
                       UNTIL       IDX2    >   11
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
               MOVE    WRK-HEN-DATE    TO  FMKARTE3N-SRYYMD(CNT-API)
      *
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   11
                   MOVE    API-NAIYO (IDX2)
                                       TO  FMKARTE3N-NAIYO(CNT-API IDX2)
               END-PERFORM
               MOVE    API-BIKOU       TO  FMKARTE3N-BIKOU (CNT-API)
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
           MOVE    WRK-Z89             TO  API-NAIYO (9)
           MOVE    WRK-TUK-SYOKUJI     TO  WRK-Z89
           MOVE    WRK-Z89             TO  API-NAIYO (10)
           MOVE    WRK-TUK-JIHIKEI     TO  WRK-Z89
           MOVE    WRK-Z89             TO  API-NAIYO (11)
           PERFORM 70021-API-BODY-SEC
      *
           IF      WRK-TUK-RSI-MONEY   NOT =   ZERO
               INITIALIZE                      API-AREA
      *
               MOVE    "※労・自"          TO  API-BIKOU
               MOVE    WRK-TUK-RSI-MONEY   TO  WRK-Z89
               MOVE    WRK-Z89             TO  API-NAIYO (9)
               MOVE    "*"                 TO  API-NAIYO (9) (1:1)
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
           MOVE    FMKARTE3N-FORM-ID    TO  RPMAIN-ID
           MOVE    FMKARTE3N-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "カルテ３号紙（入院）" 
                                        TO  RPMAIN-TITLE
           MOVE    "fm_karte_no3_h"     TO  MCP-TABLE
           MOVE    FMKARTE3N-DATA       TO  RPMAIN-DATA
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
      *    診療行為マスター読込
      *****************************************************************
       920-SRYACT-READ-SEC         SECTION.
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
       920-SRYACT-READ-EXT.
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
       930-SRYACT-RAED-SEC         SECTION.
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
      *
      *****************************************************************
      *    入院会計マスター次読込
      *****************************************************************
       900-NYUINACCT-READ-SEC       SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  NYUINACCT-REC
               MOVE    ZERO                TO  FLG-NYUINACCT
           ELSE
               INITIALIZE                      NYUINACCT-REC
               MOVE    1                   TO  FLG-NYUINACCT
           END-IF.
      *
       900-NYUINACCT-READ-EXT.
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
      *
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
