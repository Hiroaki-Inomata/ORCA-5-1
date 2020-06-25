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
       PROGRAM-ID.             ORCSP02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者登録画面編集、ＳＰＡ初期セットサブ
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/12/04    MCC-多々納    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/03/30  ドクター表示の修正
      * 01.00.02     MCC-多々納   02/04/05  労災保険の追加
      * 02.00.00     MCC-多々納   02/06/25  レイアウト変更
      * 02.00.01     NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      * 02.00.02     NACL-多々納  02/09/09  平成１４年１０月改正対応
      * 02.00.03     NACL-多々納  02/11/26  診療科・ドクター件数修正
      * 02.00.04     NACL-多々納  03/01/08  公費の最大件数１００件へ
      * 02.00.05     NACL-多々納  03/03/19  Ｈ１５年４月改正対応追加
      * 02.00.06     NACL-多々納  03/03/24  公費負担額追加
      * 02.00.07     NACL-多々納  05/02/10  特定の受給者番号表示追加
      * 02.00.08     NACL-多々納  05/11/14  MONFUNC 対応
      * 02.00.09     NACL-多々納  06/02/24  主科対応対応
      * 02.00.10     NACL-多々納  06/05/18  請求書発行区分追加
      * 03.00.01     NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      * 03.04.00     NACL-多々納  07/01/29  排他制御修正
      * 03.05.00     NACL-多々納  07/05/XX  グループ診療対応
      * 03.05.00     NACL-多々納  07/06/08  前回来院日追加
      * 03.05.01     NACL-多々納  07/08/28  公費期限切れ区分追加
      * 04.03.00     NACL-多々納  08/07/22  特記事項追加
      * 04.04.00     NACL-多々納  09/01/27  本院分院対応
      * 04.05.00     NACL-多々納  09/07/XX  患者公費負担額機能追加
      * 04.05.00     NACL-多々納  10/03/XX  平成２２年４月改正対応
      * 04.07.00     NACL-多々納  11/08/XX  保険確認年月日履歴対応
      * 04.07.00     NACL-多々納  12/11/22  地域連携ＩＤ対応
      * 04.07.00     NACL-多々納  12/12/25  患者レセプト分割対応
      * 04.07.00     NACL-多々納  14/02/XX  患者介護情報追加対応
      * 04.07.00     NACL-多々納  14/03/XX  地域包括診療対象疾病対応
      * 04.08.00     NACL-多々納  15/06/16  診療科・保険組合せ件数対応
      * 04.08.00     NACL-多々納  15/08/05  領収・明細　請求書不要対応
      * 04.08.00     NACL-多々納  15/11/XX  レセプト分割５０件他対応
      * 04.08.00     NACL-多々納  16/03/XX  平成２８年４月改正対応
      * 04.08.00     NACL-多々納  16/11/21  ＱＲコードver.2.1対応
      * 05.00.00     NACL-多々納  16/12/15  患者個人番号対応
      * 05.00.00     NACL-多々納  16/12/XX  管理料等自動発生対応
      * 04.08.00     NACL-多々納  17/05/XX  境界層該当対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-PTNUMO          PIC 9(01).
           03  FLG-PTNUM           PIC 9(01).
           03  FLG-YYK             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTHKNINF        PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-KYUSEIRRK       PIC 9(01).
           03  FLG-PTGETNUM        PIC 9(01).
           03  FLG-TSYRRK          PIC 9(01).
           03  FLG-TNKRRK          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
           03  FLG-PTPUBLIC        PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
           03  FLG-PTCARE-HKNINF   PIC 9(01).
           03  FLG-PTCARE-NINTEI   PIC 9(01).
           03  FLG-PTMYNUMBER      PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PTKOHFTN        PIC 9(01).
           03  FLG-PTTOKKI         PIC 9(01).
           03  FLG-PTHDISTDAY      PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-TENSU-OK        PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PTID            PIC 9(10).
           03  WRK-PTNUM           PIC X(20).
      *
           03  WRK-MEISAI1             PIC 9(06).
           03  WRK-MEISAI2             PIC 9(06).
           03  WRK-MEISAI3             PIC 9(06).
           03  WRK-YMD                 PIC X(08).
           03  WRK-RENNUMZ             PIC ZZZZ.
           03  WRK-RENNUMZ3            PIC ZZZ.
           03  WRK-RENNUMZ2            PIC ZZ.
           03  WRK-HST                 PIC 9(04).
           03  WRK-HED                 PIC 9(04).
           03  WRK-KST                 PIC 9(04).
           03  WRK-KED                 PIC 9(04).
           03  WRK-KBNCD               PIC X(02).
      *
           03  WRK-Z7                  PIC Z,ZZZ,ZZ9.
      *H30.11
           03  WRK-Z72                 PIC Z,ZZZ,ZZZ.
      *
           03  WRK-RATE-AREA.
               05  WRK-RATE-KBN        PIC X(02).
               05  WRK-RATE-SP         PIC X(01).
               05  WRK-RATE-EDIT.
                   07  WRK-RATE-EDITZ  PIC ZZZZZZZ.
                   07  WRK-RATE-TANI   PIC X(04).
               05  WRK-RATE-EDIT-R  REDEFINES    WRK-RATE-EDIT.
                   07  WRK-RATE-EDITR  PIC X(11).
      *
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
      *
           03  WRK-YMD-01.
               05  WRK-YY-01       PIC 9(04).
               05  WRK-MM-01       PIC 9(02).
               05  WRK-DD-01       PIC 9(02).
      *
           03  WRK-HKNID-MAX           PIC 9(10).
           03  WRK-KOHID-MAX           PIC 9(10).
           03  WRK-HKNCOMBI-MAX        PIC 9(04).
      *
           03  WRK-TEIMSG-G.
               05  WRK-TMSG1           PIC X(10).
               05  WRK-TMSG2           PIC X(02).
               05  WRK-TMSG3           PIC X(10).
           03  WRK-TEIMSG              PIC X(30).
      *
           03  WRK-IDOMSG              PIC X(10).
      *
           03  WRK-HFTNLST2-ROW        PIC S9(09).
           03  WRK-HKNKUMI-ROW         PIC S9(09).
      *
           03  WRK-P026-ROW1           PIC S9(09).
           03  WRK-P026-ROW2           PIC S9(09).
      *
           03  WRK-SRYCD-NAME          PIC X(80).
           03  WRK-SRYCD               PIC X(09).
      *
      *   年齢表示
       01  WRK-NENREI-AREA.
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-NENREI.
               05  WRK-NENREI-YY       PIC 9(04).
               05  WRK-NENREI-MM       PIC 9(02).
               05  WRK-NENREI-DD       PIC 9(02).
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-SYMD            PIC X(08).
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
      *    全角空白
       01  WRK-SPACE           PIC X(02)   VALUE  "　".
      *
      *PandaTable色
      *クロ
       01  CL-BLACK                PIC X(07)   VALUE   "#000000".
      *白
       01  CL-WHITE                PIC X(07)   VALUE   "#FFFFFF".
      *薄緑
       01  CL-GREEN                PIC X(07)   VALUE   "#E5FFD1".
      *ピンク
       01  CL-PINK                 PIC X(07)   VALUE   "#FCEEEE".
      *   
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1009.INC".
      *    状態情報
           COPY    "CPSK1018.INC".
      *    減免事由情報
           COPY    "CPSK1026.INC".
      *    割引率情報
           COPY    "CPSK1027.INC".
      *    診療内容情報
           COPY    "CPSK1012.INC".
      *    診療科目情報
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    職員設定情報
           COPY    "CPSK1017.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    患者番号変換マスタ
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    保険マスタ
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    旧姓履歴
       01  KYUSEIRRK-REC.
           COPY    "CPKYUSEIRRK.INC".
      *
      *    低所得
       01  TSYRRK-REC.
           COPY    "CPTSYRRK.INC".
      *
      *    年金
       01  TNKRRK-REC.
           COPY    "CPTNKRRK.INC".
      *
      *    紹介履歴
       01  SYKRRK-REC.
           COPY    "CPSYKRRK.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納明細
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    予約マスター保存
       01  WRK-YYK-REC.
           COPY    "CPYYK.INC"       REPLACING
                                     //YYK-// BY //WRK-YYK-//.
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    患者公費負担
       01  PTKOHFTN-REC.
           COPY    "CPPTKOHFTN.INC".
      *
      *    患者特記事項マスタ
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    患者レセプト分割テーブル
       01  PT-HDIST-DAY-REC.
           COPY    "CPPT-HDIST-DAY.INC".
      *
      *    同一患者識別テーブル
       01  PTSAME-REC.
           COPY    "CPPTSAME.INC".
      *
      *    患者地域連携テーブル
       01  PTPUBLIC-REC.
           COPY    "CPPTNUM-PUBLIC.INC".
      *
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    患者介護保険情報テーブル
       01  PTCARE-HKNINF-REC.
           COPY    "CPPTCARE-HKNINF.INC".
      *
      *    患者介護認定者情報テーブル
       01  PTCARE-NINTEI-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *
      *    患者個人番号テーブル
       01  PTMYNUMBER-REC.
           COPY    "CPPTMYNUMBER.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ−
      *01  TNSWK-TENSU-REC.
           COPY    "CPTENSUWK.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    ユーザプログラム起動チェックパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    パラメタ
           COPY    "CPORCSP02.INC".
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *    患者登録全画面スパ領域
           COPY    "P02SCR-SPA".
      *
       01  P02-SCR.
           COPY    "P02.INC".
      *
       PROCEDURE                   DIVISION    USING
           ORCSP02AREA
           SPA-AREA
           SPA-P02KYOUTU
           SPA-P02SCRAREA
           P02-SCR.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  FLG-AREA
      *
      *
           EVALUATE    ORCSP02-SYORI
               WHEN    "INIT"
      *            初期パラメタ、ＳＰＡ初期編集
                   PERFORM 100-P01PARA-OUT-SEC
               WHEN    "GMN"
      *            画面編集
                   PERFORM 200-P02-SET-SEC
               WHEN    "API"
      *            ＡＰＩ初期パラメタ、ＳＰＡ初期編集
                   PERFORM 300-APIP02-SET-SEC
      *H28.11
               WHEN    "GMKOH"
      *            公費画面編集
                   PERFORM 20011-P02W1-KOHSET-SEC
           END-EVALUATE
      *
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    患者番号発行処理
      *****************************************************************
       100-P01PARA-OUT-SEC               SECTION.
      *
      *    パラメタファイルのクリア
           PERFORM 1001-PARA-DEL-SEC
      *
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    SPA-GMN-PTNUM       TO  WRK-PTNUM
           INITIALIZE                      SPA-P02GMNAREA
           INITIALIZE                      SPA-P02SCRAREA
           MOVE    WRK-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    WRK-PTID            TO  SPA-GMN-PTID
      *
      *    システム管理より情報取り込み
           PERFORM 1003-SYSKAN-SET-SEC
      *
      *    保険・公費一覧情報取り込み
           PERFORM 1004-HKNKOH-SET-SEC
      *
      *    その他のコンボ
           PERFORM 1005-COMB-SET-SEC
      *
      *    患者情報取り込み
           IF     (SPA-P02-SYORI       =   1   )   OR
                  (SPA-GMN-PTNUM       =   SPACE )
      *        新規パラメタ作成
               PERFORM 1004-SINKI-PARA-SEC
           ELSE
      *        患者情報取り込み
               PERFORM 1002-KYOUTU-SET-SEC
           END-IF
      *
           .
       100-P01PARA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ削除処理
      *****************************************************************
       1001-PARA-DEL-SEC          SECTION.
      *
      *    パラメタ削除
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       1001-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理よりＳＰＡ設定処理
      *****************************************************************
       1003-SYSKAN-SET-SEC          SECTION.
      *
      *    医療機関ＩＤ編集
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        都道府県区分
               MOVE    SYS-1001-PREFNUM    TO  SPA-NAI-PREFNUM
      *        病院種別
               MOVE    SYS-1001-HOSPSBT    TO  SPA-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-ROUPAYKBN
      *        老人公費の支払区分用
               MOVE    SYS-1001-HOSPSBT    TO  SPA-NAI-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-NAI-ROUPAYKBN
           END-IF
      *
      *    老人公費の支払区分の病院種別取得（平成１４年９月末）
           MOVE    SPACE               TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    "20020930"          TO  SYS-1001-STYUKYMD
           MOVE    "20020930"          TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
      *        病院種別
               MOVE    SYS-1001-HOSPSBT    TO  SPA-NAI-HOSPSBT
               MOVE    SYS-1001-ROUPAYKBN  TO  SPA-NAI-ROUPAYKBN
           END-IF
      *
      *    減免事由情報
           INITIALIZE                  SPA-GMN-DISCOUNTKBN-TBL
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1026"              TO  SYS-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( FLG-SYSKANRI  =   1  )
      *H30.9   100件に
      ****           OR      ( IDX           >   99 )
                     OR      ( IDX           >  100 )
               MOVE    MCPDATA-REC     TO  SYS-1026-REC
               MOVE    SYS-1026-KBNCD (1:2)
                                   TO  SPA-GMN-DISCOUNTKBN-KBNCD  (IDX)
               MOVE    SYS-1026-DISCOUNT-BODY
                                   TO  SPA-GMN-DISCOUNTKBN-BODY   (IDX)
               MOVE    SYS-1026-DISCOUNT-KBN
                                   TO  SPA-GMN-DISCOUNTKBN-RATEKBN(IDX)
      *
               MOVE    IDX         TO  SPA-DISCOUNTKBN-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *----(01.00.07) LINE ADD START -------------------------------
           IF  SPA-GMN-DISCOUNTKBN-KBNCD (1)  =   SPACE
               MOVE    ZERO        TO  SPA-GMN-DISCOUNTKBN-KBNCD  (1)
               MOVE    "該当なし"  TO  SPA-GMN-DISCOUNTKBN-BODY   (1)
               MOVE    ZERO        TO  SPA-GMN-DISCOUNTKBN-RATEKBN(1)
               MOVE    1           TO  SPA-DISCOUNTKBN-MAX
           END-IF
      *
           MOVE    SPA-GMN-DISCOUNTKBN-KBNCD(1)
                                       TO  SPA-GMN-DISCOUNTKBN
           MOVE    SPA-GMN-DISCOUNTKBN-BODY(1)
                                       TO  SPA-GMN-DISCOUNTKBN-MEI
      *
      *    割引率情報
           INITIALIZE                  SPA-GMN-DISCOUNT-TBL
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1027"              TO  SYS-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( FLG-SYSKANRI  =   1  )
      *H30.9   100件に
      ****           OR      ( IDX           >   99 )
                     OR      ( IDX           >  100 )
               MOVE    MCPDATA-REC     TO  SYS-1027-REC
               MOVE    SPACE           TO  WRK-RATE-AREA
               MOVE    SYS-1027-KBNCD (1:2)
                                     TO  SPA-GMN-DISCOUNT-KBNCD  (IDX)
               MOVE    SYS-1027-TEIRITU
                                     TO  SPA-GMN-DISCOUNT-TEIRITU(IDX)
               MOVE    SYS-1027-DISCOUNT-RATE
                                     TO  SPA-GMN-DISCOUNT-RITU   (IDX)
               MOVE    SPACE         TO  WRK-RATE-AREA
               MOVE    SYS-1027-KBNCD (1:2)
                                       TO  WRK-RATE-KBN
               MOVE    SYS-1027-DISCOUNT-RATE
                                       TO  WRK-RATE-EDITZ
               EVALUATE    SYS-1027-TEIRITU
               WHEN    "0"
                   MOVE    "該当なし"  TO  WRK-RATE-EDIT-R
               WHEN    "1"
                   MOVE    "(%)"       TO  WRK-RATE-TANI
               WHEN    "2"
                   MOVE    "(円)"      TO  WRK-RATE-TANI
               END-EVALUATE
               MOVE    WRK-RATE-AREA   TO  SPA-GMN-DISCOUNT-EDIT (IDX)
      *
               MOVE    IDX             TO  SPA-DISCOUNT-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *----(01.00.07) LINE ADD START -------------------------------
           IF  SPA-GMN-DISCOUNT-KBNCD (1)  =   SPACE
               MOVE    ZERO        TO  SPA-GMN-DISCOUNT-KBNCD  (1)
               MOVE    ZERO        TO  SPA-GMN-DISCOUNT-TEIRITU(1)
               MOVE    ZERO        TO  SPA-GMN-DISCOUNT-RITU   (1)
               MOVE    "00 該当なし"
                                   TO  SPA-GMN-DISCOUNT-EDIT   (1)
               MOVE    1               TO  SPA-DISCOUNT-MAX
           END-IF
      *----(01.00.07) LINE ADD END   -------------------------------
      *
           MOVE    SPA-GMN-DISCOUNT-EDIT(1) TO  SPA-GMN-DISCOUNT
      *
      *    状態１〜３情報スパセット処理
           INITIALIZE                    SPA-GMN-CONDITION-TBL
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   3
             INITIALIZE                      SYSKANRI-REC
             EVALUATE    IDX
             WHEN    1
                 MOVE    "1018"          TO  SYS-KANRICD
             WHEN    2
                 MOVE    "1019"          TO  SYS-KANRICD
             WHEN    3
                 MOVE    "1020"          TO  SYS-KANRICD
             END-EVALUATE
             MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
             MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
             MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
             MOVE    SYSKANRI-REC        TO  MCPDATA-REC
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 910-DBSELECT-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_syskanri"      TO  MCP-TABLE
                 MOVE    "key2"              TO  MCP-PATHNAME
                 PERFORM 900-SYSTEM-READ-SEC
             ELSE
                 MOVE    1               TO  FLG-SYSKANRI
             END-IF
             PERFORM   VARYING   IDY   FROM    1   BY  1
                       UNTIL   ( FLG-SYSKANRI  =   1  )
      *H30.9   100件に
      ****             OR      ( IDY           >   99 )
                       OR      ( IDY           >  100 )
               MOVE    MCPDATA-REC       TO  SYS-1018-REC
               MOVE    SYS-1018-KBNCD (1:2)
                                 TO  SPA-GMN-CONDITION-KBNCD (IDX IDY)
               MOVE    SYS-1018-CONDITION-BODY
                                 TO  SPA-GMN-CONDITION-BODY  (IDX IDY)
               MOVE    SYS-1018-CONDITION-WORD
                                 TO  SPA-GMN-CONDITION-WORD  (IDX IDY)
      *
               MOVE    IDY       TO  SPA-CONDITION-MAX  (IDX)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
             END-PERFORM
      *      カーソルクロース
             MOVE    "tbl_syskanri"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 990-DBCLOSE-SEC
      *----(01.00.07) LINE ADD START -------------------------------
             IF  SPA-GMN-CONDITION-KBNCD (IDX 1)  =   SPACE
                 MOVE    ZERO        TO  SPA-GMN-CONDITION-KBNCD(IDX 1)
                 MOVE    "該当なし"  TO  SPA-GMN-CONDITION-BODY (IDX 1)
                 MOVE    SPACE       TO  SPA-GMN-CONDITION-WORD (IDX 1)
                 MOVE    1           TO  SPA-CONDITION-MAX (IDX)
             END-IF
      *----(01.00.07) LINE ADD END   -------------------------------
           END-PERFORM
      *
           MOVE    SPA-GMN-CONDITION-KBNCD (1  1)
                                       TO  SPA-GMN-CONDITION(1)
           MOVE    SPA-GMN-CONDITION-BODY  (1  1)
                                       TO  SPA-GMN-CONDITION(1)(4:)
           MOVE    SPA-GMN-CONDITION-KBNCD (2  1)
                                       TO  SPA-GMN-CONDITION(2)
           MOVE    SPA-GMN-CONDITION-BODY  (2  1)
                                       TO  SPA-GMN-CONDITION(2)(4:)
           MOVE    SPA-GMN-CONDITION-KBNCD (3  1)
                                       TO  SPA-GMN-CONDITION(3)
           MOVE    SPA-GMN-CONDITION-BODY  (3  1)
                                       TO  SPA-GMN-CONDITION(3)(4:)
      *
      *    入金方法情報
           INITIALIZE                  SPA-GMN-NYUKIN-HOHO-TBL
           INITIALIZE                  SYSKANRI-REC
           MOVE    "1041"              TO  SYS-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( FLG-SYSKANRI  =   1  )
      *H30.9   100件に
      *****          OR      ( IDX           >   99 )
                     OR      ( IDX           >   100)
               MOVE    MCPDATA-REC     TO  SYS-1041-REC
               MOVE    SYS-1041-KBNCD (1:2)
                                       TO  SPA-GMN-NYUKIN-HOHOL(IDX)
               MOVE    SYS-1041-NYKN-TANNAME
                                       TO  SPA-GMN-NYUKIN-MEIL (IDX)
      *
               MOVE    IDX             TO  SPA-NYUKIN-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-NYUKIN-LST(1)   TO  SPA-GMN-NYUKIN-HOHO-G
      *
      *    診療内容をＳＰＡにセット
           PERFORM 311-SRYNAI-SPASET-SEC
      *
      *    診療科をＳＰＡにセット
           PERFORM 313-SRYKA-SPASET-SEC
      *
      *    職員情報をＳＰＡにセット
           PERFORM 312-DOCTOR-SPASET-SEC
      *
      *    患者登録情報をＳＰＡにセット
           PERFORM 10031-SYSKANRI1017-SET-SEC
      *
           .
       1003-SYSKAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録情報スパ初期編集処理
      *****************************************************************
       10031-SYSKANRI1017-SET-SEC              SECTION.
      *
      *    患者登録機能制御情報
           MOVE    SPACE               TO  SYS-1017-REC
           INITIALIZE                  SYS-1017-REC
           MOVE    "1017"              TO  SYS-1017-KANRICD
           MOVE    "*"                 TO  SYS-1017-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1017-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1017-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1017-HOSPNUM
           MOVE    SYS-1017-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1017-REC
      *        人名辞書使用チェック
               MOVE    SYS-1017-JINMEI-USE     TO  SPA-NAI-JINMEI-USE
      *        患者登録のカルテ発行
               MOVE    SYS-1017-KARUTE-HAKKO   TO  SPA-NAI-KARUTE-HAKKO
      *        自費保険の補助区分
               MOVE    SYS-1017-JIHIHKN-TAX    TO  SPA-NAI-JIHIHKN-TAX
      *        同姓同名チェック
               MOVE    SYS-1017-DOUMEI-CHKFLG  TO  SPA-NAI-DOUMEI-CHKFLG
      *        二重登録疑いチェック区分
               MOVE    SYS-1017-JYUFUKU-CHKKBN TO
                                               SPA-NAI-JYUFUKU-CHKKBN
      *        地方公費単独チェック
               MOVE    SYS-1017-KOHONLY-CHKFLG TO
                                               SPA-NAI-KOHONLY-CHKFLG
      *        削除機能
               MOVE    SYS-1017-PTNUMDEL-FLG TO
                                               SPA-NAI-PTNUMDEL-FLG
      *        本人家族選択区分
               MOVE    SYS-1017-HONKZKKBN-FLG TO
                                               SPA-NAI-HONKZKKBN-FLG
      *        家族年齢区分
               IF      SYS-1017-KZKNEN         =   SPACE
                   MOVE    15                  TO  SPA-NAI-KZKNEN
               ELSE
                   MOVE    SYS-1017-KZKNEN     TO  SPA-NAI-KZKNEN
               END-IF
      *        被保険者名設定区分
               MOVE    SYS-1017-HIHKNJAKBN-FLG TO
                                               SPA-NAI-HIHKNJAKBN-FLG
      *        世帯主名自動記載区分
               MOVE    SYS-1017-SETAIKBN-FLG
                                           TO  SPA-NAI-SETAIKBN-FLG
      *        グループ医療機関患者リスト
               MOVE    SYS-1017-GRPPTLST-FLG
                                           TO  SPA-NAI-GRPPTLST-FLG
      *        公費上限額履歴チェック区分
               MOVE    SYS-1017-KOHRRKKBN-FLG
                                           TO  SPA-NAI-KOHRRKKBN-FLG
      *        郵便番号自動記載区分
               MOVE    SYS-1017-POSTKBN-FLG
                                           TO  SPA-NAI-POSTKBN-FLG
      *        都道府県名記載区分
               MOVE    SYS-1017-KENMEIKBN-FLG
                                           TO  SPA-NAI-KENMEIKBN-FLG
      *        京都府通り名称編集区分
               MOVE    SYS-1017-TOWNNAME2KBN-FLG
                                           TO  SPA-NAI-TOWNNAME2KBN-FLG
      *        公費法別一覧表示区分
               MOVE    SYS-1017-KOHHBTKBN-FLG
                                           TO  SPA-NAI-KOHHBTKBN-FLG
      *        保険初期表示区分
               MOVE    SYS-1017-HKNINITKBN-FLG
                                           TO  SPA-NAI-HKNINITKBN-FLG
           ELSE
      *        未登録の時、ゼロとする
               MOVE    ZERO                    TO  SPA-NAI-JINMEI-USE
               MOVE    ZERO                    TO  SPA-NAI-KARUTE-HAKKO
               MOVE    ZERO                    TO  SPA-NAI-JIHIHKN-TAX
               MOVE    ZERO                    TO  SPA-NAI-HONKZKKBN-FLG
      *        家族年齢区分
               MOVE    15                      TO  SPA-NAI-KZKNEN
      *        被保険者名設定区分
               MOVE    ZERO                TO  SPA-NAI-HIHKNJAKBN-FLG
      *        世帯主名自動記載区分
               MOVE    "1"                 TO  SPA-NAI-SETAIKBN-FLG
      *        グループ医療機関患者リスト
               MOVE    "0"                 TO  SPA-NAI-GRPPTLST-FLG
      *        公費上限額履歴チェック区分
               MOVE    "0"                 TO  SPA-NAI-KOHRRKKBN-FLG
      *        郵便番号自動記載区分
               MOVE    "1"                 TO  SPA-NAI-POSTKBN-FLG
               MOVE    "1"                 TO  SPA-NAI-KENMEIKBN-FLG
      *
           END-IF
           IF      SPA-NAI-DOUMEI-CHKFLG   =   SPACE
               MOVE    ZERO                TO  SPA-NAI-DOUMEI-CHKFLG
           END-IF
           IF      SPA-NAI-JYUFUKU-CHKKBN  =   SPACE
               MOVE    ZERO                TO  SPA-NAI-JYUFUKU-CHKKBN
           END-IF
           IF      SPA-NAI-PTNUMDEL-FLG    =   SPACE
               MOVE    ZERO                TO  SPA-NAI-PTNUMDEL-FLG
           END-IF
           IF      SPA-NAI-HIHKNJAKBN-FLG  =   SPACE
               MOVE    ZERO                TO  SPA-NAI-HIHKNJAKBN-FLG
           END-IF
      *    地方公費単独チェックはチェックするを初期とする
           IF      SPA-NAI-KOHONLY-CHKFLG  =   SPACE
               MOVE    "1"                 TO  SPA-NAI-KOHONLY-CHKFLG
           END-IF
      *
           IF      SPA-NAI-HONKZKKBN-FLG   =   SPACE
               MOVE    "0"                 TO  SPA-NAI-HONKZKKBN-FLG
           END-IF
      *
           IF      SPA-NAI-SETAIKBN-FLG    =   SPACE
               MOVE    "1"                 TO  SPA-NAI-SETAIKBN-FLG
           END-IF
      *    グループ医療機関患者リスト
           IF     (SPA-NAI-GRPPTLST-FLG    =   SPACE )
           OR     (SPA-GRPHOSPKBN          =   ZERO  )
      *            本院分院のない時、表示なし
               MOVE    "0"                 TO  SPA-NAI-GRPPTLST-FLG
           END-IF
      *
           IF      SPA-NAI-KOHRRKKBN-FLG   =   SPACE
               MOVE    "0"                 TO  SPA-NAI-KOHRRKKBN-FLG
           END-IF
      *
           IF      SPA-NAI-POSTKBN-FLG     =   SPACE
               MOVE    "1"                 TO  SPA-NAI-POSTKBN-FLG
           END-IF
      *
           IF      SPA-NAI-KENMEIKBN-FLG   =   SPACE
               MOVE    "1"                 TO  SPA-NAI-KENMEIKBN-FLG
           END-IF
      *    京都府通り名称編集区分
           IF      SPA-NAI-TOWNNAME2KBN-FLG    =   SPACE
               MOVE    "1"                 TO  SPA-NAI-TOWNNAME2KBN-FLG
           END-IF
      *    公費法別一覧表示区分
           IF      SPA-NAI-KOHHBTKBN-FLG   =   SPACE
               MOVE    "0"                 TO  SPA-NAI-KOHHBTKBN-FLG
           END-IF
      *    保険初期表示区分
           IF      SPA-NAI-HKNINITKBN-FLG  =   SPACE
               MOVE    "0"                 TO  SPA-NAI-HKNINITKBN-FLG
           END-IF
      *
           .
       10031-SYSKANRI1017-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容  スパ初期編集処理
      *****************************************************************
       311-SRYNAI-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING IDX
                   FROM    1       BY  1
                   UNTIL      (IDX             >   99    )   OR
                              (FLG-SYSKANRI    =   1     )
               MOVE    MCPDATA-REC             TO  SYS-1012-REC
               MOVE    SYS-1012-KBNCD          TO  SPA-GMN20-SRYNAICD
               MOVE    SPACE                   TO  SPA-GMN20-SRYNA1
               MOVE    SYS-1012-SRYNAIYODSP    TO  SPA-GMN20-SRYNAIMEI
               MOVE    SPA-GMN20-SRYNAI        TO  SPA-GMN20-SRYNAILST
                                                                  (IDX)
               MOVE    IDX                     TO  SPA-GMN20-SRYNAI-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           END-PERFORM
      *
           IF      SPA-SRYNAI-CD (1)   NOT =   SPACE
               MOVE    SPA-GMN20-SRYNAILST(1)  TO  SPA-GMN20-SRYNAI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       311-SRYNAI-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報  スパ初期編集処理
      *****************************************************************
       312-DOCTOR-SPASET-SEC              SECTION.
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN20-SRYKA     TO  ORCSDR-SRYKA
           MOVE    SPACE               TO  ORCSDR-IN-DRCD
           CALL   "ORCSDRSET"          USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN20-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           IF     SPA-GMN20-DRCDLST (1)    NOT =   SPACE
               MOVE    SPA-GMN20-DRCDLST(1)   TO  SPA-GMN20-DRCD
           END-IF
      *
           .
       312-DOCTOR-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科  スパ初期編集処理
      *****************************************************************
       313-SRYKA-SPASET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING IDX
                   FROM    1       BY  1
     *******       UNTIL      (IDX             >   40    )   OR
                   UNTIL      (IDX             >   99    )   OR
                              (FLG-SYSKANRI    =   1     )
               MOVE    MCPDATA-REC             TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)    TO  SPA-GMN-SRYKA-LST
                                                        (IDX)
               MOVE    SYS-1005-SRYKANAME1     TO  SPA-GMN-SRYKA-LST
                                                        (IDX)(4:)
               MOVE    IDX                     TO  SPA-SRYKA-MAX
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYKA-LST (1)   TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-SRYKA-LST (1)   TO  SPA-GMN20-SRYKA-G
      *
           .
       313-SRYKA-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費一覧情報取り込み処理
      *****************************************************************
       1004-HKNKOH-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-HKN-HOK-AREA
           INITIALIZE                      SPA-HKN-HOK-AREA
      *    システム日付＋１月
           MOVE    SPA-SYSYMD          TO  WRK-YMD-01
           COMPUTE WRK-MM-01           =   WRK-MM-01    +   1
           IF      WRK-MM-01           >   12
               COMPUTE WRK-YY-01       =   WRK-YY-01    +   1
               MOVE    01                  TO  WRK-MM-01
           END-IF
      *
      *    保険番号　全件編集（適用範囲分）
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL       FLG-HKNNUM  =   1
               MOVE    ZERO                TO  FLG-OK
      *      支払区分のないものは対象外とする
             IF      HKN-PAYKBN        =   SPACE
               CONTINUE
             ELSE
      *        システム日付から＋１までを範囲とする
               IF     (HKN-TEKSTYMD    <=  SPA-SYSYMD   AND
                       HKN-TEKEDYMD    >=  SPA-SYSYMD   )  OR
                      (HKN-TEKSTYMD    <=  WRK-YMD-01   AND
                       HKN-TEKEDYMD    >=  WRK-YMD-01   )
                   IF      HKN-HKNKOHSBTKBN    =   "1"  OR  "4"
                                               OR  "8"  OR  "9"
      *            保険（同じ種類のときは１つとする）
                       IF     (SPA-GMN-HKNMAX      >   ZERO )  AND
                              (HKN-HKNNUM          =   SPA-GMN-HKNNUML
                                                    (SPA-GMN-HKNMAX))
                           CONTINUE
                       ELSE
                           MOVE    1               TO  FLG-OK
                       END-IF
      *
                       IF     (FLG-OK              =   1   )
                          AND (SPA-GMN-HKNMAX      <   50  )
      *                    保険情報編集
                           PERFORM 10041-HKNHEN-SEC
                       END-IF
                   ELSE
      *                公費
                       IF     (SPA-GMN-KOHMAX      >   ZERO  )  AND
                              (HKN-HKNNUM          =   SPA-GMN-KOHNUML
                                                      (SPA-GMN-KOHMAX))
      *                    老人の時
                           IF     (HKN-HKNNUM      =   "027"  )  AND
                                  (HKN-PAYKBN      =   "01"  OR
                                                       "02"  OR
                                                       "03"   )  AND
                                  (HKN-PAYKBN  NOT =   SPA-NAI-KOHPAYKBN
                                                     (SPA-GMN-KOHMAX))
                               MOVE    1               TO  FLG-OK
                           END-IF
                       ELSE
                           MOVE    1               TO  FLG-OK
                       END-IF
                       IF     (FLG-OK              =   1   )
                         AND  (SPA-GMN-KOHMAX      <   200 )
      *                    公費情報編集
                           PERFORM 10042-KOHHEN-SEC
                       END-IF
                   END-IF
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       1004-HKNKOH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報編集処理
      *****************************************************************
       10041-HKNHEN-SEC          SECTION.
      *
      ***  IF      SPA-GMN-HKNMAX      >=  50
      *        GO      TO      10041-HKNHEN-EXT
      ***  END-IF
           ADD     1                   TO  SPA-GMN-HKNMAX
      *
           MOVE    HKN-HKNNUM          TO  SPA-GMN-HKNNUML
                                                    (SPA-GMN-HKNMAX)
           MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-HKNNUMMEIL
                                                    (SPA-GMN-HKNMAX)
           MOVE    HKN-HKNKOHSBTKBN    TO  SPA-NAI-HKNSBTKBN
                                                    (SPA-GMN-HKNMAX)
      *    保険法別番号
           MOVE    HKN-HBTNUM          TO  SPA-NAI-HKNHBTNUM
                                                    (SPA-GMN-HKNMAX)
      *    保険法別番号チェック区分
           MOVE    HKN-HBTNUMCHKKBN    TO  SPA-NAI-HKNHBTNUMKBN
                                                    (SPA-GMN-HKNMAX)
      *    支払区分
           MOVE    HKN-PAYKBN          TO  SPA-NAI-HKNPAYKBN
                                                    (SPA-GMN-HKNMAX)
      *
           .
       10041-HKNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報編集処理
      *****************************************************************
       10042-KOHHEN-SEC          SECTION.
      *
      **** IF    ( SPA-GMN-KOHMAX      >=  100 )
      *****OR    ( HKN-HKNNUM  = "960" OR "961" OR "962" OR "963" )
      *        GO      TO      10042-KOHHEN-EXT
      **** END-IF
           ADD     1                   TO  SPA-GMN-KOHMAX
      *
           MOVE    HKN-HKNNUM          TO  SPA-GMN-KOHNUML
                                                    (SPA-GMN-KOHMAX)
           IF    ( HKN-HKNNUM          =   "027"  )  AND
                 ( HKN-PAYKBN      NOT =   "01"  AND "02"
                                                 AND "03"  )
               MOVE    "老人保健"          TO  SPA-GMN-KOHNUMMEIL
                                                    (SPA-GMN-KOHMAX)
           ELSE
               MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-KOHNUMMEIL
                                                    (SPA-GMN-KOHMAX)
           END-IF
      *
      *    老人で負担割合別の時、１，２を編集する
           IF     (HKN-HKNNUM          =   "027"  ) AND
                  (HKN-PAYKBN          =   "01"  OR  "02"
                                                 OR  "03"  )
               MOVE    HKN-PAYKBN(2:1)     TO  SPA-GMN-KOHKBNL
                                                    (SPA-GMN-KOHMAX)
           ELSE
               MOVE    SPACE               TO  SPA-GMN-KOHKBNL
                                                    (SPA-GMN-KOHMAX)
           END-IF
      *
           MOVE    HKN-HKNKOHSBTKBN    TO  SPA-NAI-KOHSBTKBN
                                                    (SPA-GMN-KOHMAX)
           MOVE    HKN-JKYSKENSNUMCHKKBN
                                       TO  SPA-NAI-JKYSKENSNUMCHKKBN
                                                    (SPA-GMN-KOHMAX)
           MOVE    HKN-KENSNUMCHKKBN   TO  SPA-NAI-KENSNUMCHKKBN
                                                    (SPA-GMN-KOHMAX)
      *    保険法別番号
           MOVE    HKN-HBTNUM          TO  SPA-NAI-HBTNUM
                                                    (SPA-GMN-KOHMAX)
      *    保険法別番号チェック区分
           MOVE    HKN-HBTNUMCHKKBN    TO  SPA-NAI-KOHHBTNUMKBN
                                                    (SPA-GMN-KOHMAX)
      *    支払区分
      *    老人で負担割合別の時、１，２を編集する
           IF     (HKN-HKNNUM          =   "027"  ) AND
                  (HKN-PAYKBN      NOT =   "01"  AND "02"
                                                 AND "03"  )
               MOVE    ZERO                TO  SPA-NAI-KOHPAYKBN
                                                    (SPA-GMN-KOHMAX)
           ELSE
               MOVE    HKN-PAYKBN          TO  SPA-NAI-KOHPAYKBN
                                                    (SPA-GMN-KOHMAX)
           END-IF
           .
       10042-KOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    その他のコンボ設定処理
      *****************************************************************
       1005-COMB-SET-SEC          SECTION.
      *
      *    性別
           MOVE    "1 男"              TO  SPA-GMN-SEX-LST (1)
           MOVE    "2 女"              TO  SPA-GMN-SEX-LST (2)
      *
      *    テスト患者
           MOVE    "0 患者"            TO  SPA-GMN-TSTPTNUMKBN-LST (1)
           MOVE    "1 テスト患者"      TO  SPA-GMN-TSTPTNUMKBN-LST (2)
           MOVE    SPA-GMN-TSTPTNUMKBN-LST(1)
                                       TO  SPA-GMN-TSTPTNUMKBN-G
      *
           MOVE    "0 カルテ発行なし"  TO  SPA-GMN-HAKKO-LST (1)
           MOVE    "1 カルテ発行あり"  TO  SPA-GMN-HAKKO-LST (2)
           MOVE    "3 カルテ発行あり（病名無）"
                                       TO  SPA-GMN-HAKKO-LST (3)
           MOVE    3                   TO  SPA-HAKKO-MAX
           MOVE    SPA-GMN-HAKKO-LST (1)
                                       TO  SPA-GMN-HAKKO-G
      *    新規患者の時の患者登録のカルテ発行
           IF      SPA-P02-SYORI       =   1
               IF      SPA-NAI-KARUTE-HAKKO    =   "1"  OR  "2"
                   MOVE    SPA-GMN-HAKKO-LST (2)
                                           TO  SPA-GMN-HAKKO-G
               END-IF
           END-IF
      *
           MOVE    "0 発行なし"        TO  SPA-GMN20-KARUTE-LST (1)
           MOVE    "1 発行あり"        TO  SPA-GMN20-KARUTE-LST (2)
           MOVE    "2 続紙頭書"        TO  SPA-GMN20-KARUTE-LST (3)
           MOVE    "3 発行あり（病名無）"
                                       TO  SPA-GMN20-KARUTE-LST (4)
           MOVE    4                   TO  SPA-KARUTE-MAX
           MOVE    SPA-GMN20-KARUTE-LST(1)
                                       TO  SPA-GMN20-KARUTE-G
      *    患者登録のカルテ発行
      *    IF      SPA-P02-SYORI       =   1
      *        IF      SPA-NAI-KARUTE-HAKKO    =   "1"  OR  "2"
      *            MOVE    SPA-GMN20-KARUTE-LST(2)
      *                                TO  SPA-GMN20-KARUTE-G
      *        END-IF
      *    END-IF
      *
           MOVE    "0 発行なし"        TO  SPA-GMN20-SYOHOU-LST (1)
           MOVE    "1 発行あり"        TO  SPA-GMN20-SYOHOU-LST (2)
           MOVE    SPA-GMN20-SYOHOU-LST(1)
                                       TO  SPA-GMN20-SYOHOU-G
           MOVE    2                   TO  SPA-SYOHOU-MAX
      *
      *    ユーザプログラム起動初期
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        INITIALIZE                      SPA-NAI-GYOUMU-FLG
      *        INITIALIZE                      SPA-NAI-GYOUMU-ARI
      **   ELSE
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "P02"               TO  ORCSUSERCHK-GMNPG
           MOVE    "1"                 TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG  TO  SPA-NAI-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI  TO  SPA-NAI-GYOUMU-ARI
      *
           MOVE    "0 Ｕ・Ｐ指示なし"      TO  SPA-GMN-USERPG-LST (1)
           IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *        実行するプログラムがある時
               MOVE    "1 Ｕ・Ｐ指示あり"  TO  SPA-GMN-USERPG-LST (2)
               MOVE    2                   TO  SPA-USERPG-MAX
               IF      SPA-NAI-GYOUMU-FLG  =   "1"
                   MOVE    SPA-GMN-USERPG-LST (2)  TO  SPA-GMN-USERPG-G
               ELSE
                   MOVE    SPA-GMN-USERPG-LST (1)  TO  SPA-GMN-USERPG-G
               END-IF
           ELSE
      *        実行するプログラムがない時
               MOVE    1                   TO  SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LST (1)  TO  SPA-GMN-USERPG-G
           END-IF
      *
      **** END-IF
      *
      *    請求書発行区分
           MOVE    "0 請求書・明細書不要"
                                       TO  SPA-GMN-SKYPRTFLG-LST (1)
           MOVE    "1 請求書・明細書必要"
                                       TO  SPA-GMN-SKYPRTFLG-LST (2)
           MOVE    "2 請求書・明細書必要（請求あり）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (3)
           MOVE    "3 請求書・明細書必要（訂正時なし）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (4)
           MOVE    "4 請求書必要（明細書不要）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (5)
           MOVE    "5 請求書必要（請求あり）（明細書不要）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (6)
           MOVE    "6 請求書必要（訂正時なし）（明細書不要）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (7)
           MOVE    "7 請求書必要（請求あり）（明細書必要）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (8)
           MOVE    "8 請求書不要・明細書必要"
                                       TO  SPA-GMN-SKYPRTFLG-LST (9)
           MOVE    "9 請求書不要・明細書必要（請求あり）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (10)
           MOVE    "A 請求書不要・明細書必要（訂正時なし）"
                                       TO  SPA-GMN-SKYPRTFLG-LST (11)
           MOVE    11                  TO  SPA-SKYPRTFLG-MAX
           MOVE    SPACE               TO  SPA-GMN-SKYPRTFLG-G
      *
      *    所得者２（高齢者福祉年金受給者証）
           MOVE    "0 無し"            TO  SPA-GMN24-FUKUSHI-LST (1)
           MOVE    "1 有り"            TO  SPA-GMN24-FUKUSHI-LST (2)
           MOVE    2                   TO  SPA-GMN24-FUKUSHI-MAX
      *
      *    所得者２（認定範囲）
           MOVE    "0 すべて対象"
                                       TO  SPA-GMN24-TSYTEKKBN-LST (1)
           MOVE    "1 地方公費は対象外"
                                       TO  SPA-GMN24-TSYTEKKBN-LST (2)
           MOVE    "2 地方公費のみ対象"
                                       TO  SPA-GMN24-TSYTEKKBN-LST (3)
           MOVE    "3 食事標準負担額のみ対象"
                                       TO  SPA-GMN24-TSYTEKKBN-LST (4)
           MOVE    "4 地方公費＋食事標準負担額対象"
                                       TO  SPA-GMN24-TSYTEKKBN-LST (5)
           MOVE    5                   TO  SPA-GMN24-TSYTEKKBN-MAX
      *    所得者１（認定範囲）
           MOVE    "0 すべて対象"
                                       TO  SPA-GMN24-TNKTEKKBN-LST (1)
           MOVE    "1 地方公費は対象外"
                                       TO  SPA-GMN24-TNKTEKKBN-LST (2)
           MOVE    "2 地方公費のみ対象"
                                       TO  SPA-GMN24-TNKTEKKBN-LST (3)
           MOVE    "3 食事標準負担額のみ対象"
                                       TO  SPA-GMN24-TNKTEKKBN-LST (4)
           MOVE    "4 地方公費＋食事標準負担額対象"
                                       TO  SPA-GMN24-TNKTEKKBN-LST (5)
           MOVE    5                   TO  SPA-GMN24-TNKTEKKBN-MAX
      *H29.6
      *    所得者１（境界層該当区分）
           MOVE    "0 境界層非該当"
                                       TO  SPA-GMN24-TNKKYOUKAI-LST(1)
           MOVE    "1 境界層該当"
                                       TO  SPA-GMN24-TNKKYOUKAI-LST(2)
           MOVE    2                   TO  SPA-GMN24-TNKKYOUKAI-MAX
      *    所得者２（境界層該当区分）
           MOVE    "0 境界層非該当"
                                       TO  SPA-GMN24-TSYKYOUKAI-LST(1)
           MOVE    "1 境界層該当"
                                       TO  SPA-GMN24-TSYKYOUKAI-LST(2)
           MOVE    2                   TO  SPA-GMN24-TSYKYOUKAI-MAX
      *
      *ver.4.7
      *    患者個別設定
      *    お薬手帳　ＱＲ
           MOVE    "0 出力しない"
                                       TO  SPA-GMN28-MNOTE-QRKBN-LST(1)
           MOVE    "1 出力する"
                                       TO  SPA-GMN28-MNOTE-QRKBN-LST(2)
      *H28.11 追加
           MOVE    "2 出力する（ＣＳＶ出力なし）"
                                       TO  SPA-GMN28-MNOTE-QRKBN-LST(3)
           MOVE    3                   TO  SPA-GMN28-QRKBN-MAX
      *    お薬手帳　ＣＳＶ
           MOVE    "1 Ver.1.0"
                                       TO  SPA-GMN28-MNOTE-CSV-LST(1)
      *H28.11 追加
           MOVE    "2 Ver.2.1"
                                       TO  SPA-GMN28-MNOTE-CSV-LST(2)
           MOVE    2                   TO  SPA-GMN28-CSV-MAX
      *    情報提供同意有無
           MOVE    "0 同意しない"
                                       TO  SPA-GMN28-AGREEMENT-LST(1)
           MOVE    "1 同意する"
                                       TO  SPA-GMN28-AGREEMENT-LST(2)
           MOVE    2                   TO  SPA-GMN28-AGREEMENT-MAX
      *    日医データフォーマット区分
           MOVE    "1 Ver.0.9"         TO  SPA-GMN28-JMAMRVER-LST(1)
           MOVE    "2 Ver.1.0"         TO  SPA-GMN28-JMAMRVER-LST(2)
           MOVE    2                   TO  SPA-GMN28-JMAMRVER-MAX
      *ver.4.7
      *    患者介護設定
      *    要介護状態区分
           MOVE    "01 自立"
                                       TO  SPA-GMN28-CAREJOTAI-LST (1)
           MOVE    "11 要支援"
                                       TO  SPA-GMN28-CAREJOTAI-LST (2)
           MOVE    "12 要支援１"
                                       TO  SPA-GMN28-CAREJOTAI-LST (3)
           MOVE    "13 要支援２"
                                       TO  SPA-GMN28-CAREJOTAI-LST (4)
           MOVE    "21 要介護１"
                                       TO  SPA-GMN28-CAREJOTAI-LST (5)
           MOVE    "22 要介護２"
                                       TO  SPA-GMN28-CAREJOTAI-LST (6)
           MOVE    "23 要介護３"
                                       TO  SPA-GMN28-CAREJOTAI-LST (7)
           MOVE    "24 要介護４"
                                       TO  SPA-GMN28-CAREJOTAI-LST (8)
           MOVE    "25 要介護５"
                                       TO  SPA-GMN28-CAREJOTAI-LST (9)
           MOVE    "31 事業対象者"
                                       TO  SPA-GMN28-CAREJOTAI-LST (10)
           MOVE    10                  TO  SPA-GMN28-CAREJOTAI-MAX
      *
           .
       1005-COMB-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    新規パラメタ作成処理
      *****************************************************************
       1004-SINKI-PARA-SEC          SECTION.
      *
      *    患者マスタ初期編集
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
      *H29.11　削除
      **** IF      SPA-NAME        NOT =   SPACE
      *        MOVE    SPA-NAME            TO  PTINF-NAME
      *    END-IF
      *
      *    IF      SPA-NAME        NOT =   SPACE
      *        MOVE    SPA-NAME            TO  SPA-GMN-NAMEH
      *        MOVE    SPA-NAME            TO  SPA-GMN-NAME
      **** END-IF
      *
      *    患者番号変換マスタ初期編集
           MOVE    SPACE               TO  PTNUM-REC
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
           MOVE    SPA-GMN-PTNUM       TO  PTNUM-PTNUM
      *
      *    患者番号変換の初期内容
           INITIALIZE                  SPA-MAE-PTNUM-AREA
           IF      SPA-P02-UKEID   NOT =   ZERO
               PERFORM 4104-UKETUKE-HEN-SEC
               MOVE    SPA-NAME        TO  SPA-GMN-NAMEH
               MOVE    SPA-NAME        TO  SPA-GMN-NAME
            END-IF
      *     ＱＲデータより
            IF     (SPA-QR-FLG         =   2 )  AND
                   (SPA-GMN-PTNUM      =   SPACE )
               MOVE    P02-P02-KANANAME    TO  SPA-GMN-KANANAME
               MOVE    P02-P02-NAME        TO  SPA-GMN-NAME
               MOVE    P02-P02-NAME        TO  SPA-GMN-NAMEH
               MOVE    P02-P02-SEX         TO  SPA-GMN-SEX
               MOVE    P02-P02-BIRTHDAY    TO  SPA-GMN-BIRTHDAYW
               MOVE    P02-P02-HKNJANUM    TO  SPA-GMN-HKNJANUM
               MOVE    P02-P02-KIGO        TO  SPA-GMN-KIGO
               MOVE    P02-P02-NUM         TO  SPA-GMN-NUM
      *R02.4
               MOVE    P02-P02-EDABAN      TO  SPA-GMN-EDABAN
               MOVE    P02-P02-HONKZKBN    TO  SPA-GMN-HONKZKKBN
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HKAKUNINYMD
      *
           END-IF
           .
       1004-SINKI-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診データ　患者ID設定処理
      *      UKETUKEからの患者登録なので受診マスタを探して患者IDセット
      *****************************************************************
       4104-UKETUKE-HEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-UKETUKE
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM          TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-P02-UKEID       TO  UKE-UKEID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL       FLG-UKETUKE     =  1
               IF     (UKE-PTID        NOT =   ZERO )  AND
                      (UKE-PTID        NOT =   SPA-GMN-PTID)
      *            受付の患者ＩＤが画面と違う時、受付は対象外とする
                   CONTINUE
               ELSE
      *
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               MOVE    "UKETUKE"           TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
      ******** MOVE    SPA-PTID            TO  UKE-PTID
               MOVE    UKETUKE-REC         TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *        画面ＳＰＡへ編集
               IF      IDX                 <=  10
                   PERFORM 3101-UKETU-HEN-SEC
               END-IF
               MOVE    UKE-NAME            TO  SPA-NAME
      *
               MOVE    PARA-EDANUM          TO  SPA-NAI20-UKEMAX
      *
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-UKETUKE-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-NAI20-UKEMAX    =   ZERO
               MOVE    ZERO            TO  SPA-P02-UKEID
           END-IF
           .
       4104-UKETUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約，受付編集処理
      *****************************************************************
       3101-UKETU-HEN-SEC              SECTION.
      *
      **** IF      IDX                 >   10
      *        GO      TO      3101-UKETU-HEN-EXT
      **** END-IF
      *
           MOVE    IDX                 TO  SPA-GMN20-YBANGOU (IDX)
           MOVE    UKE-UKEID           TO  SPA-NAI20-UKEID (IDX)
           MOVE    UKE-UKETIME         TO  SPA-NAI20-UKETIME (IDX)
           MOVE    UKE-SRYKA           TO  SPA-NAI20-SRYKA   (IDX)
           MOVE    UKE-SRYNAIYO        TO  SPA-NAI20-SRYNAI  (IDX)
           MOVE    UKE-DRCD     (2:4)  TO  SPA-NAI20-DRCD    (IDX)
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
           IF  SPA-NAI20-SRYKA (IDX)   NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL     IDY       >   SPA-SRYKA-MAX
                   IF      SPA-NAI20-SRYKA (IDX)
                                          =    SPA-GMN-SRYKA-LST(IDY)
                                                              (1:2)
                       MOVE    SPA-GMN-SRYKA-LST (IDY)(4:)
                                               TO  SPA-GMN20-TSRYKA(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *    担当ドクター編集
           IF  SPA-NAI20-DRCD (IDX)    NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL     IDY           >   SPA-DRCD-MAX
                   IF  SPA-NAI20-DRCD (IDX)   =   SPA-GMN20-DRCDLST(IDY)
                                                        (1:4)
                       MOVE    SPA-GMN20-DRCDLST (IDY)(6:)
                                              TO  SPA-GMN20-TDRCD(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *    診療行為内容編集
           IF      SPA-NAI20-SRYNAI (IDX)  NOT =   SPACE
               PERFORM     VARYING   IDY   FROM    1   BY  1
                           UNTIL     IDY   >   SPA-GMN20-SRYNAI-MAX
                   IF  SPA-NAI20-SRYNAI (IDX)   =   SPA-SRYNAI-CD(IDY)
                       MOVE    SPA-SRYNAI-MEI (IDY)
                                             TO  SPA-GMN20-TSRYNAI(IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       3101-UKETU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録へＳＰＡ、パラメタ設定処理
      *****************************************************************
       1002-KYOUTU-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNID-MAX
           MOVE    ZERO                TO  WRK-KOHID-MAX
      *
      *    患者情報編集
           PERFORM 10021-PTINF-SET-SEC
      *
      *    他の情報編集
           PERFORM 10022-PARA-SET-SEC
      *ver.4.7
      *    地域連携編集
           PERFORM 10025-PTNUMPUBLIC-SET-SEC
      *ver.4.7
      *    患者個別情報編集
           PERFORM 10028-PTCONF-SET-SEC
      *ver.4.7
      *    患者介護情報編集
           PERFORM 10028-PTCARE-SET-SEC
      *H28.12
      *    患者個人番号編集
           PERFORM 10028-PTMYNUBER-SET-SEC
      *
      *    患者労災情報編集
           PERFORM 10023-RSIPARA-SET-SEC
      *
      *    同一患者識別編集
           PERFORM 10025-PTSAME-SET-SEC
      *
      *    前回来院日編集
           PERFORM 10024-LASTYMD-SET-SEC
           .
       1002-KYOUTU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報設定処理
      *****************************************************************
       10021-PTINF-SET-SEC          SECTION.
      *
      *    患者情報検索
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               GO      TO      10021-PTINF-SET-EXT
           END-IF
      *
      *    画面編集
           MOVE    PTINF-NAME          TO  SPA-GMN-NAMEH
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEXH
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEXH
           END-EVALUATE
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN-BIRTHDAYH
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
      *
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *    変更確認用氏名保存
           MOVE    SPA-GMN-KANANAME    TO  SPA-GMN-OLD-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-GMN-OLD-NAME
      *    氏名変更
           MOVE    PTINF-NAME          TO  SPA-HZN-NAME
           MOVE    ZERO                TO  SPA-HEN-FLG
      *    旧姓
           MOVE    PTINF-NAME          TO  SPA-OLD-NAME
           MOVE    PTINF-KANANAME      TO  SPA-OLD-KANANAME
           MOVE    PTINF-NICKNAME      TO  SPA-OLD-NICKNAME
      *
           MOVE    SPA-GMN-BIRTHDAYH   TO  SPA-GMN-BIRTHDAYW
      *    性別
           MOVE    PTINF-SEX           TO  SPA-GMN-SEX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-SEX     =   SPA-GMN-SEX-LST(IDX)(1:1)
                   MOVE    SPA-GMN-SEX-LST (IDX)   TO  SPA-GMN-SEX-G
                   MOVE    2                       TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-NICKNAME      TO  SPA-GMN-NICKNAME
           MOVE    PTINF-DEATHKBN      TO  SPA-GMN-DEATHKBN
           IF      SPA-GMN-DEATHKBN    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-DEATHKBN
           END-IF
      *    テスト患者
           MOVE    PTINF-TSTPTNUMKBN   TO  SPA-GMN-TSTPTNUMKBN
           IF      SPA-GMN-TSTPTNUMKBN =   SPACE
               MOVE    ZERO                TO  SPA-GMN-TSTPTNUMKBN
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               IF      SPA-GMN-TSTPTNUMKBN =   SPA-GMN-TSTPTNUMKBN-LST
                                                        (IDX)(1:1)
                   MOVE    SPA-GMN-TSTPTNUMKBN-LST (IDX)
                                           TO  SPA-GMN-TSTPTNUMKBN-G
                   MOVE    2               TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    PTINF-HOME-POST     TO  SPA-GMN-POST
           MOVE    PTINF-HOME-ADRS     TO  SPA-GMN-EDITADRS-NAME
           MOVE    PTINF-HOME-BANTI    TO  SPA-GMN-BANTI
           MOVE    PTINF-HOME-TEL1     TO  SPA-GMN-TEL1
           MOVE    PTINF-HOME-TEL2     TO  SPA-GMN-TEL2
           MOVE    PTINF-SETAINUSI     TO  SPA-GMN-SETAINUSI
           MOVE    PTINF-ZOKUGARA      TO  SPA-GMN-ZOKUGARA
           MOVE    PTINF-COMMENT-1     TO  SPA-GMN-COMMENT-1
           MOVE    PTINF-COMMENT-2     TO  SPA-GMN-COMMENT-2
      *    年齢計算
           IF      SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO
               MOVE    SPACE               TO  SPA-GMN-NENREI
           ELSE
               PERFORM 410121-NENREI-SET-SEC
           END-IF
      *    減免事由
           MOVE    PTINF-DISCOUNT-KBN  TO  SPA-GMN-DISCOUNTKBN
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
      *    状態１〜３
           MOVE    PTINF-CONDITION-1   TO  SPA-GMN-CONDITION(1)
           MOVE    PTINF-CONDITION-2   TO  SPA-GMN-CONDITION(2)
           MOVE    PTINF-CONDITION-3   TO  SPA-GMN-CONDITION(3)
      *    入金方法
           MOVE    PTINF-NYUKIN-HOHO   TO  SPA-GMN-NYUKIN-HOHO
      *    請求書発行区分
           MOVE    PTINF-SKYPRTFLG     TO  SPA-GMN-SKYPRTFLG
      *
      *    連絡先
           MOVE    PTINF-FAX           TO  SPA-GMN21-FAX
           MOVE    PTINF-KEITAI-TEL    TO  SPA-GMN21-KEITAI-TEL
           MOVE    PTINF-EMAIL         TO  SPA-GMN21-EMAIL
           MOVE    PTINF-JOB           TO  SPA-GMN21-JOB
           MOVE    PTINF-RENRAKU-NAME  TO  SPA-GMN21-RENRAKU-NAME
           MOVE    PTINF-RENRAKU-ZOKUGARA
                                       TO  SPA-GMN21-RENRAKU-ZOKUGARA
           MOVE    PTINF-RENRAKU-POST  TO  SPA-GMN21-RENRAKU-POST
           MOVE    PTINF-RENRAKU-ADRS  TO  SPA-GMN21-RENRAKU-ADRS
           MOVE    PTINF-RENRAKU-BANTI TO  SPA-GMN21-RENRAKU-BANTI
           MOVE    PTINF-RENRAKU-TEL1  TO  SPA-GMN21-RENRAKU-TEL1
           MOVE    PTINF-RENRAKU-TEL2  TO  SPA-GMN21-RENRAKU-TEL2
           MOVE    PTINF-OFFICE-NAME   TO  SPA-GMN21-OFFICE-NAME
           MOVE    PTINF-OFFICE-POST   TO  SPA-GMN21-OFFICE-POST
           MOVE    PTINF-OFFICE-ADRS   TO  SPA-GMN21-OFFICE-ADRS
           MOVE    PTINF-OFFICE-BANTI  TO  SPA-GMN21-OFFICE-BANTI
           MOVE    PTINF-OFFICE-TEL    TO  SPA-GMN21-OFFICE-TEL
           MOVE    PTINF-KISEI-NAME    TO  SPA-GMN21-KISEI-NAME
           MOVE    PTINF-KISEI-POST    TO  SPA-GMN21-KISEI-POST
           MOVE    PTINF-KISEI-ADRS    TO  SPA-GMN21-KISEI-ADRS
           MOVE    PTINF-KISEI-BANTI   TO  SPA-GMN21-KISEI-BANTI
           MOVE    PTINF-KISEI-TEL     TO  SPA-GMN21-KISEI-TEL
      *
      *    禁忌情報
           MOVE    PTINF-TABOO-1       TO  SPA-GMN-TABOO-1
           MOVE    PTINF-TABOO-2       TO  SPA-GMN-TABOO-2
           MOVE    PTINF-ALLERGY-1     TO  SPA-GMN-ALLERGY-1
           MOVE    PTINF-ALLERGY-2     TO  SPA-GMN-ALLERGY-2
           MOVE    PTINF-KANSENSYO-1   TO  SPA-GMN-KANSENSYO-1
           MOVE    PTINF-KANSENSYO-2   TO  SPA-GMN-KANSENSYO-2
      *
           IF  SPA-GMN-DISCOUNTKBN     =   SPACE
               MOVE    ZERO               TO  SPA-GMN-DISCOUNTKBN
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
      ******         OR    ( SPA-GMN-DISCOUNTKBN-KBNCD(IDX) = SPACE )
      *****          OR    ( SPA-GMN-DISCOUNTKBN (4:)   NOT = SPACE )
               IF  SPA-GMN-DISCOUNTKBN
                                     =   SPA-GMN-DISCOUNTKBN-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNTKBN-BODY (IDX)
                                     TO  SPA-GMN-DISCOUNTKBN-MEI
               END-IF
           END-PERFORM
      *    割引率
           MOVE    PTINF-DISCOUNT      TO  SPA-GMN-DISCOUNT
           IF  SPA-GMN-DISCOUNT (1:2)      =   SPACE
               MOVE    ZERO                TO  SPA-GMN-DISCOUNT (1:2)
           END-IF
           MOVE    SPACE                   TO  WRK-RATE-AREA
           MOVE    SPA-GMN-DISCOUNT        TO  WRK-RATE-KBN
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL   ( IDX         >   SPA-DISCOUNT-MAX )
                     OR      ( SPA-GMN-DISCOUNT-KBNCD(IDX) = SPACE )
                     OR      ( WRK-RATE-EDIT-R         NOT = SPACE )
               IF  WRK-RATE-KBN        =   SPA-GMN-DISCOUNT-KBNCD(IDX)
                   MOVE    SPA-GMN-DISCOUNT-RITU (IDX)
                                           TO  WRK-RATE-EDITZ
                   EVALUATE    SPA-GMN-DISCOUNT-TEIRITU (IDX)
                   WHEN    "0"
                       MOVE    "該当なし"  TO  WRK-RATE-EDIT-R
                   WHEN    "1"
                       MOVE    "(%)"       TO  WRK-RATE-TANI
                   WHEN    "2"
                       MOVE    "(円)"      TO  WRK-RATE-TANI
                   END-EVALUATE
               END-IF
           END-PERFORM
           MOVE    WRK-RATE-AREA       TO  SPA-GMN-DISCOUNT
      *    状態１〜３
           MOVE    PTINF-CONDITION-1   TO  SPA-GMN-CONDITION(1)
           IF      SPA-GMN-CONDITION(1)(1:2)  =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(1)(1:2)
           END-IF
           MOVE    1                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-2   TO  SPA-GMN-CONDITION(2)
           IF      SPA-GMN-CONDITION(2)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(2)(1:2)
           END-IF
           MOVE    2                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *
           MOVE    PTINF-CONDITION-3   TO  SPA-GMN-CONDITION(3)
           IF      SPA-GMN-CONDITION(3)(1:2)   =   SPACE
               MOVE    ZERO            TO  SPA-GMN-CONDITION(3)(1:2)
           END-IF
           MOVE    3                   TO  IDX
           PERFORM 31110-CONDITIONH-SPASET-SEC
      *    入金方法
           IF      SPA-GMN-NYUKIN-HOHO     =   SPACE
                                           OR  ZERO
               MOVE    SPA-GMN-NYUKIN-LST (01)
                                       TO  SPA-GMN-NYUKIN-HOHO-G
           END-IF
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   SPA-NYUKIN-MAX
               IF      SPA-GMN-NYUKIN-HOHO
                                       =   SPA-GMN-NYUKIN-HOHOL(IDX)
                   MOVE    SPA-GMN-NYUKIN-LST (IDX)
                                       TO  SPA-GMN-NYUKIN-HOHO-G
               END-IF
           END-PERFORM
      *    請求書発行区分
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   SPA-SKYPRTFLG-MAX
               IF      SPA-GMN-SKYPRTFLG
                                       =   SPA-GMN-SKYPRTFLG-LST(IDX)
                                                           (1:1)
                   MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                       TO  SPA-GMN-SKYPRTFLG-G
               END-IF
           END-PERFORM
      *
      *    入力の有無判定用
           MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-MAE-SEX
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-MAE-BIRTHDAY
           MOVE    SPA-GMN-DISCOUNTKBN TO  SPA-MAE-DISCOUNTKBN
      *
           MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           MOVE    SPA-GMN21-RENRAKU-ADRS  TO  SPA-MAE-RENRAKU-ADRS
           MOVE    SPA-GMN21-OFFICE-ADRS   TO  SPA-MAE-OFFICE-ADRS
           MOVE    SPA-GMN21-KISEI-ADRS    TO  SPA-MAE-KISEI-ADRS
      *
           .
       10021-PTINF-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    状態名称スパセット処理
      ****************************************************************
       31110-CONDITIONH-SPASET-SEC         SECTION.
      *
           MOVE    SPA-GMN-CONDITION (IDX)(1:2)    TO  WRK-KBNCD
      *
           PERFORM   VARYING   IDY     FROM    1   BY  1
                     UNTIL   ( IDY     >   SPA-CONDITION-MAX(IDX))
                     OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
               IF      WRK-KBNCD       =   SPA-GMN-CONDITION-KBNCD
                                                           (IDX IDY)
                   MOVE    SPA-GMN-CONDITION-BODY (IDX IDY)
                                         TO  SPA-GMN-CONDITION(IDX)(4:)
                   MOVE    SPA-GMN-CONDITION-WORD (IDX IDY)
                                         TO  SPA-GMN-CONDITIONH (IDX)
               END-IF
           END-PERFORM
      *
           .
       31110-CONDITIONH-SPASET-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       410121-NENREI-SET-SEC             SECTION.
      *
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-YMDS1
           MOVE    SPA-SYSYMD          TO  WRK-YMDS2
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
           IF      WRK-DDS1            >   WRK-DDS2
               COMPUTE WRK-NENREI-MM   =   WRK-NENREI-MM
                                           -   1
           END-IF
      *
      *    画面表示用
           IF      WRK-NENREI-YY       =   ZERO
               MOVE    WRK-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    WRK-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *
           MOVE    WRK-NENREI-YY       TO  SPA-NAI-NENREI-YY
           MOVE    WRK-NENREI-MM       TO  SPA-NAI-NENREI-MM
      *
           .
       410121-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     他の情報編集処理
      *****************************************************************
       10022-PARA-SET-SEC          SECTION.
      *
      *    保険者編集
           INITIALIZE                  SPA-PTHKNINF-AREA
      *    保険者編集
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
      *
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL   FLG-PTHKNINF    =   1
      *
               IF      PTHKN-HKNID         >   WRK-HKNID-MAX
                   MOVE    PTHKN-HKNID         TO  WRK-HKNID-MAX
               END-IF
      *
               IF      PTHKN-SAKJOKBN        =   SPACE
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "PTHKNINF"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *************WRITE   PARA-REC
      *            ＳＰＡにも編集する
                   IF      IDX               >  40
                       CONTINUE
                   ELSE
                       PERFORM 100221-PTHKNINF-SPA-SEC
                   END-IF
               END-IF
      *
               MOVE    "tbl_pthkninf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-PTHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費編集
           INITIALIZE                  SPA-PTKOHINF-AREA
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
      *
               IF      PTKOH-KOHID         >   WRK-KOHID-MAX
                   MOVE    PTKOH-KOHID         TO  WRK-KOHID-MAX
               END-IF
      *
               IF      PTKOH-SAKJOKBN        =   SPACE
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "PTKOHINF"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *            ＳＰＡにも編集する
                   IF      IDX               >  60
                       CONTINUE
                   ELSE
                       PERFORM 100222-PTKOHINF-SPA-SEC
                   END-IF
             END-IF
      *
             MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
             MOVE    "key2"              TO  MCP-PATHNAME
             PERFORM 920-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    受付データ
           IF      ORCSP02-SYORI       =   "API"
      *        APIの時は編集なし
               CONTINUE
           ELSE
      *
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
                   UNTIL      (FLG-UKETUKE     =   1 )
                       OR     (IDX             >=  10 )
               IF      UKE-UKEID           =   ZERO
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "UKETUKE"           TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    UKETUKE-REC         TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-UKETUKE-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           END-IF
      *
      *
      *    保険組合せより
           INITIALIZE                      HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-HKNCOMB-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNCOMBI
           END-IF
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-HKNCOMBI-MAX
           PERFORM   UNTIL     FLG-HKNCOMBI    =   1
      *        最大番号取得
               IF      WRK-HKNCOMBI-MAX    <   COMB-HKNCOMBINUM
                   MOVE    COMB-HKNCOMBINUM    TO  WRK-HKNCOMBI-MAX
               END-IF
      *
               IF      COMB-DELKBN             =   SPACE
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   INITIALIZE                      PARA-REC
                   MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    HKNCOMBI-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
      *
      *            非表示区分保存
                   ADD    1                    TO  SPA-HYO-MAX
      *H31.2       IF     SPA-HYO-MAX          <=  200
                   IF     SPA-HYO-MAX          <=  400
                       MOVE    COMB-HKNCOMBINUM
                                           TO  SPA-HYO-HKNCOMBINUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-HKNNUM
                                           TO  SPA-HYO-HKNNUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-KOH1HKNNUM
                                           TO  SPA-HYO-KOH1HKNNUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-KOH2HKNNUM
                                           TO  SPA-HYO-KOH2HKNNUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-KOH3HKNNUM
                                           TO  SPA-HYO-KOH3HKNNUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-KOH4HKNNUM
                                           TO  SPA-HYO-KOH4HKNNUM
                                                          (SPA-HYO-MAX)
                       MOVE    COMB-HYOJIKBN
                                           TO  SPA-HYO-HYOJIKBN
                                                          (SPA-HYO-MAX)
                       MOVE    ZERO        TO  SPA-HYO-ADDKBN
                                                          (SPA-HYO-MAX)
                   END-IF
               END-IF
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-HKNCOMB-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険組合せマスタ　総件数（既削除データ除く）
           MOVE    IDX                 TO  SPA-P02-COMBMAX
      *    保険組合せ削除データ数（パラメータファイル内）
           MOVE    ZERO                TO  SPA-P02-COMBDCNT
      *
      *    旧姓履歴出力
           INITIALIZE                      KYUSEIRRK-REC
           MOVE    SPA-HOSPNUM         TO  KYUSEI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  KYUSEI-PTID
           MOVE    KYUSEIRRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-KYUSEI-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
           MOVE    SPACE               TO  SPA-GMN-KYUSEIH
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL     FLG-KYUSEIRRK    =   1
      *
               ADD     1                   TO  IDX
      *        最終の旧姓を見出しへ
               MOVE    KYUSEI-NAME         TO  SPA-GMN-KYUSEIH
      *        ＳＰＡ編集を行う
               IF      IDX                 >   20
                   MOVE    20              TO  IDX
               END-IF
               MOVE    IDX                 TO  SPA-NAI25-KYU-MAX
               MOVE    KYUSEI-CHGYMD       TO  SPA-GMN25-KYU-CHGYMD(IDX)
               MOVE    KYUSEI-CHGYMD       TO  WRK-SYMD
               PERFORM 4001-SEIWA-YMDEDIT-SEC
               MOVE    WRK-HENYMD          TO  SPA-GMN25-KYU-CHGYMDW
                                                                 (IDX)
               MOVE    KYUSEI-KANANAME     TO  SPA-GMN25-KYU-KANANAME
                                                                (IDX)
               MOVE    KYUSEI-NAME         TO  SPA-GMN25-KYU-NAME(IDX)
               MOVE    KYUSEI-NICKNAME     TO  SPA-GMN25-KYU-NICKNAME
                                                                (IDX)
               MOVE    KYUSEI-UPYMD        TO  SPA-GMN25-KYU-UPYMD
                                                                (IDX)
               MOVE    KYUSEI-CREYMD       TO  SPA-GMN25-KYU-CREYMD
                                                                (IDX)
               MOVE    SPACE               TO  SPA-GMN25-KYU-DELKBN
                                                                (IDX)
      *
               MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 950-KYUSEI-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_kyuseirrk"     TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    低所得より
           MOVE    ZERO                TO  SPA-NAI-TSYRRK
           INITIALIZE                      TSYRRK-REC
           MOVE    SPA-HOSPNUM          TO  TSYRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  TSYRRK-PTID
      *
           MOVE    TSYRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TSYRRK
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM   UNTIL    FLG-TSYRRK    =   1
      *
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "TSYRRK"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
      *        更新キーを編集する
               MOVE    TSYRRK-HOSPNUM      TO  TSYRRK-HOSPNUM-O
               MOVE    TSYRRK-PTID         TO  TSYRRK-PTID-O
               MOVE    TSYRRK-NINSTYMD     TO  TSYRRK-NINSTYMD-O
      *        当日に有効
               IF     (TSYRRK-NINSTYMD     <=  SPA-SYSYMD )
                  AND (TSYRRK-NINEDYMD     >=  SPA-SYSYMD )
                   MOVE    1                   TO  SPA-NAI-TSYRRK
               END-IF
      *
               MOVE    TSYRRK-REC          TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *********WRITE   PARA-REC
      *
               MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-TSYRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tsyrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    年金履歴より
           MOVE    ZERO                TO  SPA-NAI-TNKRRK
           INITIALIZE                      TNKRRK-REC
           MOVE    SPA-HOSPNUM         TO  TNKRRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  TNKRRK-PTID
           MOVE    TNKRRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-TNKRRK
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM   UNTIL    FLG-TNKRRK   =   1
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "TNKRRK"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
      *        更新キーを編集する
               MOVE    TNKRRK-HOSPNUM      TO  TNKRRK-HOSPNUM-O
               MOVE    TNKRRK-PTID         TO  TNKRRK-PTID-O
               MOVE    TNKRRK-STYMD        TO  TNKRRK-STYMD-O
      *        当日に有効
               IF     (TNKRRK-STYMD        <=  SPA-SYSYMD )
                  AND (TNKRRK-EDYMD        >=  SPA-SYSYMD )
                   MOVE    1                   TO  SPA-NAI-TNKRRK
               END-IF
      *
               MOVE    TNKRRK-REC          TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *********WRITE   PARA-REC
      *
               MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-TNKRRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_tnkrrk"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費負担ＴＢＬから編集
           PERFORM 100229-KOHFT-TBL-SPA-SEC
      *
      *    特記事項ＴＢＬから編集
           PERFORM 100229-PTTOKKI-TBL-SPA-SEC
      *
      *    患者レセプト分割ＴＢＬから編集
           PERFORM 100229-PTHDISTDAY-TBL-SPA-SEC
      *
      *    患者番号管理(変換)マスタ編集
           PERFORM 970-PTNUM-READ-SEC
      *
      *    患者番号変換の保存
           IF      FLG-PTNUM           =   ZERO
      *        保険ＩＤを対象の最大とする
               MOVE    ZERO                TO  FLG-KOUSIN
               IF      PTNUM-HKNID         <   WRK-HKNID-MAX
                   MOVE    WRK-HKNID-MAX       TO  PTNUM-HKNID
                   MOVE    1                   TO  FLG-KOUSIN
               END-IF
               IF      PTNUM-KOHID         <   WRK-KOHID-MAX
                   MOVE    WRK-KOHID-MAX       TO  PTNUM-KOHID
                   MOVE    1                   TO  FLG-KOUSIN
               END-IF
               IF      FLG-KOUSIN          =   1
                   PERFORM 100221-PTNUM-UPDATE-SEC
               END-IF
               MOVE    PTNUM-HKNID         TO  SPA-PTNUM-HKNID
               MOVE    PTNUM-KOHID         TO  SPA-PTNUM-KOHID
               MOVE    PTNUM-AUTOCOMBINUM  TO  SPA-PTNUM-AUTOCOMBINUM
               MOVE    PTNUM-MANUCOMBINUM  TO  SPA-PTNUM-MANUCOMBINUM
      *        患者番号変換の初期内容保存
               MOVE    PTNUM-HKNID         TO  SPA-MAE-HKNID
               MOVE    PTNUM-KOHID         TO  SPA-MAE-KOHID
               MOVE    PTNUM-AUTOCOMBINUM  TO  SPA-MAE-AUTOCOMBINUM
               MOVE    PTNUM-MANUCOMBINUM  TO  SPA-MAE-MANUCOMBINUM
      *
      *        保険組合せの最大番号と最大連番が違う時
               IF      WRK-HKNCOMBI-MAX    NOT =   PTNUM-AUTOCOMBINUM
                   MOVE    "9106"              TO  SPA-ERRCD
                   MOVE    WRK-HKNCOMBI-MAX    TO  SPA-MAE-AUTOCOMBINUM
                   MOVE    WRK-HKNCOMBI-MAX    TO
                                               SPA-PTNUM-AUTOCOMBINUM
      *            ＳＰＡを追加できないので代用
                   MOVE    1                   TO  SPA-QRFLG
               END-IF
           ELSE
               INITIALIZE                  SPA-PTNUM-AREA
               INITIALIZE                  SPA-MAE-PTNUM-AREA
           END-IF
           .
       10022-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号更新処理
      *****************************************************************
       100221-PTNUM-UPDATE-SEC           SECTION.
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P029 PTNUM UPD ERR:" MCP-RC "."
                       ",KEY:" PTNUM-KEY
           END-IF
      *
           .
       100221-PTNUM-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担ＴＢＬから編集処理
      *****************************************************************
       100229-KOHFT-TBL-SPA-SEC           SECTION.
      *
      *    公費負担より
           INITIALIZE                      PTKOHFTN-REC
           MOVE    SPA-HOSPNUM         TO  PTKOHFTN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOHFTN-PTID
      *
           MOVE    PTKOHFTN-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 991-PTKOHFTN-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM
               UNTIL    FLG-PTKOHFTN       =   1
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "PTKOHFTN"          TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
      *        他一部負担金判定の為、クリア
               MOVE    SPACE               TO  PTKOHFTN-TERMID
               MOVE    PTKOHFTN-REC        TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *
               MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 991-PTKOHFTN-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptkohftn"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100229-KOHFT-TBL-SPA-EXT.
           EXIT.
      *****************************************************************
      *    特記事項ＴＢＬから編集処理
      *****************************************************************
       100229-PTTOKKI-TBL-SPA-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN26-TOKKI-TBL
      *    特記事項
           INITIALIZE                      PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTTOKKI-PTID
      *
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-PTTOKKI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM
               UNTIL    FLG-PTTOKKI        =   1
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "PTTOKKI"           TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    PTTOKKI-REC         TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *        ＳＰＡテーブル編集
               IF      IDX                 <=  50
                   MOVE    PTTOKKI-RENNUM      TO  SPA-NAI26-TRENNUM
                                                                (IDX)
                   MOVE    PTTOKKI-NYUGAIKBN   TO  SPA-NAI26-TNYUGIKBN
                                                                 (IDX)
                   MOVE    PTTOKKI-CD          TO  SPA-GMN26-TCD (IDX)
                   MOVE    PTTOKKI-NAIYO       TO  SPA-GMN26-TNAIYO
                                                                 (IDX)
                   MOVE    PTTOKKI-STYM        TO  SPA-NAI26-TSTYM(IDX)
                   MOVE    PTTOKKI-EDYM        TO  SPA-NAI26-TEDYM(IDX)
                   MOVE    IDX                 TO  SPA-GMN26-TMAX
               END-IF
      *
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-PTTOKKI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100229-PTTOKKI-TBL-SPA-EXT.
           EXIT.
      *****************************************************************
      *    患者レセプト分割ＴＢＬから編集処理
      *****************************************************************
       100229-PTHDISTDAY-TBL-SPA-SEC           SECTION.
      *
           INITIALIZE                      SPA-GMN26-HDLIST-TBL
      *    特記事項
           INITIALIZE                      PT-HDIST-DAY-REC
           MOVE    SPA-HOSPNUM         TO  PT-HDIST-DAY-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PT-HDIST-DAY-PTID
      *
           MOVE    PT-HDIST-DAY-REC    TO  MCPDATA-REC
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-PTHDISTDAY-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHDISTDAY
           END-IF
           MOVE    ZERO                TO  IDX
      *
           PERFORM
               UNTIL    FLG-PTHDISTDAY     =   1
               ADD     1                   TO  IDX
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    "PTHDISTDAY"        TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    PT-HDIST-DAY-REC    TO  PARA-DATA-REC
               PERFORM 400-PARA-INSERT-SEC
      *
               MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 991-PTHDISTDAY-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_pt_hdist_day"  TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       100229-PTHDISTDAY-TBL-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル作成処理
      *****************************************************************
       400-PARA-INSERT-SEC           SECTION.
      *
      *    パラメタ登録
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PARA
           END-IF
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PARA            =   1
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           ELSE
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02 PARA UPDATE ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
               END-IF
           END-IF
      *
           .
       400-PARA-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険情報ＳＰＡ編集処理
      *****************************************************************
       100221-PTHKNINF-SPA-SEC           SECTION.
      *
           MOVE    PTHKN-HKNID         TO  SPA-PTHKN-HKNID      (IDX)
           MOVE    PTHKN-HKNNUM        TO  SPA-PTHKN-HKNNUM     (IDX)
           MOVE    PTHKN-HKNJANUM      TO  SPA-PTHKN-HKNJANUM   (IDX)
           MOVE    PTHKN-HIHKNJANAME   TO  SPA-PTHKN-HIHKNJANAME(IDX)
           MOVE    PTHKN-HONKZKKBN     TO  SPA-PTHKN-HONKZKKBN  (IDX)
           MOVE    PTHKN-HOJOKBN       TO  SPA-PTHKN-HOJOKBN    (IDX)
           MOVE    PTHKN-CONTKBN       TO  SPA-PTHKN-CONTKBN    (IDX)
           MOVE    PTHKN-KIGO          TO  SPA-PTHKN-KIGO       (IDX)
           MOVE    PTHKN-NUM           TO  SPA-PTHKN-NUM        (IDX)
      *R02.4
           MOVE    PTHKN-EDABAN        TO  SPA-PTHKN-EDABAN        (IDX)
           MOVE    PTHKN-SKKGETYMD     TO  SPA-PTHKN-SKKGETYMD  (IDX)
           MOVE    PTHKN-TEKSTYMD      TO  SPA-PTHKN-TEKSTYMD   (IDX)
           MOVE    PTHKN-TEKEDYMD      TO  SPA-PTHKN-TEKEDYMD   (IDX)
           MOVE    PTHKN-KAKUNINYMD    TO  SPA-PTHKN-KAKUNINYMD (IDX)
           MOVE    PTHKN-SAKJOKBN      TO  SPA-PTHKN-SAKJOKBN   (IDX)
      *
           MOVE    PTHKN-SKKGETYMD     TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-SKKGETYMDW (IDX)
      *
           MOVE    PTHKN-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKSTYMDW (IDX)
      *
           MOVE    PTHKN-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-TEKEDYMDW (IDX)
      *
           MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTHKN-KAKUNINYMDW (IDX)
      *
      *    保険公費区分編集
           MOVE    PTHKN-HKNNUM        TO  WRK-HKNNUM
           MOVE    ZERO                TO  WRK-PAYKBN
           MOVE    PTHKN-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTHKN-TEKEDYMD      TO  WRK-TEKEDYMD
           PERFORM 1002211-HKNNUM-SET-SEC
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTHKN-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTHKN-HKNNUM-MEI (IDX)
      *    確認年月日保存
           MOVE    PTHKN-KAKUNINYMD    TO  SPA-PTHKN-MAEKAKUNINYMD (IDX)
           MOVE    SPA-PTHKN-KAKUNINYMDW (IDX)
                                       TO  SPA-PTHKN-MAEKAKUNINYMDW(IDX)
      *
           MOVE    IDX                 TO  SPA-PTHKN-MAX
      *
           .
       100221-PTHKNINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報ＳＰＡ編集処理
      *****************************************************************
       100222-PTKOHINF-SPA-SEC           SECTION.
      *
           MOVE    PTKOH-KOHID         TO  SPA-PTKOH-KOHID      (IDX)
           MOVE    PTKOH-KOHNUM        TO  SPA-PTKOH-KOHNUM     (IDX)
           MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN     (IDX)
           MOVE    PTKOH-FTNJANUM      TO  SPA-PTKOH-FTNJANUM   (IDX)
           MOVE    PTKOH-JKYSNUM       TO  SPA-PTKOH-JKYSNUM    (IDX)
           MOVE    PTKOH-TEKSTYMD      TO  SPA-PTKOH-TEKSTYMD   (IDX)
           MOVE    PTKOH-TEKEDYMD      TO  SPA-PTKOH-TEKEDYMD   (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-PTKOH-KAKUNINYMD (IDX)
           MOVE    PTKOH-SAKJOKBN      TO  SPA-PTKOH-SAKJOKBN   (IDX)
           MOVE    PTKOH-TEKEDCHKKBN   TO  SPA-PTKOH-TEKEDCHKKBN   (IDX)
      *
           MOVE    PTKOH-TEKSTYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKSTYMDW (IDX)
      *
           MOVE    PTKOH-TEKEDYMD      TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-TEKEDYMDW (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-PTKOH-KAKUNINYMDW (IDX)
      *
           IF     (PTKOH-KOHNUM        =   "027") AND
                  (PTKOH-TEKSTYMD      >=  "20010101" )  AND
                  (PTKOH-TEKEDYMD      <=  "20020930" )
               IF     (PTKOH-PAYKBN    NOT =   ZERO   )
                   MOVE    ZERO                TO  PTKOH-PAYKBN
                   MOVE    PTKOH-PAYKBN        TO  SPA-PTKOH-PAYKBN
                                                                (IDX)
               END-IF
           END-IF
      *    保険公費区分編集
           MOVE    PTKOH-KOHNUM        TO  WRK-HKNNUM
           MOVE    PTKOH-PAYKBN        TO  WRK-PAYKBN
           MOVE    PTKOH-TEKSTYMD      TO  WRK-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  WRK-TEKEDYMD
      *
           IF     (WRK-HKNNUM          =   "027") AND
                  (WRK-PAYKBN          =   ZERO OR SPACE)
      *        Ｈ１２．１２．３１までの支払区分は００
               IF      WRK-TEKEDYMD        >=  "20010101"
                   MOVE    SPA-NAI-HOSPSBT         TO  WRK-PAYKBN(1:1)
                   MOVE    SPA-NAI-ROUPAYKBN       TO  WRK-PAYKBN(2:1)
               END-IF
           END-IF
           PERFORM 1002211-HKNNUM-SET-SEC
           IF     (PTKOH-KOHNUM        =   "027" )  AND
                  (PTKOH-PAYKBN        =   ZERO  )
               MOVE    "老人保健"          TO  WRK-TANSEIDONAME
           END-IF
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-PTKOH-HKN-SBTKBN (IDX)
           MOVE    WRK-TANSEIDONAME    TO  SPA-PTKOH-KOHNUM-MEI (IDX)
      *    確認年月日保存
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-PTKOH-MAEKAKUNINYMD (IDX)
           MOVE    SPA-PTKOH-KAKUNINYMDW (IDX)
                                       TO  SPA-PTKOH-MAEKAKUNINYMDW(IDX)
      *
           MOVE    IDX                 TO  SPA-PTKOH-MAX
      *
           .
       100222-PTKOHINF-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費区分編集処理
      *****************************************************************
       1002211-HKNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
      *****MOVE    WRK-TEKEDYMD        TO  HKN-TEKEDYMD
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNMST-NEXT-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKOHSBTKBN
           MOVE    SPACE               TO  WRK-TANSEIDONAME
           IF      FLG-HKNNUM          =   ZERO
               IF     (WRK-TEKSTYMD        >=  HKN-TEKSTYMD )  AND
                      (WRK-TEKEDYMD        <=  HKN-TEKEDYMD )
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
      *        対象を開始日のみとする
               IF      WRK-TEKSTYMD        >=  HKN-TEKSTYMD
                   MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
                   MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
               END-IF
           END-IF
      *****PERFORM
      *        UNTIL   FLG-HKNNUM      =   1
      *        IF     (WRK-TEKSTYMD        >=  HKN-TEKSTYMD )  AND
      *               (WRK-TEKEDYMD        <=  HKN-TEKEDYMD )
      *            MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
      *            MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
      *            MOVE    1                   TO  FLG-HKNNUM
      *        ELSE
      *            MOVE    "HKNNUM-KEY2"       TO  ORC-DBPATH
      *            PERFORM 921-HKNMST-NEXT-SEC
      *        END-IF
      *****END-PERFORM
      *
           .
       1002211-HKNNUM-SET-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    地域連携編集処理
      *****************************************************************
       10025-PTNUMPUBLIC-SET-SEC           SECTION.
      *
           INITIALIZE                  PTPUBLIC-REC
           MOVE    SPA-HOSPNUM         TO  PTPUBLIC-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTPUBLIC-PTID
      *
           MOVE    PTPUBLIC-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-PTPUBLIC-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
           MOVE    "tbl_ptnum_public"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    PTPUBLIC-PATIENT-ID1    TO  SPA-GMN28-PATIENTID1
      *    情報提供同意有無
           MOVE    PTPUBLIC-AGREEMENT  TO  SPA-GMN28-AGREEMENT
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-AGREEMENT-MAX )
               IF      SPA-GMN28-AGREEMENT
                                       =  SPA-GMN28-AGREEMENT-LST
                                                  (IDX)(1:1)
                   MOVE    SPA-GMN28-AGREEMENT-LST (IDX)
                                       TO  SPA-GMN28-AGREEMENT-G
                   MOVE    SPA-GMN28-AGREEMENT-MAX    TO  IDX
               END-IF
           END-PERFORM
           .
       10025-PTNUMPUBLIC-SET-EXT.
           EXIT.
      *ver.4.7
      *****************************************************************
      *    患者個別情報編集処理
      *****************************************************************
       10028-PTCONF-SET-SEC           SECTION.
      *
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCONF-PTID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           PERFORM UNTIL       FLG-PTCONF      =   1
               EVALUATE    PTCONF-CKEY
                   WHEN    "MNOTEQR"
      *            お薬手帳ＱＲ区分
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-MNOTE-QRKBN
                   WHEN    "MNOTEVER"
      *            お薬手帳データフォーマット区分
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-MNOTE-CSV
                   WHEN    "JMAMRVER"
      *            日医データフォーマット区分
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-JMAMRVER
      *H26.1
                   WHEN    "BIRTHWEIGHT"
      *            出生時体重
                       MOVE    PTCONF-CDATA(1:4)
                                           TO  SPA-GMN-BIRTHWEIGHT-X
      *H26.4
                   WHEN    "TIIKIHOKATU"
      *            地域包括診療対象疾病
                       MOVE    PTCONF-CDATA(1:4)
                                           TO  SPA-GMN28-CHKHKTBYO-AREA
      *H28.4
                   WHEN    "NINTIHOKATU"
      *            認知症地域包括診療
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-NNCHIKBN
                   WHEN    "SYOUNIKAKARI"
      *            小児かかりつけ診療
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-SYOUNIKBN
      *H28.12
      *            指導料等自動算定
                   WHEN    "JIDOSANTEI-1"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-GMN28-AUTOSRYCD(1)
                   WHEN    "JIDOSANTEI-2"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-GMN28-AUTOSRYCD(2)
                   WHEN    "JIDOSANTEI-3"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-GMN28-AUTOSRYCD(3)
      *H30.4
                   WHEN    "NINPUKBN"
      *            妊婦区分
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN28-NINPUKBN
      *R01.6
                   WHEN    "COMBKBN"
      *            難病等＋生保保険
                       MOVE    PTCONF-CDATA(1:1)
                                           TO  SPA-GMN27-COMBKBN
               END-EVALUATE
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-QRKBN-MAX )
               IF      SPA-GMN28-MNOTE-QRKBN
                                       =  SPA-GMN28-MNOTE-QRKBN-LST
                                                  (IDX)(1:1)
                   MOVE    SPA-GMN28-MNOTE-QRKBN-LST (IDX)
                                       TO  SPA-GMN28-MNOTE-QRKBN-G
                   MOVE    SPA-GMN28-QRKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-CSV-MAX )
               IF      SPA-GMN28-MNOTE-CSV
                                       =  SPA-GMN28-MNOTE-CSV-LST
                                                  (IDX)(1:1)
                   MOVE    SPA-GMN28-MNOTE-CSV-LST (IDX)
                                       TO  SPA-GMN28-MNOTE-CSV-G
                   MOVE    SPA-GMN28-CSV-MAX       TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-JMAMRVER-MAX )
               IF      SPA-GMN28-JMAMRVER
                                       =  SPA-GMN28-JMAMRVER-LST
                                                  (IDX)(1:1)
                   MOVE    SPA-GMN28-JMAMRVER-LST (IDX)
                                           TO  SPA-GMN28-JMAMRVER-G
                   MOVE    SPA-GMN28-JMAMRVER-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *H28.12
      *    指導料等自動算定
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   3 )
               IF      SPA-GMN28-AUTOSRYCD (IDX)   NOT =   SPACE
                    MOVE    SPA-GMN28-AUTOSRYCD (IDX)
                                               TO  WRK-SRYCD
                    PERFORM 910-TENSU-READ-SEC
                    MOVE    WRK-SRYCD-NAME     TO  SPA-GMN28-AUTONAME
                                                             (IDX)
               END-IF
           END-PERFORM
      *R01.6
      *    難病等＋生保保険 P02COMMON-SPA へ
           MOVE    SPA-GMN27-COMBKBN   TO  SPA-NAI-COMBKBN
           MOVE    SPA-GMN27-COMBKBN   TO  SPA-MAE27-COMBKBN
           .
       10028-PTCONF-SET-EXT.
           EXIT.
      *ver.4.7
      *
      *****************************************************************
      *    患者介護情報編集処理
      *****************************************************************
       10028-PTCARE-SET-SEC           SECTION.
      *
      *    患者介護保険情報
           MOVE    SPACE               TO  PTCARE-HKNINF-REC
           INITIALIZE                  PTCARE-HKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTCRHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCRHKN-PTID
      *
           MOVE    PTCARE-HKNINF-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-HKNINF   =   1  )
                          OR  (IDX                 >=  10 )
               ADD     1                       TO  IDX
               PERFORM 100281-PTCAREHKNINF-HEN-SEC
      *
               MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCAREHKNINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_hkninf" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    患者介護保険情報
           INITIALIZE                  PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTCRNIN-PTID
      *
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCARE-NINTEI
               INITIALIZE                  PTCARE-NINTEI-REC
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTCARE-NINTEI   =   1  )
                          OR  (IDX                 >=  50 )
               ADD     1                       TO  IDX
               PERFORM 100281-PTCARENINTEI-HEN-SEC
      *
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10028-PTCARE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護保険情報テーブル編集処理
      *****************************************************************
       100281-PTCAREHKNINF-HEN-SEC           SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN28-TCHNUM (IDX)
      *
           MOVE    PTCRHKN-HKNJANUM    TO  SPA-GMN28-TCRHKNJANUM
                                                             (IDX)
           MOVE    PTCRHKN-HIHKNJANUM  TO  SPA-GMN28-TCRHIHKNJANUM
                                                             (IDX)
           MOVE    PTCRHKN-TEKSTYMD    TO  SPA-NAI28-TCRHSTRYMD
                                                             (IDX)
           MOVE    PTCRHKN-TEKEDYMD    TO  SPA-NAI28-TCRHENDYMD
                                                             (IDX)
      *
           MOVE    PTCRHKN-TEKSTYMD   TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN28-TCRHSTRYMD (IDX)
      *
           MOVE    PTCRHKN-TEKEDYMD   TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN28-TCRHENDYMD (IDX)
      *
           MOVE    "1"                 TO  SPA-NAI28-TCHKNSKBN (IDX)
           MOVE    IDX                 TO  SPA-GMN28-CRHKN-MAX
           .
       100281-PTCAREHKNINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護保険情報テーブル編集処理
      *****************************************************************
       100281-PTCARENINTEI-HEN-SEC           SECTION.
      *
           MOVE    IDX                 TO  SPA-GMN28-TCRNUM (IDX)
      *
           MOVE    PTCRNIN-CAREJOTAICD TO  SPA-NAI28-TCAREJOTAI
                                                             (IDX)
           MOVE    PTCRNIN-NINTEIYMD   TO  SPA-NAI28-TCRNINYMD
                                                             (IDX)
           MOVE    PTCRNIN-TEKSTYMD    TO  SPA-NAI28-TCRSTRYMD
                                                             (IDX)
           MOVE    PTCRNIN-TEKEDYMD    TO  SPA-NAI28-TCRENDYMD
                                                             (IDX)
      *
           MOVE    PTCRNIN-NINTEIYMD   TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN28-TCRNINYMD (IDX)
      *
           MOVE    PTCRNIN-TEKSTYMD   TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN28-TCRSTRYMD (IDX)
      *
           MOVE    PTCRNIN-TEKEDYMD   TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  SPA-GMN28-TCRENDYMD (IDX)
      *
      *    要介護状態
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                    UNTIL    ( IDY     >   SPA-GMN28-CAREJOTAI-MAX )
                         OR  ( FLG-OK  =   1      )
               IF      PTCRNIN-CAREJOTAICD
                                       =  SPA-GMN28-CAREJOTAI-LST 
                                                      (IDY)(1:2)
                   MOVE    SPA-GMN28-CAREJOTAI-LST (IDY)
                                       TO  SPA-GMN28-TCAREJOTAI (IDX)
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF     (FLG-OK              =   ZERO     )
               MOVE    PTCRNIN-CAREJOTAICD     TO  SPA-GMN28-TCAREJOTAI
                                                                   (IDX)
           END-IF
      *
           MOVE    "1"                 TO  SPA-NAI28-TINSKBN (IDX)
           MOVE    IDX                 TO  SPA-GMN28-CRNIN-MAX
           .
       100281-PTCARENINTEI-HEN-EXT.
           EXIT.
      *
      *H28.12
      *****************************************************************
      *    患者個人番号編集処理
      *****************************************************************
       10028-PTMYNUBER-SET-SEC           SECTION.
      *
      *    患者個人番号情報
           MOVE    SPACE               TO  PTMYNUMBER-REC
           INITIALIZE                  PTMYNUMBER-REC
           MOVE    SPA-HOSPNUM         TO  PTMYNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTMYNUM-PTID
      *
           MOVE    PTMYNUMBER-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTMYNUMBER-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTMYNUMBER
               INITIALIZE                  PTMYNUMBER-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL      (FLG-PTMYNUMBER      =   1  )
                          OR  (IDX                 >=  20 )
               ADD     1                   TO  IDX
               MOVE    IDX                 TO  SPA-GMN28-MYID-MAX
               MOVE    IDX                 TO  SPA-GMN28-TMYNUM
                                                             (IDX)
               MOVE    PTMYNUM-ID-KEY      TO  SPA-GMN28-TMYIDKEY
                                                             (IDX)
               MOVE    PTMYNUM-MY-NUMBER   TO  SPA-GMN28-TMYNUMBER
                                                             (IDX)
               MOVE    PTMYNUM-DESCRIPTION TO  SPA-GMN28-TMYDESCRIP
                                                             (IDX)
      *
               MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTMYNUMBER-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptmynumber"    TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10028-PTMYNUBER-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災情報編集処理
      *****************************************************************
       10023-RSIPARA-SET-SEC           SECTION.
      *
      *    患者労災情報編集
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-PTRSIINF
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM   UNTIL    FLG-PTRSIINF   =   1
               IF      PTRSI-SAKUJOKBN       =   SPACE
      *H26.4
      *            保険区分設定なしは、削除とする （空白で登録できた為）
                   IF      PTRSI-HKNNUM        =   SPACE
                       MOVE    "1"             TO  PTRSI-SAKUJOKBN
                       MOVE    "9108"          TO  SPA-ERRCD
                   END-IF
      *
                   ADD     1                   TO  IDX
                   MOVE    SPACE               TO  PARA-REC
                   MOVE    "PTRSIINF"          TO  PARA-FILEMEI
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PTRSIINF-REC        TO  PARA-DATA-REC
                   PERFORM 400-PARA-INSERT-SEC
               END-IF
      *H26.4
      *        保険・公費情報とのチェック（保険・公費が削除されない？）
               IF      PTRSI-HKNNUM        =   "970"
                   PERFORM 100231-RSI-PTKOHCHEK-SEC
               ELSE
                   PERFORM 100231-RSI-PTHKNCHEK-SEC
               END-IF
      *
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    患者労災保険情報マスタ　総件数（既削除データ除く）
           MOVE    IDX                 TO  SPA-P02-RSHKNMAX
      *
           .
       10023-RSIPARA-SET-EXT.
           EXIT.
      *****************************************************************
      *    労災保険と患者保険情報チェック処理
      *****************************************************************
       100231-RSI-PTHKNCHEK-SEC            SECTION.
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-PTHKN-MAX )
                          OR  (FLG-OK  =   1             )
               IF     (PTRSI-HKNID     =   SPA-PTHKN-HKNID (IDX2))
                  AND (PTRSI-HKNNUM    =   SPA-PTHKN-HKNNUM(IDX2))
      *            労災保険が削除であれば削除とする
                   IF      PTRSI-SAKUJOKBN      =   "1"
                       MOVE    "1"              TO  SPA-PTHKN-SAKJOKBN
                                                                (IDX2)
                       MOVE    "9108"           TO  SPA-ERRCD
                   END-IF
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       100231-RSI-PTHKNCHEK-EXT.
           EXIT.
      *****************************************************************
      *    労災保険と患者公費情報チェック処理
      *****************************************************************
       100231-RSI-PTKOHCHEK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-PTKOH-MAX )
                          OR  (FLG-OK  =   1             )
               IF     (PTRSI-KOHID     =   SPA-PTKOH-KOHID (IDX2) )
                  AND (PTRSI-HKNNUM    =   SPA-PTKOH-KOHNUM (IDX2))
      *            労災保険が削除であれば削除とする
                   IF      PTRSI-SAKUJOKBN      =   "1"
                       MOVE    "1"              TO  SPA-PTKOH-SAKJOKBN
                                                                (IDX2)
                       MOVE    "9108"           TO  SPA-ERRCD
                   END-IF
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *
           .
       100231-RSI-PTKOHCHEK-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別編集処理
      *****************************************************************
       10025-PTSAME-SET-SEC            SECTION.
      *
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTSAME          =   ZERO 
      *        同一患者−医療機関ＩＤ
               MOVE    PTSAME-SAME-HOSPNUM   TO  SPA-NAI-GRPHOSPNUM
      *        同一患者−患者ＩＤ
               MOVE    PTSAME-SAME-PTID      TO  SPA-NAI-GRPPTID
           END-IF
           .
       10025-PTSAME-SET-EXT.
           EXIT.
      *****************************************************************
      *    前回来院日編集処理
      *****************************************************************
       10024-LASTYMD-SET-SEC           SECTION.
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    SPA-GMN-PTID        TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *    入院中は病棟を表示
           IF      ORCSLASTYMD-BRMNUM  =   SPACE
               IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
                   STRING  "前回："
                           ORCSLASTYMD-LASTYMDW    DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD
                   END-STRING
               END-IF
           ELSE
               STRING  "病棟："
                       ORCSLASTYMD-BRMNUM      DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD
               END-STRING
           END-IF
      *
           .
       10024-LASTYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       4001-SEIWA-YMDEDIT-SEC           SECTION.
      *
           EVALUATE    WRK-SYMD
               WHEN    SPACE
                   MOVE    SPACE           TO  WRK-HENYMD
               WHEN    ZERO
                   MOVE    "0000000"       TO  WRK-HENYMD
               WHEN    "99999999"
                   MOVE    "9999999"       TO  WRK-HENYMD
               WHEN    OTHER
      *
                   MOVE    SPACE               TO  STS-AREA-DAY
                   INITIALIZE                      STS-AREA-DAY
                   MOVE    SPACE               TO  LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING
                                               STS-AREA-DAY
                                               LNK-DAY2-AREA
                   IF      STS-DAY-RC1         =   ZERO
                       MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMD
                   ELSE
                       MOVE    SPACE               TO  WRK-HENYMD
                   END-IF
           END-EVALUATE
           .
       4001-SEIWA-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       200-P02-SET-SEC         SECTION.
      *
           IF     (SPA-NAI24-INIT2     =   1   )  OR
                  (SPA-FTN2-MAX        =   ZERO )
               MOVE    ZERO                TO  WRK-HFTNLST2-ROW
               MOVE    ZERO                TO  SPA-NAI24-INIT2
           ELSE
               MOVE    P02-P024-HFTNLST2-ROW   TO  WRK-HFTNLST2-ROW
           END-IF
           IF     (SPA-GMN27-MAX        =   ZERO )
             OR   (SPA-NAI27-SELNUM     =   ZERO )
             OR   (SPA-P02-GAMEN    NOT =   "P027")
               MOVE    ZERO                    TO  WRK-HKNKUMI-ROW
           ELSE
               MOVE    P02-P027-HKNKUMI-ROW    TO  WRK-HKNKUMI-ROW
           END-IF
      *
           IF     (SPA-NAI26-INIT      =   1      )
             OR   (SPA-P02-GAMEN    NOT =   "P026")
               MOVE    ZERO                TO  WRK-P026-ROW1
               MOVE    ZERO                TO  WRK-P026-ROW2
               MOVE    ZERO                TO  SPA-NAI26-INIT
           ELSE
               MOVE    P02-P026-HDLST-ROW  TO  WRK-P026-ROW2
               MOVE    P02-P026-TOK-ROW    TO  WRK-P026-ROW1
           END-IF
      *
           MOVE    SPACE               TO  P02
           INITIALIZE                      P02
      *
           EVALUATE    SPA-P02-GAMEN
               WHEN    "P020"
                   MOVE    ZERO                TO  P02-PAGENO
               WHEN    "P02 "
                   MOVE    1                   TO  P02-PAGENO
               WHEN    "P027"
                   MOVE    2                   TO  P02-PAGENO
               WHEN    "P021"
                   MOVE    3                   TO  P02-PAGENO
      *******  WHEN    "P022"
      *******      MOVE    4                   TO  P02-PAGENO
      ******** WHEN    "P023"
      ********     MOVE    4                   TO  P02-PAGENO
               WHEN    "P024"
                   MOVE    4                   TO  P02-PAGENO
               WHEN    "P025"
                   MOVE    5                   TO  P02-PAGENO
               WHEN    "P026"
                   MOVE    6                   TO  P02-PAGENO
               WHEN    "P028"
                   MOVE    7                   TO  P02-PAGENO
               WHEN    OTHER
                   MOVE    "P02 "              TO  SPA-P02-GAMEN
                   MOVE    1                   TO  P02-PAGENO
           END-EVALUATE
      *
      *    見出し状態
           MOVE    SPA-GMN-PTNUM           TO  P02-PTNUM
           MOVE    SPA-GMN-NAMEH           TO  P02-NAMEH
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  P02-NAMEH-STYLE
           ELSE
               MOVE    "red"               TO  P02-NAMEH-STYLE
           END-IF
      *
           MOVE    SPA-GMN-SEXH            TO  P02-SEXH
           MOVE    SPA-GMN-BIRTHDAYH       TO  P02-BIRTHDAYH
           MOVE    SPA-GMN-KYUSEIH         TO  P02-KYUSEIH
           MOVE    SPA-GMN-LASTYMD         TO  P02-LASTYMD
           MOVE    SPA-GMN-CONDITIONH (1)  TO  P02-P02-CONDITIONH1
           MOVE    SPA-GMN-CONDITIONH (2)  TO  P02-P02-CONDITIONH2
           MOVE    SPA-GMN-CONDITIONH (3)  TO  P02-P02-CONDITIONH3
      *
           PERFORM 2001-P020-SET-SEC
      *
           PERFORM 2001-P02W1-SET-SEC
      *
           PERFORM 2001-P027-SET-SEC
      *
           PERFORM 2001-P021-SET-SEC
      *
           PERFORM 2001-P023-SET-SEC
      *
           PERFORM 2001-P024-SET-SEC
      *
           PERFORM 2001-P025-SET-SEC
      *
           PERFORM 2001-P026-SET-SEC
      *
           PERFORM 2001-P028-SET-SEC
      *
      *    主科対応
           IF      SPA-SYUKA-KBN       =   "1"
               MOVE    WIDGET-NORMAL       TO  P02-B10S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P02-B10S-STATE
           END-IF
      *    削除機能
           IF      SPA-NAI-PTNUMDEL-FLG    =   "1"
               MOVE    WIDGET-INSENSITIVE  TO  P02-B04-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  P02-B04-STATE
           END-IF
      *    ＱＲ一覧
      *DD  IF      SPA-QRFLG               =   ZERO
      *        MOVE    WIDGET-INSENSITIVE  TO  P02-B12S-STATE
      *    ELSE
               MOVE    WIDGET-NORMAL       TO  P02-B12S-STATE
      *DD  END-IF
      *    患者紐付け
           IF      SPA-GRPHOSPKBN      =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  P02-B03S-STATE
               MOVE    SPACE               TO  P02-B03S-LABEL
           ELSE
               MOVE    WIDGET-NORMAL       TO  P02-B03S-STATE
               MOVE    "患者紐付け"        TO  P02-B03S-LABEL
           END-IF
      *ver.4.7
      *    前次切替え
           MOVE    WIDGET-NORMAL       TO  P02-B02S-STATE
           EVALUATE    SPA-NAI-NEXTFLG
               WHEN    0
                   MOVE    "↑↓全"        TO  P02-B02S-LABEL
               WHEN    1
                   MOVE    "↑↓保険"      TO  P02-B02S-LABEL
               WHEN    2
                   MOVE    "↑↓公費"      TO  P02-B02S-LABEL
           END-EVALUATE
      *    排他中更新不可
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  P02-B12-STATE
               MOVE    WIDGET-NORMAL       TO  P02-B05S-STATE
               IF      SPA-NAI-PTNUMDEL-FLG    NOT =   "1"
                   MOVE    WIDGET-NORMAL       TO  P02-B04-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P02-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02-B05S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02-B04-STATE
               MOVE    WIDGET-INSENSITIVE  TO  P02-B03S-STATE
           END-IF
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    WIDGET-INSENSITIVE  TO  P02-P02-USERPG-STATE
      ***  ELSE
               MOVE    WIDGET-NORMAL       TO  P02-P02-USERPG-STATE
      **** END-IF
           .
       200-P02-SET-EXT.
           EXIT.
      *****************************************************************
      *    受付画面編集処理
      *****************************************************************
       2001-P020-SET-SEC         SECTION.
      *
      *****INITIALIZE                  P02-FIXPAGE1
      *
           MOVE    ZERO                TO  P02-P020-UKETUKE-LIST-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   10 )
                   OR        ( SPA-GMN20-YBANGOU(IDX)  =  ZERO )
               MOVE    SPA-GMN20-YBANGOU  (IDX) TO  WRK-RENNUMZ
               MOVE    WRK-RENNUMZ              TO  P02-P020-ULNUM(IDX)
               MOVE    SPA-GMN20-TYYKTIME (IDX)
                                       TO  P02-P020-ULYYKTIME (IDX)(3:)
               MOVE    SPA-GMN20-TUKETIME (IDX)
                                       TO  P02-P020-ULUKETIME (IDX)(3:)
      *H29.4
               IF    SPA-NAI20-DELKBN(IDX)  =   1
      *            削除分
                   MOVE    ALL "*"     TO  P02-P020-ULUKETIME(IDX)
               END-IF
      *
               MOVE    SPA-GMN20-TSRYKA   (IDX)
                                       TO  P02-P020-ULSRYKA   (IDX)
               MOVE    SPA-GMN20-TDRCD    (IDX)
                                       TO  P02-P020-ULDRNAME  (IDX)
               MOVE    SPA-GMN20-TSRYNAI  (IDX)
                                       TO  P02-P020-ULSRYNAIYO(IDX)
               MOVE    SPA-GMN20-TKARUTE  (IDX)
                                       TO  P02-P020-ULKARUTE  (IDX)
               MOVE    SPA-GMN20-TSYOHOU  (IDX)
                                       TO  P02-P020-ULSYOHO   (IDX)
               IF      SPA-GMN20-THKNCOMBIMEI (IDX)  =   SPACE  OR  ZERO
                   CONTINUE
               ELSE
                   MOVE  SPA-GMN20-THKNCOMBIMEI(IDX)
                                       TO  P02-P020-ULHKNCOMBINUM(IDX)
               END-IF
               IF      IDX             =   SPA-NAI20-YSELNUM
                   MOVE    "T"         TO  P02-P020-UKETUKE-LIST-SELECT
                                                                  (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P020-UKETUKE-LIST-SELECT
                                                                  (IDX)
               END-IF
               MOVE    IDX             TO  P02-P020-UKETUKE-LIST-COUNT
           END-PERFORM
           MOVE    SPA-GMN20-YSELNUM   TO  P02-P020-SELNUM
      *
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LST (IDX)
                                       TO  P02-P020-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  P02-P020-SRYKA-COUNT
           MOVE    SPA-GMN20-SRYKA-G   TO  P02-P020-SRYKA
      *
           MOVE    ZERO         TO  P02-P020-DRNAME-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL     ( IDX     >   SPA-DRCD-MAX  )
                   OR        ( SPA-GMN20-DRCDLST (IDX)  =   SPACE )
               MOVE    SPA-GMN20-DRCDLST(IDX)
                                       TO  P02-P020-DRNAME-ITEM (IDX)
               MOVE    IDX             TO  P02-P020-DRNAME-COUNT
           END-PERFORM
           MOVE    SPA-GMN20-DRCD      TO  P02-P020-DRNAME
      *
           MOVE    ZERO        TO  P02-P020-SRYNAIYO-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN20-SRYNAI-MAX
               MOVE    SPA-GMN20-SRYNAILST(IDX)
                                   TO  P02-P020-SRYNAIYO-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN20-SRYNAI-MAX    TO  P02-P020-SRYNAIYO-COUNT
           MOVE    SPA-GMN20-SRYNAI        TO  P02-P020-SRYNAIYO
      *
      *    カルテ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-KARUTE-MAX
               MOVE    SPA-GMN20-KARUTE-LST (IDX)
                                       TO  P02-P020-KARUTE-ITEM (IDX)
               IF      SPA-GMN20-KARUTE  =   SPA-GMN20-KARUTE-LST (IDX)
                                                                 (1:1)
                   MOVE    SPA-GMN20-KARUTE-LST (IDX)
                                           TO  SPA-GMN20-KARUTE-G
               END-IF
               MOVE    IDX             TO  P02-P020-KARUTE-COUNT
           END-PERFORM
           MOVE    SPA-GMN20-KARUTE-G  TO  P02-P020-KARUTE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOU-MAX
               MOVE    SPA-GMN20-SYOHOU-LST (IDX)
                                       TO  P02-P020-SYOHOU-ITEM (IDX)
               IF      SPA-GMN20-SYOHOU  =   SPA-GMN20-SYOHOU-LST (IDX)
                                                                 (1:1)
                   MOVE    SPA-GMN20-SYOHOU-LST (IDX)
                                           TO  SPA-GMN20-SYOHOU-G
               END-IF
               MOVE    IDX                 TO  P02-P020-SYOHOU-COUNT
           END-PERFORM
           MOVE    SPA-GMN20-SYOHOU-G  TO  P02-P020-SYOHOU
      *
           MOVE    SPA-GMN20-HKNKUMI   TO  P02-P020-HKNKUMI
      *
      *    保険組合せ
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKNCOMBI-MAX )
                           OR (IDZ     >=  30      )
               IF      SPA-NAI-THYOJIKBN (IDX) =   "1"
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDZ
                   MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                           TO   P02-P020-HKNCOBNUM (IDZ)
                   MOVE    SPA-GMN-THKNCOMBIMEI (IDX)
                                           TO  P02-P020-HKNCOBMEI (IDZ)
                   MOVE    SPA-GMN-TTEKSTYMD (IDX)
                                           TO  P02-P020-TEKSTYMD (IDZ)
                   MOVE    SPA-GMN-TTEKEDYMD (IDX)
                                           TO  P02-P020-TEKEDYMD (IDZ)
      *            非表示
                   EVALUATE    SPA-NAI-THYOJIKBN (IDX)
                   WHEN    "2"
                       MOVE    SPACE       TO  P02-P020-HKNCOBMEI (IDZ)
                       STRING  "【入非表】" 
                               SPA-GMN-THKNCOMBIMEI(IDX)
                                               DELIMITED  BY  SIZE
                                         INTO  P02-P020-HKNCOBMEI (IDZ)
                       END-STRING
                   WHEN    "3"
                       MOVE    SPACE       TO  P02-P020-HKNCOBMEI (IDZ)
                       STRING  "【外非表】" 
                               SPA-GMN-THKNCOMBIMEI(IDX)
                                               DELIMITED  BY  SIZE
                                         INTO  P02-P020-HKNCOBMEI (IDZ)
                       END-STRING
                   END-EVALUATE
      *
                   IF      SPA-GMN-THKNCOMBINUM (IDX)
                                           =   SPA-NAI20-HKNKUMI
                       MOVE    "T"         TO  P02-P020-HKNCOMBI-SELECT
                                                                  (IDZ)
                   ELSE
                       MOVE    "F"         TO  P02-P020-HKNCOMBI-SELECT
                                                                  (IDZ)
                   END-IF
               END-IF
      *
           END-PERFORM
           MOVE    IDZ                 TO  P02-P020-HKNCOMBI-COUNT
      *
           .
       2001-P020-SET-EXT.
           EXIT.
      *****************************************************************
      *    基本情報画面編集処理
      *****************************************************************
       2001-P02W1-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  P02-P02-KANANAME
           MOVE    SPA-GMN-NAME        TO  P02-P02-NAME
           MOVE    SPA-GMN-NICKNAME    TO  P02-P02-NICKNAME
      *
           MOVE    SPA-GMN-SEX-G           TO  P02-P02-SEX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-SEX-LST (IDX)
                                   TO  P02-P02-SEX-ITEM (IDX)
               MOVE    IDX         TO  P02-P02-SEX-COUNT
           END-PERFORM
      *
           MOVE    SPA-GMN-BIRTHDAYW       TO  P02-P02-BIRTHDAY
           MOVE    SPA-GMN-NENREI          TO  P02-P02-NENREI
           IF    SPA-GMN-DEATHKBN          =   "1"
               MOVE    "T"                 TO  P02-P02-DEATHKBN-VALUE
               MOVE    "死亡"              TO  P02-P02-DEATHKBN-LABEL
           ELSE
               MOVE    "F"                 TO  P02-P02-DEATHKBN-VALUE
               MOVE    SPACE               TO  P02-P02-DEATHKBN-LABEL
           END-IF
      *    テスト患者
           MOVE    SPA-GMN-TSTPTNUMKBN-G   TO  P02-TSTPTNUMKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
               MOVE    SPA-GMN-TSTPTNUMKBN-LST (IDX)
                                       TO  P02-TSTPTNUMKBN-ITEM (IDX)
               MOVE    IDX             TO  P02-TSTPTNUMKBN-COUNT
           END-PERFORM
      *    カルテ発行有無
           MOVE    SPA-GMN-HAKKO-G     TO  P02-P02-HAKKO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKKO-MAX
               MOVE    SPA-GMN-HAKKO-LST (IDX)
                                       TO  P02-P02-HAKKO-ITEM (IDX)
               MOVE    IDX             TO  P02-P02-HAKKO-COUNT
           END-PERFORM
      *
      *    Ｕ・Ｐ
           MOVE    SPA-GMN-USERPG-G    TO  P02-P02-USERPG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LST (IDX)
                                       TO  P02-P02-USERPG-ITEM (IDX)
               MOVE    IDX             TO  P02-P02-USERPG-COUNT
           END-PERFORM
      *    低所得有無
           MOVE    SPACE               TO  WRK-TEIMSG-G
           IF      SPA-NAI-TNKRRK      =   1
               MOVE    "低所１"            TO  WRK-TMSG1
           END-IF
           IF      SPA-NAI-TSYRRK      =   1
               MOVE    "低所２"            TO  WRK-TMSG3
           END-IF
           IF      (SPA-NAI-TSYRRK     =   1  )  AND
                   (SPA-NAI-TNKRRK     =   1  )
               MOVE    "、"                TO  WRK-TMSG2
           END-IF
           IF      (SPA-NAI-TSYRRK     =   1  )  OR
                   (SPA-NAI-TNKRRK     =   1  )
               STRING    "【"              DELIMITED  BY  SIZE
                         WRK-TMSG1         DELIMITED  BY  SPACE
                         WRK-TMSG2         DELIMITED  BY  SPACE
                         WRK-TMSG3         DELIMITED  BY  SPACE
                             "】"          DELIMITED  BY  SIZE
                                           INTO  WRK-TEIMSG
               END-STRING
           END-IF
           MOVE    WRK-TEIMSG          TO  P02-TEIMSG-VALUE
           MOVE    "blue"              TO  P02-TEIMSG-STYLE
      *
      *H30.4
           IF      SPA-GMN28-NINPUKBN  =   "1"
               MOVE    "（妊婦）"            TO  P02-TEIMSG3-VALUE
               MOVE    "blue"              TO  P02-TEIMSG3-STYLE
           END-IF
      *
      *
           IF      SPA-NAI-GRPHOSPNUM  =   ZERO
               MOVE    SPACE               TO  P02-TEIMSG2-VALUE
               MOVE    "black"             TO  P02-TEIMSG2-STYLE
           ELSE
               MOVE    "【他】"            TO  P02-TEIMSG2-VALUE
               MOVE    "darkgreen"         TO  P02-TEIMSG2-STYLE
           END-IF
      *
           MOVE    SPA-GMN-TABOO-1         TO  P02-P02-TABOO1
           MOVE    SPA-GMN-TABOO-2         TO  P02-P02-TABOO2
           MOVE    SPA-GMN-ALLERGY-1       TO  P02-P02-ALLERGY1
           MOVE    SPA-GMN-ALLERGY-2       TO  P02-P02-ALLERGY2
           MOVE    SPA-GMN-KANSENSYO-1     TO  P02-P02-KANSENSYO1
           MOVE    SPA-GMN-KANSENSYO-2     TO  P02-P02-KANSENSYO2
      *
           MOVE    SPA-GMN-POST            TO  P02-P02-POST
           MOVE    SPA-GMN-EDITADRS-NAME   TO  P02-P02-ADRS
           MOVE    SPA-GMN-BANTI           TO  P02-P02-BANTI
           MOVE    SPA-GMN-TEL1            TO  P02-P02-TEL1
           MOVE    SPA-GMN-TEL2            TO  P02-P02-TEL2
           MOVE    SPA-GMN-SETAINUSI       TO  P02-P02-SETAINUSI
           MOVE    SPA-GMN-ZOKUGARA        TO  P02-P02-ZOKUGARA
           MOVE    SPA-GMN-DISCOUNTKBN-G   TO  P02-P02-DISCOUNTKBN
           MOVE    SPA-GMN-CONDITION (1)   TO  P02-P02-CONDITION1
           MOVE    SPA-GMN-CONDITION (2)   TO  P02-P02-CONDITION2
           MOVE    SPA-GMN-CONDITION (3)   TO  P02-P02-CONDITION3
           MOVE    SPA-GMN-DISCOUNT        TO  P02-P02-DISCOUNT
           MOVE    SPA-GMN-COMMENT-1       TO  P02-P02-COMMENT1
           MOVE    SPA-GMN-COMMENT-2       TO  P02-P02-COMMENT2
           MOVE    SPA-GMN-NYUKIN-HOHO-G   TO  P02-P02-NYUKIN
           MOVE    SPA-GMN-SKYPRTFLG-G     TO  P02-P02-SKYPRTFLG
      *
           MOVE    ZERO                TO  P02-P02-CONDITION1-COUNT
           MOVE    ZERO                TO  P02-P02-CONDITION2-COUNT
           MOVE    ZERO                TO  P02-P02-CONDITION3-COUNT
      *    状態１
           MOVE    ZERO                TO  P02-P02-CONDITION1-COUNT
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   SPA-CONDITION-MAX(1)) OR
                              (SPA-GMN-CONDITION-KBNCD (1 IDX)
                                           =   SPACE)
               MOVE    SPA-GMN-CONDITION-KBNCD (1 IDX)
                                           TO  P02-P02-CONDITION1-ITEM
                                                           (IDX)(1:)
               MOVE    SPA-GMN-CONDITION-BODY  (1 IDX)
                                           TO  P02-P02-CONDITION1-ITEM
                                                           (IDX)(4:)
               MOVE    IDX                 TO  P02-P02-CONDITION1-COUNT
           END-PERFORM
      *    状態２
           MOVE    ZERO                TO  P02-P02-CONDITION2-COUNT
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   SPA-CONDITION-MAX(2)) OR
                              (SPA-GMN-CONDITION-KBNCD (2 IDX)
                                           =   SPACE)
               MOVE    SPA-GMN-CONDITION-KBNCD (2 IDX)
                                           TO  P02-P02-CONDITION2-ITEM
                                                           (IDX)(1:)
               MOVE    SPA-GMN-CONDITION-BODY  (2 IDX)
                                           TO  P02-P02-CONDITION2-ITEM
                                                           (IDX)(4:)
               MOVE    IDX                 TO  P02-P02-CONDITION2-COUNT
           END-PERFORM
      *    状態３
           MOVE    ZERO                TO  P02-P02-CONDITION3-COUNT
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   SPA-CONDITION-MAX(3)) OR
                              (SPA-GMN-CONDITION-KBNCD (3 IDX)
                                           =   SPACE)
               MOVE    SPA-GMN-CONDITION-KBNCD (3 IDX)
                                           TO  P02-P02-CONDITION3-ITEM
                                                           (IDX)(1:)
               MOVE    SPA-GMN-CONDITION-BODY  (3 IDX)
                                           TO  P02-P02-CONDITION3-ITEM
                                                           (IDX)(4:)
               MOVE    IDX                 TO  P02-P02-CONDITION3-COUNT
           END-PERFORM
      *    減免事由
           MOVE    ZERO                TO  P02-P02-DISCOUNTKBN-COUNT
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX           >   SPA-DISCOUNTKBN-MAX)  OR
                            (SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                                           =   SPACE)
               MOVE    SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                                       TO  P02-P02-DISCOUNTKBN-ITEM
                                                             (IDX)(1:)
               MOVE    SPA-GMN-DISCOUNTKBN-BODY  (IDX)
                                       TO  P02-P02-DISCOUNTKBN-ITEM
                                                             (IDX)(4:)
               MOVE    IDX             TO  P02-P02-DISCOUNTKBN-COUNT
           END-PERFORM
      *    割引率
           MOVE    ZERO                TO  P02-P02-DISCOUNT-COUNT
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX           >   SPA-DISCOUNT-MAX)  OR
                            (SPA-GMN-DISCOUNT-EDIT (IDX)
                                           =   SPACE)
               MOVE    SPA-GMN-DISCOUNT-EDIT (IDX)
                                       TO  P02-P02-DISCOUNT-ITEM(IDX)
               MOVE    IDX             TO  P02-P02-DISCOUNT-COUNT
           END-PERFORM
      *    入金方法
           MOVE    SPA-GMN-NYUKIN-HOHO-G   TO  P02-P02-NYUKIN
           MOVE    ZERO                TO  P02-P02-NYUKIN-COUNT
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-NYUKIN-MAX
               MOVE    SPA-GMN-NYUKIN-LST  (IDX)
                                       TO  P02-P02-NYUKIN-ITEM(IDX)
               MOVE    IDX             TO  P02-P02-NYUKIN-COUNT
           END-PERFORM
      *    入金方法
           MOVE    SPA-GMN-SKYPRTFLG-G     TO  P02-P02-SKYPRTFLG
           MOVE    ZERO                TO  P02-P02-SKYPRTFLG-COUNT
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-SKYPRTFLG-MAX
               MOVE    SPA-GMN-SKYPRTFLG-LST  (IDX)
                                       TO  P02-P02-SKYPRTFLG-ITEM(IDX)
               MOVE    IDX             TO  P02-P02-SKYPRTFLG-COUNT
           END-PERFORM
      *
      *    出生時体重
           MOVE    SPA-GMN-BIRTHWEIGHT TO  P02-P02-BIRTHWEIGHT
      *
      *    保険情報
      *    保険者番号
           MOVE    SPA-GMN-HKNJANUM    TO  P02-P02-HKNJANUM
      *    保険種別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HKNMAX
               MOVE    SPA-GMN-HKNNUMLST (IDX)
                                       TO  P02-HKNNUM-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HKNMAX      TO  P02-HKNNUM-COUNT
           MOVE    SPA-GMN-HKNNUM-G    TO  P02-P02-HKNNUM
      *    本人家族
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HONKZK-MAX
               MOVE    SPA-GMN-HONKZKKBNL (IDX)
                                       TO  P02-HONKZKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HONKZK-MAX  TO  P02-HONKZKBN-COUNT
           MOVE    SPA-GMN-HONKZKKBN-G TO  P02-P02-HONKZKBN
      *    補助区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-HOJO-MAX
               MOVE    SPA-GMN-HOJOKBNL (IDX)
                                       TO  P02-HOJOKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-HOJO-MAX    TO  P02-HOJOKBN-COUNT
           MOVE    SPA-GMN-HOJOKBN-G   TO  P02-P02-HOJOKBN
      *    継続
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-CONT-MAX
               MOVE    SPA-GMN-CONTKBNL (IDX)
                                       TO  P02-CONTKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN-CONT-MAX    TO  P02-CONTKBN-COUNT
           MOVE    SPA-GMN-CONTKBN-G   TO  P02-P02-CONTKBN
      *    有効開始
           MOVE    SPA-GMN-HTEKSTYMD   TO  P02-P02-HTEKSTYMD
           MOVE    SPA-GMN-HTEKEDYMD   TO  P02-P02-HTEKEDYMD
           MOVE    SPA-GMN-KIGO        TO  P02-P02-KIGO
           MOVE    SPA-GMN-NUM         TO  P02-P02-NUM
      *R02.4
           MOVE    SPA-GMN-EDABAN      TO  P02-P02-EDABAN
           MOVE    SPA-GMN-HKAKUNINYMD TO  P02-P02-HKAKUNINYMD
           MOVE    SPA-GMN-SKKGETYMD   TO  P02-P02-SKKGETYMD
           MOVE    SPA-GMN-HIHKNJANAME TO  P02-P02-HIHKNJANAME
      *
           IF      SPA-NAI-HKAKUNINYMD (1:6)
                                       =   SPA-SYSYMD (1:6)
               MOVE    "T"             TO  P02-BH1-VALUE
               MOVE    "済"            TO  P02-BH1-LABEL
           ELSE
               MOVE    "F"             TO  P02-BH1-VALUE
               MOVE    "未"            TO  P02-BH1-LABEL
           END-IF
      *ver.4.7
      *    確認年月日履歴
           IF     (SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
                                       OR  SPACE )
           OR     (SPA-NAI-HKNSTR      =   ZERO )
           OR    ((SPA-NAI-HKNSTR      >   ZERO )  AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) =   "1"))
               MOVE    WIDGET-INSENSITIVE  TO  P02-BHR-STATE
               MOVE    SPACE               TO  P02-BHR-LABEL
           ELSE
               MOVE    WIDGET-NORMAL       TO  P02-BHR-STATE
               MOVE    "履歴"              TO  P02-BHR-LABEL
           END-IF
      *    保険情報
           MOVE    SPA-HKNJA-HKNJANAME TO  P02-P02-HKNNAME
           MOVE    SPA-HKNJA-HKNJANAME(31:)
                                       TO  P02-P02-HKNNAME2
           MOVE    SPA-HKNJA-ADRS      TO  P02-P02-HKNJAADRS
           MOVE    SPA-HKNJA-BANTI     TO  P02-P02-HKNJAADRS2
           MOVE    SPA-HKNJA-TEL       TO  P02-P02-HKNJATEL
      *ver.4.7
      *    保険者マスタの異動年月日編集
           IF      SPA-HKNJA-IDOYMD    NOT =   SPACE
               PERFORM 20012-P02W1-HKNJASET-SEC
           END-IF
      *
      *    ラベルの全角空白を半角へ
      **** INSPECT P02-P02-HKNNAME
      *            REPLACING   ALL "　"  BY  "  "
      *    INSPECT P02-P02-HKNNAME2
      *            REPLACING   ALL "　"  BY  "  "
      *    INSPECT P02-P02-HKNJAADRS
      *            REPLACING   ALL "　"  BY  "  "
      *    INSPECT P02-P02-HKNJAADRS2
      *****        REPLACING   ALL "　"  BY  "  "
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >=  24
               IF      P02-P02-HKNNAME(IDX:2)  =   "　"
                   MOVE    "  "            TO  P02-P02-HKNNAME(IDX:2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >=  24
               IF      P02-P02-HKNNAME2(IDX:2)  =   "　"
                   MOVE    "  "            TO  P02-P02-HKNNAME2(IDX:2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >=  24
               IF      P02-P02-HKNJAADRS(IDX:2)  =   "　"
                   MOVE    "  "            TO  P02-P02-HKNJAADRS(IDX:2)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL       IDX     >=  24
               IF      P02-P02-HKNJAADRS2(IDX:2)  =   "　"
                   MOVE    "  "            TO  P02-P02-HKNJAADRS2(IDX:2)
               END-IF
           END-PERFORM
      *H28.11
      *    公費画面編集
           PERFORM 20011-P02W1-KOHSET-SEC
      *
      *    公費種類を編集する
           PERFORM 20011-HOKNUM-HEN-SEC
      *
      *    保険組合せ
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  P02-HKNLIST-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBI-MAX
               ADD     1                   TO  IDX2
               MOVE    SPA-GMN-THKNCOMBINUM(IDX)
                                           TO   P02-HKNCOBNUM (IDX2)
               MOVE    SPA-GMN-THKNCOMBIMEI (IDX)
                                           TO  P02-HKNCOBNAME (IDX2)
      *
               IF      SPA-GMN-THKNCOMBINUM (IDX)  =   SPA-GMN-SELNUM
                   MOVE    "T"             TO  P02-HKNLIST-SELECT (IDX2)
               ELSE
                   MOVE    "F"             TO  P02-HKNLIST-SELECT (IDX2)
               END-IF
      *
               ADD     1                   TO  IDX2
               MOVE    SPA-GMN-TTEKSTYMD (IDX)
                                           TO  P02-HKNCOBNAME (IDX2)
               MOVE    "〜"                TO  P02-HKNCOBNAME (IDX2)
                                                            (11:2)
               MOVE    SPA-GMN-TTEKEDYMD (IDX)
                                           TO  P02-HKNCOBNAME (IDX2)
                                                            (14:)
      *        非表示
               EVALUATE    SPA-NAI-THYOJIKBN (IDX)
               WHEN    "1"
                   MOVE    "●非"          TO  P02-HKNCOBNUM (IDX2)
               WHEN    "2"
                   MOVE    "●入"          TO  P02-HKNCOBNUM (IDX2)
               WHEN    "3"
                   MOVE    "●外"          TO  P02-HKNCOBNUM (IDX2)
               END-EVALUATE
      *
               MOVE    "F"             TO  P02-HKNLIST-SELECT (IDX2)
           END-PERFORM
           MOVE    IDX2                TO  P02-HKNLIST-COUNT
      *
           MOVE    SPA-GMN-SELNUM      TO  P02-P02-SELNUM
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LST (IDX)
                                       TO  P02-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  P02-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  P02-P02-SRYKA
           .
       2001-P02W1-SET-EXT.
           EXIT.
      *H28.11
      *****************************************************************
      *    公費画面編集処理
      *****************************************************************
       20011-P02W1-KOHSET-SEC         SECTION.
      *
      *    公費情報１
           MOVE    SPA-GMN-KOHNUM-G (01)   TO  P02-P02-KOHNUM1
           MOVE    SPA-GMN-FTNJANUM (01)   TO  P02-P02-FTNJANUM1
           MOVE    SPA-GMN-JKYSNUM  (01)   TO  P02-P02-JKYSNUM1
           MOVE    SPA-GMN-KTEKSTYMD(01)   TO  P02-P02-KTEKSTYMD1
           MOVE    SPA-GMN-KTEKEDYMD(01)   TO  P02-P02-KTEKEDYMD1
           MOVE    SPA-GMN-KKAKUNINYMD(01) TO  P02-P02-KKAKUNINYMD1
           IF      SPA-NAI-KKAKUNINYMD(01)(1:6)
                                           =   SPA-SYSYMD(1:6)
               MOVE    "T"                 TO  P02-BK1-VALUE
               MOVE    "済"                TO  P02-BK1-LABEL
           ELSE
               MOVE    "F"                 TO  P02-BK1-VALUE
               MOVE    "未"                TO  P02-BK1-LABEL
           END-IF
      *    公費期限切れ区分
           IF      SPA-NAI-TEKEDCHKKBN (1) =   "1"
               MOVE    "T"                 TO  P02-CK1-VALUE
               MOVE    "有"                TO  P02-CK1-LABEL
           ELSE
               MOVE    "F"                 TO  P02-CK1-VALUE
               MOVE    "無"                TO  P02-CK1-LABEL
           END-IF
           IF      SPA-NAI-TEKEDCHKKBN (2) =   "1"
               MOVE    "T"                 TO  P02-CK2-VALUE
               MOVE    "有"                TO  P02-CK2-LABEL
           ELSE
               MOVE    "F"                 TO  P02-CK2-VALUE
               MOVE    "無"                TO  P02-CK2-LABEL
           END-IF
           IF      SPA-NAI-TEKEDCHKKBN (3) =   "1"
               MOVE    "T"                 TO  P02-CK3-VALUE
               MOVE    "有"                TO  P02-CK3-LABEL
           ELSE
               MOVE    "F"                 TO  P02-CK3-VALUE
               MOVE    "無"                TO  P02-CK3-LABEL
           END-IF
      *    公費情報２
           MOVE    SPA-GMN-KOHNUM-G (02)   TO  P02-P02-KOHNUM2
           MOVE    SPA-GMN-FTNJANUM (02)   TO  P02-P02-FTNJANUM2
           MOVE    SPA-GMN-JKYSNUM  (02)   TO  P02-P02-JKYSNUM2
           MOVE    SPA-GMN-KTEKSTYMD(02)   TO  P02-P02-KTEKSTYMD2
           MOVE    SPA-GMN-KTEKEDYMD(02)   TO  P02-P02-KTEKEDYMD2
           MOVE    SPA-GMN-KKAKUNINYMD(02) TO  P02-P02-KKAKUNINYMD2
           IF      SPA-NAI-KKAKUNINYMD(02)(1:6)
                                           =   SPA-SYSYMD(1:6)
               MOVE    "T"                 TO  P02-BK2-VALUE
               MOVE    "済"                TO  P02-BK2-LABEL
           ELSE
               MOVE    "F"                 TO  P02-BK2-VALUE
               MOVE    "未"                TO  P02-BK2-LABEL
           END-IF
      *    公費情報３
           MOVE    SPA-GMN-KOHNUM-G (03)   TO  P02-P02-KOHNUM3
           MOVE    SPA-GMN-FTNJANUM (03)   TO  P02-P02-FTNJANUM3
           MOVE    SPA-GMN-JKYSNUM  (03)   TO  P02-P02-JKYSNUM3
           MOVE    SPA-GMN-KTEKSTYMD(03)   TO  P02-P02-KTEKSTYMD3
           MOVE    SPA-GMN-KTEKEDYMD(03)   TO  P02-P02-KTEKEDYMD3
           MOVE    SPA-GMN-KKAKUNINYMD(03) TO  P02-P02-KKAKUNINYMD3
           IF      SPA-NAI-KKAKUNINYMD(03)(1:6)
                                           =   SPA-SYSYMD(1:6)
               MOVE    "T"                 TO  P02-BK3-VALUE
               MOVE    "済"                TO  P02-BK3-LABEL
           ELSE
               MOVE    "F"                 TO  P02-BK3-VALUE
               MOVE    "未"                TO  P02-BK3-LABEL
           END-IF
           .
       20011-P02W1-KOHSET-EXT.
           EXIT.
      *****************************************************************
      *    公費種類編集処理
      *****************************************************************
       20011-HOKNUM-HEN-SEC         SECTION.
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-KOHMAX
               MOVE    SPA-GMN-KOHNUMLST (IDY)     TO  P02-KOHNUM1-ITEM
                                                                  (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOHMAX      TO  P02-KOHNUM1-COUNT
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-KOHMAX
               MOVE    SPA-GMN-KOHNUMLST (IDY)     TO  P02-KOHNUM2-ITEM
                                                                  (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOHMAX      TO  P02-KOHNUM2-COUNT
      *
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-GMN-KOHMAX
               MOVE    SPA-GMN-KOHNUMLST (IDY)     TO  P02-KOHNUM3-ITEM
                                                                  (IDY)
           END-PERFORM
           MOVE    SPA-GMN-KOHMAX      TO  P02-KOHNUM3-COUNT
           .
       20011-HOKNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険者マスタの異動年月日編集処理
      *****************************************************************
       20012-P02W1-HKNJASET-SEC         SECTION.
      *
           EVALUATE    SPA-HKNJA-IDOKBN
               WHEN    "1"
                   MOVE    "廃止"              TO  WRK-IDOMSG
               WHEN    "2"
                   MOVE    "支部廃止"          TO  WRK-IDOMSG
               WHEN    "3"
                   MOVE    "解散"              TO  WRK-IDOMSG
               WHEN    "4"
                   MOVE    "合併後消滅"        TO  WRK-IDOMSG
               WHEN    OTHER
                   MOVE    SPACE               TO  WRK-IDOMSG
           END-EVALUATE
           MOVE    SPA-HKNJA-IDOYMD    TO  WRK-SYMD
           PERFORM 4001-SEIWA-YMDEDIT-SEC
           IF      WRK-IDOMSG          =   SPACE
               STRING  "【異動年月日　"
                       WRK-HENYMD
                       "】"            DELIMITED   BY  SIZE
                                       INTO    P02-HKNJA-IDOMSG-VALUE
               END-STRING
           ELSE
               STRING  "【"
                       WRK-IDOMSG      DELIMITED   BY  SPACE
                       "　"
                       WRK-HENYMD
                       "】"            DELIMITED   BY  SIZE
                                       INTO    P02-HKNJA-IDOMSG-VALUE
               END-STRING
           END-IF
           IF      SPA-HKNJA-IDOYMD    <   SPA-NAI-HTEKEDYMD
               MOVE    "red"               TO  P02-HKNJA-IDOMSG-STYLE
           ELSE
               MOVE    "black"             TO  P02-HKNJA-IDOMSG-STYLE
           END-IF
           .
       20012-P02W1-HKNJASET-EXT.
           EXIT.
      *****************************************************************
      *    連絡先画面編集処理
      *****************************************************************
       2001-P021-SET-SEC         SECTION.
      *
           MOVE    SPA-GMN21-FAX           TO  P02-P021-FAX
           MOVE    SPA-GMN21-KEITAI-TEL    TO  P02-P021-KEITAI
           MOVE    SPA-GMN21-EMAIL         TO  P02-P021-EMAIL
           MOVE    SPA-GMN21-JOB           TO  P02-P021-JOB
           MOVE    SPA-GMN21-RENRAKU-NAME  TO  P02-P021-RENNAME
           MOVE    SPA-GMN21-RENRAKU-ZOKUGARA
                                        TO  P02-P021-RENZOKUGARA
           MOVE    SPA-GMN21-RENRAKU-POST   TO  P02-P021-RENPOST
           MOVE    SPA-GMN21-RENRAKU-ADRS   TO  P02-P021-RENADRS
           MOVE    SPA-GMN21-RENRAKU-BANTI  TO  P02-P021-RENBANTI
           MOVE    SPA-GMN21-RENRAKU-TEL1   TO  P02-P021-RENTEL1
           MOVE    SPA-GMN21-RENRAKU-TEL2   TO  P02-P021-RENTEL2
           MOVE    SPA-GMN21-OFFICE-NAME    TO  P02-P021-OFFNAME
           MOVE    SPA-GMN21-OFFICE-POST    TO  P02-P021-OFFPOST
           MOVE    SPA-GMN21-OFFICE-ADRS    TO  P02-P021-OFFADRS
           MOVE    SPA-GMN21-OFFICE-BANTI   TO  P02-P021-OFFBANTI
           MOVE    SPA-GMN21-OFFICE-TEL     TO  P02-P021-OFFTEL
           MOVE    SPA-GMN21-KISEI-NAME     TO  P02-P021-KISNAME
           MOVE    SPA-GMN21-KISEI-POST     TO  P02-P021-KISPOST
           MOVE    SPA-GMN21-KISEI-ADRS     TO  P02-P021-KISADRS
           MOVE    SPA-GMN21-KISEI-BANTI    TO  P02-P021-KISBANTI
           MOVE    SPA-GMN21-KISEI-TEL      TO  P02-P021-KISTEL
           .
       2001-P021-SET-EXT.
           EXIT.
      *****************************************************************
      *    保険組合画面編集処理
      *****************************************************************
       2001-P027-SET-SEC         SECTION.
      *
           MOVE    "負担区分"      TO  P02-P027-TITLE1
           MOVE    "回−割合"      TO  P02-P027-TITLE2
           MOVE    "回−固定額"    TO  P02-P027-TITLE3
           MOVE    "回−上限額"    TO  P02-P027-TITLE4
           MOVE    "日−上限額"    TO  P02-P027-TITLE5
           MOVE    "日−上回数"    TO  P02-P027-TITLE6
           MOVE    "月−上限額"    TO  P02-P027-TITLE7
           MOVE    "月−上回数"    TO  P02-P027-TITLE8
           IF      SPA-GMN27-NYUGAIKBN     =   "2"
               MOVE    "  薬剤負担"    TO  P02-P027-TITLE9
           ELSE
               MOVE    "食事療養費"    TO  P02-P027-TITLE9
           END-IF
      *
           MOVE    ZERO                TO  P02-P027-HKNKUMI-COUNT
           IF      WRK-HKNKUMI-ROW     NOT =   ZERO
               MOVE    WRK-HKNKUMI-ROW     TO  P02-P027-HKNKUMI-ROW
           END-IF
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL       IDX     >   SPA-GMN27-MAX
                     OR        ( SPA-GMN27-NUM   (IDX)
                                            =   SPACE )
      *
               MOVE    SPA-GMN27-NUM   (IDX)
                                         TO  P02-P027-NUM(IDX)
               MOVE    SPA-GMN27-HKNSEIDO (IDX)
                                         TO  P02-P027-HKNSEIDO(IDX)
               MOVE    SPA-GMN27-KOHSEIDO1 (IDX)
                                         TO  P02-P027-KOHSEIDO1(IDX)
               MOVE    SPA-GMN27-KOHSEIDO2 (IDX)
                                         TO  P02-P027-KOHSEIDO2(IDX)
               MOVE    SPA-GMN27-KOHSEIDO3 (IDX)
                                         TO  P02-P027-KOHSEIDO3(IDX)
               MOVE    SPA-GMN27-KOHSEIDO4(IDX)
                                         TO  P02-P027-KOHSEIDO4(IDX)
               MOVE    SPA-GMN27-TEKSTYMD(IDX) 
                                         TO  P02-P027-TEKSTYMD(IDX)
               MOVE    SPA-GMN27-TEKEDYMD(IDX) 
                                         TO  P02-P027-TEKEDYMD(IDX)
      *        特定の受給者番号
               MOVE    SPA-GMN27-TJKYSNUM(IDX) 
                                         TO  P02-P027-TJKYSNUM(IDX)
               MOVE    IDX               TO  P02-P027-HKNKUMI-COUNT
      *
      *        非表示
               EVALUATE    SPA-GMN27-THYOJIKBN (IDX)
               WHEN    "1"
                   MOVE    "非表示"        TO  P02-P027-THYOJIKBN(IDX)
               WHEN    "2"
                   MOVE    "入非表示"      TO  P02-P027-THYOJIKBN(IDX)
               WHEN    "3"
                   MOVE    "外非表示"      TO  P02-P027-THYOJIKBN(IDX)
               END-EVALUATE
      *
               IF      SPA-GMN27-NUM (IDX)(1:4)
                                           =   SPA-NAI27-SELNUM
                   MOVE    "T"             TO  P02-P027-HKNKUMI-SELECT
                                                               (IDX)
      *            MOVE    IDX             TO  P02-P027-HKNKUMI-ROW
      *            MOVE    2               TO  P02-P027-HKNKUMI-ROWATTR
               ELSE
                   MOVE    "F"             TO  P02-P027-HKNKUMI-SELECT
                                                               (IDX)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN27-SELNUM      TO  P02-P027-SELNUM
           MOVE    SPA-GMN27-HKNNUM      TO  P02-P027-HKNNUM
           MOVE    SPA-GMN27-HKNSEIDON   TO  P02-P027-HKNSEIDON
           MOVE    SPA-GMN27-KOH1HKNNUM  TO  P02-P027-KOHNUM1
           MOVE    SPA-GMN27-KOHSEIDO1N  TO  P02-P027-KOHSEIDO1N
           MOVE    SPA-GMN27-KOH2HKNNUM  TO  P02-P027-KOHNUM2
           MOVE    SPA-GMN27-KOHSEIDO2N  TO  P02-P027-KOHSEIDO2N
           MOVE    SPA-GMN27-KOH3HKNNUM  TO  P02-P027-KOHNUM3
           MOVE    SPA-GMN27-KOHSEIDO3N  TO  P02-P027-KOHSEIDO3N
           MOVE    SPA-GMN27-KOH4HKNNUM  TO  P02-P027-KOHNUM4
           MOVE    SPA-GMN27-KOHSEIDO4N  TO  P02-P027-KOHSEIDO4N
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL     IDX       >   3
               MOVE    SPA-GMN27-MEI-FTNKBN     (IDX)
                                 TO  P02-P027-FTNKBN   (IDX)
               MOVE    SPA-GMN27-MEI-KFTNRATE   (IDX)
                                 TO  P02-P027-KAIWARI  (IDX)
               MOVE    SPA-GMN27-MEI-KKOTGAK    (IDX)
                                 TO  P02-P027-KAIKOTGAK(IDX)
               MOVE    SPA-GMN27-MEI-KJGNGAK    (IDX)
                                 TO  P02-P027-KAIJOGGAK(IDX)
               MOVE    SPA-GMN27-MEI-DJGNGAK    (IDX)
                                 TO  P02-P027-HIJOGGAK (IDX)
               MOVE    SPA-GMN27-MEI-DJGNCNT    (IDX)
                                 TO  P02-P027-HIJOGCNT (IDX)
               MOVE    SPA-GMN27-MEI-MNAIJGNGAK (IDX)
                                 TO  P02-P027-TUKJOGGAK(IDX)
               MOVE    SPA-GMN27-MEI-MJGNCNT    (IDX)
                                 TO  P02-P027-TUKJOGCNT(IDX)
               MOVE    SPA-GMN27-MEI-FTNKBN2    (IDX)
                                 TO  P02-P027-YAKKBN   (IDX)
      **        MOVE    SPA-GMN27-MEI-MGAIJGNGAK(IDX)
      **                          TO  P02-MGAIJGNGAK   (IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN27-TENTNK    TO  WRK-RENNUMZ
           MOVE    WRK-RENNUMZ         TO  P02-P027-TNSTANKA
           MOVE    SPA-GMN27-TAXKBN    TO  P02-P027-TAXKBN
      *
      *!! ver.4.7
           MOVE    WIDGET-INSENSITIVE  TO  P02-BHR1-STATE
           MOVE    SPACE               TO  P02-BHR1-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  P02-BHR2-STATE
           MOVE    SPACE               TO  P02-BHR2-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  P02-BHR3-STATE
           MOVE    SPACE               TO  P02-BHR3-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  P02-BHR4-STATE
           MOVE    SPACE               TO  P02-BHR4-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  P02-BHR5-STATE
           MOVE    SPACE               TO  P02-BHR5-LABEL
      *!!
      *    保険情報（全件テーブルの開始と終了位置より）
           IF      SPA-GMN27-HKN-PCNT  =   ZERO
               MOVE    1               TO  SPA-GMN27-HKN-PCNT
           END-IF
           MOVE    SPA-GMN27-HKN-PCNT  TO  IDX2
           MOVE    ZERO                TO  P02-P027-HKNLIST-COUNT
           PERFORM VARYING     IDX     FROM    1    BY  1
                   UNTIL     ( IDX         >   5    )
                   OR        ( SPA-GMN27-HKNRENNUM (IDX2 IDX)
                                          =   ZERO )
               MOVE    SPA-GMN27-HKNRENNUM (IDX2 IDX)
                                             TO  IDY
               MOVE    SPA-PTHKN-HKNNUM-MEI(IDY)
                                             TO  P02-THKNNUM   (IDX)
               MOVE    SPA-PTHKN-HKNJANUM(IDY)
                                             TO  P02-THKNJANUM (IDX)
               MOVE    SPA-PTHKN-HONKZKKBN-MEI (IDY)
                                             TO  P02-THONKAZKBN (IDX)
               MOVE    SPA-PTHKN-HOJOKBN-MEI(IDY)
                                             TO  P02-THOJOKBN (IDX)
               MOVE    SPA-PTHKN-CONTKBN-MEI(IDY)
                                             TO  P02-TCONTKBN (IDX)
               MOVE    SPA-PTHKN-KIGO  (IDY) TO  P02-TKIGO    (IDX)
               MOVE    SPA-PTHKN-NUM   (IDY) TO  P02-TNUM    (IDX)
               MOVE    SPA-PTHKN-EDABAN(IDY)
                                             TO  P02-TEDABAN (IDX)
      *
               MOVE    SPA-PTHKN-TEKSTYMDW (IDY)
                                             TO  P02-THTEKSTYMD (IDX)
               MOVE    SPA-PTHKN-TEKEDYMDW (IDY)
                                             TO  P02-THTEKEDYMD (IDX)
               MOVE    SPA-PTHKN-KAKUNINYMDW (IDY)
                                             TO  P02-THKAKUNINYMD (IDX)
               MOVE    SPA-PTHKN-SKKGETYMDW (IDY)
                                             TO  P02-TSKKGETYMD (IDX)
               MOVE    IDX                   TO  P02-P027-HKNLIST-COUNT
      *
               IF      SPA-PTHKN-KAKUNINYMD(IDY) (1:6)
                                       NOT =   SPA-SYSYMD (1:6)
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "F"         TO  P02-BHO1-VALUE
                       MOVE    "未"        TO  P02-BHO1-LABEL
                   WHEN    2
                       MOVE    "F"         TO  P02-BHO2-VALUE
                       MOVE    "未"        TO  P02-BHO2-LABEL
                   WHEN    3
                       MOVE    "F"         TO  P02-BHO3-VALUE
                       MOVE    "未"        TO  P02-BHO3-LABEL
                   WHEN    4      
                       MOVE    "F"         TO  P02-BHO4-VALUE
                       MOVE    "未"        TO  P02-BHO4-LABEL
                   WHEN    5
                       MOVE    "F"         TO  P02-BHO5-VALUE
                       MOVE    "未"        TO  P02-BHO5-LABEL
                   END-EVALUATE
               ELSE
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "T"         TO  P02-BHO1-VALUE
                       MOVE    "済"        TO  P02-BHO1-LABEL
                   WHEN    2
                       MOVE    "T"         TO  P02-BHO2-VALUE
                       MOVE    "済"        TO  P02-BHO2-LABEL
                   WHEN    3
                       MOVE    "T"         TO  P02-BHO3-VALUE
                       MOVE    "済"        TO  P02-BHO3-LABEL
                   WHEN    4
                       MOVE    "T"         TO  P02-BHO4-VALUE
                       MOVE    "済"        TO  P02-BHO4-LABEL
                   WHEN    5
                       MOVE    "T"         TO  P02-BHO5-VALUE
                       MOVE    "済"        TO  P02-BHO5-LABEL
                   END-EVALUATE
               END-IF
      *!!
               IF      (SPA-PTHKN-HKNNUM (IDY) =   "971"
                                               OR  "973" )
                  OR   (SPA-PTHKN-SINKIKBN(IDY)    =   "1" )
                   CONTINUE
               ELSE
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    WIDGET-NORMAL       TO  P02-BHR1-STATE
                       MOVE    "歴"                TO  P02-BHR1-LABEL
                   WHEN    2
                       MOVE    WIDGET-NORMAL       TO  P02-BHR2-STATE
                       MOVE    "歴"                TO  P02-BHR2-LABEL
                   WHEN    3
                       MOVE    WIDGET-NORMAL       TO  P02-BHR3-STATE
                       MOVE    "歴"                TO  P02-BHR3-LABEL
                   WHEN    4
                       MOVE    WIDGET-NORMAL       TO  P02-BHR4-STATE
                       MOVE    "歴"                TO  P02-BHR4-LABEL
                   WHEN    5
                       MOVE    WIDGET-NORMAL       TO  P02-BHR5-STATE
                       MOVE    "歴"                TO  P02-BHR5-LABEL
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    公費情報
           IF      SPA-GMN27-KOH-PCNT  =   ZERO
               MOVE    1               TO  SPA-GMN27-KOH-PCNT
           END-IF
           MOVE    ZERO                TO  P02-P027-KOHLIST-COUNT
           MOVE    SPA-GMN27-KOH-PCNT  TO  IDX2
           PERFORM VARYING     IDX     FROM    1    BY  1
                   UNTIL     ( IDX         >   5    )
                   OR        ( SPA-GMN27-KOHRENNUM (IDX2 IDX)
                                          =   ZERO )
               MOVE    SPA-GMN27-KOHRENNUM (IDX2 IDX)
                                             TO  IDY
               MOVE    SPA-PTKOH-KOHNUM-MEI(IDY)
                                             TO  P02-TKOHNUM   (IDX)
               MOVE    SPA-PTKOH-FTNJANUM(IDY)
                                             TO  P02-TFTNJANUM (IDX)
               MOVE    SPA-PTKOH-JKYSNUM(IDY)
                                             TO  P02-TJKYSNUM (IDX)
               MOVE    SPA-PTKOH-TEKSTYMDW (IDY)
                                             TO  P02-TKTEKSTYMD (IDX)
               MOVE    SPA-PTKOH-TEKEDYMDW (IDY)
                                             TO  P02-TKTEKEDYMD (IDX)
               MOVE    SPA-PTKOH-KAKUNINYMDW (IDY)
                                             TO  P02-TKKAKUNINYMD (IDX)
               MOVE    IDX                   TO  P02-P027-KOHLIST-COUNT
      *
               IF      SPA-PTKOH-KAKUNINYMD(IDY) (1:6)
                                       NOT =   SPA-SYSYMD (1:6)
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "F"          TO  P02-BKH1-VALUE
                       MOVE    "未"         TO  P02-BKH1-LABEL
                   WHEN    2
                       MOVE    "F"          TO  P02-BKH2-VALUE
                       MOVE    "未"         TO  P02-BKH2-LABEL
                   WHEN    3
                       MOVE    "F"          TO  P02-BKH3-VALUE
                       MOVE    "未"         TO  P02-BKH3-LABEL
                   WHEN    4
                       MOVE    "F"          TO  P02-BKH4-VALUE
                       MOVE    "未"         TO  P02-BKH4-LABEL
                   WHEN    5
                       MOVE    "F"          TO  P02-BKH5-VALUE
                       MOVE    "未"         TO  P02-BKH5-LABEL
                   END-EVALUATE
               ELSE
                   EVALUATE    IDX
                   WHEN    1
                       MOVE    "T"          TO  P02-BKH1-VALUE
                       MOVE    "済"         TO  P02-BKH1-LABEL
                   WHEN    2
                       MOVE    "T"          TO  P02-BKH2-VALUE
                       MOVE    "済"         TO  P02-BKH2-LABEL
                   WHEN    3
                       MOVE    "T"          TO  P02-BKH3-VALUE
                       MOVE    "済"         TO  P02-BKH3-LABEL
                   WHEN    4
                       MOVE    "T"          TO  P02-BKH4-VALUE
                       MOVE    "済"         TO  P02-BKH4-LABEL
                   WHEN    5
                       MOVE    "T"          TO  P02-BKH5-VALUE
                       MOVE    "済"         TO  P02-BKH5-LABEL
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
      *    入院・外来
           IF    SPA-GMN27-NYUGAIKBN       =   "1"
               MOVE    "T"                 TO  P02-P027-NYUGAIKBN-VALUE
               MOVE    "入院"              TO  P02-P027-NYUGAIKBN-LABEL
           ELSE
               MOVE    "F"                 TO  P02-P027-NYUGAIKBN-VALUE
               MOVE    "外来"              TO  P02-P027-NYUGAIKBN-LABEL
           END-IF
      *
      *    非表示
           IF      SPA-GMN27-SELNUM    >   ZERO
               MOVE    SPA-GMN27-HYOJIKBN-G    TO  P02-P027-HYOJIKBN
               PERFORM VARYING     IDX     FROM    1   BY  1
                        UNTIL    ( IDX     >   SPA-GMN27-HYOJIKBN-MAX)
                   MOVE    SPA-GMN27-HYOJIKBNLST (IDX)
                                       TO  P02-P027-HYOJIKBN-ITEM (IDX)
               END-PERFORM
               MOVE    SPA-GMN27-HYOJIKBN-MAX
                                           TO  P02-P027-HYOJIKBN-COUNT
           END-IF
      *R01.6
      *    難病等＋生保保険
           IF      SPA-GMN27-COMBKBN   =   "1"
               MOVE    "T"             TO  P02-P027-COMBKBN
           ELSE
               MOVE    "F"             TO  P02-P027-COMBKBN
           END-IF
      *
           .
       2001-P027-SET-EXT.
           EXIT.
      *****************************************************************
      *    紹介画面編集処理
      *****************************************************************
       2001-P023-SET-SEC         SECTION.
           .
       2001-P023-SET-EXT.
           EXIT.
      *****************************************************************
      *    特記事項画面編集処理
      *****************************************************************
       2001-P026-SET-SEC         SECTION.
      *
           MOVE    WRK-P026-ROW1       TO  P02-P026-TOK-ROW
           MOVE    WRK-P026-ROW2       TO  P02-P026-HDLST-ROW
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-TMAX )
               MOVE    IDX                     TO  WRK-RENNUMZ3
               MOVE    WRK-RENNUMZ3            TO  P02-P026-TNUM (IDX)
               MOVE    SPA-GMN26-TNYUGI (IDX)  TO  P02-P026-TNYUGAI(IDX)
               MOVE    SPA-GMN26-TCD    (IDX)  TO  P02-P026-TCD   (IDX)
               MOVE    SPA-GMN26-TNAIYO (IDX)  TO  P02-P026-TNAIYO(IDX)
               MOVE    SPA-GMN26-TSTYM  (IDX)  TO  P02-P026-TSTYM (IDX)
               MOVE    SPA-GMN26-TEDYM  (IDX)  TO  P02-P026-TEDYM (IDX)
               IF      IDX             =   SPA-GMN26-SELNUM
                   MOVE    "T"         TO  P02-P026-TOK-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P026-TOK-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN26-TMAX      TO  P02-P026-TOK-COUNT
      *
           MOVE    SPA-GMN26-SELNUM    TO  P02-P026-SELNUM
      *
           MOVE    SPA-GMN26-NYUGAI-G  TO  P02-P026-NYUGAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-NYUMAX )
               MOVE    SPA-GMN26-NYUGAI-LST (IDX)
                                       TO  P02-P026-NYUGAI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN26-NYUMAX    TO  P02-P026-NYUGAI-COUNT
      *
           MOVE    SPA-GMN26-CD        TO  P02-P026-CD
           MOVE    SPA-GMN26-NAIYO     TO  P02-P026-NAIYO
           MOVE    SPA-GMN26-STYM      TO  P02-P026-STYM
           MOVE    SPA-GMN26-EDYM      TO  P02-P026-EDYM
      *
           MOVE    SPA-GMN26-TOKCD-G   TO  P02-P026-TOKCD
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-TOKMAX )
               MOVE    SPA-GMN26-TOKCD-LST (IDX)
                                       TO  P02-P026-TOKCD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN26-TOKMAX    TO  P02-P026-TOKCD-COUNT
      *
      *    レセプト分割
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-HDLSTMAX )
                        OR   ( IDX     >   50    )
               MOVE    SPA-GMN26-THDNUM (IDX)  TO  WRK-RENNUMZ3
               MOVE    WRK-RENNUMZ3            TO  P02-P026-THDNUM (IDX)
               MOVE    SPA-GMN26-THDSRYYM (IDX)
                                           TO  P02-P026-THDSRYYM (IDX)
               MOVE    SPA-GMN26-THDDAY (IDX)
                                           TO  P02-P026-THDDAY   (IDX)
               MOVE    SPA-GMN26-THDNYUGAI (IDX)
                                           TO  P02-P026-TDNYUGAI(IDX)
               MOVE    SPA-GMN26-THDRESEKBN(IDX)
                                           TO  P02-P026-TDRESEKBN(IDX)
               IF      IDX             =   SPA-GMN26-HDSELNUM
                   MOVE    "T"         TO  P02-P026-HDLST-SELECT (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P026-HDLST-SELECT (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN26-HDLSTMAX  TO  P02-P026-HDLST-COUNT
      *
           MOVE    SPA-GMN26-HDSELNUM  TO  P02-P026-HDSELNUM
           MOVE    SPA-GMN26-HDSRYYM   TO  P02-P026-HDSRYYM
           MOVE    SPA-GMN26-HDDAY     TO  P02-P026-HDDAY
      *
           MOVE    SPA-GMN26-HDNYUGAI-G    TO  P02-P026-HD-NYUGAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-HDNYUMAX )
               MOVE    SPA-GMN26-HDNYUGAI-LST (IDX)
                                       TO  P02-P026-HD-NYUGAI-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GMN26-HDNYUMAX  TO  P02-P026-HD-NYUGAI-COUNT
      *
           MOVE    SPA-GMN26-HDRESEKBN-G   TO  P02-P026-HD-RESEKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN26-HDRESEMAX )
               MOVE    SPA-GMN26-HDRESEKBN-LST (IDX)
                                       TO  P02-P026-HD-RESEKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN26-HDRESEMAX TO  P02-P026-HD-RESEKBN-COUNT
      *
      *    振分日以前
           MOVE    SPA-GMN26-HDMIDASI (1)  TO  P02-P026-HDMIDAS1
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   20   )
                       OR    ( SPA-GMN26-THDHKNCOMBI (1 IDX) =   SPACE)
               MOVE    SPA-GMN26-THDHKNCOMBI (1 IDX)
                                           TO  P02-P026-THKNNUM1(IDX)
               MOVE    SPA-GMN26-THDHKNNAME (1 IDX)
                                           TO  P02-P026-THKNMEI1(IDX)
               IF      SPA-GMN26-THDHKNDELFLG (1 IDX)  =   "1"
                   MOVE    "◎"            TO  P02-P026-TDELKBN1 (IDX)
               END-IF
               MOVE    IDX                 TO  P02-P026-HDHKNLST1-COUNT
           END-PERFORM
      *    振分日以降
           MOVE    SPA-GMN26-HDMIDASI (2)  TO  P02-P026-HDMIDAS2
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   20   )
                       OR    ( SPA-GMN26-THDHKNCOMBI (2 IDX) =   SPACE)
               MOVE    SPA-GMN26-THDHKNCOMBI (2 IDX)
                                           TO  P02-P026-THKNNUM2(IDX)
               MOVE    SPA-GMN26-THDHKNNAME (2 IDX)
                                           TO  P02-P026-THKNMEI2(IDX)
               IF      SPA-GMN26-THDHKNDELFLG (2 IDX)  =   "1"
                   MOVE    "◎"            TO  P02-P026-TDELKBN2 (IDX)
               END-IF
               MOVE    IDX                 TO  P02-P026-HDHKNLST2-COUNT
           END-PERFORM
           MOVE    SPA-GMN26-DELSEL1       TO  P02-P026-DELSEL1
           MOVE    SPA-GMN26-DELSEL2       TO  P02-P026-DELSEL2
           .
       2001-P026-SET-EXT.
           EXIT.
      *****************************************************************
      *    所得情報画面編集処理
      *****************************************************************
       2001-P024-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-TNK-MAX )
                    OR       ( IDY     >=  20          )
             IF  SPA-GMN24-TNKDELKBN(IDX)  =   SPACE
                 ADD     1                 TO  IDY
                 MOVE    IDY               TO  WRK-RENNUMZ3
                 MOVE    WRK-RENNUMZ3      TO  P02-P024-TNKLNUM  (IDY)
                 MOVE    SPA-GMN24-TNKDPSTYMDW (IDX)
                                           TO  P02-P024-TNKLSTYMD(IDY)
                 MOVE    SPA-GMN24-TNKDPEDYMDW (IDX)
                                           TO  P02-P024-TNKLEDYMD(IDY)
                 MOVE    SPA-GMN24-TFUKUSHI-MEI (IDX)
                                           TO  P02-P024-TNKLFUKUSHI
                                                           (IDY)
                 MOVE    SPA-GMN24-TNKLNINYMD (IDX)
                                           TO  P02-P024-TNKLNINYMD(IDY)
                 MOVE    SPA-GMN24-TNKLHKNKBN (IDX)
                                           TO  P02-P024-TNKLHKNKBN(IDY)
      *H29.6
      *          境界層該当区分
                 MOVE    SPA-GMN24-TNKLKYOUKAI (IDX)
                                           TO  P02-P024-TNKLKYOUKAI(IDY)
      *
                 IF      IDX           =   SPA-NAI24-TNKSELNUM
                     MOVE    "T"           TO  P02-P024-TNKLIST-SELECT
                                                                 (IDY)
                 ELSE
                     MOVE    "F"           TO  P02-P024-TNKLIST-SELECT
                                                                 (IDY)
                 END-IF
             END-IF
           END-PERFORM
           MOVE    IDY                 TO  P02-P024-TNKLIST-COUNT
           MOVE    SPA-GMN24-TNKSELNUM TO  P02-P024-TNKSELNUM
           MOVE    SPA-GMN24-TNKSTYMDW TO  P02-P024-TNKSTYMD
           MOVE    SPA-GMN24-TNKEDYMDW TO  P02-P024-TNKEDYMD
      *
           MOVE    SPA-GMN24-TNKNINSTYMD   TO  P02-P024-TNKNINSTYMD
           MOVE    SPA-GMN24-TNKTEKKBN-G   TO  P02-P024-TNKTEKK
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-GMN24-TNKTEKKBN-MAX
               MOVE    SPA-GMN24-TNKTEKKBN-LST (IDX)
                                           TO  P02-P024-TNKTEKK-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-GMN24-TNKTEKKBN-MAX   TO  P02-P024-TNKTEKK-COUNT
      *
      *    境界層該当区分
           MOVE    SPA-GMN24-TNKKYOUKAI-G  TO  P02-P024-TNKKYOUKAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-GMN24-TNKKYOUKAI-MAX
               MOVE    SPA-GMN24-TNKKYOUKAI-LST (IDX)
                                           TO  P02-P024-TNKKYOUKAI-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-GMN24-TNKKYOUKAI-MAX
                                       TO  P02-P024-TNKKYOUKAI-COUNT
      *
           MOVE    SPA-GMN24-FUKUSHI-G TO  P02-P024-FUKUSHI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-GMN24-FUKUSHI-MAX
               MOVE    SPA-GMN24-FUKUSHI-LST (IDX)
                                           TO  P02-P024-FUKUSHI-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-GMN24-FUKUSHI-MAX   TO  P02-P024-FUKUSHI-COUNT
      *
           MOVE    SPA-GMN24-JKYSNUM   TO  P02-P024-JKYSNUM
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-TSY-MAX )
                    OR       ( IDY     >=  20          )
             IF  SPA-GMN24-TSYDELKBN(IDX)  =   SPACE
                 ADD     1                 TO  IDY
                 MOVE    IDY               TO  WRK-RENNUMZ3
                 MOVE    WRK-RENNUMZ3      TO  P02-P024-TSYLNUM  (IDY)
                 MOVE    SPA-GMN24-TSYDPSTYMDW (IDX)
                                           TO  P02-P024-TSYLSTYMD(IDY)
                 MOVE    SPA-GMN24-TSYDPEDYMDW (IDX)
                                           TO  P02-P024-TSYLEDYMD(IDY)
                 MOVE    SPA-GMN24-TSYLSKJGNSTYMDW (IDX)
                                           TO  P02-P024-TSYLSKJGNSTYMD
                                                           (IDY)
                 MOVE    SPA-GMN24-TSYLNINYMD (IDX)
                                           TO  P02-P024-TSYLNINYMD(IDY)
                 MOVE    SPA-GMN24-TSYLHKNKBN (IDX)
                                           TO  P02-P024-TSYLHKNKBN(IDY)
      *          境界層該当区分
                 MOVE    SPA-GMN24-TSYLKYOUKAI (IDX)
                                           TO  P02-P024-TSYLKYOUKAI(IDY)
      *
                 IF      IDX           =   SPA-NAI24-TSYSELNUM
                     MOVE    "T"           TO  P02-P024-TSYLIST-SELECT
                                                                 (IDY)
                 ELSE
                     MOVE    "F"           TO  P02-P024-TSYLIST-SELECT
                                                                 (IDY)
                 END-IF
             END-IF
           END-PERFORM
           MOVE    IDY                 TO  P02-P024-TSYLIST-COUNT
           MOVE    SPA-GMN24-TSYSELNUM TO  P02-P024-TSRSELNUM
           MOVE    SPA-GMN24-TSYSTYMDW TO  P02-P024-TSYSTYMD
           MOVE    SPA-GMN24-TSYEDYMDW TO  P02-P024-TSYEDYMD
           MOVE    SPA-GMN24-TSYSKJGNSTYMDW
                                       TO  P02-P024-TSYSKJGNSTYMD
      *
           MOVE    SPA-GMN24-TSYNINSTYMD   TO  P02-P024-TSYNINSTYMD
           MOVE    SPA-GMN24-TSYTEKKBN-G   TO  P02-P024-TSYTEKK
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-GMN24-TSYTEKKBN-MAX
               MOVE    SPA-GMN24-TSYTEKKBN-LST (IDX)
                                           TO  P02-P024-TSYTEKK-ITEM
                                                                   (IDX)
           END-PERFORM
           MOVE    SPA-GMN24-TSYTEKKBN-MAX   TO  P02-P024-TSYTEKK-COUNT
      *
      *    境界層該当区分
           MOVE    SPA-GMN24-TSYKYOUKAI-G   TO  P02-P024-TSYKYOUKAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-GMN24-TSYKYOUKAI-MAX
               MOVE    SPA-GMN24-TSYKYOUKAI-LST (IDX)
                                           TO  P02-P024-TSYKYOUKAI-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-GMN24-TSYKYOUKAI-MAX
                                       TO  P02-P024-TSYKYOUKAI-COUNT
      *
      *    公費負担
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL     (IDX     >   SPA-FTN1-MAX )
               MOVE    ZERO                TO  SPA-GMN24-TGMNNUM (IDX)
               IF      SPA-NAI24-FTNDELKBN (IDX)   =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    IDY                 TO  WRK-RENNUMZ3
                   MOVE    WRK-RENNUMZ3        TO  P02-P024-TFTNNUM(IDY)
                   MOVE    SPA-GMN24-TFTNKOHMEI (IDX)
                                               TO  P02-P024-TFTNKOH(IDY)
                   MOVE    SPA-GMN24-TKOHSTYMD (IDX)
                                               TO  P02-P024-TKOHSTYMD
                                                                  (IDY)
                   MOVE    SPA-GMN24-TKOHEDYMD (IDX)
                                               TO  P02-P024-TKOHEDYMD
                                                                  (IDY)
                   MOVE    IDY                 TO  SPA-GMN24-TGMNNUM
                                                                  (IDX)
                   IF      IDY                 =   SPA-GMN24-FTNSELNUM
                       MOVE    "T"         TO  P02-P024-HFTNLST-SELECT
                                                                 (IDY)
                       MOVE    IDY         TO  P02-P024-HFTNLST-ROW
                   ELSE
                       MOVE    "F"         TO  P02-P024-HFTNLST-SELECT
                                                                 (IDY)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  P02-P024-HFTNLST-COUNT
      *
           MOVE    ZERO                TO  P02-P024-HFTNLST2-ROW
           IF      WRK-HFTNLST2-ROW    NOT =   ZERO
               MOVE    WRK-HFTNLST2-ROW    TO  P02-P024-HFTNLST2-ROW
           END-IF
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      IDX     >   SPA-FTN2-MAX
               MOVE    ZERO                TO  SPA-GMN24-TGMNNUM2(IDX)
               IF      SPA-NAI24-FTNDELKBN2 (IDX)  =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    IDY                 TO  WRK-RENNUMZ3
                   MOVE    WRK-RENNUMZ3        TO  P02-P024-TFTNNUM2
                                                                  (IDY)
                   MOVE    SPA-GMN24-TFTNSTYMD (IDX)
                                               TO  P02-P024-TFTNSTYMD
                                                                  (IDY)
                   MOVE    SPA-GMN24-TFTNEDYMD (IDX)
                                               TO  P02-P024-TFTNEDYMD
                                                                  (IDY)
                   MOVE    SPA-GMN24-TGAIJGNGAK (IDX)
                                               TO  WRK-Z7
                   MOVE    WRK-Z7              TO  P02-P024-TGAIJGNGAK
                                                                  (IDY)
      *H30.11
      *            肝がん治療はゼロ編集なし
                   IF     (SPA-NAI24-FTNSELNUM     >   ZERO )
                      AND (SPA-NAI24-KOHNUM (SPA-NAI24-FTNSELNUM)
                                                   =   "092")
                       MOVE    SPA-GMN24-TGAIJGNGAK (IDX)
                                               TO  WRK-Z72
                       MOVE    WRK-Z72         TO  P02-P024-TGAIJGNGAK
                                                                  (IDY)
                   END-IF
      *
                   MOVE    SPA-GMN24-TNYUJGNGAK (IDX)
                                               TO  WRK-Z7
                   MOVE    WRK-Z7              TO  P02-P024-TNYUJGNGAK
                                                                  (IDY)
                   MOVE    IDY                 TO  SPA-GMN24-TGMNNUM2
                                                                  (IDX)
      *            患者公費負担登録有無
                   IF      SPA-NAI24-TFTNETCKBN (IDX)
                                               =   SPACE
                                               OR  "0"
                       CONTINUE
                   ELSE
                       MOVE    "☆"            TO  P02-P024-TFTNETC(IDY)
                   END-IF
      *
                   IF      IDY                 =   SPA-GMN24-FTNSELNUM2
                       MOVE    "T"         TO  P02-P024-HFTNLST2-SELECT
                                                                 (IDY)
                       MOVE    IDY         TO  P02-P024-HFTNLST2-ROW
                   ELSE
                       MOVE    "F"         TO  P02-P024-HFTNLST2-SELECT
                                                                 (IDY)
                   END-IF
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  P02-P024-HFTNLST2-COUNT
      *    追加行へ
           IF      SPA-NAI24-INIT2     =   2
               MOVE    IDY             TO  P02-P024-HFTNLST2-ROW
               MOVE    ZERO            TO  SPA-NAI24-INIT2
           END-IF
      *
           MOVE    SPA-GMN24-FTNSELNUM TO  P02-P024-FTNSELNUM
      *
           MOVE    SPA-GMN24-FTNSELNUM2
                                       TO  P02-P024-FTNSELNUM2
      *
           IF      SPA-GMN24-FTNMSG1   =   SPACE
               MOVE    "外来上限額"        TO  P02-FTNMSG1
               MOVE    "入院上限額"        TO  P02-FTNMSG2
           ELSE
               MOVE    SPA-GMN24-FTNMSG1   TO  P02-FTNMSG1
               MOVE    SPA-GMN24-FTNMSG2   TO  P02-FTNMSG2
           END-IF
      *
           IF      SPA-GMN24-FTNMSG1   =   SPACE
               MOVE    "外来上限額"        TO  P02-P024-LABL1
               MOVE    "入院上限額"        TO  P02-P024-LABL2
           ELSE
               MOVE    SPA-GMN24-FTNMSG1   TO  P02-P024-LABL1
               MOVE    SPA-GMN24-FTNMSG2   TO  P02-P024-LABL2
           END-IF
      *
           MOVE    SPA-GMN24-FTNSTYMD  TO  P02-P024-FTNTEKSTYMD
           MOVE    SPA-GMN24-FTNEDYMD  TO  P02-P024-FTNTEKEDYMD
           MOVE    SPA-GMN24-GAIJGNGAK TO  P02-P024-GAIJGNGAK
           MOVE    SPA-GMN24-NYUJGNGAK TO  P02-P024-NYUJGNGAK
      *
           IF      SPA-NAI24-FTNETCKBN =   SPACE
                                       OR  "0"
               MOVE    WIDGET-NORMAL       TO  P02-P024-NYUJGNGAK-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  P02-P024-NYUJGNGAK-STATE
           END-IF
      *
           .
       2001-P024-SET-EXT.
           EXIT.
      *****************************************************************
      *    各種履歴画面編集処理
      *****************************************************************
       2001-P025-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  P02-P025-KYUSEILIST-COUNT
           MOVE    ZERO                TO  IDY
           PERFORM   VARYING     IDX   FROM    1   BY  1
                     UNTIL      (IDX       >   10)
                     OR         (IDX       >   SPA-NAI25-KYU-MAX)
             IF  SPA-GMN25-KYU-DELKBN (IDX)    =   SPACE
               ADD     1               TO  IDY 
               MOVE    IDY             TO  WRK-RENNUMZ3
               MOVE    WRK-RENNUMZ3    TO  P02-P025-NUM1    (IDY)
               MOVE    SPA-GMN25-KYU-CHGYMDW (IDX)
                                       TO  P02-P025-HENKOYMD(IDY)
               MOVE    SPA-GMN25-KYU-KANANAME(IDX)
                                       TO  P02-P025-KANANAME(IDY)
               MOVE    SPA-GMN25-KYU-NAME(IDX)  TO  P02-P025-NAME(IDY)
               MOVE    SPA-GMN25-KYU-NICKNAME  (IDX)
                                       TO  P02-P025-NICKNAME (IDY)
               MOVE    IDY             TO  P02-P025-KYUSEILIST-COUNT
               IF      IDY             =   SPA-NAI25-SELNUM
                   MOVE    "T"         TO  P02-P025-KYUSEILIST-SELECT
                                                              (IDY)
               ELSE
                   MOVE    "F"         TO  P02-P025-KYUSEILIST-SELECT
                                                              (IDY)
               END-IF
             END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN25-TKYU-CHGYMDW  TO  P02-P025-THENYMD
           MOVE    SPA-GMN25-TKYU-KANANAME TO  P02-P025-TKANANAME
           MOVE    SPA-GMN25-TKYU-NAME     TO  P02-P025-TNAME
           MOVE    SPA-GMN25-TKYU-NICKNAME TO  P02-P025-TNICKNAME
      *
           MOVE    SPA-NAI25-SRY-MAX   TO  P02-P025-JYUSINLIST1-COUNT
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL      (IDX         >   30)
                     OR         (IDX         >   SPA-NAI25-SRY-MAX)
               MOVE    IDX             TO  WRK-RENNUMZ3
               MOVE    WRK-RENNUMZ3     TO  P02-P025-NUM2    (IDX)
               MOVE    SPA-GMN25-SRY-SRYKA (IDX)
                                       TO  P02-P025-SRYKA1    (IDX)
               MOVE    SPA-GMN25-SRY-SYOSINYMD(IDX)
                                       TO  P02-P025-SYOSINYMD(IDX)
               MOVE    SPA-GMN25-SRY-LASTYMD(IDX)
                                       TO  P02-P025-LASTYMD  (IDX)
      *
               IF      IDX             =   SPA-NAI25-SELNUM2
                   MOVE    "T"         TO  P02-P025-JYUSINLIST1-SELECT
                                                              (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P025-JYUSINLIST1-SELECT
                                                              (IDX)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPA-GMN25-SELNUM    TO  P02-P025-SELNUM1
      *
           MOVE    SPA-NAI25-JYU-MAX   TO  P02-P025-JYUSINLIST2-COUNT
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL     ( IDX     >   50 )
                     OR        ( IDX     >   SPA-NAI25-JYU-MAX )
               MOVE    SPA-GMN25-JYU-SRYYMD (IDX)
                                       TO  P02-P025-SRYYMD  (IDX)
               MOVE    SPA-GMN25-JYU-SRYKA  (IDX)
                                       TO  P02-P025-SRYKA2  (IDX)
               MOVE    SPA-GMN25-JYU-SRYKBN(IDX)
                                       TO  P02-P025-SRYNAIYO(IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN25-SELNUM2   TO  P02-P025-SELNUM2
      *
           .
       2001-P025-SET-EXT.
           EXIT.
      *****************************************************************
      *    個別設定画面編集処理
      *****************************************************************
       2001-P028-SET-SEC         SECTION.
      *
      *    地域連携ＩＤ
           MOVE    SPA-GMN28-PATIENTID1     TO  P02-P028-PATIENTID1
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-AGREEMENT-MAX )
               MOVE    SPA-GMN28-AGREEMENT-LST (IDX)
                                       TO  P02-P028-AGREEMENT-ITEM
                                                          (IDX)
           END-PERFORM
           MOVE    SPA-GMN28-AGREEMENT-MAX
                                       TO  P02-P028-AGREEMENT-COUNT
           MOVE    SPA-GMN28-AGREEMENT-G
                                       TO  P02-P028-AGREEMENT
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-QRKBN-MAX )
               MOVE    SPA-GMN28-MNOTE-QRKBN-LST (IDX)
                                       TO  P02-P028-MNOTE-QRKBN-ITEM
                                                          (IDX)
           END-PERFORM
           MOVE    SPA-GMN28-QRKBN-MAX TO  P02-P028-MNOTE-QRKBN-COUNT
           MOVE    SPA-GMN28-MNOTE-QRKBN-G
                                       TO  P02-P028-MNOTE-QRKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-CSV-MAX )
               MOVE    SPA-GMN28-MNOTE-CSV-LST (IDX)
                                       TO  P02-P028-MNOTE-CSV-ITEM
                                                          (IDX)
           END-PERFORM
           MOVE    SPA-GMN28-CSV-MAX   TO  P02-P028-MNOTE-CSV-COUNT
           MOVE    SPA-GMN28-MNOTE-CSV-G
                                       TO  P02-P028-MNOTE-CSV
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-JMAMRVER-MAX )
               MOVE    SPA-GMN28-JMAMRVER-LST (IDX)
                                       TO  P02-P028-JMAMRVER-ITEM
                                                          (IDX)
           END-PERFORM
           MOVE    SPA-GMN28-JMAMRVER-MAX  TO  P02-P028-JMAMRVER-COUNT
           MOVE    SPA-GMN28-JMAMRVER-G
                                       TO  P02-P028-JMAMRVER
      *
      *    介護保険情報
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL     ( IDX     >   50 )
                     OR        ( IDX     >   SPA-GMN28-CRHKN-MAX )
               MOVE    SPA-GMN28-TCHNUM (IDX)   TO WRK-RENNUMZ2
               MOVE    WRK-RENNUMZ2        TO  P02-P028-TCHNUM (IDX)
               MOVE    SPA-GMN28-TCRHKNJANUM (IDX)
                                           TO  P02-P028-TCRHKNJANUM
                                                               (IDX)
               MOVE    SPA-GMN28-TCRHIHKNJANUM (IDX)
                                           TO  P02-P028-TCRHIHKNJANUM
                                                               (IDX)
               MOVE    SPA-GMN28-TCRHSTRYMD (IDX)
                                           TO  P02-P028-TCRHSTRYMD
                                                               (IDX)
               MOVE    SPA-GMN28-TCRHENDYMD (IDX)
                                           TO  P02-P028-TCRHENDYMD
                                                               (IDX)
      *
               IF      IDX             =   SPA-GMN28-CRHSELNUM
                   MOVE    "T"         TO  P02-P028-CRHKNLST-SELECT
                                                              (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P028-CRHKNLST-SELECT
                                                              (IDX)
               END-IF
      *ver.4.8 のみ
      *        今回新規
      *        IF      SPA-NAI28-TCHKNSKBN (IDX) =   SPACE
      *            MOVE    CL-GREEN    TO  P02-P028-CRHKNLST-BGCOLOR
      *                                                       (IDX)
      ***      END-IF
           END-PERFORM
           MOVE    SPA-GMN28-CRHKN-MAX     TO  P02-P028-CRHKNLST-COUNT
      *
           MOVE    SPA-GMN28-CRHSELNUM     TO  P02-P028-CRHSELNUM
      *
           MOVE    SPA-GMN28-CR-HKNJANUM   TO  P02-P028-CRHKNJANUM
           MOVE    SPA-GMN28-CR-HIHKNJANUM TO  P02-P028-CRHIHKNJANUM
           MOVE    SPA-GMN28-CHTEKSTYMD    TO  P02-P028-CHTEKSTYMD
           MOVE    SPA-GMN28-CHTEKEDYMD    TO  P02-P028-CHTEKEDYMD
      *    介護保険者情報
      *!!! MOVE    SPA-P028-CAREHKNMEI     TO  P02-P028-CAREHKNMEI
           MOVE    SPA-P028-CRHKNJANAME    TO  P02-P028-CAREHKNJAMEI
           MOVE    SPA-P028-CRHKNJAADRS    TO  P02-P028-CAREHKNJAADRS
           MOVE    SPA-P028-CRHKNJAADRS2   TO  P02-P028-CAREHKNJAADRS2
           MOVE    SPA-P028-CRHKNJATEL     TO  P02-P028-CAREHKNJATEL
      *    介護認定者情報
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL     ( IDX     >   50 )
                     OR        ( IDX     >   SPA-GMN28-CRNIN-MAX )
               MOVE    SPA-GMN28-TCRNUM (IDX)   TO WRK-RENNUMZ3
               MOVE    WRK-RENNUMZ3        TO  P02-P028-TCRNUM (IDX)
               MOVE    SPA-GMN28-TCAREJOTAI (IDX)
                                           TO  P02-P028-TCAREJOTAI
                                                               (IDX)
               MOVE    SPA-GMN28-TCRNINYMD (IDX)
                                           TO  P02-P028-TCRNINYMD
                                                               (IDX)
               MOVE    SPA-GMN28-TCRSTRYMD (IDX)
                                           TO  P02-P028-TCRTEKSTYMD
                                                               (IDX)
               MOVE    SPA-GMN28-TCRENDYMD (IDX)
                                           TO  P02-P028-TCRTEKEDYMD
                                                               (IDX)
      *
               IF      IDX             =   SPA-GMN28-CRSELNUM
                   MOVE    "T"         TO  P02-P028-CARELST-SELECT
                                                              (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P028-CARELST-SELECT
                                                              (IDX)
               END-IF
      *ver.4.8 のみ
      *        今回新規
      *        IF      SPA-NAI28-TINSKBN (IDX) =   SPACE
      *            MOVE    CL-GREEN    TO  P02-P028-CARELST-BGCOLOR
      *                                                       (IDX)
      *        END-IF
      ***      今回修正
      *        IF      SPA-NAI28-TINSKBN (IDX) =   "2"
      *            MOVE    CL-PINK     TO  P02-P028-CARELST-BGCOLOR
      *                                                       (IDX)
      ****     END-IF
           END-PERFORM
           MOVE    SPA-GMN28-CRNIN-MAX     TO  P02-P028-CARELST-COUNT
      *
           MOVE    SPA-GMN28-CRSELNUM      TO  P02-P028-CRSELNUM
      *
           MOVE    SPA-GMN28-CRNINYMD      TO  P02-P028-CRNINYMD
           MOVE    SPA-GMN28-CRTEKSTYMD    TO  P02-P028-CRTEKSTYMD
           MOVE    SPA-GMN28-CRTEKEDYMD    TO  P02-P028-CRTEKEDYMD
      *
           MOVE    SPA-GMN28-CAREJOTAICD-G TO  P02-P028-CAREJOTAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL    ( IDX     >   SPA-GMN28-CAREJOTAI-MAX )
               MOVE    SPA-GMN28-CAREJOTAI-LST (IDX)
                                       TO  P02-P028-CAREJOTAI-ITEM
                                                          (IDX)
           END-PERFORM
           MOVE    SPA-GMN28-CAREJOTAI-MAX  TO  P02-P028-CAREJOTAI-COUNT
      *
      *H26.4
      *    地域包括診療対象疾病
           IF      SPA-GMN28-CHKHKTBYO(1)  =   "1"
               MOVE    "T"                 TO  P02-P028-CHKHKTBYO1
           ELSE
               MOVE    "F"                 TO  P02-P028-CHKHKTBYO1
           END-IF
           IF      SPA-GMN28-CHKHKTBYO(2)  =   "1"
               MOVE    "T"                 TO  P02-P028-CHKHKTBYO2
           ELSE
               MOVE    "F"                 TO  P02-P028-CHKHKTBYO2
           END-IF
           IF      SPA-GMN28-CHKHKTBYO(3)  =   "1"
               MOVE    "T"                 TO  P02-P028-CHKHKTBYO3
           ELSE
               MOVE    "F"                 TO  P02-P028-CHKHKTBYO3
           END-IF
           IF      SPA-GMN28-CHKHKTBYO(4)  =   "1"
               MOVE    "T"                 TO  P02-P028-CHKHKTBYO4
           ELSE
               MOVE    "F"                 TO  P02-P028-CHKHKTBYO4
           END-IF
      *H28.4
      *    認知症地域包括診療対象
           IF      SPA-GMN28-NNCHIKBN  =   "1"
               MOVE    "T"                 TO  P02-P028-NNCHIKBN
           ELSE
               MOVE    "F"                 TO  P02-P028-NNCHIKBN
           END-IF
      *    小児かかりつけ診療対象
           IF      SPA-GMN28-SYOUNIKBN  =   "1"
               MOVE    "T"                 TO  P02-P028-SYOUNIKBN
           ELSE
               MOVE    "F"                 TO  P02-P028-SYOUNIKBN
           END-IF
      *H28.12
      *    患者個人番号情報
           PERFORM   VARYING     IDX     FROM   1   BY  1
                     UNTIL     ( IDX     >   SPA-GMN28-MYID-MAX )
               MOVE    SPA-GMN28-TMYNUM (IDX)  TO WRK-RENNUMZ2
               MOVE    WRK-RENNUMZ2        TO  P02-P028-TMYNUM (IDX)
               MOVE    SPA-GMN28-TMYIDKEY (IDX)
                                           TO  P02-P028-TMYIDKEY
                                                               (IDX)
               MOVE    SPA-GMN28-TMYNUMBER (IDX)
                                           TO  P02-P028-TMYNUMBER
                                                               (IDX)
               MOVE    SPA-GMN28-TMYDESCRIP(IDX)
                                           TO  P02-P028-TMYDESCRIP
                                                               (IDX)
               IF      IDX             =   SPA-GMN28-MYSELNUM
                   MOVE    "T"         TO  P02-P028-MYLIST-SELECT
                                                              (IDX)
               ELSE
                   MOVE    "F"         TO  P02-P028-MYLIST-SELECT
                                                              (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN28-MYID-MAX      TO  P02-P028-MYLIST-COUNT
      *
           MOVE    SPA-GMN28-MYSELNUM      TO  P02-P028-MYSELNUM
      *
           MOVE    SPA-GMN28-MYIDKEY       TO  P02-P028-MYIDKEY
           MOVE    SPA-GMN28-MYNUMBER      TO  P02-P028-MY-NUMBER
           MOVE    SPA-GMN28-MYDESCRIP     TO  P02-P028-MY-DESCRIP
      *
      *H28.12
      *    指導料等自動算定
           MOVE    SPA-GMN28-AUTOSRYCD(1)  TO  P02-P028-AUTOSRYCD1
           MOVE    SPA-GMN28-AUTOSRYCD(2)  TO  P02-P028-AUTOSRYCD2
           MOVE    SPA-GMN28-AUTOSRYCD(3)  TO  P02-P028-AUTOSRYCD3
           MOVE    SPA-GMN28-AUTONAME (1)  TO  P02-P028-AUTONAME1 
           MOVE    SPA-GMN28-AUTONAME (2)  TO  P02-P028-AUTONAME2 
           MOVE    SPA-GMN28-AUTONAME (3)  TO  P02-P028-AUTONAME3 
      *H30.4
      *    妊婦区分
           IF      SPA-GMN28-NINPUKBN  =   "1"
               MOVE    "T"                 TO  P02-P028-NINPUKBN
           ELSE
               MOVE    "F"                 TO  P02-P028-NINPUKBN
           END-IF
           .
       2001-P028-SET-EXT.
           EXIT.
      *API
      *****************************************************************
      *    ＡＰＩ初期パラメタ、ＳＰＡ初期編集
      *****************************************************************
       300-APIP02-SET-SEC               SECTION.
      *
      *    パラメタファイルのクリア
           PERFORM 1001-PARA-DEL-SEC
      *
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    SPA-GMN-PTNUM       TO  WRK-PTNUM
           INITIALIZE                      SPA-P02GMNAREA
           INITIALIZE                      SPA-P02SCRAREA
           MOVE    WRK-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    WRK-PTID            TO  SPA-GMN-PTID
      *
      *    システム管理より情報取り込み
           PERFORM 1003-SYSKAN-SET-SEC
      *
      *    保険・公費一覧情報取り込み
           PERFORM 1004-HKNKOH-SET-SEC
      *
      *    その他のコンボ
           PERFORM 1005-COMB-SET-SEC
      *
      *    患者情報取り込み
           IF     (SPA-P02-SYORI       =   1     )  OR
                  (SPA-GMN-PTNUM       =   SPACE )
               CONTINUE
           ELSE
      *        患者情報取り込み
               PERFORM 3001-API-KYOUTU-SET-SEC
           END-IF
      *
           .
       300-APIP02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ　患者登録へＳＰＡ、パラメタ設定処理
      *****************************************************************
       3001-API-KYOUTU-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  WRK-HKNID-MAX
           MOVE    ZERO                TO  WRK-KOHID-MAX
      *
      *    患者情報編集
           PERFORM 10021-PTINF-SET-SEC
      *
      *    他の情報編集
           PERFORM 10022-PARA-SET-SEC
      *ver.4.7
      *    地域連携編集
      *    PERFORM 10025-PTNUMPUBLIC-SET-SEC
      *ver.4.7
      *    患者個別情報編集
           PERFORM 10028-PTCONF-SET-SEC
      *
      *    患者労災情報編集
           PERFORM 10023-RSIPARA-SET-SEC
      *
      *    同一患者識別編集
      **** PERFORM 10025-PTSAME-SET-SEC
      *
      *    前回来院日編集
      *****PERFORM 10024-LASTYMD-SET-SEC
           .
       3001-KYOUTU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSTEM-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
     **********MOVE    MCPDATA-REC         TO  SYS-1009-REC
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-READ-EXT.
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
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       910-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTHKNINF-REC
               MOVE    ZERO            TO  FLG-PTHKNINF
           ELSE
               INITIALIZE                  PTHKNINF-REC
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           .
       910-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費履歴読み込み
      *****************************************************************
       920-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       920-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付読み込み
      *****************************************************************
       930-UKETUKE-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"      TO  MCP-TABLE
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
       930-UKETUKE-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険マスター読み込み
      *****************************************************************
       940-HKNCOMB-NEXT-SEC        SECTION.
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
       940-HKNCOMB-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    旧姓履歴マスタ読み込み
      *****************************************************************
       950-KYUSEI-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  KYUSEIRRK-REC
               MOVE    ZERO            TO  FLG-KYUSEIRRK
           ELSE
               INITIALIZE                  KYUSEIRRK-REC
               MOVE    1               TO  FLG-KYUSEIRRK
           END-IF
      *
           .
       950-KYUSEI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    公費負担マスター読み込み
      *****************************************************************
       991-PTKOHFTN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHFTN-REC
               MOVE    ZERO            TO  FLG-PTKOHFTN
           ELSE
               INITIALIZE                  PTKOHFTN-REC
               MOVE    1               TO  FLG-PTKOHFTN
           END-IF
      *
           .
       991-PTKOHFTN-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担マスター読み込み
      *****************************************************************
       991-PTTOKKI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTTOKKI-REC
               MOVE    ZERO            TO  FLG-PTTOKKI
           ELSE
               INITIALIZE                  PTTOKKI-REC
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
      *
           .
       991-PTTOKKI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者レセプト分割テーブル読み込み
      *****************************************************************
       991-PTHDISTDAY-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PT-HDIST-DAY-REC
               MOVE    ZERO            TO  FLG-PTHDISTDAY
           ELSE
               INITIALIZE                  PT-HDIST-DAY-REC
               MOVE    1               TO  FLG-PTHDISTDAY
           END-IF
      *
           .
       991-PTHDISTDAY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号変換（管理）マスタ読み込み
      *****************************************************************
       970-PTNUM-READ-SEC         SECTION.
      *
           MOVE    SPACE               TO  PTNUM-REC
           INITIALIZE                      PTNUM-REC
           MOVE    SPA-HOSPNUM         TO  PTNUM-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTNUM-PTID
      *
           MOVE    PTNUM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnum"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTNUM-REC
                   MOVE    ZERO                TO  FLG-PTNUM
               ELSE
                   MOVE    1                   TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "tbl_ptnum"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       970-PTNUM-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    低所得マスタ読み込み
      *****************************************************************
       990-TSYRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TSYRRK-REC
               MOVE    ZERO            TO  FLG-TSYRRK
           ELSE
               INITIALIZE                  TSYRRK-REC
               MOVE    1               TO  FLG-TSYRRK
           END-IF
      *
           .
       990-TSYRRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    年金マスタ読み込み
      *****************************************************************
       991-TNKRRK-NEXT-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNKRRK-REC
               MOVE    ZERO            TO  FLG-TNKRRK
           ELSE
               INITIALIZE                  TNKRRK-REC
               MOVE    1               TO  FLG-TNKRRK
           END-IF
      *
           .
       991-TNKRRK-NEXT-EXT.
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
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNMST-NEXT-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       921-HKNMST-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
       900-PTSAME-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTSAME
               MOVE    MCPDATA-REC         TO  PTSAME-REC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
      *
           .
       900-PTSAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者地域連携データ読込処理
      *****************************************************************
       920-PTPUBLIC-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTPUBLIC
               MOVE    MCPDATA-REC         TO  PTPUBLIC-REC
           ELSE
               MOVE    1                   TO  FLG-PTPUBLIC
               INITIALIZE                  PTPUBLIC-REC
           END-IF
      *
           .
       920-PTPUBLIC-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別情報データ読込処理
      *****************************************************************
       930-PTCONF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCONF
               MOVE    MCPDATA-REC         TO  PTCONF-REC
           ELSE
               MOVE    1                   TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
      *
           .
       930-PTCONF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護保険情報データデータ読込処理
      *****************************************************************
       930-PTCAREHKNINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-HKNINF
               MOVE    MCPDATA-REC         TO  PTCARE-HKNINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-HKNINF
               INITIALIZE                  PTCARE-HKNINF-REC
           END-IF
      *
           .
       930-PTCAREHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者介護者認定情報データ読込処理
      *****************************************************************
       930-PTCARENINTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-NINTEI
               MOVE    MCPDATA-REC         TO  PTCARE-NINTEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-NINTEI
               INITIALIZE                 PTCARE-NINTEI-REC
           END-IF
      *
           .
       930-PTCARENINTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個人番号情報データ読込処理
      *****************************************************************
       930-PTMYNUMBER-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTMYNUMBER
               MOVE    MCPDATA-REC         TO  PTMYNUMBER-REC
           ELSE
               MOVE    1                   TO  FLG-PTMYNUMBER
               INITIALIZE                  PTMYNUMBER-REC
           END-IF
      *
           .
       930-PTMYNUMBER-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-SYSYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SYSYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 9110-TENSU-READ-SEC
           END-IF
           MOVE    ZERO                TO  FLG-TENSU-OK
           PERFORM UNTIL  (FLG-TENSU       =   1 ) OR
                          (FLG-TENSU-OK    =   1 )
               IF     (TNS-YUKOSTYMD       <=  SPA-SYSYMD ) AND
                      (TNS-YUKOEDYMD       >=  SPA-SYSYMD )
                   MOVE    1                   TO  FLG-TENSU-OK
                   MOVE    1                   TO  FLG-TENSU
               ELSE
                   MOVE    2                   TO  FLG-TENSU-OK
                   MOVE    TNS-TENSU-REC       TO  TNSWK-TENSU-REC
               END-IF
               IF      FLG-TENSU           =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 9110-TENSU-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    当日期間外
           IF      FLG-TENSU-OK        =   2
               MOVE    TNSWK-TENSU-REC     TO  TNS-TENSU-REC
               STRING  "【期間外】"
                      TNS-NAME
                                       DELIMITED  BY  SPACE
                                   INTO    WRK-SRYCD-NAME
               END-STRING
           ELSE
               MOVE    TNS-NAME            TO  WRK-SRYCD-NAME
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       9110-TENSU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
               MOVE    ZERO                TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           .
       9110-TENSU-READ-EXT.
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
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
      *********MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
      *********INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
           EXIT.
      *
      *
