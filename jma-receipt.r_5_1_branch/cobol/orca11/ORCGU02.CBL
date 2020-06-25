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
       PROGRAM-ID.             ORCGU02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 来院受付
      *  コンポーネント名  : 来院受付（Ｕ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/04/10  項目入力チェック追加
      *  01.00.02    MCC-藤原     01/04/10  保険組合せ削除区分＝１読み飛ばし	 
      *  01.00.03    MCC-藤原     01/04/10  カルテ・処方箋の印刷追加
      *  01.00.04    MCC-藤原     01/04/20  患者登録との遷移の修正
      *  01.00.05    MCC-藤原     01/04/20  クリアボタン追加
      *  01.00.06    MCC-藤原     01/04/20  受付・予約済みのときは受診履歴を読まない
      *  01.02.01    MCC-藤原     01/04/20  受付・予約からシス管・患者マスタを読まない
      *  01.02.02    MCC-藤原     01/04/20  患者登録に遷移するとき選択番号をセット
      *  01.02.03    MCC-藤原     01/05/24  患者番号ゼロ詰め処理
      *  01.02.04    MCC-藤原     01/09/13  選択番号の編集
      *                                     患者氏名を漢字、かな同時に検索する    
      *  01.03.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.03.02    MCC-多々納   01/09/25  一覧表選択の追加
      *  01.03.03    MCC-多々納   01/11/15  患者番号等のプロテクト他
      *  01.03.03    MCC-多々納   01/11/28  画面遷移変更
      *  01.03.04    MCC-多々納   02/01/10  受診履歴キー変更
      *  01.03.05    MCC-多々納   02/01/31  選択番号未入力の時患者番号へ
      *  01.03.06    MCC-多々納   02/03/14  帳票カスタマイズ
      *  01.03.07    MCC-多々納   02/04/18  一覧表の表示修正
      *  02.00.00    NACL-多々納  02/08/12  保険確認の追加、
      *                                     インターフェースの変更
      *  02.00.01    NACL-多々納  02/10/09  未確認に記号番号追加
      *  02.00.02    NACL-多々納  02/11/20  ＣＬＡＩＭ処理追加
      *  02.00.03    NACL-多々納  02/11/26  診療科、ドクター件数変更
      *  02.00.04    NACL-多々納  02/12/16  新規患者取消修正
      *  02.00.05    NACL-多々納  03/02/26  診療科・ドクター件数修正
      *  02.00.06    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.07    NACL-多々納  03/03/26  カルテ続紙印刷追加
      *  02.00.08    NACL-多々納  03/06/25  受付一覧更新追加
      *  02.00.09    NACL-多々納  03/06/30  カスタマイズ帳票名取得サブ追加
      *  02.00.10    NACL-多々納  03/09/26  業務権限判定追加
      *                                     処方せんにドクターなし追加
      *  02.00.11    NACL-多々納  03/10/15  ２００明細へ
      *  02.00.12    NACL-多々納  03/11/07  受付更新修正
      *  02.00.13    NACL-多々納  05/02/04  受付マスタに保険組合せ追加
      *  02.00.14    NACL-多々納  05/10/07  ユーザプログラム起動追加
      *  02.00.15    NACL-多々納  05/12/07  送受信フラグ追加
      *  02.00.16    NACL-多々納  05/12/07  年齢追加、警告追加
      *  02.00.17    NACL−多々納 06/08/16  当日担当医設定追加
      *  03.02.00    NACL−多々納 06/08/30  老人警告追加
      *  03.04.00    NACL-多々納  07/02/19  ＱＲ受付対応
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  03.05.00    NACL-多々納  07/06/07  前回来院日共通化と受付に追加
      *  03.05.01    NACL-多々納  07/08/10  メモ登録追加
      *                                     ＱＲ受付削除
      *  03.05.02    NACL-多々納  07/08/28  公費期限切れ追加他
      *  04.05.00    NACL-多々納  09/05/29  性別表示追加、400明細
      *  04.05.00    NACL-多々納  09/10/27  拡張漢字対応
      *  04.06.00    NACL-多々納  10/06/21  受付選択一覧遷移追加
      *                                     患者番号と一覧内容の連動他
      *  04.06.00    NACL-多々納  10/09/29  入外非表示対応
      *  04.07.00    NACL-多々納  11/07/XX  カルテ病名なし対応
      *                                     予約選択患者登録遷移対応他
      *  04.07.00    NACL-多々納  11/08/XX  患者保険確認履歴追加
      *  04.07.01    NACL-多々納  12/01/05  クライアント印刷対応
      *  04.08.01    NACL-多々納  14/05/XX  診療行為一時データ対応
      *  04.08.01    NACL-多々納  15/06/16  診療科・保険組合せ件数対応
      *  04.08.01    NACL-多々納  16/01/XX  医師一覧遷移対応
      *  04.08.01    NACL-多々納  16/06/XX  API CLAIM対応
      *  04.08.01    NACL-多々納  16/12/XX  受付順変更対応
      *  05.00.00    NACL-多々納  17/07/XX  PUSH通信対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "U02COMMON-SPA".
      *
      *---- (01.02.02) LINE ADD START---------------------------------- 
      *    患者登録スパ領域
      *****COPY    "P02COMMON-SPA".
      *---- (01.02.02) LINE ADD END  ----------------------------------
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA                                PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-YYK                 PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-PTKOHINF            PIC 9(01).
           03  FLG-PTHKNINF            PIC 9(01).
           03  FLG-JYURRK              PIC 9(01).
           03  FLG-UKETUKE             PIC 9(01).
           03  FLG-HKNCOMBI            PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-PTRSIINF            PIC 9(01).
           03  FLG-SRYKARRK            PIC 9(01).
           03  FLG-QRCDHKN             PIC 9(01).
           03  FLG-PTMEMOINF           PIC 9(01).
      *
           03  FLG-HENFLG              PIC 9(01).
           03  FLG-TOUROKU             PIC 9(01).
           03  FLG-IN-PTNUM            PIC 9(01).
           03  FLG-IN-NAME             PIC 9(01).
           03  FLG-CLAIM               PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-HKNOK               PIC 9(01).
           03  FLG-DRSET               PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-GMN                 PIC 9(01).
           03  FLG-USERPG              PIC 9(01).
      *
           03  FLG-CLAIM-MULTIHOST     PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
      *
           03  FLG-UKE-OK              PIC 9(01).
           03  FLG-NYUIN               PIC 9(01).
           03  FLG-UKE-CHK             PIC 9(01).
           03  FLG-YYK-CHK             PIC 9(01).
      *    クライアント印刷有
           03  FLG-CLIENT-OK           PIC 9(01).
      *
           03  FLG-K02NYUIN            PIC 9(01).
           03  FLG-OK                  PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDX2                    PIC 9(04).
           03  IDX3                    PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDX-C                   PIC 9(04).
      *
           03  IDX-S1                  PIC 9(04).
           03  IDX-S2                  PIC 9(04).
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL01.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *    診療行為ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    診療行為パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "K01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-NUM               PIC ZZZ9.
           03  WRK-SYMD.
               05  WRK-SYY           PIC 9(04).
               05  WRK-SMM           PIC 9(02).
               05  WRK-SDD           PIC 9(02).
           03  WRK-WYMD              PIC X(07).
           03  WRK-UKEID             PIC 9(05). 
           03  WRK-SRYKA             PIC X(02). 
      *
           03  WRK-SELNUM            PIC 9(04). 
           03  WRK-GMN-SELNUM        PIC 9(02). 
      *
           03  WRK-MOTOPG            PIC X(08). 
      *H25.4
           03  WRK-HZN-MOTOPG          PIC X(08). 
      *H28.01
           03  WRK-HZN-DRCD            PIC X(05).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-KNJ.
               07  WRK-PTNUM                           PIC X(20).
               07  WRK-PTID                            PIC X(10).
               07  WRK-NAME                            PIC X(50).
               07  WRK-NAME1                           PIC X(50).
               07  WRK-NAME2                           PIC X(50).
      *---- (01.00.03) LINE MOD START ---------------------------------
      **       07  WRK-SEX                             PIC X(02).
               07  WRK-SEX                             PIC X(01).
               07  WRK-BIRTHDAYS                       PIC X(08). 
      *---- (01.00.03) LINE MOD END -----------------------------------
               07  WRK-BIRTHDAY                        PIC X(09).
           03  WRK-MOJISU                              PIC 9(04).
      *  受診履歴表示用
           03  WRK-JYURRK-AREA.
               07  WRK-JRK-SRYKA                       PIC X(02).
               07  WRK-JRK-DRCD                        PIC X(05).
               07  WRK-JRK-SRYYMD                      PIC X(08).
               07  WRK-JRK-HKNCOMBI                    PIC 9(04).
      *
           03  WRK-NAME-KEY            PIC X(50).
      *    カルテＰＧ名
           03  WRK-KARUTE-PG           PIC X(20).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *
           03  WRK-HKNCOMBI            PIC X(100).
           03  WRK-HKNCOMBI-2          PIC X(100).
      *
           03  WRK-RENNUM              PIC 9(02).
      *
           03  WRK-E-NAME              PIC X(50).
           03  WRK-H-NAME              PIC X(50).
           03  WRK-H-NAME1             PIC X(02).
      *
           03  WRK-SELNUM1             PIC 9(04).
           03  WRK-SELNUM2             PIC 9(04).
           03  FLG-YYK-OK              PIC 9(01).
      *
           03  WRK-U2ID1MSG            PIC X(80).
      *
           03  WRK-U02-UKETUKE-ROW     PIC S9(09).
           03  FLG-GMN-INIT            PIC 9(01).
           03  WRK-U02-HKNCOMB-ROW     PIC S9(09).
           03  FLG-GMN-INIT2           PIC 9(01).
      *
      *H28.12
           03  SORT-MAX                PIC 9(04).
           03  WRK-IDY                 PIC 9(04).
           03  WRK-IDX                 PIC 9(04).
      *H29.7
      *PUSH通信用
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THHH             PIC X(02).
               05  WRK-THMM             PIC X(02).
               05  WRK-THSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *    公費期限切れ表示用
       01  WRK-KOH-AREA.
           03  WRK-KEI-YM.
               05  WRK-KEI-YY          PIC 9(04).
               05  WRK-KEI-MM          PIC 9(02).
           03  TBL-KOH-TBL1            OCCURS   40.
               05  TBL-K1-KOHNUM       PIC X(03).
               05  TBL-K1-PAYKBN       PIC X(02).
               05  TBL-K1-TEKSTYMD     PIC X(08).
               05  TBL-K1-TEKEDYMD     PIC X(08).
           03  TBL-KOH-TBL2            OCCURS   40.
               05  TBL-K2-KOHNUM       PIC X(03).
               05  TBL-K2-TEKSTYMD     PIC X(08).
               05  TBL-K2-TEKEDYMD     PIC X(08).
           03  IDX-K1                  PIC 9(04).
           03  IDX-K2                  PIC 9(04).
           03  FLG-KOH                 PIC 9(01).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(02).
           03  WRK-NENREI-B        PIC X(01).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(04).
      *
      *D01  WRK-COMBO-SCODE-MAX                       PIC 9(03).
026700*D01  WRK-COMBO-TBL.
026800*D    03  WRK-COMBO-TBLO                        OCCURS   3.
      *D        05  WRK-COMBO-SCODEL                  PIC X(01).  
      *D        05  WRK-COMBO-FIL                     PIC X(01).  
      *D        05  WRK-COMBO-SCODEMEIL               PIC X(18).  
      *
        01  WRK-SORT-TBL.
            03  WRK-SORT-TBL-AREA.
              04  WRK-SORT-TBLO                         OCCURS   400.
      *         SORTキー
                05  WRK-SORT-NARABE                   PIC X(04).
      *
                05  WRK-SORT-TUKEID                   PIC 9(05).  
                05  WRK-SORT-TYYKTIME                 PIC X(04).
                05  WRK-SORT-TUKETIME                 PIC X(06).
                05  WRK-SORT-TPTID                    PIC 9(10).
                05  WRK-SORT-TNAME                    PIC X(50).
                05  WRK-SORT-TSRYKA                   PIC X(02).
                05  WRK-SORT-TDRCD                    PIC X(04).
                05  WRK-SORT-TSRYNAIYOCD              PIC X(02).
                05  WRK-SORT-TKYYKTIME                PIC X(04).
                05  WRK-SORT-TYYKID                   PIC X(02).
                05  WRK-SORT-THKNCOMBI                PIC 9(04).
                05  WRK-SORT-TSRFLG                   PIC X(02).
      *H28.12
                05  WRK-SORT-HENFLG                   PIC X(01).
      *         SORTキー
                05  WRK-SORT-NARABE-G.
                    07  WRK-SORT-HYOJI                PIC X(04).
                    07  WRK-SORT-TIME                 PIC X(04).
      *
           03  WORK-SORT                              PIC X(200).
      *
           03  WRK-U02Q-DATA-G.
               05  WRK-U02Q-REGISTID           PIC 9(05).
               05  WRK-U02Q-PTNUM              PIC X(20).
               05  WRK-U02Q-PTID               PIC 9(10).
               05  WRK-U02Q-HKNID              PIC 9(10).
      *
      *H28.12
      *SORT 保存
        01  WRK-SORT-TBL02-AREA.
            03  WRK-SORT-TBL02                        OCCURS   400.
                05  WRK-SORT2-DATA                    PIC X(200). 
      *
            03  WRK-SORT-KEY                PIC X(04).
            03  WRK-SORT02-KEY              PIC X(08).
            03  IDX-KEY                     PIC 9(04).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG                  PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO                  PIC X(01).
               05  WRK-HYY                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HMM                  PIC Z9.
               05  FILLER                   PIC X(01)   VALUE  ".".
               05  WRK-HDD                  PIC Z9.
      *
           03  WRK-HENYMDN.
               05  WRK-HENNYMD              PIC X(22).
               05  FILLER                   PIC X(02)   VALUE  "（".
               05  WRK-HENNYOBI             PIC X(02).
               05  FILLER                   PIC X(02)   VALUE  "）".
      *
           03  WRK-HENTIME.
               05  WRK-THH                  PIC 99.
               05  FILLER                   PIC X(01)   VALUE  ":".
               05  WRK-TMM                  PIC 99.
      *
      *    予約一覧行数
       01  MAX-LINE                    PIC 9(03)   VALUE  400.
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *    受付
       01  WRK-UKETUKE-REC.
           COPY    "CPUKETUKE.INC"   REPLACING
                                     //UKE-// BY //WRK-UKE-//.
      *
      *    患者マスタ−
       01  HZN-PTINF-REC.
           COPY    "CPPTINF.INC"     REPLACING
                                     //PTINF-// BY //HZN-PTINF-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    （医療機関情報）
           COPY    "CPSK1001.INC".
      *    （診療科情報）
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1008.INC".
      *　　（職員情報）
           COPY    "CPSK1010.INC".
      *　　（診療内容）
           COPY    "CPSK1012.INC".
      *　　（帳票プログラム）
           COPY    "CPSK1032.INC".
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
           COPY    "CPSK1045.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    予約マスター
       01  YYK-REC.
           COPY    "CPYYK.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    患者保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    QRコード(被保険者証)
       01  QRCDHKN-REC.
           COPY    "CPQRCDHKN.INC".
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    全角チェックパラメタ（拡張)
           COPY    "CPORCSKANACHK2.INC".
      *    拡張文字対応
           COPY    "CPORCSSTRING.INC".
      *
      *    カルテパラメタ
           COPY    "CPORCHC01.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    前回来院日設定パラメタ
           COPY    "CPORCSLASTYMD.INC".
      *
      *    保険確認年月日履歴登録サブ
           COPY    "CPORCSPTHKNRRK.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *===> For Claim End
      *H29.7
      *PUSH通信
           COPY    "CPPUSHACCEPT02.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "U02.INC".
           COPY    "U02Q.INC".
           COPY    "U02X.INC".
           COPY    "U2ERR.INC".
           COPY    "U2ID1.INC".
           COPY    "CPAPIMODV2.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                      FLG-AREA
           INITIALIZE                      IDX-AREA
           INITIALIZE                      WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"          ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
      *    クライアント印刷
           IF      FLG-CLIENT-OK       =   1
               PERFORM 600-CLIENT-DMY-SEC
           END-IF
      *
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-U02                       TO  SPA-FREE
           .
       000-PROC-EXIT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                            IDX-AREA
           INITIALIZE                            FLG-AREA
           INITIALIZE                            WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "U2ERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "U02"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC                  SECTION.
      *
           MOVE    2                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           MOVE    SPACE               TO  WRK-MOTOPG
           MOVE    SPACE               TO  WRK-HZN-MOTOPG
           MOVE    SPACE               TO  WRK-HZN-DRCD
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               MOVE    SPA-HZN-MOTOPG      TO  WRK-HZN-MOTOPG
      *H27.12
               IF      WRK-MOTOPG          =   "UDR"
                   MOVE    SPA-DRCD            TO  WRK-HZN-DRCD
               END-IF
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"  OR
                                               "P97"
                                           OR  "XF01"
                                           OR  "U012"
      *        ドクター一覧
                                           OR  "UDR"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-U02
               END-IF
      *
               EVALUATE    WRK-MOTOPG
      *        予約、メモ登録
               WHEN    "Y01"
               WHEN    "XF01"
      *        選択一覧
               WHEN   "U012"
      *            受付編集
                   IF      SPA-GMN-SELNUM      =   ZERO
                       MOVE    ZERO                TO  SPA-NAI-SYORI1
                       PERFORM 3101-UKETU-HEN-SEC
                   END-IF
      *            患者メモ表示処理
                   IF      SPA-GMN-PTID    NOT =   ZERO
                       PERFORM 410119-PTMEMOINF-SEC
                   END-IF
      *        外部より選択がある時
               WHEN   "U01"
               WHEN   "P97"
                   PERFORM 3101-U01SPASET-SEC
      *H28.01
      *        ドクター一覧
               WHEN   "UDR"
      *            ドクター一覧更新
                   PERFORM 31011-UDR-HENSYORI-SEC
      *            受付編集
                   IF      SPA-GMN-SELNUM      =   ZERO
                       MOVE    ZERO                TO  SPA-NAI-SYORI1
                       PERFORM 3101-UKETU-HEN-SEC
                   END-IF
      *
               WHEN    OTHER
                   MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
                   MOVE    SPACE               TO  WRK-MOTOPG
                   IF      SPA-MOTOPG          =   "M01"
                       MOVE    SPACE           TO  WRK-HZN-MOTOPG
                   END-IF
               END-EVALUATE
           END-IF
           IF      WRK-MOTOPG          =   SPACE
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "U02X"
      *        ＣＬＡＩＭ画面より
                   IF      SPA-U02X-FLG        =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    62                  TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 3009-CLAIM-SYORI-SEC
                   END-IF
      *        確認画面より
               WHEN    "U2ID1"
                   PERFORM 3010-U2ID1-SET-SEC
      *
      *DD      WHEN    "U02Q"
      *        ＱＲ一覧画面より
      *            IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *                PERFORM 30010-QRHKN-UKETUKE-SEC
      *            ELSE
      *                MOVE    1                   TO  SPA-GMN-CUR
      *DD          END-IF
               WHEN    "P02"
      *            患者登録画面より
                   PERFORM 310-SPASET-SEC
      *H27.7       排他制御解除処理
                   PERFORM 990-LOCK-OUT-SEC
      *
                   IF     (SPA-ERRMSG(1:2)    =   "NO"  )  AND
                          (SPA-PTNUM(1:1)     =   "*"   )
                       MOVE    SPACE              TO  SPA-PTNUM
                       MOVE    ZERO               TO  SPA-PTID
                   END-IF
                   IF      SPA-PTNUM       NOT =   SPACE
      *                受付選択済みで遷移後、戻った時
                       IF      SPA-P02-UKEID   NOT =   ZERO
                           PERFORM 3009-P02UKEID-CHK-SEC
                       END-IF
                       IF      SPA-GMN-SELNUM      =   ZERO
                       AND     SPA-P02-U02         =   ZERO
      *                    前回一覧選択なし
                           MOVE    SPA-PTNUM           TO  U02-PTNUM
                           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
                           MOVE    1                   TO  FLG-IN-PTNUM
      *                    予約チェックなし
                           MOVE    1                   TO  FLG-YYK-CHK
                           PERFORM 4101-PTNUM-CHK-SEC
                           MOVE    4               TO  SPA-GMN-CUR
                       END-IF
                   END-IF
                   IF      SPA-MOTOPG(1:3)     =   "K02"
                       MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
                   ELSE
                       IF     WRK-HZN-MOTOPG (1:3)  =  "K02"
                           MOVE    WRK-HZN-MOTOPG   TO  SPA-HZN-MOTOPG
                       END-IF
                   END-IF
      *
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
                   IF      SPA-MOTOPG(1:3)     =   "K02"
                       MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
                   ELSE
                       IF     WRK-HZN-MOTOPG (1:3)  =  "K02"
                           MOVE    WRK-HZN-MOTOPG   TO  SPA-HZN-MOTOPG
                       END-IF
                   END-IF
           END-EVALUATE
      *
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3009-P02UKEID-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-UKE-MAX
      *        受付ＩＤ
               IF      SPA-GMN-TUKEID (IDX)    =   SPA-P02-UKEID
                   IF     (SPA-PTNUM       NOT =   SPACE  )  AND
                          (SPA-GMN-TPTNUM(IDX) =   SPA-PTNUM)
                       MOVE    IDX             TO  SPA-GMN-SELNUM
                   END-IF
                   MOVE    SPA-UKE-MAX         TO  IDX
               END-IF
           END-PERFORM
           IF      SPA-GMN-SELNUM      NOT =   ZERO
               PERFORM 4104-SELNUM-CHK-SEC
           END-IF
          .
       3009-P02UKEID-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPACE
               MOVE    4               TO  SPA-GMN-CUR
               IF      WRK-MOTOPG(1:3)     =   "P97"
                   MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
               END-IF
               IF      SPA-GMN-NAME        =   SPACE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    ZERO                TO  SPA-NAI-SELNUM
               MOVE    WRK-SPA-PTNUM       TO  U02-PTNUM
               MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
               MOVE    1                   TO  FLG-IN-PTNUM
               PERFORM 4101-PTNUM-CHK-SEC
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *****************************************************************
      *    確認画面より
      *****************************************************************
       3010-U2ID1-SET-SEC               SECTION.
      *
           EVALUATE    SPA-U2ID1CD
      *        予約紐付け
               WHEN    "0101"
                   IF      SPA-U2ID1-FLG    =   "OK"
                       IF      SPA-NAI-SYORI1      =   1
                           MOVE    ZERO            TO  SPA-NAI-SYORI1
                           PERFORM 3101-UKETU-HEN-SEC
                       END-IF
      *                患者選択処理
                       PERFORM 30101-UKELIST-HEN-SEC
                   ELSE
      *                患者情報編集
                       PERFORM 41011-PTNUM-HENSYU-SEC
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
      *        受付済み
               WHEN    "0102"
                   IF      SPA-U2ID1-FLG    =   "OK"
                       IF      SPA-NAI-SYORI1      =   2
                           MOVE    ZERO            TO  SPA-NAI-SYORI1
                           PERFORM 3101-UKETU-HEN-SEC
                       END-IF
      *                患者選択処理
                       PERFORM 30101-UKELIST-HEN-SEC
                   ELSE
      *                患者情報編集
                       PERFORM 41011-PTNUM-HENSYU-SEC
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
      *ver.4.7
      *        受付取消
               WHEN    "0103"
                   IF      SPA-U2ID1-FLG    =   "OK"
                       PERFORM 4910-DELETE-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *ver.4.7
      *        患者登録へ
               WHEN    "0105"
                   IF      SPA-U2ID1-FLG    =   "OK"
                       MOVE    1                TO  FLG-UKE-CHK
                       PERFORM 492-ORCGP01-SEC
                   ELSE
                       MOVE    62              TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-U2ID1CD
           MOVE    SPACE               TO  SPA-U2ID1-FLG
      *
           .
       3010-U2ID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者選択処理
      *****************************************************************
       30101-UKELIST-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-UKE-MAX )
                          OR  (SPA-GMN-SELNUM  >   ZERO )
               IF     (SPA-GMN-PTNUM       =   SPA-GMN-TPTNUM(IDX))
                  AND (SPA-GMN-PTID        =   SPA-GMN-TPTID (IDX))
                  IF      SPA-U2ID1CD          =   "0101"
                       IF      (SPA-GMN-TUKEID   (IDX) =   ZERO      )
                        AND    (SPA-GMN-TYYKTIME (IDX) NOT =   SPACE )
                           MOVE    SPA-GMN-TNUM (IDX)
                                                   TO  SPA-GMN-SELNUM
                       END-IF
                   ELSE
      *                受付
                       IF     (SPA-GMN-TUKEID   (IDX)  NOT =   ZERO   )
                           MOVE    SPA-GMN-TNUM (IDX)
                                                   TO  SPA-GMN-SELNUM
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      (SPA-ERRCD          =   SPACE )  AND
                   (SPA-GMN-SELNUM     >   ZERO  )
               PERFORM 41041-SELNUM-SYORI-SEC
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
           .
       30101-UKELIST-HEN-EXT.
           EXIT.
      *
      *H27.12
      *****************************************************************
      *    医師選択より処理
      *****************************************************************
       31011-UDR-HENSYORI-SEC              SECTION.
      *
      *    ドクターを再編集する
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    WRK-HZN-DRCD(2:4)   TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           IF      ORCSDR-DRCD-IDX     >   ZERO
               MOVE    SPA-GMN-DRCDLST  (ORCSDR-DRCD-IDX)
                                           TO  SPA-GMN-DRCD-G
           ELSE
               PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-DRCD-MAX 
                   IF      WRK-HZN-DRCD(2:4)   =   SPA-GMN-DRCDLST(IDX)
                                                                 (1:4)
                       MOVE    SPA-GMN-DRCDLST(IDX)
                                               TO  SPA-GMN-DRCD-G
                       MOVE    SPA-DRCD-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           MOVE    05                  TO  SPA-GMN-CUR
      *
           .
       31011-UDR-HENSYORI-EXT.
           EXIT.
      *H27.12
      *
      *****************************************************************
      *    ＱＲより受付処理
      *****************************************************************
      *30010-QRHKN-UKETUKE-SEC              SECTION.
      *
      *    MOVE    SPA-U02Q-DATA-G     TO  WRK-U02Q-DATA-G
      *
      *    MOVE    SPA-U02Q-PTNUM      TO  SPA-GMN-PTNUM
      *    MOVE    SPA-U02Q-PTNUM      TO  U02-PTNUM
      *    MOVE    1                   TO  FLG-IN-PTNUM
      *    PERFORM 4101-PTNUM-CHK-SEC
      *    MOVE    WRK-U02Q-DATA-G     TO  SPA-U02Q-DATA-G
      *
      *    選択の保険を含む保険組合せを選択とする
      *    MOVE    ZERO                TO  WRK-IDXH
      *    MOVE    ZERO                TO  WRK-CNT-HKN
      *    MOVE    ZERO                TO  WRK-CNT-HKN2
      *    PERFORM VARYING     IDX-C   FROM    1   BY  1
      *            UNTIL       IDX-C   >   SPA-COMBI-MAX
      *        IF      SPA-NAI-COB-HKNID (IDX-C)   =   WRK-U02Q-HKNID
      *            MOVE    SPA-GMN-HKNCOMBINUM (IDX-C)
      *                                        TO  SPA-GMN-COMBINUM
      *            IF      WRK-IDXH            =   ZERO
      *                MOVE    IDX-C               TO  WRK-IDXH
      *            END-IF
      *            MOVE    ZERO                TO  WRK-CNT-HKN2
      *            IF      SPA-GMN-KOH1-TANSEIDONAME(IDX-C)
      *                                        NOT =   SPACE
      *                ADD     1               TO  WRK-CNT-HKN2
      *            END-IF
      *            IF      SPA-GMN-KOH2-TANSEIDONAME(IDX-C)
      *                                        NOT =   SPACE
      *                ADD     1               TO  WRK-CNT-HKN2
      *            END-IF
      *            IF      SPA-GMN-KOH3-TANSEIDONAME(IDX-C)
      *                                        NOT =   SPACE
      *                ADD     1               TO  WRK-CNT-HKN2
      *            END-IF
      *            IF      SPA-GMN-KOH4-TANSEIDONAME(IDX-C)
      *                                        NOT =   SPACE
      *                ADD     1               TO  WRK-CNT-HKN2
      *            END-IF
      *            IF      WRK-CNT-HKN         <=  WRK-CNT-HKN2
      *                MOVE    IDX-C               TO  WRK-IDXH
      *                MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
      *            END-IF
      *      END-IF
      *    END-PERFORM
      *
      *    IF      WRK-IDXH            =   ZERO
      *        MOVE    1               TO  IDX-C
      *    ELSE
      *        MOVE    WRK-IDXH        TO  IDX-C
      *    END-IF
      *    MOVE    SPA-GMN-HKNCOMBINUM (IDX-C)
      *                                TO  SPA-GMN-COMBINUM
      *    .
      *30010-QRHKN-UKETUKE-EXT.
      **** EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE                  SPA-U02
      *
           MOVE    2                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    SPA-SYSYMD          TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENNYMD
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY3-AREA
           MOVE  "31"                  TO  LNK-DAY3-IRAI
           MOVE  SPA-SYSYMD            TO  LNK-DAY3-YMD
           CALL    "ORCSDAY"           USING  STS-AREA-DAY
                                              LNK-DAY3-AREA
           MOVE  LNK-DAY3-YOBIMEI      TO  WRK-HENNYOBI
      *
           MOVE    WRK-HENYMDN         TO  SPA-GMN-SRYYMDH
      *
      *    診療科
           MOVE    SPACE               TO  SYS-KEY
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1005-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
      *****        UNTIL       (IDX            >   40   )  OR
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-SRYKA
               MOVE    SPACE                 TO  SPA-GMN-SRYKA1 
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-SRYKANAME
               MOVE    SPA-GMN-SRYKA-G       TO  SPA-GMN-SRYKALST (IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYKALST(01)
                                       TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
      *
      *    全職員情報をＳＰＡにセット
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-ALL-DRCD-AREA
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-ALL-DRCDMAX
      *
      *    科毎の職員情報をＳＰＡにセット
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           MOVE    SPA-GMN-DRCDLST(01) TO  SPA-GMN-DRCD-G
      *
      *    診療内容をＳＰＡにセット
           MOVE    SPACE               TO  SYS-1012-REC
           MOVE    "1012"              TO  SYS-1012-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-1012-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1012-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1012-HOSPNUM
           MOVE    SYS-1012-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1012-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL      (IDX             >   99    )   OR
                              (FLG-SYSKANRI    =   1   )
      *
               MOVE    MCPDATA-REC       TO  SYS-1012-REC
               MOVE    SYS-1012-KBNCD    TO  SPA-GMN-SRYNAIYOCD
               MOVE    SPACE             TO  SPA-GMN-SRYNAIYO1
               MOVE    SYS-1012-SRYNAIYODSP
                                         TO  SPA-GMN-SRYNAIYODSP
               MOVE    SPA-GMN-SRYNAIYOCD-G
                                         TO  SPA-GMN-SRYNAILST (IDX)
               MOVE    IDX               TO  SPA-SRYNAIYO-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-SRYNAILST(01) TO  SPA-GMN-SRYNAIYOCD-G
      *
      *    前回保険組合せ相違チェック
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
      *
           MOVE    SYS-1008-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1008-REC
           ELSE
               INITIALIZE                  SYS-1008-REC
           END-IF
           IF      SYS-1008-ZENHKNCHK-FLG  =   SPACE
               MOVE    "0"                 TO  SPA-ZENHKNCHK-FLG
           ELSE
               MOVE    SYS-1008-ZENHKNCHK-FLG  TO  SPA-ZENHKNCHK-FLG
           END-IF
      *
      *    予約連携
           MOVE    SPACE               TO  SYS-1045-REC
           INITIALIZE                  SYS-1045-REC
           MOVE    "1045"              TO  SYS-1045-KANRICD
           MOVE    "*"                 TO  SYS-1045-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1045-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1045-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1045-HOSPNUM
      *
           MOVE    SYS-1045-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1045-REC
           ELSE
               INITIALIZE                  SYS-1045-REC
           END-IF
           IF      SYS-1045-YYKUKE-KBN =   SPACE
               MOVE    "0"                 TO  SPA-NAI-YYKUKE-KBN
           ELSE
               MOVE    SYS-1045-YYKUKE-KBN TO  SPA-NAI-YYKUKE-KBN
           END-IF
      *    保険組合せチェック
           IF      SYS-1045-UKEHKNCHK-KBN  =   SPACE
               MOVE    "0"                     TO  SPA-NAI-UKEHKNCHK
           ELSE
               MOVE    SYS-1045-UKEHKNCHK-KBN  TO  SPA-NAI-UKEHKNCHK
           END-IF
      *
      *    予約，受付編集
           PERFORM 3101-UKETU-HEN-SEC
      *
      *    カルテ
           MOVE    "0:発行なし"        TO  SPA-GMN-KARUTELST (1)
           MOVE    "1:発行あり"        TO  SPA-GMN-KARUTELST (2)
           MOVE    "2:続紙頭書"        TO  SPA-GMN-KARUTELST (3)
           MOVE    "3:発行あり（病名無）"
                                       TO  SPA-GMN-KARUTELST (4)
           MOVE    4                   TO  SPA-KARUTE-MAX
           MOVE    SPA-GMN-KARUTELST (1)   TO  SPA-GMN-KARUTE-G
      *
      *    処方
           MOVE    "0:発行なし"        TO  SPA-GMN-SYOHOLST (1)
           MOVE    "1:発行あり"        TO  SPA-GMN-SYOHOLST (2)
           MOVE    SPA-GMN-SYOHOLST (1)    TO  SPA-GMN-SYOHO-G
      *
      *    ユーザプログラム起動初期
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    SPACE               TO  SPA-NAI-GYOUMU-FLG
      *        MOVE    ZERO                TO  SPA-NAI-GYOUMU-ARI
      *        GO      TO      310-SPASET-EXT
      *    END-IF
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "U02"               TO  ORCSUSERCHK-GMNPG
           MOVE    "1"                 TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG  TO  SPA-NAI-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI  TO  SPA-NAI-GYOUMU-ARI
      *
           MOVE    "0 Ｕ・Ｐ指示なし"      TO  SPA-GMN-USERPGLST (1)
           IF      SPA-NAI-GYOUMU-ARI  =   1
      *        実行するプログラムがある時
               MOVE    "1 Ｕ・Ｐ指示あり"  TO  SPA-GMN-USERPGLST (2)
               MOVE    2                   TO  SPA-USERPG-MAX
               IF      SPA-NAI-GYOUMU-FLG  =   "1"
                   MOVE    SPA-GMN-USERPGLST (2)   TO  SPA-GMN-USERPG-G
               ELSE
                   MOVE    SPA-GMN-USERPGLST (1)   TO  SPA-GMN-USERPG-G
               END-IF
           ELSE
      *        実行するプログラムがない時
               MOVE    1                   TO  SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPGLST (1)   TO  SPA-GMN-USERPG-G
           END-IF
      *DD  ＱＲデータ判定
      *DD  PERFORM 3101-QRDATA-CHK-SEC
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲデータ処理
      *****************************************************************
      *3101-QRDATA-CHK-SEC              SECTION.
      *
      *    INITIALIZE                      QRCDHKN-REC
      *    MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
      *    MOVE    QRCDHKN-REC         TO  MCPDATA-REC
      *    MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *    MOVE    "key5"              TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *        MOVE    "key5"              TO  MCP-PATHNAME
      *        PERFORM 900-QRCDHKN-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-QRCDHKN
      *        INITIALIZE                      QRCDHKN-REC
      *    END-IF
      *    MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *    MOVE    "key5"              TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *    IF      FLG-QRCDHKN         =   ZERO
      *        MOVE    1                   TO  SPA-QRFLG
      *    ELSE
      *        MOVE    ZERO                TO  SPA-QRFLG
      *    END-IF
      *    .
      *3101-QRDATA-CHK-EXT.
      *    EXIT.
      *****************************************************************
      *    予約，受付編集処理
      *****************************************************************
       3101-UKETU-HEN-SEC                        SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-UKE-MAX
           MOVE    1                   TO  FLG-GMN-INIT
      *H28.12
           MOVE    ZERO                TO  SPA-NAI-UKEUP
           MOVE    ZERO                TO  SPA-NAI-UPOK
           MOVE    ZERO                TO  SPA-NAI-UPDOWN
           MOVE    ZERO                TO  SPA-NAI-YYKARI
      *
      *
           INITIALIZE                            WRK-SORT-TBL
                                                 SPA-GMN-TBL
      *
      *    予約マスター読み込み
           INITIALIZE                  YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-YOYAKU-READ-SEC
           ELSE
               INITIALIZE                  YYK-REC
               MOVE    1               TO  FLG-YYK
           END-IF
      *
      *    IF      SPA-NAI-SYORI1      =   1
      *        予約なし
      *        MOVE    1               TO  FLG-YYK
      *    END-IF
      *
           PERFORM UNTIL     (FLG-YYK            =  1     )  OR
                             (IDX               >=  MAX-LINE   )
      *
               IF     (YYK-YYKID       NOT       =   ZERO       ) AND
                      (YYK-RAIINZUMI             =   SPACE      )
                   ADD     1             TO  IDX
                   MOVE    YYK-YYKTIME   TO  WRK-SORT-NARABE   (IDX)
                   MOVE    YYK-YYKTIME   TO  WRK-SORT-TYYKTIME (IDX)
                   MOVE    YYK-PTID      TO  WRK-SORT-TPTID    (IDX)
                   MOVE    YYK-NAME      TO  WRK-SORT-TNAME    (IDX)
                   MOVE    YYK-SRYKA     TO  WRK-SORT-TSRYKA   (IDX)
                   MOVE    YYK-DRCD(2:4) TO  WRK-SORT-TDRCD    (IDX)
                   MOVE    YYK-SRYNAIYO  TO  WRK-SORT-TSRYNAIYOCD(IDX)
                   MOVE    YYK-KEYYYKTIME
                                         TO  WRK-SORT-TKYYKTIME (IDX)
                   MOVE    YYK-YYKID     TO  WRK-SORT-TYYKID   (IDX)
      *
                   ADD     1             TO  SORT-MAX
               END-IF
      *
               PERFORM 910-YOYAKU-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H28.12
      *    予約存在チェック
           IF       SORT-MAX           >   ZERO
               MOVE    1               TO  SPA-NAI-YYKARI
               MOVE    ZERO            TO  SPA-NAI-UPOK
               IF      SPA-NAI-SYORI1      =   1
      *            予約なし
                   MOVE    ZERO                TO  SORT-MAX
                   INITIALIZE                      WRK-SORT-TBL
                   MOVE    1                   TO  SPA-NAI-UPOK
               END-IF
           ELSE
      *        予約なし
               MOVE    1               TO  SPA-NAI-UPOK
           END-IF
      *
      *    受付テーブル読み込み
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      **** MOVE    "key3"              TO  MCP-PATHNAME
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
     ******    MOVE    "key3"              TO  MCP-PATHNAME
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           IF      SPA-NAI-SYORI1      =   2
      *        受付なし
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           PERFORM     UNTIL  (FLG-UKETUKE        =  1     )  OR
                              (IDX               >=  MAX-LINE   )
               IF     (UKE-UKEID         >   ZERO    )   AND
                      (UKE-KAIKEITIME    =   ZERO    ) 
                   MOVE    1                 TO  FLG-UKE-OK
               ELSE
                   MOVE    ZERO              TO  FLG-UKE-OK
               END-IF
      *
               IF      FLG-UKE-OK          =   1
      *
                       ADD     1             TO  SORT-MAX
      *
                       ADD     1             TO  IDX
                       IF  UKE-YYKTIME       >   ZERO
                           MOVE    UKE-YYKTIME
                                             TO  WRK-SORT-TYYKTIME(IDX)
                           MOVE    UKE-KEYYYKTIME
                                             TO
                                             WRK-SORT-TKYYKTIME(IDX)
                           MOVE    UKE-YYKID TO  WRK-SORT-TYYKID  (IDX)
                       END-IF
                       MOVE    UKE-UKEID     TO  WRK-SORT-TUKEID  (IDX)
                       MOVE    UKE-UKETIME   TO  WRK-SORT-TUKETIME(IDX)
                       MOVE    UKE-UKETIME   TO  WRK-SORT-NARABE  (IDX)
                       MOVE    UKE-PTID      TO  WRK-SORT-TPTID   (IDX)
                       MOVE    UKE-NAME      TO  WRK-SORT-TNAME   (IDX)
                       MOVE    UKE-SRYKA     TO  WRK-SORT-TSRYKA  (IDX)
                       MOVE    UKE-DRCD(2:4) TO  WRK-SORT-TDRCD   (IDX)
                       MOVE    UKE-SRYNAIYO  TO 
                                             WRK-SORT-TSRYNAIYOCD(IDX)
                       MOVE    UKE-HKNCOMBI  TO 
                                             WRK-SORT-THKNCOMBI   (IDX)
      *                送受信フラグ
                       MOVE    UKE-SRFLG     TO
                                             WRK-SORT-TSRFLG   (IDX)
      *H28.12
      *            受付順変更
                   IF      UKE-KAKREN      =   "1"
                       MOVE    "1"         TO  WRK-SORT-HENFLG (IDX)
                   END-IF
      *            受付順
                   MOVE    UKE-UKETIME     TO  WRK-SORT-TIME   (IDX)
                   IF      UKE-HYOJIJUN    =   ZERO
                       MOVE    ALL "9"         TO  WRK-SORT-HYOJI (IDX)
                   ELSE
                       MOVE    UKE-HYOJIJUN    TO  WRK-SORT-HYOJI (IDX)
                   END-IF
      *
      *削除（患者氏名が未設定はありえない）
      *                患者氏名を患者マスタより
      *                IF     (UKE-PTID      NOT =   ZERO  )  AND
      *                       (UKE-NAME          =   SPACE )
      *
      *    患者マスター存在チェック
      *                    MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
      *                    MOVE    UKE-PTID            TO  PTINF-PTID
      *                    PERFORM 900-PTINF-READ-SEC
      *                    IF      FLG-PTINF       =   ZERO
      *                        MOVE    PTINF-NAME
      *                                        TO  WRK-SORT-TNAME (IDX)
      *                        MOVE    PTINF-NAME
      *                                        TO  UKE-NAME
      *
      *                        MOVE    UKETUKE-REC     TO  MCPDATA-REC
      *                        MOVE    "tbl_uketuke"   TO  MCP-TABLE
      *                        MOVE    "key"           TO  MCP-PATHNAME
      *                        MOVE    "DBUPDATE"      TO  MCP-FUNC
grpsys*                        CALL    "ORCDBMAIN"     USING   MCPAREA
      *                                                    MCPDATA-REC
      *                                                    SPA-AREA
      *                    END-IF
      **************** END-IF
               END-IF
      *
      ****     MOVE    "key3"              TO  MCP-PATHNAME
      *H28.12
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
      *
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    PERFORM  31012-SORT-SEC
      *H28.12
           IF      SORT-MAX            =   ZERO
               MOVE    ZERO            TO  SPA-NAI-UPOK
           ELSE
               IF       SPA-NAI-UPOK       =   1
      *            受付順変更順
                   PERFORM  31012-SORT02-SEC
               ELSE
      *            予約・受付時間順
                   PERFORM  31012-SORT-SEC
               END-IF
          END-IF
      *
      *
           MOVE    ZERO            TO  IDX  SPA-UKE-MAX
           INITIALIZE              SPA-GMN-TBL
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       MAX-LINE
                   OR      WRK-SORT-NARABE(IDX)   =   SPACE
               MOVE    IDX                 TO  SPA-UKE-MAX
      *
               MOVE    IDX            TO  SPA-GMN-TNUM       (IDX)
               MOVE    WRK-SORT-TUKEID  (IDX)
                                      TO  SPA-GMN-TUKEID     (IDX)
               MOVE    WRK-SORT-TYYKTIME(IDX)
                                      TO  SPA-GMN-TYYKTIME   (IDX)
               MOVE    WRK-SORT-TUKETIME(IDX)
                                      TO  SPA-GMN-TUKETIME   (IDX)
               MOVE    WRK-SORT-TPTID(IDX)
                                      TO  SPA-GMN-TPTID      (IDX)
               MOVE    WRK-SORT-TNAME(IDX)
                                      TO  SPA-GMN-TNAME      (IDX)
               MOVE    WRK-SORT-TSRYKA(IDX)
                                      TO  SPA-GMN-TSRYKA     (IDX)
               MOVE    WRK-SORT-TDRCD(IDX)
                                      TO  SPA-GMN-TDRCD      (IDX)
               MOVE    WRK-SORT-TSRYNAIYOCD(IDX)
                                      TO  SPA-GMN-TSRYNAIYOCD(IDX)
               MOVE    WRK-SORT-THKNCOMBI   (IDX)
                                      TO  SPA-GMN-THKNCOMBI  (IDX)
               MOVE    WRK-SORT-TKYYKTIME(IDX)
                                      TO  SPA-GMN-TKYYKTIME  (IDX)
               MOVE    WRK-SORT-TYYKID(IDX)
                                      TO  SPA-GMN-TYYKID     (IDX)
      *H28.12
               MOVE    WRK-SORT-HENFLG (IDX)
                                      TO  SPA-GMN-THENFLG  (IDX)
      *
      *        患者番号セット
               IF      SPA-GMN-TPTID(IDX)      =   ZERO   OR  SPACE
                   MOVE    SPACE               TO  SPA-GMN-TPTNUM(IDX)
               ELSE
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    SPA-GMN-TPTID(IDX)  TO  SPTID-PTID
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                       SPA-AREA
                   IF      SPTID-RC            =   ZERO
                       MOVE    SPTID-PTNUM     TO  SPA-GMN-TPTNUM(IDX)
      *                最終受診日セット
                       PERFORM 31019-JYURRK-HEN-SEC
      *                年齢計算
                       PERFORM 31020-NENREI-KEI-SEC
                   END-IF
               END-IF 
      *
      *        診療科情報をＳＰＡにセット
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >       SPA-SRYKA-MAX
                   IF      SPA-GMN-TSRYKA (IDX)
                                       =  SPA-GMN-SRYKALST(IDY)(1:2)
                       MOVE    SPA-GMN-SRYKALST (IDY) (4:)
                                           TO  SPA-GMN-TSRYKANAME(IDX)
                       MOVE    SPA-SRYKA-MAX       TO  IDY
                   END-IF
               END-PERFORM
      *        職員情報をＳＰＡにセット
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >       SPA-ALL-DRCDMAX
                   IF      SPA-GMN-TDRCD (IDX)
                                           =  SPA-ALL-DRCD(IDY)
                       MOVE    SPA-ALL-DRCD-G (IDY)
                                               TO  SPA-GMN-TDRCD-G(IDX)
                       MOVE    1               TO  FLG-OK
                       MOVE    SPA-ALL-DRCDMAX     TO  IDY
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   INITIALIZE                  ORCSDRSETAREA
                   MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
                   MOVE    "2"                 TO  ORCSDR-KBN
                   MOVE    SPA-GMN-TSRYKA (IDX)
                                               TO  ORCSDR-SRYKA
                   MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
                   MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
                   MOVE    SPA-GMN-TDRCD (IDX)
                                               TO  ORCSDR-IN-DRCD
                   CALL    "ORCSDRSET"         USING
                                               SPA-AREA
                                               ORCSDRSETAREA
                   MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-TDRCD-G(IDX)
               END-IF
      *
      *        INITIALIZE                  ORCSDRSETAREA
      *        MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
      *        MOVE    "2"                 TO  ORCSDR-KBN
      *        MOVE    SPA-GMN-TSRYKA (IDX)
      *                                    TO  ORCSDR-SRYKA
      *        MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
      *        MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
      *        MOVE    SPA-GMN-TDRCD (IDX)
      *                                    TO  ORCSDR-IN-DRCD
      *        CALL    "ORCSDRSET"         USING
      *                                    SPA-AREA
      *                                    ORCSDRSETAREA
      *        MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-TDRCD-G(IDX)
      *        診療内容をＳＰＡにセット
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYNAIYO-MAX
                       OR          SPA-GMN-SRYNAILST(IDY)  =   SPACE
                   IF      SPA-GMN-TSRYNAIYOCD(IDX)
                                           =   SPA-GMN-SRYNAILST(IDY)
                                                                (1:2)
                       MOVE    SPA-GMN-SRYNAILST(IDY) (4:)
                                       TO  SPA-GMN-TSRYNAIYODSP(IDX)
                       MOVE    SPA-SRYNAIYO-MAX    TO  IDY
                   END-IF
               END-PERFORM
      *        保険組合せ設定
               IF      SPA-GMN-THKNCOMBI  (IDX)    =   SPACE  OR  ZERO
                   MOVE    SPACE           TO  SPA-GMN-THKNCOMBI-G (IDX)
               ELSE
                   PERFORM 31013-HKNCOMBI-HEN-SEC
               END-IF
      *
      *        送受信フラグ
               EVALUATE    WRK-SORT-TSRFLG (IDX)
                   WHEN    "00"
                       MOVE    "送済"          TO  SPA-GMN-TSRFLG (IDX)
                   WHEN    "01"
                       MOVE    "閉塞"          TO  SPA-GMN-TSRFLG (IDX)
                   WHEN    "02"
                       MOVE    "送誤"          TO  SPA-GMN-TSRFLG (IDX)
                   WHEN    "03"
                       MOVE    "会待"          TO  SPA-GMN-TSRFLG (IDX)
               END-EVALUATE
           END-PERFORM
      *
           .
       3101-UKETU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    予約受付状況並べ替え
      *****************************************************************
       31012-SORT-SEC              SECTION.
      *
      *H28.12
      *    PERFORM VARYING IDX     FROM    1   BY  1
      *            UNTIL   IDX     >=      MAX-LINE
      *            OR      WRK-SORT-NARABE(IDX)   =   SPACE
      *        COMPUTE IDY     =   IDX     +   1
      *        PERFORM VARYING IDZ     FROM    IDY     BY  1
      *                UNTIL   IDZ     >       MAX-LINE
      *                OR      WRK-SORT-NARABE(IDZ)   =   SPACE
      *            IF      WRK-SORT-NARABE (IDX)   >   
      *                                    WRK-SORT-NARABE (IDZ)
      *                MOVE    WRK-SORT-TBLO  (IDX)   TO  WORK-SORT
      *                MOVE    WRK-SORT-TBLO  (IDZ)   TO  
      *                                        WRK-SORT-TBLO (IDX)
      *                MOVE    WORK-SORT               TO  
      *                                        WRK-SORT-TBLO (IDZ)
      *            END-IF
      *        END-PERFORM
      *    END-PERFORM
      *H28.12
      *    SORT変更
           MOVE    SPACE           TO  WRK-SORT-TBL02-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >=      MAX-LINE
                       OR  IDX     >    SORT-MAX
               MOVE    HIGH-VALUE          TO  WRK-SORT-KEY
               MOVE    ZERO                TO  IDX-KEY
               PERFORM VARYING IDZ     FROM    1       BY  1
                       UNTIL   IDZ     >       MAX-LINE
                       OR  IDX     >    SORT-MAX
                   IF      WRK-SORT-NARABE (IDZ)   NOT =   SPACE
                       IF      WRK-SORT-NARABE (IDZ)
                                           <   WRK-SORT-KEY
                           MOVE    WRK-SORT-NARABE (IDZ)
                                               TO  WRK-SORT-KEY
                           MOVE    IDZ             TO  IDX-KEY
                        END-IF
                    END-IF
               END-PERFORM
               IF      IDX-KEY             =   ZERO
                   MOVE    MAX-LINE        TO  IDX
               ELSE
                   MOVE    WRK-SORT-TBLO  (IDX-KEY)
                                           TO  WRK-SORT-TBL02
                                                        (IDX)
                   MOVE    SPACE           TO  WRK-SORT-NARABE
                                                        (IDX-KEY)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SORT-TBL-AREA
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >=      MAX-LINE
                       OR  IDX     >    SORT-MAX
               MOVE    WRK-SORT-TBL02(IDX)
                                   TO  WRK-SORT-TBLO (IDX)
           END-PERFORM
           .
       31012-SORT-EXT.
           EXIT.
      *
      *H28.12
      *****************************************************************
      *    受付　受付順並べ替え
      *****************************************************************
       31012-SORT02-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-SORT-TBL02-AREA
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >=      MAX-LINE
                       OR  IDX     >    SORT-MAX
               MOVE    HIGH-VALUE          TO  WRK-SORT02-KEY
               MOVE    ZERO                TO  IDX-KEY
               PERFORM VARYING IDZ     FROM    1       BY  1
                       UNTIL   IDZ     >       MAX-LINE
                           OR  IDX     >    SORT-MAX
                   IF      WRK-SORT-NARABE-G(IDZ)   NOT =   SPACE
                       IF      WRK-SORT-NARABE-G(IDZ)
                                           <   WRK-SORT02-KEY
                           MOVE    WRK-SORT-NARABE-G(IDZ)
                                               TO  WRK-SORT02-KEY
                           MOVE    IDZ             TO  IDX-KEY
                        END-IF
                    END-IF
               END-PERFORM
               IF      IDX-KEY             =   ZERO
                   MOVE    MAX-LINE        TO  IDX
               ELSE
                   MOVE    WRK-SORT-TBLO  (IDX-KEY)
                                           TO  WRK-SORT-TBL02
                                                        (IDX)
                   MOVE    SPACE           TO  WRK-SORT-NARABE
                                                        (IDX-KEY)
                   MOVE    SPACE           TO  WRK-SORT-NARABE-G
                                                        (IDX-KEY)
               END-IF
           END-PERFORM
           MOVE    SPACE               TO  WRK-SORT-TBL-AREA
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >=      MAX-LINE
                       OR  IDX     >    SORT-MAX
               MOVE    WRK-SORT-TBL02(IDX)
                                   TO  WRK-SORT-TBLO (IDX)
           END-PERFORM
           .
       31012-SORT02-EXT.
           EXIT.
      *
      *****************************************************************
      *    最終受診日セット処理
      *****************************************************************
       31019-JYURRK-HEN-SEC            SECTION.
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    SPA-GMN-TPTID (IDX) TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *
           MOVE    ORCSLASTYMD-LASTYMDW    TO  SPA-GMN-TRRKYMD (IDX)
           MOVE    SPACE                   TO  SPA-GMN-TNYUKBN (IDX)
      *    入院中は病棟を表示
           IF      ORCSLASTYMD-BRMNUM  NOT =   SPACE
               MOVE    ORCSLASTYMD-BRMNUM      TO  SPA-GMN-TRRKYMD (IDX)
               MOVE    "1"                     TO  SPA-GMN-TNYUKBN (IDX)
           END-IF
      *
           .
       31019-JYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴から最終受診日セット処理
      *****************************************************************
       31020-SRYKARRK-HEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-GMN-TPTID (IDX) TO  KARRK-PTID
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                  SRYKARRK-REC
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SRYKARRK        =   ZERO
      *        最終受診日
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    KARRK-LASTYMD       TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  SPA-GMN-TRRKYMD (IDX)
           END-IF
      *
           .
       31020-SRYKARRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧年齢計算処理
      *****************************************************************
       31020-NENREI-KEI-SEC            SECTION.
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-TPTID (IDX) TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF       NOT =   ZERO
               GO      TO      31020-NENREI-KEI-EXT
           END-IF
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-TPTID (IDX) TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    PTINF-BIRTHDAY      TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    画面表示用
           MOVE    SPACE               TO  WRK-NENREI-G
           IF      AGECHK-NENREI2-YY   =   ZERO
               MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-TNENREI (IDX)
      *    性別
           EVALUATE    PTINF-SEX
               WHEN    "1"
                    MOVE    "男"                TO  SPA-GMN-TSEX (IDX)
                WHEN    "2"
                    MOVE    "女"                TO  SPA-GMN-TSEX (IDX)
           END-EVALUATE
      *
           .
       31020-NENREI-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ設定処理
      *****************************************************************
       31013-HKNCOMBI-HEN-SEC              SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-TPTID (IDX) TO  COMB-PTID
           MOVE    SPA-GMN-THKNCOMBI(IDX)
                                       TO  COMB-HKNCOMBINUM
           PERFORM 9001-HKNCOMBI-KEY-READ-SEC
      *
           IF      COMB-DELKBN     NOT =   SPACE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           IF      FLG-HKNCOMBI        =   ZERO
               PERFORM 310131-HKNCOMBIMEI-HEN-SEC
           END-IF
      *
           .
       31013-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       310131-HKNCOMBIMEI-HEN-SEC      SECTION.
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
           MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI (IDX)
      *****
      *****
      *労災の部位は編集していなかったので、編集なし。要望があれば行う
           GO      TO      310131-HKNCOMBIMEI-HEN-EXT
      *****
      *
      *    労災の時、コメントの追加
           IF      COMB-HKNNUM         =   "971"  OR  "973"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    COMB-PTID           TO  PTRSI-PTID
               MOVE    COMB-HKNID          TO  PTRSI-HKNID
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
               MOVE    SPACE               TO  WRK-HKNKBN-MEI
               EVALUATE    PTRSI-HKNKBN
                   WHEN    "1"
                       MOVE    "短"        TO  WRK-HKNKBN-MEI
                   WHEN    "2"
                       MOVE    "傷"        TO  WRK-HKNKBN-MEI
                   WHEN    "3"
                       MOVE    "ア"        TO  WRK-HKNKBN-MEI
                   WHEN    "5"
                       MOVE    "公"        TO  WRK-HKNKBN-MEI
               END-EVALUATE
               IF      PTRSI-COMMENT       =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKNCOMBI
                   STRING  COMB-SYU-TANSEIDONAME   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                   END-STRING
               END-IF
           END-IF
      *H25.7
      *    第三者行為
           IF      COMB-KOH1HKNNUM     =   "970"
      *        患者保険情報検索
               INITIALIZE                      PTRSIINF-REC
               MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
               MOVE    COMB-PTID           TO  PTRSI-PTID
               MOVE    ZERO                TO  PTRSI-HKNID
               MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
      *
               MOVE    PTRSIINF-REC        TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 993-PTRSIINF-NEXT-SEC
      *
                DISPLAY "FLG-PTRSIINF:" FLG-PTRSIINF
      *
               ELSE
                   INITIALIZE                      PTRSIINF-REC
                   MOVE    1                   TO  FLG-PTRSIINF
               END-IF
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      PTRSI-COMMENT   NOT =   SPACE
                   MOVE    SPACE               TO  WRK-HKNCOMBI-2
                   STRING  WRK-HKNCOMBI            DELIMITED  BY  "  "
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI-2
                   END-STRING
                   MOVE    WRK-HKNCOMBI-2      TO  WRK-HKNCOMBI
               END-IF
           END-IF
      *
           MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI (IDX)
      *
           .
       310131-HKNCOMBIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＭＥ画面よりの処理
      *****************************************************************
       3009-CLAIM-SYORI-SEC              SECTION.
      *
           IF      SPA-U02X-FLG        =  "OK"
               MOVE    1               TO  FLG-CLAIM
           ELSE
               MOVE    ZERO            TO  FLG-CLAIM
           END-IF
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *
      *    更新処理
           PERFORM 49001-KOUSIN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
          .
       3009-CLAIM-SYORI-EXT.
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
      *---- (01.00.05) LINE ADD START --------------------------------
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 493-CLEAR-SEC
      *---- (01.00.05) LINE ADD END ----------------------------------
      *    受付一覧更新
               WHEN    "CLICKED"       ALSO    "pandatimer1"
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4931-UKELIST-SEC
      *
      *    受付取消へ
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 491-SAKUJO-SEC
      *    前回患者
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *    患者登録
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 492-ORCGP01-SEC
      *    診療行為
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 493-ORCGK02-SEC
      *    病名登録
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 494-ORCGC02-SEC
      *    収納登録
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 495-ORCGS02-SEC
      *    会計照会
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 496-ORCGJ02-SEC
      *    氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *    予約登録へ
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 460-YOYAKU-SEC
      *    受付一覧へ
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *    受付完了
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    印刷のみ
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 495-PRINT-SEC
      *    前（↑）
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 465-UP-SYORI-SEC
      *    次（↓）
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 475-DOWN-SYORI-SEC
      *    ＱＲ一覧へ
               WHEN    "CLICKED"       ALSO    "B10S"
      *@@@@
      *DDDD        PERFORM 4105-U02Q-SYORI-SEC
                   PERFORM 4105-XF01-SYORI-SEC
      *    保険チェック
               WHEN    "CLICKED"       ALSO    "BHK1"
               WHEN    "CLICKED"       ALSO    "BHK2"
               WHEN    "CLICKED"       ALSO    "BHK3"
               WHEN    "CLICKED"       ALSO    "BHK4"
               WHEN    "CLICKED"       ALSO    "BHK5"
                   PERFORM 400-HKNCHK-SYORI-SEC
      *!!
      *        予約・受付のみ切り替え
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 4905-B05S-SYORI-SEC
      *        選択一覧へ
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 4908-B08S-SYORI-SEC
      *H28.12
      *        受付順変更
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 4905-B03S-SYORI-SEC
      *
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
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *            保険組合せ選択
               WHEN    ANY             ALSO    "HKNCOMBI_LIST"
                   PERFORM 4109-HKNCOMBI-LIST-SEC
      *            受付一覧選択
               WHEN    ANY             ALSO    "UKETUKE_LIST"
                   PERFORM 4110-UKETUKE-LIST-SEC
      *           入力チェック処理
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
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
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               IF      FLG-TOUROKU         =   1
                                           OR  2
                   PERFORM 4109-TOUROKU-CHK-SEC
               END-IF
           END-IF
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
      *    患者番号
           IF      U02-PTNUM           =   SPA-MAE-PTNUM
               MOVE    ZERO                TO  FLG-IN-PTNUM
           ELSE
               MOVE    1                   TO  FLG-IN-PTNUM
           END-IF
           MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
      *    新患予約者氏名
           IF      U02-NAME            =   SPA-MAE-NAME
               MOVE    ZERO                TO  FLG-IN-NAME
           ELSE
               MOVE    1                   TO  FLG-IN-NAME
           END-IF
           MOVE    U02-NAME            TO  SPA-GMN-NAME
      *
           IF      U02-HKNCOMBI   NOT  =   SPA-GMN-COMBINUM
               MOVE    ZERO                TO  SPA-NAI-KEIFLG
           END-IF
      *
           MOVE    U02-SRYKA           TO  SPA-GMN-SRYKA-G
      *
      *H28.01
           IF      U02-DRNAME(1:1)     =   "*"
               CONTINUE
           ELSE
               MOVE    U02-DRNAME          TO  SPA-GMN-DRCD-G
           END-IF
      *
           MOVE    U02-SRYNAIYO        TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    U02-KARUTE          TO  SPA-GMN-KARUTE-G
           MOVE    U02-SYOHO           TO  SPA-GMN-SYOHO-G
           IF      U02-HKNCOMBI    NOT =   SPA-GMN-COMBINUM
               MOVE    ZERO            TO  SPA-NAI-KEIFLG3
           END-IF
           MOVE    U02-HKNCOMBI        TO  SPA-GMN-COMBINUM
           MOVE    U02-USERPG          TO  SPA-GMN-USERPG-G
      *
           MOVE    U02-SELNUM          TO  SPA-GMN-SELNUM
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
      *    選択番号入力時、選択処理
           IF      WRK-MCP-WIDGET      =   "SELNUM"
               PERFORM 4104-SELNUM-CHK-SEC
               IF      SPA-GMN-SELNUM      NOT =   ZERO
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *
      *H28.2
      *    選択番号エラー対応
           IF     (SPA-GMN-SELNUM      =   SPA-NAI-SELNUM)
            AND   (SPA-GMN-SELNUM   NOT=   ZERO       )
            AND   (FLG-TOUROKU         =   1          )
            AND  ((FLG-IN-PTNUM        =   1          )  OR
                  (FLG-IN-NAME         =   1          ))
               PERFORM 493-CLEAR-SEC
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0032"              TO  SPA-ERRCD
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *H28.01
      *    ドクター一覧へ
           IF      WRK-MCP-WIDGET      =   "DRNAME"
               IF      U02-DRNAME(1:1)     =   "*"
                   PERFORM 4908-UDR-SYORI-SEC
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    患者番号
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4101-PTNUM-CHK-SEC
           END-IF
      *
      *    氏名
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4102-NAME-CHK-SEC
           END-IF
      *    診療科
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4103-SRYKA-CHK-SEC
           END-IF
      *    ドクター
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4105-DRCD-CHK-SEC
           END-IF
      *    内容
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4106-SRYNAIYO-CHK-SEC
           END-IF
      *            カルテ
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4107-KARUTE-CHK-SEC
           END-IF
      *            処方箋
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4108-SYOHO-CHK-SEC
           END-IF
      *    保険組合せ
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4109-HKNCOMBI-CHK-SEC
           END-IF
      *    Ｕ・Ｐ
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-END             =   ZERO  )
               PERFORM 4109-USERPG-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者番号 チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC          SECTION.
      *
      *    ”＊”で、新患者登録へ
           IF      SPA-GMN-PTNUM(1:1)  =   "*"
               MOVE    ZERO                TO  SPA-GMN-PTID
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               PERFORM 492-ORCGP01-SEC
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *    入力なしの時、以下処理なし
           IF      FLG-IN-PTNUM        =   ZERO
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *
      *    クリア
           PERFORM 1009-HEAD-CLEAR-SEC
      *
           MOVE    U02-PTNUM           TO  SPA-GMN-PTNUM
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
               GO  TO  4101-PTNUM-CHK-EXT
           END-IF
      *    患者番号変換処理
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    0
      *            患者番号決定
                   MOVE    SPTNUM-PTID         TO  SPA-GMN-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
      *
      *            患者マスター存在チェック
                   MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
                   MOVE    SPA-GMN-PTID        TO  PTINF-PTID
                   PERFORM 900-PTINF-READ-SEC
                   IF      FLG-PTINF           NOT =   ZERO
                       MOVE    "0002"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
      *            患者番号なし
               WHEN    OTHER
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
           END-EVALUATE
      *
      *    排他制御処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
           AND    (SPA-NAI-YYKUKE-KBN  NOT =   "0"  )
           AND    (FLG-YYK-CHK         =   ZERO  )
      *        予約有無チェック処理
               PERFORM 41012-PTNUM-YYKCHEK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
             AND  (SPA-U2ID1CD         =   SPACE )
      *        患者情報編集
               PERFORM 41011-PTNUM-HENSYU-SEC
           END-IF
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約有無チェック処理
      *****************************************************************
       41012-PTNUM-YYKCHEK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-YYK-OK
           IF      SPA-NAI-SYORI1      =   1
      *        予約が一覧にない時、予約を検索
               PERFORM 410121-YYK-CHK-SEC
           ELSE
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL      (IDX     >   SPA-UKE-MAX )
                              OR  (FLG-YYK-OK  =   1       )
                   IF     (SPA-GMN-PTNUM       =   SPA-GMN-TPTNUM(IDX))
                      AND (SPA-GMN-PTID        =   SPA-GMN-TPTID (IDX))
                      AND (SPA-GMN-TUKEID   (IDX)  =   ZERO   )
                      AND (SPA-GMN-TYYKTIME (IDX)  NOT =   SPACE  )
                       MOVE    1                   TO  FLG-YYK-OK
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-YYK-OK          =   1
               MOVE    "0101"              TO  SPA-U2ID1CD
               GO      TO      41012-PTNUM-YYKCHEK-EXT 
           END-IF
           IF     (SPA-NAI-YYKUKE-KBN  =   "2"  )
               GO      TO      41012-PTNUM-YYKCHEK-EXT 
           END-IF
      *    受付の検索
           IF      SPA-NAI-SYORI1      =   2
      *        受付の検索
               PERFORM 410121-UKETUKE-CHK-SEC
           ELSE
               PERFORM VARYING     IDX     FROM   1   BY  1
                       UNTIL      (IDX     >   SPA-UKE-MAX )
                              OR  (FLG-YYK-OK  =   1       )
                   IF     (SPA-GMN-PTNUM       =   SPA-GMN-TPTNUM(IDX))
                      AND (SPA-GMN-PTID        =   SPA-GMN-TPTID (IDX))
                      AND (SPA-GMN-TUKEID   (IDX)  NOT =   ZERO   )
                       MOVE    2                   TO  FLG-YYK-OK
                   END-IF
               END-PERFORM
           END-IF
           IF      FLG-YYK-OK          =   2
               MOVE    "0102"              TO  SPA-U2ID1CD
           END-IF
           .
       41012-PTNUM-YYKCHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       410121-YYK-CHK-SEC            SECTION.
      *
           INITIALIZE                      YYK-REC
           MOVE    SPA-HOSPNUM         TO  YYK-HOSPNUM
           MOVE    SPA-SYSYMD          TO  YYK-YYKYMD
           MOVE    SPA-GMN-PTID        TO  YYK-PTID
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
           MOVE    ZERO                TO  FLG-YYK-OK
           PERFORM UNTIL      (FLG-YYK     =   1    )
                          OR  (FLG-YYK-OK  NOT =   ZERO )
               IF     (YYK-YYKID       NOT =   ZERO )
                  AND (YYK-RAIINZUMI       =   SPACE )
                   MOVE    1                   TO  FLG-YYK-OK
               END-IF
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-YOYAKU-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410121-YYK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付情報編集処理
      *****************************************************************
       410121-UKETUKE-CHK-SEC            SECTION.
      *
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  FLG-YYK-OK
           PERFORM UNTIL      (FLG-UKETUKE     =   1    )
                          OR  (FLG-YYK-OK  NOT =   ZERO )
               IF     (UKE-UKEID         >   ZERO    )   AND
                      (UKE-KAIKEITIME    =   ZERO    ) 
                   MOVE    2                   TO  FLG-YYK-OK
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410121-UKETUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報編集処理
      *****************************************************************
       41011-PTNUM-HENSYU-SEC            SECTION.
      *
      *    患者マスター存在チェック
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           NOT =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      41011-PTNUM-HENSYU-EXT
           END-IF
      *
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
      *
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY 
           MOVE    PTINF-SEX           TO  SPA-NAI-SEX
           EVALUATE    SPA-NAI-SEX
               WHEN    "1"
                    MOVE    "男"                TO  SPA-GMN-SEX
                WHEN    "2"
                    MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *    年齢計算
           PERFORM 41011-NENREI-SET-SEC
      *    保険組合せ表示処理
           PERFORM 410114-KUMIHYOJI-SEC
      *    保険確認表示処理
           PERFORM 410115-KAKUHYOJI-SEC
      *    前回受診履歴処理
           IF      SPA-GMN-SELNUM           =   ZERO
               PERFORM 410116-JYURRKSET-SEC
           END-IF
      *    患者メモ表示処理
           PERFORM 410119-PTMEMOINF-SEC
      *
      *    前回編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
      *
      *    前回患者ヘ編集
           MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
           .
       41011-PTNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       41011-NENREI-SET-SEC            SECTION.
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
      *    画面表示用
           MOVE    SPACE               TO  WRK-NENREI-G
           IF      AGECHK-NENREI2-YY   =   ZERO
               MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *    保険年齢
           MOVE    AGECHK-NENREI       TO  SPA-NAI-NENREI
           .
       41011-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    新患予約者氏名
      *****************************************************************
       4102-NAME-CHK-SEC            SECTION.
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
               GO      TO      4102-NAME-CHK-EXT
           END-IF
      *
           IF      FLG-IN-NAME         =   ZERO
               GO      TO      4102-NAME-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-KNJ
           MOVE    SPACE               TO  WRK-PTID
           PERFORM 41021-KNJKEN-HEN-SEC
           IF      SPA-ERRCD           =   SPACE
               IF      IDZ                  >   1
      *            氏名検索へ
                   PERFORM 480-KENSAKU-SEC
               ELSE
                   IF      WRK-PTID           =   SPACE
                       CONTINUE
                   ELSE
      *                患者マスタあり
                       MOVE    WRK-PTNUM           TO  SPA-GMN-PTNUM
                       MOVE    WRK-PTID            TO  SPA-GMN-PTID
                       MOVE    HZN-PTINF-REC       TO  PTINF-REC
      *                排他制御処理
                       PERFORM 990-LOCK-IN-SEC
      *
                       IF     (SPA-ERRCD           =   SPACE )
                       AND    (SPA-NAI-YYKUKE-KBN  NOT =   "0"  )
      *                    予約有無チェック処理
                           PERFORM 41012-PTNUM-YYKCHEK-SEC
                       END-IF
                       IF     (SPA-ERRCD           =   SPACE )
                         AND  (SPA-U2ID1CD         =   SPACE )
                           PERFORM 41011-PTNUM-HENSYU-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    3                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4102-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者検索処理
      *****************************************************************
       41021-KNJKEN-HEN-SEC        SECTION.
      *
      *    全角チェック
           IF      SPA-ENCODING        =   2
      *        拡張漢字対応
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"                 TO  KANACHK2-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"      USING
                                           ORCSKANACHK2AREA
               IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                      (KANACHK2-SYUBETU    =   1     )
                   MOVE    KANACHK2-MAE-INPUT  TO  SPA-GMN-NAME
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    KANACHK2-OUT-INPUT   TO  SPA-GMN-NAME
               END-IF
           ELSE
      *        拡張漢字不可
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-NAME        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-RC      NOT =   ZERO  )  OR
                      (KANACHK-SYUBETU     =   1     )
                   MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-NAME
                   MOVE    "0003"              TO  SPA-ERRCD
               ELSE
                   MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-NAME
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41021-KNJKEN-HEN-EXT
           END-IF
      *
           MOVE    SPA-GMN-NAME        TO  WRK-NAME-KEY
      **** IF      KANACHK2-MAX        <   50
      *        COMPUTE IDX                 =   KANACHK-MAX  +  1
      *        MOVE    "%"                 TO  WRK-NAME-KEY(IDX:1)
      *****END-IF
      *    部分検索
           PERFORM 201-NAME-HEN-SEC
      *    漢字検索
           INITIALIZE                  PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPACE               TO  PTINF-NAME
           MOVE    WRK-NAME-KEY        TO  PTINF-NAME
           MOVE    WRK-NAME-KEY        TO  PTINF-KANANAME
      *
      *    患者マスターを漢字氏名で検索
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           MOVE    ZERO                TO  IDZ
           PERFORM UNTIL      (IDZ             >   1         ) OR
                              (FLG-PTINF       =   1         )
               ADD     1                   TO  IDZ
               IF      IDZ                 =   1
                   PERFORM 41021-PTINF-HENSYU-SEC
               END-IF
      *
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41021-KNJKEN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    氏名部分検索編集処理
      *****************************************************************
       201-NAME-HEN-SEC                 SECTION.
      *
           MOVE    SPACE               TO  WRK-NAME1
           MOVE    SPACE               TO  WRK-NAME2
           MOVE    SPA-GMN-NAME        TO  WRK-NAME
      *    ＊の次の　削除
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "sub"               TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊　"              TO  ORCSSTR-STRING2
           MOVE    "＊"                TO  ORCSSTR-STRING3
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           MOVE    ORCSSTR-STRING1     TO  WRK-NAME
      *    ＊は名前とする
           INITIALIZE                  ORCSSTRINGAREA
           MOVE    "search"            TO  ORCSSTR-COMMAND
           MOVE    WRK-NAME            TO  ORCSSTR-STRING1
           MOVE    "＊"                TO  ORCSSTR-STRING2
           CALL    "orcsstring"        USING
                                       ORCSSTRINGAREA
           IF      ORCSSTR-NUM1        =   ZERO
               MOVE    WRK-NAME            TO  WRK-NAME1
           ELSE
               MOVE    ORCSSTR-NUM1        TO  IDX-S1
               MOVE    ORCSSTR-NUM2        TO  IDX-S2
               IF      ORCSSTR-NUM1        =   1
                   MOVE    SPACE               TO  WRK-NAME1
               ELSE
                   INITIALIZE                  ORCSSTRINGAREA
                   MOVE    "substr"            TO  ORCSSTR-COMMAND
                   MOVE    WRK-NAME            TO  ORCSSTR-STRING1
                   MOVE    1                   TO  ORCSSTR-NUM1
                   COMPUTE ORCSSTR-NUM2        =   IDX-S1
                                               -   1
                   CALL    "orcsstring"        USING
                                               ORCSSTRINGAREA
                   MOVE    ORCSSTR-STRING2     TO  WRK-NAME1
               END-IF
      *
               INITIALIZE                  ORCSSTRINGAREA
               MOVE    "substr"            TO  ORCSSTR-COMMAND
               MOVE    WRK-NAME            TO  ORCSSTR-STRING1
               COMPUTE ORCSSTR-NUM1        =   IDX-S1
                                           +   1
               MOVE    0                   TO  ORCSSTR-NUM2
               CALL    "orcsstring"        USING
                                           ORCSSTRINGAREA
               MOVE    ORCSSTR-STRING2     TO  WRK-NAME2
           END-IF
      **** MOVE    1                   TO  IDX2
      *    PERFORM VARYING     IDX     FROM    1   BY  2
      *            UNTIL       IDX     >   KANACHK-MAX
      *        IF      SPA-GMN-NAME(IDX:2) =   "＊"
      *            COMPUTE IDX3            =   IDX     +  2
      *            IF      SPA-GMN-NAME (IDX3:2)    =   "　"
      *                COMPUTE IDX3            =   IDX3     +  2
      *            END-IF
      *            IF      SPA-GMN-NAME (IDX3:) =   SPACE
      *                MOVE    SPACE               TO  WRK-NAME2
      *            ELSE
      *                MOVE    SPA-GMN-NAME(IDX3:) TO  WRK-NAME2
      *            END-IF
      *:           MOVE    KANACHK-MAX             TO  IDX
      *        ELSE
      *            MOVE    SPA-GMN-NAME(IDX:2)     TO  WRK-NAME1
      *                                                        (IDX2:2)
      *            ADD     2                       TO  IDX2
      *        END-IF
      **** END-PERFORM
      *
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2           =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
           IF     (WRK-NAME1       NOT =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  WRK-NAME1       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
           IF     (WRK-NAME1           =   SPACE ) AND
                  (WRK-NAME2       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-NAME-KEY
               STRING  "%"             DELIMITED  BY  SIZE
                       "　"            DELIMITED  BY  SIZE
                       WRK-NAME2       DELIMITED  BY  SPACE
                       "%"             DELIMITED  BY  SIZE
                                       INTO    WRK-NAME-KEY
               END-STRING
           END-IF
      *
           .
       201-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスタ編集処理
      *****************************************************************
       41021-PTINF-HENSYU-SEC          SECTION.
      *
           MOVE    PTINF-PTID          TO  WRK-PTID
      *    患者番号変換より、患者NOより患者IDを取得する
           INITIALIZE                      ORCSPTIDAREA
           MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
           MOVE    PTINF-PTID          TO  SPTID-PTID
           CALL    "ORCSPTID"          USING  ORCSPTIDAREA
                                              SPA-AREA
           IF      SPTID-RC        NOT =   ZERO
               MOVE    SPACE           TO  WRK-PTNUM
           ELSE
               MOVE    SPTID-PTNUM     TO  WRK-PTNUM
           END-IF
      *
           MOVE    PTINF-REC           TO  HZN-PTINF-REC
      *
           .
       41021-PTINF-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力
      *****************************************************************
       4103-SRYKA-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-SRYKA       =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO                TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYKA-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKALST
                                                       (IDY)(1:2) 
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                               TO  SPA-GMN-SRYKA-G
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0019"              TO  SPA-ERRCD
                   MOVE    4                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-GMN-SRYKA   NOT =   SPACE  )  AND
                  (SPA-GMN-SRYKA   NOT =   SPA-MAE-SRYKA)
      *        診療科が変わった時、ドクターを再編集する
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
               IF      ORCSDR-DRCD-IDX     >   ZERO
                   MOVE    SPA-GMN-DRCDLST  (ORCSDR-DRCD-IDX)
                                           TO  SPA-GMN-DRCD-G
               END-IF
      *
      *        受診履歴よりドクター、保険組合せを決定する
               PERFORM 41031-JYURRK-SRYKA-SEC
      *
               MOVE    ZERO                TO  SPA-NAI-KEIFLG
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           .
       4103-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科より保険組合せ・ドクター決定処理
      *****************************************************************
       41031-JYURRK-SRYKA-SEC               SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  JYURRK-PTID
           MOVE    SPA-SYSYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  JYURRK-SRYKA
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key30"             TO  MCP-PATHNAME
               PERFORM 900-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key30"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               MOVE    JYURRK-DRCD         TO  WRK-JRK-DRCD
      *        保険組合番号
               MOVE    JYURRK-HKNCOMBINUM  TO  WRK-JRK-HKNCOMBI
           ELSE
               MOVE    SPACE               TO  WRK-JRK-DRCD
               MOVE    ZERO                TO  WRK-JRK-HKNCOMBI
           END-IF
      *    予約からはドクター変更なし
           IF     (FLG-DRSET           =   1   )   AND
                  (SPA-GMN-DRCD    NOT =   SPACE)
               MOVE    "1"                 TO  WRK-JRK-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  WRK-JRK-DRCD(2:4)
           END-IF
      *
      *    ドクター、保険組合せ編集
           PERFORM 4101161-DRCD-HKNCOMBI-SEC
      *
           .
       41031-JYURRK-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力
      *****************************************************************
       4105-DRCD-CHK-SEC               SECTION.
      *
           IF      SPA-GMN-DRCD        =   SPACE
               GO      TO      4105-DRCD-EXT
           END-IF
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-DRCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療内容入力
      *****************************************************************
       4106-SRYNAIYO-CHK-SEC           SECTION.
      *
      *
           IF      SPA-GMN-SRYNAIYOCD        =   SPACE
               CONTINUE
           ELSE
               MOVE    ZERO               TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYNAIYO-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      SPA-GMN-SRYNAIYOCD
                                      =   SPA-GMN-SRYNAILST(IDY)(1:2)
                       MOVE    SPA-GMN-SRYNAILST (IDY)
                                           TO  SPA-GMN-SRYNAIYOCD-G
                       MOVE    1           TO  FLG-HENFLG
                   END-IF
               END-PERFORM
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0021"              TO  SPA-ERRCD
                   MOVE    6                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *            
           .
       4106-SRYNAIYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ入力
      *****************************************************************
       4107-KARUTE-CHK-SEC               SECTION.
      *
           EVALUATE    SPA-GMN-KARUTE
               WHEN    "0"
               WHEN    "2"
                   CONTINUE
               WHEN    "1"
               WHEN    "3"
                   IF      SPA-COMBI-MAX       =   ZERO
                       CONTINUE
                   ELSE
      *                保険組合せが１件の時、１とする
                       IF      SPA-COMBI-MAX       =   1
                           MOVE    SPA-GMN-HKNCOMBINUM (01)
                                               TO  SPA-GMN-COMBINUM
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0010"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
           END-EVALUATE
           .
       4107-KARUTE-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方箋入力
      *****************************************************************
       4108-SYOHO-CHK-SEC               SECTION.
      *
           EVALUATE    SPA-GMN-SYOHO
               WHEN    "0"
                   CONTINUE
               WHEN    "1"
                   IF      SPA-COMBI-MAX       =   ZERO
                       CONTINUE
                   ELSE
      *                保険組合せが１件の時、１とする
                       IF      SPA-COMBI-MAX       =   1
                           MOVE    SPA-GMN-HKNCOMBINUM (01)
                                               TO  SPA-GMN-COMBINUM
                           MOVE    62                  TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0012"              TO  SPA-ERRCD
                   MOVE    8                   TO  SPA-GMN-CUR
           END-EVALUATE
      *---- (01.00.01) LINE MOD END -----------------------------------
           .
       4108-SYOHO-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組み合わせ選択 チェック処理
      *****************************************************************
       4109-HKNCOMBI-CHK-SEC           SECTION.
      *
      *    選択番号チェック
           MOVE    ZERO                TO  FLG-HENFLG
           IF      SPA-GMN-COMBINUM    =   ZERO
               MOVE    1                   TO  FLG-HENFLG
           END-IF
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   SPA-COMBI-MAX )
                          OR  (FLG-HENFLG      =   1     )
               IF      SPA-GMN-COMBINUM    =   SPA-GMN-HKNCOMBINUM
                                                                (IDY)
                   MOVE    1                   TO  FLG-HENFLG
                   MOVE    SPA-COMBI-MAX       TO  IDY
               END-IF
           END-PERFORM
           IF      SPA-GMN-COMBINUM    =   ZERO
               CONTINUE
           ELSE
               IF      FLG-HENFLG          =   ZERO
                   MOVE    "0014"              TO  SPA-ERRCD
                   MOVE    9                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4109-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4109-HKNCOMBI-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-COMBI-MAX
              IF      U02-HKNCOMBI-SELECT (IDX)   =  "T"
                  IF      SPA-GMN-HKNCOMBINUM (IDX)
                                               =   SPA-GMN-COMBINUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-HKNCOMBINUM (IDX)
                                              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM      TO  SPA-GMN-COMBINUM
               MOVE    ZERO            TO  SPA-NAI-KEIFLG3
           END-IF
      *
           IF      SPA-GMN-COMBINUM    NOT =   ZERO
      *        組合せ
               PERFORM 4109-HKNCOMBI-CHK-SEC
           END-IF
      *R01.11
      *    カーソル移動
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-USERPG-MAX  =   1
                   MOVE    62              TO  SPA-GMN-CUR
               ELSE
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4109-HKNCOMBI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｕ・Ｐチェック処理
      *****************************************************************
       4109-USERPG-CHK-SEC        SECTION.
      *
           MOVE    ZERO               TO  FLG-HENFLG
           PERFORM VARYING IDY         FROM   1   BY  1
                   UNTIL  (IDY         >   SPA-USERPG-MAX )
                   OR     (FLG-HENFLG  =   1  )
               IF      SPA-GMN-USERPG   =   SPA-GMN-USERPGLST(IDY)(1:1)
                   MOVE    SPA-GMN-USERPGLST (IDY)
                                           TO  SPA-GMN-USERPG-G
                   MOVE    1           TO  FLG-HENFLG
               END-IF
           END-PERFORM
           IF      FLG-HENFLG          =   ZERO
              AND  SPA-USERPG-MAX      >   ZERO
               MOVE    "0029"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4109-USERPG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約選択 チェック処理
      *****************************************************************
       4104-SELNUM-CHK-SEC        SECTION.
      *
      *    未入力の時、患者番号へ
           IF      SPA-GMN-SELNUM      =   ZERO
               MOVE    2                   TO  SPA-GMN-CUR
               IF      SPA-NAI-SELNUM  NOT =   ZERO
                   PERFORM 493-CLEAR-SEC
               END-IF
           ELSE
      *
      *    選択番号チェック
               IF      SPA-GMN-SELNUM      >=  1   AND  <=  SPA-UKE-MAX
                   CONTINUE
               ELSE
                   MOVE    "0015"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *H28.12
           IF      (SPA-ERRCD          =   SPACE )  AND
                   (SPA-GMN-SELNUM     >   ZERO  )
               IF      SPA-NAI-UKEUP       =   1
      *            受付順変更
                   PERFORM 41042-SELNUM-UKEUPCHK-SEC
      *R0.11       選択番号へ
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO    4104-SELNUM-EXT
                END-IF
            END-IF
      *
           IF      (SPA-ERRCD          =   SPACE )  AND
                   (SPA-GMN-SELNUM     >   ZERO  )
               PERFORM 41041-SELNUM-SYORI-SEC
           END-IF
           .
       4104-SELNUM-EXT.
           EXIT.
      *****************************************************************
      *    予約・受付選択処理
      *****************************************************************
       41041-SELNUM-SYORI-SEC           SECTION.
      *
           MOVE    SPA-GMN-SELNUM          TO  SPA-NAI-SELNUM
           INITIALIZE                      SPA-GMN-AREA
           INITIALIZE                      SPA-NAIB-AREA
      *    受付一覧より編集
           MOVE    SPA-GMN-SELNUM          TO  IDX
           MOVE    SPA-GMN-TPTNUM   (IDX)  TO  SPA-GMN-PTNUM
           MOVE    SPA-GMN-TPTID    (IDX)  TO  SPA-GMN-PTID
      *
           MOVE    SPA-GMN-TUKEID   (IDX)  TO  SPA-NAI-UKEID
           MOVE    SPA-GMN-TKYYKTIME(IDX)  TO  SPA-NAI-KYYKTIME
           MOVE    SPA-GMN-TYYKID   (IDX)  TO  SPA-NAI-YYKID
      *
           MOVE    SPA-GMN-TNAME    (IDX)  TO  SPA-GMN-NAME
           MOVE    SPA-GMN-TSRYKA-G (IDX)  TO  SPA-GMN-SRYKA-G
           MOVE    SPA-GMN-TDRCD-G  (IDX)  TO  SPA-GMN-DRCD-G
           MOVE    SPA-GMN-TSRYNAIYOCD-G (IDX)
                                       TO  SPA-GMN-SRYNAIYOCD-G
           PERFORM 41041-DRCDLIST-HEN-SEC
      *    前回来院日
           MOVE    SPACE               TO  SPA-NAI-NYUINKBN
           IF      SPA-GMN-TRRKYMD (IDX)   NOT =   SPACE
               IF      SPA-GMN-TNYUKBN (IDX)   =   "1"
                   STRING  "病棟："
                           SPA-GMN-TRRKYMD (IDX)   DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD
                   END-STRING
                   MOVE    "1"                 TO  SPA-NAI-NYUINKBN
               ELSE
                   STRING  "前回："
                           SPA-GMN-TRRKYMD (IDX)   DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-GMN-TPTID (IDX) TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF               =   ZERO
               MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
               PERFORM 500-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
               MOVE    PTINF-BIRTHDAY      TO  SPA-NAI-BIRTHDAY
               MOVE    PTINF-SEX           TO  SPA-NAI-SEX
               EVALUATE    SPA-NAI-SEX
               WHEN    "1"
                    MOVE    "男"                TO  SPA-GMN-SEX
                WHEN    "2"
                    MOVE    "女"                TO  SPA-GMN-SEX
               END-EVALUATE
      *        年齢計算
               PERFORM 41011-NENREI-SET-SEC
           END-IF
           IF      SPA-GMN-THKNCOMBI(IDX)  =   SPACE  OR  ZERO
               MOVE    ZERO                TO  SPA-GMN-COMBINUM
           ELSE
               MOVE    SPA-GMN-THKNCOMBI(IDX)  TO  SPA-GMN-COMBINUM
           END-IF
      *    Ｕ・Ｐ
           IF     (SPA-NAI-GYOUMU-FLG  =   "1" ) AND
                  (SPA-USERPG-MAX      >   1   )
               MOVE    SPA-GMN-USERPGLST (2)   TO  SPA-GMN-USERPG-G
           ELSE
               MOVE    SPA-GMN-USERPGLST (1)   TO  SPA-GMN-USERPG-G
           END-IF
      *
      *    登録済みの患者の時、保険情報をセットする
           IF      SPA-GMN-PTID        NOT =  ZERO
      *        排他制御処理
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
      *
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 410114-KUMIHYOJI-SEC
                   PERFORM 410115-KAKUHYOJI-SEC
      *選択ゼロはない
      *             IF      SPA-GMN-SELNUM          =   ZERO
      *                 PERFORM 410116-JYURRKSET-SEC
      *             END-IF
                   IF     (SPA-GMN-TUKETIME(SPA-GMN-SELNUM)
                                                   =   SPACE )
                      AND (SPA-NAI-YYKID       NOT =   ZERO  )
                      AND (SPA-GMN-COMBINUM        =   ZERO  )
      *                予約の時保険組合せ自動設定
                       IF      SPA-NAI-UKEHKNCHK   =   "2"
                           PERFORM 410419-HKNCOMBISET-SEC
                       END-IF
                   END-IF
      *            患者メモ表示処理
                   IF      SPA-GMN-PTID    NOT =   ZERO
                       PERFORM 410119-PTMEMOINF-SEC
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        前回編集
               MOVE    SPA-GMN-PTNUM       TO  SPA-MAE-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
               MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
      *        前回患者ヘ編集
               IF      SPA-GMN-PTID    NOT =  ZERO
                   MOVE    SPA-GMN-PTNUM       TO  SPA-LAST-PTNUM
                   MOVE    SPA-GMN-PTID        TO  SPA-LAST-PTID
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-TUKETIME(SPA-GMN-SELNUM)    =  SPACE
      *         受付完了へ
                   MOVE    62                  TO  SPA-GMN-CUR
      *            予約分の時、保険組合せへ
                   IF      SPA-NAI-YYKID   NOT =   ZERO
                       MOVE    09                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF     (SPA-GMN-PTID        =   ZERO  )
      *                権限がある場合
                    AND   (SPA-GSRAUTH-12      =   "1"  )
      *                患者登録へ
                       MOVE    58                  TO  SPA-GMN-CUR
                   ELSE
      *                診療科へ
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       41041-SELNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター一覧編集処理
      *****************************************************************
       41041-DRCDLIST-HEN-SEC           SECTION.
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           .
       41041-DRCDLIST-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約の時保険組合せ自動設定処理
      *****************************************************************
       410419-HKNCOMBISET-SEC           SECTION.
      *
      *    受診履歴より保険組合せを決定する（ドクターなし）
           MOVE    1                   TO  FLG-DRSET
           PERFORM 41031-JYURRK-SRYKA-SEC
           .
       410419-HKNCOMBISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧選択処理
      *****************************************************************
       4110-UKETUKE-LIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-UKE-MAX
              IF      U02-UKETUKE-SELECT (IDX)   =  "T"
                  IF      IDX                 =   SPA-GMN-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX             TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM      NOT =   ZERO
               MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
           END-IF
      *
           IF      SPA-GMN-SELNUM      NOT =   ZERO
               PERFORM 4104-SELNUM-CHK-SEC
           END-IF
      *
           .
       4110-UKETUKE-LIST-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    保険組合せ表示処理
      *****************************************************************
       410114-KUMIHYOJI-SEC           SECTION.
      *
           MOVE    1                   TO   FLG-GMN-INIT2
      *   保険組合せ読み出し
           INITIALIZE                  SPA-GMN-COMBI
           MOVE   ZERO                 TO  SPA-COMBI-MAX
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           MOVE    ZERO                TO  IDX
      **** PERFORM UNTIL       (IDX            =   15  )  OR
           PERFORM UNTIL       (IDX            =   30  )  OR
                               (FLG-HKNCOMBI   =   1    )
               MOVE    ZERO                TO  FLG-HKNOK
               IF     (SPA-SYSYMD          >=  COMB-TEKSTYMD) AND
                      (SPA-SYSYMD          <=  COMB-TEKEDYMD) AND
                      (COMB-DELKBN         =   SPACE        )
      *                非表示区分
                 AND  (COMB-HYOJIKBN   NOT =   "1"          )
                   MOVE    1               TO  FLG-HKNOK
      *            労災保険情報の判定
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
                       PERFORM 410114-RSIINF-CHK-SEC
                       IF      FLG-PTRSIINF    =   1
                           MOVE    ZERO            TO  FLG-HKNOK
                       END-IF
                   END-IF
               END-IF
               IF      FLG-HKNOK           =   1
      *---- (01.00.02) LINE MOD END   ---------------------------------
                   ADD     1                   TO  IDX
                   MOVE    IDX                 TO  SPA-COMBI-MAX
      *
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-GMN-HKNCOMBINUM
                                                                   (IDX)
                   MOVE    COMB-SYU-TANSEIDONAME
                                    TO  SPA-GMN-SYU-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH1-TANSEIDONAME  
                                    TO  SPA-GMN-KOH1-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH2-TANSEIDONAME  
                                    TO  SPA-GMN-KOH2-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH3-TANSEIDONAME  
                                    TO  SPA-GMN-KOH3-TANSEIDONAME(IDX)
                   MOVE    COMB-KOH4-TANSEIDONAME  
                                    TO  SPA-GMN-KOH4-TANSEIDONAME(IDX)
      *
                   MOVE    COMB-HKNNUM     TO  SPA-NAI-HKNNUM  (IDX)
                   MOVE    COMB-HKNID      TO  SPA-NAI-COB-HKNID (IDX)
                   MOVE    COMB-TEKSTYMD   TO  SPA-NAI-TEKSTYMD (IDX)
                   MOVE    COMB-TEKEDYMD   TO  SPA-NAI-TEKEDYMD (IDX)
                   MOVE    COMB-HYOJIKBN   TO  SPA-NAI-THYOJIKBN (IDX)
      *            労災の時、部位を公費１の項目へ
                   IF      COMB-HKNNUM         =   "971"  OR  "973"
                       PERFORM 4101152-RSI-HENSYU-SEC
                   END-IF
      *H25.7
      *            第三者行為の時、部位を公費２の項目へ
                   IF      COMB-KOH1HKNNUM     =   "970"
                       PERFORM 4101152-DAI3-HENSYU-SEC
                   END-IF
      *
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410114-KUMIHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険情報の判定処理
      *****************************************************************
       410114-RSIINF-CHK-SEC           SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       410114-RSIINF-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    未確認保険公費表示処理
      *****************************************************************
       410115-KAKUHYOJI-SEC           SECTION.
      *
           INITIALIZE                  SPA-GMN-MIKAKUNIN
           INITIALIZE                  SPA-NAI-MIKAKUNIN
           MOVE    ZERO                TO  SPA-MIKAKU-MAX
           MOVE    ZERO                TO  IDX
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *
      *   保険読み出し
           INITIALIZE                  PTHKNINF-REC
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTHKNINF
           END-IF
      *
           MOVE    ZERO                TO  IDX 
           PERFORM UNTIL       (IDX            =   10   )  OR
                               (FLG-PTHKNINF   =   1    )
               IF     (SPA-SYSYMD          >=  PTHKN-TEKSTYMD ) AND
                      (SPA-SYSYMD          <=  PTHKN-TEKEDYMD ) AND
                      (SPA-SYSYMD(1:6)     >   PTHKN-KAKUNINYMD
                                                         (1:6)) AND
                      (PTHKN-SAKJOKBN      =   SPACE    )
      *
      *            保険番号読込み  
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                   MOVE    PTHKN-HKNNUM        TO  HKN-HKNNUM
                   MOVE    PTHKN-TEKSTYMD      TO  HKN-TEKSTYMD
                   MOVE    PTHKN-TEKEDYMD      TO  HKN-TEKEDYMD
                   MOVE    "00"                TO  HKN-PAYKBN 
                   PERFORM 900-HKNNUM-INV-SEC
      *            労災の時、対象外
                   IF    ( FLG-HKNNUM          =   ZERO   )  AND
                         ( PTHKN-HKNNUM    NOT =   "971" AND
                                                   "973"  )
                       ADD     1                   TO  IDX
                       MOVE    IDX                 TO  SPA-MIKAKU-MAX
                       MOVE    HKN-TANSEIDONAME    TO
                                                   SPA-GMN-TMIKAKUNAME
                                                                  (IDX)
                       MOVE    PTHKN-KAKUNINYMD    TO  WRK-SYMD
                       PERFORM 500-SEIWA-HEN-SEC
                       MOVE    WRK-HENYMDG         TO
                                                   SPA-GMN-TMIKAKUYMD
                                                                (IDX)
      *                記号・番号
                       IF     (PTHKN-KIGO          =   SPACE )  AND
                              (PTHKN-NUM           =   SPACE )
                           CONTINUE
                       ELSE
                           STRING  PTHKN-KIGO      DELIMITED  BY  SPACE
                                   "・"            DELIMITED  BY  SIZE
                                   PTHKN-NUM       DELIMITED  BY  SPACE
                                   INTO            SPA-GMN-TMIKIGONUM
                                                                (IDX)
                           END-STRING
                       END-IF
      *                後期高齢者は・なし
                       IF     (PTHKN-HKNNUM        =   "039")
                          AND (PTHKN-KIGO          =   SPACE )
                           MOVE    PTHKN-NUM   TO  SPA-GMN-TMIKIGONUM
                                                                (IDX)
                       END-IF
      *
                       MOVE    PTHKN-KAKUNINYMD    TO
                                                   SPA-NAI-KAKUNINYMD
                                                                 (IDX)
                       MOVE    PTHKN-HKNID         TO  SPA-NAI-HKNID
                                                                 (IDX)
                       MOVE    1                   TO  SPA-NAI-HKNKBN
                                                                 (IDX)
                       MOVE    PTHKN-KAKUNINYMD    TO
                                                   SPA-MAE-KAKUNINYMD
                                                                 (IDX)
                       MOVE    SPA-GMN-TMIKAKUYMD (IDX)
                                                   TO
                                                   SPA-MAE-KAKUNINYMDW
                                                                 (IDX)
                   END-IF
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTHKNINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費期限切れ判定追加
           MOVE    SPACE               TO  SPA-GMN-KOHMSG
           INITIALIZE                      WRK-KOH-AREA
      *    システム日付＋１ヵ月
           MOVE    SPA-SYSYMD(1:6)     TO  WRK-KEI-YM (1:6)
           IF      WRK-KEI-MM          =   12
               COMPUTE WRK-KEI-YY      =   WRK-KEI-YY   +   1
               MOVE    1                   TO  WRK-KEI-MM
           ELSE
               COMPUTE WRK-KEI-MM      =   WRK-KEI-MM   +   1
           END-IF
      *
      *    公費読み出し
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           PERFORM UNTIL      (FLG-PTKOHINF   =   1    )
      *
               IF     (SPA-SYSYMD      >=  PTKOH-TEKSTYMD )      AND
                      (SPA-SYSYMD      <=  PTKOH-TEKEDYMD )      AND
                      (SPA-SYSYMD(1:6) >   PTKOH-KAKUNINYMD(1:6)) AND 
                      (PTKOH-SAKJOKBN  =   SPACE           )
      *H25.7
      *            第三者行為対象外
                 AND  (PTKOH-KOHNUM    NOT =   "970"       )
                   IF      IDX             <   10
      *                未確認公費
                       PERFORM 4101151-KOHMIKAKU-SEC
                   END-IF
               END-IF
      *
      *        公費期限切れテーブル設定
               IF     (SPA-SYSYMD      <=  PTKOH-TEKEDYMD )      AND
                      (PTKOH-SAKJOKBN  =   SPACE          )
                   PERFORM 4101152-KOHTEKEDHEN-SEC
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-PTKOHINF-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費期限切れ判定
           IF      IDX-K1              >   ZERO
               PERFORM 4101153-KOHTEKEDCHK-SEC
           END-IF
           .
       410115-KAKUHYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    未確認公費編集処理
      *****************************************************************
       4101151-KOHMIKAKU-SEC           SECTION.
      *
      *    保険番号読込み  
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    PTKOH-KOHNUM        TO  HKN-HKNNUM
           MOVE    PTKOH-TEKSTYMD      TO  HKN-TEKSTYMD
           MOVE    PTKOH-TEKEDYMD      TO  HKN-TEKEDYMD
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                  TO  FLG-HKNNUM
           END-IF
      *
           PERFORM UNTIL       FLG-HKNNUM      =   1
               IF      HKN-HKNKOHSBTKBN  =   "3"
                   IF      HKN-TEKSTYMD    <   "20021001"
                       IF     (SPA-HOSPSBT     =   HKN-PAYKBN (1:1))
                       AND    (SPA-ROUPAYKBN   =   HKN-PAYKBN (2:1))
                           PERFORM 4101153-HOKI-HENSYU-SEC
                       END-IF
                   ELSE
                       IF      HKN-PAYKBN      =   PTKOH-PAYKBN
                           PERFORM 4101153-HOKI-HENSYU-SEC
                       END-IF
                   END-IF
               ELSE
                   PERFORM 4101153-HOKI-HENSYU-SEC
               END-IF
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 900-HKNNUM-READ-SEC
               END-IF 
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4101151-KOHMIKAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の時、部位を追加編集処理
      *****************************************************************
       4101152-RSI-HENSYU-SEC           SECTION.
      *
      *    労災の時、部位を追加
      *    患者保険情報検索
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    COMB-HKNID          TO  PTRSI-HKNID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKBN-MEI
           EVALUATE    PTRSI-HKNKBN
               WHEN    "1"
                   MOVE    "短"        TO  WRK-HKNKBN-MEI
               WHEN    "2"
                   MOVE    "傷"        TO  WRK-HKNKBN-MEI
               WHEN    "3"
                   MOVE    "ア"        TO  WRK-HKNKBN-MEI
               WHEN    "5"
                   MOVE    "公"        TO  WRK-HKNKBN-MEI
           END-EVALUATE
           IF      PTRSI-COMMENT       =   SPACE
               MOVE    WRK-HKNKBN-MEI  TO  SPA-GMN-KOH1-TANSEIDONAME
                                                                   (IDX)
           ELSE
               MOVE    SPACE           TO  SPA-GMN-KOH1-TANSEIDONAME
                                                                   (IDX)
               STRING  WRK-HKNKBN-MEI          DELIMITED  BY  SIZE
                       "（"                    DELIMITED  BY  SIZE
                       PTRSI-COMMENT           DELIMITED  BY  SPACE
                       "）"                    DELIMITED  BY  SIZE
                       INTO              SPA-GMN-KOH1-TANSEIDONAME (IDX)
               END-STRING
           END-IF
           .
       4101152-RSI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為の時、部位を追加編集処理
      *****************************************************************
       4101152-DAI3-HENSYU-SEC           SECTION.
      *
           INITIALIZE                      PTRSIINF-REC
           MOVE    SPA-HOSPNUM         TO  PTRSI-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTRSI-PTID
           MOVE    ZERO                TO  PTRSI-HKNID
           MOVE    COMB-KOH1ID         TO  PTRSI-KOHID
      *
           MOVE    PTRSIINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 993-PTRSIINF-NEXT-SEC
           ELSE
               INITIALIZE                      PTRSIINF-REC
               MOVE    1                   TO  FLG-PTRSIINF
           END-IF
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      PTRSI-COMMENT   NOT =   SPACE
               MOVE    SPACE           TO  SPA-GMN-KOH2-TANSEIDONAME
                                                                   (IDX)
               STRING  "（"                    DELIMITED  BY  SIZE
                       PTRSI-COMMENT           DELIMITED  BY  SPACE
                       "）"                    DELIMITED  BY  SIZE
                       INTO              SPA-GMN-KOH2-TANSEIDONAME (IDX)
               END-STRING
           END-IF
           .
       4101152-DAI3-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費未確認編集処理
      *****************************************************************
       4101153-HOKI-HENSYU-SEC           SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    IDX                 TO  SPA-MIKAKU-MAX
           MOVE    HKN-TANSEIDONAME    TO  SPA-GMN-TMIKAKUNAME(IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  WRK-SYMD
           PERFORM 500-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-TMIKAKUYMD(IDX)
      *    受給者番号
           MOVE    PTKOH-JKYSNUM       TO  SPA-GMN-TMIKIGONUM (IDX)
      *
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-NAI-KAKUNINYMD (IDX)
           MOVE    PTKOH-KOHID         TO  SPA-NAI-HKNID      (IDX)
           MOVE    2                   TO  SPA-NAI-HKNKBN     (IDX)
           MOVE    PTKOH-KAKUNINYMD    TO  SPA-MAE-KAKUNINYMD (IDX)
           MOVE    SPA-GMN-TMIKAKUYMD (IDX)
                                       TO  SPA-MAE-KAKUNINYMDW (IDX)
      *
           MOVE    1                   TO  FLG-HKNNUM
      *
           .
       4101153-HOKI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費期限切れテーブル設定処理
      *****************************************************************
       4101152-KOHTEKEDHEN-SEC           SECTION.
      *
      *    期限切れ対象
           IF     (PTKOH-TEKEDYMD(1:6)     =   WRK-KEI-YM
                                           OR  SPA-SYSYMD(1:6))
              AND (PTKOH-TEKEDCHKKBN       =   "1"      )
               ADD     1               TO  IDX-K1
               IF      IDX-K1          <=  40
                   MOVE    PTKOH-KOHNUM    TO  TBL-K1-KOHNUM (IDX-K1)
                   MOVE    PTKOH-PAYKBN    TO  TBL-K1-PAYKBN (IDX-K1)
                   MOVE    PTKOH-TEKSTYMD  TO  TBL-K1-TEKSTYMD (IDX-K1)
                   MOVE    PTKOH-TEKEDYMD  TO  TBL-K1-TEKEDYMD (IDX-K1)
               END-IF
           ELSE
               ADD     1               TO  IDX-K2
               IF      IDX-K2          <=  40
                   MOVE    PTKOH-KOHNUM    TO  TBL-K2-KOHNUM  (IDX-K2)
                   MOVE    PTKOH-TEKSTYMD  TO  TBL-K2-TEKSTYMD (IDX-K2)
                   MOVE    PTKOH-TEKEDYMD  TO  TBL-K2-TEKEDYMD (IDX-K2)
               END-IF
           END-IF
           .
       4101152-KOHTEKEDHEN-EXT.
           EXIT.
      *****************************************************************
      *    公費期限切れ判定処理
      *****************************************************************
       4101153-KOHTEKEDCHK-SEC           SECTION.
      *
           PERFORM VARYING     IDX-K1  FROM    1   BY  1
                   UNTIL      (IDX-K1  >   40  )  OR
                              (TBL-K1-KOHNUM (IDX-K1)  =   SPACE)
                          OR  (SPA-GMN-KOHMSG  NOT =   SPACE )
               MOVE    ZERO            TO  FLG-KOH
               PERFORM VARYING     IDX-K2  FROM    1   BY  1
                       UNTIL      (IDX-K2  >   40  )  OR
                                  (TBL-K2-KOHNUM (IDX-K2)  =   SPACE) OR
                                  (FLG-KOH     =   1  )
                   IF     (TBL-K1-KOHNUM (IDX-K1)
                                           =   TBL-K2-KOHNUM (IDX-K2))
                      AND (TBL-K1-TEKEDYMD (IDX-K1)
                                           <   TBL-K2-TEKSTYMD (IDX-K2))
                       MOVE    1           TO  FLG-KOH
                   END-IF
               END-PERFORM
               IF      FLG-KOH             =   ZERO
      *            保険番号読込み  
                   INITIALIZE                  HKNNUM-REC
                   MOVE    SPA-HOSPNUM              TO  HKN-HOSPNUM
                   MOVE    TBL-K1-KOHNUM  (IDX-K1)  TO  HKN-HKNNUM
                   MOVE    TBL-K1-PAYKBN  (IDX-K1)  TO  HKN-PAYKBN
                   MOVE    TBL-K1-TEKSTYMD (IDX-K1) TO  HKN-TEKSTYMD
                   MOVE    TBL-K1-TEKEDYMD (IDX-K1) TO  HKN-TEKEDYMD
                   PERFORM 900-HKNNUM-INV-SEC
                   MOVE    TBL-K1-TEKEDYMD (IDX-K1)    TO  WRK-SYMD
                   PERFORM 500-SEIWA-HEN-SEC
                   STRING "公費："
                          HKN-TANSEIDONAME     DELIMITED  BY  SPACE
                          "　有効期限："
                           WRK-HENYMDG         DELIMITED  BY  SIZE
                                           INTO  SPA-GMN-KOHMSG
                   END-STRING
               END-IF
           END-PERFORM
           .
       4101153-KOHTEKEDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴表示処理
      *****************************************************************
       410116-JYURRKSET-SEC           SECTION.
      *
      *    前回来院日編集
           INITIALIZE                  ORCSLASTYMDAREA
           MOVE    SPA-GMN-PTID        TO  ORCSLASTYMD-PTID
           CALL    "ORCSLASTYMD"       USING
                                       ORCSLASTYMDAREA
                                       SPA-AREA
      *
      *    入院中は病棟を表示
           MOVE    SPACE               TO  SPA-NAI-NYUINKBN
           IF      ORCSLASTYMD-BRMNUM  =   SPACE
               IF      ORCSLASTYMD-LASTYMD NOT =   SPACE
                   STRING  "前回："
                           ORCSLASTYMD-LASTYMDW    DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-LASTYMD
                   END-STRING
               END-IF
           ELSE
               STRING  "病棟："
                       ORCSLASTYMD-BRMNUM  DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-LASTYMD
               END-STRING
               MOVE    "1"                 TO  SPA-NAI-NYUINKBN
           END-IF
      *
           INITIALIZE                  WRK-JYURRK-AREA
           MOVE    ORCSLASTYMD-SRYKA   TO  WRK-JRK-SRYKA
           MOVE    ORCSLASTYMD-DRCD    TO  WRK-JRK-DRCD
           IF      ORCSLASTYMD-HKNCOMBI    =   SPACE
               MOVE    ZERO                TO  WRK-JRK-HKNCOMBI
           ELSE
               MOVE    ORCSLASTYMD-HKNCOMBI
                                           TO  WRK-JRK-HKNCOMBI
           END-IF
      *
           IF      WRK-JRK-SRYKA  NOT  =  SPACE
               MOVE    ZERO                TO  FLG-HENFLG
               PERFORM VARYING IDY         FROM   1   BY  1
                       UNTIL  (IDY         >   SPA-SRYKA-MAX )
                       OR     (FLG-HENFLG  =   1  )
                   IF      WRK-JRK-SRYKA   =   SPA-GMN-SRYKALST
                                                       (IDY)(1:2) 
                       MOVE    SPA-GMN-SRYKALST (IDY)
                                               TO  SPA-GMN-SRYKA-G
                       MOVE    1               TO  FLG-HENFLG
                   END-IF
               END-PERFORM
      *
               IF      SPA-GMN-SRYKA       NOT =   SPA-MAE-SRYKA
      *            診療科が変わった時、ドクターを再編集する
                   INITIALIZE                  ORCSDRSETAREA
                   MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
                   MOVE    "1"                 TO  ORCSDR-KBN
                   MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
                   CALL    "ORCSDRSET"         USING
                                               SPA-AREA
                                               ORCSDRSETAREA
                   MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
                   MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
                   IF      ORCSDR-DRCD-IDX     >   ZERO
                       MOVE    SPA-GMN-DRCDLST  (ORCSDR-DRCD-IDX)
                                               TO  SPA-GMN-DRCD-G
                   END-IF
               END-IF
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
      *    ドクター、保険組合せ編集
           PERFORM 4101161-DRCD-HKNCOMBI-SEC
           .
       410116-JYURRKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター、保険組合せ編集処理
      *****************************************************************
       4101161-DRCD-HKNCOMBI-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-DRCD-MAX 
               IF      WRK-JRK-DRCD(2:4)   =   SPA-GMN-DRCDLST(IDX)
                                                                 (1:4)
                   MOVE    SPA-GMN-DRCDLST(IDX)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-DRCD-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               IF      WRK-JRK-DRCD        =   SPACE
                   IF      SPA-GMN-DRCD-G      =   SPACE
                       MOVE    SPA-GMN-DRCDLST(1)  TO  SPA-GMN-DRCD-G
                   END-IF
               ELSE
                   MOVE    WRK-JRK-DRCD(2:4)   TO  SPA-GMN-DRCD
      *           ドクター
                   PERFORM 4105-DRCD-CHK-03-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPA-GMN-DRCDLST(1)
                                                   TO  SPA-GMN-DRCD-G
                       MOVE    SPACE               TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-COMBINUM
      *    削除分ならないことと
           IF      WRK-JRK-HKNCOMBI    NOT =   ZERO
               PERFORM VARYING    IDX-C    FROM    1   BY  1
                       UNTIL      IDX-C    >   SPA-COMBI-MAX
      *            非表示判定
                   IF     (WRK-JRK-HKNCOMBI    =   SPA-GMN-HKNCOMBINUM
                                                          (IDX-C) )
                       MOVE    WRK-JRK-HKNCOMBI    TO  SPA-GMN-COMBINUM
                       MOVE    SPA-COMBI-MAX       TO  IDX-C
                   END-IF
               END-PERFORM
           END-IF
           IF     (SPA-COMBI-MAX       >   ZERO )  AND
                  (SPA-GMN-COMBINUM    =   ZERO )
               PERFORM 4101161-HKNCOMBI-SET-SEC
           END-IF
           .
       4101161-DRCD-HKNCOMBI-EXT.
           EXIT.
      *****************************************************************
      *    ドクター初期表示
      *****************************************************************
       4105-DRCD-CHK-03-SEC               SECTION.
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
           MOVE    "3"                 TO  ORCSDR-KBN
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           IF      ORCSDR-ERRCD    NOT =   ZERO
               MOVE    "0020"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4105-DRCD-CHK-03-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ初期表示処理
      *****************************************************************
       4101161-HKNCOMBI-SET-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
      *
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL       IDX-C   >   SPA-COMBI-MAX
      *      自費は対象外
             IF     (SPA-NAI-HKNNUM (IDX-C)(1:2)   =   "98" )
               CONTINUE
             ELSE
               MOVE    ZERO                TO  WRK-CNT-HKN2
               IF      SPA-NAI-HKNNUM (IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH1-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH2-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH3-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-GMN-KOH4-TANSEIDONAME(IDX-C)  NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
      *
               IF      WRK-CNT-HKN         <=  WRK-CNT-HKN2
                   MOVE    IDX-C               TO  WRK-IDXH
                   MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
               END-IF
      *        入院中は入院非表示対象外
               IF     (SPA-NAI-THYOJIKBN (IDX-C)   =   "2" )
                 AND  (SPA-NAI-NYUINKBN            =   "1" )
                   MOVE    ZERO                TO  WRK-IDXH
                   MOVE    ZERO                TO  WRK-CNT-HKN
               END-IF
      *        入院中以外は外来非表示対象外
               IF     (SPA-NAI-THYOJIKBN (IDX-C)   =   "3" )
                 AND  (SPA-NAI-NYUINKBN            =   SPACE)
                   MOVE    ZERO                TO  WRK-IDXH
                   MOVE    ZERO                TO  WRK-CNT-HKN
               END-IF
      *        労災・自賠責があれば対象とする
               IF      SPA-NAI-HKNNUM (IDX-C)  =   "971"
                                               OR  "973"
                   MOVE    IDX-C               TO  WRK-IDXH
                   MOVE    SPA-COMBI-MAX       TO  IDX-C
               END-IF
             END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    1               TO  IDX-C
           ELSE
               MOVE    WRK-IDXH        TO  IDX-C
           END-IF
           MOVE    SPA-GMN-HKNCOMBINUM (IDX-C)
                                       TO  SPA-GMN-COMBINUM
      *
      *    前回の保険組合せとの判定
           MOVE    ZERO                TO  SPA-HKNCHK-CHK
           MOVE    ZERO                TO  SPA-HKNCHK-FLG
           IF     (WRK-JRK-HKNCOMBI    NOT =   ZERO  )
             AND  (SPA-ZENHKNCHK-FLG       =   "1"
      *R01.5
                                           OR  "2"   )
               PERFORM 41011611-MAE-HKNCOMBICHEK-SEC
           END-IF
      *
           .
       4101161-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回の保険組合せとの判定処理
      *****************************************************************
       41011611-MAE-HKNCOMBICHEK-SEC               SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  COMB-PTID
           MOVE    WRK-JRK-HKNCOMBI    TO  COMB-HKNCOMBINUM
           PERFORM 9001-HKNCOMBI-KEY-READ-SEC
      *
           IF      COMB-DELKBN         =   SPACE
               IF     (COMB-HKNNUM         =   "971"
                                           OR  "973" )
                   CONTINUE
               ELSE
                   IF     (SPA-NAI-TEKSTYMD (IDX-C)
                                               <   COMB-TEKEDYMD)
                       MOVE    1               TO  SPA-HKNCHK-FLG
                   END-IF
               END-IF
      *R01.5
      *        すべてチェックの時
               IF      SPA-ZENHKNCHK-FLG   =   "2"
                   MOVE    1               TO  SPA-HKNCHK-FLG
               END-IF
           ELSE
      *        前回が削除
               MOVE    2                   TO  SPA-HKNCHK-FLG
           END-IF
           .
       41011611-MAE-HKNCOMBICHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者メモ表示処理
      *****************************************************************
       410119-PTMEMOINF-SEC               SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTMEMOINF-PTID
      *D   MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    "1"             TO  SPA-GMN-MEMOKBN
           ELSE
               MOVE    SPACE           TO  SPA-GMN-MEMOKBN
           END-IF
           .
       410119-PTMEMOINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      ***  ＱＲデータ解除処理
      *    IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           IF      SPA-MACHIPG         =   "U02"
      *        待ち受け画面の時、Ｍ０１へ
               MOVE    "M01"               TO  SPA-SAKIPG
      *        診療行為のパラメタ削除
               PERFORM 210-PARA-DEL-SEC
           ELSE
      *        待ち受け以外の時、Ｋ０２へ
               IF      SPA-HZN-MOTOPG      =   SPACE
                   MOVE    "K02"               TO  SPA-SAKIPG
               ELSE
                   MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
               END-IF
           END-IF
      *
           MOVE    "U02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *---- (01.00.05) LINE ADD START ---------------------------------
      *
      *****************************************************************
      *    診療行為のパラメタクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      **                               ORCSFDELAREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    項目クリア
      *****************************************************************
       493-CLEAR-SEC                   SECTION.
      *
      **** ＱＲデータ解除処理
      *    IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
           IF     (SPA-GMN-PTNUM       =   SPACE )
             AND  (SPA-GMN-NAME        =   SPACE )
             AND  (SPA-GMN-CUR         =   2     )
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *    クリア
           PERFORM 1009-HEAD-CLEAR-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *    受付一覧
           MOVE    ZERO                TO  SPA-NAI-SYORI1
           PERFORM 3101-UKETU-HEN-SEC
      **** MOVE    2                   TO  SPA-GMN-CUR
           .
       493-CLEAR-EXT.
           EXIT.
      *---- (01.00.05) LINE ADD END -----------------------------------
      *
      *****************************************************************
      *    受付内容クリア処理
      *****************************************************************
       1009-HEAD-CLEAR-SEC                   SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           INITIALIZE                      SPA-GMN-AREA
           INITIALIZE                      SPA-NAIB-AREA
           MOVE    SPA-GMN-SRYKALST (1)
                                       TO  SPA-GMN-SRYKA-G
           IF      WRK-SRYKA       NOT =   SPA-GMN-SRYKA
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  SPA-MAE-SRYKA
           MOVE    SPA-GMN-DRCDLST  (1)
                                       TO  SPA-GMN-DRCD-G
           MOVE    SPA-GMN-SRYNAILST(1)
                                       TO  SPA-GMN-SRYNAIYOCD-G
           MOVE    SPA-GMN-SYOHOLST (1)
                                       TO  SPA-GMN-SYOHO-G
           MOVE    SPA-GMN-KARUTELST(1)
                                       TO  SPA-GMN-KARUTE-G
      *    Ｕ・Ｐ
           IF     (SPA-NAI-GYOUMU-FLG  =   "1" ) AND
                  (SPA-USERPG-MAX      >   1   )
               MOVE    SPA-GMN-USERPGLST (2)   TO  SPA-GMN-USERPG-G
           ELSE
               MOVE    SPA-GMN-USERPGLST (1)   TO  SPA-GMN-USERPG-G
           END-IF
           .
       1009-HEAD-CLEAR-EXT.
           EXIT.
      *****************************************************************
      *    受付一覧更新処理
      *****************************************************************
       4931-UKELIST-SEC                   SECTION.
      *
      *    選択中でないこと
           IF     (SPA-GMN-SELNUM  NOT =   ZERO )  OR
                  (SPA-GMN-NAME    NOT =   SPACE)
               IF      MCP-WIDGET          =   "B11S"
                   MOVE    "0027"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
               GO      TO      4931-UKELIST-EXT
           END-IF
      *    受付一覧編集
           PERFORM 3101-UKETU-HEN-SEC
      *
           .
       4931-UKELIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者処理
      *****************************************************************
       230-MAEKPTNUM-SEC                   SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      230-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  U02-PTNUM
           MOVE    SPA-LAST-PTNUM      TO  SPA-GMN-PTNUM
           MOVE    1                   TO  FLG-IN-PTNUM
           PERFORM 4101-PTNUM-CHK-SEC
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    前（↑）編集処理
      *****************************************************************
       465-UP-SYORI-SEC             SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               PERFORM 4659-UKETUKE-UP-SYORI-SEC
               GO      TO      465-UP-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      465-UP-SYORI-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    "2001"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       -   1
           END-IF
           .
       465-UP-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次（↑）編集処理
      *****************************************************************
       475-DOWN-SYORI-SEC             SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               PERFORM 4759-UKETUKE-DOWN-SYORI-SEC
               GO      TO      475-DOWN-SYORI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      475-DOWN-SYORI-EXT
           END-IF
      *
           COMPUTE IDY                 =   SPA-GMN-PAGE
                                       *   5
                                       +   1
           IF      IDY                 >   SPA-MIKAKU-MAX
               MOVE    "2002"          TO  SPA-ERRCD
               MOVE    2               TO  SPA-GMN-CUR
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE
                                       +   1
           END-IF
           .
       475-DOWN-SYORI-EXT.
           EXIT.
      *---- (01.00.05) LINE ADD END -----------------------------------
      *
      *****************************************************************
      *    ＱＲ一覧へ
      *****************************************************************
       4105-U02Q-SYORI-SEC             SECTION.
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-U02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGU02Q"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
     *
           IF      SPA-U02Q-MAX        =   ZERO
               MOVE    "0200"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
     *
               MOVE    "U02"               TO  SPA-MOTOPG
               MOVE    "U02Q"              TO  SPA-SAKIPG
               MOVE    "SELNUM"            TO  MCP-WIDGET
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "U02Q"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  FLG-END
           END-IF
      *
           .
       4105-U02Q-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    メモ登録へ
      *****************************************************************
       4105-XF01-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    "0030"          TO  SPA-ERRCD
               GO  TO  4105-XF01-SYORI-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "XF01"              TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
           IF      SPA-GMN-SELNUM      >   ZERO
      *        受付ＩＤ
               MOVE    SPA-GMN-TUKEID  (SPA-GMN-SELNUM)
                                           TO  SPA-P02-UKEID
           END-IF
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "XF01"              TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "XF01  "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4105-XF01-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    予約画面へ
      *****************************************************************
       460-YOYAKU-SEC             SECTION.
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "Y01"               TO  SPA-SAKIPG
           MOVE    "U02"               TO  SPA-MOTOPG3
      *
           INITIALIZE                  SPA-KYOTUKEY
           IF      SPA-GMN-PTID    NOT =   ZERO
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               MOVE    SPA-GMN-SEX         TO  SPA-SEX
               MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
               MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:4)
           END-IF
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "Y01    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       460-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO       TO      480-KENSAKU-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           IF      MCP-WIDGET          =   "B09"
               CONTINUE
           ELSE 
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
           END-IF
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P97    "            TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO  TO  490-UKEICHI-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U01"               TO  SPA-SAKIPG
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
        490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷のみ処理
      *****************************************************************
       495-PRINT-SEC             SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      495-PRINT-EXT
           END-IF
      *
           MOVE    2                   TO  FLG-TOUROKU
      *    登録チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      495-PRINT-EXT
           END-IF
      *    カルテ印刷
           IF      SPA-GMN-KARUTE          =   "1"  OR  "2"
                                           OR  "3"
               IF     (SPA-GMN-PTID            =   ZERO  )  OR
                      (SPA-GMN-SRYKA           =   SPACE )
                   MOVE    "0024"          TO  SPA-ERRCD
                   GO      TO   495-PRINT-EXT
               END-IF
           END-IF
      *    処方箋印刷
           IF      SPA-GMN-SYOHO          =   "1"
               IF     (SPA-GMN-PTID            =   ZERO  )  OR
                      (SPA-GMN-SRYKA           =   SPACE )
                   MOVE    "0025"          TO  SPA-ERRCD
                   GO      TO   495-PRINT-EXT
               END-IF
           END-IF
      *
           IF     (SPA-GMN-KARUTE      =   "0" )  AND
                  (SPA-GMN-SYOHO       =   "0" )
               CONTINUE
           ELSE
      *        帳票印刷処理
               PERFORM 49002-PRINT-SYORI-SEC
           END-IF
      *
           PERFORM 493-CLEAR-SEC
           .
        495-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4901-PRINT-PG-SEC              SECTION.
      *
      *    カルテ
           MOVE    SPACE               TO  WRK-KARUTE-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000001"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-KARUTE-PG
      *
      *    処方せん
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SYSYMD          TO  ORCSPRTNM-SRYYMD
           MOVE    "2"                 TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *    カルテ
           IF      WRK-KARUTE-PG       =   SPACE
               MOVE    "ORCHC01"           TO  WRK-KARUTE-PG
           END-IF
      *
      *    処方せん
           IF      WRK-SYOHOU-PG       =   SPACE
               MOVE    "ORCHC19"           TO  WRK-SYOHOU-PG
           END-IF
      *
           .
      *
       4901-PRINT-PG-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC              SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    登録チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      490-TOUROKU-EXT
           END-IF
           IF      FLG-END             =   1
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続チェック処理
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               MOVE    ZERO                TO  FLG-CLAIM
           ELSE
      *
           IF      SPA-GMN-PTNUM       =   SPACE
      *        新規患者はなし
               MOVE    ZERO                TO  FLG-CLAIM
           ELSE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
      *
           END-IF
      *
           IF      FLG-CLAIM       =   2 
               GO      TO      490-TOUROKU-EXT
           END-IF
      *
           PERFORM 49001-KOUSIN-SEC
           .
       490-TOUROKU-EXT.
           EXIT.   
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       49001-KOUSIN-SEC             SECTION.
      *
      *H25.11
      *    選択番号のＥＮＴＥＲチェック
           IF     (SPA-GMN-SELNUM  NOT =   ZERO  )
               MOVE    SPA-GMN-SELNUM          TO  IDX
               IF     (SPA-GMN-TUKEID  (IDX)   =   SPA-NAI-UKEID   )
                  AND (SPA-GMN-TKYYKTIME(IDX)  =   SPA-NAI-KYYKTIME)
                  AND (SPA-GMN-TYYKID   (IDX)  =   SPA-NAI-YYKID   )
                   CONTINUE
               ELSE
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO  TO  49001-KOUSIN-EXT
               END-IF
           END-IF
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  FLG-USERPG
      *    受付マスタ追加          
           IF      SPA-GMN-SELNUM            =   ZERO
               PERFORM 4901-UKEADD-SEC
           ELSE
               MOVE    SPA-GMN-SELNUM            TO  IDX
               IF      SPA-GMN-TUKEID(IDX)   NOT =   ZERO
      *    受付マスタ更新
                  PERFORM 4902-UKEUPD-SEC
               ELSE
      *    受付マスタ追加
                  PERFORM 4901-UKEADD-SEC
      *    予約マスタ更新
                  IF      SPA-ERRCD          =   SPACE  
                      PERFORM 4903-YYKUPD-SEC
                  END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        保険・公費情報更新
               PERFORM 4904-HKN-KOUSIN-SEC
           END-IF
      ***  IF      SPA-ERRCD           =   SPACE
      *        ＱＲデータ更新
      *        IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *            PERFORM 4905-QRHKCD-KOUSIN-SEC
      *        END-IF
      *****END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  49001-KOUSIN-EXT
           END-IF
      *
      **** MOVE    SPA-KYOUTU          TO  WRK-SPA-KYOUTU
      *
           IF     (SPA-GMN-KARUTE      =   "0" )  AND
                  (SPA-GMN-SYOHO       =   "0" )
               CONTINUE
           ELSE
      *        帳票印刷処理
               PERFORM 49002-PRINT-SYORI-SEC
           END-IF
      *
           IF      FLG-CLAIM       =   1 
               PERFORM 4906-CLAIM-SOUSIN-SEC
               PERFORM 49061-API-CLMAI-SEC
           END-IF
      *!!!!
      *H28.6
      *    ginbee対応
           IF     (SPA-MW              =   SPA-GINBEE)
             AND  (SPA-ERRCD           =   SPACE     )
             AND  (SPA-GMN-PTNUM   NOT =   SPACE     )
      *        AIP CLAIM送信処理
               PERFORM 49061-API-CLMAI-SEC
           END-IF
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    ユーザプログラム起動
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        CONTINUE
      **** ELSE
               IF      SPA-GMN-PTNUM   NOT =   SPACE
                   PERFORM 4907-USERPG-SEC
               END-IF
      **** END-IF
      ***  IF      FLG-END             =   ZERO
      *
      *        MOVE    WRK-SPA-KYOUTU          TO  SPA-KYOUTU
      ***  END-IF
      *
           PERFORM 493-CLEAR-SEC
      *****PERFORM 3101-UKETU-HEN-SEC
           .
       49001-KOUSIN-EXT.
           EXIT.   
      *
      *****************************************************************
      *    帳票印刷処理ク処理
      *****************************************************************
       49002-PRINT-SYORI-SEC             SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得処理
               PERFORM 4951-CLIENT-PRT-SEC
           END-IF
      *
      *    帳票プログラム取得
           PERFORM 4901-PRINT-PG-SEC
      *
      *    カルテ印刷
           IF      SPA-GMN-KARUTE          =   "1"   OR  "2"  OR  "3"
      *        クライアント印刷あり
      *        IF    ( SPA-CLIENT-PRT-FLG  =   1  )
                   MOVE    "01"            TO  SPA-PRT-FLG
                   MOVE    2               TO  SPA-CLIENT-TEMP-FLG
      *        END-IF
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM   
               MOVE    SPA-GMN-KARUTE      TO  ORCHC01-SYORIKBN
      *        病名なしは、区分＝４へ
               IF      SPA-GMN-KARUTE      =   "3"
                   MOVE    "4"             TO  ORCHC01-SYORIKBN
               END-IF
               CALL    WRK-KARUTE-PG       USING  SPA-AREA
                                                  ORCHC01AREA
           END-IF
      *
      *    処方箋印刷
           IF      SPA-GMN-SYOHO          =   "1"
      *        クライアント印刷あり
      *        IF    ( SPA-CLIENT-PRT-FLG  =   1  )
                   MOVE    "02"            TO  SPA-PRT-FLG
                   MOVE    2               TO  SPA-CLIENT-TEMP-FLG
      *        END-IF
      *
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
               MOVE    SPA-GMN-SEX         TO  SPA-SEX
               MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
               MOVE    "2"                 TO  SPA-NYUGAIKBN
               IF      SPA-GMN-DRCD        =   SPACE
                   MOVE    SPACE               TO  SPA-DRCD
               ELSE
                   MOVE    "1"                 TO  SPA-DRCD (1:1) 
                   MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
               END-IF
               MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
               INITIALIZE                      ORCHC19AREA
               MOVE    ZERO                TO  ORCHC19-RENNUM
               MOVE    "1"                 TO  ORCHC19-KBN
               CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
      *
      *    IF     (SPA-CLIENT-PRT-FLG   =   1  )
      *    印刷ダミー処理
      *        MOVE    "99"                TO  SPA-PRT-FLG
      *        CALL    "ORCHCDUMMY"    USING   SPA-AREA
      *        MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      **   END-IF
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           .
       49002-PRINT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4951-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "カルテ"            TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(02)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
           MOVE    1                   TO  FLG-CLIENT-OK
           .
       4951-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
       600-CLIENT-DMY-SEC              SECTION.
      *
           IF     (SPA-CLIENT-PRT-FLG   =   1  )
      *    印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *    MOVE    "_UDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      **   PERFORM 900-PUT-WINDOW
      *
           .
       600-CLIENT-DMY-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新チェック処理
      *****************************************************************
       4109-TOUROKU-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-NAME            =  SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      FLG-TOUROKU         =   1
               IF     (SPA-GMN-SRYKA           =  SPACE )  OR
                      (SPA-GMN-SRYNAIYOCD-G    =  SPACE )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0026"              TO  SPA-ERRCD
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      FLG-TOUROKU         =   2
               IF     (SPA-GMN-SRYKA           =  SPACE )
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0026"              TO  SPA-ERRCD
                       MOVE    4                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    保険組合せ非表示チェック
           IF     (FLG-TOUROKU         =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-PTNUM   NOT =   SPACE )
             AND  (SPA-GMN-COMBINUM
                                   NOT =   ZERO  )
               IF      SPA-NAI-KEIFLG3     =   ZERO
                   PERFORM 41093-HKNCOMBIHIHYOJI-SEC
               END-IF
           END-IF
      *    保険組合せチェック
           IF     (FLG-TOUROKU         =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-PTNUM   NOT =   SPACE )  AND
                  (SPA-GMN-COMBINUM    =   ZERO  )  AND
                  (SPA-NAI-UKEHKNCHK   =   "1"
                                       OR  "2"   )
               IF      SPA-NAI-KEIFLG4     =   ZERO
                   MOVE    "K012"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-NAI-KEIFLG4
                   MOVE    9               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    受付重複チェック
           IF     (FLG-TOUROKU         =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-PTNUM   NOT =   SPACE )
               IF      SPA-NAI-KEIFLG      =   ZERO
                   PERFORM 41091-UKETUKE-CHK-SEC
               END-IF
           END-IF
      *    前期高齢者のチェック
           IF     (FLG-TOUROKU         =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN-PTID    NOT =   ZERO  )
      *****  AND  (SPA-NAI-NENREI      >=  70    )
               IF      SPA-NAI-KEIFLG2     =   ZERO
                   PERFORM 41092-HKNHOJOKBN-CHK-SEC
               END-IF
           END-IF
      *H28.2
      *    予約受付済み判定
           IF     (FLG-TOUROKU         =   1     )  AND
                  (SPA-ERRCD           =   SPACE )  AND
                  (SPA-NAI-YYKID   NOT =   ZERO  )  AND
                  (SPA-NAI-UKEID       =   ZERO  )  AND
                  (SPA-GMN-SELNUM  NOT =   ZERO  )
               MOVE    SPA-GMN-SELNUM        TO  IDX
               INITIALIZE                    YYK-KEY
               MOVE   SPA-HOSPNUM            TO  YYK-HOSPNUM
               MOVE   SPA-GMN-TSRYNAIYOCD(IDX)
                                             TO  YYK-SRYNAIYO
               MOVE   "1"                    TO  YYK-DRCD
               MOVE   SPA-GMN-TDRCD    (IDX) TO  YYK-DRCD(2:4)
               MOVE   SPA-SYSYMD             TO  YYK-YYKYMD
               MOVE   SPA-GMN-TKYYKTIME(IDX) TO  YYK-KEYYYKTIME
               MOVE   SPA-GMN-TYYKID   (IDX) TO  YYK-YYKID
               PERFORM 900-YOYAKU-READ-SEC
               IF      FLG-YYK             =   ZERO
                   IF      YYK-RAIINZUMI       =   "1"
                       PERFORM 493-CLEAR-SEC
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    "0033"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せ非表示チェック処理
      *****************************************************************
       41093-HKNCOMBIHIHYOJI-SEC              SECTION.
      *
           PERFORM VARYING     IDX-C   FROM    1   BY  1
                   UNTIL       IDX-C   >   SPA-COMBI-MAX
               IF     (SPA-GMN-COMBINUM    =   SPA-GMN-HKNCOMBINUM
                                                          (IDX-C) )
      *            入院中
                   IF      (SPA-NAI-THYOJIKBN (IDX-C)  =   "2"  )
                      AND  (SPA-NAI-NYUINKBN           =   "1"  )
                       MOVE    "K010"              TO  SPA-ERRCD
                       MOVE    9                   TO  SPA-GMN-CUR
                   END-IF
                   IF      (SPA-NAI-THYOJIKBN (IDX-C)  =   "3"  )
                      AND  (SPA-NAI-NYUINKBN           =   SPACE)
                       MOVE    "K011"              TO  SPA-ERRCD
                       MOVE    9                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
           MOVE    1                   TO  SPA-NAI-KEIFLG3
           .
       41093-HKNCOMBIHIHYOJI-EXT.
           EXIT.
      *****************************************************************
      *    受付重複チェック処理
      *****************************************************************
       41091-UKETUKE-CHK-SEC              SECTION.
      *
           INITIALIZE                       UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-GMN-PTID        TO  UKE-PTID
           MOVE    SPA-GMN-SRYKA       TO  UKE-SRYKA
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM    TO  UKE-HKNCOMBI
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
                MOVE    1              TO  FLG-UKETUKE
           END-IF
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL   (FLG-UKETUKE    =   1  )  OR
                           (FLG-ERR        =   1  )
               IF      SPA-GMN-SELNUM      =   ZERO
                   MOVE    1               TO  FLG-ERR
               ELSE
                   MOVE    SPA-GMN-SELNUM      TO  IDX
                   IF      UKE-UKEID       NOT =   SPA-GMN-TUKEID(IDX)
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-IF
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ERR             =   1
               MOVE    "K001"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-NAI-KEIFLG
           END-IF
      *
           .
       41091-UKETUKE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    前期高齢者のチェック処理
      *****************************************************************
       41092-HKNHOJOKBN-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  AGECHK-PTID
           MOVE    SPA-SYSYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-NAI-BIRTHDAY    TO  AGECHK-BIRTHDAY
           MOVE    SPA-GMN-COMBINUM    TO  AGECHK-HKNCOMBINUM
           MOVE    "2"                 TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-RJNCHK       =   ZERO
               IF      AGECHK-KAKUNINCHK   =   1
      *            国保で前期高齢者日以降に確認がない時
                   MOVE    "K004"              TO  SPA-ERRCD
               END-IF
           ELSE
               IF      AGECHK-RJNCHK       =   1
      *            前期高齢者で補助区分がない時
                   MOVE    "K002"              TO  SPA-ERRCD
               END-IF
               IF      AGECHK-RJNCHK       =   3
      *            前期高齢者でＨ１８．１０の改正後の補助区分でない時
                   MOVE    "K005"              TO  SPA-ERRCD
               END-IF
               IF      AGECHK-RJNCHK       =   2
      *            老人年齢で老人公費がない時
                   MOVE    "K003"              TO  SPA-ERRCD
               END-IF
      *ver.4.7
      *        Ｈ２０．４から退職国保で６５歳以上
               IF      AGECHK-RJNCHK       =   9
                   MOVE    "K007"              TO  SPA-ERRCD
               END-IF
      *        Ｈ２０．４から後期高齢者の保険がない時
               IF      AGECHK-RJNCHK       =   4
                   MOVE    "K013"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    保険組合せ矛盾エラー
           IF      SPA-ERRCD           =   SPACE
               IF      AGECHK-HKNCHK   NOT =   ZERO
                   MOVE    "K006"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-NAI-KEIFLG2
           END-IF
           .
       41092-HKNHOJOKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    受付追加処理
      *****************************************************************
       4901-UKEADD-SEC              SECTION.
      *
      *    受付ＩＤ＝００検索
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    ZERO                TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               ADD     1                   TO  UKE-PTID
               IF      UKE-PTID            >   99999
                   MOVE    "1007"              TO  SPA-ERRCD
                   GO  TO  4901-UKEADD-EXT
               END-IF
               MOVE    UKE-PTID            TO  WRK-UKEID
      *
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    ZERO                TO  UKE-UKEID
               MOVE    1                   TO  UKE-PTID
               MOVE    1                   TO  WRK-UKEID
      *        作成年月日
               MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
               MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
               MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
               MOVE    SPA-OPID            TO  UKE-OPID
      *
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1001"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE KEY INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
               GO  TO  4901-UKEADD-EXT
           END-IF
      *
      *    受付ＩＤの二重登録チェック
           INITIALIZE                       UKETUKE-REC
           MOVE    SPA-HOSPNUM              TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD               TO  UKE-UKEYMD
           MOVE    WRK-UKEID                TO  UKE-UKEID
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               MOVE    "1006"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE INS ERR KEY:"  UKE-KEY
               GO      TO      4901-UKEADD-EXT
           END-IF
      *
      *    明細レコード追加
           INITIALIZE                       UKETUKE-REC
           MOVE    SPA-HOSPNUM              TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD               TO  UKE-UKEYMD
           MOVE    WRK-UKEID                TO  UKE-UKEID
           ACCEPT  UKE-UKETIME              FROM   TIME
           IF      SPA-GMN-PTNUM            =   SPACE
               MOVE    ZERO                     TO  UKE-PTID
           ELSE
               MOVE    SPA-GMN-PTID             TO  UKE-PTID
           END-IF
           MOVE    SPA-GMN-NAME             TO  UKE-NAME
           MOVE    SPA-GMN-SRYKA            TO  UKE-SRYKA
           IF      SPA-GMN-DRCD        NOT =   SPACE
               MOVE    "1"                      TO  UKE-DRCD
               MOVE    SPA-GMN-DRCD             TO  UKE-DRCD(2:4)
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD       TO  UKE-SRYNAIYO
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM         TO  UKE-HKNCOMBI
      *
           IF      SPA-GMN-SELNUM           >   ZERO
               MOVE    SPA-GMN-SELNUM           TO  IDX 
               IF      SPA-GMN-TYYKTIME(IDX)    NOT =  SPACE
                   MOVE    SPA-GMN-TYYKTIME (IDX)   TO  UKE-YYKTIME
                   MOVE    SPA-GMN-TKYYKTIME(IDX)   TO  UKE-KEYYYKTIME
                   MOVE    SPA-GMN-TYYKID   (IDX)   TO  UKE-YYKID
               END-IF
           END-IF
      *    作成年月日
           MOVE    SMCNDATE-YMD        TO  UKE-CREYMD
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
           MOVE    SPA-OPID            TO  UKE-OPID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1002"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE INS ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
           END-IF
      *
           MOVE    1                   TO  FLG-USERPG
      *
           .
       4901-UKEADD-EXT.
           EXIT.   
      *
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       4902-UKEUPD-SEC              SECTION.
      *
           IF     (SPA-GMN-TUKEID(IDX) NOT =   SPA-NAI-UKEID )
              OR  (SPA-NAI-UKEID           =   ZERO          )
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4902-UKEUPD-EXT
           END-IF
      *
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-NAI-UKEID       TO  UKE-UKEID
      *
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
      *        会計済み
               IF      UKE-KAIKEITIME      >   ZERO
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        受付ＩＤなし
               MOVE    "1008"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ZERO                TO  SPA-GMN-SELNUM
               MOVE    ZERO                TO  SPA-NAI-SELNUM
      *        受付一覧編集
               PERFORM 3101-UKETU-HEN-SEC
               GO      TO      4902-UKEUPD-EXT
           END-IF
      *
      *****ACCEPT  UKE-UKETIME               FROM   TIME
           MOVE    SPA-GMN-PTID              TO  UKE-PTID
           MOVE    SPA-GMN-NAME              TO  UKE-NAME
           MOVE    SPA-GMN-SRYKA             TO  UKE-SRYKA
           IF      SPA-GMN-DRCD        NOT =   SPACE
               MOVE    "1"                       TO  UKE-DRCD
               MOVE    SPA-GMN-DRCD              TO  UKE-DRCD(2:4)
           END-IF
           MOVE    SPA-GMN-SRYNAIYOCD        TO  UKE-SRYNAIYO
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM         TO  UKE-HKNCOMBI
           IF      SPA-GMN-TYYKTIME(IDX)     NOT =  SPACE
               MOVE    SPA-GMN-TYYKTIME (IDX)    TO  UKE-YYKTIME
               MOVE    SPA-GMN-TKYYKTIME(IDX)    TO  UKE-KEYYYKTIME
               MOVE    SPA-GMN-TYYKID   (IDX)    TO  UKE-YYKID
           END-IF
      *
           MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
           MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
           MOVE    SPA-OPID            TO  UKE-OPID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO 
               MOVE    "1003"              TO  SPA-ERRCD
               DISPLAY "U02 UKETUKE UPD ERR:"  MCP-RC
                       ",KEY:" UKE-KEY
           END-IF
      *
           MOVE    2                   TO  FLG-USERPG
      *
           .
       4902-UKEUPD-EXT.
           EXIT.   
      *
      *****************************************************************
      *    予約更新処理
      *****************************************************************
       4903-YYKUPD-SEC              SECTION.
      * 
           INITIALIZE                    YYK-KEY
           MOVE   SPA-HOSPNUM            TO  YYK-HOSPNUM
           MOVE   SPA-GMN-TSRYNAIYOCD(IDX)
                                         TO  YYK-SRYNAIYO
           MOVE   "1"                    TO  YYK-DRCD
           MOVE   SPA-GMN-TDRCD    (IDX) TO  YYK-DRCD(2:4)
           MOVE   SPA-SYSYMD             TO  YYK-YYKYMD
           MOVE   SPA-GMN-TKYYKTIME(IDX) TO  YYK-KEYYYKTIME
           MOVE   SPA-GMN-TYYKID   (IDX) TO  YYK-YYKID
           PERFORM 900-YOYAKU-READ-SEC
           IF      FLG-YYK             =   ZERO
               MOVE  "1"                   TO  YYK-RAIINZUMI
      * 
               MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
               MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
      *
               MOVE    SPA-OPID            TO  YYK-OPID
      * 
               MOVE    YYK-REC             TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_yyk"           TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1004"              TO  SPA-ERRCD
                   DISPLAY "U02 YYK    UPD ERR:"  MCP-RC
                       ",KEY:" YYK-KEY
               END-IF
           END-IF
           .
       4903-YYKUPD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SYSYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC         TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      FLG-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
                   IF      WRK-GMN-SELNUM        =   SPA-U02X-GMN-SEL
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-IF
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4906-CLAIM-SOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       49061-CLAIM-SOUSINSYOSI-SEC               SECTION.
      *
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *
           MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
           MOVE    SPA-GMN-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    SPA-GMN-SRYKA       TO  SHELLTBL-ARG3
           MOVE    UKE-UKETIME         TO  SHELLTBL-ARG4
           MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
           MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
           MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *    ドクター
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    "@@@@@"             TO  SHELLTBL-ARG8
           ELSE
               MOVE    SPACE               TO  SHELLTBL-ARG8
               MOVE    "1"                 TO  SHELLTBL-ARG8(1:1)
               MOVE    SPA-GMN-DRCD        TO  SHELLTBL-ARG8(2:4)
           END-IF
      *    保険組合せ
           IF      SPA-GMN-COMBINUM    =   ZERO
               MOVE    "@@@@"              TO  SHELLTBL-ARG9
           ELSE
               MOVE    SPA-GMN-COMBINUM    TO  SHELLTBL-ARG9
           END-IF
      *    診療内容
           MOVE    SPA-GMN-SRYNAIYODSP TO  SHELLTBL-ARG10
      *    連番
           ADD     1                   TO  WRK-RENNUM
           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *    システム日付
           MOVE    SPA-SYSYMD          TO   SHELLTBL-ARG12
      *
grpsys*    MOVE    SPA-HOSPNUM         TO  -HOSPNUM
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "shell"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-U02X-AREA
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  FLG-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    MCPDATA-REC         TO  SYS-9000-REC
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-U02X-GMN-SEL
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  FLG-CLAIM-MULTIHOST
               END-IF
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-U02X-CODE
                   IF      SYS-9000-UKEBUTTON (02) =   "T"
                       MOVE    2               TO  FLG-CLAIM
                   ELSE
                       MOVE    1               TO  FLG-CLAIM
                   END-IF
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   2
               CONTINUE
           ELSE
               GO      TO      4905-CLAIM-CHK-EXT
           END-IF
      *
      *    ＣＬＡＩＭ接続情報
      *
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-U02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGU02X"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-U02
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U02X"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "U02X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *H28.6
      *****************************************************************
      *    API ＣＬＡＩＭ送信処理
      *****************************************************************
       49061-API-CLMAI-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  WRK-RENNUM
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
           END-IF
      *
           IF      FLG-SYSKANRI        =   1
               CONTINUE
           ELSE
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
      *            ＣＬＡＩＭ送信あり
                   MOVE    1               TO  FLG-CLAIM
               END-IF
           END-IF
      *
           IF      FLG-CLAIM           =   1
      *        フィアルデイレクトリ位置指定
               INITIALIZE                  ORCSMKPASSAREA
               MOVE    CLAIM-FILE          TO  MKPASS-IN-01
               CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
               MOVE    "HL04.sh"           TO  SHELLTBL-NAME
      *
               MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
               MOVE    SPA-PTID            TO  WRK-PTID
               MOVE    WRK-PTID            TO  SHELLTBL-ARG2
               MOVE    SPA-GMN-SRYKA       TO  SHELLTBL-ARG3
               MOVE    UKE-UKETIME         TO  SHELLTBL-ARG4
               MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG5
      *
      *****    MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG6
      *****    MOVE    SYS-9001-PORT1      TO  SHELLTBL-ARG7
      *        ドクター
               IF      SPA-GMN-DRCD        =   SPACE
                   MOVE    "@@@@@"             TO  SHELLTBL-ARG8
               ELSE
                   MOVE    SPACE               TO  SHELLTBL-ARG8
                   MOVE    "1"                 TO  SHELLTBL-ARG8(1:1)
                   MOVE    SPA-GMN-DRCD        TO  SHELLTBL-ARG8(2:4)
               END-IF
      *        保険組合せ
               IF      SPA-GMN-COMBINUM    =   ZERO
                   MOVE    "@@@@"              TO  SHELLTBL-ARG9
               ELSE
                   MOVE    SPA-GMN-COMBINUM    TO  SHELLTBL-ARG9
               END-IF
      *        診療内容
               MOVE    SPA-GMN-SRYNAIYODSP TO  SHELLTBL-ARG10
      *        連番
               ADD     1                   TO  WRK-RENNUM
               MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
      *        システム日付
               MOVE    SPA-SYSYMD          TO   SHELLTBL-ARG12
      *        患者番号
               MOVE    SPA-GMN-PTNUM       TO   SHELLTBL-ARG14
      *
               MOVE    SHELLTBL            TO  MCPDATA-REC
               MOVE    "SHELL"             TO  MCP-FUNC
               MOVE    "shell"             TO  MCP-TABLE
               MOVE    "allways"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       49061-API-CLMAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC                 SECTION.
      *
           EVALUATE    FLG-USERPG
               WHEN    1
      *        追加
                   MOVE    1               TO  SPA-SYORI-FLG
               WHEN    2
      *        更新
                   MOVE    2               TO  SPA-SYORI-FLG
               WHEN    3
      *        削除
                   MOVE    3               TO  SPA-SYORI-FLG
           END-EVALUATE
           IF      SPA-GMN-USERPG      =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   1
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "U02"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-GMN-PTID        TO  ORCSUSERCHK-PTID
                   MOVE    SPA-GMN-PTNUM       TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-GMN-SRYKA       TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-GMN-COMBINUM    TO  ORCSUSERCHK-HKNCOMBI
                   IF      SPA-GMN-DRCD        =   SPACE
                       MOVE    SPACE               TO  ORCSUSERCHK-DRCD
                   ELSE
                       MOVE    "1"                 TO  ORCSUSERCHK-DRCD
                                                                (1:1) 
                       MOVE    SPA-GMN-DRCD        TO  ORCSUSERCHK-DRCD
                                                                (2:4)
                   END-IF
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       49071-XD01-SYORI-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           EVALUATE    SPA-MOTOPG
      *        ＣＬＡＩＭ画面より
               WHEN    "U02X"
                   MOVE    1               TO  FLG-GMN
               WHEN    OTHER 
                   MOVE    ZERO            TO  FLG-GMN
           END-EVALUATE
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-DRCD
           ELSE
               MOVE    "1"                 TO  SPA-DRCD (1:1) 
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
           END-IF
      *    削除対応
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPACE               TO  SPA-SRYYMD
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *****INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           IF      FLG-GMN             =   ZERO
               MOVE   "CHANGE"             TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE   "XD01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費情報更新処理
      *****************************************************************
       4904-HKN-KOUSIN-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MIKAKU-MAX
               EVALUATE    SPA-NAI-HKNKBN (IDX)
      *            保険
                   WHEN    1
                       INITIALIZE                  PTHKNINF-REC
                       MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
                       MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
                       MOVE    SPA-NAI-HKNID (IDX) TO  PTHKN-HKNID
      *
                       MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "key"               TO  MCP-PATHNAME
                           PERFORM 900-PTHKNINF-READ-SEC
                       ELSE
                           MOVE    1               TO  FLG-PTHKNINF
                       END-IF
                       MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF    ( FLG-PTHKNINF        =   ZERO )
                       AND   ( SPA-NAI-KAKUNINYMD (IDX)
                                               NOT =   PTHKN-KAKUNINYMD)
      *                確認年月日の変更時のみ行う
                           MOVE    SPA-NAI-KAKUNINYMD (IDX)
                                                   TO  PTHKN-KAKUNINYMD
      *
                           MOVE    SMCNDATE-YMD        TO  PTHKN-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTHKN-UPHMS
                           MOVE    SPA-OPID            TO  PTHKN-OPID
      *
                           MOVE    PTHKNINF-REC        TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                           IF      MCP-RC          NOT =   ZERO 
                               MOVE    "1011"              TO  SPA-ERRCD
                               DISPLAY "U02 PTHKNINF UPDATE ERR:" MCP-RC
                                               ",KEY:" PTHKN-KEY
                           END-IF
                           IF      SPA-ERRCD           =   SPACE
      *                        確認年月日履歴処理
                               PERFORM 49041-PTHKNRRK-SYORI-SEC
                           END-IF
                       END-IF
      *
      *            公費
                   WHEN    2
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
                       MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
                       MOVE    SPA-NAI-HKNID (IDX) TO  PTKOH-KOHID
      *
                       MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                       MOVE    "DBSELECT"          TO  MCP-FUNC
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 910-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "key"               TO  MCP-PATHNAME
                           PERFORM 900-PTKOHINF-READ-SEC
                       ELSE
                           MOVE    1               TO  FLG-PTKOHINF
                       END-IF
                       MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 990-DBCLOSE-SEC
                       IF     (FLG-PTKOHINF        =   ZERO )
                       AND    (SPA-NAI-KAKUNINYMD (IDX)
                                               NOT =   PTKOH-KAKUNINYMD)
      *                確認年月日の変更時のみ行う
                           MOVE    SPA-NAI-KAKUNINYMD (IDX)
                                                   TO  PTKOH-KAKUNINYMD
      *
                           MOVE    SMCNDATE-YMD        TO  PTKOH-UPYMD
                           MOVE    SMCNDATE-HMS        TO  PTKOH-UPHMS
                           MOVE    SPA-OPID            TO  PTKOH-OPID
      *
                           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
                           MOVE    "DBUPDATE"          TO  MCP-FUNC
                           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                           MOVE    "key"               TO  MCP-PATHNAME
grpsys                     CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       END-IF
      *
               END-EVALUATE
           END-PERFORM
      *
           .
       4904-HKN-KOUSIN-EXT.
           EXIT.
      *****************************************************************
      *    確認年月日履歴処理
      *****************************************************************
       49041-PTHKNRRK-SYORI-SEC              SECTION.
      *
           IF     (SPA-MAE-KAKUNINYMD (IDX)  =   SPACE )
               CONTINUE
           ELSE
               INITIALIZE                      ORCSPTHKNRRKAREA
               MOVE    SPA-GMN-PTID        TO  ORCSHKNRRK-PTID
               MOVE    SPA-NAI-HKNID (IDX) TO  ORCSHKNRRK-HKNID
               MOVE    SPA-MAE-KAKUNINYMD (IDX)
                                           TO  ORCSHKNRRK-KAKUNINYMD
               CALL    "ORCSPTHKNRRK"      USING
                                           SPA-AREA
                                           ORCSPTHKNRRKAREA
               IF      ORCSHKNRRK-RC       =   9
                   DISPLAY "U02 PTHKNRRK UPD ERR:" ORCSHKNRRK-RC
                                         ",HKNID:" ORCSHKNRRK-HKNID
                   MOVE    "1011"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       49041-PTHKNRRK-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ＱＲデータ更新処理
      *****************************************************************
      *4905-QRHKCD-KOUSIN-SEC              SECTION.
      *
      *    INITIALIZE                      QRCDHKN-REC
      *    MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
      *    MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
      *    MOVE    SPA-U02Q-REGISTID   TO  QRCDHKN-REGISTID
      *    MOVE    QRCDHKN-REC         TO  MCPDATA-REC
      *    MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 910-DBSELECT-SEC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
      *        PERFORM 900-QRCDHKN-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-QRCDHKN
      *        INITIALIZE                      QRCDHKN-REC
      *    END-IF
      *    MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *    MOVE    "key"               TO  MCP-PATHNAME
      *    PERFORM 990-DBCLOSE-SEC
      *
      *    IF      FLG-QRCDHKN         =   ZERO
      *        MOVE    1                   TO  QRCDHKN-MOD-FLG2
      *
      *        MOVE    QRCDHKN-REC         TO  MCPDATA-REC
      *        MOVE    "DBUPDATE"          TO  MCP-FUNC
      *        MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
grpsys*        CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                            MCPDATA-REC
      *                                            SPA-AREA
      *        IF      MCP-RC          NOT =   ZERO 
      *            MOVE    "1001"              TO  SPA-ERRCD
      *            DISPLAY "U02Q QRCDHKN KEY UPD ERR:"  MCP-RC
      *                    ",KEY:" QRCDHKN-KEY
      *        END-IF
      *    END-IF
      *    INITIALIZE                  SPA-U02Q-DATA-G
      *    .
      *4905-QRHKCD-KOUSIN-EXT.
      *    EXIT.
      *****************************************************************
      *    ＱＲデータ解除処理
      *****************************************************************
       4906-QRHKCD-KAIJO-SEC              SECTION.
      *
           INITIALIZE                      QRCDHKN-REC
           MOVE    SPA-HOSPNUM         TO  QRCDHKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  QRCDHKN-REGISTYMD
           MOVE    SPA-U02Q-REGISTID   TO  QRCDHKN-REGISTID
           MOVE    QRCDHKN-REC         TO  MCPDATA-REC
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-QRCDHKN-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-QRCDHKN
               INITIALIZE                      QRCDHKN-REC
           END-IF
           MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-QRCDHKN         =   ZERO
               MOVE    ZERO                TO  QRCDHKN-MOD-FLG2
      *
               MOVE    QRCDHKN-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_qrcdhkn"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1001"              TO  SPA-ERRCD
                   DISPLAY "U02 QRCDHKN KEY UPD ERR:"  MCP-RC
                           ",KEY:" QRCDHKN-KEY
               END-IF
           END-IF
           INITIALIZE                  SPA-U02Q-DATA-G
           .
       4906-QRHKCD-KAIJO-EXT.
           EXIT.
      *****************************************************************
      *    受付取消処理
      *****************************************************************
       491-SAKUJO-SEC              SECTION.
      *
           IF      SPA-GMN-SELNUM      =   ZERO   
               MOVE    "0016"              TO  SPA-ERRCD
               IF      SPA-UKE-MAX         >   ZERO
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
               GO  TO  491-SAKUJO-EXT
           END-IF
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO  TO  491-SAKUJO-EXT
           END-IF
      *
      *
           IF  SPA-GMN-TUKETIME(SPA-GMN-SELNUM) =  SPACE
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
      *        確認メッセージ
               MOVE    "0103"              TO  SPA-U2ID1CD
           END-IF
           .
       491-SAKUJO-EXT.
           EXIT.
      *
      ***************************************************************
      *    受付削除処理
      ***************************************************************
       4910-DELETE-SYORI-SEC        SECTION.
      *
      *    受付マスタ削除
           MOVE    SPA-GMN-SELNUM          TO  IDX
      *2019.1
           IF     (SPA-GMN-TUKEID(IDX) NOT =   SPA-NAI-UKEID )
              OR  (SPA-NAI-UKEID           =   ZERO          )
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO    TO  4910-DELETE-SYORI-EXT
           END-IF
      *
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM             TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD              TO  UKE-UKEYMD
      **** MOVE    SPA-GMN-TUKEID(IDX)     TO  UKE-UKEID
           MOVE    SPA-NAI-UKEID           TO  UKE-UKEID
      *
           PERFORM 900-UKETUKE-READ-SEC
           IF      FLG-UKETUKE         =   ZERO
               MOVE    UKETUKE-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO 
                   MOVE    "1005"              TO  SPA-ERRCD
                   DISPLAY "U02 UKETUKE DELETE ERR:" MCP-RC
                   GO  TO  4910-DELETE-SYORI-EXT
               END-IF
      *
           END-IF
      *
      *    予約マスタ更新
           IF      SPA-GMN-TYYKTIME(IDX)  NOT       =  SPACE
               INITIALIZE                      YYK-KEY
               MOVE    SPA-HOSPNUM             TO  YYK-HOSPNUM
               MOVE    SPA-GMN-TSRYNAIYOCD(IDX)
                                               TO  YYK-SRYNAIYO
               MOVE    "1"                     TO  YYK-DRCD
               MOVE    SPA-GMN-TDRCD(IDX)      TO  YYK-DRCD(2:4)
               MOVE    SPA-SYSYMD              TO  YYK-YYKYMD
               MOVE    SPA-GMN-TKYYKTIME(IDX)
                                               TO  YYK-KEYYYKTIME
               MOVE    SPA-GMN-TYYKID(IDX)     TO  YYK-YYKID
               PERFORM 900-YOYAKU-READ-SEC
               IF      FLG-YYK             =   ZERO
                   MOVE    SPACE               TO  YYK-RAIINZUMI
      *
                   MOVE    SMCNDATE-YMD        TO  YYK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  YYK-UPHMS
                   MOVE    SPA-OPID            TO  YYK-OPID
      *
                   MOVE    YYK-REC             TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_yyk"           TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO 
                       MOVE    "1004"              TO  SPA-ERRCD
                       DISPLAY "U02 YYK UPDATE ERR:" MCP-RC
                                               ",KEY:" YYK-KEY
                       GO  TO  4910-DELETE-SYORI-EXT
                   END-IF
               END-IF
           END-IF
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               MOVE    3                   TO  FLG-USERPG
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    ユーザプログラム起動
           IF      SPA-GMN-PTNUM   NOT =   SPACE
               MOVE    3                   TO  FLG-USERPG
               PERFORM 4907-USERPG-SEC
           END-IF
      *
           PERFORM 493-CLEAR-SEC
      **** PERFORM 3101-UKETU-HEN-SEC
           .
       4910-DELETE-SYORI-EXT.
           EXIT.
      *
      ***************************************************************
      *    保険確認年月日入力済み処理
      ***************************************************************
       400-HKNCHK-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  IDX
           EVALUATE    MCP-WIDGET
               WHEN    "BHK1"
                   MOVE    1              TO  IDX
               WHEN    "BHK2"
                   MOVE    2              TO  IDX
               WHEN    "BHK3"
                   MOVE    3              TO  IDX
               WHEN    "BHK4"
                   MOVE    4              TO  IDX
               WHEN    "BHK5"
                   MOVE    5              TO  IDX
           END-EVALUATE
      *
           COMPUTE IDY                 =   (SPA-GMN-PAGE   -   1  )
                                       *    5
                                       +   IDX
      *
           IF     (IDY                 =   ZERO )  OR
                  (SPA-GMN-TMIKAKUNAME (IDY)   =   SPACE)
               GO      TO      400-HKNCHK-SYORI-EXT
           END-IF
      *    労災保険の時、セットしない
           IF      SPA-NAI-HKNKBN  (IDY)   =   3
               CONTINUE
           ELSE
               IF     (SPA-SYSYMD          =   SPA-NAI-KAKUNINYMD(IDY))
                  AND (SPA-NAI-KAKUNINYMD(IDY) NOT =  SPACE)
                   MOVE    SPA-MAE-KAKUNINYMDW(IDY)
                                               TO  SPA-GMN-TMIKAKUYMD
                                                               (IDY)
                   MOVE    SPA-MAE-KAKUNINYMD (IDY)
                                               TO  SPA-NAI-KAKUNINYMD
                                                               (IDY)
               ELSE
                   MOVE    SPA-SYSYMD          TO  SPA-NAI-KAKUNINYMD
                                                               (IDY)
                   MOVE    SPA-SYSYMDWH        TO  SPA-GMN-TMIKAKUYMD
                                                               (IDY)
               END-IF
           END-IF
      *
           .
       400-HKNCHK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       492-ORCGP01-SEC              SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    患者登録の権限がない時、エラー
           IF      SPA-GSRAUTH-12      =   "1"
               CONTINUE
           ELSE
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO  TO  492-ORCGP01-EXT
           END-IF
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO  TO  492-ORCGP01-EXT
           END-IF
      *
           IF      SPA-GMN-SELNUM      >   ZERO
      *
      *    選択番号チェック
               IF    SPA-GMN-SELNUM        >   SPA-UKE-MAX
                   MOVE    "0015"              TO  SPA-ERRCD
                   GO                          TO  492-ORCGP01-EXT
               END-IF
      *
      *  受付チェック
               IF     (SPA-GMN-TUKETIME(SPA-GMN-SELNUM)   =   SPACE )
      *           患者登録なしのみ受付必須
                AND   (SPA-GMN-TPTNUM(SPA-GMN-SELNUM)     =   SPACE  )
                   MOVE    "0004"              TO  SPA-ERRCD
                   GO                          TO  492-ORCGP01-EXT
               END-IF
      *
      *  受付チェック
               IF     (SPA-GMN-TUKETIME(SPA-GMN-SELNUM)   =   SPACE )
      *           患者登録なしのみ受付必須
                AND   (SPA-GMN-TPTNUM(SPA-GMN-SELNUM) NOT =   SPACE  )
                   IF      FLG-UKE-CHK         =   ZERO
                       MOVE    "0105"              TO  SPA-U2ID1CD
                       GO                          TO  492-ORCGP01-EXT
                   END-IF
               END-IF
      *
      *  患者番号チェック
              IF      SPA-GMN-PTNUM        NOT =  SPACE
                  MOVE    SPA-HOSPNUM          TO  PTINF-HOSPNUM
                  MOVE    SPA-GMN-PTID         TO  PTINF-PTID
                  PERFORM 900-PTINF-READ-SEC
                  IF      FLG-PTINF                =   1
                      MOVE    "0002"               TO  SPA-ERRCD
                      GO  TO  492-ORCGP01-EXT
      *---- (01.00.04) LINE MOD START ---------------------------------
                  END-IF
               END-IF
      **** ELSE
      *        IF      SPA-GMN-PTID         =   ZERO               
      *            MOVE    "0005"              TO  SPA-ERRCD
      *            GO                          TO  492-ORCGP01-EXT
      *********END-IF
           END-IF    
      *---- (01.00.04) LINE MOD END -----------------------------------
      *    ＱＲデータ解除処理
      **** IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
      *    共通ＳＰＡセット
           PERFORM 4921-SPA-SET-SEC
           IF      SPA-GMN-PTNUM(1:1)  =   "*"
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           END-IF
           IF      SPA-GMN-SELNUM      >   ZERO
      *        受付ＩＤ
               MOVE    SPA-GMN-TUKEID  (SPA-GMN-SELNUM)
                                           TO  SPA-P02-UKEID
      *        患者氏名
               MOVE    SPA-GMN-NAME        TO  SPA-NAME
           END-IF
           MOVE    SPA-GMN-SELNUM      TO   SPA-P02-U02
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "P02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       492-ORCGP01-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通画面遷移処理
      *****************************************************************
       4921-SPA-SET-SEC              SECTION.
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    診療日
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
      *
      *    患者引継ぎなし
           IF      SPA-GMN-PTNUM       =   SPACE
               GO      TO      4921-SPA-SET-EXT
           END-IF
      *
      *    患者番号、患者ＩＤ
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *    診療科
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKANAME   TO  SPA-SRYKAMEI
      *    患者氏名
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
      *    ドクター
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-DRCD
           ELSE
               MOVE    "1"                 TO  SPA-DRCD (1:1) 
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD (2:)
           END-IF
      *    保険組合せ
           MOVE    SPA-GMN-COMBINUM    TO  SPA-HKNCOMBINUM
      *
           .
      *
       4921-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為へ
      *****************************************************************
       493-ORCGK02-SEC              SECTION.
      *
           IF     (SPA-ERRCD       NOT =   SPACE )
             OR   (SPA-U2ID1CD     NOT =   SPACE )
               GO       TO      493-ORCGK02-EXT
           END-IF
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO       TO      493-ORCGK02-EXT
           END-IF
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      493-ORCGK02-EXT
           END-IF
      *
      *    対象保険があること
           IF      SPA-GMN-SELNUM      >   ZERO
               PERFORM 410114-KUMIHYOJI-SEC
               IF      SPA-COMBI-MAX       =   ZERO
                   MOVE    "0023"              TO  SPA-ERRCD
                   GO       TO      493-ORCGK02-EXT
               END-IF
           END-IF
      *    ＱＲデータ解除処理
      **** IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
      *H25.4
      *    患者の選択がなく、待受けがＫ０２の時、戻る処理
           IF     (SPA-GMN-PTID       =   ZERO )  AND
                  (SPA-MACHIPG(1:3)   =   "K02")
               PERFORM 210-BACK
               GO       TO      493-ORCGK02-EXT
           END-IF
      *
           PERFORM 4921-SPA-SET-SEC
           IF      SPA-GMN-SELNUM      >   ZERO
      *        受付ＩＤ
               MOVE    SPA-GMN-TUKEID  (SPA-GMN-SELNUM)
                                           TO  SPA-P02-UKEID
           END-IF
      *H25.4
           MOVE    ZERO                TO  FLG-K02NYUIN
           IF     (SPA-GMN-PTID    NOT =   ZERO )  AND
                  (SPA-NAI-NYUINKBN    =   "1"  )
      *        入院中は入院へ
               MOVE    1                   TO  FLG-K02NYUIN
      *        前が入院以外なら外来へ
               IF      SPA-HZN-MOTOPG  NOT =   "K02N"
                   MOVE    ZERO            TO  FLG-K02NYUIN
               END-IF
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-SAKIPG
      *H25.4
      *    入院中の患者
           IF      FLG-K02NYUIN        =   1
               MOVE    "K02N"              TO  SPA-SAKIPG
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
      *****MOVE   "K02"                TO  MCP-WINDOW.
           MOVE   SPA-SAKIPG           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       493-ORCGK02-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録へ
      *****************************************************************
       494-ORCGC02-SEC              SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO       TO      494-ORCGC02-EXT
           END-IF
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      494-ORCGC02-EXT
           END-IF
      **** ＱＲデータ解除処理
      *    IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "C02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "C02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       494-ORCGC02-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       495-ORCGS02-SEC              SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO       TO      495-ORCGS02-EXT
           END-IF
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      495-ORCGS02-EXT
           END-IF
      *    ＱＲデータ解除処理
      ***  IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "S02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       495-ORCGS02-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会へ
      *****************************************************************
       496-ORCGJ02-SEC              SECTION.
      *
      *H28.12
           IF      SPA-NAI-UKEUP       =   1
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO       TO      496-ORCGJ02-EXT
           END-IF
      *
           IF     (SPA-GMN-SELNUM      >   ZERO  )  AND
                  (SPA-GMN-PTID        =   ZERO  )
               MOVE    "0022"              TO  SPA-ERRCD
               GO       TO      496-ORCGJ02-EXT
           END-IF
      *    ＱＲデータ解除処理
      **   IF      SPA-U02Q-REGISTID   NOT =   ZERO
      *        PERFORM 4906-QRHKCD-KAIJO-SEC
      **** END-IF
      *
           PERFORM 4921-SPA-SET-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "J02"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       496-ORCGJ02-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    予約・受付のみ切り替え処理
      *****************************************************************
       4905-B05S-SYORI-SEC         SECTION.
      *
           EVALUATE    SPA-NAI-SYORI1
               WHEN    0
                   MOVE    1               TO  SPA-NAI-SYORI1
               WHEN    1
                   MOVE    2               TO  SPA-NAI-SYORI1
               WHEN    2
                   MOVE    0               TO  SPA-NAI-SYORI1
           END-EVALUATE
      *    受付一覧編集
           IF      SPA-GMN-SELNUM  NOT =   ZERO
               PERFORM 1009-HEAD-CLEAR-SEC
           END-IF
           MOVE    ZERO                TO  SPA-GMN-SELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           PERFORM 3101-UKETU-HEN-SEC
           IF      SPA-UKE-MAX         =   ZERO
               IF      SPA-NAI-SYORI1      =   1
                   MOVE    2               TO  SPA-NAI-SYORI1
                   PERFORM 3101-UKETU-HEN-SEC
               END-IF
           END-IF
           IF      SPA-UKE-MAX         =   ZERO
               IF      SPA-NAI-SYORI1      =   2
                   MOVE    0               TO  SPA-NAI-SYORI1
                   PERFORM 3101-UKETU-HEN-SEC
               END-IF
           END-IF
           IF      SPA-UKE-MAX         >   ZERO
               MOVE    1               TO  SPA-GMN-CUR
           ELSE
               MOVE    2               TO  SPA-GMN-CUR
           END-IF
           .
       4905-B05S-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    選択一覧処理
      *****************************************************************
       4908-B08S-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "U012"              TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "U012"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       4908-B08S-SYORI-EXT.
           EXIT.
      *
      *H28.01
      *****************************************************************
      *    選択一覧処理
      *****************************************************************
       4908-UDR-SYORI-SEC         SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-U02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "U02"               TO  SPA-MOTOPG
           MOVE    "UDR"               TO  SPA-SAKIPG
      *
           INITIALIZE                  SPA-KYOTUKEY
      *    引き継ぎ内容
      *    診療科
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *    患者氏名
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "UDR"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       4908-UDR-SYORI-EXT.
           EXIT.
      *!!! 
      *H28.12
      *****************************************************************
      *   受付順変更処理
      *****************************************************************
       4905-B03S-SYORI-SEC         SECTION.
      *
           IF     (SPA-GMN-PTNUM   NOT =   SPACE )
               OR (SPA-GMN-NAME    NOT =   SPACE )
               MOVE    "0034"              TO  SPA-ERRCD
               GO      TO      4905-B03S-SYORI-EXT
           END-IF
      *
           EVALUATE    SPA-NAI-UKEUP
               WHEN    0
                   MOVE    1               TO  SPA-NAI-UKEUP
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    1
      *            受付順変更処理
                   PERFORM 49041-UKETUKE-JUN-SEC
                   MOVE    0               TO  SPA-NAI-UKEUP
           END-EVALUATE
           .
       4905-B03S-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   受付順更新処理
      *****************************************************************
       49041-UKETUKE-JUN-SEC         SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL  (IDX     >      SPA-UKE-MAX )
                      OR  (SPA-ERRCD   NOT =   SPACE  )
               INITIALIZE                  UKETUKE-REC
               MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
               MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
               MOVE    SPA-GMN-TUKEID(IDX)
                                           TO  UKE-UKEID
               PERFORM 900-UKETUKE-READ-SEC
      *
               IF     (FLG-UKETUKE         =   ZERO )
                 AND  (UKE-UKEID       NOT =   ZERO )
                   IF      UKE-KAIKEITIME      =   ZERO
                       ADD     1                   TO  IDY
                       MOVE    IDY                 TO  UKE-HYOJIJUN
      *                変更フラグ
                       IF      SPA-GMN-THENFLG(IDX)    =   "1"
                          MOVE    "1"              TO  UKE-KAKREN
                       END-IF
      *
                       MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                       MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                       MOVE    SPA-OPID            TO  UKE-OPID
      *
                       MOVE    UKETUKE-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_uketuke"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO 
                            MOVE    "1003"              TO  SPA-ERRCD
                            DISPLAY "U02 UKETUKE UPD ERR:"  MCP-RC
                                  ",KEY:" UKE-KEY
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     SPA-ERRCD            =   SPACE
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
      *
               MOVE    ZERO            TO  SPA-GMN-SELNUM
               MOVE    ZERO            TO  SPA-NAI-SELNUM
      *        予約，受付編集
               PERFORM 3101-UKETU-HEN-SEC
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
           .
       49041-UKETUKE-JUN-EXT.
           EXIT.
      *****************************************************************
      *   受付順選択チェック処理
      *****************************************************************
       41042-SELNUM-UKEUPCHK-SEC         SECTION.
      *
           MOVE    SPA-GMN-SELNUM          TO  SPA-NAI-SELNUM
           MOVE    SPA-GMN-SELNUM          TO  IDX
           IF      SPA-GMN-TPTID (IDX) NOT =  ZERO
      *        排他制御処理
               MOVE    SPA-GMN-TPTID  (IDX)  TO  SPA-GMN-PTID
               PERFORM 990-LOCK-IN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
               MOVE    ZERO                TO  SPA-GMN-PTID
           END-IF
           .
       41042-SELNUM-UKEUPCHK-EXT.
           EXIT.
      *****************************************************************
      *   受付順変更処理（↑）
      *****************************************************************
       4659-UKETUKE-UP-SYORI-SEC         SECTION.
      *
           IF    (SPA-GMN-SELNUM         =  ZERO    )
               GO      TO      4659-UKETUKE-UP-SYORI-EXT
           END-IF
      *
      *    受付順変更
           PERFORM 41042-SELNUM-UKEUPCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4659-UKETUKE-UP-SYORI-EXT
           END-IF
      *
           IF    (SPA-GMN-SELNUM         <=  1    )
               GO      TO      4659-UKETUKE-UP-SYORI-EXT
           END-IF
      *
           MOVE    SPA-GMN-SELNUM     TO  WRK-IDX
      *                                   入替対象インデックス取得
           COMPUTE     WRK-IDY        =   WRK-IDX  -   1
      *
           MOVE    SPA-GMN-TBLO (WRK-IDY) TO  WRK-SORT2-DATA (1)
           MOVE    "1"                TO  SPA-GMN-THENFLG(WRK-IDX)
           MOVE    SPA-GMN-TBLO (WRK-IDX)
                                      TO  SPA-GMN-TBLO   (WRK-IDY)
           MOVE    WRK-SORT2-DATA (1)
                                      TO  SPA-GMN-TBLO   (WRK-IDX)
           MOVE    WRK-IDX            TO  SPA-GMN-TNUM   (WRK-IDX)
           MOVE    WRK-IDY            TO  SPA-GMN-TNUM   (WRK-IDY)
      *
           MOVE    1                  TO  SPA-NAI-UPDOWN
      *
           IF      SPA-GMN-SELNUM     >   1
               COMPUTE SPA-GMN-SELNUM   =   SPA-GMN-SELNUM  -   1
           END-IF
           MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNUM
      *
           MOVE    63                  TO  SPA-GMN-CUR
           .
       4659-UKETUKE-UP-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   受付順変更処理（↓）
      *****************************************************************
       4759-UKETUKE-DOWN-SYORI-SEC         SECTION.
      *
           IF    (SPA-GMN-SELNUM         =  ZERO    )
               GO      TO      4759-UKETUKE-DOWN-SYORI-EXT
           END-IF
      *
      *    受付順変更
           PERFORM 41042-SELNUM-UKEUPCHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4759-UKETUKE-DOWN-SYORI-EXT
           END-IF
      *
           IF      (SPA-GMN-SELNUM     >   ZERO )
              AND  (SPA-GMN-SELNUM     <   SPA-UKE-MAX )
                CONTINUE
           ELSE
               GO      TO      4759-UKETUKE-DOWN-SYORI-EXT
           END-IF
      *                                   入替対象インデックス取得
           MOVE    SPA-GMN-SELNUM     TO  WRK-IDX
           COMPUTE     WRK-IDY        =   WRK-IDX  +   1
      *
           MOVE    SPA-GMN-TBLO (WRK-IDY) TO  WRK-SORT2-DATA (1)
           MOVE    "1"                TO  SPA-GMN-THENFLG(WRK-IDX)
           MOVE    SPA-GMN-TBLO (WRK-IDX)
                                      TO  SPA-GMN-TBLO   (WRK-IDY)
           MOVE    WRK-SORT2-DATA (1)
                                      TO  SPA-GMN-TBLO   (WRK-IDX)
           MOVE    WRK-IDX            TO  SPA-GMN-TNUM   (WRK-IDX)
           MOVE    WRK-IDY            TO  SPA-GMN-TNUM   (WRK-IDY)
      *
           MOVE    1                  TO  SPA-NAI-UPDOWN
      *
           COMPUTE SPA-GMN-SELNUM     =   SPA-GMN-SELNUM  +   1
           MOVE    SPA-GMN-SELNUM      TO  SPA-NAI-SELNUM
      *
           MOVE    63                  TO  SPA-GMN-CUR
           .
       4759-UKETUKE-DOWN-SYORI-EXT.
           EXIT.
      *H28.12
      *
      *H29.7
      *PUSH通信
      *****************************************************************
      *    PUSH通信処理
      *****************************************************************
       600-PUSH-SYORI-SEC             SECTION.
      *
           INITIALIZE                      PUSHAC02-REC
           MOVE    "patient_accept"        TO  PUSHAC02-EVENT
           EVALUATE    FLG-USERPG
           WHEN    1
      *        追加
               MOVE    "add"               TO  PUSHAC02-PMODE
           WHEN    2
      *        更新
               MOVE    "modify"            TO  PUSHAC02-PMODE
           WHEN    3
      *        削除
               MOVE    "delete"            TO  PUSHAC02-PMODE
           END-EVALUATE
           MOVE    SPA-GMN-PTNUM       TO  PUSHAC02-PTNUM
           MOVE    UKE-UKEYMD          TO  WRK-SYMD
           MOVE    UKE-UKETIME         TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSHAC02-UKEYMD
           MOVE    WRK-HEN-TIME        TO  PUSHAC02-UKETIME
           MOVE    UKE-UKEID           TO  PUSHAC02-UKEID
           MOVE    UKE-DRCD            TO  PUSHAC02-DRCD
           MOVE    UKE-SRYKA           TO  PUSHAC02-SRYKA
           MOVE    UKE-HKNCOMBI        TO  PUSHAC02-HKNCOMBI
      *
           MOVE    PUSHAC02-REC        TO  MCPDATA-REC
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_accept02"     TO  MCP-TABLE
           MOVE    "push_accept02"     TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       600-PUSH-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
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
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THHH            TO  WRK-HEN-THH
           MOVE    WRK-THMM            TO  WRK-HEN-TMM
           MOVE    WRK-THSS            TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    前回保険組合せの判定チェック
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-HKNCHK-FLG  NOT =   ZERO   )  AND 
                  (SPA-HKNCHK-CHK      =   ZERO   )
               IF      SPA-HKNCHK-FLG      =   2
                   MOVE    "K008"             TO  SPA-ERRCD
               ELSE
                   MOVE    "K009"              TO  SPA-ERRCD
               END-IF
               MOVE    1                   TO  SPA-HKNCHK-CHK
           END-IF
      *
           IF      SPA-U2ID1CD      NOT =   SPACE
               PERFORM 520-U2ID1CD-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "U02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      * 
      *    コラムリスト位置保存
           IF      FLG-GMN-INIT        =   1
               MOVE    ZERO                TO  WRK-U02-UKETUKE-ROW
           ELSE
               MOVE    U02-UKETUKE-ROW     TO  WRK-U02-UKETUKE-ROW
           END-IF
           IF      FLG-GMN-INIT2       =   1
               MOVE    ZERO                TO  WRK-U02-HKNCOMB-ROW
           ELSE
               MOVE    U02-HKNCOMBI-ROW    TO  WRK-U02-HKNCOMB-ROW
           END-IF
      *
           MOVE    SPACE                        TO  U02
           INITIALIZE                           U02
           IF      WRK-U02-UKETUKE-ROW NOT =   ZERO
               MOVE    WRK-U02-UKETUKE-ROW     TO  U02-UKETUKE-ROW
           END-IF
           IF      WRK-U02-HKNCOMB-ROW NOT =   ZERO
               MOVE    WRK-U02-HKNCOMB-ROW     TO  U02-HKNCOMBI-ROW
           END-IF
      *
           MOVE    SPA-GMN-SRYYMDH              TO  U02-SRYYMD
      *
           MOVE    SPA-GMN-SELNUM               TO  U02-SELNUM
           MOVE    SPA-GMN-PTNUM                TO  U02-PTNUM
           MOVE    SPA-GMN-NAME                 TO  U02-NAME
           MOVE    SPA-GMN-BIRTHDAY             TO  U02-BIRTHDAY
           MOVE    SPA-GMN-SEX                  TO  U02-SEX
           MOVE    SPA-GMN-NENREI               TO  U02-NENREI
      *    前回来院日
           MOVE    SPA-GMN-LASTYMD     TO  U02-LASTYMD
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST(IDX)  TO  U02-SRYKA-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  U02-SRYKA-COUNT
           MOVE    SPA-GMN-SRYKA-G     TO  U02-SRYKA
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST(IDX)    TO  U02-DRNAME-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  U02-DRNAME-COUNT
           MOVE    SPA-GMN-DRCD-G      TO  U02-DRNAME
      *!!
      *H28.01
           IF      SPA-DRCD-MAX        <   99
               MOVE    SPA-DRCD-MAX    TO  IDX
               ADD     1               TO  IDX
               MOVE    "****  医師一覧"
                                       TO  U02-DRNAME-ITEM(IDX)
               MOVE    IDX             TO  U02-DRNAME-COUNT
           END-IF
      *!!
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-SRYNAIYO-MAX
               MOVE    SPA-GMN-SRYNAILST(IDX)
                                           TO  U02-SRYNAIYO-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYNAIYO-MAX        TO  U02-SRYNAIYO-COUNT
           MOVE    SPA-GMN-SRYNAIYOCD-G           TO  U02-SRYNAIYO
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPGLST(IDX)
                                           TO  U02-USERPG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-USERPG-MAX        TO  U02-USERPG-COUNT
           MOVE    SPA-GMN-USERPG-G      TO  U02-USERPG
      *
      *    カルテ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-KARUTE-MAX
               MOVE    SPA-GMN-KARUTELST(IDX)
                                           TO  U02-KARUTE-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-KARUTE-MAX          TO  U02-KARUTE-COUNT
           IF      SPA-GMN-KARUTE-G        =   SPACE
               MOVE    SPA-GMN-KARUTELST (1)   TO  SPA-GMN-KARUTE-G
           END-IF
           MOVE    SPA-GMN-KARUTE-G        TO  U02-KARUTE
      *
      *    処方箋
           MOVE    SPA-GMN-SYOHOLST (1)    TO  U02-SYOHO-ITEM (1)
           MOVE    SPA-GMN-SYOHOLST (2)    TO  U02-SYOHO-ITEM (2)
           MOVE    2                       TO  U02-SYOHO-COUNT
           IF      SPA-GMN-SYOHO-G         =   SPACE
               MOVE    SPA-GMN-SYOHOLST (1)    TO  SPA-GMN-SYOHO-G
           END-IF
           MOVE    SPA-GMN-SYOHO-G         TO  U02-SYOHO
      *
           MOVE    SPA-GMN-COMBINUM    TO  U02-HKNCOMBI
      *
           IF      SPA-GMN-MEMOKBN     =   "1"
               MOVE    "【Ｍ】"            TO  U02-MEMOC-VALUE
               MOVE    "red"               TO  U02-MEMOC-STYLE
           END-IF
      *ver.4.7
           IF      (SPA-GMN-PTNUM      NOT =   SPACE )
               AND (SPA-GMN-COMBINUM       =   ZERO  )
               MOVE    "≪組合せ選択なし≫" 
                                           TO  U02-CHKMSG-VALUE
               MOVE    "red"               TO  U02-CHKMSG-STYLE
           ELSE
               MOVE    SPACE               TO  U02-CHKMSG-VALUE
           END-IF
      *
           IF      SPA-GMN-KOHMSG      =   SPACE
               MOVE    SPACE               TO  U02-KOHMSG-VALUE
           ELSE
               MOVE    SPA-GMN-KOHMSG      TO  U02-KOHMSG-VALUE
               MOVE    "red"               TO  U02-KOHMSG-STYLE
           END-IF
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-COMBI-MAX
               ADD     1                   TO  IDY
               MOVE    SPA-GMN-HKNCOMBINUM      (IDX)
                                                TO  U02-HCNUM (IDY)
               MOVE    SPA-GMN-SYU-TANSEIDONAME (IDX)
                                                TO  U02-HCHKN (IDY)
               MOVE    SPA-GMN-KOH1-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH1(IDY)
               MOVE    SPA-GMN-KOH2-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH2(IDY)
               MOVE    SPA-GMN-KOH3-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH3(IDY)
               MOVE    SPA-GMN-KOH4-TANSEIDONAME(IDX)
                                                TO  U02-HCKOH4(IDY)
      *
               IF      SPA-GMN-HKNCOMBINUM (IDX)
                                        =   SPA-GMN-COMBINUM
                   MOVE    "T"          TO  U02-HKNCOMBI-SELECT
                                                            (IDY)
                   MOVE    IDY              TO  U02-HKNCOMBI-ROW
                   MOVE    2                TO  U02-HKNCOMBI-ROWATTR
               ELSE
                   MOVE    "F"          TO  U02-HKNCOMBI-SELECT
                                                            (IDY)
               END-IF
           END-PERFORM
           MOVE    SPA-COMBI-MAX        TO  U02-HKNCOMBI-COUNT
      *
           MOVE    ZERO                TO  U02-MIKAKUNINN-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       5
               COMPUTE IDY             =   (SPA-GMN-PAGE  -   1  )
                                       *   5
                                       +   IDX
               IF      IDY             >   SPA-MIKAKU-MAX
                   MOVE    5               TO  IDX
               ELSE
                   MOVE    IDX             TO  U02-MIKAKUNINN-COUNT
                   MOVE    SPA-GMN-TMIKAKUNAME (IDY)
                                           TO  U02-MLSEIDONAME (IDX)
                   MOVE    SPA-GMN-TMIKIGONUM  (IDY)
                                           TO  U02-MLKIGONUM   (IDX)
                   MOVE    SPA-GMN-TMIKAKUYMD (IDY)
                                           TO  U02-MLLASTYMD   (IDX)
      *            保険確認
                   IF      SPA-NAI-KAKUNINYMD (IDY)(1:6)
                                           =   SPA-SYSYMD (1:6)
                       EVALUATE    IDX
                           WHEN    1
                               MOVE    "済"        TO  U02-BHK1-LABEL
                               MOVE    "T"         TO  U02-BHK1-VALUE
                           WHEN    2
                               MOVE    "済"        TO  U02-BHK2-LABEL
                               MOVE    "T"         TO  U02-BHK2-VALUE
                           WHEN    3
                               MOVE    "済"        TO  U02-BHK3-LABEL
                               MOVE    "T"         TO  U02-BHK3-VALUE
                           WHEN    4
                               MOVE    "済"        TO  U02-BHK4-LABEL
                               MOVE    "T"         TO  U02-BHK4-VALUE
                           WHEN    5
                               MOVE    "済"        TO  U02-BHK5-LABEL
                               MOVE    "T"         TO  U02-BHK5-VALUE
                       END-EVALUATE
                   ELSE
                       EVALUATE    IDX
                           WHEN    1
                               MOVE    "未"        TO  U02-BHK1-LABEL
                               MOVE    "F"         TO  U02-BHK1-VALUE
                           WHEN    2
                               MOVE    "未"        TO  U02-BHK2-LABEL
                               MOVE    "F"         TO  U02-BHK2-VALUE
                           WHEN    3
                               MOVE    "未"        TO  U02-BHK3-LABEL
                               MOVE    "F"         TO  U02-BHK3-VALUE
                           WHEN    4
                               MOVE    "未"        TO  U02-BHK4-LABEL
                               MOVE    "F"         TO  U02-BHK4-VALUE
                           WHEN    5
                               MOVE    "未"        TO  U02-BHK5-LABEL
                               MOVE    "F"         TO  U02-BHK5-VALUE
                       END-EVALUATE
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-UKE-MAX
               MOVE    IDX                   TO  U02-UKETUKE-COUNT
      *
               MOVE    SPA-GMN-TNUM  (IDX)   TO  WRK-NUM
               MOVE    WRK-NUM               TO  U02-ULNUM  (IDX)
               IF  SPA-GMN-TYYKTIME(IDX)  NOT   =  SPACE
                   MOVE    SPA-GMN-TYYKTIME(IDX)(1:2) 
                                                TO  WRK-THH
                   MOVE    SPA-GMN-TYYKTIME(IDX)(3:2) 
                                                TO  WRK-TMM
                   MOVE    WRK-HENTIME          TO  U02-ULYYKTIME(IDX)
               END-IF
               IF  SPA-GMN-TUKETIME(IDX)  NOT   =  SPACE
                   MOVE    SPA-GMN-TUKETIME(IDX)(1:2) 
                                          TO  WRK-THH
                   MOVE    SPA-GMN-TUKETIME(IDX)(3:2) 
                                          TO  WRK-TMM
                   MOVE    WRK-HENTIME    TO  U02-ULUKETUKETIME(IDX)
               END-IF
      *
      *H28.12
               IF      SPA-GMN-THENFLG (IDX)   =   "1"
                   IF     (SPA-NAI-SYORI1      =   0  )
                      AND (SPA-NAI-UPOK        =   0  )
                       MOVE    "**"       TO  U02-ULUKETUKETIME(IDX)
                                                           (7:2)
                   ELSE
                       MOVE    "*"        TO  U02-ULUKETUKETIME(IDX)
                                                           (8:1)
                   END-IF
               END-IF
      *
               MOVE    SPA-GMN-TPTNUM     (IDX) TO  U02-ULPTNUM   (IDX)
               MOVE    SPA-GMN-TNAME      (IDX) TO  U02-ULNAME    (IDX)
               MOVE    SPA-GMN-TNENREI    (IDX) TO  U02-ULNENREI  (IDX)
               MOVE    SPA-GMN-TSEX       (IDX) TO  U02-ULSEX     (IDX)
               MOVE    SPA-GMN-TSRYKA-G   (IDX) TO  U02-ULSRYKA   (IDX)
               MOVE    SPA-GMN-TDRCD-G    (IDX) TO  U02-ULDRNAME  (IDX)
               MOVE    SPA-GMN-TSRYNAIYOCD-G(IDX)
                                                TO  U02-ULSRYNAIYO(IDX)
               MOVE    SPA-GMN-TSRYNAIYOCD-G(IDX)
                                                TO  U02-ULSRYNAIYO(IDX)
               MOVE    SPA-GMN-THKNCOMBI-G (IDX)
                                                TO  U02-ULHKNCOMBI(IDX)
               MOVE    SPA-GMN-TRRKYMD  (IDX)   TO  U02-ULRRKYMD  (IDX)
               MOVE    SPA-GMN-TSRFLG   (IDX)   TO  U02-ULSRFLG   (IDX)
      *
               IF      SPA-GMN-TNUM  (IDX)  =   SPA-NAI-SELNUM
                   MOVE    "T"              TO  U02-UKETUKE-SELECT
                                                            (IDX)
                   MOVE    IDX              TO  U02-UKETUKE-ROW
                   MOVE    2                TO  U02-UKETUKE-ROWATTR
               ELSE
                   MOVE    "F"              TO  U02-UKETUKE-SELECT
                                                            (IDX)
               END-IF
            END-PERFORM
            MOVE    SPA-UKE-MAX        TO  U02-UKETUKE-COUNT
      *
           EVALUATE    SPA-NAI-SYORI1
               WHEN    0
                   MOVE    "現在の予約、受付状況"  TO  U02-LABEL-VALUE
                   MOVE    "black"                 TO  U02-LABEL-STYLE
               WHEN    1
                   MOVE    "現在の受付状況"        TO  U02-LABEL-VALUE
                   MOVE    "black"                 TO  U02-LABEL-STYLE
      *????        IF      SPA-NAI-YYKARI      =   1
      *                MOVE    "現在の受付状況（予約あり）"
      *                                            TO  U02-LABEL-VALUE
      *????        END-IF
               WHEN    2
                   MOVE    "現在の予約状況"        TO  U02-LABEL-VALUE
                   MOVE    "black"                 TO  U02-LABEL-STYLE
           END-EVALUATE
      *
      *H28.12
           IF      SPA-NAI-UPOK        =   1
      *        受付のみ表示中
               IF      SPA-NAI-UKEUP       =   1
                   MOVE    "受付順更新"        TO  U02-B03S-LABEL
                   MOVE    "＜受付順更新＞"    TO  U02-UPMSG-VALUE
                   MOVE    "blue"              TO  U02-UPMSG-STYLE
               ELSE
                   MOVE    "受付順変更"        TO  U02-B03S-LABEL
               END-IF
               MOVE    WIDGET-NORMAL       TO  U02-B03S-STATE
           ELSE
               MOVE    SPACE               TO  U02-B03S-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  U02-B03S-STATE
           END-IF
      *H28.12
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
            .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           MOVE    WIDGET-NORMAL       TO  U02-PTNUM-STATE
           MOVE    WIDGET-NORMAL       TO  U02-NAME-STATE
      *
      *    選択番号が入力時は、患者番号入力不可
           IF     (SPA-NAI-SELNUM  NOT =   ZERO )  AND
                  (SPA-GMN-PTNUM   NOT =   SPACE)
               MOVE    WIDGET-INSENSITIVE      TO  U02-PTNUM-STATE
           END-IF
      *    患者氏名入力不可
           IF     (SPA-GMN-PTNUM   NOT =   SPACE)   AND
                  (SPA-GMN-NAME    NOT =   SPACE)
               MOVE    WIDGET-INSENSITIVE      TO  U02-NAME-STATE
           END-IF
      *
      *業務処理権限設定
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B05-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B05-STATE
           END-IF
      *    診療行為
           IF      SPA-GSRAUTH-21      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B06-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B06-STATE
           END-IF
      *    病名登録
           IF      SPA-GSRAUTH-22      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B07-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B07-STATE
           END-IF
      *    収納登録
           IF      SPA-GSRAUTH-23      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B08-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B08-STATE
           END-IF
      *    会計照会
           IF      SPA-GSRAUTH-24      =   "1"
               MOVE    WIDGET-NORMAL       TO  U02-B09S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  U02-B09S-STATE
           END-IF
      ***  ＱＲ一覧
      *    IF      SPA-QRFLG               =   ZERO
      *        MOVE    WIDGET-INSENSITIVE  TO  U02-B10S-STATE
      *    ELSE
      *        MOVE    WIDGET-NORMAL       TO  U02-B10S-STATE
      *****END-IF
      *    患者メモ
           MOVE    WIDGET-NORMAL       TO  U02-B10S-STATE
      *v4.8
      *    ユーザプログラム
           MOVE    WIDGET-NORMAL       TO  U02-USERPG-STATE
      **** IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    WIDGET-INSENSITIVE  TO  U02-USERPG-STATE
      **** END-IF
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
      *    カーソル指定のない時、入力した項目の次へ
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM"    TO  MCP-WIDGET
               WHEN    02
                   MOVE    "PTNUM"      TO  MCP-WIDGET
               WHEN    03
                   MOVE    "NAME"       TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SRYKA"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRNAME"     TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYNAIYO"   TO  MCP-WIDGET
               WHEN    07
                   MOVE    "KARUTE"     TO  MCP-WIDGET
               WHEN    08
                   MOVE    "SYOHO"      TO  MCP-WIDGET
               WHEN    09
                   MOVE    "HKNCOMBI"   TO  MCP-WIDGET
               WHEN    10
                   MOVE    "USERPG"     TO  MCP-WIDGET
               WHEN    58
                   MOVE    "B05"        TO  MCP-WIDGET
               WHEN    62
                   MOVE    "B12"        TO  MCP-WIDGET
               WHEN    63
                   MOVE    "B03S"       TO  MCP-WIDGET
           END-EVALUATE
           .
      *
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC          SECTION.
      *
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "PTNUM"
                   IF      SPA-GMN-NAME        =   SPACE
                       MOVE    3               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "NAME"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "SRYKA"
                   MOVE    5               TO  SPA-GMN-CUR
      *            患者指定がなければ、選択へ
                   IF     (SPA-GMN-NAME        =   SPACE )
                   AND    (SPA-GMN-PTNUM       =   SPACE )
                   AND    (SPA-UKE-MAX         >   ZERO  )
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
               WHEN    "DRNAME"
                   MOVE    6               TO  SPA-GMN-CUR
               WHEN    "SRYNAIYO"
                   MOVE    7               TO  SPA-GMN-CUR
               WHEN    "KARUTE"
                   MOVE    8               TO  SPA-GMN-CUR
               WHEN    "SYOHO"
      *            マスタに追加したため
                   IF      SPA-COMBI-MAX       =   ZERO
                       MOVE    62                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    9                   TO  SPA-GMN-CUR
                   END-IF
               WHEN    "HKNCOMBI"
                   IF      SPA-USERPG-MAX  =   1
                       MOVE    62              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    10              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "USERPG"
                   MOVE    62              TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-U2ID1CD-SEC              SECTION.
      *
           MOVE    SPACE           TO  WRK-U2ID1MSG
      *
           EVALUATE    SPA-U2ID1CD
               WHEN    "0101"
                   STRING  "患者番号で予約があります。"
                           "予約を選択しますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-U2ID1MSG
                   END-STRING
               WHEN    "0102"
                   STRING  "患者番号で受付があります。"
                           "受付を選択しますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-U2ID1MSG
                   END-STRING
               WHEN    "0103"
                   STRING  "受付を取消します。"
                           "よろしいですか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-U2ID1MSG
                   END-STRING
               WHEN    "0105"
                   STRING  "受付されていません。"
                           "選択を解除して患者登録へ遷移しますか？"
                                               DELIMITED  BY  SIZE
                                               INTO  WRK-U2ID1MSG
                   END-STRING
           END-EVALUATE
      *
           MOVE    SPACE                TO  U2ID1
           INITIALIZE                       U2ID1
           MOVE    SPA-U2ID1CD          TO  U2ID1-ID1CODE
           MOVE    WRK-U2ID1MSG         TO  U2ID1-ID1MSG
      *
           MOVE    "戻る"               TO  U2ID1-B01
           MOVE    "ＯＫ"               TO  U2ID1-B12
           EVALUATE    SPA-U2ID1CD
               WHEN    "0101"
               WHEN    "0102"
                   MOVE    "ＮＯ"           TO  U2ID1-B01
                   MOVE    "ＯＫ"           TO  U2ID1-B12
           END-EVALUATE
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "U02"                TO  SPA-MOTOPG
           MOVE    "U2ID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "U2ID1"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
           .
      *
       520-U2ID1CD-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE           TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "患者氏名を入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "該当の患者番号が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "患者氏名は全角で入力して下さい"
                                               TO  SPA-ERRMSG
               WHEN    "0004"
      *            MOVE    "受付されていません"
      **                                        TO  SPA-ERRMSG
                   STRING  "患者登録されていない予約です。"
                           "受付をしてから患者登録をして下さい。"
                                               DELIMITED   BY  SIZE
                                               INTO    SPA-ERRMSG
                   END-STRING
      *----- (01.00.04) LINE ADD START --------------------------------
               WHEN    "0005"
                   MOVE    "未登録の患者です"   TO  SPA-ERRMSG
      *----- (01.00.04) LINE ADD END ----------------------------------
               WHEN    "0009"
                   MOVE    "登録できません"     TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "カルテ入力エラー"   TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "対象となる保険がありません"
                                                TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "処方箋入力エラー"   TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE
                   "カルテ、処方箋が選んであるので入力して下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "保険組合せ入力エラー"
                                                TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "選択番号入力エラー" TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "取消するデータを選んで下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "受付されていないデータです"
                                                TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "診療科がありません。"
                                                TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "ドクター入力エラー" TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "診療内容入力エラー" TO  SPA-ERRMSG
               WHEN    "0022"
                   MOVE    "新規患者です。患者登録後、遷移して下さい"
                                                TO  SPA-ERRMSG
               WHEN    "0023"
                   MOVE    "対象の保険組合せがありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "診療行為は入力できません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "0024"
                   MOVE    "カルテ印刷に必要な情報が入力されてません。"
                                                TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE
                   "処方せん印刷に必要な情報が入力されてません。"
                                                TO  SPA-ERRMSG
               WHEN    "0026"
                   MOVE
                   "診療科と診療内容は必ず選択して下さい。"
                                                TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE
                   "選択中です。受付一覧の更新はできません。"
                                                TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "患者登録の業務権限はありません。"
                                                TO  SPA-ERRMSG
                   MOVE    "患者登録はできません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "0029"
                   MOVE    "ユーザプログラム起動選択エラー"
                                                TO  SPA-ERRMSG
               WHEN    "K001"
                   MOVE
                   "警告！同じ診療科・保険組合せで受付済みです。"
                                                TO  SPA-ERRMSG
               WHEN    "K009"
                   STRING  "警告！！前回と保険組合せが違います。"
                           "確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K008"
                   STRING  "警告！！"
                           "前回の保険組合せは削除になっています。"
                           "保険組合せを確認して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K010"
                   STRING  "警告！！"
                           "保険組合せが入院非表示となっています。"
                           "入院では使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K011"
                   STRING  "警告！！"
                           "保険組合せが外来非表示となっています。"
                           "外来では使用できません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "K012"
                   STRING  "警告！！"
                           "保険組合せの選択がありません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0030"
                   STRING  "患者番号がありません。"
                           "メモ登録はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0031"
                   STRING  "患者番号で受付がありませんでした。"
                           "再度、患者番号を入力して下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *H28.2 
               WHEN    "0032"
                   STRING  "前回選択は有効ではありません。"
                           "再度、選択をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0033"
                   STRING  "予約が来院済みとなっています。"
                           "再度、選択をして下さい。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *H28.12
               WHEN    "0034"
                   STRING  "患者が選択中です。"
                           "受付順変更はできません。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0035"
                   STRING  "受付順変更中です。"
                                               DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "2001"
                   MOVE    "前の未確認保険はありません。"
                                                TO  SPA-ERRMSG
               WHEN    "2002"
                   MOVE    "次の未確認保険はありません。"
                                                TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE
                   "受付マスタ（管理）の更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "受付マスタの追加処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE    "受付マスタの更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1004"
                   MOVE    "予約マスタの更新処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1005"
                   MOVE    "受付マスタの削除処理でエラーになりました"
                                                TO  SPA-ERRMSG
               WHEN    "1006"
                   STRING  "既に受付ＩＤが登録されています。"
                           "再度、受付登録をして下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1007"
                   MOVE    "受付ＩＤが９９９９９を超えます。"
                                                TO  SPA-ERRMSG
                   MOVE    "これ以上受付はできません。"
                                                TO  SPA-ERRMSG(33:)
               WHEN    "1008"
                   STRING  "受付が削除されています。"
                           "更新はできません。"
                           "再度、受付登録をして下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1009"
                   STRING  "受付は受診済みとなっています。"
                           "更新はできません。"
                           "再度、受付登録をして下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1010"
                   STRING  "選択番号の受付情報が選択されていません。"
                           "再度、受付を選択して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1012"
                   STRING  "選択番号の情報が受付選択されていません。"
                           "再度、選択番号を入力して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1011"
                   MOVE    "患者保険情報更新エラー"
                                                TO  SPA-ERRMSG
               WHEN    "0200"
                   MOVE    "患者登録処理済みのＱＲデータはありません。"
                                                TO  SPA-ERRMSG
      *
               WHEN    "K002"
                   STRING  "警告！！老人保険の割合が設定されていません"
                           "。保険を変更して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING  "警告！！老人年齢に到達しました。"
                           "老人公費を登録して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING  "警告！！老人の保険割合が２割です。"
                           "３割に保険を登録し直して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K004"
                   STRING  "警告！！前期高齢者到達日以降に保険の確認が"
                           "ありません。保険を確認して下さい"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K006"
                   STRING  "警告！！"
                           "保険組合せに矛盾があります。"
                           "患者登録で保険を登録し直して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
      *ver.4.7
               WHEN    "K007"
                   STRING  "警告！！"
                           "６５歳以上です。"
                           "退職国保は対象ではありません。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K013"
                   STRING  "警告！！"
                           "後期高齢者の年齢です。"
                           "後期高齢者の保険を登録して下さい。"
                                                DELIMITED  BY  SIZE
                                                INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
      *
           END-EVALUATE
      *
           MOVE    SPACE                TO  U2ERR
           MOVE    SPA-ERRCD            TO  U2ERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  U2ERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "U02"                TO  SPA-MOTOPG
           MOVE    "U2ERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "U2ERR"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
123900     .
124000 510-ERRSET-EXT.
124100     EXIT.
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
      *    西暦和暦変更
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    ALL "9"             TO  WRK-WYMD
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY1-AREA
               MOVE    "11"                TO  LNK-DAY1-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY1-AREA
               MOVE    LNK-DAY1-GENGO      TO  WRK-WYMD(1:1)
               MOVE    LNK-DAY1-YMD(3:6)   TO  WRK-WYMD(2:6)
           END-IF
      *
           .
       500-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    YYK-REC             TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-YOYAKU-READ-SEC
           ELSE
                MOVE    1              TO  FLG-YYK
           END-IF
      *    カーソルクロース
           MOVE    "tbl_yyk"           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       910-YOYAKU-READ-SEC         SECTION.
      *
           MOVE    "tbl_yyk"           TO  MCP-TABLE
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
       910-YOYAKU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込処理（主キー検索、読み込み）
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-UKETUKE-READ-SEC
           ELSE
                MOVE    1              TO  FLG-UKETUKE
           END-IF
      *    カーソルクロース
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスター読込処理
      *****************************************************************
       910-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-UKETUKE
               MOVE    MCPDATA-REC         TO  UKETUKE-REC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       910-UKETUKE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込処理
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
      *    患者マスター読込処理
      *****************************************************************
       910-PTINF-READ-SEC        SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTINF
               MOVE    MCPDATA-REC         TO  PTINF-REC
           ELSE
               MOVE    1                   TO  FLG-PTINF
           END-IF
      *
           .
       910-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
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
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理主キー検索
      *****************************************************************
       9009-SYSKANRI-KEY-READ-SEC         SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
               ELSE
                   INITIALIZE                  SYSKANRI-REC
               END-IF
           ELSE
               INITIALIZE                  SYSKANRI-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9009-SYSKANRI-KEY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ主キー検索
      *****************************************************************
       9001-HKNCOMBI-KEY-READ-SEC         SECTION.
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-COMB-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9001-HKNCOMBI-KEY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-COMB-READ-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
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
       900-COMB-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者保険マスター読込
      *****************************************************************
       900-PTHKNINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_pthkninf"      TO  MCP-TABLE
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
       900-PTHKNINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費マスター読込
      *****************************************************************
       900-PTKOHINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
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
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号（主キー）読込処理
      *****************************************************************
       900-HKNNUM-INV-SEC         SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNNUM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HKNNUM
           END-IF
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-HKNNUM-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       900-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       900-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者労災保険情報読み込み
      *****************************************************************
       993-PTRSIINF-NEXT-SEC        SECTION.
      *
           MOVE    "tbl_ptrsiinf"      TO  MCP-TABLE
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
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    QRコード(被保険者証)読込
      *****************************************************************
       900-QRCDHKN-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  QRCDHKN-REC
               MOVE    ZERO            TO  FLG-QRCDHKN
           ELSE
               INITIALIZE                  QRCDHKN-REC
               MOVE    1               TO  FLG-QRCDHKN
           END-IF
           .
       900-QRCDHKN-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
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
           .
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
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
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
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"                   TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
