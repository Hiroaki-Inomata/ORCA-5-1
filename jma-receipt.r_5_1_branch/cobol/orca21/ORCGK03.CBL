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
       PROGRAM-ID.                 ORCGK03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 請求確認　（Ｋ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01/05/21  今回請求額がゼロ以上であること
      *  01.00.02    MCC−多々納  01/06/29  年齢表示
      *  01.00.03    MCC−多々納  01/08/20  処方せんをＡ５（ＨＣ１９）へ
      *                                     画面変更に伴う修正
      *  02.00.00    MCC−多々納  01/12/17  請求額がゼロの時、ゼロ表示へ
      *  02.00.01    MCC−多々納  02/02/01  ドクターを追加
      *  02.00.02    NACL−多々納 02/08/26  薬剤情報発行を追加
      *  02.00.03    NACL−多々納 02/08/29  同一剤が同一履歴に存在する時
      *                                     受診履歴に編集しない
      *  02.00.04    NACL−多々納 02/09/13  自賠責追加
      *  02.00.05    NACL-多々納  02/11/11  ＤＵＭＭＹ診察料追加
      *  02.00.06    NACL-多々納  02/11/15  自費消費税対応修正
      *  02.00.07    NACL-多々納  02/12/05  初診料算定日追加
      *  02.00.08    NACL-多々納  02/12/11  ドクターエラーカーソル位置修正
      *  02.00.09    NACL-多々納  02/12/16  ドクター未入力ＯＫ追加
      *  02.00.10    NACL-多々納  02/12/17  診療種別の判定追加
      *  02.00.11    NACL-多々納  02/12/18  継続の時今回請求額を編集する
      *  02.00.12    NACL-多々納  03/01/08  入金額の上限修正
      *  02.00.13    NACL-多々納  03/02/13  訂正時の診療科履歴修正
      *  02.00.14    NACL-多々納  03/03/04  複数診療科まとめ追加
      *                                     収納明細削除追加
      *  02.00.15    NACL-多々納  03/03/05  排他制御処理追加
      *  02.00.16    NACL-多々納  03/08/27  剤明細５０へ変更
      *  02.00.17    NACL-多々納  04/01/07  更新をサブプロへ他
      *  02.00.18    NACL-多々納  04/07/XX  複数科・保険一括入力対応
      *  02.00.19    NACL-多々納  04/10/21  CLAIMにドクター追加
      *  02.00.20    NACL-多々納  05/10/11  ユーザプログラム起動追加
      *  02.00.21    NACL-多々納  05/11/18  MONFUNC 対応
      *  02.00.23    NACL-多々納  06/04/10  診療費明細書発行追加
      *  02.00.24    NACL-多々納  06/07/06  収納変更対応
      *  03.02.00    NACL-多々納  06/09/XX  返金処理対応
      *  03.04.00    NACL-多々納  07/02/06  処方せん等の訂正時印刷無し追加
      *  03.04.01    NACL-多々納  07/02/27  複数保険に包括保険追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  03.05.01    NACL-多々納  07/08/06  訂正請求書合計印刷対応
      *  04.02.00    NACL-多々納  08/03/05  お薬手帳発行区分追加
      *  04.04.00    NACL-多々納  09/01/23  予約票発行区分追加
      *  04.05.00    NACL-多々納  09/12/15  調整金２対応
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *  04.06.00    NACL-多々納  10/08/02  入院未収額変更
      *                                     未収額・過入金額反映対応
      *  04.06.00    NACL-多々納  10/11/05  発行日変更対応
      *  04.06.00    NACL-多々納  10/11/16  マル長負担額を保険負担額反映
      *  04.06.01    NACL-伊藤    11/01/28  リアルタイム送信
      *  04.07.01    NACL-多々納  12/01/05  クライアント印刷対応
      *  04.07.01    NACL-多々納  13/07/23  お薬手帳ＣＳＶ患者毎対応
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.08.00    NACL-多々納  15/08/05  領収・明細　請求書不要対応
      *  05.00.00    NACL-多々納  17/07/XX  HAORI対応
      *  05.00.00    NACL-多々納  17/07/XX  PUSH通信対応
      *  05.00.00    NACL-多々納  19/01/XX  HAORI複数科保険対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME
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
      *01  FILE-NAME.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM        PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID         PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K03SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
           03  FLG-SYOSIN-DUMMY    PIC 9(01).
      *
           03  FLG-TOKTEI          PIC 9(01).
      *
           03  FLG-CLAIM           PIC 9(01).
           03  FLG-CLAIM-OK        PIC 9(01).
      *
           03  FLG-CHK-OK              PIC 9(01).
      *    複数科まとめ有無
           03  FLG-FUKU-FLG            PIC 9(01).
      *
           03  FLG-FUKU                PIC 9(01).
           03  FLG-TOUROKU             PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-USERPG              PIC 9(01).
      *
           03  FLG-JYOTAI              PIC 9(01).
           03  FLG-SOUSIN              PIC 9(01).
      *    感染症削除
           03  FLG-INFEC-DLT           PIC 9(01).
      *    クライアント印刷有
      **** 03  FLG-CLIENT-OK           PIC 9(01).
      *!!!!
      *H30.6
           03  FLG-ERRCHK              PIC 9(01).
      *R01.5
           03  FLG-INENTER             PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX4                PIC 9(04).
           03  IDX-P               PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(02).
           03  IDX-3               PIC 9(02).
      *
           03  IDS1               PIC 9(04).
           03  IDX-SYU            PIC 9(04).
      *
           03  IDH1               PIC 9(04).
           03  IDH2               PIC 9(04).
      *R02.2
           03  IDXD               PIC 9(04).
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
      *
      *****03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z99             PIC --,---,--9.
           03  WRK-Z9-G.
               05  WRK-Z9              PIC --,---,---.
               05  WRK-Z9-9        REDEFINES  WRK-Z9
                                       PIC --,---,--9.
           03  WRK-Z92             PIC --,---,---.
           03  WRK-Z9-1            PIC --,---,--9.
           03  WRK-Z9-2            PIC --,---,--9.
      *
           03  WRK-Z97             PIC ZZZZ,ZZ9.
      *
           03  WRK-ZZ9             PIC ZZ9.
      *
           03  WRK-HEN             PIC X(10).
           03  WRK-HEN-1           PIC X(10).
           03  WRK-HEN-2           PIC X(10).
      *
           03  WRK-KIN             PIC S9(07).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-JIHIMONEY           PIC  9(08).
      *
      *H30.6
      *!!  03  WRK-SEIKYU              PIC S9(07).
      *!!  03  WRK-SEIGOK              PIC S9(07).
           03  WRK-SEIKYU              PIC S9(08).
           03  WRK-SEIGOK              PIC S9(08).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-MEIBAN          PIC 9(03).
      *    回数
           03  WRK-KAISU           PIC 9(08).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-MCP-PUTTYPE     PIC X(8).
      *
           03  WRK-TIME1.
               05  WRK-TIME11      PIC 9(08).
      *
           03  WRK-TIME2.
               05  WRK-TIME22      PIC 9(04).
      *
           03  WRK-DEL-RENNUM              PIC 9(01).
           03  WRK-JYURRK-RENNUM           PIC 9(01).
           03  WRK-JYURRK-EDANUM           PIC 9(01).
           03  WRK-JYURRK-DOUJI-RENNUM     PIC 9(01).
           03  WRK-KAIKEI-RENNUM           PIC  9(03).
      *
           03  WRK-PATH                    PIC X(64).
      *
           03  WRK-KIDMSG              PIC X(80).
           03  WRK-RENNUM-KEY          PIC 9(01).
      *請求金額
           03  WRK-KON-SKYMONEY        PIC  9(07).
      *
           03  WRK-TEISEI-AREA.
      *請求金額
               05  WRK-SKYMONEY            PIC S9(07).
               05  WRK-SKYMONEY2           PIC S9(07).
      *差額
               05  WRK-SAGAKU              PIC S9(07).
      *消費税（再掲）
               05  WRK-SKYMONEY-TAX-SAI    PIC S9(07).
      *入金合計額
               05  WRK-NYUKIN-TOTAL        PIC S9(07).
      *入金回数
           03  WRK-NYUKIN-KAISU        PIC  9(07).
      *伝票番号枝番
           03  WRK-DENPEDANUM          PIC  9(02).
      *入金連番
           03  WRK-NYUKINRENNUM        PIC  9(02).
      *伝票状態区分
           03  WRK-DENPJTIKBN          PIC  X(01).
      *入金額
           03  WRK-NYUKIN              PIC S9(07).
           03  WRK-CHOSEI              PIC S9(07).
           03  WRK-CHOSEI1             PIC S9(07).
           03  WRK-CHOSEI2             PIC S9(07).
           03  WRK-HZN-NYUKIN          PIC S9(07).
      *前調整金
           03  WRK-MAE-CHOSEI          PIC S9(07).
           03  WRK-MAE-CHOSEI1         PIC S9(07).
           03  WRK-MAE-CHOSEI2         PIC S9(07).
      *R02.2
           03  WRK-MAE-SKYMONEY        PIC S9(07).
      *入院未収額
           03  WRK-NYUIN-MISYU         PIC S9(07).
           03  WRK-HENKIN              PIC S9(07).
           03  WRK-HENKIN-GAKU         PIC S9(07).
      *
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *    請求書ＰＧ名
           03  WRK-SEIKYU-PG           PIC X(20).
      *    診療費明細書ＰＧ名
           03  WRK-MEISAI-PG           PIC X(20).
      *    予約票ＰＧ名
           03  WRK-YYKHYO-PG           PIC X(20).
      *    入力項目名
           03  WRK-MCP-WIDGET          PIC X(64).
      *
           03  WRK-GMN-SELNUM          PIC 9(02).
      *
           03  WRK-PTID                   PIC X(10).
      *    診療種別
           03  WRK-SRYSYUKBN              PIC X(03).
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
      *    複数科集計用
           03  WRK-SYUKEI-AREA.
               05  WRK-SUM-SKYMONEY            PIC  9(07).
               05  WRK-SUM-HKNTEN              PIC  9(07).
      *
               05  WRK-SUM-MONEY               PIC  9(07).
               05  WRK-SUM-TGMONEY             PIC  9(07).
               05  WRK-SUM-TGMONEY-TAX         PIC  9(07).
               05  WRK-SUM-JIHI-TOTAL          PIC  9(07).
               05  WRK-SUM-JIHI-TOTAL-TAX      PIC  9(07).
               05  WRK-SUM-JIHI-TAX            PIC  9(07).
               05  WRK-SUM-YKZ-FTN             PIC  9(07).
      *
      *    保険組合せ保存
           03  WRK-HZN-HKNCOMBINUM     PIC X(04).
           03  WRK-HZN-HKNKBN          PIC X(01).
      *
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-SRYKA               PIC X(02).
      *
           03  WRK-RENNUM              PIC 9(02).
      *
           03  WRK-SKYPRTFLG           PIC X(01).
      *    発行日保存
           03  WRK-GMN-HAKYMD          PIC X(10).
           03  WRK-NAI-HAKYMD          PIC X(08).
           03  WRK-MAE-HAKYMD          PIC X(08).
      *
      *HAORI
           03  WRK-HAORI-AREA.
               05  WRK-NYUKIN-JYOTAI       PIC X(01).
               05  WRK-NYUKIN-HOHO         PIC X(02).
               05  WRK-NYUKINKBN           PIC X(01).
               05  WRK-HR-NYUKIN           PIC S9(07).
               05  WRK-HR-HENKIN           PIC S9(07).
               05  FLG-HAORI-NYU           PIC 9(01).
               05  FLG-HAORI-HEN           PIC 9(01).
               05  FLG-HAORI-IKT           PIC 9(01).
               05  WRK-CHOSEI1-X           PIC X(11).
               05  WRK-CHOSEI2-X           PIC X(11).
      *
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
      *ORCAサーベイランス一時退避
           03  WRK-INF-AREA.
               05  WRK-INF-HOSPNUM     PIC 9(02).
               05  WRK-INF-HOSPID      PIC X(24).
               05  WRK-INF-SYORIFLG    PIC 9(01).
               05  WRK-INF-NYUGAIKBN   PIC X(01).
               05  WRK-INF-PTID        PIC 9(10).
               05  WRK-INF-SRYYMD      PIC X(08).
               05  WRK-INF-SRYKA       PIC X(02).
               05  WRK-INF-BEDFLG      PIC 9(01).
      *
      *診療情報フォーマット一時退避
           03  WRK-JMR-AREA.
               05  WRK-JMR-HOSPNUM     PIC 9(02).
               05  WRK-JMR-NYUGAIKBN   PIC X(01).
               05  WRK-JMR-PTID        PIC 9(10).
               05  WRK-JMR-SRYYMD      PIC X(08).
               05  WRK-JMR-DRCD        PIC X(05).
               05  WRK-JMR-OPID        PIC X(16).
      *
      *    剤テーブル領域
       01  WRK-TBL-ZAINUM-AREA.
           03  WRK-TBL-ZAINUM-G            OCCURS   100.
               05  WRK-TBL-ZAINUM          PIC 9(08).
           03  WRK-TBL-IDX                 PIC 9(04).
           03  FLG-ZAINUM                  PIC 9(01).
      *
      *    まとめテーブル領域
       01  TBL-FUKU-AREA.
           03  TBL-FUKU-OCCURS             OCCURS   5.
               05  TBL-FUKU-DENPNUM        PIC 9(07).
               05  TBL-FUKU-MATKBN         PIC X(01).
               05  TBL-FUKU-CNT            PIC 9(04).
               05  TBL-FUKU-SRYKA          PIC X(01).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
       01  WRK-COM-FTNHEN.
           03  FILLER              PIC X(50)   VALUE
           "★★　収納業務で一括再計算を行ってください　★★".
      *
       01  WRK-SYSTIME.
           03  WRK-SYSTIME-X       PIC X(08).
      *
      *    感染症ファイル名
       01  INFECTION-FILE          PIC X(17)   VALUE
                                   "infectiondata2.sh".
      *
      *    日医版診療情報ファイル名
       01  JMAMR-FILE              PIC X(08)   VALUE   "jmamr.sh".
      *
      *    ＣＬＡＩＭファイル名
       01  CLAIM-FILE.
           03  FILLER              PIC X(30)   VALUE
               "scripts/claim/HL02.sh".
      *
      *    ファイルデイレクトリ位置指定サブ
           COPY  "CPMKPASS.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0041.INC".
      *
           COPY    "CPSK1013.INC".
      *    ドクター
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    診療行為一括入金管理情報
           COPY    "CPSK1038.INC".
      *    入金方法
           COPY    "CPSK1041.INC".
      *
      *　　（ＣＬＡＩＭ接続）
           COPY    "CPSK9000.INC".
           COPY    "CPSK9001.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納合計マスタ−
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
      *
      *v4.8
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
      *D   COPY    "CPORCSDAY.INC".
      *D   COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *D   COPY    "CPORCSPTID.INC".
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    負担金額計算用パラメタ
      *    COPY    "CPORCS01.INC".
      *
      *    処方箋印刷パラメタ
      *D   COPY    "CPORCHC02.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    請求書印刷パラメタ
           COPY    "CPORCHC03.INC".
      *
      *    診療費明細書印刷パラメタ
           COPY    "CPORCHC04.INC".
      *
      *    薬情報印刷パラメタ
           COPY    "CPORCSCH30.INC".
      *
      *    お薬手帳発行サブパラメタ
           COPY    "CPORCSCH62.INC".
      *
      *   診療科算定履歴更新サブ
      *    COPY    "CPORCSKARRK.INC".
      *
      *   算定履歴更新サブ
      *    COPY    "CPORCSSANTEI.INC".
      *
      *    ドクター編集処理パラメタ
           COPY    "CPORCSDRSET.INC".
      *
      *    診療会計剤チェックパラメタ
      *    COPY    "CPORCSCZAICHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    診療行為外来更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    まとめ入金編集パラメタ領域
           COPY    "CPORCSNYUKIN.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    予約票パラメタ
           COPY    "CPORCHC67.INC".
      *    オンライン帳票共通パラメタ
           COPY    "CPONPRTDATA.INC".
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *===> For Claim Start
           COPY    "CPSHELLTBL.INC".
      *****COPY    "ORCA-DBPATH".
      *===> For Claim End
      *
      *H29.7
      *PUSH通信
           COPY    "CPPUSHMEDICAL01.INC".
      *
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K03
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *H29.6
      *HAORI
               WHEN     "HAOR"           ALSO   SPACE
                   PERFORM 300-HAORI-SYORI-SEC
      *
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
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K03         TO  SPA-FREE
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
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               IF     (SPA-ERRMSG2         =   "K001")  AND
                      (SPA-KEI-FLG         =   1     )
                   MOVE    SPACE               TO  SPA-ERRMSG2
                   MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
                   PERFORM 450011-KOUSIN-ATO-SEC
                   IF      FLG-END             =   ZERO
                       PERFORM 4900-END-SYORI-SEC
                   END-IF
               ELSE
                   MOVE    SPACE               TO  SPA-ERRMSG2
                   MOVE    SPACE               TO  SPA-MOTOPG
                   PERFORM 5001-MAPCUR-SEC
               END-IF
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         NOT =   1
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END         NOT =   1
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "K03"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
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
      *        ＣＬＡＩＭ画面より
               WHEN    "K03X"
                   IF      SPA-K03X-FLG        =   SPACE
                       MOVE    1                   TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 3009-CLAIM-SYORI-SEC
                   END-IF
      *            初期画面処理
               WHEN    OTHER
                   PERFORM 3001-SCREEN-SYORI-SEC
           END-EVALUATE
           .
      *
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       3001-SCREEN-SYORI-SEC             SECTION.
      *
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-ERRMSG2
      *
           INITIALIZE                  SPA-K03-AREA
           INITIALIZE                  SYUNOU-REC
           MOVE    ZERO                TO  SPA-IDX-KOUI
           INITIALIZE                  SPA-SYUNOU-AREA
      *
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1                   TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
               IF      PARA-FILEMEI        =   "SYUNOU"
                   ADD     1                   TO  SPA-SYUNOU-MAX
                   MOVE    PARA-DATA-REC       TO  SPA-SYUNOU-REC
                                                       (SPA-SYUNOU-MAX)
               END-IF
      *        診療行為カウント
               IF      PARA-FILEMEI        =   "WKSRYACT"
                   ADD     1                   TO  SPA-IDX-KOUI
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       3001-SCREEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科テーブル編集処理
      *****************************************************************
       3003-SRYKA-HEN-SEC          SECTION.
      *
      *    診療科
           PERFORM VARYING     IDX2    FROM    1   BY  1
                    UNTIL     (IDX2    >   10  )  OR
                              (SPA-TBL-SRYKA (IDX IDX2)  =   SPACE )
               MOVE    SPA-TBL-SRYKA (IDX IDX2)    TO  WRK-SRYKA
               PERFORM VARYING     IDY     FROM    2   BY  1
                       UNTIL       IDY     >   40
                  IF      SPA-GMN-TSRYKA (IDY) =   SPACE
                       MOVE    WRK-SRYKA       TO  SPA-GMN-TSRYKA (IDY)
                  END-IF
                  IF      SPA-GMN-TSRYKA (IDY) =   WRK-SRYKA
                       MOVE    40              TO  IDY
                  END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       3003-SRYKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科名称テーブル編集処理
      *****************************************************************
       3004-SRYKAMEI-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-SRYKA-MAX
           IF      SPA-GMN-TSRYKA (02) =   SPACE
               MOVE    SPA-SRYKA           TO  SPA-GMN-TSRYKA (02)
           END-IF
           PERFORM VARYING     IDX2    FROM    2   BY  1
      ***           UNTIL     (IDX2    >   10  )  OR
                    UNTIL     (IDX2    >   40  )  OR
                              (SPA-GMN-TSRYKA (IDX2)   =   SPACE)
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-SRYKA-MAX
                  IF      SPA-GMN-TSRYKA (IDX2)    =   SPA-GMN-SRYKAL
                                                              (IDY)
                       MOVE    SPA-GMN-SRYKAMEIL(IDY)
                                           TO  SPA-GMN-TSRYKAMEI (IDX2)
                       MOVE    SPA-SRYKA-MAX   TO  IDY
                   END-IF
               END-PERFORM
               ADD     1               TO  SPA-GMN-SRYKA-MAX
           END-PERFORM
           IF      SPA-GMN-SRYKA-MAX   =   1
               MOVE    SPA-GMN-SRYKA-LIST(2)   TO  SPA-GMN-SRYKA-LIST(1)
               MOVE    SPACE                   TO  SPA-GMN-SRYKA-LIST(2)
               MOVE    SPA-GMN-SRYKA-LIST(1)   TO  SPA-GMN-SRYKA-G
           ELSE
               MOVE    "00 全科"           TO  SPA-GMN-SRYKA-LIST(1)
               ADD     1                   TO  SPA-GMN-SRYKA-MAX
               MOVE    SPA-GMN-SRYKA-LIST(1)   TO  SPA-GMN-SRYKA-G
           END-IF
           .
       3004-SRYKAMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        削除確認
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
      *                訂正時、受診履歴等の削除
                       MOVE    ZERO                TO  FLG-CLAIM-OK
                       PERFORM 4500-TOROKU-SYORI-SEC
      ******           IF      SPA-ERRCD (1:1)     =   "K"
                       IF      SPA-ERRCD       NOT =   SPACE
                           PERFORM 510-ERRSET-SEC
                       END-IF
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KID1-SET-EXT.
           EXIT. 
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                 SECTION.
      *
      *    請求書発行フラグ
           MOVE    "0 発行なし"        TO  SPA-GMN-HAKFLGLST (01)
           MOVE    "1 発行あり"        TO  SPA-GMN-HAKFLGLST (02)
           MOVE    "2 発行あり（請求あり）"
                                       TO  SPA-GMN-HAKFLGLST (03)
      *    訂正
           IF      SPA-SYORIFLG        =   1
               MOVE    "1 発行あり（訂正分）"
                                           TO  SPA-GMN-HAKFLGLST (02)
               MOVE    "2 発行あり（合計）"
                                           TO  SPA-GMN-HAKFLGLST (03)
           END-IF
      *
           MOVE    3                   TO  SPA-HAKFLG-MAX
      *H22.4 改正対応
      *    MOVE    SPA-SKYPRTFLG       TO  SPA-GMN-HAKFLG
      *@
      *    IF      SPA-SKYPRTFLG       =   "3"
      *        IF      SPA-SYORIFLG        =   ZERO
      *            MOVE    "1"             TO  SPA-GMN-HAKFLG
      *        ELSE
      *            MOVE    "0"             TO  SPA-GMN-HAKFLG
      *        END-IF
      **** END-IF
           IF      SPA-SKYPRTFLG       =   SPACE
      *        患者毎の設定なし
               MOVE    SPA-SKYPRTFLG-SYS   TO  WRK-SKYPRTFLG
           ELSE
      *        患者毎の設定あり
               MOVE    SPA-SKYPRTFLG       TO  WRK-SKYPRTFLG
      *H27.8
      *        請求書不要
               IF      SPA-SKYPRTFLG       =   "8"
                                           OR  "9"
                                           OR  "A"
                   MOVE    "0"                 TO  WRK-SKYPRTFLG
               END-IF
           END-IF
      *    患者毎の発行区分変更
           EVALUATE    WRK-SKYPRTFLG
               WHEN    "0"
      *            発行しない
                   MOVE    "0"                 TO  SPA-GMN-HAKFLG
               WHEN    "1"
               WHEN    "4"
      *            発行する
                   MOVE    "1"                 TO  SPA-GMN-HAKFLG
               WHEN    "2"
               WHEN    "5"
      *            発行する（請求あり）
                   MOVE    "2"                 TO  SPA-GMN-HAKFLG
               WHEN    "3"
               WHEN    "6"
      *            発行する（訂正時なし）
                   IF      SPA-SYORIFLG        =   ZERO
                       MOVE    "1"             TO  SPA-GMN-HAKFLG
                   ELSE
                       MOVE    "0"             TO  SPA-GMN-HAKFLG
                   END-IF
               WHEN    "7"
      *            発行する（請求あり）
                   MOVE    "2"                 TO  SPA-GMN-HAKFLG
           END-EVALUATE
      *
      *    診療費明細フラグ
           MOVE    "0 発行なし"        TO  SPA-GMN-MEIPRTFLGLST(01)
           MOVE    "1 発行あり"        TO  SPA-GMN-MEIPRTFLGLST(02)
           MOVE    2                   TO  SPA-MEIPRTFLG-MAX
           IF      SPA-MEIPRTFLG       =   SPACE
               MOVE    "0"                 TO  SPA-MEIPRTFLG
      *        設定なしは　発行ありとする（H22.4)
               IF      SPA-SRYYMD          >=  "20100401"
                   MOVE    "1"                 TO  SPA-MEIPRTFLG
               END-IF
           END-IF
      *
      *    院外処方せん発行フラグ
           MOVE    SPA-SYOHOPRTFLG     TO  SPA-GMN-SYOHOPRTFLG
           MOVE    "0 発行なし"        TO  SPA-GMN-SYOHOPRTLST (01)
           MOVE    "1 発行あり"        TO  SPA-GMN-SYOHOPRTLST (02)
           MOVE    "2 院内処方発行"    TO  SPA-GMN-SYOHOPRTLST (03)
      *@
           IF      SPA-SYOHOPRTFLG     =   "3"
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    "1"             TO  SPA-GMN-SYOHOPRTFLG
               ELSE
                   MOVE    "0"             TO  SPA-GMN-SYOHOPRTFLG
               END-IF
           END-IF
           IF      SPA-SYOHOPRTFLG     =   "4"
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    "2"             TO  SPA-GMN-SYOHOPRTFLG
               ELSE
                   MOVE    "0"             TO  SPA-GMN-SYOHOPRTFLG
               END-IF
           END-IF
      *@
           MOVE    3                   TO  SPA-SYOHOPRTFLG-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               IF      SPA-GMN-SYOHOPRTFLG     =   SPA-GMN-SYOHOPRTLST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-SYOHOPRTLST(IDX)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
               END-IF
           END-PERFORM
      *
      *    入金取扱い
           PERFORM 3105-NYUKINKBN-HEN-SEC
      *
      *    薬剤情報発行行フラグ
           MOVE    SPA-YAKJYOPRTFLG    TO  SPA-GMN-YAKJYOFLG
           MOVE    "0 発行なし"        TO  SPA-GMN-YAKJYOLST (01)
           MOVE    "1 発行あり"        TO  SPA-GMN-YAKJYOLST (02)
           MOVE    "2 院外分発行"      TO  SPA-GMN-YAKJYOLST (03)
      *@
           IF      SPA-YAKJYOPRTFLG    =   "2"
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    "1"             TO  SPA-GMN-YAKJYOFLG
               ELSE
                   MOVE    "0"             TO  SPA-GMN-YAKJYOFLG
               END-IF
           END-IF
      *    発行ありで薬剤情報提供料と連動する場合
           IF      SPA-GMN-YAKJYOFLG   =   "1"
               IF      (SYS-1038-YKJCHKKBN =   "1"  )  AND
                       (SPA-YAKJYO-ARI     =   ZERO
                                           OR  3    )
                   MOVE    "0"             TO  SPA-GMN-YAKJYOFLG
               END-IF
           END-IF
      *@
           MOVE    3                   TO  SPA-YAKJYOFLG-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKJYOFLG-MAX
               IF      SPA-GMN-YAKJYOFLG       =   SPA-GMN-YAKJYOLST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-YAKJYOLST(IDX)
                                           TO  SPA-GMN-YAKJYOFLG-G
               END-IF
           END-PERFORM
      *
      *    お薬情報発行行フラグ
           MOVE    "0 発行なし"        TO  SPA-GMN-OKUSURILST (01)
           MOVE    "1 発行あり"        TO  SPA-GMN-OKUSURILST (02)
           MOVE    "2 院外分発行"      TO  SPA-GMN-OKUSURILST (03)
           MOVE    3                   TO  SPA-OKUSURIFLG-MAX
           EVALUATE    SPA-OKUSURIPRTFLG
               WHEN    "0"
                   MOVE    SPA-OKUSURIPRTFLG   TO  SPA-GMN-OKUSURIFLG
               WHEN    "1"
      *            後期高齢者のみ
                   IF      SPA-NAI-ROUJIN      =   1
                       MOVE    "1"             TO  SPA-GMN-OKUSURIFLG
                   ELSE
                       MOVE    "0"             TO  SPA-GMN-OKUSURIFLG
                   END-IF
               WHEN    "2"
      *            後期高齢者のみ
                   IF     (SPA-NAI-ROUJIN      =   1     )
                     AND  (SPA-SYORIFLG        =   ZERO  )
                       MOVE    "1"             TO  SPA-GMN-OKUSURIFLG
                   ELSE
                       MOVE    "0"             TO  SPA-GMN-OKUSURIFLG
                   END-IF
               WHEN    "3"
                   MOVE    "1"             TO  SPA-GMN-OKUSURIFLG
               WHEN    "4"
                   IF      SPA-SYORIFLG        =   ZERO
                       MOVE    "1"             TO  SPA-GMN-OKUSURIFLG
                   ELSE
                       MOVE    "0"             TO  SPA-GMN-OKUSURIFLG
                   END-IF
           END-EVALUATE
      *    発行ありで薬剤情報提供料と連動する場合
           IF      SPA-GMN-OKUSURIFLG  =   "1"
               IF      (SYS-1038-OKUSURIKBN    =   "1"  )  AND
                       (SPA-YAKJYO-ARI         =   0
                                               OR  1    )
                   MOVE    "0"             TO  SPA-GMN-OKUSURIFLG
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OKUSURIFLG-MAX
               IF      SPA-GMN-OKUSURIFLG       =   SPA-GMN-OKUSURILST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-OKUSURILST(IDX)
                                           TO  SPA-GMN-OKUSURIFLG-G
               END-IF
           END-PERFORM
      *
      *    予約票発行行フラグ
           MOVE    "0 発行なし"        TO  SPA-GMN-YYKHYOFLG-LST (01)
           MOVE    "1 発行あり"        TO  SPA-GMN-YYKHYOFLG-LST (02)
           MOVE    2                   TO  SPA-YYKHYOFLG-MAX
      *    訂正は 発行なしとする
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPA-YYKHYOPRTFLG    TO  SPA-GMN-YYKHYOFLG
           ELSE
               MOVE    SPA-GMN-YYKHYOFLG-LST (01)(1:1)
                                           TO  SPA-GMN-YYKHYOFLG
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YYKHYOFLG-MAX
               IF      SPA-GMN-YYKHYOFLG       =   SPA-GMN-YYKHYOFLG-LST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-YYKHYOFLG-LST(IDX)
                                           TO  SPA-GMN-YYKHYOFLG-G
               END-IF
           END-PERFORM
      *
      *    入金方法フラグ
           PERFORM 3103-NYUKINHOU-HEN-SEC
      *
      *    自費名称編集
           PERFORM 3102-JIHIMSG-HEN-SEC
      *    複数科編集
           PERFORM 3104-FUKU-HEN-SEC
      *
      *    現物給付メッセージフラグ編集
           EVALUATE    SPA-FTNHEN-KBN
               WHEN    "1"
                   MOVE    WRK-COM-FTNHEN      TO  SPA-GMN-FUKUMSG
           END-EVALUATE
      *
      **** 入金取扱い
      ***  PERFORM 3105-NYUKINKBN-HEN-SEC
      *
      *    保険組合せ
           INITIALIZE                  SPA-GMN-SRYKA-LSTG
           INITIALIZE                  SPA-GMN-HKNCOMBI-LSTG
           IF      SPA-TBL-HKNCOMBI (2)    =   SPACE
              OR ((SPA-TBL-HKNCOMBI (2)    =   "9999" )  AND
                  (SPA-TBL-HKNCOMBI (3)    =   SPACE  ))
               MOVE    1                       TO  SPA-HKN-MAX
               MOVE    SPA-TBL-HKNCOMBI(1)     TO  SPA-GMN-THKNCOMBINUM
                                                       (SPA-HKN-MAX)
               MOVE    SPA-TBL-HKNCOMBIMEI(1)  TO  SPA-GMN-THKNCOMBIMEI
                                                       (SPA-HKN-MAX)
               MOVE    SPA-TBL-FTNRATEMEI (1)  TO  SPA-NAI-TFTNRATEMEI
                                                       (SPA-HKN-MAX)
               MOVE    1                       TO  IDX
               PERFORM 3003-SRYKA-HEN-SEC
           ELSE
               MOVE    "0000"              TO  SPA-GMN-THKNCOMBINUM(1)
               MOVE    "保険合計"          TO  SPA-GMN-THKNCOMBIMEI(1)
               MOVE    1                   TO  SPA-HKN-MAX
               PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )  OR
                              (SPA-TBL-HKNCOMBI (IDX)  =   SPACE )
               IF      SPA-TBL-HKNCOMBI (IDX)  NOT =   "9999"
                   ADD     1               TO  SPA-HKN-MAX
                   MOVE    SPA-TBL-HKNCOMBI(IDX)
                                           TO  SPA-GMN-THKNCOMBINUM
                                                       (SPA-HKN-MAX)
                   MOVE    SPA-TBL-HKNCOMBIMEI(IDX)
                                           TO  SPA-GMN-THKNCOMBIMEI
                                                       (SPA-HKN-MAX)
                   MOVE    SPA-TBL-FTNRATEMEI (IDX)
                                           TO  SPA-NAI-TFTNRATEMEI
                                                       (SPA-HKN-MAX)
                   PERFORM 3003-SRYKA-HEN-SEC
               END-IF
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-HKN-LIST(1)     TO  SPA-GMN-HKNCOMBI-G
           MOVE    SPA-NAI-TFTNRATEMEI(1)  TO  SPA-GMN-FTNRATEMEI
      *
      *    診療科編集
           PERFORM 3004-SRYKAMEI-HEN-SEC
      *
      *    未収金集計
           PERFORM  3110-MISYUKIN-SYUKEI-SEC
      *
      *    訂正の時、前の請求額編集
           IF      SPA-SYORIFLG        =   1
               PERFORM  3109-MAE-SYUNOU-SEC
           END-IF
      *    入金額設定
           PERFORM  3108-SYUNOU-SYOKI-SEC
      *    入院未収額設定
           IF      (SPA-K02N-NYUINYMD  NOT =   SPACE )  OR
                   (SPA-K02N-TAIINYMD  NOT =   SPACE )
               PERFORM  3109-NYUIN-MISYU-SEC
           END-IF
      *
      *    初期表示を合計とする
           MOVE    SPA-GMN-SRYKA       TO  SPA-GOK-SRYKA
           MOVE    SPA-GMN-HKNCOMBI    TO  SPA-GOK-HKNCOMBI
      *
      *    収納編集
           INITIALIZE                  SPA-GMN-AREA
      *    発行日
           MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HAKYMD
           MOVE    SPA-SYSYMD          TO  SPA-NAI-HAKYMD
           MOVE    SPA-SYSYMD          TO  SPA-MAE-HAKYMD
           PERFORM 3102-SYUNOU-HEN-SEC
      *
      *    COMPUTE WRK-SKYMONEY        =   SPA-GOK-SKYMONEY
      *                                +   SPA-GOK-CHOSEI
      *                                -   SPA-OLD-SKYMONEY
      *
      *!!!
      *    一括入金・返金処理
           MOVE    SPACE               TO  SPA-IKKATUKBN
           IF      SPA-NAI-NYKHNKKBN   =   "0"
               CONTINUE
           ELSE
               PERFORM 3106-IKKATU-INIT-SEC
           END-IF
      *!!!
      *
           COMPUTE WRK-SKYMONEY        =   SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI
                                       -   SPA-OLD-SKYMONEY
      *
      *    請求書発行フラグ
           IF     (SPA-GMN-HAKFLG      =   SPACE )  OR
                 ((WRK-SKYPRTFLG       =   "2"
                                       OR  "5"
                                       OR  "7"    )  AND
                  (WRK-SKYMONEY        <=  ZERO  ))
               MOVE    "0"             TO  SPA-GMN-HAKFLG
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKFLG-MAX
               IF      SPA-GMN-HAKFLG      =   SPA-GMN-HAKFLGLST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HAKFLGLST(IDX)  TO  SPA-GMN-HAKFLG-G
               END-IF
           END-PERFORM
      *H22.4　から
      *    診療費明細書発行区分設定
           PERFORM 3105-MEIPRTFLG-SET-SEC
      *
      *ver.4.7
      *    患者個別設定検索
           PERFORM 3108-PTCONF-SET-SEC
      *
      *    ユーザプログラム起動初期
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        INITIALIZE                  SPA-NAI-GYOUMU-FLG
      *        INITIALIZE                  SPA-NAI-GYOUMU-ARI
      *        GO      TO      310-SPA-SET-EXT
      *    END-IF
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "K03"               TO  ORCSUSERCHK-GMNPG
           MOVE    "1"                 TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG  TO  SPA-NAI-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI  TO  SPA-NAI-GYOUMU-ARI
      *
           MOVE    "0 Ｕ・Ｐ指示なし"      TO  SPA-GMN-USERPG-LIST (1)
           IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *        実行するプログラムがある時
               MOVE    "1 Ｕ・Ｐ指示あり"  TO  SPA-GMN-USERPG-LIST (2)
               MOVE    2                   TO  SPA-USERPG-MAX
               IF      SPA-NAI-GYOUMU-FLG  =   "1"
                   MOVE    SPA-GMN-USERPG-LIST (2) TO  SPA-GMN-USERPG-G
               ELSE
                   MOVE    SPA-GMN-USERPG-LIST (1) TO  SPA-GMN-USERPG-G
               END-IF
           ELSE
      *        実行するプログラムがない時
               MOVE    1                   TO  SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LIST(1)  TO  SPA-GMN-USERPG-G
           END-IF
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *H22.4　から
      *****************************************************************
      *    診療費明細書発行区分設定処理
      *****************************************************************
       3105-MEIPRTFLG-SET-SEC             SECTION.
      *
      *    患者毎の発行区分
           IF      SPA-SKYPRTFLG   NOT =   SPACE
      *        患者情報で設定があった時
               EVALUATE    SPA-SKYPRTFLG
               WHEN    "0"
      *            明細書不要
                   MOVE    "0"                 TO  SPA-MEIPRTFLG
               WHEN    "1"
               WHEN    "7"
               WHEN    "8"
      *            発行あり
                   MOVE    "1"                 TO  SPA-MEIPRTFLG
               WHEN    "2"
               WHEN    "9"
      *            請求あり
                   MOVE    "2"                 TO  SPA-MEIPRTFLG
               WHEN    "3"
               WHEN    "A"
      *            発行する（訂正時なし）
                   MOVE    "3"                 TO  SPA-MEIPRTFLG
               WHEN    "4"
               WHEN    "5"
               WHEN    "6"
      *            発行なし
                   MOVE    "0"                 TO  SPA-MEIPRTFLG
               END-EVALUATE
           END-IF
      *
           MOVE    SPA-MEIPRTFLG       TO  SPA-GMN-MEIPRTFLG
      *
           EVALUATE    SPA-MEIPRTFLG
      *    請求額あり
           WHEN    "2"
               IF      WRK-SKYMONEY        <=  ZERO
                   MOVE    "0"                 TO  SPA-GMN-MEIPRTFLG
               ELSE
                   MOVE    "1"                 TO  SPA-GMN-MEIPRTFLG
               END-IF
      *    訂正時発行なし
           WHEN    "3"
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    "1"                 TO  SPA-GMN-MEIPRTFLG
               ELSE
                   MOVE    "0"                 TO  SPA-GMN-MEIPRTFLG
               END-IF
           END-EVALUATE
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLGLST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-MEIPRTFLGLST(IDX)
                                           TO  SPA-GMN-MEIPRTFLG-G
               END-IF
           END-PERFORM
           .
       3105-MEIPRTFLG-SET-EXT.
           EXIT.
      *
      *ver.4.7
      *****************************************************************
      *    患者個別設定検索処理
      *****************************************************************
       3108-PTCONF-SET-SEC             SECTION.
      *
      *    お薬手帳データフォーマット区分設定
           MOVE    SPACE               TO  PTCONF-REC
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    "MNOTEVER"          TO  PTCONF-CKEY
           PERFORM 970-PTCONF-READ-SEC
           IF      FLG-PTCONF          =   ZERO
               MOVE    PTCONF-CDATA(1:1)   TO  SPA-NAI-CSVKBN
           END-IF
      *    日医版診療情報データフォーマット区分設定
           MOVE    SPACE               TO  PTCONF-REC
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    "JMAMRVER"          TO  PTCONF-CKEY
           PERFORM 970-PTCONF-READ-SEC
           IF      FLG-PTCONF          =   ZERO
               MOVE    PTCONF-CDATA(1:1)   TO  SPA-NAI-CSVKBN2
           END-IF
           .
       3108-PTCONF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入金・返金処理
      *****************************************************************
       3106-IKKATU-INIT-SEC             SECTION.
      *
      *    一括返金処理
           IF     (SPA-NAI-NYKHNKKBN   =   "2"
                                       OR  "3"  )
               PERFORM 3106-HENKIN-INIT-SEC
           END-IF
      *    一括入金処理
           IF     (SPA-NAI-NYKHNKKBN       =   "1"
                                           OR  "3"  )
           AND    (SPA-NAI-NYUKIN-JYOTAI
                                       NOT =   "2"  )
           AND    (SPA-GMN-NYUKINKBN   NOT =   "1"  )
           AND    (SPA-SYORIFLG            =   ZERO )
               PERFORM 3107-NYUKIN-INIT-SEC
           END-IF
           MOVE    SPA-GMN-NYUKINKBN   TO  SPA-MAE-NYUKINKBN
           MOVE    SPA-GMN-NYUKIN      TO  SPA-MAE-NYUKIN
           MOVE    SPA-GMN-HENKIN      TO  SPA-MAE-HENKIN
           .
       3106-IKKATU-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括返金初期編集処理
      *****************************************************************
       3106-HENKIN-INIT-SEC             SECTION.
      *
           IF      (SPA-SYORIFLG       =   1 )
      ***      PERFORM 4091-TEI-HENKIN-SYORI-SEC
               CONTINUE
           ELSE
               IF     (SPA-GMN-NYUKINKBN   =   "1" )
                  OR  (SPA-NAI-HENFLG      =   ZERO)
                   CONTINUE
               ELSE
      *            超過額を返金へ
                   MOVE    SPA-GMN-CHOKAKIN    TO  SPA-GMN-HENKIN-X
      *            返金チェック
                   PERFORM 450-HENKIN-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
      *                入金額計算計算処理
                       PERFORM 31051-NYUKIN-KEISAN-SEC
                   END-IF
               END-IF
           END-IF
           .
       3106-HENKIN-INIT-EXT.
           EXIT.
      *****************************************************************
      *    一括入金初期編集処理
      *****************************************************************
       3107-NYUKIN-INIT-SEC             SECTION.
      *
      *    入金上限額を入金へ
           IF      SPA-NAI-JYOGENKIN   >=  ZERO
               MOVE    SPA-NAI-JYOGENKIN   TO  SPA-GMN-NYUKIN-X
      *        基本チェックと画面処理
               PERFORM 450-NYUKIN-CHK-SEC
      *        入金額計算計算処理
               PERFORM 31051-NYUKIN-KEISAN-SEC
           ELSE
               IF     (SPA-GMN-NYUKIN      >   ZERO )
                   MOVE    ZERO            TO  SPA-GMN-NYUKIN
                   MOVE    ZERO            TO  SPA-GMN-NYUKIN-X
      *            基本チェックと画面処理
                   PERFORM 450-NYUKIN-CHK-SEC
      *            入金額計算計算処理
                   PERFORM 31051-NYUKIN-KEISAN-SEC
               END-IF
           END-IF
           .
       3107-NYUKIN-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の保険の収納編集処理
      *****************************************************************
       3102-SYUNOU-HEN-SEC             SECTION.
      *
      *    発行日保存
           MOVE    SPA-GMN-HAKYMD      TO  WRK-GMN-HAKYMD
           MOVE    SPA-NAI-HAKYMD      TO  WRK-NAI-HAKYMD
           MOVE    SPA-MAE-HAKYMD      TO  WRK-MAE-HAKYMD
      *
           INITIALIZE                  SPA-GMN-AREA
           MOVE    ZERO                TO  SPA-NAI-NYUKINFLG
           MOVE    ZERO                TO  SPA-NAI-HENFLG
      *    発行日を戻す
           MOVE    WRK-GMN-HAKYMD      TO  SPA-GMN-HAKYMD
           MOVE    WRK-NAI-HAKYMD      TO  SPA-NAI-HAKYMD
           MOVE    WRK-MAE-HAKYMD      TO  SPA-MAE-HAKYMD
      *
      *    科合計の入金額を編集する
           PERFORM 310515-KAGOK-NYUKIN-SEC
      *    合計入金額算処理
           PERFORM 3103-NYUKIN-KEI-SEC
      *
      *    対象の保険の収納
           PERFORM 3100-SYUNOU-HEN-SEC
           IF      (SPA-SYUNOU-IDX     =   ZERO )  AND
                   (FLG-FUKU           =   ZERO )
      *        対象が無い時
               MOVE    1                   TO  SPA-NAI-FLG
               MOVE    30                  TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  SPA-NAI-FLG
               PERFORM 31021-SYUNOU-GMN-HEN-SEC
               IF     (SPA-NAI-NYUKINFLG   =   1  )  OR
                     ((SPA-GMN-SRYKA       NOT =   ZERO ) AND
                      (SPA-GMN-HKNCOMBI    NOT =   ZERO ))
                   MOVE    1               TO  SPA-GMN-CUR
               ELSE
                   MOVE    2               TO  SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-HENFLG      =   1
                   IF     (SPA-GMN-NYUKINKBN   NOT =   "1") OR
                          (SPA-SYORIFLG        =   1      )
                       MOVE    7               TO  SPA-GMN-CUR
                   END-IF
               END-IF
      *        明細がない時、登録へ
               IF      SPA-IDX-KOUI        =   ZERO
                   MOVE    90                  TO  SPA-GMN-CUR
              END-IF
          END-IF
      *
           .
       3102-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納画面編集処理
      *****************************************************************
       31021-SYUNOU-GMN-HEN-SEC             SECTION.
      *
      *    発行日
      *    MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HAKYMD
      *    MOVE    SPA-SYSYMD          TO  SPA-NAI-HAKYMD
      *    MOVE    SPA-SYSYMD          TO  SPA-MAE-HAKYMD
      *    伝票番号
      *    MOVE    SYU-DENPNUM          TO  SPA-GMN-DENPNUM
      *H27 IF      SPA-SYORIFLG        =   1
      *        IF      SYU-DENPNUM     NOT =   ZERO
      *            MOVE    SYU-DENPNUM         TO  SPA-GMN-DENPNUM
      *        END-IF
      *    END-IF
      *
      *    保険料
           MOVE    ZERO                TO  SPA-GMN-HKNRYO (17)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   16
               MOVE    SYU-HKNTEN (IDX)    TO  SPA-GMN-HKNRYO (IDX)
               ADD     SYU-HKNTEN (IDX)    TO  SPA-GMN-HKNRYO (17)
      *        保険適用外
               COMPUTE SPA-GMN-TGMONEY (IDX)   =   SYU-TGMONEY (IDX)
                                               +   SYU-TGMONEY-TAX(IDX)
      *    保険適用外
      *        ADD     SYU-TGMONEY (IDX)       TO  SPA-GMN-HKNGAI
      *        ADD     SYU-TGMONEY-TAX (IDX)   TO  SPA-GMN-HKNGAI
           END-PERFORM
      *    保険適用外
           COMPUTE SPA-GMN-HKNGAI      =   SYU-TGMONEY (17)
                                       +   SYU-TGMONEY-TAX (17)
      *保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  SPA-GMN-HKNTEKI
      *    老人一部負担金
           MOVE    SYU-RJN-FTN         TO  SPA-GMN-ROUFTN
      *    公費一部負担
           MOVE    SYU-KOH-FTN         TO  SPA-GMN-KOHFTN
      *
      *    自費１
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    SYU-JIHI-TAXNASI(IDX)   TO  SPA-GMN-JIHI(IDX)
           END-PERFORM
      *    自費計
           MOVE    SYU-JIHI-TOTAL      TO  SPA-GMN-JIHIKEI
      *    自費（消費税あり）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    SYU-JIHI-TAXARI(IDX)    TO  SPA-GMN-JIHITAX(IDX)
           END-PERFORM
      *    自費計（消費税あり）
           MOVE    SYU-JIHI-TOTAL-TAX  TO  SPA-GMN-JIHITAXKEI
      *    自費消費税
           MOVE    SYU-JIHI-TAX        TO  SPA-GMN-JIHIZEI
      *    自費合計
           COMPUTE SPA-GMN-JIHIGOK     =   SPA-GMN-JIHIKEI
                                       +   SPA-GMN-JIHIZEI
      *    薬剤一部負担
           MOVE    SYU-YKZ-FTN         TO  SPA-GMN-YKZFTN
      *
      *    一部負担金
           COMPUTE SPA-GMN-ICHIFTN     =   SPA-GMN-ROUFTN
                                       +   SPA-GMN-KOHFTN
                                       +   SPA-GMN-YKZFTN
      *    調整金合計
           MOVE    SYU-CHOSEI          TO  SPA-GMN-CHOSEI
           MOVE    SYU-CHOSEI          TO  SPA-MAE-CHOSEI
      *    調整金１
           MOVE    SYU-CHOSEI1         TO  SPA-GMN-CHOSEI1
           MOVE    SPA-GMN-CHOSEI1     TO  SPA-MAE-CHOSEI1
           MOVE    SPA-GMN-CHOSEI1     TO  WRK-Z92
           MOVE    WRK-Z92             TO  SPA-GMN-CHOSEI1-X
      *    調整金２
           MOVE    SYU-CHOSEI2         TO  SPA-GMN-CHOSEI2
           MOVE    SPA-GMN-CHOSEI2     TO  SPA-MAE-CHOSEI2
           MOVE    SPA-GMN-CHOSEI2     TO  WRK-Z92
           MOVE    WRK-Z92             TO  SPA-GMN-CHOSEI2-X
      **** 前回未収
      *    MOVE    SPA-MISYUMONEY      TO  SPA-GMN-ZENMISYU
      *    前回未収額がマイナスの時、超過額とする
      *    IF      SPA-MISYUMONEY      <   ZERO
      *        MOVE    SPA-MISYUMONEY      TO  SPA-GMN-CHOKAKIN
      *        MOVE    ZERO                TO  SPA-GMN-ZENMISYU
      *****END-IF
      *
      *    労災初診
           MOVE    SYU-RSISHOSHIN-MONEY    TO  SPA-GMN-ROUSYOSIN
      *    労災再診
           MOVE    SYU-RSISAISHIN-MONEY    TO  SPA-GMN-ROUSAISIN
      *    労災指導
           MOVE    SYU-RSISDO-MONEY        TO  SPA-GMN-ROUSIDOU
      *    労災その他
           MOVE    SYU-RSIETC-MONEY        TO  SPA-GMN-ROUETC
      *
           MOVE    SYU-SKYMONEY        TO  SPA-KON-SKYMONEY
           IF     (SPA-SYORIFLG        =   1   )  AND
                  (SYU-DENPNUM     NOT =   ZERO )
      ******** 今回請求額 − 前回請求額
      *        COMPUTE SPA-NAI-SEIKYU      =   SYU-SKYMONEY
      *                                    -   WRK-SKYMONEY
      *        COMPUTE SPA-NAI-SKYMONEY-TAX-SAI
      *                                    =   SYU-SKYMONEY-TAX-SAI
      *                                    -   WRK-SKYMONEY-TAX-SAI
      *        クリアの時、ゼロとする
      *        IF      SPA-IDX-KOUI        =   ZERO
      *            MOVE    ZERO            TO  SPA-NAI-SEIKYU
      *            MOVE    ZERO            TO  SPA-NAI-SKYMONEY-TAX-SAI
      ******** END-IF
               CONTINUE
           ELSE
               MOVE    SYU-SKYMONEY        TO  SPA-NAI-SEIKYU
               MOVE    SYU-SKYMONEY-TAX-SAI
                                           TO  SPA-NAI-SKYMONEY-TAX-SAI
           END-IF
      *    入金金額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-NYUKIN
           IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA  )  AND
                  (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
      *        合計画面に充当額を加算
               COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-NYUKIN
                                       +   SPA-NYU-ZENMISYU 
           END-IF
           MOVE    SPA-GMN-NYUKIN      TO  SPA-MAE-NYUKIN
      *
      *    合計請求額計算処理
           PERFORM 3101-SEIKYU-KEI-SEC
      *
      *    返金額
           IF      (SPA-NAI-HENFLG     =   1 )  AND
                   (SPA-SYORIFLG       =   ZERO)
               MOVE    SPA-GOK-HENKIN      TO  SPA-GMN-HENKIN
               MOVE    SPACE               TO  SPA-NAI-HENKBN
           ELSE
               IF      SPA-SYUNOU-IDX      >   ZERO
                   MOVE    SPA-SYU-HENKIN (SPA-SYUNOU-IDX)
                                           TO  SPA-GMN-HENKIN
                   MOVE    SPA-SYU-HENKBN (SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-HENKBN
               ELSE
                   MOVE    SPA-NAI-HENKIN  TO  SPA-GMN-HENKIN
                   MOVE    SPACE           TO  SPA-NAI-HENKBN
               END-IF
           END-IF
           MOVE    SPA-GMN-HENKIN      TO  SPA-MAE-HENKIN
      *
      *?   合計入金額算処理
      *?    PERFORM 3103-NYUKIN-KEI-SEC
      *    訂正時の請求額
           MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS2
           IF     (SPA-SYORIFLG        =   1   )
      *D     AND  (SYU-DENPNUM         >   ZERO)
      *D     AND  (SPA-SYUNOU-IDX      >   ZERO)
             AND  (SPA-IDX-KOUI        >   ZERO)
      *        合計は集計分から
               IF     (SPA-SYUNOU-IDX      >   ZERO)
                   COMPUTE WRK-SKYMONEY    =   SPA-MAE-SKYMONEY
                                                    (SPA-SYUNOU-IDX)
                                           -   SPA-TMAE-CHOSEI
                                                    (SPA-SYUNOU-IDX)
               ELSE
                   COMPUTE WRK-SKYMONEY    =   SPA-OLD-SEIKYU
                                           -   SPA-NAI-CHOSEI
               END-IF
               MOVE    WRK-SKYMONEY        TO  WRK-Z9-9
               PERFORM 31091-KINHENSYU-SEC
               MOVE    WRK-HEN             TO  WRK-HEN-1
               COMPUTE WRK-SKYMONEY2       =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
               MOVE    WRK-SKYMONEY2       TO  WRK-Z9-9
               PERFORM 31091-KINHENSYU-SEC
               MOVE    WRK-HEN             TO  WRK-HEN-2
               STRING  "訂正前請求額："    DELIMITED  BY  SIZE
                       WRK-HEN-1           DELIMITED  BY  SPACE
                       "　今回請求額："    DELIMITED  BY  SIZE
                       WRK-HEN-2           DELIMITED  BY  SPACE
                                       INTO    SPA-GMN-FUKUZENMDS2
               END-STRING
      *D       IF      WRK-SKYMONEY    =   WRK-SKYMONEY2
      *D           MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS2
      *D       ELSE
                   INSPECT SPA-GMN-FUKUZENMDS2   REPLACING
                                               ALL "  "  BY  "　"
      *D       END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS2
           END-IF
      *
      *R02.2
      *    削除前の合計請求額表示
           IF     (SPA-SYORIFLG        =   1   )
             AND  (SPA-IDX-KOUI        =   ZERO)
      *        請求金額
               MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS2
               COMPUTE WRK-SKYMONEY        =   SPA-MAE-SKYMONEY (15)
                                           *   -1
               MOVE    WRK-SKYMONEY             TO  WRK-Z9-9
               PERFORM 31091-KINHENSYU-SEC
               MOVE    WRK-HEN              TO  WRK-HEN-1
               STRING  "＜削除＞（合計請求額：" 
                                           DELIMITED  BY  SIZE
                       WRK-HEN-1           DELIMITED  BY  SPACE
                       "）"                DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-FUKUZENMDS2
               END-STRING
           END-IF
      *!
      *
      *    複数科まとめ差額
           IF     (SYU-GRP-SGKMONEY    NOT =   ZERO )
             AND  (SYU-SRYKA           NOT =   ZERO )
               MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS
               MOVE    SYU-GRP-SGKMONEY    TO  WRK-Z9
               STRING  "複数科まとめ差額："    DELIMITED  BY  SIZE
                       WRK-Z9                  DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-FUKUZENMDS
               END-STRING
           ELSE
               MOVE    SPACE               TO  SPA-GMN-FUKUZENMDS
           END-IF
      *!!
      *    減免区分
           MOVE    SPACE               TO  SPA-GMN-GENMENMEI
           IF     (SYU-DISCOUNT-TEIRITU    NOT =   SPACE )
               IF      SPA-GMN-HKNCOMBI        =   "0000"
                   MOVE    "減免有"            TO  SPA-GMN-GENMENMEI
               ELSE
                   EVALUATE    SYU-DISCOUNT-TEIRITU
                       WHEN    "1"
                           MOVE    SYU-DISCOUNT-RATE   TO  WRK-ZZ9
                           STRING    "減免有（"
                                     WRK-ZZ9
                                     "％）"     DELIMITED  BY  SIZE
                                                INTO   SPA-GMN-GENMENMEI
                           END-STRING
                       WHEN    "2"
                           MOVE    SYU-DISCOUNT-RATE   TO  WRK-Z97
                           STRING    "減免有（"
                                     WRK-Z97
                                     "円）"     DELIMITED  BY  SIZE
                                                INTO   SPA-GMN-GENMENMEI
                           END-STRING
                   END-EVALUATE
               END-IF
           END-IF
      *
           .
       31021-SYUNOU-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正の時、前の請求額編集処理
      *****************************************************************
       3109-MAE-SYUNOU-SEC             SECTION.	
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
      *
               IF     (SYU-DENPNUM     =   ZERO  )  OR
                      (SYU-SRYKA       =   ZERO  )
                   CONTINUE
               ELSE
                   PERFORM 31092-SYUNOU-MAE-SEC
                   MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
      *
      *            今回請求額 − 前回請求額
      *            請求金額訂正差額
                   COMPUTE SPA-TEI-SKYMONEY (IDX)
                                           =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
                                           -   SPA-MAE-SKYMONEY (IDX)
      *            消費税（再掲）訂正差額
                   COMPUTE SPA-TEI-SKYMONEY-TAX-SAI (IDX)
                                           =   SYU-SKYMONEY-TAX-SAI
                                           -   SPA-MAE-SKYMONEY-TAX-SAI
                                                                (IDX)
      *            今回請求額
                   IF      SYU-SKYMONEY    <=  SYU-CHOSEI
                       MOVE    ZERO        TO  SPA-TEIKEI-SKYMONEY (IDX)
                   ELSE
                       COMPUTE SPA-TEIKEI-SKYMONEY (IDX)
                                           =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
                   END-IF
      *            クリアの時、ゼロとする
                   IF      SPA-IDX-KOUI        =   ZERO
      *
      *R02.2
      *                前回請求額合計
                       PERFORM 310912-DELETE-SYUNOU-SEC
      *
                       MOVE    ZERO        TO  SPA-TEI-SKYMONEY (IDX)
                       MOVE    ZERO        TO  SPA-TEI-SKYMONEY-TAX-SAI
                                                            (IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI (IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI1(IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI2(IDX)
      *R02.1
      *                請求金額
                       MOVE    ZERO        TO  SPA-MAE-SKYMONEY(IDX)
      *                入金合計額
                       MOVE    ZERO        TO  SPA-MAE-NYUKIN-TOTAL
                                                               (IDX)
      *                消費税（再掲）
                       MOVE    ZERO        TO  SPA-MAE-SKYMONEY-TAX-SAI
                                                               (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
      *
               IF     (SYU-DENPNUM NOT =   ZERO  )  AND
                      (SYU-SRYKA       =   ZERO  )
                   PERFORM 31091-SYUTTL-MAE-SEC
                   MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
      *            今回請求額 − 前回請求額
      *            請求金額訂正差額
                   COMPUTE SPA-TEI-SKYMONEY (IDX)
                                           =  (SYU-SKYMONEY
                                           -   SYU-CHOSEI    )
                                           -   SPA-MAE-SKYMONEY (IDX)
      *            消費税（再掲）訂正差額
                   COMPUTE SPA-TEI-SKYMONEY-TAX-SAI (IDX)
                                           =   SYU-SKYMONEY-TAX-SAI
                                           -   SPA-MAE-SKYMONEY-TAX-SAI
                                                                (IDX)
      *            今回請求額
                   IF      SYU-SKYMONEY    <=   SYU-CHOSEI
                       MOVE    ZERO        TO  SPA-TEIKEI-SKYMONEY (IDX)
                   ELSE
                       COMPUTE SPA-TEIKEI-SKYMONEY (IDX)
                                           =   SYU-SKYMONEY
                                           -   SYU-CHOSEI
                   END-IF
      *            クリアの時、ゼロとする
                   IF      SPA-IDX-KOUI        =   ZERO
                       MOVE    ZERO        TO  SPA-TEI-SKYMONEY (IDX)
                       MOVE    ZERO        TO  SPA-TEI-SKYMONEY-TAX-SAI
                                                            (IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI (IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI1(IDX)
                       MOVE    ZERO        TO  SPA-TMAE-CHOSEI2(IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       3109-MAE-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正の時、前の請求額編集処理
      *****************************************************************
       31092-SYUNOU-MAE-SEC             SECTION.
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    請求金額
           MOVE    SYU-SKYMONEY        TO  SPA-MAE-SKYMONEY (IDX)
      *    入金合計額
           MOVE    SYU-NYUKIN-TOTAL    TO  SPA-MAE-NYUKIN-TOTAL (IDX)
      *    消費税（再掲）
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  SPA-MAE-SKYMONEY-TAX-SAI
                                                            (IDX)
      *    調整額
           MOVE    SYU-CHOSEI          TO  SPA-TMAE-CHOSEI (IDX)
           MOVE    SYU-CHOSEI1         TO  SPA-TMAE-CHOSEI1(IDX)
           MOVE    SYU-CHOSEI2         TO  SPA-TMAE-CHOSEI2(IDX)
      *
           .
       31092-SYUNOU-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正の時、前の請求額編集処理
      *****************************************************************
       31091-SYUTTL-MAE-SEC             SECTION.
      *
      *    複数科まとめ集計
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM         TO  SYUTTL-HOSPNUM 
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN 
           MOVE    SYU-PTID            TO  SYUTTL-PTID 
           MOVE    SYU-DENPNUM         TO  SYUTTL-DENPNUM 
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYUTTL          =   ZERO
      *        請求金額
               MOVE    SYUTTL-SKYMONEY     TO  SPA-MAE-SKYMONEY (IDX)
      *        入金合計額
               MOVE    SYUTTL-NYUKIN-TOTAL TO  SPA-MAE-NYUKIN-TOTAL(IDX)
      *        消費税（再掲）
               MOVE    SYUTTL-SKYMONEY-TAX-SAI
                                           TO  SPA-MAE-SKYMONEY-TAX-SAI
                                                             (IDX)
      *        調整金
               MOVE    SYUTTL-CHOSEI       TO  SPA-TMAE-CHOSEI (IDX)
               MOVE    SYUTTL-CHOSEI1      TO  SPA-TMAE-CHOSEI1(IDX)
               MOVE    SYUTTL-CHOSEI2      TO  SPA-TMAE-CHOSEI2(IDX)
           ELSE
      *        同じ保険の前請求額を加算する
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2    >   SPA-SYUNOU-MAX
                   MOVE    SPA-SYUNOU-REC (IDX2)   TO  WRK-SYUNOU-REC
                   IF     (SYU-HKNCOMBINUM     =   WRK-SYU-HKNCOMBINUM)
                     AND  (WRK-SYU-SRYKA   NOT =   ZERO  )
                       ADD     SPA-MAE-SKYMONEY (IDX2)
                                           TO  SPA-MAE-SKYMONEY(IDX)
                       ADD     SPA-MAE-NYUKIN-TOTAL (IDX2)
                                           TO  SPA-MAE-NYUKIN-TOTAL(IDX)
                       ADD     SPA-MAE-SKYMONEY-TAX-SAI (IDX2)
                                           TO  SPA-MAE-SKYMONEY-TAX-SAI
                                                                  (IDX)
                       ADD     SPA-TMAE-CHOSEI  (IDX2)
                                           TO  SPA-TMAE-CHOSEI    (IDX)
                       ADD     SPA-TMAE-CHOSEI1 (IDX2)
                                           TO  SPA-TMAE-CHOSEI1   (IDX)
                       ADD     SPA-TMAE-CHOSEI2 (IDX2)
                                           TO  SPA-TMAE-CHOSEI2   (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       31091-SYUTTL-MAE-EXT.
           EXIT.
      *
      *R02.2
      *****************************************************************
      *    全削除の削除合計請求額編集処理
      *****************************************************************
       310912-DELETE-SYUNOU-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-MAE-SKYMONEY
           MOVE    ZERO                TO  WRK-MAE-CHOSEI
           IF      SPA-TEI-DENPNUM(1)  =   ZERO
               MOVE    SPA-MAE-SKYMONEY (IDX)
                                       TO  WRK-MAE-SKYMONEY
               MOVE    SPA-TMAE-CHOSEI(IDX)
                                       TO  WRK-MAE-CHOSEI
           END-IF
      *
           PERFORM VARYING     IDXD    FROM    1   BY  1
                   UNTIL      (IDXD    >   15  )  OR
                              (SPA-TEI-DENPNUM(IDXD)   =   ZERO)
               MOVE    SPACE           TO  SYUNOU-REC
               MOVE    SPA-HOSPNUM     TO  SYU-HOSPNUM
               MOVE    SPA-NYUGAIKBN   TO  SYU-NYUGAIKBN
               MOVE    SPA-PTID        TO  SYU-PTID
               MOVE    SPA-TEI-DENPNUM(IDXD)
                                       TO  SYU-DENPNUM
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
                   INITIALIZE              SYUNOU-REC
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        請求金額
               ADD     SYU-SKYMONEY        TO  WRK-MAE-SKYMONEY
               ADD     SYU-CHOSEI          TO  WRK-MAE-CHOSEI
           END-PERFORM
      *
      *    収納の最後に編集
           MOVE    WRK-MAE-SKYMONEY    TO  SPA-MAE-SKYMONEY (15)
           MOVE    WRK-MAE-CHOSEI      TO  SPA-TMAE-CHOSEI (15)
           .
       310912-DELETE-SYUNOU-EXT.
           EXIT.
      *R02.2
      *
      *****************************************************************
      *    入金額設定編集処理
      *****************************************************************
       3108-SYUNOU-SYOKI-SEC             SECTION.
      *
      *    IF      SPA-SYORIFLG        =   1
      *        OR  SPA-GMN-CHOKAKIN    NOT =   ZERO
      *        合計入金額算処理
      *        PERFORM 3103-NYUKIN-KEI-SEC
      ***  END-IF
      *!!!!
      *H30.6
           INITIALIZE  WRK-SYUNOU-REC
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF      SYU-DENPNUM         =   ZERO
      *            今回請求額
                   COMPUTE WRK-SEIKYU          =   SYU-SKYMONEY
                                               +   SYU-CHOSEI
               ELSE
      *            訂正入金額
                   COMPUTE WRK-SEIKYU      =  (SPA-TEIKEI-SKYMONEY(IDX)
                                           +   SYU-CHOSEI   )
                                           -   SPA-MAE-SKYMONEY (IDX)
               END-IF
      *        今回入金
               IF      WRK-SEIKYU          <   ZERO
                   MOVE    ZERO            TO  SYU-NYUKIN-TOTAL
               ELSE
                   MOVE    WRK-SEIKYU      TO  SYU-NYUKIN-TOTAL
               END-IF
      *        入金状態が未入金の時、ゼロ
               IF     (SPA-NAI-NYUKIN-JYOTAI   =   "2" )
                   MOVE    ZERO                TO  SYU-NYUKIN-TOTAL
               END-IF
      *
               MOVE    SYUNOU-REC          TO  SPA-SYUNOU-REC (IDX)
      *        今回入金額
               MOVE    SYU-NYUKIN-TOTAL    TO  SPA-TEI-NYUKIN-TOTAL
                                                              (IDX)
      *
      *H30.6
      *        桁オーバーチェック
               IF     (SYU-SRYKA           =   ZERO )
                   OR (SYU-HKNCOMBINUM     =   ZERO
                                           OR  SPACE )
                   CONTINUE
               ELSE
      *            自費合計
                   IF     (WRK-SYU-JIHI-TOTAL  +   SYU-JIHI-TOTAL)
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
                   IF     (WRK-SYU-JIHI-TOTAL-TAX
                                           +   SYU-JIHI-TOTAL-TAX)
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
      *            保険外合計
                   IF     (WRK-SYU-TGMONEY (17)    +   SYU-TGMONEY(17))
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
                   IF     (WRK-SYU-TGMONEY-TAX(17)
                                           +   SYU-TGMONEY-TAX(17))
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
      *            調整金合計
                   IF     (WRK-SYU-CHOSEI      +   SYU-CHOSEI)
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
      *            請求金額
                   IF     (WRK-SYU-SKYMONEY    +   SYU-SKYMONEY)
                                               >   9999999
                       MOVE    1                   TO  SPA-OVER-ERR
                   END-IF
      *
                   ADD     SYU-JIHI-TOTAL      TO  WRK-SYU-JIHI-TOTAL
                   ADD     SYU-JIHI-TOTAL-TAX  TO
                                               WRK-SYU-JIHI-TOTAL-TAX
                   ADD     SYU-CHOSEI          TO  WRK-SYU-CHOSEI
                   ADD     SYU-SKYMONEY        TO  WRK-SYU-SKYMONEY
               END-IF
      *H30.6
           END-PERFORM
      *
      *H30.6
           INITIALIZE  WRK-SYUNOU-REC
      *
           .
       3108-SYUNOU-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    対象の保険の収納編集処理
      *****************************************************************
       3100-SYUNOU-HEN-SEC             SECTION.
      *
           INITIALIZE                  SYUNOU-REC
           MOVE    ZERO                TO  SPA-SYUNOU-IDX
           MOVE    ZERO                TO  FLG-FUKU
           INITIALIZE                  WRK-TEISEI-AREA
      *    MOVE    ZERO                TO  SPA-GOK-NYUKIN
           MOVE    ZERO                TO  SPA-OLD-SEIKYU
           MOVE    ZERO                TO  SPA-NAI-SEIKYU
           MOVE    ZERO                TO  SPA-NAI-NYUKIN
           MOVE    ZERO                TO  SPA-NAI-CHOSEI
           MOVE    ZERO                TO  SPA-NAI-CHOSEI1
           MOVE    ZERO                TO  SPA-NAI-CHOSEI2
           MOVE    ZERO                TO  SPA-NAI-SKYMONEY-TAX-SAI
           MOVE    ZERO                TO  SPA-NAI-HENKIN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
      *        マル長の公費負担を保険適用金額へ
               IF      WRK-SYU-HKNTEKKBN   =   "1"
                   MOVE    WRK-SYU-KOH-FTN     TO  WRK-SYU-HKNTEKMONEY
                   MOVE    ZERO                TO  WRK-SYU-KOH-FTN
               END-IF
      *
               IF      SPA-GMN-HKNCOMBI    =   "0000"
                   IF       SPA-GMN-SRYKA      =   WRK-SYU-SRYKA
      *                集計
                       IF      SPA-SYUNOU-IDX      =   ZERO
                           MOVE    WRK-SYUNOU-REC  TO  SYUNOU-REC
                           MOVE    IDX             TO  SPA-SYUNOU-IDX
                       ELSE
                           PERFORM 31000-SYUNOU-ADD-SEC
                           MOVE    1               TO  FLG-FUKU
                       END-IF
                   END-IF
               ELSE
                   IF     (SPA-GMN-HKNCOMBI    =   WRK-SYU-HKNCOMBINUM)
                     AND  (SPA-GMN-SRYKA       =   WRK-SYU-SRYKA )
                       MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
                       MOVE    IDX                 TO  SPA-SYUNOU-IDX
                       MOVE    SPA-SYUNOU-MAX      TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF     (SPA-SYUNOU-IDX      >   ZERO ) AND
                  (SPA-DENPNUM         >   ZERO )
      *        訂正前
               ADD     SPA-TEIKEI-SKYMONEY(SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-SEIKYU
               ADD     SPA-TEI-SKYMONEY-TAX-SAI(SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-SKYMONEY-TAX-SAI
               ADD     SPA-TMAE-CHOSEI (SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-CHOSEI
               ADD     SPA-TMAE-CHOSEI1(SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-CHOSEI1
               ADD     SPA-TMAE-CHOSEI2(SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-CHOSEI2
               ADD     SPA-MAE-SKYMONEY(SPA-SYUNOU-IDX)
                                           TO  SPA-OLD-SEIKYU
      *        入金差額
               ADD     SPA-MAE-NYUKIN-TOTAL(SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-NYUKIN
      *        返金額
               ADD     SPA-SYU-HENKIN  (SPA-SYUNOU-IDX)
                                           TO  SPA-NAI-HENKIN
           END-IF
      *    全科の時、単独科の保険を集計
           IF      SPA-GMN-SRYKA       =   ZERO
               PERFORM VARYING     IDX4    FROM    1   BY  1
                       UNTIL      (IDX4    >   15  )  OR
                                  (SPA-TBL-HKNCOMBI (IDX4)  =   SPACE )
                   IF     (SPA-GMN-HKNCOMBI    =   "0000" ) OR
                          (SPA-GMN-HKNCOMBI    =   SPA-TBL-HKNCOMBI
                                                            (IDX4))
                       IF      SPA-TBL-SRYKA (IDX4 2)   =   SPACE
                           MOVE    SPA-TBL-HKNCOMBI(IDX4)
                                                   TO  WRK-HKNCOMBI
                           MOVE    SPA-TBL-SRYKA (IDX4 1)
                                                   TO  WRK-SRYKA
                           PERFORM 31001-ZENNKA-HEN-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    合計の時は、添え字ゼロにする
           IF      FLG-FUKU            =   1
               MOVE    ZERO                TO  SPA-SYUNOU-IDX
           END-IF
      *
           IF     (SPA-SYUNOU-IDX      =   ZERO )  OR
                  (SPA-GMN-SRYKA       =   ZERO )
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
           ELSE
               IF      SYU-DRCD            =   SPACE
                   MOVE    SPA-DRCD(2:4)       TO  SPA-GMN-DRCD
               ELSE
                   MOVE    SYU-DRCD            TO  SPA-GMN-DRCD
               END-IF
      *        ドクター編集
               PERFORM 3103-DRCD-HEN-SEC
           END-IF
      *    合計の時、入金額入力可
           IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                  (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
           AND    (SPA-SYORIFLG        =   ZERO  )
               MOVE    1               TO  SPA-NAI-NYUKINFLG
           END-IF
      *H27.8
           IF      SPA-SYORIFLG        =   1
               IF     (SPA-GMN-SRYKA       =   ZERO )
                AND   (SPA-GMN-HKNCOMBI    =   "0000")
                   MOVE    SPA-DENPNUM         TO  SPA-GMN-DENPNUM
               ELSE
                   IF      SYU-DENPNUM     NOT =   ZERO
                       MOVE    SYU-DENPNUM         TO  SPA-GMN-DENPNUM
                   END-IF
               END-IF
           END-IF
      *
      *
            .
       3100-SYUNOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    全科収納編集処理
      *****************************************************************
       31001-ZENNKA-HEN-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX)     TO  WRK-SYUNOU-REC
               IF     (WRK-HKNCOMBI        =   WRK-SYU-HKNCOMBINUM) AND
                      (WRK-SRYKA           =   WRK-SYU-SRYKA      )
                   IF      SYU-PTID        =   ZERO
                       MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
                       IF     (WRK-SYU-DENPNUM     >   ZERO )
                           ADD     SPA-TEIKEI-SKYMONEY(IDX)
                                           TO  SPA-NAI-SEIKYU
                           ADD     SPA-TEI-SKYMONEY-TAX-SAI(IDX)
                                           TO  SPA-NAI-SKYMONEY-TAX-SAI
                           ADD     SPA-TMAE-CHOSEI  (IDX)
                                           TO  SPA-NAI-CHOSEI
                           ADD     SPA-TMAE-CHOSEI1 (IDX)
                                           TO  SPA-NAI-CHOSEI1
                           ADD     SPA-TMAE-CHOSEI2 (IDX)
                                           TO  SPA-NAI-CHOSEI2
                           ADD     SPA-MAE-SKYMONEY(IDX)
                                           TO  SPA-OLD-SEIKYU
                           ADD     SPA-MAE-NYUKIN-TOTAL(IDX)
                                           TO  SPA-NAI-NYUKIN
      *                    返金額
                           ADD     SPA-SYU-HENKIN  (IDX)
                                           TO  SPA-NAI-HENKIN
                       END-IF
                   ELSE
                       PERFORM 31000-SYUNOU-ADD-SEC
                   END-IF
                   MOVE    1               TO  FLG-FUKU
               END-IF
           END-PERFORM
            .
       31001-ZENNKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納集計処理
      *****************************************************************
       31000-SYUNOU-ADD-SEC             SECTION.
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
      *    保険適用金額
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           ADD     WRK-SYU-SHOHOU-SAI  TO  SYU-SHOHOU-SAI
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   17
               ADD    WRK-SYU-HKNTEN (IDS1)    TO  SYU-HKNTEN(IDS1)
               ADD    WRK-SYU-MONEY (IDS1)     TO  SYU-MONEY (IDS1)
               ADD    WRK-SYU-TGMONEY (IDS1)   TO  SYU-TGMONEY(IDS1)
               ADD    WRK-SYU-TGMONEY-TAX (IDS1)
                                               TO  SYU-TGMONEY-TAX(IDS1)
           END-PERFORM
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   10
               ADD    WRK-SYU-JIHI-TAXNASI (IDS1)
                                           TO  SYU-JIHI-TAXNASI(IDS1)
               ADD    WRK-SYU-JIHI-TAXARI (IDS1)
                                           TO  SYU-JIHI-TAXARI(IDS1)
           END-PERFORM
      *
           ADD     WRK-SYU-JIHI-TOTAL       TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX   TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX         TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN          TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN          TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI   TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN          TO  SYU-YKZ-FTN
      *調整金
           ADD     WRK-SYU-CHOSEI           TO  SYU-CHOSEI
           ADD     WRK-SYU-CHOSEI1          TO  SYU-CHOSEI1
           ADD     WRK-SYU-CHOSEI2          TO  SYU-CHOSEI2
      *請求金額−消費税（再掲）
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *請求金額
           ADD     WRK-SYU-SKYMONEY         TO  SYU-SKYMONEY
      *入金合計額
           ADD     WRK-SYU-NYUKIN-TOTAL     TO  SYU-NYUKIN-TOTAL 
      *労災保険
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *
      *訂正前
      *    ADD     SPA-MAE-SKYMONEY(IDX)   TO  WRK-SKYMONEY
      *    ADD     SPA-MAE-NYUKIN-TOTAL(IDX)
      *                                    TO  WRK-NYUKIN-TOTAL
      *    ADD     SPA-MAE-SKYMONEY-TAX-SAI(IDX)
      *                                    TO  WRK-SKYMONEY-TAX-SAI
           IF     (WRK-SYU-DENPNUM     >   ZERO )
      *        訂正前
               ADD     SPA-TEIKEI-SKYMONEY(IDX)
                                           TO  SPA-NAI-SEIKYU
               ADD     SPA-TEI-SKYMONEY-TAX-SAI(IDX)
                                           TO  SPA-NAI-SKYMONEY-TAX-SAI
               ADD     SPA-TMAE-CHOSEI  (IDX)
                                           TO  SPA-NAI-CHOSEI
               ADD     SPA-TMAE-CHOSEI1 (IDX)
                                           TO  SPA-NAI-CHOSEI1
               ADD     SPA-TMAE-CHOSEI2 (IDX)
                                           TO  SPA-NAI-CHOSEI2
               ADD     SPA-MAE-SKYMONEY(IDX)
                                           TO  SPA-OLD-SEIKYU
               ADD     SPA-MAE-NYUKIN-TOTAL(IDX)
                                           TO  SPA-NAI-NYUKIN
      *        返金額
               ADD     SPA-SYU-HENKIN (IDX)
                                           TO  SPA-NAI-HENKIN
           END-IF
      *    訂正で新規の収納
           IF    (SPA-SYORIFLG         =   1    )
             AND (WRK-SYU-DENPNUM      =   ZERO )
      *        請求金額
               ADD     WRK-SYU-SKYMONEY
                                           TO  SPA-NAI-SEIKYU
      *        請求金額−消費税（再掲）
               ADD     WRK-SYU-SKYMONEY-TAX-SAI
                                           TO  SPA-NAI-SKYMONEY-TAX-SAI
           END-IF
      *
      *!
      *    減免区分
           IF     (WRK-SYU-DISCOUNT-TEIRITU    NOT =   SPACE )
               MOVE    WRK-SYU-DISCOUNT-TEIRITU
                                               TO  SYU-DISCOUNT-TEIRITU
               MOVE    WRK-SYU-DISCOUNT-RATE
                                               TO  SYU-DISCOUNT-RATE
           END-IF
            .
       31000-SYUNOU-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費名称編集処理
      *****************************************************************
       3102-JIHIMSG-HEN-SEC             SECTION.
      *
      *    医療機関情報−所在地、連絡先
           MOVE    SPACE               TO  SYS-1013-REC
           MOVE    "1013"              TO  SYS-1013-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1013-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1013-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1013-HOSPNUM
           MOVE    SYS-1013-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    SYSKANRI-REC    TO  SYS-1013-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-1013-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
               UNTIL       FLG-SYSKANRI    =   1
               MOVE    SYSKANRI-REC        TO  SYS-1013-REC
               IF      SPA-NYUGAIKBN       =   SYS-1013-KBNCD(1:1)
                   EVALUATE    SYS-1013-KBNCD(2:1)
                       WHEN    "1"
                           MOVE    SYS-1013-JIHINAME   
                                               TO  SPA-GMN-JIHIMSG (1)
                       WHEN    "2"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (2)
                       WHEN    "3"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (3)
                       WHEN    "4"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (4)
                       WHEN    "5"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (5)
                       WHEN    "6"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (6)
                       WHEN    "7"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (7)
                       WHEN    "8"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (8)
                       WHEN    "9"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (9)
                       WHEN    "0"
                           MOVE    SYS-1013-JIHINAME
                                               TO  SPA-GMN-JIHIMSG (10)
                   END-EVALUATE
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3102-JIHIMSG-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター編集処理
      *****************************************************************
       3103-DRCD-HEN-SEC                  SECTION.
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-GMN-SRYKA       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD        TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
      *
           .
       3103-DRCD-HEN-EXT.
           EXIT.
      * 
      *****************************************************************
      *    複数科編集処理
      *****************************************************************
       3104-FUKU-HEN-SEC                  SECTION.
      *
           INITIALIZE                  SPA-FUKU-AREA
      *    当日の複数科まとめの有無判定
           PERFORM 31040-FUKU-CHK-SEC
      *
      *    訂正で確定によりメッセージ表示
           EVALUATE    SPA-FUKU-MATKBN
               WHEN    "1"
                   MOVE    "＜複数科の継続中＞"    TO  SPA-GMN-FUKUMSG
               WHEN    "2"
                   MOVE    "＜複数科の確定済＞"    TO  SPA-GMN-FUKUMSG
               WHEN    OTHER
                   MOVE    SPACE                   TO  SPA-GMN-FUKUMSG
           END-EVALUATE
      *
           .
       3104-FUKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象のまとめ判定処理
      *****************************************************************
       31040-FUKU-CHK-SEC                  SECTION.
      *
      *    訂正で、同時入力が無い時
           INITIALIZE                      TBL-FUKU-AREA
           INITIALIZE                      SPA-FUKU-AREA
           IF      SPA-SYORIFLG        =   1
               IF     (SYU-FUKU-DENPNUM    NOT =   ZERO )
      *            複数科・保険入力が無い時
                 AND  (SYU-DOUJI-DENPNUM       =   ZERO )
                   ADD     1                   TO  SPA-FUKU-CNT
                   MOVE    SYU-FUKU-DENPNUM    TO  SPA-FUKU-DENPNUM
                   MOVE    SYU-FUKU-KBN        TO  SPA-FUKU-MATKBN
               END-IF
           END-IF
      *
           .
       31040-FUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金取扱い編集処理
      *****************************************************************
       3105-NYUKINKBN-HEN-SEC           SECTION.
      *
      *    入院の取扱い
           INITIALIZE                  SPA-GMN-NYUKINKBN-G
           INITIALIZE                  SPA-GMN-NYUKINKBN-LISTG
           INITIALIZE                  SPA-NYUKINKBN-MAX
           MOVE    "1 今回請求分のみ入金"
                                       TO  SPA-GMN-NYUKINKBN-LST(01)
           MOVE    "2 今回分・伝票の古い未収順に入金"
                                       TO  SPA-GMN-NYUKINKBN-LST(02)
           MOVE    "3 今回分・伝票の新しい未収順に入金"
                                       TO  SPA-GMN-NYUKINKBN-LST(03)
           MOVE    "4 伝票の古い未収順に入金"
                                       TO  SPA-GMN-NYUKINKBN-LST(04)
           MOVE    "5 伝票の新しい未収順に入金"
                                       TO  SPA-GMN-NYUKINKBN-LST(05)
           MOVE    5                   TO  SPA-NYUKINKBN-MAX
      *
      *    請求書発行方法
           MOVE    "1 診療科・保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(1)
           MOVE    "2 保険組合せ別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(2)
           MOVE    "3 診療科別に発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(3)
           MOVE    "4 全体をまとめて発行"
                                       TO  SPA-GMN-HAKHOUFLG-LST(4)
           MOVE    4                   TO  SPA-HAKHOUFLG-MAX
      *
      *    診療行為一括入金管理情報
           MOVE    SPACE               TO  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1038-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC     TO  SYS-1038-REC
      *        入金管理区分
               MOVE    SYS-1038-NYKNKNRKBN     TO  SPA-GMN-NYUKINKBN
      *        請求書発行方法
               MOVE    SYS-1038-HAKHOUFLG      TO  SPA-GMN-HAKHOUFLG
      *        訂正の請求書金額
               IF      SYS-1038-TEIKINKBN      =   SPACE
                   MOVE    "0"                 TO  SPA-NAI-TEIKINKBN
               ELSE
                   MOVE    SYS-1038-TEIKINKBN  TO  SPA-NAI-TEIKINKBN
               END-IF
      *        入金・返金額設定区分
               MOVE    SYS-1038-NYKHNKKBN      TO  SPA-NAI-NYKHNKKBN
      *        発行日変更区分
               MOVE    SYS-1038-YMDHENKBN      TO  SPA-HAKYMD-FLG
           ELSE
               MOVE    "1"                     TO  SPA-GMN-NYUKINKBN
               MOVE    "1"                     TO  SPA-GMN-HAKHOUFLG
               MOVE    "0"                     TO  SPA-NAI-TEIKINKBN
               MOVE    "0"                     TO  SPA-NAI-NYKHNKKBN
               MOVE    "0"                     TO  SPA-HAKYMD-FLG
           END-IF
      *    訂正の時、入金管理区分は１のみとする
           IF      SPA-SYORIFLG        =   1
               MOVE    1                   TO  SPA-NYUKINKBN-MAX
               MOVE    SPA-GMN-NYUKINKBN-LST(01)
                                           TO  SPA-GMN-NYUKINKBN-G
               MOVE    SPA-GMN-NYUKINKBN   TO  SPA-MAE-NYUKINKBN
           END-IF
           IF      SPA-GMN-NYUKINKBN       =   SPACE
               MOVE    "1"                 TO  SPA-GMN-NYUKINKBN
           END-IF
           IF      SPA-GMN-HAKHOUFLG       =   SPACE
               MOVE    "1"                 TO  SPA-GMN-HAKHOUFLG
           END-IF
           IF      SPA-NAI-NYKHNKKBN       =   SPACE
               MOVE    "0"                 TO  SPA-NAI-NYKHNKKBN
           END-IF
      *    訂正で請求書合計金額の時
           IF     (SPA-SYORIFLG        =   1   )  AND
      *********** (SPA-SKYPRTFLG       =   "1" OR  "2"
                  (WRK-SKYPRTFLG       =   "1" OR  "2"
                                       OR  "4" OR  "5"
                                       OR  "7"     )
               IF      SPA-NAI-TEIKINKBN   =   "1"
                   MOVE    "2"             TO  SPA-GMN-HAKFLG
               ELSE
                   MOVE    "1"             TO  SPA-GMN-HAKFLG
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUKINKBN-MAX
               IF      SPA-GMN-NYUKINKBN   =   SPA-GMN-NYUKINKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUKINKBN-LST (IDX)
                                               TO  SPA-GMN-NYUKINKBN-G
                   MOVE    SPA-NYUKINKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG   =   SPA-GMN-HAKHOUFLG-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-HAKHOUFLG-LST (IDX)
                                               TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    SPA-HAKHOUFLG-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-NYUKINKBN   TO  SPA-MAE-NYUKINKBN
      *
           .
       3105-NYUKINKBN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金方法フラグ編集処理
      *****************************************************************
       3103-NYUKINHOU-HEN-SEC           SECTION.
      *
      *    入院の取扱い
           INITIALIZE                  SPA-GMN-NYUKINHOU-LSTG
           INITIALIZE                  SPA-GMN-NYUKINHOU-G
      *    入金方法入金状態
           INITIALIZE                  SPA-NAI-NYUKINHOU-LSTG
      *
           MOVE    SPACE               TO  SYS-1041-REC
           MOVE    "1041"              TO  SYS-1041-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1041-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1041-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1041-HOSPNUM
           MOVE    SYS-1041-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1041-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM
               UNTIL     ( FLG-SYSKANRI    =   1  )  OR
                         ( SPA-NYUKINHOU-MAX   >=  99)
               MOVE    SYSKANRI-REC        TO  SYS-1041-REC
               ADD     1                   TO  SPA-NYUKINHOU-MAX
               MOVE    SYS-1041-KBNCD(1:2) TO  SPA-GMN-NYUKINHOU-LIST
                                                     (SPA-NYUKINHOU-MAX)
                                                     (1:2)
               MOVE    SYS-1041-NYKN-TANNAME
                                           TO  SPA-GMN-NYUKINHOU-LIST
                                                     (SPA-NYUKINHOU-MAX)
                                                     (4:)
               MOVE    SYS-1041-NYKN-JYOTAI
                                           TO  SPA-NAI-NYUKIN-JYOTAIL
                                                     (SPA-NYUKINHOU-MAX)
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    初期表示
      *    患者の入金方法
           IF      SPA-NYUKIN-HOHO     =   SPACE
               MOVE    SPA-GMN-NYUKINHOU-LIST (1)
                                       TO  SPA-GMN-NYUKINHOU-G
               MOVE    SPA-NAI-NYUKIN-JYOTAIL (1)
                                       TO  SPA-NAI-NYUKIN-JYOTAI
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NYUKINHOU-MAX )  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-NYUKIN-HOHO     =   SPA-GMN-NYUKINHOU-LIST
                                                           (IDX)(1:2)
                   MOVE    SPA-GMN-NYUKINHOU-LIST (IDX)
                                           TO  SPA-GMN-NYUKINHOU-G
                   MOVE    SPA-NAI-NYUKIN-JYOTAIL (IDX)
                                           TO  SPA-NAI-NYUKIN-JYOTAI
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    SPA-GMN-NYUKINHOU-LIST (1)
                                       TO  SPA-GMN-NYUKINHOU-G
               MOVE    SPA-NAI-NYUKIN-JYOTAIL(1)
                                       TO  SPA-NAI-NYUKIN-JYOTAI
           END-IF
      *
           .
       3103-NYUKINHOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    未収金集計処理
      *****************************************************************
       3110-MISYUKIN-SYUKEI-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-ZENMISYU
           MOVE    ZERO                TO  SPA-GMN-CHOKAKIN
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "2"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
           PERFORM UNTIL     FLG-SYUNOU    NOT =  ZERO
               IF      SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
      *            未収額
                   COMPUTE SPA-GMN-ZENMISYU    =   SPA-GMN-ZENMISYU
                                               +  (SYU-SKYMONEY
                                               -   SYU-NYUKIN-TOTAL)
               ELSE
      *            超過額
                   COMPUTE SPA-GMN-CHOKAKIN    =   SPA-GMN-CHOKAKIN
                                               +  (SYU-NYUKIN-TOTAL
                                               -   SYU-SKYMONEY     )
               END-IF
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3110-MISYUKIN-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院未収額設定処理
      *****************************************************************
       3109-NYUIN-MISYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-NYUIN-MISYU
      *    入院分未収集計
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    "1"                 TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUNOU
               INITIALIZE              SYUNOU-REC
           END-IF
           PERFORM UNTIL     FLG-SYUNOU    NOT =  ZERO
               IF      SYU-DENPPRTYMD    <=  SPA-SRYYMD
      *            IF      SYU-SKYMONEY  >   SYU-NYUKIN-TOTAL
      *                未収額
                       COMPUTE WRK-NYUIN-MISYU =   WRK-NYUIN-MISYU
                                               +  (SYU-SKYMONEY
                                               -   SYU-NYUKIN-TOTAL)
      *             END-IF
               END-IF
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SPA-GMN-NYUMISYU
           MOVE    ZERO                TO  SPA-NAI-NYUMIKBN
           IF      WRK-NYUIN-MISYU NOT =   ZERO
               MOVE    WRK-NYUIN-MISYU     TO  WRK-Z9
               PERFORM 31091-KINHENSYU-SEC
               STRING  "【入院　未収額：" 
                                           DELIMITED  BY  SIZE
                       WRK-HEN             DELIMITED  BY  SPACE
                       "】"                DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-NYUMISYU
               END-STRING
               IF      WRK-NYUIN-MISYU     <   ZERO
                   MOVE    1               TO  SPA-NAI-NYUMIKBN
               END-IF
           END-IF
           .
       3109-NYUIN-MISYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    金額計算処理
      *****************************************************************
       31091-KINHENSYU-SEC           SECTION.
      *
           MOVE    SPACE             TO  WRK-HEN
           MOVE    ZERO              TO  IDH1
           PERFORM VARYING     IDH2  FROM    1   BY  1
                   UNTIL       IDH2  >   10
               IF      WRK-Z9-G(IDH2:1)  NOT =   SPACE
                   ADD     1             TO  IDH1
                   MOVE    WRK-Z9-G(IDH2:1)  TO  WRK-HEN(IDH1:1)
               END-IF
           END-PERFORM
           .
       31091-KINHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計請求額計算処理
      *****************************************************************
       3101-SEIKYU-KEI-SEC                  SECTION.
      *
      *    今回請求額
           COMPUTE SPA-GMN-SEIKYU      =  (SPA-NAI-SEIKYU
                                       +   SPA-GMN-CHOSEI1
                                       +   SPA-GMN-CHOSEI2)
                                       -   SPA-OLD-SEIKYU
      *?   今回入金
      *    IF      SPA-GMN-SEIKYU      <   ZERO
      *        MOVE    ZERO            TO  SPA-GMN-NYUKIN
      *?   END-IF
      *
      *    返金額入力可
           MOVE    SPACE               TO  SPA-GMN-TEISEIMSG
           MOVE    ZERO                TO  SPA-NAI-HENFLG
           IF      SPA-SYORIFLG        =   ZERO
               IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI) AND
                      (SPA-GMN-CHOKAKIN    NOT =   ZERO  )   AND
                      (SPA-IDX-KOUI        >   ZERO ) 
                   MOVE    1               TO  SPA-NAI-HENFLG
               END-IF
           ELSE
               IF     (SPA-GMN-SEIKYU      <   ZERO )  AND
                      (SPA-IDX-KOUI        >   ZERO )  AND
                      (SPA-GMN-SRYKA   NOT =   ZERO )  AND
                      (SPA-GMN-HKNCOMBI    NOT =   ZERO )
      *            前回入金が未収の時
                   IF     (SPA-OLD-SEIKYU  +   SPA-NAI-CHOSEI)
                                               <=  SPA-NAI-NYUKIN
                       MOVE    1               TO  SPA-NAI-HENFLG
                   ELSE
                       MOVE    "＜収納が未収です＞"
                                               TO  SPA-GMN-TEISEIMSG
                   END-IF
               END-IF
           END-IF
           .
       3101-SEIKYU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計入金額算処理
      *****************************************************************
       3103-NYUKIN-KEI-SEC                  SECTION.
      *
           MOVE    ZERO            TO  SPA-GOK-NYUKIN
           MOVE    ZERO            TO  SPA-GOK-CHOSEI
           MOVE    ZERO            TO  SPA-GOK-CHOSEI1
           MOVE    ZERO            TO  SPA-GOK-CHOSEI2
           MOVE    ZERO            TO  SPA-GOK-SKYMONEY
           MOVE    ZERO            TO  SPA-OLD-SKYMONEY
           MOVE    ZERO            TO  SPA-NAI-HENKIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
               IF      WRK-SYU-SRYKA   NOT =   ZERO
                   ADD    WRK-SYU-NYUKIN-TOTAL TO  SPA-GOK-NYUKIN
                   ADD    WRK-SYU-CHOSEI       TO  SPA-GOK-CHOSEI
                   ADD    WRK-SYU-CHOSEI1      TO  SPA-GOK-CHOSEI1
                   ADD    WRK-SYU-CHOSEI2      TO  SPA-GOK-CHOSEI2
                   IF     WRK-SYU-DENPNUM      =   ZERO
                       ADD    WRK-SYU-SKYMONEY     TO  SPA-GOK-SKYMONEY
      *                返金を入金額へ
                       ADD     SPA-SYU-HENKIN (IDX)
                                                   TO  SPA-NAI-HENKIN
                   ELSE
                       ADD    SPA-TEIKEI-SKYMONEY(IDX)
                                                   TO  SPA-GOK-SKYMONEY
                       ADD    SPA-MAE-SKYMONEY(IDX)
                                                   TO  SPA-OLD-SKYMONEY
      *                返金（訂正）
                       ADD     SPA-SYU-HENKIN (IDX)
                                                   TO  SPA-NAI-HENKIN
                   END-IF
               END-IF
           END-PERFORM
      *
      *****初期表示の時、未収額の編集へ
      *    IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA  )  AND
      *           (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
      *        MOVE    ZERO                TO  SPA-NYU-ZENMISYU
      *        IF     (SPA-GMN-NYUKIN      >   SPA-GOK-NYUKIN ) AND
      *               (SPA-GMN-ZENMISYU    >   ZERO           )
      *            IF      SPA-GMN-NYUKINKBN   =   "1"
      *                CONTINUE
      *            ELSE
      *                COMPUTE SPA-NYU-ZENMISYU    =   SPA-GMN-NYUKIN
      *                                            -   SPA-GOK-NYUKIN
      *            END-IF
      *        END-IF
      ***  END-IF
      *
           COMPUTE SPA-GOK-NYUKIN      =   SPA-GOK-NYUKIN
                                       +   SPA-NYU-ZENMISYU
      *
      *    合計請求額（合計未収額）
           IF      SPA-OLD-SKYMONEY    NOT =   ZERO
               COMPUTE SPA-GMN-SEIGOK  =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
                                       -   SPA-OLD-SKYMONEY
                                       +   SPA-GMN-ZENMISYU
                                       -   SPA-GMN-CHOKAKIN
           ELSE
               COMPUTE SPA-GMN-SEIGOK  =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
                                       +   SPA-GMN-ZENMISYU
                                       -   SPA-GMN-CHOKAKIN
           END-IF
      *    入金額上限
           PERFORM 31031-JYOGENKIN-KEI-SEC
      *
      *
      *    合計入金額
           IF     (SPA-SYUNOU-MAX      >   1    )  OR
                  (SPA-GMN-CHOKAKIN    >   ZERO )  OR
                  (SPA-GOK-NYUKIN      >   ZERO )
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    SPA-GOK-NYUKIN      TO  WRK-KIN
               ELSE
                   COMPUTE WRK-KIN             =   SPA-GOK-NYUKIN
                                               -   SPA-NAI-HENKIN
               END-IF
               MOVE    SPACE               TO  SPA-GMN-NYUKINMSG
               MOVE    WRK-KIN             TO  WRK-Z9
               IF      WRK-KIN             >=  ZERO
                   STRING  "合計入金額："      DELIMITED  BY  SIZE
                           WRK-Z9              DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-NYUKINMSG
                   END-STRING
               ELSE
                   STRING  "合計返金額："      DELIMITED  BY  SIZE
                           WRK-Z9              DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-NYUKINMSG
                   END-STRING
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-NYUKINMSG
           END-IF
           .
       3103-NYUKIN-KEI-EXT.
           EXIT.
      *****************************************************************
      *    入金額上限計算処理
      *****************************************************************
       31031-JYOGENKIN-KEI-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-JYOGENKIN
           IF      SPA-OLD-SKYMONEY    NOT =   ZERO
               COMPUTE SPA-NAI-JYOGENKIN
                                       =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
                                       -   SPA-OLD-SKYMONEY
           ELSE
               COMPUTE SPA-NAI-JYOGENKIN
                                       =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
           END-IF
           IF      SPA-GMN-NYUKINKBN   =   "1"
               CONTINUE
           ELSE
               COMPUTE SPA-NAI-JYOGENKIN
                                       =   SPA-NAI-JYOGENKIN
                                       +   SPA-GMN-ZENMISYU
                                       -   SPA-GOK-HENKIN
           END-IF
           MOVE    SPACE               TO  SPA-GMN-JYOGENMSG
      *
           IF      SPA-NAI-JYOGENKIN   =   ZERO
               MOVE    "0"             TO  WRK-HEN
           ELSE
               MOVE    SPA-NAI-JYOGENKIN   TO  WRK-Z9
               PERFORM 31091-KINHENSYU-SEC
           END-IF
      *    訂正時は表示なし
           IF      SPA-SYORIFLG        =   ZERO
             IF      SPA-NAI-JYOGENKIN   >=  ZERO
               STRING  "入金上限額：" 
                                       DELIMITED  BY  SIZE
                   WRK-HEN             DELIMITED  BY  SPACE
                   "円"                DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-JYOGENMSG
               END-STRING
             ELSE
               STRING  "今回返金額：" 
                                       DELIMITED  BY  SIZE
                   WRK-HEN             DELIMITED  BY  SPACE
                   "円"                DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-JYOGENMSG
               END-STRING
             END-IF
           END-IF
           .
       31031-JYOGENKIN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＭＥ画面よりの処理
      *****************************************************************
       3009-CLAIM-SYORI-SEC              SECTION.
      *
           IF      SPA-K03X-FLG        =  "OK"
               MOVE    1               TO  FLG-CLAIM
           ELSE
               MOVE    ZERO            TO  FLG-CLAIM
           END-IF
      *
           MOVE    ZERO                TO  SPA-CLAIM-MULTIHOST
      *    更新処理
           MOVE    1                   TO  FLG-CLAIM-OK
           PERFORM 4500-TOROKU-SYORI-SEC
      **** IF      SPA-ERRCD (1:1)     =   "K"
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
          .
       3009-CLAIM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
           MOVE    SPACE               TO  K03
      *
           INITIALIZE                      K03
           MOVE    SPA-PTNUM           TO  K03-PTNUM
           MOVE    SPA-KANANAME        TO  K03-KANANAME
           MOVE    SPA-NAME            TO  K03-NAME
           MOVE    SPA-SEX             TO  K03-SEX
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  K03-HKNCOMBI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
               MOVE     SPA-GMN-HKN-LIST(IDX)  TO  K03-HKNCOMBI-ITEM
                                                              (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K03-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-FTNRATEMEI  TO  K03-FTNRATE
           MOVE    SPA-SRYYMDW         TO  K03-SRYYMD
           MOVE    SPA-BIRTHDAYW       TO  K03-BIRTHDAY
           MOVE    SPA-GMN-SRYKA-G
                                       TO  K03-SRYKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-SRYKA-MAX
               MOVE     SPA-GMN-SRYKA-LIST(IDX)  TO  K03-SRYKA-ITEM
                                                              (IDX)
           END-PERFORM
           MOVE    SPA-GMN-SRYKA-MAX   TO  K03-SRYKA-COUNT
      *
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  K03-NENREI
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K03-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K03-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K03-SYORIMEI-VALUE
           END-EVALUATE
      *
           MOVE    SPA-GMN-HAKYMD      TO  K03-HAKYMD
           MOVE    SPA-GMN-DENPNUM     TO  K03-DENPNUM
      *    診療料
           MOVE    SPA-GMN-HKNRYO (01) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SSUHKNTEN
      *    指導料
           MOVE    SPA-GMN-HKNRYO (02) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SDOHKNTEN
      *    在宅料
           MOVE    SPA-GMN-HKNRYO (03) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ZTKHKNTEN
      *    投薬料
           MOVE    SPA-GMN-HKNRYO (04) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-TYKHKNTEN
      *    注射料
           MOVE    SPA-GMN-HKNRYO (05) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-CSYHKNTEN
      *    処置料
           MOVE    SPA-GMN-HKNRYO (06) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SYCHKNTEN
      *    手術料
           MOVE    SPA-GMN-HKNRYO (07) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SJTHKNTEN
      *    麻酔料
           MOVE    SPA-GMN-HKNRYO (08) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-MSIHKNTEN
      *    検査料
           MOVE    SPA-GMN-HKNRYO (09) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-KNSHKNTEN
      *    Ｘ線料
           MOVE    SPA-GMN-HKNRYO (10) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-GZUHKNTEN
      *    リハビリ
           MOVE    SPA-GMN-HKNRYO (11) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ETCHKNTEN
      *    精神専門
           MOVE    SPA-GMN-HKNRYO (12) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SSNHKNTEN
      *    放射線
           MOVE    SPA-GMN-HKNRYO (13) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-HOUHKNTEN
      *    病理診断
           MOVE    SPA-GMN-HKNRYO (14) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-BYRHKNTEN
      *    入院料等
           MOVE    SPA-GMN-HKNRYO (15) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-NYUHKNTEN
      *    療養担当手当て
           MOVE    SPA-GMN-HKNRYO (16) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-RYOHKNTEN
      *    合計点数
           MOVE    SPA-GMN-HKNRYO (17) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-TOTALHKNTEN
      *    保険適用外
      *    診療料
           MOVE    SPA-GMN-TGMONEY (01) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SUUTGMONEY
      *    指導料
           MOVE    SPA-GMN-TGMONEY (02) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SDOTGMONEY
      *    在宅料
           MOVE    SPA-GMN-TGMONEY (03) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ZTKTGMONEY
      *    投薬料
           MOVE    SPA-GMN-TGMONEY (04) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-TYKTGMONEY
      *    注射料
           MOVE    SPA-GMN-TGMONEY (05) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-CSYTGMONEY
      *    処置料
           MOVE    SPA-GMN-TGMONEY (06) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SYCTGMONEY
      *    手術料
           MOVE    SPA-GMN-TGMONEY (07) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SJTGMONEY
      *    麻酔料
           MOVE    SPA-GMN-TGMONEY (08) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-MSITGMONEY
      *    検査料
           MOVE    SPA-GMN-TGMONEY (09) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-KNSTGMONEY
      *    Ｘ線料
           MOVE    SPA-GMN-TGMONEY (10) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-GZUTGMONEY
      *    その他
           MOVE    SPA-GMN-TGMONEY (11) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ETCTGMONEY
      *    精神療法
           MOVE    SPA-GMN-TGMONEY (12) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-SSNTGMONEY
      *    放射線
           MOVE    SPA-GMN-TGMONEY (13) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-HOUTGMONEY
      *    病理診断
           MOVE    SPA-GMN-TGMONEY (14) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-BYRTGMONEY
      *    入院料
           MOVE    SPA-GMN-TGMONEY (15) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-NYUTGMONEY
      *    療養担当
           MOVE    SPA-GMN-TGMONEY (16) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-RYOTGMONEY
      *    見出し
           IF     (SPA-GMN-HKNRYO (15) =   ZERO )  AND
                  (SPA-GMN-TGMONEY(15) =   ZERO )
               CONTINUE
           ELSE
               MOVE    "入院料等"          TO  K03-SRYMEI14
           END-IF
           IF     (SPA-GMN-HKNRYO (16) =   ZERO )  AND
                  (SPA-GMN-TGMONEY(16) =   ZERO )
               CONTINUE
           ELSE
               MOVE    "療養担当手当"      TO  K03-SRYMEI15
           END-IF
      *
      *    自費
           MOVE    SPA-GMN-JIHI (1)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI1
           MOVE    SPA-GMN-JIHI (2)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI2
           MOVE    SPA-GMN-JIHI (3)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI3
           MOVE    SPA-GMN-JIHI (4)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI4
           MOVE    SPA-GMN-JIHI (5)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI5
           MOVE    SPA-GMN-JIHI (6)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI6
           MOVE    SPA-GMN-JIHI (7)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI7
           MOVE    SPA-GMN-JIHI (8)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI8
           MOVE    SPA-GMN-JIHI (9)    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI9
           MOVE    SPA-GMN-JIHI (10)   TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHI10
      *    自費計
           MOVE    SPA-GMN-JIHIKEI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHIGOK
      *    自費（消費税あり）
           MOVE    SPA-GMN-JIHITAX(1)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX1
           MOVE    SPA-GMN-JIHITAX(2)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX2
           MOVE    SPA-GMN-JIHITAX(3)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX3
           MOVE    SPA-GMN-JIHITAX(4)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX4
           MOVE    SPA-GMN-JIHITAX(5)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX5
           MOVE    SPA-GMN-JIHITAX(6)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX6
           MOVE    SPA-GMN-JIHITAX(7)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX7
           MOVE    SPA-GMN-JIHITAX(8)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX8
           MOVE    SPA-GMN-JIHITAX(9)  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX9
           MOVE    SPA-GMN-JIHITAX(10) TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX10
      *    自費計
           MOVE    SPA-GMN-JIHITAXKEI  TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAXGOK
      *    自費税
           MOVE    SPA-GMN-JIHIZEI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-JIHITAX
      *    自費合計
      **** MOVE    SPA-GMN-JIHIGOK     TO  WRK-Z9
      *****MOVE    WRK-Z9              TO  K03-JIHITOTAL
      *    薬剤一部負担金
           MOVE    SPA-GMN-YKZFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-YKZFTN
      *    老人一部負担金
           MOVE    SPA-GMN-ROUFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-RJNFTN
      *    公費一部負担金
           MOVE    SPA-GMN-KOHFTN      TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-KOHFTN
      *    一部負担金計
           MOVE    SPA-GMN-ICHIFTN     TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-FTNTOTAL
      *    保険適用分
           MOVE    SPA-GMN-HKNTEKI     TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-HKNTEKMONEY
      *    保険適用外
           MOVE    SPA-GMN-HKNGAI      TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-HKNTEKGAI
      *    調整金
      *    MOVE    SPA-GMN-CHOSEI      TO  WRK-Z92
      *    MOVE    WRK-Z92             TO  K03-CHOSEI
           MOVE    SPA-GMN-CHOSEI1-X   TO  K03-CHOSEI
           MOVE    SPA-GMN-CHOSEI2-X   TO  K03-CHOSEI2
      *    今回請求額
      *    継続の時今回請求額をゼロする
      *    IF      SPA-CONTKBN         =   "1"
      *        MOVE    SPACE               TO  K03-SKYMONEY
      *        MOVE    SPACE               TO  K03-NYUKIN
      *****ELSE
      *    今回請求額
           MOVE    SPA-GMN-SEIKYU      TO  WRK-Z99
           MOVE    WRK-Z99             TO  K03-SKYMONEY
      *    入金額
           MOVE    SPA-GMN-NYUKIN      TO  WRK-Z99
           MOVE    WRK-Z99(2:)         TO  K03-NYUKIN
      *
      *    返金額
           IF      SPA-NAI-HENKBN      =   "1"
               IF      SPA-GMN-HENKIN  NOT =   ZERO
      *        充当分
                   MOVE    SPA-GMN-HENKIN      TO  WRK-Z97
                   MOVE    "("                 TO  K03-HENKIN(1:1)
                   MOVE    WRK-Z97             TO  K03-HENKIN(2:)
                   MOVE    ")"                 TO  K03-HENKIN(10:)
               END-IF
           ELSE
               MOVE    SPA-GMN-HENKIN      TO  WRK-Z92
               MOVE    WRK-Z92(2:)         TO  K03-HENKIN
           END-IF
      *    入金上限額
           MOVE    SPA-GMN-JYOGENMSG   TO  K03-JYOGENMSG-VALUE
           IF      SPA-NAI-JYOGENKIN   <   ZERO
               MOVE    "red"               TO  K03-JYOGENMSG-STYLE
           ELSE
               MOVE    "black"             TO  K03-JYOGENMSG-STYLE
           END-IF
      *
      *    前回未収
           MOVE    SPA-GMN-ZENMISYU    TO  WRK-Z92
           MOVE    WRK-Z92             TO  K03-MISYUZAN
           MOVE    "blue"              TO  K03-MISYUZAN-STYLE
      *    前回超過
           MOVE    SPA-GMN-CHOKAKIN    TO  WRK-Z92
           MOVE    WRK-Z92             TO  K03-CHOUKAKIN
           MOVE    "red"               TO  K03-CHOUKAKIN-STYLE
      *    合計請求額
      *    MOVE    SPA-GMN-SEIGOK      TO  WRK-Z99
           COMPUTE WRK-KIN             =   SPA-GMN-SEIGOK
                                       -   SPA-GOK-NYUKIN
           MOVE    WRK-KIN             TO  WRK-Z99
           MOVE    WRK-Z99             TO  K03-GOKSKY
      *    請求書発行
           MOVE    SPA-GMN-HAKFLG-G    TO  K03-HAKFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKFLG-MAX
               MOVE    SPA-GMN-HAKFLGLST (IDX)
                                           TO  K03-HAKFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-HAKFLG-MAX      TO  K03-HAKFLG-COUNT
      *    請求書発行方法
           MOVE    SPA-GMN-HAKHOUFLG-G TO  K03-HAKHOUFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKHOUFLG-MAX
               MOVE    SPA-GMN-HAKHOUFLG-LST (IDX)
                                           TO  K03-HAKHOUFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-HAKHOUFLG-MAX   TO  K03-HAKHOUFLG-COUNT
      *    請求書発行
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYOHOPRTFLG-MAX
               MOVE    SPA-GMN-SYOHOPRTLST (IDX)
                                           TO  K03-SYOHOPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-SYOHOPRTFLG-G   TO  K03-SYOHOPRTFLG
           MOVE    SPA-SYOHOPRTFLG-MAX     TO  K03-SYOHOPRTFLG-COUNT
      *    診療費明細書発行
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               MOVE    SPA-GMN-MEIPRTFLGLST (IDX)
                                           TO  K03-MEIPRTFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-GMN-MEIPRTFLG-G     TO  K03-MEIPRTFLG
           MOVE    SPA-MEIPRTFLG-MAX       TO  K03-MEIPRTFLG-COUNT
      *    薬剤情報発行
           MOVE    SPA-GMN-YAKJYOFLG-G     TO  K03-YAKJYOFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YAKJYOFLG-MAX
               MOVE    SPA-GMN-YAKJYOLST (IDX)
                                           TO  K03-YAKJYOFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-YAKJYOFLG-MAX   TO  K03-YAKJYOFLG-COUNT
      *    お薬手帳報発行
           MOVE    SPA-GMN-OKUSURIFLG-G    TO  K03-OKUSURIFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OKUSURIFLG-MAX
               MOVE    SPA-GMN-OKUSURILST (IDX)
                                           TO  K03-OKUSURIFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-OKUSURIFLG-MAX   TO  K03-OKUSURIFLG-COUNT
      *    予約票発行
           MOVE    SPA-GMN-YYKHYOFLG-G     TO  K03-YYKHYOFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YYKHYOFLG-MAX
               MOVE    SPA-GMN-YYKHYOFLG-LST (IDX)
                                           TO  K03-YYKHYOFLG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-YYKHYOFLG-MAX   TO  K03-YYKHYOFLG-COUNT
      *    Ｕ・Ｐ
           MOVE    SPA-GMN-USERPG-G    TO  K03-USERPG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LIST (IDX)
                                           TO  K03-USERPG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-USERPG-MAX      TO  K03-USERPG-COUNT
      *
      *    自費名称
           MOVE    SPA-GMN-JIHIMSG(1)  TO  K03-JIHIMSG1
           MOVE    SPA-GMN-JIHIMSG(2)  TO  K03-JIHIMSG2
           MOVE    SPA-GMN-JIHIMSG(3)  TO  K03-JIHIMSG3
           MOVE    SPA-GMN-JIHIMSG(4)  TO  K03-JIHIMSG4
           MOVE    SPA-GMN-JIHIMSG(5)  TO  K03-JIHIMSG5
           MOVE    SPA-GMN-JIHIMSG(6)  TO  K03-JIHIMSG6
           MOVE    SPA-GMN-JIHIMSG(7)  TO  K03-JIHIMSG7
           MOVE    SPA-GMN-JIHIMSG(8)  TO  K03-JIHIMSG8
           MOVE    SPA-GMN-JIHIMSG(9)  TO  K03-JIHIMSG9
           MOVE    SPA-GMN-JIHIMSG(10) TO  K03-JIHIMSG10
      *
      *    ドクター
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (IDX)  TO  K03-DRCD-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  K03-DRCD-COUNT
           MOVE    SPA-GMN-DRCD-G      TO  K03-DRCD
      *
      *    労災初診
           MOVE    SPA-GMN-ROUSYOSIN   TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ROUSYOSIN
      *    労災再診
           MOVE    SPA-GMN-ROUSAISIN   TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ROUSAISIN
      *    労災指導
           MOVE    SPA-GMN-ROUSIDOU    TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ROUSIDOU
      *    労災その他
           MOVE    SPA-GMN-ROUETC      TO  WRK-Z9
           MOVE    WRK-Z9              TO  K03-ROUETC
      *
      *    複数診療科まとめ
      *!   MOVE    SPA-GMN-FUKUSAGAKU  TO  WRK-Z92
      *!   MOVE    WRK-Z92             TO  K03-FUKUSAGAKU
      *!   MOVE    SPA-GMN-FUKUMDS     TO  K03-FUKUMDS
           IF      SPA-GMN-FUKUMSG     =   SPACE
               MOVE    SPACE               TO  K03-FUKUMSG
           ELSE
               MOVE    "red"               TO  K03-FUKUMSG-STYLE
               MOVE    SPA-GMN-FUKUMSG     TO  K03-FUKUMSG-VALUE
           END-IF
           MOVE    SPA-GMN-FUKUZENMDS  TO  K03-FUKUZENMDS
           MOVE    SPA-GMN-FUKUZENMDS2 TO  K03-FUKUZENMDS2
      *
      *TEST
           IF      SPA-GMN-FUKUZENMDS2 =   SPACE
               EVALUATE    SPA-SKYPRTFLG
               WHEN    "0"
                   MOVE    "【請求書・明細書不要】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "1"
                   MOVE    "【請求書・明細書必要】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "2"
                   MOVE    "【請求書・明細書必要（請求あり）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "3"
                   MOVE    "【請求書・明細書必要（訂正時なし）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "4"
                   MOVE    "【請求書必要（明細書不要）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "5"
                   MOVE    "【請求書必要（請求あり）（明細書不要）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "6"
                   MOVE    "【請求書必要（訂正時なし）（明細書不要）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "7"
                   MOVE    "【請求書必要（請求あり）（明細書必要）】"
                                           TO  K03-FUKUZENMDS2
      *H27.8
               WHEN    "8"
                   MOVE    "【請求書不要・明細書必要】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "9"
                   MOVE    "【請求書不要・明細書必要（請求あり）】"
                                           TO  K03-FUKUZENMDS2
               WHEN    "A"
                   MOVE    "【請求書不要・明細書必要（訂正時なし）】"
                                           TO  K03-FUKUZENMDS2
               END-EVALUATE
           END-IF
      *
      *    合計入金額
           MOVE    SPA-GMN-NYUKINMSG   TO  K03-NYUKINMSG-VALUE
           IF      SPA-GMN-NYUKINMSG(1:8)  =   "合計入金"
               MOVE    "black"             TO  K03-NYUKINMSG-STYLE
           ELSE
               MOVE    "red"               TO  K03-NYUKINMSG-STYLE
           END-IF
      *
      *R02.2
      *    診療クリア（削除）対応
           IF     (SPA-IDX-KOUI        =   ZERO )
      *        前回請求額の合計を入金情報に編集
               MOVE    SPA-GMN-FUKUZENMDS2 TO  K03-NYUKINMSG-VALUE
               MOVE    "blue"              TO  K03-NYUKINMSG-STYLE
               MOVE    SPACE               TO  K03-FUKUZENMDS2
           END-IF
      *
      *    入金の取扱い
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUKINKBN-MAX
               MOVE    SPA-GMN-NYUKINKBN-LST (IDX)
                                           TO  K03-NYUKINKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUKINKBN-MAX       TO  K03-NYUKINKBN-COUNT
           MOVE    SPA-GMN-NYUKINKBN-G     TO  K03-NYUKINKBN
      *
      *    入金方法
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUKINHOU-MAX
               MOVE    SPA-GMN-NYUKINHOU-LIST (IDX)
                                           TO  K03-NYUKINHOU-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-NYUKINHOU-MAX       TO  K03-NYUKINHOU-COUNT
           MOVE    SPA-GMN-NYUKINHOU-G     TO  K03-NYUKINHOU
      *
      *    入院の前回までの未収額
           IF      SPA-GMN-NYUMISYU    =   SPACE
               MOVE    SPACE               TO  K03-NYUMISYU
           ELSE
               MOVE    "red"               TO  K03-NYUMISYU-STYLE
               MOVE    SPA-GMN-NYUMISYU    TO  K03-NYUMISYU-VALUE
           END-IF
      *
           MOVE    SPA-GMN-TEISEIMSG       TO  K03-TEISEIMSG
      *!
      *減免
           MOVE    SPA-GMN-GENMENMEI   TO  K03-GENMENMEI-VALUE
           IF      SPA-GMN-GENMENMEI   NOT =   SPACE
               MOVE    "blue"              TO  K03-GENMENMEI-STYLE
           END-IF
      *
      *    対象なしの場合
           IF      SPA-NAI-FLG         =   1
               PERFORM 5002-GMNHEN-NASI-SEC
           END-IF
      *!!
           IF      SPA-IKKATUKBN       =   SPACE
               MOVE    "一括入返金"        TO  K03-B10-LABEL
           ELSE
               MOVE    "今回請求"          TO  K03-B10-LABEL
           END-IF
      *!!
           IF      SPA-HAKYMD-FLG      =   1
               MOVE    "発行日"            TO  K03-B03-LABEL
           ELSE
               MOVE    SPACE               TO  K03-B03-LABEL
           END-IF
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象なし画面編集処理
      *****************************************************************
       5002-GMNHEN-NASI-SEC             SECTION.
      *
      *    診療料
           MOVE    ALL "*"             TO  K03-SSUHKNTEN
      *    指導料
           MOVE    ALL "*"             TO  K03-SDOHKNTEN
      *    在宅料
           MOVE    ALL "*"              TO  K03-ZTKHKNTEN
      *    投薬料
           MOVE    ALL "*"              TO  K03-TYKHKNTEN
      *    注射料
           MOVE    ALL "*"              TO  K03-CSYHKNTEN
      *    処置料
           MOVE    ALL "*"              TO  K03-SYCHKNTEN
      *    手術料
           MOVE    ALL "*"              TO  K03-SJTHKNTEN
      *    麻酔料
           MOVE    ALL "*"              TO  K03-MSIHKNTEN
      *    検査料
           MOVE    ALL "*"              TO  K03-KNSHKNTEN
      *    Ｘ線料
           MOVE    ALL "*"              TO  K03-GZUHKNTEN
      *    その他
           MOVE    ALL "*"              TO  K03-ETCHKNTEN
      *    精神専門
           MOVE    ALL "*"              TO  K03-SSNHKNTEN
      *    放射線
           MOVE    ALL "*"              TO  K03-HOUHKNTEN
      *    病理診断
           MOVE    ALL "*"              TO  K03-BYRHKNTEN
      *    入院料等
           MOVE    ALL "*"              TO  K03-NYUHKNTEN
      *    療養担当手当て
           MOVE    ALL "*"              TO  K03-RYOHKNTEN
      *    合計点数
           MOVE    ALL "*"              TO  K03-TOTALHKNTEN
      *    保険適用外
      *    診療料
           MOVE    ALL "*"              TO  K03-SUUTGMONEY
      *    指導料
           MOVE    ALL "*"              TO  K03-SDOTGMONEY
      *    在宅料
           MOVE    ALL "*"              TO  K03-ZTKTGMONEY
      *    投薬料
           MOVE    ALL "*"              TO  K03-TYKTGMONEY
      *    注射料
           MOVE    ALL "*"              TO  K03-CSYTGMONEY
      *    処置料
           MOVE    ALL "*"              TO  K03-SYCTGMONEY
      *    手術料
           MOVE    ALL "*"              TO  K03-SJTGMONEY
      *    麻酔料
           MOVE    ALL "*"              TO  K03-MSITGMONEY
      *    検査料
           MOVE    ALL "*"              TO  K03-KNSTGMONEY
      *    Ｘ線料
           MOVE    ALL "*"              TO  K03-GZUTGMONEY
      *    その他
           MOVE    ALL "*"              TO  K03-ETCTGMONEY
      *    精神療法
           MOVE    ALL "*"              TO  K03-SSNTGMONEY
      *    放射線
           MOVE    ALL "*"              TO  K03-HOUTGMONEY
      *    病理診断
           MOVE    ALL "*"              TO  K03-BYRTGMONEY
      *    入院料
           MOVE    ALL "*"              TO  K03-NYUTGMONEY
      *    療養担当
           MOVE    ALL "*"              TO  K03-RYOTGMONEY
      *
      *    自費
           MOVE    ALL "*"              TO  K03-JIHI1
           MOVE    ALL "*"              TO  K03-JIHI2
           MOVE    ALL "*"              TO  K03-JIHI3
           MOVE    ALL "*"              TO  K03-JIHI4
           MOVE    ALL "*"              TO  K03-JIHI5
           MOVE    ALL "*"              TO  K03-JIHI6
           MOVE    ALL "*"              TO  K03-JIHI7
           MOVE    ALL "*"              TO  K03-JIHI8
           MOVE    ALL "*"              TO  K03-JIHI9
           MOVE    ALL "*"              TO  K03-JIHI10
      *    自費計
           MOVE    ALL "*"              TO  K03-JIHIGOK
      *    自費（消費税あり）
           MOVE    ALL "*"              TO  K03-JIHITAX1
           MOVE    ALL "*"              TO  K03-JIHITAX2
           MOVE    ALL "*"              TO  K03-JIHITAX3
           MOVE    ALL "*"              TO  K03-JIHITAX4
           MOVE    ALL "*"              TO  K03-JIHITAX5
           MOVE    ALL "*"              TO  K03-JIHITAX6
           MOVE    ALL "*"              TO  K03-JIHITAX7
           MOVE    ALL "*"              TO  K03-JIHITAX8
           MOVE    ALL "*"              TO  K03-JIHITAX9
           MOVE    ALL "*"              TO  K03-JIHITAX10
      *    自費計
           MOVE    ALL "*"              TO  K03-JIHITAXGOK
      *    自費税
           MOVE    ALL "*"              TO  K03-JIHITAX
      *    自費合計
      *****MOVE    ALL "*"              TO  K03-JIHITOTAL
      *    薬剤一部負担金
           MOVE    ALL "*"              TO  K03-YKZFTN
      *    老人一部負担金
           MOVE    ALL "*"              TO  K03-RJNFTN
      *    公費一部負担金
           MOVE    ALL "*"              TO  K03-KOHFTN
      *    一部負担金計
           MOVE    ALL "*"              TO  K03-FTNTOTAL
      *    保険適用分
           MOVE    ALL "*"              TO  K03-HKNTEKMONEY
      *    保険適用外
           MOVE    ALL "*"              TO  K03-HKNTEKGAI
           MOVE    ALL "*"              TO  K03-SKYMONEY
           MOVE    ALL "*"              TO  K03-NYUKIN
      *
      *    労災初診
           MOVE    ALL "*"              TO  K03-ROUSYOSIN
      *    労災再診
           MOVE    ALL "*"              TO  K03-ROUSAISIN
      *    労災指導
           MOVE    ALL "*"              TO  K03-ROUSIDOU
      *    労災その他
           MOVE    ALL "*"              TO  K03-ROUETC
      *
           .
       5002-GMNHEN-NASI-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *    調整額
           IF     (SPA-GMN-SRYKA       =   ZERO ) OR
                  (SPA-GMN-HKNCOMBI    =   ZERO ) OR
                  (SPA-NAI-FLG         =   1    )
      *R02.1
      *     削除
             OR   (SPA-IDX-KOUI        =   ZERO )
      *
               MOVE    WIDGET-INSENSITIVE  TO  K03-CHOSEI-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K03-CHOSEI2-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  K03-CHOSEI-STATE
               MOVE    WIDGET-NORMAL       TO  K03-CHOSEI2-STATE
           END-IF
      *     入金額
           IF    ((SPA-GMN-SRYKA       NOT =   ZERO ) AND
                  (SPA-GMN-HKNCOMBI    NOT =   ZERO )) OR
                  ( SPA-NAI-NYUKINFLG   =   1    )
               IF     (SPA-NAI-FLG         =   ZERO )  AND
                      (SPA-IDX-KOUI        >   ZERO )
                   MOVE    WIDGET-NORMAL       TO  K03-NYUKIN-STATE
               ELSE
                   MOVE    WIDGET-INSENSITIVE  TO  K03-NYUKIN-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K03-NYUKIN-STATE
           END-IF
      *    返金
           IF     (SPA-NAI-HENFLG      =   1    )  AND
                  (SPA-IDX-KOUI        >   ZERO )
               MOVE    WIDGET-NORMAL       TO  K03-HENKIN-STATE
               IF      SPA-SYORIFLG        =   1
                   MOVE    WIDGET-INSENSITIVE  TO  K03-NYUKIN-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K03-HENKIN-STATE
           END-IF
      *    発行日
      *    IF      SPA-HAKYMD-FLG      =   1
               MOVE    WIDGET-NORMAL       TO  K03-HAKYMD-STATE
      *    ELSE
      *        MOVE    WIDGET-INSENSITIVE  TO  K03-HAKYMD-STATE
      *    END-IF
      *
           MOVE    WIDGET-NORMAL       TO  K03-B10-STATE
           MOVE    WIDGET-NORMAL       TO  K03-B03-STATE
      *v4.8
      *    ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    WIDGET-INSENSITIVE  TO  K03-USERPG-STATE
      *    ELSE
      *        MOVE    WIDGET-NORMAL       TO  K03-USERPG-STATE
      *    END-IF
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソル指定のない時、入力した項目の次ぎへ
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (SPA-GMN-CUR         =   ZERO )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "NYUKIN"          TO  MCP-WIDGET
               WHEN    07
                   MOVE    "HENKIN"          TO  MCP-WIDGET
               WHEN    02
                   MOVE    "HAKFLG"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SYOHOPRTFLG"      TO  MCP-WIDGET
               WHEN    04
                   MOVE    "YAKJYOFLG"       TO  MCP-WIDGET
               WHEN    05
                   MOVE    "DRCD"            TO  MCP-WIDGET
               WHEN    06
                   MOVE    "CHOSEI"          TO  MCP-WIDGET
               WHEN    08
                   MOVE    "CHOSEI2"          TO  MCP-WIDGET
               WHEN    10
                   MOVE    "NYUKINKBN"       TO  MCP-WIDGET
               WHEN    11
                   MOVE    "HKNCOMBI"        TO  MCP-WIDGET
               WHEN    12
                   MOVE    "SRYKA"           TO  MCP-WIDGET
               WHEN    13
                   MOVE    "NYUKINHOU"       TO  MCP-WIDGET
               WHEN    14
                   MOVE    "HAKHOUFLG"       TO  MCP-WIDGET
               WHEN    15
                   MOVE    "USERPG"          TO  MCP-WIDGET
               WHEN    16
                   MOVE    "MEIPRTFLG"       TO  MCP-WIDGET
               WHEN    17
                   MOVE    "OKUSURIFLG"      TO  MCP-WIDGET
               WHEN    18
                   MOVE    "YYKHYOFLG"       TO  MCP-WIDGET
               WHEN    20
                   MOVE    "HAKYMD"          TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B12"             TO  MCP-WIDGET
      *!       WHEN    08
      *            MOVE    "B09"             TO  MCP-WIDGET
      *        WHEN    09
      *!           MOVE    "B10"             TO  MCP-WIDGET
               WHEN    30
                   MOVE    "B05"             TO  MCP-WIDGET
               WHEN    31
                   MOVE    "B06"             TO  MCP-WIDGET
               WHEN    91
                   MOVE    "B01"             TO  MCP-WIDGET
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
      *        入金
               WHEN    "NYUKIN"
                   MOVE    02          TO  SPA-GMN-CUR
      *        返金
               WHEN    "HENKIN"
                   IF      SPA-NAI-NYUKINFLG   =   1
                       MOVE    01          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    02          TO  SPA-GMN-CUR
                   END-IF
      *        調整金
               WHEN    "CHOSEI"
      *            IF     SPA-NAI-HENFLG   =   1
      *                MOVE    07          TO  SPA-GMN-CUR
      *            ELSE
                       MOVE    08          TO  SPA-GMN-CUR
      *            END-IF
      *        調整金２
               WHEN    "CHOSEI2"
                   IF     SPA-NAI-HENFLG   =   1
                       MOVE    07          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    01          TO  SPA-GMN-CUR
                   END-IF
      *!           複数科継続中の時、確定へ
      *!           IF     (SPA-FUKU-DENPNUM    NOT =   ZERO )  AND
      *!                  (SPA-SYORIFLG            =   ZERO )
      *!               MOVE    09              TO  SPA-GMN-CUR
      *!           END-IF
      *        入金方法
               WHEN    "NYUKINHOU"
                   MOVE    10          TO  SPA-GMN-CUR
      *        入院の取扱い
               WHEN    "NYUKINKBN"
                   MOVE    02          TO  SPA-GMN-CUR
      *       請求発行
               WHEN    "HAKFLG"
                   MOVE    14          TO  SPA-GMN-CUR
      *       発行方法
               WHEN    "HAKHOUFLG"
      ************ MOVE    03          TO  SPA-GMN-CUR
                   MOVE    16          TO  SPA-GMN-CUR
      *       診療費明細書発行
               WHEN    "MEIPRTFLG"
                   MOVE    03          TO  SPA-GMN-CUR
      *       院外処方せん発行フラグ
               WHEN    "SYOHOPRTFLG"
                   MOVE    04          TO  SPA-GMN-CUR
      *       薬剤情報発行フラグ
               WHEN    "YAKJYOFLG"
      ****         IF      (SPA-GMN-SYOHOPRTFLG    =   ZERO )  OR
      *                    (SPA-DRCD-MAX           =   ZERO )
      *                MOVE    90                TO  SPA-GMN-CUR
      *            ELSE
      *                MOVE    5                 TO  SPA-GMN-CUR
      *****        END-IF
                   MOVE    17          TO  SPA-GMN-CUR
      *        お薬手帳報発行フラグ
               WHEN    "OKUSURIFLG"
      *************IF      (SPA-GMN-SYOHOPRTFLG    =   ZERO )  OR
      *                    (SPA-DRCD-MAX           =   ZERO )
      *                MOVE    90                TO  SPA-GMN-CUR
      *            ELSE
      *                MOVE    5                 TO  SPA-GMN-CUR
      ************ END-IF
                   MOVE    18          TO  SPA-GMN-CUR
      *        予約票報発行フラグ
               WHEN    "YYKHYOFLG"
                   IF      (SPA-GMN-SYOHOPRTFLG    =   ZERO )  OR
                           (SPA-DRCD-MAX           =   ZERO )
                       MOVE    90                TO  SPA-GMN-CUR
                   ELSE
                       MOVE    5                 TO  SPA-GMN-CUR
                   END-IF
      *       ドクター
               WHEN    "DRCD"
                   MOVE    90          TO  SPA-GMN-CUR
      *!           複数科継続中の時、確定へ
      *!           IF     (SPA-FUKU-DENPNUM    NOT =   ZERO )  AND
      *!                  (SPA-SYORIFLG            =   ZERO )
      *!               MOVE    09              TO  SPA-GMN-CUR
      *!           END-IF
      *            Ｕ・Ｐ
                   IF      SPA-USERPG-MAX      >   1
                       MOVE    15              TO  SPA-GMN-CUR
                   END-IF
      *        Ｕ・Ｐ
               WHEN    "USERPG"
                   MOVE    90          TO  SPA-GMN-CUR
      *        発行日
               WHEN    "HAKYMD"
                   MOVE    1           TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
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
      *        調整
               WHEN    "CLICKED"       ALSO    "B02"
                    PERFORM 4150-CHOSEI-SYORI-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    MOVE    ZERO                TO  SPA-GMN-SYORI
                    PERFORM 450-TOROKU-SEC
      ****     複数診療科継続
      *        WHEN    "CLICKED"       ALSO    "B09"
      *             PERFORM 460-FUKU-KEIZOKU-SEC
      *        複数診療科確定
      *        WHEN    "CLICKED"       ALSO    "B10"
      ********      PERFORM 460-FUKU-KAKUATEI-SEC
      *        複数保険表示
               WHEN    "CLICKED"       ALSO    "B05"
                    PERFORM 480-FUKU-HKNCOMBI-SEC
      *        複数科表示
               WHEN    "CLICKED"       ALSO    "B06"
                    PERFORM 490-FUKU-SRYKA-SEC
      *        入金
               WHEN    "CLICKED"       ALSO    "B08"
                    PERFORM 408-NYUKIN-SYORI-SEC
      *        返金
               WHEN    "CLICKED"       ALSO    "B09"
                    PERFORM 409-HENKIN-SYORI-SEC
      *!!
      *        一括入金・返金
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 410-IKKATU-SYORI-SEC
      *!!
      *        発行日変更
               WHEN    "CLICKED"       ALSO    "B03"
                    PERFORM 440-HAKKO-SYORI-SEC
           END-EVALUATE
          .
      *
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
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "HKNCOMBI"
                   PERFORM 4100-HKNCOMBI-SEC
               WHEN    "SRYKA"
                   PERFORM 4100-SRYKA-SEC
               WHEN    OTHER
      *            入力チェック処理
                   MOVE    ZERO                TO  FLG-TOUROKU
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       4100-HKNCOMBI-SEC          SECTION.
      *
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
      *
           MOVE    1                   TO  FLG-TOUROKU
           MOVE    1                   TO  FLG-INENTER
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41001-HKNCOMBI-CHG-SEC
           END-IF
      *
      *H30.6
           MOVE    1                   TO  FLG-ERRCHK
           .
      *
       4100-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ変更チェック処理
      *****************************************************************
       41001-HKNCOMBI-CHG-SEC          SECTION.
      *
           MOVE    11                  TO  SPA-GMN-CUR
           MOVE    K03-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-HKN-MAX
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    SPA-GMN-HKN-LIST(IDX)
                                           TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-NAI-TFTNRATEMEI(IDX)
                                           TO  SPA-GMN-FTNRATEMEI
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-HKN-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0030"              TO  SPA-ERRCD
           ELSE
               PERFORM                     3102-SYUNOU-HEN-SEC
           END-IF
           .
      *
       4100-HKNCOMBI-EXT.
           EXIT.
      *
      *R01.5
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       4100-HKNCOMBI-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-HKN-MAX
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    SPA-GMN-HKN-LIST(IDX)
                                           TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-NAI-TFTNRATEMEI(IDX)
                                           TO  SPA-GMN-FTNRATEMEI
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-HKN-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0030"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
           .
       4100-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       4100-SRYKA-SEC          SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
      *
           MOVE    1                   TO  FLG-TOUROKU
      *R01.5
           MOVE    2                   TO  FLG-INENTER
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41001-SRYKA-CHG-SEC
           END-IF
      *!!!!
      *H30.6
           MOVE    1                   TO  FLG-ERRCHK
           .
       4100-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科チェック処理
      *****************************************************************
       41001-SRYKA-CHG-SEC          SECTION.
      *
           MOVE    12                  TO  SPA-GMN-CUR
           MOVE    K03-SRYKA           TO  SPA-GMN-SRYKA-G
           MOVE    ZERO                TO  FLG-CHK
      *
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA       =   SPA-GMN-TSRYKA
                                                             (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
           ELSE
               PERFORM                 3102-SYUNOU-HEN-SEC
           END-IF
           .
       41001-SRYKA-CHG-EXT.
           EXIT.
      *
      *R01.5
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       41001-SRYKA-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL      IDX      >   SPA-GMN-SRYKA-MAX
               IF      SPA-GMN-SRYKA       =   SPA-GMN-TSRYKA
                                                             (IDX)
                   MOVE    SPA-GMN-SRYKA-LIST(IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
           .
       41001-SRYKA-CHK-EXT.
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
      *    調整金
           MOVE    K03-CHOSEI          TO  SPA-GMN-CHOSEI1-X
           MOVE    K03-CHOSEI2         TO  SPA-GMN-CHOSEI2-X
      *    入金額
           MOVE    K03-NYUKIN          TO  SPA-GMN-NYUKIN-X
      *    返金
           MOVE    K03-HENKIN          TO  SPA-GMN-HENKIN-X
      *    入金の取扱い
           MOVE    K03-NYUKINKBN       TO  SPA-GMN-NYUKINKBN-G
      *    入金方法
           MOVE    K03-NYUKINHOU       TO  SPA-GMN-NYUKINHOU-G
      *
           MOVE    K03-HAKFLG          TO  SPA-GMN-HAKFLG-G
           MOVE    K03-HAKHOUFLG       TO  SPA-GMN-HAKHOUFLG-G
           MOVE    K03-SYOHOPRTFLG     TO  SPA-GMN-SYOHOPRTFLG-G
           MOVE    K03-YAKJYOFLG       TO  SPA-GMN-YAKJYOFLG-G
           MOVE    K03-OKUSURIFLG      TO  SPA-GMN-OKUSURIFLG-G
           MOVE    K03-MEIPRTFLG       TO  SPA-GMN-MEIPRTFLG-G
           MOVE    K03-YYKHYOFLG       TO  SPA-GMN-YYKHYOFLG-G
           MOVE    K03-DRCD            TO  SPA-GMN-DRCD-G
           MOVE    K03-USERPG          TO  SPA-GMN-USERPG-G
      *    発行日
           IF      SPA-HAKYMD-FLG      =   1
               MOVE    K03-HAKYMD          TO  SPA-GMN-HAKYMD
           END-IF
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
      *HAORI
           IF      SPA-API-FLG       =   1
               PERFORM 4102-HAORI-KIHON-CHK-SEC
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *HAORI
      *
      *R01.5
      *    保険組合せ、診療科の入力エラー
           IF      FLG-INENTER     NOT =   1
               PERFORM 4100-HKNCOMBI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
           IF      FLG-INENTER     NOT =   2
               PERFORM 41001-SRYKA-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                  GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *
      *    発行日
           IF      SPA-HAKYMD-FLG      =   1
               PERFORM 410-HAKYMD-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *    調整金
           IF     (SPA-GMN-HKNCOMBI    =   ZERO )  OR
                  (SPA-GMN-SRYKA       =   ZERO )
               CONTINUE
           ELSE
               PERFORM 410-CHOSEI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4102-KIHON-CHK-EXT
               END-IF
           END-IF
      *    返金
           IF     (SPA-NAI-HENFLG       =   1    )
               IF      SPA-ERRCD        =   SPACE
                   PERFORM 450-HENKIN-CHK-SEC
               END-IF
           END-IF
      *    入金
           IF    ((SPA-GMN-HKNCOMBI    NOT =   ZERO )  AND
                  (SPA-GMN-SRYKA       NOT =   ZERO ))  OR
                  (SPA-NAI-NYUKINFLG       =   1    )
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 450-NYUKIN-CHK-SEC
               END-IF
           END-IF
      *    入金方法
           IF      SPA-ERRCD           =   SPACE
               PERFORM 450-NYUKINHOU-CHK-SEC
           END-IF
      *    入金の取扱い
           IF      SPA-ERRCD           =   SPACE
               PERFORM 450-NYUKINKBN-CHK-SEC
           END-IF
      *    請求発行
           IF      SPA-ERRCD           =   SPACE
              PERFORM 420-SKYKUKBN-CHK-SEC
           END-IF
      *    請求発行方法
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-HAKHOUFLG-CHK-SEC
           END-IF
      *    診療費明細書発行
           IF      SPA-ERRCD           =   SPACE
               PERFORM 420-MEIPRTFLG-CHK-SEC
           END-IF
      *    院外処方せん発行フラグ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 430-SYOHOPRTFLG-CHK-SEC
           END-IF
      *    薬剤情報発行フラグ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 430-YAKJYOFLG-CHK-SEC
           END-IF
      *    お薬手帳発行フラグ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 430-OKUSURIFLG-CHK-SEC
           END-IF
      *    予約票発行フラグ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 430-YYKHYOFLG-CHK-SEC
           END-IF
      *    ドクター
           IF      SPA-ERRCD           =   SPACE
               PERFORM 440-DRCD-CHK-SEC
           END-IF
      *    Ｕ・Ｐ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 440-USERPG-CHK-SEC
           END-IF
      *
      *    関連チェック
      *    変更が無い時
           IF    ((SPA-GMN-NYUKINKBN   =   SPA-MAE-NYUKINKBN )  AND
                  (SPA-GMN-NYUKIN      =   SPA-MAE-NYUKIN    )  AND
                  (SPA-GMN-HENKIN      =   SPA-MAE-HENKIN    )  AND
                  (SPA-GMN-CHOSEI1     =   SPA-MAE-CHOSEI1   )  AND
                  (SPA-GMN-CHOSEI2     =   SPA-MAE-CHOSEI2   )  AND
                  (FLG-TOUROKU         =   ZERO              ))
              OR  (SPA-ERRCD       NOT =   SPACE             )
               CONTINUE
           ELSE
      *        入金額計算計算処理
               PERFORM 31051-NYUKIN-KEISAN-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-GMN-NYUKINKBN   TO  SPA-MAE-NYUKINKBN
                   MOVE    SPA-GMN-NYUKIN      TO  SPA-MAE-NYUKIN
                   MOVE    SPA-GMN-HENKIN      TO  SPA-MAE-HENKIN
                   MOVE    SPA-GMN-CHOSEI1     TO  SPA-MAE-CHOSEI1
                   MOVE    SPA-GMN-CHOSEI2     TO  SPA-MAE-CHOSEI2
                   COMPUTE SPA-MAE-CHOSEI      =   SPA-MAE-CHOSEI1
                                               +   SPA-MAE-CHOSEI2
               ELSE
                   MOVE    "E"                 TO  SPA-MAE-NYUKINKBN
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返金  入力　処理
      *****************************************************************
       450-HENKIN-CHK-SEC                SECTION.
      *
           IF      SPA-GMN-HENKIN-X(1:1)   =   "*"
      *        返金額複写
               IF      SPA-SYORIFLG        =   ZERO
                   MOVE    SPA-GMN-CHOKAKIN    TO  SPA-GMN-HENKIN-X
               ELSE
                   COMPUTE WRK-HENKIN          =   SPA-GMN-SEIKYU
                                               *   -1
      *H30.1
      *            前回入金額以上であること
                   IF      SPA-NAI-NYUKIN      >=  WRK-HENKIN
                       MOVE    WRK-HENKIN          TO  SPA-GMN-HENKIN-X
                   ELSE
      *                前回入金額以下
                       MOVE    SPACE           TO  SPA-GMN-HENKIN-X
                       MOVE    "0114"          TO  SPA-ERRCD
                       MOVE    07              TO  SPA-GMN-CUR
                       GO      TO      450-HENKIN-CHK-EXT
                   END-IF
      *
               END-IF
           END-IF
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-HENKIN-X    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0106"                  TO  SPA-ERRCD
               MOVE    07                      TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTNUM             TO  SPA-GMN-HENKIN
               MOVE    SPA-GMN-HENKIN          TO  WRK-Z99
               MOVE    WRK-Z99                 TO  SPA-GMN-HENKIN-X
               IF      SPA-SYORIFLG            =   ZERO
      *            超過額かゼロであること
                   IF     (SPA-GMN-HENKIN      =   ZERO )
                      OR  (SPA-GMN-HENKIN      =   SPA-GMN-CHOKAKIN) 
                       IF     (SPA-GMN-HENKIN  NOT =   SPA-MAE-HENKIN )
                          AND (SPA-NAI-NYUKINFLG   =   1  )
                           PERFORM 4501-HENKIN-NYUKIN-SEC
                       END-IF
                   ELSE
                       MOVE    "0107"              TO  SPA-ERRCD
                       MOVE    07                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
      *            今回請求額であること
                   IF      SPA-GMN-SEIKYU      <   ZERO
                       IF     (SPA-GMN-HENKIN      =   ZERO )
                          OR  (SPA-GMN-HENKIN      =  (SPA-GMN-SEIKYU
                                                       * -1 ) )
                           CONTINUE
                       ELSE
                           MOVE    "0108"              TO  SPA-ERRCD
                           MOVE    07                  TO  SPA-GMN-CUR
                       END-IF
      *H30.1
      *                前回入金額以下であること
                       IF     (SPA-NAI-NYUKIN      <   SPA-GMN-HENKIN)
                          AND (SPA-ERRCD           =   SPACE         )
                           MOVE    "0114"              TO  SPA-ERRCD
                           MOVE    07                  TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       IF     (SPA-GMN-HENKIN  NOT =   ZERO )
                           MOVE    "0109"              TO  SPA-ERRCD
                           MOVE    07                  TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       450-HENKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返金から入金額設定
      *****************************************************************
       4501-HENKIN-NYUKIN-SEC                SECTION.
      *
      *    返金額＞未収額の時
           IF       SPA-GMN-HENKIN     >= (SPA-GMN-ZENMISYU
                                       +   SPA-GMN-SEIKYU  )
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
               MOVE    SPA-GMN-NYUKIN  TO  SPA-GMN-NYUKIN-X
           ELSE
               IF      (SPA-GMN-NYUKINKBN   =   "2"  OR  "3" )
                   IF      SPA-GMN-NYUKIN  =   SPA-GMN-SEIKYU
                       IF      SPA-GMN-SEIKYU  >   SPA-GMN-HENKIN
                           COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-SEIKYU
                                                   -   SPA-GMN-HENKIN
                       ELSE
                           MOVE    ZERO            TO  SPA-GMN-NYUKIN
                       END-IF
                       MOVE    SPA-GMN-NYUKIN  TO  SPA-GMN-NYUKIN-X
                   ELSE
                       IF     (SPA-GMN-NYUKIN  +   SPA-GMN-CHOKAKIN)
                                               =   SPA-GMN-SEIKYU
                           MOVE    SPA-GMN-SEIKYU  TO  SPA-GMN-NYUKIN
                           MOVE    SPA-GMN-NYUKIN  TO  SPA-GMN-NYUKIN-X
                       END-IF
                   END-IF
               ELSE
      *            未収から返金
                   IF      SPA-GMN-ZENMISYU    =   ZERO
                       IF      SPA-GMN-SEIKYU  >   SPA-GMN-HENKIN
                           COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-SEIKYU
                                                   -   SPA-GMN-HENKIN
                           MOVE    SPA-GMN-NYUKIN  TO  SPA-GMN-NYUKIN-X
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       4501-HENKIN-NYUKIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金  入力　処理
      *****************************************************************
       450-NYUKIN-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   10
               IF      SPA-GMN-NYUKIN-X (IDX:1)   =   "*"
                   MOVE    1               TO  FLG-CHK
                   MOVE    10              TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   1
               IF     (SPA-SYORIFLG        =   ZERO)  AND
                      (SPA-NAI-JYOGENKIN   >   ZERO)  AND
                      (SPA-GOK-SRYKA       =   SPA-GMN-SRYKA )  AND
                      (SPA-GOK-HKNCOMBI    =   SPA-GMN-HKNCOMBI )
                   MOVE    SPA-NAI-JYOGENKIN   TO  SPA-GMN-NYUKIN-X
               END-IF
               IF     (SPA-SYORIFLG        =   1   )  AND
                      (SPA-GMN-SEIKYU      >=  ZERO )
                   MOVE    SPA-GMN-SEIKYU      TO  SPA-GMN-NYUKIN-X
               END-IF
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-NYUKIN-X    TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0007"                  TO  SPA-ERRCD
               MOVE    1                       TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTNUM             TO  SPA-GMN-NYUKIN
               MOVE    SPA-GMN-NYUKIN          TO  WRK-Z99
               MOVE    WRK-Z99                 TO  SPA-GMN-NYUKIN-X
      *        今回入金がゼロ以上であること
               IF      SPA-GMN-NYUKIN          <   ZERO
                   MOVE    "0007"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       450-NYUKIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金の取扱い処理
      *****************************************************************
       450-NYUKINHOU-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  FLG-JYOTAI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NYUKINHOU-MAX )  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-NYUKINHOU   =   SPA-GMN-NYUKINHOU-LIST
                                                           (IDX)(1:2)
                   MOVE    SPA-GMN-NYUKINHOU-LIST (IDX)
                                           TO  SPA-GMN-NYUKINHOU-G
                   IF      SPA-NAI-NYUKIN-JYOTAIL (IDX)
                                           NOT =   SPA-NAI-NYUKIN-JYOTAI
                       MOVE    1               TO  FLG-JYOTAI
                   END-IF
                   MOVE    SPA-NAI-NYUKIN-JYOTAIL (IDX)
                                           TO  SPA-NAI-NYUKIN-JYOTAI
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK               =   ZERO
               MOVE    "0016"            TO  SPA-ERRCD
               MOVE    13                TO  SPA-GMN-CUR
           ELSE
               IF      FLG-JYOTAI        =   1
      *            入金額へ
                   IF    ((SPA-GMN-SRYKA       NOT =   ZERO ) AND
                          (SPA-GMN-HKNCOMBI    NOT =   ZERO )) OR
                         ( SPA-NAI-NYUKINFLG   =   1    )
                       IF     (SPA-NAI-FLG         =   ZERO )  AND
                              (SPA-IDX-KOUI        >   ZERO )
                          MOVE    1                 TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
      *
       450-NYUKINHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    入金の取扱い処理
      *****************************************************************
       450-NYUKINKBN-CHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-NYUKINKBN-MAX )  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-NYUKINKBN   =   SPA-GMN-NYUKINKBN-LST
                                                           (IDX)(1:1)
                   MOVE    SPA-GMN-NYUKINKBN-LST (IDX)
                                           TO  SPA-GMN-NYUKINKBN-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK               =   ZERO
               MOVE    "0012"            TO  SPA-ERRCD
               MOVE    10                TO  SPA-GMN-CUR
           END-IF
           .
      *
       450-NYUKINKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金額計算計算処理
      *****************************************************************
       31051-NYUKIN-KEISAN-SEC           SECTION.
      *
      *    入金額振替編集処理
           MOVE    SPA-GMN-SRYKA       TO  WRK-SRYKA
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
      *    収納へ編集
           PERFORM 40011-NYUKIN-SET-SEC
      *
      *    合計請求額算処理
           PERFORM 310511-GOK-SKYMONEY-SEC
      *
           IF      SPA-SYORIFLG        =   ZERO
      *        未収・返金なし
               IF    ( SPA-GMN-NYUKINKBN   =   "1"  )
      *@@
                OR   ((SPA-GMN-ZENMISYU    =   ZERO ) AND
                      (SPA-GMN-CHOKAKIN    =   ZERO ))
      *            今回分のみ
                   MOVE    ZERO                TO  SPA-NYU-ZENMISYU
                   PERFORM 31050-NYUKIN-SYOKICHK-SEC
               ELSE
      *            入金充当チェック
                   PERFORM 310512-NYUKIN-JYUTOU-SEC
      *            返金額明細振替
                   PERFORM 310515-HENKIN-SYORI-SEC
               END-IF
           ELSE
      *        訂正時
      *        返金チェック
               PERFORM 310516-HENKIN-TEISEI-SEC
           END-IF
      *    科合計の入金額を編集する
           PERFORM 310515-KAGOK-NYUKIN-SEC
      *    合計入金額算処理
           PERFORM 3103-NYUKIN-KEI-SEC
      *
           EVALUATE    WRK-MCP-WIDGET
      *        調整金入力時、合計額がゼロ以下の時、発行なしとする
               WHEN    "CHOSEI"
               WHEN    "CHOSEI2"
      *
      ***          COMPUTE WRK-SEIKYU      =   SPA-GOK-SKYMONEY
      ***                                  +   SPA-GOK-CHOSEI
                   IF      SPA-SKYPRTFLG       =   SPACE
      *                患者毎の設定なし
                       MOVE    SPA-SKYPRTFLG-SYS   TO  WRK-SKYPRTFLG
                   ELSE
      *                患者毎の設定あり
                       MOVE    SPA-SKYPRTFLG       TO  WRK-SKYPRTFLG
      *H27.8
      *                請求書不要
                       IF      SPA-SKYPRTFLG       =   "8"
                                                   OR  "9"
                                                   OR  "A"
                           MOVE    "0"             TO  WRK-SKYPRTFLG
                       END-IF
                   END-IF
      *            請求額がゼロ以下の時、請求書発行区分変更
                   IF      WRK-SKYPRTFLG       =   "2"
                                               OR  "5"
                                               OR  "7"
                       IF      SPA-GMN-SEIGOK      <=  ZERO
                           MOVE    SPA-GMN-HAKFLGLST(1)
                                               TO  SPA-GMN-HAKFLG-G
                       ELSE
                           MOVE    SPA-GMN-HAKFLGLST(3)
                                               TO  SPA-GMN-HAKFLG-G
                           IF     (SPA-SYORIFLG        =   1   )  AND
                                  (SPA-NAI-TEIKINKBN   NOT =   "1" )
                               MOVE    SPA-GMN-HAKFLGLST(2)
                                               TO  SPA-GMN-HAKFLG-G
                           END-IF
                       END-IF
                   END-IF
      *            請求額がゼロ以下の時、明細書発行区分変更
                   IF      SPA-MEIPRTFLG       =   "2"
                       IF      SPA-GMN-SEIGOK      <=  ZERO
                           MOVE    SPA-GMN-MEIPRTFLGLST(1)
                                               TO  SPA-GMN-MEIPRTFLG-G
                       ELSE
                           MOVE    SPA-GMN-MEIPRTFLGLST(2)
                                               TO  SPA-GMN-MEIPRTFLG-G
                       END-IF
                   END-IF
           END-EVALUATE
           .
       31051-NYUKIN-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    合計請求額算処理
      *****************************************************************
       310511-GOK-SKYMONEY-SEC           SECTION.
      *
           MOVE    ZERO            TO  SPA-GOK-CHOSEI
           MOVE    ZERO            TO  SPA-GOK-CHOSEI1
           MOVE    ZERO            TO  SPA-GOK-CHOSEI2
           MOVE    ZERO            TO  SPA-GOK-SKYMONEY
           MOVE    ZERO            TO  SPA-OLD-SKYMONEY
           MOVE    ZERO            TO  SPA-GOK-NYUKIN
           MOVE    ZERO            TO  SPA-NAI-HENKIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  WRK-SYUNOU-REC
               IF      WRK-SYU-SRYKA   NOT =   ZERO
                   ADD    WRK-SYU-CHOSEI       TO  SPA-GOK-CHOSEI
                   ADD    WRK-SYU-CHOSEI1      TO  SPA-GOK-CHOSEI1
                   ADD    WRK-SYU-CHOSEI2      TO  SPA-GOK-CHOSEI2
                   ADD    WRK-SYU-NYUKIN-TOTAL TO  SPA-GOK-NYUKIN
                   ADD     SPA-SYU-HENKIN (IDX)    TO  SPA-NAI-HENKIN
                   IF     WRK-SYU-DENPNUM      =   ZERO
                       ADD    WRK-SYU-SKYMONEY     TO  SPA-GOK-SKYMONEY
                   ELSE
                       ADD    SPA-TEIKEI-SKYMONEY(IDX)
                                                   TO  SPA-GOK-SKYMONEY
                       ADD    SPA-MAE-SKYMONEY(IDX)
                                                   TO  SPA-OLD-SKYMONEY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       310511-GOK-SKYMONEY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金額充当計算処理
      *****************************************************************
       310512-NYUKIN-JYUTOU-SEC           SECTION.
      *
      *    今回請求額合計
           COMPUTE WRK-SEIKYU          =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
                                       -   SPA-OLD-SKYMONEY
      *
           IF     (SPA-GOK-SRYKA       =   SPA-GMN-SRYKA )  AND
                  (SPA-GOK-HKNCOMBI    =   SPA-GMN-HKNCOMBI )
               MOVE    SPA-GMN-NYUKIN      TO  SPA-GOK-NYUKIN
               IF      SPA-NAI-HENFLG      =   1
                   MOVE    SPA-GMN-HENKIN  TO  SPA-GOK-HENKIN
               END-IF
           ELSE
               IF     (SPA-GMN-NYUKINKBN   =   "2"  OR  "3"  ) AND
                      (SPA-GOK-HENKIN      >   ZERO          ) AND
                      (SPA-GOK-HENKIN  NOT =   SPA-NAI-HENKIN) AND
                      (SPA-NAI-HENKIN      <=  WRK-SEIKYU    )
      *            返金があり、今回請求額で充当可能な場合
                   COMPUTE SPA-GOK-NYUKIN      =   SPA-GOK-NYUKIN
                                               -   SPA-GMN-NYUKIN
               END-IF
               COMPUTE SPA-GOK-NYUKIN      =   SPA-GOK-NYUKIN
                                           +   SPA-NYU-ZENMISYU
           END-IF
      *
           IF      SPA-GMN-NYUKINKBN   =   "2"  OR  "3"
               COMPUTE WRK-SEIKYU          =   WRK-SEIKYU
                                           -   SPA-GOK-HENKIN
      *        入金額−今回請求額
               IF     (WRK-SEIKYU          <   SPA-GOK-NYUKIN )
                   IF      WRK-SEIKYU      <   ZERO
                       MOVE    SPA-GOK-NYUKIN  TO  WRK-NYUKIN
                   ELSE
                       COMPUTE WRK-NYUKIN      =   SPA-GOK-NYUKIN
                                               -   WRK-SEIKYU
                   END-IF
               ELSE
      *            入金額が今回請求額以下の場合、ゼロ
                   MOVE    ZERO            TO  WRK-NYUKIN
               END-IF
      *        返金
               IF     (WRK-SEIKYU          <   ZERO )
                   COMPUTE WRK-HENKIN-GAKU =   WRK-SEIKYU
                                           *   -1
               ELSE
                   MOVE    ZERO            TO  WRK-HENKIN-GAKU
               END-IF
           ELSE
               MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
               MOVE    SPA-GOK-HENKIN      TO  WRK-HENKIN-GAKU
           END-IF
      *
      *    まとめなし
           IF     ((SPA-GMN-ZENMISYU   =   ZERO )  AND
                   (SPA-GOK-HENKIN     =   ZERO ))  OR
                  ((WRK-NYUKIN         =   ZERO )  AND
                   (SPA-GOK-HENKIN     =   ZERO ))
               MOVE    SPA-GOK-NYUKIN  TO  WRK-NYUKIN
               MOVE    ZERO            TO  SPA-NYU-ZENHENKIN
               MOVE    ZERO            TO  SPA-NYU-ZENMISYU
               GO      TO      310512-NYUKIN-JYUTOU-EXT
           END-IF
      *
      *    まとめ入金編集
           INITIALIZE                  ORCSNYUKINAREA
           MOVE    SPA-HOSPNUM         TO  LNK-SCNYUKIN-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-SCNYUKIN-PTID
           MOVE    SPA-NYUGAIKBN       TO  LNK-SCNYUKIN-NYUGAIKBN
      *    処理区分
           MOVE    "1"                 TO  LNK-SCNYUKIN-SYORIKBN
      *    更新区分
           MOVE    "1"                 TO  LNK-SCNYUKIN-KOUSHINKBN
      *    入金管理区分
           MOVE    SPA-GMN-NYUKINKBN   TO  LNK-SCNYUKIN-NYKNKNRKBN
      *    入金額
           MOVE    WRK-NYUKIN          TO  LNK-SCNYUKIN-NYUKIN-MONEY
      *    入金日
           MOVE    SPA-SYSYMD          TO  LNK-SCNYUKIN-NYUKIN-YMD
      *!
      *    返金額
           MOVE    WRK-HENKIN-GAKU     TO  LNK-SCNYUKIN-HENKIN-MONEY
      *    入金方法
           MOVE    SPA-GMN-NYUKINHOU   TO  LNK-SCNYUKIN-NYUKIN-HOHO
      *    返金のみ
           IF     (SPA-GMN-NYUKINKBN   =   "2"  OR  "3" )  AND
                  (SPA-GOK-HENKIN  NOT =   ZERO   )  AND
                  (WRK-HENKIN-GAKU     =   ZERO         )
               MOVE    "1"                 TO  LNK-SCNYUKIN-HENKINKBN
           END-IF
      *
           CALL    "ORCSNYUKIN"        USING
                                       ORCSNYUKINAREA
                                       SPA-AREA
      *
           IF      LNK-SCNYUKIN-RC NOT =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      310512-NYUKIN-JYUTOU-EXT
           END-IF
      *
           IF      SPA-GMN-NYUKINKBN   =   "2"  OR  "3"
               IF      LNK-SCNYUKIN-FURIWAKEZAN    >   ZERO
      *            今回請求有りの時、入金振り分け残額ありはエラー
                   MOVE    "0014"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           ELSE
               IF     (LNK-SCNYUKIN-HENKINZAN  >=  WRK-SEIKYU)
                   IF      LNK-SCNYUKIN-FURIWAKEZAN   >   ZERO
                       MOVE    "0014"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF     (LNK-SCNYUKIN-FURIWAKEZAN
                                       +   LNK-SCNYUKIN-HENKINZAN)
                                               >  WRK-SEIKYU
      *            今回請求額＜入金振り分け残額+返金振り分け残額はエラー
                       MOVE    "0014"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    充当額
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-NYU-ZENMISYU    =   WRK-NYUKIN
                                           -   LNK-SCNYUKIN-FURIWAKEZAN
           END-IF
      *
      *    返金振り分け残額
           MOVE    LNK-SCNYUKIN-HENKINZAN  TO  SPA-NYU-ZENHENKIN
           IF      SPA-GMN-NYUKINKBN   =   "2"  OR  "3"
               COMPUTE SPA-NYU-ZENHENKIN   =   SPA-NYU-ZENHENKIN
                                           +   SPA-GOK-HENKIN
               IF     (SPA-GOK-NYUKIN      =   ZERO  )
                 OR   (WRK-SEIKYU          <   ZERO  )
                   MOVE    ZERO                TO  WRK-NYUKIN
               ELSE
                   IF      SPA-GOK-NYUKIN      >   WRK-SEIKYU
                       MOVE    WRK-SEIKYU          TO  WRK-NYUKIN
                   ELSE
                       MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
                   END-IF
               END-IF
           ELSE
               MOVE    LNK-SCNYUKIN-FURIWAKEZAN  TO  WRK-NYUKIN
           END-IF
           .
       310512-NYUKIN-JYUTOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金額計算計算処理
      *****************************************************************
       31050-NYUKIN-SYOKICHK-SEC           SECTION.
      *
      *    返金額のクリア
           IF     (SPA-NAI-HENFLG      =   1    ) AND
                  (SPA-GMN-NYUKINKBN   =   "1"  )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-SYUNOU-MAX
                   MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
                   MOVE    ZERO            TO  SPA-SYU-HENKIN
                                                            (IDX)
                   MOVE    SPACE           TO  SPA-SYU-HENKBN
                                                            (IDX)
                   MOVE    SYUNOU-REC      TO  SPA-SYUNOU-REC (IDX)
               END-PERFORM
           END-IF
      *    入金額の振替
           IF     (SPA-NAI-NYUKINFLG   =   1 ) AND
                 ((WRK-HKNCOMBI        =   ZERO ) OR
                  (WRK-SRYKA           =   ZERO ))  AND
                 ( SPA-SYORIFLG        =   ZERO )
      *        入金合計を各明細に振り分ける
               MOVE    SPA-GMN-NYUKIN      TO  WRK-NYUKIN
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   SPA-SYUNOU-MAX
                   MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
                   IF      SYU-SRYKA       NOT =   ZERO
                       IF      SYU-DENPNUM     =   ZERO
                           COMPUTE WRK-SKYMONEY    =   SYU-SKYMONEY
                                                   +   SYU-CHOSEI
                       ELSE
                           COMPUTE WRK-SKYMONEY
                                               =  (SPA-TEIKEI-SKYMONEY
                                                                  (IDX)
                                               +   SYU-CHOSEI   )
                                               -   SPA-MAE-SKYMONEY(IDX)
                       END-IF
      *
                       IF      WRK-SKYMONEY    <=  WRK-NYUKIN
                           MOVE    WRK-SKYMONEY    TO  SYU-NYUKIN-TOTAL
                           COMPUTE WRK-NYUKIN      =   WRK-NYUKIN
                                                   -   WRK-SKYMONEY
                       ELSE
                           MOVE    WRK-NYUKIN      TO  SYU-NYUKIN-TOTAL
                           MOVE    ZERO            TO  WRK-NYUKIN
                       END-IF
                       MOVE    SYUNOU-REC      TO  SPA-SYUNOU-REC (IDX)
      *                今回入金額
                       MOVE    SYU-NYUKIN-TOTAL    TO
                                                   SPA-TEI-NYUKIN-TOTAL
                                                                  (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           COMPUTE WRK-SEIKYU          =  (SPA-GMN-SEIKYU
                                       -   SPA-GMN-HENKIN )
      *
           IF    ((SPA-GMN-HKNCOMBI    NOT =   ZERO )  AND
                  (SPA-GMN-SRYKA       NOT =   ZERO ))  OR
                  (SPA-NAI-NYUKINFLG       =   1    )
      *        今回請求額が入金額以内であること
               IF    ((WRK-SEIKYU              >=  ZERO  )  AND
                      (SPA-GMN-NYUKIN          >   WRK-SEIKYU )) OR
                     ((WRK-SEIKYU              <   ZERO  )  AND
                      (SPA-GMN-NYUKIN          >   ZERO  ))
                   IF      SPA-GMN-HENKIN      =   ZERO
                       MOVE    "0008"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "0110"              TO  SPA-ERRCD
                   END-IF
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-SYORIFLG        =   ZERO
               IF     (SPA-NAI-HENFLG  =   1    )  AND
                      (SPA-GMN-HENKIN  NOT =   ZERO)
                   MOVE    "0109"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       31050-NYUKIN-SYOKICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返金額充当計算処理
      *****************************************************************
       310515-HENKIN-SYORI-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-GOK-NYUKIN
           MOVE    ZERO                TO  SPA-NAI-HENKIN
      *    返金
           MOVE    SPA-NYU-ZENHENKIN   TO  WRK-HENKIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF     (SYU-SRYKA       NOT =   ZERO    )
                   COMPUTE WRK-SKYMONEY        =   SYU-SKYMONEY
                                               +   SYU-CHOSEI
                   IF      WRK-SKYMONEY        <=  WRK-HENKIN
                       MOVE    WRK-SKYMONEY    TO  SPA-SYU-HENKIN
                                                                (IDX)
                       MOVE    "1"             TO  SPA-SYU-HENKBN
                                                                (IDX)
                       COMPUTE WRK-HENKIN      =   WRK-HENKIN
                                               -   WRK-SKYMONEY
                       MOVE    ZERO            TO  SYU-NYUKIN-TOTAL
                   ELSE
                       MOVE    WRK-HENKIN      TO  SPA-SYU-HENKIN
                                                                (IDX)
                       MOVE    "1"             TO  SPA-SYU-HENKBN
                                                                (IDX)
                       COMPUTE WRK-SKYMONEY    =   WRK-SKYMONEY
                                               -   WRK-HENKIN
                       IF      WRK-SKYMONEY    <=  WRK-NYUKIN
                           MOVE    WRK-SKYMONEY    TO  SYU-NYUKIN-TOTAL
                           COMPUTE WRK-NYUKIN      =   WRK-NYUKIN
                                                   -   WRK-SKYMONEY
                           MOVE    WRK-SKYMONEY    TO  SYU-NYUKIN-TOTAL
                       ELSE
                           MOVE    WRK-NYUKIN      TO  SYU-NYUKIN-TOTAL
                           MOVE    ZERO            TO  WRK-NYUKIN
                       END-IF
                       MOVE    ZERO            TO  WRK-HENKIN
                   END-IF
                   ADD     SYU-NYUKIN-TOTAL    TO  SPA-GOK-NYUKIN
                   ADD     SPA-SYU-HENKIN (IDX)    TO  SPA-NAI-HENKIN
      *
                   MOVE    SYUNOU-REC          TO  SPA-SYUNOU-REC (IDX)
      *            今回入金額
                   MOVE    SYU-NYUKIN-TOTAL    TO
                                               SPA-TEI-NYUKIN-TOTAL
                                                                  (IDX)
               END-IF
           END-PERFORM
      *
      *    画面再表示
           IF     (SPA-SYUNOU-IDX      >   ZERO )
              AND (SPA-GMN-SRYKA   NOT =   ZERO )
               IF     (SPA-GOK-SRYKA       =   SPA-GMN-SRYKA )  AND
                      (SPA-GOK-HKNCOMBI    =   SPA-GMN-HKNCOMBI )
                   CONTINUE
               ELSE
                   MOVE    SPA-SYUNOU-REC (SPA-SYUNOU-IDX)
                                           TO  SYUNOU-REC
                   MOVE    SYU-NYUKIN-TOTAL    TO  SPA-GMN-NYUKIN
                   MOVE    SPA-SYU-HENKIN  (SPA-SYUNOU-IDX)
                                               TO  SPA-GMN-HENKIN
               END-IF
           END-IF
           .
       310515-HENKIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    訂正入金額チェック処理
      *****************************************************************
       310516-HENKIN-TEISEI-SEC          SECTION.
      *
           IF    ((SPA-GMN-HKNCOMBI    NOT =   ZERO )  AND
                  (SPA-GMN-SRYKA       NOT =   ZERO ))  OR
                  (SPA-NAI-NYUKINFLG       =   1    )
      *        今回請求額が入金額以内であること
               IF     (SPA-GMN-NYUKIN          >   ZERO   )  AND
                      (SPA-GMN-NYUKIN          >   SPA-GMN-SEIKYU )
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       310516-HENKIN-TEISEI-EXT.
           EXIT.
      *****************************************************************
      *    入金額振替編集処理
      *****************************************************************
       40011-NYUKIN-SET-SEC          SECTION.
      *
      *    現在の内容を保存する
           IF     (SPA-SYUNOU-IDX      >   ZERO )
               MOVE    SPA-SYUNOU-REC (SPA-SYUNOU-IDX)
                                           TO  SYUNOU-REC
               IF     (WRK-SRYKA       NOT =   ZERO )  AND
                      (WRK-HKNCOMBI    NOT =   ZERO )
                   MOVE    SPA-GMN-CHOSEI      TO  SYU-CHOSEI
                   MOVE    SPA-GMN-CHOSEI1     TO  SYU-CHOSEI1
                   MOVE    SPA-GMN-CHOSEI2     TO  SYU-CHOSEI2
               END-IF
               IF      WRK-SRYKA           =   SYU-SRYKA
                   IF      SPA-GMN-DRCD        =   SPACE
                       MOVE    SPACE           TO  SYU-DRCD-G
                   ELSE
                       MOVE    "1"             TO  SYU-DRCD-SIK
                       MOVE    SPA-GMN-DRCD    TO  SYU-DRCD
                   END-IF
               END-IF
               IF     (WRK-SRYKA       NOT =   ZERO ) AND
                      (WRK-HKNCOMBI    NOT =   ZERO )
                   MOVE    SPA-GMN-NYUKIN  TO  SYU-NYUKIN-TOTAL
               END-IF
      *
               MOVE    SYUNOU-REC          TO  SPA-SYUNOU-REC
                                                 (SPA-SYUNOU-IDX)
      *        今回入金額
               MOVE    SYU-NYUKIN-TOTAL    TO
                                           SPA-TEI-NYUKIN-TOTAL
                                                 (SPA-SYUNOU-IDX)
      *        返金
               IF     (WRK-SRYKA       NOT =   ZERO ) AND
                      (WRK-HKNCOMBI    NOT =   ZERO )
                   IF      SPA-SYORIFLG        =   ZERO
                       IF      (SPA-NAI-HENFLG      =   1    )
                           MOVE    SPA-GMN-HENKIN  TO  SPA-GOK-HENKIN
                       END-IF
                   ELSE
      *                訂正
                       MOVE    SPA-GMN-HENKIN  TO  SPA-SYU-HENKIN
                                                 (SPA-SYUNOU-IDX)
                   END-IF
               END-IF
           END-IF
      *
           .
       40011-NYUKIN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    科合計入金額集計処理
      *****************************************************************
       310515-KAGOK-NYUKIN-SEC              SECTION.
      *
      *    科合計の入金額を編集する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF      SYU-SRYKA           =   ZERO
                   MOVE    ZERO            TO  WRK-NYUKIN
                   MOVE    ZERO            TO  WRK-CHOSEI
                   MOVE    ZERO            TO  WRK-CHOSEI1
                   MOVE    ZERO            TO  WRK-CHOSEI2
                   MOVE    ZERO            TO  WRK-HENKIN
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   SPA-SYUNOU-MAX
                       MOVE    SPA-SYUNOU-REC (IDX2)
                                               TO  WRK-SYUNOU-REC
                       IF     (WRK-SYU-SRYKA   NOT =   ZERO  )  AND
                              (SYU-HKNCOMBINUM =   WRK-SYU-HKNCOMBINUM)
                           ADD     WRK-SYU-CHOSEI      TO  WRK-CHOSEI
                           ADD     WRK-SYU-CHOSEI1     TO  WRK-CHOSEI1
                           ADD     WRK-SYU-CHOSEI2     TO  WRK-CHOSEI2
                           ADD     WRK-SYU-NYUKIN-TOTAL
                                                       TO  WRK-NYUKIN
                           ADD     SPA-SYU-HENKIN(IDX2)
                                                       TO  WRK-HENKIN
                       END-IF
                   END-PERFORM
                   MOVE    WRK-NYUKIN          TO  SYU-NYUKIN-TOTAL
                   MOVE    WRK-CHOSEI          TO  SYU-CHOSEI
                   MOVE    WRK-CHOSEI1         TO  SYU-CHOSEI1
                   MOVE    WRK-CHOSEI2         TO  SYU-CHOSEI2
                   MOVE    SYUNOU-REC          TO  SPA-SYUNOU-REC (IDX)
      *            今回入金額
                   MOVE    SYU-NYUKIN-TOTAL    TO  SPA-TEI-NYUKIN-TOTAL
                                                                  (IDX)
      *            今回返金額
                   MOVE    WRK-HENKIN          TO  SPA-SYU-HENKIN
                                                                  (IDX)
               END-IF
           END-PERFORM
      *
           .
       310515-KAGOK-NYUKIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金入力　処理
      *****************************************************************
       410-CHOSEI-CHK-SEC                SECTION.
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CHOSEI1-X   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-CHOSEI1
               MOVE    SPA-GMN-CHOSEI1     TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-GMN-CHOSEI1-X
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-CHOSEI2-X   TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-SEISUU         >   7     )   OR
                  (SNUM-SYOKBN         =   "1"   )
               IF      SPA-ERRCD           =   SPACE
                   MOVE    08                  TO  SPA-GMN-CUR
               END-IF
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  SPA-GMN-CHOSEI2
               MOVE    SPA-GMN-CHOSEI2     TO  WRK-Z92
               MOVE    WRK-Z92             TO  SPA-GMN-CHOSEI2-X
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-GMN-CHOSEI1     NOT =   ZERO )  OR
                      (SPA-GMN-CHOSEI2     NOT =   ZERO )
      *            今回請求額がゼロ以上であること
                   COMPUTE WRK-SEIKYU      =   SPA-GMN-CHOSEI1
                                           +   SPA-GMN-CHOSEI2
                                           +   SPA-NAI-SEIKYU
      *H30.6
      *            ７桁数チェック
                   IF      WRK-SEIKYU          >   9999999
                       MOVE    "0035"                  TO  SPA-ERRCD
                       MOVE    06                      TO  SPA-GMN-CUR
                   END-IF
      *
                   IF     (WRK-SEIKYU          <   ZERO )
      *H29.7  請求額がなくてもマイナスはエラー
      *****           AND (SPA-NAI-SEIKYU      >   ZERO )
                       MOVE    "0004"                  TO  SPA-ERRCD
                       MOVE    06                      TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-GMN-CHOSEI      =   SPA-GMN-CHOSEI1
                                           +   SPA-GMN-CHOSEI2
               IF      (SPA-GMN-CHOSEI NOT =   SPA-MAE-CHOSEI)
      *H29.7
      *    HAORI対応
                 IF      SPA-API-FLG       =   1
      *            HAORI対応
      *            変更があった時、入金額等を再編集
                   PERFORM 3OO29-HAORICHOSEI-HENKOU-SEC
                 ELSE
      *
      *            変更があった時、入金額等を再編集
                   PERFORM 4101-CHOSEI-HENKOU-SEC
                 END-IF
               END-IF
           END-IF
           .
       410-CHOSEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    調整金から入金額等を再編集処理
      *****************************************************************
       4101-CHOSEI-HENKOU-SEC              SECTION.
      *
      *    今回請求額
           COMPUTE SPA-GMN-SEIKYU      =  (SPA-NAI-SEIKYU
                                       +   SPA-GMN-CHOSEI )
                                       -   SPA-OLD-SEIKYU
           IF      SPA-GMN-SEIKYU      <   ZERO
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
           ELSE
               MOVE    SPA-GMN-SEIKYU  TO  SPA-GMN-NYUKIN
           END-IF
      *    入金状態が未入金の時、ゼロ
           IF     (SPA-NAI-NYUKIN-JYOTAI   =   "2" ) 
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
           END-IF
      *
      *    訂正時入金可能へ
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-SEIKYU      <   ZERO
                   MOVE    1               TO  SPA-NAI-HENFLG
               ELSE
                   MOVE    ZERO            TO  SPA-NAI-HENFLG
               END-IF
           END-IF
      *!!
      *
           IF      (SPA-SYORIFLG       =   0  )
              AND  (SPA-NAI-NYKHNKKBN  =   "1"  OR  "3" )
              AND  (SPA-GMN-NYUKINKBN   NOT =   "1"  )
               IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
      *            入金上限額を入金へ
                   COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-NYUKIN
                                           +   SPA-GMN-ZENMISYU
               END-IF
           END-IF
      *
      *    返金額対応
           IF      SPA-GMN-HENKIN      >   ZERO
               IF      SPA-SYORIFLG        =   ZERO
                   IF      SPA-GMN-NYUKIN  >   SPA-GMN-HENKIN
                       COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-NYUKIN
                                               -   SPA-GMN-HENKIN
                   END-IF
               ELSE
                   IF      SPA-GMN-SEIKYU      >=  ZERO
                       MOVE    ZERO            TO  SPA-GMN-HENKIN
                       MOVE    SPA-GMN-HENKIN  TO  SPA-GMN-HENKIN-X
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-NYUKIN      TO  WRK-Z99
           MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
           .
       4101-CHOSEI-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    発行日入力　処理
      *****************************************************************
       410-HAKYMD-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-HAKYMD      =   SPACE
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-HAKYMD
           END-IF
           IF      SPA-GMN-HAKYMD(1:1) =   "*"
               MOVE    SPA-SRYYMD          TO  SPA-GMN-HAKYMD
           END-IF
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-HAKYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-HAKYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-HAKYMD
           ELSE
               MOVE    "0034"              TO  SPA-ERRCD
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
      *
           IF      (SPA-ERRCD          =   SPACE )
             AND   (SPA-NAI-HAKYMD NOT =   SPA-MAE-HAKYMD)
      *        未来日は警告表示
               IF     (SPA-NAI-HAKYMD      >   SPA-SYSYMD )
               AND    (SPA-NAI-HAKYMD  NOT =   SPA-SRYYMD )
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
               END-IF
      *        診療日＞発行日は警告
               IF     (SPA-NAI-HAKYMD      <   SPA-SRYYMD )
               AND    (SPA-NAI-HAKYMD  NOT =   SPA-SYSYMD )
                   MOVE    "K035"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
               END-IF
               MOVE    SPA-NAI-HAKYMD          TO  SPA-MAE-HAKYMD
           END-IF
           .
       410-HAKYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求発行入力　処理
      *****************************************************************
       420-SKYKUKBN-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-HAKFLG      =   SPACE
               MOVE    ZERO                TO  SPA-GMN-HAKFLG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKFLG-MAX
               IF      SPA-GMN-HAKFLG      =   SPA-GMN-HAKFLGLST(IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HAKFLGLST(IDX)  TO  SPA-GMN-HAKFLG-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-HAKFLG-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       420-SKYKUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求発行方法入力　処理
      *****************************************************************
       420-HAKHOUFLG-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKHOUFLG-MAX
               IF      SPA-GMN-HAKHOUFLG   =   SPA-GMN-HAKHOUFLG-LST
                                                               (IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-HAKHOUFLG-LST(IDX)
                                           TO  SPA-GMN-HAKHOUFLG-G
                   MOVE    SPA-HAKHOUFLG-MAX   TO  IDX
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN-CUR
           END-IF
           .
       420-HAKHOUFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    診療費明細書発行入力　処理
      *****************************************************************
       420-MEIPRTFLG-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEIPRTFLG-MAX
               IF      SPA-GMN-MEIPRTFLG   =   SPA-GMN-MEIPRTFLGLST
                                                               (IDX)
                                                                (1:1)
                   MOVE    SPA-GMN-MEIPRTFLGLST(IDX)
                                           TO  SPA-GMN-MEIPRTFLG-G
                   MOVE    SPA-MEIPRTFLG-MAX   TO  IDX
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   ZERO
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    16                  TO  SPA-GMN-CUR
           END-IF
           .
       420-MEIPRTFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方せん発行フラグ入力　処理
      *****************************************************************
       430-SYOHOPRTFLG-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-SYOHOPRTFLG   =   SPACE
               MOVE    ZERO              TO  SPA-GMN-SYOHOPRTFLG-G
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      SPA-GMN-SYOHOPRTFLG     =   SPA-GMN-SYOHOPRTLST
                                                          (IDX) (1:1)
                   MOVE    SPA-GMN-SYOHOPRTLST(IDX)
                                           TO  SPA-GMN-SYOHOPRTFLG-G
                   MOVE    1               TO  FLG-CHK
                   MOVE    3               TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       430-SYOHOPRTFLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力　処理
      *****************************************************************
       440-DRCD-CHK-SEC              SECTION.
      *
      *    IF      SPA-GMN-DRCD-G      =   SPACE
      *        MOVE    SPACE               TO  SPA-DRCD
      *        GO      TO      440-DRCD-CHK-EXT
      *    END-IF
      *
      *****MOVE    ZERO                TO  FLG-CHK
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-DRCD-MAX
      *        IF      SPA-GMN-DRCD        =   SPA-GMN-DRCDL (IDX)
      *            MOVE    SPA-GMN-DRCDLST (IDX)
      *                                    TO  SPA-GMN-DRCD-G
      *            MOVE    1               TO  FLG-CHK
      *            MOVE    SPA-DRCD-MAX    TO  IDX
      *        END-IF
      *****END-PERFORM
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD-G      TO  ORCSDR-IN-DRCD-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           CALL   "ORCSDRSET"          USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           IF      ORCSDR-ERRCD        =   ZERO
               MOVE    1                   TO  FLG-CHK
           ELSE
               MOVE    ZERO                TO  FLG-CHK
           END-IF
      *
      *
           IF      FLG-CHK             =   1
      *!       ＳＰＡ変更
      *!       MOVE    "1"                 TO  SPA-DRCD(1:1)
      *!       MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:)
               CONTINUE
           ELSE
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-DRCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報発行フラグ入力　処理
      *****************************************************************
       430-YAKJYOFLG-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-YAKJYOFLG   =   SPACE
               MOVE    ZERO                TO  SPA-GMN-YAKJYOFLG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   3   )  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-YAKJYOFLG   =   SPA-GMN-YAKJYOLST (IDX)
                                                               (1:1)
                   MOVE    SPA-GMN-YAKJYOLST (IDX)
                                           TO  SPA-GMN-YAKJYOFLG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK               =   ZERO
               MOVE    "0010"            TO  SPA-ERRCD
               MOVE    4                 TO  SPA-GMN-CUR
           END-IF
      *
           .
       430-YAKJYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳発行フラグ入力　処理
      *****************************************************************
       430-OKUSURIFLG-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-OKUSURIFLG  =   SPACE
               MOVE    ZERO                TO  SPA-GMN-OKUSURIFLG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-OKUSURIFLG-MAX)  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-OKUSURIFLG   =   SPA-GMN-OKUSURILST(IDX)
                                                               (1:1)
                   MOVE    SPA-GMN-OKUSURILST (IDX)
                                           TO  SPA-GMN-OKUSURIFLG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK               =   ZERO
               MOVE    "0018"            TO  SPA-ERRCD
               MOVE    17                TO  SPA-GMN-CUR
           END-IF
      *
           .
       430-OKUSURIFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *   予約票発行フラグ入力　処理
      *****************************************************************
       430-YYKHYOFLG-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-YYKHYOFLG   =   SPACE
               MOVE    ZERO                TO  SPA-GMN-YYKHYOFLG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-YYKHYOFLG-MAX)  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-YYKHYOFLG   =   SPA-GMN-YYKHYOFLG-LST
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-YYKHYOFLG-LST (IDX)
                                           TO  SPA-GMN-YYKHYOFLG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK               =   ZERO
               MOVE    "0019"            TO  SPA-ERRCD
               MOVE    18                TO  SPA-GMN-CUR
           END-IF
      *
           .
       430-YYKHYOFLG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    Ｕ・Ｐ入力　処理
      *****************************************************************
       440-USERPG-CHK-SEC              SECTION.
      *
           IF      SPA-GMN-USERPG      =   SPACE
            AND   (SPA-USERPG-MAX      >   ZERO  )
               MOVE    "0"                 TO  SPA-GMN-USERPG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-USERPG-MAX)  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-USERPG      =   SPA-GMN-USERPG-LIST (IDX)
                                                               (1:1)
                   MOVE    SPA-GMN-USERPG-LIST (IDX)
                                           TO  SPA-GMN-USERPG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF     (FLG-CHK             =   ZERO )
            AND   (SPA-USERPG-MAX      >   ZERO )
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-USERPG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
      *    調整金
      *    MOVE    SPA-GMN-CHOSEI      TO  SPA-K03-CHOSEI
      *    請求書作成区分
      *D   MOVE    SPA-GMN-HAKFLG      TO  SPA-K03-SKYKUKBN
      *
           MOVE    "NO"                TO  SPA-ERRMSG(1:2)
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K03"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    調整処理
      *****************************************************************
       4150-CHOSEI-SYORI-SEC           SECTION.
      *
           IF      (SPA-GMN-SRYKA      =   ZERO )  OR
                   (SPA-GMN-HKNCOMBI   =   ZERO )
               MOVE    "0033"          TO  SPA-ERRCD
               MOVE    7               TO  SPA-GMN-CUR
           ELSE
               MOVE    6               TO  SPA-GMN-CUR
           END-IF
           .
       4150-CHOSEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    次保険編集処理
      *****************************************************************
       480-FUKU-HKNCOMBI-SEC           SECTION.
      *
           MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4801-FUKU-HKNCOMBI-CHG-SEC
           END-IF
      *!!!!
      *H30.6
           MOVE    1                   TO  FLG-ERRCHK
           .
       480-FUKU-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    次保険編集処理
      *****************************************************************
       4801-FUKU-HKNCOMBI-CHG-SEC           SECTION.
      *
           IF      SPA-GMN-HKNCOMBI    =   SPACE
               MOVE    SPA-GMN-THKNCOMBINUM (SPA-HKN-MAX)
                                           TO  SPA-GMN-HKNCOMBI
               MOVE    SPA-GMN-HKNCOMBI    TO  WRK-HKNCOMBI
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-HKN-MAX ) OR
                             (FLG-CHK  =   1     )
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   IF      IDX             =   SPA-HKN-MAX
                       MOVE    SPA-GMN-HKN-LIST(1)
                                           TO  SPA-GMN-HKNCOMBI-G
                       MOVE    SPA-NAI-TFTNRATEMEI(1)
                                           TO  SPA-GMN-FTNRATEMEI
                   ELSE
                       MOVE    SPA-GMN-HKN-LIST(IDX + 1)
                                           TO  SPA-GMN-HKNCOMBI-G
                       MOVE    SPA-NAI-TFTNRATEMEI(IDX + 1)
                                           TO  SPA-GMN-FTNRATEMEI
                   END-IF
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    対象があること
           PERFORM 48011-SYUNOU-CHK-SEC
           IF      FLG-OK          =   ZERO
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING    IDX      FROM    1   BY  1
                       UNTIL     (FLG-CHK  =   1   )
                             OR  (IDX      >   SPA-GMN-SRYKA-MAX )
                   IF      SPA-GMN-SRYKA   =   SPA-GMN-TSRYKA
                                                             (IDX)
                       IF      IDX             =   SPA-GMN-SRYKA-MAX
                           MOVE    SPA-GMN-SRYKA-LIST(1)
                                               TO  SPA-GMN-SRYKA-G
                           MOVE    ZERO        TO  IDX
                       ELSE
                           MOVE    SPA-GMN-SRYKA-LIST(IDX + 1)
                                               TO  SPA-GMN-SRYKA-G
                       END-IF
      *                対象があること
                       PERFORM 48011-SYUNOU-CHK-SEC
                       IF      FLG-OK          =   1
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           PERFORM 3102-SYUNOU-HEN-SEC
           .
       4801-FUKU-HKNCOMBI-CHG-EXT.
           EXIT.
      *****************************************************************
      *    対象収納チェック処理
      *****************************************************************
       48011-SYUNOU-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2    >   SPA-SYUNOU-MAX) OR
                              (FLG-OK      =   1   )
               MOVE    SPA-SYUNOU-REC (IDX2)   TO  WRK-SYUNOU-REC
               IF      SPA-GMN-HKNCOMBI    =   "0000"
                   IF       SPA-GMN-SRYKA      =   WRK-SYU-SRYKA
                       MOVE    1               TO  FLG-OK
                   END-IF
               ELSE
                   IF     (SPA-GMN-HKNCOMBI    =   WRK-SYU-HKNCOMBINUM)
                     AND  (SPA-GMN-SRYKA       =   WRK-SYU-SRYKA )
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
           .
       48011-SYUNOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    次診療科編集処理
      *****************************************************************
       490-FUKU-SRYKA-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4901-FUKU-SRYKA-CHG-SEC
           END-IF
      *!!!!
      *H30.6
           MOVE    1                   TO  FLG-ERRCHK
           .
       490-FUKU-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    次診療科編集処理
      *****************************************************************
       4901-FUKU-SRYKA-CHG-SEC          SECTION.
      *
           IF      SPA-GMN-SRYKA    =   SPACE
               MOVE    SPA-GMN-TSRYKA (SPA-GMN-SRYKA-MAX)
                                           TO  SPA-GMN-SRYKA
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-GMN-SRYKA-MAX ) OR
                             (FLG-CHK  =   1     )
               IF      SPA-GMN-SRYKA       =   SPA-GMN-TSRYKA (IDX)
                   IF      IDX             =   SPA-GMN-SRYKA-MAX
                       MOVE    SPA-GMN-SRYKA-LIST(1)
                                           TO  SPA-GMN-SRYKA-G
                   ELSE
                       MOVE    SPA-GMN-SRYKA-LIST(IDX + 1)
                                           TO  SPA-GMN-SRYKA-G
                   END-IF
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    対象があること
           PERFORM 48011-SYUNOU-CHK-SEC
           IF      FLG-OK          =   ZERO
               PERFORM 49011-FUKU-SRYKA-CHK-SEC
           END-IF
      *
           PERFORM 3102-SYUNOU-HEN-SEC
           .
       4901-FUKU-SRYKA-CHG-EXT.
           EXIT.
      *****************************************************************
      *    対象収納チェック処理
      *****************************************************************
       49011-FUKU-SRYKA-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING    IDX      FROM    1   BY  1
                   UNTIL     (IDX      >   SPA-HKN-MAX ) OR
                             (FLG-CHK  =   1     )
               IF      SPA-GMN-HKNCOMBI    =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   IF      IDX             =   SPA-HKN-MAX
                       MOVE    SPA-GMN-HKN-LIST(1)
                                           TO  SPA-GMN-HKNCOMBI-G
                       MOVE    SPA-NAI-TFTNRATEMEI(1)
                                           TO  SPA-GMN-FTNRATEMEI
                       MOVE    ZERO        TO  IDX
                   ELSE
                       MOVE    SPA-GMN-HKN-LIST(IDX + 1)
                                           TO  SPA-GMN-HKNCOMBI-G
                       MOVE    SPA-NAI-TFTNRATEMEI(IDX + 1)
                                           TO  SPA-GMN-FTNRATEMEI
                   END-IF
                   PERFORM 48011-SYUNOU-CHK-SEC
                   IF      FLG-OK          =   1
                       MOVE    1               TO  FLG-CHK
                   ELSE
                       IF     (SPA-GMN-HKNCOMBI    =   ZERO )  AND
                              (SPA-GMN-SRYKA       =   ZERO )
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       49011-FUKU-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括返金処理
      *****************************************************************
       409-HENKIN-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   ZERO
      *        過入金なし
               IF      SPA-GMN-CHOKAKIN    =   ZERO
                   CONTINUE
               ELSE
      *
               IF      SPA-GMN-NYUKINKBN   =   "1"
                   MOVE    "0113"           TO  SPA-ERRCD
               ELSE
                   IF      SPA-NAI-HENFLG      =   1
      *                超過額を返金へ
                       MOVE    SPA-GMN-CHOKAKIN    TO  SPA-GMN-HENKIN-X
      *                基本チェックと画面処理
                       PERFORM 4102-KIHON-CHK-SEC
                   ELSE
                       IF      SPA-GMN-CHOKAKIN     >   ZERO
                           MOVE    "0112"           TO  SPA-ERRCD
                       END-IF
                   END-IF
               END-IF
      *
               END-IF
           ELSE
               PERFORM 4091-TEI-HENKIN-SYORI-SEC
           END-IF
           .
       409-HENKIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正返金処理
      *****************************************************************
       4091-TEI-HENKIN-SYORI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF      SYU-SRYKA       NOT =   ZERO
                   COMPUTE WRK-HENKIN  =   SPA-TEIKEI-SKYMONEY (IDX)
                                       +   SYU-CHOSEI
                                       -   SPA-MAE-SKYMONEY(IDX)
      *            前回入金額判定
                   IF     (SPA-MAE-SKYMONEY (IDX)
                                   +  SPA-TMAE-CHOSEI (IDX))
                                           <=  SPA-MAE-NYUKIN-TOTAL(IDX)
                       IF      WRK-HENKIN      <   ZERO
                           COMPUTE SPA-SYU-HENKIN (IDX)
                                           =   WRK-HENKIN   *  -1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    対象の保険の収納編集処理
           PERFORM 3102-SYUNOU-HEN-SEC
           .
       4091-TEI-HENKIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括入金処理
      *****************************************************************
       408-NYUKIN-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   ZERO
               IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
      *            入金上限額を入金へ
                   IF      SPA-NAI-JYOGENKIN   >=  ZERO
                       MOVE    SPA-NAI-JYOGENKIN   TO  SPA-GMN-NYUKIN-X
                   ELSE
                       MOVE    ZERO                TO  SPA-GMN-NYUKIN-X
                   END-IF
      *            基本チェックと画面処理
                   PERFORM 4102-KIHON-CHK-SEC
               ELSE
                   IF      (SPA-NAI-JYOGENKIN   >   ZERO )
                       MOVE    "0111"           TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               PERFORM 4081-TEI-NYUKIN-SYORI-SEC
           END-IF
           .
       408-NYUKIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正返金処理
      *****************************************************************
       4081-TEI-NYUKIN-SYORI-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC (IDX)    TO  SYUNOU-REC
               IF      SYU-SRYKA       NOT =   ZERO
                   COMPUTE WRK-HENKIN  =   SPA-TEIKEI-SKYMONEY (IDX)
                                       +   SYU-CHOSEI
                                       -   SPA-MAE-SKYMONEY(IDX)
      *            入金額判定
                   IF      WRK-HENKIN      >   ZERO
                       MOVE    WRK-HENKIN      TO  SYU-NYUKIN-TOTAL
                   END-IF
                   MOVE    SYUNOU-REC      TO  SPA-SYUNOU-REC (IDX) 
               END-IF
           END-PERFORM
      *    対象の保険の収納編集処理
           PERFORM 3102-SYUNOU-HEN-SEC
           .
       4081-TEI-NYUKIN-SYORI-EXT.
           EXIT.
      *
      *!!!!
      *****************************************************************
      *    返金　処理
      *****************************************************************
       410-IKKATU-SYORI-SEC             SECTION.
      *
           IF      SPA-IKKATUKBN       =   SPACE
      *        一括返金
               PERFORM 409-HENKIN-SYORI-SEC
      *        一括入金
               PERFORM 408-NYUKIN-SYORI-SEC
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1"                 TO  SPA-IKKATUKBN
               END-IF
           ELSE
      *        過入金・未収額
               IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
                   MOVE    SPACE           TO  SPA-GMN-HENKIN-X
                   MOVE    ZERO            TO  SPA-GMN-HENKIN
                   COMPUTE WRK-NYUKIN      =  (SPA-GOK-SKYMONEY
                                           +   SPA-GOK-CHOSEI )
                   IF      SPA-OLD-SKYMONEY    NOT =   ZERO
                       COMPUTE WRK-NYUKIN      =  (SPA-GOK-SKYMONEY
                                               +   SPA-GOK-CHOSEI )
                                               -   SPA-OLD-SKYMONEY
                   END-IF
                   IF      WRK-NYUKIN          >   ZERO
                       MOVE    WRK-NYUKIN          TO  SPA-GMN-NYUKIN
                       MOVE    SPA-GMN-NYUKIN      TO  SPA-GMN-NYUKIN-X
                   ELSE
                       MOVE    SPACE               TO  SPA-GMN-NYUKIN-X
                       MOVE    ZERO                TO  SPA-GMN-NYUKIN
                   END-IF
      *            基本チェックと画面処理
                   PERFORM 4102-KIHON-CHK-SEC
      *
                   MOVE    SPACE               TO  SPA-IKKATUKBN
               END-IF
           END-IF
           .
       410-IKKATU-SYORI-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *    発行日変更処理
      *****************************************************************
       440-HAKKO-SYORI-SEC             SECTION.
      *
           IF      SPA-HAKYMD-FLG      =   "1"
               MOVE    20              TO  SPA-GMN-CUR
           END-IF
           .
       440-HAKKO-SYORI-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD      NOT =   SPACE
               GO      TO      450-TOROKU-EXT
           END-IF
      *!!!!
      *H30.6
           IF      SPA-OVER-ERR        =   1
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    訂正時、受診履歴等の削除
           IF      SPA-SYORIFLG         =   1
      *        訂正時、診療行為が無かった時、確認表示へ
               IF      SPA-IDX-KOUI        =   ZERO
                   MOVE    "0101"          TO  SPA-KIDCD
                   GO      TO      450-TOROKU-EXT
               END-IF
           ELSE
      *        診療行為が無かった時、終了とする
               IF      SPA-IDX-KOUI        =   ZERO
                   PERFORM 4909-NASI-SYORI-SEC
                   GO      TO      450-TOROKU-EXT
               END-IF
           END-IF
      *    ＣＬＡＩＭ接続チェック処理
      *v4.8
      *    ginbee対応
           IF      SPA-MW              =   SPA-GINBEE
               MOVE    ZERO                TO  FLG-CLAIM
           ELSE
               PERFORM 4905-CLAIM-CHK-SEC
           END-IF
           IF      FLG-CLAIM           =   2
                GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    更新処理
           MOVE    ZERO                TO  FLG-CLAIM-OK
           PERFORM 4500-TOROKU-SYORI-SEC
           .
       450-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録・更新処理
      *****************************************************************
       4909-NASI-SYORI-SEC             SECTION.
      *
      *    初診料算定がある時は、算定履歴と診療科履歴を作成する
           MOVE    SPACE               TO  SPA-ERRMSG2
           IF      SPA-SYOSIN-YMD  NOT =   SPACE
      *        外来更新処理
               INITIALIZE                  ORCSCGAIRAIAREA
      *        初診算定日登録
               MOVE    "3"                 TO  ORCSCGAIRAI-KBN
               CALL    "ORCSCGAIRAI"       USING
                                           SPA-AREA
                                           ORCSCGAIRAIAREA
               MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           END-IF
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)  AND
                      (SPA-KEI-FLG         =   ZERO )
                   MOVE    "K001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-KEI-FLG
                END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "CHANGE"            TO  WRK-MCP-PUTTYPE
               PERFORM 4900-END-SYORI-SEC
           END-IF
      *
           .
       4909-NASI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    登録・更新処理
      *****************************************************************
       4500-TOROKU-SYORI-SEC             SECTION.
      *
      *    登録・更新処理
           IF      SPA-KIDCD           =   SPACE
               PERFORM 45001-TOROKU-KOUSIN-SEC
               IF      FLG-CLAIM-OK        =   ZERO
                   MOVE    "CHANGE"            TO  WRK-MCP-PUTTYPE
               ELSE
                   MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
               END-IF
           ELSE
      *        訂正時、受診履歴等の削除
               PERFORM 450012-TEISEI-DELETE-SEC
               MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )   OR
                  (SPA-KIDCD       NOT =   SPACE )
               MOVE    SPACE               TO  WRK-MCP-PUTTYPE
           ELSE
               IF      FLG-END         =   ZERO
      *        終了処理
                   PERFORM 4900-END-SYORI-SEC
               END-IF
           END-IF
           .
       4500-TOROKU-SYORI-EXT.
           EXIT.
      *!
      *****************************************************************
      *    複数診療科継続処理
      *****************************************************************
      *460-FUKU-KEIZOKU-SEC             SECTION.
      *
      *    請求書発行なし
      *    IF      SPA-GMN-HAKFLG  NOT =   "1"
      *        MOVE    "0024"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KEIZOKU-EXT
      *    END-IF
      *    MOVE    ZERO                TO  FLG-FUKU-FLG
      *    一般保険で、主保険単独の時のみ
      *    IF      SPA-HKNKBN          =   ZERO
      *        IF     (SPA-HKNNUM      NOT =   SPACE )  AND
      *               (SPA-KOH1HKNNUM      =   SPACE )  AND
      *               (SPA-KOH2HKNNUM      =   SPACE )  AND
      *               (SPA-KOH3HKNNUM      =   SPACE )  AND
      *               (SPA-KOH4HKNNUM      =   SPACE )
      *            MOVE    1               TO  FLG-FUKU-FLG
      *        END-IF
      *        IF     (SPA-HKNNUM      NOT =   SPACE )  AND
      *               (SPA-KOH1HKNNUM      =   "027" )  AND
      *               (SPA-KOH2HKNNUM      =   SPACE )  AND
      *               (SPA-KOH3HKNNUM      =   SPACE )  AND
      *               (SPA-KOH4HKNNUM      =   SPACE )
      *            MOVE    1               TO  FLG-FUKU-FLG
      *        END-IF
      *    END-IF
      *    保険継続分
      *    IF      SPA-CONTKBN         =   "1"  OR  "2"
      *        MOVE    ZERO            TO  FLG-FUKU-FLG
      *    END-IF
      *    複数科まとめ有無
      *    IF      FLG-FUKU-FLG        =   ZERO
      *        MOVE    "0020"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KEIZOKU-EXT
      *    END-IF
      *    訂正の時
      *    IF      SPA-SYORIFLG        =   1
      *        IF      SPA-FUKU-MATKBN     =   "2"
      *            MOVE    "0021"              TO  SPA-ERRCD
      *            MOVE    07                  TO  SPA-GMN-CUR
      *            GO      TO      460-FUKU-KEIZOKU-EXT
      *        END-IF
      *    END-IF
      *    複数科まとめ中
      *    IF      SPA-FUKU-SYORI      =   "1"
      *        MOVE    "0025"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KEIZOKU-EXT
      *    END-IF
      *
      *    入金の充当をしないこと
      *    IF     (SPA-GMN-NYUKINKBN   =   "2"  OR  "3" )  AND
      *           (SPA-GMN-SEIKYU      <   SPA-GMN-NYUKIN)
      *        MOVE    "0026"              TO  SPA-ERRCD
      *        MOVE    10                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KEIZOKU-EXT
      *    END-IF
      *   IF       SPA-GMN-NYUKINKBN   =   "4"  OR  "5"
      *        IF     (SPA-GMN-ZENMISYU    >   ZERO   ) OR
      *               (SPA-GMN-NYUKIN      >   ZERO   )
      *            MOVE    "0026"              TO  SPA-ERRCD
      *            MOVE    10                  TO  SPA-GMN-CUR
      *            GO      TO      460-FUKU-KEIZOKU-EXT
      *        END-IF
      *    END-IF
      *
      *    MOVE    1                   TO  SPA-GMN-SYORI
      *    確定　処理
      *    PERFORM 450-TOROKU-SEC
      *    .
      *460-FUKU-KEIZOKU-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    複数診療科確定処理
      *****************************************************************
      *460-FUKU-KAKUATEI-SEC             SECTION.
      *
      *    請求書発行なし
      *    IF      SPA-GMN-HAKFLG  NOT =   "1"
      *        MOVE    "0024"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KAKUATEI-EXT
      *    END-IF
      *
      *    MOVE    ZERO                TO  FLG-FUKU-FLG
      *    一般保険で、主保険単独の時のみ
      *    IF      SPA-HKNKBN          =   ZERO
      *        IF     (SPA-HKNNUM      NOT =   SPACE )  AND
      *               (SPA-KOH1HKNNUM      =   SPACE )  AND
      *               (SPA-KOH2HKNNUM      =   SPACE )  AND
      *               (SPA-KOH3HKNNUM      =   SPACE )  AND
      *               (SPA-KOH4HKNNUM      =   SPACE )
      *            MOVE    1               TO  FLG-FUKU-FLG
      *        END-IF
      *        IF     (SPA-HKNNUM      NOT =   SPACE )  AND
      *               (SPA-KOH1HKNNUM      =   "027" )  AND
      *               (SPA-KOH2HKNNUM      =   SPACE )  AND
      *               (SPA-KOH3HKNNUM      =   SPACE )  AND
      *               (SPA-KOH4HKNNUM      =   SPACE )
      *            MOVE    1               TO  FLG-FUKU-FLG
      *        END-IF
      *    END-IF
      *    保険継続分
      *    IF      SPA-CONTKBN         =   "1"  OR  "2"
      *        MOVE    ZERO            TO  FLG-FUKU-FLG
      *    END-IF
      *    複数科まとめ有無
      *    IF      FLG-FUKU-FLG        =   ZERO
      *        MOVE    "0020"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KAKUATEI-EXT
      *    END-IF
      *    複数科継続なし
      *    IF      SPA-FUKU-MATKBN     =   SPACE
      *        MOVE    "0022"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KAKUATEI-EXT
      *    END-IF
      *    複数科まとめ中
      *    IF      SPA-FUKU-SYORI      =   "1"
      *        MOVE    "0025"              TO  SPA-ERRCD
      *        MOVE    07                  TO  SPA-GMN-CUR
      *        GO      TO      460-FUKU-KAKUATEI-EXT
      *    END-IF
      *
      *    訂正の時
      *    IF      SPA-SYORIFLG        =   1
      *        MOVE    ZERO            TO  FLG-OK
      *        継続中で１件の時
      *        IF     (SPA-FUKU-MATKBN     =   "1" )  AND
      *               (SPA-FUKU-CNT        =   1   )
      *            MOVE    1               TO  FLG-OK
      *        END-IF
      *        継続中で複数件の時、基準でないこと
      *        IF     (SPA-FUKU-MATKBN     =   "1" )  AND
      *               (SPA-FUKU-CNT        >   1   )  AND
      *               (SPA-FUKU-DENPNUM    NOT =   SPA-DENPNUM)
      *            MOVE    1               TO  FLG-OK
      *        END-IF
      *        IF      FLG-OK              =   ZERO
      *            MOVE    "0023"              TO  SPA-ERRCD
      *            MOVE    07                  TO  SPA-GMN-CUR
      *            GO      TO      460-FUKU-KAKUATEI-EXT
      *        END-IF
      *    END-IF
      *
      *    MOVE    2                   TO  SPA-GMN-SYORI
      *    確定　処理
      *    PERFORM 450-TOROKU-SEC
      *    .
      *460-FUKU-KAKUATEI-EXT.
      **** EXIT.
      *!
      *****************************************************************
      *     処理終了処理
      *****************************************************************
       4900-END-SYORI-SEC          SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K03"               TO  SPA-MOTOPG
      *
           MOVE    WRK-MCP-PUTTYPE     TO  MCP-PUTTYPE
           MOVE    "K02"               TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4900-END-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録・更新処理
      *****************************************************************
       45001-TOROKU-KOUSIN-SEC          SECTION.
      *
      *    現在の内容を保存する
      *    IF     (SPA-SYUNOU-IDX      >   ZERO ) AND
      *           (SPA-GMN-SRYKA       NOT =   ZERO ) AND
      *           (SPA-GMN-HKNCOMBI    NOT =   ZERO )
      *        MOVE    SPA-SYUNOU-REC (SPA-SYUNOU-IDX)
      *                                    TO  SYUNOU-REC
      *        MOVE    SPA-GMN-CHOSEI  TO  SYU-CHOSEI
      *        MOVE    SPA-GMN-NYUKIN  TO  SYU-NYUKIN-TOTAL
      *        MOVE    "1"             TO  SYU-DRCD-SIK
      *        MOVE    SPA-GMN-DRCD    TO  SYU-DRCD
      *        MOVE    SYUNOU-REC      TO  SPA-SYUNOU-REC
      *                                        (SPA-SYUNOU-IDX)
      *****END-IF
      *ver.4.7
           MOVE    SPACE               TO  SPA-MATOME-NYUHENKIN-KBN
      *
      *    入金額振り分け処理
           IF      SPA-GMN-NYUKINKBN   =   "1"
               MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
           ELSE
      *        未入金が無い場合対象外
      *D       IF     (SPA-GOK-NYUKIN      =   ZERO )  OR
               IF    ((SPA-GMN-ZENMISYU    =   ZERO )  AND
                      (SPA-GMN-CHOKAKIN    =   ZERO ))
                  OR ((SPA-GOK-NYUKIN      =   ZERO )  AND
                      (SPA-GOK-HENKIN      =   ZERO ))
                   MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
               ELSE
                   PERFORM 450011-NYUKIN-KEISAN-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        外来更新処理
               PERFORM 450012-GAIRAI-CALL-SEC
           END-IF
      *
           MOVE    SPACE               TO  WRK-MCP-PUTTYPE
           IF      SPA-ERRCD           =   SPACE
               MOVE    FLG-CLAIM       TO  SPA-FLG-CLAIM
               MOVE    FLG-CLAIM-OK    TO  SPA-CLAIM-OK
               MOVE    1               TO  SPA-USERPG-FLG
               PERFORM 450011-KOUSIN-ATO-SEC
           ELSE
               IF      SPA-ERRCD(1:1)  =   "K"
                   MOVE    FLG-CLAIM-OK    TO  SPA-CLAIM-OK
                   MOVE    FLG-CLAIM       TO  SPA-FLG-CLAIM
                   MOVE    1               TO  SPA-USERPG-FLG
               END-IF
           END-IF
      *
           .
       45001-TOROKU-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    警告後処理
      *****************************************************************
       450011-KOUSIN-ATO-SEC           SECTION.
      *
      *    各帳票印刷
           PERFORM 45009-PRINT-SEC
      *
      *    ＣＬＡＩＭ送信
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-CLAIM       =   1     )
               PERFORM 4906-CLAIM-SOUSIN-SEC
               PERFORM 49061-API-CLAIM-SEC
           END-IF
      *!!!!
      *H28.6
      *    ginbee対応
           IF     (SPA-MW              =   SPA-GINBEE)
             AND  (SPA-ERRCD           =   SPACE     )
      *        AIP CLAIM送信処理
               PERFORM 49061-API-CLAIM-SEC
           END-IF
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    感染症送信一時退避（ユーザプログラムの後で起動）
           IF      SPA-DATAINFECTIONKBN    =   1
               INITIALIZE  WRK-INF-AREA
               MOVE    SPA-HOSPNUM         TO  WRK-INF-HOSPNUM
               MOVE    SPA-HOSPID          TO  WRK-INF-HOSPID
               MOVE    SPA-SYORIFLG        TO  WRK-INF-SYORIFLG
               MOVE    SPA-NYUGAIKBN       TO  WRK-INF-NYUGAIKBN
               MOVE    SPA-PTID            TO  WRK-INF-PTID
               MOVE    SPA-SRYYMD          TO  WRK-INF-SRYYMD
               MOVE    SPA-SRYKA           TO  WRK-INF-SRYKA
               MOVE    SPA-BEDFLG          TO  WRK-INF-BEDFLG
           END-IF
      *
      *    診療情報フォーマット一時退避（ユーザプログラムの後で起動）
           IF      SPA-NAI-CSVKBN2 NOT =   SPACE
               INITIALIZE  WRK-JMR-AREA
               MOVE    SPA-HOSPNUM         TO  WRK-JMR-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  WRK-JMR-NYUGAIKBN
               MOVE    SPA-PTID            TO  WRK-JMR-PTID
               MOVE    SPA-SRYYMD          TO  WRK-JMR-SRYYMD
               MOVE    SPA-DRCD            TO  WRK-JMR-DRCD
               MOVE    SPA-OPID            TO  WRK-JMR-OPID
           END-IF
      *
      *!!!!
      *    ユーザプログラム起動
           IF     (SPA-ERRCD           =   SPACE ) 
               MOVE    SPA-USERPG-FLG      TO  FLG-USERPG
               IF      WRK-MCP-PUTTYPE     =   SPACE
                   IF      SPA-CLAIM-OK        =   ZERO
                       MOVE    "CHANGE"            TO  WRK-MCP-PUTTYPE
                   ELSE
                       MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
                  END-IF
               END-IF
               PERFORM 4907-USERPG-SEC
           END-IF
      *
      *    日医版診療情報データ作成
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-NAI-CSVKBN2 NOT =   SPACE )
               PERFORM 4908-JMAMR-SEC
           END-IF
      *
      *    感染症送信
           IF      SPA-DATAINFECTIONKBN    =   1
               MOVE    ZERO                TO  FLG-INFEC-DLT
               PERFORM 4904-INFECTION-SEND-SEC
           END-IF
           .
       450011-KOUSIN-ATO-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来更新処理
      *****************************************************************
       450012-GAIRAI-CALL-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG2
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
      *    登録・更新
           MOVE    "1"                 TO  ORCSCGAIRAI-KBN
      *    調整金
      **** MOVE    SPA-GMN-CHOSEI      TO  ORCSCGAIRAI-CHOSEI
      *    請求書作成区分
           MOVE    SPA-GMN-HAKFLG      TO  ORCSCGAIRAI-HAKFLG
      *    今回請求額
           MOVE    SPA-KON-SKYMONEY    TO  ORCSCGAIRAI-KON-SKYMONEY
      *    入金額
           MOVE    WRK-NYUKIN          TO  ORCSCGAIRAI-NYUKIN
      *    入金方法
           MOVE    SPA-GMN-NYUKINHOU   TO  ORCSCGAIRAI-NYUKIN-HOHO
      *    発行方法
           MOVE    SPA-GMN-HAKHOUFLG   TO  ORCSCGAIRAI-HAKHOUFLG
      *    発行日
           MOVE    SPA-NAI-HAKYMD      TO  ORCSCGAIRAI-HAKYMD
      *    複数科まとめ
      **** MOVE    SPA-GMN-SYORI       TO  ORCSCGAIRAI-SYORI
      *    収納情報
           MOVE    SPA-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
      *        訂正の時、請求額の再セット
               MOVE    SPA-SYUNOU-REC(IDX) TO  SYUNOU-REC
               IF      SYU-DENPNUM     NOT =   ZERO
                   MOVE    SPA-TEIKEI-SKYMONEY(IDX)
                                           TO  SYU-SKYMONEY
                   MOVE    SYUNOU-REC      TO  SPA-SYUNOU-REC(IDX)
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX) TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
               IF      SPA-SYU-HENKBN(IDX) =   "1"
                    MOVE    SPA-SYU-HENKIN(IDX)
                                           TO  ORCSCGAIRAI-SYU-HENKIN
                                                               (IDX)
               ELSE
                   IF     (SPA-SYORIFLG        =   1)
                       COMPUTE ORCSCGAIRAI-SYU-HENKIN (IDX)
                                           =   SPA-SYU-HENKIN(IDX)
                                           *   -1
                   END-IF
               END-IF
           END-PERFORM
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *
      *    伝票番号を編集する
           IF     (SPA-ERRCD           =   SPACE )
             OR   (SPA-ERRCD(1:1)      =   "K"   )
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-SYUNOU-MAX )
                   MOVE    ORCSCGAIRAI-SYUNOU-REC (IDX)
                                       TO  SPA-SYUNOU-REC(IDX)
               END-PERFORM
           END-IF
      *
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)  AND
                      (SPA-KEI-FLG         =   ZERO )
                   MOVE    "K001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-KEI-FLG
                END-IF
          END-IF
      *H26.8
      *    外用回数エラー、戻るへ
           IF      SPA-ERRCD           =   "0052"
                                       OR  "0051"
                                       OR  "0053"
               MOVE    91              TO  SPA-GMN-CUR
           END-IF
           .
       450012-GAIRAI-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金額計算計算処理
      *****************************************************************
       450011-NYUKIN-KEISAN-SEC           SECTION.
      *
      *    今回請求額合計
           COMPUTE WRK-SEIKYU          =  (SPA-GOK-SKYMONEY
                                       +   SPA-GOK-CHOSEI )
                                       -   SPA-OLD-SKYMONEY
      *
           COMPUTE WRK-SEIKYU          =   WRK-SEIKYU
                                       -   SPA-GOK-HENKIN
      *
           IF      SPA-GMN-NYUKINKBN   =   "2"  OR  "3"
      *        入金額−今回請求額
               IF      WRK-SEIKYU          <   SPA-GOK-NYUKIN
                   IF      WRK-SEIKYU      <   ZERO
                       MOVE    SPA-GOK-NYUKIN  TO  WRK-NYUKIN
                   ELSE
                       COMPUTE WRK-NYUKIN      =   SPA-GOK-NYUKIN
                                               -   WRK-SEIKYU
                   END-IF
               ELSE
      *            入金額が今回請求額以下の場合、ゼロ
                   MOVE    ZERO            TO  WRK-NYUKIN
               END-IF
      *        返金
               IF     (WRK-SEIKYU          <   ZERO )
                   COMPUTE WRK-HENKIN-GAKU =   WRK-SEIKYU
                                           *   -1
               ELSE
                   MOVE    ZERO            TO  WRK-HENKIN-GAKU
               END-IF
           ELSE
               MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
               MOVE    SPA-GOK-HENKIN      TO  WRK-HENKIN-GAKU
           END-IF
      *    まとめなし
           IF     ((SPA-GMN-ZENMISYU   =   ZERO )  AND
                   (SPA-GOK-HENKIN     =   ZERO ))  OR
                  ((WRK-NYUKIN         =   ZERO )  AND
                   (SPA-GOK-HENKIN     =   ZERO ))
               MOVE    SPA-GOK-NYUKIN  TO  WRK-NYUKIN
               MOVE    ZERO            TO  SPA-NYU-ZENHENKIN
               GO      TO      450011-NYUKIN-KEISAN-EXT
           END-IF
      *
      *    まとめ入金編集
           INITIALIZE                  ORCSNYUKINAREA
           MOVE    SPA-HOSPNUM         TO  LNK-SCNYUKIN-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-SCNYUKIN-PTID
           MOVE    SPA-NYUGAIKBN       TO  LNK-SCNYUKIN-NYUGAIKBN
      *    処理区分
           MOVE    "1"                 TO  LNK-SCNYUKIN-SYORIKBN
      *    更新区分
           MOVE    "2"                 TO  LNK-SCNYUKIN-KOUSHINKBN
      *    入金管理区分
           MOVE    SPA-GMN-NYUKINKBN   TO  LNK-SCNYUKIN-NYKNKNRKBN
      *    入金額
           MOVE    WRK-NYUKIN          TO  LNK-SCNYUKIN-NYUKIN-MONEY
      *    入金日
           MOVE    SPA-SYSYMD          TO  LNK-SCNYUKIN-NYUKIN-YMD
      *!
      *    返金額
           MOVE    WRK-HENKIN-GAKU     TO  LNK-SCNYUKIN-HENKIN-MONEY
      *    入金方法
           MOVE    SPA-GMN-NYUKINHOU   TO  LNK-SCNYUKIN-NYUKIN-HOHO
      *    返金のみ
           IF     (SPA-GMN-NYUKINKBN   =   "2"  OR  "3" )  AND
                  (SPA-GOK-HENKIN  NOT =   ZERO   )  AND
                  (WRK-HENKIN-GAKU     =   ZERO         )
               MOVE    "1"                 TO  LNK-SCNYUKIN-HENKINKBN
           END-IF
      *
           CALL    "ORCSNYUKIN"        USING
                                       ORCSNYUKINAREA
                                       SPA-AREA
      *
           IF      LNK-SCNYUKIN-RC NOT =   ZERO
               MOVE    "0015"              TO  SPA-ERRCD
               GO      TO      450011-NYUKIN-KEISAN-EXT
           END-IF
      *    充当額
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-NYU-ZENMISYU    =   WRK-NYUKIN
                                           -   LNK-SCNYUKIN-FURIWAKEZAN
           END-IF
      *    返金振り分け残額
           MOVE    LNK-SCNYUKIN-HENKINZAN    TO  SPA-NYU-ZENHENKIN
      *
      *ver.4.7
      *    充当処理あり
           MOVE    LNK-SCNYUKIN-MATOME-NYUHENKIN-KBN
                                       TO  SPA-MATOME-NYUHENKIN-KBN
      *
           IF      SPA-GMN-NYUKINKBN   =   "2"  OR  "3"
               COMPUTE SPA-NYU-ZENHENKIN   =   SPA-NYU-ZENHENKIN
                                           +   SPA-GOK-HENKIN
               IF     (SPA-GOK-NYUKIN      =   ZERO  )
                 OR   (WRK-SEIKYU          <   ZERO  )
                   MOVE    ZERO                TO  WRK-NYUKIN
               ELSE
                   MOVE    WRK-SEIKYU          TO  WRK-NYUKIN
      *!!
                   IF      WRK-SEIKYU          >=  SPA-GOK-NYUKIN
                       MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
                   END-IF
      *!!
               END-IF
           ELSE
               MOVE    LNK-SCNYUKIN-FURIWAKEZAN    TO  WRK-NYUKIN
           END-IF
      *    合計入金額に変更があった時、振り分けへ
           IF      SPA-GOK-NYUKIN      =   WRK-NYUKIN
               GO      TO      450011-NYUKIN-KEISAN-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-SRYKA
           MOVE    ZERO                TO  WRK-HKNCOMBI
      *    返金額明細振替
           PERFORM 310515-HENKIN-SYORI-SEC
      *    科合計の入金額を編集する
           PERFORM 310515-KAGOK-NYUKIN-SEC
      *    合計入金額算処理
           PERFORM 3103-NYUKIN-KEI-SEC
           MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
      *****MOVE    SPA-GMN-NYUKIN      TO  WRK-HZN-NYUKIN
      *    MOVE    WRK-NYUKIN          TO  SPA-GMN-NYUKIN
      *    MOVE    1                   TO  SPA-NAI-NYUKINFLG
      *    入金額振替編集処理
      *    PERFORM 40011-NYUKIN-SET-SEC
      **** MOVE    WRK-HZN-NYUKIN      TO  SPA-GMN-NYUKIN
      *
           .
       450011-NYUKIN-KEISAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正時、受診履歴等の削除処理
      *****************************************************************
       450012-TEISEI-DELETE-SEC          SECTION.
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
      *    削除
           MOVE    "2"                 TO  ORCSCGAIRAI-KBN
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
      *
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           MOVE    SPACE               TO  SPA-KIDCD
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)  AND
                      (SPA-KEI-FLG         =   ZERO )
                   MOVE    "K001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-KEI-FLG
                END-IF
           END-IF
      *
      *H29.7
      *PUSH通信
           IF      SPA-ERRCD           =   SPACE
               PERFORM 600-PUSH-SYORI-SEC
           END-IF
      *
      *    感染症送信一時退避（ユーザプログラムの後で起動）
           IF      SPA-DATAINFECTIONKBN    =   1
               INITIALIZE  WRK-INF-AREA
               MOVE    SPA-HOSPNUM         TO  WRK-INF-HOSPNUM
               MOVE    SPA-HOSPID          TO  WRK-INF-HOSPID
               MOVE    SPA-SYORIFLG        TO  WRK-INF-SYORIFLG
               MOVE    SPA-NYUGAIKBN       TO  WRK-INF-NYUGAIKBN
               MOVE    SPA-PTID            TO  WRK-INF-PTID
               MOVE    SPA-SRYYMD          TO  WRK-INF-SRYYMD
               MOVE    SPA-SRYKA           TO  WRK-INF-SRYKA
               MOVE    SPA-BEDFLG          TO  WRK-INF-BEDFLG
           END-IF
      *
      *    ユーザプログラム起動
           IF      SPA-ERRCD           =   SPACE
               MOVE    3               TO  FLG-USERPG
               MOVE    3               TO  SPA-USERPG-FLG
               MOVE    "JOIN"          TO  WRK-MCP-PUTTYPE
               PERFORM 4907-USERPG-SEC
           ELSE
               IF      SPA-ERRCD(1:1)  =   "K"
                   MOVE    ZERO        TO  SPA-FLG-CLAIM
                   MOVE    3           TO  SPA-USERPG-FLG
               END-IF
           END-IF
      *
      *    感染症送信
           IF      SPA-DATAINFECTIONKBN    =   1
               MOVE    1                   TO  FLG-INFEC-DLT
               PERFORM 4904-INFECTION-SEND-SEC
           END-IF
      *
           .
       450012-TEISEI-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    各帳票印刷処理
      *****************************************************************
       45009-PRINT-SEC              SECTION.
      *
      *    帳票プログラム取得
           PERFORM 4901-PRINT-PG-SEC
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
               IF     (SPA-GMN-SYOHOPRTFLG =   ZERO)
               AND    (SPA-GMN-HAKFLG      =   ZERO)
               AND    (SPA-GMN-YAKJYOFLG   =   ZERO)
               AND    (SPA-GMN-MEIPRTFLG   =   ZERO)
               AND    (SPA-GMN-OKUSURIFLG  =   ZERO)
               AND    (SPA-GMN-YYKHYOFLG   =   ZERO)
                   CONTINUE
               ELSE
      *            クライアント印刷制御情報取得処理
                   PERFORM 4901-CLIENT-PRT-SEC
               END-IF
           END-IF
      *
      *    主たる伝票番号
           MOVE    SPA-SYUNOU-REC(1)   TO  SYUNOU-REC
           MOVE    SYU-DENPNUM         TO  WRK-DENPNUM
      *
           MOVE    ORCSCGAIRAI-JYURRK-RENNUM (1)
                                       TO  WRK-JYURRK-RENNUM
      *
      *
      *    処方箋印刷（Ａ５）
           INITIALIZE                      ORCHC19AREA
           MOVE    WRK-JYURRK-RENNUM   TO  ORCHC19-RENNUM
           MOVE    SPACE               TO  ORCHC19-KBN
           IF      SPA-GMN-SYOHOPRTFLG =   "2"
               MOVE    "3"                 TO  ORCHC19-KBN
           END-IF
           MOVE    WRK-DENPNUM         TO  ORCHC19-DENPNUM
      *
           IF      SPA-GMN-SYOHOPRTFLG =   "1"  OR  "2"
               MOVE    "01"                TO  SPA-PRT-FLG
               CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
      *
      *    請求書印刷
           INITIALIZE                      ORCHC03AREA
           MOVE    WRK-DENPNUM         TO  ORCHC03-DENPNUM
           MOVE    SPA-GMN-SEIKYU      TO  ORCHC03-SEIKYU
      **** MOVE    SPA-GMN-NYUKIN      TO  ORCHC03-NYUKIN
           MOVE    SPA-GOK-NYUKIN      TO  ORCHC03-NYUKIN
      *    消費税（再掲）
           MOVE    SPA-NAI-SKYMONEY-TAX-SAI
                                       TO  ORCHC03-SEIKYU-TAX-SAI
      *    前回未収額−前回返金額
           IF      SPA-GMN-ZENMISYU    >   SPA-GMN-CHOKAKIN
               COMPUTE ORCHC03-ZENMISYU    =   SPA-GMN-ZENMISYU
                                           -   SPA-GMN-CHOKAKIN
               MOVE    ZERO                TO  ORCHC03-HENKIN
           ELSE
               COMPUTE ORCHC03-HENKIN      =   SPA-GMN-ZENMISYU
                                           -   SPA-GMN-CHOKAKIN
               MOVE    ZERO                TO  ORCHC03-ZENMISYU
           END-IF
      *    前回未収額２
           MOVE    SPA-GMN-ZENMISYU    TO  ORCHC03-ZENMISYU-2
      *    前回過入金
           MOVE    SPA-GMN-CHOKAKIN    TO  ORCHC03-KANYUKIN
           IF      SPA-SYORIFLG        =   1
               MOVE    ZERO                TO  ORCHC03-ZENMISYU-2
               MOVE    ZERO                TO  ORCHC03-KANYUKIN
      *        入金額は返金との差額とする
               COMPUTE ORCHC03-NYUKIN  =   ORCHC03-NYUKIN
                                       -   SPA-NAI-HENKIN
           END-IF
      *
      *    発行フラグ
           IF      SPA-SYORIFLG        =   1
      *        訂正分
               MOVE    2                   TO  ORCHC03-HAKKOFLG
           ELSE
               MOVE    ZERO                TO  ORCHC03-HAKKOFLG
           END-IF
      *    発行方法
           MOVE    SPA-GMN-HAKHOUFLG   TO  ORCHC03-HAKHOUFLG
           IF      SPA-SYUNOU-MAX      =   1
      *        単独の時
               MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
           END-IF
      *    訂正で合計金額編集
           IF      SPA-SYORIFLG        =   1
               EVALUATE    SPA-GMN-HAKFLG
                   WHEN    "0"
                   WHEN    "1"
                       MOVE    ZERO            TO  ORCHC03-TEIKIN-KBN
                   WHEN    "2"
                       MOVE    1               TO  ORCHC03-TEIKIN-KBN
               END-EVALUATE
           END-IF
      *
           MOVE    ZERO                TO  WRK-HENKIN
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-SYU
           PERFORM VARYING     IDX-S   FROM    1   BY  1
                   UNTIL       IDX-S   >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX-S)   TO  SYUNOU-REC
               IF      SYU-HKNCOMBINUM     NOT =   "9999"
      *
               ADD     1               TO  IDX
               ADD     1               TO  IDX-SYU
               MOVE    SYU-DENPNUM     TO  ORCHC03-OLD-DENPNUM (IDX)
               MOVE    SYU-SRYKA       TO  ORCHC03-OLD-SRYKA   (IDX)
               MOVE    SYU-HKNCOMBINUM TO  ORCHC03-OLD-HKNCOMBI (IDX)
               MOVE    SPA-MAE-SKYMONEY(IDX)
                                       TO  ORCHC03-OLD-SKYMONEY(IDX)
               MOVE    SPA-MAE-SKYMONEY-TAX-SAI(IDX)
                                       TO  ORCHC03-OLD-SKYMONEY-SAI
                                                               (IDX)
               MOVE    SPA-TEI-NYUKIN-TOTAL (IDX)
                                       TO  ORCHC03-OLD-NYUKIN-TOTAL
                                                               (IDX)
      *        返金対応
               IF      SPA-SYORIFLG        =   1
                   IF      SPA-SYU-HENKIN(IDX) NOT =   ZERO
      *                マイナス入金
                       COMPUTE ORCHC03-OLD-NYUKIN-TOTAL (IDX)
                                       =   SPA-TEI-NYUKIN-TOTAL (IDX)
                                       -   SPA-SYU-HENKIN (IDX)
                   END-IF
               ELSE
      *            充当
                   MOVE    SPA-SYU-HENKIN (IDX)
                                       TO  ORCHC03-KON-HENKIN (IDX)
                   IF      SYU-SRYKA       NOT =   ZERO
                       ADD     SPA-SYU-HENKIN (IDX)    TO  WRK-HENKIN
                   END-IF
               END-IF
      *
               END-IF
           END-PERFORM
      *    収納単独
           IF      IDX-SYU             =   1
               MOVE    ZERO                TO  ORCHC03-HAKHOUFLG
           END-IF
      *    今回返金充当額
           IF      SPA-SYORIFLG        =   1
               MOVE    ZERO                TO  ORCHC03-ZEN-HENKIN
           ELSE
               COMPUTE ORCHC03-ZEN-HENKIN  =   SPA-GOK-HENKIN
                                           -   WRK-HENKIN
           END-IF
      *ver.4.7
      *    充当処理あり
           MOVE    SPA-MATOME-NYUHENKIN-KBN
                                       TO  ORCHC03-MATOME-NYUHENKIN-KBN
      *
           IF      SPA-GMN-HAKFLG  NOT =   ZERO
               MOVE    "02"                TO  SPA-PRT-FLG
               CALL    WRK-SEIKYU-PG       USING
                                           SPA-AREA
                                           ORCHC03AREA
           END-IF
      *
      *    薬情報出力
           IF      SPA-GMN-YAKJYOFLG   =   "1"  OR  "2"
               INITIALIZE                      ORCSCH30AREA
               MOVE    WRK-DENPNUM         TO  ORCSCH30-DENPNUM
               MOVE    SPA-GMN-YAKJYOFLG   TO  ORCSCH30-KBN
               MOVE    "03"                TO  SPA-PRT-FLG
               CALL    "ORCSCH30"          USING   SPA-AREA
                                                   ORCSCH30AREA
           END-IF
      *
      *    診療費明細書印刷
           INITIALIZE                      ORCHC04AREA
           MOVE    WRK-DENPNUM         TO  ORCHC04-DENPNUM
      *    発行フラグ
           IF      SPA-SYORIFLG        =   1
      *        訂正分
               MOVE    2                   TO  ORCHC04-HAKKOFLG
           ELSE
               MOVE    ZERO                TO  ORCHC04-HAKKOFLG
           END-IF
      *    発行方法
           MOVE    SPA-GMN-HAKHOUFLG   TO  ORCHC04-HAKHOUFLG
           IF      IDX-SYU             =   1
      *        単独の時
               MOVE    ZERO                TO  ORCHC04-HAKHOUFLG
           END-IF
           IF      SPA-GMN-MEIPRTFLG   NOT =   ZERO
               MOVE    "04"                TO  SPA-PRT-FLG
               CALL    WRK-MEISAI-PG       USING
                                           SPA-AREA
                                           ORCHC04AREA
           END-IF
      *    お薬手帳出力
           IF      SPA-GMN-OKUSURIFLG  =   "1"  OR  "2"
               INITIALIZE                      ORCSCH62AREA
               MOVE    WRK-DENPNUM         TO  ORCSCH62-DENPNUM
               MOVE    SPA-GMN-OKUSURIFLG  TO  ORCSCH62-KBN
               MOVE    "05"                TO  SPA-PRT-FLG
               MOVE    "1"                 TO  ORCSCH62-PGKBN
               CALL    "ORCSCH62"          USING   SPA-AREA
                                                   ORCSCH62AREA
           ELSE
      *        ＣＳＶのみ出力（院内分の出力）
               IF      SPA-NAI-CSVKBN      NOT =   SPACE
                   INITIALIZE                      ORCSCH62AREA
                   MOVE    WRK-DENPNUM         TO  ORCSCH62-DENPNUM
                   MOVE    "1"                 TO  ORCSCH62-KBN
                   MOVE    "1"                 TO  ORCSCH62-KBN2
                   MOVE    "05"                TO  SPA-PRT-FLG
                   MOVE    "1"                 TO  ORCSCH62-PGKBN
                   CALL    "ORCSCH62"          USING   SPA-AREA
                                                       ORCSCH62AREA
               END-IF
           END-IF
      *
      *    予約票発行
           IF     (SPA-GMN-YYKHYOFLG   =   "0"    )
           OR     (WRK-YYKHYO-PG       =   SPACE  )
               CONTINUE
           ELSE
               INITIALIZE                  ORCHC67AREA
               MOVE    SPA-SRYYMD          TO  ORCHC67-SRYYMD
               MOVE    "2"                 TO  ORCHC67-KBN
               MOVE    "06"                TO  SPA-PRT-FLG
      *ver.4.7 プレビューの為、直接印刷へ変更
               MOVE    ORCHC67AREA         TO  ONPRTDATA-DATA
               CALL    WRK-YYKHYO-PG       USING   SPA-AREA
                                                   ONPRTDATA
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           .
       45009-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       4901-PRINT-PG-SEC              SECTION.
      *
      *    請求書
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000003"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1031"              TO  SPA-ERRCD
               GO      TO      4901-PRINT-PG-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SEIKYU-PG
      *
      *    処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1030"              TO  SPA-ERRCD
               GO      TO      4901-PRINT-PG-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *    診療費明細書
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000008"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1030"              TO  SPA-ERRCD
               GO      TO      4901-PRINT-PG-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-MEISAI-PG
      *
      *    予約票
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000010"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                           ORCSPRTNMAREA
                                           SPA-AREA
                                           SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1030"              TO  SPA-ERRCD
               GO      TO      4901-PRINT-PG-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-YYKHYO-PG
      *
           .
      *
       4901-PRINT-PG-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    INITIALIZE                  KDMY01
      *    MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "請求書兼領収書"    TO  ORCSPRTCTRL-TITLE-I(02)
      *    MOVE    "お薬情報"          TO  ORCSPRTCTRL-TITLE-I(03)
      *    MOVE    "診療費明細書"      TO  ORCSPRTCTRL-TITLE-I(04)
      *    MOVE    "お薬手帳"          TO  ORCSPRTCTRL-TITLE-I(05)
      *    MOVE    "予約票"            TO  ORCSPRTCTRL-TITLE-I(06)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *****MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      *****MOVE    1                   TO  FLG-CLIENT-OK
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    感染症送信処理
      *****************************************************************
       4904-INFECTION-SEND-SEC         SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    INFECTION-FILE      TO  SHELLTBL-NAME
           MOVE    "K03"               TO  SHELLTBL-ARG1
           MOVE    WRK-INF-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    WRK-INF-HOSPID      TO  SHELLTBL-ARG3
           MOVE    WRK-INF-SYORIFLG    TO  SHELLTBL-ARG4
           IF      FLG-INFEC-DLT       =   1
               MOVE    "2"                 TO  SHELLTBL-ARG4
           END-IF
           MOVE    WRK-INF-NYUGAIKBN   TO  SHELLTBL-ARG5
           MOVE    WRK-INF-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG6
           MOVE    WRK-INF-SRYYMD      TO  SHELLTBL-ARG7
           MOVE    WRK-INF-SRYKA       TO  SHELLTBL-ARG8
           MOVE    WRK-INF-BEDFLG      TO  SHELLTBL-ARG9
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       4904-INFECTION-SEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4905-CLAIM-CHK-SEC               SECTION.
      *
           INITIALIZE                  SPA-K03X-AREA
      *
           MOVE    ZERO                TO  FLG-CLAIM
           MOVE    ZERO                TO  SPA-CLAIM-MULTIHOST
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
               MOVE    SYS-9000-CODE       TO  SPA-GMN-SCODE
               MOVE    1                   TO  SPA-K03X-GMN-SEL
      *        マルチホストフラグ
               IF      SYS-9000-MULTIHOST-FLG  =   "1"
                    MOVE    1                   TO  SPA-CLAIM-MULTIHOST
               END-IF
               IF      SYS-9000-SETUZOKU   =   ZERO
                                           OR  SPACE
                   CONTINUE
               ELSE
                   MOVE    SYS-9000-CODE       TO  SPA-K03X-CODE
                   IF      SYS-9000-SYNBUTTON (02) =   "T"
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
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K03         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK03X"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K03
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
     *
           MOVE    "K03"               TO  SPA-MOTOPG
           MOVE    "K03X"              TO  SPA-SAKIPG
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K03X"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4905-CLAIM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＬＡＩＭ接続情報取得処理
      *****************************************************************
       4906-CLAIM-SOUSIN-SEC               SECTION.
      *
           MOVE    SPACE               TO  SYS-9001-REC
           INITIALIZE                  SYS-9001-REC
           MOVE    "9001"              TO  SYS-9001-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-9001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9001-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9001-HOSPNUM
           MOVE    SYS-9001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-9001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           PERFORM
                   UNTIL       (FLG-SYSKANRI   =   1    )
               MOVE    SYSKANRI-REC        TO  SYS-9001-REC
               MOVE    ZERO                TO  FLG-SOUSIN
               IF      SPA-CLAIM-MULTIHOST =   1
      *            複数送信処理
                   MOVE    1                   TO  FLG-SOUSIN
               ELSE
                   MOVE    SYS-9001-KBNCD (1:2)  TO  WRK-GMN-SELNUM
                   IF      WRK-GMN-SELNUM        =   SPA-K03X-GMN-SEL
                       MOVE    1                   TO  FLG-SOUSIN
                       MOVE    1                   TO  FLG-SYSKANRI
                   END-IF
               END-IF
               IF      FLG-SOUSIN          =   1
                   PERFORM 49061-CLAIM-SOUSINSYOSI-SEC
               END-IF
      *
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
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
       49061-CLAIM-SOUSINSYOSI-SEC     SECTION.
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *    連番
           ADD     1                   TO  WRK-RENNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX) TO  SYUNOU-REC
               IF     (SYU-SRYKA           =   ZERO OR
                                               SPACE )  OR
                      (SYU-HKNCOMBINUM     =   SPACE OR
                                               ZERO  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
                   MOVE    SPA-PTID            TO  WRK-PTID
                   MOVE    WRK-PTID            TO  SHELLTBL-ARG2
                   MOVE    SYU-SRYKA           TO  SHELLTBL-ARG3
                   MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG4
                   MOVE    SYU-HKNCOMBINUM     TO  SHELLTBL-ARG5
                   MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG6
                   MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG7
                   MOVE    SYS-9001-PORT2      TO  SHELLTBL-ARG8
      *            ドクター
                   IF      SYU-DRCD-G          =   SPACE
                       MOVE    "@@@@@"             TO  SHELLTBL-ARG9
                   ELSE
                       MOVE    SYU-DRCD-G          TO  SHELLTBL-ARG9
                   END-IF
                   MOVE    SYU-DENPNUM         TO  SHELLTBL-ARG10
      *            連番
      *D           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
                   MOVE    SPA-SYORIFLG        TO  SHELLTBL-ARG11
                   MOVE    SHELLTBL            TO  MCPDATA-REC
                   MOVE    "SHELL"             TO  MCP-FUNC
                   MOVE    "shell"             TO  MCP-TABLE
                   MOVE    "shell"             TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
           END-PERFORM
      *
           .
       49061-CLAIM-SOUSINSYOSI-EXT.
           EXIT.
      *H28.6
      *****************************************************************
      *    API ＣＬＡＩＭ送信処理
      *****************************************************************
       49061-API-CLAIM-SEC  SECTION.
      *
           MOVE    ZERO                TO  FLG-CLAIM
      *    ＣＬＡＩＭ接続判定
           MOVE    SPACE               TO  SYS-9000-REC
           INITIALIZE                  SYS-9000-REC
           MOVE    "9000"              TO  SYS-9000-KANRICD
           MOVE    "*"                 TO  SYS-9000-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-9000-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-9000-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-9000-HOSPNUM
           MOVE    SYS-9000-REC        TO  SYSKANRI-REC
           PERFORM 9009-SYSKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-9000-REC
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
           IF      FLG-CLAIM           =   ZERO
               GO  TO  49061-API-CLAIM-EXT
           END-IF
      *
      *
      *    フィアルデイレクトリ位置指定
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    CLAIM-FILE          TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING   ORCSMKPASSAREA
      *
           MOVE    MKPASS-OT-01        TO  SHELLTBL-NAME
      *    連番
           MOVE    ZERO                TO  WRK-RENNUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX) TO  SYUNOU-REC
               IF     (SYU-SRYKA           =   ZERO OR
                                               SPACE )  OR
                      (SYU-HKNCOMBINUM     =   SPACE OR
                                               ZERO  )
                   CONTINUE
               ELSE
      *
                   MOVE    "HL05.sh"           TO  SHELLTBL-NAME
      *
                   MOVE    SPA-GMN-SCODE       TO  SHELLTBL-ARG1
                   MOVE    SPA-PTID            TO  WRK-PTID
                   MOVE    WRK-PTID            TO  SHELLTBL-ARG2
                   MOVE    SYU-SRYKA           TO  SHELLTBL-ARG3
                   MOVE    SPA-SRYYMD          TO  SHELLTBL-ARG4
                   MOVE    SYU-HKNCOMBINUM     TO  SHELLTBL-ARG5
                   MOVE    SPA-HOSPNUM         TO  SHELLTBL-ARG6
      ****         MOVE    SYS-9001-ADDRESS    TO  SHELLTBL-ARG7
      ****         MOVE    SYS-9001-PORT2      TO  SHELLTBL-ARG8
      *            ドクター
                   IF      SYU-DRCD-G          =   SPACE
                       MOVE    "@@@@@"             TO  SHELLTBL-ARG9
                   ELSE
                       MOVE    SYU-DRCD-G          TO  SHELLTBL-ARG9
                   END-IF
                   MOVE    SYU-DENPNUM         TO  SHELLTBL-ARG10
      *            連番
      *            ADD     1                   TO  WRK-RENNUM
      *D           MOVE    WRK-RENNUM          TO  SHELLTBL-ARG11 (1:2)
                   MOVE    SPA-SYORIFLG        TO  SHELLTBL-ARG11
      *            患者番号
                   MOVE    SPA-PTNUM           TO  SHELLTBL-ARG12
      *
                   MOVE    SHELLTBL            TO  MCPDATA-REC
                   MOVE    "SHELL"             TO  MCP-FUNC
                   MOVE    "shell"             TO  MCP-TABLE
                   MOVE    "allways"           TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
           END-PERFORM
      *
           .
       49061-API-CLAIM-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC  SECTION.
      *
      *v4.8
      **   ginbee対応
      *    IF      SPA-MW              =   SPA-GINBEE
      *        GO      TO      4907-USERPG-EXT
      **   END-IF
      *
           EVALUATE    SPA-USERPG-FLG
               WHEN    1
      *        追加
                   IF      SPA-SYORIFLG        =   ZERO
                       MOVE    1               TO  SPA-SYORI-FLG
                   ELSE
      *        更新
                       MOVE    2               TO  SPA-SYORI-FLG
                   END-IF
               WHEN    3
      *        削除
                   MOVE    3               TO  SPA-SYORI-FLG
           END-EVALUATE
      *
           IF      SPA-GMN-USERPG      =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "K03"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SRYYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
                   MOVE    SPA-PTNUM           TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-HKNCOMBINUM     TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPA-DRCD            TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
      *H28.6
      *            伝票番号
                   MOVE    WRK-DENPNUM         TO  ORCSUSERCHK-DENPNUM
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
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "K03"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *H28.6
      *    伝票番号
           MOVE    WRK-DENPNUM         TO  SPA-DENPNUM
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *!!!
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "K03"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K03"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-MCP-PUTTYPE     TO  MCP-PUTTYPE
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    日医版診療情報データ作成処理
      *****************************************************************
       4908-JMAMR-SEC              SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    JMAMR-FILE          TO  SHELLTBL-NAME
           MOVE    WRK-JMR-HOSPNUM     TO  SHELLTBL-ARG1
           MOVE    WRK-JMR-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG2
           MOVE    WRK-JMR-NYUGAIKBN   TO  SHELLTBL-ARG3
           MOVE    WRK-JMR-SRYYMD      TO  SHELLTBL-ARG4
           MOVE    SPA-NAI-CSVKBN2     TO  SHELLTBL-ARG5
           MOVE    WRK-JMR-DRCD        TO  SHELLTBL-ARG6
           MOVE    WRK-JMR-OPID        TO  SHELLTBL-ARG7
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "allways"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       4908-JMAMR-EXT.
           EXIT.
      *
      *HAORI
      *****************************************************************
      *    HAPRI　処理
      *****************************************************************
       300-HAORI-SYORI-SEC                SECTION.
      *
            MOVE    1                   TO  FLG-END
      *
           IF      SPA-API-FLG     NOT =   1
               MOVE    "XXXX"          TO  SPA-ERRCD
               GO      TO  300-HAORI-SYORI-EXT
           END-IF
      *
      *    送信項目保存
           EVALUATE    SPA-GMN-PAGE
           WHEN    1
      *        初期処理
               PERFORM 3001-HAORI-INI-SEC
           WHEN    2
      *        入金額等チェック処理
               PERFORM 3002-HAORI-CHK-SEC
      *2019.1
           WHEN    4
      *        複数科調整金チェック処理
               PERFORM 3004-HAORI-CHK04-SEC
           WHEN    3
      *        更新処理
               PERFORM 3003-HAORI-UPD-SEC
           END-EVALUATE
           .
       300-HAORI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期処理編集処理
      *****************************************************************
       3001-HAORI-INI-SEC         SECTION.
      *
           PERFORM 310-SPA-SET-SEC
      *
           .
       3001-HAORI-INI-EXT.
           EXIT.
      *
      *2019.1
      *****************************************************************
      *    調整金入力チェック処理（複数科・保険対応）
      *****************************************************************
       3004-HAORI-CHK04-SEC         SECTION.
      *
           INITIALIZE                  WRK-HAORI-AREA
      *    入金金額
           IF      SPA-GMN-NYUKIN-X        =   "HAORI"
               MOVE    SPA-GMN-NYUKIN      TO  WRK-HR-NYUKIN
               MOVE    1                   TO  FLG-HAORI-NYU
           END-IF
      *
           MOVE    SPA-GMN-CHOSEI1-X   TO  WRK-CHOSEI1-X
           MOVE    SPA-GMN-CHOSEI2-X   TO  WRK-CHOSEI2-X
           MOVE    SPA-GMN-CHOSEI1     TO  WRK-CHOSEI1
           MOVE    SPA-GMN-CHOSEI2     TO  WRK-CHOSEI2
      *
           PERFORM 3102-SYUNOU-HEN-SEC
      *
      *    調整金
           MOVE    WRK-CHOSEI1-X       TO  SPA-GMN-CHOSEI1-X
           MOVE    WRK-CHOSEI2-X       TO  SPA-GMN-CHOSEI2-X
      *    入金
           IF      FLG-HAORI-NYU       =   1
               MOVE    WRK-HR-NYUKIN       TO  WRK-Z99
               MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
               MOVE    WRK-HR-NYUKIN       TO  SPA-GMN-NYUKIN
           END-IF
           MOVE    1                   TO  FLG-TOUROKU
      *    調整金処理
           PERFORM 410-CHOSEI-CHK-SEC
      *    入金
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HAORI-NYU       =   1
                   PERFORM 450-NYUKIN-CHK-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        入金額計算計算処理
               PERFORM 31051-NYUKIN-KEISAN-SEC
           END-IF
      *
           .
       3004-HAORI-CHK04-EXT.
           EXIT.
      *
      *****************************************************************
      *    入金額等チェック処理
      *****************************************************************
       3002-HAORI-CHK-SEC         SECTION.
      *
           INITIALIZE                  WRK-HAORI-AREA
           IF      SPA-GMN-NYUKIN-X(1:1)   =   "*"
      *        一括入金
               MOVE    1                   TO  FLG-HAORI-IKT
           ELSE
      *        入金金額
               IF      SPA-GMN-NYUKIN-X        =   "HAORI"
                   MOVE    SPA-GMN-NYUKIN      TO  WRK-HR-NYUKIN
                   MOVE    1                   TO  FLG-HAORI-NYU
               END-IF
      *        返金金額
               IF      SPA-GMN-HENKIN-X        =   "HAORI"
                   MOVE    SPA-GMN-HENKIN      TO  WRK-HR-HENKIN
                   MOVE    1                   TO  FLG-HAORI-HEN
               END-IF
           END-IF
      *
           MOVE    SPA-GOK-SRYKA       TO  SPA-GMN-SRYKA
           MOVE    SPA-GOK-HKNCOMBI    TO  SPA-GMN-HKNCOMBI
      *
      *    収納から画面編集
           PERFORM 3102-SYUNOU-HEN-SEC
      *
      *    入力値へ編集
      **** MOVE    SPA-GMN-HENKIN      TO  SPA-GMN-HENKIN-X
      **** MOVE    SPA-GMN-NYUKIN      TO  SPA-GMN-NYUKIN-X
      *
      *    入金額
           IF      FLG-HAORI-NYU       =   1
               MOVE    WRK-HR-NYUKIN       TO  WRK-Z99
               MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
               MOVE    WRK-HR-NYUKIN       TO  SPA-GMN-NYUKIN
           END-IF
           IF     (FLG-HAORI-HEN       =   1 )
      *       訂正は一括返金のみ
             AND  (SPA-SYORIFLG        =   0 )
      *R01.9
               IF      WRK-HR-HENKIN       =   ZERO
                   MOVE    ZERO                TO  SPA-GMN-HENKIN
                   MOVE    WRK-HR-HENKIN       TO  WRK-Z99
                   MOVE    WRK-Z99             TO  SPA-GMN-HENKIN-X
               ELSE
      *            超過額を返金へ（一括返金のみ）
                   MOVE    SPA-GMN-CHOKAKIN    TO  SPA-GMN-HENKIN
                   MOVE    SPA-GMN-CHOKAKIN    TO  SPA-GMN-HENKIN-X
               END-IF
           END-IF
      *R01.9
      *    返金ありで返金送信なしは返金なし
           IF     (FLG-HAORI-HEN       =   ZERO )
              AND (FLG-HAORI-IKT       =   ZERO )
               IF      SPA-GMN-HENKIN  NOT =   ZERO
                   MOVE    1                   TO  FLG-HAORI-HEN
                   MOVE    ZERO                TO  SPA-GMN-HENKIN
                   MOVE    SPACE               TO  SPA-GMN-HENKIN-X
              END-IF
           END-IF
      *
           MOVE    1                   TO  FLG-TOUROKU
           IF     (FLG-HAORI-NYU       =   1 )
              OR ((FLG-HAORI-HEN       =   1 )  AND
                  (SPA-SYORIFLG        =   0 ))
              OR ( SPA-GMN-NYUKINKBN   NOT =   SPA-MAE-NYUKINKBN )
      *        基本チェックと別画面処理
               PERFORM 4102-HAORI-KIHON-CHK-SEC
           END-IF
      *
      *    一括入金・返金処理
           MOVE    SPACE               TO  SPA-IKKATUKBN
           IF      FLG-HAORI-IKT       =   1
               IF     (SPA-GMN-NYUKINKBN   =   "1" )
                 AND  (SPA-SYORIFLG        =   ZERO)
                   CONTINUE
               ELSE
      *            一括返金
                   PERFORM 409-HENKIN-SYORI-SEC
               END-IF
      *        一括入金
               PERFORM 408-NYUKIN-SYORI-SEC
      *
           ELSE
      *        訂正は一括返金のみ
               IF     (FLG-HAORI-HEN       =   1 )
                 AND  (SPA-SYORIFLG        =   1 )
                 AND  (WRK-HR-HENKIN   NOT =   ZERO)
      *            一括返金
                   PERFORM 409-HENKIN-SYORI-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーメッセージ編集
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3002-HAORI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    HAORI 基本チェックと別画面処理
      *****************************************************************
       4102-HAORI-KIHON-CHK-SEC          SECTION.
      *
      *    発行日（警告チェックなのでなし）
      **** PERFORM 410-HAKYMD-CHK-SEC
      *
      *入金額は全件再設定
      *    調整金
      *    IF     (SPA-GMN-HKNCOMBI    =   ZERO )  OR
      *           (SPA-GMN-SRYKA       =   ZERO )
      *        CONTINUE
      *    ELSE
      *        PERFORM 410-CHOSEI-CHK-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      4102-HAORI-KIHON-CHK-EXT
      *        END-IF
      **   END-IF
      *    返金
           IF     (SPA-NAI-HENFLG       =   1   )
             OR   (FLG-HAORI-HEN        =   1   )
               IF      SPA-ERRCD        =   SPACE
                   PERFORM 450-HENKIN-CHK-SEC
               END-IF
           END-IF
      *    入金
      *    入金があればそのまま編集
           IF      FLG-HAORI-NYU       =   1
               MOVE    WRK-HR-NYUKIN       TO  WRK-Z99
               MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
               MOVE    WRK-HR-NYUKIN       TO  SPA-GMN-NYUKIN
           END-IF
      *R01.9
           MOVE    SPA-GMN-NYUKIN      TO  WRK-Z99
           MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
      *
      *
           IF     (SPA-NAI-NYUKINFLG       =   1    )
             OR  ((SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                  (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI))
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 450-NYUKIN-CHK-SEC
               END-IF
           END-IF
      *    入金方法
           IF      SPA-ERRCD           =   SPACE
               PERFORM 450-NYUKINHOU-CHK-SEC
           END-IF
      *    入金の取扱い
           IF      SPA-ERRCD           =   SPACE
               PERFORM 450-NYUKINKBN-CHK-SEC
           END-IF
      *
      *    関連チェック
      *    変更が無い時
           IF    ((SPA-GMN-NYUKINKBN   =   SPA-MAE-NYUKINKBN )  AND
                  (SPA-GMN-NYUKIN      =   SPA-MAE-NYUKIN    )  AND
                  (SPA-GMN-HENKIN      =   SPA-MAE-HENKIN    )  AND
                  (SPA-GMN-CHOSEI1     =   SPA-MAE-CHOSEI1   )  AND
                  (SPA-GMN-CHOSEI2     =   SPA-MAE-CHOSEI2   )  AND
                  (FLG-TOUROKU         =   ZERO              ))
              OR  (SPA-ERRCD       NOT =   SPACE             )
               CONTINUE
           ELSE
      *        入金額計算計算処理
               PERFORM 31051-NYUKIN-KEISAN-SEC
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-GMN-NYUKINKBN   TO  SPA-MAE-NYUKINKBN
                   MOVE    SPA-GMN-NYUKIN      TO  SPA-MAE-NYUKIN
                   MOVE    SPA-GMN-HENKIN      TO  SPA-MAE-HENKIN
                   MOVE    SPA-GMN-CHOSEI1     TO  SPA-MAE-CHOSEI1
                   MOVE    SPA-GMN-CHOSEI2     TO  SPA-MAE-CHOSEI2
                   COMPUTE SPA-MAE-CHOSEI      =   SPA-MAE-CHOSEI1
                                               +   SPA-MAE-CHOSEI2
               ELSE
                   MOVE    "E"                 TO  SPA-MAE-NYUKINKBN
               END-IF
           END-IF
      *
           .
       4102-HAORI-KIHON-CHK-EXT.
           EXIT.
      *
      *2019.1 変更
      *****************************************************************
      *    HAORI 調整金から入金額等を再編集処理
      *****************************************************************
       3OO29-HAORICHOSEI-HENKOU-SEC         SECTION.
      *
      *    今回請求額
           COMPUTE SPA-GMN-SEIKYU      =  (SPA-NAI-SEIKYU
                                       +   SPA-GMN-CHOSEI )
                                       -   SPA-OLD-SEIKYU
      *
      *    訂正時入金可能へ
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-SEIKYU      <   ZERO
                   MOVE    1               TO  SPA-NAI-HENFLG
               ELSE
                   MOVE    ZERO            TO  SPA-NAI-HENFLG
               END-IF
           END-IF
      *
           IF      FLG-HAORI-NYU       =   1
      *        入金額送信ありはそのまま
               MOVE    SPA-GMN-NYUKIN      TO  WRK-Z99
               MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
               GO      TO      3OO29-HAORICHOSEI-HENKOU-EXT
           END-IF
      *
           IF      SPA-GMN-SEIKYU      <   ZERO
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
           ELSE
               MOVE    SPA-GMN-SEIKYU  TO  SPA-GMN-NYUKIN
           END-IF
      *    入金状態が未入金の時、ゼロ
           IF     (SPA-NAI-NYUKIN-JYOTAI   =   "2" ) 
               MOVE    ZERO            TO  SPA-GMN-NYUKIN
           END-IF
      *
      *    訂正時入金可能へ
           IF      SPA-SYORIFLG        =   1
               IF      SPA-GMN-SEIKYU      <   ZERO
                   MOVE    1               TO  SPA-NAI-HENFLG
               ELSE
                   MOVE    ZERO            TO  SPA-NAI-HENFLG
               END-IF
           END-IF
      *!!
      *
           IF      (SPA-SYORIFLG       =   0  )
              AND  (SPA-NAI-NYKHNKKBN  =   "1"  OR  "3" )
              AND  (SPA-GMN-NYUKINKBN   NOT =   "1"  )
               IF     (SPA-GMN-SRYKA       =   SPA-GOK-SRYKA ) AND
                      (SPA-GMN-HKNCOMBI    =   SPA-GOK-HKNCOMBI)
      *            入金上限額を入金へ
                   COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-NYUKIN
                                           +   SPA-GMN-ZENMISYU
               END-IF
           END-IF
      *
      *    返金額対応
           IF      SPA-GMN-HENKIN      >   ZERO
               IF      SPA-SYORIFLG        =   ZERO
                   IF      SPA-GMN-NYUKIN  >   SPA-GMN-HENKIN
                       COMPUTE SPA-GMN-NYUKIN  =   SPA-GMN-NYUKIN
                                               -   SPA-GMN-HENKIN
                   END-IF
               ELSE
                   IF      SPA-GMN-SEIKYU      >=  ZERO
                       MOVE    ZERO            TO  SPA-GMN-HENKIN
                       MOVE    SPA-GMN-HENKIN  TO  SPA-GMN-HENKIN-X
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-NYUKIN      TO  WRK-Z99
           MOVE    WRK-Z99             TO  SPA-GMN-NYUKIN-X
           .
       3OO29-HAORICHOSEI-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    HAORI更新処理
      *****************************************************************
       3003-HAORI-UPD-SEC         SECTION.
      *
           MOVE    SPACE               TO  SPA-MATOME-NYUHENKIN-KBN
      *
      *    入金額振り分け処理
           IF      SPA-GMN-NYUKINKBN   =   "1"
               MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
           ELSE
      *        未入金が無い場合対象外
               IF    ((SPA-GMN-ZENMISYU    =   ZERO )  AND
                      (SPA-GMN-CHOKAKIN    =   ZERO ))
                  OR ((SPA-GOK-NYUKIN      =   ZERO )  AND
                      (SPA-GOK-HENKIN      =   ZERO ))
                   MOVE    SPA-GOK-NYUKIN      TO  WRK-NYUKIN
               ELSE
                   PERFORM 450011-NYUKIN-KEISAN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        外来更新処理
               IF     (SPA-HKNCOMBINUM     =   "9999" )
                   PERFORM 30031-GAIRAI999-CALL-SEC
               ELSE
                    PERFORM 450012-GAIRAI-CALL-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
      *        エラーメッセージ編集
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       3003-HAORI-UPD-EXT.
           EXIT.
      *****************************************************************
      *    HAORI　保険組合わせ9999　更新処理
      *****************************************************************
       30031-GAIRAI999-CALL-SEC             SECTION.
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
      *    新規、更新
           MOVE    "1"                 TO  ORCSCGAIRAI-KBN
      *    収納情報
           MOVE    SPA-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SYUNOU-MAX
               MOVE    SPA-SYUNOU-REC(IDX) TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
           END-PERFORM
      *
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           .
       30031-GAIRAI999-CALL-EXT.
           EXIT.
      *
      *HAORI
      *H29.7
      *PUSH通信
      *****************************************************************
      *    PUSH通信処理
      *****************************************************************
       600-PUSH-SYORI-SEC             SECTION.
      *
           INITIALIZE                      PUSHMD01-REC
           MOVE    "patient_account"       TO  PUSHMD01-EVENT
           EVALUATE    SPA-SYORIFLG
           WHEN    ZERO
      *        追加
               MOVE    "add"               TO  PUSHMD01-PMODE
           WHEN    1
      *        訂正
               IF      SPA-IDX-KOUI        =   ZERO
      *            削除
                   MOVE    "delete"            TO  PUSHMD01-PMODE
               ELSE
      *            更新
                   MOVE    "modify"            TO  PUSHMD01-PMODE
               END-IF
           END-EVALUATE
      *    処理時間取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSHMD01-SYSYMD
           MOVE    WRK-HEN-TIME        TO  PUSHMD01-SYSTIME
           MOVE    SPA-PTNUM           TO  PUSHMD01-PTNUM
      *    診療日付
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSHMD01-SRYYMD
      *
           IF      SPA-IDX-KOUI        =   ZERO
      *        削除
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   15  )  OR
                                  (SPA-TEI-DENPNUM(IDX)   =   ZERO)
                   MOVE    SPA-TEI-SRYKA(IDX)  TO  PUSHMD01-SRYKA
                                                          (IDX)
                   MOVE    SPA-TEI-HKNCOMBI(IDX)
                                           TO  PUSHMD01-HKNCOMBI
                                                          (IDX)
                   MOVE    SPA-TEI-DENPNUM(IDX)
                                           TO  PUSHMD01-DENPNUM
                                                          (IDX)
               END-PERFORM
               IF      SPA-TEI-DENPNUM(1)  =   ZERO
      *            単独の時
                   MOVE    SPA-OLD-SRYKA   TO  PUSHMD01-SRYKA
                                                          (1)
                   MOVE    SPA-OLD-HKNCOMBI
                                           TO  PUSHMD01-HKNCOMBI
                                                          (1)
                   MOVE    SPA-DENPNUM     TO  PUSHMD01-DENPNUM
                                                          (1)
               END-IF
           ELSE
      *        新規・更新
               MOVE    ZERO                TO  IDX
               PERFORM VARYING     IDX-S   FROM    1   BY  1
                       UNTIL      (IDX-S   >   SPA-SYUNOU-MAX )
                              OR  (IDX     >=  15  )
                   MOVE    SPA-SYUNOU-REC(IDX-S)   TO  SYUNOU-REC
                   IF     (SYU-SRYKA           =   ZERO )
                       OR (SYU-HKNCOMBINUM     =   ZERO )
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX
                       MOVE    SYU-SRYKA   TO  PUSHMD01-SRYKA
                                                          (IDX)
                       MOVE    SYU-HKNCOMBINUM
                                           TO  PUSHMD01-HKNCOMBI
                                                          (IDX)
                       MOVE    SYU-DRCD-G  TO  PUSHMD01-DRCD
                                                          (IDX)
                       MOVE    SYU-DENPNUM
                                           TO  PUSHMD01-DENPNUM
                                                          (IDX)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    PUSHMD01-REC        TO  MCPDATA-REC
      *
           MOVE    "PUSHEVENT"         TO  MCP-FUNC
           MOVE    "push_medical01"    TO  MCP-TABLE
           MOVE    "push_medical01"    TO  MCP-PATHNAME
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
      *H30.6
      *    合計桁数オーバー
           IF     (SPA-OVER-ERR        =   1    )
             AND  (FLG-ERRCHK          =   ZERO )
               MOVE    "0036"          TO  SPA-ERRCD
               MOVE    91              TO  SPA-GMN-CUR
           END-IF
      *
           PERFORM 500-GMNHEN-SEC
      * 
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
           MOVE    "K03"               TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
                   MOVE    "調整金は整数７桁で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   STRING  "【患者情報が検索できませんでした。"
                           "患者情報テーブルを確認して下さい。】"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0003"
                   STRING  "【伝票番号が取得できませんでした。"
                           "システム管理テーブルを確認して下さい。】"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0004"
                   STRING  "今回請求額がゼロ以下になります。"
                           "調整金を再入力して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0005"
                   STRING  "【ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。】"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *VER.4.5
               WHEN    "0105"
                   STRING  "この診療科の当日の受診が９回登録済です。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *VER.4.5
               WHEN    "0006"
                   MOVE    "請求書兼領収書選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "入金額は整数で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "入金額は今回請求額以下にして下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "院外処方せん選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "薬剤情報選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "ドクター選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "入金取扱い選択エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "入金振り分けができません。"
                                       TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "入金額が入金可能金額以上です。"
                                       TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "入金振り分け更新エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "入金方法区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "発行方法区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "お薬手帳区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "予約票区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "Ｕ・Ｐがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "診療費明細書区分がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "複数科まとめのできない保険です。"
                                       TO  SPA-ERRMSG
                   MOVE    "登録を押下して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0021"
                   MOVE    "既に複数科まとめの確定済です。"
                                       TO  SPA-ERRMSG
                   MOVE    "登録を押下して下さい。"
                                       TO  SPA-ERRMSG(31:)
               WHEN    "0022"
                   MOVE    "複数科まとめの継続がありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "登録を押下して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0023"
                   MOVE    "複数科まとめ確定へ変更できません。"
                                       TO  SPA-ERRMSG
                   MOVE    "登録を押下して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "0024"
                   MOVE    "複数科まとめは請求書発行ありの時"
                                       TO  SPA-ERRMSG
                   MOVE    "選択して下さい。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0025"
                   MOVE    "複数科まとめ継続中の科です。"
                                       TO  SPA-ERRMSG
                   MOVE    "複数科選択はできません。"
                                       TO  SPA-ERRMSG(33:)
               WHEN    "0026"
                   MOVE    "複数科継続の時は、入金の充当はできません。"
                                       TO  SPA-ERRMSG
                   MOVE    "入金額か取扱いを変更して下さい。"
                                       TO  SPA-ERRMSG(43:)
               WHEN    "0030"
                   MOVE    "保険組合せがありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0031"
                   MOVE    "診療科がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0032"
                   STRING  "合計を表示中です。今回請求額以下の場合は、"
                           "各科・保険毎で入力して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0033"
                   MOVE    "調整金は入力できません。"
                                       TO  SPA-ERRMSG
      *H30.6
               WHEN    "0035"
                   STRING  "今回請求額が７桁を超えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0036"
                   STRING  "収納合計で７桁を超える項目があります。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *
               WHEN    "1030"
                   STRING  "処方せんプログラムがありません。"
                           "システム管理を確認して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1031"
                   STRING  "請求書プログラムがありません。"
                           "システム管理を確認して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "1036"
                   STRING  "受診履歴の伝票番号がゼロです。"
                           "訂正できません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *        返金追加
               WHEN    "0106"
                   MOVE    "返金額は整数で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0107"
                   MOVE    "返金額は前回までの超過額にして下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0108"
                   MOVE    "返金額が今回請求額ではありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0109"
                   MOVE    "返金額は入力できません。"
                                       TO  SPA-ERRMSG
               WHEN    "0110"
                   STRING  "入金額は今回請求額＋返金額以下に"
                           "して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0111"
                   STRING  "一括入金は合計表示で押下して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0112"
                   STRING  "一括返金は合計表示で押下して下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0113"
                   STRING  "今回請求分のみの入金です。"
                           "過入金の返金はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *H30.1
               WHEN    "0114"
                   STRING  "返金額が前回入金額以下となります。"
                           "返金はできません。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.6
               WHEN    "0034"
                   MOVE    "発行日が暦日エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "K034"
                   STRING  "警告！！"
                           "発行日＞システム日付です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K035"
                   STRING  "警告！！"
                           "発行日＜診療日付です。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *ver.4.6
      *
               WHEN    "9000"
                   MOVE    "【算定履歴の更新ができませんでした】"
                                       TO  SPA-ERRMSG
               WHEN    "0051"
                   STRING  "中間データに不整合が発生しました。"
                           "再度、診療行為から入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K001"
                   MOVE    SPA-ERRMSG2     TO  SPA-ERRMSG
      *D           MOVE    SPACE           TO  SPA-ERRMSG2
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG2
      *H26.10
               WHEN    "0052"
                   STRING  "外用薬剤の回数が１回以上になります。"
                           "同じ剤は回数をまとめて入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
      *H31.2
               WHEN    "0053"
                   STRING  "訂正の上限回数をオーバーします。"
                           "更新できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           IF     (SPA-ERRMSG2     NOT =   SPACE )  AND
                  (SPA-ERRCD       NOT =   "K001")
      *        エラー画面２
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K03"               TO  SPA-MOTOPG
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
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG2(1:80)
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K03"               TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING  "継続分も含め受診履歴を削除します。"
                           "よろしいですか？"
                                            DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
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
           MOVE    "K03"               TO  SPA-MOTOPG
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
      *    収納合計マスター読込
      *****************************************************************
       981-SYUTTL-READ-SEC         SECTION.
      *
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUTTL-REC
               MOVE    ZERO                TO  FLG-SYUTTL
           ELSE
               INITIALIZE                      SYUTTL-REC
               MOVE    1                   TO  FLG-SYUTTL
           END-IF
      *
           .
       981-SYUTTL-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
               MOVE    MCPDATA-REC         TO  SYSKANRI-REC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "tbl_syunou"       TO  MCP-TABLE
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
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       970-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCONF-REC
                   MOVE    ZERO                TO  FLG-PTCONF
               ELSE
                   INITIALIZE                  PTCONF-REC
                   MOVE    1                   TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1                   TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       970-PTCONF-READ-EXT.
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
               PERFORM 960-KANRIMST-READ-SEC
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
      *v.4.8
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
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
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
