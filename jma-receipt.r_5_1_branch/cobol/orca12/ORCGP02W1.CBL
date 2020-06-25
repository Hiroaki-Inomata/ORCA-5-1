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
       PROGRAM-ID.             ORCGP02W1.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 患者登録
      *  コンポーネント名  : 患者基本情報（Ｐ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-太田     01/04/12  保険番号マスタ確認の方法
      *  01.00.02    MCC-太田     01/04/16  組合せ作成のパラメータ変更
      *  01.00.03    MCC-太田     01/04/18  生年月日未来日チェック追加
      *  01.00.04    MCC-太田     01/05/02  性別必須入力削除
      *  01.00.05    MCC-太田     01/05/10  保険削除不能の対応/タイトル
      *  01.00.06    MCC-太田     01/05/10  日付変換後ゼロ表示変更
      *  01.00.07    MCC-太田     01/05/16  減免事由未存在時の対応
      *  01.00.08    MCC-太田     01/05/23  患者番号構成管理の追加
      *  01.00.09    MCC-太田     01/05/23  スパ領域転送追加
      *  01.00.10    MCC-太田     01/05/24  HOKENJANAME 
      *  01.00.11    MCC-多々納   01/06/13  登録後、診療行為遷移を追加
      *  01.00.12    MCC-多々納   01/07/02  日付桁数変更・年齢追加
      *  01.00.13    MCC-多々納   02/04/05  労災保険の追加
      *  01.00.14    MCC-多々納   02/05/02  半角チェックの追加
      *  01.00.15    MCC-太田     02/05/31  カナ氏名候補画面を追加
      *  02.00.00    MCC-多々納   02/06/20  画面レイアウト変更による修正
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  02/09/09  平成１４年１０月改正対応
      *                                     自賠責保険の追加
      *  02.00.03    NACL-多々納  02/10/07  船員の老人補助区分の初期表示修正
      *  02.00.04    NACL-多々納  02/10/31  船員保険補助区分追加
      *  02.00.05    NACL-多々納  02/11/06  保険・公費の確認日修正
      *  02.00.06    NACL-多々納  02/11/14  船員保険の変更初期表示修正
      *  02.00.07    NACL-多々納  02/11/21  共済下船３月追加
      *  02.00.08    NACL-多々納  03/03/24  Ｈ１５年４月改正対応追加
      *                                     公費負担額追加
      *  02.00.09    NACL-多々納  03/07/08  保険者異動日チェック追加
      *  02.00.10    NACL-多々納  03/08/28  公費負担額に’０１２’と
      *                                     ’０５１’を追加
      *  02.00.11    NACL-多々納  03/10/04  公費負担額に
      *                                     東京の特定疾患公費追加
      *  02.00.12    NACL-多々納  03/10/09  公費負担額に東京の'451'追加
      *  02.00.13    NACL-多々納  03/10/27  人名辞書使用区分追加
      *                                     自費保険の初期区分追加
      *                                     カルテ発行初期区分追加
      *  02.00.14    NACL-多々納  04/08/20  公費負担額に
      *                                     兵庫の１８６公費追加
      *  02.00.15    NACL-多々納  05/03/23  平成１７年４月改正対応
      *                                     （公費負担額に'025'追加）
      *  02.00.16    NACL-多々納  05/10/07  ユーザプログラム起動追加
      *  02.00.17    NACL-多々納  05/10/19  特別療養費追加
      *  02.00.18    NACL-多々納  05/10/20  公費削除修正
      *  02.00.19    NACL-多々納  05/11/14  MONFUNC 対応 他
      *  02.00.20    NACL-多々納  06/05/18  請求書発行区分追加
      *  02.00.21    NACL-多々納  06/05/19  退職国保・老人公費チェック追加
      *  03.00.01    NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      *  03.03.00    NACL-多々納  06/10/17  減免割合・金額追加
      *  03.04.00    NACL-多々納  06/11/14  高額委任払い追加
      *  03.04.01    NACL-多々納  07/02/16  ＱＲ受付対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/06/12  保険複写機能追加
      *  03.05.01    NACL-多々納  07/08/28  公費期限切れ区分追加
      *  04.01.00    NACL-多々納  07/11/XX  公害保険対応
      *  04.00.00    NACL-多々納  08/01/25  公費負担額に結核入所'011'追加
      *  04.02.00    NACL-多々納  08/03/XX  平成２０年４月改正対応
      *  04.03.00    NACL-多々納  08/10/03  公費負担額に北海道の'183'追加
      *  04.04.00    NACL-多々納  08/12/03  住所から郵便番号設定追加
      *  04.05.00    NACL-多々納  09/08/11  退職国保年齢チェック追加
      *                                     国保等７０歳到達警告追加
      *  04.05.00    NACL-多々納  09/09/08  公費負担額に愛知県の'191'追加
      *  04.05.00    NACL-多々納  09/11/20  郵便番号自動記載区分追加
      *                                     都道府県名記載なし対応
      * 04.06.00     NACL-多々納  10/06/30  本人家族区分、補助区分変更対応
      * 04.06.00     NACL-多々納  11/02/10  京都府通り名称編集対応
      * 04.07.00     NACL-多々納  11/06/15  保険・公費毎頁変更対応
      * 04.07.00     NACL-多々納  11/07/01  公費一覧表示対応
      * 04.07.00     NACL-多々納  11/08/XX  保険確認年月日履歴対応
      * 04.07.00     NACL-多々納  11/09/06  住所等半角全角変換へ
      * 04.07.00     NACL-多々納  11/11/07  地方公費負担額変更対応
      * 04.07.00     NACL-多々納  13/07/XX  第三者行為対応など
      * 04.07.00     NACL-多々納  13/11/01  API保険・公費追加対応
      * 04.07.00     NACL-多々納  13/11/13  生活保護重複警告対応
      * 04.07.00     NACL-多々納  14/01/20  出生時体重追加対応
      * 04.07.00     NACL-多々納  14/03/XX  前期期高齢者２割対応
      * 04.07.00     NACL-多々納  14/08/08  高齢非該当２割対応
      * 04.07.00     NACL-多々納  14/12/XX  難病等対応
      * 04.08.00     NACL-多々納  15/06/16  診療科・保険組合せ件数対応
      * 04.08.00     NACL-多々納  15/11/XX  APIV3(HAORI)対応
      * 04.08.00     NACL-多々納  16/05/31  原爆一般対応
      * 05.00.00     NACL-多々納  19/06/XX  難病等＋生保保険組合せ作成対応
      * 05.00.00     NACL-多々納  20/04/XX  番号枝番追加対応
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
      *    画面非表示
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      **    患者画面スパ領域
           COPY    "P02SCR-SPA".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-LNKPARA         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-HKNJAINF        PIC 9(01).
           03  FLG-ADRS            PIC 9(01).
      *
      *    入力の有無
           03  FLG-IN-KANANAME        PIC 9(01).
           03  FLG-IN-NAME            PIC 9(01).
           03  FLG-IN-SEX             PIC 9(01).
           03  FLG-IN-BIRTHDAY        PIC 9(01).
           03  FLG-IN-POST            PIC 9(01).
           03  FLG-IN-DISCOUNTKBN     PIC 9(01).
      *    二重登録疑い用
           03  FLG-P02A-SYORI         PIC 9(01).
           03  FLG-CHK                PIC 9(01).
           03  FLG-ERR                PIC 9(01).
           03  FLG-OK                 PIC 9(01).
      *    登録フラグ
           03  FLG-TOUROKU            PIC 9(01).
      *    登録フラグ
           03  FLG-TOUROKU2           PIC 9(01).
      *
      *    保険入力
           03  FLG-IN-HKN             PIC 9(01).
      *    保険削除
           03  FLG-HKN-DEL            PIC 9(01).
      *    公費削除
           03  FLG-KOH-DEL            PIC 9(01).
      *    公費追加
           03  FLG-KOH-ADD            PIC 9(01).
      *    公費追加
           03  FLG-KOH-ADD2           PIC 9(01).
      *
      *----(01.00.01) LINE ADD START ---------------------------------
           03  FLG-RJN             PIC 9(01).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  FLG-JINMEI          PIC 9(01).
      *    カナ氏名検索用フラグ
      *    0:対象１件/1:対象なし/2:複数件該当
           03  FLG-NAME            PIC 9(01)   OCCURS 2.
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
           03  FLG-KYUSEI          PIC 9(01).
      *
           03  FLG-RJNCHK          PIC 9(01).
           03  FLG-HKNCHK          PIC 9(01).
           03  FLG-HKNCHK2         PIC 9(01).
           03  FLG-SEIHOGO         PIC 9(01).
           03  FLG-HKNCHK3         PIC 9(01).
      *
           03  FLG-P02A-CHK       PIC 9(01).
      *    公費単独チェック
           03  FLG-KOHCHK         PIC 9(01).
      *
      *    重複エラーなし
           03  FLG-ERRCHK         PIC 9(01).
      *    Ｈ１８．１０改正対応
           03  FLG-RJNCHK2             PIC 9(01).
           03  FLG-RJNCHK3             PIC 9(01).
           03  FLG-RJNERR              PIC 9(01).
           03  FLG-KOKUHO              PIC 9(01).
           03  FLG-RJNKOHERR           PIC 9(01).
           03  FLG-KOH27               PIC 9(01).
      *
           03  FLG-KANA                PIC 9(01).
           03  FLG-HKN-ADD2            PIC 9(01).
           03  FLG-27KOH-OK            PIC 9(01).
      *    高齢者非該当（H22.9)
           03  FLG-RJNCHK4             PIC 9(01).
      *    API処理
           03  FLG-APICHK              PIC 9(01).
      *    第三者行為
           03  FLG-KOHCHK-END          PIC 9(01).
           03  FLG-KOH-01              PIC 9(01).
           03  FLG-KOH-02              PIC 9(01).
           03  FLG-KOH-03              PIC 9(01).
      *H26.5
           03  FLG-RJN-02              PIC 9(01).
      *H26.8
           03  FLG-RJNCHK5             PIC 9(01).
      *H28.5
           03  FLG-P027                PIC 9(01).
      *H28.11
           03  FLG-GYODEL              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  CNT-JINMEI          PIC 9(06).
           03  CNT-KANANAME        PIC 9(06).
      *----(01.00.15) LINE ADD END   ---------------------------------
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
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
      *----(01.00.15) LINE ADD START ---------------------------------
           03  IDX1                PIC 9(05).
      *----(01.00.15) LINE ADD END   ---------------------------------
           03  IDX2                PIC 9(04).
           03  IDX-H               PIC 9(04).
           03  IDXH                PIC 9(04).
      *
           03  IDT                 PIC 9(04).
      *
           03  IDX-R               PIC 9(04).
           03  IDX-R2              PIC 9(04).
      *
           03  IDX-K               PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-KOH             PIC 9(04).
      *
           03  IDXK                PIC 9(04).
           03  IDX5                PIC 9(04).
      *
           03  IDX-API             PIC 9(04).
           03  IDXS                PIC 9(04).
      *APIV3
           03  IDX-STR             PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-SYMD6           PIC 9(08).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SAKIPG          PIC X(08).
           03  WRK-RENNUMZ         PIC ZZZZ.
      *    基準日
           03  WRK-BASEYMD.
               05  WRK-BYY         PIC 9(04).
               05  WRK-BMM         PIC 9(02).
               05  WRK-BDD         PIC 9(02).
      *    保存年齢
           03  WRK-MAE-NENREI.
               05  WRK-MAE-NENREI-YY       PIC 9(03).
               05  WRK-MAE-NENREI-MM       PIC 9(02).
      *    入力項目名
      *****03  WRK-MCP-WIDGET          PIC X(64).
      *    保険開始位置
           03  WRK-HKNSTR              PIC 9(04).
      *    公費開始位置
           03  WRK-KOHSTR              PIC 9(04).
           03  WRK-KOHEND              PIC 9(04).
      *
           03  WRK-SELNUM              PIC X(04).
           03  WRK-GMN-SELNUM          PIC 9(04).
      *
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-NAIBU-IDX-AREA.
               05  WRK-G-IDX       PIC 9(04).
               05  WRK-N-IDX       PIC 9(04).
      *        1:保険テーブル /2:公費テーブル
               05  WRK-W-TBL       PIC 9(01).
      *
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-HKNNUM      PIC X(03).
      *----(01.00.01) LINE MOD START ---------------------------------
      *         05  WRK-KEN-TEKYMD      PIC X(08).
               05  WRK-KEN-TEKSTYMD    PIC X(08).
               05  WRK-KEN-TEKEDYMD    PIC X(08).
      *----(01.00.01) LINE MOD END   ---------------------------------
               05  WRK-KEN-ERFLG       PIC X(01).
               05  WRK-KEN-SBTKBN      PIC X(01).
      *
           03  WRK-PARACNT-AREA.
               05  WRK-PCNT-HKN        PIC 9(04).
               05  WRK-PCNT-KOH        PIC 9(04).
      *
           03  WRK-SYSKAN-AREA.
               05  WRK-SYSKAN-KANRICD  PIC X(04).
               05  WRK-SYSKAN-KBNCD    PIC X(08).
               05  WRK-SYSKAN-STYUKYMD PIC X(08).
               05  WRK-SYSKAN-EDYUKYMD PIC X(08).
      *
           03  WRK-HKNKOH-FLG          PIC 9(01).
      *
           03  WRK-MEISAI1             PIC 9(06).
           03  WRK-MEISAI2             PIC 9(06).
           03  WRK-MEISAI3             PIC 9(06).
           03  WRK-HST                 PIC 9(04).
           03  WRK-HED                 PIC 9(04).
           03  WRK-KST                 PIC 9(04).
           03  WRK-KED                 PIC 9(04).
           03  WRK-DISCOUNT            PIC Z9.
           03  WRK-KYUSEI              PIC 9(01).
           03  WRK-RATEKBN             PIC X(02).
           03  WRK-KBNCD               PIC X(02).
           03  WRK-OK                  PIC 9(01).
      *----(01.00.06) LINE MOD START ---------------------------------
           03  WRK-WWYMD               PIC X(10).
      *----(01.00.06) LINE MOD END   ---------------------------------
      *
      *    労災保険ＩＤ
           03  WRK-PTRSI-HKNID         PIC 9(10).
      *
      *----(01.00.15) LINE ADD START ---------------------------------
           03  WRK-NAME-AREA.
               05  WRK-NAME-X-AREA.
                   07  WRK-NAME-X  PIC X(02)   OCCURS 25.
               05  WRK-NAME-VAL    PIC X(50)   OCCURS  3.
               05  WRK-NAME-KANJI  PIC X(50)   OCCURS  2.
               05  WRK-NAME-KANA   PIC X(50)   OCCURS  2.
               05  WRK-KANJINAME-AREA.
                   07  WRK-KANJINAME-OCC       OCCURS  50
                                               INDEXED BY  IDX-NAME.
                       09  WRK-KANJINAME-T
                                   PIC X(50).
               05  WRK-KANJINAME   PIC X(50).
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
      *    保険公費種別
           03  WRK-HKNNUM              PIC X(03).
           03  WRK-PAYKBN              PIC X(02).
           03  WRK-HKNKOHSBTKBN        PIC X(01).
           03  WRK-TANSEIDONAME        PIC X(20).
           03  WRK-TEKSTYMD            PIC X(08).
           03  WRK-TEKEDYMD            PIC X(08).
      *
           03  WRK-NAI-HKNSTR       PIC 9(04).
           03  WRK-NAI-KOHSTR       PIC 9(04).
      *APIV3
           03  WRK-NAI-KOHEND       PIC 9(04).
      *
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-IDXH2               PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *    病院種別
           03  WRK-HOSPSBT             PIC 9(01).
      *    老人支払区分
           03  WRK-ROUPAYKBN           PIC X(01).
      *
      *    全角チェック桁数
           03  WRK-LEN                 PIC 9(04).
      *
           03  WRK-EDANUM              PIC 9(04).
      *
      *!!!!
      *    ７０歳チェック
           03  FLG-KOKUHO-CHK          PIC 9(01).
           03  WRK-KOKUHO-CHK          PIC 9(04).
      *!!!!
      *ver4/7
           03  WRK-P02-KOHNUM-AREA.
               05  WRK-P02-KOHNUM-G        OCCURS   3.
                   07  WRK-P02-KOHNUM      PIC X(03).
                   07  WRK-P02-KOHNUM2     PIC X(01).
                   07  WRK-P02-KOHNUMMEI   PIC X(16).
           03  WRK-P02-KOHNUM-AREA2.
               05  WRK-P02-KOHNUM-G2        OCCURS   3.
                   07  WRK-P02-KOHNUM2      PIC X(03).
                   07  WRK-P02-KOHNUM22     PIC X(01).
                   07  WRK-P02-KOHNUMMEI2   PIC X(16).
      *H26.5
           03  WRK-HOJO-ERR            PIC X(01).
      *
      *    公費ＳＯＲＴ用（ＳＰＡと同じ領域）
       01  WRK-KOUHI-AREA.
      *    公費情報
           03  WRK-GMN-KOH-AREA.
               05  WRK-GMN-KOH-TBL         OCCURS   3.
                   07  WRK-GMN-HOK-G.
                       09  WRK-GMN-KOHNUM-G.
                          11  WRK-GMN-KOHNUM      PIC X(03).
                          11  WRK-GMN-KOHKBN      PIC X(01).
      ****************    11  WRK-GMN-H1          PIC X(01).
                          11  WRK-GMN-KOHNUMMEI   PIC X(16).
      *
                       09  WRK-GMN-KTEKSTYMD        PIC X(10).
                       09  WRK-GMN-KTEKEDYMD        PIC X(10).
                       09  WRK-GMN-KKAKUNINYMD      PIC X(10).
                       09  WRK-GMN-FTNJANUM         PIC X(08).
                       09  WRK-GMN-JKYSNUM          PIC X(20).
      *
                   07  WRK-NAI-KTEKSTYMD        PIC X(08).
                   07  WRK-NAI-KTEKEDYMD        PIC X(08).
                   07  WRK-NAI-KKAKUNINYMD      PIC X(08).
      *            期限切れ区分
                   07  WRK-NAI-TEKEDCHKKBN      PIC X(01).
      *
                   07  WRK-NAI-PTKOH-EDANUM       PIC 9(04).
                   07  WRK-NAI-PTKOH-KOHID        PIC 9(10).
                   07  WRK-NAI-PTKOH-SBTKBN       PIC X(01).
                   07  WRK-NAI-PTKOH-PAYKBN       PIC X(02).
      *
      *                登録時の負担者番号・公費種類
                   07  WRK-MAE-KOHNUM-G.
                       09  WRK-MAE-KOHNUM      PIC X(03).
                       09  WRK-MAE-KOHKBN      PIC X(01).
      **************** 09  WRK-MAE-H1          PIC X(01).
                       09  WRK-MAE-KOHNUMMEI   PIC X(16).
                   07  WRK-MAE-FTNJANUM            PIC X(08).
                   07  WRK-MAE-KKAKUNINYMD         PIC X(08).
                   07  WRK-MAE-KKAKUNINYMDW        PIC X(09).
      *            ６５歳以上警告
                   07  WRK-NAI-AGE65               PIC 9(01).
      *            年齢警告
                   07  WRK-NAI-AGECHK              PIC 9(01).
      *            負担者番号警告
                   07  WRK-NAI-FTNCHK              PIC 9(01).
      *            保険者異動日警告
                   07  WRK-PTKOH-IDOYMD            PIC X(08).
                   07  WRK-NAI-PTKOH-KEIFLG        PIC X(01).
      *            受給者番号警告
                   07  WRK-NAI-JYUCHK              PIC 9(01).
      *            確認年月日警告
                   07  WRK-KOH-KAKUINFLG           PIC 9(01).
      *v48         重複警告（生保）
                   07  WRK-NAI-JFKCHKFLG           PIC 9(01).
               05  WRK-KOH-MAX             PIC 9(04).
      *
      *    保険組合せ（ＳＰＡと同じ）
       01  WRK-HKNCOBMI-SORT-AREA.
           03  WRK-HKNCOMBI-AREA.
               05  WRK-GMN-HKNCOMBI-LIST       OCCURS   30.
                       09  WRK-GMN-THKNCOMBINUM    PIC 9(04).
                       09  WRK-GMN-THKNCOMBIMEI    PIC X(60).
                       09  WRK-GMN-TTEKSTYMD       PIC X(09).
                       09  WRK-GMN-TTEKEDYMD       PIC X(09).
                       09  WRK-GMN-THKNNUM         PIC X(03).
                       09  WRK-GMN-THKNID          PIC 9(10).
      *
                       09  WRK-NAI-TTEKSTYMD       PIC X(08).
                       09  WRK-NAI-TTEKEDYMD       PIC X(08).
                       09  WRK-NAI-TKOH1HKNNUM     PIC X(03).
                       09  WRK-NAI-TKOH2HKNNUM     PIC X(03).
                       09  WRK-NAI-TKOH3HKNNUM     PIC X(03).
                       09  WRK-NAI-TKOH4HKNNUM     PIC X(03).
                       09  WRK-NAI-TKOH1ID         PIC 9(10).
                       09  WRK-NAI-THYOJIKBN       PIC X(01).
               05  WRK-HKNCOMBI-MAX            PIC 9(04).
      *
           03  WRK-SORT-KEY.
               05  WRK-SRT-TEKISTYMD      PIC X(08).
               05  WRK-SRT-HKNCOMBINUM    PIC X(04).
               05  WRK-SRT-IDX            PIC 9(04).
      *
           03  WRK-HKNCOMBI02-AREA.
               05  WRK-GMN-HKNCOMBI02-LIST       OCCURS   30.
                       09  WRK-GMN02-THKNCOMBINUM    PIC 9(04).
                       09  WRK-GMN02-THKNCOMBIMEI    PIC X(60).
                       09  WRK-GMN02-TTEKSTYMD       PIC X(09).
                       09  WRK-GMN02-TTEKEDYMD       PIC X(09).
                       09  WRK-GMN02-THKNNUM         PIC X(03).
                       09  WRK-GMN02-THKNID          PIC 9(10).
      *
                       09  WRK-NAI02-TTEKSTYMD       PIC X(08).
                       09  WRK-NAI02-TTEKEDYMD       PIC X(08).
                       09  WRK-NAI02-TKOH1HKNNUM     PIC X(03).
                       09  WRK-NAI02-TKOH2HKNNUM     PIC X(03).
                       09  WRK-NAI02-TKOH3HKNNUM     PIC X(03).
                       09  WRK-NAI02-TKOH4HKNNUM     PIC X(03).
                       09  WRK-NAI02-TKOH1ID         PIC 9(10).
                       09  WRK-NAI02-THYOJIKBN       PIC X(01).
      *
       01  WRK-HENSYU-AREA.
      *
           03  WRK-HONKA-TBL.
               05  WRK-HONKA-REC-G.
                   07  FILLER          PIC X(06) VALUE "1 本人".
                   07  FILLER          PIC X(06) VALUE "2 家族".
               05  WRK-HONKA-REC-R     REDEFINES   WRK-HONKA-REC-G.
                   07  WRK-HONKA-REC   PIC X(06) OCCURS  2.
      *
           03  WRK-HJO-TBL.
               05  WRK-HJO-MAX         PIC 9(04).
               05  WRK-HJO-REC-G1.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
               05  WRK-HJO-REC-R1      REDEFINES   WRK-HJO-REC-G1.
                   07  WRK-HJO-REC1    PIC X(10) OCCURS  11.
               05  WRK-HJO-REC-G2.
                   07  FILLER          PIC X(16) VALUE "0 無し".
                   07  FILLER          PIC X(16) VALUE "1 １割".
                   07  FILLER          PIC X(16) VALUE "2 ２割".
                   07  FILLER          PIC X(16) VALUE "3 ３割".
                   07  FILLER          PIC X(16) VALUE
                                                 "4 外１割 入０割".
                   07  FILLER          PIC X(16) VALUE
                                                 "5 外２割 入１割".
                   07  FILLER          PIC X(16) VALUE
                                                 "6 外３割 入２割".
               05  WRK-HJO-REC-R2      REDEFINES   WRK-HJO-REC-G2.
                   07  WRK-HJO-REC2    PIC X(16) OCCURS  7.
               05  WRK-HJO-REC-G3.
                   07  FILLER          PIC X(06) VALUE "1 課税".
                   07  FILLER          PIC X(06) VALUE "2 非課".
               05  WRK-HJO-REC-R3      REDEFINES   WRK-HJO-REC-G3.
                   07  WRK-HJO-REC3    PIC X(06) OCCURS  2.
      *        老人の補助区分追加
               05  WRK-HJO-REC-G4.
                   07  FILLER          PIC X(06) VALUE "8 ２割".
                   07  FILLER          PIC X(06) VALUE "9 １割".
               05  WRK-HJO-REC-R4      REDEFINES   WRK-HJO-REC-G4.
                   07  WRK-HJO-REC4    PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加
               05  WRK-HJO-REC-G5.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
               05  WRK-HJO-REC-R5      REDEFINES   WRK-HJO-REC-G5.
                   07  WRK-HJO-REC5    PIC X(10) OCCURS  5.
      *        船員（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G1-2.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "A １割職務".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "C １割通勤".
                   07  FILLER          PIC X(10) VALUE "G ３割職務".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
                   07  FILLER          PIC X(10) VALUE "I ３割通勤".
               05  WRK-HJO-REC-R1-2    REDEFINES   WRK-HJO-REC-G1-2.
                   07  WRK-HJO-REC1-2  PIC X(10) OCCURS  11.
      *        老人の補助区分追加（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G4-2.
                   07  FILLER          PIC X(06) VALUE "7 ３割".
                   07  FILLER          PIC X(06) VALUE "9 １割".
               05  WRK-HJO-REC-R4-2    REDEFINES   WRK-HJO-REC-G4-2.
                   07  WRK-HJO-REC4-2  PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加（Ｈ１８．１０以降）
               05  WRK-HJO-REC-G5-2.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "9 １割".
                   07  FILLER          PIC X(10) VALUE "B １割下船".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
               05  WRK-HJO-REC-R5-2      REDEFINES   WRK-HJO-REC-G5-2.
                   07  WRK-HJO-REC5-2    PIC X(10) OCCURS  5.
      *
      *        後期高齢者の補助区分（Ｈ２０．４以降）
               05  WRK-HJO-REC-G6.
                   07  FILLER          PIC X(10) VALUE "1 １割".
                   07  FILLER          PIC X(10) VALUE "3 ３割".
               05  WRK-HJO-REC-R6        REDEFINES   WRK-HJO-REC-G6.
                   07  WRK-HJO-REC6      PIC X(10) OCCURS  2.
      *
      *H25.5 開始日（S19.4.2生　以降）
      *        老人の補助区分追加（前期高齢者）
               05  WRK-HJO-REC-G4-3.
                   07  FILLER          PIC X(06) VALUE "7 ３割".
                   07  FILLER          PIC X(06) VALUE "8 ２割".
               05  WRK-HJO-REC-R4-3    REDEFINES   WRK-HJO-REC-G4-3.
                   07  WRK-HJO-REC4-3      PIC X(06) OCCURS  2.
      *        共済の下船後補助区分追加（前期高齢者）
               05  WRK-HJO-REC-G5-3.
                   07  FILLER          PIC X(10) VALUE "2 下船３月".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
               05  WRK-HJO-REC-R5-3    REDEFINES   WRK-HJO-REC-G5-3.
                   07  WRK-HJO-REC5-3      PIC X(10) OCCURS  5.
      *        船員（前期高齢者）
               05  WRK-HJO-REC-G1-3.
                   07  FILLER          PIC X(10) VALUE "1 職務".
                   07  FILLER          PIC X(10) VALUE "2 下船".
                   07  FILLER          PIC X(10) VALUE "3 通勤".
                   07  FILLER          PIC X(10) VALUE "7 ３割".
                   07  FILLER          PIC X(10) VALUE "8 ２割".
                   07  FILLER          PIC X(10) VALUE "D ２割職務".
                   07  FILLER          PIC X(10) VALUE "E ２割下船".
                   07  FILLER          PIC X(10) VALUE "F ２割通勤".
                   07  FILLER          PIC X(10) VALUE "G ３割職務".
                   07  FILLER          PIC X(10) VALUE "H ３割下船".
                   07  FILLER          PIC X(10) VALUE "I ３割通勤".
               05  WRK-HJO-REC-R1-3    REDEFINES   WRK-HJO-REC-G1-3.
                   07  WRK-HJO-REC1-3  PIC X(10) OCCURS  11.
      *
           03  WRK-KEI-TBL.
               05  WRK-KEI-REC-G.
                   07  FILLER          PIC X(06) VALUE "1 継続".
                   07  FILLER          PIC X(06) VALUE "2 任継".
                   07  FILLER          PIC X(06) VALUE "3 特別".
               05  WRK-KEI-REC-R       REDEFINES   WRK-KEI-REC-G.
                   07  WRK-KEI-REC     PIC X(06) OCCURS  3.
               05  WRK-CONT-MAX        PIC 9(04).
      *
           03  WRK-HENYMD.
               05  WRK-HGO             PIC X(01).
               05  WRK-HYY             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HMM             PIC Z9.
               05  FILLER              PIC X(01)   VALUE  ".".
               05  WRK-HDD             PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH             PIC 99.
               05  FILLER              PIC X(01)   VALUE  ":".
               05  WRK-TMM             PIC 99.
      *
           03  WRK-KEI1                PIC X(06)   VALUE  "（継）".
           03  WRK-KEI2                PIC X(06)   VALUE  "（任）".
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
           03  TBL-MAX-KOH             PIC 9(02)  VALUE    60.
           03  TBL-MAX-HKN             PIC 9(02)  VALUE    40.
      *
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
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *----(01.00.12) LINE ADD END   ---------------------------------
      *    半角全角変換
       01  WRK-HENKAN-AREA.
           03  WRK-MAE-INPUT       PIC X(100).
           03  WRK-ATO-INPUT       PIC X(100).
           03  WRK-MAX             PIC 9(04).
      *
      *APIV3
      *    患者画面スパ領域の保存
           COPY    "P02SCR-SPA"
                              REPLACING  //SPA-// BY //APIS-//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    状態情報
      *    COPY    "CPSK1018.INC".
      *    減免事由情報
      *    COPY    "CPSK1026.INC".
      *    割引率情報
      *****COPY    "CPSK1027.INC".
      *    周辺住所
           COPY    "CPSK1015.INC".
      *
      *    住所マスタ
       01  ADRS-REC.
           COPY    "CPADRS.INC".
      *
      *    保険番号情報  *
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    受付   *
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    保険者号マスタ
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者保険情報  *
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
      *    組合せ作成時使用項目
           03  PTHKN-SBT          PIC X(01).
           03  PTHKN-SEIDO        PIC X(10).
      *
      *    患者公費情報  *
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    組合せ作成時使用項目
           03  PTKOH-SBT          PIC X(01).
           03  PTKOH-SEIDO        PIC X(10).
           03  PTKOH-HKNKOHKBN    PIC X(01).
           03  PTKOH-GENHKNNUM1   PIC X(03).
           03  PTKOH-GENHKNNUM2   PIC X(03).
           03  PTKOH-GENHKNNUM3   PIC X(03).
      *    患者情報  *
       01  PTINF-REC.
           COPY "CPPTINF.INC".
      *
      *    患者情報  *
      *01  LNK-PTINF-REC.
      *    COPY "CPPTINF.INC" REPLACING  //PTINF-// BY //LNK-PTINF-//.
      *
      *    旧姓履歴   *
      *01  LNK-KYUSEIRRK-AREA.
      *    02  LNK-KYUSEIRRK-REC.
      *    COPY  "CPKYUSEIRRK.INC" REPLACING  //KYUSEI-//
      *                                    BY //LNK-KYU-//.
      *    患者番号変換  *
      *01  LNK-PTNUM-AREA.
      *    02  LNK-PTNUM-REC.
      *    COPY "CPPTNUM.INC" REPLACING //PTNUM-// BY //LNK-PTNUM-//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *----(01.00.15) LINE ADD START ---------------------------------
      *    人名
       01  JINMEI-REC.
           COPY    "CPJINMEI.INC".
      *----(01.00.15) LINE ADD END   ---------------------------------
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *    患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    公費月上限額取得用パラメタ
           COPY    "CPORCSJGNGET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *    全角チェックパラメタ（拡張漢字）
           COPY    "CPORCSKANACHK2.INC".
      *
           COPY    "CPORCXKANACONV.INC".
      *
      *    画面編集パラメタ
           COPY    "CPORCSP02.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  SPA-FLG-END
           MOVE    SPACE           TO  SPA-MCP-WIDGET
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
      *    画面編集後、表示へ
           IF      SPA-FLG-END             =   ZERO
      ******** PERFORM 500-GMNHEN-SEC
               PERFORM 5001-MAPCUR-SEC
      *
           END-IF
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  WRK-AREA
      *
      *    初期画面セット
           PERFORM 300-SCREEN-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
           MOVE    ZERO            TO  SPA-FLG-END
      *
           EVALUATE    SPA-MOTOPG
      ****     住所検索より
      *        WHEN    "P99"
      *            IF      JPAR-POST     NOT =   SPACE
      *                MOVE    JPAR-POST   TO  SPA-GMN-POST
      *                MOVE    JPAR-EDITADRS-NAME
      *                                    TO  SPA-GMN-EDITADRS-NAME
      *                MOVE    42          TO  SPA-GMN-CUR
      *            ELSE
      *                MOVE    SPACE       TO  SPA-GMN-POST
      *                MOVE    40          TO  SPA-GMN-CUR
      ***********  END-IF
      *        住所検索より
               WHEN    "P98" 
                   IF      JPAR-POST   NOT =   SPACE
                       MOVE    JPAR-POST   TO  SPA-GMN-POST
                       IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *                    都道府県名なし
                           MOVE    JPAR-CITYNAME-NAME
                                           TO  SPA-GMN-EDITADRS-NAME
                       ELSE
                           MOVE    JPAR-EDITADRS-NAME
                                           TO  SPA-GMN-EDITADRS-NAME
                       END-IF
                       MOVE    SPA-GMN-EDITADRS-NAME
                                           TO  SPA-MAE-ADRS
                       MOVE    SPA-GMN-POST
                                           TO  SPA-MAE-POST
                       MOVE    42          TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPACE       TO  SPA-GMN-POST
                       MOVE    40          TO  SPA-GMN-CUR
                   END-IF
      *        住所一覧より
               WHEN    "P982"
                   IF     (SPA-GMN982-POST         =   SPACE )
                   AND    (SPA-GMN982-TOWNNAME     =   SPACE )
      *                選択なし
                       IF      SPA-GMN982-KBN      =   2
                           MOVE    41              TO  SPA-GMN-CUR
                       ELSE
                           MOVE    40              TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       PERFORM 30011-P982SET-SEC
                       MOVE    42              TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPA-GMN-EDITADRS-NAME
                                               TO  SPA-MAE-ADRS
                   MOVE    ZERO                TO  SPA-GMN982-KBN
      *        二重登録確認画面処理
               WHEN    "P02A"
      *             新規でリスト表示時のみ
                   IF      (SPA-P02G-FLG       =   2 )
                       PERFORM 41013-GRPPTNAME-CHK-SEC
                   END-IF
                   CONTINUE
      *        労災保険更新画面処理
               WHEN    "P02D"
                   PERFORM 4910-RSKOH-PARAOUT-SEC
      *        カナ氏名検索
               WHEN    "P100"
                   PERFORM 3010-P100-HEN-SEC
      *        患者複写
               WHEN    "P02E"
      *            保険組合せ作成
                   PERFORM 4503-HKNCOMBI-SYORI-SEC
                   PERFORM 310-SPASET-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
      *        患者複写
               WHEN    "P02G"
      *            保険組合せ作成
                   PERFORM 4503-HKNCOMBI-SYORI-SEC
                   PERFORM 310-SPASET-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
      *ver.4.7 
      *        公費種類選択
               WHEN    "P02H"
                   PERFORM 3010-P02H-HEN-SEC
      *        確認メッセージ
               WHEN    "PID1"
                   PERFORM 3001-PID1-SYORI-SEC
      *
      *        初期画面編集
               WHEN    OTHER
      *            保険・公費編集処理
                   PERFORM 310-SPASET-SEC
                   MOVE    1                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ氏名検索画面より処理
      *****************************************************************
       3010-P100-HEN-SEC              SECTION.
      *
           IF    ( SPA-GMN-P100-FLG-BTN  NOT =   ZERO )
               MOVE    SPA-GMN-P100-KAKUTEI-NAME
                                           TO  SPA-GMN-NAME
                                               SPA-GMN-OLD-NAME
                                               SPA-GMN-NAMEH
      *        世帯主名
               IF  SPA-GMN-SETAINUSI      =   SPACE
                   IF      SPA-NAI-SETAIKBN-FLG    =   "1"
                       MOVE    SPA-GMN-NAME    TO  SPA-GMN-SETAINUSI
                   END-IF
               END-IF
           END-IF
      *
           MOVE    2                   TO  SPA-GMN-CUR
           .
       3010-P100-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費種類選択より処理
      *****************************************************************
       3010-P02H-HEN-SEC              SECTION.
      *
           IF      SPA-P02H-KOHNUM    NOT =   SPACE
               EVALUATE    SPA-P02H-KOHIDX
                   WHEN    1
                       MOVE    SPA-P02H-KOHNUM TO  P02-P02-KOHNUM1
                       MOVE    "P02_KOHNUM1"   TO  MCP-WIDGET
                   WHEN    2
                       MOVE    SPA-P02H-KOHNUM TO  P02-P02-KOHNUM2
                       MOVE    "P02_KOHNUM2"   TO  MCP-WIDGET
                   WHEN    3
                       MOVE    SPA-P02H-KOHNUM TO  P02-P02-KOHNUM3
                       MOVE    "P02_KOHNUM3"   TO  MCP-WIDGET
               END-EVALUATE
           END-IF
           IF      SPA-P02H-KOHNUM        =   SPACE
               EVALUATE    SPA-P02H-KOHIDX
                   WHEN    1
                       MOVE    "P02_FTNJANUM1"   TO  MCP-WIDGET
                   WHEN    2
                       MOVE    "P02_FTNJANUM2"   TO  MCP-WIDGET
                   WHEN    3
                       MOVE    "P02_FTNJANUM3"   TO  MCP-WIDGET
               END-EVALUATE
           END-IF
           INITIALIZE                  SPA-P02H-AREA
           PERFORM 200-ENTRY
           .
       3010-P02H-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面より処理
      *****************************************************************
       3001-PID1-SYORI-SEC              SECTION.
      *
           EVALUATE    SPA-PIDCD
               WHEN    "0104"
               WHEN    "0105"
               WHEN    "0110"
      *            保険重複処理
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
               WHEN    "0111"
      *            老人公費２割を３割へ
               IF      (SPA-PTKOH-MAX      >=  TBL-MAX-KOH )
                   MOVE    "0106"          TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO                TO  SPA-KOH-ADD
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
               END-IF
               WHEN    "0106"
               WHEN    "0107"
      *            公費重複処理
               IF      (SPA-PTKOH-MAX      >=  TBL-MAX-KOH )
                   MOVE    "0106"          TO  SPA-ERRCD
               ELSE
                   MOVE    ZERO                TO  SPA-KOH-ADD
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
      *            老人公費追加（Ｈ１４．１０改正対応）
                   IF       SPA-KOH-ADD        =   1
      *                最初の１行を追加する
                       MOVE    ZERO            TO  SPA-HKNKOH-FLG
                       PERFORM 4104-KOH-ADD-SEC
      *                公費重複処理
                       CALL    "ORCSP03B"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
                   END-IF
               END-IF
      *        保険終了処理
               WHEN    "0201"
               WHEN    "0203"
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
               WHEN    "0202"
               WHEN    "0204"
                   IF      SPA-HKN-IDX1        =   ZERO
      *                新規の時、１行開ける
                       PERFORM 410311-HKNTBL-INS-SEC
                       MOVE    1                   TO  IDX
                   ELSE
                       MOVE    SPA-HKN-IDX1        TO  IDX
                   END-IF
                   PERFORM 410312-HKN-SPATBL-SET-SEC
                   CALL    "ORCSP03A"          USING   SPA-AREA
                                                       SPA-P02SCRAREA
                                                       SPA-P02KYOUTU
      *        住所確認
               WHEN    "1098"
                   PERFORM 30011-P982SET-SEC
      *        旧姓履歴確認
               WHEN    "0250"
                   IF      SPA-PID1-FLG        =   "OK"
                       MOVE    1                   TO  FLG-KYUSEI
                       PERFORM 41091-KYUSEI-KOU-SEC
                   END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-PIDCD
      *
           .
       3001-PID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所確認編集処理
      *****************************************************************
       30011-P982SET-SEC              SECTION.
      *
           IF      SPA-GMN982-POST     NOT =   SPACE
               MOVE    SPA-GMN982-POST     TO  SPA-GMN-POST
           END-IF
      *
           IF      SPA-GMN982-TOWNNAME NOT =   SPACE
               IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *        都道府県名なし
                   MOVE    SPA-GMN982-CITYNAMET
                                           TO  SPA-GMN-EDITADRS-NAME
               ELSE
      *        都道府県名あり
                   MOVE    SPA-GMN982-TOWNNAME
                                           TO  SPA-GMN-EDITADRS-NAME
               END-IF
           END-IF
           MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           MOVE    SPA-GMN-POST            TO  SPA-MAE-POST
           .
       30011-P982SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
      *    年齢計算
           IF     (SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO )  OR
                  (SPA-BYMD-70     NOT =   ZERO            )
               CONTINUE
           ELSE
               PERFORM 410121-NENREI-SET-SEC
           END-IF
      *
      *    テーブル内の保険情報に名称を編集する
           PERFORM 3100-HKNTBL-KBNHEN-SEC
      *
      *TEST
      *    保険初期表示変更
           IF      SPA-NAI-HKNINITKBN-FLG  NOT =   "0"
      *        自費以外を初期表示
               PERFORM 3109-HKNINIT-SYORI-SEC
           ELSE
      *        １件目
               MOVE    1               TO  SPA-NAI-HKNSTR
           END-IF
      *TEST
      *    保険表示連番
      *****MOVE    1                   TO  SPA-NAI-HKNSTR
      *    保険情報ＴＢＬから編集
           PERFORM 3101-HKN-TBL-SPA-SEC
      *
      *    公費表示連番開始
           MOVE    1                   TO  SPA-NAI-KOHSTR
      *    公費ＴＢＬから編集
           PERFORM 3101-KOH-TBL-SPA-SEC
      *
      *H30.10  初期表示の為
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    保険組合せ表示（現在対象分のみ）
           PERFORM 3103-HKNCOMB-PARA-SEC
      *
      *
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険初期表示変更処理
      *****************************************************************
       3109-HKNINIT-SYORI-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-PTHKN-MAX )
                         OR   (FLG-CHK NOT =   ZERO )
               IF     (SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE      )
                  AND (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD )
                  AND (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD )
                   IF     (SPA-PTHKN-HKNNUM (IDX)(1:2) NOT =   "98" )
                    AND   (SPA-NAI-HKNINITKBN-FLG  =   "1"   )
                       MOVE    IDX             TO  IDY
                       MOVE    1               TO  FLG-CHK
                   END-IF
                   IF     (SPA-PTHKN-HKNNUM (IDX)(1:1) NOT =   "9" )
                    AND   (SPA-NAI-HKNINITKBN-FLG  =   "2"   )
                       MOVE    IDX             TO  IDY
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-NAI-HKNSTR
           IF      IDY             NOT =   ZERO
               MOVE    IDY             TO  SPA-NAI-HKNSTR
           END-IF
           .
       3109-HKNINIT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分等の名称編集処理
      *****************************************************************
       3100-HKNTBL-KBNHEN-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX         >   SPA-PTHKN-MAX
      *
      *        本人家族
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
                   IF      WRK-HONKA-REC (IDT)(1:1)
                                           =   SPA-PTHKN-HONKZKKBN(IDX)
                       MOVE    WRK-HONKA-REC (IDT)(3:)
                                           TO  SPA-PTHKN-HONKZKKBN-MEI
                                                                  (IDX)
                   END-IF
               END-PERFORM
      *
      *        継続区分
               IF      SPA-PTHKN-HKNNUM (IDX)  =   "003"
                   MOVE    3                   TO  WRK-CONT-MAX
               ELSE
                   MOVE    2                   TO  WRK-CONT-MAX
               END-IF
               PERFORM VARYING IDT     FROM    1  BY  1
                       UNTIL   IDT     >   WRK-CONT-MAX
                   IF      WRK-KEI-REC (IDT)(1:1)
                                           =   SPA-PTHKN-CONTKBN (IDX)
                       MOVE    WRK-KEI-REC (IDT)(3:)
                                               TO  SPA-PTHKN-CONTKBN-MEI
                                                                  (IDX)
                   END-IF
               END-PERFORM
      *
      *H26.4
      *        H26.5.1以降のS19.4.2生の高齢者負担割合
               MOVE    ZERO                TO  FLG-RJN-02
               IF      (SPA-NAI-BIRTHDAY        >=  "19440402")
                   AND (SPA-PTHKN-TEKSTYMD(IDX) >=  "20140501")
                   AND (SPA-PTHKN-HKNNUM(IDX)(1:1)
                                             NOT =  "9"       )
                   AND (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                   MOVE    1                 TO  FLG-RJN-02
               END-IF
      *H26.8
               IF      (SPA-PTHKN-TEKSTYMD(IDX) >=  "20140501")
                   AND (SPA-PTHKN-HKNNUM(IDX)(1:1)
                                             NOT =  "9"       )
                   AND (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-75-2)
      *            高齢者非該当判定
                   PERFORM 310111-HIGAITOU-CHK-SEC
                   IF      FLG-RJNCHK5          =   1
                       MOVE    1                 TO  FLG-RJN-02
                   END-IF
               END-IF
               IF      FLG-RJN-02          =   1
                   PERFORM 31002-HKNTBL-HOJKBNHEN-SEC
               ELSE
      *
      *        負担割合
               EVALUATE    SPA-PTHKN-HKNNUM (IDX)
      *        船員
               WHEN    "002"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    4               TO  IDY
                       MOVE    11              TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    3               TO  WRK-HJO-MAX
                   END-IF
      *************MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
      *              Ｈ１８．１０ 改正
                     IF    ((SPA-PTHKN-TEKSTYMD(IDX) =   SPACE )  AND
                            (SPA-SYSYMD              >=  "20061001")) OR
                            (SPA-PTHKN-TEKSTYMD(IDX) >=  "20061001")
                       IF      WRK-HJO-REC1-2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC1-2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                     ELSE
                       IF      WRK-HJO-REC1 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC1 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                     END-IF
                   END-PERFORM
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   7
                       IF      WRK-HJO-REC2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    7           TO  IDT
                       END-IF
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    2               TO  IDY
                       MOVE    5               TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
      *              Ｈ１８．１０ 改正
                     IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20061001")
                        OR ((SPA-PTHKN-TEKSTYMD(IDX) =   SPACE  )  AND
                            (SPA-SYSYMD              >=  "20061001"))
                       IF      WRK-HJO-REC5-2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC5-2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                     ELSE
                       IF      WRK-HJO-REC5 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC5 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                     END-IF
                   END-PERFORM
      *Ｈ２０．４改正
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   2
                       IF      WRK-HJO-REC6 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC6 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    2           TO  IDT
                       END-IF
                   END-PERFORM
      *        その他
               WHEN    OTHER
                   EVALUATE    SPA-PTHKN-HKNNUM (IDX)(1:2)
      *            自費
                   WHEN    "98"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           IF      WRK-HJO-REC3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                               MOVE    WRK-HJO-REC3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                               MOVE    2           TO  IDT
                           END-IF
                       END-PERFORM
      *            治験
                   WHEN    "90"
                   WHEN    "91"
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           IF      WRK-HJO-REC3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                               MOVE    WRK-HJO-REC3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                               MOVE    2           TO  IDT
                           END-IF
                       END-PERFORM
                   WHEN    OTHER
      *                Ｈ１４．１０ 改正より
                       IF      SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001"
                           PERFORM 31001-HKNTBL-RJNHEN-SEC
                       END-IF
                   END-EVALUATE
               END-EVALUATE
      *
      *
               END-IF
           END-PERFORM
      *
           .
       3100-HKNTBL-KBNHEN-EXT.
           EXIT.
      *H26.8
      ****************************************************************
      *    高齢非該当判定処理
      ****************************************************************
       310111-HIGAITOU-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNCHK4
           MOVE    ZERO                TO  FLG-RJNCHK5
           PERFORM VARYING     IDXK    FROM    1   BY  1
                   UNTIL      (IDXK    >   SPA-PTKOH-MAX   )
      *        高齢者非該当
               IF     (SPA-PTKOH-TEKSTYMD (IDXK)  <=
                                               SPA-PTHKN-TEKEDYMD(IDX))
                 AND  (SPA-PTKOH-TEKEDYMD (IDXK)  >=
                                               SPA-PTHKN-TEKSTYMD(IDX))
                 AND  (SPA-PTKOH-SAKJOKBN (IDXK) NOT =  "1"        )
                   IF     (SPA-PTKOH-KOHNUM  (IDXK) =   "976" )
                       MOVE    1                   TO  FLG-RJNCHK4
                   END-IF
                   IF     (SPA-PTKOH-KOHNUM  (IDXK) =   "977" )
                       MOVE    1                   TO  FLG-RJNCHK5
                   END-IF
               END-IF
           END-PERFORM
           .
       310111-HIGAITOU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    老人補助区分編集処理
      *****************************************************************
       31001-HKNTBL-RJNHEN-SEC            SECTION.
      *
      *    適用開始日が７０歳以上で、労災・治験・自費
           IF      (SPA-PTHKN-TEKSTYMD(IDX)    >=  SPA-BYMD-70)  AND
                   (SPA-BYMD-70                >   ZERO      )  AND
                   (SPA-PTHKN-HKNNUM(IDX)(1:1) NOT =   "9")
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
      *          Ｈ１８．１０ 改正より
                 IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20061001")
                    OR ((SPA-PTHKN-TEKSTYMD(IDX) =   SPACE  )  AND
                        (SPA-SYSYMD              >=  "20061001"))
                   IF      WRK-HJO-REC4-2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                       MOVE    WRK-HJO-REC4-2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                       MOVE    2               TO  IDT
                   END-IF
                 ELSE
                   IF      WRK-HJO-REC4 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                       MOVE    WRK-HJO-REC4 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                       MOVE    2               TO  IDT
                   END-IF
                 END-IF
               END-PERFORM
           END-IF
           .
       31001-HKNTBL-RJNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢者負担割合（補助区分）設定処理(H26.5から）
      *****************************************************************
       31002-HKNTBL-HOJKBNHEN-SEC            SECTION.
      *
      *    負担割合
           EVALUATE    SPA-PTHKN-HKNNUM (IDX)
      *        船員
               WHEN    "002"
                   MOVE    4               TO  IDY
                   MOVE    11              TO  WRK-HJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       IF      WRK-HJO-REC1-3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC1-3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                   END-PERFORM
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   7
                       IF      WRK-HJO-REC2 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC2 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    7           TO  IDT
                       END-IF
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   MOVE    2                   TO  IDY
                   MOVE    5                   TO  WRK-HJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       IF      WRK-HJO-REC5-3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC5-3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    WRK-HJO-MAX     TO  IDT
                       END-IF
                   END-PERFORM
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   2
                       IF      WRK-HJO-REC6 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC6 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    2           TO  IDT
                       END-IF
                   END-PERFORM
      *        その他
               WHEN    OTHER
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   2
                       IF      WRK-HJO-REC4-3 (IDT)(1:1)
                                           =   SPA-PTHKN-HOJOKBN(IDX)
                           MOVE    WRK-HJO-REC4-3 (IDT)(3:)
                                               TO  SPA-PTHKN-HOJOKBN-MEI
                                                                 (IDX)
                           MOVE    2           TO  IDT
                       END-IF
                   END-PERFORM
           END-EVALUATE
      *
           .
       31002-HKNTBL-HOJKBNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報パラメタファイル検索処理
      *****************************************************************
       3101-HKN-TBL-SPA-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  FLG-CHK
           IF      SPA-NAI-HKNSTR      =   ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDX     FROM    SPA-NAI-HKNSTR   BY  1
                   UNTIL      (IDX         >   SPA-PTHKN-MAX )  OR
                              (FLG-CHK NOT =   ZERO )
                   IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
      *                ＱＲデータの表示
                       IF     (SPA-QR-FLG          =   1   )  AND
                              (SPA-QR-HKNID    NOT =   ZERO )  AND
                              (SPA-QR-HKNID    NOT =   SPA-PTHKN-HKNID
                                                               (IDX))
                           CONTINUE
                       ELSE
                           MOVE    IDX             TO  SPA-NAI-HKNSTR
                           PERFORM 3101-HOKEN-HEN-SEC
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    対象の保険がない時、新規とする
           IF      FLG-CHK             =   ZERO
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
           END-IF
      *
           .
       3101-HKN-TBL-SPA-EXT.
           EXIT.
      *****************************************************************
      *    公費情報パラメタファイル検索処理
      *****************************************************************
       3101-KOH-TBL-SPA-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-KOH-AREA
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  SPA-NAI-KOHEND
           IF      SPA-NAI-KOHSTR      =   ZERO
               CONTINUE
           ELSE
      *
           PERFORM VARYING     IDX     FROM    SPA-NAI-KOHSTR BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (IDY         >=  3    )
               IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
           END-PERFORM
           END-IF
      *
      *    対象の保険がない時、新規とする
           IF      FLG-CHK             =   ZERO
               MOVE    ZERO                TO  SPA-NAI-KOHSTR
               MOVE    ZERO                TO  SPA-NAI-KOHEND
           END-IF
      *
           .
       3101-KOH-TBL-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険編集処理
      *****************************************************************
       3101-HOKEN-HEN-SEC              SECTION.
      *
           MOVE    SPA-PTHKN-HKNNUM (IDX)  TO  SPA-GMN-HKNNUM
      *    保険ＩＤ（労災保険のとの関連）
           MOVE    SPA-PTHKN-HKNID  (IDX)  TO  SPA-NAI-HKNID
      *
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
           MOVE    SPA-PTHKN-HIHKNJANAME (IDX)
                                           TO  SPA-GMN-HIHKNJANAME
           MOVE    SPA-PTHKN-HONKZKKBN (IDX)
                                           TO  SPA-GMN-HONKZKKBN
           MOVE    SPA-GMN-HONKZKKBN       TO  SPA-MAE-HONKZKKBN
           MOVE    SPA-PTHKN-HOJOKBN (IDX) TO  SPA-GMN-HOJOKBN
           MOVE    SPA-PTHKN-CONTKBN (IDX) TO  SPA-GMN-CONTKBN
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-GMN-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-GMN-NUM
      *R02.4
           MOVE    SPA-PTHKN-EDABAN (IDX)  TO  SPA-GMN-EDABAN
      *
           MOVE    SPA-PTHKN-HKN-SBTKBN(IDX)
                                           TO  SPA-NAI-PTHKN-SBTKBN
           MOVE    SPA-PTHKN-SAKJOKBN(IDX) TO  SPA-NAI-SAKJOKBN
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)
                                           TO  SPA-NAI-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMD(IDX) TO  SPA-NAI-HTEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMD(IDX) TO  SPA-NAI-HTEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMD (IDX)
                                           TO  SPA-NAI-HKAKUNINYMD
      *
           MOVE    SPA-PTHKN-SKKGETYMDW (IDX)
                                           TO  SPA-GMN-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMDW (IDX)
                                           TO  SPA-GMN-HTEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMDW (IDX)
                                           TO  SPA-GMN-HTEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMDW (IDX)
                                           TO  SPA-GMN-HKAKUNINYMD
      *
      *    前保存
           MOVE    SPA-PTHKN-HKNNUM  (IDX) TO  SPA-MAE-HKNNUM
           MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-MAE-HKNJANUM
           MOVE    SPA-PTHKN-HOJOKBN (IDX) TO  SPA-MAE-HOJOKBN
           MOVE    SPA-PTHKN-MAEKAKUNINYMD (IDX)
                                           TO  SPA-MAE-HKAKUNINYMD
           MOVE    SPA-PTHKN-MAEKAKUNINYMDW(IDX)
                                           TO  SPA-MAE-HKAKUNINYMDW
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-OLD-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-OLD-NUM
      *
           MOVE    SPA-PTHKN-KIGO    (IDX) TO  SPA-MAE-KIGO
           MOVE    SPA-PTHKN-NUM     (IDX) TO  SPA-MAE-NUM
           MOVE    SPA-PTHKN-KIGO-KEIFLG (IDX)
                                           TO  SPA-KIGO-KEIFLG
           MOVE    SPA-PTHKN-NUM-KEIFLG (IDX)
                                           TO  SPA-NUM-KEIFLG
      *    本人家族警告
           MOVE    SPA-PTHKN-HONKZKKBN (IDX)
                                           TO  SPA-OLD-HONKZKKBN
           MOVE    SPA-PTHKN-HONKZKFLG (IDX)
                                           TO  SPA-HONKZK-KEIFLG
      *    補助区分警告
           MOVE    SPA-PTHKN-HOJOKBN (IDX)
                                           TO  SPA-OLD-HOJOKBN
           MOVE    SPA-PTHKN-HOJOKEIFLG (IDX)
                                           TO  SPA-HOJOKBN-KEIFLG
      *!API
      *    API処理（新規入力とする）
           IF      FLG-APICHK              =   1
                                           OR  2
               IF      SPA-PTHKN-SINKIKBN (IDX)    =   "1"
                   MOVE    SPACE               TO  SPA-OLD-KIGO
                   MOVE    SPACE               TO  SPA-OLD-NUM
                   MOVE    SPACE               TO  SPA-MAE-HKNNUM
                   MOVE    SPACE               TO  SPA-MAE-HKNJANUM
      *            後期高齢者警告済みとする
                   IF      FLG-APICHK              =   1
      *                V2
                   MOVE    1                   TO  SPA-PTHKN-ROU-KEIFLG
                                                               (IDX)
                   END-IF
               END-IF
           END-IF
      *!API
      *
      *    更新前の記号空白チェックは当日有効のもの
           IF      FLG-TOUROKU     NOT =   ZERO
               IF      (SPA-PTHKN-TEKEDYMD(IDX)  <=  SPA-SYSYMD )
                   MOVE    1               TO  SPA-KIGO-KEIFLG
                   MOVE    1               TO  SPA-NUM-KEIFLG
               END-IF
           END-IF
      *    後期高齢者警告
           MOVE    SPA-PTHKN-ROU-KEIFLG (IDX)
                                       TO  SPA-ROU-KEIFLG
      *    新規以外はチェック済みとする
           IF      SPA-PTHKN-SINKIKBN (IDX)    =   SPACE
               MOVE    1                   TO  SPA-ROU-KEIFLG
           END-IF
      *!!
           MOVE    SPA-PTHKN-KEIFLG(IDX)   TO  SPA-HKNJA-KEIFLG
      *    確認年月日
           MOVE    SPA-PTHKN-KAKUINFLG(IDX) TO  SPA-HKN-KAKUINFLG
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM           TO  HKNJA-HOSPNUM
           MOVE    SPA-GMN-HKNJANUM      TO  HKNJA-HKNJANUM
      *
           MOVE    HKNJAINF-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknjainf"        TO  MCP-TABLE
               MOVE    "key"                 TO  MCP-PATHNAME
               PERFORM 900-HKNJAINF-READ-SEC
           ELSE
               INITIALIZE                  HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           MOVE    "tbl_hknjainf"        TO  MCP-TABLE
           MOVE    "key"                 TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-HKNJANAME     TO  SPA-HKNJA-HKNJANAME
               MOVE    HKNJA-ADRS          TO  SPA-HKNJA-ADRS
               MOVE    HKNJA-BANTI         TO  SPA-HKNJA-BANTI
               MOVE    HKNJA-TEL           TO  SPA-HKNJA-TEL
      *
               MOVE    HKNJA-IDOYMD        TO  SPA-HKNJA-IDOYMD
               MOVE    HKNJA-IDOKBN        TO  SPA-HKNJA-IDOKBN
      *!       MOVE    SPA-PTHKN-KEIFLG(IDX)
      *!                                   TO  SPA-HKNJA-KEIFLG
           ELSE
               MOVE    SPACE               TO  SPA-HKNJA-HKNJANAME
               MOVE    SPACE               TO  SPA-HKNJA-ADRS
               MOVE    SPACE               TO  SPA-HKNJA-BANTI
               MOVE    SPACE               TO  SPA-HKNJA-TEL
      *
               MOVE    SPACE               TO  SPA-HKNJA-IDOYMD
               MOVE    SPACE               TO  SPA-HKNJA-IDOKBN
      *!!      MOVE    SPACE               TO  SPA-HKNJA-KEIFLG
           END-IF
      *
      *    保険情報
           MOVE    SPA-GMN-HKNNUM      TO  WRK-HKNNUM
           MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-TEKSTYMD
           MOVE    SPA-NAI-HTEKEDYMD   TO  WRK-TEKEDYMD
           MOVE    ZERO                TO  WRK-PAYKBN
           PERFORM 31012-HKNNUM-SET-SEC
           IF      WRK-HKNKOHSBTKBN    NOT =   SPACE
               MOVE    WRK-TANSEIDONAME    TO  SPA-GMN-HKNNUMMEI
               MOVE    WRK-HKNKOHSBTKBN    TO  SPA-NAI-PTHKN-SBTKBN
           END-IF
      *****PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   SPA-GMN-HKNMAX
      *        IF      SPA-GMN-HKNNUML (IDX)   =   SPA-GMN-HKNNUM
      *            MOVE    SPA-GMN-HKNNUMLST (IDX)
      *                                        TO  SPA-GMN-HKNNUM-G
      *            識別
      *            MOVE    SPA-NAI-HKNSBTKBN (IDX)
      *                                        TO  SPA-NAI-PTHKN-SBTKBN
      *            MOVE    SPA-GMN-HKNMAX      TO  IDX
      *        END-IF
      *****END-PERFORM
      *
      *    本人家族区分等
           PERFORM 31011-HKN-HEN-SEC
      *!API
      *    API処理（保険種類設定なし）
           IF     (FLG-APICHK          =   1   )
               GO      TO      3101-HOKEN-HEN-EXT
           END-IF
      *!API
      *    本人家族区分等
      *    PERFORM 31011-HKN-HEN-SEC
      *
      *    労災保険の番号設定
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
               PERFORM 310111-PARA-RSIHKN-SET-SEC
           END-IF
      *
           .
       3101-HOKEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費区分編集処理
      *****************************************************************
       31012-HKNNUM-SET-SEC           SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    WRK-HKNNUM          TO  HKN-HKNNUM
           IF      WRK-PAYKBN          =   SPACE
               MOVE    ZERO                TO  HKN-PAYKBN
           ELSE
               MOVE    WRK-PAYKBN          TO  HKN-PAYKBN
           END-IF
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKSTYMD
      *H26.12
      *    開始日の内容を設定
           MOVE    WRK-TEKSTYMD        TO  HKN-TEKEDYMD
      **** MOVE    WRK-TEKEDYMD        TO  HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
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
               MOVE    HKN-HKNKOHSBTKBN    TO  WRK-HKNKOHSBTKBN
               MOVE    HKN-TANSEIDONAME    TO  WRK-TANSEIDONAME
      *        リストの名称より長くする
      ***      INSPECT WRK-TANSEIDONAME    REPLACING ALL "  " BY "　"
           END-IF
      *
           .
       31012-HKNNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    本人家族区分等編集処理
      *****************************************************************
       31011-HKN-HEN-SEC      SECTION.
      *
      *H26.8
      *    高齢者非該当判定
           PERFORM 310111-HIGAITOU-CHK-SEC
      *
      *    本人家族
           MOVE    ZERO                TO  SPA-GMN-HONKZK-MAX
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   2
               MOVE    WRK-HONKA-REC (IDT)
                                       TO  SPA-GMN-HONKZKKBNL (IDT)
               MOVE    IDT             TO  SPA-GMN-HONKZK-MAX
               IF      SPA-GMN-HONKZKKBNL (IDT)(1:1)
                                           =   SPA-GMN-HONKZKKBN
                   MOVE    SPA-GMN-HONKZKKBNL (IDT)
                                           TO  SPA-GMN-HONKZKKBN-G
               END-IF
      *        自官は本人のみ
               IF      SPA-GMN-HKNNUM      =   "007"
      *        公害保険は本人のみ
                                           OR  "975"
      *        後期高齢者は本人のみ
                                           OR  "040"
                                           OR  "039"
                   MOVE    2               TO  IDT
               END-IF
           END-PERFORM
           IF      SPA-GMN-HONKZKKBN   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HONKZKKBN-G
           END-IF
      *
      *    継続区分
           IF    ( SPA-NAI-PTHKN-SBTKBN    NOT =   "1"  ) OR
                 ( SPA-GMN-HKNNUM          =   "060"
                                           OR  "067"
      *              特別療養費追加
                                           OR  "068"
                                           OR  "069"
      *              後期高齢者追加
                                           OR  "039"
                                           OR  "040" )
               MOVE    ZERO                TO  SPA-GMN-CONT-MAX
               MOVE    SPACE               TO  SPA-GMN-CONTKBN-G
           ELSE
               IF      SPA-GMN-HKNNUM      =   "003"
                   MOVE    3               TO  WRK-CONT-MAX
               ELSE
                   MOVE    2               TO  WRK-CONT-MAX
               END-IF
               PERFORM VARYING IDT     FROM    1  BY  1
                       UNTIL   IDT     >   WRK-CONT-MAX
                   MOVE    WRK-KEI-REC (IDT)
                                           TO  SPA-GMN-CONTKBNL (IDT)
                   IF      WRK-KEI-REC (IDT)(1:1)
                                           =   SPA-GMN-CONTKBN
                       MOVE    SPA-GMN-CONTKBNL (IDT)
                                               TO  SPA-GMN-CONTKBN-G
                   END-IF
                   MOVE    IDT             TO  SPA-GMN-CONT-MAX
               END-PERFORM
           END-IF
      *
      *H26.4
      *    H26.5.1以降のS19.4.2生の高齢者負担割合
           MOVE    ZERO                TO  FLG-RJN-02
           IF      (SPA-NAI-BIRTHDAY       >=  "19440402")
               AND (SPA-NAI-HTEKSTYMD      >=  "20140501")
               AND (SPA-GMN-HKNNUM(1:1) NOT =  "9"       )
               AND (SPA-NAI-HTEKSTYMD      >=  SPA-BYMD-70)
               MOVE    1                   TO  FLG-RJN-02
           END-IF
           IF      (FLG-RJNCHK5        =   1  )
               AND (SPA-NAI-HTEKSTYMD  >=  SPA-BYMD-75-2)
               AND (SPA-NAI-HTEKSTYMD  >=  "20140501")
               AND (SPA-GMN-HKNNUM(1:1) NOT =  "9"       )
               MOVE    1                   TO  FLG-RJN-02
           END-IF
      *
           IF      FLG-RJN-02          =   1
               PERFORM 310112-HKN-HOJOHEN2-SEC
      *
           ELSE
      *
      *    負担割合
           EVALUATE    SPA-GMN-HKNNUM
      *        船員
               WHEN    "002"
                   IF     (SPA-NAI-HTEKSTYMD       >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-NAI-HTEKSTYMD       >=  SPA-BYMD-70)
                       MOVE    4               TO  IDY
                       MOVE    11              TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    3               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
      *            Ｈ１８．１０ 改正より
                       IF     (SPA-NAI-HTEKSTYMD   >=  "20061001" )
                          OR ((SPA-NAI-HTEKSTYMD   =   SPACE  )  AND
                              (SPA-SYSYMD          >=  "20061001"))
                           MOVE    WRK-HJO-REC1-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       ELSE
                           MOVE    WRK-HJO-REC1 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       END-IF
                   END-PERFORM
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   7
                       MOVE    WRK-HJO-REC2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF     (SPA-PTHKN-TEKSTYMD(IDX) >=  "20021001") AND
                          (SPA-BYMD-70             >   ZERO      )  AND
                          (SPA-PTHKN-TEKSTYMD(IDX) >=  SPA-BYMD-70)
                       MOVE    2               TO  IDY
                       MOVE    5               TO  WRK-HJO-MAX
                   ELSE
                       MOVE    1               TO  IDY
                       MOVE    1               TO  WRK-HJO-MAX
                   END-IF
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1           TO  SPA-GMN-HOJO-MAX
      *            Ｈ１８．１０ 改正より
                       IF     (SPA-NAI-HTEKSTYMD   >=  "20061001"  )
                          OR ((SPA-NAI-HTEKSTYMD   =   SPACE  )  AND
                              (SPA-SYSYMD          >=  "20061001"))
                           MOVE    WRK-HJO-REC5-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       ELSE
                           MOVE    WRK-HJO-REC5   (IDT)
                                           TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                       END-IF
                   END-PERFORM
      *Ｈ２０．４改正
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC6 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *        その他
               WHEN    OTHER
      *            自費・治験
                   IF     (SPA-GMN-HKNNUM (1:2)    =   "98" )
                    OR    (SPA-GMN-HKNNUM (1:2)    =   "90"
                                                   OR  "91" )
                       PERFORM VARYING     IDT     FROM    1  BY  1
                               UNTIL       IDT     >   2
                           MOVE    WRK-HJO-REC3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                           MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                       END-PERFORM
                   ELSE
                       MOVE    ZERO        TO  SPA-GMN-HOJO-MAX
      *                Ｈ１４．１０ 改正より
                       IF      SPA-NAI-HTEKSTYMD   >=  "20021001"
                           PERFORM 310111-HKN-RJNHEN-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           END-IF
      *
      *    IF      SPA-GMN-HOJO-MAX    =   ZERO
      *        MOVE    SPACE           TO  SPA-GMN-HOJOKBN-G
      *    END-IF
           PERFORM VARYING     IDT     FROM    1  BY  1
                   UNTIL       IDT     >   SPA-GMN-HOJO-MAX
               IF      SPA-GMN-HOJOKBNL (IDT)(1:1)
                                           =   SPA-GMN-HOJOKBN
                   MOVE    SPA-GMN-HOJOKBNL (IDT)
                                           TO  SPA-GMN-HOJOKBN-G
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-HOJO-MAX    =   ZERO
               MOVE    SPACE           TO  SPA-GMN-HOJOKBN-G
           END-IF
      *
           .
       31011-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人補助区分編集処理
      *****************************************************************
       310111-HKN-RJNHEN-SEC            SECTION.
      *
      *    適用開始日が７０歳以上で、労災・治験・自費
           IF      (SPA-NAI-HTEKSTYMD      >=  SPA-BYMD-70)  AND
                   (SPA-BYMD-70            >   ZERO       )  AND
                   (SPA-GMN-HKNNUM(1:1) NOT =   "9")
               PERFORM VARYING     IDT     FROM    1  BY  1
                       UNTIL       IDT     >   2
      *            Ｈ１８．１０ 改正より
                   IF     (SPA-NAI-HTEKSTYMD       >=  "20061001" )
                      OR ((SPA-NAI-HTEKSTYMD       =   SPACE  )  AND
                          (SPA-SYSYMD              >=  "20061001"))
                       MOVE    WRK-HJO-REC4-2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                   ELSE
                       MOVE    WRK-HJO-REC4 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                   END-IF
                   MOVE    IDT             TO  SPA-GMN-HOJO-MAX
               END-PERFORM
           END-IF
           .
       310111-HKN-RJNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    高齢者負担割合（補助区分）設定処理(開始日H26.5から）
      *****************************************************************
       310112-HKN-HOJOHEN2-SEC            SECTION.
      *
      *    負担割合
           EVALUATE    SPA-GMN-HKNNUM
      *        船員
               WHEN    "002"
                   MOVE    4                   TO  IDY
                   MOVE    11                  TO  WRK-HJO-MAX
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1               TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC1-3 (IDT)
                                               TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
      *        国保
               WHEN    "060"
               WHEN    "068"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   7
                       MOVE    WRK-HJO-REC2 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   MOVE    2                   TO  IDY
                   MOVE    5                   TO  WRK-HJO-MAX
                   MOVE    ZERO                TO  SPA-GMN-HOJO-MAX
                   PERFORM VARYING     IDT     FROM    IDY BY  1
                           UNTIL       IDT     >   WRK-HJO-MAX
                       ADD     1               TO  SPA-GMN-HOJO-MAX
                       MOVE    WRK-HJO-REC5-3 (IDT)
                                               TO  SPA-GMN-HOJOKBNL
                                                    (SPA-GMN-HOJO-MAX)
                   END-PERFORM
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   PERFORM VARYING   IDT     FROM    1  BY  1
                         UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC6 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
      *        その他
               WHEN    OTHER
                   PERFORM VARYING     IDT     FROM    1  BY  1
                           UNTIL       IDT     >   2
                       MOVE    WRK-HJO-REC4-3 (IDT)
                                           TO  SPA-GMN-HOJOKBNL (IDT)
                       MOVE    IDT         TO  SPA-GMN-HOJO-MAX
                   END-PERFORM
           END-EVALUATE
      *
           .
       310112-HKN-HOJOHEN2-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    労災保険編集処理
      *****************************************************************
       310111-PARA-RSIHKN-SET-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
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
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF      PTRSI-HKNID         =   SPA-NAI-HKNID
                   MOVE    PARA-EDANUM     TO  SPA-NAI-RSHKN-EDANUM
                   MOVE    1               TO  FLG-PARA
               ELSE
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       310111-PARA-RSIHKN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費編集処理
      *****************************************************************
       3102-KOUHI-HEN-SEC              SECTION.
      *
           MOVE    IDY                 TO  SPA-KOH-MAX
           MOVE    IDX                 TO  SPA-NAI-PTKOH-EDANUM (IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM (IDX)  TO  SPA-GMN-KOHNUM  (IDY)
           MOVE    SPA-PTKOH-FTNJANUM(IDX) TO  SPA-GMN-FTNJANUM (IDY)
           MOVE    SPA-PTKOH-JKYSNUM (IDX) TO  SPA-GMN-JKYSNUM   (IDY)
      *
           IF      SPA-PTKOH-PAYKBN (IDX)  NOT =   ZERO
               MOVE    SPA-PTKOH-PAYKBN (IDX)(2:1)
                                           TO  SPA-GMN-KOHKBN  (IDY)
           ELSE
               MOVE    SPACE               TO  SPA-GMN-KOHKBN  (IDY)
           END-IF
           MOVE    SPA-PTKOH-PAYKBN (IDX)  TO  SPA-NAI-PTKOH-PAYKBN(IDY)
      *
           MOVE    SPA-PTKOH-TEKSTYMD(IDX) TO  SPA-NAI-KTEKSTYMD(IDY)
           MOVE    SPA-PTKOH-TEKEDYMD(IDX) TO  SPA-NAI-KTEKEDYMD (IDY)
           MOVE    SPA-PTKOH-KAKUNINYMD (IDX)
                                           TO  SPA-NAI-KKAKUNINYMD(IDY)
      *
           MOVE    SPA-PTKOH-TEKSTYMDW (IDX)
                                           TO  SPA-GMN-KTEKSTYMD (IDY)
           MOVE    SPA-PTKOH-TEKEDYMDW (IDX)
                                           TO  SPA-GMN-KTEKEDYMD (IDY)
           MOVE    SPA-PTKOH-KAKUNINYMDW (IDX)
                                           TO  SPA-GMN-KKAKUNINYMD (IDY)
      *
           MOVE    SPA-PTKOH-KOHID (IDX)   TO SPA-NAI-PTKOH-KOHID (IDY)
      *    期限切れ区分
           MOVE    SPA-PTKOH-TEKEDCHKKBN(IDX)
                                           TO  SPA-NAI-TEKEDCHKKBN(IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM(IDX)   TO  SPA-MAE-KOHNUM  (IDY)
           IF      SPA-PTKOH-PAYKBN(IDX)   NOT =   ZERO
               MOVE    SPA-PTKOH-PAYKBN(IDX)(2:1)
                                           TO  SPA-MAE-KOHKBN  (IDY)
           END-IF
           MOVE    SPA-PTKOH-FTNJANUM (IDX)
                                           TO  SPA-MAE-FTNJANUM   (IDY)
           MOVE    SPA-PTKOH-MAEKAKUNINYMD (IDX)
                                           TO  SPA-MAE-KKAKUNINYMD(IDY)
           MOVE    SPA-PTKOH-MAEKAKUNINYMDW(IDX)
                                           TO  SPA-MAE-KKAKUNINYMDW(IDY)
      *    生保重複警告
           MOVE    SPA-PTKOH-JFKCHKFLG (IDX)
                                           TO  SPA-NAI-JFKCHKFLG(IDY)
      *    新規以外の時老人チェック済みとする
           IF      SPA-MAE-KOHNUM (IDY)    NOT =   SPACE
               MOVE    1                   TO  SPA-NAI-AGE65 (IDY)
               MOVE    1                   TO  SPA-NAI-AGECHK(IDY)
               MOVE    1                   TO  SPA-NAI-FTNCHK(IDY)
      *        受給者番号警告
               MOVE    1                   TO  SPA-NAI-JYUCHK(IDY)
      *        生保重複警告
               MOVE    1                   TO  SPA-NAI-JFKCHKFLG(IDY)
           END-IF
           MOVE    SPA-PTKOH-KAKUINFLG (IDX)
                                           TO  SPA-KOH-KAKUINFLG(IDY)
      *
      *    公費情報
           MOVE    SPA-GMN-KOHNUM (IDY)    TO  WRK-HKNNUM
           MOVE    SPA-NAI-KTEKSTYMD(IDY)  TO  WRK-TEKSTYMD
           MOVE    SPA-NAI-KTEKEDYMD(IDY)  TO  WRK-TEKEDYMD
           MOVE    SPA-NAI-PTKOH-PAYKBN(IDY)
                                           TO  WRK-PAYKBN
           IF     (WRK-HKNNUM          =   "027"  )  AND
                  (WRK-PAYKBN          =   ZERO  OR  SPACE )
      *        Ｈ１２．１２．３１までの支払区分は００
               IF      WRK-TEKEDYMD        >=  "20010101"
                   MOVE    SPA-NAI-HOSPSBT         TO  WRK-PAYKBN(1:1)
                   MOVE    SPA-NAI-ROUPAYKBN       TO  WRK-PAYKBN(2:1)
               END-IF
           END-IF
           PERFORM 31012-HKNNUM-SET-SEC
           IF     (SPA-GMN-KOHNUM (IDY)        =   "027" )  AND
                  (SPA-NAI-PTKOH-PAYKBN(IDY)   =   ZERO  )
               MOVE    "老人保健"          TO  WRK-TANSEIDONAME
               MOVE    "3"                 TO  WRK-HKNKOHSBTKBN
           END-IF
           MOVE    WRK-TANSEIDONAME    TO  SPA-GMN-KOHNUMMEI(IDY)
           MOVE    WRK-TANSEIDONAME    TO  SPA-MAE-KOHNUMMEI(IDY)
           MOVE    WRK-HKNKOHSBTKBN    TO  SPA-NAI-PTKOH-SBTKBN(IDY)
      *
      *    保険者番号　検索チェック
           INITIALIZE                    HKNJAINF-REC
           MOVE    SPA-HOSPNUM            TO  HKNJA-HOSPNUM
           MOVE    SPA-PTKOH-FTNJANUM(IDX)
                                         TO  HKNJA-HKNJANUM
      *
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
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNJAINF        =   ZERO
      *        保険者番号の公費種別が違う時
               IF      HKNJA-HKNNUM    NOT =   SPA-GMN-KOHNUM (IDY)
                   MOVE    1               TO  FLG-HKNJAINF
               END-IF
           END-IF
      *
      *    保険者番号あり
           IF      FLG-HKNJAINF        =   ZERO
               MOVE    HKNJA-IDOYMD        TO  SPA-PTKOH-IDOYMD(IDY)
               MOVE    SPA-PTKOH-KEIFLG(IDX)
                                           TO  SPA-NAI-PTKOH-KEIFLG(IDY)
           ELSE
               MOVE    SPACE               TO  SPA-PTKOH-IDOYMD(IDY)
               MOVE    "1"                 TO  SPA-NAI-PTKOH-KEIFLG(IDY)
           END-IF
      *
      *!API
      *    API処理（新規入力とする）
           IF      FLG-APICHK          =   1
                                       OR  2
               IF      SPA-PTKOH-SINKIKBN (IDX)    =   "1"
                   MOVE    SPACE           TO  SPA-MAE-KOHNUM  (IDY)
                   MOVE    SPACE           TO  SPA-MAE-FTNJANUM(IDY)
                   MOVE    SPACE           TO  SPA-NAI-PTKOH-KEIFLG(IDY)
      *            警告チェック済みとする
                   IF      FLG-APICHK          =   1
      *                V2のみ
                   MOVE    1               TO  SPA-NAI-AGE65 (IDY)
                   MOVE    1               TO  SPA-NAI-AGECHK(IDY)
                   MOVE    1               TO  SPA-NAI-FTNCHK(IDY)
      *            受給者番号警告
                   MOVE    1               TO  SPA-NAI-JYUCHK(IDY)
      *
                   END-IF
               END-IF
           END-IF
      *!API
      *
           .
       3102-KOUHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ表示（現在対象分のみ）処理
      *****************************************************************
       3103-HKNCOMB-PARA-SEC        SECTION.
      *
           INITIALIZE                  SPA-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
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
               UNTIL   (FLG-PARA       NOT =   ZERO      )  OR
      *??              (IDX                >=  15        )
                       (IDX                >=  30        )
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
      *
               IF     (COMB-DELKBN     NOT =   "1"       )  AND
                      (SPA-SYSYMD          >=  COMB-TEKSTYMD  AND
                       SPA-SYSYMD          <=  COMB-TEKEDYMD )
                   ADD     1               TO  SPA-HKNCOMBI-MAX
                   ADD     1               TO  IDX
      *
                   MOVE    COMB-HKNCOMBINUM    TO  SPA-GMN-THKNCOMBINUM
                                                              (IDX)
                   MOVE    COMB-HKNNUM         TO  SPA-GMN-THKNNUM
                                                              (IDX)
                   MOVE    COMB-HKNID          TO  SPA-GMN-THKNID
                                                              (IDX)
      *
                   MOVE    COMB-TEKSTYMD       TO  WRK-SYMD
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-TTEKSTYMD
                                                              (IDX)
                   MOVE    COMB-TEKEDYMD       TO  WRK-SYMD
                   PERFORM 410112-SE-WAFORMAT-YMDEDIT-SEC
                   MOVE    WRK-HENYMD          TO  SPA-GMN-TTEKEDYMD
                                                              (IDX)
      *
                   PERFORM 31031-HKNCOMBI-HEN-SEC
                   MOVE    WRK-HKNCOMBI        TO  SPA-GMN-THKNCOMBIMEI
                                                              (IDX)
      *            内部
                   MOVE    COMB-TEKSTYMD       TO  SPA-NAI-TTEKSTYMD
                                                              (IDX)
                   MOVE    COMB-TEKEDYMD       TO  SPA-NAI-TTEKEDYMD
                                                              (IDX)
      *            カルテ用保険指定
                   MOVE    COMB-KOH1HKNNUM     TO  SPA-NAI-TKOH1HKNNUM
                                                              (IDX)
                   MOVE    COMB-KOH2HKNNUM     TO  SPA-NAI-TKOH2HKNNUM
                                                              (IDX)
                   MOVE    COMB-KOH3HKNNUM     TO  SPA-NAI-TKOH3HKNNUM
                                                              (IDX)
                   MOVE    COMB-KOH4HKNNUM     TO  SPA-NAI-TKOH4HKNNUM
                                                              (IDX)
      *            第三者判定用
                   MOVE    COMB-KOH1ID         TO  SPA-NAI-TKOH1ID
                                                              (IDX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    労災保険の時コメントを表示する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBI-MAX
      *        非表示区分設定
               PERFORM 3134-HKNCOMBI-HYOJI-SEC
      *
               IF      SPA-GMN-THKNNUM (IDX)   =   "971"
                                               OR  "973"
                   PERFORM 31032-RSI-HKNCOMBI-HEN-SEC
               END-IF
      *        第三者行為
               IF      SPA-NAI-TKOH1HKNNUM (IDX)   =   "970"
                   PERFORM 31033-DAI3-HKNCOMBI-HEN-SEC
               END-IF
           END-PERFORM
      *
      *    保険適用日順にソートする
           PERFORM 30133-HKNCOMBI-SORT-SEC
      *
           .
       3103-HKNCOMB-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せソート処理
      *****************************************************************
       3134-HKNCOMBI-HYOJI-SEC             SECTION.
      *
           PERFORM VARYING     IDX5    FROM    1   BY  1
                   UNTIL      (IDX5        >   SPA-HYO-MAX )
               IF     (SPA-GMN-THKNCOMBINUM (IDX)
                                       =   SPA-HYO-HKNCOMBINUM(IDX5))
               AND    (SPA-GMN-THKNNUM  (IDX)
                                       =   SPA-HYO-HKNNUM     (IDX5))
               AND    (SPA-NAI-TKOH1HKNNUM (IDX)
                                       =   SPA-HYO-KOH1HKNNUM (IDX5))
               AND    (SPA-NAI-TKOH2HKNNUM (IDX)
                                       =   SPA-HYO-KOH2HKNNUM (IDX5))
               AND    (SPA-NAI-TKOH3HKNNUM (IDX)
                                       =   SPA-HYO-KOH3HKNNUM (IDX5))
               AND    (SPA-NAI-TKOH4HKNNUM (IDX)
                                       =   SPA-HYO-KOH4HKNNUM (IDX5))
                   MOVE    SPA-HYO-HYOJIKBN (IDX5)
                                           TO  SPA-NAI-THYOJIKBN (IDX)
                   MOVE    SPA-HYO-MAX     TO  IDX5
               END-IF
           END-PERFORM
      *
           .
       3134-HKNCOMBI-HYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せソート処理
      *****************************************************************
       30133-HKNCOMBI-SORT-SEC             SECTION.
      *
      *    表示分のみ行う
           INITIALIZE                  WRK-HKNCOBMI-SORT-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   30
               MOVE    ALL "9"         TO  WRK-SRT-TEKISTYMD
               MOVE    ALL "9"         TO  WRK-SRT-HKNCOMBINUM
               MOVE    ZERO            TO  WRK-SRT-IDX
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2     >   SPA-HKNCOMBI-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDX2) =   ZERO
                       OR (SPA-NAI-THYOJIKBN    (IDX2) =   "1"
                                                       OR  "2"
                                                       OR  "3"  )
                       CONTINUE
                   ELSE
                       PERFORM 301331-SORTSYORI-SEC
                   END-IF
               END-PERFORM
               IF      WRK-SRT-IDX     NOT =   ZERO
                   MOVE    SPA-GMN-HKNCOMBI-LIST (WRK-SRT-IDX)
                                       TO  WRK-GMN-HKNCOMBI-LIST(IDX)
                   INITIALIZE              SPA-GMN-HKNCOMBI-LIST
                                                         (WRK-SRT-IDX)
               END-IF
           END-PERFORM
      *
      *    非表示分のみ行う
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   30
               MOVE    ALL "9"         TO  WRK-SRT-TEKISTYMD
               MOVE    ALL "9"         TO  WRK-SRT-HKNCOMBINUM
               MOVE    ZERO            TO  WRK-SRT-IDX
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL       IDX2     >   SPA-HKNCOMBI-MAX
                   IF      SPA-GMN-THKNCOMBINUM (IDX2) =   ZERO
                      AND (SPA-NAI-THYOJIKBN    (IDX2) =   SPACE )
                       CONTINUE
                   ELSE
                       PERFORM 301331-SORTSYORI-SEC
                   END-IF
               END-PERFORM
               IF      WRK-SRT-IDX     NOT =   ZERO
                   MOVE    SPA-GMN-HKNCOMBI-LIST (WRK-SRT-IDX)
                                       TO  WRK-GMN-HKNCOMBI02-LIST(IDX)
                   INITIALIZE              SPA-GMN-HKNCOMBI-LIST
                                                         (WRK-SRT-IDX)
               END-IF
           END-PERFORM
      *
      *    ＳＰＡ編集（表示分）
           MOVE    SPA-GMN-SELNUM      TO  WRK-GMN-SELNUM
           MOVE    ZERO                TO  SPA-GMN-SELNUM
      *
           MOVE    ZERO                TO   SPA-HKNCOMBI-MAX
           INITIALIZE                  SPA-HKNCOMBI-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   30
               IF      WRK-GMN-THKNCOMBINUM (IDX) NOT =   ZERO
                   ADD     1           TO   SPA-HKNCOMBI-MAX
                   MOVE    WRK-GMN-HKNCOMBI-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-LIST
                                                  (SPA-HKNCOMBI-MAX)
      *
                   IF     (WRK-GMN-SELNUM  NOT =   ZERO )
                      AND (WRK-GMN-SELNUM      =   SPA-GMN-THKNCOMBINUM
                                                   (SPA-HKNCOMBI-MAX))
                       MOVE    WRK-GMN-SELNUM  TO  SPA-GMN-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *    ＳＰＡ編集（非表示分）
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   30
               IF      WRK-GMN02-THKNCOMBINUM (IDX) NOT =   ZERO
                   ADD     1           TO   SPA-HKNCOMBI-MAX
                   MOVE    WRK-GMN-HKNCOMBI02-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-LIST
                                                  (SPA-HKNCOMBI-MAX)
      *R02.4
      *        選択が「１　非表示」以外は対象とする
                   IF     (WRK-GMN-SELNUM  NOT =   ZERO )
                      AND (SPA-GMN-SELNUM      =   ZERO )
                      AND (SPA-NAI-THYOJIKBN (SPA-HKNCOMBI-MAX)
                                               =   "2"
                                               OR  "3"   )
                      AND (WRK-GMN-SELNUM      =   SPA-GMN-THKNCOMBINUM
                                                   (SPA-HKNCOMBI-MAX))
                       MOVE    WRK-GMN-SELNUM  TO  SPA-GMN-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       30133-HKNCOMBI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       301331-SORTSYORI-SEC             SECTION.
      *
           IF      WRK-SRT-TEKISTYMD   >   SPA-NAI-TTEKSTYMD  (IDX2)
               MOVE    IDX2                    TO  WRK-SRT-IDX
               MOVE    SPA-NAI-TTEKSTYMD (IDX2) TO  WRK-SRT-TEKISTYMD
               MOVE    SPA-GMN-THKNCOMBINUM(IDX2)
                                               TO  WRK-SRT-HKNCOMBINUM
           ELSE
               IF      WRK-SRT-TEKISTYMD   =   SPA-NAI-TTEKSTYMD(IDX2)
                   IF      WRK-SRT-HKNCOMBINUM
                                               >   SPA-GMN-THKNCOMBINUM
                                                              (IDX2)
                       MOVE    IDX2                TO  WRK-SRT-IDX
                       MOVE    SPA-NAI-TTEKSTYMD (IDX2)
                                                   TO  WRK-SRT-TEKISTYMD
                       MOVE    SPA-GMN-THKNCOMBINUM(IDX2)
                                                   TO
                                                   WRK-SRT-HKNCOMBINUM
                   END-IF
               END-IF
           END-IF
           .
       301331-SORTSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ編集処理
      *****************************************************************
       31031-HKNCOMBI-HEN-SEC             SECTION.
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
           .
       31031-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険組合せ編集処理
      *****************************************************************
       31032-RSI-HKNCOMBI-HEN-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
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
           MOVE    SPACE               TO  WRK-HKNCOMBI
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF      PTRSI-HKNID         =   SPA-GMN-THKNID (IDX)
                   IF      PTRSI-COMMENT       =   SPACE
                       MOVE    SPACE               TO  WRK-HKNCOMBI
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
                       STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           INTO              WRK-HKNCOMBI
                       END-STRING
                   ELSE
                       MOVE    SPACE               TO  WRK-HKNCOMBI
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
                       STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                   DELIMITED  BY  SPACE
                           " "                     DELIMITED  BY  SIZE
                           WRK-HKNKBN-MEI          DELIMITED  BY  SPACE
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                       END-STRING
                   END-IF
                   MOVE    WRK-HKNCOMBI    TO  SPA-GMN-THKNCOMBIMEI
                                                                   (IDX)
                   MOVE    1               TO  FLG-PARA
               END-IF
      *
               IF      FLG-PARA         =   ZERO
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-HKNCOMBI        =   SPACE
               IF      SPA-NAI-RSIONLY     =   ZERO
                   MOVE    1               TO  SPA-NAI-RSIONLY
                   MOVE    "K120"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       31032-RSI-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険組合せ編集処理（第三者行為）
      *****************************************************************
       31033-DAI3-HKNCOMBI-HEN-SEC      SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTRSIINF"          TO  PARA-FILEMEI
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
           MOVE    SPACE               TO  WRK-HKNCOMBI
           PERFORM
               UNTIL   FLG-PARA            =   1
      *
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF      PTRSI-KOHID         =   SPA-NAI-TKOH1ID (IDX)
                   IF      PTRSI-COMMENT   NOT =   SPACE
                       MOVE    SPACE               TO  WRK-HKNCOMBI
                       STRING  SPA-GMN-THKNCOMBIMEI (IDX)
                                                   DELIMITED  BY  "  "
                           "（"                    DELIMITED  BY  SIZE
                           PTRSI-COMMENT(1:24)     DELIMITED  BY  SPACE
                           "）"                    DELIMITED  BY  SIZE
                           INTO              WRK-HKNCOMBI
                       END-STRING
                       MOVE    WRK-HKNCOMBI    TO  SPA-GMN-THKNCOMBIMEI
                                                                   (IDX)
                   END-IF
                   MOVE    1               TO  FLG-PARA
               END-IF
      *
               IF      FLG-PARA         =   ZERO
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-PARA-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       31033-DAI3-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険更新画面処理
      *****************************************************************
       4910-RSKOH-PARAOUT-SEC         SECTION.
      *
      *    処理なし
           IF      SPA-RSPAR-FLG      =   "1"
               MOVE    8              TO  SPA-GMN-CUR
               GO      TO      4910-RSKOH-PARAOUT-EXT
           END-IF
      *
      *    第三者行為　公費登録
           IF     (SPA-NAI2D-HKNNUM    =   "970")
             AND  (SPA-NAI2D-HKNID     =   ZERO )
               PERFORM 49101-RSIKOH-SYORI-SEC
               GO      TO      4910-RSKOH-PARAOUT-EXT
           END-IF
      *
      *    削除処理
           IF      SPA-NAI-SAKJOKBN    =   "1"
               MOVE    "/D"            TO  SPA-GMN-HKNJANUM(1:2)
               PERFORM 4101-KHNJANUM-CHK-SEC
           END-IF
      *    保険画面の内容をＴＢＬへ編集する
           PERFORM 41031-HKN-SPA-TBL-SET-SEC
      *    登録ボタンへ
           MOVE    60                  TO  SPA-GMN-CUR
      *    労災保険情報
           MOVE    SPACE               TO  SPA-P02D-AREA
           INITIALIZE                      SPA-P02D-AREA
      *
           .
       4910-RSKOH-PARAOUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険更新　第三者行為処理
      *****************************************************************
       49101-RSIKOH-SYORI-SEC         SECTION.
      *
           IF      SPA-NAI2D-KOHIADD   =   1
      *        新規追加　１行目に追加
               MOVE    ZERO            TO  SPA-HKNKOH-FLG
               PERFORM 4104-KOH-ADD-SEC
               MOVE    1               TO  SPA-NAI2D-KOHIDX
               MOVE    1               TO  SPA-KOH-GMN2
               MOVE    1               TO  SPA-NAI-KOHSTR
           ELSE
               MOVE    ZERO                TO  FLG-KOHCHK-END
               MOVE    SPA-NAI-KOHSTR      TO  WRK-KOHSTR
               MOVE    SPA-NAI-KOHEND      TO  WRK-KOHEND
               MOVE    ZERO                TO  SPA-NAI-KOHSTR
               PERFORM 
                       UNTIL   (SPA-NAI2D-KOHIDX   >   ZERO )
                          OR   (SPA-ERRCD      NOT =   SPACE )
                          OR   (FLG-KOHCHK-END NOT =   ZERO  )
      *            １件目の公費情報ＴＢＬから編集
                   ADD     1                   TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX         >   SPA-KOH-MAX )
                                 OR   (SPA-NAI2D-KOHIDX
                                                   NOT =   ZERO)
                       IF      SPA-NAI-PTKOH-KOHID (IDX)
                                              =   SPA-NAI2D-KOHID
                           MOVE    IDX         TO  SPA-NAI2D-KOHIDX
                       END-IF
                   END-PERFORM
                   IF     (SPA-NAI-KOHEND      <   SPA-PTKOH-MAX )
                     AND  (SPA-GMN-KOHNUM(3)   NOT =   SPACE )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-KOHCHK-END
                   END-IF
               END-PERFORM
      *
      *        対象がない時元へ
               IF      SPA-NAI2D-KOHIDX    =   ZERO
                   MOVE    WRK-KOHSTR      TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
                   MOVE    "0144"          TO  SPA-ERRCD
                   GO      TO      49101-RSIKOH-SYORI-EXT
               END-IF
           END-IF
           IF      SPA-NAI2D-KOHIDX    >   ZERO
               MOVE    SPA-NAI2D-KOHIDX    TO  IDX
      *
               MOVE    "970"               TO  SPA-GMN-KOHNUM (IDX)
      *        部位を受給者番号へ
               MOVE    SPA-GMN2D-COMMENT   TO  SPA-GMN-JKYSNUM (IDX)
      *        療養開始年月日
               MOVE    SPA-GMN2D-RYOSTYMD  TO  SPA-GMN-KTEKSTYMD (IDX)
               MOVE    SPA-GMN2D-RYOEDYMD  TO  SPA-GMN-KTEKEDYMD (IDX)
               MOVE    SPA-NAI2D-RYOSTYMD  TO  SPA-NAI-KTEKSTYMD (IDX)
               MOVE    SPA-NAI2D-RYOEDYMD  TO  SPA-NAI-KTEKEDYMD (IDX)
      *
               PERFORM VARYING     IDXS    FROM    1   BY  1
                       UNTIL       IDXS    >   SPA-GMN-KOHMAX
                   IF     (SPA-GMN-KOHNUM(IDX)
                                      =    SPA-GMN-KOHNUML (IDXS))
                       MOVE    SPA-GMN-KOHNUMLST (IDXS)
                                           TO  SPA-GMN-KOHNUM-G(IDX)
                       MOVE    SPA-NAI-KOHSBTKBN (IDXS)
                                           TO  SPA-NAI-PTKOH-SBTKBN
                                                              (IDX)
                       MOVE    SPA-NAI-KOHPAYKBN (IDXS)
                                           TO  SPA-NAI-PTKOH-PAYKBN
                                                              (IDX)
                       MOVE    SPA-GMN-KOHMAX  TO  IDXS
                   END-IF
               END-PERFORM
      *        削除処理
               IF      SPA-NAI-SAKJOKBN    =   "1"
                   MOVE    "/D"            TO  SPA-GMN-FTNJANUM (IDX)
      *            公費
                   PERFORM 4102-KOH-CHKALL-SEC
               END-IF
      *        公費画面の内容をＴＢＬへ編集する
               PERFORM 41032-KOH-SPA-TBL-SET-SEC
      *        登録ボタンへ
               MOVE    60                  TO  SPA-GMN-CUR
           END-IF
           MOVE    SPACE               TO  SPA-NAI-SAKJOKBN
      *    労災保険情報
           MOVE    SPACE               TO  SPA-P02D-AREA
           INITIALIZE                      SPA-P02D-AREA
           .
       49101-RSIKOH-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    死亡区分
               WHEN    "CLICKED"       ALSO    "P02_DEATHKBN"
                   PERFORM 490-DEATHKBN-SEC
      *    保険組合せ
               WHEN    "CLICKED"       ALSO    "B05"
                   MOVE    ZERO        TO  FLG-TOUROKU 
                   MOVE    1           TO  FLG-TOUROKU2
                   PERFORM 450-HKNCOMBI-SEC
      *    保険追加
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 460-HKN-TUIKA-SEC
      *    公費追加
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 470-KOH-TUIKA-SEC
      *    確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 490-TOUROKU-SEC
      *    [前↑]押下
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 440-ZEN-GMNHYOUJI-SEC
      *    [次↓]押下
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 450-TUGI-GMNHYOUJI-SEC
      *    保険チェック
               WHEN    "CLICKED"       ALSO    "BH1"
                   PERFORM 450-HKNKAKUNINYMD-ZUMI-SEC
      *    労災・自賠責入力画面へ
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 481-ROUSAI-SEC
      *    公費チェック
               WHEN    "CLICKED"       ALSO    "BK1"
               WHEN    "CLICKED"       ALSO    "BK2"
               WHEN    "CLICKED"       ALSO    "BK3"
                   PERFORM 460-KOHKAKUNINYMD-ZUMI-SEC
      *    期限切れチェック
               WHEN    "CLICKED"       ALSO    "CK1"
               WHEN    "CLICKED"       ALSO    "CK2"
               WHEN    "CLICKED"       ALSO    "CK3"
                   PERFORM 460-TEKEDCHKKBN-CHK-SEC
      *!ver.4.7
      *    保険確認日付履歴
               WHEN    "CLICKED"       ALSO    "BHR"
                   PERFORM 4501-HKNKAKUINYMD-RRK-SEC
      *!API
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *   API保険情報処理
               WHEN    "CLICKED"       ALSO    "API"
               WHEN    "CLICKED"       ALSO    "API4"
                   MOVE    2           TO  FLG-TOUROKU 
                   MOVE    1           TO  FLG-TOUROKU2
                   PERFORM 4199-API-SYORI-SEC
      *   APIV3保険情報処理
               WHEN    "CLICKED"       ALSO    "APIV3"
                   MOVE    2           TO  FLG-TOUROKU 
                   MOVE    1           TO  FLG-TOUROKU2
                   PERFORM 4198-APIV3-SYORI-SEC
      *   APIV3労災保険情報処理
               WHEN    "CLICKED"       ALSO    "APIV3D"
                   MOVE    2           TO  FLG-TOUROKU 
                   MOVE    1           TO  FLG-TOUROKU2
                   PERFORM 41910-APIV3-RSISYORI-SEC
      *!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      *!API
      *    別画面
               WHEN    "CLICKED"       ALSO     ANY
                   PERFORM 4101-BETUGAMEN-SEC
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
           PERFORM 400-GMN-INPUT-SEC
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
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
      *****MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    保険組合せ選択
               WHEN    ANY             ALSO    "HKNLIST"
                   PERFORM 4001-HKNLIST-SEL-SEC
           END-EVALUATE
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4001-HKNLIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-GMN-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   60
               IF      P02-HKNLIST-SELECT (IDX)    =   "T"
                   IF     (P02-HKNCOBNUM (IDX)     =   SPACE ) AND
                          (IDX                     >   1     )
                       MOVE    P02-HKNCOBNUM (IDX - 1) TO  WRK-SELNUM
                   ELSE
                       MOVE    P02-HKNCOBNUM (IDX)     TO  WRK-SELNUM
                   END-IF
                   IF     (WRK-SELNUM          NUMERIC   )  AND
                          (WRK-SELNUM      NOT =   SPA-GMN-SELNUM )
                       MOVE    WRK-SELNUM      TO  WRK-GMN-SELNUM
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-GMN-SELNUM      >   ZERO
               MOVE    WRK-GMN-SELNUM      TO  P02-P02-SELNUM
               MOVE    38                  TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4001-HKNLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *H28.5
           IF      FLG-P027       =   1
               CONTINUE
           ELSE
      *H28.5
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           END-IF
      *
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               CONTINUE
           ELSE
               PERFORM 410-INPUT-CHK2-SEC
      *        タグ切り替えの時、期間重複はクリアする
               IF     (SPA-TAGU-FLG        =   1 )
                   OR (FLG-ERRCHK          =   1 )
                   IF      SPA-ERRCD       =   "0064"  OR
                                               "0089"
                       MOVE   SPACE            TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       410-INPUT-CHK2-SEC          SECTION.
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
               PERFORM 4109-TOUROKU-CHK-SEC
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO     TO      410-INPUT-CHK2-EXT
           END-IF
      *
      *    保険・公費のテーブルセットとチェック
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-HKNKOH-FLG      =   1
                   PERFORM 4103-HKNKOH-TBL-SEC
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               CONTINUE
           ELSE
      *
      *        公費に追加があった時、最初の１行を追加する
               IF      FLG-KOH-ADD         =   1
                   PERFORM 4104-KOH-ADD-SEC
               END-IF
           END-IF
           .
      *
       410-INPUT-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-P02A-FLG1
           MOVE    1                   TO  SPA-P02A-FLG2
           MOVE    1                   TO  SPA-P02G-FLG
      *
      *    かな氏名
           IF      P02-P02-KANANAME    =   SPA-MAE-KANANAME
               MOVE    ZERO                TO  FLG-IN-KANANAME
           ELSE
               MOVE    1                   TO  FLG-IN-KANANAME
               MOVE    ZERO                TO  SPA-NAI-KANAFLG
               MOVE    ZERO                TO  SPA-P02A-FLG1
               MOVE    ZERO                TO  SPA-P02A-FLG2
               MOVE    ZERO                TO  SPA-P02G-FLG
           END-IF
           MOVE    P02-P02-KANANAME    TO  SPA-GMN-KANANAME
      *    漢字氏名
           IF      P02-P02-NAME        =   SPA-MAE-NAME
               MOVE    ZERO                TO  FLG-IN-NAME
           ELSE
               MOVE    1                   TO  FLG-IN-NAME
               MOVE    ZERO                TO  SPA-P02A-FLG1
               MOVE    ZERO                TO  SPA-P02A-FLG2
               MOVE    ZERO                TO  SPA-P02G-FLG
      *ver.4.7
               MOVE    ZERO                TO  SPA-NAI-KYUSEIKBN
           END-IF
           MOVE    P02-P02-NAME        TO  SPA-GMN-NAME
      *    氏名変更
           IF      SPA-GMN-NAME    NOT =   SPA-HZN-NAME
               MOVE    1                   TO  SPA-HEN-FLG
           END-IF
      *
           MOVE    P02-P02-NICKNAME    TO  SPA-GMN-NICKNAME
      *
           IF      P02-P02-SEX(1:1)    =   SPA-MAE-SEX
               MOVE    ZERO                TO  FLG-IN-SEX
           ELSE
               MOVE    1                   TO  FLG-IN-SEX
               MOVE    ZERO                TO  SPA-P02A-FLG2
               MOVE    ZERO                TO  SPA-P02G-FLG
           END-IF
           MOVE    P02-P02-SEX         TO  SPA-GMN-SEX-G
      *
           IF      P02-P02-BIRTHDAY    =   SPA-MAE-BIRTHDAY
               MOVE    ZERO                TO  FLG-IN-BIRTHDAY
           ELSE
               MOVE    1                   TO  FLG-IN-BIRTHDAY
               MOVE    ZERO                TO  SPA-P02A-FLG2
               MOVE    ZERO                TO  SPA-P02G-FLG
           END-IF
           MOVE    P02-P02-BIRTHDAY    TO  SPA-GMN-BIRTHDAYW
      *
           MOVE    P02-TSTPTNUMKBN     TO  SPA-GMN-TSTPTNUMKBN-G
           MOVE    P02-P02-HAKKO       TO  SPA-GMN-HAKKO-G
           MOVE    P02-P02-USERPG      TO  SPA-GMN-USERPG-G
      *
           IF      P02-P02-POST        =   SPA-MAE-POST
               MOVE    ZERO                TO  FLG-IN-POST
           ELSE
               MOVE    1                   TO  FLG-IN-POST
           END-IF
      *
           IF      P02-P02-DISCOUNTKBN(1:2)
                                           =   SPA-MAE-DISCOUNTKBN
               MOVE    ZERO                TO  FLG-IN-DISCOUNTKBN
           ELSE
               MOVE    1                   TO  FLG-IN-DISCOUNTKBN
           END-IF
      *
           MOVE    P02-P02-POST        TO  SPA-GMN-POST
           MOVE    P02-P02-ADRS        TO  SPA-GMN-EDITADRS-NAME
           MOVE    P02-P02-TEL2        TO  SPA-GMN-TEL2
           MOVE    P02-P02-BANTI       TO  SPA-GMN-BANTI
           MOVE    P02-P02-TEL1        TO  SPA-GMN-TEL1
           MOVE    P02-P02-SETAINUSI   TO  SPA-GMN-SETAINUSI
           MOVE    P02-P02-ZOKUGARA    TO  SPA-GMN-ZOKUGARA
           MOVE    P02-P02-DISCOUNTKBN TO  SPA-GMN-DISCOUNTKBN-G
           MOVE    P02-P02-DISCOUNT    TO  SPA-GMN-DISCOUNT
           MOVE    P02-P02-CONDITION1  TO  SPA-GMN-CONDITION(1)
           MOVE    P02-P02-CONDITION2  TO  SPA-GMN-CONDITION(2)
           MOVE    P02-P02-CONDITION3  TO  SPA-GMN-CONDITION(3)
           MOVE    P02-P02-NYUKIN      TO  SPA-GMN-NYUKIN-HOHO-G
           MOVE    P02-P02-SKYPRTFLG   TO  SPA-GMN-SKYPRTFLG-G
           MOVE    P02-P02-COMMENT1    TO  SPA-GMN-COMMENT-1
           MOVE    P02-P02-COMMENT2    TO  SPA-GMN-COMMENT-2
           MOVE    P02-P02-TABOO1      TO  SPA-GMN-TABOO-1
           MOVE    P02-P02-TABOO2      TO  SPA-GMN-TABOO-2
           MOVE    P02-P02-ALLERGY1    TO  SPA-GMN-ALLERGY-1
           MOVE    P02-P02-ALLERGY2    TO  SPA-GMN-ALLERGY-2
           MOVE    P02-P02-KANSENSYO1  TO  SPA-GMN-KANSENSYO-1
           MOVE    P02-P02-KANSENSYO2  TO  SPA-GMN-KANSENSYO-2
      *    出生時体重
           MOVE    P02-P02-BIRTHWEIGHT TO  SPA-GMN-BIRTHWEIGHT
      *
           MOVE    P02-P02-SELNUM      TO  SPA-GMN-SELNUM
           MOVE    P02-P02-SRYKA       TO  SPA-GMN-SRYKA-G
      *    保険情報
           IF     (SPA-MAE-HKNNUM      =   "971"
                                       OR  "973" )
             OR   (P02-P02-HKNNUM(1:3) =    "971"
                                       OR  "973" )
      *        労災の時、保険情報に入力がないこと
               PERFORM 41011-ROUSAI-CHK-SEC
           ELSE
      *        保険者番号変更時に異動チェックをクリアする
               IF      P02-P02-HKNJANUM    NOT =   SPA-GMN-HKNJANUM
                   MOVE    SPACE               TO  SPA-HKNJA-KEIFLG
               END-IF
               IF      P02-P02-HONKZKBN(1:1)   NOT = SPA-GMN-HONKZKKBN
                   MOVE    SPA-GMN-HONKZKKBN   TO  SPA-MAE-HONKZKKBN
               END-IF
               MOVE    P02-P02-HKNJANUM    TO  SPA-GMN-HKNJANUM
               MOVE    P02-P02-HKNNUM      TO  SPA-GMN-HKNNUM-G
               MOVE    P02-P02-HONKZKBN    TO  SPA-GMN-HONKZKKBN-G
               MOVE    P02-P02-HOJOKBN     TO  SPA-GMN-HOJOKBN-G
               MOVE    P02-P02-CONTKBN     TO  SPA-GMN-CONTKBN-G
               MOVE    P02-P02-KIGO        TO  SPA-GMN-KIGO
               MOVE    P02-P02-NUM         TO  SPA-GMN-NUM
      *R02.4
               MOVE    P02-P02-EDABAN      TO  SPA-GMN-EDABAN
               MOVE    P02-P02-SKKGETYMD   TO  SPA-GMN-SKKGETYMD
      *        開始日変更で警告クリア
               IF      P02-P02-HTEKSTYMD   NOT =   SPA-GMN-HTEKSTYMD
                   MOVE    ZERO                TO  SPA-ROU-KEIFLG
               END-IF
               MOVE    P02-P02-HTEKSTYMD   TO  SPA-GMN-HTEKSTYMD
               MOVE    P02-P02-HTEKEDYMD   TO  SPA-GMN-HTEKEDYMD
               MOVE    P02-P02-HIHKNJANAME TO  SPA-GMN-HIHKNJANAME
      *        確認日変更で警告クリア
               IF      P02-P02-HKAKUNINYMD NOT =   SPA-GMN-HKAKUNINYMD
                   MOVE    ZERO                TO  SPA-HKN-KAKUINFLG
               END-IF
               MOVE    P02-P02-HKAKUNINYMD TO  SPA-GMN-HKAKUNINYMD
      *
           END-IF
      *
      *    公費　第三者行為
           IF     (SPA-GMN-KOHNUM (1)      =   "970" )
             OR   (P02-P02-KOHNUM1(1:3)    =   "970" )
      *        労災の時、保険情報に入力がないこと
               IF     (P02-P02-FTNJANUM1   =   SPA-GMN-FTNJANUM (1))
                 AND  (P02-P02-KOHNUM1     =   SPA-GMN-KOHNUM-G (1))
                 AND  (P02-P02-JKYSNUM1    =   SPA-GMN-JKYSNUM  (1))
                 AND  (P02-P02-KTEKSTYMD1  =   SPA-GMN-KTEKSTYMD(1))
                 AND  (P02-P02-KTEKEDYMD1  =   SPA-GMN-KTEKEDYMD(1))
                   MOVE    ZERO            TO  FLG-KOH-01
               ELSE
                   MOVE    1               TO  FLG-KOH-01
               END-IF
               MOVE    SPA-GMN-JKYSNUM  (1)   TO  P02-P02-JKYSNUM1
               MOVE    SPA-GMN-KOHNUM-G (1)   TO  P02-P02-KOHNUM1
               MOVE    SPA-GMN-KTEKSTYMD (1)  TO  P02-P02-KTEKSTYMD1
               MOVE    SPA-GMN-KTEKEDYMD (1)  TO  P02-P02-KTEKEDYMD1
           END-IF
      *
           IF     (SPA-GMN-KOHNUM (2)      =   "970" )
             OR   (P02-P02-KOHNUM2(1:3)    =   "970" )
      *        労災の時、保険情報に入力がないこと
               IF     (P02-P02-FTNJANUM2   =   SPA-GMN-FTNJANUM (2))
                 AND  (P02-P02-KOHNUM2     =   SPA-GMN-KOHNUM-G (2))
                 AND  (P02-P02-JKYSNUM2    =   SPA-GMN-JKYSNUM  (2))
                 AND  (P02-P02-KTEKSTYMD2  =   SPA-GMN-KTEKSTYMD(2))
                 AND  (P02-P02-KTEKEDYMD2  =   SPA-GMN-KTEKEDYMD(2))
                   MOVE    ZERO            TO  FLG-KOH-02
               ELSE
                   MOVE    1               TO  FLG-KOH-02
               END-IF
               MOVE    SPA-GMN-JKYSNUM  (2)   TO  P02-P02-JKYSNUM2
               MOVE    SPA-GMN-KOHNUM-G (2)   TO  P02-P02-KOHNUM2
               MOVE    SPA-GMN-KTEKSTYMD (2)  TO  P02-P02-KTEKSTYMD2
               MOVE    SPA-GMN-KTEKEDYMD (2)  TO  P02-P02-KTEKEDYMD2
           END-IF
      *
           IF     (SPA-GMN-KOHNUM (3)      =   "970" )
             OR   (P02-P02-KOHNUM3(1:3)    =   "970" )
      *        労災の時、保険情報に入力がないこと
               IF     (P02-P02-FTNJANUM3   =   SPA-GMN-FTNJANUM (3))
                 AND  (P02-P02-KOHNUM3     =   SPA-GMN-KOHNUM-G (3))
                 AND  (P02-P02-JKYSNUM3    =   SPA-GMN-JKYSNUM  (3))
                 AND  (P02-P02-KTEKSTYMD3  =   SPA-GMN-KTEKSTYMD(3))
                 AND  (P02-P02-KTEKEDYMD3  =   SPA-GMN-KTEKEDYMD(3))
                   MOVE    ZERO            TO  FLG-KOH-03
               ELSE
                   MOVE    1               TO  FLG-KOH-03
               END-IF
               MOVE    SPA-GMN-JKYSNUM  (3)   TO  P02-P02-JKYSNUM3
               MOVE    SPA-GMN-KOHNUM-G (3)   TO  P02-P02-KOHNUM3
               MOVE    SPA-GMN-KTEKSTYMD (3)  TO  P02-P02-KTEKSTYMD3
               MOVE    SPA-GMN-KTEKEDYMD (3)  TO  P02-P02-KTEKEDYMD3
           END-IF
      *
      *    保険者番号変更時に異動チェックをクリアする
           IF      P02-P02-FTNJANUM1   NOT =   SPA-GMN-FTNJANUM (1)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(1)
           END-IF
           IF      P02-P02-FTNJANUM2   NOT =   SPA-GMN-FTNJANUM (2)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(2)
           END-IF
           IF      P02-P02-FTNJANUM3   NOT =   SPA-GMN-FTNJANUM (3)
               MOVE    SPACE               TO  SPA-NAI-PTKOH-KEIFLG(3)
           END-IF
      *    年齢チェックをクリアする
           IF     (P02-P02-FTNJANUM1   NOT =   SPA-GMN-FTNJANUM (1)) OR
                  (P02-P02-KOHNUM1     NOT =   SPA-GMN-KOHNUM-G (1)) OR
                  (P02-P02-KTEKSTYMD1  NOT =   SPA-GMN-KTEKSTYMD(1)) OR
                  (P02-P02-KTEKEDYMD1  NOT =   SPA-GMN-KTEKEDYMD(1))
      *H28.11  行追加のみは変更なしとする
             IF     (P02-P02-FTNJANUM1(1:2)  =   "/A"
                                             OR  "/a" )
               AND  (P02-P02-KOHNUM1         =   SPA-GMN-KOHNUM-G(1))
               AND  (P02-P02-KTEKSTYMD1      =   SPA-GMN-KTEKSTYMD(1))
               AND  (P02-P02-KTEKEDYMD1      =   SPA-GMN-KTEKEDYMD(1))
                 CONTINUE
             ELSE
      *
               MOVE    ZERO                TO  SPA-NAI-AGECHK   (1)
      *        重複チェック
               MOVE    ZERO                TO  SPA-NAI-JFKCHKFLG (1)
             END-IF
      *
           END-IF
           IF     (P02-P02-FTNJANUM2   NOT =   SPA-GMN-FTNJANUM (2)) OR
                  (P02-P02-KOHNUM2     NOT =   SPA-GMN-KOHNUM-G (2)) OR
                  (P02-P02-KTEKSTYMD2  NOT =   SPA-GMN-KTEKSTYMD(2)) OR
                  (P02-P02-KTEKEDYMD2  NOT =   SPA-GMN-KTEKEDYMD(2))
      *H28.11  行追加のみは変更なしとする
             IF     (P02-P02-FTNJANUM2(1:2)  =   "/A"
                                             OR  "/a" )
               AND  (P02-P02-KOHNUM2        =   SPA-GMN-KOHNUM-G(2))
               AND  (P02-P02-KTEKSTYMD2     =   SPA-GMN-KTEKSTYMD(2))
               AND  (P02-P02-KTEKEDYMD2     =   SPA-GMN-KTEKEDYMD(2))
               CONTINUE
             ELSE
      *
               MOVE    ZERO                TO  SPA-NAI-AGECHK   (2)
      *        重複チェック
               MOVE    ZERO                TO  SPA-NAI-JFKCHKFLG (2)
             END-IF
           END-IF
           IF     (P02-P02-FTNJANUM3   NOT =   SPA-GMN-FTNJANUM (3)) OR
                  (P02-P02-KOHNUM3     NOT =   SPA-GMN-KOHNUM-G (3)) OR
                  (P02-P02-KTEKSTYMD3  NOT =   SPA-GMN-KTEKSTYMD(3)) OR
                  (P02-P02-KTEKEDYMD3  NOT =   SPA-GMN-KTEKEDYMD(3))
      *H28.11  行追加のみは変更なしとする
             IF     (P02-P02-FTNJANUM3(1:2)  =   "/A"
                                               OR  "/a" )
               AND  (P02-P02-KOHNUM3         =   SPA-GMN-KOHNUM-G(3))
               AND  (P02-P02-KTEKSTYMD3      =   SPA-GMN-KTEKSTYMD(3))
               AND  (P02-P02-KTEKEDYMD3      =   SPA-GMN-KTEKEDYMD(3))
               CONTINUE
             ELSE
      *
               MOVE    ZERO                TO  SPA-NAI-AGECHK   (3)
      *        重複チェック
               MOVE    ZERO                TO  SPA-NAI-JFKCHKFLG (3)
      *
             END-IF
           END-IF
      *    負担者番号警告クリアする
           IF     (P02-P02-FTNJANUM1   NOT =   SPA-GMN-FTNJANUM (1))
               MOVE    ZERO                TO  SPA-NAI-FTNCHK   (1)
           END-IF
           IF     (P02-P02-FTNJANUM2   NOT =   SPA-GMN-FTNJANUM (2))
               MOVE    ZERO                TO  SPA-NAI-FTNCHK   (2)
           END-IF
           IF     (P02-P02-FTNJANUM3   NOT =   SPA-GMN-FTNJANUM (3))
               MOVE    ZERO                TO  SPA-NAI-FTNCHK   (3)
           END-IF
      *    受給者者番号警告クリアする
           IF     (P02-P02-JKYSNUM1    NOT =   SPA-GMN-JKYSNUM (1))
               MOVE    ZERO                TO  SPA-NAI-JYUCHK  (1)
           END-IF
           IF     (P02-P02-JKYSNUM2    NOT =   SPA-GMN-JKYSNUM (2))
               MOVE    ZERO                TO  SPA-NAI-JYUCHK  (2)
           END-IF
           IF     (P02-P02-JKYSNUM3    NOT =   SPA-GMN-JKYSNUM (3))
               MOVE    ZERO                TO  SPA-NAI-JYUCHK  (3)
           END-IF
      *    公費
           MOVE    P02-P02-KOHNUM1     TO  WRK-P02-KOHNUM-G (1)
           MOVE    P02-P02-KOHNUM2     TO  WRK-P02-KOHNUM-G (2)
           MOVE    P02-P02-KOHNUM3     TO  WRK-P02-KOHNUM-G (3)
           IF      P02-P02-KOHNUM1(1:1)    =   "/"
               MOVE    SPA-GMN-KOHNUM-G (1)    TO  P02-P02-KOHNUM1
           END-IF
           IF      P02-P02-KOHNUM2(1:1)    =   "/"
               MOVE    SPA-GMN-KOHNUM-G (2)    TO  P02-P02-KOHNUM2
           END-IF
           IF      P02-P02-KOHNUM3(1:1)    =   "/"
               MOVE    SPA-GMN-KOHNUM-G (3)    TO  P02-P02-KOHNUM3
           END-IF
      *
      *    公費１
           MOVE    P02-P02-FTNJANUM1   TO  SPA-GMN-FTNJANUM (1)
           MOVE    P02-P02-KOHNUM1     TO  SPA-GMN-KOHNUM-G (1)
           MOVE    P02-P02-JKYSNUM1    TO  SPA-GMN-JKYSNUM  (1)
           IF      P02-P02-KTEKSTYMD1  NOT =   SPA-GMN-KTEKSTYMD (1)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (1)
           END-IF
           MOVE    P02-P02-KTEKSTYMD1  TO  SPA-GMN-KTEKSTYMD (1)
           MOVE    P02-P02-KTEKEDYMD1  TO  SPA-GMN-KTEKEDYMD (1)
           IF      P02-P02-KKAKUNINYMD1
                                   NOT =   SPA-GMN-KKAKUNINYMD(1)
               MOVE    ZERO            TO  SPA-KOH-KAKUINFLG (1)
           END-IF
           MOVE    P02-P02-KKAKUNINYMD1
                                       TO  SPA-GMN-KKAKUNINYMD(1)
      *    公費２
           MOVE    P02-P02-FTNJANUM2   TO  SPA-GMN-FTNJANUM (2)
           MOVE    P02-P02-KOHNUM2     TO  SPA-GMN-KOHNUM-G (2)
           MOVE    P02-P02-JKYSNUM2    TO  SPA-GMN-JKYSNUM  (2)
           IF      P02-P02-KTEKSTYMD2  NOT =   SPA-GMN-KTEKSTYMD (2)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (2)
           END-IF
           MOVE    P02-P02-KTEKSTYMD2  TO  SPA-GMN-KTEKSTYMD (2)
           MOVE    P02-P02-KTEKEDYMD2  TO  SPA-GMN-KTEKEDYMD (2)
           IF      P02-P02-KKAKUNINYMD2
                                   NOT =   SPA-GMN-KKAKUNINYMD(2)
               MOVE    ZERO            TO  SPA-KOH-KAKUINFLG (2)
           END-IF
           MOVE    P02-P02-KKAKUNINYMD2
                                       TO  SPA-GMN-KKAKUNINYMD(2)
      *    公費３
           MOVE    P02-P02-FTNJANUM3   TO  SPA-GMN-FTNJANUM (3)
           MOVE    P02-P02-KOHNUM3     TO  SPA-GMN-KOHNUM-G (3)
           MOVE    P02-P02-JKYSNUM3    TO  SPA-GMN-JKYSNUM  (3)
           IF      P02-P02-KTEKSTYMD3  NOT =   SPA-GMN-KTEKSTYMD (3)
               MOVE    ZERO             TO  SPA-NAI-AGE65    (3)
               MOVE    ZERO             TO  SPA-NAI-AGECHK   (3)
           END-IF
           MOVE    P02-P02-KTEKSTYMD3  TO  SPA-GMN-KTEKSTYMD (3)
           MOVE    P02-P02-KTEKEDYMD3  TO  SPA-GMN-KTEKEDYMD (3)
           IF      P02-P02-KKAKUNINYMD3
                                   NOT =   SPA-GMN-KKAKUNINYMD(3)
               MOVE    ZERO            TO  SPA-KOH-KAKUINFLG (3)
           END-IF
           MOVE    P02-P02-KKAKUNINYMD3
                                       TO  SPA-GMN-KKAKUNINYMD(3)
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災の保険情報入力チェック処理
      *****************************************************************
       41011-ROUSAI-CHK-SEC          SECTION.
      *
           IF     (P02-P02-HKNJANUM    =   SPA-GMN-HKNJANUM ) AND
                  (P02-P02-HKNNUM      =   SPA-GMN-HKNNUM-G ) AND
                  (P02-P02-HONKZKBN    =   SPA-GMN-HONKZKKBN-G) AND
                  (P02-P02-HOJOKBN     =   SPA-GMN-HOJOKBN-G) AND
                  (P02-P02-CONTKBN     =   SPA-GMN-CONTKBN-G) AND
                  (P02-P02-KIGO        =   SPA-GMN-KIGO     ) AND
                  (P02-P02-NUM         =   SPA-GMN-NUM      ) AND
      *R02.4
                  (P02-P02-EDABAN      =   SPA-GMN-EDABAN  ) AND
                  (P02-P02-SKKGETYMD   =   SPA-GMN-SKKGETYMD) AND
                  (P02-P02-HTEKSTYMD   =   SPA-GMN-HTEKSTYMD) AND
                  (P02-P02-HTEKEDYMD   =   SPA-GMN-HTEKEDYMD) AND
                  (P02-P02-HIHKNJANAME =   SPA-GMN-HIHKNJANAME) AND
                  (P02-P02-HKAKUNINYMD =   SPA-GMN-HKAKUNINYMD)
               MOVE    ZERO                TO  FLG-IN-HKN
           ELSE
               MOVE    P02-P02-HKNJANUM    TO  SPA-GMN-HKNJANUM
               MOVE    1                   TO  FLG-IN-HKN
           END-IF
           .
      *
       41011-ROUSAI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェックと別画面処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    カナ氏名
           PERFORM 4101-KANANAME-CHK-SEC
      *
      *    漢字氏名
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-NAME-CHK-SEC
           END-IF
      *
      *    性別
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-SEX-CHK-SEC
           END-IF
      *    生年月日
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-BIRTHDAY-CHK-SEC
           END-IF
      *    通称
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-NICKNAME-CHK-SEC
           END-IF
      *    テスト患者
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-TSTPTNUMKBN-CHK-SEC
           END-IF
      *    カルテ発行
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-HAKKO-CHK-SEC
           END-IF
      *    Ｕ・Ｐチェック処理
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-USERPG-CHK-SEC
           END-IF
      *    保険者番号
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-KHNJANUM-CHK-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               IF      FLG-HKN-DEL         =   ZERO
      *            保険情報チェックサブ
                   CALL    "ORCSP03A"      USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
      *            タグ切り替えの時、期間重複はクリアする
                   IF     (SPA-TAGU-FLG        =   1 )
                       OR (FLG-ERRCHK          =   1 )
                       IF      SPA-ERRCD       =   "0064"  OR
                                                   "0089"
                           MOVE   SPACE            TO  SPA-ERRCD
                       END-IF
                       IF      SPA-PIDCD   NOT =   SPACE
                           MOVE   SPACE            TO  SPA-PIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費
           PERFORM 4102-KOH-CHKALL-SEC
      *    公費法別一覧編集画面へ
           IF     (SPA-FLG-END     NOT =   ZERO  )  AND
                  (SPA-P02H-HBTNUM NOT =   SPACE )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    公費情報チェックサブ
           CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
      *    タグ切り替えの時、期間重複はクリアする
           IF      SPA-TAGU-FLG        =   1
               OR (FLG-ERRCHK          =   1 )
               IF      SPA-ERRCD       =   "0088"  OR
                                           "0089"
                   MOVE   SPACE            TO  SPA-ERRCD
               END-IF
               IF      SPA-PIDCD   NOT =   SPACE
                   MOVE   SPACE            TO  SPA-PIDCD
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *!!!
      *    公費法別一覧編集画面へ
           IF     (SPA-FLG-END     NOT =   ZERO  )  AND
                  (SPA-P02H-HBTNUM NOT =   SPACE )
               PERFORM 41021-P02H-SYORI-SEC
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
      *
      *    郵便番号
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-POST-CHK-SEC
           END-IF
      *    住所
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-ADRS-CHK-SEC
           END-IF
      *    番地方書
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-BANTI-CHK-SEC
           END-IF
      *ver.4.7
      *    電話番号
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-TEL-CHK-SEC
           END-IF
      *    世帯主
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-SETAINUSI-CHK-SEC
           END-IF
      *    続柄
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-ZOKUGARA-CHK-SEC
           END-IF
      *    減免事由
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-DISCOUNTKBN-CHK-SEC
           END-IF
      *    割引率
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-DISCOUNT-CHK-SEC
           END-IF
      *    入金方法
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-NYUKIN-HOHO-CHK-SEC
           END-IF
      *    状態１から３
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-CONDITIONH-SET-SEC
           END-IF
      *    状態１から３
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-CONDITIONH-SET-SEC
           END-IF
      *    請求書発行区分
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-SKYPRTFLG-CHK-SEC
           END-IF
      *    出生時体重
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-BIRTHWEIGHT-CHK-SEC
           END-IF
      *    禁忌他
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-COMMENT-CHK-SEC
           END-IF
      *    保険組合せ選択
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-SELNUM-CHK-SEC
           END-IF
      *    診療科選択
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-FLG-END         =   ZERO  )
               PERFORM 4101-SRYKA-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費法別一覧編集画面処理
      *****************************************************************
       41021-P02H-SYORI-SEC   SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02H"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           IF      SPA-P02H-MAX        >   1
               MOVE    "SELNUM"            TO  MCP-WIDGET
               MOVE    "P02H"              TO  SPA-SAKIPG
               MOVE    "P02"               TO  SPA-MOTOPG
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02H"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
           END-IF
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       41021-P02H-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号入力チェック処理
      *****************************************************************
       4101-KHNJANUM-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-DEL
      *    労災で入力があった時、エラー
           IF      FLG-IN-HKN          =   1
      *
               EVALUATE    SPA-GMN-HKNJANUM(1:2)
                   WHEN    "/A"
                   WHEN    "/a"
      *                追加はＯＫ
                       INITIALIZE                  SPA-GMN-HKN-AREA
                       MOVE    ZERO                TO  SPA-NAI-HKNSTR
                       MOVE    8                   TO  SPA-GMN-CUR
                       MOVE    1                   TO  FLG-HKN-DEL
                   WHEN    "/D"
                   WHEN    "/d"
      *                削除は労災保険画面へ
                       MOVE    SPACE               TO  SPA-GMN-HKNJANUM
                       MOVE    "0043"              TO  SPA-ERRCD
                       MOVE    61                  TO  SPA-GMN-CUR
                   WHEN    OTHER
                       MOVE    SPACE               TO  SPA-GMN-HKNJANUM
                       MOVE    "0043"              TO  SPA-ERRCD
                       MOVE    61                  TO  SPA-GMN-CUR
               END-EVALUATE
               GO      TO      4101-KHNJANUM-CHK-EXT
           END-IF
      *    削除の判定
           IF      SPA-GMN-HKNJANUM(1:2)   =   "/D"  OR  "/d"
      *D       労災の時、労災保険画面へ
      *        IF      SPA-GMN-HKNNUM      =   "971"
      *                                    OR  "973"
      *            MOVE    "0043"          TO  SPA-ERRCD
      *            MOVE    61              TO  SPA-GMN-CUR
      *            GO      TO      4101-KHNJANUM-CHK-EXT
      *D       END-IF
      *        新規の時、ＴＢＬも削除
               IF     (SPA-NAI-HKNSTR      >   ZERO      ) AND
                      (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR)
                                           =    "1"           )
                   MOVE    SPACE           TO  SPA-PTHKNINF-TBL
                                                     (SPA-NAI-HKNSTR)
                   INITIALIZE                  SPA-PTHKNINF-TBL
                                                     (SPA-NAI-HKNSTR)
      *            保険テーブル空白行削除処理
                   PERFORM 410311-HKNTBL-DEL-SEC
               ELSE
                   IF      SPA-NAI-HKNSTR      >   ZERO
                       MOVE    "1"             TO  SPA-PTHKN-SAKJOKBN
                                                       (SPA-NAI-HKNSTR)
                       MOVE    "1"             TO  SPA-NAI-SAKJOKBN
                   END-IF
               END-IF
               MOVE    1                   TO  FLG-HKN-DEL
               INITIALIZE                  SPA-GMN-HKN-AREA
               IF      SPA-NAI-HKNSTR      =   ZERO
                   MOVE    1               TO  SPA-NAI-HKNSTR
               END-IF
      *        次の対象を表示する
      *        保険情報ＴＢＬから編集
               PERFORM 3101-HKN-TBL-SPA-SEC
               MOVE    8                   TO  SPA-GMN-CUR
               GO      TO      4101-KHNJANUM-CHK-EXT
           END-IF
      *
      *    追加の判定
           IF      SPA-GMN-HKNJANUM(1:2)   =   "/A"  OR  "/a"
               INITIALIZE                  SPA-GMN-HKN-AREA
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
               MOVE    8                   TO  SPA-GMN-CUR
               MOVE    1                   TO  FLG-HKN-DEL
           END-IF
      *    複写の判定
           IF      SPA-GMN-HKNJANUM(1:2)   =   "/C"  OR  "/c"
      *        補助区分、有効期間は複写しない
               MOVE    SPA-NAI-HKNSTR      TO  IDX
               MOVE    SPA-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
               MOVE    SPACE               TO  SPA-GMN-HTEKSTYMD
               MOVE    SPACE               TO  SPA-GMN-HTEKEDYMD
               MOVE    SPACE               TO  SPA-GMN-HOJOKBN-G
               MOVE    SPACE               TO  SPA-GMN-HKAKUNINYMD
               MOVE    ZERO                TO  SPA-NAI-HKNID
               MOVE    SPACE               TO  SPA-NAI-HTEKSTYMD
               MOVE    SPACE               TO  SPA-NAI-HTEKEDYMD
               MOVE    SPACE               TO  SPA-NAI-HKAKUNINYMD
               INITIALIZE                  SPA-MAE-HKN-AREA
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
               MOVE    8                   TO  SPA-GMN-CUR
               MOVE    1                   TO  FLG-HKN-DEL
           END-IF
      *
           .
       4101-KHNJANUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費入力チェック処理
      *****************************************************************
       4102-KOH-CHKALL-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH-ADD
           MOVE    ZERO                TO  FLG-KOH-DEL
      *    第三者行為判定
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K   >   3 )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               PERFORM 41023-RSIKOH-CHK-SEC
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KOH-CHKALL-EXT
           END-IF
      *
      *    削除の判定
           PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL       IDX-K   >   3
               PERFORM 41021-KOH-DEL-CHK-SEC
           END-PERFORM
      *    Ｂ０７Ｓ押下時、最後の行が入力済みであること
           IF      FLG-KOH-ADD2        =   1
               IF     (SPA-GMN-KOHNUM(3)    NOT =   SPACE ) AND
                      (SPA-GMN-KTEKSTYMD(3) NOT =   SPACE )
                   MOVE    1                TO  FLG-KOH-ADD
               END-IF
           END-IF
      *
      *    画面の空白行をつめる
      *
           PERFORM 41022-PTKOH-TBL-SET-SEC
      *
      *    次のデータを編集する
      **** IF      FLG-KOH-DEL         =   1
           IF      SPA-KOH-MAX         <   3
               PERFORM 41023-PTKOH-SAI-SET-SEC
           END-IF
      **** END-IF
      *H28.11
           IF      FLG-GYODEL          =   1
      *        空白行削除ありは画面編集
               PERFORM 41024-PTKOH-GMN-SET-SEC
           END-IF
      *
      *ver.4.7
           IF     (FLG-KOH-ADD2        =   ZERO )
             AND  (FLG-KOH-DEL         =   ZERO )
      *        公費種別一覧
               PERFORM VARYING     IDX-K   FROM    1   BY  1
                   UNTIL      (IDX-K   >   3  )
                          OR  (SPA-ERRCD   NOT =   SPACE)
               IF     (WRK-P02-KOHNUM (IDX-K)(1:1) =   "/"   )
                   MOVE    SPA-NAI-PTKOH-EDANUM (IDX-K)    TO  IDX-K2
                   IF    ((IDX-K2              =   ZERO      ) OR
                          (SPA-PTKOH-SINKIKBN (IDX-K2)
                                               =    "1"      ))
                    AND   (SPA-GMN-FTNJANUM(IDX-K) NOT =   SPACE )
      *                新規入力分
                       PERFORM 41021-KOHHBTCHEK-SEC
                   END-IF
                   IF      SPA-ERRCD       NOT =   SPACE
                       EVALUATE    IDX-K
                       WHEN    1
                           MOVE    21          TO  SPA-GMN-CUR
                       WHEN    2
                           MOVE    27          TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    33          TO  SPA-GMN-CUR
                       END-EVALUATE
                   END-IF
               END-IF
               END-PERFORM
           END-IF
           .
      *
       4102-KOH-CHKALL-EXT.
           EXIT.
      *
      ****************************************************************
      *    第三者行為処理
      ****************************************************************
       41023-RSIKOH-CHK-SEC      SECTION.
      *
           IF     ((IDX-K           =   1   )  AND
                   (FLG-KOH-01      =   1   ))
             OR   ((IDX-K           =   2   )  AND
                   (FLG-KOH-02      =   1   ))
             OR   ((IDX-K           =   3   )  AND
                   (FLG-KOH-03      =   1   ))
      *        削除
               EVALUATE    SPA-GMN-FTNJANUM(IDX-K)(1:2)
               WHEN    "/D"
               WHEN    "/d"
                   MOVE    SPACE            TO  SPA-GMN-FTNJANUM(IDX-K)
                   MOVE    "0143"           TO  SPA-ERRCD
                   MOVE    61               TO  SPA-GMN-CUR
      *        行追加
               WHEN    "/A"
               WHEN    "/a"
                   CONTINUE
               WHEN    OTHER
                   MOVE    "0143"           TO  SPA-ERRCD
                   MOVE    61               TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           .
      *
       41023-RSIKOH-CHK-EXT.
           EXIT.
      *!!!!!
      *
      ****************************************************************
      *    公費削除処理
      ****************************************************************
       41021-KOH-DEL-CHK-SEC      SECTION.
      *
      *    削除の判定
           IF      SPA-GMN-FTNJANUM(IDX-K)(1:2)
                                        =   "/D"  OR  "/d"
               MOVE    SPA-NAI-PTKOH-EDANUM(IDX-K)  TO  IDX-KOH
      *        新規の時、ＴＢＬも削除
               IF     (IDX-KOH             >   ZERO      ) AND
                      (SPA-PTKOH-SINKIKBN (IDX-KOH)
                                           =    "1"      )
                   MOVE    SPACE           TO  SPA-PTKOHINF-TBL
                                                     (IDX-KOH)
                   INITIALIZE                  SPA-PTKOHINF-TBL
                                                     (IDX-KOH)
               ELSE
                   IF      IDX-KOH         >   ZERO
                       MOVE    "1"             TO  SPA-PTKOH-SAKJOKBN
                                                       (IDX-KOH)
                   END-IF
               END-IF
               MOVE    1                   TO  FLG-KOH-DEL
               INITIALIZE                  SPA-GMN-KOH-TBL(IDX-K)
               EVALUATE    IDX-K
                   WHEN    1
                       MOVE    20          TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    26          TO  SPA-GMN-CUR
                   WHEN    3
                        MOVE    32         TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
      *    追加の判定（どの行で入力されても、最初の1行のみを開ける）
           IF      SPA-GMN-FTNJANUM(IDX-K)(1:2)
                                        =   "/A"  OR  "/a"
      *        最後の行が入力済みであること
               IF     (SPA-GMN-KOHNUM(3)    NOT =   SPACE ) AND
                      (SPA-GMN-KTEKSTYMD(3) NOT =   SPACE )
                   MOVE    1                TO  FLG-KOH-ADD
               END-IF
               MOVE    SPA-MAE-FTNJANUM(IDX-K)
                                        TO  SPA-GMN-FTNJANUM(IDX-K)
               EVALUATE    IDX-K
                   WHEN    1
                       MOVE    20          TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    26          TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    32          TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           .
      *
       41021-KOH-DEL-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費空白削除編集処理
      ****************************************************************
       41022-PTKOH-TBL-SET-SEC      SECTION.
      *
           MOVE    SPA-GMN-KOH-AREA    TO  WRK-GMN-KOH-AREA
           INITIALIZE                      SPA-GMN-KOH-AREA
      *H28.11
           MOVE    WRK-P02-KOHNUM-AREA TO  WRK-P02-KOHNUM-AREA2
           INITIALIZE                      WRK-P02-KOHNUM-AREA
           MOVE    ZERO                TO  FLG-GYODEL
           MOVE    ZERO                TO  IDX-K
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
               IF      WRK-GMN-HOK-G (IDX) NOT =   SPACE
                   ADD     1               TO  IDX-K
                   MOVE    WRK-GMN-KOH-TBL (IDX)
                                           TO  SPA-GMN-KOH-TBL (IDX-K)
                   MOVE    WRK-P02-KOHNUM-G2 (IDX)
                                           TO  WRK-P02-KOHNUM-G(IDX-K)
               END-IF
      *H28.11
      *        削除あり
               IF      IDX-K           NOT =   IDX
                   MOVE    1                   TO  FLG-GYODEL
               END-IF
           END-PERFORM
      *    最大を編集する
           MOVE    IDX-K               TO  SPA-KOH-MAX
           .
      *
       41022-PTKOH-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費削除編集処理
      ****************************************************************
       41023-PTKOH-SAI-SET-SEC      SECTION.
      *
      *    空いた行へ次ぎのデータを編集する
           MOVE    SPA-KOH-MAX         TO  IDX-K
           IF      IDX-K               =   ZERO
               MOVE    1                   TO  IDX-K2
           ELSE
               IF      (SPA-NAI-PTKOH-EDANUM(IDX-K) NOT =   ZERO)
                   MOVE    SPA-NAI-PTKOH-EDANUM(IDX-K) TO  IDX-K2
                   ADD     1                   TO  IDX-K2
               ELSE
      *********    MOVE    4                   TO  IDX-K2
                   COMPUTE IDX-K2              =   SPA-PTKOH-MAX + 1
               END-IF
           END-IF
           MOVE    IDX-K               TO  IDY
           PERFORM VARYING     IDX     FROM    IDX-K2 BY  1
                   UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                              (IDY     >=  3    )
               IF     (SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE) AND
                      (SPA-PTKOH-KOHNUM (IDX)  NOT =   SPACE )
                   ADD     1                   TO  IDY
                   MOVE    IDY                 TO  SPA-KOH-MAX
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
           END-PERFORM
      *
           .
      *
       41023-PTKOH-SAI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費画面編集処理
      ****************************************************************
       41024-PTKOH-GMN-SET-SEC      SECTION.
      *
      *    P02へ編集（P02H表示の為）
           MOVE    "GMKOH"             TO  ORCSP02-SYORI
           CALL    "ORCSP02"           USING   ORCSP02AREA
                                               SPA-AREA
                                               SPA-P02KYOUTU
                                               SPA-P02SCRAREA
                                               P02
           .
       41024-PTKOH-GMN-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    新規の時一覧表示処理
      ****************************************************************
       41021-KOHHBTCHEK-SEC      SECTION.
      *
      *    新規の時一覧表示
           INITIALIZE                  SPA-P02H-AREA
           MOVE    SPA-GMN-FTNJANUM(IDX-K) (1:2)
                                       TO  SPA-P02H-HBTNUM
           MOVE    IDX-K               TO  SPA-P02H-KOHIDX
           PERFORM 41021-P02H-SYORI-SEC
           IF      SPA-P02H-MAX        <=  1
               IF      SPA-P02H-MAX        =   1
                   MOVE    SPA-P02H-TKOHNUM (1)
                                           TO  SPA-GMN-KOHNUM (IDX-K)
               END-IF
               INITIALIZE                  SPA-P02H-AREA
               MOVE    ZERO                TO  SPA-FLG-END
           END-IF
           .
      *
       41021-KOHHBTCHEK-EXT.
           EXIT.
      *
      ****************************************************************
      *    カナ氏名チェック処理
      ****************************************************************
       4101-KANANAME-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-KANANAME    =   SPACE
               MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
               GO      TO      4101-KANANAME-CHK-EXT
           END-IF
      *
      *    半角空白を全角空白へ
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    50                  TO  WRK-LEN
           PERFORM 890-HALF-ZEN-SEC
           MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   1
               MOVE    "0025"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4101-KANANAME-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-KANANAME    TO  SPA-MAE-KANANAME
      *    カタカナのみの判定
           MOVE    ZERO                TO  FLG-KANA
           PERFORM VARYING     IDX     FROM    1   BY  2
                   UNTIL      (IDX     >=   50  )  OR
                              (SPA-GMN-KANANAME(IDX:1) =  SPACE)
               IF     (SPA-GMN-KANANAME (IDX:2)
                                       =   "　" 
                                       OR  "ー" 
                                       OR  "−"  )  OR
                      (SPA-GMN-KANANAME (IDX:2)
                                       >=  "ァ"  AND
                                       <=  "ヶ"   )  OR
                      (SPA-GMN-KANANAME (IDX:2)
                                       >=  "０"  AND
                                       <=  "ｚ"   )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-KANA
               END-IF
           END-PERFORM
           IF      FLG-KANA            =   1
               IF      SPA-NAI-KANAFLG     =   ZERO
                   MOVE    "K101"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KANAFLG
                   GO      TO      4101-KANANAME-CHK-EXT
               END-IF
           END-IF
      *
      *    漢字氏名検索処理
           IF     (FLG-IN-KANANAME         =   1    )  AND
                  (SPA-GMN-KANANAME    NOT =   SPA-GMN-OLD-KANANAME)
      *        人名辞書不使用の場合
               IF      SPA-NAI-JINMEI-USE  =   "1"
                   MOVE    SPA-GMN-KANANAME
                                           TO  SPA-GMN-OLD-KANANAME
                   MOVE    SPA-GMN-NAME    TO  SPA-GMN-OLD-NAME
               ELSE
      *            ＱＲからの初期表示
                   IF      SPA-QR-FLG          =   2
                       MOVE    SPACE               TO  SPA-MAE-KANANAME
                   ELSE
                       PERFORM 4101-KANJI-NAME-EDIT-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       4101-KANANAME-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    漢字氏名チェック処理
      ****************************************************************
       4101-NAME-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-NAME    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NAME
               IF      FLG-ERR             =   1
                   MOVE    "0026"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
                   GO      TO      4101-NAME-CHK-EXT
               END-IF
           END-IF
      *
      *    氏名変更時、保険本人を表示中の場合
           IF     (SPA-HEN-FLG         =   1   )    AND
                  (SPA-GMN-HKNNUM  NOT =   SPACE )  AND
                  (SPA-GMN-HONKZKKBN   =   "1"   )  AND
                  (SPA-GMN-HIHKNJANAME NOT =   SPACE )
      *        本人の場合、氏名を編集する
               IF      (SPA-HZN-NAME       =   SPA-GMN-HIHKNJANAME) OR
                       (SPA-MAE-NAME       =   SPA-GMN-HIHKNJANAME)
                   MOVE    SPA-GMN-NAME    TO  SPA-GMN-HIHKNJANAME
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-NAME        TO  SPA-MAE-NAME
      *    見出しへ
           MOVE    SPA-GMN-NAME        TO  SPA-GMN-NAMEH
      *    世帯主へ
           IF      SPA-GMN-SETAINUSI   =   SPACE
               IF      SPA-NAI-SETAIKBN-FLG    =   "1"
                   MOVE    SPA-GMN-NAME        TO  SPA-GMN-SETAINUSI
               END-IF
           END-IF
      *
      *    同姓同名
           IF      (SPA-P02A-FLG1      =   ZERO  )  AND
                  ((SPA-MCP-WIDGET NOT =   "P02_KANANAME" ) OR
                   (SPA-NAI-JINMEI-USE NOT =   "1"   ))
               MOVE    1               TO  FLG-P02A-SYORI
               MOVE    1               TO  SPA-P02A-FLG1
      *        同姓同名チェックあり
               IF      SPA-NAI-DOUMEI-CHKFLG   =   ZERO
                   PERFORM 41012-NIJYU-TRKGMN-SEC
               END-IF
           END-IF
      *ver.4.7
      *    旧姓履歴
           IF    (SPA-NAI-KYUSEIKBN    =   ZERO )
             AND (SPA-P02-SYORI    NOT =   1    )
               MOVE    ZERO                TO  FLG-KYUSEI
               PERFORM 41091-KYUSEI-KOU-SEC
           END-IF
           .
       4101-NAME-CHK-EXT.
           EXIT.
      ****************************************************************
      *    性別チェック処理
      ****************************************************************
       4101-SEX-CHK-SEC      SECTION.
      *
           EVALUATE    SPA-GMN-SEX
               WHEN    "1"
                   MOVE    "男"            TO  SPA-GMN-SEXH
                   MOVE    SPA-GMN-SEX     TO  SPA-MAE-SEX
               WHEN    "2"
                   MOVE    "女"            TO  SPA-GMN-SEXH
                   MOVE    SPA-GMN-SEX     TO  SPA-MAE-SEX
               WHEN    SPACE
                   CONTINUE
               WHEN    OTHER
                   MOVE    "0021"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
           END-EVALUATE
      *
      *H30.4
      *    妊婦区分チェック
           IF     (SPA-GMN-SEX         =   "1"  )
              AND (SPA-GMN28-NINPUKBN  =   "1"  )
              MOVE    "0"             TO  SPA-GMN28-NINPUKBN
              MOVE    "2853"          TO  SPA-ERRCD
              MOVE    3               TO  SPA-GMN-CUR
           END-IF
           .
       4101-SEX-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    生年月日チェック処理
      ****************************************************************
       4101-BIRTHDAY-CHK-SEC           SECTION.
      *
           IF      SPA-GMN-BIRTHDAYW   =   SPACE
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-BIRTHDAYW   TO  SGDAY-INDAY
           MOVE    SPA-SYSYMD          TO  SGDAY-SYSYMD
           MOVE    "2"                 TO  SGDAY-SYSKBN
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
      *
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-BIRTHDAYW
               MOVE    SGDAY-SDAY          TO  SPA-NAI-BIRTHDAY
               MOVE    SGDAY-SDAY          TO  WRK-SYMD
           ELSE
               MOVE    "0028"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
      *    システム日付とのチェック
           IF      SPA-NAI-BIRTHDAY    >   SPA-SYSYMD
               MOVE    SPACE           TO  SPA-NAI-BIRTHDAY
               MOVE    "0020"          TO  SPA-ERRCD
               MOVE    4               TO  SPA-GMN-CUR
               GO      TO      4101-BIRTHDAY-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-MAE-BIRTHDAY
      *
      *    年齢計算
           PERFORM 410121-NENREI-SET-SEC
      *    見出しに編集
           MOVE    SPA-GMN-BIRTHDAYW   TO  SPA-GMN-BIRTHDAYH
      *
      *    二重登録疑いチェック
      *    ＱＲからの初期表示
           IF      SPA-QR-FLG          =   2
               CONTINUE
           ELSE
             IF      SPA-P02A-FLG2       =   ZERO
               MOVE    2               TO  FLG-P02A-SYORI
               MOVE    1               TO  SPA-P02A-FLG2
               PERFORM 41012-NIJYU-TRKGMN-SEC
             END-IF
           END-IF
      *
      *!!
      *    グループ医療機関の同姓同名リスト
           IF     (SPA-P02-SYORI       =   1    )  AND
                  (SPA-NAI-GRPPTLST-FLG    NOT =   "0" )  AND
                  (SPA-P02G-FLG        =   ZERO   )
      *        新規でリスト表示時のみ
               IF     (SPA-FLG-END         =   ZERO )
                   PERFORM 41013-GRPPTNAME-CHK-SEC
               ELSE
                   MOVE    2               TO  SPA-P02G-FLG
               END-IF
           END-IF
      *!!
           .
       4101-BIRTHDAY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    年齢計算処理
      *****************************************************************
       410121-NENREI-SET-SEC             SECTION.
      *
           IF      SPA-NAI-BIRTHDAY    =   SPACE  OR  ZERO
               MOVE    SPACE               TO  SPA-GMN-NENREI
               MOVE    ZERO                TO  SPA-NAI-NENREI
               GO      TO      410121-NENREI-SET-EXT
           END-IF
      *
           MOVE    SPA-NAI-NENREI      TO  WRK-MAE-NENREI
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
           MOVE    WRK-NENREI-YY       TO  SPA-NAI-NENREI-YY
           MOVE    WRK-NENREI-MM       TO  SPA-NAI-NENREI-MM
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
      *    ７０歳到達日
      *    生年月日から基準日を求める
      *    １日ならば生年月日、１日以外ならば生年月日の翌月１日
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BASEYMD
           IF      WRK-BDD         NOT =   01
               COMPUTE WRK-BMM         =   WRK-BMM     +   1
               IF      WRK-BMM         >   12
                   COMPUTE WRK-BYY     =   WRK-BYY     +   1
                   MOVE    01              TO  WRK-BMM
               END-IF
               MOVE    01                  TO  WRK-BDD
           END-IF
      *    ６５歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-65
           COMPUTE SPA-B65-YY          =   SPA-B65-YY  +   65
      *    ７０歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-70
           COMPUTE SPA-B70-YY          =   SPA-B70-YY  +   70
      *    ７５歳を迎える日
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-75
           COMPUTE SPA-B75-YY          =   SPA-B75-YY  +   75
      *
      *    ７５歳の誕生日（Ｈ２０．４から）
           MOVE    SPA-NAI-BIRTHDAY    TO  WRK-BASEYMD
           MOVE    WRK-BASEYMD         TO  SPA-BYMD-75-2
           COMPUTE SPA-B75-YY2         =   SPA-B75-YY2  +   75
           IF      WRK-BASEYMD(5:4)    =   "0229"
               MOVE    "0301"              TO  SPA-BYMD-75-2(5:4)
           END-IF
      *
      *    新規複写後、生年月日変更時に補助区分の再編集をおこなう
           IF     (SPA-P02-SYORI       =   1    )  AND
                  (WRK-MAE-NENREI      =   ZERO )
               PERFORM 31011-HKN-HEN-SEC
           END-IF
           .
       410121-NENREI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    通称チェック処理
      *****************************************************************
       4101-NICKNAME-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-NICKNAME    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-NICKNAME    TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-NICKNAME
               IF      FLG-ERR             =   1
                   MOVE    "0027"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-NICKNAME-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    テスト患者処理
      ***************************************************************
       4101-TSTPTNUMKBN-CHK-SEC         SECTION.
      *
      *    テスト患者
           IF      SPA-GMN-TSTPTNUMKBN =   "0"  OR  "1"
               CONTINUE
           ELSE
      *        エラーの時すべて、患者へ
               MOVE    SPA-GMN-TSTPTNUMKBN-LST(1)
                                       TO  SPA-GMN-TSTPTNUMKBN-G
           END-IF
      *
           .
       4101-TSTPTNUMKBN-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    カルテ発行チェック処理
      ***************************************************************
       4101-HAKKO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HAKKO-MAX
               IF      SPA-GMN-HAKKO   =   SPA-GMN-HAKKO-LST(IDX)(1:1)
                   MOVE    SPA-GMN-HAKKO-LST (IDX)
                                           TO  SPA-GMN-HAKKO-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0029"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-HAKKO-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    Ｕ・Ｐチェック処理
      ***************************************************************
       4101-USERPG-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USERPG-MAX
               IF      SPA-GMN-USERPG  =   SPA-GMN-USERPG-LST(IDX)(1:1)
                   MOVE    SPA-GMN-USERPG-LST (IDX)
                                           TO  SPA-GMN-USERPG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
             AND   SPA-USERPG-MAX      >   ZERO
               MOVE    "0901"              TO  SPA-ERRCD
               MOVE    70                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    郵便番号 チェック処理
      *****************************************************************
       4101-POST-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-POST        =   SPACE
               MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-POST        TO  KANACHK-MAE-INPUT
           MOVE    7                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-POST
           IF     (KANACHK-RC          =   ZERO  )  AND
                  (KANACHK-SYUBETU     =   1     )
      *        半角であること
               CONTINUE
           ELSE
               MOVE    "0131"              TO  SPA-ERRCD
               MOVE     40                 TO  SPA-GMN-CUR
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
      *    周辺住所一覧へ
           IF      SPA-GMN-POST (1:1)  =   "/"
               IF      SPA-GMN-POST (2:3)  NUMERIC
                   PERFORM 41011-SYUHEN-JYUSYO-SEC
               ELSE
                   PERFORM 440-JYUSYO-SEC
               END-IF
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
      *    入力があった時のみ、行う
           IF      FLG-IN-POST         =   ZERO
               GO      TO      4101-POST-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
      *
      *    郵便番号より、住所一覧を表示する
           INITIALIZE                  SPA-P982-AREA
           MOVE    SPA-GMN-POST        TO  SPA-GMN982-POST
           MOVE    "P02"               TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *    対象なし（警告とする）
           IF      SPA-GMN982-MAX      =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE     40                 TO  SPA-GMN-CUR
               GO  TO  4101-POST-CHK-EXT
           END-IF
      *    １件の時、対象
           IF      SPA-GMN982-MAX      =   1
      *        住所検索編集
               PERFORM 30011-P982SET-SEC
           ELSE
      *        住所一覧画面へ
               PERFORM 41012-P982-SYORI-SEC
           END-IF
      *
           .
       4101-POST-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    周辺住所チェック処理
      *****************************************************************
       41011-SYUHEN-JYUSYO-SEC         SECTION.
      *
      *    システム管理より周辺住所を検索
           INITIALIZE                  SYS-1015-REC
           MOVE    "1015"              TO  SYS-1015-KANRICD
           MOVE    SPA-GMN-POST (2:3)  TO  SYS-1015-KBNCD
           MOVE    SPA-SYSYMD          TO  SYS-1015-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1015-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1015-HOSPNUM
           MOVE    SYS-1015-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSTEM-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1015-REC
           ELSE
      *        周辺住所一覧へ
               PERFORM 440-JYUSYO-SEC
               GO      TO      41011-SYUHEN-JYUSYO-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *    住所データ読み出し
           INITIALIZE                      ADRS-REC
      **** MOVE    SYS-1015-LPUBCD     TO  ADRS-LPUBCD
           MOVE    SYS-1015-POST       TO  ADRS-POST
           MOVE    SYS-1015-RENNUM     TO  ADRS-EDANUM
           MOVE    SPA-HOSPNUM         TO  ADRS-HOSPNUM
           MOVE    ADRS-REC            TO  MCPDATA-REC
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_adrs"          TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  ADRS-REC
                   MOVE    ADRS-POST           TO  SPA-GMN-POST
                   PERFORM 41011-ADRS-HENSYU-SEC
                   MOVE    SPA-GMN-EDITADRS-NAME
                                               TO  SPA-MAE-ADRS
                   MOVE    SPA-GMN-POST        TO  SPA-MAE-POST
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-IF
           MOVE    "tbl_adrs"          TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-OK              =   ZERO
      *        周辺住所一覧へ
               PERFORM 440-JYUSYO-SEC
           END-IF
           .
       41011-SYUHEN-JYUSYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所編集処理
      *****************************************************************
       41011-ADRS-HENSYU-SEC         SECTION.
      *
           IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *    都道府県名なし
               MOVE    SPACE               TO  SPA-GMN-EDITADRS-NAME
      *        市町村名＋町域名
               STRING  ADRS-CITYNAME
                       ADRS-TOWNNAME       DELIMITED  BY  SPACE
                                           INTO    SPA-GMN-EDITADRS-NAME
               END-STRING
               IF      ADRS-TOWNNAME       =   "以下に掲載がない場合"
                   MOVE    ADRS-CITYNAME       TO  SPA-GMN-EDITADRS-NAME
               END-IF
           ELSE
               MOVE    ADRS-EDITADRS-NAME  TO  SPA-GMN-EDITADRS-NAME
           END-IF
      *    京都で通り名称を編集する時
           IF     (ADRS-LPUBCD(1:2)    =   "26"  )
             AND  (ADRS-TOWNNAME2  NOT =   SPACE )
             AND  (SPA-NAI-TOWNNAME2KBN-FLG    =   "1" )
               IF     (ADRS-TOWNNAME2(1:2)     =   "（"  )
                   CONTINUE
               ELSE
                   PERFORM 41011-ADRS-KYOTOHEN-SEC
               END-IF
           END-IF
           .
       41011-ADRS-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    京都通り名称編集処理
      *****************************************************************
       41011-ADRS-KYOTOHEN-SEC         SECTION.
      *
           IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *    都道府県名なし
               MOVE    SPACE               TO  SPA-GMN-EDITADRS-NAME
      *        市町村名＋通り名称
               STRING  ADRS-CITYNAME
                       ADRS-TOWNNAME2
                       ADRS-TOWNNAME
                                           DELIMITED  BY  SPACE
                                           INTO    SPA-GMN-EDITADRS-NAME
               END-STRING
           ELSE
      *        都道府県名＋市町村名＋通り名称
               MOVE    SPACE               TO  SPA-GMN-EDITADRS-NAME
               STRING  ADRS-PREFNAME
                       ADRS-CITYNAME
                       ADRS-TOWNNAME2
                       ADRS-TOWNNAME
                                           DELIMITED  BY  SPACE
                                           INTO    SPA-GMN-EDITADRS-NAME
               END-STRING
           END-IF
           .
       41011-ADRS-KYOTOHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所チェック処理
      *****************************************************************
       4101-ADRS-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-EDITADRS-NAME       =   SPACE
               MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           ELSE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-EDITADRS-NAME   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-EDITADRS-NAME
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    41                  TO  SPA-GMN-CUR
               ELSE
      *            郵便番号自動設定
                   IF     (SPA-GMN-EDITADRS-NAME
                                           NOT =   SPA-MAE-ADRS )
                      AND (SPA-MCP-WIDGET      =   "P02_ADRS"   )
                       PERFORM 41012-ADRS-POSTCHK-SEC
                   ELSE
                       MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
                   END-IF
               END-IF
           END-IF
           .
       4101-ADRS-CHK-EXT.
           EXIT.
      *****************************************************************
      *    住所から郵便番号編集処理
      *****************************************************************
       41012-ADRS-POSTCHK-SEC         SECTION.
      *
           EVALUATE    SPA-NAI-POSTKBN-FLG
               WHEN     "0"
                   MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
               WHEN     "1"
                   PERFORM 410121-ADRS-POST-SEC
               WHEN     "2"
                   IF      SPA-GMN-POST        =   SPACE
                       PERFORM 410121-ADRS-POST-SEC
                   ELSE
                       MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
                   END-IF
           END-EVALUATE
           .
       41012-ADRS-POSTCHK-EXT.
           EXIT.
      *****************************************************************
      *    住所から郵便番号編集処理
      *****************************************************************
       410121-ADRS-POST-SEC         SECTION.
      *
      *    郵便番号より、住所一覧を表示する
           INITIALIZE                  SPA-P982-AREA
           MOVE    2                   TO  SPA-GMN982-KBN
           MOVE    SPA-GMN-EDITADRS-NAME
                                       TO  SPA-GMN982-TOWNNAME
           MOVE    "P02"               TO  SPA-GMN982-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP982"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
      *    １件の時、対象
           IF      SPA-GMN982-MAX      =   1
               IF      SPA-GMN982-POST     NOT =   SPA-GMN-POST
      *            住所検索編集
                   PERFORM 30011-P982SET-SEC
               ELSE
                   IF      SPA-NAI-KENMEIKBN-FLG   =   "0"
      *                都道府県名なし
                       IF      SPA-GMN982-CITYNAMET
                                           NOT =   SPA-GMN-EDITADRS-NAME
                           MOVE    "1098"          TO  SPA-PIDCD
                       END-IF
                   ELSE
                       IF      SPA-GMN982-TOWNNAME
                                           NOT =   SPA-GMN-EDITADRS-NAME
                           MOVE    "1098"          TO  SPA-PIDCD
                       END-IF
                   END-IF
               END-IF
               MOVE    SPA-GMN-POST            TO  SPA-MAE-POST
               MOVE    SPA-GMN-EDITADRS-NAME   TO  SPA-MAE-ADRS
           ELSE
               IF      SPA-GMN982-MAX      >   1
                   PERFORM 41012-P982-SYORI-SEC
               END-IF
           END-IF
      *
           .
       410121-ADRS-POST-EXT.
           EXIT.
      *****************************************************************
      *    住所一覧遷移処理
      *****************************************************************
       41012-P982-SYORI-SEC         SECTION.
      *
      *    住所一覧画面へ
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
      *    一覧へ
           MOVE    "P982"              TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P982"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       41012-P982-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    番地チェック処理
      *****************************************************************
       4101-BANTI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-BANTI   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-BANTI       TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-BANTI
               IF      FLG-ERR             =   1
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    42                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-BANTI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    電話番号 チェック処理
      *****************************************************************
       4101-TEL-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-TEL1    NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TEL1        TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-TEL1
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角であること
                   CONTINUE
               ELSE
                   MOVE    "0132"              TO  SPA-ERRCD
                   MOVE     43                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-GMN-TEL2    NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-TEL2        TO  KANACHK-MAE-INPUT
               MOVE    15                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-TEL2
               IF     (KANACHK-RC          =   ZERO  )  AND
                      (KANACHK-SYUBETU     =   1     )
      *            半角であること
                   CONTINUE
               ELSE
                   MOVE    "0132"              TO  SPA-ERRCD
                   MOVE     44                 TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-TEL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    世帯主チェック処理
      *****************************************************************
       4101-SETAINUSI-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-SETAINUSI   NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-SETAINUSI   TO  WRK-MAE-INPUT
               MOVE    50                  TO  WRK-LEN
               PERFORM 891-HALF-ZEN2-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-SETAINUSI
               IF      FLG-ERR             =   1
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    47                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-SETAINUSI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    続柄チェック処理
      *****************************************************************
       4101-ZOKUGARA-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-ZOKUGARA    NOT =   SPACE
      *        半角空白を全角空白へ
               MOVE    SPA-GMN-ZOKUGARA    TO  WRK-MAE-INPUT
               MOVE    30                  TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ZOKUGARA
               IF      FLG-ERR             =   1
                   MOVE    "0034"              TO  SPA-ERRCD
                   MOVE    48                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-ZOKUGARA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    減免事由チェック処理
      *****************************************************************
       4101-DISCOUNTKBN-CHK-SEC        SECTION.
      *
      *    減免事由が未入力
      *    IF      SPA-GMN-DISCOUNTKBN =   SPACE
      *        MOVE    ZERO            TO  SPA-GMN-DISCOUNTKBN
      ******** GO  TO                  4101-DISCOUNTKBN-CHK-EXT
      *    END-IF
      *    減免事由より割引率区分を取得
           MOVE    SPACE               TO  WRK-RATEKBN
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL   ( IDX       >   SPA-DISCOUNTKBN-MAX )
                   OR      ( SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                                             =   SPACE )
               IF      SPA-GMN-DISCOUNTKBN
                                   =   SPA-GMN-DISCOUNTKBN-KBNCD (IDX)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-GMN-DISCOUNTKBN-RATEKBN (IDX)
                                           TO  WRK-RATEKBN
                   MOVE    SPA-DISCOUNTKBN-MAX TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    45                  TO  SPA-GMN-CUR
               GO  TO                  4101-DISCOUNTKBN-CHK-EXT
           END-IF
      *
      *    見つからない場合、セットしない
           IF      WRK-RATEKBN             =   SPACE
               GO  TO                  4101-DISCOUNTKBN-CHK-EXT
           END-IF
      *
           MOVE    SPA-GMN-DISCOUNTKBN     TO  SPA-MAE-DISCOUNTKBN
      *    減免事由を入力時のみ、割引率設定
           IF      FLG-IN-DISCOUNTKBN  =   ZERO
               GO  TO                  4101-DISCOUNTKBN-CHK-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-RATE-AREA
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL  ( IDX     >     SPA-DISCOUNT-MAX )
                     OR     ( SPA-GMN-DISCOUNT-KBNCD (IDX)  =   SPACE )
      *
             IF  WRK-RATEKBN           =   SPA-GMN-DISCOUNT-KBNCD (IDX)
                 MOVE    SPA-GMN-DISCOUNT-KBNCD(IDX)
                                       TO  WRK-RATE-KBN
                 MOVE    SPA-GMN-DISCOUNT-RITU (IDX)
                                       TO  WRK-RATE-EDITZ
                 EVALUATE    SPA-GMN-DISCOUNT-TEIRITU (IDX)
                 WHEN    "0"
                     MOVE    "該当なし"    TO  WRK-RATE-EDIT-R
                 WHEN    "1"
                     MOVE    "(%)"         TO  WRK-RATE-TANI
                 WHEN    "2"
                     MOVE    "(円)"        TO  WRK-RATE-TANI
                 END-EVALUATE
                 MOVE    SPA-DISCOUNT-MAX  TO  IDX
             END-IF
           END-PERFORM
           MOVE    WRK-RATE-AREA       TO  SPA-GMN-DISCOUNT
      *
           .
       4101-DISCOUNTKBN-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    割引率チェック処理
      ***************************************************************
       4101-DISCOUNT-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-DISCOUNT-MAX)  OR
                              (SPA-GMN-DISCOUNT-KBNCD(IDX)
                                           =   SPACE  )
               IF      SPA-GMN-DISCOUNT(1:2)
                                       =   SPA-GMN-DISCOUNT-KBNCD
                                                              (IDX)
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0035"              TO  SPA-ERRCD
               MOVE    46                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-DISCOUNT-CHK-EXT.
           EXIT.
      ***************************************************************
      *    入金方法チェック処理
      ***************************************************************
       4101-NYUKIN-HOHO-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-NYUKIN-MAX)
                          OR  (FLG-CHK     =   1   )
               IF      SPA-GMN-NYUKIN-HOHO
                                           =   SPA-GMN-NYUKIN-HOHOL
                                                              (IDX)
                   MOVE    SPA-GMN-NYUKIN-LST (IDX)
                                           TO  SPA-GMN-NYUKIN-HOHO-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "0166"              TO  SPA-ERRCD
               MOVE    66                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       4101-NYUKIN-HOHO-CHK-EXT.
           EXIT.
      ***************************************************************
      *    請求書発行区分チェック処理
      ***************************************************************
       4101-SKYPRTFLG-CHK-SEC         SECTION.
      *
           IF      SPA-GMN-SKYPRTFLG   =   SPACE
               MOVE    SPACE               TO  SPA-GMN-SKYPRTFLG-G
           ELSE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-SKYPRTFLG-MAX)
                              OR  (FLG-CHK     =   1   )
                   IF      SPA-GMN-SKYPRTFLG
                                           =   SPA-GMN-SKYPRTFLG-LST
                                                         (IDX)(1:1)
                       MOVE    SPA-GMN-SKYPRTFLG-LST (IDX)
                                           TO  SPA-GMN-SKYPRTFLG-G
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0167"              TO  SPA-ERRCD
                   MOVE    67                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4101-SKYPRTFLG-CHK-EXT.
           EXIT.
      *
      ***************************************************************
      *    出生時体重チェック処理
      ***************************************************************
       4101-BIRTHWEIGHT-CHK-SEC         SECTION.
      *
      *
           .
       4101-BIRTHWEIGHT-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    状態名称セット処理
      ****************************************************************
       4101-CONDITIONH-SET-SEC         SECTION.
      *
           PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL    (IDX       >  3    )  OR
                            (SPA-ERRCD NOT =   SPACE )
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDY       FROM    1   BY  1
                       UNTIL   ( IDY       >   SPA-CONDITION-MAX(IDX))
                       OR      ( SPA-GMN-CONDITION-KBNCD (IDX IDY)
                                           =   SPACE )
                   IF      SPA-GMN-CONDITION (IDX)(1:2)
                                           =   SPA-GMN-CONDITION-KBNCD
                                                            (IDX IDY)
                       MOVE    SPA-GMN-CONDITION-WORD (IDX IDY)
                                           TO  SPA-GMN-CONDITIONH (IDX)
                       MOVE    1           TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK         =   ZERO
                   MOVE    "0036"          TO  SPA-ERRCD
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    49          TO  SPA-GMN-CUR
                       WHEN    2
                           MOVE    50          TO  SPA-GMN-CUR
                       WHEN    3
                           MOVE    51          TO  SPA-GMN-CUR
                   END-EVALUATE
               END-IF
           END-PERFORM
      *
           .
       4101-CONDITIONH-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌他チェック処理
      *****************************************************************
       4101-COMMENT-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-TABOO-1     NOT =   SPACE
               MOVE    SPA-GMN-TABOO-1     TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-TABOO-1
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    52                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-GMN-TABOO-2     NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-TABOO-2     TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-TABOO-2
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    53                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-GMN-ALLERGY-1     NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-ALLERGY-1   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ALLERGY-1
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    54                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-GMN-ALLERGY-2     NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-ALLERGY-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-ALLERGY-2
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    55                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-GMN-KANSENSYO-1 NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-KANSENSYO-1 TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANSENSYO-1
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    56                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF     (SPA-GMN-KANSENSYO-2 NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-KANSENSYO-2 TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZEN-SEC
               MOVE    WRK-ATO-INPUT       TO  SPA-GMN-KANSENSYO-2
               IF      FLG-ERR             =   1
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    57                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    コメント
           IF     (SPA-GMN-COMMENT-1   NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-1   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZENS-SEC
               MOVE    WRK-MAE-INPUT       TO  SPA-GMN-COMMENT-1
           END-IF
           IF     (SPA-GMN-COMMENT-2   NOT =   SPACE ) AND
                  (SPA-ERRCD               =   SPACE )
               MOVE    SPA-GMN-COMMENT-2   TO  WRK-MAE-INPUT
               MOVE    100                 TO  WRK-LEN
               PERFORM 890-HALF-ZENS-SEC
               MOVE    WRK-MAE-INPUT       TO  SPA-GMN-COMMENT-2
           END-IF
           .
       4101-COMMENT-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    選択番号チェック処理
      ****************************************************************
       4101-SELNUM-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SELNUM  NOT =   ZERO
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDX       FROM    1   BY  1
                       UNTIL     IDX       >  SPA-HKNCOMBI-MAX
                   IF      SPA-GMN-SELNUM  =   SPA-GMN-THKNCOMBINUM(IDX)
                       IF      SPA-NAI-THYOJIKBN (IDX)  =   "1"
                           MOVE    "0137"          TO  SPA-ERRCD
                           MOVE    38              TO  SPA-GMN-CUR
                       END-IF
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0037"              TO  SPA-ERRCD
                   MOVE    38                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       4101-SELNUM-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療科チェック処理
      ****************************************************************
       4101-SRYKA-CHK-SEC      SECTION.
      *
           IF      SPA-GMN-SRYKA   NOT =   SPACE
               MOVE    ZERO                TO  FLG-CHK
               PERFORM VARYING   IDX       FROM    1   BY  1
                       UNTIL     IDX       >   SPA-SRYKA-MAX
                   IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKA-LST (IDX)
                                                              (1:2)
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-PERFORM
               IF      FLG-CHK             =   ZERO
                   MOVE    "0038"              TO  SPA-ERRCD
                   MOVE    39                  TO  SPA-GMN-CUR
               END-IF
           ELSE
      *        カルテ発行時は、必須とする
               IF      SPA-GMN-HAKKO       =   "1"
                   MOVE    "0039"              TO  SPA-ERRCD
                   MOVE    39                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
      *
       4101-SRYKA-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-KANANAME    =   SPACE
               MOVE    "0100"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "0101"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-SEX         =   SPACE
               MOVE    "0103"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-BIRTHDAYW   =   SPACE
               MOVE    "0102"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    旧姓履歴の更新
     ****  PERFORM 41091-KYUSEI-KOU-SEC
      *    旧姓履歴
      *    IF    (SPA-NAI-KYUSEIKBN    =   ZERO )
      *      AND (SPA-P02-SYORI    NOT =   1    )
      *        MOVE    ZERO                TO  FLG-KYUSEI
      *        PERFORM 41091-KYUSEI-KOU-SEC
      *****END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    旧姓履歴の更新処理
      ****************************************************************
       41091-KYUSEI-KOU-SEC      SECTION.
      *
      *    新規の時、旧姓履歴は出力しない
      *    IF      SPA-P02-SYORI       =   1
      *        GO      TO      41091-KYUSEI-KOU-EXT
      *    END-IF
      *ver.4.7
           MOVE    1               TO  SPA-NAI-KYUSEIKBN
      *ver.4.7
      *
           MOVE    ZERO                TO  IDX
           IF     (SPA-OLD-NAME    NOT =   SPACE   )  AND
                  (SPA-GMN-NAME    NOT =   SPACE   )  AND
                  (SPA-GMN-NAME    NOT =   SPA-OLD-NAME)
               MOVE    SPA-NAI25-KYU-MAX   TO  IDX
               IF    ( IDX                         >   ZERO      ) AND
                     ((SPA-GMN25-KYU-CHGYMD(IDX)   =   SPA-SYSYMD) OR
                      (SPA-GMN25-KYU-NAME  (IDX)   =   SPA-OLD-NAME
                  AND  SPA-GMN25-KYU-KANANAME(IDX) =   SPA-OLD-KANANAME
                  AND  SPA-GMN25-KYU-NICKNAME(IDX) =  SPA-OLD-NICKNAME))
                   CONTINUE
               ELSE
      *ver.4.7
                   IF      FLG-KYUSEI      =   ZERO
                       MOVE    "0250"          TO  SPA-PIDCD
                   ELSE
      *
                   ADD     1               TO  IDX
                   IF      IDX             >   20
                      MOVE    "2510"           TO  SPA-ERRCD
                      MOVE    1                TO  SPA-GMN-CUR
                      GO      TO      41091-KYUSEI-KOU-EXT
                   END-IF
                   ADD     1               TO  SPA-NAI25-KYU-MAX
      *
                   MOVE    SPA-OLD-NAME    TO  SPA-GMN25-KYU-NAME
                                                               (IDX)
                   MOVE    SPA-OLD-KANANAME
                                           TO  SPA-GMN25-KYU-KANANAME
                                                                (IDX)
                   MOVE    SPA-OLD-NICKNAME
                                           TO  SPA-GMN25-KYU-NICKNAME
                                                                (IDX)
                   MOVE    SPA-SYSYMD      TO  SPA-GMN25-KYU-CHGYMD
                                                               (IDX)
                   MOVE    SPA-SYSYMDWH    TO  SPA-GMN25-KYU-CHGYMDW
                                                              (IDX)
                   MOVE    SPA-SYSYMD      TO  SPA-GMN25-KYU-UPYMD(IDX)
      *!!!
                   MOVE    "1"             TO  SPA-GMN25-KYU-INSKBN(IDX)
      *            旧姓を見出しへ
                   MOVE    SPA-OLD-NAME    TO  SPA-GMN-KYUSEIH
      *!!!
               END-IF
           END-IF
      *
           END-IF
      *
           .
      *
       41091-KYUSEI-KOU-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険・公費のテーブルセット処理
      ****************************************************************
       4103-HKNKOH-TBL-SEC      SECTION.
      *
           IF      FLG-APICHK          =   ZERO
      *H28.5
              AND (FLG-P027            =   ZERO )
      *
      *    保険画面の内容をＴＢＬへ編集する
           PERFORM 41031-HKN-SPA-TBL-SET-SEC
      *
      *    公費画面の内容をＴＢＬへ編集する
           PERFORM 41032-KOH-SPA-TBL-SET-SEC
      *
           END-IF
      *
      *    保険と公費の期間重複チェック
           PERFORM 41033-HKNKOH-CHK-SEC
      *
      *!!
      *    退職国保の年齢チェック（Ｈ２０．４ 改正）
           IF     (FLG-KOH-ADD2         =   ZERO  )
              AND (FLG-ERRCHK           =   ZERO  )
              AND (FLG-HKN-ADD2         =   ZERO  )
              AND (SPA-ERRCD            =   SPACE )
               IF     (SPA-SYSYMD          >=  "20080401" )  AND
                      (SPA-BYMD-65         >   ZERO       )  AND
                      (SPA-SYSYMD          >=  SPA-BYMD-65)
                   PERFORM 410334-TAIKOKUHO-CHK-SEC
               END-IF
           END-IF
      *    Ｈ１４．１０ 改正
      *!   IF      FLG-TOUROKU2         =   1
      *    公費追加以外
           IF      FLG-KOH-ADD2         =   ZERO
              AND  FLG-ERRCHK           =   ZERO
              AND  FLG-HKN-ADD2         =   ZERO
              AND (SPA-ERRCD            =   SPACE )
              AND (SPA-PIDCD            =   SPACE )
      *!!
      *        ７０歳以上の時、老人割合が設定されていること
               IF     (SPA-SYSYMD          >=  "20021001" )  AND
                      (SPA-BYMD-70         >   ZERO       )  AND
                      (SPA-SYSYMD          >=  SPA-BYMD-70)
                   PERFORM 410331-RJN-HKNCHK-SEC
               ELSE
      *        Ｈ１８．１０ 改正
      *        老人公費２割チェック
                   PERFORM 410332-RJN27-CHK-SEC
               END-IF
           END-IF
      *    Ｈ２０．４ 改正
           IF     (FLG-TOUROKU2         =   1   )
             AND  (SPA-ERRCD            =   SPACE )
             AND  (SPA-PIDCD            =   SPACE )
      *H30.10  H30.4までのチェックとする
             AND  (SPA-SYSYMD           <   "20180401")
      *        老人公費終了チェック
               PERFORM 410333-RJN27-ENDCHK-SEC
           END-IF
           .
      *
       4103-HKNKOH-TBL-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    保険画面の内容をＴＢＬへ編集処理
      ****************************************************************
       41031-HKN-SPA-TBL-SET-SEC      SECTION.
      *
           IF      SPA-GMN-HKNNUM      =   SPACE
      *        空白行削除
               PERFORM 410311-HKNTBL-DEL-SEC
               GO      TO      41031-HKN-SPA-TBL-SET-EXT
           END-IF
      *
           IF      SPA-NAI-HKNSTR      =   ZERO
      *        新規の時、１行開ける
               PERFORM 410311-HKNTBL-INS-SEC
               MOVE    1                   TO  IDX
           ELSE
               MOVE    SPA-NAI-HKNSTR      TO  IDX
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41031-HKN-SPA-TBL-SET-EXT
           END-IF
           PERFORM 410312-HKN-SPATBL-SET-SEC
      *
      *    空白行削除
           PERFORM 410311-HKNTBL-DEL-SEC
      *
      *    保険情報ＴＢＬから編集
           IF      SPA-NAI-HKNSTR      =   ZERO
               MOVE    1               TO  SPA-NAI-HKNSTR
           END-IF
           PERFORM 3101-HKN-TBL-SPA-SEC
           .
      *
       41031-HKN-SPA-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険テーブル空白行削除処理
      ****************************************************************
       410311-HKNTBL-DEL-SEC      SECTION.
      *
      *    空白行をつめる
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (IDX                 <   40  )  AND
                      (SPA-PTHKN-HKNNUM (IDX)  =   SPACE)
                   MOVE    SPA-PTHKNINF-TBL (IDX + 1)
                                           TO  SPA-PTHKNINF-TBL (IDX)
                   INITIALIZE              SPA-PTHKNINF-TBL (IDX + 1)
               END-IF
               IF      SPA-PTHKN-HKNNUM (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  SPA-PTHKN-MAX
      *
           .
      *
       410311-HKNTBL-DEL-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険テーブル１行挿入処理
      ****************************************************************
       410311-HKNTBL-INS-SEC           SECTION.
      *
           IF      (SPA-PTHKN-MAX       >=  TBL-MAX-HKN )
               MOVE    "0107"          TO  SPA-ERRCD
           ELSE
      *
      *    １行開ける
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX     FROM    SPA-PTHKN-MAX BY -1
                   UNTIL       IDX     <   1
               MOVE    SPA-PTHKNINF-TBL (IDX)
                                       TO  SPA-PTHKNINF-TBL (IDX + 1)
           END-PERFORM
           INITIALIZE                  SPA-PTHKNINF-TBL (01)
           ADD     1                   TO  SPA-PTHKN-MAX
      *
           END-IF
           .
       410311-HKNTBL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険画面→ＳＰＡＴＢＬ編集処理
      *****************************************************************
       410312-HKN-SPATBL-SET-SEC              SECTION.
      *
           MOVE    SPA-GMN-HKNNUM      TO  SPA-PTHKN-HKNNUM (IDX)
           MOVE    SPA-GMN-HKNJANUM    TO  SPA-PTHKN-HKNJANUM (IDX)
           MOVE    SPA-GMN-HONKZKKBN   TO  SPA-PTHKN-HONKZKKBN (IDX)
           MOVE    SPA-GMN-HOJOKBN     TO  SPA-PTHKN-HOJOKBN (IDX)
           MOVE    SPA-GMN-CONTKBN     TO  SPA-PTHKN-CONTKBN (IDX)
           MOVE    SPA-GMN-KIGO        TO  SPA-PTHKN-KIGO    (IDX)
           MOVE    SPA-GMN-NUM         TO  SPA-PTHKN-NUM     (IDX)
      *R02.4
           MOVE    SPA-GMN-EDABAN      TO  SPA-PTHKN-EDABAN (IDX)
           MOVE    SPA-GMN-HIHKNJANAME TO  SPA-PTHKN-HIHKNJANAME (IDX)
           MOVE    SPA-NAI-SKKGETYMD   TO  SPA-PTHKN-SKKGETYMD (IDX)
           MOVE    SPA-NAI-HTEKSTYMD   TO  SPA-PTHKN-TEKSTYMD (IDX)
           MOVE    SPA-NAI-HTEKEDYMD   TO  SPA-PTHKN-TEKEDYMD (IDX)
           MOVE    SPA-NAI-HKAKUNINYMD TO  SPA-PTHKN-KAKUNINYMD (IDX)
      *
           MOVE    SPA-NAI-SAKJOKBN    TO  SPA-PTHKN-SAKJOKBN (IDX)
           MOVE    SPA-NAI-PTHKN-SBTKBN
                                       TO  SPA-PTHKN-HKN-SBTKBN(IDX)
      *
           MOVE    SPA-HKNJA-KEIFLG    TO  SPA-PTHKN-KEIFLG  (IDX)
           MOVE    SPA-KIGO-KEIFLG     TO  SPA-PTHKN-KIGO-KEIFLG (IDX)
           MOVE    SPA-NUM-KEIFLG      TO  SPA-PTHKN-NUM-KEIFLG (IDX)
      *    後期高齢者警告
           MOVE    SPA-ROU-KEIFLG      TO  SPA-PTHKN-ROU-KEIFLG (IDX)
      *    確認年月日
           MOVE    SPA-HKN-KAKUINFLG   TO  SPA-PTHKN-KAKUINFLG (IDX)
      *    本人家族警告
           MOVE    SPA-HONKZK-KEIFLG   TO  SPA-PTHKN-HONKZKFLG (IDX)
      *    補助区分警告
           MOVE    SPA-HOJOKBN-KEIFLG  TO  SPA-PTHKN-HOJOKEIFLG (IDX)
      *
      *    新規の時保険ＩＤ取得（労災は労災画面で取得する）
           IF      SPA-NAI-HKNID       =   ZERO
               ADD     1                   TO  SPA-PTNUM-HKNID
               MOVE    SPA-PTNUM-HKNID     TO  SPA-NAI-HKNID
           END-IF
           MOVE    SPA-NAI-HKNID       TO  SPA-PTHKN-HKNID (IDX)
      *
           IF      SPA-NAI-HKNSTR      =   ZERO
               MOVE    "1"             TO  SPA-PTHKN-SINKIKBN (IDX)
           END-IF
      *
           MOVE    SPA-GMN-HKNNUMMEI   TO  SPA-PTHKN-HKNNUM-MEI(IDX)
           MOVE    SPA-GMN-HONKZKKBNMEI
                                       TO  SPA-PTHKN-HONKZKKBN-MEI(IDX)
           MOVE    SPA-GMN-HOJOKBNMEI  TO  SPA-PTHKN-HOJOKBN-MEI(IDX)
           MOVE    SPA-GMN-CONTKBNMEI  TO  SPA-PTHKN-CONTKBN-MEI(IDX)
           MOVE    SPA-GMN-SKKGETYMD   TO  SPA-PTHKN-SKKGETYMDW(IDX)
           MOVE    SPA-GMN-HTEKSTYMD   TO  SPA-PTHKN-TEKSTYMDW(IDX)
           MOVE    SPA-GMN-HTEKEDYMD   TO  SPA-PTHKN-TEKEDYMDW(IDX)
           MOVE    SPA-GMN-HKAKUNINYMD TO  SPA-PTHKN-KAKUNINYMDW(IDX)
      *
      *
           .
       410312-HKN-SPATBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費画面の内容をＴＢＬへ編集処理
      ****************************************************************
       41032-KOH-SPA-TBL-SET-SEC      SECTION.
      *
      *    変更分のみ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM      (IDX)  NOT =   SPACE ) AND
                     (SPA-NAI-PTKOH-EDANUM(IDX)  NOT =   ZERO  )
                   MOVE    SPA-NAI-PTKOH-EDANUM(IDX)   TO  IDY
                   PERFORM 3102-KOH-SPATBL-HEN-SEC
              END-IF
           END-PERFORM
      *    新規分のみ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM      (IDX)  NOT =   SPACE ) AND
                     (SPA-NAI-PTKOH-EDANUM(IDX)      =   ZERO  )
                   IF     (IDX             =   1  )  AND
                          (SPA-PTKOH-MAX   >   ZERO )
      *                最初の行追加の時、１行目をあける
                       PERFORM 410321-KOHTBL-INS-SEC
                       MOVE    1               TO  IDY
                   ELSE
                       ADD     1               TO  SPA-PTKOH-MAX
      *!!              上限チェック
                       IF      (SPA-PTKOH-MAX      >   TBL-MAX-KOH )
                           MOVE    "0106"          TO  SPA-ERRCD
                           MOVE    TBL-MAX-KOH     TO  SPA-PTKOH-MAX
                       END-IF
                       MOVE    SPA-PTKOH-MAX   TO  IDY
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 3102-KOH-SPATBL-HEN-SEC
                   END-IF
              END-IF
           END-PERFORM
      *    空白行削除
           PERFORM 410323-KOHTBL-DEL-SEC
      *    公費情報ＴＢＬから編集
           IF      SPA-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  SPA-NAI-KOHSTR
           END-IF
           PERFORM 3101-KOH-TBL-SPA-SEC
           .
      *
       41032-KOH-SPA-TBL-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費テーブル１行挿入処理
      ****************************************************************
       410321-KOHTBL-INS-SEC      SECTION.
      *
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX2    FROM    SPA-PTKOH-MAX BY -1
                   UNTIL       IDX2    <   1
               MOVE    SPA-PTKOHINF-TBL (IDX2)
                                       TO  SPA-PTKOHINF-TBL (IDX2 + 1)
           END-PERFORM
           INITIALIZE                  SPA-PTKOHINF-TBL (01)
           ADD     1                   TO  SPA-PTKOH-MAX
      *
           .
       410321-KOHTBL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費画面→ＳＰＡＴＢＬ編集処理
      *****************************************************************
       3102-KOH-SPATBL-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-KOHNUM   (IDX)  TO  SPA-PTKOH-KOHNUM (IDY)
           MOVE    SPA-GMN-FTNJANUM (IDX)  TO  SPA-PTKOH-FTNJANUM (IDY)
           MOVE    SPA-GMN-JKYSNUM  (IDX)  TO  SPA-PTKOH-JKYSNUM (IDY)
           MOVE    SPA-NAI-KTEKSTYMD(IDX)  TO  SPA-PTKOH-TEKSTYMD (IDY)
           MOVE    SPA-NAI-KTEKEDYMD(IDX)  TO  SPA-PTKOH-TEKEDYMD (IDY)
           MOVE    SPA-NAI-KKAKUNINYMD(IDX)
                                           TO  SPA-PTKOH-KAKUNINYMD(IDY)
           MOVE    SPA-NAI-PTKOH-PAYKBN(IDX)
                                           TO  SPA-PTKOH-PAYKBN (IDY)
      *
      ****MOVE    SPA-NAI-KSAKJOKBN (IDX) TO  SPA-PTKOH-SAKJOKBN(IDY)
      *
      *    新規の時公費ＩＤ取得
           IF      SPA-NAI-PTKOH-KOHID (IDX)   =   ZERO
      *        第三者行為追加のＩＤは設定済み
               IF     (SPA-NAI2D-KOHIADD   =   1  )
                AND   (SPA-GMN-KOHNUM(IDX) =   "970" )
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-PTNUM-KOHID
               END-IF
     ********* ADD     1                   TO  SPA-PTNUM-KOHID
               MOVE    SPA-PTNUM-KOHID     TO  SPA-NAI-PTKOH-KOHID(IDX)
      *
               MOVE    "1"                 TO  SPA-PTKOH-SINKIKBN (IDY)
           END-IF
           MOVE    SPA-NAI-PTKOH-KOHID (IDX)   TO  SPA-PTKOH-KOHID (IDY)
      *    保険公費種別区分
           MOVE    SPA-NAI-PTKOH-SBTKBN (IDX)  TO  SPA-PTKOH-HKN-SBTKBN
                                                                 (IDY)
      *    期限切れ区分
           MOVE    SPA-NAI-TEKEDCHKKBN (IDX)
                                       TO  SPA-PTKOH-TEKEDCHKKBN(IDY)
           MOVE   SPA-GMN-KOHNUMMEI (IDX)  TO  SPA-PTKOH-KOHNUM-MEI(IDY)
           MOVE   SPA-GMN-KTEKSTYMD (IDX)  TO  SPA-PTKOH-TEKSTYMDW(IDY)
           MOVE   SPA-GMN-KTEKEDYMD (IDX)  TO  SPA-PTKOH-TEKEDYMDW(IDY)
           MOVE   SPA-GMN-KKAKUNINYMD(IDX) TO  SPA-PTKOH-KAKUNINYMDW
                                                                  (IDY)
      *
           MOVE   SPA-NAI-PTKOH-KEIFLG(IDX)    TO  SPA-PTKOH-KEIFLG
                                                                  (IDY)
      *
           MOVE    SPA-KOH-KAKUINFLG (IDX) TO  SPA-PTKOH-KAKUINFLG (IDY)
           MOVE    SPA-NAI-JFKCHKFLG (IDX) TO  SPA-PTKOH-JFKCHKFLG (IDY)
      *
           .
       3102-KOH-SPATBL-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費テーブル空白行削除処理
      ****************************************************************
       410323-KOHTBL-DEL-SEC      SECTION.
      *
      *    空白行をつめる
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               IF      SPA-PTKOH-KOHNUM (IDX)  =   SPACE
                   MOVE    SPA-PTKOHINF-TBL (IDX + 1)
                                           TO  SPA-PTKOHINF-TBL (IDX)
                   INITIALIZE              SPA-PTKOHINF-TBL (IDX + 1)
               END-IF
               IF      SPA-PTKOH-KOHNUM (IDX)  NOT =   SPACE
                   MOVE    IDX             TO  IDY
               END-IF
           END-PERFORM
           MOVE    IDY                 TO  SPA-PTKOH-MAX
      *
           .
      *
       410323-KOHTBL-DEL-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険と公費の期間重複チェック処理
      ****************************************************************
       41033-HKNKOH-CHK-SEC      SECTION.
      *
      *    国保（退職者含む）・生活保護 重複チェック
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-PTHKN-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-PTHKN-SAKJOKBN(IDX) =   SPACE    )  AND
                      (SPA-PTHKN-HKNNUM  (IDX) =   "060" OR "067"
      *              特別療養費追加
                                               OR  "068"
                                               OR  "069"
      *              後期高齢者
                                               OR  "039"
                                               OR  "040" )
                   PERFORM VARYING  IDY     FROM    1   BY  1
                           UNTIL   (IDY     >   SPA-PTKOH-MAX )  OR
                                   (SPA-ERRCD   NOT =   SPACE )
                      IF       SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE
                           PERFORM 410331-HKNKOH-JYUFUKU-SEC
                      END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      SPA-GMN-HKNNUM      =   "060" OR "067"
      *        特別療養費追加
                                       OR  "068"
                                       OR  "069"
               MOVE    16              TO  SPA-GMN-CUR
           ELSE
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   SPA-KOH-MAX
                   IF      SPA-NAI-PTKOH-SBTKBN (IDY)  =   "5"
                       EVALUATE    IDY
                           WHEN    1
                               MOVE    23          TO  SPA-GMN-CUR
                           WHEN    2
                               MOVE    29          TO  SPA-GMN-CUR
                           WHEN    3
                               MOVE    35          TO  SPA-GMN-CUR
                       END-EVALUATE
                       MOVE    SPA-KOH-MAX         TO  IDY
                   END-IF
               END-PERFORM
           END-IF
      **** IF      SPA-ERRCD           =   SPACE
      *        減免重複チェック
      *        PERFORM 4909-960JYUFUKU-CHK-SEC
      **** END-IF
      *
           .
       41033-HKNKOH-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    ７０歳以上の老人保険チェック処理
      ****************************************************************
       410331-RJN-HKNCHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNCHK
           MOVE    ZERO                TO  FLG-RJNCHK2
           MOVE    ZERO                TO  FLG-HKNCHK
           MOVE    ZERO                TO  FLG-HKNCHK2
           MOVE    ZERO                TO  FLG-SEIHOGO
           MOVE    ZERO                TO  SPA-HKN-IDX1
           MOVE    ZERO                TO  FLG-RJNERR
           MOVE    ZERO                TO  FLG-RJNKOHERR
           MOVE    ZERO                TO  FLG-KOKUHO
      *!!
           MOVE    ZERO                TO  FLG-KOKUHO-CHK
           MOVE    ZERO                TO  WRK-KOKUHO-CHK
      *!!
      **   PERFORM VARYING     IDX     FROM    1  BY  1
      *            UNTIL       IDX     >   SPA-PTHKN-MAX
      *        IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
      *               (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
      *               (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
      *            IF      SPA-PTHKN-HKNNUM(IDX)(1:1)  NOT =   "9"
      *                前期高齢者・後期高齢者区分判定
      *                PERFORM 4103311-HKNKBN-CHK-SEC
      *            END-IF
      *            保険が存在すること
      *            IF      SPA-PTHKN-HKN-SBTKBN (IDX)   =   "1"
      *                MOVE    1               TO  FLG-HKNCHK
      *                IF      SPA-HKN-IDX1    =   ZERO
      *                    MOVE    IDX             TO  SPA-HKN-IDX1
      *                END-IF
      *            ELSE
      *            他の自費等が存在すること
      *                MOVE    1               TO  FLG-HKNCHK2
      *            END-IF
      *        END-IF
      *****END-PERFORM
      *
      *    公費
           MOVE    ZERO                TO  SPA-KOH-IDX2
           MOVE    ZERO                TO  FLG-27KOH-OK
           MOVE    ZERO                TO  FLG-RJNCHK3
           MOVE    ZERO                TO  FLG-RJNCHK4
           MOVE    ZERO                TO  FLG-RJNCHK5
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               IF     (SPA-PTKOH-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
      *            老人
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "027" )  OR
                          (SPA-PTKOH-HKN-SBTKBN (IDX)  =   "3" )
      *             Ｈ２０．４ 後期高齢者該当
                       OR (SPA-PTKOH-KOHNUM  (IDX) =   "968" )
                       MOVE    1                   TO  FLG-RJNCHK2
                   END-IF
      *            生活保護
                   IF      SPA-PTKOH-KOHNUM  (IDX) =   "012"
                       MOVE    1                   TO  FLG-SEIHOGO
                   END-IF
      *!!          後期高齢者非該当(H21.3)
                   IF      SPA-PTKOH-KOHNUM  (IDX) =   "977"
                       MOVE    1                   TO  FLG-RJNCHK3
                       MOVE    1                   TO  FLG-RJNCHK5
                   END-IF
      *!!
      *!!          高齢者非該当(H22.9)
                   IF      SPA-PTKOH-KOHNUM  (IDX) =   "976"
                       MOVE    1                   TO  FLG-RJNCHK3
                       MOVE    1                   TO  FLG-RJNCHK4
                   END-IF
      *!!
               END-IF
      *        Ｈ１８．１０改正 老人２割を３割へ
               IF     (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "027" )  AND
                          (SPA-PTKOH-TEKEDYMD(IDX) >=  "20061001") AND
                          (SPA-PTKOH-PAYKBN(IDX)(2:1)  =   "2"   )
      *                切替え後の３割があること
                       PERFORM 4103319-RJNKOH-CHK-SEC
                       IF      FLG-KOH27           =   1
                           MOVE    1                   TO  FLG-RJNKOHERR
                           MOVE    IDX                 TO  SPA-KOH-IDX2
                       END-IF
                   END-IF
               END-IF
      *        Ｈ２０．４改正 老人公費
               IF     (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"  )   AND
                      (SPA-PTKOH-KOHNUM   (IDX)     =  "027" )  AND
                      (SPA-PTKOH-TEKEDYMD (IDX)     =  "20080331")
                   MOVE    1                   TO  FLG-27KOH-OK
               END-IF
           END-PERFORM
      *!!
      *H26.8   保険を後
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
                   IF      SPA-PTHKN-HKNNUM(IDX)(1:1)  NOT =   "9"
      *                前期高齢者・後期高齢者区分判定
                       PERFORM 4103311-HKNKBN-CHK-SEC
                   END-IF
      *            保険が存在すること
                   IF      SPA-PTHKN-HKN-SBTKBN (IDX)   =   "1"
                       MOVE    1               TO  FLG-HKNCHK
                       IF      SPA-HKN-IDX1    =   ZERO
                           MOVE    IDX             TO  SPA-HKN-IDX1
                       END-IF
                   ELSE
      *            他の自費等が存在すること
                       MOVE    1               TO  FLG-HKNCHK2
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-SYSYMD          <   "20080401"
      *        前期高齢者区分があり、老人公費がない時
               IF     (FLG-RJNCHK          =   1   )
                 AND  (FLG-RJNCHK2         =   ZERO)
                 AND  (FLG-RJNERR          =   ZERO)
                 AND  (FLG-RJNKOHERR       =   ZERO)
                   CONTINUE
               ELSE
                   PERFORM 4103311-RJN-CHEK02-SEC
               END-IF
           ELSE
      *        Ｈ２０．４から
               IF      SPA-SYSYMD          >=  SPA-BYMD-75-2
      *            ７５歳以上で後期高齢者がない時
                   IF     (FLG-RJNCHK          =   2   )
                       OR (FLG-RJNCHK2         =   1   )
                       IF      (FLG-RJNCHK2        =   1   )
                           AND (FLG-RJNCHK     NOT =   2   )
                           AND (FLG-HKNCHK         =   1   )
      *                保険があり、後期非該当がない時
                           AND (FLG-RJNCHK3        =   ZERO )
                           MOVE    "0121"              TO  SPA-ERRCD
                           MOVE    10                  TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       IF     (FLG-HKNCHK         =   1   )
      *                保険があり、後期非該当がない時
                       AND    (FLG-RJNCHK3        =   ZERO )
                           IF      FLG-HKNCHK2        =    ZERO
                               MOVE    "0120"              TO  SPA-ERRCD
                           ELSE
                               MOVE    "0121"              TO  SPA-ERRCD
                           END-IF
                           MOVE    10                  TO  SPA-GMN-CUR
                       END-IF
                   END-IF
      *!!
      *            後期非該当があり、前期高齢者保険がない時
                   IF     (FLG-RJNCHK3         =   1 )
                   AND    (FLG-HKNCHK          =   1 )
                   AND    (FLG-RJNCHK          =   ZERO )
      *            高齢者非該当以外
                   AND    (FLG-RJNCHK4         =   ZERO )
                       MOVE    "0128"              TO  SPA-ERRCD
                       MOVE    11                  TO  SPA-GMN-CUR
                   END-IF
      *!!
      *H26.8       後期非該当で１割エラー（H26.5から）
                   IF      FLG-RJNERR          =   4
                       MOVE    "0129"              TO  SPA-ERRCD
                       MOVE    11                  TO  SPA-GMN-CUR
                   END-IF
      *
      *            終了する保険を再表示
                   IF     (SPA-ERRCD           =   "0121" )
                      AND (SPA-HKN-IDX1    NOT =   ZERO   )
                      AND (SPA-NAI-HKNSTR  NOT =   SPA-HKN-IDX1)
                       INITIALIZE                  SPA-GMN-HKN-AREA
                       MOVE    SPA-HKN-IDX1    TO  IDX
                       MOVE    IDX             TO  SPA-NAI-HKNSTR
                       PERFORM 3101-HOKEN-HEN-SEC
                   END-IF
               ELSE
      *            ７０歳から７５歳まで
      *            前期高齢者区分があり、老人公費がない時
                   IF     (FLG-RJNCHK          =   1   )
                     AND  (FLG-RJNCHK2         =   ZERO)
                     AND  (FLG-RJNERR          =   ZERO)
                     AND  (FLG-RJNKOHERR       =   ZERO)
                       CONTINUE
                   ELSE
      *              高齢者非該当以外
                     IF   (FLG-RJNCHK4         =   ZERO   )
                       PERFORM 4103311-RJN-CHEK02-SEC
                   END-IF
                   END-IF
               END-IF
           END-IF
      *!!!
      *    前期高齢者（７０歳）で保険の期間変更がない場合、確認年月日チェック
           IF      (SPA-ERRCD          =   SPACE )
              AND  (SPA-PIDCD          =   SPACE )
              AND  (FLG-KOKUHO-CHK     =   1     )
               MOVE    WRK-KOKUHO-CHK      TO  IDX
               IF     (SPA-PTHKN-KAKUNINYMD (IDX)
                                           >=  SPA-BYMD-70 )
                 OR   (SPA-PTHKN-KAKUNINYMD (IDX)
                                           >=  SPA-SYSYMD  )
                    CONTINUE
               ELSE
                   IF      SPA-KEI-KOUKI3      =   ZERO
                       MOVE    "K151"              TO  SPA-ERRCD
                       MOVE    17                  TO  SPA-GMN-CUR
                       MOVE    1                   TO  SPA-KEI-KOUKI3
                       INITIALIZE                  SPA-GMN-HKN-AREA
                       MOVE    IDX             TO  SPA-NAI-HKNSTR
                       MOVE    IDX             TO  SPA-HKN-IDX1
                       PERFORM 3101-HOKEN-HEN-SEC
                   END-IF
               END-IF
           END-IF
      *
      *
           .
       410331-RJN-HKNCHK-EXT.
           EXIT.
      ****************************************************************
      *    前期高齢者・老人公費の切替え・エラー処理
      ****************************************************************
       4103311-RJN-CHEK02-SEC     SECTION.
      *
      *    老人保険がなく、保険が存在する時
           IF     (FLG-RJNCHK          =   ZERO  )  AND
                  (FLG-RJNCHK2         =   ZERO  )  AND
                  (FLG-HKNCHK          =   1     )
             AND  (FLG-27KOH-OK        =   ZERO )
      *        Ｓ７．９．３０以前の時老人公費があることとする
               IF     (SPA-NAI-BIRTHDAY    <=  "19320930" )  OR
                      (SPA-SYSYMD          >=  SPA-BYMD-75)
                   MOVE    "0105"              TO  SPA-ERRCD
                   MOVE    20                  TO  SPA-GMN-CUR
               ELSE
      *        高齢者保険を自動変換へ
      *            開始日が７０歳以上
                   IF      SPA-PTHKN-TEKSTYMD(SPA-HKN-IDX1)
                                               >=  SPA-BYMD-70
                       MOVE    "1066"              TO  SPA-ERRCD
                       MOVE    11                  TO  SPA-GMN-CUR
                   ELSE
                       MOVE    SPA-PTHKN-HKNNUM-MEI(SPA-HKN-IDX1)
                                                   TO  SPA-PIDMSG
                       MOVE    "0104"              TO  SPA-PIDCD
                       MOVE    8                   TO  SPA-GMN-CUR
                  END-IF
               END-IF
           END-IF
      *    老人公費がなく、２割の保険が存在する時
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )
               IF     (FLG-RJNCHK          =   1     )  AND
                      (FLG-RJNERR          =   1     )  AND
                      (FLG-RJNCHK2         =   ZERO  )
                 AND  (FLG-27KOH-OK        =   ZERO )
                   MOVE    SPA-PTHKN-HKNNUM-MEI(SPA-HKN-IDX1)
                                                   TO  SPA-PIDMSG
                   MOVE    "0110"              TO  SPA-PIDCD
                   MOVE    8                   TO  SPA-GMN-CUR
               END-IF
      *        補助区分選択エラー
               IF      FLG-RJNERR          =   2
                   MOVE    "1067"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
               IF      FLG-RJNERR          =   3
                   MOVE    "1068"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    老人公費２割の保険が存在する時（VER.3.2.0から）
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )
               IF     (FLG-RJNKOHERR       =   1     )
      *            画面位置
                   IF      SPA-KOH-MAX         >=  3
      *                行挿入
                       MOVE    ZERO                TO  SPA-HKNKOH-FLG
                       PERFORM 4104-KOH-ADD-SEC
                       MOVE    1                   TO  SPA-KOH-GMN2
                   ELSE
                       COMPUTE SPA-KOH-GMN2   =   SPA-KOH-MAX
                                              +   1
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
      *
                   MOVE    ZERO                TO  SPA-KOH-GMN1
                   MOVE    ZERO                TO  FLG-KOH27
                   PERFORM VARYING     IDX-K   FROM    1   BY  1
                           UNTIL       IDX-K   >   3
                       IF     (SPA-NAI-PTKOH-EDANUM(IDX-K)
                                               =   SPA-KOH-IDX2 )
                         AND  (SPA-GMN-KOHNUM (IDX-K)  =   "027" )
                           MOVE    IDX-K           TO  SPA-KOH-GMN1
                       END-IF
                   END-PERFORM
                   MOVE    "0111"              TO  SPA-PIDCD
                   MOVE    21                  TO  SPA-GMN-CUR
      *
               END-IF
      *
               END-IF
           END-IF
      *
           .
       4103311-RJN-CHEK02-EXT.
           EXIT.
      ****************************************************************
      *    前期高齢者・後期高齢者区分処理
      ****************************************************************
       4103311-HKNKBN-CHK-SEC     SECTION.
      *
      *H26.4
      *    H26.5.1以降のS19.4.2生以降の高齢者負担割合
           MOVE    ZERO                TO  FLG-RJN-02
           IF      (SPA-NAI-BIRTHDAY       >=  "19440402")
             AND   (SPA-PTHKN-TEKSTYMD (IDX)   >=  "20140501")
      *        前期高齢者到達後は、対象
               IF     (SPA-PTHKN-TEKSTYMD (IDX)   >=  SPA-BYMD-70)
                  OR ((SPA-SYSYMD                 >=  SPA-BYMD-70 ) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)   >=  SPA-BYMD-70))
                   MOVE    1               TO  FLG-RJN-02
               END-IF
           END-IF
      *H26.8
      *    高齢者非該当も負担割合変更
           IF      FLG-RJN-02          =   ZERO
               IF     (SPA-PTHKN-TEKSTYMD (IDX)   >=  SPA-BYMD-75-2)
                  OR ((SPA-SYSYMD                 >=  SPA-BYMD-75-2) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)   >=  SPA-BYMD-75-2))
                   IF     (FLG-RJNCHK5         =   1   )
                       MOVE    2               TO  FLG-RJN-02
                   END-IF
               END-IF
           END-IF
      *
      *
           EVALUATE    SPA-PTHKN-HKNNUM  (IDX)
      *        国保
               WHEN    "060"
               WHEN    "068"
      *            Ｈ１８．１０まで
                   IF     (SPA-PTHKN-TEKEDYMD (IDX) <  "20061001")
                       IF    (SPA-PTHKN-HOJOKBN (IDX)  =   "1"
                                                       OR  "2"
                                                       OR  "4"
                                                       OR  "5")
                           MOVE    1               TO  FLG-RJNCHK
                       END-IF
                   ELSE
                       IF    (SPA-BYMD-70          <  "20061001")
                         AND (SPA-PTHKN-TEKSTYMD (IDX)
                                                   <= SPA-BYMD-70)
                         AND (SPA-SYSYMD           <  "20061001")
                           IF    (SPA-PTHKN-HOJOKBN (IDX)
                                                       =   "1"
                                                       OR  "2"
                                                       OR  "4"
                                                       OR  "5")
                               MOVE    1           TO  FLG-RJNCHK
                           END-IF
                       ELSE
      *                Ｈ１８．１０以降
                           IF      SPA-PTHKN-HOJOKBN (IDX)  NOT =   "0"
                               MOVE    1               TO  FLG-RJNCHK
                           END-IF
                       END-IF
      *H26.4
                       IF     FLG-RJN-02           =   ZERO
      *                    ２割エラー
                           MOVE    "2"             TO  WRK-HOJO-ERR
                       ELSE
      *                    １割エラー
                           MOVE    "1"             TO  WRK-HOJO-ERR
                       END-IF
      *                組合国保以外は２割エラー
                       IF    (SPA-PTHKN-HKNJANUM(IDX)(3:1)
                                                   NOT =  "3" ) AND
                             (SPA-PTHKN-HOJOKBN (IDX)  =   WRK-HOJO-ERR)
                           IF      SPA-PTHKN-TEKSTYMD (IDX)
                                                       <  "20061001"
                               MOVE    1           TO  FLG-RJNERR
                               MOVE    IDX         TO  SPA-HKN-IDX1
                           ELSE
                               MOVE    2           TO  FLG-RJNERR
                               IF      WRK-HOJO-ERR    =   "1"
                                   MOVE   3            TO  FLG-RJNERR
                               END-IF
                               MOVE    IDX         TO  SPA-HKN-IDX1
                           END-IF
                       END-IF
      *                組合国保
                       IF    (SPA-PTHKN-HKNJANUM(IDX)(3:1)
                                                   =  "3" )
                           MOVE    1               TO  FLG-KOKUHO
                       END-IF
      *!!
      *                開始日が７０歳未満で終了日が７０歳以上
                       IF     (SPA-PTHKN-TEKSTYMD (IDX)
                                                   <   SPA-BYMD-70 ) AND
                              (SPA-PTHKN-TEKEDYMD (IDX)
                                                   >   SPA-BYMD-70 )
                           MOVE    1               TO  FLG-KOKUHO-CHK
                           MOVE    IDX             TO  WRK-KOKUHO-CHK
                       END-IF
      *!!
                   END-IF
      *        船員で補助区分あり
               WHEN    "002"
                   IF     (SPA-PTHKN-HOJOKBN (IDX)     =   "8" OR "9"
                                                       OR  "A" OR "B"
                                                       OR  "C" OR "D"
                                                       OR  "E" OR "F"
                                                       OR  "7"
                                                       OR  "G" OR "H"
                                                       OR  "I"        )
                       MOVE    1                   TO  FLG-RJNCHK
      *                Ｈ１８．１０改正（老人２割は変更）
                       IF     (SPA-PTHKN-TEKSTYMD (IDX) <   "20061001")
                          AND (SPA-PTHKN-TEKEDYMD (IDX) >=  "20061001")
                           IF     (SPA-PTHKN-HOJOKBN (IDX) =   "8"
                                                           OR  "D"
                                                           OR  "E"
                                                           OR  "F"  )
                              AND (SPA-PTHKN-CONTKBN (IDX) =   SPACE)
                               MOVE    1               TO  FLG-RJNERR
                               MOVE    IDX             TO  SPA-HKN-IDX1
                           END-IF
                       END-IF
                   END-IF
      *H26.8
      *           高齢者非該当チェック
                  IF      (FLG-RJN-02         =   2  )
                       IF     (SPA-PTHKN-HOJOKBN (IDX) =   "9"
                                                       OR  "A"
                                                       OR  "B"
                                                       OR  "C"  )
                          AND (SPA-PTHKN-HONKZKKBN (IDX) =   "2"  )
                           MOVE    4               TO  FLG-RJNERR
                           MOVE    IDX             TO  SPA-HKN-IDX1
                       END-IF
                   END-IF
      *
      *            本人で下船後なら後期高齢者扱いとする
                   IF      (SPA-PTHKN-HOJOKBN (IDX)    =   "A"
                                                       OR  "B"
                                                       OR  "C"
                                                       OR  "D"
                                                       OR  "E"
                                                       OR  "F"
                                                       OR  "G"
                                                       OR  "H"
                                                       OR  "I" )
                       AND (SPA-PTHKN-HONKZKKBN(IDX)   =   "1" )
                       AND (SPA-PTHKN-TEKEDYMD (IDX)   >=  "20080401")
                       MOVE    2                   TO  FLG-RJNCHK
                   END-IF
      *!!
      *                開始日が７０歳未満で終了日が７０歳以上
                       IF     (SPA-PTHKN-TEKSTYMD (IDX)
                                                   <   SPA-BYMD-70 ) AND
                              (SPA-PTHKN-TEKEDYMD (IDX)
                                                   >   SPA-BYMD-70 )
                           MOVE    1               TO  FLG-KOKUHO-CHK
                           MOVE    IDX             TO  WRK-KOKUHO-CHK
                       END-IF
      *!!
      *        共済
               WHEN    "031"
               WHEN    "032"
               WHEN    "033"
               WHEN    "034"
                   IF     (SPA-PTHKN-HOJOKBN (IDX)     =   "8" OR "9"
                                                       OR  "7"
                                                       OR  "B" OR "E"
                                                       OR  "H"       )
      *                Ｈ１８．１０改正（老人２割は変更）
                       IF     (SPA-PTHKN-TEKSTYMD (IDX) <   "20061001")
                          AND (SPA-PTHKN-TEKEDYMD (IDX) >=  "20061001")
                           IF     (SPA-PTHKN-HOJOKBN (IDX) =   "8"
                                                           OR  "E"  )
                              AND (SPA-PTHKN-CONTKBN (IDX) =   SPACE)
                               MOVE    1               TO  FLG-RJNERR
                               MOVE    IDX             TO  SPA-HKN-IDX1
                           END-IF
                       END-IF
      *H26.8
      *               高齢者非該当チェック
                      IF      (FLG-RJN-02         =   2  )
                           IF     (SPA-PTHKN-HOJOKBN (IDX) =   "9"
                                                           OR  "B"  )
                              AND (SPA-PTHKN-CONTKBN (IDX) =   SPACE)
                               MOVE    4               TO  FLG-RJNERR
                               MOVE    IDX             TO  SPA-HKN-IDX1
                           END-IF
                       END-IF
      *
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
      *        後期高齢者
               WHEN    "039"
               WHEN    "040"
                   MOVE    2                   TO  FLG-RJNCHK
      *        その他
               WHEN    OTHER
                   IF      SPA-PTHKN-HOJOKBN (IDX) =   "8"  OR  "9"
                                                   OR  "7"
                       MOVE    1                   TO  FLG-RJNCHK
                   END-IF
      *            Ｈ１８．１０改正（老人２割は変更）
                   IF     (SPA-PTHKN-TEKSTYMD (IDX) <   "20061001")
                      AND (SPA-PTHKN-TEKEDYMD (IDX) >=  "20061001")
                       IF     (SPA-PTHKN-HOJOKBN (IDX) =   "8" )
                          AND (SPA-PTHKN-CONTKBN (IDX) =   SPACE)
                           MOVE    1               TO  FLG-RJNERR
                           MOVE    IDX             TO  SPA-HKN-IDX1
                        END-IF
                   END-IF
      *H26.8
      *            高齢者非該当チェック
                   IF      (FLG-RJN-02         =   2  )
                      AND  (SPA-PTHKN-HOJOKBN (IDX)   =   "9" )
                       IF     (SPA-PTHKN-CONTKBN (IDX) =   SPACE)
                           MOVE    4               TO  FLG-RJNERR
                           MOVE    IDX             TO  SPA-HKN-IDX1
                        END-IF
                   END-IF
           END-EVALUATE
      *
           .
       4103311-HKNKBN-CHK-EXT.
           EXIT.
      ****************************************************************
      *    老人公費２割チェック処理
      ****************************************************************
       410332-RJN27-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNKOHERR
      *DDD MOVE    ZERO                TO  FLG-KOKUHO
      *    PERFORM VARYING     IDX     FROM    1  BY  1
      *            UNTIL       IDX     >   SPA-PTHKN-MAX
      *        IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
      *               (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
      *               (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
      *            組合国保の有無
      *            IF     (SPA-PTHKN-HKNNUM  (IDX) =   "060"
      *                                            OR  "068" )  AND
      *                   (SPA-PTHKN-TEKEDYMD (IDX) <   "20061001")
      *                組合国保
      *                IF    (SPA-PTHKN-HKNJANUM(IDX)(3:1)
      *                                            =  "3" )
      *                    MOVE    1               TO  FLG-KOKUHO
      *                END-IF
      *            END-IF
      *        END-IF
      *DDD END-PERFORM
      *
      *    公費
           MOVE    ZERO                TO  SPA-KOH-IDX2
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
      ******** IF     (SPA-PTKOH-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
               IF     (SPA-PTKOH-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
      *            Ｈ１８．１０改正 老人２割を３割へ
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "027" )  AND
                          (SPA-PTKOH-TEKEDYMD(IDX) >=  "20061001") AND
                          (SPA-PTKOH-PAYKBN(IDX)(2:1)  =   "2"   )
      *                切替え後の３割があること
                       PERFORM 4103319-RJNKOH-CHK-SEC
                       IF      FLG-KOH27           =   1
                           MOVE    1                   TO  FLG-RJNKOHERR
                           MOVE    IDX                 TO  SPA-KOH-IDX2
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
      *    老人公費２割の保険が存在する時
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PIDCD           =   SPACE )
               IF     (FLG-RJNKOHERR       =   1     )
      *            画面位置
                   IF      SPA-KOH-MAX         >=  3
      *                行挿入
                       MOVE    ZERO                TO  SPA-HKNKOH-FLG
                       PERFORM 4104-KOH-ADD-SEC
                       MOVE    1                   TO  SPA-KOH-GMN2
                   ELSE
                       COMPUTE SPA-KOH-GMN2   =   SPA-KOH-MAX
                                              +   1
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
      *
                   MOVE    ZERO                TO  SPA-KOH-GMN1
                   MOVE    ZERO                TO  FLG-KOH27
                   PERFORM VARYING     IDX-K   FROM    1   BY  1
                           UNTIL       IDX-K   >   3
                       IF     (SPA-NAI-PTKOH-EDANUM(IDX-K)
                                               =   SPA-KOH-IDX2 )
                         AND  (SPA-GMN-KOHNUM (IDX-K)  =   "027" )
                           MOVE    IDX-K           TO  SPA-KOH-GMN1
                       END-IF
                   END-PERFORM
                   MOVE    "0111"              TO  SPA-PIDCD
                   MOVE    21                  TO  SPA-GMN-CUR
               END-IF
      *
               END-IF
      *
           END-IF
           .
       410332-RJN27-CHK-EXT.
           EXIT.
      ****************************************************************
      *    老人公費終了チェック処理
      ****************************************************************
       410333-RJN27-ENDCHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-RJNCHK
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
                   IF      SPA-PTHKN-HKNNUM(IDX)    =   "039"
                                                    OR  "040"
      *                後期高齢者区分判定
                       MOVE    1               TO  FLG-RJNCHK
                   END-IF
               END-IF
           END-PERFORM
      *    公費
           MOVE    ZERO                TO  FLG-RJNCHK2
           MOVE    ZERO                TO  FLG-27KOH-OK
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
               IF     (SPA-PTKOH-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"        )
      *             Ｈ２０．４ 後期高齢者該当
                   IF     (SPA-PTKOH-KOHNUM  (IDX) =   "968" )
      *!!               後期高齢者非該当(H21.3)
                       OR (SPA-PTKOH-KOHNUM  (IDX) =   "977" )
      *!!               高齢者非該当(H22.9)
                       OR (SPA-PTKOH-KOHNUM  (IDX) =   "976" )
                       MOVE    1                   TO  FLG-RJNCHK2
                   END-IF
               END-IF
      *        Ｈ２０．４改正 老人公費
               IF     (SPA-PTKOH-SAKJOKBN (IDX) NOT =  "1"  )   AND
                      (SPA-PTKOH-KOHNUM   (IDX)     =  "027" )  AND
                      (SPA-PTKOH-TEKEDYMD (IDX)     =  "20080331")
                  IF     (SPA-PTKOH-KAKUNINYMD (IDX)   <  "20080401")
      *            ４月以降は当日
                  OR    ((SPA-PTKOH-KAKUNINYMD (IDX)   >=  "20080401")
                     AND (SPA-PTKOH-KAKUNINYMD (IDX)   =   SPA-SYSYMD))
                       MOVE    1                   TO  FLG-27KOH-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ２７老人公費がＨ２０．３．３１で終了、かつ、後期がない時
           IF     (FLG-27KOH-OK        =   1     )
               IF     (FLG-RJNCHK          =   1    )  OR
                      (FLG-RJNCHK2         =   1    )
                   CONTINUE
               ELSE
                   IF      SPA-KEI-KOUKI2       =   ZERO
                       MOVE    1                   TO  SPA-KEI-KOUKI2
                       MOVE    "K123"              TO  SPA-ERRCD
                       MOVE    10                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       410333-RJN27-ENDCHK-EXT.
           EXIT.
      *!!
      ****************************************************************
      *    退職国保チェック処理
      ****************************************************************
       410334-TAIKOKUHO-CHK-SEC     SECTION.
      *
           MOVE    ZERO                TO  WRK-NAI-HKNSTR
           MOVE    ZERO                TO  FLG-HKNCHK3
           MOVE    ZERO                TO  FLG-HKNCHK2
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               IF     (SPA-PTHKN-TEKSTYMD (IDX)    <=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-SYSYMD ) AND
                      (SPA-PTHKN-SAKJOKBN (IDX) NOT =  "1"        )
                   IF     (SPA-PTHKN-HKNNUM   (IDX)    =   "067"  ) AND
                          (SPA-PTHKN-TEKEDYMD (IDX)    >=  SPA-BYMD-65)
                       MOVE    IDX             TO  WRK-NAI-HKNSTR
                       MOVE    1                   TO  FLG-HKNCHK3
                   END-IF
      *            他保険
                   IF     (SPA-PTHKN-HKNNUM (IDX)  NOT =   "067"  )
                       MOVE    1                   TO  FLG-HKNCHK2
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-NAI-HKNSTR      >   ZERO
               MOVE    "1051"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
               IF      WRK-NAI-HKNSTR      NOT =   SPA-NAI-HKNSTR
                   INITIALIZE                  SPA-GMN-HKN-AREA
                   MOVE    WRK-NAI-HKNSTR      TO  SPA-NAI-HKNSTR
                   MOVE    SPA-NAI-HKNSTR      TO  IDX
                   PERFORM 3101-HOKEN-HEN-SEC
               END-IF
           END-IF
           .
       410334-TAIKOKUHO-CHK-EXT.
           EXIT.
      *!!
      *
      ****************************************************************
      *    保険と公費の期間重複チェック処理
      ****************************************************************
       4103319-RJNKOH-CHK-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-KOH27
           PERFORM VARYING     IDXK    FROM    1   BY  1
                   UNTIL       IDXK    >   SPA-GMN-KOHMAX
               IF     (SPA-GMN-KOHNUML (IDXK)   =   "027" )  AND
                      (SPA-GMN-KOHKBNL (IDXK)   =   "3"   )
                   MOVE    1               TO  FLG-KOH27
                   MOVE    SPA-GMN-KOHMAX  TO  IDXK
               END-IF
           END-PERFORM
      *
           .
       4103319-RJNKOH-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険と公費の期間重複チェック処理
      ****************************************************************
       410331-HKNKOH-JYUFUKU-SEC      SECTION.
      *
          IF     (SPA-PTKOH-TEKEDYMD (IDY)
                                       <   SPA-PTHKN-TEKSTYMD (IDX)) OR
                  (SPA-PTKOH-TEKSTYMD (IDY)
                                       >   SPA-PTHKN-TEKEDYMD (IDX))
               CONTINUE
           ELSE
      *        生活保護の場合エラー
               IF      SPA-PTKOH-HKN-SBTKBN (IDY)  =   "5"
                   MOVE    "0089"              TO  SPA-ERRCD
      *
                   IF      SPA-PTHKN-HKNNUM(IDX)    =   "039"
                                                    OR  "040"
                       MOVE    "0124"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *        残留邦人の場合エラー
               IF      SPA-PTKOH-KOHNUM (IDY)  =   "025"
                   MOVE    "0126"              TO  SPA-ERRCD
      *
                   IF      SPA-PTHKN-HKNNUM(IDX)    =   "039"
                                                    OR  "040"
                       MOVE    "0125"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       410331-HKNKOH-JYUFUKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費追加処理
      ****************************************************************
       4104-KOH-ADD-SEC      SECTION.
      *
           IF      SPA-HKNKOH-FLG  NOT =   1
      *        公費画面の内容をＴＢＬへ編集する
               PERFORM 41032-KOH-SPA-TBL-SET-SEC
           END-IF
      *    テーブル件数
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PTKOH-MAX       >=  TBL-MAX-KOH )
               MOVE    "0106"          TO  SPA-ERRCD
               GO      TO      4104-KOH-ADD-EXT
           END-IF
      *
      *    最初の１行を開ける
           IF      SPA-NAI-PTKOH-EDANUM(1) NOT =   ZERO
               MOVE    SPA-NAI-PTKOH-EDANUM(1) TO  IDX-K2
               INITIALIZE                  SPA-GMN-KOH-AREA
               MOVE    1                   TO  IDY
               PERFORM VARYING     IDX     FROM    IDX-K2 BY  1
                       UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                                  (IDY     >=  3    )
                   IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                       ADD     1                   TO  IDY
                       MOVE    IDY                 TO  SPA-KOH-MAX
                       PERFORM 3102-KOUHI-HEN-SEC
                   END-IF
               END-PERFORM
           ELSE
               INITIALIZE                  SPA-GMN-KOH-AREA
           END-IF
           MOVE    20                  TO  SPA-GMN-CUR
      *
           .
       4104-KOH-ADD-EXT.
           EXIT.
      *
      *
      ****************************************************************
      *    漢字氏名編集処理
      ****************************************************************
       4101-KANJI-NAME-EDIT-SEC     SECTION.
      *
           MOVE    SPA-GMN-KANANAME
                                   TO  SPA-GMN-OLD-KANANAME
           MOVE    SPA-GMN-NAME
                                   TO  SPA-GMN-OLD-NAME
      *
      **** 人名辞書不使用の場合
      *    IF      SPA-NAI-JINMEI-USE  =   "1"
      *        GO      TO      4101-KANJI-NAME-EDIT-EXT
      **** END-IF
      *
           MOVE    SPACE           TO  WRK-NAME-AREA
      *
           MOVE    SPACE           TO  SPA-P100-AREA
           INITIALIZE              SPA-P100-AREA
      *
      *    全角スペースを"@@"に置き換える
      *    （これをしておかないと"ァー"を含む文字列が
      *    うまく分解できない。）
           MOVE    SPA-GMN-KANANAME
                                   TO  WRK-NAME-X-AREA
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >   25 )
                OR       ( WRK-NAME-X (IDX1) = SPACE )
      *
               IF  ( WRK-NAME-X (IDX1) =   "　" )
                   MOVE    "@@"    TO  WRK-NAME-X (IDX1)
               END-IF
      *
           END-PERFORM
      *
      *    ミドルネームがある人を考慮して編集を行う
           UNSTRING    WRK-NAME-X-AREA
                                   DELIMITED BY    "@@" OR SPACE
           INTO    WRK-NAME-VAL (1)
                   WRK-NAME-VAL (2)
                   WRK-NAME-VAL (3)
           END-UNSTRING
      *
           IF ( WRK-NAME-VAL (3)   NOT =   SPACE )
               MOVE    WRK-NAME-VAL (1)
                                   TO  WRK-NAME-KANA (1)
               MOVE    WRK-NAME-VAL (3)
                                   TO  WRK-NAME-KANA (2)
           ELSE
               MOVE    WRK-NAME-VAL (1)
                                   TO  WRK-NAME-KANA (1)
               MOVE    WRK-NAME-VAL (2)
                                   TO  WRK-NAME-KANA (2)
           END-IF
      *
      *    氏編集
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       2 )
      *
               PERFORM 4101-KANJI-NAME-SELECT-SEC
      *
               MOVE    WRK-KANJINAME-T (1)
                                   TO  WRK-NAME-KANJI (IDX1)
               MOVE    WRK-KANJINAME-AREA
                                   TO  SPA-GMN-P100-KANJILST-ITM-AREA
                                                                (IDX1)
               MOVE    CNT-JINMEI  TO  SPA-GMN-P100-KANJILST-COUNT
                                                                (IDX1)
      *
           END-PERFORM
      *
      *    漢字氏名検索画面呼出
           PERFORM SPA-GMN-P100-CALL-SEC
      *
           .
      *
       4101-KANJI-NAME-EDIT-EXT.
           EXIT.
      ****************************************************************
      *    漢字氏名検索処理
      ****************************************************************
       4101-KANJI-NAME-SELECT-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-NAME (IDX1)
      *
           MOVE    SPACE               TO  WRK-KANJINAME-AREA
           MOVE    ZERO                TO  FLG-JINMEI
                                           CNT-JINMEI
      *
           IF  ( WRK-NAME-KANA (IDX1)  =   SPACE )
               MOVE    1               TO  FLG-NAME (IDX1)
               GO  TO  4101-KANJI-NAME-SELECT-EXT
           END-IF
      *
           MOVE    WRK-NAME-KANA (IDX1)
                                       TO  JINMEI-KANA
      *
      *    人名テーブル検索処理
           PERFORM 941-JINMEI-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-JINMEI  =   1 )
                    OR   ( CNT-JINMEI >=   50 )
      *
      *        人名テーブル読込処理
               PERFORM 941-JINMEI-NEXT-SEC
               IF  ( FLG-JINMEI        =   ZERO )
                   COMPUTE CNT-JINMEI  =   CNT-JINMEI  +   1
                   MOVE    JINMEI-KANJI
                                       TO  WRK-KANJINAME-T (CNT-JINMEI)
               END-IF
      *
           END-PERFORM
      *
           PERFORM 941-JINMEI-CLOSE-SEC
      *
           IF  ( CNT-JINMEI   >=   50 )
               GO  TO  4101-KANJI-NAME-SELECT-EXT
           END-IF
      *
      *    カタカナを一覧の最後に載せる
      *    （既にリストに存在している場合は載せない）
           SET     IDX-NAME    TO  1
           SEARCH  WRK-KANJINAME-OCC   VARYING  IDX-NAME
           AT  END
               COMPUTE CNT-JINMEI  =   CNT-JINMEI  +   1
               MOVE    WRK-NAME-KANA (IDX1)
                                   TO  WRK-KANJINAME-T (CNT-JINMEI)
           WHEN    WRK-KANJINAME-T (IDX-NAME)
                                   =   WRK-NAME-KANA (IDX1)
               CONTINUE
           END-SEARCH
           .
      *
       4101-KANJI-NAME-SELECT-EXT.
           EXIT.
      ****************************************************************
      *    カナ氏名検索画面呼出処理
      ****************************************************************
       SPA-GMN-P100-CALL-SEC           SECTION.
      *
           PERFORM VARYING IDX1    FROM    1   BY  1
               UNTIL     ( IDX1    >       2 )
               MOVE    "　　　　　　　　　　　　　　"
                                   TO  SPA-GMN-P100-KANANAME (IDX1)
      *
               STRING  WRK-NAME-KANA (IDX1)
                                   DELIMITED   BY  SPACE
               INTO    SPA-GMN-P100-KANANAME (IDX1)
               END-STRING
      *
      *
               IF  ( FLG-NAME (IDX1)   =   ZERO )
                   MOVE    WIDGET-NORMAL
                               TO  SPA-GMN-P100-SELNUM-STATE  (IDX1)
                   MOVE   1        TO  SPA-GMN-P100-SELNUM (IDX1)
                   MOVE   SPA-GMN-P100-KANJILST-KANJI (IDX1 1)
                               TO  SPA-GMN-P100-KANJINAME (IDX1)
               ELSE
                   MOVE    WIDGET-INSENSITIVE
                               TO  SPA-GMN-P100-SELNUM-STATE (IDX1)
                   MOVE    1   TO  SPA-GMN-P100-KANJILST-COUNT (IDX1)
                   MOVE    "該当なし"
                               TO  SPA-GMN-P100-KANJILST-KANJI(IDX1 1)
               END-IF
      *
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP100"          USING
                                           MCPAREA
                                           SPAAREA
                                           LINKAREA
                                           SCRAREA
      *
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-P02SCRAREA
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
           IF (SPA-GMN-P100-SELNUM-STATE (1)
                                       =   WIDGET-NORMAL )
               MOVE  "SELNUM1"         TO  MCP-WIDGET
           ELSE
               MOVE  "SELNUM2"         TO  MCP-WIDGET
           END-IF
      *
           MOVE  "P02"                 TO  SPA-MOTOPG
           MOVE  "P100"                TO  SPA-SAKIPG
      *
           MOVE  "NEW"                 TO  MCP-PUTTYPE
           MOVE  "P100"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE  1             TO  SPA-FLG-END
      *
           .
       SPA-GMN-P100-CALL-EXT.
           EXIT.
      *----(01.00.15) LINE ADD END   ---------------------------------
      ****************************************************************
      *    死亡区分チェック処理
      ****************************************************************
       490-DEATHKBN-SEC                 SECTION.
      *
           EVALUATE    SPA-GMN-DEATHKBN
               WHEN    "1"
                   MOVE    SPACE           TO  SPA-GMN-DEATHKBN
               WHEN    SPACE
               WHEN    "0"
                   MOVE    "1"             TO  SPA-GMN-DEATHKBN
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-DEATHKBN    =   "1"
      *            登録ボタンへ
                   MOVE    60                  TO  SPA-GMN-CUR
               ELSE
      *            保険へ
                   MOVE    8                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       490-DEATHKBN-EXT.
           EXIT.
      *
      ***************************************************************
      *    保険確認年月日入力済み処理
      ***************************************************************
       450-HKNKAKUNINYMD-ZUMI-SEC      SECTION.
      *
      *    労災の時、労災保険画面へ
           IF      SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"
              GO      TO      450-HKNKAKUNINYMD-ZUMI-EXT
           END-IF
      *
           IF      P02-P02-HKNNUM  NOT =   SPACE
               IF     (SPA-SYSYMD          =   SPA-NAI-HKAKUNINYMD) AND
                      (SPA-MAE-HKAKUNINYMD NOT =   SPACE         )
                   MOVE    SPA-MAE-HKAKUNINYMDW
                                               TO  P02-P02-HKAKUNINYMD
               ELSE
                   MOVE    SPA-SYSYMDWH        TO  P02-P02-HKAKUNINYMD
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       450-HKNKAKUNINYMD-ZUMI-EXT.
           EXIT.
      *!ver.4.7
      *
      ***************************************************************
      *    保険確認年月日履歴表示処理
      ***************************************************************
       4501-HKNKAKUINYMD-RRK-SEC      SECTION.
      *
           IF     (SPA-NAI-HKNSTR      >   ZERO )
           AND    (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) NOT =   "1")
               CONTINUE
           ELSE
      *        新規はエラー
               MOVE    "XXXX"              TO  SPA-ERRCD
           END-IF
      *    労災・自賠責
           IF     (SPA-GMN-HKNNUM      =   "971"
                                       OR  "973"  )
           OR     (SPA-NAI-HKNSTR      =   ZERO   )
           OR    ((SPA-NAI-HKNSTR      >   ZERO   )  AND
                  (SPA-PTHKN-SINKIKBN (SPA-NAI-HKNSTR) =   "1"))
               CONTINUE
           ELSE
               PERFORM 45011-P02T-SYORI-SEC
           END-IF
           .
       4501-HKNKAKUINYMD-RRK-EXT.
           EXIT.
      *****************************************************************
      *    保険確認年月日履歴画面処理
      *****************************************************************
       45011-P02T-SYORI-SEC   SECTION.
      *
           INITIALIZE                  SPA-P02T-AREA
           MOVE    SPA-GMN-HKNNUM-G    TO  SPA-P02T-HKNMSG
           MOVE    SPA-NAI-HKNID       TO  SPA-P02T-HKNID
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02T"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           IF      SPA-P02T-MAX        >   ZERO
               MOVE    "B01"               TO  MCP-WIDGET
               MOVE    "P02T"              TO  SPA-SAKIPG
               MOVE    "P02"               TO  SPA-MOTOPG
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02T"              TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  SPA-FLG-END
           ELSE
               MOVE    "0116"              TO  SPA-ERRCD
           END-IF
      *
           .
       45011-P02T-SYORI-EXT.
           EXIT.
      *
      ****************************************************************
      *    公費確認年月日入力済み処理
      *****************************************************************
       460-KOHKAKUNINYMD-ZUMI-SEC             SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "BK1"
                   IF      (SPA-GMN-KOHNUM (1) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (1) NOT =   "970" )
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (1) ) AND
                              (SPA-MAE-KKAKUNINYMD (1)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (1)
                                               TO  P02-P02-KKAKUNINYMD1
                           MOVE    SPA-MAE-KKAKUNINYMDW (1)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (1)
                           MOVE    SPA-MAE-KKAKUNINYMD (1)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (1)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD1
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (1)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (1)
                       END-IF
                   END-IF
               WHEN    "BK2"
                   IF      (SPA-GMN-KOHNUM (2) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (2) NOT =   "970" )
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (2) ) AND
                              (SPA-MAE-KKAKUNINYMD (2)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (2)
                                               TO  P02-P02-KKAKUNINYMD2
                           MOVE    SPA-MAE-KKAKUNINYMDW (2)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (2)
                           MOVE    SPA-MAE-KKAKUNINYMD (2)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (2)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD2
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (2)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (2)
                       END-IF
                   END-IF
               WHEN    "BK3"
                   IF      (SPA-GMN-KOHNUM (3) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (3) NOT =   "970" )
                       IF     (SPA-SYSYMD      =   SPA-NAI-KKAKUNINYMD
                                                             (3) ) AND
                              (SPA-MAE-KKAKUNINYMD (3)
                                               NOT =   SPACE )
                           MOVE    SPA-MAE-KKAKUNINYMDW (3)
                                               TO  P02-P02-KKAKUNINYMD3
                           MOVE    SPA-MAE-KKAKUNINYMDW (3)
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (3)
                           MOVE    SPA-MAE-KKAKUNINYMD (3)
                                               TO  SPA-NAI-KKAKUNINYMD
                                                                    (3)
                       ELSE
                           MOVE    SPA-SYSYMDWH
                                               TO  P02-P02-KKAKUNINYMD3
                           MOVE    SPA-SYSYMDWH
                                               TO  SPA-GMN-KKAKUNINYMD
                                                                    (3)
                           MOVE    SPA-SYSYMD  TO  SPA-NAI-KKAKUNINYMD
                                                                    (3)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       460-KOHKAKUNINYMD-ZUMI-EXT.
           EXIT.
      ****************************************************************
      *    公費期限切れチェック処理
      *****************************************************************
       460-TEKEDCHKKBN-CHK-SEC             SECTION.
      *
           EVALUATE    MCP-WIDGET
               WHEN    "CK1"
                   IF      (SPA-GMN-KOHNUM (1) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (1) NOT =   "970" )
                       IF     (SPA-NAI-TEKEDCHKKBN (1)
                                                   =   "1"    )
                           MOVE    SPACE       TO  SPA-NAI-TEKEDCHKKBN
                                                                  (1)
                       ELSE
                           MOVE    "1"         TO  SPA-NAI-TEKEDCHKKBN
                                                                  (1)
                       END-IF
                   END-IF
               WHEN    "CK2"
                   IF      (SPA-GMN-KOHNUM (2) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (2) NOT =   "970" )
                       IF     (SPA-NAI-TEKEDCHKKBN (2)
                                                   =   "1"    )
                           MOVE    SPACE       TO  SPA-NAI-TEKEDCHKKBN
                                                                  (2)
                       ELSE
                           MOVE    "1"         TO  SPA-NAI-TEKEDCHKKBN
                                                                  (2)
                       END-IF
                   END-IF
               WHEN    "CK3"
                   IF      (SPA-GMN-KOHNUM (3) NOT =   SPACE )
                     AND   (SPA-GMN-KOHNUM (3) NOT =   "970" )
                       IF     (SPA-NAI-TEKEDCHKKBN (3)
                                                   =   "1"    )
                           MOVE    SPACE       TO  SPA-NAI-TEKEDCHKKBN
                                                                  (3)
                       ELSE
                           MOVE    "1"         TO  SPA-NAI-TEKEDCHKKBN
                                                                  (3)
                       END-IF
                   END-IF
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
       460-TEKEDCHKKBN-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    二重登録疑い画面処理
      ****************************************************************
       41012-NIJYU-TRKGMN-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-P02A-CHK
           IF      FLG-P02A-SYORI      =   1
               IF     (SPA-GMN-KANANAME    =   SPACE )  OR
                      (SPA-GMN-NAME        =   SPACE )
                   MOVE    1               TO  FLG-P02A-CHK
               END-IF
           ELSE
               IF  ( SPA-GMN-KANANAME      =   SPACE )  OR
                   ( SPA-NAI-BIRTHDAY      =   SPACE )  OR
                   ( SPA-GMN-SEX           =   SPACE )
                   MOVE    1               TO  FLG-P02A-CHK
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN2A-MAX
           IF      FLG-P02A-CHK        =   ZERO
               PERFORM 41013-NIJYU-GMN-SET-SEC
           END-IF
      *
           IF      SPA-GMN2A-MAX       >   ZERO
      *        二重登録疑い画面へ
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    "P02A"              TO  SPA-SAKIPG
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02A"              TO  MCP-WINDOW
               MOVE    "B01"               TO  MCP-WIDGET
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  SPA-FLG-END
           END-IF
           .
       41012-NIJYU-TRKGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    二重登録画面　患者マスタ検索処理
      *****************************************************************
       41013-NIJYU-GMN-SET-SEC   SECTION.
      *
           INITIALIZE                  SPA-P02A-AREA
           MOVE    FLG-P02A-SYORI      TO  SPA-P02A-SYORI
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02A"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           .
       41013-NIJYU-GMN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ医療機関の同姓同名リスト処理
      *****************************************************************
       41013-GRPPTNAME-CHK-SEC          SECTION.
      *
           IF     (SPA-GMN-KANANAME    =   SPACE )  OR
                  (SPA-NAI-BIRTHDAY    =   SPACE )  OR
                  (SPA-GMN-SEX         =   SPACE )
               CONTINUE
           ELSE
               MOVE    1               TO  SPA-P02G-FLG
               PERFORM 410131-P02G-SYORI-SEC
           END-IF
           .
       41013-GRPPTNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    グループ医療機関の同姓同名リスト処理
      *****************************************************************
       410131-P02G-SYORI-SEC          SECTION.
      *
           INITIALIZE                  SPA-P02G-AREA
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02G"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           IF      SPA-P02G-MAX    >   ZERO
      *        グループ医療機関同一患者一覧へ
               MOVE    "P02"               TO  SPA-MOTOPG
               MOVE    "P02G"              TO  SPA-SAKIPG
      *
               MOVE    "NEW"               TO  MCP-PUTTYPE
               MOVE    "P02G"              TO  MCP-WINDOW
               MOVE    "SELNUM"            TO  MCP-WIDGET
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                   TO  SPA-FLG-END
           END-IF
           .
       410131-P02G-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ処理
      *****************************************************************
       450-HKNCOMBI-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-ERRCHK
           IF      FLG-TOUROKU2        =   2
               CONTINUE
           ELSE
      *        入力チェック処理
               PERFORM 410-INPUT-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO      450-HKNCOMBI-EXT
               END-IF
           END-IF
      *
      *    登録の時、新規患者の時すべての保険・公費をチェックする
      *    Ｈ１５．４改正対応のため、毎回、すべてのチェックを行う
      *!   IF    ( SPA-P02-SYORI       =   1     )  AND
      *!         ( FLG-TOUROKU2    NOT =   ZERO  )
           IF     (FLG-TOUROKU     NOT =   ZERO )
            OR    (FLG-TOUROKU2        =   1    )
               PERFORM 4501-ALLHKN-CHK-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )  OR
                      (SPA-FLG-END     NOT =   ZERO  )
                   GO      TO      450-HKNCOMBI-EXT
               END-IF
           END-IF
      *
      *
      *    保険ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4501-PTHKNINF-LNK-SEC
      *    公費ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4502-PTKOHINF-LNK-SEC
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
      *
      *    保険組合せ作成
           PERFORM 4503-HKNCOMBI-SYORI-SEC
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       450-HKNCOMBI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・公費チェック処理
      *****************************************************************
       4501-ALLHKN-CHK-SEC          SECTION.
      *
      *    エラーがない時は、元へもどす
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPA-NAI-HKNSTR      TO  WRK-NAI-HKNSTR
           IF      SPA-PTHKN-MAX       >   ZERO
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-PTHKN-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
                   INITIALIZE                  SPA-GMN-HKN-AREA
      *            被保険者名の自動変更はしない
                   MOVE    1               TO  SPA-NAI-HIHKNFLG
                   IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
                       IF      WRK-NAI-HKNSTR      =   ZERO
                           MOVE    IDX             TO  WRK-NAI-HKNSTR
                       END-IF
                       MOVE    IDX             TO  SPA-NAI-HKNSTR
                       PERFORM 3101-HOKEN-HEN-SEC
      *                保険情報チェックサブ
                       CALL    "ORCSP03A"          USING
                                                   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
                       PERFORM 410312-HKN-SPATBL-SET-SEC
                   END-IF
               END-PERFORM
               IF     (SPA-ERRCD       NOT =   SPACE )  OR
                      (SPA-PIDCD       NOT =   SPACE )
                   GO      TO      4501-ALLHKN-CHK-EXT
               END-IF
               MOVE    WRK-NAI-HKNSTR      TO  SPA-NAI-HKNSTR
               MOVE    SPA-NAI-HKNSTR      TO  IDX
               IF      IDX                 =   ZERO
                   MOVE    1               TO  IDX
               END-IF
               INITIALIZE                  SPA-GMN-HKN-AREA
               PERFORM 3101-HOKEN-HEN-SEC
           END-IF
      *
      *    公費は３件以上の時行う
           IF     (SPA-PTKOH-MAX       =   ZERO )  OR
                  (SPA-NAI-KOHEND      =   SPA-PTKOH-MAX )
               GO      TO      4501-ALLHKN-CHK-EXT
           END-IF
      *
           MOVE    SPA-NAI-KOHSTR      TO  WRK-NAI-KOHSTR
           MOVE    ZERO                TO  IDY
           MOVE    SPA-GMN-KOH-AREA    TO  WRK-GMN-KOH-AREA
           IF      WRK-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  WRK-NAI-KOHSTR
           END-IF
      *
           PERFORM VARYING     IDX     FROM    WRK-NAI-KOHSTR BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDY                 =   ZERO
                   INITIALIZE                  SPA-GMN-KOH-AREA
                   MOVE    ZERO                TO  SPA-NAI-KOHEND
               END-IF
               IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
               IF      IDY                 >=  3
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    ZERO                TO  IDY
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-PIDCD           =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               IF      IDY                 >   ZERO
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
               END-IF
           END-IF
      *    元へ戻す
           IF     (SPA-ERRCD           =   SPACE  )
           AND    (SPA-PIDCD           =   SPACE  )
               MOVE    WRK-GMN-KOH-AREA    TO  SPA-GMN-KOH-AREA
           END-IF
           .
       4501-ALLHKN-CHK-EXT.
           EXIT.
      *
      *!API
      *****************************************************************
      *    API公費変更内容編集処理
      *****************************************************************
       4509-API-KOHSPA-TBL-SEC          SECTION.
      *
           MOVE    IDX                 TO  IDX-API
      *    変更分のみ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   3
              IF     (SPA-GMN-KOHNUM      (IDX)  NOT =   SPACE ) AND
                     (SPA-NAI-PTKOH-EDANUM(IDX)  NOT =   ZERO  )
                   MOVE    SPA-NAI-PTKOH-EDANUM(IDX)   TO  IDY
                   PERFORM 3102-KOH-SPATBL-HEN-SEC
               END-IF
           END-PERFORM
      *
           MOVE    IDX-API             TO  IDX
           .
       4509-API-KOHSPA-TBL-EXT.
           EXIT.
      *!API
      *
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       4501-PTHKNINF-LNK-SEC          SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHKNINF"          TO  PARA-FILEMEI
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
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTHKN-MAX
               PERFORM 45011-HKN-SPA-PARA-SEC
      *        追加
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTHKNINF"          TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02W1 PARA HKNINF INS ERR:"  MCP-RC
                           ",PARA-KEY;" PARA-KEY "@"
               END-IF
           END-PERFORM
      *
           .
       4501-PTHKNINF-LNK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険ＳＰＡ→パラメタ編集処理
      *****************************************************************
       45011-HKN-SPA-PARA-SEC          SECTION.
      *
           MOVE    SPACE               TO  PTHKNINF-REC
           INITIALIZE                      PTHKNINF-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTHKN-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTHKN-PTID
           MOVE    SPA-PTHKN-HKNID     (IDX)   TO  PTHKN-HKNID
           MOVE    SPA-PTHKN-HKNNUM    (IDX)   TO  PTHKN-HKNNUM
           MOVE    SPA-PTHKN-HKNJANUM  (IDX)   TO  PTHKN-HKNJANUM
           MOVE    SPA-PTHKN-HIHKNJANAME(IDX)  TO  PTHKN-HIHKNJANAME
           MOVE    SPA-PTHKN-HONKZKKBN  (IDX)  TO  PTHKN-HONKZKKBN
           MOVE    SPA-PTHKN-HOJOKBN    (IDX)  TO  PTHKN-HOJOKBN
           MOVE    SPA-PTHKN-CONTKBN    (IDX)  TO  PTHKN-CONTKBN
           MOVE    SPA-PTHKN-KIGO       (IDX)  TO  PTHKN-KIGO
           MOVE    SPA-PTHKN-NUM        (IDX)  TO  PTHKN-NUM
      *R02.4
           MOVE    SPA-PTHKN-EDABAN     (IDX)  TO  PTHKN-EDABAN
           MOVE    SPA-PTHKN-SKKGETYMD  (IDX)  TO  PTHKN-SKKGETYMD
           MOVE    SPA-PTHKN-TEKSTYMD   (IDX)  TO  PTHKN-TEKSTYMD
           MOVE    SPA-PTHKN-TEKEDYMD   (IDX)  TO  PTHKN-TEKEDYMD
           MOVE    SPA-PTHKN-KAKUNINYMD (IDX)  TO  PTHKN-KAKUNINYMD
           MOVE    SPA-PTHKN-SAKJOKBN   (IDX)  TO  PTHKN-SAKJOKBN
      *
           .
       45011-HKN-SPA-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加処理
      *****************************************************************
       4502-PTKOHINF-LNK-SEC          SECTION.
      *
      *    削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHINF"          TO  PARA-FILEMEI
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
           MOVE    ZERO                TO  WRK-EDANUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
      *
               PERFORM 45021-KOH-SPA-PARA-SEC
      *
               ADD     1                   TO  WRK-EDANUM
      *        追加
               INITIALIZE                      PARA-REC
               MOVE    "P02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "PTKOHINF"          TO  PARA-FILEMEI
               MOVE    WRK-EDANUM          TO  PARA-EDANUM
               MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
      *
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "P02W1 PARA PTKOHINF INS ERR:"  MCP-RC
               END-IF
           END-PERFORM
      *
           .
       4502-PTKOHINF-LNK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険ＳＰＡ→パラメタ編集処理
      *****************************************************************
       45021-KOH-SPA-PARA-SEC          SECTION.
      *
           MOVE    SPACE               TO  PTKOHINF-REC
           INITIALIZE                      PTKOHINF-REC
      *
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-GMN-PTID        TO  PTKOH-PTID
           MOVE    SPA-PTKOH-KOHID     (IDX)   TO  PTKOH-KOHID
           MOVE    SPA-PTKOH-KOHNUM    (IDX)   TO  PTKOH-KOHNUM
           MOVE    SPA-PTKOH-PAYKBN    (IDX)   TO  PTKOH-PAYKBN
           MOVE    SPA-PTKOH-FTNJANUM  (IDX)   TO  PTKOH-FTNJANUM
           MOVE    SPA-PTKOH-JKYSNUM   (IDX)   TO  PTKOH-JKYSNUM
           MOVE    SPA-PTKOH-TEKSTYMD  (IDX)   TO  PTKOH-TEKSTYMD
           MOVE    SPA-PTKOH-TEKEDYMD  (IDX)   TO  PTKOH-TEKEDYMD
           MOVE    SPA-PTKOH-KAKUNINYMD(IDX)   TO  PTKOH-KAKUNINYMD
           MOVE    SPA-PTKOH-SAKJOKBN  (IDX)   TO  PTKOH-SAKJOKBN
           MOVE    SPA-PTKOH-TEKEDCHKKBN (IDX) TO  PTKOH-TEKEDCHKKBN
      *
           .
       45021-KOH-SPA-PARA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費ＴＢＬから公費負担へ編集処理
      *****************************************************************
       4504-PTKOHFTN-HEN-SEC          SECTION.
      *
      *    すべてを削除としておく
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-FTN1-MAX
               MOVE    "1"             TO  SPA-NAI24-FTNDELKBN (IDX)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PTKOH-MAX
      *ver.4.7
               MOVE    SPACE               TO  SPA-PTKOH-JYOGENKBN (IDX)
      *        地方公費の上限額設定対象判定
               IF     (SPA-PTKOH-KOHNUM(IDX)(1:1)  >=  "1"
                                               AND <=  "8"  )
                   PERFORM 450411-KOHJYOGENHEN-SEC
               END-IF
      *
      *        更正、育成
               IF     (SPA-PTKOH-KOHNUM(IDX)   =   "015"
                                               OR  "016"
      *            生活保護
                                               OR  "012" )
      *            特定負有（Ｈ１５．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "051" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            小児特定（０５２）（Ｈ１７．０４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "052" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20050401"))
      *
      *            精神通院（０２１）（Ｈ１８．０４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "021" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20060401"))
      *            精神入院（０２０）（Ｈ１９．４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "020" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20070401"))
      *           療養介護医療（０２４）、障害児（０７９）（Ｈ１８．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "079"
                                               OR  "024"  )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20061001"))
      *            減免（９６０、９６１）（Ｈ１８．１０以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "960"
                                               OR  "961"  )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20061001"))
      *           高額委任払い（９６４）
                 OR   (SPA-PTKOH-KOHNUM(IDX)   =   "964"  )
      *            結核入所（０１１）（Ｈ１９．４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "011" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20070401"))
      *            肝炎治療（０３８）（Ｈ２０．４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "038" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20080401"))
      *            残留邦人（０２５）（Ｈ２０．４以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "025" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20080401"))
      *H26.12
      *            難病（０５４）（Ｈ２７．１から）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "054" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20150101"))
      *H28.5
      *            原爆一般（０１９）（Ｈ２８．４から）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "019" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20160401"))
      *H28.11
      *            減額（円超（９５３）（Ｈ２７．１から）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "953" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20150401"))
      *H30.11
      *            肝がん治療（０９２）（Ｈ３０．１２以降）
                 OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "092" )  AND
                      (SPA-PTKOH-TEKSTYMD(IDX) >=  "20181201"))
      *ver.4.7
      *        地方公費で上限額設定対象
                 OR  ( SPA-PTKOH-JYOGENKBN (IDX)   NOT =   SPACE )
      *
      *            福島の地方公費（Ｈ１９．２以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "281" )  AND
      *               (SPA-PREFNUM             =   07    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20070201"))
      *
      *            北海道の地方公費（Ｈ２０．１０以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "183" )  AND
      *               (SPA-PREFNUM             =   01    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20081001"))
      *
      *            東京の特定疾患公費（Ｈ１５．１０以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "151" )  AND
      *               (SPA-PREFNUM             =   13    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            東京の４５１公費（Ｈ１５．１０以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "451" )  AND
      *               (SPA-PREFNUM             =   13    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            兵庫県の１８６公費（Ｈ１５．１０以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "186" )  AND
      *               (SPA-PREFNUM             =   28    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20031001"))
      *            愛知県の地方公費（Ｈ２１．１０以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "191" )  AND
      *               (SPA-PREFNUM             =   23    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20091001"))
      *            茨城県の地方公費（Ｈ２１．４以降）
      *          OR  ((SPA-PTKOH-KOHNUM(IDX)   =   "152" )  AND
      *               (SPA-PREFNUM             =   08    )  AND
      *               (SPA-PTKOH-TEKSTYMD(IDX) >=  "20090401"))
      *
                   MOVE    ZERO                TO  IDY
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   SPA-FTN1-MAX
                       IF      SPA-PTKOH-KOHID(IDX)
                                               =   SPA-NAI24-KOHID(IDX2)
                           MOVE    IDX2            TO  IDY
                           MOVE    SPA-FTN1-MAX    TO  IDX2
                       END-IF
                   END-PERFORM
                   IF      IDY             =   ZERO
                       COMPUTE IDY         =   SPA-FTN1-MAX     +  1
                       IF      IDY         >   60
                           MOVE    "0097"          TO  SPA-ERRCD
                       ELSE
                           MOVE    IDY         TO  SPA-FTN1-MAX
                           INITIALIZE          SPA-GMN24-FTNTBLO (IDY)
                       END-IF
                   END-IF
      *
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 45041-PTKOHFTN-TBLHEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       4504-PTKOHFTN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    地方公費負担額設定判定処理
      *****************************************************************
       450411-KOHJYOGENHEN-SEC         SECTION.
      *
           INITIALIZE                          LNK-ORCSJGNGET-REC
           MOVE    SPA-HOSPNUM             TO  LNK-ORCSJGNGET-HOSPNUM
      *    公費保険番号
           MOVE    SPA-PTKOH-KOHNUM(IDX)   TO  LNK-ORCSJGNGET-KOHHKNNUM
      *    公費適用開始日
           MOVE    SPA-PTKOH-TEKSTYMD(IDX) TO  LNK-ORCSJGNGET-TEKSTYMD
      *    都道府県番号
           MOVE    SPA-PREFNUM             TO  LNK-ORCSJGNGET-PREFNUM
      *    編集フラグ
           MOVE    "1"                     TO  LNK-ORCSJGNGET-HENFLG
           CALL    "ORCSJGNGET"        USING   LNK-ORCSJGNGET-REC
                                               SPA-AREA
           IF      LNK-ORCSJGNGET-RC   =   ZERO
      *        上限金額対象区分
               IF      LNK-ORCSJGNGET-TAISYOKBN    =   ZERO
                   MOVE    SPACE           TO  SPA-PTKOH-JYOGENKBN(IDX)
               ELSE
                   MOVE    LNK-ORCSJGNGET-TAISYOKBN
                                           TO  SPA-PTKOH-JYOGENKBN (IDX)
               END-IF
           END-IF
           .
       450411-KOHJYOGENHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費負担へ編集処理
      *****************************************************************
       45041-PTKOHFTN-TBLHEN-SEC          SECTION.
      *
           MOVE    SPA-PTKOH-KOHID     (IDX)   TO  SPA-NAI24-KOHID (IDY)
           MOVE    SPA-PTKOH-KOHNUM    (IDX)   TO  SPA-NAI24-KOHNUM(IDY)
           MOVE    SPA-PTKOH-JKYSNUM   (IDX)   TO  SPA-NAI24-TJKYSNUM
                                                                 (IDY)
           MOVE    SPA-PTKOH-PAYKBN    (IDX)   TO  SPA-NAI24-PAYKBN(IDY)
           MOVE    SPA-PTKOH-TEKSTYMD  (IDX)   TO  SPA-NAI24-TKOHSTYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKEDYMD  (IDX)   TO  SPA-NAI24-TKOHEDYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-SAKJOKBN  (IDX)   TO  SPA-NAI24-FTNDELKBN
                                                                   (IDY)
      *
           MOVE    SPA-PTKOH-KOHNUM-MEI(IDX)   TO  SPA-GMN24-TFTNKOHMEI
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKSTYMDW (IDX)   TO  SPA-GMN24-TKOHSTYMD
                                                                   (IDY)
           MOVE    SPA-PTKOH-TEKEDYMDW (IDX)   TO  SPA-GMN24-TKOHEDYMD
                                                                   (IDY)
      *    上限金額対象区分
           MOVE    SPA-PTKOH-JYOGENKBN (IDX)   TO  SPA-NAI24-TJYOGENKBN
                                                                   (IDY)
           .
       45041-PTKOHFTN-TBLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ作成処理
      *****************************************************************
       4503-HKNCOMBI-SYORI-SEC          SECTION.
      *
           MOVE    SPA-HOSPSBT         TO  WRK-HOSPSBT
           MOVE    SPA-ROUPAYKBN       TO  WRK-ROUPAYKBN
           MOVE    SPA-NAI-HOSPSBT     TO  SPA-HOSPSBT
           MOVE    SPA-NAI-ROUPAYKBN   TO  SPA-ROUPAYKBN
      *R01.6
      *    難病等＋生保保険 
           IF      SPA-GMN27-COMBKBN   =   "1"
               MOVE    "1"             TO  SPA-NAI-COMBKBN
           ELSE
               MOVE    SPACE           TO  SPA-NAI-COMBKBN
           END-IF
      *
      *    保険組合せ作成
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           CALL    "ORCGP02W2"         USING    SPA-AREA
           MOVE    WRK-HOSPSBT         TO  SPA-HOSPSBT
           MOVE    WRK-ROUPAYKBN       TO  SPA-ROUPAYKBN
      *    保険組合ＩＤ設定処理
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGP02W3"     USING    SPA-AREA
                                                SPA-P02KYOUTU
           END-IF
      *
      *    保険組合せ表示（現在対象分のみ）
           IF      SPA-ERRCD           =   SPACE
               PERFORM 3103-HKNCOMB-PARA-SEC
           END-IF
      *
           .
       4503-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険追加処理
      *****************************************************************
       460-HKN-TUIKA-SEC          SECTION.
      *
      *    追加の判定
           MOVE    1                   TO  FLG-HKN-ADD2
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      460-HKN-TUIKA-EXT
           END-IF
      *
      *    追加の判定
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  SPA-NAI-HKNSTR
           MOVE    8                   TO  SPA-GMN-CUR
      *
           .
       460-HKN-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費追加処理
      *****************************************************************
       470-KOH-TUIKA-SEC          SECTION.
      *
      *    追加の判定
           MOVE    1                   TO  FLG-KOH-ADD2
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      470-KOH-TUIKA-EXT
           END-IF
      *
           IF      FLG-KOH-ADD         =   ZERO
               MOVE    "0090"          TO  SPA-ERRCD
           END-IF
      *    テーブル件数
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-PTKOH-MAX       >=  TBL-MAX-KOH )
               MOVE    "0106"          TO  SPA-ERRCD
               GO      TO      470-KOH-TUIKA-EXT
           END-IF
      *
           EVALUATE    TRUE
               WHEN    SPA-GMN-KOHNUM(1)   =   SPACE
                   MOVE    20                  TO  SPA-GMN-CUR
               WHEN    SPA-GMN-KOHNUM(2)   =   SPACE
                   MOVE    26                  TO  SPA-GMN-CUR
               WHEN    SPA-GMN-KOHNUM(3)   =   SPACE
                   MOVE    32                  TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       470-KOH-TUIKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    タグ変更・別画面切替　表示処理
      *****************************************************************
       4101-BETUGAMEN-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU 
           MOVE    ZERO                TO  FLG-TOUROKU2
      *    保険組合せを作成する（入力チェック込み）
           PERFORM 450-HKNCOMBI-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      4101-BETUGAMEN-EXT
           END-IF
           .
       4101-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    前表示（基本：保険公費情報）処理
      *****************************************************************
       440-ZEN-GMNHYOUJI-SEC           SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      440-ZEN-GMNHYOUJI-EXT
           END-IF
      *ver.4.7.0
           IF     SPA-NAI-NEXTFLG      =   0
                                       OR  1
      *
      *    保険情報
           IF      SPA-NAI-HKNSTR      >   1
               MOVE    SPA-NAI-HKNSTR      TO  WRK-HKNSTR
      *        保険情報ＴＢＬから編集
               COMPUTE SPA-NAI-HKNSTR      =   SPA-NAI-HKNSTR  -  1
               PERFORM 4401-ZENHKN-TBL-SPA-SEC
      *        対象がない時元へ
               IF      SPA-NAI-HKNSTR      =   ZERO
                   MOVE    WRK-HKNSTR          TO  SPA-NAI-HKNSTR
                   PERFORM 3101-HKN-TBL-SPA-SEC
               END-IF
               MOVE    8                   TO  SPA-GMN-CUR
           ELSE
               IF     SPA-NAI-NEXTFLG      =   1
                   MOVE    "0111"              TO  SPA-ERRCD
               END-IF
           END-IF
      *ver.4.7.0
           END-IF
      *
      *ver.4.7.0
           IF     SPA-NAI-NEXTFLG      =   0
                                       OR  2
      *
      *    公費情報
           IF      SPA-NAI-KOHSTR      >   1
      *        対象開始位置を求める
               MOVE    ZERO                TO  WRK-KOHSTR
               MOVE    ZERO                TO  IDY
               PERFORM VARYING     IDX     FROM    1  BY  1
                       UNTIL      (IDX     >   SPA-PTKOH-MAX) OR
                                  (IDX     =   SPA-NAI-KOHSTR)
                   IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                       ADD     1               TO  IDY
                       IF      IDY             >   3
                           MOVE   1                TO  IDY
                       END-IF
                       IF      IDY             =   1
                           MOVE    IDX             TO  WRK-KOHSTR
                       END-IF
                   END-IF
               END-PERFORM
      *        公費情報ＴＢＬから編集
               IF      WRK-KOHSTR          >   ZERO
                   MOVE    WRK-KOHSTR          TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
               END-IF
               MOVE    20                  TO  SPA-GMN-CUR
           ELSE
      *        前頁なし
               IF     SPA-NAI-NEXTFLG      =   2
                   MOVE    "0113"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           END-IF
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       440-ZEN-GMNHYOUJI-EXT.
           EXIT.
      *****************************************************************
      *    保険情報パラメタファイル検索処理
      *****************************************************************
       4401-ZENHKN-TBL-SPA-SEC            SECTION.
      *
           INITIALIZE                  SPA-GMN-HKN-AREA
           MOVE    ZERO                TO  FLG-CHK
           IF      SPA-NAI-HKNSTR      =   ZERO
               CONTINUE
           ELSE
               PERFORM VARYING IDX     FROM    SPA-NAI-HKNSTR   BY  -1
                   UNTIL      (IDX         <   1             )  OR
                              (FLG-CHK NOT =   ZERO )
                   IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
      *                ＱＲデータの表示
                       IF     (SPA-QR-FLG          =   1   )  AND
                              (SPA-QR-HKNID    NOT =   ZERO )  AND
                              (SPA-QR-HKNID    NOT =   SPA-PTHKN-HKNID
                                                               (IDX))
                           CONTINUE
                       ELSE
                           MOVE    IDX             TO  SPA-NAI-HKNSTR
                           PERFORM 3101-HOKEN-HEN-SEC
                           MOVE    1               TO  FLG-CHK
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    対象の保険がない時、新規とする
           IF      FLG-CHK             =   ZERO
               MOVE    ZERO                TO  SPA-NAI-HKNSTR
           END-IF
           .
       4401-ZENHKN-TBL-SPA-EXT.
           EXIT.
      *
      *****************************************************************
      *    次表示（基本：保険公費情報）処理
      *****************************************************************
       450-TUGI-GMNHYOUJI-SEC          SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-ERRCHK
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      450-TUGI-GMNHYOUJI-EXT
           END-IF
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           IF     SPA-NAI-NEXTFLG      =   0
                                       OR  1
      *
      *    保険情報
           IF      SPA-NAI-HKNSTR      <   SPA-PTHKN-MAX
               MOVE    SPA-NAI-HKNSTR      TO  WRK-HKNSTR
      *        保険情報ＴＢＬから編集
               ADD     1                   TO  SPA-NAI-HKNSTR
               PERFORM 3101-HKN-TBL-SPA-SEC
      *        対象がない時元へ
               IF     (SPA-NAI-HKNSTR      =   ZERO )  AND
                      (WRK-HKNSTR          >   ZERO )
                   MOVE    WRK-HKNSTR          TO  SPA-NAI-HKNSTR
                   PERFORM 3101-HKN-TBL-SPA-SEC
               END-IF
               MOVE    8               TO  SPA-GMN-CUR
           ELSE
               IF     SPA-NAI-NEXTFLG      =   1
                   MOVE    "0114"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           END-IF
      *
      *ver.4.7.0
           IF     SPA-NAI-NEXTFLG      =   0
                                       OR  2
      *
      *    公費情報
           IF     (SPA-NAI-KOHEND      <   SPA-PTKOH-MAX )  AND
                  (SPA-GMN-KOHNUM(3)   NOT =   SPACE )
               MOVE    SPA-NAI-KOHSTR      TO  WRK-KOHSTR
               MOVE    SPA-NAI-KOHEND      TO  SPA-NAI-KOHSTR
      *        保険情報ＴＢＬから編集
               ADD     1                   TO  SPA-NAI-KOHSTR
               PERFORM 3101-KOH-TBL-SPA-SEC
      *        対象がない時元へ
               IF      SPA-NAI-KOHSTR      =   ZERO
                   MOVE    WRK-KOHSTR          TO  SPA-NAI-KOHSTR
                   PERFORM 3101-KOH-TBL-SPA-SEC
               END-IF
               MOVE    20                  TO  SPA-GMN-CUR
           ELSE
               IF     SPA-NAI-NEXTFLG      =   2
                   MOVE    "0115"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           END-IF
      *
           IF      SPA-GMN-CUR         =   ZERO
               MOVE    8               TO  SPA-GMN-CUR
           END-IF
      *
           .
       450-TUGI-GMNHYOUJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    住所検索へ
      *****************************************************************
       440-JYUSYO-SEC             SECTION.
      *
      ***  INITIALIZE                            P98
      *    INITIALIZE                            P99
      ***  INITIALIZE                            JPARA-REC
      *    INITIALIZE                            SPA-GMN98-TABLE
      *    MOVE  ZERO                        TO  SPA-GMN98-MAX
      *    MOVE  ZERO                        TO  SPA-GMN98-PAGE
     ****  CALL  "ORCSADR"                       USING
      *                                          SPA-P02KYOUTU
      *                                          SPA-P02SCRAREA
      *                                          P98
      *                                          P99
      ****                                       SPA-SYSYMD
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           MOVE    "P02"               TO  SPA-GMN98-MOTOPG
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
      *
      *    一覧へ
           MOVE    "P98"               TO  SPA-SAKIPG
           MOVE    "P02"               TO  SPA-MOTOPG
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "P98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  SPA-FLG-END
      *
           .
       440-JYUSYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災入力画面処理
      *****************************************************************
       481-ROUSAI-SEC             SECTION.
      *
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    "1001"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO      TO      481-ROUSAI-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
               GO      TO      481-ROUSAI-EXT
           END-IF
      *
      *    労災か自賠責が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    "971"               TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-ROSAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-ROSAI-FLG
           END-IF
      *
      *    自賠責が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM          TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    "973"               TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-JIBAI-FLG
           ELSE
               MOVE    ZERO            TO  SPA-JIBAI-FLG
           END-IF
      *
      *    第三者行為（自賠責）が登録されていること
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    SPA-SYSYMD          TO  HKN-TEKSTYMD
           MOVE    SPA-SYSYMD          TO  HKN-TEKEDYMD
           MOVE    "970"               TO  HKN-HKNNUM
           MOVE    ZERO                TO  HKN-PAYKBN
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNNUM          =   ZERO
               MOVE    1               TO  SPA-DAISAN-FLG
           ELSE
               MOVE    ZERO            TO  SPA-DAISAN-FLG
           END-IF
      *
      *
           IF     (SPA-ROSAI-FLG       =   ZERO )  AND
                  (SPA-JIBAI-FLG       =   ZERO )
            AND   (SPA-DAISAN-FLG      =   ZERO )
               MOVE    "0024"          TO  SPA-ERRCD
               MOVE    8               TO  SPA-GMN-CUR
               GO      TO              481-ROUSAI-EXT
           END-IF
      *
      *    現在表示されているかの判定
           IF      SPA-GMN-HKNNUM      =   "971"  OR
                                           "973"  OR
                                           SPACE
               CONTINUE
           ELSE
      *        最初の労災・自賠責とする
               PERFORM 4811-RSIHKN-SET-SEC
      *?       MOVE    "0022"        TO  SPA-ERRCD
      *?       MOVE    8             TO  SPA-GMN-CUR
               IF      SPA-ERRCD     NOT =   SPACE
                    GO      TO            481-ROUSAI-EXT
               END-IF
           END-IF
      *
      *    保険　遷移パラメータセット
           MOVE    SPA-P02KYOUTU         TO  SPA-WORK
      *
      *    保険　画面情報セット処理
           PERFORM 481-RSIHKNGMN-SET-SEC
      *
           MOVE    "P02"                 TO  SPA-MOTOPG
           MOVE    "P02D"                TO  SPA-SAKIPG
      *
      *    保険入力画面へ
           MOVE   "NEW"                  TO  MCP-PUTTYPE.
           MOVE   "P02D"                 TO  MCP-WINDOW.
      *    カーソル位置指定
           EVALUATE    SPA-GMN2D-HKNKBN
               WHEN    "1"
                   MOVE   3                    TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM1"            TO  MCP-WIDGET
               WHEN    "2"
                   MOVE   4                    TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM2"            TO  MCP-WIDGET
               WHEN    "3"
                   MOVE   16                   TO  SPA-GMN2D-CUR
                   MOVE   "KOHNUM3"            TO  MCP-WIDGET
               WHEN    "4"
               WHEN    "5"
                   MOVE    22                  TO  SPA-GMN2D-CUR
                   MOVE   "JIBAISEIKBN"        TO  MCP-WIDGET
               WHEN    "6"
                   MOVE    6                   TO  SPA-GMN2D-CUR
                   MOVE   "TENKI"              TO  MCP-WIDGET
               WHEN    OTHER
                   MOVE   1                    TO  SPA-GMN2D-CUR
                   MOVE   "HKNKBN"             TO  MCP-WIDGET
           END-EVALUATE
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                     TO  SPA-FLG-END
      *
           .
       481-ROUSAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険初期画面セット処理
      *****************************************************************
       4811-RSIHKN-SET-SEC           SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  IDX-R2
           PERFORM VARYING IDX-R       FROM    1   BY  1
                   UNTIL   IDX-R       >   SPA-PTHKN-MAX
              IF      (SPA-PTHKN-HKNNUM (IDX-R)    =   "971"  OR
                                                       "973" )  AND
                      (SPA-PTHKN-SAKJOKBN(IDX-R)   =   SPACE )
                   IF      SPA-PTHKN-TEKEDYMD(IDX-R)
                                                   =    "99999999"
                       IF      FLG-CHK             =   ZERO
                           MOVE    IDX-R           TO  IDX-R2
                           MOVE    1               TO  FLG-CHK
      *                ELSE
      *                    MOVE    ZERO            TO  IDX-R2
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    有効な労災・自賠責が１件の時、対象とする
      *    IF     (FLG-CHK             =   1  )  AND
      ***         (IDX-R2              >   ZERO)
           IF      IDX-R2              >   ZERO
               MOVE    IDX-R2          TO  SPA-NAI-HKNSTR
               MOVE    IDX-R2          TO  IDX
               PERFORM 3101-HOKEN-HEN-SEC
      *        本人家族
               MOVE    SPACE           TO  SPA-GMN-HONKZKKBN-G
      *        補助区分
               MOVE    SPACE           TO  SPA-GMN-HOJOKBN-G
               MOVE    SPACE           TO  SPA-GMN-HOJOKBN-LIST
               MOVE    ZERO            TO  SPA-GMN-HOJO-MAX
      *
               MOVE    SPACE           TO  SPA-GMN-CONTKBN-G
               MOVE    SPACE           TO  SPA-GMN-CONTKBN-LIST
               MOVE    ZERO            TO  SPA-GMN-CONT-MAX
      *    ELSE
      *        有効な労災・自賠責がない時、新規追加
      *??      IF      FLG-CHK             =   ZERO
      *            INITIALIZE                  SPA-GMN-HKN-AREA
      *            MOVE    ZERO                TO  SPA-NAI-HKNSTR
      *        ELSE
      *            MOVE    "0022"        TO  SPA-ERRCD
      *            MOVE    8             TO  SPA-GMN-CUR
      *        END-IF
           END-IF
           .
       4811-RSIHKN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災保険　画面情報セット処理
      *****************************************************************
       481-RSIHKNGMN-SET-SEC           SECTION.
      *
           MOVE    SPA-P02KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-P02SCRAREA      TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGP02D"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-P02SCRAREA
           MOVE    SPA-WORK        TO  SPA-P02KYOUTU
      *
           .
       481-RSIHKNGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       490-TOUROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-TOUROKU 
           MOVE    1                   TO  FLG-TOUROKU2
           MOVE    ZERO                TO  FLG-P027
      *
      *H28.5
           EVALUATE    SPA-P02-GAMEN
      *        保険組合せ履歴
               WHEN    "P027"
      ********     MOVE    2               TO  FLG-TOUROKU2
                   MOVE    1               TO  FLG-P027
           END-EVALUATE
      *
      *    保険組合せを作成する（入力チェック込み）
           PERFORM 450-HKNCOMBI-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-FLG-END     NOT =   ZERO  )
              OR  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      490-TOUROKU-EXT
           END-IF
      *    保険組合せ公費単独チェック
      **** IF     (SPA-NAI-KOHONLY     =   ZERO ) AND
      *           (SPA-TAGU-FLG        =   ZERO )
      *       AND (SPA-NAI-KOHONLY-CHKFLG  =   "1"  )
      *        PERFORM 4901-KOHONLY-CHK-SEC
      *****END-IF
      *
           IF     (SPA-TAGU-FLG        =   ZERO )  AND
                  (SPA-ERRCD           =   SPACE)
               PERFORM 4901-KOHONLY-CHK-SEC
           END-IF
      *
      *    カルテ発行ありの時、保険組合せのチェック
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-HAKKO       =   "1"
                                           OR  "2"
                                           OR  "3"
                   IF      SPA-NAI-KARUTE-HAKKO    =   "2"
                       IF      SPA-GMN-SELNUM      =   ZERO
                           PERFORM 4901-KARUTE-CHEK-SEC
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       490-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    減免重複チェック処理
      *****************************************************************
       4909-960JYUFUKU-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PTKOH-MAX )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      (SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE )  AND
                       (SPA-PTKOH-KOHNUM   (IDX)    =   "027" )
                   PERFORM VARYING  IDY     FROM    1   BY  1
                           UNTIL   (IDY     >   SPA-PTKOH-MAX )  OR
                                   (SPA-ERRCD   NOT =   SPACE )
                      IF      (SPA-PTKOH-SAKJOKBN (IDY)    =   SPACE )
                        AND   (SPA-PTKOH-KOHNUM   (IDY)    =   "960"
                                                           OR  "961"
                                                           OR  "962"
                                                           OR  "963" )
                           IF     (SPA-PTKOH-TEKEDYMD (IDY)
                                       <   SPA-PTKOH-TEKSTYMD (IDX)) OR
                                  (SPA-PTKOH-TEKSTYMD (IDY)
                                       >   SPA-PTKOH-TEKEDYMD (IDX))
                               CONTINUE
                           ELSE
                               MOVE    "0112"      TO  SPA-ERRCD
      *                        公費の１番目にカーソル移動
                               MOVE    21          TO  SPA-GMN-CUR
                           END-IF
                      END-IF
                   END-PERFORM
              END-IF
           END-PERFORM
           .
       4909-960JYUFUKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ公費単独チェック処理
      *****************************************************************
       4901-KOHONLY-CHK-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "HKNCOMBI"          TO  PARA-FILEMEI
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
               UNTIL  (FLG-PARA        NOT =   ZERO )  OR
                      (SPA-ERRCD       NOT =   SPACE )
               MOVE    PARA-DATA-REC       TO  HKNCOMBI-REC
               IF     (COMB-DELKBN         =   "1"       )  OR
                      (SPA-SYSYMD          >   COMB-TEKEDYMD)
                   CONTINUE
               ELSE
      *            地方公費単独判定
                   IF     (SPA-NAI-KOHONLY     =   ZERO ) AND
                          (SPA-NAI-KOHONLY-CHKFLG  =   "1" )
                       PERFORM 49011-KOHONLY-CHKSYORI-SEC
                   END-IF
      *            退職国保と老人公費の組合せ
                   IF      (COMB-HKNNUM        =   "067" ) AND
                          ((COMB-KOH1HKNNUM    =   "027" ) OR
                           (COMB-KOH2HKNNUM    =   "027" ) OR
                           (COMB-KOH3HKNNUM    =   "027" ) OR
                           (COMB-KOH4HKNNUM    =   "027" ))
                       IF      SPA-NAI-TAICHK      =   ZERO
                           MOVE    "K011"          TO  SPA-ERRCD
                           MOVE    1               TO  SPA-NAI-TAICHK
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       4901-KOHONLY-CHK-EXT.
           EXIT.
      *****************************************************************
      *    単独公費の判定処理
      *****************************************************************
       49011-KOHONLY-CHKSYORI-SEC             SECTION.
      *
      *    長崎県の被爆者医療（１８６）は単独可能
           IF      (COMB-HKNNUM         =   SPACE )  AND
                   (COMB-KOH1HKNNUM     =   "186" )  AND
                   (COMB-KOH2HKNNUM     =   SPACE )  AND
                   (COMB-KOH3HKNNUM     =   SPACE )  AND
                   (COMB-KOH4HKNNUM     =   SPACE )  AND
                   (SPA-PREFNUM         =   "42"  )
               MOVE    ZERO             TO  FLG-KOHCHK
           ELSE
               MOVE    ZERO             TO  FLG-KOHCHK
               IF      (COMB-HKNNUM         =   SPACE )  AND
                       (COMB-KOH1HKNNUM     >=  "100"  AND
                                            <=  "899"  )  AND
                       ((COMB-KOH2HKNNUM     =   SPACE  ) OR
                        (COMB-KOH2HKNNUM     >=  "100"  AND
                                             <=  "899"  )) AND
                       ((COMB-KOH3HKNNUM     =   SPACE  ) OR
                        (COMB-KOH3HKNNUM     >=  "100"  AND
                                             <=  "899"  )) AND
                       ((COMB-KOH4HKNNUM     =   SPACE  ) OR
                        (COMB-KOH4HKNNUM     >=  "100"  AND
                                             <=  "899"  ))
                   PERFORM 49011-KOHONLY-HKNCHK-SEC
               END-IF
           END-IF
           IF      FLG-KOHCHK      =   1
               MOVE    "K010"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-NAI-KOHONLY
           END-IF
           .
       49011-KOHONLY-CHKSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独公費の判定処理
      *****************************************************************
       49011-KOHONLY-HKNCHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-KOHCHK
      *
           IF      COMB-KOH1HKNNUM NOT =   SPACE
               MOVE    COMB-KOH1HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH1PAYKBN     TO  WRK-PAYKBN
               PERFORM 49011-KOHHKNNUM-CHK-SEC
           END-IF
      *
           IF      COMB-KOH2HKNNUM NOT =   SPACE
               MOVE    COMB-KOH2HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH2PAYKBN     TO  WRK-PAYKBN
               PERFORM 49011-KOHHKNNUM-CHK-SEC
           END-IF
      *
           IF      COMB-KOH3HKNNUM NOT =   SPACE
               MOVE    COMB-KOH3HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH3PAYKBN     TO  WRK-PAYKBN
               PERFORM 49011-KOHHKNNUM-CHK-SEC
           END-IF
      *
           IF      COMB-KOH4HKNNUM NOT =   SPACE
               MOVE    COMB-KOH4HKNNUM     TO  WRK-HKNNUM
               MOVE    COMB-KOH4PAYKBN     TO  WRK-PAYKBN
               PERFORM 49011-KOHHKNNUM-CHK-SEC
           END-IF
           .
       49011-KOHONLY-HKNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    単独公費の判定処理
      *****************************************************************
       49011-KOHHKNNUM-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-KOHMAX
               IF     (WRK-HKNNUM      =   SPA-GMN-KOHNUML (IDX))
      *            単独公費以外
                   IF      SPA-NAI-KOHSBTKBN (IDX) =   "6"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-KOHCHK
                   END-IF
               END-IF
           END-PERFORM
           .
       49011-KOHHKNNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カルテ発行の保険組合決定処理
      *****************************************************************
       4901-KARUTE-CHEK-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
           MOVE    ZERO                TO  WRK-IDXH2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNCOMBI-MAX
               MOVE    ZERO                TO  WRK-CNT-HKN2
      *        非表示対象外
               IF      SPA-NAI-THYOJIKBN (IDX) =   "1"
                   CONTINUE
               ELSE
                   IF      WRK-IDXH2           =   ZERO
                       MOVE    IDX                 TO  WRK-IDXH2
                   END-IF
      *
               IF      SPA-GMN-THKNNUM (IDX)       NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-TKOH1HKNNUM(IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-TKOH2HKNNUM(IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-TKOH3HKNNUM(IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-TKOH4HKNNUM(IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      WRK-CNT-HKN         <=  WRK-CNT-HKN2
                   MOVE    IDX                 TO  WRK-IDXH
                   MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
               END-IF
      *        労災・自賠責があれば対象とする
               IF      SPA-GMN-THKNNUM (IDX)       =   "971"
                                                   OR  "973"
                   MOVE    IDX                 TO  WRK-IDXH
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDX
               END-IF
      *
               END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    WRK-IDXH2       TO  WRK-IDXH
           END-IF
           IF      WRK-IDXH            =   ZERO
               MOVE    ZERO            TO  SPA-GMN-SELNUM
           ELSE
               MOVE    SPA-GMN-THKNCOMBINUM (WRK-IDXH)
                                       TO  SPA-GMN-SELNUM
           END-IF
      *
           .
       4901-KARUTE-CHEK-EXT.
           EXIT.
      *
      *!API
      *****************************************************************
      *   API保険情報処理
      *****************************************************************
       4199-API-SYORI-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-ERRCHK
           MOVE    ZERO                TO  SPA-KOH-ERRIDX
           MOVE    1                   TO  FLG-APICHK
      *
      *    新規保険チェック
           IF     (SPA-PTHKN-MAX       >   ZERO  )
             AND  (SPA-PTHKN-SINKIKBN (SPA-PTHKN-MAX)  =   "1" )
               INITIALIZE                  SPA-GMN-HKN-AREA
      *        被保険者名の自動変更はしない
               MOVE    SPA-PTHKN-MAX       TO  IDX
               MOVE    1                   TO  SPA-NAI-HIHKNFLG
               MOVE    IDX             TO  WRK-NAI-HKNSTR
               MOVE    IDX             TO  SPA-NAI-HKNSTR
               PERFORM 3101-HOKEN-HEN-SEC
      *        保険情報チェックサブ
               CALL    "ORCSP03A"          USING
                                           SPA-AREA
                                           SPA-P02SCRAREA
                                           SPA-P02KYOUTU
               PERFORM 410312-HKN-SPATBL-SET-SEC
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      4199-API-SYORI-EXT
           END-IF
      *
           MOVE    SPA-NAI-KOHSTR      TO  WRK-NAI-KOHSTR
           MOVE    ZERO                TO  IDY
           INITIALIZE                  SPA-GMN-KOH-AREA
           MOVE    SPA-GMN-KOH-AREA    TO  WRK-GMN-KOH-AREA
           IF      WRK-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  WRK-NAI-KOHSTR
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      IDY                 =   ZERO
                   INITIALIZE                  SPA-GMN-KOH-AREA
                   MOVE    ZERO                TO  SPA-NAI-KOHEND
               END-IF
               IF      SPA-PTKOH-SINKIKBN (IDX)    =   "1"
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
               END-IF
               IF      IDY                 >=  3
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
                   IF      SPA-ERRCD           =   SPACE
      *                公費画面の内容をＴＢＬへ編集する
                       PERFORM 4509-API-KOHSPA-TBL-SEC
                       MOVE    ZERO                TO  IDY
                   END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-PIDCD           =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               IF      IDY                 >   ZERO
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
                   IF      SPA-ERRCD           =   SPACE
      *                公費画面の内容をＴＢＬへ編集する
                       PERFORM 4509-API-KOHSPA-TBL-SEC
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      4199-API-SYORI-EXT
           END-IF
      *
      *    保険・公費一括チェック処理
           PERFORM 4501-ALLHKN-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      4199-API-SYORI-EXT
           END-IF
      *    保険・公費のテーブルセットとチェック
           PERFORM 4103-HKNKOH-TBL-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               GO      TO      4199-API-SYORI-EXT
           END-IF
      *
      *    保険ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4501-PTHKNINF-LNK-SEC
      *    公費ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4502-PTKOHINF-LNK-SEC
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
      *
      *    保険組合せ作成
           PERFORM 4503-HKNCOMBI-SYORI-SEC
      *
           .
       4199-API-SYORI-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *   APIV3　保険情報処理
      *****************************************************************
       4198-APIV3-SYORI-SEC             SECTION.
      *
      *    LNK-SCRSPA にＡＰＩ情報を登録
           MOVE    LNK-SCRSPA           TO  APIS-P02SCRAREA
      *
      *    年齢計算
           PERFORM 410121-NENREI-SET-SEC
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-HKNKOH-FLG
           MOVE    ZERO                TO  FLG-ERRCHK
      *
           MOVE    ZERO                TO  SPA-KOH-ERRIDX
           MOVE    2                   TO  FLG-APICHK
      *
           MOVE    MCP-WIDGET          TO  SPA-MCP-WIDGET
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPA-NAI-HKNSTR      TO  WRK-NAI-HKNSTR
           IF      WRK-NAI-HKNSTR      =   ZERO
               MOVE    1               TO  IDX-STR
           ELSE
               MOVE    WRK-NAI-HKNSTR  TO  IDX-STR
           END-IF
      *    保険チェック
           PERFORM VARYING     IDX     FROM    IDX-STR     BY  1
                   UNTIL      (IDX         >   SPA-PTHKN-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-PTHKN-SAKJOKBN (IDX)    =   SPACE
                   IF      IDX                 =   WRK-NAI-HKNSTR
                       CONTINUE
                   ELSE
                       MOVE    IDX                 TO  SPA-NAI-HKNSTR
                       INITIALIZE                  SPA-GMN-HKN-AREA
                       PERFORM 3101-HOKEN-HEN-SEC
      *                被保険者名の自動変更はしない
                       MOVE    1                   TO  SPA-NAI-HIHKNFLG
      *                変更内容編集
                       IF      SPA-PTHKN-MODE (IDX)    =   "2"
                           PERFORM 41981-APIHKNHEN-SEC
                       END-IF
                   END-IF
      *
      *            保険情報チェックサブ
                   CALL    "ORCSP03A"          USING
                                               SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
                   PERFORM 410312-HKN-SPATBL-SET-SEC
               END-IF
           END-PERFORM
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               MOVE    98              TO  SPA-GMN-CUR
               GO      TO      4198-APIV3-SYORI-EXT
           END-IF
      *
      *    公費チェック
           MOVE    SPA-NAI-KOHSTR      TO  WRK-NAI-KOHSTR
           MOVE    SPA-NAI-KOHEND      TO  WRK-NAI-KOHEND
           IF      WRK-NAI-KOHSTR      =   ZERO
               MOVE    1                   TO  IDX-STR
           ELSE
               MOVE    WRK-NAI-KOHSTR      TO  IDX-STR
           END-IF
           MOVE    ZERO                TO  IDY
      **   INITIALIZE                  SPA-GMN-KOH-AREA
           PERFORM VARYING     IDX     FROM    IDX-STR   BY  1
                   UNTIL      (IDX         >   SPA-PTKOH-MAX )  OR
                              (SPA-PIDCD   NOT =   SPACE )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (WRK-NAI-KOHSTR      >   ZERO)
                  AND (WRK-NAI-KOHSTR      >=  IDX )
                  AND (WRK-NAI-KOHEND      <=  IDX )
                   ADD     1               TO  IDY
               ELSE
      *
               IF      IDY                 =   ZERO
                   INITIALIZE                  SPA-GMN-KOH-AREA
                   MOVE    ZERO                TO  SPA-NAI-KOHEND
               END-IF
               IF      SPA-PTKOH-SAKJOKBN (IDX)    =   SPACE
                   IF      IDY             =   ZERO
                       MOVE    IDX             TO  SPA-NAI-KOHSTR
                   END-IF
                   MOVE    IDX                 TO  SPA-NAI-KOHEND
                   ADD     1                   TO  IDY
                   MOVE    1                   TO  FLG-CHK
                   PERFORM 3102-KOUHI-HEN-SEC
      *            変更内容編集
                   IF      SPA-PTKOH-MODE (IDX)    =   "2"
                       PERFORM 41982-APIKOHHEN-SEC
                   END-IF
               END-IF
      *
               END-IF
               IF      IDY                 >=  3
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                               SPA-P02SCRAREA
                                               SPA-P02KYOUTU
      *            IF      SPA-ERRCD           =   SPACE
      *                公費画面の内容をＴＢＬへ編集する
                       PERFORM 4509-API-KOHSPA-TBL-SEC
                       MOVE    ZERO                TO  IDY
      *            END-IF
               END-IF
           END-PERFORM
      *
           IF     (SPA-PIDCD           =   SPACE )
              AND (SPA-ERRCD           =   SPACE )
               IF      IDY                 >   ZERO
      *            公費情報チェックサブ
                   CALL    "ORCSP03B"          USING   SPA-AREA
                                                   SPA-P02SCRAREA
                                                   SPA-P02KYOUTU
                   IF      SPA-ERRCD           =   SPACE
      *                公費画面の内容をＴＢＬへ編集する
                       PERFORM 4509-API-KOHSPA-TBL-SEC
                       MOVE    ZERO                TO  IDY
                   END-IF
               END-IF
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               MOVE    99              TO  SPA-GMN-CUR
               GO      TO      4198-APIV3-SYORI-EXT
           END-IF
           MOVE    ZERO              TO  SPA-GMN-CUR
      *
      *    保険・公費一括チェック処理
      *    PERFORM 4501-ALLHKN-CHK-SEC
      *    IF     (SPA-ERRCD       NOT =   SPACE )  OR
      *           (SPA-PIDCD       NOT =   SPACE )
      *        GO      TO      4198-APIV3-SYORI-EXT
      *    END-IF
      *
      *    保険・公費のテーブルセットと重複チェック
           PERFORM 4103-HKNKOH-TBL-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-PIDCD       NOT =   SPACE )
               MOVE    97              TO  SPA-GMN-CUR
               GO      TO      4198-APIV3-SYORI-EXT
           END-IF
      *
      *    保険ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4501-PTHKNINF-LNK-SEC
      *    公費ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4502-PTKOHINF-LNK-SEC
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
      *
      *    保険組合せ作成
           PERFORM 4503-HKNCOMBI-SYORI-SEC
           MOVE    90              TO  SPA-GMN-CUR
      *
           .
       4198-APIV3-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   APIV3　保険入力情報処理
      *****************************************************************
       41981-APIHKNHEN-SEC             SECTION.
      *
           MOVE    APIS-PTHKN-HKNNUM (IDX)  TO  SPA-GMN-HKNNUM
      *
           MOVE    APIS-PTHKN-HKNJANUM(IDX) TO  SPA-GMN-HKNJANUM
           MOVE    APIS-PTHKN-HIHKNJANAME (IDX)
                                           TO  SPA-GMN-HIHKNJANAME
           MOVE    APIS-PTHKN-HONKZKKBN (IDX)
                                           TO  SPA-GMN-HONKZKKBN
           MOVE    APIS-PTHKN-HOJOKBN (IDX) TO  SPA-GMN-HOJOKBN
           MOVE    APIS-PTHKN-CONTKBN (IDX) TO  SPA-GMN-CONTKBN
           MOVE    APIS-PTHKN-KIGO    (IDX) TO  SPA-GMN-KIGO
           MOVE    APIS-PTHKN-NUM     (IDX) TO  SPA-GMN-NUM
           MOVE    APIS-PTHKN-EDABAN (IDX)  TO  SPA-GMN-EDABAN
      *
           MOVE    APIS-PTHKN-SKKGETYMD (IDX)
                                           TO  SPA-GMN-SKKGETYMD
           MOVE    APIS-PTHKN-TEKSTYMD (IDX)
                                           TO  SPA-GMN-HTEKSTYMD
           MOVE    APIS-PTHKN-TEKEDYMD (IDX)
                                           TO  SPA-GMN-HTEKEDYMD
           IF      APIS-PTHKN-KAKUNINYMD (IDX) NOT =   SPACE
               MOVE    APIS-PTHKN-KAKUNINYMD (IDX)
                                           TO  SPA-GMN-HKAKUNINYMD
               MOVE    APIS-PTHKN-KAKUNINYMD (IDX)
                                           TO  SPA-NAI-HKAKUNINYMD
           END-IF
      *H30.11
           MOVE    APIS-PTHKN-SKKGETYMD (IDX)
                                           TO  SPA-NAI-SKKGETYMD
           MOVE    APIS-PTHKN-TEKSTYMD (IDX)
                                           TO  SPA-NAI-HTEKSTYMD
           MOVE    APIS-PTHKN-TEKEDYMD (IDX)
                                           TO  SPA-NAI-HTEKEDYMD
      *
      *H30.11
      *    開始日変更があれば、補助区分変更
           IF      APIS-PTHKN-TEKSTYMD (IDX)
                                   NOT =   SPA-PTHKN-TEKSTYMD(IDX)
               MOVE    SPA-NAI-HTEKSTYMD   TO  WRK-TEKSTYMD
      *        本人家族区分等
               PERFORM 31011-HKN-HEN-SEC
           END-IF
      *
           .
       41981-APIHKNHEN-EXT.
           EXIT.
      *****************************************************************
      *   APIV3　公費入力情報処理
      *****************************************************************
       41982-APIKOHHEN-SEC             SECTION.
      *
           MOVE    APIS-PTKOH-KOHNUM (IDX)  TO  SPA-GMN-KOHNUM  (IDY)
           MOVE    APIS-PTKOH-FTNJANUM(IDX) TO  SPA-GMN-FTNJANUM (IDY)
           MOVE    APIS-PTKOH-JKYSNUM (IDX) TO  SPA-GMN-JKYSNUM (IDY)
      *
           MOVE    APIS-PTKOH-TEKSTYMD (IDX)
                                           TO  SPA-GMN-KTEKSTYMD (IDY)
           MOVE    APIS-PTKOH-TEKEDYMD (IDX)
                                           TO  SPA-GMN-KTEKEDYMD (IDY)
           IF      APIS-PTKOH-KAKUNINYMD (IDX) NOT =   SPACE
               MOVE    APIS-PTKOH-KAKUNINYMD (IDX)
                                           TO  SPA-GMN-KKAKUNINYMD (IDY)
           END-IF
      *
           .
       41982-APIKOHHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   APIV3　労災保険情報処理
      *****************************************************************
       41910-APIV3-RSISYORI-SEC             SECTION.
      *
      *    年齢計算
           PERFORM 410121-NENREI-SET-SEC
      *
      *    保険ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4501-PTHKNINF-LNK-SEC
      *    公費ＴＢＬからＰＡＲＡファイルへ編集
           PERFORM 4502-PTKOHINF-LNK-SEC
      *    公費ＴＢＬから公費負担へ編集
           PERFORM 4504-PTKOHFTN-HEN-SEC
      *
      *    保険組合せ作成
           PERFORM 4503-HKNCOMBI-SYORI-SEC
           MOVE    90              TO  SPA-GMN-CUR
      *
           .
       41910-APIV3-RSISYORI-EXT.
           EXIT.
      *!API
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
               ADD     1               TO  SPA-GMN-CUR
           END-IF
      *
           EVALUATE   SPA-GMN-CUR
              WHEN    01
                  MOVE    "P02_KANANAME"         TO  MCP-WIDGET
              WHEN    02
                  MOVE    "P02_NAME"             TO  MCP-WIDGET
              WHEN    03
                  MOVE    "P02_SEX"              TO  MCP-WIDGET
              WHEN    04
                  MOVE    "P02_BIRTHDAY"         TO  MCP-WIDGET
              WHEN    05
                  MOVE    "P02_NICKNAME"         TO  MCP-WIDGET
              WHEN    06
                  MOVE    "P02_HAKKO"            TO  MCP-WIDGET
              WHEN    70
                  MOVE    "P02_USERPG"           TO  MCP-WIDGET
              WHEN    07
                  MOVE    "TSTPTNUMKBN"          TO  MCP-WIDGET
      *       保険情報
              WHEN    08
                  MOVE    "P02_HKNJANUM"         TO  MCP-WIDGET
              WHEN    09
                  MOVE    "P02_HKNNUM"         TO  MCP-WIDGET
              WHEN    10
                  MOVE    "P02_HONKZKBN"         TO  MCP-WIDGET
              WHEN    11
                  MOVE    "P02_HOJOKBN"         TO  MCP-WIDGET
              WHEN    12
                  MOVE    "P02_CONTKBN"         TO  MCP-WIDGET
              WHEN    13
                  MOVE    "P02_KIGO"             TO  MCP-WIDGET
              WHEN    14
                  MOVE    "P02_NUM"              TO  MCP-WIDGET
      *R02.4
              WHEN    72
                  MOVE    "P02_EDABAN"           TO  MCP-WIDGET
              WHEN    15
                  MOVE    "P02_SKKGETYMD"        TO  MCP-WIDGET
              WHEN    16
                  MOVE    "P02_HTEKSTYMD"        TO  MCP-WIDGET
              WHEN    17
                  MOVE    "P02_HTEKEDYMD"        TO  MCP-WIDGET
              WHEN    18
                  MOVE    "P02_HIHKNJANAME"      TO  MCP-WIDGET
              WHEN    19
                  MOVE    "P02_HKAKUNINYMD"      TO  MCP-WIDGET
      *    公費１
              WHEN    20
                  MOVE    "P02_FTNJANUM1"        TO  MCP-WIDGET
              WHEN    21
                  MOVE    "P02_KOHNUM1"          TO  MCP-WIDGET
              WHEN    22
                  MOVE    "P02_JKYSNUM1"         TO  MCP-WIDGET
              WHEN    23
                  MOVE    "P02_KTEKSTYMD1"       TO  MCP-WIDGET
              WHEN    24
                  MOVE    "P02_KTEKEDYMD1"       TO  MCP-WIDGET
              WHEN    25
                  MOVE    "P02_KKAKUNINYMD1"     TO  MCP-WIDGET
      *    公費１
              WHEN    26
                  MOVE    "P02_FTNJANUM2"        TO  MCP-WIDGET
              WHEN    27
                  MOVE    "P02_KOHNUM2"          TO  MCP-WIDGET
              WHEN    28
                  MOVE    "P02_JKYSNUM2"         TO  MCP-WIDGET
              WHEN    29
                  MOVE    "P02_KTEKSTYMD2"       TO  MCP-WIDGET
              WHEN    30
                  MOVE    "P02_KTEKEDYMD2"       TO  MCP-WIDGET
              WHEN    31
                  MOVE    "P02_KKAKUNINYMD2"     TO  MCP-WIDGET
      *    公費３
              WHEN    32
                  MOVE    "P02_FTNJANUM3"        TO  MCP-WIDGET
              WHEN    33
                  MOVE    "P02_KOHNUM3"          TO  MCP-WIDGET
              WHEN    34
                  MOVE    "P02_JKYSNUM3"         TO  MCP-WIDGET
              WHEN    35
                  MOVE    "P02_KTEKSTYMD3"       TO  MCP-WIDGET
              WHEN    36
                  MOVE    "P02_KTEKEDYMD3"       TO  MCP-WIDGET
              WHEN    37
                  MOVE    "P02_KKAKUNINYMD3"     TO  MCP-WIDGET
      *    
              WHEN    38
                  MOVE    "P02_SELNUM"           TO  MCP-WIDGET
              WHEN    39
                  MOVE    "P02_SRYKA"            TO  MCP-WIDGET
      *
              WHEN    40
                  MOVE    "P02_POST"             TO  MCP-WIDGET
              WHEN    41
                  MOVE    "P02_ADRS"            TO  MCP-WIDGET
              WHEN    42
                  MOVE    "P02_BANTI"            TO  MCP-WIDGET
              WHEN    43
                  MOVE    "P02_TEL1"             TO  MCP-WIDGET
              WHEN    44
                  MOVE    "P02_TEL2"             TO  MCP-WIDGET
              WHEN    45
                  MOVE    "P02_DISCOUNTKBN"     TO  MCP-WIDGET
              WHEN    46
                  MOVE    "P02_DISCOUNT"        TO  MCP-WIDGET
              WHEN    66
                  MOVE    "P02_NYUKIN"          TO  MCP-WIDGET
              WHEN    67
                  MOVE    "P02_SKYPRTFLG"        TO  MCP-WIDGET
              WHEN    68
                  MOVE    "P02_BIRTHWEIGHT"      TO  MCP-WIDGET
              WHEN    47
                  MOVE    "P02_SETAINUSI"        TO  MCP-WIDGET
              WHEN    48
                  MOVE    "P02_ZOKUGARA"        TO  MCP-WIDGET
              WHEN    49
                  MOVE    "P02_CONDITION1"      TO  MCP-WIDGET
              WHEN    50
                  MOVE    "P02_CONDITION2"      TO  MCP-WIDGET
              WHEN    51
                  MOVE    "P02_CONDITION3"      TO  MCP-WIDGET
              WHEN    52
                  MOVE    "P02_TABOO1"          TO  MCP-WIDGET
              WHEN    53
                  MOVE    "P02_TABOO2"          TO  MCP-WIDGET
              WHEN    54
                  MOVE    "P02_ALLERGY1"        TO  MCP-WIDGET
              WHEN    55
                  MOVE    "P02_ALLERGY2"        TO  MCP-WIDGET
              WHEN    56
                  MOVE    "P02_KANSENSYO1"      TO  MCP-WIDGET
              WHEN    57
                  MOVE    "P02_KANSENSYO2"      TO  MCP-WIDGET
              WHEN    58
                  MOVE    "P02_COMMENT1"        TO  MCP-WIDGET
              WHEN    59
                  MOVE    "P02_COMMENT2"        TO  MCP-WIDGET
      *       登録
              WHEN    60
                  MOVE    "B12"                 TO  MCP-WIDGET
      *       労災・自賠責
              WHEN    61
                  MOVE    "B09S"                TO  MCP-WIDGET
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
      *
           EVALUATE    SPA-MCP-WIDGET
      *    保険組合せ選択
               WHEN    "HKNLIST"
                   MOVE    38          TO  SPA-GMN-CUR
      *    かな氏名
               WHEN    "P02_KANANAME"
                   MOVE    1           TO  SPA-GMN-CUR
      *    漢字氏名
               WHEN    "P02_NAME"
                   MOVE    2           TO  SPA-GMN-CUR
      *    性別
               WHEN    "P02_SEX"
                   MOVE    3           TO  SPA-GMN-CUR
      *    生年月日
               WHEN    "P02_BIRTHDAY"
      *            カルテ発行へ
      ***********  MOVE    5           TO  SPA-GMN-CUR
      *            保険情報へ
                   MOVE    7           TO  SPA-GMN-CUR
      *    通称
               WHEN    "P02_NICKNAME"
      *************MOVE    5           TO  SPA-GMN-CUR
      *            保険情報へ
                   MOVE    7           TO  SPA-GMN-CUR
      *    カルテ発行
               WHEN    "P02_HAKKO"
      *            保険情報へ
                   MOVE    7           TO  SPA-GMN-CUR
      *    Ｕ・Ｐ
               WHEN    "P02_USERPG"
      *            保険情報へ
                   MOVE    7           TO  SPA-GMN-CUR
      *    テスト患者
               WHEN    "TSTPTNUMKBN"
                   MOVE    7           TO  SPA-GMN-CUR
      *    保険情報
               WHEN    "P02_HKNJANUM"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "P02_HKNNUM"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "P02_HONKZKBN"
                   MOVE    10          TO  SPA-GMN-CUR
      *            補助がない時、継続へ
                   IF      SPA-GMN-HOJO-MAX    =   ZERO
                       MOVE    11              TO  SPA-GMN-CUR
      *                継続がない時、記号へ
                       IF      SPA-GMN-CONT-MAX    =   ZERO
                           MOVE    12              TO  SPA-GMN-CUR
                       END-IF
                   END-IF
               WHEN    "P02_HOJOKBN"
                   MOVE    11          TO  SPA-GMN-CUR
      *            継続がない時、記号へ
                   IF      SPA-GMN-CONT-MAX    =   ZERO
                       MOVE    12              TO  SPA-GMN-CUR
                   END-IF
      *!!!         後期高齢者は、番号へ
                   IF      SPA-GMN-HKNNUM      =   "039"
                       MOVE    13              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_CONTKBN"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "P02_KIGO"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "P02_NUM"
                   MOVE    14          TO  SPA-GMN-CUR
      *R02.4
      *            枝番が必要でない保険、番号なし
                   IF     (SPA-GMN-HKNNUM(1:1)     =   "9" )
                      OR  (SPA-GMN-HKNNUM          =   "039")
                      OR  (SPA-GMN-HKNNUM          =   "040")
                      OR  (SPA-GMN-NUM             =   SPACE)
                       CONTINUE
                   ELSE
                       MOVE    71          TO  SPA-GMN-CUR
                   END-IF
      *R02.4
               WHEN    "P02_EDABAN"
                   MOVE    14          TO  SPA-GMN-CUR
      *
               WHEN    "P02_SKKGETYMD"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "P02_HTEKSTYMD"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "P02_HTEKEDYMD"
                   MOVE    17          TO  SPA-GMN-CUR
               WHEN    "P02_HIHKNJANAME"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "P02_HKAKUNINYMD"
                   MOVE    19          TO  SPA-GMN-CUR
      *    公費１
               WHEN    "P02_FTNJANUM1"
                   MOVE    20          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(1) =   SPACE) AND
                          (SPA-GMN-KOHNUM (1)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM1"
                   MOVE    21          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM1"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD1"
                   MOVE    23          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD1"
                   MOVE    24          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD1"
                   MOVE    25          TO  SPA-GMN-CUR
      *    公費２
               WHEN    "P02_FTNJANUM2"
                   MOVE    26          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(2) =   SPACE) AND
                          (SPA-GMN-KOHNUM (2)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM2"
                   MOVE    27          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM2"
                   MOVE    28          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD2"
                   MOVE    29          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD2"
                   MOVE    30          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD2"
                   MOVE    31          TO  SPA-GMN-CUR
      *    公費３
               WHEN    "P02_FTNJANUM3"
                   MOVE    32          TO  SPA-GMN-CUR
                   IF     (SPA-GMN-FTNJANUM(3) =   SPACE) AND
                          (SPA-GMN-KOHNUM (3)  =   SPACE)
                       MOVE    39          TO  SPA-GMN-CUR
                   END-IF
               WHEN    "P02_KOHNUM3"
                   MOVE    33          TO  SPA-GMN-CUR
               WHEN    "P02_JKYSNUM3"
                   MOVE    34          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKSTYMD3"
                   MOVE    35          TO  SPA-GMN-CUR
               WHEN    "P02_KTEKEDYMD3"
                   MOVE    36          TO  SPA-GMN-CUR
               WHEN    "P02_KKAKUNINYMD3"
                   MOVE    37          TO  SPA-GMN-CUR
      *    選択番号
               WHEN    "P02_SELNUM"
                   MOVE    38          TO  SPA-GMN-CUR
      *    診療科
               WHEN    "P02_SRYKA"
                   MOVE    39          TO  SPA-GMN-CUR
      *    郵便番号
               WHEN    "P02_POST"
                   MOVE    40          TO  SPA-GMN-CUR
      *            住所があれば次ぎへ
                   IF      SPA-GMN-EDITADRS-NAME   NOT =   SPACE
                       MOVE    41          TO  SPA-GMN-CUR
                   END-IF
      *    住所
               WHEN    "P02_ADRS"
                   MOVE    41          TO  SPA-GMN-CUR
      *    番地方書
               WHEN    "P02_BANTI"
                   MOVE    42          TO  SPA-GMN-CUR
      *    電話昼
               WHEN    "P02_TEL1"
                   MOVE    43          TO  SPA-GMN-CUR
      *    電話夜
               WHEN    "P02_TEL2"
                   MOVE    44          TO  SPA-GMN-CUR
      *    減免事由
               WHEN    "P02_DISCOUNTKBN"
                   MOVE    45          TO  SPA-GMN-CUR
      *    割引率
               WHEN    "P02_DISCOUNT"
      ************ MOVE    46          TO  SPA-GMN-CUR
                   MOVE    65          TO  SPA-GMN-CUR
      *    入金方法
               WHEN    "P02_NYUKIN"
                   MOVE    46          TO  SPA-GMN-CUR
      *    世帯主
               WHEN    "P02_SETAINUSI"
                   MOVE    47          TO  SPA-GMN-CUR
      *    続柄
               WHEN    "P02_ZOKUGARA"
                   MOVE    48          TO  SPA-GMN-CUR
      *    状態１
               WHEN    "P02_CONDITION1"
                   MOVE    49          TO  SPA-GMN-CUR
      *    状態２
               WHEN    "P02_CONDITION2"
                   MOVE    50          TO  SPA-GMN-CUR
      *    状態３
               WHEN    "P02_CONDITION3"
      *******      MOVE    51                  TO  SPA-GMN-CUR
                   MOVE    66                  TO  SPA-GMN-CUR
      *H26.1
      *            １歳未満は出生時体重へ
                   IF     (SPA-NAI-NENREI-YY   =   ZERO )
      ****           AND  (SPA-P02-SYORI       =   1    )
                       MOVE    67                  TO  SPA-GMN-CUR
                   END-IF
      *H26.1
      *    出生時体重
               WHEN    "P02_BIRTHWEIGHT"
                   MOVE    66                  TO  SPA-GMN-CUR
      *    請求書発行区分
               WHEN    "P02_SKYPRTFLG"
                   MOVE    51                  TO  SPA-GMN-CUR
      *
      *    禁忌
               WHEN    "P02_TABOO1"
                   MOVE    52                  TO  SPA-GMN-CUR
               WHEN    "P02_TABOO2"
                   MOVE    53                  TO  SPA-GMN-CUR
      *    アレルギー
               WHEN    "P02_ALLERGY1"
                   MOVE    54                  TO  SPA-GMN-CUR
               WHEN    "P02_ALLERGY2"
                   MOVE    55                  TO  SPA-GMN-CUR
      *    感染症
               WHEN    "P02_KANSENSYO1"
                   MOVE    56                  TO  SPA-GMN-CUR
               WHEN    "P02_KANSENSYO2"
                   MOVE    57                  TO  SPA-GMN-CUR
      *    コメント
               WHEN    "P02_COMMENT1"
                   MOVE    58                  TO  SPA-GMN-CUR
      *    コメント２
               WHEN    "P02_COMMENT2"
                   MOVE    59                  TO  SPA-GMN-CUR
           END-EVALUATE
           .
      *
       50011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦FORMAT日付編集
      *****************************************************************
       410112-SE-WAFORMAT-YMDEDIT-SEC  SECTION.
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
       410112-SE-WAFORMAT-YMDEDIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角を全角へ変換処理
      *****************************************************************
       890-HALF-ZEN-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角空白を全角空白へ変換処理
      *****************************************************************
       890-HALF-ZENS-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  WRK-MAE-INPUT
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ATO-INPUT
           END-IF
      *
           .
       890-HALF-ZENS-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       891-HALF-ZEN2-SEC         SECTION.
      *
           IF     SPA-ENCODING         =   2
      *        拡張漢字対応
               PERFORM 8910-HALF-ZENC2-SEC
           ELSE
      *        拡張漢字不可
               PERFORM 890-HALF-ZENS-SEC
           END-IF
      *
           .
       891-HALF-ZEN2-EXT.
           EXIT.
      *****************************************************************
      *    半角空白を全角空白へ変換処理（外字あり）
      *****************************************************************
       8910-HALF-ZENC2-SEC         SECTION.
      *
           INITIALIZE                  ORCSKANACHK2AREA
           MOVE    "1"                 TO  KANACHK2-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK2-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK2-MAX-CNT
           CALL    "ORCSKANACHK2"      USING
                                       ORCSKANACHK2AREA
           MOVE    KANACHK2-MAE-INPUT  TO  WRK-MAE-INPUT
           IF     (KANACHK2-RC     NOT =   ZERO  )  OR
                  (KANACHK2-SYUBETU    =   1     )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK2-MAE-INPUT  TO  WRK-ATO-INPUT
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK2-OUT-INPUT  TO  WRK-ATO-INPUT
           END-IF
      *
           .
       8910-HALF-ZENC2-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ　ＲＥＡＤ
      *****************************************************************
       900-SYSTEM-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSTEM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      **    患者マスター読み込み
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTINF-REC
               MOVE    ZERO            TO  FLG-PTINF
           ELSE
               INITIALIZE                  PTINF-REC
               MOVE    1               TO  FLG-PTINF
           END-IF
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    保険番号マスタ次検索処理
      ****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
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
           .
       921-HKNNUM-NEXT-EXT.
           EXIT.
      *
      *----(01.00.15) LINE ADD START ---------------------------------
      *****************************************************************
      *    人名テーブル検索
      *****************************************************************
       941-JINMEI-SELECT-SEC           SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  JINMEI-HOSPNUM
           MOVE    JINMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_jinmei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JINMEI-REC
           ELSE
               INITIALIZE                  JINMEI-REC
               MOVE    1               TO  FLG-JINMEI
           END-IF
           .
      *
       941-JINMEI-SELECT-EXT.
           EXIT.
      *****************************************************************
      *    人名テーブル読み込み
      *****************************************************************
       941-JINMEI-NEXT-SEC           SECTION.
      *
           MOVE    "tbl_jinmei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JINMEI-REC
           ELSE
               INITIALIZE                  JINMEI-REC
               MOVE    1               TO  FLG-JINMEI
           END-IF
           .
      *
       941-JINMEI-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    人名テーブルクローズ処理
      *****************************************************************
       941-JINMEI-CLOSE-SEC           SECTION.
      *
           MOVE    "tbl_jinmei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
      *
       941-JINMEI-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者番号マスタ次検索処理
      *****************************************************************
       900-HKNJAINF-READ-SEC              SECTION.
      *
           MOVE    "tbl_hknjainf"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNJAINF-REC
               MOVE    ZERO                TO  FLG-HKNJAINF
           ELSE
               INITIALIZE               HKNJAINF-REC
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
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
      *----(01.00.15) LINE ADD END   ---------------------------------
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW           SECTION.
      *
           MOVE    "PUTWINDOW"      TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
