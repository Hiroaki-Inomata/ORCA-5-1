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
       PROGRAM-ID.             ORCGP02W2.
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
      *  01.00.03    MCC-太田     01/04/20  長期付加組合せ作成
      *  01.00.04    MCC-太田     01/05/01  特別地方公費組合せ修正
      *  01.00.05    MCC-太田     01/05/16  再書き込み対応修正
      *  02.00.01    NACL-多々納  02/07/31  パラメタファイルをＤＢへ
      *  02.00.02    NACL-多々納  02/10/31  船員保険補助区分追加
      *  02.00.03    NACL-多々納  02/11/21  共済下船３月追加
      *  02.00.04    NACL-多々納  04/07/15  国保補助区分追加
      *  02.00.05    NACL-小豆沢  05/03/16  一般療養、特別療養（旧日雇保険）
      *                                     の場合は原爆１９の単独を作成可能
      *                                     に変更
      *  02.00.06    NACL-小豆沢  05/06/10  原爆一般＋長期の組合せを作成可能
      *                                     に変更
      *  02.00.07    NACL-門脇    05/09/08  補助公費を追加した組合せの作成
      *                                     （全国公費＋地方公費＋補助公費）
      *                                     省庁対応
      *  02.00.08    NACL-多々納  05/10/19  特別療養費 単独保険組合追加
      *  02.00.09    NACL-小豆沢  05/10/25  公費適用期間１日対応
      *  02.00.10    NACL-小豆沢  05/10/26  公費適用期間１日対応（生活保護）
      *  02.00.11    NACL-多々納  05/11/14  MONFUNC 対応他
      *  02.00.12    NACL-門脇    06/02/24  053児童保護、019原爆に関する
      *                                     組合せ追加
      *  02.00.13    NACL-門脇    06/03/23  021精神通院に関する編集修正
      *  02.00.14    NACL-門脇    06/03/27  020精神入院に関する編集修正
      *  02.00.15    NACL-門脇    06/04/03  自立支援医療組合せ編集修正
      *  02.00.16    NACL-門脇    06/05/17  任意継続・継続の保険と公費がある
      *                                     場合の保険組合せ作成修正
      *  02.00.17    NACL-門脇    06/05/18  岡山県西粟倉村退職者国保２割対応
      *  02.00.18    NACL-門脇    06/06/01  特定疾患等＋長期対応
      *  03.01.01    NACL-門脇    06/06/26  ＜北海道＞０９１＋４１老人の
      *                                     保険組合せ作成
      *  03.02.01    NACL-門脇    06/07/31  高齢者上限額経過措置対応
      *  03.02.02    NACL-多々納  06/08/25  平成１８年１０月改正対応
      *                                     （老人割合変更）
      *  03.03.01    NACL-門脇    06/10/23  長期併用の保険組み合わせ修正
      *  03.04.01    NACL-門脇    06/11/20  高額委任払い制度、高額４回目以降
      *                                     対応
      *  03.04.02    NACL-門脇    06/11/29  保険組み合わせ作成条件修正
      *  03.04.03    NACL-門脇    07/01/25  ＜岡山＞０９１＋重心の
      *                                     保険組合せ作成
      *  03.03.04    NACL-門脇    07/02/15  ＜宮崎＞０９１＋障害の
      *                                     保険組合せ作成
      *  03.04.05    NACL-門脇    07/02/21  高額現物給付対応
      *  03.05.00    NACL-多々納  07/05/XX  グループ診療対応
      *  03.05.01    NACL-門脇    07/10/16  ＜岡山＞０９１＋地方公費
      *                                     保険組合せ作成
      *  04.00.01    NACL-小豆沢  07/11/30  一般療養、特別療養（旧日雇保険）
      *                                     の場合は生活保護の単独を作成可能
      *                                     に変更
      *  04.00.02    NACL-小豆沢  **/**/**  精神通院と特定疾患（水俣）の
      *                                     保険組合せ作成（暫定対応）
      *  04.00.03    NACL-小豆沢  07/12/21  一般療養、特別療養（旧日雇保険）
      *                                     の場合は結核入院の単独を作成可能
      *                                     に変更
      *  04.00.04    NACL-門脇    08/02/14  自立支援＋生保の組合せを作成しない
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０．４改正対応
      *  04.02.01    NACL-門脇    08/06/16  大阪府地方公費組み合わせ作成
      *  04.02.02    NACL-門脇    08/11/11  適用開始終了同一日対応
      *  04.03.01    NACL-門脇    08/12/08  公費単独の保険組み合わせ修正
      *  04.03.02    NACL-門脇    09/01/21  長期併用の保険組み合わせ修正
      *  04.03.03    NACL-門脇    09/03/11  ７５歳以上の前期高齢者対応
      *
      *  04.04.01    NACL-門脇    09/04/13  平成２１年５月改正対応
      *  04.04.02    NACL-門脇    09/07/24  ０５１、０５２の保険組み合わせ修正
      *                                     （生保をもつ場合）
      *  04.04.03    NACL-門脇    10/09/09  災害該当追加対応
      *
      *  04.05.01    NACL-門脇    10/02/17  生保適用期間の社保単独の
      *                                     保険組み合わせ作成
      *  04.05.02    NACL-門脇    11/03/02  山梨県地方公費対応
      *  04.05.03    NACL-門脇    11/07/20  長崎県186被爆と地方公費の
      *                                     保険組合せ作成
      *  04.05.04    NACL-門脇    11/07/20  019原爆と地方公費の
      *                                     保険組合せ作成
      *  04.05.05    NACL-門脇    12/06/28  一般療養、特別療養（旧日雇保険）
      *                                     の場合の生保単独の保険組み合わせ
      *                                     作成変更
      *  04.05.06    NACL-門脇    12/12/10  ７５歳到達月の特例非該当対応
      *
      *  04.06.01    NACL-小豆沢  11/07/04  更正と特定疾患負担無（水俣）の
      *                                     保険組合せ作成
      *  04.06.02    NACL-門脇    12/08/22  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *  04.06.03    NACL-門脇    12/11/20  特記事項０１（公）に関わる窓口計算・
      *                                     レセプト一部負担金記載対応
      *  04.06.04    NACL-門脇    13/05/08  特定負有と特定負無（水俣）の
      *                                     保険組合せ作成
      *  04.06.05    NACL-門脇    13/09/13  生保に関わる保険組合せ作成修正
      *
      *  04.07.01    NACL-門脇    13/07/08  自賠責（第三者行為）対応
      *  04.07.02    NACL-多々納  13/10/29  第三者行為対応
      *  04.07.03    NACL-門脇    14/05/08  053児童保護と地方公費の
      *                                     保険組合せ作成
      *  04.07.04    NACL-門脇    14/05/16  地公等の下船後の短縮制度名修正
      *  04.07.05    NACL-門脇    14/08/04  更生と特定負無の保険組合せ作成
      *  04.07.06    NACL-門脇    14/08/08  特定負有と特定負有の保険組合せ作成
      *  04.07.07    NACL-門脇    14/10/07  011結核入院と地方公費の
      *                                     保険組合せ作成
      *  04.07.08    NACL-門脇    14/11/04  平成２７年１月改正対応
      *                                     （０５４対応）
      *  04.07.09    NACL-門脇    15/07/23  062Ｂ型肝炎と地方公費の
      *                                     保険組合せ作成
      *  04.08.01    NACL-門脇    15/12/22  「入所中」入力時の保険組合せ
      *                                     適用期間対応
      *  04.08.02    NACL-門脇    16/02/10  地方公費の保険組合せ作成修正
      *                                     （同一保険番号が重複する
      *                                       保険組合せは作成しない）
      *  04.08.03    NACL-門脇    16/08/02  災害該当分のレセプト作成対応
      *                                     （非減免対象分と減免対象分の診療が
      *                                     混在する場合）
      *  04.08.04    NACL-門脇    16/11/11  減額（円超）の対応
      *  04.08.05    NACL-門脇    17/02/13  東京都地方公費組み合わせ作成
      *  04.08.06    NACL-門脇    17/04/04  全国公費に関わる保険組み合わせ
      *                                     作成対応
      *  04.08.07    NACL-門脇    17/04/17  継続区分「３特別」選択時は、
      *                                     主保険名に（特）をつける
      *  04.08.08    NACL-門脇    17/11/01  主保険＋052小児特定＋053児童保護の
      *                                     保険組み合わせ作成対応
      *  04.08.09    NACL-門脇    18/03/27  平成３０年４月改正対応
      *  04.08.10    NACL-門脇    18/07/02  平成３０年８月改正
      *                                     （高額療養費制度の見直し）
      *
      *  05.00.01    NACL-門脇    18/11/07  肝がん・重度肝硬変治療研究促進
      *                                     事業医療対応
      *  05.00.02    NACL-門脇    18/11/21  広島県地方公費組み合わせ作成
      *  05.00.03    NACL-門脇    18/12/04  053児童保護に関する組み合わせ作成
      *  05.00.04    NACL-多々納  19/06/04  生保に関わる保険組み合わせ作成
      *                                     対応（主保険なしの場合）
      *  05.00.05    NACL-門脇    19/06/13  省庁対応修正
      *  05.00.06    NACL-門脇    20/03/05  新型コロナウイルス対応
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
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-LNKPARA         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-NC01PARA        PIC 9(01).
      *----(01.00.01) LINE ADD START ---------------------------------
           03  FLG-RJN             PIC 9(01).
      *----(01.00.01) LINE ADD END   ---------------------------------
           03  FLG-SYOCYO          PIC 9(01).
           03  FLG-HANTEI          PIC 9(01).
           03  FLG-KOH-CHK         PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-HIYATOI         PIC 9(01).
           03  FLG-NYUSYO          PIC 9(01).
           03  FLG-SAIGAI          PIC 9(01).
           03  FLG-TUUKA           PIC 9(01).
           03  FLG-DAIRSIFLG       PIC 9(01).
           03  FLG-CHECK           PIC 9(01).
           03  FLG-NG              PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-HKN             PIC 9(04).
           03  CNT-KOH             PIC 9(04).
           03  CNT-COMB            PIC 9(04).
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
           03  IDZ2                PIC 9(04).
           03  IDZ3                PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDX7                PIC 9(04).
           03  IDX8                PIC 9(04).
           03  IDX9                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(04).
           03  IDY4                PIC 9(04).
           03  IDY5                PIC 9(04).
           03  IDY6                PIC 9(04).
           03  IDXR                PIC 9(04).
      *
       01  WRK-KCOMB-AREA.
      *      組合せ退避用
           02  WRK-KCOMB-REC       OCCURS   200.
           COPY  "CPHKNCOMBI.INC"  REPLACING //COMB-//
                                         BY //WRK-KCOMB-//.
       01  WRK-RCOMB-AREA.
      *    　組合せ退避用
           02  WRK-RCOMB-REC       OCCURS   200.
           COPY  "CPHKNCOMBI.INC"  REPLACING //COMB-//
                                         BY //WRK-RCOMB-//.
      *
       01  WRK-LCOMB-AREA.
      *    　組合せ退避用（組合せ最終形 元SPA-KUMI-）
           02  WRK-LCOMB-REC       OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-LCOMB-//.
       01  WRK-LCOMB2-AREA.
           02  WRK-LCOMB2-REC      OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-LCOMB2-//.
       01  WRK-LCOMB3-AREA.
           02  WRK-LCOMB3-REC      OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-LCOMB3-//.
      *
       01  WRK-TCOMB-AREA.
           02  WRK-TCOMB-REC.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-TCOMB-//.
       01  WRK-TCOMB2-AREA.
           02  WRK-TCOMB2-REC      OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-TCOMB2-//.
       01  WRK-TCOMB3-AREA.
           02  WRK-TCOMB3-REC      OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-TCOMB3-//.
       01  WRK-TCOMB4-AREA.
           02  WRK-TCOMB4-REC      OCCURS   200.
           COPY  "CPHKNCOMBI.INC" REPLACING //COMB-//
                                         BY //WRK-TCOMB4-//.
      *    新組合せパラメタデータ用レイアウト
       01  NC01PARA-REC.
           03  PARA-NC01-REC           PIC X(500).
           03  PARA-NC01-FLG           PIC 9(01).
      *
      *    公費表中の老人のインデックス　退避
       01  WRK-IDXSRT-AREA.
           03  WRK-SRT-STYMD       PIC X(08).
           03  WRK-SRT-EDYMD       PIC X(08).
           03  WRK-SRT-RJN-IDX     PIC 9(04)    OCCURS 200.
           03  WRK-SRT-RJN-CNT     PIC 9(04).
           03  WRK-SRT-RJN-CNT2    PIC 9(04).
           03  WRK-SRT-HKN-IDX     PIC 9(04)    OCCURS 200.
           03  WRK-SRT-HKN-CNT     PIC 9(04).
           03  WRK-SRT-HKN-CNT2    PIC 9(04).
           03  WRK-SRT-IDX         PIC 9(04).
           03  WRK-SRT-SEH-IDX     PIC 9(04)    OCCURS 200.
           03  WRK-SRT-SEH-CNT     PIC 9(04).
           03  WRK-SRT-SEH-CNT2    PIC 9(04).
           03  WRK-SRT-HJO-IDX     PIC 9(04)    OCCURS 200.
           03  WRK-SRT-HJO-CNT     PIC 9(04).
           03  WRK-SRT-HJO-CNT2    PIC 9(04).
           03  WRK-SRT-HJO-KOHNO   PIC X(03).
           03  WRK-SRT-CHO-IDX     PIC 9(04)    OCCURS 200.
           03  WRK-SRT-CHO-CNT     PIC 9(04).
           03  WRK-SRT-CHO-CNT2    PIC 9(04).
      *    1:保険表中から老人を検索 / 2:公費表中から老人を検索
           03  WRK-SRT-FLG         PIC X(01).
           03  WRK-SRT-NOIDX       PIC 9(04).
      *
      *    主公費中心時、生保付加の判定に使用
      *    1:外・入 / 2:低所得　外・入 /3:低年金　外・入
       01  WRK-FTNKBN-AREA.
           03  WRK-GAI-KBN    OCCURS 3 PIC X(01).
           03  WRK-NYU-KBN    OCCURS 3 PIC X(01).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-KCOMB-MAX       PIC 9(04).
           03  WRK-RCOMB-MAX       PIC 9(04).
           03  WRK-LCOMB-MAX       PIC 9(04).
           03  WRK-LCOMB2-MAX      PIC 9(04).
           03  WRK-LCOMB-MAX-O     PIC 9(04).
           03  WRK-TCOMB2-MAX      PIC 9(04).
           03  WRK-TCOMB3-MAX      PIC 9(04).
           03  WRK-FTN-FLG         PIC X(01).
      *    負担割合
           03  WRK-GAIKFTNRATE     PIC 9(03).
      *
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
      *
           03  WRK-DISCOUNTRATE    PIC Z9.
           03  WRK-DISCOUNTRATEC   PIC 9(03).
           03  WRK-IDC             PIC 9(10).
           03  WRK-KBN             PIC X(01).
           03  WRK-SAKIPG          PIC X(08).
      *
           03  WRK-NAIBU-IDX-AREA.
               05  WRK-G-IDX       PIC 9(04).
               05  WRK-N-IDX       PIC 9(04).
      *        1:保険テーブル /2:公費テーブル
               05  WRK-W-TBL   PIC 9(01).
      *
           03  WRK-COMB-AREA.
               05  WRK-COMB-RJNIDX     PIC 9(04).
               05  WRK-COMB-SYUHOIDX   PIC 9(04).
               05  WRK-COMB-HJOIDX     PIC 9(04).
               05  WRK-COMB-CHOIDX     PIC 9(04).
               05  WRK-COMB-SYUKOHIDX  PIC 9(04).
               05  WRK-COMB-IDX        PIC 9(04).
      *        １件目の保険番号マスタ情報転送済み
               05  WRK-COMB-TENSO      PIC X(01).
      *        テーブル判定フラグ　1:保険テーブル / 2:公費テーブル
               05  WRK-COMB-TBLKBN     PIC X(01).
      *        検索対象となる保険番号/公費番号
               05  WRK-COMB-HKNNUM      PIC X(03).
      *        検索対象となる支払区分
               05  WRK-COMB-PAYKBN      PIC X(02).
      *        検索対象となる適用開始年月日
               05  WRK-COMB-TEKSTYMD   PIC X(08).
      *
           03  WRK-KENSAKU-AREA.
               05  WRK-KEN-HKNNUM      PIC X(03).
               05  WRK-KEN-PAYKBN      PIC X(02).
      *----(01.00.01) LINE MOD START ---------------------------------
      *         05  WRK-KEN-TEKYMD      PIC X(08).
               05  WRK-KEN-TEKSTYMD    PIC X(08).
               05  WRK-KEN-TEKEDYMD    PIC X(08).
      *----(01.00.01) LINE MOD END   ---------------------------------
               05  WRK-KEN-ERFLG       PIC X(01).
               05  WRK-KEN-SBTKBN      PIC X(01).
               05  WRK-KEN-HOJOKBN     PIC X(01).
               05  WRK-KEN-CONTKBN     PIC X(01).
               05  WRK-KEN-SEIDO       PIC X(10).
      *
           03  WRK-HOKEN-AREA.
               05  WRK-HOKEN-REC       OCCURS   200.
                   07  WRK-HKN-HKNID       PIC 9(10).
                   07  WRK-HKN-HKNNUM      PIC X(03).
                   07  WRK-HKN-HKNJANUM    PIC X(08).
                   07  WRK-HKN-HONKZKKBN   PIC X(01).
                   07  WRK-HKN-HOJOKBN     PIC X(01).
                   07  WRK-HKN-CONTKBN     PIC X(01).
                   07  WRK-HKN-TEKSTYMD    PIC 9(08).
                   07  WRK-HKN-TEKEDYMD    PIC 9(08).
                   07  WRK-HKN-SAKJOKBN    PIC X(01).
                   07  WRK-HKN-SBT         PIC X(01).
                   07  WRK-HKN-SEIDO       PIC X(10).
      *
           03  WRK-KOUHI-AREA.
               05  WRK-KOUHI-REC       OCCURS   200.
                   07  WRK-KOH-KOHID       PIC 9(10).
                   07  WRK-KOH-HKNNUM      PIC X(03).
                   07  WRK-KOH-PAYKBN      PIC X(02).
                   07  WRK-KOH-TEKSTYMD    PIC 9(08).
                   07  WRK-KOH-TEKEDYMD    PIC 9(08).
                   07  WRK-KOH-SAKJOKBN    PIC X(01).
                   07  WRK-KOH-SBT         PIC X(01).
                   07  WRK-KOH-SEIDO       PIC X(10).
                   07  WRK-KOH-HKNKOHKBN   PIC X(01).
                   07  WRK-KOH-GENHKNNUM1  PIC X(03).
                   07  WRK-KOH-GENHKNNUM2  PIC X(03).
                   07  WRK-KOH-GENHKNNUM3  PIC X(03).
                   07  WRK-KOH-FTNJANUM    PIC X(08).
      *            第三者
                   07  WRK-KOH-JIBAISEIKBN PIC X(01).
      *
           03  WRK-SYOCYO-TABLE.
               05  WRK-SYOCYO-OCC            OCCURS   200.
                   07  WRK-SYOCYO-TEKSTYMD   PIC 9(08).
                   07  WRK-SYOCYO-TEKEDYMD   PIC 9(08).
                   07  WRK-SYOCYO-JKYSNUM    PIC X(20).
                   07  WRK-SYOCYO-CFLG1      PIC X(01).
                   07  WRK-SYOCYO-CFLG2      PIC X(01).
      *
           03  WRK-NYUSYO-TABLE.
               05  WRK-NYUSYO-OCC            OCCURS   200.
                   07  WRK-NYUSYO-TEKSTYMD   PIC 9(08).
                   07  WRK-NYUSYO-TEKEDYMD   PIC 9(08).
                   07  WRK-NYUSYO-JKYSNUM    PIC X(20).
                   07  WRK-NYUSYO-CFLG1      PIC X(01).
                   07  WRK-NYUSYO-CFLG2      PIC X(01).
      *
           03  WRK-SAIGAI-TABLE.
               05  WRK-SAIGAI-OCC            OCCURS   200.
                   07  WRK-SAIGAI-TEKSTYMD   PIC 9(08).
                   07  WRK-SAIGAI-TEKEDYMD   PIC 9(08).
                   07  WRK-SAIGAI-JKYSNUM    PIC X(20).
                   07  WRK-SAIGAI-CFLG1      PIC X(01).
                   07  WRK-SAIGAI-CFLG2      PIC X(01).
      *
           03  WRK-BUNKAT-TABLE.
               05  WRK-BUNKAT-OCC            OCCURS   200.
                   07  WRK-BUNKAT-TEKSTYMD   PIC 9(08).
                   07  WRK-BUNKAT-TEKEDYMD   PIC 9(08).
                   07  WRK-BUNKAT-JKYSNUM    PIC X(20).
                   07  WRK-BUNKAT-CFLG1      PIC X(01).
                   07  WRK-BUNKAT-CFLG2      PIC X(01).
      *
           03  WRK-BUNKAT-DATA               PIC X(38).
           03  WRK-CHECK-TEKSTYMD            PIC 9(08).
           03  WRK-CHECK-TEKEDYMD            PIC 9(08).
      *
           03  WRK-SEIHO-YMD               PIC X(08).
      *
           03  TBL-SEIHO-YMD-T.
               05  TBL-SEIHO-YMD-OCC       OCCURS   200.
                   07  TBL-SEIHO-YMD       PIC X(08).
           03  TBL-SEIHO-YMD2-T.
               05  TBL-SEIHO-YMD2-OCC      OCCURS   200.
                   07  TBL-SEIHO-YMD2      PIC X(08).
      *
       01  WRK-HIYATOI-AREA.
           03  WRK-HIYATOI-FLG             PIC 9(01).
           03  WRK-HIYATOI-TEKSTYMD        PIC X(08).
           03  WRK-HIYATOI-TEKEDYMD        PIC X(08).
      *
       01  WRK-EDIT-AREA.
           03  WRK-KEI1            PIC X(06)   VALUE  "（継）".
           03  WRK-KEI2            PIC X(06)   VALUE  "（任）".
           03  WRK-KEI3            PIC X(06)   VALUE  "（特）".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    保険番号情報
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険情報
       01  PTHKNINF-REC.
           COPY    "CPPTHKNINF.INC".
           03  PTHKN-SBT          PIC X(01).
           03  PTHKN-SEIDO        PIC X(10).
      *
      *    公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
           03  PTKOH-SBT          PIC X(01).
           03  PTKOH-SEIDO        PIC X(10).
           03  PTKOH-HKNKOHKBN    PIC X(01).
           03  PTKOH-GENHKNNUM1   PIC X(03).
           03  PTKOH-GENHKNNUM2   PIC X(03).
           03  PTKOH-GENHKNNUM3   PIC X(03).
      *    第三者
           03  PTKOH-JIBAISEIKBN  PIC X(01).
      *
      *    パラメタ
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *R01.6
      *    患者登録スパ領域
           COPY    "P02COMMON-SPA".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
           COPY    "MCPAREA".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *----(01.00.02) LINE DEL START ---------------------------------
      *    患者画面スパ領域
      *     COPY    "P02SCR-SPA".
      *----(01.00.02) LINE DEL START ---------------------------------
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA.
      *----(01.00.02) LINE DEL START ---------------------------------
      *     SPA-P02SCRAREA.
      *----(01.00.02) LINE DEL START ---------------------------------
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      000-PROC-EXT
           END-IF
      *
           PERFORM 5701-COMB-HEN-SEC
      *
           .
       000-PROC-EXT
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-KCOMB-AREA
           INITIALIZE                  WRK-RCOMB-AREA
           INITIALIZE                  WRK-LCOMB-AREA
           INITIALIZE                  WRK-TCOMB-AREA
           INITIALIZE                  WRK-TCOMB2-AREA
           INITIALIZE                  WRK-TCOMB3-AREA
           INITIALIZE                  WRK-TCOMB4-AREA
           INITIALIZE                  WRK-IDXSRT-AREA
           INITIALIZE                  WRK-HOKEN-AREA
           INITIALIZE                  WRK-KOUHI-AREA
           INITIALIZE                  WRK-FTNKBN-AREA
      *R01.6
      *    難病等＋生保保険組合せ作成区分
           MOVE    SPA-WORK            TO  SPA-P02KYOUTU
      *
      *    パラメータファイル読込
           PERFORM 320-PARAFILE-KENSAKU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタファイル検索処理
      *****************************************************************
       320-PARAFILE-KENSAKU-SEC        SECTION.
      *
      *    保険情報
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTHKNINF"          TO  PARA-FILEMEI
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
               UNTIL  (FLG-PARA    NOT =   ZERO      )  OR
                      (SPA-ERRCD   NOT =   SPACE     )
      *
               MOVE    PARA-DATA-REC       TO  PTHKNINF-REC
               IF      PTHKN-SAKJOKBN      =   SPACE
                   ADD     1               TO  CNT-HKN
                   IF  CNT-HKN             >   200
                       MOVE    "0015"      TO  SPA-ERRCD
                   ELSE
                       PERFORM 330-HKNINF-SET-SEC
                   END-IF
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    ZERO                TO  FLG-DAIRSIFLG
           MOVE    ZERO                TO  FLG-NYUSYO
           MOVE    ZERO                TO  FLG-SAIGAI
           MOVE    ZERO                TO  IDY3
           MOVE    ZERO                TO  IDY4
           MOVE    ZERO                TO  IDY6
           INITIALIZE                      WRK-SYOCYO-TABLE
           INITIALIZE                      WRK-NYUSYO-TABLE
           INITIALIZE                      WRK-SAIGAI-TABLE
      *    公費情報
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "PTKOHINF"          TO  PARA-FILEMEI
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
               UNTIL  (FLG-PARA    NOT =   ZERO      )  OR
                      (SPA-ERRCD   NOT =   SPACE     )
      *
               MOVE    PARA-DATA-REC       TO  PTKOHINF-REC
               IF     (PTKOH-SAKJOKBN  NOT =   SPACE          )
               OR     (PTKOH-KOHNUM =
                       "950" OR "951" OR "953" OR "954" OR "955" OR
                       "956" OR "957" OR "958" OR "959" OR
                       "976" OR "977" OR "978" OR "979" OR
                       "945" OR "946" OR "948" OR "949"       )
               OR     (PTKOH-KOHNUM(1:2)   =   "96"           )
                   IF   (PTKOH-KOHNUM     =
                          "950" OR "951" OR "948" OR "949")
                   AND  (PTKOH-SAKJOKBN   =  SPACE        )
                     MOVE   1            TO FLG-NYUSYO
                     ADD    1            TO IDY3
                     MOVE PTKOH-TEKSTYMD TO WRK-NYUSYO-TEKSTYMD(IDY3)
                     MOVE PTKOH-TEKEDYMD TO WRK-NYUSYO-TEKEDYMD(IDY3)
                     MOVE PTKOH-JKYSNUM  TO WRK-NYUSYO-JKYSNUM(IDY3)
                   END-IF
                   IF   (PTKOH-KOHNUM     = "959"     )
                   AND  (PTKOH-SAKJOKBN   =  SPACE    )
                   AND  (PTKOH-TEKSTYMD  >= "20161001")
                     MOVE   1            TO FLG-SAIGAI
                     ADD    1            TO IDY4
                     MOVE PTKOH-TEKSTYMD TO WRK-SAIGAI-TEKSTYMD(IDY4)
                     MOVE PTKOH-TEKEDYMD TO WRK-SAIGAI-TEKEDYMD(IDY4)
                   END-IF
               ELSE
                   ADD     1               TO  CNT-KOH
                   IF  CNT-KOH             >   200
                       MOVE    "0015"      TO  SPA-ERRCD
                   ELSE
                       PERFORM 330-KOHINF-SET-SEC
                   END-IF
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    第三者行為　負担区分の取得
           IF      FLG-DAIRSIFLG       =   1
               PERFORM 3301-DAI3RSIINF-SEC
           END-IF
      *
           .
       320-PARAFILE-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報セット処理
      *****************************************************************
       330-HKNINF-SET-SEC             SECTION.
      *
           MOVE    PTHKN-HKNID          TO  WRK-HKN-HKNID    (CNT-HKN)
           MOVE    PTHKN-HKNNUM         TO  WRK-HKN-HKNNUM   (CNT-HKN)
           MOVE    PTHKN-HKNJANUM       TO  WRK-HKN-HKNJANUM (CNT-HKN)
           MOVE    PTHKN-HONKZKKBN      TO  WRK-HKN-HONKZKKBN(CNT-HKN)
           MOVE    PTHKN-HOJOKBN        TO  WRK-HKN-HOJOKBN  (CNT-HKN)
           MOVE    PTHKN-CONTKBN        TO  WRK-HKN-CONTKBN  (CNT-HKN)
           MOVE    PTHKN-TEKSTYMD       TO  WRK-HKN-TEKSTYMD (CNT-HKN)
           MOVE    PTHKN-TEKEDYMD       TO  WRK-HKN-TEKEDYMD (CNT-HKN)
           MOVE    PTHKN-SAKJOKBN       TO  WRK-HKN-SAKJOKBN (CNT-HKN)
      *
      *    制度名等は毎回編集する
      *****IF  PTHKN-SBT                =   SPACE
               INITIALIZE                   WRK-KENSAKU-AREA
               MOVE    PTHKN-HKNNUM     TO  WRK-KEN-HKNNUM
               MOVE    ZERO             TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *         MOVE    PTHKN-TEKSTYMD   TO  WRK-KEN-TEKYMD
               MOVE    PTHKN-TEKSTYMD   TO  WRK-KEN-TEKSTYMD
               MOVE    PTHKN-TEKEDYMD   TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
      *    保険番号マスタより制度名取得
           PERFORM 4410-HKNKOH-KAKUNIN-SEC
      *
           IF  WRK-KEN-ERFLG    NOT =   SPACE
               MOVE    "0065"       TO  SPA-ERRCD
           ELSE
               MOVE    HKN-HKNKOHSBTKBN
                                           TO  WRK-HKN-SBT   (CNT-HKN)
               MOVE    PTHKN-HOJOKBN       TO  WRK-KEN-HOJOKBN
               MOVE    PTHKN-CONTKBN       TO  WRK-KEN-CONTKBN
      *        短縮制度名称編集
               PERFORM 4420-TANSEIDO-HEN-SEC
               MOVE    WRK-KEN-SEIDO       TO  WRK-HKN-SEIDO (CNT-HKN)
      *        制度名などセットしてＲＥＷＲＩＴＥする
               MOVE    HKN-HKNKOHSBTKBN    TO  PTHKN-SBT
               MOVE    WRK-KEN-SEIDO       TO  PTHKN-SEIDO
           END-IF
      *
      ******   MOVE    PTHKNINF-REC        TO  PARA-DATA-REC
      *        MOVE    PARA-REC            TO  MCPDATA-REC
      *        MOVE    "DBUPDATE"          TO  MCP-FUNC
      *        MOVE    "PARA-KEY"          TO  ORC-DBPATH
      *        CALL    "ORCMCPSUB"         USING
      *                                    MCPAREA
      *                                    ORCMCPAREA
      *                                    MCPDATA-REC
      *        IF      MCP-RC          NOT =   ZERO
      *            DISPLAY "P02W2*<330> HKNINF UPD ERR! PAR-KEY:"
      *                     PARA-KEY " HKNKEY:" PTHKN-KEY
      ******   END-IF
      *****ELSE
      *        MOVE    PTHKN-SBT        TO  WRK-HKN-SBT      (CNT-HKN)
      *        MOVE    PTHKN-SEIDO      TO  WRK-HKN-SEIDO    (CNT-HKN)
      *****END-IF
      *
           .
       330-HKNINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費情報セット処理
      *****************************************************************
       330-KOHINF-SET-SEC               SECTION.
      *
           IF      PTKOH-KOHNUM(1:2) = "99"
             MOVE   1            TO FLG-SYOCYO
             ADD    1            TO IDY6
             MOVE PTKOH-TEKSTYMD TO WRK-SYOCYO-TEKSTYMD(IDY6)
             MOVE PTKOH-TEKEDYMD TO WRK-SYOCYO-TEKEDYMD(IDY6)
           END-IF
      *
           MOVE    PTKOH-KOHID          TO  WRK-KOH-KOHID     (CNT-KOH)
           MOVE    PTKOH-KOHNUM         TO  WRK-KOH-HKNNUM    (CNT-KOH)
      *
           MOVE    PTKOH-PAYKBN         TO  WRK-KOH-PAYKBN    (CNT-KOH)
      *
           MOVE    PTKOH-TEKSTYMD       TO  WRK-KOH-TEKSTYMD  (CNT-KOH)
           MOVE    PTKOH-TEKEDYMD       TO  WRK-KOH-TEKEDYMD  (CNT-KOH)
           MOVE    PTKOH-SAKJOKBN       TO  WRK-KOH-SAKJOKBN  (CNT-KOH)
      *    精神通院と特定疾患（水俣）の保険組合せ作成用の負担者番号判定用
           MOVE    PTKOH-FTNJANUM       TO  WRK-KOH-FTNJANUM  (CNT-KOH)
      *
      *
      *    制度名等は毎回編集する
      *****IF  PTKOH-SBT                =   SPACE
               INITIALIZE                   WRK-KENSAKU-AREA
               MOVE    PTKOH-KOHNUM     TO  WRK-KEN-HKNNUM
               MOVE    PTKOH-PAYKBN     TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *         MOVE    PTKOH-TEKSTYMD   TO  WRK-KEN-TEKYMD
               MOVE    PTKOH-TEKSTYMD   TO  WRK-KEN-TEKSTYMD
               MOVE    PTKOH-TEKEDYMD   TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
      *    保険番号マスタより制度名取得
           PERFORM 4410-HKNKOH-KAKUNIN-SEC
           IF  WRK-KEN-ERFLG    NOT =   SPACE
               MOVE    "0065"       TO  SPA-ERRCD
           ELSE
               MOVE    HKN-HKNKOHSBTKBN TO  WRK-KOH-SBT       (CNT-KOH)
               MOVE    HKN-HKNKOHKBN    TO  WRK-KOH-HKNKOHKBN (CNT-KOH)
               MOVE    HKN-GENHKNNUM1   TO  WRK-KOH-GENHKNNUM1(CNT-KOH)
               MOVE    HKN-GENHKNNUM2   TO  WRK-KOH-GENHKNNUM2(CNT-KOH)
               MOVE    HKN-GENHKNNUM3   TO  WRK-KOH-GENHKNNUM3(CNT-KOH)
               MOVE    HKN-TANSEIDONAME TO  WRK-KOH-SEIDO     (CNT-KOH)
      *        制度名などセットしてＲＥＷＲＩＴＥする
               MOVE    HKN-HKNKOHSBTKBN TO  PTKOH-SBT
               MOVE    HKN-HKNKOHKBN    TO  PTKOH-HKNKOHKBN
               MOVE    HKN-GENHKNNUM1   TO  PTKOH-GENHKNNUM1
               MOVE    HKN-GENHKNNUM2   TO  PTKOH-GENHKNNUM2
               MOVE    HKN-GENHKNNUM3   TO  PTKOH-GENHKNNUM3
               MOVE    HKN-TANSEIDONAME TO  PTKOH-SEIDO
      *
      *        第三者行為　負担区分の取得
               IF      PTKOH-KOHNUM     =   "970"
                   MOVE    1                TO  FLG-DAIRSIFLG
               END-IF
           END-IF
      *
      ****     MOVE    PTKOHINF-REC        TO  PARA-DATA-REC
      *        MOVE    PARA-REC            TO  MCPDATA-REC
      *        MOVE    "DBUPDATE"          TO  MCP-FUNC
      *        MOVE    "PARA-KEY"          TO  ORC-DBPATH
      *        CALL    "ORCMCPSUB"         USING
      *                                    MCPAREA
      *                                    ORCMCPAREA
      *                                    MCPDATA-REC
      *        IF      MCP-RC          NOT =   ZERO
      *            DISPLAY "P02W2*<330> KOHINF UPD ERR! PAR-KEY:"
      *                     PARA-KEY " KOHKEY:" PTKOH-KEY
      ****     END-IF
      **** ELSE
      *        MOVE    PTKOH-SBT        TO  WRK-KOH-SBT       (CNT-KOH)
      *        MOVE    PTKOH-HKNKOHKBN  TO  WRK-KOH-HKNKOHKBN (CNT-KOH)
      *        MOVE    PTKOH-GENHKNNUM1 TO  WRK-KOH-GENHKNNUM1(CNT-KOH)
      *        MOVE    PTKOH-GENHKNNUM2 TO  WRK-KOH-GENHKNNUM2(CNT-KOH)
      *        MOVE    PTKOH-GENHKNNUM3 TO  WRK-KOH-GENHKNNUM3(CNT-KOH)
      *        MOVE    PTKOH-SEIDO      TO  WRK-KOH-SEIDO     (CNT-KOH)
      *****END-IF
      *
           .
       330-KOHINF-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    第三者行為　負担区分の取得処理
      *****************************************************************
       3301-DAI3RSIINF-SEC         SECTION.
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
               UNTIL  (FLG-PARA    NOT =   ZERO      )
      *
               MOVE    PARA-DATA-REC       TO  PTRSIINF-REC
               IF     (PTRSI-HKNKBN        =   "6"    )
                AND   (PTRSI-SAKUJOKBN     =   SPACE  )
      *
                   PERFORM VARYING     IDXR    FROM    1   BY  1
                           UNTIL       IDXR    >   CNT-KOH
                       IF      (WRK-KOH-KOHID (IDXR)   =   PTRSI-KOHID)
                           MOVE    PTRSI-JIBAISEIKBN
                                               TO  WRK-KOH-JIBAISEIKBN
                                                              (IDXR)
      *TEST
      ***     DISPLAY "P02W2 IDXR:" IDXR
      ***             ",KOHID:" WRK-KOH-KOHID (IDXR) 
      ***             ",HKNNUM:" WRK-KOH-HKNNUM (IDXR) 
      ***             ",JIBAISEIKBN:" WRK-KOH-JIBAISEIKBN (IDXR)
      *TEST
                           MOVE    CNT-KOH     TO  IDXR
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3301-DAI3RSIINF-EXT.
           EXIT.
      *
      *****************************************************************
      *    短縮制度名　編集処理
      *****************************************************************
       4420-TANSEIDO-HEN-SEC         SECTION.
      *
           MOVE    SPACE             TO  WRK-KEN-SEIDO
      *
           IF ((WRK-KEN-HKNNUM       =   "002" )  AND
               (WRK-KEN-HOJOKBN  NOT =   SPACE) )   OR
      *    共済
              ((WRK-KEN-HKNNUM       =   "031"  OR  "032"  OR
                                         "033"  OR  "034" ) AND
               (WRK-KEN-HOJOKBN      =   "2"  OR  "B"  OR  "E"
                                                       OR  "H"))
               EVALUATE    WRK-KEN-HOJOKBN
               WHEN    "1"
                   MOVE    "船員職務上"        TO  WRK-KEN-SEIDO
               WHEN    "2"
                   MOVE    "船員下船後"        TO  WRK-KEN-SEIDO
               WHEN    "3"
                   MOVE    "船員通勤"          TO  WRK-KEN-SEIDO
               WHEN    "A"
                   MOVE    "船職務高１"        TO  WRK-KEN-SEIDO
               WHEN    "B"
                   MOVE    "船下船高１"        TO  WRK-KEN-SEIDO
               WHEN    "C"
                   MOVE    "船通勤高１"        TO  WRK-KEN-SEIDO
               WHEN    "D"
                   MOVE    "船職務高２"        TO  WRK-KEN-SEIDO
               WHEN    "E"
                   MOVE    "船下船高２"        TO  WRK-KEN-SEIDO
               WHEN    "F"
                   MOVE    "船通勤高２"        TO  WRK-KEN-SEIDO
      *        Ｈ１８．１０改正
               WHEN    "G"
                   MOVE    "船職務高３"        TO  WRK-KEN-SEIDO
               WHEN    "H"
                   MOVE    "船下船高３"        TO  WRK-KEN-SEIDO
               WHEN    "I"
                   MOVE    "船通勤高３"        TO  WRK-KEN-SEIDO
               WHEN    OTHER
                   MOVE    HKN-TANSEIDONAME    TO  WRK-KEN-SEIDO
               END-EVALUATE
      *
               IF  WRK-KEN-HKNNUM = "031" OR "032" OR "033" OR "034"
                   MOVE    SPACE               TO  WRK-KEN-SEIDO
                   EVALUATE    WRK-KEN-HKNNUM
                     WHEN    "031"
                       EVALUATE    WRK-KEN-HOJOKBN
                         WHEN    "2"
                           MOVE  "国公下船後"  TO  WRK-KEN-SEIDO
                         WHEN    "B"
                           MOVE  "国下船高１"  TO  WRK-KEN-SEIDO
                         WHEN    "E"
                           MOVE  "国下船高２"  TO  WRK-KEN-SEIDO
                         WHEN    "H"
                           MOVE  "国下船高３"  TO  WRK-KEN-SEIDO
                       END-EVALUATE
                     WHEN    "032"
                       EVALUATE    WRK-KEN-HOJOKBN
                         WHEN    "2"
                           MOVE  "地公下船後"  TO  WRK-KEN-SEIDO
                         WHEN    "B"
                           MOVE  "地下船高１"  TO  WRK-KEN-SEIDO
                         WHEN    "E"
                           MOVE  "地下船高２"  TO  WRK-KEN-SEIDO
                         WHEN    "H"
                           MOVE  "地下船高３"  TO  WRK-KEN-SEIDO
                       END-EVALUATE
                     WHEN    "033"
                       EVALUATE    WRK-KEN-HOJOKBN
                         WHEN    "2"
                           MOVE  "警察下船後"  TO  WRK-KEN-SEIDO
                         WHEN    "B"
                           MOVE  "警下船高１"  TO  WRK-KEN-SEIDO
                         WHEN    "E"
                           MOVE  "警下船高２"  TO  WRK-KEN-SEIDO
                         WHEN    "H"
                           MOVE  "警下船高３"  TO  WRK-KEN-SEIDO
                       END-EVALUATE
                     WHEN    "034"
                       EVALUATE    WRK-KEN-HOJOKBN
                         WHEN    "2"
                           MOVE  "学校下船後"  TO  WRK-KEN-SEIDO
                         WHEN    "B"
                           MOVE  "学下船高１"  TO  WRK-KEN-SEIDO
                         WHEN    "E"
                           MOVE  "学下船高２"  TO  WRK-KEN-SEIDO
                         WHEN    "H"
                           MOVE  "学下船高３"  TO  WRK-KEN-SEIDO
                       END-EVALUATE
                   END-EVALUATE
               END-IF
           ELSE
               MOVE    HKN-TANSEIDONAME    TO  WRK-KEN-SEIDO
               EVALUATE    WRK-KEN-CONTKBN
               WHEN    "1"
      *        ...継続区分＝"1"の場合、後に[継]　EX.船員（継）
                   STRING  WRK-KEN-SEIDO   DELIMITED  BY  " "
                           WRK-KEI1        DELIMITED  BY  SIZE
                           INTO            WRK-KEN-SEIDO
                   END-STRING
               WHEN    "2"
      *        ...継続区分＝"2"の場合、後に[任]　EX.船員（任）
                   STRING  WRK-KEN-SEIDO   DELIMITED  BY  " "
                           WRK-KEI2        DELIMITED  BY  SIZE
                           INTO            WRK-KEN-SEIDO
                   END-STRING
               WHEN    "3"
      *        ...継続区分＝"3"の場合、後に[特]　EX.一般（特）
                   STRING  WRK-KEN-SEIDO   DELIMITED  BY  " "
                           WRK-KEI3        DELIMITED  BY  SIZE
                           INTO            WRK-KEN-SEIDO
                   END-STRING
               END-EVALUATE
           END-IF
      *
          .
       4420-TANSEIDO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    組合せ情報編集
      *****************************************************************
       5701-COMB-HEN-SEC             SECTION.
      *
      *
      *    組み合わせ用作業パラメタを削除する
           INITIALIZE                      PARA-REC
           MOVE    "P02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "NC01PARA"          TO  PARA-FILEMEI
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
      *   主保険中心　組合せ作成処理
           PERFORM     57011-SYUHKN-HEN-SEC
      *
      *   生活保護単独　組合せ作成処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM     57013-SEIHO-HEN-SEC
           END-IF
      *
      *    公費単独　　組合せ作成処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM     57013-KOHTANDOKU-HEN-SEC
           END-IF
      *
      *   主公費中心　組合せ作成処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM     57014-SYUKOH-HEN-SEC
           END-IF
      *
      *   自費・治験・労災　組合せ作成処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM     57015-JIHCHIROU-HEN-SEC
           END-IF
      *
           .
       5701-COMB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ出力処理
      *****************************************************************
       57010-NC01PARA-OUT-SEC          SECTION.
      *
           IF    FLG-SYOCYO   =   1
             INITIALIZE  WRK-BUNKAT-TABLE
             MOVE        WRK-SYOCYO-TABLE     TO  WRK-BUNKAT-TABLE
      *      日付順に並び替え
             PERFORM  VARYING  IDY3  FROM  1  BY  1
                       UNTIL  (IDY3                      >  200 )
                        OR    (WRK-BUNKAT-TEKSTYMD(IDY3) =  ZERO)
               COMPUTE  IDY4  =  IDY3  +  1
               PERFORM  VARYING  IDY5  FROM  IDY4  BY  1
                         UNTIL  (IDY5                      >  200 )
                          OR    (WRK-BUNKAT-TEKSTYMD(IDY5) =  ZERO)
                 IF   WRK-BUNKAT-TEKSTYMD(IDY3) >
                                          WRK-BUNKAT-TEKSTYMD(IDY5)
                   MOVE  WRK-BUNKAT-OCC(IDY3) TO  WRK-BUNKAT-DATA
                   MOVE  WRK-BUNKAT-OCC(IDY5) TO  WRK-BUNKAT-OCC(IDY3)
                   MOVE  WRK-BUNKAT-DATA      TO  WRK-BUNKAT-OCC(IDY5)
                 END-IF
               END-PERFORM
             END-PERFORM
           END-IF
      *
           INITIALIZE                    WRK-TCOMB2-AREA
           INITIALIZE                    WRK-TCOMB3-AREA
           INITIALIZE                    WRK-TCOMB4-AREA
           MOVE    ZERO              TO  WRK-TCOMB2-MAX
           MOVE    ZERO              TO  WRK-TCOMB3-MAX
      *
           PERFORM   VARYING   IDZ   FROM    1   BY  1
                      UNTIL  ( IDZ   >       200           )
                       OR    ( IDZ   >       WRK-LCOMB-MAX )
            IF     (  FLG-SYOCYO   =  ZERO                    )
            OR     (( FLG-SYOCYO   =   1                     ) AND
                    ((WRK-LCOMB-KOH1HKNNUM(IDZ)(1:2) = "99" ) OR
                     (WRK-LCOMB-KOH2HKNNUM(IDZ)(1:2) = "99" ) OR
                     (WRK-LCOMB-KOH3HKNNUM(IDZ)(1:2) = "99" ) OR
                     (WRK-LCOMB-KOH4HKNNUM(IDZ)(1:2) = "99" ) OR
                     (WRK-LCOMB-HKNNUM(IDZ)(1:1)     = "9"  )))
            OR     ( (WRK-LCOMB-KOH1HKNNUM(IDZ)      = "970"
                                                    OR "940") OR
                     (WRK-LCOMB-KOH2HKNNUM(IDZ)      = "970"
                                                    OR "940") OR
                     (WRK-LCOMB-KOH3HKNNUM(IDZ)      = "970"
                                                    OR "940") OR
                     (WRK-LCOMB-KOH4HKNNUM(IDZ)      = "970"
                                                    OR "940") )
              IF  (WRK-LCOMB-KOH1HKNNUM(IDZ) = "970" OR "940")
              OR  (WRK-LCOMB-KOH2HKNNUM(IDZ) = "970" OR "940")
              OR  (WRK-LCOMB-KOH3HKNNUM(IDZ) = "970" OR "940")
              OR  (WRK-LCOMB-KOH4HKNNUM(IDZ) = "970" OR "940")
                IF   (WRK-LCOMB-HKNNUM(IDZ)       NOT = SPACE)
                AND  (WRK-LCOMB-KOH1HKNNUM(IDZ)  = "970"     )
                AND  (WRK-LCOMB-KOH2HKNNUM(IDZ)  =  SPACE    )
                AND  (WRK-LCOMB-KOH3HKNNUM(IDZ)  =  SPACE    )
                AND  (WRK-LCOMB-KOH4HKNNUM(IDZ)  =  SPACE    )
                AND  (WRK-LCOMB-TEKEDYMD(IDZ)   >= "20080401")
                  IF  WRK-LCOMB-TEKSTYMD(IDZ)   <  "20080401"
                    MOVE  "20080401"  TO  WRK-LCOMB-TEKSTYMD(IDZ)
                  END-IF
                  IF  WRK-LCOMB-DELKBN(IDZ) = "9"
                    MOVE   SPACE      TO  WRK-LCOMB-DELKBN(IDZ)
                  END-IF
                  ADD     1    TO  WRK-TCOMB2-MAX
                  MOVE    WRK-LCOMB-REC(IDZ)
                               TO  WRK-TCOMB2-REC(WRK-TCOMB2-MAX)
                END-IF
                IF   (WRK-LCOMB-HKNNUM(IDZ)       NOT = SPACE)
                AND  (WRK-LCOMB-KOH1HKNNUM(IDZ)  = "940"     )
                AND  (WRK-LCOMB-KOH2HKNNUM(IDZ)  =  SPACE    )
                AND  (WRK-LCOMB-KOH3HKNNUM(IDZ)  =  SPACE    )
                AND  (WRK-LCOMB-KOH4HKNNUM(IDZ)  =  SPACE    )
                  ADD     1    TO  WRK-TCOMB2-MAX
                  MOVE    WRK-LCOMB-REC(IDZ)
                               TO  WRK-TCOMB2-REC(WRK-TCOMB2-MAX)
                END-IF
              ELSE
                  ADD     1    TO  WRK-TCOMB2-MAX
                  MOVE    WRK-LCOMB-REC(IDZ)
                               TO  WRK-TCOMB2-REC(WRK-TCOMB2-MAX)
              END-IF
            ELSE
              IF  FLG-SYOCYO   =   1
                PERFORM   57010X-NC01PARA-SYOCYO-HEN-SEC
              END-IF
            END-IF
           END-PERFORM
      *
      *    組合せ出力処理（入所分編集）
           IF    FLG-NYUSYO   =   1
             MOVE        1                  TO   FLG-CHECK
             INITIALIZE  WRK-BUNKAT-TABLE
             MOVE        WRK-NYUSYO-TABLE   TO   WRK-BUNKAT-TABLE
             PERFORM     57010X-NC01PARA-BUNKAT-HEN-SEC
           END-IF
      *    組合せ出力処理（災害分編集）
           IF    FLG-SAIGAI   =   1
             MOVE        ZERO               TO   FLG-CHECK
             INITIALIZE  WRK-BUNKAT-TABLE
             MOVE        WRK-SAIGAI-TABLE   TO   WRK-BUNKAT-TABLE
             PERFORM     57010X-NC01PARA-BUNKAT-HEN-SEC
           END-IF
      *
           PERFORM   VARYING   IDZ   FROM    1   BY  1
                      UNTIL  ( IDZ   >       200            )
                       OR    ( IDZ   >       WRK-TCOMB2-MAX )
             ADD     1             TO  CNT-COMB
             MOVE    SPA-HOSPNUM   TO  WRK-TCOMB2-HOSPNUM    (IDZ)
             MOVE    SPA-PTID      TO  WRK-TCOMB2-PTID       (IDZ)
             MOVE    ZERO          TO  WRK-TCOMB2-HKNCOMBINUM(IDZ)
             INITIALIZE                PARA-REC
             MOVE    "P02"         TO  PARA-GYOUMUID
             MOVE    SPA-TERMID    TO  PARA-TERMID
             MOVE    "NC01PARA"    TO  PARA-FILEMEI
             MOVE    CNT-COMB      TO  PARA-EDANUM
             INITIALIZE                NC01PARA-REC
             MOVE    WRK-TCOMB2-REC(IDZ)
                                   TO  PARA-NC01-REC
             MOVE    NC01PARA-REC  TO  PARA-DATA-REC
      *
             MOVE    SPA-HOSPNUM   TO  PARA-HOSPNUM
             MOVE    PARA-REC      TO  MCPDATA-REC
             MOVE    "DBINSERT"    TO  MCP-FUNC
             MOVE    "tbl_para"    TO  MCP-TABLE
             MOVE    "key"         TO  MCP-PATHNAME
             CALL    "ORCDBMAIN"   USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           END-PERFORM
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
      *
           .
       57010-NC01PARA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ出力処理（省庁分編集）
      *****************************************************************
       57010X-NC01PARA-SYOCYO-HEN-SEC          SECTION.
      *
           INITIALIZE           WRK-TCOMB4-AREA
           MOVE    WRK-LCOMB-REC(IDZ)
                            TO  WRK-TCOMB4-REC(1)
      *
           PERFORM   VARYING   IDY3   FROM    1   BY  1
                      UNTIL  ( IDY3   >       200               )
                       OR    ( WRK-BUNKAT-TEKSTYMD(IDY3) = ZERO )
      *
             INITIALIZE           WRK-TCOMB3-AREA
             MOVE     ZERO    TO  WRK-TCOMB3-MAX
      *
             PERFORM   VARYING   IDY6   FROM    1   BY  1
                        UNTIL  ( IDY6   >       200                )
                         OR    ( WRK-TCOMB4-TEKSTYMD(IDY6) = SPACE )
                 MOVE   ZERO    TO  FLG-TUUKA
      *
                 IF  (WRK-TCOMB4-TEKSTYMD(IDY6)
                                         >= WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB4-TEKEDYMD(IDY6)
                                         >  WRK-BUNKAT-TEKEDYMD(IDY3))
                 AND (WRK-TCOMB4-TEKSTYMD(IDY6)
                                         <= WRK-BUNKAT-TEKEDYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB3-MAX
                   MOVE    WRK-TCOMB4-REC(IDY6)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB3-MAX)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE     1              TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO
                              WRK-TCOMB3-TEKSTYMD(WRK-TCOMB3-MAX)
                 END-IF
                 IF  (WRK-TCOMB4-TEKSTYMD(IDY6)
                                         <  WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB4-TEKEDYMD(IDY6)
                                         <= WRK-BUNKAT-TEKEDYMD(IDY3))
                 AND (WRK-TCOMB4-TEKEDYMD(IDY6)
                                         >= WRK-BUNKAT-TEKSTYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB3-MAX
                   MOVE    WRK-TCOMB4-REC(IDY6)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB3-MAX)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO
                              WRK-TCOMB3-TEKEDYMD(WRK-TCOMB3-MAX)
                 END-IF
                 IF  (WRK-TCOMB4-TEKSTYMD(IDY6)
                                         <  WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB4-TEKEDYMD(IDY6)
                                         >  WRK-BUNKAT-TEKEDYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB3-MAX
                   MOVE    WRK-TCOMB4-REC(IDY6)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB3-MAX)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE    -1              TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO
                              WRK-TCOMB3-TEKEDYMD(WRK-TCOMB3-MAX)
      *
                   ADD     1    TO  WRK-TCOMB3-MAX
                   MOVE    WRK-TCOMB4-REC(IDY6)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB3-MAX)
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"            TO  LNK-DAY6-IRAI
                   MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)
                                           TO  LNK-DAY6-KIJUN
                   MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                   MOVE     1              TO  LNK-DAY6-ZOGEN
                   CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
                   MOVE    LNK-DAY6-KEISAN TO
                              WRK-TCOMB3-TEKSTYMD(WRK-TCOMB3-MAX)
                 END-IF
                 IF   FLG-TUUKA  =  ZERO
                   ADD     1    TO  WRK-TCOMB3-MAX
                   MOVE    WRK-TCOMB4-REC(IDY6)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB3-MAX)
                 END-IF
             END-PERFORM
      *
             MOVE     WRK-TCOMB3-AREA    TO  WRK-TCOMB4-AREA
      *
           END-PERFORM
      *
           PERFORM   VARYING   IDY6   FROM    1   BY  1
                      UNTIL  ( IDY6   >       200                )
                       OR    ( WRK-TCOMB4-TEKSTYMD(IDY6) = SPACE )
             MOVE      ZERO    TO    FLG-NG
             PERFORM   VARYING   IDY3   FROM    1   BY  1
                        UNTIL  ( IDY3   >       200               )
                         OR    ( WRK-BUNKAT-TEKSTYMD(IDY3) = ZERO )
               IF ((WRK-TCOMB4-TEKSTYMD(IDY6)
                                < WRK-BUNKAT-TEKSTYMD(IDY3)) AND
                   (WRK-TCOMB4-TEKEDYMD(IDY6)
                                < WRK-BUNKAT-TEKSTYMD(IDY3)))
               OR ((WRK-BUNKAT-TEKEDYMD(IDY3)
                                < WRK-TCOMB4-TEKSTYMD(IDY6)) AND
                   (WRK-BUNKAT-TEKEDYMD(IDY3)
                                < WRK-TCOMB4-TEKEDYMD(IDY6)))
                  CONTINUE
               ELSE
                  MOVE    1    TO    FLG-NG
                  MOVE   200   TO    IDY3
               END-IF
             END-PERFORM
             IF   FLG-NG  =  ZERO
               ADD        1    TO    WRK-TCOMB2-MAX
               MOVE      WRK-TCOMB4-REC(IDY6)
                               TO    WRK-TCOMB2-REC(WRK-TCOMB2-MAX)
             END-IF
           END-PERFORM
      *
           .
       57010X-NC01PARA-SYOCYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    組合せ出力処理（入所分・災害分編集）
      *****************************************************************
       57010X-NC01PARA-BUNKAT-HEN-SEC          SECTION.
      *
      *    日付順に並び替え
           PERFORM  VARYING  IDY3  FROM  1  BY  1
                     UNTIL  (IDY3                      >  200 )
                      OR    (WRK-BUNKAT-TEKSTYMD(IDY3) =  ZERO)
             COMPUTE  IDY4  =  IDY3  +  1
             PERFORM  VARYING  IDY5  FROM  IDY4  BY  1
                       UNTIL  (IDY5                      >  200 )
                        OR    (WRK-BUNKAT-TEKSTYMD(IDY5) =  ZERO)
               IF   WRK-BUNKAT-TEKSTYMD(IDY3) >
                                        WRK-BUNKAT-TEKSTYMD(IDY5)
                 MOVE  WRK-BUNKAT-OCC(IDY3) TO  WRK-BUNKAT-DATA
                 MOVE  WRK-BUNKAT-OCC(IDY5) TO  WRK-BUNKAT-OCC(IDY3)
                 MOVE  WRK-BUNKAT-DATA      TO  WRK-BUNKAT-OCC(IDY5)
               END-IF
             END-PERFORM
           END-PERFORM
      *
           IF    FLG-CHECK   =   1
             PERFORM  VARYING  IDY3  FROM  1  BY  1
                       UNTIL  (IDY3                      >  200 )
                        OR    (WRK-BUNKAT-TEKSTYMD(IDY3) =  ZERO)
               MOVE    ZERO                       TO  WRK-CHECK-TEKSTYMD
               MOVE    ZERO                       TO  WRK-CHECK-TEKEDYMD
               COMPUTE IDY4  =  IDY3  -  1
               COMPUTE IDY5  =  IDY3  +  1
               IF   IDY4  >  ZERO
                 INITIALIZE                           STS-AREA-DAY
                 INITIALIZE                           LNK-DAY6-AREA
                 MOVE  "61"                       TO  LNK-DAY6-IRAI
                 MOVE  WRK-BUNKAT-TEKSTYMD(IDY3)  TO  LNK-DAY6-KIJUN
                 MOVE  "1"                        TO  LNK-DAY6-ZOGENPTN
                 MOVE  -1                         TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"  USING  STS-AREA-DAY
                                         LNK-DAY6-AREA
                 MOVE  LNK-DAY6-KEISAN            TO  WRK-CHECK-TEKEDYMD
               END-IF
               IF  (IDY5  <  201                             )
               AND (WRK-BUNKAT-TEKEDYMD(IDY3)  NOT = 99999999)
                 INITIALIZE                           STS-AREA-DAY
                 INITIALIZE                           LNK-DAY6-AREA
                 MOVE  "61"                       TO  LNK-DAY6-IRAI
                 MOVE  WRK-BUNKAT-TEKEDYMD(IDY3)  TO  LNK-DAY6-KIJUN
                 MOVE  "1"                        TO  LNK-DAY6-ZOGENPTN
                 MOVE   1                         TO  LNK-DAY6-ZOGEN
                 CALL  "ORCSDAY"  USING  STS-AREA-DAY
                                         LNK-DAY6-AREA
                 MOVE  LNK-DAY6-KEISAN            TO  WRK-CHECK-TEKSTYMD
               END-IF
      *
               IF  WRK-CHECK-TEKEDYMD  NOT = ZERO
                 IF  WRK-CHECK-TEKEDYMD = WRK-BUNKAT-TEKEDYMD(IDY4)
                       MOVE   "1"   TO   WRK-BUNKAT-CFLG1(IDY3)
                 END-IF
               END-IF
               IF  WRK-CHECK-TEKSTYMD  NOT = ZERO
                 IF  WRK-CHECK-TEKSTYMD = WRK-BUNKAT-TEKSTYMD(IDY5)
                       MOVE   "1"   TO   WRK-BUNKAT-CFLG2(IDY3)
                 END-IF
               END-IF
             END-PERFORM
           END-IF
      *
           PERFORM   VARYING   IDY3   FROM    1   BY  1
                      UNTIL  ( IDY3   >       200               )
                       OR    ( WRK-BUNKAT-TEKSTYMD(IDY3) = ZERO )
      *
             INITIALIZE           WRK-TCOMB3-AREA
             MOVE     ZERO    TO  WRK-TCOMB2-MAX
      *
             PERFORM   VARYING   IDZ    FROM    1   BY  1
                        UNTIL  ( IDZ    >       200               )
                         OR    ( WRK-TCOMB2-TEKSTYMD(IDZ) = SPACE )
               IF  WRK-TCOMB2-SYU-HKNKOHSBTKBN(IDZ)
                                               = "4" OR "8" OR "9"
                 ADD       1    TO  WRK-TCOMB2-MAX
                 MOVE      WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
               ELSE
                 MOVE   ZERO    TO  FLG-TUUKA
                 IF  (WRK-TCOMB2-TEKSTYMD(IDZ)
                                         >= WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB2-TEKEDYMD(IDZ)
                                         >  WRK-BUNKAT-TEKEDYMD(IDY3))
                 AND (WRK-TCOMB2-TEKSTYMD(IDZ)
                                         <= WRK-BUNKAT-TEKEDYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)  TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   IF   WRK-BUNKAT-JKYSNUM(IDY3)(1:1) = "1"
                   AND  WRK-BUNKAT-CFLG2(IDY3)        = SPACE
                    MOVE    WRK-BUNKAT-TEKEDYMD(IDY3) TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                   ELSE
                    INITIALIZE                  STS-AREA-DAY
                    INITIALIZE                  LNK-DAY6-AREA
                    MOVE    "61"            TO  LNK-DAY6-IRAI
                    MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)
                                            TO  LNK-DAY6-KIJUN
                    MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                    MOVE     1              TO  LNK-DAY6-ZOGEN
                    CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                    MOVE    LNK-DAY6-KEISAN TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                   END-IF
                 END-IF
                 IF  (WRK-TCOMB2-TEKSTYMD(IDZ)
                                         <  WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB2-TEKEDYMD(IDZ)
                                         <= WRK-BUNKAT-TEKEDYMD(IDY3))
                 AND (WRK-TCOMB2-TEKEDYMD(IDZ)
                                         >= WRK-BUNKAT-TEKSTYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   IF   WRK-BUNKAT-JKYSNUM(IDY3)(1:1) = "1"
                   AND  WRK-BUNKAT-CFLG1(IDY3)        = SPACE
                    MOVE    WRK-BUNKAT-TEKSTYMD(IDY3) TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
                   ELSE
                    INITIALIZE                  STS-AREA-DAY
                    INITIALIZE                  LNK-DAY6-AREA
                    MOVE    "61"            TO  LNK-DAY6-IRAI
                    MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)
                                            TO  LNK-DAY6-KIJUN
                    MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                    MOVE    -1              TO  LNK-DAY6-ZOGEN
                    CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                    MOVE    LNK-DAY6-KEISAN TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
                   END-IF
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)  TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                 END-IF
                 IF  (WRK-TCOMB2-TEKSTYMD(IDZ)
                                         <  WRK-BUNKAT-TEKSTYMD(IDY3))
                 AND (WRK-TCOMB2-TEKEDYMD(IDZ)
                                         >  WRK-BUNKAT-TEKEDYMD(IDY3))
                   MOVE    1    TO  FLG-TUUKA
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   IF   WRK-BUNKAT-JKYSNUM(IDY3)(1:1) = "1"
                   AND  WRK-BUNKAT-CFLG1(IDY3)        = SPACE
                    MOVE    WRK-BUNKAT-TEKSTYMD(IDY3) TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
                   ELSE
                    INITIALIZE                  STS-AREA-DAY
                    INITIALIZE                  LNK-DAY6-AREA
                    MOVE    "61"            TO  LNK-DAY6-IRAI
                    MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)
                                            TO  LNK-DAY6-KIJUN
                    MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                    MOVE    -1              TO  LNK-DAY6-ZOGEN
                    CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                    MOVE    LNK-DAY6-KEISAN TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
                   END-IF
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   MOVE    WRK-BUNKAT-TEKSTYMD(IDY3)  TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                   MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)  TO
                               WRK-TCOMB3-TEKEDYMD(WRK-TCOMB2-MAX)
      *
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                   IF   WRK-BUNKAT-JKYSNUM(IDY3)(1:1) = "1"
                   AND  WRK-BUNKAT-CFLG2(IDY3)        = SPACE
                    MOVE    WRK-BUNKAT-TEKEDYMD(IDY3) TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                   ELSE
                    INITIALIZE                  STS-AREA-DAY
                    INITIALIZE                  LNK-DAY6-AREA
                    MOVE    "61"            TO  LNK-DAY6-IRAI
                    MOVE    WRK-BUNKAT-TEKEDYMD(IDY3)
                                            TO  LNK-DAY6-KIJUN
                    MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                    MOVE     1              TO  LNK-DAY6-ZOGEN
                    CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                    LNK-DAY6-AREA
                    MOVE    LNK-DAY6-KEISAN TO
                               WRK-TCOMB3-TEKSTYMD(WRK-TCOMB2-MAX)
                   END-IF
                 END-IF
                 IF   FLG-TUUKA  =  ZERO
                   ADD     1    TO  WRK-TCOMB2-MAX
                   MOVE    WRK-TCOMB2-REC(IDZ)
                                TO  WRK-TCOMB3-REC(WRK-TCOMB2-MAX)
                 END-IF
               END-IF
             END-PERFORM
      *
             MOVE     WRK-TCOMB3-AREA    TO  WRK-TCOMB2-AREA
      *
           END-PERFORM
      *
           .
       57010X-NC01PARA-BUNKAT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険中心　組合せ作成処理
      *****************************************************************
       57011-SYUHKN-HEN-SEC           SECTION.
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
           INITIALIZE                      WRK-HIYATOI-AREA
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX   >       200 )
                       OR      ( IDX   >       CNT-HKN )
             IF  WRK-HKN-SBT (IDX)     =   "1"
      *!       特別療養費は対象外
             AND (WRK-HKN-HKNNUM (IDX) NOT =   "068"
                                           AND "040"
                                           AND "069")
      *!
      *        一般療養、特別療養（旧日雇、旧特別日雇）の判定
               IF   WRK-HKN-HKNNUM (IDX)       =   "003"  OR "004"
                   MOVE    1           TO   WRK-HIYATOI-FLG
                   MOVE    WRK-HKN-TEKSTYMD (IDX)
                                       TO   WRK-HIYATOI-TEKSTYMD
                   MOVE    WRK-HKN-TEKEDYMD (IDX)
                                       TO   WRK-HIYATOI-TEKEDYMD
               END-IF
      *        == １組目 ==
               INITIALIZE                  WRK-COMB-AREA
               INITIALIZE                  WRK-KCOMB-AREA
               MOVE    ZERO            TO  WRK-KCOMB-MAX
               ADD     1               TO  WRK-KCOMB-MAX
               MOVE    WRK-KCOMB-MAX   TO  IDX2
               MOVE    SPACE           TO  WRK-KCOMB-REC        (IDX2)
               INITIALIZE                  WRK-KCOMB-REC        (IDX2)
               MOVE    WRK-HKN-HKNID(IDX)
                                       TO  WRK-KCOMB-HKNID      (IDX2)
               MOVE    WRK-HKN-HKNNUM(IDX)
                                       TO  WRK-KCOMB-HKNNUM     (IDX2)
               IF      WRK-HKN-HKNJANUM(IDX) = "67330753"
               AND     WRK-HKN-HOJOKBN(IDX)  =  SPACE
               AND     WRK-HKN-TEKSTYMD(IDX) <  "20111001"
                 MOVE      9999        TO  WRK-KCOMB-HKNCOMBINUM(IDX2)
               END-IF
               MOVE    WRK-HKN-HONKZKKBN (IDX) 
                                       TO  WRK-KCOMB-HONKZKKBN  (IDX2)
               MOVE    WRK-HKN-HOJOKBN   (IDX)
                                       TO  WRK-KCOMB-HOJOKBN    (IDX2)
               MOVE    WRK-HKN-CONTKBN(IDX)
                                       TO  WRK-KCOMB-CONTKBN    (IDX2)
               MOVE    WRK-HKN-TEKSTYMD (IDX)
                                       TO  WRK-KCOMB-TEKSTYMD   (IDX2)
               MOVE    WRK-HKN-TEKEDYMD (IDX)
                                       TO  WRK-KCOMB-TEKEDYMD   (IDX2)
               MOVE    WRK-HKN-SEIDO(IDX)
                                 TO  WRK-KCOMB-SYU-TANSEIDONAME (IDX2)
      *        ------------------------------------
               MOVE    WRK-HKN-SBT  (IDX)
                                  TO  WRK-KCOMB-SYU-HKNKOHSBTKBN(IDX2)
      *
               IF ((WRK-HKN-HKNNUM (IDX)       =   "002")   AND
      *************(WRK-HKN-HOJOKBN(IDX)   NOT =   SPACE)
                   (WRK-HKN-HOJOKBN(IDX)       =   "1"  OR
                                                   "2"  OR
                                                   "3"  OR
                                                   "A"  OR
                                                   "B"  OR
                                                   "C"  OR
                                                   "D"  OR
                                                   "E"  OR
                                                   "F"  OR
                                                   "G"  OR
                                                   "H"  OR
                                                   "I" ) )   OR
                  ((WRK-HKN-HKNNUM (IDX)       =   "031" OR
                                                   "032" OR
                                                   "033" OR
                                                   "034" ) AND
                   (WRK-HKN-HOJOKBN(IDX)       =   "2"  OR
                                                   "B"  OR
                                                   "H"  OR
                                                   "E"   ))
                 ADD     1             TO  WRK-LCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                       TO  WRK-LCOMB-REC
                                                       (WRK-LCOMB-MAX)
                 MOVE    WRK-LCOMB-MAX TO  IDX2
                 PERFORM 57012-FUTAN-SET-SEC
               ELSE
      *        公費表中より老人を検索し、適用開始日順にインデックス取得
                 INITIALIZE                WRK-RCOMB-AREA
                 INITIALIZE                WRK-IDXSRT-AREA
                 MOVE    ZERO              TO  WRK-RCOMB-MAX
                 MOVE    "1"               TO  WRK-SRT-FLG
                 MOVE    WRK-HKN-TEKSTYMD (IDX)  TO  WRK-SRT-STYMD
                 MOVE    WRK-HKN-TEKEDYMD (IDX)  TO  WRK-SRT-EDYMD
                 PERFORM 570143-RJNIDX-SORT-SEC
      *
                 IF  WRK-SRT-RJN-CNT       >   ZERO
      *            [公費テーブル]より老人を検索
                   PERFORM     VARYING   IDZ   FROM  1   BY  1
                               UNTIL     IDZ   >     WRK-SRT-RJN-CNT
                     MOVE    WRK-SRT-RJN-IDX (IDZ) TO  WRK-COMB-RJNIDX
                     PERFORM 570142-RJN-HEN-SEC
                   END-PERFORM
      *            老人保険が終了しその後、主保険のみが存在する場合
                   IF  WRK-KCOMB-TEKSTYMD (IDX2)
                                     NOT =   WRK-KCOMB-TEKEDYMD (IDX2)
                       ADD     1         TO  WRK-RCOMB-MAX
                       MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC
                                                       (WRK-RCOMB-MAX)
                   END-IF
                 ELSE
                   ADD     1             TO  WRK-RCOMB-MAX
                   MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC
                                                       (WRK-RCOMB-MAX)
                 END-IF
      *         ------------------------------------------------------
      *         生活保護が存在しなおかつ負担区分有りの場合生活保護編集
      *         ------------------------------------------------------
      *          [編集後テーブル]を再度元テーブルに転送
                 INITIALIZE                  WRK-KCOMB-AREA
                 MOVE    ZERO            TO  WRK-KCOMB-MAX
                 PERFORM     VARYING   IDZ   FROM    1   BY  1
                             UNTIL   ( IDZ   >       200 )
                             OR      ( IDZ   >       WRK-RCOMB-MAX )
                     ADD     1           TO  WRK-KCOMB-MAX
                     MOVE    WRK-RCOMB-REC (IDZ)
                                         TO  WRK-KCOMB-REC
                                                     (WRK-KCOMB-MAX)
                 END-PERFORM
      *
                 INITIALIZE                WRK-RCOMB-AREA
                 MOVE    ZERO              TO  WRK-RCOMB-MAX
      *          再度元テーブルより生保　付加
                 PERFORM     VARYING   IDX2  FROM    1   BY  1
                             UNTIL   ( IDX2  >       200 )
                             OR      ( IDX2  >       WRK-KCOMB-MAX )
      *           公費表中より生保を検索し、適用開始日順に
      *                                インデックス取得
                   INITIALIZE                WRK-IDXSRT-AREA
                   IF (WRK-KCOMB-HKNNUM (IDX2)        =   "060"
                                                      OR  "039")
                   OR (WRK-KCOMB-KOH4HKNNUM(IDX2) NOT =   SPACE)
                     CONTINUE
                   ELSE
                     MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                             TO  WRK-SRT-STYMD
                     MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                             TO  WRK-SRT-EDYMD
                     PERFORM 570111-SEHIDX-SORT-SEC
                   END-IF
                   ADD     1           TO  WRK-RCOMB-MAX
                   MOVE    WRK-KCOMB-REC (IDX2)
                                       TO  WRK-RCOMB-REC
                                                 (WRK-RCOMB-MAX)
                   IF  WRK-SRT-SEH-CNT       >   ZERO
      *              生保を検索
                     PERFORM     VARYING   IDZ   FROM  1   BY  1
                                 UNTIL     IDZ   >     WRK-SRT-SEH-CNT
                       IF   WRK-RCOMB-MAX  >=  200
                         MOVE    200    TO  IDZ
                       ELSE
                         MOVE    WRK-SRT-SEH-IDX(IDZ)
                                        TO  WRK-COMB-IDX
                         PERFORM 570144-FUTAN-KONZAI-SEH-SEC
                       END-IF
                     END-PERFORM
                   END-IF
                 END-PERFORM
      *
                 INITIALIZE                        WRK-KCOMB-AREA
                 MOVE    ZERO                  TO  WRK-KCOMB-MAX
                 MOVE    WRK-LCOMB-MAX         TO  WRK-LCOMB-MAX-O
                 PERFORM     VARYING   IDZ   FROM    1   BY  1
                             UNTIL   ( IDZ   >       200 )
                             OR      ( IDZ   >       WRK-RCOMB-MAX )
                     ADD     1           TO  WRK-LCOMB-MAX
                     MOVE    WRK-RCOMB-REC (IDZ)
                                    TO  WRK-LCOMB-REC (WRK-LCOMB-MAX)
                     MOVE    WRK-LCOMB-MAX   TO  IDX2
                     PERFORM 57012-FUTAN-SET-SEC
      *              後に長期高額をセットするため再転送
                     ADD     1               TO  WRK-KCOMB-MAX
                     MOVE    WRK-LCOMB-REC (WRK-LCOMB-MAX)
                                   TO  WRK-KCOMB-REC (WRK-KCOMB-MAX)
                 END-PERFORM
      *         ------------------------------------------------------
      *          長期高額が存在する場合、長期高額編集
      *         ------------------------------------------------------
                 INITIALIZE                      WRK-RCOMB-AREA
                 MOVE    ZERO                TO  WRK-RCOMB-MAX
      *          再度元テーブルより生保　付加
                 PERFORM     VARYING   IDX2  FROM    1   BY  1
                             UNTIL   ( IDX2  >       200 )
                             OR      ( IDX2  >       WRK-KCOMB-MAX )
                   INITIALIZE                WRK-IDXSRT-AREA
      *
                   ADD     1                 TO  WRK-RCOMB-MAX
                   MOVE    WRK-KCOMB-REC (IDX2)
                                             TO  WRK-RCOMB-REC
                                                      (WRK-RCOMB-MAX)
                   IF  (WRK-KCOMB-KOH1-HKNKOHSBTKBN(IDX2)  =   "5") OR
                       (WRK-KCOMB-KOH2-HKNKOHSBTKBN(IDX2)  =   "5") OR
                       (WRK-KCOMB-KOH3-HKNKOHSBTKBN(IDX2)  =   "5") OR
                       (WRK-KCOMB-HKNNUM(IDX2)             = SPACE) OR
                       (WRK-KCOMB-KOH4HKNNUM(IDX2)     NOT = SPACE)
                       CONTINUE
                   ELSE
      *                公費表中より長期高額を検索し、
      *                              適用開始日順にインデックス取得
                       MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                             TO  WRK-SRT-STYMD
                       MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                             TO  WRK-SRT-EDYMD
                       PERFORM 570113-CHOKIIDX-SORT-SEC
                       IF  WRK-SRT-CHO-CNT       >   ZERO
      *                  長期高額を検索
                         PERFORM   VARYING IDZ   FROM  1   BY  1
                                   UNTIL   IDZ   >     WRK-SRT-CHO-CNT
                           IF   WRK-RCOMB-MAX  >=  200
                             MOVE    200    TO  IDZ
                           ELSE
                             MOVE    WRK-SRT-CHO-IDX(IDZ)
                                            TO  WRK-COMB-IDX
                             PERFORM 570144-FUTAN-KONZAI-SEH-SEC
                           END-IF
                         END-PERFORM
                       END-IF
                   END-IF
                 END-PERFORM
      *
                 MOVE    WRK-LCOMB-MAX-O      TO  WRK-LCOMB-MAX
                 PERFORM     VARYING   IDZ   FROM    1   BY  1
                             UNTIL   ( IDZ   >       200 )
                             OR      ( IDZ   >       WRK-RCOMB-MAX )
                     ADD     1           TO  WRK-LCOMB-MAX
                     MOVE    WRK-RCOMB-REC (IDZ)
                                    TO  WRK-LCOMB-REC (WRK-LCOMB-MAX)
                     MOVE    WRK-LCOMB-MAX    TO  IDX2
                     PERFORM 57012-FUTAN-SET-SEC
                 END-PERFORM
               END-IF
      *
               PERFORM     57010-NC01PARA-OUT-SEC
      *
             END-IF
           END-PERFORM
          .
       57011-SYUHKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費付加　公費１〜４入替処理
      *****************************************************************
       5701121-KOH-IREKAE-SEC       SECTION.
      *
           IF  WRK-RCOMB-KOH1HKNNUM  (IDX1)   =   SPACE
              MOVE    WRK-KOH-KOHID         (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH1ID      (IDX1)
              MOVE    WRK-KOH-HKNNUM         (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH1HKNNUM   (IDX1)
              MOVE    WRK-KOH-PAYKBN         (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH1PAYKBN   (IDX1)
              MOVE    WRK-KOH-SEIDO     (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
              MOVE    WRK-KOH-SBT       (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
           ELSE
              IF  WRK-RCOMB-KOH2HKNNUM (IDX1)  =   SPACE
                  MOVE    WRK-KOH-KOHID     (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH2ID      (IDX1)
                  MOVE    WRK-KOH-HKNNUM     (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH2HKNNUM   (IDX1)
                  MOVE    WRK-KOH-PAYKBN     (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH2PAYKBN   (IDX1)
                  MOVE    WRK-KOH-SEIDO (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH2-TANSEIDONAME(IDX1)
                  MOVE    WRK-KOH-SBT   (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH2-HKNKOHSBTKBN(IDX1)
              ELSE
                  IF  WRK-RCOMB-KOH3HKNNUM (IDX1)  =   SPACE
                      MOVE    WRK-KOH-KOHID     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH3ID      (IDX1)
                      MOVE    WRK-KOH-HKNNUM     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH3HKNNUM   (IDX1)
                      MOVE    WRK-KOH-PAYKBN     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH3PAYKBN   (IDX1)
                      MOVE    WRK-KOH-SEIDO (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH3-TANSEIDONAME(IDX1)
                      MOVE    WRK-KOH-SBT   (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH3-HKNKOHSBTKBN(IDX1)
                  ELSE
                    IF  WRK-RCOMB-KOH4HKNNUM (IDX1)  =   SPACE
                      MOVE    WRK-KOH-KOHID     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH4ID      (IDX1)
                      MOVE    WRK-KOH-HKNNUM     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH4HKNNUM   (IDX1)
                      MOVE    WRK-KOH-PAYKBN     (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-KOH4PAYKBN   (IDX1)
                      MOVE    WRK-KOH-SEIDO (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH4-TANSEIDONAME(IDX1)
                      MOVE    WRK-KOH-SBT   (WRK-COMB-IDX)
                                  TO  WRK-RCOMB-KOH4-HKNKOHSBTKBN(IDX1)
                    ELSE
                      DISPLAY "IREKAE NASI"
                    END-IF
                  END-IF
              END-IF
           END-IF
           .
       5701121-KOH-IREKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助公費インデックスＳＯＲＴ処理
      *****************************************************************
       5701412-HJOIDX-SORT-SEC       SECTION.
      *
      *    公費表中より補助公費を適用開始日の早い順にインデックス設定
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       CNT-KOH )
      *
      *----(01.00.04) LINE MOD START -------------------------------
      *         IF  ( WRK-COMB-SYUKOHIDX    NOT =   IDZ  )      AND
      *             ( WRK-KOH-SAKJOKBN  (IDZ)   =   SPACE)      AND
      *             ( WRK-KOH-SBT       (IDZ)   =   "7")        AND
               IF  ( WRK-COMB-SYUKOHIDX    NOT =   IDZ        ) AND
                   ( WRK-KOH-SAKJOKBN  (IDZ)   =   SPACE      ) AND
                   ( WRK-KOH-SBT       (IDZ)   =   "7" OR "A" ) AND
      *----(01.00.04) LINE MOD END   -------------------------------
                   ((WRK-KOH-HKNKOHKBN (IDZ)   =   "2" OR "3") OR
                    (WRK-KOH-HKNNUM(IDZ)   =  "019" OR "053"
                                           OR "091" OR "051" ))
      ****         ( WRK-KOH-HKNKOHKBN (IDZ)   =   "2" OR "3") AND
      ****         ((WRK-KOH-GENHKNNUM1(IDZ)   =   SPACE)  OR
      ****          (WRK-KOH-GENHKNNUM1(IDZ)
      ****                                 =   WRK-SRT-HJO-KOHNO) OR
      ****          (WRK-KOH-GENHKNNUM2(IDZ)
      ****                                 =   WRK-SRT-HJO-KOHNO) OR
      ****          (WRK-KOH-GENHKNNUM3(IDZ)
      ****                                 =   WRK-SRT-HJO-KOHNO))
      *          [補助公費]適用年月日と[組合せ]適用年月日 
                 IF  (WRK-KOH-TEKEDYMD(IDZ)    <   WRK-SRT-STYMD) OR
                     (WRK-KOH-TEKSTYMD(IDZ)    >   WRK-SRT-EDYMD)
                       CONTINUE
                 ELSE
                   IF  WRK-SRT-HJO-CNT     =   ZERO
                       ADD     1           TO  WRK-SRT-HJO-CNT
                       MOVE    IDZ         TO  WRK-SRT-HJO-IDX (1)
                   ELSE
                     MOVE    ZERO          TO  WRK-SRT-HJO-CNT2
                     PERFORM   VARYING   IDY   FROM  1   BY  1
                               UNTIL     IDY   >   WRK-SRT-HJO-CNT
      *
                       MOVE    WRK-SRT-HJO-IDX (IDY)   TO  IDX1
                       IF  WRK-KOH-TEKSTYMD(IDZ)
                                         <   WRK-KOH-TEKSTYMD(IDX1)
                         PERFORM   VARYING   WRK-SRT-IDX
                                   FROM      WRK-SRT-HJO-CNT   BY  -1
                                   UNTIL     WRK-SRT-IDX   <   IDY
                           COMPUTE WRK-SRT-HJO-CNT2    =   WRK-SRT-IDX
                                                       +   1
                           MOVE    WRK-SRT-HJO-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-HJO-IDX
                                                   (WRK-SRT-HJO-CNT2)
                         END-PERFORM
                         MOVE    IDZ       TO  WRK-SRT-HJO-IDX (IDY)
                         MOVE    200       TO  IDY
                       END-IF
                     END-PERFORM
                     ADD     1             TO  WRK-SRT-HJO-CNT
                     IF  WRK-SRT-HJO-CNT2  =   ZERO
                         IF      WRK-SRT-HJO-CNT    >   200
                             MOVE    "0015"         TO  SPA-ERRCD
                         ELSE
                             MOVE    IDZ       TO  WRK-SRT-HJO-IDX
                                                   (WRK-SRT-HJO-CNT)
                         END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
           .
       5701412-HJOIDX-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保インデックスＳＯＲＴ処理
      *****************************************************************
       570111-SEHIDX-SORT-SEC       SECTION.
      *
      *    公費表中より生保を適用開始日の早い順にインデックス設定
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200     )
                       OR      ( IDZ   >       CNT-KOH )
      *
             IF  (WRK-KOH-SBT(IDZ)         =   "5"  )  AND
                 (WRK-KOH-SAKJOKBN (IDZ)   =   SPACE)
      *
      *        [生保]適用年月日と[組合せ]適用年月日
               IF  (WRK-KOH-TEKEDYMD(IDZ)  <   WRK-SRT-STYMD)  OR
                   (WRK-KOH-TEKSTYMD(IDZ)  >   WRK-SRT-EDYMD)
                   CONTINUE
               ELSE
                 IF  WRK-SRT-SEH-CNT       =   ZERO
                     ADD     1             TO  WRK-SRT-SEH-CNT
                     MOVE    IDZ           TO  WRK-SRT-SEH-IDX (1)
                 ELSE
                   MOVE    ZERO            TO  WRK-SRT-SEH-CNT2
                   PERFORM   VARYING   IDY   FROM  1   BY  1
                             UNTIL     IDY   >   WRK-SRT-SEH-CNT
      *
                     MOVE    WRK-SRT-SEH-IDX (IDY)   TO  IDX1
                     IF  WRK-KOH-TEKSTYMD(IDZ)
                                       <   WRK-KOH-TEKSTYMD(IDX1)
                       PERFORM   VARYING   WRK-SRT-IDX
                                 FROM      WRK-SRT-SEH-CNT   BY  -1
                                 UNTIL     WRK-SRT-IDX   <   IDY
                         COMPUTE WRK-SRT-SEH-CNT2    =   WRK-SRT-IDX
                                                     +   1
                         MOVE    WRK-SRT-SEH-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-SEH-IDX
                                                   (WRK-SRT-SEH-CNT2)
                       END-PERFORM
                       MOVE    IDZ         TO  WRK-SRT-SEH-IDX (IDY)
                       MOVE    200         TO  IDY
                     END-IF
                   END-PERFORM
                   ADD     1               TO  WRK-SRT-SEH-CNT
                   IF  WRK-SRT-SEH-CNT2    =   ZERO
                       IF      WRK-SRT-SEH-CNT    >   200
                           MOVE    "0015"            TO  SPA-ERRCD
                       ELSE
                           MOVE    IDZ         TO  WRK-SRT-SEH-IDX
                                                   (WRK-SRT-SEH-CNT)
                       END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-PERFORM
      *
           .
       570111-SEHIDX-SORT-EXT.
           EXIT.
      *****************************************************************
      *    長期高額インデックスＳＯＲＴ処理
      *****************************************************************
       570113-CHOKIIDX-SORT-SEC       SECTION.
      *
      *    公費表中より長期高額を適用開始日の早い順にインデックス設定
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       CNT-KOH )
      *
             IF  (WRK-KOH-SBT (IDZ)    =   "2"  )  AND
                 (WRK-KOH-SAKJOKBN (IDZ)    =   SPACE)
      *
      *        [長期高額]適用年月日と[組合せ]適用年月日
               IF  (WRK-KOH-TEKEDYMD(IDZ)   <   WRK-SRT-STYMD)  OR
                   (WRK-KOH-TEKSTYMD(IDZ)   >   WRK-SRT-EDYMD)
                   CONTINUE
               ELSE
                 IF  WRK-SRT-CHO-CNT     =   ZERO
                     ADD     1           TO  WRK-SRT-CHO-CNT
                     MOVE    IDZ         TO  WRK-SRT-CHO-IDX (1)
                 ELSE
                   MOVE    ZERO          TO  WRK-SRT-CHO-CNT2
                   PERFORM   VARYING   IDY   FROM  1   BY  1
                             UNTIL     IDY   >   WRK-SRT-CHO-CNT
      *
                     MOVE    WRK-SRT-CHO-IDX (IDY)   TO  IDX1
                     IF  WRK-KOH-TEKSTYMD(IDZ)
                                       <   WRK-KOH-TEKSTYMD(IDX1)
                       PERFORM   VARYING   WRK-SRT-IDX
                                 FROM      WRK-SRT-CHO-CNT   BY  -1
                                 UNTIL     WRK-SRT-IDX   <   IDY
                         COMPUTE WRK-SRT-CHO-CNT2    =   WRK-SRT-IDX
                                                     +   1
                         MOVE    WRK-SRT-CHO-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-CHO-IDX
                                                   (WRK-SRT-CHO-CNT2)
                       END-PERFORM
                       MOVE    IDZ         TO  WRK-SRT-CHO-IDX (IDY)
                       MOVE    200         TO  IDY
                     END-IF
                   END-PERFORM
                   ADD     1               TO  WRK-SRT-CHO-CNT
                   IF  WRK-SRT-CHO-CNT2    =   ZERO
                       IF      WRK-SRT-CHO-CNT    >   200
                           MOVE    "0015"         TO  SPA-ERRCD
                       ELSE
                           MOVE    IDZ         TO  WRK-SRT-CHO-IDX
                                                   (WRK-SRT-CHO-CNT)
                       END-IF
                   END-IF
                 END-IF
               END-IF
             END-IF
           END-PERFORM
          .
       570113-CHOKIIDX-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者負担内容セット処理
      *****************************************************************
       57012-FUTAN-SET-SEC         SECTION.
      *
      *   [船員保険]でかつ[補助区分]が入力されていた場合
      *     <負担区分＝２>で各内容を初期化
           IF  ((WRK-LCOMB-HKNNUM  (IDX2)      =   "002")  AND
      **********(WRK-LCOMB-HOJOKBN (IDX2)  NOT =   SPACE))
                (WRK-LCOMB-HOJOKBN (IDX2)      =   "1" OR
                                                   "2" OR
                                                   "3"  OR
                                                   "A"  OR
                                                   "B"  OR
                                                   "C"  OR
                                                   "D"  OR
                                                   "E"  OR
                                                   "F"  OR
                                                   "G"  OR
                                                   "H"  OR
                                                   "I" ) )   OR
                  ((WRK-LCOMB-HKNNUM (IDX2)    =   "031" OR
                                                   "032" OR
                                                   "033" OR
                                                   "034" ) AND
                   (WRK-LCOMB-HOJOKBN(IDX2)    =   "2"  OR
                                                   "B"  OR
                                                   "H"  OR
                                                   "E"   ))
            INITIALIZE                         WRK-KENSAKU-AREA
            MOVE    WRK-LCOMB-HKNNUM  (IDX2)   TO  WRK-KEN-HKNNUM
      *----(01.00.01) LINE MOD START ---------------------------------
      *      MOVE    WRK-LCOMB-TEKSTYMD(IDX2)   TO  WRK-KEN-TEKYMD
            MOVE    WRK-LCOMB-TEKSTYMD(IDX2)   TO  WRK-KEN-TEKSTYMD
            MOVE    WRK-LCOMB-TEKEDYMD(IDX2)   TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
            MOVE    WRK-LCOMB-SYU-HKNKOHSBTKBN(IDX2)
                                               TO  WRK-KEN-SBTKBN
            PERFORM 4410-HKNKOH-KAKUNIN-SEC
            IF  WRK-KEN-ERFLG       NOT =   SPACE
                MOVE    "0014"          TO  SPA-ERRCD
                GO  TO                  57012-FUTAN-SET-EXT
            END-IF
            MOVE    HKN-TENTNK  TO  WRK-LCOMB-TENTNK          (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-TAXKBN          (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-GAI-FTNKBN      (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-NYU-FTNKBN      (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-TSY-GAIFTNKBN   (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-TSY-NYUFTNKBN   (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-TNK-GAIFTNKBN   (IDX2)
            MOVE    "2"         TO  WRK-LCOMB-TNK-NYUFTNKBN   (IDX2)
            MOVE    "3"         TO  WRK-LCOMB-GAI-YKZFTNKBN   (IDX2)
            MOVE    "3"         TO  WRK-LCOMB-NYU-SKJRYOKBN   (IDX2)
            MOVE    "3"         TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
            MOVE    "3"         TO  WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
            MOVE    "3"         TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
            MOVE    "3"         TO  WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
            INITIALIZE              WRK-LCOMB-GAI-NAIYO       (IDX2)
            INITIALIZE              WRK-LCOMB-NYU-NAIYO       (IDX2)
            INITIALIZE              WRK-LCOMB-TSY-GAI-NAIYO   (IDX2)
            INITIALIZE              WRK-LCOMB-TSY-NYU-NAIYO   (IDX2)
            INITIALIZE              WRK-LCOMB-TNK-GAI-NAIYO   (IDX2)
            INITIALIZE              WRK-LCOMB-TNK-NYU-NAIYO   (IDX2)
            GO  TO                  57012-FUTAN-SET-EXT
           END-IF
      *
      *   関連する保険番号マスタを検索し、退避
           MOVE    SPACE            TO  WRK-COMB-TENSO
           PERFORM    VARYING       IDA    FROM    1   BY  1
                      UNTIL   (IDA            >   5)
                      OR      (SPA-ERRCD  NOT =   SPACE)
                      OR      (WRK-LCOMB-GAI-FTNKBN(IDX2)  =   "0"
                        AND    WRK-LCOMB-NYU-FTNKBN(IDX2)  =   "0"
                        AND    WRK-LCOMB-TSY-GAIFTNKBN(IDX2) =  "0"
                        AND    WRK-LCOMB-TSY-NYUFTNKBN(IDX2) =  "0"
                        AND    WRK-LCOMB-TNK-GAIFTNKBN(IDX2) =  "0"
                        AND    WRK-LCOMB-TNK-NYUFTNKBN(IDX2) =  "0")
            INITIALIZE                 WRK-KENSAKU-AREA
            EVALUATE    IDA
      *     // 保険 //
            WHEN    1
              IF  WRK-LCOMB-HKNNUM (IDX2)      NOT =   SPACE
                IF  (WRK-LCOMB-HKNNUM (IDX2)       =   "060")   AND
                    (WRK-LCOMB-HOJOKBN(IDX2)       =   "0"  )
                  MOVE    WRK-LCOMB-HKNNUM  (IDX2) TO  WRK-KEN-HKNNUM
      *----(01.00.01) LINE MOD START ---------------------------------
      *            MOVE    WRK-LCOMB-TEKSTYMD(IDX2) TO  WRK-KEN-TEKYMD
                  MOVE    WRK-LCOMB-TEKSTYMD(IDX2) TO  WRK-KEN-TEKSTYMD
                  MOVE    WRK-LCOMB-TEKEDYMD(IDX2) TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                  MOVE    WRK-LCOMB-SYU-HKNKOHSBTKBN(IDX2)
                                                   TO  WRK-KEN-SBTKBN
                ELSE
                  IF  WRK-LCOMB-KOH1-HKNKOHSBTKBN(IDX2)  NOT =   "3"
                      MOVE  WRK-LCOMB-HKNNUM  (IDX2) TO  WRK-KEN-HKNNUM
      *----(01.00.01) LINE MOD START ---------------------------------
      *                MOVE  WRK-LCOMB-TEKSTYMD(IDX2) TO WRK-KEN-TEKYMD
                      MOVE  WRK-LCOMB-TEKSTYMD(IDX2)
                                                  TO  WRK-KEN-TEKSTYMD
                      MOVE  WRK-LCOMB-TEKEDYMD(IDX2)
                                                  TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                      MOVE  WRK-LCOMB-SYU-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
                  END-IF
                END-IF
              END-IF
      *     //  公費１ //
            WHEN    2
              IF  WRK-LCOMB-KOH1HKNNUM(IDX2)    NOT =   SPACE
                IF  WRK-LCOMB-KOH1-HKNKOHSBTKBN(IDX2)   =   "3"
                  IF  (WRK-LCOMB-HKNNUM (IDX2)       =   "060")   AND
                      (WRK-LCOMB-HOJOKBN(IDX2)       =   "0"  )
                    MOVE  "3" TO  WRK-LCOMB-GAI-YKZFTNKBN   (IDX2)
                    MOVE  "3" TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
                    MOVE  "3" TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
                  ELSE
                    MOVE  WRK-LCOMB-KOH1HKNNUM(IDX2) TO  WRK-KEN-HKNNUM
                    MOVE  WRK-LCOMB-KOH1PAYKBN(IDX2) TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *              MOVE  WRK-LCOMB-TEKSTYMD(IDX2)  TO  WRK-KEN-TEKYMD
                    MOVE  WRK-LCOMB-TEKSTYMD(IDX2)  TO WRK-KEN-TEKSTYMD
                    MOVE  WRK-LCOMB-TEKEDYMD(IDX2)  TO WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                    MOVE  WRK-LCOMB-KOH1-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
                  END-IF
                ELSE
                  MOVE  WRK-LCOMB-KOH1HKNNUM(IDX2)   TO  WRK-KEN-HKNNUM
                  MOVE  WRK-LCOMB-KOH1PAYKBN(IDX2)   TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *            MOVE  WRK-LCOMB-TEKSTYMD  (IDX2)  TO  WRK-KEN-TEKYMD
                  MOVE  WRK-LCOMB-TEKSTYMD (IDX2)  TO  WRK-KEN-TEKSTYMD
                  MOVE  WRK-LCOMB-TEKEDYMD (IDX2)  TO  WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                  MOVE  WRK-LCOMB-KOH1-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
                END-IF
              END-IF
      *     //  公費２ //
            WHEN    3
              IF  WRK-LCOMB-KOH2HKNNUM(IDX2)     NOT =   SPACE
                  MOVE    WRK-LCOMB-KOH2HKNNUM(IDX2) TO  WRK-KEN-HKNNUM
                  MOVE    WRK-LCOMB-KOH2PAYKBN(IDX2) TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *            MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO  WRK-KEN-TEKYMD
                  MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO WRK-KEN-TEKSTYMD
                  MOVE    WRK-LCOMB-TEKEDYMD (IDX2) TO WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                  MOVE    WRK-LCOMB-KOH2-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
              END-IF
      *     //  公費３ //
            WHEN    4
              IF  WRK-LCOMB-KOH3HKNNUM(IDX2)     NOT =   SPACE
                  MOVE    WRK-LCOMB-KOH3HKNNUM(IDX2) TO  WRK-KEN-HKNNUM
                  MOVE    WRK-LCOMB-KOH3PAYKBN(IDX2) TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *            MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO  WRK-KEN-TEKYMD
                  MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO WRK-KEN-TEKSTYMD
                  MOVE    WRK-LCOMB-TEKEDYMD (IDX2) TO WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                  MOVE    WRK-LCOMB-KOH3-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
              END-IF
      *     //  公費４ //
            WHEN    5
              IF  WRK-LCOMB-KOH4HKNNUM(IDX2)     NOT =   SPACE
                  MOVE    WRK-LCOMB-KOH4HKNNUM(IDX2) TO  WRK-KEN-HKNNUM
                  MOVE    WRK-LCOMB-KOH4PAYKBN(IDX2) TO  WRK-KEN-PAYKBN
      *----(01.00.01) LINE MOD START ---------------------------------
      *            MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO  WRK-KEN-TEKYMD
                  MOVE    WRK-LCOMB-TEKSTYMD (IDX2) TO WRK-KEN-TEKSTYMD
                  MOVE    WRK-LCOMB-TEKEDYMD (IDX2) TO WRK-KEN-TEKEDYMD
      *----(01.00.01) LINE MOD END   ---------------------------------
                  MOVE    WRK-LCOMB-KOH4-HKNKOHSBTKBN(IDX2)
                                                     TO  WRK-KEN-SBTKBN
              END-IF
            END-EVALUATE
      *
            IF  WRK-KEN-HKNNUM              NOT =   SPACE
                PERFORM 4410-HKNKOH-KAKUNIN-SEC
              IF  WRK-KEN-ERFLG            NOT =   SPACE
                  MOVE    "0014"               TO  SPA-ERRCD
                  GO  TO                       57012-FUTAN-SET-EXT
              ELSE
                IF  WRK-COMB-TENSO             =   SPACE
      *           == 1件目 ==
                  MOVE    "1"                  TO  WRK-COMB-TENSO
                  IF  IDA                      =   1
                      IF  WRK-LCOMB-HKNCOMBINUM(IDX2) = 9999
                          MOVE    20       TO  HKN-HON-GAIKFTNRATE
                                               HKN-HON-NYUKFTNRATE
                                               HKN-KZK-GAIKFTNRATE
                                               HKN-KZK-NYUKFTNRATE
                      END-IF
                      IF  WRK-LCOMB-HONKZKKBN (IDX2) NOT =   "2"
      *                   <本人>
                          MOVE    HKN-HON  TO  WRK-LCOMB-GAINYU(IDX2)
                          MOVE    HKN-HON  TO  WRK-LCOMB-TSY   (IDX2)
                          MOVE    HKN-HON  TO  WRK-LCOMB-TNK   (IDX2)
                      ELSE
      *                   <家族>
                          MOVE    HKN-KZK  TO  WRK-LCOMB-GAINYU(IDX2)
                          MOVE    HKN-KZK  TO  WRK-LCOMB-TSY   (IDX2)
                          MOVE    HKN-KZK  TO  WRK-LCOMB-TNK   (IDX2)
                      END-IF
                      IF  HKN-HKNNUM               =   "060"
                                                   OR  "039"
      *----(02.00.0) LINE UPD START -------------------------------
      *                   国保の保険割合を補助区分より編集
                          PERFORM 570120-KOKUHO-SET-SEC
      *                   国保の保険割合
                          IF      WRK-LCOMB-TEKSTYMD (IDX2)
                                               >=   "20021001"
                              MOVE    WRK-LCOMB-GAI-KFTNRATE(IDX2)
                                               TO  WRK-GAIKFTNRATE
                          ELSE
                              MOVE    ZERO     TO  WRK-GAIKFTNRATE
                          END-IF
                      ELSE
      *----(02.00.0) LINE UPD END   -------------------------------
                          IF  HKN-HKNNUM               =   "068"
                                                       OR  "069"
                                                       OR  "040"
      *                       特別療養費対象外
                              CONTINUE
                          ELSE
      *                   老人の負担割合編集
                          PERFORM 570129-RJN-FUTAN-SET-SEC
                          END-IF
                      END-IF
                  ELSE
      *               本人
                      MOVE    HKN-HON    TO  WRK-LCOMB-GAINYU  (IDX2)
                      MOVE    HKN-TSY    TO  WRK-LCOMB-TSY  (IDX2)
                      MOVE    HKN-TNK    TO  WRK-LCOMB-TNK  (IDX2)
                  END-IF
      *           [点数単価]
                  MOVE    HKN-TENTNK     TO  WRK-LCOMB-TENTNK(IDX2)
      *           [消費税区分]
                  IF    HKN-HKNNUM (1:2)        =   "98"
      *               自費
                      MOVE    WRK-LCOMB-HOJOKBN (IDX2)
                                  TO  WRK-LCOMB-TAXKBN (IDX2)
                  ELSE
      *               自費以外
                      MOVE    "2"        TO  WRK-LCOMB-TAXKBN (IDX2)
                  END-IF
                ELSE
      *           == 2件目以降 == 保険マスタを比較し、負担区分を転送
      *           [通常]本人
                  IF  WRK-LCOMB-GAI-FTNKBN(IDX2)      NOT =   "0"
                      IF  (HKN-HON-GAIFTNKBN          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-HON-GAIFTNKBN
                                         TO  WRK-LCOMB-GAI-FTNKBN(IDX2)
                      END-IF
                  END-IF
                  IF  WRK-LCOMB-NYU-FTNKBN(IDX2)      NOT =   "0"
                      IF  (HKN-HON-NYUFTNKGN          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-HON-NYUFTNKGN
                                         TO  WRK-LCOMB-NYU-FTNKBN(IDX2)
                      END-IF
                  END-IF
      *           [低所得]
                  IF  WRK-LCOMB-TSY-GAIFTNKBN(IDX2)   NOT =   "0"
                      IF  (HKN-TSY-GAIFTNKBN          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-TSY-GAIFTNKBN
                                 TO  WRK-LCOMB-TSY-GAIFTNKBN(IDX2)
                      END-IF
                  END-IF
                  IF  WRK-LCOMB-TSY-NYUFTNKBN(IDX2)   NOT =   "0"
                      IF  (HKN-TSY-NYUFTNNBK          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-TSY-NYUFTNNBK
                              TO  WRK-LCOMB-TSY-NYUFTNKBN(IDX2)
                      END-IF
                  END-IF
      *           [低年金]
                  IF  WRK-LCOMB-TNK-GAIFTNKBN(IDX2)   NOT =   "0"
                      IF  (HKN-TNK-GAIFTNKBN          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-TNK-GAIFTNKBN
                              TO  WRK-LCOMB-TNK-GAIFTNKBN(IDX2)
                      END-IF
                  END-IF
                  IF  WRK-LCOMB-TNK-NYUFTNKBN(IDX2)   NOT =   "0"
                      IF  (HKN-TNK-NYUFTNKBN          =   "0"  OR  "2")
                      AND (WRK-LCOMB-DELKBN(IDX2) NOT =   "9"         )
                          MOVE    HKN-TNK-NYUFTNKBN
                              TO  WRK-LCOMB-TNK-NYUFTNKBN(IDX2)
                      END-IF
                  END-IF
      *           [点数単価]
                  IF  HKN-TENTNK         <   WRK-LCOMB-TENTNK (IDX2)
                      MOVE    HKN-TENTNK TO  WRK-LCOMB-TENTNK (IDX2)
                  END-IF
                END-IF
      *
      *         負担区分＝０ OR ２の場合、各内容を初期化
      *         負担区分＝１（患者負担有り）の場合、保険番号マスタと
      *                 SPA退避領域の内容を比較後、小さい値の方をセット
                IF  IDA                  =   1
                    PERFORM 570121-FUTAN-IREKAE-HKN-SEC
                ELSE
                    PERFORM 570121-FUTAN-IREKAE-SEC
                END-IF
      *
      *    　　 薬剤負担区分・食事療養費負担区分入替(転送)処理
                PERFORM 570122-YKZ-SKJ-IREKAE-SEC
      *
      *         各上限区分について　日・月の上限回数
      *                            or上限額=ZEROならば"1"セット
                PERFORM 570123-JOGKBN-SET-SEC
              END-IF
            END-IF
           END-PERFORM
      *
           .
       57012-FUTAN-SET-EXT.
           EXIT.
      *
      *----(02.00.0) LINE ADD START ---------------------------------
      *****************************************************************
      *    国保の保険割合を補助区分より編集処理
      *****************************************************************
       570120-KOKUHO-SET-SEC        SECTION.
      *
           EVALUATE    WRK-LCOMB-HOJOKBN (IDX2)
               WHEN    "0"
                   MOVE  "2"       TO  WRK-LCOMB-GAI-FTNKBN (IDX2)
                   MOVE  "2"       TO  WRK-LCOMB-NYU-FTNKBN (IDX2)
                   MOVE  "2"       TO  WRK-LCOMB-TSY-GAIFTNKBN(IDX2)
                   MOVE  "2"       TO  WRK-LCOMB-TSY-NYUFTNKBN(IDX2)
                   MOVE  "2"       TO  WRK-LCOMB-TNK-GAIFTNKBN(IDX2)
                   MOVE  "2"       TO  WRK-LCOMB-TNK-NYUFTNKBN(IDX2)
               WHEN    "1"
               WHEN    "2"
               WHEN    "3"
                   MOVE    WRK-LCOMB-HOJOKBN(IDX2)
                                              TO  WRK-DISCOUNTRATEC
                   COMPUTE   WRK-DISCOUNTRATEC
                                   =   WRK-DISCOUNTRATEC *   10
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-GAI-KFTNRATE(IDX2)
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-NYU-KFTNRATE(IDX2)
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
                   MOVE    WRK-DISCOUNTRATEC
                                   TO   WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
               WHEN    "4"
      *            外来１割、入院０割
                   MOVE    10      TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
                   MOVE    ZERO    TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
                   MOVE    10      TO  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
                   MOVE    ZERO    TO  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
                   MOVE    10      TO  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
                   MOVE    ZERO    TO  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
                   MOVE    "2"     TO  WRK-LCOMB-NYU-FTNKBN (IDX2)
                   MOVE    "2"     TO  WRK-LCOMB-TSY-NYUFTNKBN(IDX2)
                   MOVE    "2"     TO  WRK-LCOMB-TNK-NYUFTNKBN(IDX2)
               WHEN    "5"
      *            外来２割、入院１割
                   MOVE    20      TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
                   MOVE    10      TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
                   MOVE    20      TO  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
                   MOVE    10      TO  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
                   MOVE    20      TO  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
                   MOVE    10      TO  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
               WHEN    "6"
      *            外来３割、入院２割
                   MOVE    30      TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
                   MOVE    20      TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
                   MOVE    30      TO  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
                   MOVE    20      TO  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
                   MOVE    30      TO  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
                   MOVE    20      TO  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
           END-EVALUATE
           .
       570120-KOKUHO-SET-EXT.
           EXIT.
      *----(02.00.0) LINE ADD END ---------------------------------
      *
      *****************************************************************
      *    老人の負担割合編集処理
      *****************************************************************
       570129-RJN-FUTAN-SET-SEC        SECTION.
      *    Ｈ１４．１０改正 老人の時給付割合が補助区分にある
           IF      WRK-LCOMB-TEKSTYMD (IDX2)     >=   "20021001"
               EVALUATE    WRK-LCOMB-HOJOKBN (IDX2)
                   WHEN    "8"
      *                ２割負担
                       MOVE    20                TO  WRK-GAIKFTNRATE
                   WHEN    "9"
      *                １割負担
                       MOVE    10                TO  WRK-GAIKFTNRATE
      *            Ｈ１８．１０改正 追加
                   WHEN    "7"
      *                ３割負担
                       MOVE    30                TO  WRK-GAIKFTNRATE
                   WHEN    OTHER
      *                その他
                       MOVE    ZERO              TO  WRK-GAIKFTNRATE
               END-EVALUATE
               IF      WRK-GAIKFTNRATE     NOT =   ZERO
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-GAI-KFTNRATE
                                                              (IDX2)
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-NYU-KFTNRATE
                                                              (IDX2)
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-TSY-GAIKFTNRATE
                                                              (IDX2)
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-TSY-NYUKFTNRATE
                                                              (IDX2)
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-TNK-GAIKFTNRATE
                                                              (IDX2)
                   MOVE    WRK-GAIKFTNRATE TO  WRK-LCOMB-TNK-NYUKFTNRATE
                                                              (IDX2)
               END-IF
           ELSE
               MOVE    ZERO                TO  WRK-GAIKFTNRATE
           END-IF
      *
           .
       570129-RJN-FUTAN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来負担内容入れ替え処理
      *         負担区分＝０ OR ２の場合、各内容を初期化
      *****************************************************************
       570121-FUTAN-IREKAE-HKN-SEC        SECTION.
      *
      *    [通常] (外来)
           IF  WRK-LCOMB-GAI-FTNKBN (IDX2)   =   "0"  OR  "2"
               INITIALIZE           WRK-LCOMB-GAI-NAIYO (IDX2)
           END-IF
      *    [通常] (入院)
           IF  WRK-LCOMB-NYU-FTNKBN (IDX2)   =   "0"  OR  "2"
               INITIALIZE            WRK-LCOMB-NYU-NAIYO (IDX2)
           END-IF
      *    [低所得] (外来)
           IF  WRK-LCOMB-TSY-GAIFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TSY-GAI-NAIYO (IDX2)
           END-IF
      *    [低所得] (入院)
           IF  WRK-LCOMB-TSY-NYUFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TSY-NYU-NAIYO (IDX2)
           END-IF
      *    [低年金] (外来)
           IF  WRK-LCOMB-TNK-GAIFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TNK-GAI-NAIYO (IDX2)
           END-IF
      *    [低年金] (入院)
           IF  WRK-LCOMB-TNK-NYUFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TNK-NYU-NAIYO (IDX2)
           END-IF
      *
           .
       570121-FUTAN-IREKAE-HKN-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来負担内容入れ替え処理
      *         負担区分＝０ OR ２の場合、各内容を初期化
      *         負担区分＝１（患者負担有り）の場合、保険番号マスタと
      *                 SPA退避領域の内容を比較後、小さい値の方をセット
      *****************************************************************
       570121-FUTAN-IREKAE-SEC        SECTION.
      *
      *    [通常] (外来)
           IF  WRK-LCOMB-GAI-FTNKBN (IDX2)       =   "0"  OR  "2"
               INITIALIZE           WRK-LCOMB-GAI-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
      *      Ｈ１４．１０改正 老人の時給付割合が補助区分にある
      *      IF      WRK-LCOMB-TEKSTYMD (IDX2)     >=   "20021001"
      *          EVALUATE    WRK-LCOMB-HOJOKBN (IDX2)
      *              WHEN    "8"
      *                  ２割負担
      *                  MOVE    20                TO  WRK-GAIKFTNRATE
      *              WHEN    "9"
      *                  １割負担
      *                  MOVE    10                TO  WRK-GAIKFTNRATE
      *              WHEN    OTHER
      *                  その他
      *                  MOVE    HKN-HON-GAIKFTNRATE
      *                                            TO  WRK-GAIKFTNRATE
      *          END-EVALUATE
      *******ELSE
      *         MOVE    HKN-HON-GAIKFTNRATE    TO  WRK-GAIKFTNRATE
      *******END-IF
      *
      *   <回−負担割合>
      *      老人公費の時、老人公費の割合を対象とする
             IF      WRK-KEN-HKNNUM            =   "027"
                 IF      WRK-GAIKFTNRATE   NOT =   ZERO
                     MOVE    HKN-HON-GAIKFTNRATE
                                           TO  WRK-LCOMB-GAI-KFTNRATE
                                                                  (IDX2)
                 END-IF
             END-IF
      *
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-GAI-KFTNRATE(IDX2)      =   ZERO
                MOVE    HKN-HON-GAIKFTNRATE
                        TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
              ELSE
                IF  (HKN-HON-GAIKFTNRATE     >   ZERO )    AND
                    (HKN-HON-GAIKFTNRATE
                         <   WRK-LCOMB-GAI-KFTNRATE(IDX2))
                  MOVE  HKN-HON-GAIKFTNRATE
                         TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
                END-IF
              END-IF
             END-IF
      *****  IF  WRK-LCOMB-GAI-KFTNRATE(IDX2)      =   ZERO
      *         MOVE    WRK-GAIKFTNRATE
      *                 TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
      *      ELSE
      *          IF  (WRK-GAIKFTNRATE     >   ZERO )    AND
      *              (WRK-GAIKFTNRATE     <   WRK-LCOMB-GAI-KFTNRATE
      *                                                     (IDX2))
      *              MOVE  WRK-GAIKFTNRATE
      *                            TO  WRK-LCOMB-GAI-KFTNRATE(IDX2)
      *          END-IF
      *******END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-GAI-KKOTGAK(IDX2)    =   ZERO
                MOVE   HKN-HON-GAIKKOTGAK
                         TO  WRK-LCOMB-GAI-KKOTGAK(IDX2)
             ELSE
               IF  (HKN-HON-GAIKKOTGAK   >   ZERO )   AND
                   (HKN-HON-GAIKKOTGAK
                      <   WRK-LCOMB-GAI-KKOTGAK(IDX2))
                 MOVE    HKN-HON-GAIKKOTGAK
                               TO  WRK-LCOMB-GAI-KKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-GAI-KJGNGAK(IDX2)    =   ZERO
                MOVE    HKN-HON-GAIKJGNGAK
                      TO  WRK-LCOMB-GAI-KJGNGAK(IDX2)
             ELSE
               IF  (HKN-HON-GAIKJGNGAK >   ZERO )   AND
                   (HKN-HON-GAIKJGNGAK <   WRK-LCOMB-GAI-KJGNGAK(IDX2))
                 MOVE    HKN-HON-GAIKJGNGAK
                                   TO  WRK-LCOMB-GAI-KJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-GAI-DJGNGAK(IDX2)     =   ZERO
               MOVE   HKN-HON-GAIDJGNGAK TO WRK-LCOMB-GAI-DJGNGAK(IDX2)
             ELSE
               IF (HKN-HON-GAIDJGNGAK  >   ZERO )   AND
                  (HKN-HON-GAIDJGNGAK  <   WRK-LCOMB-GAI-DJGNGAK(IDX2))
                MOVE    HKN-HON-GAIDJGNGAK
                                   TO  WRK-LCOMB-GAI-DJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-GAI-DJGNCNT(IDX2)     =   ZERO
               MOVE   HKN-HON-GAIDJGNCNT TO WRK-LCOMB-GAI-DJGNCNT(IDX2)
             ELSE
               IF (HKN-HON-GAIDJGNCNT  >   ZERO )   AND
                  (HKN-HON-GAIDJGNCNT  <   WRK-LCOMB-GAI-DJGNCNT(IDX2))
                  MOVE    HKN-HON-GAIDJGNCNT
                                   TO  WRK-LCOMB-GAI-DJGNCNT(IDX2)
               END-IF
             END-IF
      *    <月−院内−上限額>
             IF  WRK-LCOMB-GAI-MNAIJGNGAK(IDX2)    =   ZERO
               MOVE  HKN-HON-GAIMNAIJGNGAK
                                 TO  WRK-LCOMB-GAI-MNAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-HON-GAIMNAIJGNGAK > ZERO )   AND
                  (HKN-HON-GAIMNAIJGNGAK
                                    < WRK-LCOMB-GAI-MNAIJGNGAK(IDX2))
                  MOVE    HKN-HON-GAIMNAIJGNGAK
                                TO  WRK-LCOMB-GAI-MNAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−院外−上限額>
             IF  WRK-LCOMB-GAI-MGAIJGNGAK(IDX2)    =   ZERO
               MOVE  HKN-HON-GAIMGAIJGNGAK
                                 TO  WRK-LCOMB-GAI-MGAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-HON-GAIMGAIJGNGAK > ZERO )   AND
                  (HKN-HON-GAIMGAIJGNGAK
                                    < WRK-LCOMB-GAI-MGAIJGNGAK(IDX2))
                  MOVE    HKN-HON-GAIMGAIJGNGAK
                                TO  WRK-LCOMB-GAI-MGAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-GAI-MJGNCNT(IDX2)    =   ZERO
               MOVE   HKN-HON-GAIMJGNCNT
                                TO  WRK-LCOMB-GAI-MJGNCNT(IDX2)
             ELSE
               IF  (HKN-HON-GAIMJGNCNT >   ZERO )   AND
                  (HKN-HON-GAIMJGNCNT
                           <   WRK-LCOMB-GAI-MJGNCNT(IDX2))
                  MOVE    HKN-HON-GAIMJGNCNT
                                TO  WRK-LCOMB-GAI-MJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *  [通常] (入院)
           IF  WRK-LCOMB-NYU-FTNKBN (IDX2)     =   "0"  OR  "2"
               INITIALIZE            WRK-LCOMB-NYU-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
      *      Ｈ１４．１０改正 老人の時給付割合が補助区分にある
      *      IF      WRK-LCOMB-TEKSTYMD (IDX2)     >=   "20021001"
      *          EVALUATE    WRK-LCOMB-HOJOKBN (IDX2)
      *              WHEN    "8"
      *                  ２割負担
      *                  MOVE    20                TO  WRK-GAIKFTNRATE
      *              WHEN    "9"
      *                  １割負担
      *                  MOVE    10                TO  WRK-GAIKFTNRATE
      *              WHEN    OTHER
      *                  その他
      *                  MOVE    HKN-HON-NYUKFTNRATE
      *                                            TO  WRK-GAIKFTNRATE
      *          END-EVALUATE
      *      ELSE
      *         MOVE    HKN-HON-NYUKFTNRATE    TO  WRK-GAIKFTNRATE
      ****** END-IF
      *
      *   <回−負担割合>
      *      老人公費の時、老人公費の割合を対象とする
             IF      WRK-KEN-HKNNUM            =   "027"
                 IF      WRK-GAIKFTNRATE   NOT =   ZERO
                     MOVE    HKN-HON-NYUKFTNRATE
                                           TO  WRK-LCOMB-NYU-KFTNRATE
                                                                  (IDX2)
                 END-IF
             END-IF
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-NYU-KFTNRATE(IDX2)      =   ZERO
                MOVE    HKN-HON-NYUKFTNRATE
                     TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
              ELSE
               IF  (HKN-HON-NYUKFTNRATE     >   ZERO )    AND
                  (HKN-HON-NYUKFTNRATE < WRK-LCOMB-NYU-KFTNRATE(IDX2))
                 MOVE  HKN-HON-NYUKFTNRATE
                    TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
               END-IF
              END-IF
             END-IF
     ****    IF  WRK-LCOMB-NYU-KFTNRATE(IDX2)      =   ZERO
      *         MOVE    WRK-GAIKFTNRATE
      *              TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
      *      ELSE
      *        IF  (WRK-GAIKFTNRATE    >   ZERO )    AND
      *            (WRK-GAIKFTNRATE    <   WRK-LCOMB-NYU-KFTNRATE
      *                                                     (IDX2))
      *          MOVE  WRK-GAIKFTNRATE TO  WRK-LCOMB-NYU-KFTNRATE(IDX2)
      *        END-IF
      ****** END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-NYU-KKOTGAK(IDX2)    =   ZERO
               MOVE   HKN-HON-NYUKKOTGAK
                              TO  WRK-LCOMB-NYU-KKOTGAK(IDX2)
             ELSE
               IF  (HKN-HON-NYUKKOTGAK   >   ZERO )   AND
                (HKN-HON-NYUKKOTGAK   <   WRK-LCOMB-NYU-KKOTGAK(IDX2))
                MOVE    HKN-HON-NYUKKOTGAK
                                        TO  WRK-LCOMB-NYU-KKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-NYU-KJGNGAK(IDX2)    =   ZERO
                MOVE    HKN-HON-NYUKJGNGAK
                      TO  WRK-LCOMB-NYU-KJGNGAK(IDX2)
             ELSE
               IF  (HKN-HON-NYUKJGNGAK >   ZERO )   AND
                   (HKN-HON-NYUKJGNGAK <   WRK-LCOMB-NYU-KJGNGAK(IDX2))
                  MOVE    HKN-HON-NYUKJGNGAK
                                   TO  WRK-LCOMB-NYU-KJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-NYU-DJGNGAK(IDX2)     =   ZERO
                MOVE    HKN-HON-NYUDJGNGAK
                             TO WRK-LCOMB-NYU-DJGNGAK(IDX2)
             ELSE
               IF (HKN-HON-NYUDJGNGAK  >   ZERO )   AND
                  (HKN-HON-NYUDJGNGAK  <   WRK-LCOMB-NYU-DJGNGAK(IDX2))
                  MOVE    HKN-HON-NYUDJGNGAK
                                   TO  WRK-LCOMB-NYU-DJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-NYU-DJGNCNT(IDX2)     =   ZERO
               MOVE   HKN-HON-NYUDGJNCNT TO WRK-LCOMB-NYU-DJGNCNT(IDX2)
             ELSE
               IF (HKN-HON-NYUDGJNCNT  >   ZERO )   AND
                  (HKN-HON-NYUDGJNCNT  <   WRK-LCOMB-NYU-DJGNCNT(IDX2))
                  MOVE    HKN-HON-NYUDGJNCNT
                                   TO  WRK-LCOMB-NYU-DJGNCNT(IDX2)
               END-IF
             END-IF
      *   <月−上限額>
             IF  WRK-LCOMB-NYU-MJGNGAK(IDX2)    =   ZERO
               MOVE  HKN-HON-NYUMJGNGAK
                                 TO  WRK-LCOMB-NYU-MJGNGAK(IDX2)
             ELSE
               IF  (HKN-HON-NYUMJGNGAK > ZERO )   AND
                   (HKN-HON-NYUMJGNGAK
                                       < WRK-LCOMB-NYU-MJGNGAK(IDX2))
                  MOVE    HKN-HON-NYUMJGNGAK
                                TO  WRK-LCOMB-NYU-MJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-NYU-MJGNCNT(IDX2)    =   ZERO
               MOVE   HKN-HON-NYUMJGNCNT
                                TO  WRK-LCOMB-NYU-MJGNCNT(IDX2)
             ELSE
               IF  (HKN-HON-NYUMJGNCNT >   ZERO )   AND
                   (HKN-HON-NYUMJGNCNT <   WRK-LCOMB-NYU-MJGNCNT(IDX2))
                  MOVE    HKN-HON-NYUMJGNCNT
                                TO  WRK-LCOMB-NYU-MJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *    [低所得] (外来)
           IF  WRK-LCOMB-TSY-GAIFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TSY-GAI-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)  =   ZERO
               MOVE    HKN-TSY-GAIKFTNRATE
                      TO  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
              ELSE
               IF  (HKN-TSY-GAIKFTNRATE   >   ZERO )    AND
                   (HKN-TSY-GAIKFTNRATE
                      <   WRK-LCOMB-TSY-GAIKFTNRATE(IDX2))
                 MOVE  HKN-TSY-GAIKFTNRATE
                     TO  WRK-LCOMB-TSY-GAIKFTNRATE(IDX2)
               END-IF
              END-IF
             END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-TSY-GAIKKOTGAK(IDX2) =   ZERO
               MOVE   HKN-TSY-GAIKKOTGAK
                  TO  WRK-LCOMB-TSY-GAIKKOTGAK(IDX2)
             ELSE
               IF  (HKN-TSY-GAIKKOTGAK   >   ZERO )   AND
                   (HKN-TSY-GAIKKOTGAK
                         <   WRK-LCOMB-TSY-GAIKKOTGAK(IDX2))
                 MOVE    HKN-TSY-GAIKKOTGAK
                             TO  WRK-LCOMB-TSY-GAIKKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-TSY-GAIKJGNGAK(IDX2)    =   ZERO
               MOVE    HKN-TSY-GAIKJGNGAK
                      TO  WRK-LCOMB-TSY-GAIKJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-GAIKJGNGAK >   ZERO )   AND
                   (HKN-TSY-GAIKJGNGAK
                        <   WRK-LCOMB-TSY-GAIKJGNGAK(IDX2))
                  MOVE    HKN-TSY-GAIKJGNGAK
                                   TO  WRK-LCOMB-TSY-GAIKJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-TSY-GAIDJGNGAK(IDX2)     =   ZERO
               MOVE    HKN-TSY-GAIDJGNGAK
                 TO  WRK-LCOMB-TSY-GAIDJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-GAIDJGNGAK  >   ZERO )   AND
                   (HKN-TSY-GAIDJGNGAK
                    <   WRK-LCOMB-TSY-GAIDJGNGAK(IDX2))
                 MOVE    HKN-TSY-GAIDJGNGAK
                                   TO  WRK-LCOMB-TSY-GAIDJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-TSY-GAIDJGNCNT(IDX2)  =   ZERO
               MOVE    HKN-TSY-GAIDJGNCNT
                  TO  WRK-LCOMB-TSY-GAIDJGNCNT(IDX2)
             ELSE
               IF  (HKN-TSY-GAIDJGNCNT  >   ZERO )   AND
                   (HKN-TSY-GAIDJGNCNT
                         <   WRK-LCOMB-TSY-GAIDJGNCNT(IDX2))
                  MOVE    HKN-TSY-GAIDJGNCNT
                                   TO  WRK-LCOMB-TSY-GAIDJGNCNT(IDX2)
               END-IF
             END-IF
      *    <月−院内−上限額>
             IF  WRK-LCOMB-TSY-GAIMNAIJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TSY-GAIMNAIJGNGAK
                                 TO  WRK-LCOMB-TSY-GAIMNAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-GAIMNAIJGNGAK > ZERO )   AND
                   (HKN-TSY-GAIMNAIJGNGAK
                           < WRK-LCOMB-TSY-GAIMNAIJGNGAK(IDX2))
                  MOVE    HKN-TSY-GAIMNAIJGNGAK
                                TO  WRK-LCOMB-TSY-GAIMNAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−院外−上限額>
             IF  WRK-LCOMB-TSY-GAIMGAIJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TSY-GAIMGAIJGNGAK
                                 TO  WRK-LCOMB-TSY-GAIMGAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-GAIMGAIJGNGAK > ZERO )   AND
                   (HKN-TSY-GAIMGAIJGNGAK
                               < WRK-LCOMB-TSY-GAIMGAIJGNGAK(IDX2))
                  MOVE    HKN-TSY-GAIMGAIJGNGAK
                                TO  WRK-LCOMB-TSY-GAIMGAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-TSY-GAIMJGNCNT(IDX2)  =   ZERO
               MOVE   HKN-TSY-GAIMJGNCNT
                                TO  WRK-LCOMB-TSY-GAIMJGNCNT(IDX2)
             ELSE
               IF  (HKN-TSY-GAIMJGNCNT >   ZERO )   AND
                   (HKN-TSY-GAIMJGNCNT
                     <   WRK-LCOMB-TSY-GAIMJGNCNT(IDX2))
                  MOVE    HKN-TSY-GAIMJGNCNT
                                TO  WRK-LCOMB-TSY-GAIMJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *    [低所得] (入院)
           IF  WRK-LCOMB-TSY-NYUFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TSY-NYU-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)   =   ZERO
               MOVE    HKN-TSY-NYUKFTNRATE
                   TO  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
              ELSE
               IF  (HKN-TSY-NYUKFTNRATE  >   ZERO )    AND
                   (HKN-TSY-NYUKFTNRATE
                         <   WRK-LCOMB-TSY-NYUKFTNRATE(IDX2))
                 MOVE  HKN-TSY-NYUKFTNRATE
                         TO  WRK-LCOMB-TSY-NYUKFTNRATE(IDX2)
               END-IF
              END-IF
             END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-TSY-NYUKKOTGAK(IDX2)    =   ZERO
               MOVE   HKN-TSY-NYUKKOTGAK
                        TO  WRK-LCOMB-TSY-NYUKKOTGAK(IDX2)
             ELSE
               IF  (HKN-TSY-NYUKKOTGAK >   ZERO )   AND
                   (HKN-TSY-NYUKKOTGAK
                       <   WRK-LCOMB-TSY-NYUKKOTGAK(IDX2))
                 MOVE    HKN-TSY-NYUKKOTGAK
                              TO  WRK-LCOMB-TSY-NYUKKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-TSY-NYUKJGNGAK(IDX2)    =   ZERO
               MOVE    HKN-TSY-NYUKJGNGAK
                      TO  WRK-LCOMB-TSY-NYUKJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-NYUKJGNGAK >   ZERO )   AND
                   (HKN-TSY-NYUKJGNGAK
                       <   WRK-LCOMB-TSY-NYUKJGNGAK(IDX2))
                  MOVE    HKN-TSY-NYUKJGNGAK
                                   TO  WRK-LCOMB-TSY-NYUKJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-TSY-NYUDJGNGAK(IDX2)   =   ZERO
               MOVE    HKN-TSY-NYUDJGNGAK
                        TO  WRK-LCOMB-TSY-NYUDJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-NYUDJGNGAK  >   ZERO )   AND
                   (HKN-TSY-NYUDJGNGAK
                           <   WRK-LCOMB-TSY-NYUDJGNGAK(IDX2))
                  MOVE    HKN-TSY-NYUDJGNGAK
                                   TO  WRK-LCOMB-TSY-NYUDJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-TSY-NYUDJGNCNT(IDX2)  =   ZERO
               MOVE    HKN-TSY-NYUDJGNCNT
                      TO  WRK-LCOMB-TSY-NYUDJGNCNT(IDX2)
             ELSE
               IF  (HKN-TSY-NYUDJGNCNT  >   ZERO )   AND
                   (HKN-TSY-NYUDJGNCNT
                            <   WRK-LCOMB-TSY-NYUDJGNCNT(IDX2))
                  MOVE    HKN-TSY-NYUDJGNCNT
                                   TO  WRK-LCOMB-TSY-NYUDJGNCNT(IDX2)
               END-IF
             END-IF
      *   <月−上限額>
             IF  WRK-LCOMB-TSY-NYUMJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TSY-NYUMJGNGAK
                                 TO  WRK-LCOMB-TSY-NYUMJGNGAK(IDX2)
             ELSE
               IF  (HKN-TSY-NYUMJGNGAK > ZERO )   AND
                   (HKN-TSY-NYUMJGNGAK
                                   < WRK-LCOMB-TSY-NYUMJGNGAK(IDX2))
                  MOVE    HKN-TSY-NYUMJGNGAK
                                TO  WRK-LCOMB-TSY-NYUMJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-TSY-NYUMJGNCNT(IDX2)  =   ZERO
               MOVE   HKN-TSY-NYUMJGNCNT
                                TO  WRK-LCOMB-TSY-NYUMJGNCNT(IDX2)
             ELSE
               IF (HKN-TSY-NYUMJGNCNT > ZERO )   AND
                  (HKN-TSY-NYUMJGNCNT < WRK-LCOMB-TSY-NYUMJGNCNT(IDX2))
                  MOVE    HKN-TSY-NYUMJGNCNT
                                TO  WRK-LCOMB-TSY-NYUMJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *  [低年金] (外来)
           IF  WRK-LCOMB-TNK-GAIFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TNK-GAI-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)  =   ZERO
               MOVE    HKN-TNK-GAIKFTNRATE
                      TO  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
              ELSE
               IF  (HKN-TNK-GAIKFTNRATE   >   ZERO )    AND
                   (HKN-TNK-GAIKFTNRATE
                      <   WRK-LCOMB-TNK-GAIKFTNRATE(IDX2))
                 MOVE  HKN-TNK-GAIKFTNRATE
                     TO  WRK-LCOMB-TNK-GAIKFTNRATE(IDX2)
               END-IF
              END-IF
             END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-TNK-GAIKKOTGAK(IDX2) =   ZERO
               MOVE   HKN-TNK-GAIKKOTGAK
                  TO  WRK-LCOMB-TNK-GAIKKOTGAK(IDX2)
             ELSE
               IF  (HKN-TNK-GAIKKOTGAK   >   ZERO )   AND
                   (HKN-TNK-GAIKKOTGAK
                         <   WRK-LCOMB-TNK-GAIKKOTGAK(IDX2))
                 MOVE    HKN-TNK-GAIKKOTGAK
                             TO  WRK-LCOMB-TNK-GAIKKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-TNK-GAIKJGNGAK(IDX2)    =   ZERO
               MOVE    HKN-TNK-GAIKJGNGAK
                      TO  WRK-LCOMB-TNK-GAIKJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-GAIKJGNGAK >   ZERO )   AND
                   (HKN-TNK-GAIKJGNGAK
                        <   WRK-LCOMB-TNK-GAIKJGNGAK(IDX2))
                   MOVE    HKN-TNK-GAIKJGNGAK
                                   TO  WRK-LCOMB-TNK-GAIKJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-TNK-GAIDJGNGAK(IDX2)     =   ZERO
               MOVE    HKN-TNK-GAIDJGNGAK
                 TO  WRK-LCOMB-TNK-GAIDJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-GAIDJGNGAK  >   ZERO )   AND
                   (HKN-TNK-GAIDJGNGAK
                    <   WRK-LCOMB-TNK-GAIDJGNGAK(IDX2))
                  MOVE    HKN-TNK-GAIDJGNGAK
                                   TO  WRK-LCOMB-TNK-GAIDJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-TNK-GAIDJGNCNT(IDX2)  =   ZERO
               MOVE    HKN-TNK-GAIDJGNCNT
                  TO  WRK-LCOMB-TNK-GAIDJGNCNT(IDX2)
             ELSE
               IF  (HKN-TNK-GAIDJGNCNT  >   ZERO )   AND
                   (HKN-TNK-GAIDJGNCNT
                         <   WRK-LCOMB-TNK-GAIDJGNCNT(IDX2))
                  MOVE    HKN-TNK-GAIDJGNCNT
                                   TO  WRK-LCOMB-TNK-GAIDJGNCNT(IDX2)
               END-IF
             END-IF
      *    <月−院内−上限額>
             IF  WRK-LCOMB-TNK-GAIMNAIJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TNK-GAIMNAIJGNGAK
                                 TO  WRK-LCOMB-TNK-GAIMNAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-GAIMNAIJGNGAK > ZERO )   AND
                   (HKN-TNK-GAIMNAIJGNGAK
                           < WRK-LCOMB-TNK-GAIMNAIJGNGAK(IDX2))
                  MOVE    HKN-TNK-GAIMNAIJGNGAK
                                TO  WRK-LCOMB-TNK-GAIMNAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−院外−上限額>
             IF  WRK-LCOMB-TNK-GAIMGAIJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TNK-GAIMGAIJGNGAK
                                 TO  WRK-LCOMB-TNK-GAIMGAIJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-GAIMGAIJGNGAK > ZERO )   AND
                   (HKN-TNK-GAIMGAIJGNGAK
                             < WRK-LCOMB-TNK-GAIMGAIJGNGAK(IDX2))
                  MOVE    HKN-TNK-GAIMGAIJGNGAK
                                TO  WRK-LCOMB-TNK-GAIMGAIJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-TNK-GAIMJGNCNT(IDX2)  =   ZERO
               MOVE   HKN-TNK-GAIMJGNCNT
                                TO  WRK-LCOMB-TNK-GAIMJGNCNT(IDX2)
             ELSE
               IF  (HKN-TNK-GAIMJGNCNT >   ZERO )   AND
                   (HKN-TNK-GAIMJGNCNT
                     <   WRK-LCOMB-TNK-GAIMJGNCNT(IDX2))
                  MOVE    HKN-TNK-GAIMJGNCNT
                                TO  WRK-LCOMB-TNK-GAIMJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *    [低年金] (入院)
           IF  WRK-LCOMB-TNK-NYUFTNKBN(IDX2) =   "0"  OR  "2"
               INITIALIZE        WRK-LCOMB-TNK-NYU-NAIYO (IDX2)
           ELSE
      *   <回−負担割合>
             IF  WRK-LCOMB-DELKBN(IDX2)  NOT =   "9"
              IF  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)   =   ZERO
               MOVE    HKN-TNK-NYUKFTNRATE
                   TO  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
              ELSE
               IF  (HKN-TNK-NYUKFTNRATE  >   ZERO )    AND
                   (HKN-TNK-NYUKFTNRATE
                         <   WRK-LCOMB-TNK-NYUKFTNRATE(IDX2))
                 MOVE  HKN-TNK-NYUKFTNRATE
                         TO  WRK-LCOMB-TNK-NYUKFTNRATE(IDX2)
               END-IF
              END-IF
             END-IF
      *   <回−固定額>
             IF  WRK-LCOMB-TNK-NYUKKOTGAK(IDX2)    =   ZERO
               MOVE   HKN-TNK-NYUKKOTGAK
                        TO  WRK-LCOMB-TNK-NYUKKOTGAK(IDX2)
             ELSE
               IF  (HKN-TNK-NYUKKOTGAK >   ZERO )   AND
                   (HKN-TNK-NYUKKOTGAK
                       <   WRK-LCOMB-TNK-NYUKKOTGAK(IDX2))
                 MOVE    HKN-TNK-NYUKKOTGAK
                              TO  WRK-LCOMB-TNK-NYUKKOTGAK(IDX2)
               END-IF
             END-IF
      *   <回−上限額>
             IF  WRK-LCOMB-TNK-NYUKJGNGAK(IDX2)    =   ZERO
               MOVE    HKN-TNK-NYUKJGNGAK
                      TO  WRK-LCOMB-TNK-NYUKJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-NYUKJGNGAK >   ZERO )   AND
                   (HKN-TNK-NYUKJGNGAK
                           <   WRK-LCOMB-TNK-NYUKJGNGAK(IDX2))
                   MOVE    HKN-TNK-NYUKJGNGAK
                                   TO  WRK-LCOMB-TNK-NYUKJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限額>
             IF  WRK-LCOMB-TNK-NYUDJGNGAK(IDX2)   =   ZERO
               MOVE    HKN-TNK-NYUDJGNGAK
                        TO  WRK-LCOMB-TNK-NYUDJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-NYUDJGNGAK  >   ZERO )   AND
                   (HKN-TNK-NYUDJGNGAK
                           <   WRK-LCOMB-TNK-NYUDJGNGAK(IDX2))
                  MOVE    HKN-TNK-NYUDJGNGAK
                                   TO  WRK-LCOMB-TNK-NYUDJGNGAK(IDX2)
               END-IF
             END-IF
      *   <日−上限回数>
             IF  WRK-LCOMB-TNK-NYUDJGNCNT(IDX2)  =   ZERO
               MOVE    HKN-TNK-NYUDJGNCNT
                      TO  WRK-LCOMB-TNK-NYUDJGNCNT(IDX2)
             ELSE
               IF  (HKN-TNK-NYUDJGNCNT  >   ZERO )   AND
                   (HKN-TNK-NYUDJGNCNT
                            <   WRK-LCOMB-TNK-NYUDJGNCNT(IDX2))
                  MOVE    HKN-TNK-NYUDJGNCNT
                                   TO  WRK-LCOMB-TNK-NYUDJGNCNT(IDX2)
               END-IF
             END-IF
      *   <月−上限額>
             IF  WRK-LCOMB-TNK-NYUMJGNGAK(IDX2)  =   ZERO
               MOVE  HKN-TNK-NYUMJGNGAK
                                 TO  WRK-LCOMB-TNK-NYUMJGNGAK(IDX2)
             ELSE
               IF  (HKN-TNK-NYUMJGNGAK > ZERO )   AND
                   (HKN-TNK-NYUMJGNGAK
                                   < WRK-LCOMB-TNK-NYUMJGNGAK(IDX2))
                  MOVE    HKN-TNK-NYUMJGNGAK
                                TO  WRK-LCOMB-TNK-NYUMJGNGAK(IDX2)
               END-IF
             END-IF
      *   <月−上限回数>
             IF  WRK-LCOMB-TNK-NYUMJGNCNT(IDX2)  =   ZERO
               MOVE   HKN-TNK-NYUMJGNCNT
                                TO  WRK-LCOMB-TNK-NYUMJGNCNT(IDX2)
             ELSE
               IF (HKN-TNK-NYUMJGNCNT > ZERO )   AND
                  (HKN-TNK-NYUMJGNCNT < WRK-LCOMB-TNK-NYUMJGNCNT(IDX2))
                   MOVE    HKN-TNK-NYUMJGNCNT
                                TO  WRK-LCOMB-TNK-NYUMJGNCNT(IDX2)
               END-IF
             END-IF
           END-IF
      *
           .
       570121-FUTAN-IREKAE-EXT.
           EXIT.
      *
      *****************************************************************
      **    薬剤負担区分・食事療養費負担区分　入れ替え処理
      *****************************************************************
       570122-YKZ-SKJ-IREKAE-SEC        SECTION.
      *
      *   [通常]本人
           EVALUATE    WRK-LCOMB-GAI-YKZFTNKBN(IDX2)
           WHEN    "0"
              CONTINUE
           WHEN    "3"
              IF  HKN-HON-GAIYKZFTNKBN      =   "0"
                  MOVE    HKN-HON-GAIYKZFTNKBN    
                              TO  WRK-LCOMB-GAI-YKZFTNKBN(IDX2)
              END-IF
           WHEN    "2"
              IF  HKN-HON-GAIYKZFTNKBN      =   "3"  OR  "0"
                  MOVE    HKN-HON-GAIYKZFTNKBN
                              TO  WRK-LCOMB-GAI-YKZFTNKBN(IDX2)
              END-IF
           WHEN    "1"
              IF  HKN-HON-GAIYKZFTNKBN      =   "3"  OR  "0"  OR  "2"
                  MOVE    HKN-HON-GAIYKZFTNKBN
                              TO  WRK-LCOMB-GAI-YKZFTNKBN(IDX2)
              END-IF
           WHEN    OTHER
              MOVE    HKN-HON-GAIYKZFTNKBN
                              TO  WRK-LCOMB-GAI-YKZFTNKBN(IDX2)
           END-EVALUATE
      *
           EVALUATE    WRK-LCOMB-NYU-SKJRYOKBN(IDX2)
           WHEN    "0"
              CONTINUE
           WHEN    "3"
              IF  HKN-HON-NYUSKJRYYKBN      =   "0"
                IF  (IDA                    =    1 )
                AND (HKN-HON-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-HON-NYUSKJRYYKBN
                               TO  WRK-LCOMB-NYU-SKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "2"
              IF  HKN-HON-NYUSKJRYYKBN      =   "3"  OR  "0"
                IF  (IDA                    =    1 )
                AND (HKN-HON-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-HON-NYUSKJRYYKBN
                               TO  WRK-LCOMB-NYU-SKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "1"
              IF  (HKN-HON-NYUSKJRYYKBN       =  "3"  OR  "0"  OR  "2")
              AND (WRK-LCOMB-DELKBN(IDX2) NOT =  "9"                  )
                IF  (IDA                    =    1 )
                AND (HKN-HON-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-HON-NYUSKJRYYKBN
                               TO  WRK-LCOMB-NYU-SKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    OTHER
              MOVE    HKN-HON-NYUSKJRYYKBN
                               TO  WRK-LCOMB-NYU-SKJRYOKBN(IDX2)
           END-EVALUATE
      *   [低所得]
           EVALUATE    WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
           WHEN    "0"
              CONTINUE
           WHEN    "3"
              IF  HKN-TSY-GAIYKZFTNKBN      =   "0"
                  MOVE    HKN-TSY-GAIYKZFTNKBN
                               TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    "2"
              IF  HKN-TSY-GAIYKZFTNKBN      =   "3"  OR  "0"
                  MOVE    HKN-TSY-GAIYKZFTNKBN
                               TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    "1"
              IF  HKN-TSY-GAIYKZFTNKBN    =   "3"  OR  "0"  OR  "2"
                  MOVE    HKN-TSY-GAIYKZFTNKBN
                               TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    OTHER
              MOVE    HKN-TSY-GAIYKZFTNKBN
                               TO  WRK-LCOMB-TSY-GAIYKZFTNKBN(IDX2)
           END-EVALUATE
      *
           EVALUATE    WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
           WHEN    "0"
              CONTINUE
           WHEN    "3"
              IF  HKN-TSY-NYUSKJRYYKBN   =   "0"
                IF  (IDA                    =    1 )
                AND (HKN-TSY-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TSY-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "2"
              IF  HKN-TSY-NYUSKJRYYKBN   =   "3"  OR  "0"
                IF  (IDA                    =    1 )
                AND (HKN-TSY-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TSY-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "1"
              IF  (HKN-TSY-NYUSKJRYYKBN       =  "3"  OR  "0"  OR  "2")
              AND (WRK-LCOMB-DELKBN(IDX2) NOT =  "9"                  )
                IF  (IDA                    =    1 )
                AND (HKN-TSY-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TSY-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    OTHER
              MOVE    HKN-TSY-NYUSKJRYYKBN
                     TO  WRK-LCOMB-TSY-NYUSKJRYOKBN(IDX2)
           END-EVALUATE
      *   [低年金]
           EVALUATE    WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
           WHEN    "0"
              CONTINUE
           WHEN    "3"
              IF  HKN-TNK-GAIYKZFTNKBN    =   "0"
                  MOVE    HKN-TNK-GAIYKZFTNKBN
                          TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    "2"
              IF  HKN-TNK-GAIYKZFTNKBN    =   "3"  OR  "0"
                  MOVE    HKN-TNK-GAIYKZFTNKBN
                          TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    "1"
              IF  HKN-TNK-GAIYKZFTNKBN    =   "3"  OR  "0"  OR  "2"
                  MOVE    HKN-TNK-GAIYKZFTNKBN
                          TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
              END-IF
           WHEN    OTHER
               MOVE    HKN-TNK-GAIYKZFTNKBN
                          TO  WRK-LCOMB-TNK-GAIYKZFTNKBN(IDX2)
           END-EVALUATE
      *
           EVALUATE    WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
           WHEN    "0"
               CONTINUE
           WHEN    "3"
              IF  HKN-TNK-NYUSKJRYYKBN    =   "0"
                IF  (IDA                    =    1 )
                AND (HKN-TNK-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TNK-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "2"
              IF  HKN-TNK-NYUSKJRYYKBN    =   "3"  OR  "0"
                IF  (IDA                    =    1 )
                AND (HKN-TNK-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TNK-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    "1"
              IF  (HKN-TNK-NYUSKJRYYKBN       =  "3"  OR  "0"  OR  "2")
              AND (WRK-LCOMB-DELKBN(IDX2) NOT =  "9"                  )
                IF  (IDA                    =    1 )
                AND (HKN-TNK-NYUSKJRYYKBN   =   "0")
                  CONTINUE
                ELSE
                  MOVE    HKN-TNK-NYUSKJRYYKBN
                          TO  WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
                END-IF
              END-IF
           WHEN    OTHER
              MOVE    HKN-TNK-NYUSKJRYYKBN
                      TO  WRK-LCOMB-TNK-NYUSKJRYOKBN(IDX2)
           END-EVALUATE
      *
           .
       570122-YKZ-SKJ-IREKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    上限区分セット処理
      *****************************************************************
       570123-JOGKBN-SET-SEC        SECTION.
      *
           EVALUATE    IDA
      *   // 保険 //
           WHEN    1
              IF  WRK-LCOMB-HONKZKKBN (IDX2)   NOT =   "2"
                  IF  (HKN-HON-GAIDJGNGAK      >   ZERO)  OR
                      (HKN-HON-GAIDJGNCNT      >   ZERO)  OR
                      (HKN-HON-GAIMNAIJGNGAK   >   ZERO)  OR
                      (HKN-HON-GAIMGAIJGNGAK   >   ZERO)  OR
                      (HKN-HON-GAIMJGNCNT      >   ZERO)
                      MOVE    "1"  TO  WRK-LCOMB-SYU-GAIJGNKBN(IDX2)
                  END-IF
                  IF  (HKN-HON-NYUDJGNGAK      >   ZERO)  OR
                      (HKN-HON-NYUDGJNCNT      >   ZERO)  OR
                      (HKN-HON-NYUMJGNGAK      >   ZERO)  OR
                      (HKN-HON-NYUMJGNCNT      >   ZERO)
                      MOVE    "1"  TO  WRK-LCOMB-SYU-NYUJGNKBN(IDX2)
                  END-IF
              ELSE
                  IF  (HKN-KZK-GAIDJGNGAK      >   ZERO)  OR
                      (HKN-KZK-GAIDJGNCNT      >   ZERO)  OR
                      (HKN-KZK-GAIMNAIJGNGAK   >   ZERO)  OR
                      (HKN-KZK-GAIMGAIJGNGAK   >   ZERO)  OR
                      (HKN-KZK-GAIMJGNCNT      >   ZERO)
                      MOVE    "1"  TO  WRK-LCOMB-SYU-GAIJGNKBN(IDX2)
                  END-IF
                  IF  (HKN-KZK-NYUDJGNGAK      >   ZERO)  OR
                      (HKN-KZK-NYUDJGNCNT      >   ZERO)  OR
                      (HKN-KZK-NYUMJGNGAK      >   ZERO)  OR
                      (HKN-KZK-NYUMJGNCNT      >   ZERO)
                      MOVE    "1"  TO  WRK-LCOMB-SYU-NYUJGNKBN(IDX2)
                  END-IF
              END-IF
      *   // 公費１ //
           WHEN    2
           WHEN    3
           WHEN    4
           WHEN    5
              IF  (HKN-HON-GAIDJGNGAK          >   ZERO)  OR
                  (HKN-HON-GAIDJGNCNT          >   ZERO)  OR
                  (HKN-HON-GAIMNAIJGNGAK       >   ZERO)  OR
                  (HKN-HON-GAIMGAIJGNGAK       >   ZERO)  OR
                  (HKN-HON-GAIMJGNCNT          >   ZERO)  OR
                  (HKN-TSY-GAIDJGNGAK          >   ZERO)  OR
                  (HKN-TSY-GAIDJGNCNT          >   ZERO)  OR
                  (HKN-TSY-GAIMNAIJGNGAK       >   ZERO)  OR
                  (HKN-TSY-GAIMGAIJGNGAK       >   ZERO)  OR
                  (HKN-TSY-GAIMJGNCNT          >   ZERO)  OR
                  (HKN-TNK-GAIDJGNGAK          >   ZERO)  OR
                  (HKN-TNK-GAIDJGNCNT          >   ZERO)  OR
                  (HKN-TNK-GAIMNAIJGNGAK       >   ZERO)  OR
                  (HKN-TNK-GAIMGAIJGNGAK       >   ZERO)  OR
                  (HKN-TNK-GAIMJGNCNT          >   ZERO)
                  EVALUATE    IDA
                  WHEN    2
                      MOVE    "1"   TO  WRK-LCOMB-KOH1-GAIJGNKBN (IDX2)
                  WHEN    3
                      MOVE    "1"   TO  WRK-LCOMB-KOH2-GAIJGNKBN (IDX2)
                  WHEN    4
                      MOVE    "1"   TO  WRK-LCOMB-KOH3-GAIJGNKBN (IDX2)
                  WHEN    5
                      MOVE    "1"   TO  WRK-LCOMB-KOH4-GAIJGNKBN (IDX2)
                  END-EVALUATE
              END-IF
              IF  (HKN-HON-NYUDJGNGAK         >   ZERO)  OR
                  (HKN-HON-NYUDGJNCNT         >   ZERO)  OR
                  (HKN-HON-NYUMJGNGAK         >   ZERO)  OR
                  (HKN-HON-NYUMJGNCNT         >   ZERO)  OR
                  (HKN-TSY-NYUDJGNGAK         >   ZERO)  OR
                  (HKN-TSY-NYUDJGNCNT         >   ZERO)  OR
                  (HKN-TSY-NYUMJGNGAK         >   ZERO)  OR
                  (HKN-TSY-NYUMJGNCNT         >   ZERO)  OR
                  (HKN-TNK-NYUDJGNGAK         >   ZERO)  OR
                  (HKN-TNK-NYUDJGNCNT         >   ZERO)  OR
                  (HKN-TNK-NYUMJGNGAK         >   ZERO)  OR
                  (HKN-TNK-NYUMJGNCNT         >   ZERO)
                  EVALUATE    IDA
                  WHEN    2
                    MOVE    "1" TO  WRK-LCOMB-KOH1-NYUJGNKBN(IDX2)
                  WHEN    3
                    MOVE    "1" TO  WRK-LCOMB-KOH2-NYUJGNKBN(IDX2)
                  WHEN    4
                    MOVE    "1" TO  WRK-LCOMB-KOH3-NYUJGNKBN(IDX2)
                  WHEN    5
                    MOVE    "1" TO  WRK-LCOMB-KOH4-NYUJGNKBN(IDX2)
                  END-EVALUATE
              END-IF
           END-EVALUATE
      *
           .
       570123-JOGKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    生活保護中心　組合せ作成処理
      *****************************************************************
       57013-SEIHO-HEN-SEC        SECTION.
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX   >       200 )
                       OR      ( IDX   >       CNT-KOH )
             IF  (WRK-KOH-SBT(IDX)     =   "5")   AND
                 (WRK-KOH-SAKJOKBN(IDX)     =   SPACE)
      *
                 INITIALIZE                  WRK-KCOMB-AREA
                 MOVE    ZERO            TO  WRK-KCOMB-MAX
                 ADD     1               TO  WRK-KCOMB-MAX
                 MOVE    WRK-KCOMB-MAX   TO  IDX2
                 MOVE    SPACE           TO  WRK-KCOMB-REC  (IDX2)
                 INITIALIZE                  WRK-KCOMB-REC  (IDX2)
                 MOVE    WRK-KOH-KOHID(IDX)
                                       TO  WRK-KCOMB-KOH1ID      (IDX2)
                 MOVE    WRK-KOH-HKNNUM(IDX)
                                       TO  WRK-KCOMB-KOH1HKNNUM  (IDX2)
                 MOVE    WRK-KOH-PAYKBN(IDX)
                                       TO  WRK-KCOMB-KOH1PAYKBN  (IDX2)
                 MOVE    WRK-KOH-TEKSTYMD (IDX)
                                       TO  WRK-KCOMB-TEKSTYMD    (IDX2)
                 MOVE    WRK-KOH-TEKEDYMD (IDX)
                                       TO  WRK-KCOMB-TEKEDYMD    (IDX2)
                 MOVE    WRK-KOH-SEIDO(IDX)
                                 TO  WRK-KCOMB-KOH1-TANSEIDONAME(IDX2)
      *          ------------------------------------
                 MOVE    WRK-KOH-SBT  (IDX)
                                  TO  WRK-KCOMB-KOH1-HKNKOHSBTKBN(IDX2)
      *
      *          保険表中より主保険を検索し、適用開始日順に
      *                                   インデックス取得
                 INITIALIZE                WRK-RCOMB-AREA
                 INITIALIZE                WRK-IDXSRT-AREA
                 MOVE    ZERO              TO  WRK-RCOMB-MAX
                 MOVE    ZERO              TO  WRK-COMB-RJNIDX
                 PERFORM 570131-HKNIDX-SORT-SEC
      *
                 MOVE    ZERO              TO  FLG-HIYATOI
                 IF  WRK-SRT-HKN-CNT       >   ZERO
      *            [保険テーブル]より主保険を検索
                   PERFORM     VARYING   IDZ   FROM  1   BY  1
                               UNTIL     IDZ   >     WRK-SRT-HKN-CNT
                       MOVE    WRK-SRT-HKN-IDX (IDZ)
                                         TO  WRK-COMB-SYUHOIDX
                       IF  (WRK-HKN-HKNNUM(WRK-COMB-SYUHOIDX)
                                                =  "003" OR "004")
                          IF  (WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                        NOT = "99999999")
                          AND (WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                           >= "20120601")
                            MOVE    1    TO  FLG-HIYATOI
                          END-IF
                          PERFORM 570132X-SEIHO-HKN-HEN-SEC
                       ELSE
                          PERFORM 570132-SEIHO-HKN-HEN-SEC
                       END-IF
                   END-PERFORM
      *            主保険が終了しその後、生保のみが存在する場合
                   IF  WRK-KCOMB-TEKSTYMD (IDX2)
      *@@>                           NOT =   WRK-KCOMB-TEKEDYMD (IDX2)
                                        <=   WRK-KCOMB-TEKEDYMD (IDX2)
                       ADD     1         TO  WRK-LCOMB-MAX
                       MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-LCOMB-REC
                                                       (WRK-LCOMB-MAX)
                       MOVE    WRK-LCOMB-MAX  TO  IDX2
                       PERFORM 57012-FUTAN-SET-SEC
                   END-IF
      *            [編集後テーブル]より転送
                   PERFORM     VARYING   IDZ   FROM    1   BY  1
                               UNTIL   ( IDZ   >       200 )
                               OR      ( IDZ   >       WRK-RCOMB-MAX )
                     ADD     1           TO  WRK-LCOMB-MAX
                     MOVE    WRK-RCOMB-REC (IDZ)
                                   TO  WRK-LCOMB-REC (WRK-LCOMB-MAX)
                     MOVE    WRK-LCOMB-MAX    TO  IDX2
                     PERFORM 57012-FUTAN-SET-SEC
                   END-PERFORM
                 ELSE
                   ADD     1             TO  WRK-LCOMB-MAX
                   MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-LCOMB-REC
                                                       (WRK-LCOMB-MAX)
                   MOVE    WRK-LCOMB-MAX  TO  IDX2
                   PERFORM 57012-FUTAN-SET-SEC
                 END-IF
      *
                 IF    FLG-HIYATOI  =   1
                    PERFORM  570133-SEIHO-OKIKAE-HENSYU-SEC
                 END-IF
      *
                 PERFORM     57010-NC01PARA-OUT-SEC
      *
             END-IF
           END-PERFORM
           .
       57013-SEIHO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費単独　組合せ作成処理
      *****************************************************************
       57013-KOHTANDOKU-HEN-SEC        SECTION.
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX   >       200 )
                       OR      ( IDX   >       CNT-KOH )
             IF ((WRK-KOH-SBT   (IDX)  =   "6")   AND
                 (WRK-KOH-SAKJOKBN   (IDX)  =   SPACE)) OR
                ((WRK-KOH-HKNNUM   (IDX)  =   "019"
                                          OR  "011")   AND
                 (WRK-KOH-SAKJOKBN (IDX)  =   SPACE)   AND
                 (WRK-HIYATOI-FLG         =   1 )     )
      *
               MOVE    ZERO            TO  FLG-KOH-CHK
               IF      WRK-KOH-HKNNUM   (IDX)  =   "019"
                                               OR  "011"
                   MOVE    1           TO  FLG-KOH-CHK
      *            日雇いの範囲外は作成しない
                   IF    (WRK-KOH-TEKSTYMD(IDX)
                                       >   WRK-HIYATOI-TEKEDYMD )
                     OR  (WRK-KOH-TEKEDYMD(IDX)
                                       <   WRK-HIYATOI-TEKSTYMD )
                       MOVE    2           TO  FLG-KOH-CHK
                   END-IF
               END-IF
      *
               IF     (FLG-KOH-CHK      =   2    )
                   CONTINUE
               ELSE
      *
               INITIALIZE                  WRK-COMB-AREA
      *
      *        組合せレコード編集転送処理
               ADD     1                        TO  WRK-LCOMB-MAX
               MOVE    WRK-LCOMB-MAX             TO  IDX2
               MOVE    SPACE            TO  WRK-LCOMB-REC  (IDX2)
               INITIALIZE                   WRK-LCOMB-REC  (IDX2)
               MOVE    WRK-KOH-KOHID (IDX)
                                        TO  WRK-LCOMB-KOH1ID (IDX2)
               MOVE    WRK-KOH-HKNNUM(IDX)
                                        TO  WRK-LCOMB-KOH1HKNNUM(IDX2)
               MOVE    WRK-KOH-PAYKBN(IDX)
                                        TO  WRK-LCOMB-KOH1PAYKBN(IDX2)
               MOVE    WRK-KOH-TEKSTYMD(IDX)
                                        TO  WRK-LCOMB-TEKSTYMD   (IDX2)
               MOVE    WRK-KOH-TEKEDYMD(IDX)
                                        TO  WRK-LCOMB-TEKEDYMD   (IDX2)
               IF      (FLG-KOH-CHK      =   1    )
                   AND (WRK-HIYATOI-TEKSTYMD
                                         >=    WRK-KOH-TEKSTYMD(IDX))
                   MOVE   WRK-HIYATOI-TEKSTYMD
                                        TO  WRK-LCOMB-TEKSTYMD   (IDX2)
               END-IF
               IF    (FLG-KOH-CHK      =   1    )
                 AND (WRK-HIYATOI-TEKEDYMD
                                            <=   WRK-KOH-TEKEDYMD(IDX))
                      MOVE   WRK-HIYATOI-TEKEDYMD
                                       TO  WRK-LCOMB-TEKEDYMD   (IDX2)
               END-IF
               MOVE    WRK-KOH-SEIDO  (IDX)
                                  TO  WRK-LCOMB-KOH1-TANSEIDONAME(IDX2)
      *        ------------------------------------
               MOVE    WRK-KOH-SBT  (IDX)
                                  TO  WRK-LCOMB-KOH1-HKNKOHSBTKBN(IDX2)
      *        患者負担内容セット
               PERFORM 57012-FUTAN-SET-SEC
      *
               PERFORM     57010-NC01PARA-OUT-SEC
      *
               END-IF
             END-IF
           END-PERFORM
          .
       57013-KOHTANDOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主公費中心　組合せ作成処理
      *****************************************************************
       57014-SYUKOH-HEN-SEC            SECTION.
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX   >       200 )
                       OR      ( IDX   >       CNT-KOH )
      *
      *----(01.00.04) LINE MOD START -------------------------------
      *       IF  (WRK-KOH-SBT      (IDX)   =   "7")   AND
             IF  (WRK-KOH-SBT      (IDX)   =   "7"  OR  "A")   AND
      *----(01.00.04) LINE MOD END   -------------------------------
                 (WRK-KOH-HKNKOHKBN(IDX)   =   "1"  OR  "3")   AND
                 (WRK-KOH-SAKJOKBN (IDX)   =   SPACE)
      *
      *        [保険テーブル]より主保険を検索
               PERFORM     VARYING   IDY2  FROM    1   BY  1
                           UNTIL   ( IDY2  >       200 )
                           OR      ( IDY2  >       CNT-HKN )
      *
                 INITIALIZE                    WRK-COMB-AREA
                 INITIALIZE                    WRK-KCOMB-AREA
                 MOVE    ZERO              TO  WRK-KCOMB-MAX
                 MOVE    ZERO              TO  WRK-RCOMB-MAX
      *
                 IF  WRK-HKN-SAKJOKBN (IDY2)    =   SPACE
      *              [公費]適用年月日と[保険]適用年月日
                     IF  (WRK-HKN-TEKEDYMD(IDY2)
                                      <   WRK-KOH-TEKSTYMD(IDX))  OR
                         (WRK-HKN-TEKSTYMD(IDY2)
                                      >   WRK-KOH-TEKEDYMD(IDX))
                         CONTINUE
                     ELSE
      *                [主保険]
                       IF  WRK-HKN-SBT(IDY2)  =   "1"
      *!       特別療養費は対象外
                       AND (WRK-HKN-HKNNUM (IDY2)  NOT =   "068"
                                                       AND "040"
                                                       AND "069" )
      *!
      ****                 IF  ((WRK-HKN-HKNNUM (IDY)  =   "002") AND
      *                         (WRK-HKN-HOJOKBN(IDY)  =   SPACE OR
      *                                                    "8"  OR
      *                                                    "9"  ))  OR
      *****                    ( WRK-HKN-HKNNUM (IDY)  NOT =  "002")
      *                船員以外
                           IF  ((WRK-HKN-HKNNUM (IDY2)  =   "002" ) AND
                                (WRK-HKN-HOJOKBN(IDY2)  =   "1"  OR
                                                           "2"  OR
                                                           "3"  OR
                                                           "A"  OR
                                                           "B"  OR
                                                           "C"  OR
                                                           "D"  OR
                                                           "E"  OR
                                                           "F"  OR
                                                           "G"  OR
                                                           "H"  OR
                                                           "I"  ))  OR
                               ((WRK-HKN-HKNNUM (IDY2)  =   "031" OR
                                                           "032" OR
                                                           "033" OR
                                                           "034" )  AND
                                (WRK-HKN-HOJOKBN(IDY2)  =   "2" OR
                                                           "B" OR
                                                           "H" OR
                                                           "E"  ))
                               CONTINUE
                           ELSE
                               MOVE    IDX       TO  WRK-COMB-SYUKOHIDX
                               MOVE    IDY2      TO  WRK-COMB-SYUHOIDX
      *                        <主公費 & 主保険 組合せ処理>
                               PERFORM 570141-SYUKOH-SYUHO-HEN-SEC
                           END-IF
                       END-IF
                     END-IF
                 END-IF
               END-PERFORM
             END-IF
           END-PERFORM
      *
           MOVE    WRK-LCOMB-MAX      TO  WRK-LCOMB-MAX-O
      *    -------------------------------------------------------
      *    主保険が存在しない期間を求め、主公費中心の組合せを作成
      *    -------------------------------------------------------
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX       >   200 )
                       OR      ( IDX       >   CNT-KOH )
      *
      *----(01.00.04) LINE MOD START -----------------------------
      *       IF  (WRK-KOH-SBT      (IDX)   =   "7")   AND
             IF  (WRK-KOH-SBT      (IDX)   =   "7"  OR  "A")   AND
      *----(01.00.04) LINE MOD END   -----------------------------
                 (WRK-KOH-HKNKOHKBN(IDX)   =   "1"  OR  "3")   AND
                 (WRK-KOH-SAKJOKBN (IDX)   =   SPACE)
      *
               MOVE    IDX                 TO  WRK-COMB-SYUKOHIDX
      *
               INITIALIZE                      WRK-KCOMB-AREA
               MOVE    1                   TO  WRK-KCOMB-MAX
               MOVE    WRK-KCOMB-MAX       TO  IDX2
               INITIALIZE                      WRK-KCOMB-REC (IDX2)
               MOVE    WRK-KOH-TEKSTYMD(IDX)
                                     TO  WRK-KCOMB-TEKSTYMD    (IDX2)
               MOVE    WRK-KOH-TEKEDYMD(IDX)
                                     TO  WRK-KCOMB-TEKEDYMD    (IDX2)
               MOVE    WRK-KOH-KOHID   (IDX)
                                     TO  WRK-KCOMB-KOH1ID      (IDX2)
               MOVE    WRK-KOH-HKNNUM  (IDX)
                                     TO  WRK-KCOMB-KOH1HKNNUM  (IDX2)
               MOVE    WRK-KOH-PAYKBN (IDX)
                                     TO  WRK-KCOMB-KOH1PAYKBN  (IDX2)
               MOVE    WRK-KOH-SEIDO   (IDX)
                                TO  WRK-KCOMB-KOH1-TANSEIDONAME(IDX2)
               MOVE    WRK-KOH-SBT     (IDX)
                                TO  WRK-KCOMB-KOH1-HKNKOHSBTKBN(IDX2)
      *
      *        保険表中より主保険を検索し、適用開始日順に
      *                                        インデックス取得
               INITIALIZE                WRK-IDXSRT-AREA
               INITIALIZE                WRK-RCOMB-AREA
               MOVE    ZERO              TO  WRK-RCOMB-MAX
               PERFORM 570131-HKNIDX-SORT-SEC
               IF  WRK-SRT-HKN-CNT       >   ZERO
      *            [保険テーブル]より主保険を検索
                   PERFORM     VARYING   IDZ   FROM  1   BY  1
                               UNTIL     IDZ   >     WRK-SRT-HKN-CNT
                       MOVE    WRK-SRT-HKN-IDX (IDZ)
                                         TO  WRK-COMB-SYUHOIDX
                       PERFORM 570145-SYUHKNNASI-HEN-SEC
                   END-PERFORM
      *            主保険と期間が重複しない主公費のみが存在する場合
                   IF  WRK-KCOMB-TEKSTYMD (IDX2)
      *@@>                           NOT =   WRK-KCOMB-TEKEDYMD(IDX2)
                                        <=   WRK-KCOMB-TEKEDYMD(IDX2)
                       ADD     1         TO  WRK-RCOMB-MAX
                       MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC
                                                       (WRK-RCOMB-MAX)
                   END-IF
               ELSE
      *            主保険と期間が重複しない主公費の場合
                   ADD     1             TO  WRK-RCOMB-MAX
                   MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
               END-IF
      *  -----ここまでで　主公費+生保　ができてる
      *
               MOVE    WRK-LCOMB-MAX       TO  WRK-LCOMB-MAX-O
      *        入替転送
               MOVE    ZERO                TO  WRK-KCOMB-MAX
               INITIALIZE                      WRK-KCOMB-AREA
               PERFORM     VARYING   IDZ   FROM  1   BY  1
                           UNTIL   ( IDZ   >     200 )
                           OR      ( IDZ   >     WRK-RCOMB-MAX )
                   ADD     1               TO  WRK-LCOMB-MAX
                   MOVE    WRK-RCOMB-REC (IDZ)
                                           TO  WRK-LCOMB-REC
                                                    (WRK-LCOMB-MAX)
                   MOVE    WRK-LCOMB-MAX   TO  IDX2
                   PERFORM 57012-FUTAN-SET-SEC
                   ADD     1               TO  WRK-KCOMB-MAX
                   MOVE    WRK-LCOMB-REC (WRK-LCOMB-MAX)
                                           TO  WRK-KCOMB-REC
                                                    (WRK-KCOMB-MAX)
              END-PERFORM
              PERFORM 5701410-SYUKOH-KYOUTU-HEN-SEC
      *
             END-IF
           END-PERFORM
      *
           .
       57014-SYUKOH-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主公費−主保険　組合せ作成処理
      *     IDX...主公費, WRK-COMB-SYUHOIDX & IDX5...主保険
      *****************************************************************
       570141-SYUKOH-SYUHO-HEN-SEC       SECTION.
      *
      *    組合せレコード編集転送処理
           ADD     1                 TO  WRK-KCOMB-MAX
           MOVE    WRK-KCOMB-MAX     TO  IDX2
           MOVE    SPACE             TO  WRK-KCOMB-REC (IDX2)
           INITIALIZE                    WRK-KCOMB-REC (IDX2)
           MOVE    WRK-KOH-TEKSTYMD(IDX)
                                     TO  WRK-KCOMB-TEKSTYMD    (IDX2)
           MOVE    WRK-KOH-TEKEDYMD(IDX)
                                     TO  WRK-KCOMB-TEKEDYMD    (IDX2)
           MOVE    WRK-KOH-KOHID      (IDX)
                                     TO  WRK-KCOMB-KOH1ID      (IDX2)
           MOVE    WRK-KOH-HKNNUM      (IDX)
                                     TO  WRK-KCOMB-KOH1HKNNUM   (IDX2)
           MOVE    WRK-KOH-PAYKBN      (IDX)
                                     TO  WRK-KCOMB-KOH1PAYKBN   (IDX2)
           MOVE    WRK-KOH-SEIDO  (IDX)
                                  TO  WRK-KCOMB-KOH1-TANSEIDONAME(IDX2)
           MOVE    WRK-KOH-SBT  (IDX)
                                  TO  WRK-KCOMB-KOH1-HKNKOHSBTKBN(IDX2)
      *
           IF  (WRK-KOH-HKNNUM(IDX)       = "970")
           AND (WRK-KOH-JIBAISEIKBN(IDX)  = "1"  )
             MOVE  "9"               TO  WRK-KCOMB-DELKBN(IDX2)
           END-IF
      *
      *    [主保険 開始日]≦[主公費 開始日]
           IF  WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)
                                     <=  WRK-KOH-TEKSTYMD (IDX)
      *      [主公費 終了日]≦[主保険 終了日]
             IF  WRK-KOH-TEKEDYMD(IDX)
                                     <=  WRK-HKN-TEKEDYMD
                                                   (WRK-COMB-SYUHOIDX)
      *          ●[主公費] 開始 <---> [主公費] 終了
                 CONTINUE
             ELSE
      *          ●[主公費] 開始 <---> [主保険] 終了
                 MOVE    WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                     TO  WRK-KCOMB-TEKEDYMD  (IDX2)
             END-IF
           ELSE
      *      [主公費 開始日]＜[主保険 開始日]
             MOVE    WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)
                                     TO  WRK-KCOMB-TEKSTYMD  (IDX2)
      *      [主公費 終了日]≦[主保険 終了日]
             IF  WRK-KOH-TEKEDYMD(IDX)
                                     <=  WRK-HKN-TEKEDYMD
                                                  (WRK-COMB-SYUHOIDX)
      *          ●[主保険] 開始日 <---> [主公費] 終了
                 CONTINUE
             ELSE
      *      [主保険 終了日]≦[主公費 終了日]
      *          ●[主保険] 開始日 <---> [主保険] 終了
                 MOVE    WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                     TO  WRK-KCOMB-TEKEDYMD  (IDX2)
             END-IF
           END-IF
           MOVE    WRK-HKN-HKNID(WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-HKNID      (IDX2)
           MOVE    WRK-HKN-HKNNUM     (WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-HKNNUM     (IDX2)
           IF      WRK-HKN-HKNJANUM(WRK-COMB-SYUHOIDX)  = "67330753"
           AND     WRK-HKN-HOJOKBN (WRK-COMB-SYUHOIDX)  =  SPACE
           AND     WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)  <  "20111001"
               MOVE     9999       TO  WRK-KCOMB-HKNCOMBINUM(IDX2)
           END-IF
           MOVE    WRK-HKN-HONKZKKBN  (WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-HONKZKKBN  (IDX2)
           MOVE    WRK-HKN-HOJOKBN    (WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-HOJOKBN    (IDX2)
           MOVE    WRK-HKN-CONTKBN    (WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-CONTKBN    (IDX2)
           MOVE    WRK-HKN-SEIDO(WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-SYU-TANSEIDONAME(IDX2)
      *    ------------------------------------
           MOVE    WRK-HKN-SBT  (WRK-COMB-SYUHOIDX)
                                   TO  WRK-KCOMB-SYU-HKNKOHSBTKBN(IDX2)
      *
      *    老人付加処理
      *
      *    [公費テーブル]より老人を検索し、
      *                     適用開始日順にインデックス取得
           INITIALIZE                WRK-RCOMB-AREA
           INITIALIZE                WRK-IDXSRT-AREA
           MOVE    WRK-KCOMB-TEKSTYMD(IDX2)    TO  WRK-SRT-STYMD
           MOVE    WRK-KCOMB-TEKEDYMD(IDX2)    TO  WRK-SRT-EDYMD
           MOVE    "2"               TO  WRK-SRT-FLG
           PERFORM 570143-RJNIDX-SORT-SEC
      *
      *    [公費テーブル]より老人を検索
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL     IDZ      >    WRK-SRT-RJN-CNT
               MOVE    WRK-SRT-RJN-IDX (IDZ)   TO  WRK-COMB-RJNIDX
               PERFORM 570142-RJN-HEN-SEC
           END-PERFORM
      *
      *     DISPLAY "IDX2 = " IDX2
      *     DISPLAY "WRK-KCOMB-HKNNUM(IDX2) = "
      *              WRK-KCOMB-HKNNUM(IDX2)
      *     DISPLAY "WRK-KCOMB-KOH1HKNNUM(IDX2) = "
      *              WRK-KCOMB-KOH1HKNNUM(IDX2)
      *     DISPLAY "WRK-KCOMB-KOH2HKNNUM(IDX2) = "
      *              WRK-KCOMB-KOH2HKNNUM(IDX2)
      *     DISPLAY "WRK-KCOMB-TEKSTYMD (IDX2) = "
      *              WRK-KCOMB-TEKSTYMD (IDX2)
      *     DISPLAY "WRK-KCOMB-TEKEDYMD (IDX2) = "
      *              WRK-KCOMB-TEKEDYMD (IDX2)
      *-----判定変更
           IF    WRK-SRT-RJN-CNT    NOT =   ZERO
              IF  WRK-KCOMB-TEKSTYMD (IDX2)
                                NOT =   WRK-KCOMB-TEKEDYMD (IDX2)
                  ADD     1             TO  WRK-RCOMB-MAX
                  MOVE    WRK-KCOMB-REC (IDX2)
                                     TO  WRK-RCOMB-REC (WRK-RCOMB-MAX)
              END-IF
           ELSE
              IF  WRK-KCOMB-TEKSTYMD (IDX2)
                                     <=   WRK-KCOMB-TEKEDYMD (IDX2)
                  ADD     1             TO  WRK-RCOMB-MAX
                  MOVE    WRK-KCOMB-REC (IDX2)
                                     TO  WRK-RCOMB-REC (WRK-RCOMB-MAX)
              END-IF
           END-IF
      *-----判定変更ここまで
      *    老人保険が終了しその後、主保険のみが存在する場合
      **     IF  WRK-KCOMB-TEKSTYMD (IDX2)
      ****** (2005.10.25)      NOT =   WRK-KCOMB-TEKEDYMD (IDX2)
      **                            <=   WRK-KCOMB-TEKEDYMD (IDX2)
      **         ADD     1             TO  WRK-RCOMB-MAX
      **         MOVE    WRK-KCOMB-REC (IDX2)
      **                               TO  WRK-RCOMB-REC (WRK-RCOMB-MAX)
      **     END-IF
      *
           MOVE    WRK-LCOMB-MAX     TO  WRK-LCOMB-MAX-O
      *       まだ生保くっついた組合せなし
           MOVE    ZERO              TO  WRK-KCOMB-MAX
           INITIALIZE                    WRK-KCOMB-AREA
      *    [編集後テーブル]より転送
           PERFORM   VARYING   IDZ   FROM    1   BY  1
                     UNTIL   ( IDZ   >       200 )
                     OR      ( IDZ   >       WRK-RCOMB-MAX )
               ADD     1             TO  WRK-LCOMB-MAX
               MOVE    WRK-RCOMB-REC (IDZ)
                                     TO  WRK-LCOMB-REC (WRK-LCOMB-MAX)
               MOVE    WRK-LCOMB-MAX TO  IDX2
               PERFORM 57012-FUTAN-SET-SEC
      *        後に補助公費をセットするため再転送
               ADD     1             TO  WRK-KCOMB-MAX
               MOVE    WRK-LCOMB-REC (WRK-LCOMB-MAX)
                                     TO  WRK-KCOMB-REC (WRK-KCOMB-MAX)
           END-PERFORM
      *
           PERFORM 5701410-SYUKOH-KYOUTU-HEN-SEC
      *
           .
       570141-SYUKOH-SYUHO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    主公費中心　共通処理
      *****************************************************************
       5701410-SYUKOH-KYOUTU-HEN-SEC   SECTION.
      *
           INITIALIZE                  WRK-RCOMB-AREA
           MOVE    ZERO                TO  WRK-RCOMB-MAX
      *    [公費テーブル]より補助公費を検索
           PERFORM     VARYING   IDX2  FROM    1   BY  1
                       UNTIL   ( IDX2  >       200 )
                       OR      ( IDX2  >       WRK-KCOMB-MAX )
      *
             INITIALIZE                WRK-IDXSRT-AREA
      *
      *      補助公費を付加する対象かどうかの判定
      *        1:補助公費つける  2:補助公費つけない
             MOVE    SPACE             TO  WRK-FTNKBN-AREA
             INITIALIZE                    WRK-FTNKBN-AREA
             MOVE    1                 TO  FLG-HANTEI
             PERFORM 5701411-FUKA-HANTEI-SEC
      *
             EVALUATE    WRK-FTN-FLG
             WHEN    "1"
      *        公費表中より補助公費を検索し適用開始日順に
      *                                   インデックス取得
               MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                           TO  WRK-SRT-STYMD
               MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                           TO  WRK-SRT-EDYMD
               IF  WRK-KCOMB-KOH1-HKNKOHSBTKBN (IDX2)  =   "3"
                   MOVE    WRK-KCOMB-KOH2HKNNUM (IDX2)
                                           TO  WRK-SRT-HJO-KOHNO
               ELSE
                   MOVE    WRK-KCOMB-KOH1HKNNUM (IDX2)
                                           TO  WRK-SRT-HJO-KOHNO
               END-IF
               PERFORM 5701412-HJOIDX-SORT-SEC
               IF  WRK-SRT-HJO-CNT     >   ZERO
                 ADD     1                 TO  WRK-RCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                           TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
      *          補助公費を検索
                 PERFORM     VARYING   IDZ   FROM  1   BY  1
                             UNTIL     IDZ   >     WRK-SRT-HJO-CNT
                   MOVE    WRK-SRT-HJO-IDX (IDZ)
                                             TO  WRK-COMB-HJOIDX
                   IF   WRK-SRT-HJO-KOHNO(1:2) = "99"
                     CONTINUE
                   ELSE
                     IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                       =               SPACE)  OR
                        (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                       =   WRK-SRT-HJO-KOHNO)  OR
                        (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX)
                                       =   WRK-SRT-HJO-KOHNO)  OR
                        (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX)
                                       =   WRK-SRT-HJO-KOHNO)
      *
                      IF WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:2) = "99"
                            PERFORM 5701413-SYUKOH-HJO-SEC
                      ELSE
                       IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX) =
                               "019" OR "053" OR "091" OR "051"
                          IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX) =
                               "019" OR "053" OR "091"
                            IF  WRK-SRT-HJO-KOHNO =
                                 "010" OR "021" OR "015" OR "016" OR
                                 "011" OR "020" OR
                                 "052" OR "038" OR "024" OR "079" OR
                                 "051" OR "091" OR "054" OR "092"
                              PERFORM 5701413-SYUKOH-HJO-SEC
                            END-IF
                          END-IF
                          IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX) =
                               "051"
                            IF  WRK-SRT-HJO-KOHNO = "051"
                              PERFORM 5701413-SYUKOH-HJO-SEC
                            END-IF
                          END-IF
                          IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX) =
                               "051" OR "091"
                            IF   (SPA-PREFNUM            =  19 )
                            AND  (WRK-SRT-HJO-KOHNO(2:2) = "41")
                              PERFORM 5701413-SYUKOH-HJO-SEC
                            END-IF
                          END-IF
                       ELSE
      ******************  地方公費
                          IF  (WRK-SRT-HJO-KOHNO  NOT =  "020"       )
                          AND (WRK-SRT-HJO-KOHNO  NOT =
                                      WRK-KOH-HKNNUM(WRK-COMB-HJOIDX))
                                PERFORM 5701413-SYUKOH-HJO-SEC
                          END-IF
                       END-IF
                      END-IF
      *
                     END-IF
                   END-IF
                 END-PERFORM
      *          上で作成した組合せに、補助公費又は省庁公費を追加した組合せを作成する。
      *          （公費＋公費＋（補助公費又は省庁公費））
                 MOVE   WRK-RCOMB-MAX  TO   IDZ3
                 PERFORM     VARYING   IDZ   FROM  1   BY  1
                             UNTIL     IDZ   >     WRK-SRT-HJO-CNT
                   MOVE    WRK-SRT-HJO-IDX (IDZ)
                                       TO   WRK-COMB-HJOIDX
                   IF  WRK-KOH-HKNKOHKBN (WRK-COMB-HJOIDX)  =   "2"
                   OR  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:2) =   "99"
                     PERFORM   VARYING   IDZ2  FROM  1   BY  1
                               UNTIL     IDZ2  >     IDZ3
                       IF  (WRK-RCOMB-KOH1HKNNUM(IDZ2)      = "027" AND
                            WRK-RCOMB-KOH2HKNNUM(IDZ2)  NOT = SPACE AND
                            WRK-RCOMB-KOH3HKNNUM(IDZ2)  NOT = SPACE)
                       OR  (WRK-RCOMB-KOH1HKNNUM(IDZ2)  NOT = "027" AND
                            WRK-RCOMB-KOH1HKNNUM(IDZ2)  NOT = SPACE AND
                            WRK-RCOMB-KOH2HKNNUM(IDZ2)  NOT = SPACE)
                         IF   WRK-RCOMB-KOH1HKNNUM(IDZ2)    = "027"
                           IF  (WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)
                                          = WRK-RCOMB-KOH3HKNNUM(IDZ2))
                           OR  (WRK-RCOMB-KOH3HKNNUM(IDZ2)(1:2) = "99")
                              CONTINUE
                           ELSE
                             IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                      =                       SPACE) OR
                                (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH3HKNNUM(IDZ2)) OR
                                (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH3HKNNUM(IDZ2)) OR
                                (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH3HKNNUM(IDZ2))
                                 PERFORM 5701414-SYUKOH-HJO2-SEC
                             END-IF
                           END-IF
                         ELSE
                           IF  (WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)
                                          = WRK-RCOMB-KOH2HKNNUM(IDZ2))
                           OR  (WRK-RCOMB-KOH2HKNNUM(IDZ2)(1:2) = "99")
                              CONTINUE
                           ELSE
                             IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                      =                       SPACE) OR
                                (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH2HKNNUM(IDZ2)) OR
                                (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH2HKNNUM(IDZ2)) OR
                                (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX)
                                      =  WRK-RCOMB-KOH2HKNNUM(IDZ2))
                                 PERFORM 5701414-SYUKOH-HJO2-SEC
                             END-IF
                           END-IF
                         END-IF
                       END-IF
                     END-PERFORM
                   ELSE
                     EVALUATE  SPA-PREFNUM
                      WHEN       13
                      WHEN       34
                        IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:1)
                                                 >=  "1" AND <=  "8"
                         PERFORM   VARYING   IDZ2  FROM  1   BY  1
                                    UNTIL    IDZ2    >   IDZ3
                          IF  (WRK-RCOMB-KOH1HKNNUM(IDZ2) =
                                          "021" OR "010" OR "011")
                          AND (WRK-RCOMB-KOH2HKNNUM(IDZ2) = "193")
                            IF WRK-RCOMB-KOH2HKNNUM(IDZ2) =
                                         WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)
                              CONTINUE
                            ELSE
                              IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX) =
                                                                  SPACE)
                              OR (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                              OR (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                              OR (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                                 PERFORM 5701414-SYUKOH-HJO2-SEC
                              END-IF
                            END-IF
                          END-IF
                         END-PERFORM
                        END-IF
                      WHEN       27
                        IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:1)
                                                 >=  "1" AND <=  "8"
                         PERFORM   VARYING   IDZ2  FROM  1   BY  1
                                    UNTIL    IDZ2    >   IDZ3
                          IF  (WRK-RCOMB-KOH1HKNNUM(IDZ2) =
                                          "021" OR "010" OR "011")
                          AND (WRK-RCOMB-KOH2HKNNUM(IDZ2) = "121")
                            IF WRK-RCOMB-KOH2HKNNUM(IDZ2) =
                                         WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)
                              CONTINUE
                            ELSE
                              IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX) =
                                                                  SPACE)
                              OR (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                              OR (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                              OR (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX) =
                                             WRK-RCOMB-KOH2HKNNUM(IDZ2))
                                 PERFORM 5701414-SYUKOH-HJO2-SEC
                              END-IF
                            END-IF
                          END-IF
                         END-PERFORM
                        END-IF
                     END-EVALUATE
                   END-IF
                 END-PERFORM
      *          上で作成した組合せに、省庁公費を追加した組合せを作成する。
      *          （公費＋公費＋補助公費＋省庁公費）
                 MOVE   WRK-RCOMB-MAX  TO   IDZ3
                 PERFORM     VARYING   IDZ   FROM  1   BY  1
                             UNTIL     IDZ   >     WRK-SRT-HJO-CNT
                   MOVE    WRK-SRT-HJO-IDX (IDZ)
                                       TO   WRK-COMB-HJOIDX
                   IF  WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:2) =   "99"
                     PERFORM   VARYING   IDZ2  FROM  1   BY  1
                               UNTIL     IDZ2  >     IDZ3
                       IF  (WRK-RCOMB-KOH1HKNNUM(IDZ2)  NOT = "027" AND
                            WRK-RCOMB-KOH1HKNNUM(IDZ2)  NOT = SPACE AND
                            WRK-RCOMB-KOH2HKNNUM(IDZ2)  NOT = SPACE AND
                            WRK-RCOMB-KOH3HKNNUM(IDZ2)  NOT = SPACE)
                         IF   WRK-RCOMB-KOH3HKNNUM(IDZ2)(1:2) = "99"
                            CONTINUE
                         ELSE
                           IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                    =                       SPACE) OR
                              (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                    =  WRK-RCOMB-KOH3HKNNUM(IDZ2)) OR
                              (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX)
                                    =  WRK-RCOMB-KOH3HKNNUM(IDZ2)) OR
                              (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX)
                                    =  WRK-RCOMB-KOH3HKNNUM(IDZ2))
                               PERFORM 5701414-SYUKOH-HJO2-SEC
                           END-IF
                         END-IF
                       END-IF
                     END-PERFORM
                   END-IF
                 END-PERFORM
               ELSE
                 ADD     1                 TO  WRK-RCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                           TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
               END-IF
      *
             WHEN    "2"
      ***>     ADD     1                 TO  WRK-RCOMB-MAX
      ***>     MOVE    WRK-KCOMB-REC (IDX2)
      ***>                               TO  WRK-RCOMB-REC
      ***>                                         (WRK-RCOMB-MAX)
      *
      *        公費表中より補助公費を検索し適用開始日順に
      *                                   インデックス取得
               MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                           TO  WRK-SRT-STYMD
               MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                           TO  WRK-SRT-EDYMD
               IF  WRK-KCOMB-KOH1-HKNKOHSBTKBN (IDX2)  =   "3"
                   MOVE    WRK-KCOMB-KOH2HKNNUM (IDX2)
                                           TO  WRK-SRT-HJO-KOHNO
               ELSE
                   MOVE    WRK-KCOMB-KOH1HKNNUM (IDX2)
                                           TO  WRK-SRT-HJO-KOHNO
               END-IF
               PERFORM 5701412-HJOIDX-SORT-SEC
               IF  WRK-SRT-HJO-CNT     >   ZERO
                 ADD     1                 TO  WRK-RCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                           TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
      *          補助公費を検索
                 PERFORM     VARYING   IDZ   FROM  1   BY  1
                             UNTIL     IDZ   >     WRK-SRT-HJO-CNT
                   MOVE    WRK-SRT-HJO-IDX (IDZ)
                                             TO  WRK-COMB-HJOIDX
                   IF   WRK-KOH-HKNNUM(WRK-COMB-HJOIDX)(1:2) = "99"
                     IF   WRK-SRT-HJO-KOHNO(1:2) = "99"
                       CONTINUE
                     ELSE
                       IF (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                         =               SPACE)  OR
                          (WRK-KOH-GENHKNNUM1(WRK-COMB-HJOIDX)
                                         =   WRK-SRT-HJO-KOHNO)  OR
                          (WRK-KOH-GENHKNNUM2(WRK-COMB-HJOIDX)
                                         =   WRK-SRT-HJO-KOHNO)  OR
                          (WRK-KOH-GENHKNNUM3(WRK-COMB-HJOIDX)
                                         =   WRK-SRT-HJO-KOHNO)
                            PERFORM 5701413-SYUKOH-HJO-SEC
                       END-IF
                     END-IF
                   END-IF
                 END-PERFORM
               ELSE
                 ADD     1                 TO  WRK-RCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                           TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
               END-IF
      *
             END-EVALUATE
           END-PERFORM
      *
           MOVE    ZERO                 TO  WRK-KCOMB-MAX
           INITIALIZE                       WRK-KCOMB-AREA
           MOVE    WRK-LCOMB-MAX-O       TO  WRK-LCOMB-MAX
      *    [編集後テーブル]より転送
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       WRK-RCOMB-MAX )
               ADD     1               TO  WRK-LCOMB-MAX
               MOVE    WRK-RCOMB-REC (IDZ)
                                   TO  WRK-LCOMB-REC (WRK-LCOMB-MAX)
               MOVE    WRK-LCOMB-MAX    TO  IDX2
               PERFORM 57012-FUTAN-SET-SEC
      *        後に補助公費をセットするため再転送
               ADD     1           TO  WRK-KCOMB-MAX
               MOVE    WRK-LCOMB-REC (WRK-LCOMB-MAX)
                                   TO  WRK-KCOMB-REC (WRK-KCOMB-MAX)
           END-PERFORM
      **  -----------------------------------------------------------
      *
           INITIALIZE                WRK-RCOMB-AREA
           MOVE    ZERO              TO  WRK-RCOMB-MAX
      *    再度元テーブルより生保　付加
           PERFORM     VARYING   IDX2  FROM    1   BY  1
                       UNTIL   ( IDX2  >       200 )
                       OR      ( IDX2  >       WRK-KCOMB-MAX )
      *
             INITIALIZE                WRK-IDXSRT-AREA
      *
      *      生保を付加する対象かどうかの判定
      *        1:生保つける  2:生保つけない
             MOVE    SPACE             TO  WRK-FTN-FLG
             MOVE    SPACE             TO  WRK-FTNKBN-AREA
             INITIALIZE                    WRK-FTNKBN-AREA
             MOVE    2                 TO  FLG-HANTEI
             PERFORM 5701411-FUKA-HANTEI-SEC
      *
             EVALUATE    WRK-FTN-FLG
             WHEN    "1"
             WHEN    "2"
                 ADD     1                 TO  WRK-RCOMB-MAX
                 MOVE    WRK-KCOMB-REC (IDX2)
                                           TO  WRK-RCOMB-REC
                                                     (WRK-RCOMB-MAX)
                 IF  WRK-FTN-FLG           =   "1"
                   IF (WRK-KCOMB-HKNNUM (IDX2)        =   "060"
                                                      OR  "039")
                   OR (WRK-KCOMB-KOH4HKNNUM(IDX2) NOT =   SPACE)
                       CONTINUE
                   ELSE
      *                公費表中より生保を検索し、
      *                               適用開始日順にインデックス取得
                       MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                           TO  WRK-SRT-STYMD
                       MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                           TO  WRK-SRT-EDYMD
                       PERFORM 570111-SEHIDX-SORT-SEC
                   END-IF
                   IF  WRK-SRT-SEH-CNT       >   ZERO
      *                生保を検索
                     PERFORM     VARYING   IDZ   FROM  1   BY  1
                                 UNTIL     IDZ   >     WRK-SRT-SEH-CNT
                       IF   WRK-RCOMB-MAX  >=  200
                         MOVE    200    TO  IDZ
                       ELSE
                         MOVE    WRK-SRT-SEH-IDX(IDZ)
                                        TO  WRK-COMB-IDX
                         PERFORM 570144-FUTAN-KONZAI-SEH-SEC
                       END-IF
                     END-PERFORM
                   END-IF
                 END-IF
             END-EVALUATE
           END-PERFORM
      *
      *----(01.00.03) LINE ADD START --------------------------------
           INITIALIZE                      WRK-KCOMB-AREA
           MOVE    ZERO                TO  WRK-KCOMB-MAX
      *----(01.00.03) LINE ADD END   --------------------------------
           MOVE    WRK-LCOMB-MAX-O     TO  WRK-LCOMB-MAX
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       WRK-RCOMB-MAX )
               ADD     1               TO  WRK-LCOMB-MAX
               MOVE    WRK-RCOMB-REC (IDZ)
                                       TO  WRK-LCOMB-REC(WRK-LCOMB-MAX)
               MOVE    WRK-LCOMB-MAX   TO  IDX2
               PERFORM 57012-FUTAN-SET-SEC
      *----(01.00.03) LINE ADD START --------------------------------
               ADD     1               TO  WRK-KCOMB-MAX
               MOVE    WRK-LCOMB-REC (WRK-LCOMB-MAX)
                                       TO  WRK-KCOMB-REC(WRK-KCOMB-MAX)
      *----(01.00.03) LINE ADD END   --------------------------------
           END-PERFORM
      *
      *----(01.00.03) LINE ADD START --------------------------------
      *    長期高額が存在する場合、長期高額編集
           INITIALIZE                      WRK-RCOMB-AREA
           MOVE    ZERO                TO  WRK-RCOMB-MAX
      *    再度元テーブルより長期　付加
           PERFORM     VARYING   IDX2  FROM    1   BY  1
                       UNTIL   ( IDX2  >       200 )
                       OR      ( IDX2  >       WRK-KCOMB-MAX )
               INITIALIZE                  WRK-IDXSRT-AREA
               ADD     1               TO  WRK-RCOMB-MAX
               MOVE    WRK-KCOMB-REC (IDX2)
                                       TO  WRK-RCOMB-REC(WRK-RCOMB-MAX)
      *        生保がある場合、長期はつけない
               IF  (WRK-KCOMB-KOH1-HKNKOHSBTKBN(IDX2)  =   "5") OR
                   (WRK-KCOMB-KOH2-HKNKOHSBTKBN(IDX2)  =   "5") OR
                   (WRK-KCOMB-KOH3-HKNKOHSBTKBN(IDX2)  =   "5") OR
                   (WRK-KCOMB-HKNNUM(IDX2)             = SPACE) OR
                   (WRK-KCOMB-KOH4HKNNUM(IDX2)     NOT = SPACE)
                   CONTINUE
               ELSE
      *          更生医療・小児特定・マル福がある場合、長期をつける
                 IF (( WRK-KCOMB-KOH1HKNNUM(IDX2)  = 
                       "010" OR "011" OR "015" OR "016" OR "019" OR
                       "020" OR "021" OR "024" OR "038" OR "051" OR
                       "052" OR "079" OR "091" OR "054" OR "092"   ) OR
                     ((WRK-KCOMB-KOH1HKNNUM(IDX2) >=  "100")  AND
                      (WRK-KCOMB-KOH1HKNNUM(IDX2) <=  "899")       ) OR
                     ( WRK-KCOMB-KOH1HKNNUM(IDX2)(1:2)  =  "99"    )) OR
                    (( WRK-KCOMB-KOH2HKNNUM(IDX2)  =
                       "010" OR "011" OR "015" OR "016" OR "019" OR
                       "020" OR "021" OR "024" OR "038" OR "051" OR
                       "052" OR "079" OR "091" OR "054" OR "092"   ) OR
                     ((WRK-KCOMB-KOH2HKNNUM(IDX2) >=  "100")  AND
                      (WRK-KCOMB-KOH2HKNNUM(IDX2) <=  "899")       ) OR
                     ( WRK-KCOMB-KOH2HKNNUM(IDX2)(1:2)  =  "99"    )) OR
                    (( WRK-KCOMB-KOH3HKNNUM(IDX2)  =
                       "010" OR "011" OR "015" OR "016" OR "019" OR
                       "020" OR "021" OR "024" OR "038" OR "051" OR
                       "052" OR "079" OR "091" OR "054" OR "092"   ) OR
                     ((WRK-KCOMB-KOH3HKNNUM(IDX2) >=  "100")  AND
                      (WRK-KCOMB-KOH3HKNNUM(IDX2) <=  "899")       ) OR
                     ( WRK-KCOMB-KOH3HKNNUM(IDX2)(1:2)  =  "99"    ))   
      *
                   IF ((WRK-KCOMB-KOH1HKNNUM(IDX2)  NOT =
                        "027" AND "010" AND "011" AND "015" AND
                        "016" AND "019" AND "020" AND "021" AND
                        "024" AND "038" AND "051" AND "052" AND
                        "079" AND "091" AND "054" AND "092"     ) AND
                       (WRK-KCOMB-KOH1HKNNUM(IDX2)(1:1) =  "0"  ))  OR
                      ((WRK-KCOMB-KOH2HKNNUM(IDX2)  NOT =
                        "027" AND "010" AND "011" AND "015" AND
                        "016" AND "019" AND "020" AND "021" AND
                        "024" AND "038" AND "051" AND "052" AND
                        "079" AND "091" AND "054" AND "092"     ) AND
                       (WRK-KCOMB-KOH2HKNNUM(IDX2)(1:1) =  "0"  ))  OR
                      ((WRK-KCOMB-KOH3HKNNUM(IDX2)  NOT =
                        "027" AND "010" AND "011" AND "015" AND
                        "016" AND "019" AND "020" AND "021" AND
                        "024" AND "038" AND "051" AND "052" AND
                        "079" AND "091" AND "054" AND "092"     ) AND
                       (WRK-KCOMB-KOH3HKNNUM(IDX2)(1:1) =  "0"  ))
                     CONTINUE
                   ELSE
      *              公費表中より長期高額を検索し、
      *                            適用開始日順にインデックス取得
                     MOVE    WRK-KCOMB-TEKSTYMD (IDX2)
                                               TO  WRK-SRT-STYMD
                     MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                               TO  WRK-SRT-EDYMD
                     PERFORM 570113-CHOKIIDX-SORT-SEC
                     IF  WRK-SRT-CHO-CNT       >   ZERO
      *                  長期高額を検索
                         PERFORM   VARYING   IDZ   FROM    1   BY  1
                                   UNTIL     IDZ   >   WRK-SRT-CHO-CNT
                           IF   WRK-RCOMB-MAX  >=  200
                             MOVE    200    TO  IDZ
                           ELSE
                             MOVE    WRK-SRT-CHO-IDX(IDZ)
                                            TO  WRK-COMB-IDX
                             PERFORM 570144-FUTAN-KONZAI-SEH-SEC
                           END-IF
                         END-PERFORM
                     END-IF
                   END-IF
                 END-IF
               END-IF
           END-PERFORM
      *
           MOVE    WRK-LCOMB-MAX-O     TO  WRK-LCOMB-MAX
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       WRK-RCOMB-MAX )
               ADD     1               TO  WRK-LCOMB-MAX
               MOVE    WRK-RCOMB-REC (IDZ)
                                       TO  WRK-LCOMB-REC(WRK-LCOMB-MAX)
      ************
      *        DISPLAY "WRK-LCOMB-MAX=" WRK-LCOMB-MAX
      *        DISPLAY "WRK-LCOMB-KOH1HKNNUM(WRK-LCOMB-MAX)="
      *                   WRK-LCOMB-KOH1HKNNUM(WRK-LCOMB-MAX)
      *        DISPLAY "WRK-LCOMB-KOH2HKNNUM(WRK-LCOMB-MAX)="
      *                   WRK-LCOMB-KOH2HKNNUM(WRK-LCOMB-MAX)
      *        DISPLAY "WRK-LCOMB-KOH3HKNNUM(WRK-LCOMB-MAX)="
      *                   WRK-LCOMB-KOH3HKNNUM(WRK-LCOMB-MAX)
      *        DISPLAY "WRK-LCOMB-KOH4HKNNUM(WRK-LCOMB-MAX)="
      *                   WRK-LCOMB-KOH4HKNNUM(WRK-LCOMB-MAX)
      **************
               MOVE    WRK-LCOMB-MAX   TO  IDX2
               PERFORM 57012-FUTAN-SET-SEC
           END-PERFORM
      *----(01.00.03) LINE ADD END   --------------------------------
      *
           PERFORM     57010-NC01PARA-OUT-SEC
      *
           .
       5701410-SYUKOH-KYOUTU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主公費−補助　組合せ作成処理
      *****************************************************************
       5701413-SYUKOH-HJO-SEC       SECTION.
      *
           ADD     1                    TO  WRK-RCOMB-MAX
           MOVE    WRK-RCOMB-MAX        TO  IDX1
           MOVE    WRK-KCOMB-REC (IDX2) TO  WRK-RCOMB-REC  (IDX1)
      *    [補助 開始日]≦[組合せ 開始日]
           IF  WRK-KOH-TEKSTYMD (WRK-COMB-HJOIDX)
                                    <=  WRK-KCOMB-TEKSTYMD (IDX2)
      *        [補助 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *            ●[組合せ] 開始 <---> [補助] 終了日
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKEDYMD (IDX1)
               ELSE
      *            [組合せ 終了日]≦[補助 終了日]
                   CONTINUE
               END-IF
           ELSE
      *        [組合せ 開始日]＜[補助 開始日]
      *        ●[補助] 開始 <---> [組合せ] 終了日
               MOVE    WRK-KOH-TEKSTYMD   (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKSTYMD    (IDX1)
      *        [補助 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *            ●[補助] 開始 <---> [補助] 終了日に変更
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
               ELSE
                   CONTINUE
               END-IF
           END-IF
           PERFORM 5701413-HJO-IREKAE-SEC
           .
       5701413-SYUKOH-HJO-EXT.
           EXIT.
      *
      *****************************************************************
      *    主公費−補助　組合せ作成処理
      *****************************************************************
       5701414-SYUKOH-HJO2-SEC       SECTION.
      *
      *    [補助 開始日]≦[組合せ 開始日]
           IF    WRK-KOH-TEKSTYMD(WRK-COMB-HJOIDX)
                                    <=  WRK-RCOMB-TEKSTYMD(IDZ2)
             IF  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    <   WRK-RCOMB-TEKSTYMD(IDZ2)
               CONTINUE
             ELSE
               ADD     1                    TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX        TO  IDX1
               MOVE    WRK-RCOMB-REC(IDZ2)  TO  WRK-RCOMB-REC(IDX1)
      *        [補助 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    <   WRK-RCOMB-TEKEDYMD(IDZ2)
                 MOVE  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKEDYMD(IDX1)
               END-IF
               PERFORM 5701413-HJO-IREKAE-SEC
             END-IF
           ELSE
      *      [組合せ 開始日]＜[補助 開始日]
             IF  WRK-KOH-TEKSTYMD(WRK-COMB-HJOIDX)
                                    >   WRK-RCOMB-TEKEDYMD(IDZ2)
               CONTINUE
             ELSE
               ADD     1                    TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX        TO  IDX1
               MOVE    WRK-RCOMB-REC(IDZ2)  TO  WRK-RCOMB-REC(IDX1)
      *
               MOVE    WRK-KOH-TEKSTYMD(WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKSTYMD(IDX1)
      *        [補助 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    <   WRK-RCOMB-TEKEDYMD(IDZ2)
                 MOVE  WRK-KOH-TEKEDYMD(WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-TEKEDYMD(IDX1)
               END-IF
               PERFORM 5701413-HJO-IREKAE-SEC
             END-IF
           END-IF
           .
       5701414-SYUKOH-HJO2-EXT.
           EXIT.
      *
      *****************************************************************
      *    補助公費付加　公費１〜４入替処理
      *****************************************************************
       5701413-HJO-IREKAE-SEC       SECTION.
      *
           IF  WRK-RCOMB-KOH1HKNNUM  (IDX1)   =   SPACE
              MOVE    WRK-KOH-KOHID         (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH1ID      (IDX1)
              MOVE    WRK-KOH-HKNNUM         (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH1HKNNUM   (IDX1)
              MOVE    WRK-KOH-PAYKBN         (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH1PAYKBN   (IDX1)
              MOVE    WRK-KOH-SEIDO     (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
      *       ------------------------------------
              MOVE    WRK-KOH-SBT       (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
           ELSE
              IF  WRK-RCOMB-KOH2HKNNUM (IDX1)  =   SPACE
                  MOVE    WRK-KOH-KOHID     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH2ID      (IDX1)
                  MOVE    WRK-KOH-HKNNUM     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH2HKNNUM   (IDX1)
                  MOVE    WRK-KOH-PAYKBN     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH2PAYKBN   (IDX1)
                  MOVE    WRK-KOH-SEIDO (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH2-TANSEIDONAME(IDX1)
      *           ------------------------------------
                  MOVE    WRK-KOH-SBT   (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH2-HKNKOHSBTKBN(IDX1)
              ELSE
                  IF  WRK-RCOMB-KOH3HKNNUM (IDX1)  =   SPACE
                      MOVE    WRK-KOH-KOHID     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH3ID      (IDX1)
                      MOVE    WRK-KOH-HKNNUM     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH3HKNNUM   (IDX1)
                      MOVE    WRK-KOH-PAYKBN     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH3PAYKBN   (IDX1)
                      MOVE    WRK-KOH-SEIDO (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH3-TANSEIDONAME(IDX1)
      *               ------------------------------------
                      MOVE    WRK-KOH-SBT   (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH3-HKNKOHSBTKBN(IDX1)
                  ELSE
                    IF  WRK-RCOMB-KOH4HKNNUM (IDX1)  =   SPACE
                      MOVE    WRK-KOH-KOHID     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH4ID      (IDX1)
                      MOVE    WRK-KOH-HKNNUM     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH4HKNNUM   (IDX1)
                      MOVE    WRK-KOH-PAYKBN     (WRK-COMB-HJOIDX)
                                    TO  WRK-RCOMB-KOH4PAYKBN   (IDX1)
                      MOVE    WRK-KOH-SEIDO (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH4-TANSEIDONAME(IDX1)
      *               ------------------------------------
                      MOVE    WRK-KOH-SBT   (WRK-COMB-HJOIDX)
                                  TO  WRK-RCOMB-KOH4-HKNKOHSBTKBN(IDX1)
                    ELSE
                      DISPLAY "IREKAE NASI"
                    END-IF
                  END-IF
              END-IF
           END-IF
           .
       5701413-HJO-IREKAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    負担混在時処理（生保/長期）
      *****************************************************************
       570144-FUTAN-KONZAI-SEH-SEC       SECTION.
      *
      *    [生保 開始日]≦[組合せ 開始日]
           IF  WRK-KOH-TEKSTYMD(WRK-COMB-IDX)
                                    <=  WRK-KCOMB-TEKSTYMD (IDX2)
               ADD     1                TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX    TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                        TO  WRK-RCOMB-REC  (IDX1)
      *        [生保 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-IDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *            ●[組合せ] 開始 <---> [生保] 終了日
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-TEKEDYMD (IDX1)
               END-IF
               PERFORM 5701121-KOH-IREKAE-SEC
           ELSE
      *      [組合せ 開始日]＜[生保 開始日]
               ADD     1                TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX    TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                        TO  WRK-RCOMB-REC  (IDX1)
      *        ●[生保] 開始 <---
               MOVE    WRK-KOH-TEKSTYMD   (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-TEKSTYMD    (IDX1)
      *        [生保 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-IDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *          ● ---> [生保] 終了日
                 MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-IDX)
                                    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
               END-IF
               PERFORM 5701121-KOH-IREKAE-SEC
           END-IF
      *
           .
       570144-FUTAN-KONZAI-SEH-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険存在しない期間取得処理
      *****************************************************************
       570145-SYUHKNNASI-HEN-SEC       SECTION.
      *
      *    [主保険 開始日]≦[組合せ 開始日]
           IF  WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)
                                      <=  WRK-KCOMB-TEKSTYMD (IDX2)
      *      [組合せ 終了日]≦[主保険 終了日]
             IF  WRK-KCOMB-TEKEDYMD (IDX2)
                                      <=  WRK-HKN-TEKEDYMD
                                                   (WRK-COMB-SYUHOIDX)
      ***        MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
      ***                             TO  WRK-KCOMB-TEKSTYMD (IDX2)
                 MOVE    "99999999"   TO  WRK-KCOMB-TEKSTYMD (IDX2)
                 MOVE    "00000000"   TO  WRK-KCOMB-TEKEDYMD (IDX2)
             ELSE
      *  　　    [主保険 終了日]≦[組合せ 終了日]
      *          ○主保険終了日の翌日取得
                 INITIALIZE              STS-AREA-DAY
                 INITIALIZE              LNK-DAY6-AREA
                 MOVE    "61"            TO  LNK-DAY6-IRAI
                 MOVE    WRK-HKN-TEKEDYMD (WRK-COMB-SYUHOIDX)
                                         TO  LNK-DAY6-KIJUN
                 MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                 MOVE     1              TO  LNK-DAY6-ZOGEN
                 PERFORM 4221-YMD-GET-SEC
                 MOVE    WRK-SYMD        TO  WRK-KCOMB-TEKSTYMD (IDX2)
             END-IF
           ELSE
      *      [組合せ 開始日]＜[主保険 開始日]
             ADD     1                   TO  WRK-RCOMB-MAX
             MOVE    WRK-KCOMB-REC(IDX2) TO  WRK-RCOMB-REC
                                                  (WRK-RCOMB-MAX)
      *      ○主保険開始日の前日取得
             INITIALIZE              STS-AREA-DAY
             INITIALIZE              LNK-DAY6-AREA
             MOVE    "61"            TO  LNK-DAY6-IRAI
             MOVE    WRK-HKN-TEKSTYMD (WRK-COMB-SYUHOIDX)
                                     TO  LNK-DAY6-KIJUN
             MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
             MOVE    -1              TO  LNK-DAY6-ZOGEN
             PERFORM 4221-YMD-GET-SEC
             MOVE    WRK-SYMD        TO  WRK-RCOMB-TEKEDYMD
                                                  (WRK-RCOMB-MAX)
             MOVE    WRK-HKN-TEKSTYMD (WRK-COMB-SYUHOIDX)
                                     TO  WRK-KCOMB-TEKSTYMD (IDX2)
      *      [組合せ 終了日]≦[主保険 終了日]
             IF  WRK-KCOMB-TEKEDYMD (IDX2)
                                     <=  WRK-HKN-TEKEDYMD
                                                  (WRK-COMB-SYUHOIDX)
      ***        MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
      ***                            TO  WRK-KCOMB-TEKSTYMD (IDX2)
                 MOVE    "99999999"  TO  WRK-KCOMB-TEKSTYMD (IDX2)
                 MOVE    "00000000"  TO  WRK-KCOMB-TEKEDYMD (IDX2)
             ELSE
      *          [主保険 終了日]≦[組合せ 終了日]
      *          ○主保険終了日の翌日取得
                 INITIALIZE                  STS-AREA-DAY
                 INITIALIZE                  LNK-DAY6-AREA
                 MOVE    "61"            TO  LNK-DAY6-IRAI
                 MOVE    WRK-HKN-TEKEDYMD (WRK-COMB-SYUHOIDX)
                                         TO  LNK-DAY6-KIJUN
                 MOVE    "1"             TO  LNK-DAY6-ZOGENPTN
                 MOVE     1              TO  LNK-DAY6-ZOGEN
                 PERFORM 4221-YMD-GET-SEC
                 MOVE    WRK-SYMD        TO  WRK-KCOMB-TEKSTYMD(IDX2)
             END-IF
           END-IF
           .
       570145-SYUHONASI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保付加判定処理
      *****************************************************************
       5701411-FUKA-HANTEI-SEC       SECTION.
      *
      *    <通常>
           IF  WRK-KCOMB-GAI-FTNKBN(IDX2)        =   "0"
               MOVE    "0"              TO  WRK-GAI-KBN (1)
           ELSE
             IF  (WRK-KCOMB-GAI-FTNKBN   (IDX2)  =   "2")  AND
                 (WRK-KCOMB-GAI-YKZFTNKBN(IDX2)  =   "0"  OR  "3")
                 MOVE    "2"            TO  WRK-GAI-KBN (1)
             ELSE
                 MOVE    "1"            TO  WRK-GAI-KBN (1)
             END-IF
           END-IF
           IF  WRK-KCOMB-NYU-FTNKBN (IDX2)       =   "0"
               MOVE    "0"              TO  WRK-NYU-KBN (1)
           ELSE
             IF  (WRK-KCOMB-NYU-FTNKBN    (IDX2) =   "2")  AND
                 (WRK-KCOMB-NYU-SKJRYOKBN (IDX2) =   "0"  OR  "3")
                 MOVE    "2"            TO  WRK-NYU-KBN (1)
             ELSE
                 MOVE    "1"            TO  WRK-NYU-KBN (1)
             END-IF
           END-IF
      *    <低所得>
           IF  WRK-KCOMB-TSY-GAIFTNKBN (IDX2)    =   "0"
               MOVE    "0"              TO  WRK-GAI-KBN (2)
           ELSE
             IF  (WRK-KCOMB-TSY-GAIFTNKBN   (IDX2)   =  "2")  AND
                 (WRK-KCOMB-TSY-GAIYKZFTNKBN(IDX2)   =  "0" OR "3")
                 MOVE    "2"            TO  WRK-GAI-KBN (2)
             ELSE
                 MOVE    "1"            TO  WRK-GAI-KBN (2)
             END-IF
           END-IF
           IF  WRK-KCOMB-TSY-NYUFTNKBN (IDX2)    =   "0"
               MOVE    "0"              TO  WRK-NYU-KBN (2)
           ELSE
             IF  (WRK-KCOMB-TSY-NYUFTNKBN    (IDX2) =   "2")  AND
                 (WRK-KCOMB-TSY-NYUSKJRYOKBN (IDX2) =   "0" OR "3")
                 MOVE    "2"            TO  WRK-NYU-KBN (2)
             ELSE
                 MOVE    "1"            TO  WRK-NYU-KBN (2)
             END-IF
           END-IF
      *    <低年金>
           IF  WRK-KCOMB-TNK-GAIFTNKBN (IDX2)    =   "0"
               MOVE    "0"              TO  WRK-GAI-KBN (3)
           ELSE
             IF  (WRK-KCOMB-TNK-GAIFTNKBN   (IDX2)  =  "2")  AND
                 (WRK-KCOMB-TNK-GAIYKZFTNKBN(IDX2)  =  "0" OR "3")
                 MOVE    "2"            TO  WRK-GAI-KBN (3)
             ELSE
                 MOVE    "1"            TO  WRK-GAI-KBN (3)
             END-IF
           END-IF
           IF  WRK-KCOMB-TNK-NYUFTNKBN (IDX2)    =   "0"
               MOVE    "0"              TO  WRK-NYU-KBN (3)
           ELSE
             IF  (WRK-KCOMB-TNK-NYUFTNKBN    (IDX2) =   "2")  AND
                 (WRK-KCOMB-TNK-NYUSKJRYOKBN (IDX2) =   "0" OR "3")
                 MOVE    "2"            TO  WRK-NYU-KBN (3)
             ELSE
                 MOVE    "1"            TO  WRK-NYU-KBN (3)
             END-IF
           END-IF
      *
      *    <判定>
           IF  (WRK-GAI-KBN (1)     NOT =   "2")  AND
               (WRK-GAI-KBN (2)     NOT =   "2")  AND
               (WRK-GAI-KBN (3)     NOT =   "2")  AND
               (WRK-NYU-KBN (1)     NOT =   "2")  AND
               (WRK-NYU-KBN (2)     NOT =   "2")  AND
               (WRK-NYU-KBN (3)     NOT =   "2")
                 MOVE    "1"            TO  WRK-FTN-FLG
           ELSE
             IF  (WRK-GAI-KBN (1)     NOT =   "1")  AND
                 (WRK-GAI-KBN (2)     NOT =   "1")  AND
                 (WRK-GAI-KBN (3)     NOT =   "1")  AND
                 (WRK-NYU-KBN (1)     NOT =   "1")  AND
                 (WRK-NYU-KBN (2)     NOT =   "1")  AND
                 (WRK-NYU-KBN (3)     NOT =   "1")
                 MOVE    "2"            TO  WRK-FTN-FLG
             ELSE
                 MOVE    "1"            TO  WRK-FTN-FLG
             END-IF
           END-IF
      *
           IF    FLG-HANTEI   =    1
             IF   SPA-PREFNUM  =  42
               IF  ((WRK-KCOMB-KOH1-HKNKOHSBTKBN (IDX2)  =  "3" ) AND
                    (WRK-KCOMB-KOH2HKNNUM (IDX2)         = "186"))
               OR  ( WRK-KCOMB-KOH1HKNNUM (IDX2)         = "186" )
                   MOVE    "1"            TO  WRK-FTN-FLG
               END-IF
             END-IF
      *
             IF  ((WRK-KCOMB-KOH1-HKNKOHSBTKBN (IDX2)  =  "3" ) AND
                  (WRK-KCOMB-KOH2HKNNUM (IDX2)
                        = "020" OR "091" OR "019" OR
                          "053" OR "011" OR "062"             ))
             OR  ( WRK-KCOMB-KOH1HKNNUM (IDX2)
                        = "020" OR "091" OR "019" OR
                          "053" OR "011" OR "062"              )
                   MOVE    "1"            TO  WRK-FTN-FLG
             END-IF
           END-IF
      *
           IF    FLG-HANTEI   =    2
             IF    WRK-KCOMB-TEKSTYMD(IDX2)(1:6) >= "200604"
               IF  ((WRK-KCOMB-KOH1-HKNKOHSBTKBN (IDX2)  =  "3"  ) AND
                    (WRK-KCOMB-KOH2HKNNUM (IDX2) = "021" OR "015"
                                                         OR "016"))
               OR  ( WRK-KCOMB-KOH1HKNNUM (IDX2) = "021" OR "015"
                                                         OR "016" )
                 IF  WRK-KCOMB-HKNNUM (IDX2)     =  SPACE
      *            主保険がない場合は、
      *            自立支援＋生保の組合せを作成しない
      *            （「難病等＋生保の組合せ作成」にチェックがある
      *              場合は、作成する）
                   IF  SPA-NAI-COMBKBN  =  "1"
                     CONTINUE
                   ELSE
                     MOVE    "2"            TO  WRK-FTN-FLG
                   END-IF
                 END-IF
               END-IF
             END-IF
      *
             IF    WRK-KCOMB-TEKSTYMD(IDX2)(1:6) >= "201501"
               IF    WRK-KCOMB-KOH1HKNNUM (IDX2) = "054" OR "052"
                 IF  WRK-KCOMB-HKNNUM (IDX2)     =  SPACE
      *            主保険がない場合は、
      *            難病＋生保、小児慢性＋生保の組合せを作成しない
      *            （「難病等＋生保の組合せ作成」にチェックがある
      *              場合は、作成する）
                   IF  SPA-NAI-COMBKBN  =  "1"
                     CONTINUE
                   ELSE
                     MOVE    "2"            TO  WRK-FTN-FLG
                   END-IF
                 END-IF
               END-IF
             END-IF
      *
             IF    WRK-KCOMB-TEKSTYMD(IDX2)(1:6) >= "200804"
               IF    WRK-KCOMB-KOH1HKNNUM (IDX2) = "091"
                     MOVE    "1"            TO  WRK-FTN-FLG
               END-IF
             END-IF
           END-IF
      *
           .
       5701411-FUKA-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人インデックスＳＯＲＴ処理
      *****************************************************************
       570143-RJNIDX-SORT-SEC       SECTION.
      *
      *    === 保険表より呼ばれた ===
           IF  WRK-SRT-FLG       =   "1"
      *      公費表中より老人を適用開始日の早い順にインデックス設定
             PERFORM     VARYING   IDZ   FROM    1   BY  1
                         UNTIL   ( IDZ   >       200 )
                         OR      ( IDZ   >       CNT-KOH )
               IF  WRK-KOH-SAKJOKBN (IDZ)  =   SPACE
      *          [公費]適用年月日と[組合せ]適用年月日
                 IF  (WRK-KOH-TEKEDYMD(IDZ)
                                         <   WRK-SRT-STYMD)   OR
                     (WRK-KOH-TEKSTYMD(IDZ)
                                         >   WRK-SRT-EDYMD)
                   CONTINUE
                 ELSE
      *            [老人]
                   IF  WRK-KOH-SBT(IDZ)  =   "3"
                     IF  WRK-SRT-RJN-CNT   =   ZERO
                         ADD     1           TO  WRK-SRT-RJN-CNT
                         MOVE    IDZ         TO  WRK-SRT-RJN-IDX (1)
                     ELSE
                       MOVE    ZERO        TO  WRK-SRT-RJN-CNT2
                       PERFORM   VARYING   IDY FROM  1   BY  1
                                 UNTIL     IDY >   WRK-SRT-RJN-CNT
      *
                         MOVE    WRK-SRT-RJN-IDX (IDY)   TO  IDX1
                         IF  WRK-KOH-TEKSTYMD(IDZ)
                                       <   WRK-KOH-TEKSTYMD(IDX1)
                           PERFORM   VARYING   WRK-SRT-IDX
                                     FROM      WRK-SRT-RJN-CNT
                                     BY  -1
                                     UNTIL  WRK-SRT-IDX   <   IDY
                             COMPUTE WRK-SRT-RJN-CNT2
                                     =   WRK-SRT-IDX    +   1
                             MOVE    WRK-SRT-RJN-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-RJN-IDX
                                                   (WRK-SRT-RJN-CNT2)
                           END-PERFORM
                           MOVE    IDZ    TO  WRK-SRT-RJN-IDX (IDY)
                           MOVE    200    TO  IDY
                         END-IF
                       END-PERFORM
                       ADD     1           TO  WRK-SRT-RJN-CNT
                       IF  WRK-SRT-RJN-CNT2  =   ZERO
                           IF      WRK-SRT-RJN-CNT   >   200
                               MOVE    "0015"            TO  SPA-ERRCD
                           ELSE
                               MOVE    IDZ     TO  WRK-SRT-RJN-IDX
                                                   (WRK-SRT-RJN-CNT)
                           END-IF
                       END-IF
                     END-IF
                   END-IF
                 END-IF
               END-IF
             END-PERFORM
           ELSE
      *    === 公費表より呼ばれた ===
      *      公費表中より老人を適用開始日の早い順にインデックス設定
             PERFORM     VARYING   IDZ   FROM    1   BY  1
                         UNTIL   ( IDZ   >       200 )
                         OR      ( IDZ   >       CNT-KOH )
             IF  (IDZ                NOT =   IDX  )  AND
                 (WRK-KOH-SAKJOKBN (IDZ)  =   SPACE)
      *          [公費]適用年月日と[組合せ]適用年月日
               IF  (WRK-KOH-TEKEDYMD(IDZ)
                                         <   WRK-SRT-STYMD)   OR
                   (WRK-KOH-TEKSTYMD(IDZ)
                                         >   WRK-SRT-EDYMD)
                   CONTINUE
               ELSE
      *            [老人]
                   IF  WRK-KOH-SBT(IDZ)  =   "3"
                     IF  WRK-SRT-RJN-CNT   =   ZERO
                         ADD     1           TO  WRK-SRT-RJN-CNT
                         MOVE    IDZ         TO  WRK-SRT-RJN-IDX (1)
                     ELSE
                       MOVE    ZERO        TO  WRK-SRT-RJN-CNT2
                       PERFORM   VARYING   IDY FROM  1   BY  1
                                 UNTIL     IDY >   WRK-SRT-RJN-CNT
      *
                         MOVE    WRK-SRT-RJN-IDX (IDY)   TO  IDX1
                         IF  WRK-KOH-TEKSTYMD(IDZ)
                                       <   WRK-KOH-TEKSTYMD(IDX1)
                           PERFORM   VARYING   WRK-SRT-IDX
                                     FROM      WRK-SRT-RJN-CNT
                                     BY  -1
                                     UNTIL  WRK-SRT-IDX   <   IDY
                             COMPUTE WRK-SRT-RJN-CNT2
                                     =   WRK-SRT-IDX    +   1
                             MOVE    WRK-SRT-RJN-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-RJN-IDX
                                                   (WRK-SRT-RJN-CNT2)
                           END-PERFORM
                           MOVE    IDZ    TO  WRK-SRT-RJN-IDX (IDY)
                           MOVE    200    TO  IDY
                         END-IF
                       END-PERFORM
                       ADD     1           TO  WRK-SRT-RJN-CNT
                       IF  WRK-SRT-RJN-CNT2  =   ZERO
                           IF      WRK-SRT-RJN-CNT   >   200
                               MOVE    "0015"            TO  SPA-ERRCD
                           ELSE
                               MOVE    IDZ     TO  WRK-SRT-RJN-IDX
                                                   (WRK-SRT-RJN-CNT)
                           END-IF
                       END-IF
                     END-IF
                   END-IF
               END-IF
             END-IF
             END-PERFORM
           END-IF
      *
           .
       570143-RJNIDX-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主保険インデックスＳＯＲＴ処理
      *****************************************************************
       570131-HKNIDX-SORT-SEC       SECTION.
      *
      *    保険表中より主保険を適用開始日の早い順にインデックス設定
           PERFORM     VARYING   IDZ   FROM    1   BY  1
                       UNTIL   ( IDZ   >       200 )
                       OR      ( IDZ   >       CNT-HKN )
      *
             IF  ((WRK-HKN-SBT(IDZ)     =   "1"  )  AND
      *!         特別療養費は対象外
                  (WRK-HKN-HKNNUM (IDZ)  NOT =   "068"
                                              AND "040"
                                              AND "069" )) AND
      *!
                 (WRK-HKN-SAKJOKBN (IDZ)     =   SPACE)
      ***        (WRK-HKN-SAKJOKBN (IDZ)     =   SPACE)  AND
      ***        (WRK-HKN-CONTKBN (IDZ)     =   SPACE)
      *
      ******* IF  (WRK-HKN-HKNNUM  (IDZ)  NOT =   "002")   OR
      *          ((WRK-HKN-HKNNUM  (IDZ)      =   "002") AND
      *           (WRK-HKN-HOJOKBN (IDZ)      =   SPACE))
      ******* IF  (WRK-HKN-HKNNUM  (IDZ)  NOT =   "002")   OR
              IF ((WRK-HKN-HKNNUM  (IDZ)      =   "002") AND
                  (WRK-HKN-HOJOKBN (IDZ)  NOT =    SPACE  AND
                                                   "7"    AND
                                                   "8"    AND
                                                   "9" ))  OR
                 ((WRK-HKN-HKNNUM  (IDZ)      =   "031"  OR
                                                  "032"  OR
                                                  "033"  OR
                                                  "034"  )  AND
                  (WRK-HKN-HOJOKBN (IDZ)  NOT =    SPACE  AND
                                                   "7"    AND
                                                   "8"    AND
                                                   "9" ))
                   CONTINUE
               ELSE
      *
      *        [主保険]適用年月日と[生保]適用年月日
               IF  (WRK-HKN-TEKEDYMD(IDZ)
                                    <   WRK-KOH-TEKSTYMD(IDX))  OR
                   (WRK-HKN-TEKSTYMD(IDZ)
                                    >   WRK-KOH-TEKEDYMD(IDX))
                   CONTINUE
               ELSE
                 IF  WRK-SRT-HKN-CNT     =   ZERO
                     ADD     1           TO  WRK-SRT-HKN-CNT
                     MOVE    IDZ         TO  WRK-SRT-HKN-IDX (1)
                 ELSE
                   MOVE    ZERO          TO  WRK-SRT-HKN-CNT2
                   PERFORM   VARYING   IDY   FROM  1   BY  1
                             UNTIL     IDY   >   WRK-SRT-HKN-CNT
      *
                     MOVE    WRK-SRT-HKN-IDX (IDY)   TO  IDX1
                     IF  WRK-HKN-TEKSTYMD(IDZ)
                                       <   WRK-HKN-TEKSTYMD(IDX1)
                       PERFORM   VARYING   WRK-SRT-IDX
                                 FROM      WRK-SRT-HKN-CNT   BY  -1
                                 UNTIL     WRK-SRT-IDX   <   IDY
                         COMPUTE WRK-SRT-HKN-CNT2    =   WRK-SRT-IDX
                                                     +   1
                         MOVE    WRK-SRT-HKN-IDX   (WRK-SRT-IDX)
                                           TO  WRK-SRT-HKN-IDX
                                                   (WRK-SRT-HKN-CNT2)
                       END-PERFORM
                       MOVE    IDZ         TO  WRK-SRT-HKN-IDX (IDY)
                       MOVE    200         TO  IDY
                     END-IF
                   END-PERFORM
                   ADD     1             TO  WRK-SRT-HKN-CNT
                   IF  WRK-SRT-HKN-CNT2  =   ZERO
                       IF      WRK-SRT-HKN-CNT   >   200
                           MOVE    "0015"            TO  SPA-ERRCD
                       ELSE
                           MOVE    IDZ     TO  WRK-SRT-HKN-IDX
                                                   (WRK-SRT-HKN-CNT)
                       END-IF
                   END-IF
                 END-IF
               END-IF
              END-IF
             END-IF
           END-PERFORM
          .
       570131-HKNIDX-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保−主保険　組合せ作成処理
      *****************************************************************
       570132-SEIHO-HKN-HEN-SEC       SECTION.
      *
      *    [保険 開始日]≦[生保 開始日]
           IF  WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)
                                    <=  WRK-KCOMB-TEKSTYMD (IDX2)
      *        [保険 終了日]＜[生保 終了日]
               IF  WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *            ○生保の開始日変更
      *            主保険　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-HKN-TEKEDYMD   (WRK-COMB-SYUHOIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
                   MOVE    WRK-SYMD TO  WRK-KCOMB-TEKSTYMD (IDX2)
               ELSE
      *            [生保 終了日]≦[保険 終了日]
      ***          MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
      ***                           TO  WRK-KCOMB-TEKSTYMD (IDX2)
                   MOVE  "99999999" TO  WRK-KCOMB-TEKSTYMD (IDX2)
                   MOVE  "00000000" TO  WRK-KCOMB-TEKEDYMD (IDX2)
               END-IF
           ELSE
      *        [生保 開始日]＜[保険 開始日]
      *        保険　開始日の前日取得
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-HKN-TEKSTYMD   (WRK-COMB-SYUHOIDX)
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               PERFORM 4221-YMD-GET-SEC
               MOVE    WRK-SYMD            TO  WRK-SYMD6
      *
      *        ●[生保] 開始 <---> [主保険] 開始日の前日
               ADD     1                 TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX     TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC    (IDX1)
               MOVE    WRK-SYMD6    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
      *        ○生保の開始日変更
               MOVE    WRK-HKN-TEKSTYMD (WRK-COMB-SYUHOIDX)
                                    TO  WRK-KCOMB-TEKSTYMD    (IDX2)
      *
      *        [主保険 終了日]＜[生保 終了日]
               IF  WRK-HKN-TEKEDYMD (WRK-COMB-SYUHOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *
      *            主保険　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-HKN-TEKEDYMD   (WRK-COMB-SYUHOIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
      *            ○生保の開始日変更
                   MOVE    WRK-SYMD  TO  WRK-KCOMB-TEKSTYMD (IDX2)
               ELSE
      *            [生保 終了日]≦[主保険 終了日]
      *            ○生保の開始日変更
      ***          MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
      ***                            TO  WRK-KCOMB-TEKSTYMD (IDX2)
                   MOVE   "99999999" TO  WRK-KCOMB-TEKSTYMD (IDX2)
                   MOVE   "00000000" TO  WRK-KCOMB-TEKEDYMD (IDX2)
               END-IF
           END-IF
           .
       570132-SEIHO-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保−主保険　組合せ作成処理
      *****************************************************************
       570132X-SEIHO-HKN-HEN-SEC       SECTION.
      *
      *    [保険 開始日]≦[生保 開始日]
           IF  WRK-HKN-TEKSTYMD(WRK-COMB-SYUHOIDX)
                                    <=  WRK-KCOMB-TEKSTYMD (IDX2)
      *        [保険 終了日]＜[生保 終了日]
               IF  WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *@@
                   ADD     1               TO  WRK-RCOMB-MAX
                   MOVE    WRK-RCOMB-MAX   TO  IDX1
                   MOVE    WRK-KCOMB-REC(IDX2)
                                           TO  WRK-RCOMB-REC(IDX1)
                   MOVE    WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                           TO  WRK-RCOMB-TEKEDYMD(IDX1)
      *@@
      *            ○生保の開始日変更
      *            主保険　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-HKN-TEKEDYMD   (WRK-COMB-SYUHOIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
                   MOVE    WRK-SYMD TO  WRK-KCOMB-TEKSTYMD (IDX2)
               END-IF
           ELSE
      *        [生保 開始日]＜[保険 開始日]
      *        保険　開始日の前日取得
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-HKN-TEKSTYMD   (WRK-COMB-SYUHOIDX)
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               PERFORM 4221-YMD-GET-SEC
               MOVE    WRK-SYMD            TO  WRK-SYMD6
      *
      *        ●[生保] 開始 <---> [主保険] 開始日の前日
               ADD     1                 TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX     TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC    (IDX1)
               MOVE    WRK-SYMD6    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
      *        ○生保の開始日変更
               MOVE    WRK-HKN-TEKSTYMD (WRK-COMB-SYUHOIDX)
                                    TO  WRK-KCOMB-TEKSTYMD    (IDX2)
      *
      *        [主保険 終了日]＜[生保 終了日]
               IF  WRK-HKN-TEKEDYMD (WRK-COMB-SYUHOIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *@@
                   ADD     1               TO  WRK-RCOMB-MAX
                   MOVE    WRK-RCOMB-MAX   TO  IDX1
                   MOVE    WRK-KCOMB-REC(IDX2)
                                           TO  WRK-RCOMB-REC(IDX1)
                   MOVE    WRK-HKN-TEKEDYMD(WRK-COMB-SYUHOIDX)
                                           TO  WRK-RCOMB-TEKEDYMD(IDX1)
      *@@
      *            主保険　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-HKN-TEKEDYMD   (WRK-COMB-SYUHOIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
      *            ○生保の開始日変更
                   MOVE    WRK-SYMD  TO  WRK-KCOMB-TEKSTYMD (IDX2)
               END-IF
           END-IF
           .
       570132X-SEIHO-HKN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    生保置き換え編集処理
      *****************************************************************
       570133-SEIHO-OKIKAE-HENSYU-SEC       SECTION.
      *
           INITIALIZE                  WRK-LCOMB2-AREA
           INITIALIZE                  WRK-LCOMB3-AREA
           INITIALIZE                  WRK-TCOMB-AREA
           MOVE     ZERO       TO      WRK-LCOMB2-MAX
           INITIALIZE                  TBL-SEIHO-YMD-T
           INITIALIZE                  TBL-SEIHO-YMD2-T
           MOVE     SPACE      TO      WRK-SEIHO-YMD
           MOVE     ZERO       TO      IDX4
           MOVE     ZERO       TO      IDX5
           MOVE     ZERO       TO      IDX7
           MOVE     ZERO       TO      IDX8
           MOVE     ZERO       TO      IDX9
           MOVE     ZERO       TO      FLG-ARI
      *
           IF    WRK-LCOMB-MAX   >    1
      *
             MOVE    WRK-LCOMB-MAX        TO  WRK-LCOMB2-MAX
             MOVE    WRK-LCOMB-AREA       TO  WRK-LCOMB2-AREA
      *
             MOVE    WRK-LCOMB2-REC(1)    TO  WRK-TCOMB-REC
             PERFORM    VARYING   IDX4   FROM   2   BY   1
                         UNTIL  ( IDX4    >    200           )
                          OR    ( IDX4    >   WRK-LCOMB2-MAX )
               COMPUTE   IDX5  =  IDX4  -  1
               MOVE  WRK-LCOMB2-REC(IDX4) TO  WRK-LCOMB2-REC(IDX5)
             END-PERFORM
             MOVE    WRK-TCOMB-REC        TO
                                    WRK-LCOMB2-REC(WRK-LCOMB2-MAX)
      *
             PERFORM    VARYING   IDX4   FROM   1   BY   1
                         UNTIL  ( IDX4    >    200           )
                          OR    ( IDX4    >   WRK-LCOMB2-MAX )
               IF  (IDX4   =   WRK-LCOMB2-MAX)
               OR  (IDX4   =   200           )
                  MOVE      9999   TO   IDX5
               ELSE
                  COMPUTE   IDX5    =   IDX4  +  1
               END-IF
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-LCOMB2-TEKEDYMD(IDX4)
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE     1                  TO  LNK-DAY6-ZOGEN
               PERFORM 4221-YMD-GET-SEC
               IF    (IDX5              NOT  =  9999      )
               AND   (WRK-LCOMB2-HKNID(IDX4) =
                                WRK-LCOMB2-HKNID(IDX5)    )
               AND   (WRK-LCOMB2-KOHI-AREA(IDX4) =
                                WRK-LCOMB2-KOHI-AREA(IDX5))
               AND   (WRK-SYMD = WRK-LCOMB2-TEKSTYMD(IDX5))
               AND   (WRK-LCOMB2-TEKEDYMD(IDX4)
                                          NOT = "99999999")
               AND   (WRK-LCOMB2-TEKEDYMD(IDX4)
                                             >= "20120601")
                 INITIALIZE               STS-AREA-DAY
                 INITIALIZE               LNK-DAY7-AREA
                 MOVE    "71"        TO   LNK-DAY7-IRAI
                 MOVE    WRK-LCOMB2-TEKEDYMD(IDX4)(1:6)
                                     TO   LNK-DAY7-YM
                 CALL    "ORCSDAY"   USING   STS-AREA-DAY
                                             LNK-DAY7-AREA
                 MOVE    LNK-DAY7-KOYOMI
                                     TO   WRK-SEIHO-YMD
                 MOVE    ZERO        TO   FLG-ARI
                 PERFORM  VARYING  IDX7   FROM    1   BY  1
                           UNTIL   IDX7                >   200
                            OR     TBL-SEIHO-YMD(IDX7) =  SPACE
                            OR     FLG-ARI             =   1
                   IF  TBL-SEIHO-YMD(IDX7) = WRK-SEIHO-YMD
                     MOVE     1      TO   FLG-ARI
                   END-IF
                 END-PERFORM
                 IF  FLG-ARI  =  ZERO
                   MOVE  WRK-SEIHO-YMD
                                     TO   TBL-SEIHO-YMD(IDX7)
                 END-IF
      *
                 MOVE    WRK-LCOMB2-TEKSTYMD(IDX4)
                                     TO   WRK-LCOMB2-TEKSTYMD(IDX5)
                 MOVE    "9"         TO   WRK-LCOMB2-HYOJIKBN(IDX5)
      *
                 MOVE    "99999999"  TO   WRK-LCOMB2-TEKSTYMD(IDX4)
                 MOVE    "99999999"  TO   WRK-LCOMB2-TEKEDYMD(IDX4)
               END-IF
             END-PERFORM
      *
             PERFORM    VARYING   IDX4   FROM   1   BY   1
                         UNTIL  ( IDX4    >    200           )
                          OR    ( IDX4    >   WRK-LCOMB2-MAX )
               IF   WRK-LCOMB2-TEKSTYMD(IDX4) = "99999999"
               AND  WRK-LCOMB2-TEKEDYMD(IDX4) = "99999999"
                 CONTINUE
               ELSE
                IF  WRK-LCOMB2-HYOJIKBN(IDX4) = SPACE
                   ADD   1         TO   IDX9
                   MOVE  WRK-LCOMB2-REC(IDX4)
                                   TO   WRK-LCOMB3-REC(IDX9)
                ELSE
                 MOVE     ZERO     TO    IDX8
                 INITIALIZE              TBL-SEIHO-YMD2-T
                 PERFORM  VARYING  IDX7   FROM    1   BY  1
                           UNTIL   IDX7                >   200
                            OR     TBL-SEIHO-YMD(IDX7) =  SPACE
                   IF  (WRK-LCOMB2-TEKSTYMD(IDX4)
                                      <= TBL-SEIHO-YMD(IDX7))
                   AND (WRK-LCOMB2-TEKEDYMD(IDX4)
                                      >  TBL-SEIHO-YMD(IDX7))
                     ADD     1     TO    IDX8
                     MOVE    TBL-SEIHO-YMD(IDX7)
                                   TO    TBL-SEIHO-YMD2(IDX8)
                   END-IF
                 END-PERFORM
                 IF    TBL-SEIHO-YMD2(1) =  SPACE
                   ADD   1         TO   IDX9
                   MOVE  SPACE     TO   WRK-LCOMB2-HYOJIKBN(IDX4)
                   MOVE  WRK-LCOMB2-REC(IDX4)
                                   TO   WRK-LCOMB3-REC(IDX9)
                 ELSE
                   MOVE  WRK-LCOMB2-TEKSTYMD(IDX4)
                                   TO   WRK-SEIHO-YMD
                   PERFORM  VARYING  IDX8   FROM    1   BY  1
                             UNTIL   IDX8                 >   200
                              OR     TBL-SEIHO-YMD2(IDX8) =  SPACE
                     ADD   1       TO   IDX9
                     MOVE  SPACE   TO   WRK-LCOMB2-HYOJIKBN(IDX4)
                     MOVE  WRK-LCOMB2-REC(IDX4)
                                   TO   WRK-LCOMB3-REC(IDX9)
                     MOVE  WRK-SEIHO-YMD
                                   TO   WRK-LCOMB3-TEKSTYMD(IDX9)
                     MOVE  TBL-SEIHO-YMD2(IDX8)
                                   TO   WRK-LCOMB3-TEKEDYMD(IDX9)
                     INITIALIZE         STS-AREA-DAY
                     INITIALIZE         LNK-DAY6-AREA
                     MOVE    "61"  TO   LNK-DAY6-IRAI
                     MOVE    TBL-SEIHO-YMD2(IDX8)
                                   TO   LNK-DAY6-KIJUN
                     MOVE    "1"   TO   LNK-DAY6-ZOGENPTN
                     MOVE     1    TO   LNK-DAY6-ZOGEN
                     PERFORM 4221-YMD-GET-SEC
                     MOVE    WRK-SYMD
                                   TO   WRK-SEIHO-YMD
                   END-PERFORM
                   ADD   1         TO   IDX9
                   MOVE  SPACE     TO   WRK-LCOMB2-HYOJIKBN(IDX4)
                   MOVE  WRK-LCOMB2-REC(IDX4)
                                   TO   WRK-LCOMB3-REC(IDX9)
                   MOVE  WRK-SEIHO-YMD
                                   TO   WRK-LCOMB3-TEKSTYMD(IDX9)
                 END-IF
                END-IF
               END-IF
             END-PERFORM
      *
             MOVE    IDX9               TO   WRK-LCOMB-MAX
             MOVE    WRK-LCOMB3-AREA    TO   WRK-LCOMB-AREA
      *
           END-IF
      *
           .
       570133-SEIHO-OKIKAE-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    主公費−老人　組合せ作成処理
      *
      *       IDX2...組合せ, WRK-COMB-RJNIDX   & IDX6...老人
      *       IDX...主公費,  WRK-COMB-SYUHOIDX & IDX5...主保険
      *****************************************************************
       570142-RJN-HEN-SEC       SECTION.
      *
      *    [老人 開始日]≦[組合せ 開始日]
           IF  WRK-KOH-TEKSTYMD (WRK-COMB-RJNIDX)
                                    <=  WRK-KCOMB-TEKSTYMD (IDX2)
               ADD     1                TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX    TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                        TO  WRK-RCOMB-REC  (IDX1)
      *        [老人 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-RJNIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *            ●[組合せ] 開始 <---> [老人] 終了日
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-RJNIDX)
                                    TO  WRK-RCOMB-TEKEDYMD (IDX1)
      *            ○組合せ元の開始日変更
      *            老人　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-RJNIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
                   MOVE    WRK-SYMD TO  WRK-KCOMB-TEKSTYMD (IDX2)
               ELSE
      *            [組合せ 終了日]≦[老人 終了日]
                   MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                    TO  WRK-KCOMB-TEKSTYMD (IDX2)
               END-IF
               MOVE    WRK-RCOMB-KOH1ID      (IDX1)
                                  TO  WRK-RCOMB-KOH2ID      (IDX1)
               MOVE    WRK-RCOMB-KOH1HKNNUM   (IDX1)
                                  TO  WRK-RCOMB-KOH2HKNNUM   (IDX1)
               MOVE    WRK-RCOMB-KOH1PAYKBN   (IDX1)
                                  TO  WRK-RCOMB-KOH2PAYKBN   (IDX1)
               MOVE    WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
                                  TO  WRK-RCOMB-KOH2-TANSEIDONAME(IDX1)
               MOVE    WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
                                  TO  WRK-RCOMB-KOH2-HKNKOHSBTKBN(IDX1)
               MOVE    WRK-KOH-KOHID         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1ID      (IDX1)
               MOVE    WRK-KOH-HKNNUM         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1HKNNUM   (IDX1)
               MOVE    WRK-KOH-PAYKBN         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1PAYKBN   (IDX1)
               MOVE    WRK-KOH-SEIDO     (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
      *        ------------------------------------
               MOVE    WRK-KOH-SBT       (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
           ELSE
      *        [組合せ 開始日]＜[老人 開始日]
      *        老人　開始日の前日取得
               INITIALIZE                  STS-AREA-DAY
               INITIALIZE                  LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-KOH-TEKSTYMD   (WRK-COMB-RJNIDX)
                                           TO  LNK-DAY6-KIJUN
               MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    -1                  TO  LNK-DAY6-ZOGEN
               PERFORM 4221-YMD-GET-SEC
               MOVE    WRK-SYMD            TO  WRK-SYMD6
      *
      *        ●[組合せ] 開始 <---> [老人] 終了日の前日
               ADD     1                 TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX     TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                         TO  WRK-RCOMB-REC    (IDX1)
               MOVE    WRK-SYMD6    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
      *        ○組合せ元の開始日変更
               MOVE    WRK-KOH-TEKSTYMD (WRK-COMB-RJNIDX)
                                    TO  WRK-KCOMB-TEKSTYMD    (IDX2)
      *        ●[老人] 開始 <---> [組合せ] 終了日
               ADD     1                 TO  WRK-RCOMB-MAX
               MOVE    WRK-RCOMB-MAX     TO  IDX1
               MOVE    WRK-KCOMB-REC (IDX2)
                                    TO  WRK-RCOMB-REC         (IDX1)
               MOVE    WRK-KOH-TEKSTYMD   (WRK-COMB-RJNIDX)
                                    TO  WRK-RCOMB-TEKSTYMD    (IDX1)
               MOVE    WRK-RCOMB-KOH1ID      (IDX1)
                                    TO  WRK-RCOMB-KOH2ID      (IDX1)
               MOVE    WRK-RCOMB-KOH1HKNNUM   (IDX1)
                                    TO  WRK-RCOMB-KOH2HKNNUM   (IDX1)
               MOVE    WRK-RCOMB-KOH1PAYKBN   (IDX1)
                                    TO  WRK-RCOMB-KOH2PAYKBN   (IDX1)
               MOVE    WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
                                  TO  WRK-RCOMB-KOH2-TANSEIDONAME(IDX1)
               MOVE    WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
                                  TO  WRK-RCOMB-KOH2-HKNKOHSBTKBN(IDX1)
               MOVE    WRK-KOH-KOHID         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1ID      (IDX1)
               MOVE    WRK-KOH-HKNNUM         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1HKNNUM   (IDX1)
               MOVE    WRK-KOH-PAYKBN         (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1PAYKBN   (IDX1)
               MOVE    WRK-KOH-SEIDO     (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1-TANSEIDONAME(IDX1)
      *        ------------------------------------
               MOVE    WRK-KOH-SBT       (WRK-COMB-RJNIDX)
                                  TO  WRK-RCOMB-KOH1-HKNKOHSBTKBN(IDX1)
      *
      *        [老人 終了日]＜[組合せ 終了日]
               IF  WRK-KOH-TEKEDYMD(WRK-COMB-RJNIDX)
                                    <   WRK-KCOMB-TEKEDYMD (IDX2)
      *
      *            ●[老人] 開始 <---> [老人] 終了日に変更
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-RJNIDX)
                                    TO  WRK-RCOMB-TEKEDYMD    (IDX1)
      *
      *            老人　終了日の翌日取得
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-KOH-TEKEDYMD   (WRK-COMB-RJNIDX)
                                               TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE     1                  TO  LNK-DAY6-ZOGEN
                   PERFORM 4221-YMD-GET-SEC
      *            ○組合せ元の開始日変更
                   MOVE    WRK-SYMD  TO  WRK-KCOMB-TEKSTYMD (IDX2)
               ELSE
      *            ○組合せ元の開始日変更
                   MOVE    WRK-KCOMB-TEKEDYMD (IDX2)
                                     TO  WRK-KCOMB-TEKSTYMD (IDX2)
               END-IF
           END-IF
          .
       570142-RJN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自費・治験・労災　組合せ作成処理
      *****************************************************************
       57015-JIHCHIROU-HEN-SEC             SECTION.
      *
           INITIALIZE                      WRK-LCOMB-AREA
           MOVE    ZERO                TO  WRK-LCOMB-MAX
      *
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL   ( IDX   >       200 )
                       OR      ( IDX   >       CNT-HKN )
             IF  ((WRK-HKN-SBT   (IDX)  =   "4" OR "8" OR "9")
      *!          特別療養費 対象
               OR (WRK-HKN-HKNNUM (IDX) =   "068"
                                        OR  "040"
                                        OR  "069" )) AND
      *!
                 (WRK-HKN-SAKJOKBN    (IDX)  =   SPACE)
      *        自費・治験・労災  特別療養費
               INITIALIZE                  WRK-COMB-AREA
      *
      *        組合せレコード編集転送処理
               ADD     1                   TO  WRK-LCOMB-MAX
               MOVE    WRK-LCOMB-MAX       TO  IDX2
               MOVE    SPACE               TO  WRK-LCOMB-REC(IDX2)
               INITIALIZE                      WRK-LCOMB-REC(IDX2)
               MOVE    WRK-HKN-HKNID    (IDX)
                                       TO  WRK-LCOMB-HKNID  (IDX2)
               MOVE    WRK-HKN-HKNNUM    (IDX)
                                       TO  WRK-LCOMB-HKNNUM     (IDX2)
               MOVE    WRK-HKN-HONKZKKBN (IDX)
                                       TO  WRK-LCOMB-HONKZKKBN  (IDX2)
               MOVE    WRK-HKN-HOJOKBN   (IDX)
                                       TO  WRK-LCOMB-HOJOKBN    (IDX2)
               MOVE    WRK-HKN-CONTKBN   (IDX)
                                       TO  WRK-LCOMB-CONTKBN    (IDX2)
               MOVE    WRK-HKN-TEKSTYMD(IDX)
                                       TO  WRK-LCOMB-TEKSTYMD  (IDX2)
               MOVE    WRK-HKN-TEKEDYMD(IDX)
                                       TO  WRK-LCOMB-TEKEDYMD  (IDX2)
               MOVE    WRK-HKN-SEIDO(IDX)
                                   TO  WRK-LCOMB-SYU-TANSEIDONAME(IDX2)
      *        ------------------------------------
               MOVE    WRK-HKN-SBT   (IDX)
                                  TO  WRK-LCOMB-SYU-HKNKOHSBTKBN(IDX2)
      *        患者負担内容セット
               PERFORM 57012-FUTAN-SET-SEC
      *
               PERFORM     57010-NC01PARA-OUT-SEC
      *
             END-IF
           END-PERFORM
          .
       57015-JIHCHIROU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費確認処理
      *****************************************************************
       4410-HKNKOH-KAKUNIN-SEC        SECTION.
      *
           MOVE    ZERO               TO  FLG-HKNNUM
           MOVE    SPACE              TO  HKNNUM-REC
           MOVE    SPA-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    WRK-KEN-HKNNUM     TO  HKN-HKNNUM
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *----(01.00.01) LINE MOD START ---------------------------------
           IF      MCP-RC             =   ZERO
               MOVE    "tbl_hknnum"       TO  MCP-TABLE
               MOVE    "key2"             TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-NEXT-SEC
               MOVE    "1"            TO  WRK-KEN-ERFLG
           ELSE
               INITIALIZE                 HKNNUM-REC
               MOVE    1              TO  FLG-HKNNUM
               MOVE    "2"            TO  WRK-KEN-ERFLG
           END-IF
      *
           MOVE    ZERO                   TO  FLG-RJN
           PERFORM   UNTIL   FLG-HKNNUM   =   1
               IF    ( WRK-KEN-TEKSTYMD       >=  HKN-TEKSTYMD ) AND
                     ( WRK-KEN-TEKEDYMD       <=  HKN-TEKEDYMD )
                   IF      WRK-KEN-TEKSTYMD   >=  "20021001"
      *                Ｈ１４．１０ 以降
                       PERFORM 44102-HKNKOH-KAKUNIN-10-SEC
                   ELSE
      *                Ｈ１４．１０ 以前
                       PERFORM 44101-HKNKOH-KAKUNIN-09-SEC
                   END-IF
               END-IF
               IF      FLG-HKNNUM          =   ZERO
                   MOVE    "tbl_hknnum"       TO  MCP-TABLE
                   MOVE    "key2"             TO  MCP-PATHNAME
                   PERFORM 921-HKNNUM-NEXT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
           MOVE    "key2"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4410-HKNKOH-KAKUNIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費確認処理（Ｈ１４．１０ 以前）
      *****************************************************************
       44101-HKNKOH-KAKUNIN-09-SEC        SECTION.
      *
      *    老人あり
           IF      HKN-HKNKOHSBTKBN    =   "3"
               MOVE    1                   TO  FLG-RJN
           END-IF
      *    老人でＨ１４．１０以前の時は、システム管理より支払区分を決定
      *    支払区分は老人（０２７）のみ
           IF      WRK-KEN-HKNNUM      =   "027"
               MOVE    1                   TO  FLG-RJN
      *        Ｈ１２．１２．３１までの支払区分は００
               IF     (WRK-KEN-TEKEDYMD    >=  "20010101"   )
                   IF     (HKN-PAYKBN (1:1)    =   SPA-HOSPSBT)  AND
                          (HKN-PAYKBN (2:1)    =   SPA-ROUPAYKBN)
                       MOVE    1               TO  FLG-HKNNUM
                       MOVE    SPACE           TO  WRK-KEN-ERFLG
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-HKNNUM
                   MOVE    SPACE           TO  WRK-KEN-ERFLG
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNNUM
               MOVE    SPACE           TO  WRK-KEN-ERFLG
           END-IF
          .
       44101-HKNKOH-KAKUNIN-09-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険公費確認処理（Ｈ１４．１０ 以降）
      *****************************************************************
       44102-HKNKOH-KAKUNIN-10-SEC        SECTION.
      *
      *    老人あり
           IF      HKN-HKNKOHSBTKBN    =   "3"
               MOVE    1                   TO  FLG-RJN
           END-IF
      *
      *    支払区分が同じこと
           IF      WRK-KEN-PAYKBN      =   SPACE  OR  ZERO
               MOVE    SPACE           TO  WRK-KEN-ERFLG
               MOVE    1               TO  FLG-HKNNUM
           ELSE
               IF      WRK-KEN-PAYKBN      =  HKN-PAYKBN
                   MOVE    SPACE           TO  WRK-KEN-ERFLG
                   MOVE    1               TO  FLG-HKNNUM
               END-IF
           END-IF
      *
           .
       44102-HKNKOH-KAKUNIN-10-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付取得処理
      *****************************************************************
       4221-YMD-GET-SEC            SECTION.
      *
           CALL    "ORCSDAY"    USING   STS-AREA-DAY  LNK-DAY6-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       4221-YMD-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       921-HKNNUM-NEXT-SEC              SECTION.
      *
           MOVE    "tbl_hknnum"       TO  MCP-TABLE
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
