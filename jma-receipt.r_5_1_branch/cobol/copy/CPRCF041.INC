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
      ************************************************************
      *    レセプト明細書     COPY (CPRCF041.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01   NACL-門脇     03.02.06  コメントカウントフラグの追加
      *  01.01.02   NACL-門脇     03.06.19  実日数テーブルの追加
      *  01.01.03   NACL-門脇     04.07.28  休業証明フラグ、休業期間終了
      *                                     月日の追加
      *  01.01.04   NACL-門脇     05.01.07  自賠責担当医コード、自賠責保
      *                                     険会社名の追加
      *  01.01.05   NACL-門脇     05.01.26  ４００１情報の追加
      *  01.01.06   NACL-門脇     05.08.02  自賠責従来様式用項目追加
      *  01.01.07   NACL-門脇     05.12.01  労災レセ回数記載：基準年月
      *                                     労災レセ回数記載：回数
      *                                     労災レセプト回数
      *                                     労災レセ回数記載の追加
      *
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *                                     外来迅速検体検査加算フラグ追加
      *  03.04.01    NACL-門脇    06/12/01  自賠責健保準拠対応
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *
      *  04.01.01    NACL-門脇    07/10/25  公務災害対応
      *  04.02.01    NACL-門脇    08/02/26  請求区分追加
      *
      *  04.05.01    NACL-門脇    09/06/29  休業期間開始月日、
      *                                     休業証明療養期間記載の追加
      *  04.05.02    NACL-門脇    10/02/16  予備項目追加
      *
      *  04.06.01    NACL-門脇    10/07/02  小児特定疾患カウンセリング料
      *                                     （１回目）算定フラグ追加
      *
      *  04.07.01    NACL-門脇    11/09/01  労災初回請求分フラグ追加
      *                                     労災基準監督署コード追加
      *                                     年齢追加
      *                                     テスト患者番号区分追加
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.03    NACL-門脇    13/04/16  労災レセ電対応
      *                                     労災指定医療機関区分追加
      *  04.07.04    NACL-門脇    13/08/20  ０９５９３で始まるコードの
      *                                     有無区分追加
      *  04.07.05    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.06    NACL-門脇    14/07/31  第三者行為に関するフラグ追加
      *  04.07.07    NACL-門脇    14/10/15  入院フラグ追加
      *  04.07.08    NACL-門脇    15/09/08  自賠責（新様式）（ＯＣＲ様式）
      *                                     用の項目追加
      *
      *  04.08.01    NACL-門脇    15/08/27  自賠責・請求金額欄端数処理
      *                                     区分追加
      *  04.08.02    NACL-門脇    17/02/24  レセプト種別不明エラー情報追加
      *  04.08.03    NACL-門脇    18/04/06  平成３０年４月改正対応
      *  04.08.04    NACL-門脇    18/05/22  平成３０年４月改正対応
      *  04.08.05    NACL-門脇    18/06/11  逓減点数集計のサイン対応
      *
      *  05.00.01    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      *  05.00.02    NACL-門脇    20/04/07  令和２年４月改正対応
      ************************************************************
      *01  RECE41-REC.
           03  RECE41-KEY.
      *医療機関ＩＤ
               05  RECE41-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE41-SRYYM           PIC  9(06).
      *入外区分
               05  RECE41-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE41-PTID            PIC  9(10).
      *レセプト科
               05  RECE41-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE41-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE41-TEKSTYMD        PIC  X(08). 
      *レセプト種別
           03  RECE41-RECESYUBETU         PIC  9(04).
      *患者番号
           03  RECE41-PTNUM               PIC  X(20).
      *診療科情報
           03  RECE41-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE41-SRYKA           PIC  X(02).
      *カナ氏名
           03  RECE41-KANANAME            PIC  X(50).
      *漢字氏名
           03  RECE41-NAME                PIC  X(100).
      *性別
           03  RECE41-SEX                 PIC  X(01).
      *生年月日
           03  RECE41-BIRTHDAY            PIC  9(08).
      *死亡区分
           03  RECE41-DEATHKBN            PIC  X(01).
      *年齢
           03  RECE41-AGE                 PIC  9(03).
      *テスト患者番号区分
           03  RECE41-TSTPTNUMKBN         PIC  X(01).
      *
      *主保険情報
           03  RECE41-HKN.
      *主保険−労災保険区分
               05  RECE41-HKNKBN          PIC  X(01).
      *主保険−保険番号
               05  RECE41-HKNNUM          PIC  X(03).
      *主保険−患者交付番号
               05  RECE41-KOFUNUM         PIC  X(14).
      *主保険−事業所名称
               05  RECE41-JIGYOUNAME      PIC  X(80).
      *主保険−所在地都道府県名
               05  RECE41-PREFNAME        PIC  X(20).
      *主保険−所在地都道府県区分
               05  RECE41-PREFKBN         PIC  X(01).
      *主保険−所在地郡市区名
               05  RECE41-CITYNAME        PIC  X(80).
      *主保険−所在地郡市区区分
               05  RECE41-CITYKBN         PIC  X(01).
      *主保険−新継再別
               05  RECE41-SINKEI          PIC  X(01).
      *主保険−転記事由
               05  RECE41-TENKI           PIC  X(01).
      *主保険−傷病年月日
               05  RECE41-SHOBYOYMD       PIC  X(08).
      *主保険−四肢特例区分
               05  RECE41-SISIKBN         PIC  X(01).
      *主保険−療養開始年月日
               05  RECE41-RYOSTYMD        PIC  X(08).
      *主保険−療養終了年月日
               05  RECE41-RYOEDYMD        PIC  X(08).
      *主保険−損傷区分
               05  RECE41-SONSHOUKBN      PIC  X(02).
      *主保険−損傷区分−枝番
               05  RECE41-SONSHOUKBN-EDANUM  PIC  X(01).
      *主保険−診察年月日
               05  RECE41-SINSATUYMD      PIC  X(08).
      *主保険−労災コメント
               05  RECE41-COMMENT         PIC  X(200).
      *主保険−災害区分
               05  RECE41-SAIGAIKBN       PIC  X(01).
      *主保険−自賠責担当医コード
               05  RECE41-DRCD            PIC  X(05).
      *主保険−自賠責保険会社名
               05  RECE41-HOKENNAME       PIC  X(80).
      *主保険−労災レセ回数記載：基準年月
               05  RECE41-KIJYUNYM-KSI    PIC  X(06).
      *主保険−労災レセ回数記載：回数
               05  RECE41-KAISU-KSI       PIC  9(03).
      *主保険−請求区分
               05  RECE41-SEIKYUKBN       PIC  X(01).
      *
      *保険組合せ番号
           03  RECE41-HKNCOMBI            PIC  9(04).
      *
      *病棟区分
           03  RECE41-BYOTOKBN            PIC  X(02).
      *
      *実日数
           03  RECE41-JNISSU              PIC  9(03).
      *
      *診療区分別点数情報
           03  RECE41-SRYKBNINF           OCCURS  50.
      *基本点数
               05  RECE41-KHNTEN          PIC  9(08).
      *回数
               05  RECE41-KAISU           PIC  9(05).
      *明細点数
               05  RECE41-TENSU           PIC S9(08).
      *明細点数（薬剤等）
               05  RECE41-TENSU-YAK       PIC S9(08).
      **明細金額（自賠責：従来様式用）
               05  RECE41-KINGAKU-JYU     PIC  9(09).
      *合計点数
           03  RECE41-TOTALTEN            PIC S9(08).
      *合計点数（薬剤等）
           03  RECE41-TOTALTEN-YAK        PIC  9(08).
      **合計金額（自賠責：従来様式用）
           03  RECE41-TOTALKIN-JYU        PIC  9(09).
      *
      *合計金額
           03  RECE41-TOTALKIN            PIC  9(09).
      *合計金額（薬剤等）
           03  RECE41-TOTALKIN-YAK        PIC  9(09). 
      *
      *診療区分別金額情報
           03  RECE41-SRYKBNINF2          OCCURS  10.
      *回数
               05  RECE41-KAISU2          PIC  9(05).
      *初再診年月日
               05  RECE41-SYOSAIYMD2      PIC  X(08).
      *明細金額
               05  RECE41-KINGAKU2        PIC  9(09).
      *
      *合計金額
           03  RECE41-TOTALKIN2           PIC  9(09).
      *合計金額（その他）
           03  RECE41-TOTALKIN2-ETC       PIC  9(09).
      *
      *診断書料
           03  RECE41-SINDAN-TUU          PIC  9(02).
           03  RECE41-SINDAN-KIN          PIC  9(09).
      *明細書料
           03  RECE41-MEISAI-TUU          PIC  9(02).
           03  RECE41-MEISAI-KIN          PIC  9(09).
      **その他欄金額（自賠責：従来様式用）
           03  RECE41-SONOTAKIN-JYU       PIC  9(09).
      *
      *食事療養費情報
           03  RECE41-SHOKUJIINF.
      *食事日数
               05  RECE41-SHOKUJINISSU    PIC  9(02).
      *食事療養費
               05  RECE41-SHOKUJIRYOYOHI  PIC  9(09).
      *標準負担額
               05  RECE41-SHOKUJIFTN      PIC  9(09).
      *標準負担区分
           03  RECE41-SHOKUJIFTNKBN       PIC  X(01).
      *
      *初診時間区分
           03  RECE41-SHOSHINTIME         PIC  X(01).
      *初診乳児区分
           03  RECE41-SHOSHIN-NYU         PIC  X(01).
      *初診年月日
           03  RECE41-SHOSHINYMD          PIC  X(08).
      *再診時間区分
           03  RECE41-SAISHINTIME         PIC  X(01).
      *再診摘要印刷区分
           03  RECE41-SAISHIN-PRTKBN      PIC  X(01).
      *再診摘要印刷区分２
           03  RECE41-SAISHIN-PRTKBN2     PIC  X(01).
      *外来管理加算摘要印刷区分
           03  RECE41-GAIRAIKSN-PRTKBN    PIC  X(01).
      *継続管理加算印刷区分
           03  RECE41-KEIZOKU-PRTKBN      PIC  X(01).
      *救急医療管理加算区分
           03  RECE41-KYUKYU-PRTKBN       PIC  X(01).
      *救急医療管理加算算定日
           03  RECE41-KYUKYUYMD           PIC  X(08).
      *外来管理加算（特例:５２）
           03  RECE41-GAITOKU-PRTKBN1     PIC  X(01).
           03  RECE41-GAITOKU-TEN1        PIC  9(03).
           03  RECE41-GAITOKU-KAISU1      PIC  9(02).
      *外来管理加算（特例:２６）
           03  RECE41-GAITOKU-PRTKBN2     PIC  X(01).
           03  RECE41-GAITOKU-TEN2        PIC  9(03).
           03  RECE41-GAITOKU-KAISU2      PIC  9(02).
      *検査区分
           03  RECE41-KENSAKBN            PIC  X(01).
      *前回検査年月日
           03  RECE41-KENSAYMD-ZEN        PIC  X(08).
      *通院日情報
           03  RECE41-TUUININF            OCCURS  31.
               05  RECE41-TUUIN-DAY       PIC  X(01).
      *通院日数計
           03  RECE41-TUUIN-KEI           PIC  9(02).
      *日別手技点・薬剤等点数情報
           03  RECE41-TENSUINF            OCCURS  31.
               05  RECE41-SYUTEN-JI       PIC  9(09).
               05  RECE41-YKZTEN-JI       PIC  9(09).
      *コメントカウントフラグ
           03  RECE41-COMCNTFLG           PIC  X(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE41-BEDSUFLG            PIC  X(01).
      *外来迅速検体検査加算フラグ
           03  RECE41-KENTAIKASANFLG      PIC  X(01).
      *乳幼児・幼児のコメントフラグ
           03  RECE41-NYUYOJIFLG          PIC  X(01).
      *小児特定疾患カウンセリング料（１回目）算定フラグ
           03  RECE41-SYOUNIFLG           PIC  X(01).
      *乳腺炎重症化予防ケア・指導料（２回目から
      *４回目まで）算定フラグ
           03  RECE41-NYUSENFLG           PIC  X(01).
      *
      *実日数テーブル情報
           03  RECE41-JNISSUTBLINF        OCCURS 31.
      *実日数テーブル
               05  RECE41-JNISSUTBL       PIC  X(01).
      *
      *入院日テーブル情報
           03  RECE41-NYUINBITBLINF       OCCURS 31.
      *入院日テーブル
               05  RECE41-NYUINBITBL      PIC  X(01).
      *
      *入院フラグ（同一保険組み合わせで入院が有るか）
           03  RECE41-NYUINFLG            PIC  X(01).
      *
      *休業証明フラグ
           03  RECE41-KYUGYOFLG           PIC  X(01).
      *休業期間開始月日
           03  RECE41-KYUGYO-STR          PIC  X(04).
      *休業期間終了月日
           03  RECE41-KYUGYO-END          PIC  X(04).
      *
      *労災レセプト回数
           03  RECE41-RECEKAISU           PIC  9(03).
      *
      *公務災害−転記年月日
           03  RECE41-TENKIYMD            PIC  X(08).
      *
      *労災初回請求分フラグ
           03  RECE41-SYOKAIFLG           PIC  X(01).
      *
      *労災基準監督署コード
           03  RECE41-SYOCD               PIC  X(05).
      *
      *４００１情報
           03  RECE41-4001-INF.
      *点数単価
               05  RECE41-TANKA           PIC  9(02)V99.
      *自賠責加算率
               05  RECE41-KSNRATE         PIC  9(03).
      *自賠責文書料消費税
               05  RECE41-TAXKBN          PIC  X(01).
      *自賠責特定器材等消費税
               05  RECE41-TAXKBN-KZI      PIC  X(01).
      *自賠責固定帯加算等取扱
               05  RECE41-KOTEIKBN        PIC  X(01).
      *労災レセ回数記載
               05  RECE41-ROUKAISU        PIC  X(01).
      *自賠責区分
               05  RECE41-JIBAIKBN        PIC  X(01).
      *技術点単価
               05  RECE41-SYUTENTANKA     PIC  9(02).
      *薬剤等点単価
               05  RECE41-YKZTENTANKA     PIC  9(02).
      *公務災害区分
               05  RECE41-KOMUKBN         PIC  X(01).
      *公務災害点単価
               05  RECE41-KOMUTENTANKA    PIC  9(02).
      *休業証明療養期間記載
               05  RECE41-KYUGYO-PRTKBN   PIC  X(01).
      *労災指定医療機関区分
               05  RECE41-RSIIRYOKBN      PIC  X(01).
      *
      *０９５９３で始まるコードの有無区分
           03  RECE41-ORIGINALCDKBN       PIC  9(01).
      *
      *第三者行為時の医保分主保険情報
           03  RECE41-HKN-X.
      *医保分主保険番号
               05  RECE41-HKNNUM-X        PIC  X(03).
      *医保分主保険ＩＤ
               05  RECE41-HKNID-X         PIC  9(10).
      *医保分保険者番号
               05  RECE41-HKNJANUM        PIC  X(08).
      *医保分保険者名
               05  RECE41-HKNJANAME       PIC  X(200).
      *医保分記号
               05  RECE41-KIGO            PIC  X(80).
      *医保分番号
               05  RECE41-NUM             PIC  X(80).
      *
      *第三者行為（社会保険への請求額）
           03  RECE41-SKYMONEY            PIC  9(09).
      *第三者行為（患者負担）
           03  RECE41-FTNMONEY            PIC  9(09).
      *第三者行為（患者割合）
           03  RECE41-FTNRATE             PIC  9(03).
      *第三者行為レセプト現物給付該当フラグ
           03  RECE41-DAISAN-GENBUTU-GAITO
                                          PIC  9(01).
      *第三者行為一部負担金金額置換フラグ
           03  RECE41-DAISAN-CHIKAN       PIC  9(01).
      *
      *自賠責・請求金額欄端数処理区分
           03  RECE41-JIBAI-HASUKBN       PIC  X(01).
      *
      *自賠責（新様式）（ＯＣＲ様式）用
      *合計額
           03  RECE41-JIBAI-GOKEI-BE      PIC  9(09).
           03  RECE41-JIBAI-GOKEI-AF      PIC  9(09).
      *請求額
           03  RECE41-JIBAI-SKYKINFLG     PIC  9(01).
           03  RECE41-JIBAI-SKYKIN        PIC  9(09).
      *受領額・受領先
           03  RECE41-JIBAI-JYUKINFLG     PIC  9(01).
           03  RECE41-JIBAI-JYUKIN        PIC  9(09).
           03  RECE41-JIBAI-JYUNAMEFLG    PIC  9(01).
           03  RECE41-JIBAI-JYUNAME       PIC  X(80).
      *自賠責受領金額欄置換入力分
           03  RECE41-JIBAI-CHIKANKINFLG  PIC  9(01).
           03  RECE41-JIBAI-CHIKANKIN     PIC  9(09).
           03  RECE41-JIBAI-CHIKANNAMEFLG PIC  9(01).
           03  RECE41-JIBAI-CHIKANNAME    PIC  X(80).
      *
      *湿布薬制限枚数解除及び院外湿布薬７０枚超理由コメント情報
           03  RECE41-SIPPUCHKFLG         PIC  9(01).
           03  RECE41-SIPPUCHKINF.
             04  RECE41-SIPPUCHK-OCC      OCCURS  10.
               05  RECE41-SIPPUCHK-OCC2   OCCURS  31.
                   07  RECE41-SIPPUCHK    PIC  9(03).
      *
      *レセプト種別不明エラー情報
           03  RECE41-ERR-AREA.
               05  RECE41-ERR-ERRKBN      PIC  9(02).
               05  RECE41-ERR-DAY         PIC  9(02).
               05  RECE41-ERR-HKNCOMBINUM PIC  9(04).
               05  RECE41-ERR-TEKSTYMD    PIC  9(08).
               05  RECE41-ERR-TEKEDYMD    PIC  9(08).
      *
      *妊婦フラグ
           03  RECE41-NINPUFLG            PIC  9(01).
      *
      *検査フラグ
           03  RECE41-KENSA-BNPFLG        PIC  9(01).
           03  RECE41-KENSA-NTPROFLG      PIC  9(01).
           03  RECE41-KENSA-ANPFLG        PIC  9(01).
      *
      *リハビリ情報
           03  RECE41-RIHAINF-AREA.
      *リハビリ算定テーブル２
               05  RECE41-RIHA2-DAY-AREA.
                 06  RECE41-RIHA2-SINDAI-AREA.
                   07  RECE41-RIHA2-SINDAI      PIC 9(01).
                   07  RECE41-RIHA2-SINYMD-OCC1.
                     08  RECE41-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SINYMD-OCC2.
                     08  RECE41-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-KOKYUKI-AREA.
                   07  RECE41-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE41-RIHA2-KOKYMD-OCC1.
                     08  RECE41-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-KOKYMD-OCC2.
                     08  RECE41-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-NOUKE-AREA.
                   07  RECE41-RIHA2-NOUKE       PIC 9(01).
                   07  RECE41-RIHA2-NOUYMD-OCC1.
                     08  RECE41-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-NOUYMD-OCC2.
                     08  RECE41-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-UNDOKI-AREA.
                   07  RECE41-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE41-RIHA2-UNDYMD-OCC1.
                     08  RECE41-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-UNDYMD-OCC2.
                     08  RECE41-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-NANBYO-AREA.
                   07  RECE41-RIHA2-NANBYO      PIC 9(01).
                   07  RECE41-RIHA2-NANYMD-OCC1.
                     08  RECE41-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-NANYMD-OCC2.
                     08  RECE41-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-SYOGAI-AREA.
                   07  RECE41-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE41-RIHA2-SYOYMD-OCC1.
                     08  RECE41-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SYOYMD-OCC2.
                     08  RECE41-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-SESSYOKU-AREA.
                   07  RECE41-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE41-RIHA2-SESYMD-OCC1.
                     08  RECE41-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SESYMD-OCC2.
                     08  RECE41-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-HAIYO-AREA.
                   07  RECE41-RIHA2-HAIYO       PIC 9(01).
                   07  RECE41-RIHA2-HAIYMD-OCC1.
                     08  RECE41-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-HAIYMD-OCC2.
                     08  RECE41-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *リハビリ加算算定テーブル２
               05  RECE41-RIHA2-DAY9-AREA.
                 06  RECE41-RIHA2-SINDAI9-AREA.
                   07  RECE41-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE41-RIHA2-SIN9YMD-OCC1.
                     08  RECE41-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-SIN9YMD-OCC2.
                     08  RECE41-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-KOKYUKI9-AREA.
                   07  RECE41-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE41-RIHA2-KOK9YMD-OCC1.
                     08  RECE41-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-KOK9YMD-OCC2.
                     08  RECE41-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE41-RIHA2-HAIYO9-AREA.
                   07  RECE41-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE41-RIHA2-HAI9YMD-OCC1.
                     08  RECE41-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE41-RIHA2-HAI9YMD-OCC2.
                     08  RECE41-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *外来排尿自立指導料の該当月最初の算定日
           03  RECE41-GAIJIRYMD           PIC  X(08).
      *
      *予備
           03  RECE41-YOBIETC-AREA.
               05  RECE41-YOBIETCINF           OCCURS  10.
                   07  RECE41-YOBIETC     PIC  9(09).
      *
