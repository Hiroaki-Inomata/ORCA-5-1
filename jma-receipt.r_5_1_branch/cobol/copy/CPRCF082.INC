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
      *    労災レセプト明細書（入院）     COPY (CPRCF082.INC)    *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01   NACL-門脇     05.01.07  自賠責担当医コード、自賠責保
      *                                     険会社名の追加
      *  01.01.02   NACL-門脇     05.01.26  ４００１情報の追加
      *  01.01.03   NACL-門脇     05.08.02  自賠責従来様式用項目追加
      *  01.01.04   NACL-門脇     05.12.01  労災レセ回数記載：基準年月
      *                                     労災レセ回数記載：回数
      *                                     労災レセプト回数
      *                                     労災レセ回数記載の追加
      *  01.01.05   NACL-門脇     06.02.07  食事基本料情報（その他）追加
      *
      *  02.09.01   NACL-藤原     06.04.14  ２００６年４月改正対応
      *                                     外来迅速検体検査加算フラグ追加
      *  03.03.01   NACL-門脇     06.10.10  入院時生活療養費食事対応
      *  03.04.01   NACL-門脇     06.12.01  自賠責健保準拠対応
      *  03.05.01   NACL-門脇     07.06.01  グループ診療対応
      *  03.05.02   NACL-門脇     07.06.26  自賠責平成１９年４月改正様式対応
      *
      *  04.01.01   NACL-門脇     07/10/25  公務災害対応
      *  04.01.02   NACL-門脇     07/12/19  入院料未算定日対応
      *  04.02.01   NACL-門脇     08/02/26  請求区分追加
      *
      *  04.04.01   NACL-藤原     09/03/10  特別な関係にある他院歴を初回
      *                                     入院日として起算しレセプト記
      *                                     載を行なう対応
      *  04.05.01   NACL-門脇     10/01/25  日別手技点にサイン追加
      *  04.05.02   NACL-門脇     10/02/16  予備項目追加
      *  04.05.03   NACL-藤原     11/11/22  同日再入院対応
      *  04.05.04   NACL-門脇     12/04/09  入院略称の文字数拡張
      *
      *  04.07.01   NACL-門脇     11/09/01  労災初回請求分フラグ追加
      *                                     労災基準監督署コード追加
      *                                     テスト患者番号区分追加
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.03    NACL-門脇    13/04/16  労災レセ電対応
      *                                     労災指定医療機関区分追加
      *  04.07.04    NACL-門脇    13/08/20  ０９５９３で始まるコードの
      *                                     有無区分追加
      *  04.07.05    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.06    NACL-門脇    14/05/13  超重症児（者）入院診療加算等の
      *                                     算定フラグ追加
      *  04.07.07    NACL-門脇    14/07/31  第三者行為に関するフラグ追加
      *  04.07.08    NACL-門脇    15/09/08  自賠責（新様式）（ＯＣＲ様式）
      *                                     用の項目追加
      *
      *  04.08.01    NACL-門脇    15/08/27  自賠責・請求金額欄端数処理
      *                                     区分追加
      *  04.08.02    NACL-門脇    16/04/15  平成２８年４月改正対応
      *  04.08.03    NACL-門脇    17/02/24  レセプト種別不明エラー情報修正
      *  04.08.04    NACL-門脇    18/04/06  平成３０年４月改正対応
      *
      *  05.00.01    NACL-門脇    19/07/01  リハビリ発症日レセプト記載対応
      *                                     （機能追加）
      ************************************************************
      *01  RECE82-REC.
           03  RECE82-KEY.
      *医療機関ＩＤ
               05  RECE82-HOSPNUM         PIC  9(02).
      *診療年月
               05  RECE82-SRYYM           PIC  9(06).
      *入外区分
               05  RECE82-NYUGAIKBN       PIC  X(01).
      *患者ＩＤ
               05  RECE82-PTID            PIC  9(10).
      *レセプト科
               05  RECE82-RECEKA          PIC  X(02).
      *保険ＩＤ        
               05  RECE82-HKNID           PIC  9(10).
      *適用開始年月日
               05  RECE82-TEKSTYMD        PIC  X(08).
      *レセプト種別
           03  RECE82-RECESYUBETU         PIC  9(04).
      *患者番号
           03  RECE82-PTNUM               PIC  X(20).
      *診療科情報
           03  RECE82-SRYKAINF            OCCURS  20.
      *診療科
               05  RECE82-SRYKA           PIC  X(02).
      *カナ氏名
           03  RECE82-KANANAME            PIC  X(50).
      *漢字氏名
           03  RECE82-NAME                PIC  X(100).
      *性別
           03  RECE82-SEX                 PIC  X(01).
      *生年月日
           03  RECE82-BIRTHDAY            PIC  9(08).
      *死亡区分
           03  RECE82-DEATHKBN            PIC  X(01).
      *年齢
           03  RECE82-AGE                 PIC  9(03).
      *テスト患者番号区分
           03  RECE82-TSTPTNUMKBN         PIC  X(01).
      *
      *主保険情報
           03  RECE82-HKN.
      *主保険−労災保険区分
               05  RECE82-HKNKBN          PIC  X(01).
      *主保険−保険番号
               05  RECE82-HKNNUM          PIC  X(03).
      *主保険−患者交付番号
               05  RECE82-KOFUNUM         PIC  X(14).
      *主保険−事業所名称
               05  RECE82-JIGYOUNAME      PIC  X(80).
      *主保険−所在地都道府県名
               05  RECE82-PREFNAME        PIC  X(20).
      *主保険−所在地都道府県区分
               05  RECE82-PREFKBN         PIC  X(01).
      *主保険−所在地郡市区名
               05  RECE82-CITYNAME        PIC  X(80).
      *主保険−所在地郡市区区分
               05  RECE82-CITYKBN         PIC  X(01).
      *主保険−新継再別
               05  RECE82-SINKEI          PIC  X(01).
      *主保険−転記事由
               05  RECE82-TENKI           PIC  X(01).
      *主保険−傷病年月日
               05  RECE82-SHOBYOYMD       PIC  X(08).
      *主保険−四肢特例区分
               05  RECE82-SISIKBN         PIC  X(01).
      *主保険−療養開始年月日
               05  RECE82-RYOSTYMD        PIC  X(08).
      *主保険−療養終了年月日
               05  RECE82-RYOEDYMD        PIC  X(08).
      *主保険−労災コメント
               05  RECE82-COMMENT         PIC  X(200).
      *主保険−災害区分
               05  RECE82-SAIGAIKBN       PIC  X(01).
      *主保険−自賠責担当医コード
               05  RECE82-DRCD            PIC  X(05).
      *主保険−自賠責保険会社名
               05  RECE82-HOKENNAME       PIC  X(80).
      *主保険−労災レセ回数記載：基準年月
               05  RECE82-KIJYUNYM-KSI    PIC  X(06).
      *主保険−労災レセ回数記載：回数
               05  RECE82-KAISU-KSI       PIC  9(03).
      *主保険−請求区分
               05  RECE82-SEIKYUKBN       PIC  X(01).
      *
      *保険組合せ番号
           03  RECE82-HKNCOMBI            PIC  9(04).
      *
      *実日数
           03  RECE82-JNISSU              PIC  9(03).
      *
      *診療区分別点数情報
           03  RECE82-SRYKBNINF           OCCURS  50.
      *基本点数
               05  RECE82-KHNTEN          PIC  9(08).
      *回数
               05  RECE82-KAISU           PIC  9(05).
      *明細点数
               05  RECE82-TENSU           PIC  9(08).
      *明細点数（薬剤等）
               05  RECE82-TENSU-YAK       PIC  9(08).
      **明細金額（自賠責：従来様式用）
               05  RECE82-KINGAKU-JYU     PIC  9(09).
      *
      *入院年月日
           03  RECE82-NYUINYMD            PIC  X(08).
      *
      *医療機関種別
           03  RECE82-HOSPSBT             PIC  X(01).
      *
      *病棟区分情報
           03  RECE82-BYOTOKBNINF         OCCURS  4.
      *病棟区分
               05  RECE82-BYOTOKBN        PIC  X(02).
      *
      *病棟種別情報
           03  RECE82-BYOTOSYUBETUINF     OCCURS 31.
      *病棟番号
               05  RECE82-BYOTONUM        PIC  X(02).
      *病棟種別
               05  RECE82-BYOTOSYUBETU    PIC  X(02).
      *
      *入院基本料種別情報
           03  RECE82-NYUSYUINF           OCCURS  20.
      *入院基本料種別
               05  RECE82-NYUSYUBETU      PIC  X(20).
      *
      *入院基本料情報
           03  RECE82-NYUKHNINF           OCCURS  20.
      *入院基本料・加算
               05  RECE82-NYUKHNRYO       PIC  9(08).
      *入院日数
               05  RECE82-NYUKHNNISSU     PIC  9(02).
      *合計点数
               05  RECE82-NYUKHNKEI       PIC  9(08).
      **合計金額（自賠責：従来様式用）
               05  RECE82-NYUKHNKIN-JYU   PIC  9(09).
      *入院基本料情報２
           03  RECE82-NYUKHNINF2.
      *合計点数２
               05  RECE82-NYUKHNKEI2      PIC S9(08).
      **合計金額２（自賠責：従来様式用）
               05  RECE82-NYUKHNKIN2-JYU  PIC S9(09).
      *
      *特定入院基本料情報
           03  RECE82-NYUTOKINF           OCCURS  20.
      *特定入院料・その他（点）
               05  RECE82-NYUTOKRYO       PIC  9(08).
      *特定入院料・その他（略称）
               05  RECE82-NYUTOKRYO-RYAKU PIC  X(20).
      *入院日数
               05  RECE82-NYUTOKNISSU     PIC  9(02).
      *合計点数
               05  RECE82-NYUTOKKEI       PIC  9(08).
      **合計金額（自賠責：従来様式用）
               05  RECE82-NYUTOKKIN-JYU   PIC  9(09).
      *特定入院基本料情報２
           03  RECE82-NYUTOKINF2.
      *合計点数２
               05  RECE82-NYUTOKKEI2      PIC S9(08).
      **合計金額２（自賠責：従来様式用）
               05  RECE82-NYUTOKKIN2-JYU  PIC S9(09).
      *
      *入院基本料点数計
           03  RECE82-NYUTOTALTEN         PIC  9(08).
      **入院基本料金額計（自賠責：従来様式用）
           03  RECE82-NYUTOTALKIN-JYU     PIC  9(09).
      *
      *合計点数
           03  RECE82-TOTALTEN            PIC  9(08).
      *合計点数（薬剤等）
           03  RECE82-TOTALTEN-YAK        PIC  9(08).
      **合計金額（自賠責：従来様式用）
           03  RECE82-TOTALKIN-JYU        PIC  9(09).
      *
      *診療区分別金額情報
           03  RECE82-SRYKBNINF2          OCCURS  5.
      *明細金額
               05  RECE82-KINGAKU2        PIC  9(09).
      *小計
           03  RECE82-KINGAKU2-SYO        PIC  9(09).
      *
      *診療区分別金額情報（その他）（自賠責のみ）
           03  RECE82-SRYKBNINF3          OCCURS  5.
      *明細金額（その他）
               05  RECE82-KINGAKU3        PIC  9(09).
      *小計（その他）
           03  RECE82-KINGAKU3-SYO        PIC  9(09).
      *入院室料加算（その他）
           03  RECE82-HEYA-NINZU          PIC  9(02).
           03  RECE82-HEYA-NISSU          PIC  9(02).
      *
      *食事種別情報
           03  RECE82-SHOKUJISYUINF       OCCURS   4.
      *食事種別
               05  RECE82-SHOKUJISYUBETU  PIC  X(04).
      *
      *食事基本料情報
           03  RECE82-SHOKUJIKHNINF       OCCURS   4.
      *食事基本料
               05  RECE82-SHOKUJIKHNRYO   PIC  9(09).
      *食事日数
               05  RECE82-SHOKUJIKHNNISSU PIC  9(02).
      *
      *食事種別情報‐基本（生活）
           03  RECE82-SHOKUJISYUBETU-SEI  PIC  X(02).
      *
      *食事基本料情報（生活）
           03  RECE82-SHOKUJIKHNINF-SEI   OCCURS   2.
      *食事基本料
               05  RECE82-SHOKUJIKHNRYO-SEI   PIC  9(09).
      *食事日数
               05  RECE82-SHOKUJIKHNNISSU-SEI PIC  9(02).
      *
      *食事基本料情報（その他）
           03  RECE82-SHOKUJIKHNINF-ETC       OCCURS   3.
      *食事基本料
               05  RECE82-SHOKUJIKHNRYO-ETC   PIC  9(09).
      *食事日数
               05  RECE82-SHOKUJIKHNNISSU-ETC PIC  9(02).
      *合計日数（食事）
           03  RECE82-SHOKUJITOTALDAY     PIC  9(02).
      *合計金額（食事）（ハ又はホ）
           03  RECE82-SHOKUJITOTALKIN     PIC  9(09).
      *
      *診断書料
           03  RECE82-SINDAN-TUU          PIC  9(02).
           03  RECE82-SINDAN-KIN          PIC  9(09).
      *明細書料
           03  RECE82-MEISAI-TUU          PIC  9(02).
           03  RECE82-MEISAI-KIN          PIC  9(09).
      **入院室料加算金額（自賠責：従来様式用）
           03  RECE82-SITURYO-JYU         PIC  9(09).
      **その他欄金額（自賠責：従来様式用）
           03  RECE82-SONOTAKIN-JYU       PIC  9(09).
      *
      *合計金額（イ又はＡ）
           03  RECE82-TOTALKIN            PIC  9(09).
      *合計金額（薬剤等）（Ｂ）
           03  RECE82-TOTALKIN-YAK        PIC  9(09).
      *合計金額（Ｃ）
           03  RECE82-TOTALKIN2           PIC  9(09).
      *合計金額（他）（Ｄ）
           03  RECE82-TOTALKIN-ETC        PIC  9(09).
      *
      *初診時間区分
           03  RECE82-SHOSHINTIME         PIC  X(01).
      *初診乳児区分
           03  RECE82-SHOSHIN-NYU         PIC  X(01).
      *初診年月日
           03  RECE82-SHOSHINYMD          PIC  X(08).
      *救急医療管理加算区分
           03  RECE82-KYUKYU-PRTKBN       PIC  X(01).
      *救急医療管理加算算定日
           03  RECE82-KYUKYUYMD           PIC  X(08).
      *日別手技点・薬剤等点数情報
           03  RECE82-TENSUINF            OCCURS  31.
               05  RECE82-SYUTEN-JI       PIC S9(08).
               05  RECE82-YKZTEN-JI       PIC  9(08).
      *コメントカウントフラグ
           03  RECE82-COMCNTFLG           PIC  X(01).
      *病床数編集フラグ（特定疾患療養指導料等）
           03  RECE82-BEDSUFLG            PIC  X(01).
      *外来迅速検体検査加算フラグ
           03  RECE82-KENTAIKASANFLG      PIC  X(01).
      *特別な関係にある他院歴ありフラグ
           03  RECE82-TAINFLG             PIC  X(01).
      *診療回数テーブル
           03  RECE82-DAY-AREA.
               05  RECE82-DAY             PIC  9(03)
                                               OCCURS   31.
      *レセプト種別不明エラー情報
           03  RECE82-ERR-AREA.
               05  RECE82-ERR-ERRKBN      PIC 9(02).
               05  RECE82-ERR-DAY         PIC 9(02).
               05  RECE82-ERR-HKNCOMBINUM PIC 9(04).
               05  RECE82-ERR-TEKSTYMD    PIC 9(08).
               05  RECE82-ERR-TEKEDYMD    PIC 9(08).
      *他保険入院料算定済みフラグ
           03  RECE82-NYUINRYOFLG         PIC  X(01).
      *
      *労災レセプト回数
           03  RECE82-RECEKAISU           PIC  9(03).
      *
      *公務災害−転記年月日
           03  RECE82-TENKIYMD            PIC  X(08).
      *
      *労災初回請求分フラグ
           03  RECE82-SYOKAIFLG           PIC  X(01).
      *
      *労災基準監督署コード
           03  RECE82-SYOCD               PIC  X(05).
      *
      *４００１情報
           03  RECE82-4001-INF.
      *点数単価
               05  RECE82-TANKA           PIC  9(02)V99.
      *自賠責加算率
               05  RECE82-KSNRATE         PIC  9(03).
      *自賠責文書料消費税
               05  RECE82-TAXKBN          PIC  X(01).
      *自賠責食事療養費
               05  RECE82-SKJKBN          PIC  X(01).
      *自賠責特定器材等消費税
               05  RECE82-TAXKBN-KZI      PIC  X(01).
      *自賠責固定帯加算等取扱
               05  RECE82-KOTEIKBN        PIC  X(01).
      *労災レセ回数記載
               05  RECE82-ROUKAISU        PIC  X(01).
      *自賠責区分
               05  RECE82-JIBAIKBN        PIC  X(01).
      *技術点単価
               05  RECE82-SYUTENTANKA     PIC  9(02).
      *薬剤等点単価
               05  RECE82-YKZTENTANKA     PIC  9(02).
      *公務災害区分
               05  RECE82-KOMUKBN         PIC  X(01).
      *公務災害点単価
               05  RECE82-KOMUTENTANKA    PIC  9(02).
      *労災指定医療機関区分
               05  RECE82-RSIIRYOKBN      PIC  X(01).
      *
      *０９５９３で始まるコードの有無区分
           03  RECE82-ORIGINALCDKBN       PIC 9(01).
      *
      *入院料情報取得フラグ
           03  RECE82-DOUJITSU-FLG        PIC 9(01).
      *入院料情報
           03  RECE82-DOUJITSU-AREA.
      *同日再入院（入院分）の保険組合せ
               05  RECE82-DOUJITSU-HKNCOMBI
                                          PIC 9(04)   OCCURS  31.
      *同日再入院（入院分）の食事開始時間区分
               05  RECE82-DOUJITSU-SKJKBN PIC X(01)   OCCURS  31.
      *入院区分
               05  RECE82-NYUINDAY-TBL.
                   07  RECE82-NYUINDAY-KBN
                                          PIC  X(01)  OCCURS  31.
      *
      *第三者行為時の医保分主保険情報
           03  RECE82-HKN-X.
      *医保分主保険番号
               05  RECE82-HKNNUM-X        PIC X(03).
      *医保分主保険ＩＤ
               05  RECE82-HKNID-X         PIC 9(10).
      *医保分保険者番号
               05  RECE82-HKNJANUM        PIC X(08).
      *医保分保険者名
               05  RECE82-HKNJANAME       PIC X(200).
      *医保分記号
               05  RECE82-KIGO            PIC X(80).
      *医保分番号
               05  RECE82-NUM             PIC X(80).
      *
      *第三者行為（社会保険への請求額）
           03  RECE82-SKYMONEY            PIC  9(09).
      *第三者行為（患者負担）
           03  RECE82-FTNMONEY            PIC  9(09).
      *第三者行為（患者割合）
           03  RECE82-FTNRATE             PIC  9(03).
      *第三者行為（食事標準負担）
           03  RECE82-SKJFTNMONEY         PIC  9(09).
      *第三者行為レセプト現物給付該当フラグ
           03  RECE82-DAISAN-GENBUTU-GAITO
                                          PIC  9(01).
      *第三者行為一部負担金金額置換フラグ
           03  RECE82-DAISAN-CHIKAN       PIC  9(01).
      *
      *超重症児（者）入院診療加算等の算定フラグ
           03  RECE82-CYOJKSNFLG          PIC  X(01).
      *
      *自賠責・請求金額欄端数処理区分
           03  RECE82-JIBAI-HASUKBN       PIC  X(01).
      *
      *自賠責（新様式）（ＯＣＲ様式）用
      *合計額
           03  RECE82-JIBAI-GOKEI-BE      PIC  9(09).
           03  RECE82-JIBAI-GOKEI-AF      PIC  9(09).
      *請求額
           03  RECE82-JIBAI-SKYKINFLG     PIC  9(01).
           03  RECE82-JIBAI-SKYKIN        PIC  9(09).
      *受領額・受領先
           03  RECE82-JIBAI-JYUKINFLG     PIC  9(01).
           03  RECE82-JIBAI-JYUKIN        PIC  9(09).
           03  RECE82-JIBAI-JYUNAMEFLG    PIC  9(01).
           03  RECE82-JIBAI-JYUNAME       PIC  X(80).
      *自賠責受領金額欄置換入力分
           03  RECE82-JIBAI-CHIKANKINFLG  PIC  9(01).
           03  RECE82-JIBAI-CHIKANKIN     PIC  9(09).
           03  RECE82-JIBAI-CHIKANNAMEFLG PIC  9(01).
           03  RECE82-JIBAI-CHIKANNAME    PIC  X(80).
      *
      *標準負担額経過措置（精神）
           03  RECE82-SEISIN-SKJFLG       PIC  9(01).
           03  RECE82-SEISIN-NYUINYMD     PIC  X(08).
      *
      *再診加算（入院）情報
           03  RECE82-NYUSAIINF.
               05  RECE82-NYUSAI-TENSU    PIC  9(08).
      *入院料日別診療区分情報
           03  RECE82-NYUDAYINF.
               05  RECE82-NYUDAY-SRYKBN   PIC  X(02)  OCCURS 31.
      *
      *妊婦フラグ
           03  RECE82-NINPUFLG            PIC  9(01).
      *
      *検査フラグ
           03  RECE82-KENSA-BNPFLG        PIC  9(01).
           03  RECE82-KENSA-NTPROFLG      PIC  9(01).
           03  RECE82-KENSA-ANPFLG        PIC  9(01).
      *
      *リハビリ情報
           03  RECE82-RIHAINF-AREA.
      *リハビリ算定テーブル２
               05  RECE82-RIHA2-DAY-AREA.
                 06  RECE82-RIHA2-SINDAI-AREA.
                   07  RECE82-RIHA2-SINDAI      PIC 9(01).
                   07  RECE82-RIHA2-SINYMD-OCC1.
                     08  RECE82-RIHA2-SINYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SINYMD-OCC2.
                     08  RECE82-RIHA2-SINYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-KOKYUKI-AREA.
                   07  RECE82-RIHA2-KOKYUKI     PIC 9(01).
                   07  RECE82-RIHA2-KOKYMD-OCC1.
                     08  RECE82-RIHA2-KOKYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-KOKYMD-OCC2.
                     08  RECE82-RIHA2-KOKYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-NOUKE-AREA.
                   07  RECE82-RIHA2-NOUKE       PIC 9(01).
                   07  RECE82-RIHA2-NOUYMD-OCC1.
                     08  RECE82-RIHA2-NOUYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-NOUYMD-OCC2.
                     08  RECE82-RIHA2-NOUYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-UNDOKI-AREA.
                   07  RECE82-RIHA2-UNDOKI      PIC 9(01).
                   07  RECE82-RIHA2-UNDYMD-OCC1.
                     08  RECE82-RIHA2-UNDYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-UNDYMD-OCC2.
                     08  RECE82-RIHA2-UNDYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-NANBYO-AREA.
                   07  RECE82-RIHA2-NANBYO      PIC 9(01).
                   07  RECE82-RIHA2-NANYMD-OCC1.
                     08  RECE82-RIHA2-NANYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-NANYMD-OCC2.
                     08  RECE82-RIHA2-NANYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-SYOGAI-AREA.
                   07  RECE82-RIHA2-SYOGAI      PIC 9(01).
                   07  RECE82-RIHA2-SYOYMD-OCC1.
                     08  RECE82-RIHA2-SYOYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SYOYMD-OCC2.
                     08  RECE82-RIHA2-SYOYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-SESSYOKU-AREA.
                   07  RECE82-RIHA2-SESSYOKU    PIC 9(01).
                   07  RECE82-RIHA2-SESYMD-OCC1.
                     08  RECE82-RIHA2-SESYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SESYMD-OCC2.
                     08  RECE82-RIHA2-SESYMD2   PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-HAIYO-AREA.
                   07  RECE82-RIHA2-HAIYO       PIC 9(01).
                   07  RECE82-RIHA2-HAIYMD-OCC1.
                     08  RECE82-RIHA2-HAIYMD1   PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-HAIYMD-OCC2.
                     08  RECE82-RIHA2-HAIYMD2   PIC 9(01)  OCCURS 31.
      *リハビリ加算算定テーブル２
               05  RECE82-RIHA2-DAY9-AREA.
                 06  RECE82-RIHA2-SINDAI9-AREA.
                   07  RECE82-RIHA2-SINDAI9     PIC 9(01).
                   07  RECE82-RIHA2-SIN9YMD-OCC1.
                     08  RECE82-RIHA2-SIN9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-SIN9YMD-OCC2.
                     08  RECE82-RIHA2-SIN9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-KOKYUKI9-AREA.
                   07  RECE82-RIHA2-KOKYUKI9    PIC 9(01).
                   07  RECE82-RIHA2-KOK9YMD-OCC1.
                     08  RECE82-RIHA2-KOK9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-KOK9YMD-OCC2.
                     08  RECE82-RIHA2-KOK9YMD2  PIC 9(01)  OCCURS 31.
      *
                 06  RECE82-RIHA2-HAIYO9-AREA.
                   07  RECE82-RIHA2-HAIYO9      PIC 9(01).
                   07  RECE82-RIHA2-HAI9YMD-OCC1.
                     08  RECE82-RIHA2-HAI9YMD1  PIC X(08)  OCCURS  2.
                   07  RECE82-RIHA2-HAI9YMD-OCC2.
                     08  RECE82-RIHA2-HAI9YMD2  PIC 9(01)  OCCURS 31.
      *
      *排尿自立支援加算の該当月最初の算定日
           03  RECE82-HAIJSKYMD                 PIC X(08).
      *
      *予備
           03  RECE82-YOBIETC-AREA.
               05  RECE82-YOBIETCINF           OCCURS  10.
                   07  RECE82-YOBIETC     PIC  9(09).
      *
