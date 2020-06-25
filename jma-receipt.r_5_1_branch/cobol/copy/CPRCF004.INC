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
      *    レセプト明細書     COPY (CPRCF04.INC)        *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *  01.00.02    NACL-藤原    02.09.02  日数のテーブルを変更
      *  01.00.03    NACL-藤原    03/03/12  補助区分の追加
      *  01.00.04    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.05    NACL-門脇    03.09.24  コメント編集フラグ追加
      *  01.00.06    NACL-門脇    04.02.05  診療コード＊数量の累計
      *                                     追加
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料
      *                                     の追加
      *  01.01.02    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指
      *                                     導料算定時の点滴注射の追加
      *  01.01.03    NACL-門脇    05.01.17  酸素の追加
      *  01.01.04    NACL-門脇    05.02.28  （サ）フラグの追加
      *
      *  02.09.01    NACL-門脇    06.04.17  ２００６年４月改正対応
      *  03.00.01    NACL-門脇    06.06.22  往診・特別往診、
      *                                     在宅患者訪問診療料の追加
      *  03.01.01    NACL-門脇    06.07.11  在支援に関する診療の追加
      *
      *  03.03.01    NACL-藤原    06/09/29  検査の判断料の記載方法の変更
      *                                     検査等実施判断の追加
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/28  小児特定疾患カウンセリングの追加
      *  04.03.01    NACL-門脇    08/07/14  投薬剤点０フラグの追加
      *
      *  04.04.01    NACL-門脇    09/02/04  検査（ＢＮＰ等）の追加
      *  04.04.02    NACL-門脇    10/03/23  同日電話再診フラグの追加
      *  04.04.03    NACL-門脇    10/03/29  画像診断（２回目以降減算あり）
      *                                     の追加
      *  04.04.04    NACL-門脇    10/04/06  改正対応項目追加
      *
      *  04.05.01    NACL-藤原    09/06/10  摘要欄下部へのコメント記載対応
      *                                     診療種別区分の追加
      *  04.05.02    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.03    NACL-門脇    09/11/30  レセ電フォーマットコメント拡張
      *                                     対応（ファイル編集）
      *  04.05.04    NACL-門脇    10/01/12  剤区分追加
      *  04.05.05    NACL-門脇    11/11/30  画像診断フイルム追加
      *  04.05.06    NACL-門脇    12/04/10  改正対応項目追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *  04.06.02    NACL-藤原    11/01/05  機能追加（注加算、同時併施等）
      *  04.06.03    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-門脇    12/09/25  コメント差込項目追加
      *  04.07.02    NACL-門脇    14/04/07  改正対応項目追加
      *  04.07.03    NACL-門脇    14/09/01  訪問診療にかかる記録書コメント
      *                                     フラグ追加
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-門脇    16/02/05  医保レセプト「他保険にて診察料算定
      *                                     済」コメント不記載対応
      *  04.08.04    NACL-門脇    16/03/31  平成２８年４月改正対応
      *  04.08.05    NACL-門脇    16/08/02  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.06    NACL-門脇    18/04/02  平成３０年４月改正対応
      *  04.08.07    NACL-門脇    18/05/07  平成３０年４月改正対応
      *  04.08.08    NACL-門脇    18/09/27  選択式コメント対応
      *  05.00.01    NACL-門脇    20/04/07  令和２年４月改正対応
      ************************************************************
      *01  RECE04-REC.
           03  RECE04-KEY.
               05  RECE04-KEY1.
      *医療機関ＩＤ
                   07  RECE04-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE04-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE04-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE04-PTID        PIC 9(10).
      *レセプト科
                   07  RECE04-RECEKA      PIC X(02).
      *審査支払機関
                   07  RECE04-TEISYUTUSAKI
                                          PIC 9(01).
      *レセプト種別
                   07  RECE04-RECESYUBETU PIC 9(04).
      *保険者番号        
                   07  RECE04-HKNJANUM    PIC X(08).
      *補助区分
                   07  RECE04-HOJOKBN     PIC X(01).
      *公費ＩＤ
                   07  RECE04-KOHID       PIC 9(10).
      *適用開始年月日
                   07  RECE04-TEKSTYMD    PIC X(08). 
      *分割区分
                   07  RECE04-RECEIPT-KBN PIC X(10).
      *剤番号
               05  RECE04-ZAINUM          PIC  9(08).
      *公費分離区分
           03  RECE04-KOHBUNRI            PIC  9(01).
      *診療区分
           03  RECE04-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE04-SRYSYUKBN           PIC  X(03).
      *患者番号
           03  RECE04-PTNUM               PIC  X(20).
      *診療科
           03  RECE04-SRYKA               PIC  X(02).
      *保険組合せ番号
           03  RECE04-HKNCOMBI            PIC  9(04).
      *自保区分
           03  RECE04-JIHOKBN             PIC  X(01).
      *剤点数
           03  RECE04-ZAITEN              PIC  9(08).
      *診療コード計
           03  RECE04-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  RECE04-SURYOUTOTAL         PIC  9(07)V9(05).
      *診療コード＊数量の累計
           03  RECE04-RUIKEITOTAL         PIC  9(13)V9(05).
      *明細数
           03  RECE04-MEISAISU            PIC  9(04).
      *剤回数
           03  RECE04-ZAIKAISU            PIC  9(04).
      *手技点数１
           03  RECE04-SYUTEN1             PIC  9(07).
      *手技点数２
           03  RECE04-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  RECE04-YKZTEN              PIC  9(07).
      *器材点数
           03  RECE04-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  RECE04-DRCD                PIC  X(05).
      *剤請求フラグ
           03  RECE04-ZAIREQFLG           PIC  X(01).
      *逓減率
           03  RECE04-TEIGENRATE          PIC  X(01).
      *コメントコードフラグ
           03  RECE04-COMCDFLG            PIC  X(01).
      *（サ）フラグ
           03  RECE04-SAFLG               PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
           03  RECE04-TOYAKUFLG           PIC  X(01).
      *関係コメント存在フラグ
           03  RECE04-SETCOMFLG           PIC  X(01).
      *選択式コメント存在フラグ
           03  RECE04-SENTAKUCOMFLG       PIC  X(01).
      *剤区分
           03  RECE04-ZAIKBN              PIC  9(03).
      *画像診断（２回目以降減算あり）
           03  RECE04-GAZOGEN             PIC  X(01).
      *訪問診療にかかる記録書コメントフラグ
           03  RECE04-HOMONCOMFLG         PIC  X(01).
      *他保険にて診察料算定済不記載フラグ
           03  RECE04-TAHOKENNGFLG        PIC  X(01).
      *院外湿布薬７０枚超理由コメントフラグ
           03  RECE04-SIPPUCOMFLG         PIC  X(01).
      *湿布薬フラグ（１：院外分　２：院内分）
           03  RECE04-SIPPUFLG            PIC  X(01).
      *１５円以下薬剤レセプト記載フラグ
           03  RECE04-YKZKISAIFLG         PIC  X(01).
      *
      *初診料算定科：
           03  RECE04-SYOSANTEIKA-COMMENT PIC  X(01).
      *再診料算定科：
           03  RECE04-SAISANTEIKA-COMMENT PIC  X(01).
      *電話再診
           03  RECE04-DENWASAI-COMMENT    PIC  X(01).
      *電話再診（３０年３月以前継続）
           03  RECE04-DENWASAI2-COMMENT   PIC  X(01).
      *同日再診
           03  RECE04-DOUJITUSAI-COMMENT  PIC  X(01).
      *同日電話再診
           03  RECE04-DOUDENSAI-COMMENT   PIC  X(01).
      *同日電話再診（３０年３月以前継続）
           03  RECE04-DOUDENSAI2-COMMENT  PIC  X(01).
      *再診（４回目以降／人工腎臓）
           03  RECE04-SAIJINKO-COMMENT    PIC  X(01).
      *人工腎臓
           03  RECE04-JINKOJIN-COMMENT    PIC  X(01).
      *リハビリ
           03  RECE04-RIHABIRI-COMMENT    PIC  X(01).
      *リハビリ（心大血管疾患）
           03  RECE04-SINDAI-COMMENT      PIC  X(01).
      *リハビリ（呼吸器）
           03  RECE04-KOKYUKI-COMMENT     PIC  X(01).
      *リハビリ（脳血管疾患等）
           03  RECE04-NOUKE-COMMENT       PIC  X(01).
      *リハビリ（運動器）
           03  RECE04-UNDOKI-COMMENT      PIC  X(01).
      *リハビリ（難病患者）
           03  RECE04-NANBYO-COMMENT      PIC  X(01).
      *リハビリ（障害児（者））
           03  RECE04-SYOGAI-COMMENT      PIC  X(01).
      *リハビリ（摂食）
           03  RECE04-SESSYOKU-COMMENT    PIC  X(01).
      *リハビリ（廃用症候群）
           03  RECE04-HAIYO-COMMENT       PIC  X(01).
      *介達牽引
           03  RECE04-KAITATU-COMMENT     PIC  X(01).
      *
      *リハビリ加算（心大血管疾患）
           03  RECE04-SINDAI9-COMMENT     PIC  X(01).
      *リハビリ加算（呼吸器）
           03  RECE04-KOKYUKI9-COMMENT    PIC  X(01).
      *リハビリ加算（廃用症候群）
           03  RECE04-HAIYO9-COMMENT      PIC  X(01).
      *
      *慢性疼痛疾患管理料
           03  RECE04-FLG-SHIDOU-COMMENT  PIC  X(01).
      *傷病手当金意見書交付料
           03  RECE04-FLG-SYOBYO-COMMENT  PIC  X(01).
      *療養費同意書交付料
           03  RECE04-FLG-RYOYO-COMMENT   PIC  X(01).
      *訪問看護指示料
           03  RECE04-FLG-HOUSIJI-COMMENT PIC  X(01).
      *手術（輸血は除く）
           03  RECE04-FLG-SHUJUTU-COMMENT PIC  X(01).
      *ＣＴ・ＭＲＩ
           03  RECE04-FLG-CTMRI-COMMENT   PIC  X(01).
      *在宅血液透析頻回指導管理料
           03  RECE04-FLG-HINKAI-COMMENT  PIC  X(01).
      *在宅患者訪問点滴注射管理指導料算定時の点滴注射
           03  RECE04-FLG-HOUTEN-COMMENT  PIC  X(01).
      *酸素
           03  RECE04-FLG-SANSO-COMMENT   PIC  X(01).
      *初診（同日複数診療科）
           03  RECE04-FLG-SYODOU-COMMENT  PIC  X(01).
           03  RECE04-SYODOU-SRYKA-AREA.
               05  RECE04-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *再診（同一日２科目）
           03  RECE04-FLG-SAIDOU-COMMENT  PIC  X(01).
           03  RECE04-SAIDOU-SRYKA-AREA.
               05  RECE04-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
      *ニコチン依存症管理料
           03  RECE04-FLG-NIKOCHIN-COMMENT PIC X(01).
      *重度喘息患者治療管理加算
           03  RECE04-FLG-ZENSOKU-COMMENT PIC  X(01).
      *検査等実施判断
           03  RECE04-KNSJISKBN           PIC  9(01).
      *検査等実施判断グループ
           03  RECE04-KNSJISGRPKBN        PIC  9(02).
      *往診・特別往診
           03  RECE04-FLG-OSIN-COMMENT    PIC  X(01).
      *在宅患者訪問診療料
           03  RECE04-FLG-ZAIHOU-COMMENT  PIC  X(01).
      *在支援に関する診療
           03  RECE04-FLG-ZAISIEN-COMMENT PIC  X(01).
      *小児特定疾患カウンセリング
           03  RECE04-FLG-SYOUNI-COMMENT  PIC  X(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
           03  RECE04-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（遠隔モニタリング）
           03  RECE04-FLG-SINPESU-COMMENT PIC  X(01).
      *集団栄養食事指導料
           03  RECE04-FLG-SYUEIYO-COMMENT PIC  X(01).
      *人工腎臓等
           03  RECE04-FLG-JINZOU-COMMENT  PIC  X(01).
      *局所陰圧閉鎖処置
           03  RECE04-FLG-KYOKUIN-COMMENT PIC  X(01).
      *麻酔
           03  RECE04-FLG-MASUI-COMMENT   PIC  X(01).
      *認知療法・認知行動療法
           03  RECE04-FLG-NINRYO-COMMENT  PIC  X(01).
      *
      *画像診断フイルム
           03  RECE04-FLG-FILM-COMMENT    PIC  X(01).
      *
      *認知症専門診断管理料２他
           03  RECE04-FLG-NINSEN-COMMENT  PIC  X(01).
      *退院時共同指導料１（１以外）
           03  RECE04-FLG-TAKYO-COMMENT   PIC  X(01).
      *退院時共同指導料特別管理指導加算
           03  RECE04-FLG-TAKYOKSN-COMMENT
                                          PIC  X(01).
      *外来リハビリテーション診療料・外来放射線照射診療料
           03  RECE04-FLG-GAIRIHA-COMMENT PIC  X(01).
      *介護職員等喀痰吸引等指示料
           03  RECE04-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *
      *在宅持続陽圧呼吸療法指導管理料
           03  RECE04-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（着用型自動除細動器）
      *心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
           03  RECE04-FLG-SINPESUR-COMMENT
                                          PIC  X(01).
      *がん患者指導管理料２・３
           03  RECE04-FLG-GANSIDO-COMMENT PIC  X(01).
      *在宅患者共同診療料
           03  RECE04-FLG-ZAIKYO-COMMENT  PIC  X(01).
      *在宅患者訪問リハビリテーション指導管理料
           03  RECE04-FLG-ZAIRIHA-COMMENT PIC  X(01).
      *磁気による膀胱等刺激法
           03  RECE04-FLG-BOKOHO-COMMENT  PIC  X(01).
      *通院・在宅精神療法（２０歳未満）加算
           03  RECE04-FLG-TUINZAI-COMMENT PIC  X(01).
           03  RECE04-FLG-TUINZAI2-COMMENT
                                          PIC  X(01).
      *依存症集団療法
           03  RECE04-FLG-IZONSYO-COMMENT PIC  X(01).
      *在宅患者訪問薬剤管理指導料
           03  RECE04-FLG-ZAIYAK-COMMENT  PIC  X(01).
      *乳腺炎重症化予防ケア・指導料
           03  RECE04-FLG-NYUSEN-COMMENT  PIC  X(01).
      *高気圧酸素治療
           03  RECE04-FLG-KIATU-COMMENT   PIC  X(01).
      *
      *オンライン診療料
           03  RECE04-FLG-ONLINE-COMMENT  PIC  X(01).
           03  RECE04-FLG-ONLINE2-COMMENT PIC  9(02).
           03  RECE04-FLG-ONLINE3-COMMENT PIC  X(01).
      *
      *人工内耳機器調整加算
           03  RECE04-FLG-JINNAI-COMMENT  PIC  X(01).
      *
      *外来排尿自立指導料
           03  RECE04-FLG-GAIJIR-COMMENT  PIC  X(01).
      *
      *日数回数テーブル
           03  RECE04-DAY-AREA.
      *        診療回数テーブル（1:合計、2-9:１回〜９回）
             04  RECE04-DAY-TBL            OCCURS  10.
                 05  RECE04-KAISUINF       OCCURS  31.
                     07  RECE04-KAISU      PIC  9(03).
      *
      *手技毎の内容（手術・画像診断・処置）
           03  RECE04-ACCTP-SYUGI-TBL-G.
               05  RECE04-ACCTP-SYUGI-TBL  OCCURS  5.
      *手技開始コード
                   07  RECE04-ACCTP-SRYCD  PIC X(09).
      *手技点数
                   07  RECE04-ACCTP-SYUTEN PIC 9(07).
      *薬剤点数
                   07  RECE04-ACCTP-YKZTEN PIC 9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                   07  RECE04-ACCTP-KIZAITEN
                                           PIC 9(07).
      *フィルム点数
                   07  RECE04-ACCTP-FIRTEN PIC 9(07).
      *酸素点数
           03  RECE04-ACCTP-SANSOTEN       PIC 9(07).
      *窒素点数
           03  RECE04-ACCTP-CHISOTEN       PIC 9(07).
      *
      *包括診療分退避点数
           03  RECE04-HOKATU-TENSU-AREA.
      *剤点数
               05  RECE04-HOKATU-ZAITEN    PIC 9(08).
      *手技点数１
               05  RECE04-HOKATU-SYUTEN1   PIC 9(07).
      *手技点数２
               05  RECE04-HOKATU-SYUTEN2   PIC 9(07).
      *薬剤点数
               05  RECE04-HOKATU-YKZTEN    PIC 9(07).
      *器材点数
               05  RECE04-HOKATU-KIZAITEN  PIC 9(07).
