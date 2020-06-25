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
      *    レセプト明細書     COPY (CPRCF044.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02.09.20  日数のテーブルを変更
      *  01.00.02    NACL-門脇    03.03.03  剤番号桁修正
      *  01.00.03    NACL-門脇    03.09.24  コメント編集フラグ追加
      *  01.00.04    NACL-門脇    04.02.05  診療コード＊数量の累計
      *                                     追加
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料
      *                                     の追加
      *  01.01.02    NACL-門脇    04.04.10  在宅患者訪問点滴注射管理指
      *                                     導料算定時の点滴注射の追加
      *  01.01.03    NACL-門脇    05.01.17  酸素の追加
      *  01.01.04    NACL-門脇    05.02.28  （サ）フラグの追加
      *  02.09.01    NACL-門脇    06.04.17  ２００６年４月改正対応
      *  03.00.01    NACL-門脇    06.06.22  往診・特別往診、
      *                                     在宅患者訪問診療料の追加
      *  03.01.01    NACL-門脇    06.07.11  在支援に関する診療の追加
      *  03.01.02    NACL-門脇    06.07.14  診療区分置き換えフラグの追加
      *  03.04.01    NACL-門脇    06.12.01  自賠責健保準拠対応
      *  03.05.01    NACL-門脇    07/05/28  グループ診療対応
      *  04.02.01    NACL-門脇    08/03/28  小児特定疾患カウンセリングの追加
      *  04.03.01    NACL-門脇    08/07/14  投薬剤点０フラグの追加
      *  04.03.02    NACL-門脇    08/09/25  自賠責集計等取扱の追加
      *
      *  04.04.01    NACL-門脇    09/02/04  検査（ＢＮＰ等）の追加
      *  04.04.02    NACL-門脇    10/03/23  同日電話再診フラグの追加
      *  04.04.03    NACL-門脇    10/03/29  画像診断（２回目以降減算あり）
      *                                     の追加
      *  04.04.04    NACL-門脇    10/04/06  改正対応項目追加
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    10/01/19  剤区分追加
      *  04.05.03    NACL-門脇    12/04/10  改正対応項目追加
      *
      *  04.07.01    NACL-門脇    12/09/25  コメント差込項目追加
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.03    NACL-門脇    13/02/12  労災レセ電対応
      *                                     剤（点数）・剤（金額）追加
      *  04.07.04    NACL-門脇    14/04/07  改正対応項目追加
      *  04.07.05    NACL-門脇    15/03/09  訪問診療にかかる記録書コメント
      *                                     フラグ追加
      *
      *  04.08.01    NACL-門脇    13/01/22  日数回数テーブル１０回対応
      *  04.08.02    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.03    NACL-門脇    16/03/31  平成２８年４月改正対応
      *  04.08.04    NACL-門脇    16/08/02  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.05    NACL-門脇    18/04/02  平成３０年４月改正対応
      *  04.08.06    NACL-門脇    18/05/07  平成３０年４月改正対応
      *  04.08.07    NACL-門脇    18/09/27  選択式コメント対応
      *  05.00.01    NACL-門脇    20/04/07  令和２年４月改正対応
      ************************************************************
      *01  RECE44-REC.
           03  RECE44-KEY.
               05  RECE44-KEY1.
      *医療機関ＩＤ
                   07  RECE44-HOSPNUM     PIC  9(02).
      *診療年月
                   07  RECE44-SRYYM       PIC  9(06).
      *入外区分
                   07  RECE44-NYUGAIKBN   PIC  X(01).
      *患者ＩＤ
                   07  RECE44-PTID        PIC  9(10).
      *レセプト科
                   07  RECE44-RECEKA      PIC  X(02).
      *保険ＩＤ        
                   07  RECE44-HKNID       PIC  9(10).
      *適用開始年月日
                   07  RECE44-TEKSTYMD    PIC  X(08).
      *剤番号
               05  RECE44-ZAINUM          PIC  9(08).
      *診療区分
           03  RECE44-SRYKBN              PIC  X(02).
      *
           03  RECE44-DATA-AREA. 
      *診療種別区分
               05  RECE44-SRYSYUKBN           PIC  X(03).
      *患者番号
               05  RECE44-PTNUM               PIC  X(20).
      *診療科
               05  RECE44-SRYKA               PIC  X(02).
      *保険組合せ番号
               05  RECE44-HKNCOMBI            PIC  9(04).
      *自保区分
               05  RECE44-JIHOKBN             PIC  X(01).
      *剤点数
               05  RECE44-ZAITEN              PIC  9(08).
      *診療コード計
               05  RECE44-SRYCDTOTAL          PIC  9(14).
      *数量計
               05  RECE44-SURYOUTOTAL         PIC  9(07)V9(05).
      *診療コード＊数量の累計
               05  RECE44-RUIKEITOTAL         PIC  9(13)V9(05).
      *明細数
               05  RECE44-MEISAISU            PIC  9(04).
      *剤回数
               05  RECE44-ZAIKAISU            PIC  9(04).
      *手技点数１
               05  RECE44-SYUTEN1             PIC  9(07).
      *手技点数２
               05  RECE44-SYUTEN2             PIC  9(07).
      *薬剤点数
               05  RECE44-YKZTEN              PIC  9(07).
      *器材点数
               05  RECE44-KIZAITEN            PIC  9(07).
      *ドクターコード
               05  RECE44-DRCD                PIC  X(05).
      *剤請求フラグ
               05  RECE44-ZAIREQFLG           PIC  X(01).
      *逓減率
               05  RECE44-TEIGENRATE          PIC  X(01).
      *コメントコードフラグ
               05  RECE44-COMCDFLG            PIC  X(01).
      *（サ）フラグ
               05  RECE44-SAFLG               PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
               05  RECE44-TOYAKUFLG           PIC  X(01).
      *診療区分置き換えフラグ（自賠責リハ用）
               05  RECE44-OKIKAEFLG           PIC  X(01).
      *自賠責集計等取扱
               05  RECE44-JIBAISYUKEIKBN      PIC  9(01).
      *関係コメント存在フラグ
               05  RECE44-SETCOMFLG           PIC  X(01).
      *選択式コメント存在フラグ
               05  RECE44-SENTAKUCOMFLG       PIC  X(01).
      *剤区分
               05  RECE44-ZAIKBN              PIC  9(03).
      *画像診断（２回目以降減算あり）
               05  RECE44-GAZOGEN             PIC  X(01).
      *訪問診療にかかる記録書コメントフラグ
               05  RECE44-HOMONCOMFLG         PIC  X(01).
      *院外湿布薬７０枚超理由コメントフラグ
               05  RECE44-SIPPUCOMFLG         PIC  X(01).
      *湿布薬フラグ（１：院外分　２：院内分）
               05  RECE44-SIPPUFLG            PIC  X(01).
      *１５円以下薬剤レセプト記載フラグ
               05  RECE44-YKZKISAIFLG         PIC  X(01).
      *
      *初診料算定科：
               05  RECE44-SYOSANTEIKA-COMMENT PIC  X(01).
      *再診料算定科：
               05  RECE44-SAISANTEIKA-COMMENT PIC  X(01).
      *電話再診
               05  RECE44-DENWASAI-COMMENT    PIC  X(01).
      *電話再診（３０年３月以前継続）
               05  RECE44-DENWASAI2-COMMENT   PIC  X(01).
      *同日再診
               05  RECE44-DOUJITUSAI-COMMENT  PIC  X(01).
      *同日電話再診
               05  RECE44-DOUDENSAI-COMMENT   PIC  X(01).
      *同日電話再診（３０年３月以前継続）
               05  RECE44-DOUDENSAI2-COMMENT  PIC  X(01).
      *リハビリ
               05  RECE44-RIHABIRI-COMMENT    PIC  X(01).
      *リハビリ（心大血管疾患）
               05  RECE44-SINDAI-COMMENT      PIC  X(01).
      *リハビリ（呼吸器）
               05  RECE44-KOKYUKI-COMMENT     PIC  X(01).
      *リハビリ（脳血管疾患等）
               05  RECE44-NOUKE-COMMENT       PIC  X(01).
      *リハビリ（運動器）
               05  RECE44-UNDOKI-COMMENT      PIC  X(01).
      *リハビリ（難病患者）
               05  RECE44-NANBYO-COMMENT      PIC  X(01).
      *リハビリ（障害児（者））
               05  RECE44-SYOGAI-COMMENT      PIC  X(01).
      *リハビリ（摂食）
               05  RECE44-SESSYOKU-COMMENT    PIC  X(01).
      *リハビリ（廃用症候群）
               05  RECE44-HAIYO-COMMENT       PIC  X(01).
      *介達牽引
               05  RECE44-KAITATU-COMMENT     PIC  X(01).
      *
      *リハビリ加算（心大血管疾患）
               05  RECE44-SINDAI9-COMMENT     PIC  X(01).
      *リハビリ加算（呼吸器）
               05  RECE44-KOKYUKI9-COMMENT    PIC  X(01).
      *リハビリ加算（廃用症候群）
               05  RECE44-HAIYO9-COMMENT      PIC  X(01).
      *
      *慢性疼痛疾患管理料
               05  RECE44-FLG-SHIDOU-COMMENT  PIC  X(01).
      *傷病手当金意見書交付料
               05  RECE44-FLG-SYOBYO-COMMENT  PIC  X(01).
      *療養費同意書交付料
               05  RECE44-FLG-RYOYO-COMMENT   PIC  X(01).
      *訪問看護指示料
               05  RECE44-FLG-HOUSIJI-COMMENT PIC  X(01).
      *手術（輸血は除く）
               05  RECE44-FLG-SHUJUTU-COMMENT PIC  X(01).
      *ＣＴ・ＭＲＩ
               05  RECE44-FLG-CTMRI-COMMENT   PIC  X(01).
      *在宅血液透析頻回指導管理料
               05  RECE44-FLG-HINKAI-COMMENT  PIC  X(01).
      *在宅患者訪問点滴注射管理指導料算定時の点滴注射
               05  RECE44-FLG-HOUTEN-COMMENT  PIC  X(01).
      *酸素
               05  RECE44-FLG-SANSO-COMMENT   PIC  X(01).
      *初診（同日複数診療科）
               05  RECE44-FLG-SYODOU-COMMENT  PIC  X(01).
               05  RECE44-SYODOU-SRYKA-AREA.
                   07  RECE44-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *再診（同一日２科目）
               05  RECE44-FLG-SAIDOU-COMMENT  PIC  X(01).
               05  RECE44-SAIDOU-SRYKA-AREA.
                   07  RECE44-SAIDOU-SRYKA    PIC  X(02)     OCCURS  10.
      *ニコチン依存症管理料
               05  RECE44-FLG-NIKOCHIN-COMMENT PIC X(01).
      *重度喘息患者治療管理加算
               05  RECE44-FLG-ZENSOKU-COMMENT PIC  X(01).
      *検査等実施判断
               05  RECE44-KNSJISKBN           PIC  9(01).
      *検査等実施判断グループ
               05  RECE44-KNSJISGRPKBN        PIC  9(02).
      *往診・特別往診
               05  RECE44-FLG-OSIN-COMMENT    PIC  X(01).
      *在宅患者訪問診療料
               05  RECE44-FLG-ZAIHOU-COMMENT  PIC  X(01).
      *在支援に関する診療
               05  RECE44-FLG-ZAISIEN-COMMENT PIC  X(01).
      *小児特定疾患カウンセリング
               05  RECE44-FLG-SYOUNI-COMMENT  PIC  X(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
               05  RECE44-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（遠隔モニタリング）
               05  RECE44-FLG-SINPESU-COMMENT PIC  X(01).
      *集団栄養食事指導料
               05  RECE44-FLG-SYUEIYO-COMMENT PIC  X(01).
      *人工腎臓等
               05  RECE44-FLG-JINZOU-COMMENT  PIC  X(01).
      *局所陰圧閉鎖処置
               05  RECE44-FLG-KYOKUIN-COMMENT PIC  X(01).
      *麻酔
               05  RECE44-FLG-MASUI-COMMENT   PIC  X(01).
      *認知療法・認知行動療法
               05  RECE44-FLG-NINRYO-COMMENT  PIC  X(01).
      *
      *画像診断フイルム
               05  RECE44-FLG-FILM-COMMENT    PIC  X(01).
      *
      *認知症専門診断管理料２他
               05  RECE44-FLG-NINSEN-COMMENT  PIC  X(01).
      *退院時共同指導料１（１以外）
               05  RECE44-FLG-TAKYO-COMMENT   PIC  X(01).
      *退院時共同指導料特別管理指導加算
               05  RECE44-FLG-TAKYOKSN-COMMENT
                                           PIC X(01).
      *外来リハビリテーション診療料・外来放射線照射診療料
               05  RECE44-FLG-GAIRIHA-COMMENT PIC  X(01).
      *介護職員等喀痰吸引等指示料
               05  RECE44-FLG-KAISYOKU-COMMENT
                                           PIC X(01).
      *
      *在宅持続陽圧呼吸療法指導管理料
               05  RECE44-FLG-YOUATU-COMMENT
                                           PIC X(01).
      *外来管理加算（読み替え）
               05  RECE44-FLG-GAIKAN-COMMENT
                                           PIC X(01).
      *
      *心臓ペースメーカー指導管理料（着用型自動除細動器）
      *心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
               05  RECE44-FLG-SINPESUR-COMMENT PIC  X(01).
      *がん患者指導管理料２・３
               05  RECE44-FLG-GANSIDO-COMMENT  PIC  X(01).
      *在宅患者共同診療料
               05  RECE44-FLG-ZAIKYO-COMMENT   PIC  X(01).
      *在宅患者訪問リハビリテーション指導管理料
               05  RECE44-FLG-ZAIRIHA-COMMENT  PIC  X(01).
      *磁気による膀胱等刺激法
               05  RECE44-FLG-BOKOHO-COMMENT   PIC  X(01).
      *通院・在宅精神療法（２０歳未満）加算
               05  RECE44-FLG-TUINZAI-COMMENT  PIC  X(01).
               05  RECE44-FLG-TUINZAI2-COMMENT PIC  X(01).
      *依存症集団療法
               05  RECE44-FLG-IZONSYO-COMMENT  PIC  X(01).
      *在宅患者訪問薬剤管理指導料
               05  RECE44-FLG-ZAIYAK-COMMENT   PIC  X(01).
      *乳腺炎重症化予防ケア・指導料
               05  RECE44-FLG-NYUSEN-COMMENT   PIC  X(01).
      *高気圧酸素治療
               05  RECE44-FLG-KIATU-COMMENT    PIC  X(01).
      *
      *オンライン診療料
               05  RECE44-FLG-ONLINE-COMMENT   PIC  X(01).
               05  RECE44-FLG-ONLINE2-COMMENT  PIC  9(02).
               05  RECE44-FLG-ONLINE3-COMMENT  PIC  X(01).
      *
      *人工内耳機器調整加算
               05  RECE44-FLG-JINNAI-COMMENT   PIC  X(01).
      *
      *外来排尿自立指導料
               05  RECE44-FLG-GAIJIR-COMMENT   PIC  X(01).
      *
      *手技毎の内容（手術・画像診断・処置）
               05  RECE44-ACCTP-SYUGI-TBL-G.
                   07  RECE44-ACCTP-SYUGI-TBL  OCCURS  5.
      *手技開始コード
                       09  RECE44-ACCTP-SRYCD  PIC X(09).
      *手技点数
                       09  RECE44-ACCTP-SYUTEN PIC 9(07).
      *薬剤点数
                       09  RECE44-ACCTP-YKZTEN PIC 9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                       09  RECE44-ACCTP-KIZAITEN
                                               PIC 9(07).
      *フィルム点数
                       09  RECE44-ACCTP-FIRTEN PIC 9(07).
      *酸素点数
               05  RECE44-ACCTP-SANSOTEN   PIC 9(07).
      *窒素点数
               05  RECE44-ACCTP-CHISOTEN   PIC 9(07).
      *
      *包括診療分退避点数
               05  RECE44-HOKATU-TENSU-AREA.
      *剤点数
                   07    RECE44-HOKATU-ZAITEN  PIC 9(08).
      *手技点数１
                   07    RECE44-HOKATU-SYUTEN1 PIC 9(07).
      *手技点数２
                   07    RECE44-HOKATU-SYUTEN2 PIC 9(07).
      *薬剤点数
                   07    RECE44-HOKATU-YKZTEN  PIC 9(07).
      *器材点数
                   07    RECE44-HOKATU-KIZAITEN
                                               PIC 9(07).
      *
      *剤（点数）
               05  RECE44-ZAI-TENSU        PIC 9(09).
      *剤（金額）
               05  RECE44-ZAI-KINGAKU      PIC 9(09).
      *
      *日数回数テーブル
               05  RECE44-DAY-AREA.
      *            診療回数テーブル（1:合計、2-9:１回〜９回）
                    07  RECE44-DAY-TBL         OCCURS  10.
                        09  RECE44-KAISUINF    OCCURS  31.
                            11  RECE44-KAISU   PIC 9(03).
      *レセプト種別
               05  RECE44-RECESYUBETU          PIC 9(04).

