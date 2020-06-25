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
      *    公害レセプト（入院）・点数情報    COPY (CPRCF220.INC) 
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.03.01    NACL-門脇    08/07/14  投薬剤点０フラグの追加
      *
      *  04.04.01    NACL-門脇    09/02/04  検査（ＢＮＰ等）の追加
      *  04.04.02    NACL-門脇    10/03/29  画像診断（２回目以降減算あり）
      *                                     の追加
      *  04.04.03    NACL-門脇    10/04/06  改正対応項目追加
      *
      *  04.05.01    NACL-藤原    09/07/14  小数点以下５桁対応
      *  04.05.02    NACL-門脇    10/01/19  剤区分追加
      *  04.05.03    NACL-門脇    12/04/10  改正対応項目追加
      *
      *  04.07.01    NACL-門脇    12/09/25  コメント差込項目追加
      *  04.07.02    NACL-門脇    14/04/07  改正対応項目追加
      *
      *  04.08.01    NACL-門脇    13/01/25  包括診療分の点検用レセプト記載対応
      *  04.08.02    NACL-門脇    16/03/31  平成２８年４月改正対応
      *  04.08.03    NACL-門脇    16/08/02  １５円以下薬剤の提出用レセプト記載
      *                                     （記録）対応
      *  04.08.04    NACL-門脇    16/08/23  持参薬対応
      *  04.08.05    NACL-門脇    18/04/17  平成３０年４月改正対応
      *  04.08.06    NACL-門脇    18/09/27  選択式コメント対応
      *
      *  05.00.01    NACL-門脇    19/05/16  退院時共同指導料２コメント自動記載
      *                                     対応
      *  05.00.02    NACL-門脇    20/04/07  令和２年４月改正対応
      ************************************************************
      *01  RECE220-REC.
           03  RECE220-KEY.
               05  RECE220-KEY1.
      *医療機関番号
                   07  RECE220-HOSPNUM     PIC 9(2).
      *診療年月
                   07  RECE220-SRYYM       PIC 9(06).
      *入外区分
                   07  RECE220-NYUGAIKBN   PIC X(01).
      *患者ＩＤ
                   07  RECE220-PTID        PIC 9(10).
      *レセプト診療科
                   07  RECE220-RECEKA      PIC X(02).
      *公費ＩＤ
                   07  RECE220-HKNID       PIC 9(10).
      *適用開始年月日
                   07  RECE220-TEKSTYMD    PIC X(08). 
      *剤番号
               05  RECE220-ZAINUM          PIC 9(08).
      *診療区分
           03  RECE220-SRYKBN              PIC X(02).
      *剤識別区分
           03  RECE220-ZAISKBKBN           PIC X(01).
      *患者番号
           03  RECE220-PTNUM               PIC X(20).
      *診療科
           03  RECE220-SRYKA               PIC X(02).
      *保険組合せ番号
           03  RECE220-HKNCOMBI            PIC 9(04).
      *自保区分
           03  RECE220-JIHOKBN             PIC X(01).
      *剤点数
           03  RECE220-ZAITEN              PIC 9(08).
      *診療コード計
           03  RECE220-SRYCDTOTAL          PIC 9(14).
      *数量計
           03  RECE220-SURYOUTOTAL         PIC 9(07)V9(05).
      *診療コード＊数量の累計
           03  RECE220-RUIKEITOTAL         PIC 9(13)V9(05).
      *明細数
           03  RECE220-MEISAISU            PIC 9(04).
      *剤回数
           03  RECE220-ZAIKAISU            PIC 9(04).
      *手技点数１
           03  RECE220-SYUTEN1             PIC 9(07).
      *手技点数２
           03  RECE220-SYUTEN2             PIC 9(07).
      *薬剤点数
           03  RECE220-YKZTEN              PIC 9(07).
      *器材点数
           03  RECE220-KIZAITEN            PIC 9(07).
      *ドクターコード
           03  RECE220-DRCD                PIC X(05).
      *剤請求フラグ
           03  RECE220-ZAIREQFLG           PIC X(01).
      *逓減率
           03  RECE220-TEIGENRATE          PIC X(01).
      *コメントコードフラグ
           03  RECE220-COMCDFLG            PIC X(01).
      *（サ）フラグ
           03  RECE220-SAFLG               PIC X(01).
      *投薬剤点０（１又は２（コメントのみ））
           03  RECE220-TOYAKUFLG           PIC X(01).
      *療養病棟入院料剤点記載明細数
           03  RECE220-RYOTENKISAISU       PIC 9(04).
      *選択式コメント存在フラグ
           03  RECE220-SENTAKUCOMFLG       PIC X(01).
      *剤区分
           03  RECE220-ZAIKBN              PIC 9(03).
      *画像診断（２回目以降減算あり）
           03  RECE220-GAZOGEN             PIC X(01).
      *再診加算（入院）の剤
           03  RECE220-NYUSAIFLG           PIC X(01).
      *１５円以下薬剤レセプト記載フラグ
           03  RECE220-YKZKISAIFLG         PIC X(01).
      *持参薬区分
           03  RECE220-JISANYAKKBN         PIC X(01).
      *
      *リハビリ
           03  RECE220-RIHABIRI-COMMENT    PIC X(01).
      *リハビリ（心大血管疾患）
           03  RECE220-SINDAI-COMMENT      PIC X(01).
      *リハビリ（呼吸器）
           03  RECE220-KOKYUKI-COMMENT     PIC X(01).
      *リハビリ（脳血管疾患等）
           03  RECE220-NOUKE-COMMENT       PIC X(01).
      *リハビリ（運動器）
           03  RECE220-UNDOKI-COMMENT      PIC X(01).
      *リハビリ（難病患者）
           03  RECE220-NANBYO-COMMENT      PIC X(01).
      *リハビリ（障害児（者））
           03  RECE220-SYOGAI-COMMENT      PIC X(01).
      *リハビリ（摂食）
           03  RECE220-SESSYOKU-COMMENT    PIC X(01).
      *リハビリ（廃用症候群）
           03  RECE220-HAIYO-COMMENT       PIC X(01).
      *介達牽引
           03  RECE220-KAITATU-COMMENT     PIC X(01).
      *
      *リハビリ加算（心大血管疾患）
           03  RECE220-SINDAI9-COMMENT     PIC X(01).
      *リハビリ加算（呼吸器）
           03  RECE220-KOKYUKI9-COMMENT    PIC X(01).
      *リハビリ加算（廃用症候群）
           03  RECE220-HAIYO9-COMMENT      PIC X(01).
      *
      *薬剤管理指導料
           03  RECE220-FLG-YAKUKAN-COMMENT PIC X(01).
      *入院栄養食事指導料
           03  RECE220-FLG-NYUEIYO-COMMENT PIC X(01).
      *退院前訪問指導料
           03  RECE220-FLG-HOUMON-COMMENT  PIC X(01).
      *傷病手当金意見書交付料
           03  RECE220-FLG-SYOBYO-COMMENT  PIC X(01).
      *療養費同意書交付料
           03  RECE220-FLG-RYOYO-COMMENT   PIC X(01).
      *退院時服薬指導加算を算定時の薬剤管理指導料
           03  RECE220-FLG-TAIIN-COMMENT   PIC X(01).
      *訪問看護指示料
           03  RECE220-FLG-HOUSIJI-COMMENT PIC X(01).
      *手術（輸血は除く）
           03  RECE220-FLG-SHUJUTU-COMMENT PIC X(01).
      *ＣＴ・ＭＲＩ
           03  RECE220-FLG-CTMRI-COMMENT   PIC X(01).
      *精神科退院前訪問指導料
           03  RECE220-FLG-HOUSEI-COMMENT  PIC X(01).
      *精神科隔離病室管理加算（入院料加算）
           03  RECE220-FLG-KAKURI-COMMENT  PIC X(01).
      *在宅血液透析頻回指導管理料
           03  RECE220-FLG-HINKAI-COMMENT  PIC X(01).
      *酸素
           03  RECE220-FLG-SANSO-COMMENT   PIC X(01).
      *初診（同日複数診療科）
           03  RECE220-FLG-SYODOU-COMMENT  PIC X(01).
           03  RECE220-SYODOU-SRYKA-AREA.
               05  RECE220-SYODOU-SRYKA    PIC X(02)   OCCURS  10.
      *検査等実施判断
           03  RECE220-KNSJISKBN           PIC 9(01).
      *検査等実施判断グループ
           03  RECE220-KNSJISGRPKBN        PIC 9(02).
      *往診・特別往診
           03  RECE220-FLG-OSIN-COMMENT    PIC X(01).
      *在支援に関する診療
           03  RECE220-FLG-ZAISIEN-COMMENT PIC X(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
           03  RECE220-FLG-KENSA-COMMENT   PIC X(01).
      *
      *介護支援連携指導料
           03  RECE220-FLG-KAISIEN-COMMENT PIC X(01).
      *集団栄養食事指導料
           03  RECE220-FLG-SYUEIYO-COMMENT PIC X(01).
      *人工腎臓等
           03  RECE220-FLG-JINZOU-COMMENT  PIC X(01).
      *局所陰圧閉鎖処置
           03  RECE220-FLG-KYOKUIN-COMMENT PIC X(01).
      *麻酔
           03  RECE220-FLG-MASUI-COMMENT   PIC X(01).
      *栄養サポートチーム加算
           03  RECE220-FLG-EISAPO-COMMENT  PIC X(01).
      *呼吸ケアチーム加算
           03  RECE220-FLG-KOKKEA-COMMENT  PIC X(01).
      *重症者加算（精神療養病棟入院料）
           03  RECE220-FLG-JYUSYO-COMMENT  PIC X(01).
      *
      *介護職員等喀痰吸引等指示料
           03  RECE220-FLG-KAISYOKU-COMMENT
                                           PIC  X(01).
      *精神科ショート・ケア・精神科デイ・ケア
           03  RECE220-FLG-SEIKEA-COMMENT  PIC  X(01).
      *精神科リエゾンチーム加算
           03  RECE220-FLG-SEIRIE-COMMENT  PIC  X(01).
      *病棟薬剤業務実施加算
           03  RECE220-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *在宅持続陽圧呼吸療法指導管理料
           03  RECE220-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（着用型自動除細動器）
      *心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
           03  RECE220-FLG-SINPESUR-COMMENT  PIC  X(01).
      *遠隔モニタリング加算
           03  RECE220-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *がん患者指導管理料２・３
           03  RECE220-FLG-GANSIDO-COMMENT   PIC  X(01).
      *在宅患者共同診療料
           03  RECE220-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *磁気による膀胱等刺激法
           03  RECE220-FLG-BOKOHO-COMMENT    PIC  X(01).
      *超重症児（者）入院診療加算・準超重症児（者）入院診療加算
           03  RECE220-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *排尿自立指導料
           03  RECE220-FLG-HAIJRT-COMMENT    PIC  X(01).
      *在宅半固形栄養経管栄養法指導管理料
           03  RECE220-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *高気圧酸素治療
           03  RECE220-FLG-KIATU-COMMENT     PIC  X(01).
      *疾患別等専門プログラム加算
           03  RECE220-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *時間外緊急院内検査加算・外来迅速検体検査加算
           03  RECE220-FLG-KENKSN-COMMENT    PIC  X(01).
      *時間外緊急院内画像診断加算
           03  RECE220-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *退院時共同指導料２
           03  RECE220-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *人工内耳機器調整加算
           03  RECE220-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *排尿自立支援加算
           03  RECE220-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *日数回数テーブル
           03  RECE220-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
             04  RECE220-DAY-TBL           OCCURS  4.
                 05  RECE220-KAISUINF      OCCURS  31.
                     07  RECE220-KAISU     PIC 9(03).
      *
      *包括診療分退避点数
           03  RECE220-HOKATU-TENSU-AREA.
      *剤点数
               05  RECE220-HOKATU-ZAITEN   PIC  9(08).
      *手技点数１
               05  RECE220-HOKATU-SYUTEN1  PIC  9(07).
      *手技点数２
               05  RECE220-HOKATU-SYUTEN2  PIC  9(07).
      *薬剤点数
               05  RECE220-HOKATU-YKZTEN   PIC  9(07).
      *器材点数
               05  RECE220-HOKATU-KIZAITEN PIC  9(07).
