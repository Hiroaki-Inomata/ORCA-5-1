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
      *    レセプト明細書     COPY (CPRCF064.INC)                *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    03.09.24  コメント編集フラグ追加
      *  01.00.02    NACL-門脇    04.02.05  診療コード＊数量の累計
      *                                     追加
      *  ２００４年４月改正対応
      *  01.01.01    NACL-門脇    04.04.05  在宅血液透析頻回指導管理料
      *                                     の追加
      *  01.01.02    NACL-門脇    05.01.17  酸素の追加
      *  01.01.03    NACL-門脇    05.02.28  （サ）フラグの追加
      *  02.09.01    NACL-門脇    06.04.17  ２００６年４月改正対応
      *  03.00.01    NACL-門脇    06.06.22  往診・特別往診の追加
      *  03.01.01    NACL-門脇    06.07.11  在支援に関する診療の追加
      *  03.03.01    NACL-藤原    06/09/29  検査の判断料の記載方法の変更
      *                                     検査等実施判断の追加
      *  03.05.01    NACL-小豆沢  07/05/30  グループ診療対応
      *  03.05.02    NACL-門脇    08/04/10  レセ電用入院料加算フラグ追加
      *  04.03.01    NACL-門脇    08/07/14  投薬剤点０フラグの追加
      *
      *  04.04.01    NACL-門脇    09/02/04  検査（ＢＮＰ等）の追加
      *  04.04.02    NACL-門脇    10/03/29  画像診断（２回目以降減算あり）
      *                                     の追加
      *  04.04.03    NACL-門脇    10/04/06  改正対応項目追加
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
      *
      *  04.07.01    NACL-門脇    12/09/25  コメント差込項目追加
      *  04.07.02    NACL-藤原    12/12/17  複数枚レセプト分割対応
      *  04.07.03    NACL-門脇    14/04/07  改正対応項目追加
      *  04.07.04    NACL-門脇    15/08/10  日別保険組合せ番号追加
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
      *01  RECE64-REC.
           03  RECE64-KEY.
               05  RECE64-KEY1.
      *医療機関ＩＤ
                   07  RECE64-HOSPNUM      PIC 9(2).
      *診療年月
                   07  RECE64-SRYYM        PIC 9(06).
      *入外区分
                   07  RECE64-NYUGAIKBN    PIC X(01).
      *患者ＩＤ
                   07  RECE64-PTID         PIC 9(10).
      *レセプト科
                   07  RECE64-RECEKA       PIC X(02).
      *審査支払機関
                   07  RECE64-TEISYUTUSAKI
                                           PIC 9(01).
      *レセプト種別
                   07  RECE64-RECESYUBETU  PIC 9(04).
      *保険者番号        
                   07  RECE64-HKNJANUM     PIC X(08).
      *補助区分
                   07  RECE64-HOJOKBN      PIC X(01).
      *公費ＩＤ
                   07  RECE64-KOHID        PIC 9(10).
      *適用開始年月日
                   07  RECE64-TEKSTYMD     PIC X(08). 
      *分割区分
                   07  RECE64-RECEIPT-KBN  PIC X(10).
      *剤番号
               05  RECE64-ZAINUM          PIC  9(08).
      *剤連番
               05  RECE64-ZAIREN          PIC  9(02).
      *公費分離区分
           03  RECE64-KOHBUNRI            PIC  9(01).
      *診療区分
           03  RECE64-SRYKBN              PIC  X(02).
      *診療種別区分
           03  RECE64-SRYSYUKBN           PIC  X(03).
      *剤識別区分
           03  RECE64-ZAISKBKBN           PIC  X(01).
      *患者番号
           03  RECE64-PTNUM               PIC  X(20).
      *診療科
           03  RECE64-SRYKA               PIC  X(02).
      *保険組合せ番号
           03  RECE64-HKNCOMBI            PIC  9(04).
      *自保区分
           03  RECE64-JIHOKBN             PIC  X(01).
      *剤点数
           03  RECE64-ZAITEN              PIC  9(08).
      *診療コード計
           03  RECE64-SRYCDTOTAL          PIC  9(14).
      *数量計
           03  RECE64-SURYOUTOTAL         PIC  9(07)V9(05).
      *診療コード＊数量の累計
           03  RECE64-RUIKEITOTAL         PIC  9(13)V9(05).
      *明細数
           03  RECE64-MEISAISU            PIC  9(04).
      *剤回数
           03  RECE64-ZAIKAISU            PIC  9(04).
      *手技点数１
           03  RECE64-SYUTEN1             PIC  9(07).
      *手技点数２
           03  RECE64-SYUTEN2             PIC  9(07).
      *薬剤点数
           03  RECE64-YKZTEN              PIC  9(07).
      *器材点数
           03  RECE64-KIZAITEN            PIC  9(07).
      *ドクターコード
           03  RECE64-DRCD                PIC  X(05).
      *剤請求フラグ
           03  RECE64-ZAIREQFLG           PIC  X(01).
      *逓減率
           03  RECE64-TEIGENRATE          PIC  X(01).
      *コメントコードフラグ
           03  RECE64-COMCDFLG            PIC  X(01).
      *（サ）フラグ
           03  RECE64-SAFLG               PIC  X(01).
      *投薬剤点０（１又は２（コメントのみ））
           03  RECE64-TOYAKUFLG           PIC  X(01).
      *療養病棟入院料剤点記載明細数
           03  RECE64-RYOTENKISAISU       PIC  9(04).
      *関係コメント存在フラグ
           03  RECE64-SETCOMFLG           PIC  X(01).
      *選択式コメント存在フラグ
           03  RECE64-SENTAKUCOMFLG       PIC  X(01).
      *剤区分
           03  RECE64-ZAIKBN              PIC  9(03).
      *画像診断（２回目以降減算あり）
           03  RECE64-GAZOGEN             PIC  X(01).
      *再診加算（入院）の剤
           03  RECE64-NYUSAIFLG           PIC  X(01).
      *１５円以下薬剤レセプト記載フラグ
           03  RECE64-YKZKISAIFLG         PIC  X(01).
      *持参薬区分
           03  RECE64-JISANYAKKBN         PIC  X(01).
      *
      *リハビリ
           03  RECE64-RIHABIRI-COMMENT    PIC  X(01).
      *リハビリ（心大血管疾患）
           03  RECE64-SINDAI-COMMENT      PIC  X(01).
      *リハビリ（呼吸器）
           03  RECE64-KOKYUKI-COMMENT     PIC  X(01).
      *リハビリ（脳血管疾患等）
           03  RECE64-NOUKE-COMMENT       PIC  X(01).
      *リハビリ（運動器）
           03  RECE64-UNDOKI-COMMENT      PIC  X(01).
      *リハビリ（難病患者）
           03  RECE64-NANBYO-COMMENT      PIC  X(01).
      *リハビリ（障害児（者））
           03  RECE64-SYOGAI-COMMENT      PIC  X(01).
      *リハビリ（摂食）
           03  RECE64-SESSYOKU-COMMENT    PIC  X(01).
      *リハビリ（廃用症候群）
           03  RECE64-HAIYO-COMMENT       PIC  X(01).
      *介達牽引
           03  RECE64-KAITATU-COMMENT     PIC  X(01).
      *
      *リハビリ加算（心大血管疾患）
           03  RECE64-SINDAI9-COMMENT     PIC  X(01).
      *リハビリ加算（呼吸器）
           03  RECE64-KOKYUKI9-COMMENT    PIC  X(01).
      *リハビリ加算（廃用症候群）
           03  RECE64-HAIYO9-COMMENT      PIC  X(01).
      *
      *薬剤管理指導料
           03  RECE64-FLG-YAKUKAN-COMMENT PIC  X(01).
      *入院栄養食事指導料
           03  RECE64-FLG-NYUEIYO-COMMENT PIC  X(01).
      *退院前訪問指導料
           03  RECE64-FLG-HOUMON-COMMENT  PIC  X(01).
      *傷病手当金意見書交付料
           03  RECE64-FLG-SYOBYO-COMMENT  PIC  X(01).
      *療養費同意書交付料
           03  RECE64-FLG-RYOYO-COMMENT   PIC  X(01).
      *退院時服薬指導加算を算定時の薬剤管理指導料
           03  RECE64-FLG-TAIIN-COMMENT   PIC  X(01).
      *訪問看護指示料
           03  RECE64-FLG-HOUSIJI-COMMENT PIC  X(01).
      *手術（輸血は除く）
           03  RECE64-FLG-SHUJUTU-COMMENT PIC  X(01).
      *ＣＴ・ＭＲＩ
           03  RECE64-FLG-CTMRI-COMMENT   PIC  X(01).
      *精神科退院前訪問指導料
           03  RECE64-FLG-HOUSEI-COMMENT  PIC  X(01).
      *精神科隔離病室管理加算（入院料加算）
           03  RECE64-FLG-KAKURI-COMMENT  PIC  X(01).
      *在宅血液透析頻回指導管理料
           03  RECE64-FLG-HINKAI-COMMENT  PIC  X(01).
      *酸素
           03  RECE64-FLG-SANSO-COMMENT   PIC  X(01).
      *初診（同日複数診療科）
           03  RECE64-FLG-SYODOU-COMMENT  PIC  X(01).
           03  RECE64-SYODOU-SRYKA-AREA.
               05  RECE64-SYODOU-SRYKA    PIC  X(02)     OCCURS  10.
      *検査等実施判断
           03  RECE64-KNSJISKBN           PIC  9(01).
      *検査等実施判断グループ
           03  RECE64-KNSJISGRPKBN        PIC  9(02).
      *往診・特別往診
           03  RECE64-FLG-OSIN-COMMENT    PIC  X(01).
      *在支援に関する診療
           03  RECE64-FLG-ZAISIEN-COMMENT PIC  X(01).
      *レセ電用入院料加算フラグ
           03  RECE64-NYUKSNFLG           PIC  9(01).
      *検査（ＢＮＰ、ＮＴ−ｐｒｏＢＮＰ、ＨＡＮＰ）
           03  RECE64-FLG-KENSA-COMMENT   PIC  X(01).
      *
      *介護支援連携指導料
           03  RECE64-FLG-KAISIEN-COMMENT PIC  X(01).
      *集団栄養食事指導料
           03  RECE64-FLG-SYUEIYO-COMMENT PIC  X(01).
      *人工腎臓等
           03  RECE64-FLG-JINZOU-COMMENT  PIC  X(01).
      *局所陰圧閉鎖処置
           03  RECE64-FLG-KYOKUIN-COMMENT PIC  X(01).
      *麻酔
           03  RECE64-FLG-MASUI-COMMENT   PIC  X(01).
      *栄養サポートチーム加算
           03  RECE64-FLG-EISAPO-COMMENT  PIC  X(01).
      *呼吸ケアチーム加算
           03  RECE64-FLG-KOKKEA-COMMENT  PIC  X(01).
      *重症者加算（精神療養病棟入院料）
           03  RECE64-FLG-JYUSYO-COMMENT  PIC  X(01).
      *
      *画像診断フイルム
           03  RECE64-FLG-FILM-COMMENT    PIC  X(01).
      *
      *介護職員等喀痰吸引等指示料
           03  RECE64-FLG-KAISYOKU-COMMENT
                                          PIC  X(01).
      *精神科ショート・ケア・精神科デイ・ケア
           03  RECE64-FLG-SEIKEA-COMMENT  PIC  X(01).
      *精神科リエゾンチーム加算
           03  RECE64-FLG-SEIRIE-COMMENT  PIC  X(01).
      *病棟薬剤業務実施加算
           03  RECE64-FLG-BYOYAK-COMMENT  PIC  X(01).
      *
      *在宅持続陽圧呼吸療法指導管理料
           03  RECE64-FLG-YOUATU-COMMENT  PIC  X(01).
      *
      *心臓ペースメーカー指導管理料（着用型自動除細動器）
      *心臓ペースメーカー指導管理料（ロ・植込型除細動器移行期加算含む）
           03  RECE64-FLG-SINPESUR-COMMENT  PIC  X(01).
      *遠隔モニタリング加算
           03  RECE64-FLG-SINPESU-COMMENT   PIC  X(01).
      *
      *がん患者指導管理料２・３
           03  RECE64-FLG-GANSIDO-COMMENT   PIC  X(01).
      *在宅患者共同診療料
           03  RECE64-FLG-ZAIKYO-COMMENT    PIC  X(01).
      *磁気による膀胱等刺激法
           03  RECE64-FLG-BOKOHO-COMMENT    PIC  X(01).
      *超重症児（者）入院診療加算・準超重症児（者）入院診療加算
           03  RECE64-FLG-CYOJKSN-COMMENT   PIC  X(01).
      *排尿自立指導料
           03  RECE64-FLG-HAIJRT-COMMENT    PIC  X(01).
      *在宅半固形栄養経管栄養法指導管理料
           03  RECE64-FLG-ZAIHANKO-COMMENT  PIC  X(01).
      *高気圧酸素治療
           03  RECE64-FLG-KIATU-COMMENT     PIC  X(01).
      *疾患別等専門プログラム加算
           03  RECE64-FLG-SCPRO-COMMENT     PIC  X(01).
      *
      *時間外緊急院内検査加算・外来迅速検体検査加算
           03  RECE64-FLG-KENKSN-COMMENT    PIC  X(01).
      *時間外緊急院内画像診断加算
           03  RECE64-FLG-KINGAZ-COMMENT    PIC  X(01).
      *
      *退院時共同指導料２
           03  RECE64-FLG-TAIKYO-COMMENT    PIC  X(01).
      *
      *人工内耳機器調整加算
           03  RECE64-FLG-JINNAI-COMMENT    PIC  X(01).
      *
      *排尿自立支援加算
           03  RECE64-FLG-HAIJSK-COMMENT    PIC  X(01).
      *
      *日数回数テーブル
           03  RECE64-DAY-AREA.
      *        診療回数テーブル（1:合計,2:１回,3:２回, 4:３回以上）
             04  RECE64-DAY-TBL           OCCURS  4.
                 05  RECE64-KAISUINF      OCCURS  31.
                     07  RECE64-KAISU     PIC  9(03).
      *
      *手技毎の内容（手術・画像診断・処置）
           03  RECE64-ACCTP-SYUGI-TBL-G.
               05  RECE64-ACCTP-SYUGI-TBL  OCCURS  5.
      *手技開始コード
                   07  RECE64-ACCTP-SRYCD  PIC X(09).
      *手技点数
                   07  RECE64-ACCTP-SYUTEN PIC 9(07).
      *薬剤点数
                   07  RECE64-ACCTP-YKZTEN PIC 9(07).
      *その他器材点数（フィルム・酸素・窒素以外）
                   07  RECE64-ACCTP-KIZAITEN
                                           PIC 9(07).
      *フィルム点数
                   07  RECE64-ACCTP-FIRTEN PIC 9(07).
      *酸素点数
           03  RECE64-ACCTP-SANSOTEN       PIC 9(07).
      *窒素点数
           03  RECE64-ACCTP-CHISOTEN       PIC 9(07).
      *
      *包括診療分退避点数
           03  RECE64-HOKATU-TENSU-AREA.
      *剤点数
               05  RECE64-HOKATU-ZAITEN    PIC 9(08).
      *手技点数１
               05  RECE64-HOKATU-SYUTEN1   PIC 9(07).
      *手技点数２
               05  RECE64-HOKATU-SYUTEN2   PIC 9(07).
      *薬剤点数
               05  RECE64-HOKATU-YKZTEN    PIC 9(07).
      *器材点数
               05  RECE64-HOKATU-KIZAITEN  PIC 9(07).
      *
      *日別保険組合せ番号
           03  RECE64-HKNCOMBI-DAY-AREA.
               05  RECE64-HKNCOMBI-DAY     PIC 9(04)     OCCURS  31.
