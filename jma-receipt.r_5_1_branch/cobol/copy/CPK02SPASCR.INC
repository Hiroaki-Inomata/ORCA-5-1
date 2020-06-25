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
      *****************************************************************
      *    診療行為ＳＰＡ　データ　　　 　　 COPY (CPK02SPASCR.INC)
      *             画面のスパ領域をデータで使用する
      *                                  （各診療行為チェックに使用）
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   02/03/05  外用薬用数量追加
      * 01.00.02     MCC-多々納   02/03/15  後発医薬品等追加
      * 01.00.03     MCC-多々納   02/04/09  労災用追加(RL=487×200+4)
      * 01.00.04     MCC-多々納   02/05/27  禁忌用追加(RL=488×200+4)
      * 01.00.05     MCC-多々納   02/05/27  禁忌用追加(RL=510×200+4)
      * 01.00.06     NACL-多々納  02/10/23  入院対応追加(RL=674×200+4)
      * 01.00.07     NACL-多々納  03/04/30  自費金額追加(RL=682×200+4)
      * 01.00.08     NACL-多々納  03/05/12  名称を８０桁
      * 01.00.09     NACL-多々納  05/04/01  告示区分追加
      * 01.00.10     NACL-多々納  05/06/01  検体検査コメント等追加
      * 01.00.10     NACL-多々納  05/06/10  前回区分等追加
      * 01.00.11     NACL-多々納  06/03/17  処置乳幼児加算追加
      * 03.04.00     NACL-多々納  06/12/11  薬剤附加情報追加
      * 04.00.00     NACL-多々納  07/09/XX  テーブル４００へ
      * 04.02.00     NACL-多々納  08/03/XX  画像等手術支援
      *                                     麻酔識別追加
      * 04.05.00     NACL-多々納  09/10/02  換算値入力他対応
      * 04.06.00     NACL-多々納  11/02/14  用法コメント対応
      * 04.07.00     NACL-多々納  11/09/21  院外処方点数対応
      * 04.07.00     NACL-多々納  11/12/20  商品名コード単価対応
      * 04.07.00     NACL-多々納  12/11/07  警告区分等追加
      * 04.08.00     NACL-多々納  16/02/XX  Ｈ２８年改正対応
      * 05.00.00     NACL-多々納  20/03/XX  Ｒ０２年改正対応
      ************************************************************
      *
      *01  SPA-K02SCR-REC.
      *
           03  SPA-GMN-MAX             PIC 9(04).
      *
           03  SPA-GMN-REC.
               05  SPA-GMN-TBL             OCCURS   400.
                   07  SPA-GMN-TBLREC.
                       09  SPA-GMN-SRYKBN        PIC X(02).
                       09  SPA-GMN-INPUTCD       PIC X(54).
                       09  SPA-GMN-MEISYO-G.
                           11  SPA-GMN-MEIH      PIC X(02).
      *                    11  SPA-GMN-MEISYO    PIC X(78).
      *R02.3
                           11  SPA-GMN-MEISYO    PIC X(138).
      **               09  SPA-GMN-SURYO         PIC X(09).
                       09  SPA-GMN-SURYO         PIC X(11).
                       09  SPA-GMN-TANINAME      PIC X(12).
                       09  SPA-GMN-TENSU         PIC X(08).
                       09  SPA-GMN-KAISU         PIC X(04).
                       09  SPA-GMN-KEI           PIC S9(08).
      *---(01.00.01)
      *                入力コードの桁数
                       09  SPA-GMN-INCNT         PIC 9(02).
      *
                   07  SPA-COM-TBLREC.
      *---(01.00.01)
                       09  SPA-COM-SET           PIC X(01).
                       09  SPA-COM-PAGE          PIC 9(04).
                       09  SPA-COM-GYOU          PIC 9(04).
                       09  SPA-COM-CUR           PIC X(01).
      *                今回入力位置
                       09  SPA-COM-IN            PIC X(01).
      *                今回入力有り
                       09  SPA-COM-IN2           PIC X(01).
      *                数量入力有り
                       09  SPA-COM-IN3           PIC X(01).
      *ver.4.7         入力有り
                       09  SPA-COM-IN4           PIC X(01).
                       09  SPA-MAE-INPUTCD       PIC X(54).
      *---(01.00.01)
      *
                       09  SPA-COM-SRYSYUKBN     PIC X(03).
                       09  SPA-COM-SRYKBN        PIC X(02).
                       09  SPA-COM-INPUTCD       PIC X(09).
                       09  SPA-COM-SURYO         PIC 9(05)V9(05).
      *                剤合計点数
                       09  SPA-COM-TENSU         PIC 9(08).
      *                マイナス区分
                       09  SPA-COM-PLUSKBN       PIC 9(01).
      *                自費明細金額
                       09  SPA-COM-JIHIMONEY     PIC 9(07).
      *                点数
      *R02.4           09  SPA-COM-TEN           PIC 9(08)V9(4).
                       09  SPA-COM-TEN           PIC 9(10)V9(4).
      *                点数（きざみ値計算後）
      *R02.4           09  SPA-COM-KISOTEN       PIC 9(08)V9(4).
                       09  SPA-COM-KISOTEN       PIC 9(10)V9(4).
                       09  SPA-COM-KAISU         PIC 9(03).
                       09  SPA-COM-KEI           PIC 9(08).
      *                外用薬用入力用
                       09  SPA-COM-SURYO2        PIC 9(05)V9(05).
                       09  SPA-COM-KAISU2        PIC 9(03).
      *
                       09  SPA-COM-BUNGSU        PIC 9(08).
                       09  SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI1         PIC X(10).
                           11  SPA-COM-ATAI2         PIC X(10).
                           11  SPA-COM-ATAI3         PIC X(10).
                           11  SPA-COM-ATAI4         PIC X(10).
                           11  SPA-COM-ATAI5         PIC X(10).
                       09  SPA-COM-INPUTCHI-R    REDEFINES
                                                 SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI          PIC X(10)
                                                     OCCURS  5.
      *R02.4
      *                8311 の入力ＣＤ
                       09  SPA-GMN-ATAI83        PIC X(20).
      *
      *                手技料計１
                       09  SPA-COM-SYUTEN1       PIC 9(08).
      *                手技料計２
                       09  SPA-COM-SYUTEN2       PIC 9(08).
      *                薬剤点数
                       09  SPA-COM-YKZTEN        PIC 9(08).
      *                器材点数
                       09  SPA-COM-KIZAITEN      PIC 9(08).
      *
                       09  SPA-COM-ZAINUM        PIC 9(08).
      *                剤終了
                       09  SPA-COM-ZAIEND        PIC X(01).
      *                自動算定区分（１：画面表示、２：入力条件により変更）
                       09  SPA-COM-AUTOKBN       PIC X(01).
                       09  SPA-COM-AUTOKBN2      PIC X(01).
      *                数量入力
                       09  SPA-COM-SURFLG        PIC X(01).
      *                回数入力
                       09  SPA-COM-KAIFLG        PIC X(01).
      *                名称入力
                       09  SPA-COM-MEIFLG        PIC X(01).
      *                計入力
                       09  SPA-COM-KEIFLG        PIC X(01).
      *                自費入力
                       09  SPA-COM-JIHIFLG       PIC X(01).
      *
      *                履歴算定用診療行為コード
                       09  SPA-COM-CHK-SRYCD     PIC X(09).
      *                画面表示用入力コード
                       09  SPA-COM-ICD-INPUTCD   PIC X(22).
      *                入力コード表示済み
                       09  SPA-COM-GMNFLG        PIC X(01).
      *
                       09  SPA-COM-FLGAREA.
      *                    禁忌薬剤警告済フラグ
                           11  SPA-COM-KNKFLG        PIC X(01).
      *                    禁忌薬剤チェック済フラグ
                           11  SPA-COM-KNKUMU        PIC X(01).
      *                    労災読み替え加算分
                           11  SPA-COM-RSIFLG2       PIC X(01).
      *                    数量チェック済（投与量チェック）
                           11  SPA-COM-SURCHK        PIC X(01).
      *                    併用算定済フラグ
                           11  SPA-COM-HCHKFLG       PIC X(01).
      *                    併用算定警告済フラグ
                           11  SPA-COM-HKEIFLG       PIC X(01).
      *                    基本チェック済フラグ
                           11  SPA-COM-CHKFLG        PIC X(01).
      *                    時間加算フラグ
                           11  SPA-COM-TIMEFLG       PIC 9(01).
      *ver.4.8             再診自動算定フラグ
                           11  SPA-COM-KSNAUTO       PIC X(01).
      *                警告済みフラグ
                       09  SPA-COM-KZMFLGAREA.
      *                    患者禁忌薬剤チェック済フラグ
                           11  SPA-COM-KINKIFLG      PIC X(01).
      *                    警告済フラグ
                           11  SPA-COM-KZMFLG        PIC X(01).
      *                    警告済フラグ２
                           11  SPA-COM-KZMFLG2       PIC X(01).
      *                    警告済フラグ３（同日併用チェック）
                           11  SPA-COM-KZMFLG3       PIC X(01).
      *                    警告済フラグ４（長期日数チェック）
                           11  SPA-COM-KZMFLG4       PIC X(01).
      *                    警告済フラグ５（投与量チェック）
                           11  SPA-COM-KZMFLG5       PIC X(01).
      *ver.4.7             処方せん料警告２
                           11  SPA-COM-KZMFLG6       PIC X(01).
      *                    警告（湿布薬警告）
                           11  SPA-COM-KZMFLG7       PIC X(01).
      *                    警告（リハビリ減点）
                           11  SPA-COM-KZMFLG8       PIC X(01).
      *                    警告（包括算定入力警告(H29.9）
                           11  SPA-COM-KZMFLG9       PIC X(01).
      *                    警告（リハ介護）(H31.4）
                           11  SPA-COM-KZMFLG10      PIC X(01).
      *                    入院料併用警告
                           11  SPA-COM-KZMFLGNK      PIC X(01).
      *                    翌月入院料なし警告
                           11  SPA-COM-KZMFLGN2      PIC X(01).
      *
      *                自動発生済フラグ
                       09  SPA-COM-AUTOFLG           PIC X(01).
      *                残量廃棄自動発生済フラグ
                       09  SPA-COM-AUTOFLG2          PIC X(01).
      *                計算チェック済フラグ（毎回クリア）
                       09  SPA-COM-SANFLG        PIC 9(01).
      *                逓減検査対象有無（１：逓減有り）
                       09  SPA-COM-TEIGEN-FLG    PIC  9(01).
      *                逓減検査対象チェック済
                       09  SPA-COM-TEIGEN-CHK    PIC  9(01).
      *                労災区分（労災用のコード）
                       09  SPA-COM-RSIKBN        PIC  9(01).
      *                労災 （外来管理加算の読み替えフラグ）
                       09  SPA-COM-RSIGIRI       PIC  9(01).
      *                入院回数・日数
                       09  SPA-COM-NYUIN-AREA.
                           11  SPA-COM-NYU-KAI        PIC 9(03).
                           11  SPA-COM-NYUINDAY       PIC 9(01)
                                                      OCCURS  31.
      *                入院警告済フラグ（注射）
                       09  SPA-COM-NYU-KZMFLG-G.
                           11  SPA-COM-NYU-KZMFLG        PIC X(01)
                                                     OCCURS   31.
      *                保険組合せ（空白は選択保険）
                       09  SPA-COM-HKNCOMBI      PIC  X(04).
      *                治験保険（１：治験）
                       09  SPA-COM-CHIKENKBN     PIC  X(01).
      *                診療科（空白は選択）
                       09  SPA-COM-SRYKA         PIC  X(02).
      *                ドクター（空白は選択）
                       09  SPA-COM-DRCD          PIC  X(04).
      *                剤合計点数（訂正前）
                       09  SPA-COM-MAE-TENSU     PIC 9(08).
      *                訂正時区分
                       09  SPA-COM-MAE-AUTOKBN   PIC X(01).
      *                同一検査判定コード
                       09  SPA-COM-KENGRP        PIC  X(09).
      *                後発医薬品有無（１：後発あり）
                       09  SPA-COM-KOUHATU-FLG   PIC  9(01).
      *                日毎のチェックデータあり
                       09  SPA-COM-DAYCHK-FLG    PIC  9(01).
      *!????
      *                包括コード（システム管理毎）
                       09  SPA-COM-HOUKATU-KFLG  PIC  9(01).
                       09  SPA-COM-HOUKATU-KFLG2 PIC  9(01).
                       09  SPA-COM-HOUKATU-KERR  PIC  9(01).
      *                包括コード（１：包括分）
                       09  SPA-COM-HOUKATU-FLG   PIC  9(01).
                       09  SPA-COM-HOUKATU-FLG2  PIC  9(01).
      *                包括剤
                       09  SPA-COM-HOUKATU-ZAI   PIC  9(01).
      *                包括エラ−データ
                       09  SPA-COM-HOUKATU-ERR   PIC  9(01).
      *ver.4.3
      *                検体検査１０項目（入院初回加算用）
                       09  SPA-COM-HOUKNS-10     PIC  9(01).
      *ver.4.5.0
      *                換算値入力用
                       09  SPA-COM-KANSURYO       PIC 9(05)V9(05).
      *                内服１種類区分
                       09  SPA-COM-TANDKKBN       PIC X(01).
      *                種類数指示入力
                       09  SPA-COM-INPUTNAIF      PIC X(01).
      *                継続コメント入力
                       09  SPA-COM-INPUTCONT      PIC X(01).
      *                薬評入力
                       09  SPA-COM-YAKUHYO        PIC X(01).
      *                減点入力
                       09  SPA-COM-GENTEN         PIC X(01).
      *
      *                月上限数量チェック
                       09  SPA-COM-TUKIJGNFLG     PIC X(01).
                       09  SPA-COM-TUKIJGNFLG1    PIC X(01).
                       09  SPA-COM-TUKIJGNFLG2    PIC 9(01).
                       09  SPA-COM-KAIFLG2        PIC X(01).
      *ver.4.5.0
      *
      *        診療会計附加項目
                       09  SPA-COM-PLUS-AREA.
      *                手技毎
                           11  SPA-COM-SYUFLG        PIC  9(01).
      *                労災金額
                           11  SPA-COM-RSIKIN        PIC  9(08).
      *                酸素点数
                           11  SPA-COM-SANSOTEN      PIC  9(08).
      *                窒素点数
                           11  SPA-COM-CHISOTEN      PIC  9(08).
      *                フィルム点数
                           11  SPA-COM-FIRTEN-M      PIC  9(08).
      *                手技毎の点数
                           11  SPA-COM-SYUTEN-M      PIC  9(08).
      *                薬剤点数
                           11  SPA-COM-YKZTEN-M      PIC 9(08).
      *                器材点数
                           11  SPA-COM-KIZTEN-M      PIC 9(08).
      *ver.4.7
      *                院外処方剤点数
                           11  SPA-COM-INGAITEN      PIC 9(08).
      *
      *        点数マスタ項目
      *                点数マスタの診療区分
                       09  SPA-COM-TNSSRYKBN     PIC X(02).
      *                点数マスタの診療種別
                       09  SPA-COM-TNSSRYSYUKBN  PIC X(03).
      *                データ区分
                       09  SPA-COM-DATAKBN       PIC X(01).
      *                点数マスタ名称（包括検査用）
      *R02.4           09  SPA-COM-NAME          PIC X(78).
                       09  SPA-COM-NAME          PIC X(100).
      *                点数マスタ　薬剤内容
                       09  SPA-COM-MADOKUKBN     PIC 9(01).
                       09  SPA-COM-SINKEIKBN     PIC 9(01).
                       09  SPA-COM-SEIBUTUKBN    PIC 9(01).
                       09  SPA-COM-ZOEIZAIKBN    PIC 9(01).
                       09  SPA-COM-CSYYOURYO     PIC 9(05).
      *                単位コード
                       09  SPA-COM-TANICD        PIC X(03).
                       09  SPA-COM-TANINAME      PIC X(12).
      *                薬剤区分
                       09  SPA-COM-YKZKBN        PIC 9(01).
                       09  SPA-COM-ZAIGATAKBN    PIC 9(01).
      *                点数マスタ　部位区分
                       09  SPA-COM-BUIKBN        PIC X(01).
      *                検査等実施判断区分
                       09  SPA-COM-KNSJISKBN     PIC  9(01).
      *                検査等実施判断グループ
                       09  SPA-COM-KNSJISGRPKBN  PIC  9(02).
      *                注加算コード
                       09  SPA-COM-CHUKSNCD      PIC  X(04).
      *                注加算通番
                       09  SPA-COM-CHUKSNTUBAN   PIC  X(01).
      *                通則加算対象外区分
                       09  SPA-COM-TUSOKUGAIKBN   PIC  X(01).
      *                通則年齢区分
                       09  SPA-COM-TSUSOKUAGE    PIC  X(01).
      *                時間加算区分
                       09  SPA-COM-TIMEKSNKBN    PIC  9(01).
      *                検査用　包括対象検査
                       09  SPA-COM-HOUKNSKBN     PIC  9(02).
      *                検査用　検体検査コメント
                       09  SPA-COM-KENTAICOMMENT     PIC  9(01).
      *                検査用　区分番号
                       09  SPA-COM-CDKBN.
                           11  SPA-COM-CDKBN-KBN   PIC  X(01).
      ****                 11  SPA-COM-CDKBN-SYO   PIC  X(01).
      ****                 11  SPA-COM-CDKBN-BU    PIC  X(02).
      *                    労災用　区分番号
                           11  SPA-COM-CDKBN-KBNNUM     PIC  X(03).
      *                    労災用　区分番号枝番
                           11  SPA-COM-CDKBN-KBNNUM-EDA PIC  X(02).
      *H28.9               項番
                           11  SPA-COM-CDKBN-KOUBAN     PIC  X(02).
      *                算定履歴区分
                       09  SPA-COM-SANTEIRRKKBN    PIC  9(01).
      *                外来管理加算区分
                       09  SPA-COM-GAIKANRIKBN     PIC  9(01).
      *                指導管理料
                       09  SPA-COM-SDOKNRYO        PIC  9(02).
      *                点数識別
                       09  SPA-COM-TENSIKIBETU     PIC  9(01).
      *                特定器材年齢加算区分
                       09  SPA-COM-TOKUKIZAIAGEKSNKBN   PIC  9(01).
      *                酸素等区分
                       09  SPA-COM-SANSOKBN        PIC  9(01).
      *                特定器材種別１
                       09  SPA-COM-TOKUKIZAISBT1   PIC  9(01).
      *                上限点数（器材の上限）
                       09  SPA-COM-JGNTEN          PIC  9(07).
      *                保険適用区分
                       09  SPA-COM-HKNTEKKBN       PIC  9(01).
      *                点数欄集計先識別
                       09  SPA-COM-GAITENTTLKBN    PIC  9(03).
      *                点数欄集計先識別
                       09  SPA-COM-NYUTENTTLKBN    PIC  9(03).
      *                往診区分
                       09  SPA-COM-OSINKBN         PIC  9(01).
      *                告示区分１（点数記載用）
                       09  SPA-COM-KOKUJISKBKBN1   PIC  9(01).
      *                告示区分２（点数記載用）
                       09  SPA-COM-KOKUJISKBKBN2   PIC  9(01).
      *    画像診断
      *                きざみ値計算識別
                       09  SPA-COM-KZMCOMPSIKIBETU     PIC 9(01).
      *                きざみ値計算下限
                       09  SPA-COM-KZMKGN              PIC  9(08).
      *                きざみ値計算上限
                       09  SPA-COM-KZMJGN              PIC  9(08).
      *                きざみ値
                       09  SPA-COM-KZM                 PIC  9(08).
      *                きざみ値
                       09  SPA-COM-KZMTEN              PIC  9(07)V99.
      *                きざみ値
                       09  SPA-COM-KZMERR              PIC  9(01).
      *                包括逓減区分
                       09  SPA-COM-HOUKATUTEIGENKBN    PIC  9(03).
      *                年齢加算
                       09  SPA-COM-AGEKSNTBL           OCCURS 4.
      *                    年齢加算下限
                           11  SPA-COM-AGEKSNKGN       PIC  X(02).
      *                    年齢加算上限
                           11  SPA-COM-AGEKSNJGN       PIC  X(02).
      *                    年齢加算診療コード
                           11  SPA-COM-AGEKSNSRYCD     PIC  X(09).
      *                日数・回数
                       09  SPA-COM-DAYCNT              PIC  9(01).
      *                逓減対象区分
                       09  SPA-COM-TEIGENKBN           PIC  9(01).
      *    Ｈ１４．改正分
      *                後発医薬品区分
                       09  SPA-COM-KOUHATUKBN          PIC  9(01).
      *                基準不適合逓減区分
                       09  SPA-COM-KIJUNTEIGENKBN      PIC  9(01).
      *                基準不適合逓減対象施設基準
                       09  SPA-COM-KIJUNTEIGENCD       PIC  9(04).
      *                手術施設基準不適合
                       09  SPA-COM-KIJUNCHK-FLG        PIC  9(01).
      *    Ｈ１８．改正分
      *                処置乳幼児加算区分
                       09  SPA-COM-LASERKBN            PIC  9(01).
      *                極低出生体重児加算区分
                       09  SPA-COM-CHPMESUKSN          PIC  9(01).
      *                商品名等関連コード
                       09  SPA-COM-SYOMEIKANREN        PIC  X(09).
      *    Ｈ２０．改正分
      *                画像等手術支援加算
                       09  SPA-COM-GAZOOPESUP          PIC  9(01).
      *                麻酔識別区分
                       09  SPA-COM-MASUISKBKBN         PIC  9(01).
      *ver.4.6
      *                用法の数量埋め込み区分
                       09  SPA-COM-YCOMKBN             PIC  9(01).
      *ver.4.7
      *H24.4 改正分（ver.4.7.0　から使用）
      *                一般名記載区分（１：一般名）
                       09  SPA-COM-IPN-KISAIKBN        PIC  9(01).
      *                最低薬価
                       09  SPA-COM-GEN-PRICE           PIC  9(09)V9(4).
      *                薬価基準コード
                       09  SPA-COM-YAKKAKJNCD          PIC  X(12).
      *H28.4 改正分
      *                湿布薬区分
                       09  SPA-COM-SIPPUKBN        PIC  X(01).
      *                湿布薬　膏体量１
                       09  SPA-COM-SIPRYO1         PIC 9(05).
                       09  SPA-COM-SIPRYO2         PIC 9(05).
      *                湿布薬換算数量
                       09  SPA-COM-SIP-SURYO       PIC 9(05)V9(05).
      *                湿布薬減算有
                       09  SPA-COM-SIPGEN          PIC X(01).
                       09  SPA-COM-SIPKBN          PIC X(01).
      *                湿布減対象剤
                       09  SPA-COM-SIPZAI          PIC X(01).
      *
      *                労災　算定不可
                       09  SPA-COM-ROSAIKBN        PIC  X(01).
      *                労災　四肢加算
                       09  SPA-COM-SISIKSN         PIC  X(01).
      *                非侵襲的血行動態モニタリング加算
                       09  SPA-COM-HISINKSN        PIC  X(01).
      *                凍結保存同種組織加算
                       09  SPA-COM-TOUKETUHOZONKSN PIC  X(01).
      *H30.4
      *                悪性腫瘍病理組織標本加算
                       09  SPA-COM-AKUSEIBYORIKSN  PIC  X(01).
      *R02.4
      *                創外固定器加算
                       09  SPA-COM-SOGAIKOTEIKSN   PIC  X(01).
      *                超音波切削器加算
                       09  SPA-COM-CHPSESSAKUKSN   PIC  X(01).
      *                左心耳閉鎖術併施区分
                       09  SPA-COM-SASINJIHEISIKBN PIC  X(01).
      *                変更年月日日（マスタ更新判定用）
                       09  SPA-COM-CHGYMD          PIC  X(08).
      *
      *H29.1           ユーザー管理区分
                       09  SPA-COM-MASTER-CLASS    PIC  X(01).
      *
      *    点数マスタ付加情報より
      *                採血区分
                       09  SPA-COM-SAIKETUKBN          PIC 9(01).
      *                前回検査日区分
                       09  SPA-COM-ZENKNSKBN           PIC 9(01).
      *                入力チェック区分
                       09  SPA-COM-INPCHKKBN           PIC 9(01).
      *                数量換算単位
                       09  SPA-COM-KANZANTANICD        PIC X(03).
      *                数量換算値
                       09  SPA-COM-KANZANCHI           PIC 9(05)V9(05).
      *ver.4.7         内服滴剤
                       09  SPA-COM-SOURYOHENKBN        PIC  9(01).
      *    点数マスタ薬剤附加情報より
      *                内服算定
                       09  SPA-COM-NAIFUKUKBN          PIC 9(01).
      *                長期投薬日数
                       09  SPA-COM-CHOKINISSU          PIC 9(03).
                       09  SPA-COM-CHOKIERR            PIC 9(01).
      *                投与量
                       09  SPA-COM-TOUYOERR            PIC 9(01).
                       09  SPA-COM-TOUYO-G             OCCURS   10.
                           11  SPA-COM-TOUYOKGNAGE     PIC X(02).
                           11  SPA-COM-TOUYOJGNAGE     PIC X(02).
                           11  SPA-COM-TOUYORYO1       PIC 9(05)V9(05).
                           11  SPA-COM-TOUYORYO2       PIC 9(05)V9(05).
      *                月上限回数
                       09  SPA-COM-TUKIJGNKAISU        PIC  9(3).
      *                月総投与量
                       09  SPA-COM-TUKIJGNTOUYORYO     PIC  9(5)V9(5).
      *    患者禁忌薬剤
      *                禁忌設定
                       09  SPA-COM-PTKINKI             PIC 9(01).
                       09  SPA-COM-PTKINKI-ARI         PIC 9(01).
                       09  SPA-COM-KINKSTYMD           PIC X(08).
                       09  SPA-COM-TOUYOYMD            PIC X(08).
                       09  SPA-COM-PTKINKICD           PIC 9(01).
      *v4.8  連続行挿入用
                       09  SPA-COM-GMNINS              PIC X(01).
      *H28.2 一般名表示対応
      *                銘柄名
      *??????????      09  SPA-COM-MEI-NAME            PIC X(78).
                       09  SPA-COM-MEI-NAME            PIC X(98).
      *                一般名（処方せん記載）
                       09  SPA-COM-IPN-NAME            PIC X(100).
                       09  SPA-COM-IPN-TANINAME        PIC X(02).
                       09  SPA-COM-MEI-KBN             PIC X(01).
      *H28.5           一般名記載区分
                       09  SPA-COM-TNSP-KISAIKBN       PIC X(01).
      *                向精神薬区分
                       09  SPA-COM-PSY-CLASS           PIC X(01).
      *H31.4
      *                向精神薬長期処方対象
                       09  SPA-COM-PSYCHO2             PIC X(01).
      *H28.9           持参薬剤区分
                       09  SPA-COM-JISANYAKU           PIC X(01).
      *H30.9           レセプト記載事項区分
                       09  SPA-COM-RECEKISAI           PIC X(01).
                       09  SPA-COM-RECEAUTO            PIC X(01).
      *R01.5
      *                一般名コード
                       09  SPA-COM-GECODE              PIC X(12).
      *ver.4.7 から予備
                       09  SPA-COM-YOBI1               PIC X(10).
                       09  SPA-COM-YOBI2               PIC X(10).
                       09  SPA-COM-YOBI3               PIC S9(10)V9(5).
      * 
