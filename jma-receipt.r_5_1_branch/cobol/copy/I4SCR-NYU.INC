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
      *    入院行為ＳＰＡ　データ　　　 　　 COPY (I4SCR-NYU.INC)
      *             画面のスパ領域をデータで使用する
      *                                  （各行為チェックに使用）
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *
      ************************************************************
      *
           03  SPA-GMN-MAX             PIC 9(04).
      *
           03  SPA-GMN-REC.
               05  SPA-GMN-TBL             OCCURS   200.
                   07  SPA-GMN-TBLREC.
                       09  SPA-GMN-SRYKBN        PIC X(02).
                       09  SPA-GMN-INPUTCD       PIC X(22).
                       09  SPA-GMN-MEISYO-G.
                           11  SPA-GMN-MEIH      PIC X(02).
                           11  SPA-GMN-MEISYO    PIC X(60).
                       09  SPA-GMN-SURYO         PIC X(09).
                       09  SPA-GMN-TANINAME      PIC X(12).
                       09  SPA-GMN-TENSU         PIC X(08).
                       09  SPA-GMN-KAISU         PIC X(04).
                       09  SPA-GMN-KEI           PIC 9(08).
      *                入力コードの桁数
                       09  SPA-GMN-INCNT         PIC 9(02).
      *
                   07  SPA-COM-TBLREC.
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
                       09  SPA-MAE-INPUTCD       PIC X(22).
      *
                       09  SPA-COM-SRYSYUKBN     PIC X(03).
                       09  SPA-COM-SRYKBN        PIC X(02).
                       09  SPA-COM-INPUTCD       PIC X(09).
                       09  SPA-COM-SURYO         PIC 9(05)V9(03).
      *                剤合計点数
                       09  SPA-COM-TENSU         PIC 9(08).
      *                点数
                       09  SPA-COM-TEN           PIC 9(07)V99.
      *                点数（きざみ値計算後）
                       09  SPA-COM-KISOTEN       PIC 9(07)V99.
                       09  SPA-COM-KAISU         PIC 9(08).
                       09  SPA-COM-KEI           PIC 9(08).
      *                外用薬用入力用
                       09  SPA-COM-SURYO2        PIC 9(05)V9(03).
                       09  SPA-COM-KAISU2        PIC 9(08).
      *
                       09  SPA-COM-BUNGSU        PIC 9(08).
                       09  SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI1         PIC X(08).
                           11  SPA-COM-ATAI2         PIC X(08).
                           11  SPA-COM-ATAI3         PIC X(08).
                           11  SPA-COM-ATAI4         PIC X(08).
                       09  SPA-COM-INPUTCHI-R    REDEFINES
                                                 SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI          PIC X(08)
                                                     OCCURS  4.
      *                手技料計１
                       09  SPA-COM-SYUTEN1       PIC 9(08).
      *                手技料計２
                       09  SPA-COM-SYUTEN2       PIC 9(08).
      *                薬剤点数
                       09  SPA-COM-YKZTEN        PIC 9(08).
      *                薬剤点数
                       09  SPA-COM-KIZAITEN      PIC 9(08).
      *
                       09  SPA-COM-ZAINUM        PIC 9(08).
      *                剤終了
                       09  SPA-COM-ZAIEND        PIC X(01).
      *                自動算定区分（１：画面表示、２：入力条件により変更）
                       09  SPA-COM-AUTOKBN       PIC X(01).
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
                           11  SPA-COM-KNKUMU        PIC X(01).
      *                    労災読み替え加算分
                           11  SPA-COM-RSIFLG2       PIC X(01).
      *                    警告済フラグ
                           11  SPA-COM-KZMFLG        PIC X(01).
      *                    併用算定済フラグ
                           11  SPA-COM-SDOFLG        PIC X(01).
      *                    基本チェック済フラグ
                           11  SPA-COM-CHKFLG        PIC X(01).
      *                    時間加算フラグ
                           11  SPA-COM-TIMEFLG       PIC 9(01).
      *                計算チェック済フラグ（毎回クリア）
                       09  SPA-COM-SANFLG        PIC 9(01).
      *                逓減対象有無（１：逓減有り）
                       09  SPA-COM-TEIGEN-FLG    PIC  9(01).
      *                逓減対象チェック済
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
      *        点数マスタ項目
      *                点数マスタの診療区分
                       09  SPA-COM-TNSSRYKBN     PIC X(02).
      *                点数マスタの診療種別
                       09  SPA-COM-TNSSRYSYUKBN  PIC X(03).
      *                データ区分
                       09  SPA-COM-DATAKBN       PIC X(01).
      *                点数マスタ　薬剤内容
                       09  SPA-COM-MADOKUKBN     PIC 9(01).
                       09  SPA-COM-SINKEIKBN     PIC 9(01).
                       09  SPA-COM-SEIBUTUKBN    PIC 9(01).
                       09  SPA-COM-ZOEIZAIKBN    PIC 9(01).
                       09  SPA-COM-CSYYOURYO     PIC 9(05).
      *                単位コード
                       09  SPA-COM-TANICD        PIC X(03).
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
      *                検査用　区分番号
                       09  SPA-COM-CDKBN.
                           11  SPA-COM-CDKBN-KBN   PIC  X(01).
                           11  SPA-COM-CDKBN-SYO   PIC  X(01).
                           11  SPA-COM-CDKBN-BU    PIC  X(02).
      *                    労災用　区分番号
                           11  SPA-COM-CDKBN-KBNNUM     PIC  X(03).
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
      *    画像診断
      *                きざみ値計算識別
                       09  SPA-COM-KZMCOMPSIKIBETU     PIC 9(01).
      *                きざみ値計算上限
                       09  SPA-COM-KZMJGN              PIC  9(08).
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
      *                後発医薬品区分
                       09  SPA-COM-KOUHATUKBN          PIC  9(01).
      *                基準不適合逓減区分
                       09  SPA-COM-KIJUNTEIGENKBN      PIC  9(01).
      *                基準不適合逓減対象施設基準
                       09  SPA-COM-KIJUNTEIGENCD       PIC  9(03).
      *    点数マスタ付加情報より
      *                採血区分
                       09  SPA-COM-SAIKETUKBN          PIC 9(01).
      *                入力チェック区分
                       09  SPA-COM-INPCHKKBN           PIC 9(01).
      * 
