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
      *----------------------------------------------------------*
      *    診療行為算定用共通パラメタ　   COPY (CPORCSC00.INC)
      *                              (ORCSC10 〜 ORCSC80 で使用）
      *----------------------------------------------------------*
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      * 01.00.01     MCC-多々納   01.06.15  入力値の追加 
      * 01.00.02     多々納       02/04/19  生活習慣病指導算定時の追加
      * 01.00.03     NACL-多々納  02/10/25  入院対応
      * 01.00.04     NACL-多々納  04/03/08  時間外加算特例乳幼児加算追加
      * 01.00.05     NACL-多々納  05/12/26  急性発生日追加（入院）
      * 03.05.01     NACL-多々納  07/07/18  皮下筋肉注射料変換選択
      * 04.08.01     NACL-多々納  16/03/XX  終了区分等追加
      ************************************************************
      *
       01  ORCSC00AREA.
      *
         02  LNK-ORCSZAI-AREA.
      *
      *    １：剤ごとのチェック＆算定
      *    ２：全体のチェック
      *    ３：終了時の自動加算算定
           03  LNK-ORCSC-KBN           PIC X(01).
      *    1：終了時
           03  LNK-ORCSC-KBN2          PIC X(01).
      *    メインＰＧ
           03  LNK-ORCSC-PG            PIC X(04).
      *    剤ごとの回数
           03  LNK-ORCSC-KAISU         PIC 9(03).
      *    訂正フラグ
           03  LNK-ORCSC-SYORIFLG      PIC 9(01).
      *    初診フラグ（新患かどうかの判定）
           03  LNK-ORCSC-SYOSIN        PIC 9(01).
      *    初診フラグ２（今回初診料かどうかの判定）
           03  LNK-ORCSC-SYOSIN2       PIC 9(01).
      *    同日初診フラグ２（今回初診料かどうかの判定）
           03  LNK-ORCSC-DOUSIN       PIC 9(01).
      *    訂正前診療科
           03  LNK-ORCSC-OLD-SRYKA     PIC X(02).
      *    訂正前保険組合せ
           03  LNK-ORCSC-OLD-HKNCOMBI  PIC X(04).
      *
      *    画面行位置
           03  LNK-ORCSC-CHK-IDX       PIC 9(04).
      *    警告行位置
           03  LNK-ORCSC-KEI-IDX       PIC 9(04).
      *
      *    合計点数
           03  LNK-ORCSC-ZAITEN        PIC 9(08).
           03  LNK-ORCSC-ZAIKEI        PIC 9(08).
      *    老人区分
           03  LNK-ORCSC-ROUJIN        PIC 9(01).
      *    年齢 
           03  LNK-ORCSC-NENREI.
               05  LNK-ORCSC-NENREI-YY     PIC 9(03).
               05  LNK-ORCSC-NENREI-MM     PIC 9(02).
               05  LNK-ORCSC-NENREI-DD     PIC 9(02).
      *    時間外加算有り
           03  LNK-ORCSC-TIMEFLG           PIC 9(01).
      *    外来管理料算定有無
           03  LNK-ORCSC-GAIFLG            PIC 9(01).
      *複数診療科あり
           03  LNK-ORCSC-FUKU-SRYKA        PIC  9(01).
      *
      *診察料あり
           03  LNK-ORCSC-SINSATU-ARI       PIC  9(01).
      *
      *初診算定日、退院日
           03  LNK-ORCSC-SYOYMD            PIC  X(08).
      *訂正時時の初診算定日
           03  LNK-ORCSC-SYOYMD-OLD        PIC  X(08).
      *最終受診日
           03  LNK-ORCSC-LSTYMD            PIC  X(08).
      *最終受診科
           03  LNK-ORCSC-LSTSRYKA          PIC  X(02).
      *最終受診科
           03  LNK-ORCSC-LSTSRYKAMEI       PIC  X(20).
      *院外区分
           03  LNK-ORCSC-INGAIKBN          PIC  X(01).
      *初期表示
           03  LNK-ORCSC-TOROKU            PIC  X(01).
      *同日再診有無
           03  LNK-ORCSC-RRK-LSTYMD        PIC  X(08).
      *皮下筋肉注射料変換選択
           03  LNK-ORCSC-CHG310FLG         PIC  X(01).
      *腫瘍マーカー一覧展開選択
           03  LNK-ORCSC-AKUSEIHYOFLG      PIC  X(01).
      *残量廃棄区分
           03  LNK-ORCSC-HAIKIKBN-FLG      PIC  X(01).
      *
      *更新前自動算定のみに使用（Ｋ０２のみ）
      *調剤技術基本料有無
           03  LNK-ORCSC-CHOKI-FLG         PIC  9(01).
      *薬剤情報提供料有無(一般）
           03  LNK-ORCSC-YAKJYO-1          PIC  9(01).
      *薬剤情報提供料有無(老人１）
           03  LNK-ORCSC-YAKJYO-2          PIC  9(01).
      *薬剤情報提供料有無(老人２）
           03  LNK-ORCSC-YAKJYO-3          PIC  9(01).
      *特定薬剤治療管理料有無
           03  LNK-ORCSC-TOKUTEI-FLG       PIC  9(01).
      *外来診療料算定有無
           03  LNK-ORCSC-GAIRAISIN         PIC  9(01).
      *
      *
      *処方箋料算定判定用
           03  LNK-ORCSC-SANTEI.
      *        老人慢性疾患外来総合診療科有り
               05  LNK-ORCSC-ROUMANSEI-ARI       PIC  9(01).
               05  LNK-ORCSC-ROUMANSEI-ARI2      PIC  9(01).
      *        寝たきり老人在宅総合診察料あり
               05  LNK-ORCSC-NETAKIRI-ARI        PIC  9(01).
               05  LNK-ORCSC-NETAKIRI-ARI2       PIC  9(01).
               05  LNK-ORCSC-NETAKIRI-ARI3       PIC  9(01).
      *        在宅訪問診療料有り
               05  LNK-ORCSC-HOUMON-ARI           PIC  9(01).
      *        小児科外来診療科算定有り
               05  LNK-ORCSC-SYONIKA-ARI         PIC  9(01).
               05  LNK-ORCSC-SYONIKA-ARI2        PIC  9(01).
      *
      *    電話再診の有無
           03  LNK-ORCSC-TELSAI-ARI        PIC 9(01).
      *特定疾患有無（病名）
           03  LNK-ORCSC-TOKUTEI           PIC  9(01).
           03  LNK-ORCSC-TOKUTEIFLG        PIC  9(01).
      *
      *受診回数
           03  LNK-ORCSC-SINSATU-CNT       PIC  9(03).
      *特定薬剤治療管理料初回年月日（Ｉ／Ｏ）
           03  LNK-ORCSC-TOKTEI-YMD        PIC  X(08).
      *１５歳未満再診科算定有り
           03  LNK-ORCSC-15MIMAN-ARI       PIC  9(01).
      *
      *厚生省が定める患者（再診料）
           03  LNK-ORCSC-TOKUTEI-KNJ       PIC  9(01).
           03  LNK-ORCSC-TOKUTEI-KNJ2      PIC  9(01).
      *厚生省が定める患者（外来管理）
           03  LNK-ORCSC-TOKUTEIG-KNJ      PIC  9(01).
           03  LNK-ORCSC-TOKUTEIG-KNJ2     PIC  9(01).
      *
      *生活習慣病指導管理料あり
           03  LNK-ORCSC-SEIKATU-ARI       PIC  9(01).
           03  LNK-ORCSC-SEIKATU-ARI2      PIC  9(01).
      *人工腎臓
           03  LNK-ORCSC-TOKUTEI-JNZ       PIC  9(01).
      *再診料変更確認済み
           03  LNK-ORCSC-SAISIN-CHK        PIC  9(01).
      *消炎鎮痛処置当月内算定
           03  LNK-ORCSC-SYOEN-CNT         PIC  9(03).
      *急性発生１８０日以内
           03  LNK-ORCSC-KYUSEI-180          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-90           PIC  X(08).
           03  LNK-ORCSC-KYUSEI-YMD          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-END          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-FLG          PIC  9(01).
      *当月外来管理加算の取れない患者
           03  LNK-ORCSC-GAIRAI-NASI       PIC  9(01).
      *時間外特例区分
           03  LNK-ORCSC-JIKANTOKU         PIC  9(01).
      *時間外加算特例乳幼児加算
           03  LNK-ORCSC-JIKANTOKU-NYU     PIC  9(01).
      *時間外加算乳幼児チェック
           03  LNK-ORCSC-JIKANERR-FLG      PIC  9(01).
      *
      *    リハビリ逓減確認
           03  LNK-ORCSC-FLG-RIHTEI            PIC 9(01).
      *
      *ver.4.8
      *    労災レセ電対応
           03  LNK-ORCSC-ROURECEKBN        PIC 9(01).
      *
      *投薬の継続分チェック
           03  LNK-ORCSC-KBN20.
      *継続区分
               05  LNK-ORCSC-CONTKBN           PIC  X(01).
      *伝票番号
               05  LNK-ORCSC-DENPNUM           PIC  9(07).
      *同時診療連番
               05  LNK-ORCSC-DOUJI-RENNUM      PIC  9(01).
      *
      *    診療行為月別チェック用（検査用）
           03  LNK-NAIACCT-AREA.
               05  LNK-ACCT-TBL             OCCURS   100.
                   07  LNK-ACCT-SRYKBN          PIC  X(02).
                   07  LNK-ACCT-SRYCD           PIC  X(09).
                   07  LNK-ACCT-CDKBN.
                       09  LNK-ACCT-CDKBN-KBN      PIC  X(01).
                       09  LNK-ACCT-CDKBN-KBNNUM   PIC  X(03).
      *****************09  LNK-ACCT-CDKBN-SYO   PIC  X(01).
      *****************09  LNK-ACCT-CDKBN-BU    PIC  X(02).
      *
           03  LNK-NAIACCT2-AREA.
      *        当日チェック用（点滴）
               05  LNK-ACCT2-SRYCD           PIC X(09).
               05  LNK-ACCT2-ZAINUM          PIC 9(08).
               05  LNK-ACCT2-CSYYOURYO       PIC 9(08)V9(5).
      *        静脈注射
               05  LNK-ACCT2-SRYCD-JYO1      PIC X(09).
               05  LNK-ACCT2-SRYCD-JYO2      PIC X(09).
      *
      *    訂正時の診療コード
           03  LNK-ORCSC-TEISEI-AREA.
               05  LNK-ORCSC-TEISEI-OCC      OCCURS   400.
                   07  LNK-ORCSC-TEI-SRYCD   PIC X(09).
                   07  LNK-ORCSC-TEI-KAISU   PIC 9(03).
      *
      *    労災の外来管理加算読み替え点数
           03  LNK-ORCSC-GAIRAI-TEN        PIC  9(05)V9(03).
      *    外来管理加算削除のチェック
           03  LNK-ORCSC-GAIKANRIKBN-CHK   PIC  X(01).
      *
      *    入院期間
           03  LNK-ORCSC-NYUINYMD           PIC X(08).
           03  LNK-ORCSC-TAIINYMD           PIC X(08).
      *    入院検査包括
           03  LNK-ORCSC-KENSAFLG           PIC 9(01).
      *
      *    同日初診算定科（外来のみ）
      *D   03  LNK-ORCSC-DOUSYOSIN-AREA.
      *        05  LNK-ORCSC-DOUSYOSIN-OCC       OCCURS   10.
      *            07  LNK-ORCSC-DOU-SRYKA       PIC X(02).
      *D           07  LNK-ORCSC-DOU-SRYYMD      PIC X(08).
      *
      *    加算追加領域（更新時の自動加算（単独剤））
           03  LNK-ORCSC-KASAN.
               05  LNK-ORCSC-KASAN-TBL  OCCURS  10.
                   07  LNK-ORCSC-KASN-TBL2     OCCURS   30.
                       09  LNK-ORCSC-SRYKA         PIC X(02).
                       09  LNK-ORCSC-HKNCOMBI      PIC X(04).
                       09  LNK-ORCSC-CHIKENKBN     PIC X(01).
                       09  LNK-ORCSC-SRYSYUKBN     PIC X(03).
                       09  LNK-ORCSC-SRYKBN        PIC X(02).
                       09  LNK-ORCSC-SRYCD         PIC X(09).
                       09  LNK-ORCSC-TENSU         PIC 9(05)V9(05).
                       09  LNK-ORCSC-SURYO         PIC 9(05)V9(05).
                       09  LNK-ORCSC-SRYCD-KS      PIC X(09).
                       09  LNK-ORCSC-TENSU-KS      PIC 9(05)V9(05).
      *H28.4 追加
                       09  LNK-ORCSC-SRYCD-KS2     PIC X(09).
                       09  LNK-ORCSC-TENSU-KS2     PIC 9(05)V9(05).
                       09  LNK-ORCSC-ATAI-G.
                           11  LNK-ORCSC-ATAI   PIC X(08) OCCURS  5.
      *                入院時の回数
                       09  LNK-ORCSC-INPUTCD-G.
                           11  LNK-ORCSC-INPUTCD       PIC X(22)
                                                   OCCURS  5.
      *                自動発生分としない
                       09  LNK-ORCSC-AUTOKBN       PIC X(01).
      *                包括剤
                       09  LNK-ORCSC-HKTKBN        PIC 9(01).
      *                算定履歴区分
                       09  LNK-ORCSC-SANTEIRRKKBN  PIC X(01).
      *    確認メッセージ
           03  LNK-ORCSC-KIDCD              PIC  X(04).
      *
      *    労災の時、読み替えあり
           03  LNK-ORCSC-RSIOK              PIC  X(01).
      *    エラーメッセージ
           03  LNK-ORCSC-ERRMSG             PIC  X(80).
      *
      *    全体件数
         02  LNK-ORCSALL-AREA.
      *
           03  LNK-ORCSC-CHUSYA-AREA.
      *        点滴・静脈注射
               05  LNK-ORCSC-TEN             PIC X(01).
               05  LNK-ORCSC-JYO1            PIC X(01).
               05  LNK-ORCSC-JYO2            PIC X(01).
      *
      *リハビリ当月内回数集計
           03  LNK-RIGAKU-NCNT-AREA          PIC X(1301).
      *
      *    検査逓減領域
           03  LNK-ORCSC-TEIGEN-AREA.
               05  LNK-ORCSC-TEIGEN-OCC        OCCURS   20.
                   07  LNK-ORCSC-TEIGEN-SRYCD     PIC X(09).
                   07  LNK-ORCSC-TEIGEN-HOU       PIC X(02).
                   07  LNK-ORCSC-TEIGEN-CNT       PIC 9(03).
                   07  LNK-ORCSC-TEIGEN-SANTEI    PIC 9(03).
      *
      *外来まとめ算定履歴領域
           03  LNK-ORCSC-KT01-AREA.
               05  LNK-ORCSC-KT01FLG         PIC X(01).
               05  LNK-ORCSC-KT01-OCC        OCCURS   100.
                   07  LNK-ORCSC-KT01-SRYCD  PIC X(09).
                   07  LNK-ORCSC-KT01-DAY-G.
                       09  LNK-ORCSC-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *
