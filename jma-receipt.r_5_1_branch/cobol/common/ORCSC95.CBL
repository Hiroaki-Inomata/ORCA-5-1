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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCSC95.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 初期ＳＰＡ編集処理(K01,K02,K02Nで使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/08/03    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/10  労災追加
      *  01.00.02    NACL-多々納  02/08/06  アフターケア用追加
      *  01.00.03    NACL-多々納  02/09/11  自賠責追加
      *  01.00.04    NACL−多々納 02/10/10  外来管理加算チェックの有無
      *  01.00.05    NACL−多々納 02/10/18  入院追加
      *  01.00.06    NACL-多々納  03/02/19  療養担当手当追加
      *  01.00.07    NACL-多々納  03/04/09  保険編集サブプロ追加
      *  01.00.08    NACL-多々納  03/07/17  検索変更他
      *  01.00.09    NACL-多々納  03/10/27  育児栄養加算、カーソル移動追加
      *  01.00.10    NACL-多々納  04/10/08  ワーク診療行為にドクター追加
      *  01.00.11    NACL-多々納  05/01/14  病名編集追加
      *  01.00.12    NACL-多々納  05/11/18  MONFUNC 対応 他
      *  03.04.00    NACL-多々納  07/02/XX  包括診療対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.01    NACL-多々納  07/08/27  公費期限切れ対応
      *  04.00.00    NACL-多々納  07/09/XX  テーブル４００対応
      *  04.00.01    NACL-多々納  07/10/XX  受診履歴一覧次頁対応
      *  04.01.00    NACL-多々納  07/12/XX  .213 自動変換設定対応
      *                                     病名疾患区分設定対応
      *                                     アトピー性皮膚炎の年齢対応
      *  04.01.01    NACL-多々納  08/03/03  特定疾患区分改正対応
      *  04.03.01    NACL-多々納  08/08/04  施設入居対応
      *  04.03.01    NACL=多々納  08/08/07  労災初診料Ｈ２０年４月改正対応他
      *  04.04.00    NACL=多々納  08/11/27  入院一括関連対応
      *  04.04.00    NACL=多々納  09/01/30  本院分院対応
      *  04.05.00    NACL=多々納  09/10/30  多剤投与・換算値入力他対応
      *  04.05.00    NACL=多々納  10/03/XX  平成２２年４月改正対応
      *  04.06.00    NACL=多々納  10/07/01  残量廃棄区分対応
      *  04.06.00    NACL=多々納  10/11/02  難病外来指導管理料対応
      *  04.06.00    NACL=多々納  10/11/25  病名補足コメント対応
      *  04.07.00    NACL=多々納  11/09/08  アトピー性皮膚炎と他病名対応
      *  04.07.00    NACL=多々納  11/10/XX  同日再入院対応
      *  04.07.00    NACL=多々納  11/10/30  API検索対応
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      *  04.07.00    NACL-多々納  13/03/XX  Ｈ２５年４月改正対応
      *  04.07.00    NACL-多々納  13/07/XX  第三者行為対応
      *  04.08.00    NACL-多々納  14/03/XX  Ｈ２６年４月改正対応
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.08.00    NACL=多々納  15/12/25  HAORI対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/09/XX  Ｈ２８年１０月改正対応
      *  04.08.00    NACL-多々納  16/10/04  要介護状態表示対応
      *  04.08.00    NACL-多々納  16/12/08  病名レセ非表示対応
      *  04.08.00    NACL-多々納  16/12/XX  指導料等自動算定対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  04.08.00    NACL-多々納  18/06/XX  複数科展開ドクター対応
      *****************************************************************
      *
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDABAN          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-TENSUPLUS2      PIC 9(01).
           03  FLG-PTRSIINF        PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-PTTOKKI         PIC 9(01).
           03  FLG-PTSAME          PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
           03  FLG-PTCARE-NINTEI   PIC 9(01).
      *
           03  FLG-SYSKANRIOK      PIC 9(01).
           03  FLG-HKNCOMBICHK     PIC 9(01).
           03  FLG-WKSYORI         PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
      *
           03  FLG-DRCDOK          PIC 9(01).
           03  FLG-DRCDCHK         PIC 9(01).
           03  FLG-NO              PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-TIME            PIC 9(01).
      *
           03  FLG-JIKAN           PIC 9(01).
           03  FLG-COMMENT         PIC 9(01).
      *
           03  FLG-UKETUKE-ARI     PIC 9(01).
           03  FLG-HEN             PIC 9(01).
           03  FLG-CHK             PIC 9(01).
      *
           03  FLG-ATOPII          PIC 9(01).
           03  FLG-HAORI           PIC 9(01).
      *H28.12
           03  FLG-BYONO           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-STR             PIC 9(02).
           03  IDX-R               PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-SRY             PIC 9(04).
           03  IDX-TEI             PIC 9(04).
           03  IDX-HKN             PIC 9(04).
           03  IDXF                PIC 9(04).
      *
           03  IDX-J1              PIC 9(04).
           03  IDX-J2              PIC 9(04).
      *
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDX-S1              PIC 9(04).
           03  IDX-S2              PIC 9(06).
           03  IDX-SP              PIC 9(06).
      *
           03  IDX-F3              PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
      *
           03  IDY-TBL             PIC 9(04).
      *
           03  IDX-AC              PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-SRYKA           PIC X(02).
           03  WRK-SRYKAMEI        PIC X(20).
      *
           03  WRK-SRYYYMM         PIC X(06).
           03  WRK-HKNCOMBI        PIC X(100).
           03  WRK-HKNKBN-MEI      PIC X(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAIKAISU        PIC 9(08).
           03  WRK-EDABAN          PIC 9(03).
      *剤番号
           03  WRK-ZAINUM-TBL.
               05  WRK-ZAINUM-G        OCCURS   50.
                   07  WRK-TZAINUM         PIC  9(08)
                                           OCCURS 15.
           03  WRK-RENNUM              PIC  9(01).
           03  WRK-RRK-SRYYMD          PIC  X(08).
           03  WRK-RRK-DENPNUM         PIC  9(07).
      *    診療履歴保存
           03  WRK-CONTKBN-AREA.
               05  WRK-CONTKBN-TBL      OCCURS    3.
                   07  WRK-CONTKBN-NUM      PIC X(04).
                   07  WRK-CONTKBN-TEN      PIC X(01).
           03  IDX-JYU                 PIC 9(04).
           03  WRK-IDX                 PIC 9(04).
      *    初診料算定チェック
           03  FLG-TENKI               PIC 9(01).
           03  FLG-CHIYU               PIC 9(01).
           03  WRK-TENKIYMD            PIC X(08).
      *    剤点数　集計
           03  WRK-ZAITENKEI           PIC 9(11).
      *
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-ZZ9             PIC ZZZ.
      *    未収金
           03  WRK-MISYU           PIC S9(08).
           03  WRK-CHIYUYMD        PIC X(08).
      *    合計点数
           03  WRK-GOK-TENSU       PIC S9(11).
      *
           03  WRK-HEN-SRYCD       PIC X(09).
      *
      *    表示用保険
           03  WRK-IDXH                PIC 9(04).
           03  WRK-IDXH2               PIC 9(04).
           03  WRK-CNT-HKN             PIC 9(04).
           03  WRK-CNT-HKN2            PIC 9(04).
      *    複数科まとめ
           03  WRK-FUKU-AREA.
               05  WRK-FUKU-TBL        OCCURS   10.
                   07  WRK-FUKU-HKNCOMBI       PIC X(04).
                   07  WRK-FUKU-SRYKA          PIC X(02).
                   07  WRK-FUKU-KBN            PIC X(01).
                   07  WRK-FUKU-DENPNUM        PIC 9(07).
           03  IDX-F                   PIC 9(04).
      *
           03  WRK-DRCD                PIC X(05).
           03  WRK-DRCD2               PIC X(05).
           03  WRK-DRCDNAME            PIC X(50).
           03  WRK-DRCDNAME2           PIC X(50).
      *
      *    時間
           03  WRK-SYS-TIME.
               05  WRK-SYS-HHMM        PIC 9(04).
               05  WRK-SYS-SS          PIC 9(04).
      *
           03  SYS-TIME                PIC 9(08).
      *    直帰の初診算定日
           03  WRK-YMD.
               05  WRK-YYMM            PIC X(06).
               05  WRK-DD              PIC X(02).
           03  WRK-SYOSINYMD1          PIC X(08).
           03  WRK-SYOSINYMDS          PIC X(08).
           03  WRK-YMDS                PIC X(08).
           03  WRK-YMDS-1              PIC X(08).
           03  WRK-DD-1                PIC X(02).
           03  WRK-SRYCD               PIC X(09).
      *
           03  WRK-KEY-SRYYMD          PIC X(08).
           03  WRK-KEY-SRYKA           PIC X(02).
           03  WRK-KEY-DRCD            PIC X(05).
           03  WRK-KEY-HKNCOMBI        PIC X(04).
           03  WRK-FUKU-ZAINUM         PIC 9(08).
      *    ワーク診療行為内容追加
           03  WRK-INPUTCD             PIC X(22).
      *    病名編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEI-TBL.
      *H30.8       07  WRK-BYOMEI-OCC          OCCURS   100.
                   07  WRK-BYOMEI-OCC          OCCURS   999.
                       09  WRK-BYOMEI-NO       PIC X(02).
                       09  WRK-BYOMEI          PIC X(164).
                       09  WRK-UTAGAIFLG       PIC X(02).
                       09  WRK-MANSEIKBN       PIC X(02).
      *                レセプト表示なし
                       09  WRK-REZEFLG         PIC X(01).
      *
               05  WRK-BYOMEI-HEN          PIC X(164).
               05  WRK-Z9                  PIC Z9.
               05  IDX-B                   PIC 9(04).
               05  IDX-B2                  PIC 9(04).
               05  IDX-B3                  PIC 9(04).
               05  FLG-BYOMEI-END          PIC 9(01).
      *        主病名フラグ
               05  WRK-SYUBYOFLG-AREA.
                   07  WRK-SYUBYOFLG       PIC X(01)   OCCURS   10.
      *H28.12
      *    レセプト表示期間
           03  WRK-BYOYMD.
               05  WRK-BYOYYMM.
                   07  WRK-BYO-YY          PIC 9(04).
                   07  WRK-BYO-MM          PIC 9(02).
               05  WRK-BYO-DD              PIC 9(02).
           03  WRK-BYO-TUKI                PIC 9(03).
      *
      *    診療年月日−１ヶ月
           03  WRK-SRYYMD-1.
               05  WRK-SRYYM-1.
                   07  WRK-SRYYY       PIC 9(04).
                   07  WRK-SRYMM       PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
      *
           03  WRK-MAE-HKNCOMBI        PIC X(04).
      *    薬剤総量集計用
           03  WRK-SOURYO              PIC 9(05)V9(5).
      *
           03  WRK-CHKHKTBYO-G.
               05  WRK-CHKHKTBYO       PIC X(01)   OCCURS   4.
           03  WRK-CNT                 PIC 9(02).
      *
           03  WRK-KAIGOMEI            PIC X(60).
      *
      *    禁忌１
       01  WRK-TABOO-G.
           03  WRK-TABOO               PIC X(62)
                                       OCCURS   32.
      *初診算定編集
       01  WRK-SYOSINYMD-AREA.
           03  WRK-SYO-YMD             PIC X(08).
           03  WRK-SYO-YMD1            PIC X(08).
           03  WRK-RSI-YMD             PIC X(08).
           03  WRK-RSI-YMD1            PIC X(08).
      *
      *他科受診科有り
       01  WRK-TAKA-SRYKA-AREA.
           03  WRK-TAKA-SRYKA          PIC  X(02)
                                               OCCURS  10.
      *他保険受診科有り
       01  WRK-TA-HKNCOMBI-AREA.
           03  WRK-TA-HKNCOMBI         PIC  9(04)
                                               OCCURS  10.
      *    公費期限切れ表示用
       01  WRK-KOH-AREA.
           03  WRK-KEI-YM.
               05  WRK-KEI-YY          PIC 9(04).
               05  WRK-KEI-MM          PIC 9(02).
           03  TBL-KOH-TBL1            OCCURS   40.
               05  TBL-K1-KOHNUM       PIC X(03).
               05  TBL-K1-PAYKBN       PIC X(02).
               05  TBL-K1-TEKSTYMD     PIC X(08).
               05  TBL-K1-TEKEDYMD     PIC X(08).
           03  TBL-KOH-TBL2            OCCURS   40.
               05  TBL-K2-KOHNUM       PIC X(03).
               05  TBL-K2-TEKSTYMD     PIC X(08).
               05  TBL-K2-TEKEDYMD     PIC X(08).
           03  IDX-K1                  PIC 9(04).
           03  IDX-K2                  PIC 9(04).
           03  FLG-KOH                 PIC 9(01).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *    リハビリ同一算定コードテーブル
           COPY   "CMRIHACDTBL.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    COPY    "CPSK1005.INC".
      *
      *    COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    COPY    "CPSK1001.INC".
      *
      *    COPY    "CPSK1007.INC".
      *
      *    COPY    "CPSK1008.INC".
      *
      *    COPY    "CPSK1038.INC".
      *
      *    COPY    "CPSK1030.INC".
      *
      *    COPY    "CPSK1014.INC".
      *    ＣＬＡＩＭ接続情報 
      *    COPY    "CPSK9000.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *
      *    ワーク診療行為マスタ−
       01  HZN-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"   REPLACING
                                     //WKSRY-// BY //HZN-WKSRY-//.
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受付
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    収納
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    点数マスタ薬剤付加コード
        01 TENSUPLUS2-REC.
           COPY    "CPTENSUPLUS2.INC".
      *
      *    患者労災保険情報
       01  PTRSIINF-REC.
           COPY    "CPPTRSIINF.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *    公費マスタ
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *    患者特記事項
       01  PTTOKKI-REC.
           COPY    "CPPTTOKKI.INC".
      *
      *    同一患者識別テーブル
       01  PTSAME-REC.
           COPY    "CPPTSAME.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    患者個別設定テーブル
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *H28.10
      *    患者介護認定者情報テーブル
       01  PTCARE-NINTEI-REC.
           COPY    "CPPTCARE-NINTEI.INC".
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    病名編集サブ
           COPY    "CPORCSMKBYOMEI.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *****COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    診療行為入力 共通画面ＳＰＡ
           COPY    "K02SCR-SPA".
      *
      *    初期ＳＰＡ編集処理パラメタ
           COPY    "CPORCSC95.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           ORCSC95AREA
           SPA-AREA
           SPA-K01KYOUTU
           SPA-K02
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           EVALUATE    LNK-SC95-KBN
      *        初期共通ＳＰＡ編集（外来）
               WHEN    "1"
                   PERFORM 100-K01KYOUTU-SEC
      *        初期共通ＳＰＡ編集（入院）
               WHEN    "9"
                   PERFORM 900-K01KYOUTU-SEC
      *        外来のＳＰＡ編集、ワーク診療行為作成
               WHEN    "2"
               WHEN    "3"
                   PERFORM 200-GAIRAI-SPASET-SEC
      *        入院のＳＰＡ編集、ワーク診療行為作成
               WHEN    "5"
               WHEN    "8"
                   PERFORM 500-NYUIN-SPASET-SEC
      *        訂正時ワーク診療行為作成
               WHEN    "4"
                   PERFORM 300-WKSRYACT-SYORI-SEC
               WHEN    "6"
      *           未収金額編集
                   PERFORM 49011-MISYU-HEN-SEC
      *        保険変更時
               WHEN    "7"
      *            保険組合せが使用できるかのチェック
                   PERFORM 100131-HKNCOMBI-CHK-SEC
      *            中途終了分編集
                   PERFORM 200-WKSRYACT-SYORI-SEC
      *        診療科変更時
               WHEN    "K"
      *           受付マスタより、診療科、ドクターの決定
                   IF     (SPA-SYSYMD          =   SPA-SRYYMD ) AND
                          (SPA-SYORIFLG        =   ZERO       )
                       PERFORM 10014-UKETUKE-CHK-SEC
                   END-IF
      *            受診履歴情報セット
                   PERFORM 1001-SPA-JYUSIN-SEC
      *            病名編集
      *!!!         PERFORM 11012-BYOMEI-HENSYU-SEC
      *            特定疾患の有無検索
                   PERFORM 11011-TOKUTEI-SYORI-SEC
      *            中途終了分編集
                   PERFORM 200-WKSRYACT-SYORI-SEC
      *        診療科変更時の保険チェック
               WHEN    "L"
      *           受付マスタより、診療科、ドクターの決定
                   IF     (SPA-SYSYMD          =   SPA-SRYYMD ) AND
                          (SPA-SYORIFLG        =   ZERO       )
                       PERFORM 10014-UKETUKE-CHK-SEC
                   END-IF
      *            受診履歴より、診療科等を確定する
                   PERFORM 10011-JYURRK-SEC
      *            保険組合せ名称編集
                   PERFORM 10013-HKNCOMBI-SEC
      *        中途終了分追加
               WHEN    "A"
                   PERFORM 300-WKSRYACT-INSERT-SEC
      *        受診履歴ページ変更
               WHEN    "N"
      *            受診履歴一覧編集
                   PERFORM 2002-JYURRK-SET-SEC
      *        病名のみ変更
               WHEN    "B"
      *            病名編集
                   PERFORM 11012-BYOMEI-HENSYU-SEC
      *            特定疾患の有無検索
                   PERFORM 11011-TOKUTEI-SYORI-SEC
      *        保険変更時
               WHEN    "H"
      *            保険組合せチェック・編集
                   IF      SPA-HKNCOMBINUM     =   SPACE
                       PERFORM 100131-HKNCOMBI-CHK-SEC
                   ELSE
                       PERFORM 400-HKNCOMBI-CHKHEN-SEC
                   END-IF
      *ver.4.7
      *        API　患者初期情報取得
               WHEN    "@"
                   PERFORM 600-API-SPASET-SEC
      *        HAORI　訂正時ワーク診療行為作成
               WHEN    "R"
                   MOVE    "4"             TO  LNK-SC95-KBN
                   MOVE    1               TO  FLG-HAORI
                   PERFORM 300-WKSRYACT-SYORI-SEC
      *
           END-EVALUATE
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    共通ＳＰＡ編集処理
      *****************************************************************
       100-K01KYOUTU-SEC            SECTION.
      *
           INITIALIZE                  SPA-K01KYOUTU
      *    診療日
           MOVE    LNK-SC95-SRYYMDG    TO  SPA-SRYYMDW
           MOVE    LNK-SC95-SRYYMD     TO  SPA-SRYYMD
      *    入外区分
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    システム管理からの設定
           CALL    "ORCSC95SUB"        USING
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
      *!
           .
       100-K01KYOUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    共通ＳＰＡ編集処理（入院）
      *****************************************************************
       900-K01KYOUTU-SEC            SECTION.
      *
           INITIALIZE                  SPA-K01KYOUTU
      *    診療日
           MOVE    LNK-SC95-SRYYMDG    TO  SPA-SRYYMDW
           MOVE    LNK-SC95-SRYYMD     TO  SPA-SRYYMD
      *    入外区分
           MOVE    "1"                 TO  SPA-NYUGAIKBN
           CALL    "ORCSC95SUB"        USING
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
      *
      *
           .
       900-K01KYOUTU-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来患者情報ＳＰＡ編集処理（患者選択時）
      *****************************************************************
       200-GAIRAI-SPASET-SEC           SECTION.
      *
           INITIALIZE                  SPA-K02
      *    受付有無
           MOVE    ZERO                TO  SPA-UKETUKE-ARI
           IF      LNK-SC95-PG         =   "K01"  OR  "K10"
               CONTINUE
               MOVE    1                   TO  FLG-UKETUKE-ARI
           ELSE
               MOVE    ZERO                TO  FLG-UKETUKE-ARI
               MOVE    SPACE               TO  SPA-HKNCOMBINUM
               MOVE    ZERO                TO  SPA-FTNRATE
               MOVE    SPACE               TO  SPA-FTNRATEMEI
           END-IF
      *
           IF      LNK-SC95-PG         =   "K02"  OR  "K01"
               CONTINUE
           ELSE
               MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           END-IF
      *
      *    患者情報セット
           PERFORM 110-K02SYOSIN-SEC
      *    初期編集時
           IF      LNK-SC95-KBN        =   "2"
      *        受診履歴一覧編集
               MOVE    ZERO                TO  SPA-RRK-PAGE
               PERFORM 2002-JYURRK-SET-SEC
      *        最終来院日からの判定
               IF      SPA-LAST-KIKAN      NOT =   ZERO
                   PERFORM 2003-LAST-KIKAN-CHK-SEC
               END-IF
      *        公費期限切れ判定
               PERFORM 2009-KOHENDCHK-SEC
           END-IF
      *
      *    受付マスタより、診療科、ドクターの決定
           IF    ((SPA-GMN-SRYKA       =   SPACE    ) OR
                  (SPA-P02-UKEID   NOT =   ZERO     )) AND
                  (SPA-SYSYMD          =   SPA-SRYYMD ) AND
                  (SPA-SYORIFLG        =   ZERO       )
             AND  (LNK-SC95-PG    NOT  =   "K01"    )
             AND  (FLG-UKETUKE-ARI     =   ZERO     )
               PERFORM 10014-UKETUKE-CHK-SEC
           END-IF
      *
      *    中途終了からの場合、ドクターを受付から決定する
           IF     (LNK-SC95-PG         =   "K10" )  AND
                  (SPA-DRCD            =   SPACE )
               MOVE    ZERO                TO  FLG-UKETUKE-ARI
               PERFORM 10014-UKETUKE-CHK-SEC
           END-IF
      *    受診履歴情報セット
           PERFORM 1001-SPA-JYUSIN-SEC
      *    病名編集
           PERFORM 11012-BYOMEI-HENSYU-SEC
      *    特定疾患の有無検索
           PERFORM 11011-TOKUTEI-SYORI-SEC
      *
      *    中途終了分処理
           PERFORM 200-WKSRYACT-SYORI-SEC
      *    中途終了データがある時、前回保険チェックはしない
           IF      FLG-WKSYORI         =   1
               MOVE    ZERO                TO  SPA-HKNCHK-FLG
           END-IF
      *
      *    前回患者保存
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
           MOVE    SPA-PTNUM           TO  SPA-LAST-PTNUM
           .
       200-GAIRAI-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者受診履歴一覧編集処理
      *****************************************************************
       2002-JYURRK-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
           INITIALIZE                  HZN-JYURRK-REC
      *    受診歴を検索する  医療機関、患者ＩＤ
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  IDX
      *    開始位置指定
           IF      SPA-RRK-PAGE        =   ZERO
               MOVE    1               TO  SPA-RRK-PAGE
           END-IF
           COMPUTE IDX-SP              =  ((SPA-RRK-PAGE  -   1  )
                                       *   150  )
                                       +   1
           MOVE    ZERO                TO  IDX-S1
           MOVE    ZERO                TO  IDX-S2
           MOVE    ZERO                TO  SPA-RRK-NEXT
      *
           PERFORM
               UNTIL   (FLG-JYURRK         =   1   )
                  OR   (SPA-RRK-NEXT       =   1   )
      *D          OR   (SPA-GMN-MAX1       >=  150 )
      *        最終受診日と科をが同じ時、保険組合せを決定する
               IF      (JYURRK-SRYYMD      =   SPA-LSTYMD )  AND
                       (JYURRK-SRYKA       =   SPA-LSTSRYKA) AND
                       (JYURRK-SRYKA       =   SPA-SRYKA  ) AND
                       (SPA-HKNCOMBINUM    =   SPACE      )  AND
                       (JYURRK-HKNCOMBINUM NOT =   "9999"  )
                 AND   (SPA-RRK-PAGE       =   1          )
                   MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
               END-IF
      *TEST
      **       診療日、連番で１行とする
      *        MOVE    ZERO            TO  IDX-S
      *        PERFORM VARYING     IDX2    FROM    1   BY  1
      *                UNTIL       IDX2    >   SPA-GMN-MAX1
      *            IF     (JYURRK-RENNUM       =   SPA-NAI-RENNUM
      *                                                (IDX2))  AND
      *                   (JYURRK-DOUJI-RENNUM =   SPA-NAI-DOUJI-RENNUM
      *                                                (IDX2))  AND
      *                   (JYURRK-SRYYMD       =   SPA-NAI-RRKYMD
      *                                                (IDX2))
      *              AND  (JYURRK-SRYKA        =   SPA-NAI-RRK-SRYKA
      *                                                (IDX2))
      *                MOVE    IDX2            TO  IDX-S
      *                MOVE    SPA-GMN-MAX1    TO  IDX2
      *            END-IF
      **       END-PERFORM
      *TEST
               MOVE    ZERO                TO  IDX-S
               IF      JYURRK-EDANUM       =   1
                   ADD     1                   TO  IDX-S2
                   IF      IDX-S1              =   9999
                       MOVE    ZERO            TO  IDX-S1
                   END-IF
                   ADD     1                   TO  IDX-S1
               ELSE
                   IF     (JYURRK-SRYYMD       =   HZN-JYURRK-SRYYMD)
                     AND  (JYURRK-SRYKA        =   HZN-JYURRK-SRYKA )
                     AND  (JYURRK-HKNCOMBINUM  =
                                               HZN-JYURRK-HKNCOMBINUM )
                     AND  (JYURRK-RENNUM       =   HZN-JYURRK-RENNUM)
                     AND  (JYURRK-DOUJI-RENNUM =
                                               HZN-JYURRK-DOUJI-RENNUM)
                       MOVE    IDX-S1              TO  IDX-S
                   ELSE
                       ADD     1                   TO  IDX-S2
                       IF      IDX-S1              =   9999
                           MOVE    ZERO            TO  IDX-S1
                       END-IF
                       ADD     1                   TO  IDX-S1
                       DISPLAY "ORCSC95 JYURRK EDANUM ERR??:"
                                                    JYURRK-KEY
                   END-IF
               END-IF
               IF      IDX-SP          <=   IDX-S2
      *            編集対象
                   IF      IDX-S           =   ZERO
                       ADD     1               TO  IDX
                       IF      IDX             >   150
                           MOVE    1           TO  SPA-RRK-NEXT
                       ELSE
                           MOVE    IDX         TO  SPA-GMN-MAX1
                       END-IF
                   END-IF
                   IF      SPA-RRK-NEXT        =   ZERO
      *                明細項目退避
                       PERFORM 20021-JYURRK-HEN-SEC
                   END-IF
               END-IF
      *
      **       明細項目退避
      **       PERFORM 20021-JYURRK-HEN-SEC
      *
               MOVE    JYURRK-REC          TO  HZN-JYURRK-REC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    診察日の再編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
               MOVE    ZERO            TO  FLG-HEN
               PERFORM VARYING     IDX2    FROM    IDX BY  1
                       UNTIL       IDX2    >   SPA-GMN-MAX1
                   IF     (IDX2            NOT =   IDX )  AND
                          (SPA-NAI-RRKYMD  (IDX)
                                               =   SPA-NAI-RRKYMD 
                                                          (IDX2)) AND
                          (SPA-NAI-RRK-SRYKA(IDX)
                                               =   SPA-NAI-RRK-SRYKA
                                                          (IDX2))
                       MOVE    1                   TO  FLG-HEN
                       MOVE    SPA-GMN-MAX1        TO  IDX2
                   ELSE
                       IF     (SPA-NAI-RRKYMD(IDX)
                                           NOT =   SPA-NAI-RRKYMD
                                                       (IDX2))
                           MOVE    SPA-GMN-MAX1    TO  IDX2
                       END-IF
                   END-IF
               END-PERFORM
               IF     (SPA-NAI-RENNUM (IDX)        >   1 ) OR
                      (SPA-NAI-DOUJI-RENNUM (IDX)  >  ZERO )
                   MOVE    1                   TO  FLG-HEN
               END-IF
               IF      FLG-HEN             =   1
                   MOVE    "("                 TO  SPA-GMN-RRKYMD (IDX)
                                                               (10:1)
                   MOVE    SPA-NAI-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (11:1)
                   MOVE    ")"                 TO  SPA-GMN-RRKYMD (IDX)
                                                              (12:1)
                   IF      SPA-NAI-DOUJI-RENNUM (IDX)  NOT =   ZERO
                       MOVE    SPA-NAI-DOUJI-RENNUM (IDX) 
                                               TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:1)
                   END-IF
               END-IF
      *        複数診療科
               IF      SPA-NAI-RRK-FUKU-KBN (IDX)  =   "1"  OR  "2"
                   MOVE    "F"                 TO  SPA-GMN-RRKYMD (IDX)
                                                              (13:1)
               END-IF
      *
               MOVE    SPA-NAI-RRK-HKNCOMBI(IDX)
                                             TO  SPA-GMN-RRKHKNCOMBI
                                                                  (IDX)
               IF      SPA-NAI-RRK-HKNKBN  (IDX)  =   "1"
                                                  OR  "5"
                   MOVE    "労"              TO  SPA-GMN-RRKHKNCOMBI
                                                                  (IDX)
               END-IF
               IF      SPA-NAI-RRK-HKNKBN  (IDX)  =   "2"
                                                  OR  "4"
                   MOVE    "自"              TO  SPA-GMN-RRKHKNCOMBI
                                                                  (IDX)
               END-IF
      *        科名
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY         >   SPA-SRYKA-MAX
                   IF      SPA-NAI-RRK-SRYKA (IDX)
                                           =   SPA-GMN-SRYKAL(IDY)
                       MOVE    SPA-GMN-SRYKAMEIL(IDY)(1:2)
                                               TO  SPA-GMN-RRKSRYKA(IDX)
                       MOVE    SPA-SRYKA-MAX   TO  IDY
                   END-IF
               END-PERFORM
      *	H25.3  科コードなし
               IF      SPA-GMN-RRKSRYKA(IDX)   =   SPACE
                   MOVE    SPA-NAI-RRK-SRYKA (IDX)
                                               TO  SPA-GMN-RRKSRYKA(IDX)
               END-IF
      *!!!!!!!
           END-PERFORM
      *
           .
       2002-JYURRK-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       20021-JYURRK-HEN-SEC             SECTION.
      *
      *D   MOVE    IDX                 TO  SPA-GMN-NO    (IDX)
           MOVE    IDX-S1              TO  SPA-GMN-NO    (IDX)
      *    連番
           MOVE    JYURRK-RENNUM       TO  SPA-NAI-RENNUM(IDX)
      *    連番
           MOVE    JYURRK-DOUJI-RENNUM TO  SPA-NAI-DOUJI-RENNUM(IDX)
      *    伝票番号
           MOVE    JYURRK-DENPNUM      TO  SPA-NAI-RRK-DENPNUM(IDX)
      *    診察日
           MOVE    JYURRK-SRYYMD       TO  SPA-NAI-RRKYMD(IDX)
           MOVE    JYURRK-SRYYMD       TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDX)
      *    保険区分
           MOVE    JYURRK-HKNKBN       TO  SPA-NAI-RRK-HKNKBN (IDX)
      *    保険組合せ
           MOVE    JYURRK-HKNCOMBINUM  TO  SPA-NAI-RRK-HKNCOMBI (IDX)
      *    ドクター
           MOVE    JYURRK-DRCD         TO  SPA-NAI-RRK-DRCD  (IDX)
      *    診療科
           MOVE    JYURRK-SRYKA        TO  SPA-NAI-RRK-SRYKA  (IDX)
      *    診察を算定しているか判定
           IF      JYURRK-SRYKBN-G NOT =   SPACE
               MOVE    "1"                 TO  SPA-NAI-RRK-SINSATU (IDX)
           END-IF
      *    複数診療科・保険
           MOVE    JYURRK-GRP-DENPNUM TO  SPA-NAI-GRP-DENPNUM (IDX)
           MOVE    JYURRK-GRP-RENNUM  TO  SPA-NAI-GRP-RENNUM (IDX)
           MOVE    JYURRK-SYU-UPDKBN  TO  SPA-NAI-SYU-UPDKBN (IDX)
      *
           MOVE    IDX                 TO  SPA-GMN-MAX1
      *
      *****外来で診療日の時のみ
      *    IF     (SPA-NYUGAIKBN       =   "2" )
      *      AND  (SPA-SRYYMD          =   JYURRK-SRYYMD)
      *        複数科診療の判定をする
      *        PERFORM 200211-FUKU-HEN-SEC
      *        IF      FLG-SYUNOU          =   ZERO
      *            複数診療科
      *            IF      SYU-FUKU-DENPNUM    >   ZERO
      *                MOVE    SYU-FUKU-KBN    TO  SPA-NAI-RRK-FUKU-KBN
      *                                                           (IDX)
      *            END-IF
      *        END-IF
      *****END-IF
      *
           .
       20021-JYURRK-HEN-EXT.
           EXIT.
      *DEL
      *****************************************************************
      *    複数科診療の判定編集処理
      *****************************************************************
       200211-FUKU-HEN-SEC              SECTION.
      *
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
      *****MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
           MOVE    SPA-NAI-RRK-DENPNUM(IDX)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       200211-FUKU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    最終来院日から初診の判定処理
      *****************************************************************
       2003-LAST-KIKAN-CHK-SEC              SECTION.
      *
           IF     (SPA-LSTYMD          =   SPACE )  OR
                  (SPA-SYOSIN          =   1     )
               GO      TO      2003-LAST-KIKAN-CHK-EXT
           END-IF
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-LSTYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    SPA-LAST-KIKAN      TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      LNK-DAY6-KEISAN     <=  SPA-SRYYMD
               MOVE    1               TO  SPA-SYOSIN
           END-IF
           .
       2003-LAST-KIKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    公費期限切れ判定処理
      *****************************************************************
       2009-KOHENDCHK-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-KOHMSG
           INITIALIZE                      WRK-KOH-AREA
      *
      *    システム日付＋１ヵ月
           MOVE    SPA-SYSYMD(1:6)     TO  WRK-KEI-YM (1:6)
           IF      WRK-KEI-MM          =   12
               COMPUTE WRK-KEI-YY      =   WRK-KEI-YY   +   1
               MOVE    1                   TO  WRK-KEI-MM
           ELSE
               COMPUTE WRK-KEI-MM      =   WRK-KEI-MM   +   1
           END-IF
      *
           INITIALIZE                  PTKOHINF-REC
           MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
           MOVE    SPA-PTID            TO  PTKOH-PTID
           MOVE    SPA-SYSYMD          TO  PTKOH-TEKEDYMD
      *
           MOVE    PTKOHINF-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-PTKOHINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
           PERFORM     UNTIL     FLG-PTKOHINF  =   1
      *        期限切れ対象
               IF     (PTKOH-TEKEDYMD(1:6)     =   WRK-KEI-YM
                                               OR  SPA-SYSYMD(1:6))
                  AND (PTKOH-TEKEDCHKKBN       =   "1"      )
                   ADD     1               TO  IDX-K1
                   MOVE    PTKOH-KOHNUM    TO  TBL-K1-KOHNUM (IDX-K1)
                   MOVE    PTKOH-PAYKBN    TO  TBL-K1-PAYKBN (IDX-K1)
                   MOVE    PTKOH-TEKSTYMD  TO  TBL-K1-TEKSTYMD
                                                              (IDX-K1)
                   MOVE    PTKOH-TEKEDYMD  TO  TBL-K1-TEKEDYMD
                                                              (IDX-K1)
               ELSE
                   ADD     1               TO  IDX-K2
                   MOVE    PTKOH-KOHNUM    TO  TBL-K2-KOHNUM (IDX-K2)
                   MOVE    PTKOH-TEKSTYMD  TO  TBL-K2-TEKSTYMD
                                                              (IDX-K2)
                   MOVE    PTKOH-TEKEDYMD  TO  TBL-K2-TEKEDYMD
                                                              (IDX-K2)
               END-IF
      *
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-PTKOHINF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           PERFORM VARYING     IDX-K1  FROM    1   BY  1
                   UNTIL      (IDX-K1  >   40  )  OR
                              (TBL-K1-KOHNUM (IDX-K1)  =   SPACE)
                          OR  (SPA-GMN-KOHMSG  NOT =   SPACE )
               MOVE    ZERO            TO  FLG-KOH
               PERFORM VARYING     IDX-K2  FROM    1   BY  1
                       UNTIL      (IDX-K2  >   40  )  OR
                                  (TBL-K2-KOHNUM (IDX-K2)  =   SPACE) OR
                                  (FLG-KOH     =   1  )
                   IF     (TBL-K1-KOHNUM (IDX-K1)
                                           =   TBL-K2-KOHNUM (IDX-K2))
                      AND (TBL-K1-TEKEDYMD (IDX-K1)
                                           <   TBL-K2-TEKSTYMD (IDX-K2))
                       MOVE    1           TO  FLG-KOH
                   END-IF
               END-PERFORM
               IF      FLG-KOH             =   ZERO
                   PERFORM 20091-KOHNUMMEI-SEC
                   MOVE    TBL-K1-TEKEDYMD (IDX-K1)    TO  WRK-SYMD
                   PERFORM 800-SEIWA-HEN-SEC
                   STRING "公費："
                          HKN-TANSEIDONAME     DELIMITED  BY  SPACE
                          "　有効期限："
                           WRK-HENYMDG         DELIMITED  BY  SIZE
                                           INTO  SPA-GMN-KOHMSG
                   END-STRING
               END-IF
           END-PERFORM
           .
       2009-KOHENDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費名称取得処理
      *****************************************************************
       20091-KOHNUMMEI-SEC              SECTION.
      *
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
           MOVE    TBL-K1-KOHNUM  (IDX-K1)  TO  HKN-HKNNUM
           MOVE    TBL-K1-PAYKBN  (IDX-K1)  TO  HKN-PAYKBN
           MOVE    TBL-K1-TEKSTYMD (IDX-K1) TO  HKN-TEKSTYMD
           MOVE    TBL-K1-TEKEDYMD (IDX-K1) TO  HKN-TEKEDYMD
      *    保険番号マスタ検索処理
           PERFORM 900-HKNNUM-KEY-READ-SEC
           .
       20091-KOHNUMMEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者の受診履歴編集処理（診療日変更時）
      *****************************************************************
       1001-SPA-JYUSIN-SEC              SECTION.
      *
      *    受診履歴より、診療科等を確定する
           PERFORM 10011-JYURRK-SEC
      *
      *    診療科、ドクター名称編集
           PERFORM 10012-SYSKAN-SEC
      *
      *    保険組合せ名称編集
           PERFORM 10013-HKNCOMBI-SEC
      *
      *    診療日の受診有無
           MOVE    ZERO                TO  SPA-NAI-GRPKBN2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )
                           OR (SPA-NAI-GRPHOSPNUM (IDX)    =   ZERO)
               PERFORM 100151-PTSAME-JYURRK-SEC
           END-PERFORM
      *
           .
       1001-SPA-JYUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスタより、診療科等を確定する
      *****************************************************************
       10014-UKETUKE-CHK-SEC        SECTION.
      *
      *    受付マスタ検索
           MOVE    SPACE               TO  UKETUKE-REC
           INITIALIZE                  UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-UKETUKE-READ-SEC
           ELSE
               MOVE    SPACE               TO  UKETUKE-REC
               INITIALIZE                  UKETUKE-REC
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           PERFORM UNTIL  (FLG-UKETUKE     =   1  )
      **********   OR     (SPA-DRCD    NOT =   SPACE )
      *        会計のまだの最初の受付を対象とする
               IF      SPA-P02-UKEID   NOT =   ZERO
                   IF     SPA-P02-UKEID        =   UKE-UKEID
                       MOVE    1               TO  FLG-UKETUKE-ARI
                   END-IF
               ELSE
                   IF     (UKE-KAIKEITIME      =   ZERO )  AND
                          (UKE-UKEID       NOT =   ZERO )
                       MOVE    1               TO  FLG-UKETUKE-ARI
                   END-IF
               END-IF
      *        診療科選択時
               IF      FLG-UKETUKE-ARI     =   1
                   IF     (SPA-GMN-SRYKA   NOT =   SPACE  )  AND
                          (SPA-GMN-SRYKA   NOT =   UKE-SRYKA)
                       MOVE    ZERO            TO  FLG-UKETUKE-ARI
                   END-IF
               END-IF
               IF      FLG-UKETUKE-ARI     =   1
                 IF      LNK-SC95-PG         =   "K10"
      *            中途終了からはドクターのみ
                   IF       SPA-GMN-SRYKA       =   UKE-SRYKA
                       MOVE    UKE-DRCD            TO  SPA-DRCD
                       MOVE    1                   TO  FLG-UKETUKE
                   END-IF
                 ELSE
      *              受付あり
                     MOVE    1                    TO  SPA-UKETUKE-ARI
                  IF     (SPA-GMN-SRYKA   NOT =   SPACE  )  AND
                          (SPA-GMN-SRYKA       =   UKE-SRYKA )
                       MOVE    UKE-SRYKA           TO  SPA-SRYKA
                       MOVE    UKE-DRCD            TO  SPA-DRCD
      *                保険組合せ追加
                       IF      UKE-HKNCOMBI        =   ZERO
                           MOVE    SPACE           TO  SPA-HKNCOMBINUM
                       ELSE
                           MOVE    UKE-HKNCOMBI    TO  SPA-HKNCOMBINUM
                       END-IF
                       MOVE    1                   TO  FLG-UKETUKE
                   ELSE
                       MOVE    UKE-SRYKA           TO  SPA-SRYKA
                       MOVE    UKE-DRCD            TO  SPA-DRCD
      *                保険組合せ追加
                       IF      UKE-HKNCOMBI        =   ZERO
                           MOVE    SPACE           TO  SPA-HKNCOMBINUM
                       ELSE
                           MOVE    UKE-HKNCOMBI    TO  SPA-HKNCOMBINUM
                       END-IF
                       MOVE    1                   TO  FLG-UKETUKE
                   END-IF
                END-IF
               END-IF
               IF      FLG-UKETUKE         =   ZERO
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-UKETUKE-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       10014-UKETUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診歴より、診療科等を確定する
      *****************************************************************
       10011-JYURRK-SEC        SECTION.
      *
      *    受診歴を検索する 医療機関、患者ＩＤの最終レコードを対象
      *
      *    MOVE    SPACE               TO  JYURRK-REC
      *    INITIALIZE                  JYURRK-REC
      *    MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
      *    MOVE    SPA-PTID            TO  JYURRK-PTID
      *    MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *    MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *    MOVE    SPACE               TO  JYURRK-SRYKA
      *
      *    MOVE    "JYURRK-KEY10"       TO  WRK-PATH
      *
      *    MOVE    JYURRK-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    WRK-PATH            TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    WRK-PATH            TO  ORC-DBPATH
      *        PERFORM 970-JYURRK-READ-SEC
      *    ELSE
      *        MOVE    SPACE               TO  JYURRK-REC
      *        INITIALIZE                  JYURRK-REC
      *        MOVE    1                   TO  FLG-JYURRK
      *****END-IF
      *
           INITIALIZE                  SPA-FUKU-AREA
           INITIALIZE                  WRK-FUKU-AREA
           MOVE    SPACE               TO  HZN-JYURRK-REC
           MOVE    ZERO                TO  IDX-JYU
           MOVE    SPACE               TO  WRK-TAKA-SRYKA-AREA
           MOVE    ZERO                TO  IDX-SRY
           INITIALIZE                      SPA-TA-HKNCOMBI-AREA
           MOVE    ZERO                TO  IDX-HKN
      **** PERFORM
      *        UNTIL    FLG-JYURRK         =   1
      *
      *        最後の受診履歴を対象とする（診療日以前のもの）
      *        IF    ((SPA-SRYKA       NOT =   SPACE  )  AND
      *               (SPA-SRYKA           =   JYURRK-SRYKA ) )  OR
      *              ( SPA-SRYKA           =   SPACE          )
      *            IF      SPA-SRYYMD          >=   JYURRK-SRYYMD
      *                訂正の時、選択した連番 
      *                IF     (SPA-SYORIFLG        =   ZERO    )   OR
      *                      ((SPA-SYORIFLG        =   1          )
      *                   AND (SPA-RENNUM          =   JYURRK-RENNUM)
      *                   AND (SPA-DOUJI-RENNUM    =
      *                                            JYURRK-DOUJI-RENNUM))
      *                    MOVE    JYURRK-REC      TO  WRK-JYURRK-REC
      *                END-IF
      *            END-IF
      *
      *            継続のチェック・複数診療科チェック
      *            IF     (SPA-SRYYMD          =   JYURRK-SRYYMD)
      *            AND    (JYURRK-DOUJI-RENNUM NOT =   ZERO     )
      *                PERFORM 100111-CONTKBN-HANTEI-SEC
      *            END-IF
      *        END-IF
      *
      *        診療日の受診内容を保存
      *        IF      SPA-SRYYMD          =   JYURRK-SRYYMD
      *            PERFORM 100111-DOUJI-JYURRK-SEC
      *        END-IF
      *
      *        MOVE    WRK-PATH            TO  ORC-DBPATH
      *        PERFORM 970-JYURRK-READ-SEC
      *    END-PERFORM
      *
      *    MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
      *    MOVE    WRK-PATH            TO  ORC-DBPATH
      *    CALL    "MCPDBSUB"          USING
      *                                MCPAREA
      *********************************MCPDATA-REC
      *
      *    テーブルより検索
           MOVE    ZERO                TO  IDX-J1
           MOVE    ZERO                TO  IDX-J2
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
      *        診療日より後の履歴は対象外
               IF      SPA-SRYYMD      >=  SPA-NAI-RRKYMD (IDX)
      *        包括保険は対象外
                 AND   (SPA-NAI-RRK-HKNCOMBI(IDX)  NOT =   "9999")
      *
      *            最後の受診履歴を対象とする（診療日以前のもの）
                   IF    ( SPA-SRYKA           =   SPACE  )  OR
                         ((SPA-SRYKA       NOT =   SPACE  )  AND
                          (SPA-SRYKA           =   SPA-NAI-RRK-SRYKA
                                                             (IDX)))
                       IF      SPA-SYORIFLG        =   ZERO
                           IF       IDX-J1         =   ZERO
                               MOVE     IDX            TO  IDX-J1
                           ELSE
                               IF      (SPA-NAI-RRK-SRYKA(IDX-J1)
                                                   =   SPA-NAI-RRK-SRYKA
                                                                 (IDX))
                               AND     (SPA-NAI-RRKYMD(IDX-J1)
                                                   =   SPA-NAI-RRKYMD
                                                                 (IDX))
                                   MOVE    IDX         TO  IDX-J1
                               END-IF
                           END-IF
                       ELSE
      *                訂正の時、選択した連番 
                           IF     (SPA-RENNUM      =   SPA-NAI-RENNUM
                                                               (IDX))
                              AND (SPA-DOUJI-RENNUM    =
                                               SPA-NAI-DOUJI-RENNUM
                                                               (IDX))
                              AND (SPA-SRYYMD      =   SPA-NAI-RRKYMD
                                                               (IDX))
                               MOVE     IDX            TO  IDX-J1
                           END-IF
                       END-IF
                   END-IF
      *
      *            継続のチェック・複数診療科チェック
                   IF     (SPA-SRYYMD          =   SPA-NAI-RRKYMD (IDX))
                   AND    (SPA-NAI-DOUJI-RENNUM(IDX)
                                           NOT =   ZERO     )
                       PERFORM 100111-CONTKBN-HANTEI-SEC
                   END-IF
               END-IF
      *
      *        診療日の受診内容を保存
               IF      SPA-SRYYMD          =   SPA-NAI-RRKYMD (IDX)
                AND   (SPA-NAI-RRK-HKNCOMBI(IDX)   NOT =   "9999")
                   PERFORM 100111-DOUJI-JYURRK-SEC
               END-IF
      *
      *        終了判定
               IF     (SPA-SRYYMD          >   SPA-NAI-RRKYMD (IDX)) AND
                      (IDX-J1          NOT =   ZERO   )
                   MOVE    SPA-GMN-MAX1        TO  IDX
               END-IF
      *
           END-PERFORM
      *
      *    同日再診日のセット
           IF      IDX-J2              =   ZERO
               MOVE    SPACE               TO  SPA-RRK-LSTYMD
               MOVE    SPACE               TO  SPA-LSTSRYKA
               MOVE    SPACE               TO  SPA-RRK-LSTSRYKA
           ELSE
               MOVE    SPA-NAI-RRKYMD (IDX-J2)
                                           TO  SPA-RRK-LSTYMD
               MOVE    SPA-NAI-RRK-SRYKA(IDX-J2)
                                           TO  SPA-LSTSRYKA
               MOVE    SPA-NAI-RRK-SRYKA(IDX-J2)
                                           TO  SPA-RRK-LSTSRYKA
           END-IF
      *
      *    診療科
           IF     (SPA-SRYKA           =   SPACE )  AND
                  (IDX-J1              >   ZERO  )
               MOVE    SPA-NAI-RRK-SRYKA(IDX-J1)   TO  SPA-SRYKA
           END-IF
      *    同日診療科保存
           MOVE    SPACE               TO  SPA-TAKA-SRYKA-R
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10   )  OR
                              (WRK-TAKA-SRYKA (IDX)    =   SPACE )
               IF      WRK-TAKA-SRYKA (IDX)    =   SPA-SRYKA
                   CONTINUE
               ELSE
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL       IDX2    >   10
                       IF     (SPA-TAKA-SRYKA(IDX2)    =   SPACE  )  OR
                              (SPA-TAKA-SRYKA(IDX2)    =
                                                   WRK-TAKA-SRYKA(IDX))
                           MOVE    WRK-TAKA-SRYKA (IDX)    TO
                                                   SPA-TAKA-SRYKA (IDX2)
                           MOVE    10              TO  IDX2
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *****受診歴なし
      *    IF      JYURRK-REC          =   SPACE
      *        複数科まとめ編集
      *        PERFORM 100112-FUKU-HEN-SEC
      *        GO      TO      10011-JYURRK-EXT
      **** END-IF
      *!   受診歴なし
      *    IF      IDX-J1              =   ZERO
      *        複数科まとめ編集
      *        PERFORM 100112-FUKU-HEN-SEC
      *        GO      TO      10011-JYURRK-EXT
      *!   END-IF
      *
      *    ドクター
           IF     (SPA-DRCD            =   SPACE)
             AND  (IDX-J1              >   ZERO )
             AND  (FLG-UKETUKE-ARI     =   ZERO )
               MOVE    SPA-NAI-RRK-DRCD(IDX-J1)    TO  SPA-DRCD
           END-IF
      *    保険組合せ（中途終了分選択時は選択済み）
           IF     (LNK-SC95-KBN        =   "8"  )  OR
      ************(SPA-SYORIFLG        =   1    )  OR
                  (SPA-HKNCOMBINUM NOT =   SPACE)
             OR   (IDX-J1              =   ZERO )
               CONTINUE
           ELSE
               MOVE    SPA-NAI-RRK-HKNCOMBI(IDX-J1)
                                           TO  SPA-HKNCOMBINUM
           END-IF
      *
      *    最終受診履歴
           IF      IDX-J1              >   ZERO
               MOVE    SPA-NAI-RENNUM (IDX-J1) TO  LNK-SC95-LAST-RENNUM
           END-IF
      *    複数科まとめ編集
      *!   PERFORM 100112-FUKU-HEN-SEC
           .
       10011-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ編集処理
      *****************************************************************
       100112-FUKU-HEN-SEC        SECTION.
      *    複数科まとめ編集
            IF      SPA-SYORIFLG        =   ZERO
               PERFORM VARYING     IDX-F   FROM   1   BY  1
                    UNTIL     (IDX-F   >   10 )   OR
                              (WRK-FUKU-SRYKA (IDX-F) =  SPACE )
                   IF     (WRK-FUKU-KBN (IDX-F)    =   "1"  )  AND
                          (SPA-SRYKA           NOT =   SPACE    )  AND
                          (WRK-FUKU-SRYKA(IDX-F)   NOT =   SPA-SRYKA)
                       IF      WRK-FUKU-HKNCOMBI(IDX-F)
                                                   =   SPA-HKNCOMBINUM
                           MOVE    WRK-FUKU-KBN (IDX-F)
                                                   TO  SPA-FUKU-SYORI
                       ELSE
                           MOVE    WRK-FUKU-HKNCOMBI(IDX-F)
                                                   TO  SPA-FUKU-HKNCOMBI
                           MOVE    WRK-FUKU-KBN (IDX-F)
                                                   TO  SPA-FUKU-MATKBN
                       END-IF
                   END-IF
                   IF     (WRK-FUKU-KBN (IDX-F)    =   "1"  )  AND
                          (WRK-FUKU-SRYKA(IDX-F)   =   SPA-SRYKA)  AND
                          (SPA-FUKU-SYORI          =   SPACE    )
                       MOVE    "F"                 TO  SPA-FUKU-SYORI
                   END-IF
               END-PERFORM
           END-IF
      *
           .
       100112-FUKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日の受診内容を保存処理
      *****************************************************************
       100111-DOUJI-JYURRK-SEC        SECTION.
      *
      *    訂正の時、訂正分以前のもの
           MOVE    ZERO                TO  FLG-OK
           IF      (SPA-SYORIFLG       =   1       )  AND
                   (SPA-SRYKA          =   SPA-NAI-RRK-SRYKA(IDX) ) AND
                   ((SPA-RENNUM        =   SPA-NAI-RENNUM (IDX)  AND
                     SPA-DOUJI-RENNUM  =   SPA-NAI-DOUJI-RENNUM (IDX))
                OR  (SPA-RENNUM        <   SPA-NAI-RENNUM (IDX) ))
               CONTINUE
           ELSE
      *        同時でなく、診察がない時対象外とする
               IF     (SPA-NAI-DOUJI-RENNUM (IDX)  =   ZERO  )  AND
                      (SPA-NAI-RRK-SINSATU (IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-OK
               END-IF
           END-IF
      *
           IF      FLG-OK              =   1
      *        同日受診の他科編集
               IF      SPA-SRYKA           =   SPA-NAI-RRK-SRYKA(IDX)
                   MOVE    IDX                 TO  IDX-J2
               ELSE
                   IF      IDX-J2              =   ZERO
                       MOVE    IDX                 TO  IDX-J2
                   END-IF
      *            労災の時、同じ保険であること
                   IF     ((SPA-NAI-RRK-HKNKBN (IDX-J2)
                                               =   "1"  OR "2"
                                                        OR "4"
                                                        OR "5")  AND
                           (SPA-NAI-RRK-HKNCOMBI (IDX-J2)
                                               =   SPA-NAI-RRK-HKNCOMBI
                                                             (IDX) ))
                     OR   ((SPA-NAI-RRK-HKNKBN (IDX-J2)
                                               =   ZERO
                                               OR  "6"
      *H26.4  第三者行為追加
                                               OR  "7"  ) AND
                           (SPA-NAI-RRK-HKNKBN (IDX)
                                               =   ZERO
                                               OR  "6"
                                               OR  "7" ))
                       ADD     1               TO  IDX-SRY
                       IF      IDX-SRY         <=  10
                           MOVE    SPA-NAI-RRK-SRYKA(IDX)
                                               TO  WRK-TAKA-SRYKA
                                                              (IDX-SRY)
                       END-IF
                   END-IF
               END-IF
      *        保険組合せ編集
               PERFORM VARYING     IDX-HKN     FROM    1   BY  1
                       UNTIL       IDX-HKN     >   10
                   IF     SPA-TA-HKNCOMBI (IDX-HKN)    =   ZERO
                       MOVE    SPA-NAI-RRK-HKNCOMBI (IDX)
                                               TO  SPA-TA-HKNCOMBI
                                                              (IDX-HKN)
                   END-IF
                   IF      SPA-TA-HKNCOMBI (IDX-HKN)
                                               =   SPA-NAI-RRK-HKNCOMBI
                                                                  (IDX)
                       MOVE    10              TO  IDX-HKN
                   END-IF
               END-PERFORM
      *
           END-IF
      *
      *    複数科診療の判定をする
           IF      SPA-SYORIFLG        =   1
               IF     (SPA-SRYKA           =   SPA-NAI-RRK-SRYKA(IDX))
                 AND  (SPA-RENNUM          =   SPA-NAI-RENNUM   (IDX))
                 AND  (SPA-DOUJI-RENNUM    =   SPA-NAI-DOUJI-RENNUM
                                                                (IDX))
                 AND  (SPA-NAI-GRP-DENPNUM(IDX)    =   ZERO  )
      *        複数科診療の判定をする
                   PERFORM 200211-FUKU-HEN-SEC
                   IF      FLG-SYUNOU          =   ZERO
      *            複数診療科
                       MOVE    SYU-FUKU-KBN    TO  SPA-FUKU-SYORI
                   END-IF
               END-IF
           END-IF
           .
       100111-DOUJI-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    継続区分判定処理
      *****************************************************************
       100111-CONTKBN-HANTEI-SEC        SECTION.
      *
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
      **** MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
           MOVE    SPA-NAI-RRK-DENPNUM(IDX)
                                       TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (FLG-SYUNOU          =   ZERO  )  AND
                  (SPA-NAI-DOUJI-RENNUM(IDX)
                                   NOT =   ZERO  )
      *        伝票状態区分
               IF      SYU-DENPJTIKBN      =   "7"
      *            同時診療伝票番号
                   MOVE    SYU-DOUJI-DENPNUM  TO
                                              LNK-SC95-DOUJI-DENPNUM
      *            受診連番
                   MOVE    SPA-NAI-DOUJI-RENNUM(IDX)
                                              TO  LNK-SC95-DOUJI-RENNUM
      *
                   ADD     1                  TO  IDX-JYU
                   IF      IDX-JYU            <=  3
                       MOVE    SPA-NAI-RRK-HKNCOMBI(IDX)
                                              TO  LNK-SC95-DOUJI-HKNNUM
                                                             (IDX-JYU)
                   END-IF
               END-IF
           END-IF
      *
           .
       100111-CONTKBN-HANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理より編集処理
      *****************************************************************
       10012-SYSKAN-SEC        SECTION.
      *
           MOVE    SPACE               TO  SPA-SRYKAMEI
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-SRYKA-MAX
               IF      SPA-SRYKA       =   SPA-GMN-SRYKAL   (IDX)
                   MOVE    SPA-GMN-SRYKAMEIL(IDX)  TO  SPA-SRYKAMEI
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *    前回の診療科がなかった時、最初の診療科
           IF      SPA-SRYKAMEI        =   SPACE
               MOVE    SPA-GMN-SRYKAL(01)      TO  SPA-SRYKA
               MOVE    SPA-GMN-SRYKAMEIL(01)   TO  SPA-SRYKAMEI
           END-IF
      *
      *    訂正か受付ありの場合
           IF     (LNK-SC95-KBN        =   "4"  ) OR
                  (FLG-UKETUKE-ARI     =   1    )
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
               MOVE    SPA-DRCD(2:4)       TO  ORCSDR-IN-DRCD
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               IF      SPA-DRCD        NOT =   SPACE
                   MOVE    ORCSDR-IN-DRCD-G    TO  SPA-DRCD-NAME
               ELSE
                   MOVE    SPACE               TO  SPA-DRCD-NAME
               END-IF
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           ELSE
      *
      *    ドクター初期表示
           IF     (SPA-SRYYMD          =   SPA-SYSYMD )
             AND  (SPA-NYUGAIKBN       =   "2"        )
      *        当日有効なドクターへ
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SYSYMD          TO  ORCSDR-SYSYMD
               MOVE    "3"                 TO  ORCSDR-KBN
               MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
               MOVE    SPA-DRCD(2:4)       TO  ORCSDR-IN-DRCD
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
               IF      ORCSDR-IN-DRCD  NOT =   SPACE
                   MOVE    "1"                 TO  SPA-DRCD(1:1)
                   MOVE    ORCSDR-IN-DRCD      TO  SPA-DRCD(2:4)
                   MOVE    ORCSDR-IN-DRCD-G    TO  SPA-DRCD-NAME
               END-IF
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           ELSE
               INITIALIZE                  ORCSDRSETAREA
               MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
               MOVE    "1"                 TO  ORCSDR-KBN
               MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
               MOVE    SPA-DRCD(2:4)       TO  ORCSDR-IN-DRCD
               CALL    "ORCSDRSET"         USING
                                           SPA-AREA
                                           ORCSDRSETAREA
      *        訂正か受付ありの場合
               IF      SPA-DRCD        NOT =   SPACE
                   MOVE    ORCSDR-IN-DRCD-G    TO  SPA-DRCD-NAME
               ELSE
                   MOVE    "1"                 TO  SPA-DRCD (1:1)
                   MOVE    ORCSDR-DRCD     (1) TO  SPA-DRCD (2:4)
                   MOVE    ORCSDR-DRCD-TBL (1) TO  SPA-DRCD-NAME
               END-IF
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
           END-IF
      *
           END-IF
      *
           IF     (SPA-DRCD        NOT =   SPACE )  AND
                  (SPA-DRCD(2:4)   NOT =   SPA-DRCD-NAME(1:4))
               PERFORM 100122-DRCDNAME-SET-SEC
           END-IF
      *
           MOVE    SPA-DRCD-NAME       TO  SPA-GMN-DRCD-G
           .
       10012-SYSKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター名編集処理
      *****************************************************************
       100122-DRCDNAME-SET-SEC        SECTION.
      *
      *    職員情報（ドクター）
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-DRCD            TO  SYS-1010-KBNCD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SPA-DRCD(2:4)       TO  SPA-DRCD-NAME(1:4)
               MOVE    SYS-1010-NAME       TO  SPA-DRCD-NAME(6:)
      *
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               IF      SPA-DRCD-MAX        <   99
                   ADD     1               TO  SPA-DRCD-MAX
               END-IF
               MOVE    SPA-DRCD-NAME       TO  SPA-GMN-DRCDLST
                                                (SPA-DRCD-MAX)
      *
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       100122-DRCDNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象の科のドクター処理
      *****************************************************************
       100121-SEMONKACD-CHK-SEC        SECTION.
      *
      *    専門科コード
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-DRCDCHK
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2            >   5   )   OR
                              (FLG-DRCDCHK     NOT =   ZERO )
               IF      SYS-1010-SEMONKACD (IDX2)   NOT =   SPACE
                   MOVE    1               TO  FLG-OK
                   IF      SYS-1010-SEMONKACD (IDX2)
                                           =   SPA-SRYKA
                       MOVE    1               TO  FLG-DRCDCHK
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-DRCDCHK         =   ZERO
               IF      FLG-OK          =   ZERO
                   MOVE    2               TO  FLG-DRCDCHK
               END-IF
           END-IF
      *
           .
       100121-SEMONKACD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       10013-HKNCOMBI-SEC        SECTION.
      *
           INITIALIZE                  SPA-GMN-HKNCOMBI-LISTG
      *
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKN-MAX
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )
                           OR (IDX     >   30   )
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-NAI-THKNCOMBIMEI (IDX)
      *        受給者番号編集
               IF      ORCSHKN-NAI-HKNJANUM (IDX) NOT =   SPACE
                   MOVE    SPACE       TO  SPA-GMN-THKNCOMBIMEI (IDX)
                   MOVE    SPACE       TO  WRK-HKNCOMBI
                   STRING  ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                               DELIMITED BY  "  "
                       " ("                    DELIMITED  BY  SIZE
                       ORCSHKN-NAI-HKNJANUM (IDX)
                                               DELIMITED  BY  SPACE
                       ")"                     DELIMITED  BY  SIZE
                         INTO             WRK-HKNCOMBI
                   END-STRING
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    WRK-HKNCOMBI        TO  KANACHK-MAE-INPUT
                   MOVE    60                  TO  KANACHK-MAX-CNT
                   CALL    "ORCSKANACHK"       USING
                                               ORCSKANACHKAREA
                   MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-THKNCOMBIMEI
                                                            (IDX)
               END-IF
      *
               MOVE    ORCSHKN-NAI-HKNID (IDX)
                                       TO  SPA-NAI-HKNID  (IDX)
               MOVE    ORCSHKN-NAI-HKNNUM (IDX)
                                       TO  SPA-NAI-HKNNUM  (IDX)
               MOVE    ORCSHKN-NAI-KOH1HKNNUM (IDX)
                                       TO  SPA-NAI-KOH1HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH2HKNNUM (IDX)
                                       TO  SPA-NAI-KOH2HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH3HKNNUM (IDX)
                                       TO  SPA-NAI-KOH3HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-KOH4HKNNUM (IDX)
                                       TO  SPA-NAI-KOH4HKNNUM (IDX)
               MOVE    ORCSHKN-NAI-TEKSTYMD (IDX)
                                       TO  SPA-NAI-TEKSTYMD (IDX)
               MOVE    ORCSHKN-NAI-TEKEDYMD (IDX)
                                       TO  SPA-NAI-TEKEDYMD (IDX)
      *
               MOVE    ORCSHKN-NAI-FTNRATE (IDX)
                                       TO  SPA-NAI-TFTNRATE (IDX)
               MOVE    ORCSHKN-GMN-FTNRATEMEI (IDX)
                                       TO  SPA-GMN-TFTNRATEMEI (IDX)
      *
      *        四肢区分
               MOVE    ORCSHKN-NAI-RSI-SISIKBN (IDX)
                                           TO  SPA-NAI-RSI-SISIKBN
                                                              (IDX)
      *        傷病年月日
               MOVE    ORCSHKN-NAI-RSI-SHOBYOYMD (IDX)
                                           TO  SPA-NAI-RSI-SHOBYOYMD
                                                               (IDX)
      *        労災区分
               MOVE    ORCSHKN-NAI-RSI-HKNKBN (IDX)
                                           TO  SPA-NAI-RSI-HKNKBN
                                                               (IDX)
      *        治験区分
               MOVE    ORCSHKN-NAI-CHIKENKBN  (IDX)
                                           TO  SPA-NAI-CHIKENKBN
                                                               (IDX)
               MOVE    IDX                 TO  SPA-HKN-MAX
           END-PERFORM
      *
           IF      SPA-HKN-MAX         =   ZERO
      **       CONTINUE
               MOVE    4                   TO  SPA-HKN-ERR
           ELSE
               IF      ORCSHKN-IDX         =   ZERO
                   MOVE    1               TO  ORCSHKN-IDX
               END-IF
      *
      *        受診履歴から自動設定した保険組合せ
               MOVE    SPACE               TO  WRK-MAE-HKNCOMBI
               MOVE    ZERO                TO  SPA-HKNCHK-CHK
               IF     (SPA-HKNCOMBINUM NOT =   SPACE )  AND
                      (FLG-UKETUKE-ARI     =   ZERO  )
                   MOVE    SPA-HKNCOMBINUM     TO  WRK-MAE-HKNCOMBI
               END-IF
      *
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    SPA-GMN-THKNCOMBINUM (ORCSHKN-IDX)
                                           TO  SPA-HKNCOMBINUM
               END-IF
      *
      *        保険組合せが使用できるかのチェック
               PERFORM 100131-HKNCOMBI-CHK-SEC
           END-IF
      *
           .
       10013-HKNCOMBI-EXT.
      *
           EXIT.
      *
      *****************************************************************
      *    保険組合せ名称編集処理
      *****************************************************************
       100131-HKNCOMBI-CHK-SEC             SECTION.
      *
           PERFORM 400-HKNCOMBI-CHKHEN-SEC
           IF      FLG-HKNCOMBICHK     =   ZERO
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    ZERO            TO  SPA-HKN-ERR
      *        前回の保険組合せが使用できない時、１件目
      *        Ｋ１０からの時は、そのまま
               IF      LNK-SC95-PG         =   "K10"
                   MOVE    SPACE               TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
                   MOVE    ZERO                TO  SPA-HKNKBN
                   MOVE    ZERO                TO  SPA-HKNID
                   MOVE    SPACE               TO  SPA-RSI-HKNKBN
                   GO      TO      100131-HKNCOMBI-CHK-EXT
               END-IF
      *
      *!!!!
      *H31.2
      *        外来の入院中訂正の保険は自費・アフターのみ
               IF     (SPA-SYORIFLG        =   1  )
                  AND (LNK-SC95-KBN        =   "4")
                  AND (SPA-NYUGAIKBN       =   2  )
                  AND (SPA-NAI-NYUIN       =   1  )
      *            外来の入院中訂正の保険組合せ決定
                   PERFORM 10013119-HKNCOMBI-SETNYUIN-SEC
               ELSE
      *
      *        省略時の保険組合せ決定
               PERFORM 1001311-HKNCOMBI-SET-SEC
      *
               END-IF
      *        前回の保険組合せとの判定
               IF     (WRK-MAE-HKNCOMBI    NOT =   SPACE )
                 AND  (SPA-ZENHKNCHK-FLG       =   "1"
                                               OR  "2"   )
                   PERFORM 1001312-MAE-HKNCOMBICHEK-SEC
               END-IF
           END-IF
      *
           .
       100131-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ決定処理
      *****************************************************************
       1001311-HKNCOMBI-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
           MOVE    ZERO                TO  WRK-IDXH2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
      *      自費は対象外とする
             IF      SPA-NAI-HKNNUM (IDX)(1:2)   =   "98"
                 CONTINUE
             ELSE
               MOVE    ZERO                TO  WRK-CNT-HKN2
               IF      SPA-NAI-HKNNUM (IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-KOH1HKNNUM (IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-KOH2HKNNUM (IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-KOH3HKNNUM (IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
               IF      SPA-NAI-KOH4HKNNUM (IDX)    NOT =   SPACE
                   ADD     1                   TO  WRK-CNT-HKN2
               END-IF
      *        治験・自費は他の保険がない時に有効とする
               IF     (ORCSHKN-NAI-HKNNUM (IDX)(1:2) =   "98"
                                                     OR  "90"
                                                     OR  "91" )
                   IF      WRK-IDXH2           =   ZERO
                       MOVE    IDX             TO  WRK-IDXH2
                   END-IF
               ELSE
                   IF      WRK-CNT-HKN         <   WRK-CNT-HKN2
                       MOVE    IDX                 TO  WRK-IDXH
                       MOVE    WRK-CNT-HKN2        TO  WRK-CNT-HKN
                   END-IF
               END-IF
      *        労災・自賠責があれば対象とする
               IF      SPA-NAI-HKNNUM (IDX)    =   "971"
                                               OR  "973"
                   MOVE    IDX                 TO  WRK-IDXH
                   MOVE    SPA-HKN-MAX         TO  IDX
               END-IF
             END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    WRK-IDXH2       TO  WRK-IDXH
           END-IF
           IF      WRK-IDXH            =   ZERO
               MOVE    1               TO  WRK-IDXH
           END-IF
      *
           MOVE    SPA-GMN-THKNCOMBINUM (WRK-IDXH)
                                       TO  SPA-HKNCOMBINUM
      *    保険決定
           PERFORM 400-HKNCOMBI-CHKHEN-SEC
      *
           .
       1001311-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *H31.2
      *****************************************************************
      *    外来の入院中訂正の保険組合せ決定処理
      *****************************************************************
       10013119-HKNCOMBI-SETNYUIN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-CNT-HKN
           MOVE    ZERO                TO  WRK-CNT-HKN2
           MOVE    ZERO                TO  WRK-IDXH
           MOVE    ZERO                TO  WRK-IDXH2
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKN-MAX
      *        労災（アフターケア）対象とする
               IF     (SPA-NAI-HKNNUM (IDX)    =   "971" )
                  AND (SPA-NAI-RSI-HKNKBN(IDX) =   "3"   )
                   MOVE    IDX                 TO  WRK-IDXH
                   MOVE    SPA-HKN-MAX         TO  IDX
               END-IF
      *
      *        自費はアフターケがない時に有効とする
               IF     (ORCSHKN-NAI-HKNNUM (IDX)(1:2) =   "98")
                   IF      WRK-IDXH2           =   ZERO
                       MOVE    IDX             TO  WRK-IDXH2
                   END-IF
                END-IF
           END-PERFORM
      *
           IF      WRK-IDXH            =   ZERO
               MOVE    WRK-IDXH2       TO  WRK-IDXH
           END-IF
      *    対象がなければ、選択なし
           IF      WRK-IDXH            =   ZERO
               GO      TO      10013119-HKNCOMBI-SETNYUIN-EXT
           END-IF
      *
           MOVE    SPA-GMN-THKNCOMBINUM (WRK-IDXH)
                                       TO  SPA-HKNCOMBINUM
      *    保険決定
           PERFORM 400-HKNCOMBI-CHKHEN-SEC
      *
           .
       10013119-HKNCOMBI-SETNYUIN-EXT.
           EXIT.
      *H31.2
      *
      *****************************************************************
      *    前回の保険組合せとの判定処理
      *****************************************************************
       1001312-MAE-HKNCOMBICHEK-SEC    SECTION.
      *
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    WRK-MAE-HKNCOMBI    TO  COMB-HKNCOMBINUM
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HKNCOMBI-READ-SEC
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      COMB-DELKBN         =   SPACE
               IF     (COMB-HKNNUM         =   "971"
                                           OR  "973" )
                   CONTINUE
               ELSE
                   IF     (ORCSHKN-NAI-TEKSTYMD (WRK-IDXH)
                                               <   COMB-TEKEDYMD)
                       MOVE    1               TO  SPA-HKNCHK-FLG
                   END-IF
               END-IF
      *        すべてチェックの時
               IF      SPA-ZENHKNCHK-FLG   =   "2"
                   MOVE    1               TO  SPA-HKNCHK-FLG
               END-IF
           ELSE
      *        前回が削除
               MOVE    2                   TO  SPA-HKNCHK-FLG
           END-IF
           .
       1001312-MAE-HKNCOMBICHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せチェック・編集処理
      *****************************************************************
       400-HKNCOMBI-CHKHEN-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKNCOMBICHK
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-HKN-MAX
               IF      SPA-HKNCOMBINUM     =   SPA-GMN-THKNCOMBINUM
                                                         (IDY)
                   MOVE    1               TO  FLG-HKNCOMBICHK
                   MOVE    IDY             TO  IDX
               END-IF
           END-PERFORM
      *
      *
           IF      FLG-HKNCOMBICHK     =   ZERO
      *        選択した保険がない場合、エラー
      *****    MOVE   "1034"               TO  SPA-ERRCD
               MOVE    4                   TO  SPA-HKN-ERR
           ELSE
      *        保険情報編集・年齢
               IF      LNK-SC95-KBN        =   "H"
      *API対応
                                           OR  "@"
                   PERFORM 4001-HKNCOMBI-HEN-SEC
               ELSE
                   MOVE    SPA-GMN-HKN-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-G
               END-IF
           END-IF
      *
           .
       400-HKNCOMBI-CHKHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険情報編集・年齢編集処理
      *****************************************************************
       4001-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-HKN-ERR
      *
           MOVE    SPA-GMN-HKN-LIST (IDX)
                                       TO  SPA-GMN-HKNCOMBI-G
           MOVE    SPA-GMN-TFTNRATEMEI (IDX)
                                       TO  SPA-GMN-FTNRATEMEI
           MOVE    SPA-NAI-TFTNRATE    (IDX)
                                       TO  SPA-GMN-FTNRATE
      *    共通ＳＰＡへ
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-NAI-THKNCOMBIMEI(IDX)
                                       TO  SPA-HKNCOMBIMEI
           MOVE    SPA-GMN-HKNCOMBIMEI TO  SPA-HKNCOMBIMEI-60
           MOVE    SPA-NAI-TFTNRATE    (IDX)
                                       TO  SPA-FTNRATE
           MOVE    SPA-GMN-TFTNRATEMEI (IDX)
                                       TO  SPA-FTNRATEMEI
           MOVE    SPA-NAI-HKNNUM     (IDX)
                                       TO  SPA-HKNNUM
           MOVE    SPA-NAI-KOH1HKNNUM  (IDX)
                                       TO  SPA-KOH1HKNNUM
           MOVE    SPA-NAI-KOH2HKNNUM  (IDX)
                                       TO  SPA-KOH2HKNNUM
           MOVE    SPA-NAI-KOH3HKNNUM  (IDX)
                                       TO  SPA-KOH3HKNNUM
           MOVE    SPA-NAI-KOH4HKNNUM  (IDX)
                                       TO  SPA-KOH4HKNNUM
      *    有効期間
           MOVE    SPA-NAI-TEKSTYMD  (IDX)
                                       TO  SPA-COB-TEKSTYMD
           MOVE    SPA-NAI-TEKEDYMD  (IDX)
                                       TO  SPA-COB-TEKEDYMD
      *    治験
           MOVE    SPA-NAI-CHIKENKBN (IDX)
                                       TO  SPA-CHIKENKBN
      *
      *    労災 四肢区分
           MOVE    SPA-NAI-RSI-SISIKBN (IDX)
                                       TO  SPA-RSI-SISIKBN
      *    労災 傷病年月日
           MOVE    SPA-NAI-RSI-SHOBYOYMD (IDX)
                                       TO  SPA-RSI-SHOBYOYMD
           MOVE    SPA-NAI-RSI-HKNKBN (IDX)
                                       TO  SPA-RSI-HKNKBN
      *    労災判定
           IF      SPA-HKNNUM          =   "971"
                                       OR  "973"
               MOVE    1               TO  SPA-HKNKBN
           ELSE
               MOVE    ZERO            TO  SPA-HKNKBN
          END-IF
      *    公害判定
           IF      SPA-HKNNUM          =   "975"
               MOVE    2               TO  SPA-HKNKBN
           END-IF
      *
      *    保険算定年齢チェック等
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-HKNCOMBINUM     TO  AGECHK-HKNCOMBINUM
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           IF      AGECHK-FTNRATE  NOT =   ZERO
      *        負担率
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-GMN-FTNRATEMEI
               MOVE    AGECHK-FTNRATEMEI   TO  SPA-FTNRATEMEI
               MOVE    AGECHK-FTNRATE      TO  SPA-GMN-FTNRATE
               MOVE    AGECHK-FTNRATE      TO  SPA-FTNRATE
           END-IF
           MOVE    SPACE               TO  SPA-RSI-JUNKYO
           EVALUATE    AGECHK-HKNKBN
      *        自賠責の健保準拠判定
               WHEN     4
                   MOVE    "4"             TO  SPA-RSI-HKNKBN
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
      *        公務災害の健保準拠判定
               WHEN    5
                   MOVE    "5"             TO  SPA-RSI-HKNKBN
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
      *        第三者行為（自賠責の健保準拠）
               WHEN    7
                   MOVE    1               TO  SPA-HKNKBN
                   MOVE    "6"             TO  SPA-RSI-HKNKBN
                   MOVE    "1"             TO  SPA-RSI-JUNKYO
           END-EVALUATE
      *
           MOVE    ZERO                TO  SPA-RJN-ERR
           MOVE    ZERO                TO  SPA-HKN-ERR
           MOVE    ZERO                TO  SPA-NAI-KOHFLG-ERR
           MOVE    ZERO                TO  SPA-NAI-KOHFLG
           MOVE    ZERO                TO  SPA-NAI-ZENKIFLG-ERR
           MOVE    ZERO                TO  SPA-NAI-ZENKIFLG
      *
      *    Ｈ１５．４以降の保険期間が違う時
           IF      AGECHK-HKNCHK       =   1
               MOVE    1                   TO  SPA-HKN-ERR
           END-IF
      *    Ｈ１５．４以降の公費期間が違う時
           IF      AGECHK-HKNCHK       =   2
               MOVE    2                   TO  SPA-HKN-ERR
           END-IF
      *    老人でＨ１４．１０の保険変更がない時
           IF      AGECHK-RJNCHK       =   1
               MOVE    1                   TO  SPA-RJN-ERR
           END-IF
      *    老人でＨ１８．１０の保険変更がない時（２割）
           IF      AGECHK-RJNCHK       =   3
               MOVE    2                   TO  SPA-RJN-ERR
           END-IF
      *    Ｈ２０．４から後期高齢者の保険がない時
           IF      AGECHK-RJNCHK       =   4
               MOVE    3                   TO  SPA-RJN-ERR
           END-IF
      *    Ｈ２０．４から退職国保で６５歳以上
           IF      AGECHK-RJNCHK       =   9
               MOVE    9                   TO  SPA-RJN-ERR
           END-IF
      *!!!
           IF      AGECHK-RJNCHK       =   2
               MOVE    1                   TO  SPA-NAI-ROUFLG
           ELSE
               MOVE    ZERO                TO  SPA-NAI-ROUFLG
           END-IF
      *
      *    老人判定
           MOVE    AGECHK-ROUJIN       TO  SPA-NAI-ROUJIN
      *
      *    地方公費単独
           IF      AGECHK-KOHCHK       =   1
               MOVE    ZERO            TO  SPA-NAI-KOHFLG-ERR
               MOVE    1               TO  SPA-NAI-KOHFLG
           END-IF
      *
      *    国保 前期高齢者変更チェック
           IF      AGECHK-KAKUNINCHK   =   1
               MOVE    ZERO            TO  SPA-NAI-ZENKIFLG-ERR
               MOVE    1               TO  SPA-NAI-ZENKIFLG
           END-IF
      *H28.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
           .
       4001-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２初画面用編集処理
      *****************************************************************
       110-K02SYOSIN-SEC              SECTION.
      *
      *    患者決定時編集
           IF      LNK-SC95-KBN        =   "2"  OR  "5"
               PERFORM 1101-PTINF-HENSYU-SEC
           END-IF
      *    患者特記事項編集
           PERFORM 1102-PTTOKKI-SET-SEC
      *
           MOVE    ZERO            TO  SPA-JIKANCHK-ERR
      *    患者決定時、時間チェック
           IF     (LNK-SC95-KBN        =   "2" ) AND
                  (SPA-JIKANTOKU-CHK   =   "1" )
               PERFORM 1109-TIMEI-CHK-SEC
           END-IF
      *
      *    初診日、最終診察日
           MOVE    SPACE               TO  SPA-SYOYMD 
           MOVE    SPACE               TO  SPA-LSTYMD 
           PERFORM 1101-HKN-SYOYMD-SET-SEC
      *
      *    最終算定診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-SRYKA-MAX
               IF      SPA-LSTSRYKA        =   SPA-GMN-SRYKAL   (IDX)
                   MOVE    SPA-GMN-SRYKAMEIL(IDX)  TO  SPA-LSTSRYKAMEI
                   MOVE    SPA-SRYKA-MAX           TO  IDX
               END-IF
           END-PERFORM
      *
      *    未収金額編集
           PERFORM 49011-MISYU-HEN-SEC
      *
           .
       110-K02SYOSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者決定時編集処理
      *****************************************************************
       1101-PTINF-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-CHKHKTBYO-ARI
      *H28.4
           MOVE    ZERO                TO  SPA-NNCHIH-ARI
           MOVE    ZERO                TO  SPA-SYOUNI-ARI
      *H30.4
           MOVE    ZERO                TO  SPA-NINPUKBN-ARI
      *H28.9
      *    要介護対象
           MOVE    ZERO                TO  SPA-YOUKAIGO-ARI
      *
      *    患者マスター読み込み
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               INITIALIZE                  SPA-KYOTUKEY
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               INITIALIZE                  SPA-K02-AREA
               MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
               MOVE    SPA-PTID            TO  SPA-GMN-PTID
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
               MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
               MOVE    "1003"              TO  SPA-ERRCD
               GO      TO      1101-PTINF-HENSYU-EXT
           END-IF
      *
           MOVE    PTINF-KANANAME      TO  SPA-KANANAME
           MOVE    PTINF-NAME          TO  SPA-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-SEX
           END-EVALUATE
      *H30.4
           MOVE    PTINF-SEX           TO  SPA-NAI-SEX
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-BIRTHDAYW
      *    禁忌情報設定
           PERFORM 401011-TABOO-SET-SEC
      *    入金方法
           MOVE    PTINF-NYUKIN-HOHO   TO  SPA-NYUKIN-HOHO
      *    入金方法
      *    IF      PTINF-SKYPRTFLG     =   SPACE
      *        MOVE    SPA-SKYPRTFLG-SYS   TO  SPA-SKYPRTFLG
      *    ELSE
               MOVE    PTINF-SKYPRTFLG     TO  SPA-SKYPRTFLG
      *        患者情報を優先
      *!       MOVE    "P"                 TO  SPA-SKYPRTFLG-SYS
      *    END-IF
      *!!
      *    同一患者識別編集
           MOVE    ZERO                TO  SPA-NAI-GRPKBN
           IF      SPA-GRPHOSPKBN  NOT =   ZERO
               PERFORM 10015-PTSAME-SET-SEC
           END-IF
      *
      *H26.4
      *    地域包括診療対象疾病
           IF     (SPA-SRYYMD          >=  "20140401" )
            AND   (SPA-CHKHTKKSN       =   1          )
               PERFORM 11019-CHKHTKBYO-SET-SEC
           END-IF
      *H28.4
      *    認知症地域包括診療対象・小児かかりつけ対象
           IF     (SPA-SRYYMD          >=  "20160401" )
               PERFORM 110110-NNCHISYOU-SET-SEC
           END-IF
      *H28.10
      *    要介護対象
      *    IF     (SPA-SRYYMD          >=  "20161001" )
               PERFORM 110111-YOUKAIGO-SET-SEC
      *    END-IF
      *
           .
       1101-PTINF-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別編集処理
      *****************************************************************
       10015-PTSAME-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-GRPKBN
           MOVE    ZERO                TO  SPA-NAI-GRPKBN2
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      PTSAME-REC
           MOVE    SPA-HOSPNUM         TO  PTSAME-HOSPNUM
           MOVE    SPA-PTID            TO  PTSAME-PTID
           MOVE    PTSAME-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-PTSAME-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTSAME
           END-IF
           MOVE    "tbl_ptsame"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTSAME          =   ZERO
      *        同一患者情報
               MOVE    PTSAME-REC          TO  MCPDATA-REC
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-PTSAME
               END-IF
               PERFORM UNTIL    FLG-PTSAME     =   1
                   IF     (PTSAME-HOSPNUM      =   SPA-HOSPNUM   )
                     AND  (PTSAME-PTID         =   SPA-PTID      )
                       CONTINUE
                   ELSE
                       ADD     1               TO  IDX
                       MOVE    PTSAME-HOSPNUM  TO  SPA-NAI-GRPHOSPNUM
                                                                (IDX)
                       MOVE    PTSAME-PTID     TO  SPA-NAI-GRPPTID
                                                                (IDX)
                   END-IF
      *
                   MOVE    "tbl_ptsame"        TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-PTSAME-READ-SEC
               END-PERFORM
               MOVE    "tbl_ptsame"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           IF      IDX                 >   ZERO
               MOVE    1                   TO  SPA-NAI-GRPKBN
           ELSE
               MOVE    ZERO                TO  SPA-NAI-GRPKBN
           END-IF
      *    診療日の受診有無
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )
                           OR (SPA-NAI-GRPHOSPNUM (IDX)    =   ZERO)
               PERFORM 100151-PTSAME-JYURRK-SEC
           END-PERFORM
           .
       10015-PTSAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日の受診有無設定処理
      *****************************************************************
       100151-PTSAME-JYURRK-SEC         SECTION.
      *
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-NAI-GRPHOSPNUM(IDX)  TO  JYURRK-HOSPNUM
           MOVE    SPA-NAI-GRPPTID   (IDX)  TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-SRYYMD          TO  JYURRK-UPSRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key13"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key13"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-JYURRK          =   ZERO
               MOVE    1                   TO  SPA-NAI-GRPKBN2
           END-IF
           .
       100151-PTSAME-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌情報設定処理
      *****************************************************************
       401011-TABOO-SET-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-TABOO-TBL
           MOVE    ZERO                TO  IDX
      *
      *    禁忌１
           MOVE    1                  TO  IDX
           MOVE    ZERO               TO  IDY
           IF      PTINF-TABOO-1   NOT =   SPACE
                MOVE    SPACE              TO  WRK-TABOO-G
                STRING  "禁忌："           DELIMITED  BY  SIZE
                        PTINF-TABOO-1      DELIMITED  BY  SIZE
                                           INTO  WRK-TABOO-G
                END-STRING
                PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *    禁忌２
           IF      PTINF-TABOO-2   NOT =   SPACE
               MOVE    PTINF-TABOO-2   TO  WRK-TABOO-G
               PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *    アレルギー１
           MOVE    2                  TO  IDX
           MOVE    ZERO               TO  IDY
           IF      PTINF-ALLERGY-1     NOT =   SPACE
                MOVE    SPACE              TO  WRK-TABOO-G
                STRING  "アレルギー："     DELIMITED  BY  SIZE
                        PTINF-ALLERGY-1    DELIMITED  BY  SIZE
                                           INTO  WRK-TABOO-G
                END-STRING
                PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *    アレルギー２
           IF      PTINF-ALLERGY-2     NOT =   SPACE
               MOVE    PTINF-ALLERGY-2     TO  WRK-TABOO-G
               PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *    感染症１
           MOVE    3                  TO  IDX
           MOVE    ZERO               TO  IDY
           IF      PTINF-KANSENSYO-1   NOT =   SPACE
                MOVE    SPACE              TO  WRK-TABOO-G
                STRING  "感染症："         DELIMITED  BY  SIZE
                        PTINF-KANSENSYO-1  DELIMITED  BY  SIZE
                                           INTO  WRK-TABOO-G
                END-STRING
                PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *    感染症２
           IF      PTINF-KANSENSYO-2   NOT =   SPACE
               MOVE    PTINF-KANSENSYO-2   TO  WRK-TABOO-G
               PERFORM 4010110-TABOO-HEN-SEC
           END-IF
      *
      *    コメント１
           MOVE    4                   TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  FLG-COMMENT
           IF      PTINF-COMMENT-1 NOT =   SPACE
                MOVE    SPACE              TO  WRK-TABOO-G
                STRING  "コメント："       DELIMITED  BY  SIZE
                        PTINF-COMMENT-1    DELIMITED  BY  SIZE
                                           INTO  WRK-TABOO-G
                END-STRING
      *        全角・半角判定
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    PTINF-COMMENT-1     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-SYUBETU     =   ZERO ) OR
                      (KANACHK-RC2         =   8    )
                   MOVE    1                   TO  FLG-COMMENT
               ELSE
                   MOVE    ZERO                TO  FLG-COMMENT
               END-IF
      *        最初が正常に表示できない時
               IF     (FLG-COMMENT        =   1   )  AND
                      (KANACHK-MAX        >   52 )
                   ADD     1                  TO  IDY
                   MOVE    "コメント：全角半角混在の為表示できません"
                                              TO  SPA-GMN-TABOO
                                                           (IDX IDY)
               ELSE
                   PERFORM 4010110-TABOO-HEN-SEC
               END-IF
           END-IF
      *    コメント２
           IF      PTINF-COMMENT-2 NOT =   SPACE
      *        全角・半角判定
               MOVE    PTINF-COMMENT-2    TO  WRK-TABOO-G
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    PTINF-COMMENT-2     TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               IF     (KANACHK-SYUBETU     =   ZERO ) OR
                      (KANACHK-RC2         =   8    )
      *            最大桁数判定
                   IF      KANACHK-MAX        <=  62
                       PERFORM 4010110-TABOO-HEN-SEC
                   ELSE
                       ADD     1                   TO  IDY
                       MOVE    "全角半角混在の為表示できません"
                                               TO  SPA-GMN-TABOO
                                                           (IDX IDY)
                   END-IF
               ELSE
                   PERFORM 4010110-TABOO-HEN-SEC
               END-IF
           END-IF
      *
      **   MOVE    IDX                 TO  SPA-TABOO-MAX
      *
           .
       401011-TABOO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       4010110-TABOO-HEN-SEC            SECTION.
      *
           PERFORM VARYING     IDX-P   FROM    1  BY  1
                   UNTIL      (IDX-P       >   32 )
                           OR (IDX         >=  8  )
               IF      WRK-TABOO (IDX-P)   NOT =   SPACE
                   ADD     1                   TO  IDY
                   MOVE    WRK-TABOO (IDX-P)   TO  SPA-GMN-TABOO
                                                         (IDX IDY)
               END-IF
           END-PERFORM
           .
       4010110-TABOO-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者特記事項編集処理
      *****************************************************************
       1102-PTTOKKI-SET-SEC            SECTION.
      *
      *    施設入居
           MOVE    SPACE               TO  SPA-PTTOKK-KBN
           MOVE    SPACE               TO  SPA-PTTOKK-NAIYO
      *    患者特記事項検索
           MOVE    SPACE               TO  PTTOKKI-REC
           MOVE    SPA-HOSPNUM         TO  PTTOKKI-HOSPNUM
           MOVE    SPA-PTID            TO  PTTOKKI-PTID
           MOVE    PTTOKKI-REC         TO  MCPDATA-REC
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_pttokki"       TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 900-PTTOKKI-READ-SEC
           ELSE
               INITIALIZE                  PTTOKKI-REC
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
           PERFORM UNTIL       FLG-PTTOKKI     =   1
      *        施設入居
               IF      PTTOKKI-CD          =   "07"
      *****************                    OR  "08"
                                           OR  "09"
      *H30.4       医併
                                           OR  "38"
                   IF     (PTTOKKI-STYM    <=  SPA-SRYYMD(1:6))
                      AND (PTTOKKI-EDYM    >=  SPA-SRYYMD(1:6))
                       MOVE    PTTOKKI-CD      TO  SPA-PTTOKK-KBN
                       MOVE    PTTOKKI-NAIYO   TO  SPA-PTTOKK-NAIYO
                       MOVE    1               TO  FLG-PTTOKKI
                   END-IF
               END-IF
               IF      FLG-PTTOKKI         =   ZERO
                   MOVE    "tbl_pttokki"       TO  MCP-TABLE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 900-PTTOKKI-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_pttokki"       TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    公費　950　判定
           IF      SPA-PTTOKK-KBN      =   SPACE
      *
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    "950"               TO  PTKOH-KOHNUM
               MOVE    SPA-SRYYMD          TO  PTKOH-TEKSTYMD
               MOVE    SPA-SRYYMD          TO  PTKOH-TEKEDYMD
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               MOVE    "DBSELECT"          TO  MCP-FUNC
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 970-PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF        =   ZERO
      *                07　０７老併　とする
                       MOVE    "07"                TO  SPA-PTTOKK-KBN
      *                名称編集
                       INITIALIZE                  HKNNUM-REC
                       MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                       MOVE    PTKOH-KOHNUM        TO  HKN-HKNNUM
                       MOVE    "00"                TO  HKN-PAYKBN
                       MOVE    SPA-SRYYMD          TO  HKN-TEKSTYMD
                       MOVE    SPA-SRYYMD          TO  HKN-TEKEDYMD
      *                保険番号マスタ検索処理
                       PERFORM 900-HKNNUM-KEY-READ-SEC
                       MOVE    HKN-TANSEIDONAME    TO  SPA-PTTOKK-NAIYO
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-IF
      *
      *H30.4
      *    公費　948　判定
           IF      SPA-PTTOKK-KBN      =   SPACE
               INITIALIZE                  PTKOHINF-REC
               MOVE    SPA-HOSPNUM         TO  PTKOH-HOSPNUM
               MOVE    SPA-PTID            TO  PTKOH-PTID
               MOVE    "948"               TO  PTKOH-KOHNUM
               MOVE    SPA-SRYYMD          TO  PTKOH-TEKSTYMD
               MOVE    SPA-SRYYMD          TO  PTKOH-TEKEDYMD
      *
               MOVE    PTKOHINF-REC        TO  MCPDATA-REC
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               MOVE    "DBSELECT"          TO  MCP-FUNC
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 970-PTKOHINF-READ-SEC
                   IF      FLG-PTKOHINF        =   ZERO
      *                38　３８医併　とする
                       MOVE    "38"                TO  SPA-PTTOKK-KBN
      *                名称編集
                       INITIALIZE                  HKNNUM-REC
                       MOVE    SPA-HOSPNUM         TO  HKN-HOSPNUM
                       MOVE    PTKOH-KOHNUM        TO  HKN-HKNNUM
                       MOVE    "00"                TO  HKN-PAYKBN
                       MOVE    SPA-SRYYMD          TO  HKN-TEKSTYMD
                       MOVE    SPA-SRYYMD          TO  HKN-TEKEDYMD
      *                保険番号マスタ検索処理
                       PERFORM 900-HKNNUM-KEY-READ-SEC
                       MOVE    HKN-TANSEIDONAME    TO  SPA-PTTOKK-NAIYO
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
           END-IF
           .
       1102-PTTOKKI-SET-EXT.
           EXIT.
      *****************************************************************
      *    地域包括診療対象疾病編集処理
      *****************************************************************
       11019-CHKHTKBYO-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-CHKHKTBYO-ARI
      *
           INITIALIZE                  PTCONF-REC
           MOVE    PTINF-HOSPNUM       TO  PTCONF-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCONF-PTID
           MOVE    "TIIKIHOKATU"       TO  PTCONF-CKEY
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-PTCONF          =   ZERO
      *        地域包括診療対象疾病チェック
               MOVE    PTCONF-CDATA(1:4)   TO  WRK-CHKHKTBYO-G
               MOVE    ZERO                TO  WRK-CNT
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      WRK-CHKHKTBYO (IDX) =   "1"
                       ADD     1               TO  WRK-CNT
                   END-IF
               END-PERFORM
               IF      WRK-CNT             >=  2
      *            地域包括診療加算　算定あり
                   MOVE    1               TO  SPA-CHKHKTBYO-ARI
               END-IF
           END-IF
           .
       11019-CHKHTKBYO-SET-EXT.
           EXIT.
      *H28.4
      *****************************************************************
      *    認知症地域包括診療対象・小児かかりつけ対象病編集処理
      *****************************************************************
       110110-NNCHISYOU-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-NNCHIH-ARI
           MOVE    ZERO                TO  SPA-SYOUNI-ARI
           MOVE    ZERO                TO  SPA-NINPUKBN-ARI
      *
           INITIALIZE                  PTCONF-REC
           MOVE    PTINF-HOSPNUM       TO  PTCONF-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCONF-PTID
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCONF
               INITIALIZE                  PTCONF-REC
           END-IF
           PERFORM UNTIL       FLG-PTCONF      =   1
               EVALUATE    PTCONF-CKEY
               WHEN    "NINTIHOKATU"
      *            認知症地域包括診療
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    1               TO  SPA-NNCHIH-ARI
                   END-IF
               WHEN    "SYOUNIKAKARI"
      *            小児かかりつけ診療
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    1               TO  SPA-SYOUNI-ARI
                   END-IF
      *H28.12
      *            指導料等自動算定
                   WHEN    "JIDOSANTEI-1"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-AUTOSIDOU (1)
                   WHEN    "JIDOSANTEI-2"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-AUTOSIDOU (2)
                   WHEN    "JIDOSANTEI-3"
                       MOVE    PTCONF-CDATA(1:9)
                                           TO  SPA-AUTOSIDOU (3)
      *H30.4
               WHEN    "NINPUKBN"
      *            妊婦設定
                   IF      PTCONF-CDATA(1:1)   =   "1"
                       MOVE    1               TO  SPA-NINPUKBN-ARI
                   END-IF
      *H30.12
      *            2019.1から妊婦加算なし
                   IF      SPA-SRYYMD      >=  "20190101"
                       MOVE    ZERO            TO  SPA-NINPUKBN-ARI
                   END-IF
               END-EVALUATE
               MOVE    "tbl_ptconf"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-PTCONF-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    小児かかりつけ診療は、施設基準の判定
           IF      SPA-SYOUNI-ARI      =   1
               IF      SPA-SYOUNIKJN   NOT =   1
                   MOVE    ZERO            TO  SPA-SYOUNI-ARI
               END-IF
           END-IF
           .
       110110-NNCHISYOU-SET-EXT.
           EXIT.
      *H28.10
      *****************************************************************
      *    要介護対象編集処理
      *****************************************************************
       110111-YOUKAIGO-SET-SEC            SECTION.
      *
           MOVE    ZERO                TO  SPA-YOUKAIGO-ARI
      *H28.10
           MOVE    SPACE               TO  SPA-GMN-YOUKAIGO
           MOVE    SPACE               TO  WRK-KAIGOMEI
      *    患者介護保険情報
           INITIALIZE                  PTCARE-NINTEI-REC
           MOVE    SPA-HOSPNUM         TO  PTCRNIN-HOSPNUM
           MOVE    PTINF-PTID          TO  PTCRNIN-PTID
           MOVE    SPA-SRYYMD          TO  PTCRNIN-TEKSTYMD
           MOVE    SPA-SRYYMD          TO  PTCRNIN-TEKEDYMD
      *
           MOVE    PTCARE-NINTEI-REC   TO  MCPDATA-REC
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTCARE-NINTEI
               INITIALIZE                  PTCARE-NINTEI-REC
           END-IF
           PERFORM UNTIL      (FLG-PTCARE-NINTEI   =   1  )
      *                   OR  (SPA-YOUKAIGO-ARI    NOT =   ZERO )
      *H28.10
      *        要介護名称
               EVALUATE    PTCRNIN-CAREJOTAICD
               WHEN    "01"
                   MOVE    "自立"          TO  WRK-KAIGOMEI
               WHEN    "11"
                   MOVE    "要支援"        TO  WRK-KAIGOMEI
               WHEN    "12"
                   MOVE    "要支援１"      TO  WRK-KAIGOMEI
               WHEN    "13"
                   MOVE    "要支援２"      TO  WRK-KAIGOMEI
               WHEN    "21"
                   MOVE    "要介護１"      TO  WRK-KAIGOMEI
               WHEN    "22"
                   MOVE    "要介護２"      TO  WRK-KAIGOMEI
               WHEN    "23"
                   MOVE    "要介護３"      TO  WRK-KAIGOMEI
               WHEN    "24"
                   MOVE    "要介護４"      TO  WRK-KAIGOMEI
               WHEN    "25"
                   MOVE    "要介護５"      TO  WRK-KAIGOMEI
      *H30.7
               WHEN    "31"
                   MOVE    "事業対象者"    TO  WRK-KAIGOMEI
               WHEN    OTHER
                   MOVE    PTCRNIN-CAREJOTAICD TO  WRK-KAIGOMEI
               END-EVALUATE
      *
      *        要介護状態
               IF       PTCRNIN-CAREJOTAICD    =   "01"
                                               OR  "31"
      *            自立・事業対象者　は対象外
                   CONTINUE
               ELSE
                   MOVE    1               TO  SPA-YOUKAIGO-ARI
               END-IF
      *
               MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 930-PTCARENINTEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptcare_nintei" TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    要介護対象
           IF     (SPA-SRYYMD          >=  "20161001" )
               CONTINUE
           ELSE
      *        H28/10まではなし
               MOVE    ZERO            TO  SPA-YOUKAIGO-ARI
           END-IF
      *
           IF      WRK-KAIGOMEI    NOT =   SPACE
               STRING  "要介護状態："
                       WRK-KAIGOMEI        DELIMITED  BY  SPACE
                                       INTO      SPA-GMN-YOUKAIGO
               END-STRING
           END-IF
           .
       110111-YOUKAIGO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    時間外チェック処理
      *****************************************************************
       1109-TIMEI-CHK-SEC            SECTION.
      *
           ACCEPT  SYS-TIME            FROM    TIME
      *
           MOVE    SYS-TIME            TO  WRK-SYS-TIME
      *
           MOVE    ZERO                TO  FLG-TIME
           IF      SPA-JIKANCHK-TIME (1)   NOT =   ZERO
               IF      (WRK-SYS-HHMM       >=  SPA-JIKANCHK-STR(1))
                   AND (WRK-SYS-HHMM       <=  SPA-JIKANCHK-END(1))
                   MOVE    1               TO  FLG-TIME
               END-IF
           END-IF
           IF      SPA-JIKANCHK-TIME (2)   NOT =   ZERO
               IF      (WRK-SYS-HHMM       >=  SPA-JIKANCHK-STR(2))
                   AND (WRK-SYS-HHMM       <=  SPA-JIKANCHK-END(2))
                   MOVE    1               TO  FLG-TIME
               END-IF
           END-IF
      *    除外時間外
           IF     (SPA-JIKANCHK-TIME (1)   =   ZERO )  AND
                  (SPA-JIKANCHK-TIME (2)   =   ZERO )
               MOVE    ZERO            TO  SPA-JIKANCHK-ERR
           ELSE
               IF      FLG-TIME            =   1
                   MOVE    ZERO            TO  SPA-JIKANCHK-ERR
               ELSE
                   MOVE    "1"             TO  SPA-JIKANCHK-ERR
               END-IF
           END-IF
           .
       1109-TIMEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険 初診日、最終診察日編集処理
      *****************************************************************
       1101-HKN-SYOYMD-SET-SEC             SECTION.
      *
           MOVE    ZERO                TO  SPA-SYOSINFLG
      *    診療科歴を検索
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                  SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
      *D   MOVE    SPA-SRYKA           TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                  SRYKARRK-REC
           END-IF
           PERFORM UNTIL       FLG-SRYKARRK    =   1
      *
               IF      KARRK-SRYKA         =   SPACE
                   CONTINUE
               ELSE
      *        初診算定日
               IF     (KARRK-SYOSINYMD2 NOT =  SPACE      ) AND
                      (KARRK-SYOSINYMD2    >=  SPA-SYOYMD )
                   MOVE    KARRK-SYOSINYMD2    TO  SPA-SYOYMD
                   MOVE    1                   TO  SPA-SYOSINFLG
               END-IF
      *        最終受診日
               IF     (KARRK-LASTYMD   NOT =   SPACE      ) AND
                      (KARRK-LASTYMD       >=  SPA-LSTYMD )
                   MOVE    KARRK-LASTYMD       TO  SPA-LSTYMD
                   MOVE    KARRK-SRYKA         TO  SPA-LSTSRYKA
               END-IF
      *
               END-IF
      *
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-RRK-LSTSRYKA    NOT =   SPACE
               MOVE    SPA-RRK-LSTSRYKA    TO  SPA-LSTSRYKA
           END-IF
      *    初診日、最終診察日編集
           IF      SPA-NYUGAIKBN       =   "1"
      *        入院時
               PERFORM 11011-NYU-SYOYMD-SET-SEC
           ELSE
      *        外来
               PERFORM 11012-GAI-SYOYMD-SET-SEC
           END-IF
      *
           .
       1101-HKN-SYOYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    外来初診日、最終診察日編集処理
      *****************************************************************
       11012-GAI-SYOYMD-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-TENKIYMD
           MOVE    SPACE               TO  WRK-CHIYUYMD
           MOVE    ZERO                TO  FLG-TENKI
           MOVE    ZERO                TO  FLG-CHIYU
      *----(01.00.01) LINE UPD START ----------------------------------
           MOVE    SPACE               TO  WRK-CHIYUYMD
      *----(01.00.01) LINE INS END   ----------------------------------
      *
      *    患者病名検索
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
           MOVE    SPA-SRYYMD          TO  PTBYO-TENKIYMD
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
           END-IF
           PERFORM UNTIL       FLG-PTBYOMEI    =   1
              IF      PTBYO-SRYYMD        <   SPA-SRYYMD
                   EVALUATE    PTBYO-TENKIKBN
      *                治癒
                       WHEN    "1"
                         IF      PTBYO-TENKIYMD  <   SPA-SRYYMD
                           MOVE    1               TO  FLG-CHIYU
      *----(01.00.01) LINE ADD START ----------------------------------
                           IF      PTBYO-TENKIYMD  >=  WRK-CHIYUYMD
                               MOVE    PTBYO-TENKIYMD  TO  WRK-CHIYUYMD
                           END-IF
                         ELSE
                           MOVE    1           TO  FLG-TENKI
                         END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *                継続
                       WHEN    " "
                           MOVE    1           TO  FLG-TENKI
      *                中止
                       WHEN    "3"
                           IF      PTBYO-TENKIYMD  <=  SPA-SRYYMD
                               IF      PTBYO-TENKIYMD  >=  WRK-TENKIYMD
                                   MOVE    PTBYO-TENKIYMD
                                                       TO  WRK-TENKIYMD
                               END-IF
                           ELSE
      *                        転帰日が未来日は継続とする
                               MOVE    1           TO  FLG-TENKI
                           END-IF
                   END-EVALUATE
               END-IF
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *----(01.00.01) LINE ADD START ----------------------------------
      *    治癒の最終日の判定（最終来院日より前なら治癒でない）
           IF      WRK-CHIYUYMD    NOT =   SPACE
               IF      WRK-CHIYUYMD        <   SPA-LSTYMD
                   MOVE    ZERO                TO  FLG-CHIYU
                   MOVE    SPA-LSTYMD          TO  WRK-CHIYUYMD
               END-IF
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    最終転帰日のチェック
           IF      WRK-TENKIYMD    NOT =   SPACE
      *        １月後算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-TENKIYMD        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      STS-DAY-RC1         =   ZERO
      *            1ヶ月以内なら継続中
                   IF      LNK-DAY6-KOYOMI     >=  SPA-SRYYMD
                       MOVE    1               TO  FLG-TENKI
                       MOVE    ZERO            TO  FLG-CHIYU
                   ELSE
      *D               MOVE    ZERO            TO  FLG-TENKI
                       IF     (WRK-CHIYUYMD        <=   WRK-TENKIYMD)
      *                最終日より前の時、初診算定へ
                       AND    (WRK-TENKIYMD        >=  SPA-LSTYMD )
                           MOVE    1               TO  FLG-CHIYU
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    初診料算定有無
           IF      SPA-SYOYMD          =   SPACE
      *            初診算定
               MOVE    1               TO  SPA-SYOSIN
           ELSE
               IF     (FLG-CHIYU       =   1   )  AND
                      (FLG-TENKI       =   ZERO)
      *            初診算定
                   MOVE    1                   TO  SPA-SYOSIN
               ELSE
      *            初診算定なし
                   MOVE    ZERO                TO  SPA-SYOSIN
               END-IF
           END-IF
      *
      *    初診料算定日が診療日の時、再診とする
           IF      SPA-SYOSIN          =   1
               IF     (SPA-SYOYMD          =   SPA-SRYYMD )
                   OR (SPA-SRYYMD          =   SPA-LSTYMD )
                   MOVE    ZERO            TO  SPA-SYOSIN
               END-IF
           END-IF
      *
      *    最初の初診算定日
           MOVE    SPACE               TO  SPA-SYOYMD1
      *    初診算定日を算定履歴から検索
           IF     (SPA-SYOYMD      NOT =   SPACE )
               PERFORM 3009-SYOSIN-SYORI-SEC
           ELSE
               MOVE    SPACE               TO  WRK-SYOSINYMD1
               MOVE    SPACE               TO  WRK-SYOSINYMDS
           END-IF
      *    初診算定日が診療日以降の時、直帰の初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOYMD          >   SPA-SRYYMD)
      *        前回の初診算定日
               MOVE    WRK-SYOSINYMD1      TO  SPA-SYOYMD-OLD
           ELSE
               MOVE    SPACE               TO  SPA-SYOYMD-OLD
           END-IF
      *Ｈ２０．４の労災初診改正対応
      *    初診診算定日＞Ｈ２０．４．１
           MOVE    SPACE               TO  SPA-SYOYMD-RSI
           MOVE    SPACE               TO  SPA-SYOYMD-HKN
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOYMD          >=  "20080401" )
               IF      WRK-RSI-YMD         >=  "20080401"
                   MOVE    WRK-RSI-YMD         TO  SPA-SYOYMD-RSI
                   MOVE    WRK-SYO-YMD         TO  SPA-SYOYMD-HKN
               END-IF
           END-IF
      *
      *    同日初診算定日
           IF      SPA-SRYYMD          >=  "20060401"
               PERFORM 30091-DOUSYOSIN-SYORI-SEC
           ELSE
               INITIALIZE              SPA-DOUSYOSIN-AREA
               MOVE    SPACE           TO  SPA-DOUYMD
           END-IF
      *
           .
       11012-GAI-SYOYMD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院時初診日、最終診察日編集処理
      *****************************************************************
       11011-NYU-SYOYMD-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-TENKIYMD
           MOVE    ZERO                TO  FLG-TENKI
           MOVE    ZERO                TO  FLG-CHIYU
           MOVE    SPACE               TO  WRK-CHIYUYMD
      *
      *    患者病名検索
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-K02N-NYUINYMD   TO  PTBYO-SRYYMD
           MOVE    SPA-K02N-NYUINYMD   TO  PTBYO-TENKIYMD
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
           END-IF
           PERFORM UNTIL       FLG-PTBYOMEI    =   1
      *       入院日より前の開始であること
              IF      PTBYO-SRYYMD        <   SPA-K02N-NYUINYMD
                   EVALUATE    PTBYO-TENKIKBN
      *                治癒
                       WHEN    "1"
      *                    治癒日が入院日前
                           IF      PTBYO-TENKIYMD  <   SPA-K02N-NYUINYMD
                               MOVE    1               TO  FLG-CHIYU
                               IF      PTBYO-TENKIYMD  >=  WRK-CHIYUYMD
                                   MOVE    PTBYO-TENKIYMD
                                                       TO  WRK-CHIYUYMD
                               END-IF
                           ELSE
                               MOVE    1               TO  FLG-TENKI
                           END-IF
      *                継続
                       WHEN    " "
                           MOVE    1               TO  FLG-TENKI
      *                中止
                       WHEN    "3"
                           IF      PTBYO-TENKIYMD  <=  SPA-K02N-NYUINYMD
                               IF      PTBYO-TENKIYMD  >=  WRK-TENKIYMD
                                   MOVE    PTBYO-TENKIYMD  TO
                                                           WRK-TENKIYMD
                               END-IF
                           ELSE
      *                        転帰日が入院日以降は継続とする
                               MOVE    1           TO  FLG-TENKI
                           END-IF
                   END-EVALUATE
               END-IF
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最終転帰日のチェック
           IF      WRK-TENKIYMD    NOT =   SPACE
      *        １月後算定
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY6-AREA
               MOVE    "61"                TO  LNK-DAY6-IRAI
               MOVE    WRK-TENKIYMD        TO  LNK-DAY6-KIJUN
               MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
               MOVE    1                   TO  LNK-DAY6-ZOGEN
               CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                   LNK-DAY6-AREA
               IF      STS-DAY-RC1         =   ZERO
      *            1ヶ月以内なら継続中
                   IF      LNK-DAY6-KOYOMI     >=  SPA-K02N-NYUINYMD
                       MOVE    1               TO  FLG-TENKI
                       MOVE    ZERO            TO  FLG-CHIYU
                   ELSE
                       IF      WRK-CHIYUYMD        <   WRK-TENKIYMD
      *                入院日より前の時、初診算定へ
      ***              AND    (WRK-TENKIYMD        <  SPA-K02N-NYUINYMD)
                           MOVE    1               TO  FLG-CHIYU
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    初診料算定有無
           IF      SPA-SYOYMD          =   SPACE
      *            初診算定
               MOVE    1               TO  SPA-SYOSIN
           ELSE
               IF     (FLG-CHIYU       =   1   )  AND
                      (FLG-TENKI       =   ZERO)
      *            初診算定
                   MOVE    1                   TO  SPA-SYOSIN
               ELSE
      *            初診算定なし
                   MOVE    ZERO                TO  SPA-SYOSIN
               END-IF
           END-IF
      *
      *    初診料算定日が入院日
           IF      SPA-SYOSIN          =   1
               IF      SPA-SYOYMD          >=  SPA-K02N-NYUINYMD
                   MOVE    ZERO            TO  SPA-SYOSIN
               END-IF
           END-IF
      *
           .
       11011-NYU-SYOYMD-SET-EXT.
           EXIT.
      *    
      *
      *****************************************************************
      *    労災保険 初診日、最終診察日編集処理
      *****************************************************************
      *1102-RSI-SYOYMD-SET-SEC             SECTION.
      *
      *    該当の保険のみ受診歴を検索する
      *    MOVE    SPACE               TO  JYURRK-REC
      *    INITIALIZE                  JYURRK-KEY
      *    MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
      *    MOVE    SPA-PTID            TO  JYURRK-PTID
      *    MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *    MOVE    SPA-GMN-HKNCOMBINUM TO  JYURRK-HKNCOMBINUM
      *
      *    MOVE    "JYURRK-KEY15"      TO  WRK-PATH
      *
      *    MOVE    JYURRK-REC          TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    WRK-PATH            TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    WRK-PATH            TO  ORC-DBPATH
      *        PERFORM 970-JYURRK-READ-SEC
      *    ELSE
      *        MOVE    SPACE               TO  JYURRK-REC
      *        INITIALIZE                  JYURRK-REC
      *        MOVE    1               TO  FLG-JYURRK
      *    END-IF
      *    PERFORM
      *            UNTIL    FLG-JYURRK     =   1
      *        初診算定日
      *        IF     (SPA-SYOYMD          =   SPACE      )  OR
      *               (JYURRK-SRYYMD       <   SPA-SYOYMD )
      *            MOVE    JYURRK-SRYYMD       TO  SPA-SYOYMD
      *        END-IF
      *        最終受診日
      *        IF      JYURRK-SRYYMD       >=  SPA-LSTYMD
      *            MOVE    JYURRK-SRYYMD       TO  SPA-LSTYMD
      *            MOVE    JYURRK-SRYKA        TO  SPA-LSTSRYKA
      *        END-IF
      *        同日再診のチェック
      *        IF      SPA-SRYYMD          =   JYURRK-SRYYMD
      *            IF      SPA-GMN-SRYKA   NOT =   JYURRK-SRYKA
      *                MOVE    JYURRK-SRYYMD   TO  SPA-RRK-LSTYMD
      *                MOVE    JYURRK-SRYKA    TO  SPA-RRK-LSTSRYKA
      *            END-IF
      *        END-IF
      *
      *        MOVE    WRK-PATH            TO  ORC-DBPATH
      *        PERFORM 970-JYURRK-READ-SEC
      *    END-PERFORM
      *
      *    初診料算定有無
      *    IF      SPA-SYOYMD          =   SPACE
      *            初診算定
      *        MOVE    1                   TO  SPA-SYOSIN
      *    ELSE
      *            初診算定なし
      *        MOVE    ZERO                TO  SPA-SYOSIN
      *    END-IF
      *
      *    .
      *1102-RSI-SYOYMD-SET-EXT.
      *****EXIT.
      *
      *****************************************************************
      *    未収金額編集処理
      *****************************************************************
       49011-MISYU-HEN-SEC             SECTION.
           MOVE    ZERO                TO  SPA-MISYUMONEY
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key42"             TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               COMPUTE SPA-MISYUMONEY  =   SYU-SKYMONEY
                                       -   SYU-NYUKIN-TOTAL
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key42"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       49011-MISYU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    特定疾患の有無検索編集処理
      *****************************************************************
       11011-TOKUTEI-SYORI-SEC         SECTION.
      *
           MOVE    ZERO                TO  SPA-TOKUTEI
           MOVE    ZERO                TO  SPA-TOKUTEI-F
           MOVE    ZERO                TO  SPA-TOKUTEIFLG
           MOVE    ZERO                TO  SPA-CHOKITOFLG
      *    特定疾患科テーブルより設定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )   OR
                              (SPA-TOKU-SRYKA (IDX)    =   SPACE)
               IF      SPA-TOKU-SRYKA (IDX)    =   SPA-SRYKA
                   IF      SPA-TOKU-FLG(IDX)   =   05
                                               OR  08
      *                特定疾患の有無
                       MOVE    1                   TO  SPA-TOKUTEI
                   ELSE
      *                皮膚科特定疾患の有無
                       MOVE    SPA-TOKU-FLG(IDX)   TO  SPA-TOKUTEI-F
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       11011-TOKUTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名編集処理
      *****************************************************************
       11012-BYOMEI-HENSYU-SEC             SECTION.
      *
      *    病名内容
           INITIALIZE                  SPA-GMN-BYOMEITBL
           INITIALIZE                  WRK-BYOMEI-AREA
      *    特定疾患科
           INITIALIZE                  SPA-TOKUTEI-AREA-G
      *
      *    特定疾患の有無検索
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYKA           TO  PTBYO-SRYKA
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
           END-IF
      *
           MOVE    ZERO                TO  IDX-B
           PERFORM UNTIL      (FLG-PTBYOMEI    =   1 )
      *H30.8             OR   (IDX-B           >=  100)
      *
      *H30.8   テーブル編集
               IF      IDX-B           >=  999
      *            テーブルオーバー
                   CONTINUE
               ELSE
      *
               ADD     1               TO  IDX-B
               MOVE    PTBYO-SRYKA     TO  WRK-BYOMEI-NO(IDX-B)
      ***      MOVE    PTBYO-BYOMEI    TO  WRK-BYOMEI (IDX-B)
               PERFORM 110121-ORCSMKBYOMEI-SEC
               MOVE    WRK-BYOMEI-HEN  TO  WRK-BYOMEI (IDX-B)
      *        疾患区分
               IF      PTBYO-MANSEIKBN NOT =   ZERO
                   MOVE    SPACE           TO  WRK-BYOMEI-HEN
                   STRING  "◎"                DELIMITED   BY  SIZE
                           WRK-BYOMEI (IDX-B)  DELIMITED   BY  SPACE
                                   INTO    WRK-BYOMEI-HEN
                   END-STRING
                   MOVE    WRK-BYOMEI-HEN  TO  WRK-BYOMEI (IDX-B)
               END-IF
      *        疑い区分
               IF      PTBYO-UTAGAIFLG NOT =   SPACE
                   MOVE    SPACE           TO  WRK-BYOMEI-HEN
                   STRING  "△"                DELIMITED   BY  SIZE
                           WRK-BYOMEI (IDX-B)  DELIMITED   BY  SPACE
                                   INTO    WRK-BYOMEI-HEN
                   END-STRING
                   MOVE    WRK-BYOMEI-HEN  TO  WRK-BYOMEI (IDX-B)
               END-IF
      *        主病名フラグ
               IF      PTBYO-SYUBYOFLG     =   "1"
                   MOVE    SPACE           TO  WRK-BYOMEI-HEN
                   STRING  "□"                DELIMITED   BY  SIZE
                           WRK-BYOMEI (IDX-B)  DELIMITED   BY  SPACE
                                   INTO    WRK-BYOMEI-HEN
                   END-STRING
                   MOVE    WRK-BYOMEI-HEN  TO  WRK-BYOMEI (IDX-B)
               END-IF
      *H28.12
      *        レセプト表示フラグ
               IF      PTBYO-REZEFLG       =   "1"
                   MOVE    "1"             TO  WRK-REZEFLG (IDX-B)
               END-IF
      *        レセプト表示月数
               IF      PTBYO-REZEMM    NOT =   ZERO
                   PERFORM 110123-BYOMEI-KIKAN-SEC
               END-IF
      *H30.8
               END-IF
      *
      *        特定疾患の有無
               IF     (PTBYO-SRYYMD        <=   SPA-SRYYMD )
      *            疑い病名は対象外とする
                 AND  (PTBYO-UTAGAIFLG  NOT =   "1"  AND  "3" )
                   IF      PTBYO-MANSEIKBN     =   05
                                               OR  03
                                               OR  04
                                               OR  08
                                               OR  09
                       IF     (PTBYO-TENKIKBN      =   SPACE )  OR
                              (PTBYO-TENKIYMD      >=  SPA-SRYYMD)
                           PERFORM 110111-MANSEIKBN-SET-SEC
                        END-IF
                   END-IF
               END-IF
      *        開始日＜当日
               IF      PTBYO-SRYYMD        <   SPA-SRYYMD
                   MOVE    1               TO  SPA-BYOMEI-STR
               END-IF
      *
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key25"             TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key25"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    病名表示なし
           IF      SPA-BYOHYOKBN-FLG   =   ZERO
               MOVE    ZERO                TO  SPA-BYOMEI-MAX
           ELSE
      *
           MOVE    ZERO                TO  FLG-BYONO
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-B   )   OR
                              (SPA-BYOMEI-MAX  >=  100)
      *H28.12
      *      レセプト表示外の表示なし
             IF     (SPA-BYOHYOKBN-FLG   =   "2" )
               AND  (WRK-REZEFLG (IDX)   =   "1" )
                 MOVE    1               TO  FLG-BYONO
             ELSE
      *        病名編集
               MOVE    ZERO            TO  FLG-BYOMEI-END
               MOVE    1               TO  IDX-B2
               PERFORM
                       UNTIL  (FLG-BYOMEI-END  =   1  )  OR
                              (SPA-BYOMEI-MAX  >=  100)
                   IF     (IDX-B2  +   22)     >   164
                       COMPUTE IDX-B3          =   164   -   IDX-B2
                   ELSE
                       MOVE    22              TO  IDX-B3
                   END-IF
                   ADD     1               TO  SPA-BYOMEI-MAX
                   MOVE    WRK-BYOMEI(IDX)(IDX-B2:IDX-B3)
                                           TO  SPA-GMN-BYOMEI
                                                    (SPA-BYOMEI-MAX)
                   IF      IDX-B2          =   1
                       MOVE    WRK-BYOMEI-NO(IDX)
                                           TO  SPA-GMN-BYOMEI-NO
                                                    (SPA-BYOMEI-MAX)
                       PERFORM VARYING  IDX-K   FROM   1   BY  1
                               UNTIL    IDX-K      >   SPA-SRYKA-MAX
                           IF     SPA-GMN-SRYKAL(IDX-K)   =
                                                   WRK-BYOMEI-NO(IDX)
                               MOVE    SPA-GMN-SRYKAMEIL(IDX-K)(1:2)
                                               TO  SPA-GMN-BYOMEI-NO
                                                    (SPA-BYOMEI-MAX)
                               MOVE    SPA-SRYKA-MAX   TO  IDX-K
                           END-IF
                       END-PERFORM
                   END-IF
                   COMPUTE IDX-B2      =   IDX-B2  +   22
                   IF     (IDX-B2              >   164     )  OR
                          (WRK-BYOMEI(IDX)(IDX-B2:)    =   SPACE)
                       MOVE    1           TO  FLG-BYOMEI-END
                   END-IF
               END-PERFORM
      *H28.12
             END-IF
           END-PERFORM
      *
           END-IF
      *
           IF      SPA-BYOMEI-MAX      >   100
               MOVE    100             TO  SPA-BYOMEI-MAX
           END-IF
      *H28.12
           IF      SPA-BYOMEI-MAX      <   100
      *        レセプト表示なし
               IF     (SPA-BYOHYOKBN-FLG   =   "2" )
                 AND  (FLG-BYONO           =   1   )
                   ADD     1               TO  SPA-BYOMEI-MAX
                   MOVE    "＜非表示病名あり＞"
                                           TO  SPA-GMN-BYOMEI
                                                    (SPA-BYOMEI-MAX)
               END-IF
           END-IF
      *
           .
       11012-BYOMEI-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    病名補助コメント編集処理
      *****************************************************************
       110121-ORCSMKBYOMEI-SEC          SECTION.
      *
           INITIALIZE                  ORCSMKBYOMEIAREA
           MOVE    SPACE               TO  SMKBYO-UTAGAIFLG
           MOVE    PTBYO-BYOMEI        TO  SMKBYO-BYOMEI
           MOVE    PTBYO-HOSOKU-COMMENT    TO  SMKBYO-HOSOKU-COMMENT
           CALL    "ORCSMKBYOMEI"      USING
                                           ORCSMKBYOMEIAREA
           MOVE    SMKBYO-BYOMEI       TO  WRK-BYOMEI-HEN
           .
       110121-ORCSMKBYOMEI-EXT.
           EXIT.
      *H28.12
      *****************************************************************
      *    レセプト表示月数チェック処理
      *****************************************************************
       110123-BYOMEI-KIKAN-SEC              SECTION.
      *
           MOVE    PTBYO-SRYYMD        TO  WRK-BYOYMD
           COMPUTE WRK-BYO-TUKI        =   WRK-BYO-MM
                                       +   PTBYO-REZEMM
           IF      WRK-BYO-TUKI        <=  12
               MOVE    WRK-BYO-TUKI        TO  WRK-BYO-MM
           END-IF
           PERFORM UNTIL    WRK-BYO-TUKI   <=  12
               COMPUTE WRK-BYO-TUKI    =   WRK-BYO-TUKI
                                       -   12
               MOVE    WRK-BYO-TUKI        TO  WRK-BYO-MM
               COMPUTE WRK-BYO-YY      =   WRK-BYO-YY
                                       +   1
           END-PERFORM
      *    レセプト表示期間外
           IF      WRK-BYOYYMM         <=  SPA-SRYYMD(1:6)
               MOVE    "1"             TO  WRK-REZEFLG (IDX-B)
           END-IF
           .
       110123-BYOMEI-KIKAN-EXT.
           EXIT.
      *****************************************************************
      *    診療科毎の特定疾患編集処理
      *****************************************************************
       110111-MANSEIKBN-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-ATOPII
      *    皮膚科特定疾患（２）の病名チェック
           IF      PTBYO-MANSEIKBN     =   04
               PERFORM 110111-ATOPIICHEK-SEC
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   10  )
               IF      SPA-TOKU-SRYKA (IDX)    =   SPACE
                   MOVE    PTBYO-SRYKA     TO  SPA-TOKU-SRYKA (IDX)
                   MOVE    PTBYO-MANSEIKBN TO  SPA-TOKU-FLG   (IDX)
      *            主病名フラグ
                   MOVE    PTBYO-SYUBYOFLG TO  WRK-SYUBYOFLG (IDX)
                   IF      PTBYO-MANSEIKBN     =   05
                                               OR  08
                       MOVE    PTBYO-MANSEIKBN TO  SPA-TOKU05-FLG(IDX)
                   END-IF
      *            アトピー性皮膚フラグ
                   MOVE    FLG-ATOPII      TO  SPA-ATOPII (IDX)
                   MOVE    10              TO  IDX
               ELSE
                   IF      SPA-TOKU-SRYKA (IDX)    =   PTBYO-SRYKA
      *                疾患区分優先設定
                       PERFORM 110111-MANSEIKBN-CHK-SEC
                       MOVE    10              TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       110111-MANSEIKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    疾患区分優先編集処理
      *****************************************************************
       110111-MANSEIKBN-CHK-SEC          SECTION.
      *
           EVALUATE    SPA-TOKU-FLG (IDX)
           WHEN    09
      *        難病外来
               IF      WRK-SYUBYOFLG (IDX) =   SPACE
      *            難病外来が主病以外で特定疾患が主病
                   IF      (PTBYO-MANSEIKBN    =   05
                                               OR  08   )
                      AND  (PTBYO-SYUBYOFLG    =  "1"   )
                       MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU-FLG(IDX)
                       MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG
                                                                   (IDX)
                   END-IF
                   IF      (PTBYO-MANSEIKBN     =   09 )
                      AND  (PTBYO-SYUBYOFLG     =  "1" )
                       MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG
                                                                   (IDX)
                   END-IF
               END-IF
           WHEN    05
           WHEN    08
      *        特定疾患
               IF     (PTBYO-MANSEIKBN     =   05
                                           OR  08 )
                 AND  (PTBYO-SYUBYOFLG     =  "1" )
      *            主病でないなら、０５を優先
                   IF     (PTBYO-MANSEIKBN     =   05    )  AND
                          (WRK-SYUBYOFLG (IDX) =   SPACE )
                       MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU-FLG(IDX)
                   END-IF
                   MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG (IDX)
               END-IF
               IF     (PTBYO-MANSEIKBN     =   09    ) 
      *            特定疾患が主病以外か難病であれば変更
                   IF      (WRK-SYUBYOFLG (IDX)    =   SPACE )
                      OR   (PTBYO-SYUBYOFLG        =  "1"    )
                       MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU-FLG(IDX)
                       MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG
                                                                   (IDX)
                   END-IF
               END-IF
           WHEN    03
      *        皮膚科特定（１）
               IF      PTBYO-MANSEIKBN     =   05
                                           OR  08
                                           OR  09
                   MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU-FLG  (IDX)
                   MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG (IDX)
               END-IF
           WHEN    04
      *        皮膚科特定（２）
               IF      PTBYO-MANSEIKBN     =   03
                                           OR  05
                                           OR  08
                                           OR  09
                   MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU-FLG  (IDX)
                   MOVE    PTBYO-SYUBYOFLG     TO  WRK-SYUBYOFLG (IDX)
                   MOVE    ZERO                TO  SPA-ATOPII (IDX)
               END-IF
      *
      *        アトピー以外の皮膚科特定疾患（２）あり
               IF      PTBYO-MANSEIKBN     =   04
                   IF      FLG-ATOPII          =   ZERO
                       MOVE    ZERO            TO  SPA-ATOPII (IDX)
                   END-IF
               END-IF
           END-EVALUATE
      *    特定疾患
           IF      PTBYO-MANSEIKBN     =   05
                                       OR  08
      *        05 を優先
               IF      SPA-TOKU05-FLG(IDX)     =   05
                   CONTINUE
               ELSE
                   MOVE    PTBYO-MANSEIKBN     TO  SPA-TOKU05-FLG(IDX)
               END-IF
           END-IF
      *
           .
       110111-MANSEIKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    皮膚科特定疾患（２）の病名チェック処理
      *****************************************************************
       110111-ATOPIICHEK-SEC             SECTION.
      *
      *    基本病名コード
           IF      PTBYO-KHNBYOMEICD   =   "6918001"
                                       OR  "6918002"
                                       OR  "6918024"
                                       OR  "8830277"
      *        アトピー性皮膚炎
               MOVE    1               TO  SPA-ATOPII-FLG
               MOVE    1               TO  FLG-ATOPII
           END-IF
           .
       110111-ATOPIICHEK-EXT.
           EXIT.
      *****************************************************************
      *    入院ＳＰＡ編集、ワーク診療行為作成処理
      *****************************************************************
       500-NYUIN-SPASET-SEC              SECTION.
      *
      *?   MOVE    SPACE               TO  SPA-HKNCOMBINUM
      *?   MOVE    ZERO                TO  SPA-FTNRATE
      *?   MOVE    SPACE               TO  SPA-FTNRATEMEI
           MOVE    ZERO                TO  FLG-UKETUKE-ARI
      *
           IF      LNK-SC95-PG         =   "K02"  OR  "K01"
               CONTINUE
           ELSE
               MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           END-IF
      *
      *    前回患者保存
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
           MOVE    SPA-PTNUM           TO  SPA-LAST-PTNUM
      *
           PERFORM 110-K02SYOSIN-SEC
      *    初期編集時
           IF      LNK-SC95-KBN        =   "5"
      *        受診履歴一覧編集
               MOVE    ZERO                TO  SPA-RRK-PAGE
               PERFORM 2002-JYURRK-SET-SEC
      *H29.5
      *        公費期限切れ判定
               PERFORM 2009-KOHENDCHK-SEC
           END-IF
      *
      *    受付マスタより、診療科、ドクターの決定
           IF     (SPA-GMN-SRYKA       =   SPACE      ) AND
                  (SPA-SYSYMD          =   SPA-SRYYMD ) AND
                  (SPA-SYORIFLG        =   ZERO       )
               PERFORM 10014-UKETUKE-CHK-SEC
           END-IF
      *
      *    中途終了からの場合、ドクターを受付から決定する
           IF     (LNK-SC95-PG         =   "K10" )  AND
                  (SPA-DRCD            =   SPACE )
               PERFORM 10014-UKETUKE-CHK-SEC
           END-IF
      *
      *    診療科、ドクター名称編集
           PERFORM 10012-SYSKAN-SEC
      *
      *    保険組合せ名称編集
           PERFORM 10013-HKNCOMBI-SEC
      *
      *    病名編集
      *!!  PERFORM 11012-BYOMEI-HENSYU-SEC
      *    特定疾患の有無検索
           PERFORM 11011-TOKUTEI-SYORI-SEC
      *
           PERFORM 200-WKSRYACT-SYORI-SEC
           .
       500-NYUIN-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為作成処理
      *****************************************************************
       200-WKSRYACT-SYORI-SEC              SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      ***  OPEN    OUTPUT              PARA-FILE
      *    一時ファイル削除
           PERFORM 8001-PARA-DELETE-SEC
      *
      *    排他中は作成無し
           IF      SPA-LOCK            =   ZERO
      *
      *    ワーク診療行為データから作成 
           PERFORM 4902-WKBUNSAN-SEC
      *
      *    前回処方かつ、分散入力がなかったとき、
      *    診療診療行為マスタより作成
           IF     (FLG-WKSYORI         =   ZERO )  AND
                  (SPA-ZENSYOHOFLG     =   "0"  )  AND
      *           新規の時のみ
                  (SPA-SYORIFLG        =   0    )  AND
      *           再診の時のみ
                  (SPA-SYOSIN          =   0    )
               PERFORM 4901-PARA-OUT-SEC
      *        ワーク診療行為作成有無
               IF     LNK-SC95-WKSRYACT    =   1
                   MOVE    2               TO  LNK-SC95-WKSRYACT
               END-IF 
           END-IF
      *
           END-IF
      *
      *****CLOSE                       PARA-FILE
      *
      *    前回患者番号のセット
           MOVE    SPA-PTNUM           TO  SPA-LAST-PTNUM
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
      *
          .
       200-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正ワーク診療行為作成処理
      *****************************************************************
       300-WKSRYACT-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-GOK-TENSU
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    OPEN    OUTPUT              PARA-FILE
      *    一時ファイル削除
           PERFORM 8001-PARA-DELETE-SEC
      *
           PERFORM 4901-PARA-OUT-SEC
      *
      **** CLOSE                       PARA-FILE
      *
           MOVE    WRK-GOK-TENSU       TO  LNK-SC95-GOK-TENSU
      *!
      *    システム管理からの設定
           CALL    "ORCSC95SUB"        USING
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
      *!
      *    患者情報の再セット
           PERFORM 1001-SPA-JYUSIN-SEC
      *
      *    病名編集
           PERFORM 11012-BYOMEI-HENSYU-SEC
      *
      *    特定疾患の有無検索
           PERFORM 11011-TOKUTEI-SYORI-SEC
      *
      *    訂正日の初診算定日
           PERFORM 3009-SYOSIN-SYORI-SEC
      *    前回の初診算定日
           MOVE    WRK-SYOSINYMD1      TO  SPA-SYOYMD-OLD
      *    最初の初診算定日
           MOVE    WRK-SYOSINYMDS      TO  SPA-SYOYMD1
      *
      *Ｈ２０．４の労災初診改正対応
      *    初診診算定日＞Ｈ２０．４．１
           MOVE    SPACE               TO  SPA-SYOYMD-RSI
           MOVE    SPACE               TO  SPA-SYOYMD-HKN
           IF     (SPA-SYOYMD-OLD  NOT =   SPACE )  AND
                  (SPA-SYOYMD-OLD      >=  "20080401" )
               IF      WRK-RSI-YMD         >=  "20080401"
                   MOVE    WRK-RSI-YMD         TO  SPA-SYOYMD-RSI
                   MOVE    WRK-SYO-YMD         TO  SPA-SYOYMD-HKN
               END-IF
           END-IF
      *H28.10
      *    要介護対象
           PERFORM 110111-YOUKAIGO-SET-SEC
      *
      *    同日初診算定日
           IF      SPA-SRYYMD          >=  "20060401"
               PERFORM 30091-DOUSYOSIN-SYORI-SEC
           ELSE
               INITIALIZE              SPA-DOUSYOSIN-AREA
               MOVE    SPACE           TO  SPA-DOUYMD
           END-IF
      *    保険組合せが使用できるかのチェック
      *****PERFORM 100131-HKNCOMBI-CHK-SEC
      *
      *    継続のチェック
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    SPA-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUNOU          =   ZERO
      *        伝票状態区分
               MOVE    SYU-CONTKBN         TO  SPA-CONTKBN
               MOVE    SYU-DENPJTIKBN      TO  SPA-DENPJTIKBN
               MOVE    SYU-DOUJI-DENPNUM   TO  SPA-DOUJI-DENPNUM
           END-IF
      *
      *    診療月の累計点数編集
      *****PERFORM 49012-ZAITENTOTAL-SEC
      *
          .
       300-WKSRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日の初診算定日編集
      *****************************************************************
       3009-SYOSIN-SYORI-SEC               SECTION.
      *
      *    初診料の算定履歴のチェック
           MOVE    SPACE               TO  WRK-SYOSINYMD1
           MOVE    SPACE               TO  WRK-SYOSINYMDS
           PERFORM 300911-SRYCD-SYOSIN-SEC
      *
      *    最終算定日
           IF     (WRK-SYO-YMD     NOT =   SPACE  )
               MOVE    WRK-SYO-YMD         TO  WRK-SYOSINYMD1
           END-IF
           IF     (WRK-RSI-YMD     NOT =   SPACE  )
             AND  (WRK-RSI-YMD         >   WRK-SYOSINYMD1 )
               MOVE    WRK-RSI-YMD         TO  WRK-SYOSINYMD1
           END-IF
      *    最終月の初回
           IF     (WRK-SYO-YMD1    NOT =   SPACE  )
               MOVE    WRK-SYO-YMD1        TO  WRK-SYOSINYMDS
           END-IF
           IF     (WRK-RSI-YMD1    NOT =   SPACE  )
             AND  (WRK-RSI-YMD1        <   WRK-SYOSINYMD1 )
               MOVE    WRK-RSI-YMD1        TO  WRK-SYOSINYMDS
           END-IF
      *
          .
       3009-SYOSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定日検索処理
      *****************************************************************
       300911-SRYCD-SYOSIN-SEC         SECTION.
      *
           INITIALIZE                  WRK-SYOSINYMD-AREA
      *
      *    初診料（Ｈ１８．４から共通） 老人外
           MOVE    "111000110"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
           END-IF
      *H25.4 
      *    初診料（紹介状なし）（Ｈ２５．４から）
           MOVE    "111012510"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                  MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *
      *H27.1 
      *    初診料（妥結率５割以下）
           IF      SPA-SRYYMD          >=  "20150101"
               MOVE    "111012710"         TO  WRK-SRYCD
               PERFORM 30091-SANTEI-CHK-SEC
               IF      WRK-YMDS        NOT =   SPACE
                   IF      WRK-YMDS        >   WRK-SYO-YMD
                       MOVE    WRK-YMDS        TO  WRK-SYO-YMD
                   END-IF
                   IF     (WRK-SYO-YMD1    =   SPACE        )
                   OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                      MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
                   END-IF
               END-IF
           END-IF
      *
      *    小児科診察料
      *    院外処方   初診
           MOVE    "113003510"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                   MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *    院内処方   初診
           MOVE    "113003710"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                   MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *H28.4
      *    小児かかりつけ
      *    院外処方   初診
           MOVE    "113019710"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                   MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *    院内処方   初診
           MOVE    "113019910"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                   MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *
      *    仮の初診料
           MOVE    "099110001"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               IF      WRK-YMDS        >   WRK-SYO-YMD
                   MOVE    WRK-YMDS        TO  WRK-SYO-YMD
               END-IF
               IF     (WRK-SYO-YMD1    =   SPACE        )
               OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                   MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
               END-IF
           END-IF
      *
      *    労災初診料
           MOVE    "101110010"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF      WRK-YMDS        NOT =   SPACE
               MOVE    WRK-YMDS        TO  WRK-RSI-YMD
               MOVE    WRK-YMDS-1      TO  WRK-RSI-YMD1
           END-IF
      *    初診算定日が当日で前回初診算定日がない時、改正前のコード検索
           IF     (WRK-SYO-YMD     NOT =   WRK-SYO-YMD1 )
            AND   (WRK-SYO-YMD     NOT =   SPACE        )
            AND   (WRK-SYO-YMD         <   SPA-SRYYMD   )
               CONTINUE
           ELSE
      *        病院（老人）
               MOVE    "111700110"         TO  WRK-SRYCD
               PERFORM 30091-SANTEI-CHK-SEC
               IF      WRK-YMDS        NOT =   SPACE
                   IF      WRK-YMDS        >   WRK-SYO-YMD
                       MOVE    WRK-YMDS        TO  WRK-SYO-YMD
                   END-IF
                   IF     (WRK-SYO-YMD1    =   SPACE        )
                   OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                       MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
                   END-IF
               END-IF
      *        診療所（老人）
               MOVE    "111700210"         TO  WRK-SRYCD
               PERFORM 30091-SANTEI-CHK-SEC
               IF      WRK-YMDS        NOT =   SPACE
                   IF      WRK-YMDS        >   WRK-SYO-YMD
                       MOVE    WRK-YMDS        TO  WRK-SYO-YMD
                   END-IF
                   IF     (WRK-SYO-YMD1    =   SPACE        )
                   OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                       MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
                   END-IF
               END-IF
      *        診療所（老人外）
               MOVE    "111003610"         TO  WRK-SRYCD
               PERFORM 30091-SANTEI-CHK-SEC
               IF      WRK-YMDS        NOT =   SPACE
                   IF      WRK-YMDS        >   WRK-SYO-YMD
                       MOVE    WRK-YMDS        TO  WRK-SYO-YMD
                   END-IF
                   IF     (WRK-SYO-YMD1    =   SPACE        )
                   OR     (WRK-YMDS-1      <   WRK-SYO-YMD1 )
                       MOVE    WRK-YMDS-1      TO  WRK-SYO-YMD1
                   END-IF
               END-IF
           END-IF
           .
       300911-SRYCD-SYOSIN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定履歴の算定日編集処理
      *****************************************************************
       30091-SANTEI-CHK-SEC               SECTION.
      *
           MOVE    SPACE               TO  WRK-YMD
           MOVE    SPACE               TO  WRK-YMDS
           MOVE    SPACE               TO  WRK-YMDS-1
      *
      *    算定履歴検索
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        終了判定
               IF     (SANTEI-MSANTEICNT   >   ZERO )
                 AND  (SANTEI-SRYYM        <=  SPA-SRYYMD(1:6) )
      *            診療日以前の算定日
                   IF      SANTEI-SRYYM    =   SPA-SRYYMD(1:6)
                       MOVE    SPA-SRYYMD(7:2) TO  IDX-Y3
                   ELSE
                       MOVE    31              TO  IDX-Y3
                   END-IF
                   MOVE    ZERO                TO  WRK-DD
                   MOVE    ZERO                TO  WRK-DD-1
                   PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                           UNTIL       IDX-Y2      >   IDX-Y3
                       IF      SANTEI-DAY (IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           IF      WRK-DD-1        =   ZERO
                               MOVE    IDX-Y2      TO  WRK-DD-1
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              >   ZERO
                       MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                       IF     (WRK-YMDS            =   SPACE )  OR
                              (WRK-YMD             >   WRK-YMDS)
                           MOVE    WRK-YMD         TO  WRK-YMDS
                       END-IF
                       MOVE    WRK-DD-1            TO  WRK-DD
                       IF     (WRK-YMDS-1          =   SPACE )  OR
                              (WRK-YMD             <   WRK-YMDS-1)
                           MOVE    WRK-YMD         TO  WRK-YMDS-1
                       END-IF
      *                訂正時はすべて
                       IF      SPA-SYORIFLG        =   ZERO
                           MOVE    1               TO  FLG-SANTEI
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
          .
       30091-SANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日初診算定日編集処理
      *****************************************************************
       30091-DOUSYOSIN-SYORI-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOSINYMD1
      *    同日初診
           MOVE    "111011810"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMDS            >   WRK-SYOSINYMD1)
               MOVE    WRK-YMDS            TO  WRK-SYOSINYMD1
           END-IF
      *H25.4   紹介なし
      *    同日初診（紹介状なし）（Ｈ２５．４から）
      *    （H27.1　初診（同一日２科目 減点規定該当）
           MOVE    "111012610"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMDS            >   WRK-SYOSINYMD1)
               MOVE    WRK-YMDS            TO  WRK-SYOSINYMD1
           END-IF
      *
      *H28.4  初診（同一日　妥結率５割以下）
           MOVE    "111012810"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMDS            >   WRK-SYOSINYMD1)
               MOVE    WRK-YMDS            TO  WRK-SYOSINYMD1
           END-IF
      *
      *    同日初診（労災）
           MOVE    "101110040"         TO  WRK-SRYCD
           PERFORM 30091-SANTEI-CHK-SEC
           IF     (WRK-SYOSINYMD1      =   SPACE  )  OR
                  (WRK-YMDS            >   WRK-SYOSINYMD1)
               MOVE    WRK-YMDS            TO  WRK-SYOSINYMD1
           END-IF
           MOVE    WRK-SYOSINYMD1      TO  SPA-DOUYMD
      *
      *    同日初診の診療科設定
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD-1
           IF      WRK-SRYMM           =   1
               MOVE    12              TO  WRK-SRYMM
               COMPUTE WRK-SRYYY       =   WRK-SRYYY   -   1
           ELSE
               COMPUTE WRK-SRYMM       =   WRK-SRYMM   -   1
           END-IF
           INITIALIZE              SPA-DOUSYOSIN-AREA
           IF      (SPA-DOUYMD     NOT =   SPACE )  AND
                   (SPA-DOUYMD(1:6)    >=  WRK-SRYYM-1 )
               IF     ( SPA-DOUYMD         >   SPA-SYOYMD ) OR
                      ((SPA-SYOYMD-OLD NOT =   SPACE   ) AND
                       (SPA-DOUYMD         >   SPA-SYOYMD-OLD ))
                   PERFORM 300911-DOUSYOSIN-SRYKA-SEC
               END-IF
           END-IF
           .
       30091-DOUSYOSIN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    同日初診診療科設定処理 
      *****************************************************************
       300911-DOUSYOSIN-SRYKA-SEC               SECTION.
      *
      *    同日初診（前月）
           MOVE    "111011810"         TO  WRK-SRYCD
           MOVE    WRK-SRYYM-1         TO  WRK-SRYYYMM
           PERFORM 3009111-DOU-SANTEI-SEC
      *    同日初診（今月）
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYYMM
           PERFORM 3009111-DOU-SANTEI-SEC
      *H25.4
      *    同日初診（紹介状なし）（Ｈ２５．４から）
           MOVE    "111012610"         TO  WRK-SRYCD
           MOVE    WRK-SRYYM-1         TO  WRK-SRYYYMM
           IF      WRK-SRYYYMM         >=  "201304"
               PERFORM 3009111-DOU-SANTEI-SEC
           END-IF
      *    同日初診（今月）
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYYMM
           IF      WRK-SRYYYMM         >=  "201304"
               PERFORM 3009111-DOU-SANTEI-SEC
           END-IF
      *H28.4
      *    同日初診（妥結率５割以下）
           MOVE    "111012810"         TO  WRK-SRYCD
           MOVE    WRK-SRYYM-1         TO  WRK-SRYYYMM
           IF      WRK-SRYYYMM         >=  "201604"
               PERFORM 3009111-DOU-SANTEI-SEC
           END-IF
      *    同日初診（今月）
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYYMM
           IF      WRK-SRYYYMM         >=  "201604"
               PERFORM 3009111-DOU-SANTEI-SEC
           END-IF
      *
      *    同日初診（労災）（前月）
           MOVE    "101110040"         TO  WRK-SRYCD
           MOVE    WRK-SRYYM-1         TO  WRK-SRYYYMM
           PERFORM 3009111-DOU-SANTEI-SEC
      *    同日初診（今月）
           MOVE    SPA-SRYYMD(1:6)     TO  WRK-SRYYYMM
           PERFORM 3009111-DOU-SANTEI-SEC
           .
       300911-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日初診診療科設定処理 
      *****************************************************************
       3009111-DOU-SANTEI-SEC               SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    WRK-SRYYYMM         TO  SRY-SRYYM
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(1)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(2)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(3)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(4)
           MOVE    WRK-SRYCD           TO  SRY-SRYCD(5)
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        剤回数取得
               MOVE    SPACE               TO  SRYACCT-REC
               INITIALIZE                      SRYACCT-REC
               MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
               MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    SRY-PTID            TO  ACCT-PTID
               MOVE    SRY-SRYKA           TO  ACCT-SRYKA
               MOVE    SRY-SRYYM           TO  ACCT-SRYYM
               MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
               MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *        主キー読み
               PERFORM 940-SRYACCT-READ-SEC
               IF      FLG-SRYACCT         =   ZERO
                   MOVE    SPACE           TO  WRK-YMD
                   MOVE    ACCT-SRYYM      TO  WRK-YMD(1:6)
                   MOVE    ZERO            TO  WRK-DD
                   PERFORM VARYING     IDX-Y2  FROM    31 BY  -1
                           UNTIL       IDX-Y2  <   1
                       IF      ACCT-DAY (1 IDX-Y2) >   ZERO
                           MOVE    IDX-Y2          TO  WRK-DD
                           IF      WRK-YMD         >   SPA-SRYYMD
                               MOVE    ZERO        TO  WRK-DD
                           ELSE
                               MOVE    1               TO  IDX-Y2
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      WRK-DD              =   ZERO
                       MOVE    SPACE           TO  WRK-YMD
                   END-IF
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   10  )
                                  OR  (WRK-YMD =   SPACE )
                       IF      SPA-DOU-SRYKA (IDX) =   SPACE
                           MOVE    ACCT-SRYKA      TO  SPA-DOU-SRYKA
                                                             (IDX)
                           MOVE    WRK-YMD         TO  SPA-DOU-SRYYMD
                                                             (IDX)
                           MOVE    10              TO  IDX
                       ELSE
                           IF      SPA-DOU-SRYKA (IDX)
                                                   =   ACCT-SRYKA
                               IF     SPA-DOU-SRYYMD (IDX)
                                                   <   WRK-YMD
                                   MOVE    WRK-YMD TO  SPA-DOU-SRYYMD
                                                             (IDX)
                               END-IF
                               MOVE    10              TO  IDX
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300911-DOUSYOSIN-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       4901-PARA-OUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  IDX-TEI
           INITIALIZE                  SPA-K02-TEISEI-AREA
           INITIALIZE                  SPA-YAKUZAI-AREA
      *
           MOVE    ZERO                TO  WRK-EDABAN
      *
           IF     (LNK-SC95-RRKYMD     =   SPACE)  OR
                  (LNK-SC95-RENNUM     =   ZERO )
               PERFORM 4901-RRKNASI-SET-SEC
           ELSE
               PERFORM 4902-RRKARI-SET-SEC
           END-IF
      *
           IF      FLG-SYORI           =   1
               PERFORM 49011-ZAISET-SEC
           END-IF
      *
          .
       4901-PARA-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する（受診歴選択なし） 
      *****************************************************************
       4901-RRKNASI-SET-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-RRK-SRYYMD
           MOVE    ZERO                TO  WRK-RRK-DENPNUM
           INITIALIZE                      WRK-ZAINUM-TBL
      *    受診歴を検索する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key11"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    ZERO                TO  IDX-JYU
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               IF      (JYURRK-SRYYMD      <=  SPA-SRYYMD     ) AND
                       (JYURRK-HKNCOMBINUM =   SPA-HKNCOMBINUM)
                   IF     (JYURRK-SRYYMD       =   WRK-RRK-SRYYMD ) AND
                          (JYURRK-DENPNUM      =   WRK-RRK-DENPNUM)
                       ADD     1               TO  IDX-JYU
                   ELSE
                       INITIALIZE                  WRK-ZAINUM-TBL
                       MOVE    1                   TO  IDX-JYU
                   END-IF
                   IF      IDX-JYU                 >   50
                       CONTINUE
                   ELSE
                       MOVE    JYURRK-SRYYMD       TO  WRK-RRK-SRYYMD
                       MOVE    JYURRK-DENPNUM      TO  WRK-RRK-DENPNUM
                       MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
                       MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                       MOVE    JYURRK-ZAINUM-G     TO  WRK-ZAINUM-G
                                                          (IDX-JYU)
                       MOVE    1                   TO  FLG-SYORI
                   END-IF
               END-IF
               IF      JYURRK-SRYYMD       >   SPA-SRYYMD
                   MOVE    1                   TO  FLG-JYURRK
               ELSE
      *
                  MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                  MOVE    "key11"             TO  MCP-PATHNAME
                  PERFORM 970-JYURRK-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key11"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
          .
       4901-RRKNASI-SET-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する（受診歴選択あり） 
      *****************************************************************
       4902-RRKARI-SET-SEC            SECTION.
      *
           MOVE    LNK-SC95-RRKYMD     TO  WRK-RRK-SRYYMD
           INITIALIZE                      WRK-ZAINUM-TBL
      *    受診歴を検索する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    LNK-SC95-RRKYMD     TO  JYURRK-SRYYMD
           MOVE    LNK-SC95-RENNUM     TO  JYURRK-RENNUM
           IF      LNK-SC95-GRP-DENPNUM
                                   NOT =   ZERO
               MOVE    LNK-SC95-GRP-DENPNUM    TO  JYURRK-GRP-DENPNUM
               MOVE    "key26"             TO  WRK-PATH
           ELSE
               MOVE    "key5"              TO  WRK-PATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYORI
           MOVE    ZERO                TO  IDX-JYU
           MOVE    SPACE               TO  WRK-KEY-SRYKA
           MOVE    SPACE               TO  WRK-KEY-HKNCOMBI
           MOVE    SPACE               TO  WRK-KEY-DRCD
           MOVE    SPACE               TO  WRK-KEY-SRYYMD
           MOVE    ZERO                TO  WRK-RENNUM
      *    複数診療科・保険
           INITIALIZE                  SPA-TEI-AREA
           PERFORM
                   UNTIL    FLG-JYURRK     =   1
               MOVE    ZERO                TO  FLG-NO
      *        訂正処理時、同日連番の同じもの
               IF      LNK-SC95-KBN        =   "4"
                  IF    ((LNK-SC95-GRP-DENPNUM =   ZERO )  AND
                         (LNK-SC95I-DOUJI-RENNUM
                                               =   JYURRK-DOUJI-RENNUM))
                   OR   ((LNK-SC95-GRP-DENPNUM NOT =   ZERO )  AND
                         (LNK-SC95-GRP-DENPNUM
                                               =   JYURRK-GRP-DENPNUM))
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-NO
                   END-IF
      *
                   IF      WRK-KEY-SRYKA       =   SPACE
                       MOVE    JYURRK-SRYKA        TO  WRK-KEY-SRYKA
                       MOVE    JYURRK-HKNCOMBINUM  TO  WRK-KEY-HKNCOMBI
                       MOVE    JYURRK-SRYYMD       TO  WRK-KEY-SRYYMD
                       MOVE    JYURRK-DRCD         TO  WRK-KEY-DRCD
                   END-IF
                   IF     (JYURRK-NYUGAIKBN        =   "2"      ) AND
                          (LNK-SC95-GRP-DENPNUM    NOT =   ZERO ) AND
                          (LNK-SC95-GRP-DENPNUM
                                               =   JYURRK-GRP-DENPNUM)
                     AND  (JYURRK-EDANUM       =   1            )
      *                訂正時の複数履歴編集
                       PERFORM 49022-TEISEI-RRKHENSYU-SEC
                   END-IF
                   IF     (LNK-SC95-GRP-DENPNUM    NOT =   ZERO ) AND
                         ((JYURRK-SRYKA        NOT =  WRK-KEY-SRYKA) OR
                          (JYURRK-HKNCOMBINUM  NOT =  WRK-KEY-HKNCOMBI))
                       IF      FLG-SYORI           =   1
                           PERFORM 49021-FUKU-HENSYU-SEC
                           MOVE    JYURRK-SRYKA    TO  WRK-KEY-SRYKA
                           MOVE    JYURRK-HKNCOMBINUM
                                                   TO  WRK-KEY-HKNCOMBI
                           MOVE    JYURRK-SRYYMD   TO  WRK-KEY-SRYYMD
                           MOVE    JYURRK-DRCD     TO  WRK-KEY-DRCD
                       END-IF
                   END-IF
               END-IF
      *
               IF      FLG-NO              =   ZERO
                   ADD     1                   TO  IDX-JYU
                   IF      IDX-JYU                 >   50
                       CONTINUE
                   ELSE
                       MOVE    JYURRK-RENNUM       TO  WRK-RENNUM
                       MOVE    JYURRK-ZAINUM-G     TO  WRK-ZAINUM-G
                                                            (IDX-JYU)
                       MOVE    1                   TO  FLG-SYORI
                   END-IF
      *            訂正処理時、ドクター、保険組合せ、伝票番号を取得する
                   IF      LNK-SC95-KBN        =   "4"
                       IF     (LNK-SC95-GRP-DENPNUM    =   ZERO ) OR
                             ((LNK-SC95-GRP-DENPNUM    NOT =   ZERO) AND
                              (JYURRK-GRP-RENNUM       =   1    ))
                           MOVE    JYURRK-DRCD         TO  SPA-DRCD
                           MOVE    JYURRK-SRYKA        TO  SPA-SRYKA
                           MOVE    JYURRK-HKNCOMBINUM  TO
                                                       SPA-HKNCOMBINUM
                           MOVE    JYURRK-DENPNUM      TO  SPA-DENPNUM
                      END-IF
                  END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
          .
       4902-RRKARI-SET-EXT.
           EXIT.
      *****************************************************************
      *    複数科・保険分の診療行為マスタを作成する 
      *****************************************************************
       49021-FUKU-HENSYU-SEC           SECTION.
      *
           IF      FLG-SYORI           =   1
               PERFORM 49011-ZAISET-SEC
           END-IF
      *
           IF      WRK-EDABAN          >   ZERO
      *        科・保険の行
               MOVE    SPACE               TO  WKSRYACT-REC
               INITIALIZE                  WKSRYACT-REC
               MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
               MOVE    SPA-PTID            TO  WKSRY-PTID
               MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
               MOVE    WRK-KEY-SRYYMD      TO  WKSRY-SRYYMD
               MOVE    1                   TO  WKSRY-RENNUM
      *        剤回数計
               MOVE    1                   TO  WKSRY-ZAIKAIKEI
      *        保険組合
               MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *        複数診療科
               IF    ( WRK-KEY-SRYKA       NOT =   JYURRK-SRYKA )
      *H30.6
      *        ドクター不意一致も対象
                  OR ((WRK-KEY-SRYKA           =   JYURRK-SRYKA) AND
                      (JYURRK-DRCD         NOT =   ZERO        ) AND
                      (WRK-KEY-DRCD        NOT =   JYURRK-DRCD ))
      *
                   MOVE    SPACE               TO  WKSRY-INPUTCD(1)
                   MOVE    "$"                 TO  WKSRY-INPUTCD(1)(1:1)
                   MOVE    JYURRK-SRYKA        TO  WKSRY-INPUTCD(1)(2:2)
                   IF      JYURRK-DRCD(2:4)    =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    JYURRK-DRCD(2:4)
                                               TO  WKSRY-INPUTCD(1)(5:4)
                   END-IF
                   PERFORM 490211-WKSRYACT-OUT-SEC
               END-IF
               IF      WRK-KEY-HKNCOMBI    NOT =   JYURRK-HKNCOMBINUM
                   MOVE    SPACE               TO  WKSRY-INPUTCD(1)
                   MOVE    "#"                 TO  WKSRY-INPUTCD(1)(1:1)
                   MOVE    JYURRK-HKNCOMBINUM  TO  WKSRY-INPUTCD(1)(2:4)
                   PERFORM 490211-WKSRYACT-OUT-SEC
               END-IF
           END-IF
      *
           INITIALIZE                      WRK-ZAINUM-TBL
           MOVE    ZERO                TO  IDX-JYU
      *
          .
       49021-FUKU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    訂正時の複数履歴編集処理
      *****************************************************************
       49022-TEISEI-RRKHENSYU-SEC       SECTION.
      *
           MOVE    JYURRK-GRP-RENNUM   TO  IDX-F3
           IF     (IDX-F3              >=   1 )  AND
                  (IDX-F3              <=   15)
               MOVE    JYURRK-DENPNUM      TO  SPA-TEI-DENPNUM
                                                              (IDX-F3)
               MOVE    JYURRK-SRYKA        TO  SPA-TEI-SRYKA  (IDX-F3)
               MOVE    JYURRK-HKNCOMBINUM  TO  SPA-TEI-HKNCOMBI
                                                              (IDX-F3)
               MOVE    JYURRK-RENNUM       TO  SPA-TEI-RENNUM (IDX-F3)
               MOVE    JYURRK-DOUJI-RENNUM TO  SPA-TEI-DOUJI-RENNUM
                                                              (IDX-F3)
           ELSE
               MOVE    "0141"              TO  SPA-ERRCD
           END-IF
           .
       49022-TEISEI-RRKHENSYU-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為作成処理
      *****************************************************************
       490211-WKSRYACT-OUT-SEC           SECTION.
      *
           ADD     1                   TO  WRK-FUKU-ZAINUM
           MOVE    WRK-FUKU-ZAINUM     TO  WKSRY-ZAINUM
      *
           MOVE    SPACE               TO  PARA-REC
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           ADD     1                   TO  WRK-EDABAN
           MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
           PERFORM 8002-PARA-DBINSERT-SEC
      *
      **** WRITE                       PARA-REC
      *
      *    ワーク診療行為作成有無
           MOVE    1               TO  LNK-SC95-WKSRYACT
          .
       490211-WKSRYACT-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       49011-ZAISET-SEC           SECTION.
      *
      *    MOVE    ZERO                TO  IDX-TEI
      *    INITIALIZE                  SPA-K02-TEISEI-AREA
      *
      *    MOVE    ZERO                TO  WRK-EDABAN
           MOVE    ZERO                TO  FLG-JIKAN
      *
           PERFORM VARYING     IDX-JYU     FROM    1   BY  1
                   UNTIL      (IDX-JYU     >   50   )
             IF    WRK-ZAINUM-G (IDX-JYU)  NOT =   ZERO
      *
               PERFORM VARYING     IDX-R   FROM    1   BY  1
                       UNTIL      (IDX-R           >   15  )
                   IF     (WRK-TZAINUM (IDX-JYU IDX-R)
                                                 =   ZERO  )
                       CONTINUE
                   ELSE
                       PERFORM 490111-ZAISET-HEN-SEC
                   END-IF
               END-PERFORM
             END-IF
           END-PERFORM
      *
          .
       49011-ZAISET-EXT.
           EXIT.
           .
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       490111-ZAISET-HEN-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    WRK-KEY-SRYKA       TO  SRY-SRYKA
           MOVE    WRK-RRK-SRYYMD(1:6) TO  SRY-SRYYM
           MOVE    WRK-TZAINUM (IDX-JYU IDX-R)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  WRK-IDX
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *
      *        訂正処理時以外、診察を対象外とする
               IF    ( SRY-SRYKBN(1:1)     =   "1"  )  AND
                     ( LNK-SC95-KBN    NOT =   "4"  )
                   CONTINUE
               ELSE
      *
                   PERFORM 49011-SRYACT-HEN-SEC
      *            剤回数がゼロの時、対象外
                   IF      WKSRY-ZAIKAIKEI     =   ZERO
                       CONTINUE
                   ELSE
                       MOVE    SRY-ZAINUM          TO  WRK-ZAINUM
      *TEST
                       MOVE    "99999"             TO  WKSRY-UPHMS
      *TEST
                       MOVE    SPACE               TO  PARA-REC
                       MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                       ADD     1                   TO  WRK-EDABAN
      *
                       MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
                       PERFORM 8002-PARA-DBINSERT-SEC
      ***************  WRITE                       PARA-REC
      *                ワーク診療行為作成有無
                       MOVE    1               TO  LNK-SC95-WKSRYACT
                   END-IF
               END-IF
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
          .
       490111-ZAISET-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       49011-SRYACT-HEN-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAIKAISU
      *    剤回数取得
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *    主キー読み
           PERFORM 940-SRYACCT-READ-SEC
           IF      FLG-SRYACCT         =   ZERO
      *    回数決定
               MOVE    WRK-RRK-SRYYMD(7:2)     TO  IDX-STR
      *        診療連番のある時、連番の回数
      ******** EVALUATE    LNK-SC95-RENNUM 
      *!!!!
      *    診療回数テーブル１０対応
      *        EVALUATE    WRK-RENNUM
      *            WHEN    ZERO
      *            同日再診の最終回セット
      *                EVALUATE    TRUE
      *                  WHEN    ACCT-DAY (4 IDX-STR)    >   ZERO
      *                    MOVE    ACCT-DAY(4 IDX-STR) TO  WRK-ZAIKAISU
      *                  WHEN    ACCT-DAY (3 IDX-STR)    >   ZERO
      *                    MOVE    ACCT-DAY(3 IDX-STR) TO  WRK-ZAIKAISU
      *                  WHEN    ACCT-DAY (2 IDX-STR)    >   ZERO
      *                    MOVE    ACCT-DAY(2 IDX-STR) TO  WRK-ZAIKAISU
      *              END-EVALUATE
      *            WHEN    1
      *                MOVE    ACCT-DAY(2 IDX-STR) TO  WRK-ZAIKAISU
      *            WHEN    2
      *                MOVE    ACCT-DAY(3 IDX-STR) TO  WRK-ZAIKAISU
      *            WHEN    OTHER
      *                MOVE    ACCT-DAY(4 IDX-STR) TO  WRK-ZAIKAISU
      *        END-EVALUATE
      *ver.4.8
               EVALUATE    WRK-RENNUM
                   WHEN    ZERO
      *            同日再診の最終回セット
                       PERFORM VARYING    IDX-AC   FROM    10  BY  -1
                               UNTIL     (IDX-AC   <=  1   )
                           IF      ACCT-DAY (IDX-AC IDX-STR)   >   ZERO
                               MOVE    ACCT-DAY(IDX-AC IDX-STR)
                                                   TO  WRK-ZAIKAISU
                           END-IF
                       END-PERFORM
                   WHEN    OTHER
      *            連番＋１
                       COMPUTE IDX-AC          =   WRK-RENNUM   +  1
                       MOVE    ACCT-DAY(IDX-AC IDX-STR)
                                                   TO  WRK-ZAIKAISU
               END-EVALUATE
           END-IF
      *
      *
           IF      WRK-ZAINUM      NOT =   SRY-ZAINUM
               MOVE    ZERO                TO  IDX-KAI
      *
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (WRK-ZAIKAISU        =   ZERO )  OR
                      (ACCT-SRYKBN         =   "95" OR "96")
                  OR  (ACCT-HKNCOMBI       =   "9999" )
                  OR  (ACCT-JIHOKBN        =   "1"    )
                   CONTINUE
               ELSE
                   INITIALIZE                  SRYACCTPLUS-REC
                   IF      ACCT-ZAIREQFLG      =   "1"
      *                附加情報あり
                       PERFORM 32012-SRYACCTPLUS-READ-SEC
                   END-IF
      *            対象分の合計点数計算
                   IF      ACCTP-PLUSKBN        =   1
                       COMPUTE WRK-GOK-TENSU    =   WRK-GOK-TENSU
                                                -  (ACCT-ZAITEN
                                                *   WRK-ZAIKAISU )
                   ELSE
                       COMPUTE WRK-GOK-TENSU    =   WRK-GOK-TENSU
                                                +  (ACCT-ZAITEN
                                                *   WRK-ZAIKAISU )
                   END-IF
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SRY-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SRY-PTID            TO  WKSRY-PTID
      *!   MOVE    SRY-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SRY-SRYYM           TO  WKSRY-SRYYMD(1:6)
           MOVE    SRY-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    SRY-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    SRY-RENNUM          TO  WKSRY-RENNUM
           MOVE    SRY-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL  TO  WKSRY-JIHIMONEYTOTAL
      *剤回数計
           MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAIKEI
      *剤点数
           MOVE    ACCT-ZAITEN         TO  WKSRY-ZAITENKEI
      *保険組合
      *!   MOVE    ACCT-HKNCOMBI       TO  WKSRY-HKNCOMBI
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *@
      *自保区分が１の場合、包括剤として継続区分に'H'を編集
           IF      ACCT-JIHOKBN        =   1
               MOVE    "H"                 TO  WKSRY-CONTKBN
           END-IF
      *    剤内容（薬評）
           MOVE    ACCT-ZAIKBN         TO  WKSRY-ZAIKBN
      *!!!
      *HAORI 対応
           IF      FLG-HAORI           =   1
               IF     (WKSRY-ZAIKBN        =   ZERO )
                 AND  (ACCTP-PLUSKBN       =   1    )
      *            減点
                   MOVE    2               TO  WKSRY-ZAIKBN
               END-IF
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )   OR
                              (SRY-SRYCD (IDX)     =   SPACE )
      *
               MOVE    SRY-SRYCD (IDX)     TO  WKSRY-SRYCD    (IDX)
               MOVE    SRY-SRYSURYO (IDX)  TO  WKSRY-SRYSURYO (IDX)
               MOVE    SRY-SRYKAISU (IDX)  TO  WKSRY-SRYKAISU (IDX)
               MOVE    SRY-AUTOKBN  (IDX)  TO  WKSRY-AUTOKBN  (IDX)
               MOVE    SRY-INPUTNUM (IDX)  TO  WKSRY-INPUTNUM (IDX)
               MOVE    SRY-JIHIMONEY(IDX)  TO  WKSRY-JIHIMONEY (IDX)
               IF      SRY-INPUTNUM (IDX)  NOT =   ZERO
                   PERFORM 490111-PTCOM-SET-SEC
               END-IF
      *!!
      *       器材商品名対応
              IF       SRY-MEISKYFLG (IDX) =   "1"
                   MOVE    "T"             TO  WKSRY-AUTOKBN  (IDX)
               END-IF
      *!!
      *ver.4.5
      *        換算値
               MOVE    SRY-KANSURYO (IDX)  TO  WKSRY-KANSURYO (IDX)
      *        関連コメント指示
               IF       SRY-MEISKYFLG (IDX) =   "2"
                   MOVE    "1"             TO  WKSRY-INPUTKBN (IDX)
               END-IF
      *        内服種類数指示
               IF       SRY-MEISKYFLG (IDX) =   "3"
                   MOVE    "2"             TO  WKSRY-INPUTKBN (IDX)
               END-IF
      *
               MOVE    IDX                 TO  IDX-W
               ADD     1                   TO  IDX-KAI
      *        訂正の時、時間外の自動発生の有無
               IF      LNK-SC95-KBN        =   "4"
      *            時間外区分あり
                   IF     SRY-AUTOKBN  (IDX)   =   "A" OR "B" OR "C"
                                                       OR "D"
                                                       OR "E"
                                                       OR "F"
                                                       OR "G"
                                                       OR "H"
                       PERFORM 49012-JIKAN-SYORI-SEC
                   END-IF
      *            Ｈ１６．４から特定疾患処方管理料等の自動発生チェック
                   IF     (SRY-SRYYM       >=   "200404" ) AND
                          (SRY-AUTOKBN(IDX) =   "3"      )
                       PERFORM 49013-SYOHOU-CHK-SEC
                   END-IF
               END-IF
      *    点滴判定
      ****     IF      SRY-SRYKBN          =   "33" 
      *        IF      SRY-SRYSYUKBN       =   "330"  OR
      *                                        "331"
      *            MOVE    SPACE               TO  TNS-TENSU-REC
      *            MOVE    SRY-SRYCD (IDX)     TO  TNS-SRYCD
      *            PERFORM 910-TENSU-READ-SEC
      *            IF     (SRY-SRYCD (IDX)(1:1)    =   "1"  ) AND
      *                   (TNS-DATAKBN             =   "1"  )
      *                MOVE    1               TO  LNK-SC95-TENTEKI
      *            END-IF
      *            IF      SRY-SRYCD (IDX)(1:1)    =   "6"
      *                COMPUTE LNK-SC95-CHUSYA =   LNK-SC95-CHUSYA
      *                                        +  (TNS-CSYYOURYO
      *                                        *   SRY-SRYSURYO(IDX))
      *            END-IF
      *****    END-IF
      *
      *        訂正の時、診療コードと回数をセット
               IF     (LNK-SC95-KBN            =   "4" )  AND
                      (SRY-SRYCD (IDX)(1:1)    =   "1"
                                               OR  "0"
                                               OR  "6" )  AND
                      (ACCT-ZAIKBN             =   ZERO )
      *            訂正内容再編集処理
                   PERFORM 490112-TEISEI-HEN-SEC
               END-IF
           END-PERFORM
      *
           IF      ACCT-MEISAISU        =   IDX-KAI
               MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAISU (IDX-W)
           END-IF
      *
           .
       49011-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    訂正内容再編集処理
      *****************************************************************
       490112-TEISEI-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           MOVE    SRY-SRYCD (IDX)     TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *    診療コードと回数セット
           IF     (TNS-SANTEIRRKKBN    NOT=   ZERO ) OR
      *           処方せん料
                  (SRY-SRYSYUKBN       =   "820"   ) OR
      *           検査
                  (SRY-SRYKBN          =   "60"    ) OR
      *           自動発生分
                  (SRY-AUTOKBN(IDX)    =   "3"     )
      *            静脈注射・中心静脈注射
             OR  ((TNS-DATAKBN         =   "1"  ) AND
                  (TNS-CDKBN-KBN       =   "G"  ) AND
                  (TNS-CDKBN-KBNNUM    =   001
                                       OR  005
                                       OR  006  ) AND
                  (TNS-CDKBN-KBNNUM-EDA    =  00 ))
      *
               ADD     1               TO  IDX-TEI
               IF      IDX-TEI         <=  SPA-MAX-LINE
                   MOVE    SRY-SRYCD (IDX) TO  SPA-K02-TEI-SRYCD
                                                             (IDX-TEI)
                   MOVE    WRK-ZAIKAISU    TO  SPA-K02-TEI-KAISU
                                                             (IDX-TEI)
                   IF     (TNS-SANTEIRRKKBN NOT =   ZERO )
                   AND    (TNS-SRYCD (1:1)      =   "1"  )
                   AND    (TNS-JGNCNT1D         >   ZERO)
                   AND    (TNS-TANICD       NOT =   "001")
      *            １日上限の場合、数量＊回数
                       IF     SRY-SRYSURYO(IDX)    >   1
                           COMPUTE SPA-K02-TEI-KAISU (IDX-TEI)
                                                   =   SRY-SRYSURYO(IDX)
                                                   *   WRK-ZAIKAISU
                       END-IF
                   END-IF
               END-IF
           END-IF
      *    ＣＴ・ＭＲＩの時、コード変更
           IF     (TNS-SRYSYUKBN       =   "723"  OR  "724"
                                       OR  "713"  OR  "714")
             OR   (TNS-SRYKBN          =   "80"         )
               PERFORM 490111-CTMRT-HEN-SEC
           END-IF
      *    点滴注射
           IF      SRY-SRYSYUKBN       =   "330"
                                       OR  "331"
               IF     (SRY-SRYCD (IDX)(1:1)    =   "1"  ) AND
                      (TNS-DATAKBN             =   "1"  )
                   MOVE    1                   TO  LNK-SC95-TENTEKI
               END-IF
      *H27.8
      *        包括分入力は対象外
               IF     (ACCT-HKNCOMBI     =   "9999" )
                  OR  (ACCT-JIHOKBN      =   "1"    )
                   CONTINUE
               ELSE
      *
               IF      SRY-SRYCD (IDX)(1:1)    =   "6"
                   COMPUTE LNK-SC95-CHUSYA     =   LNK-SC95-CHUSYA
                                               +  (TNS-CSYYOURYO
                                               *   SRY-SRYSURYO(IDX))
               END-IF
      *
               END-IF
      *
           END-IF
      *
      *    薬剤月上限回数
           IF     (SRY-SRYCD (IDX)(1:1)    =   "6"   )
           AND   ((TNSP2-TUKIJGNKAISU      >   ZERO  )  OR
                  (TNSP2-TUKIJGNTOUYORYO   >   ZERO  ))
           AND    (SRY-SRYSYUKBN       NOT =   "95"
                                           AND "96"  )
               PERFORM 490112-TEISEI-YAKUZAI-SEC
           END-IF
           .
       490112-TEISEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤月上限回数処理
      *****************************************************************
       490112-TEISEI-YAKUZAI-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDY-TBL
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY     >   100 )
               IF      SPA-YAKUZAI-SRYCD (IDY) =   SPACE
                   MOVE    SRY-SRYCD (IDX)     TO  SPA-YAKUZAI-SRYCD
                                                            (IDY)
               END-IF
               IF      SPA-YAKUZAI-SRYCD (IDY) =   SRY-SRYCD (IDX)
                   MOVE    IDY                 TO  IDY-TBL
                   MOVE    100                 TO  IDY
               END-IF
           END-PERFORM
      *
      *    今回分削除
           IF      IDY-TBL             >   ZERO
               COMPUTE WRK-SOURYO          =  SRY-SRYSURYO (IDX)
                                           *  WRK-ZAIKAISU
               COMPUTE SPA-YAKUTEI-SURYO (IDY-TBL)
                                           =   SPA-YAKUTEI-SURYO
                                                             (IDY-TBL)
                                           -   WRK-SOURYO
      *        月回数
               COMPUTE SPA-YAKUTEI-KAISU (IDY-TBL)
                                           =   SPA-YAKUTEI-KAISU
                                                             (IDY-TBL)
                                           -   WRK-ZAIKAISU
           END-IF
           .
       490112-TEISEI-YAKUZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＴ・ＭＲＩの時、コード算定履歴用編集処理
      *****************************************************************
       490111-CTMRT-HEN-SEC              SECTION.
      *
      *    包括逓減区分判定（Ｋ０２、３でも同様の判定をしている）
           MOVE    SPACE               TO  WRK-HEN-SRYCD
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    "201"
                   MOVE    "099700101"         TO  WRK-HEN-SRYCD
      *        躯幹
               WHEN    "202"
                   MOVE    "099700102"         TO  WRK-HEN-SRYCD
      *        四肢
               WHEN    "203"
                   MOVE    "099700103"        TO  WRK-HEN-SRYCD
           END-EVALUATE
      *    リハビリ
           IF     (SRY-SRYCD (IDX)(1:1)    =   "1"   )  AND
                  (TNS-SRYKBN              =   "80"  )  AND
                  (TNS-DATAKBN             =   "1"   )  AND
                  (TNS-SANTEIRRKKBN    NOT =   ZERO )
               SET     TBL-RIH             TO  1
               SEARCH  TBL-RIHABIRI-OCC    VARYING   TBL-RIH
                   AT  END
                       MOVE    ZERO            TO  FLG-RIHA-OK
                   WHEN    SRY-SRYCD (IDX)     =   TBL-RIHA-SRYCD
                                                           (TBL-RIH)
                       MOVE    1               TO  FLG-RIHA-OK
               END-SEARCH
      *
      *        労災コードのリハビリ料
               IF     (SPA-SRYYMD              >=  "20080401")
                  AND (SRY-SRYCD  (IDX)(1:3)   =   "101"     )
                  AND (TNS-CDKBN-KBN           =   "H"       )
                   MOVE    1               TO  FLG-RIHA-OK
               END-IF
      *        H22.4 から　新規のリハビリ料コードは区分で判定
               IF     (SPA-SRYYMD              >=  "20100401")
                  AND (FLG-RIHA-OK             =   ZERO    )
                  AND (TNS-CDKBN-KBN           =   "H"     )
                  AND (TNS-CDKBN-KBNNUM        =   "001"
                                               OR  "002"   )
                  AND (TNS-CDKBN-KBNNUM-EDA    =   "00"    )
                   MOVE    1               TO  FLG-RIHA-OK
               END-IF
      *        H28.4 廃用症候群
               IF     (SPA-SRYYMD              >=  "20160401")
                  AND (FLG-RIHA-OK             =   ZERO    )
                  AND (TNS-CDKBN-KBN           =   "H"     )
                  AND (TNS-CDKBN-KBNNUM        =   "001"   )
                  AND (TNS-CDKBN-KBNNUM-EDA    =   "02"    )
                   MOVE    1               TO  FLG-RIHA-OK
               END-IF
      *
               IF      FLG-RIHA-OK         =   1
                   MOVE    "099800201"         TO  WRK-HEN-SRYCD
               END-IF
           END-IF
      *
           IF      WRK-HEN-SRYCD   NOT =   SPACE
               IF     (TNS-SANTEIRRKKBN        =   ZERO       )
      *H25.4      リハビリは追加
                  OR  (WRK-HEN-SRYCD           =   "099800201" )
                   ADD     1                   TO  IDX-TEI
               END-IF
               IF      IDX-TEI             <=  SPA-MAX-LINE
                  MOVE    WRK-HEN-SRYCD       TO  SPA-K02-TEI-SRYCD
                                                            (IDX-TEI)
                  IF      (TNS-JGNCNT1D         >   ZERO )
                     AND  (TNS-TANICD       NOT =   "001")
      *           １日上限の場合、数量＊回数
                      COMPUTE SPA-K02-TEI-KAISU (IDX-TEI)
                                               =   SRY-SRYSURYO(IDX)
                                               *   WRK-ZAIKAISU
                   ELSE
                       MOVE    WRK-ZAIKAISU    TO  SPA-K02-TEI-KAISU
                                                           (IDX-TEI)
                   END-IF
               END-IF
           END-IF
      *
           .
       490111-CTMRT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する
      *****************************************************************
       490111-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDX)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX)   TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WKSRY-INPUTCOMENT (IDX)
               MOVE    PTCOM-INPUTCHI (1)  TO  WKSRY-INPUTCHI (IDX 1)
               MOVE    PTCOM-INPUTCHI (2)  TO  WKSRY-INPUTCHI (IDX 2)
               MOVE    PTCOM-INPUTCHI (3)  TO  WKSRY-INPUTCHI (IDX 3)
               MOVE    PTCOM-INPUTCHI (4)  TO  WKSRY-INPUTCHI (IDX 4)
               MOVE    PTCOM-INPUTCHI (5)  TO  WKSRY-INPUTCHI (IDX 5)
           END-IF
           .
       490111-PTCOM-SET-EXT.
           EXIT.
      *****************************************************************
      *    時間外のチェック、作成処理
      *****************************************************************
       49012-JIKAN-SYORI-SEC               SECTION.
      *
           EVALUATE    SRY-AUTOKBN  (IDX)
               WHEN    "A"
                   MOVE    "1"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "B"
                   MOVE    "2"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "C"
                   MOVE    "3"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "D"
                   MOVE    "4"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "E"
                   MOVE    "5"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "F"
                   MOVE    "6"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "G"
                   MOVE    "7"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
               WHEN    "H"
                   MOVE    "8"             TO  WKSRY-INPUTCD(IDX)
                                                                (1:1)
           END-EVALUATE
           MOVE    WKSRY-SRYCD (IDX)   TO  WKSRY-INPUTCD (IDX)(3:)
           MOVE    SPACE               TO  WKSRY-AUTOKBN  (IDX)
      *
           .
       49012-JIKAN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    自動発生分チェック処理
      *****************************************************************
       49013-SYOHOU-CHK-SEC              SECTION.
      *
      *    長期投薬加算
           IF      (SRY-SRYSYUKBN      =   "820")  AND
                   (SRY-SRYCD  (IDX)   =   "120003270")
               MOVE    SPACE           TO  WKSRY-AUTOKBN  (IDX)
           END-IF
      *    特定疾患処方管理
           IF      (SRY-SRYSYUKBN      =   "820")  AND
                   (SRY-SRYCD  (IDX)   =   "120002570")
               MOVE    SPACE           TO  WKSRY-AUTOKBN  (IDX)
           END-IF
      *    長期投薬加算
           IF      (SRY-SRYSYUKBN      =   "250")  AND
                   (SRY-SRYCD  (IDX)   =   "120003170")
               MOVE    SPACE           TO  WKSRY-AUTOKBN  (IDX)
           END-IF
      *    特定疾患処方管理
           IF      (SRY-SRYSYUKBN      =   "250")  AND
                   (SRY-SRYCD  (IDX)   =   "120002270")
               MOVE    SPACE           TO  WKSRY-AUTOKBN  (IDX)
           END-IF
      *
           .
       49013-SYOHOU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為マスタを作成する 
      *****************************************************************
       4902-WKBUNSAN-SEC               SECTION.
      *
      *    ワーク診療行為クリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
           MOVE    ZERO               TO  FLG-WKSYORI
      *    対象の保険組合せの保存
           MOVE    ZERO                TO  SPA-WKSRY-HKNCOMBI
      *    ＣＬＡＩＭ展開
           MOVE    ZERO                TO  SPA-WKSRY-KARTE-FLG
      *
           MOVE    SPACE               TO  WRK-SRYYYMM
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYKA           TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    SPA-HKNCOMBINUM     TO  WKSRY-HKNCOMBI
      *ver.4.7
      *    同日再入院区分
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-WKSRY-DOUKBN    TO  WKSRY-DOUJITSUKBN
               IF     (SPA-WKSRY-DOUKBN    =   SPACE )
                 AND  (SPA-K02N-DOUKBN     =   "1"   )
                   MOVE    "1"             TO  WKSRY-DOUJITSUKBN
               END-IF
           END-IF
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  WRK-EDABAN
           MOVE    ZERO                TO  FLG-DRCDCHK
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
               IF      WKSRY-HKNCOMBI      =   SPA-HKNCOMBINUM
      *            ワーク診療行為マスタからの作成判定とする
                   MOVE    "WKSRYACT"          TO  WKSRY-TERMID
      *
                   MOVE    SPACE               TO  PARA-REC
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   ADD     1                   TO  WRK-EDABAN
      *
                   MOVE    WKSRYACT-REC        TO  PARA-DATA-REC
      *
                   PERFORM 8002-PARA-DBINSERT-SEC
      ************ WRITE                       PARA-REC
      *            ワーク診療行為作成有無
                   MOVE    1                   TO  LNK-SC95-WKSRYACT
      *
      *            継続区分が入力値と同じ時、前回の伝票内容をパラメタへ
                   IF      WKSRY-CONTKBN       =   "1"  OR  "2" 
      ***************AND  (SPA-GMN-CONTKBN NOT =   SPACE        )
                       MOVE    WKSRY-DOUJI-DENPNUM
                                               TO  SPA-DOUJI-DENPNUM
                       MOVE    WKSRY-DOUJI-RENNUM
                                               TO  SPA-DOUJI-RENNUM
                       MOVE    WKSRY-CONTKBN   TO  SPA-CONTKBN
                   END-IF
      *            ドクター
                   IF     (WKSRY-DRCD          =   SPACE  OR  ZERO)
                       OR (WKSRY-NYUGAIKBN     =   "1"            )
                       CONTINUE
                   ELSE
                       IF      WKSRY-DRCD      NOT =   SPA-DRCD
                           MOVE    1               TO  FLG-DRCDCHK
                       END-IF
                       MOVE    WKSRY-DRCD          TO  SPA-DRCD
                   END-IF
      *
                   MOVE    1                   TO  FLG-WKSYORI
      *            対象の保険組合せの保存
                   MOVE    WKSRY-HKNCOMBI      TO  SPA-WKSRY-HKNCOMBI
      *            ＣＬＡＩＭ展開
                   MOVE    WKSRY-KARTE-FLG     TO  SPA-WKSRY-KARTE-FLG
      *
      *            更新中フラグ
                   MOVE    1                   TO  WKSRY-MOD-FLG
      *
                   MOVE    WKSRYACT-REC        TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1999"          TO  SPA-ERRCD
                       MOVE    1               TO  FLG-WKSRYACT
                       DISPLAY "C95 WKSRYACT UPD ERR:" MCP-RC
                              ",KEY:" WKSRY-KEY
                   END-IF
               END-IF
      *
               IF      FLG-WKSRYACT        =   ZERO
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 940-WKSRYACT-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-DRCDCHK         =   1
      *        職員情報（ドクター）
               MOVE    SPACE               TO  SYS-1010-REC
               INITIALIZE                  SYS-1010-REC
               MOVE    "1010"              TO  SYS-1010-KANRICD
               MOVE    SPA-DRCD            TO  SYS-1010-KBNCD
               MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
               MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *        システム管理検索
               MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
               MOVE    SYS-1010-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "key10"             TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   INITIALIZE                  SYS-1010-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1010-REC
                   MOVE    SPA-DRCD(2:4)       TO  SPA-DRCD-NAME
                   MOVE    SYS-1010-NAME       TO  SPA-DRCD-NAME (6:)
      *
                   MOVE    SPA-DRCD-NAME       TO  SPA-GMN-DRCD-G
               END-IF
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-IF
           .
       4902-WKBUNSAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為内容追加処理
      *****************************************************************
       300-WKSRYACT-INSERT-SEC              SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      **** OPEN    OUTPUT              PARA-FILE
      *    一時ファイル削除
           PERFORM 8001-PARA-DELETE-SEC
      *
      *    ＣＬＡＩＭ展開
           MOVE    ZERO                TO  SPA-WKSRY-KARTE-FLG
      *    対象の保険組合せの保存
           MOVE    SPACE               TO  WKSRYACT-REC
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    LNK-SC95-SRYKA      TO  WKSRY-SRYKA
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
           MOVE    LNK-SC95-HKNCOMBI   TO  WKSRY-HKNCOMBI
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  WRK-EDABAN
           MOVE    ZERO                TO  FLG-CHK
           MOVE    ZERO                TO  WRK-ZAINUM
           PERFORM
               UNTIL   FLG-WKSRYACT   =   1
      *        診療科・保険の追加
               IF      WRK-EDABAN          =   ZERO
                   PERFORM 3001-SRYKA-HKNCOMBI-SEC
               END-IF
      *
               IF      WKSRY-RENNUM        =   1
                   MOVE    ZERO                TO  FLG-CHK
      *            剤番号
                   ADD     1                   TO  WRK-ZAINUM
               END-IF
      *        診察料の削除
               IF     (WKSRY-SRYKBN        =   "11"  OR
                                               "12"  OR
                                               "13"  )
      *        初再診自動発生ありのみ
               AND    (SPA-INITAUTO-FLG    =   "1"   )
                   PERFORM 3002-SRYKBN-CHK-SEC
               END-IF
               IF      FLG-CHK             =   ZERO
      ************ MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      ***********  MOVE    WRK-EDABAN          TO  PARA-EDABAN
      *            剤番号
                   MOVE    WKSRYACT-REC        TO  HZN-WKSRYACT-REC
                   MOVE    WRK-ZAINUM          TO  HZN-WKSRY-ZAINUM
      *            ワーク診療行為マスタからの作成判定とする
                   MOVE    "WKSRYACT"          TO  HZN-WKSRY-TERMID
      *
                   MOVE    SPACE               TO  PARA-REC
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   ADD     1                   TO  WRK-EDABAN
                   MOVE    HZN-WKSRYACT-REC    TO  PARA-DATA-REC
      *
                   PERFORM 8002-PARA-DBINSERT-SEC
      ************ WRITE                       PARA-REC
               END-IF
      *        ＣＬＡＩＭ展開
               MOVE    WKSRY-KARTE-FLG     TO  SPA-WKSRY-KARTE-FLG
      *        ワーク診療行為作成有無
               MOVE    1                   TO  LNK-SC95-WKSRYACT
      *        更新中フラグ
               MOVE    1                   TO  WKSRY-MOD-FLG
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C95 WKSRYACT UPD ERR:" MCP-RC
                              ",KEY:" WKSRY-KEY
                   MOVE    "1999"          TO  SPA-ERRCD
                   MOVE    1               TO  FLG-WKSRYACT
               END-IF
      *
               IF      FLG-WKSRYACT        =   ZERO
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 940-WKSRYACT-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *****CLOSE                       PARA-FILE
      *
          .
       300-WKSRYACT-INSERT-EXT.
           EXIT.
      *****************************************************************
      *    診療科保険追加処理
      *****************************************************************
       3001-SRYKA-HKNCOMBI-SEC             SECTION.
      *
      *    診療科＝９９ 複数展開なし
           IF      WKSRY-SRYKA     =   "99"
               GO  TO  3001-SRYKA-HKNCOMBI-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-INPUTCD
           MOVE    "$"             TO  WRK-INPUTCD
           MOVE    WKSRY-SRYKA     TO  WRK-INPUTCD(2:2)
           MOVE    WKSRY-DRCD(2:4) TO  WRK-INPUTCD(5:4)
           PERFORM 30011-WKSRYACT-INS-SEC
      *
           IF     (SPA-HKNKBN          =   1   )  OR
                  (SPA-HKNKBN          =   2   )  OR
                 ((SPA-NAI-ROUJIN2     =   1  )  AND
                  (SPA-HKNCOMBINUM     =   LNK-SC95-HKNCOMBI ))
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-INPUTCD
               MOVE    "#"             TO  WRK-INPUTCD
               MOVE    WKSRY-HKNCOMBI  TO  WRK-INPUTCD(2:)
               PERFORM 30011-WKSRYACT-INS-SEC
           END-IF
           .
       3001-SRYKA-HKNCOMBI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為追加処理
      *****************************************************************
       30011-WKSRYACT-INS-SEC             SECTION.
      *
           MOVE    WKSRYACT-REC        TO  HZN-WKSRYACT-REC
           MOVE    1                   TO  HZN-WKSRY-RENNUM
           ADD     1                   TO  WRK-ZAINUM
           MOVE    WRK-ZAINUM          TO  HZN-WKSRY-ZAINUM
           MOVE    SPACE               TO  HZN-WKSRY-SRYSYUKBN
           MOVE    SPACE               TO  HZN-WKSRY-SRYKBN
           INITIALIZE                      HZN-WKSRY-SINRYO-TBL-G
           MOVE    WRK-INPUTCD         TO  HZN-WKSRY-INPUTCD (1)
      *
           MOVE    SPACE               TO  PARA-REC
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
           ADD     1                   TO  WRK-EDABAN
      *
      *    ワーク診療行為マスタからの作成判定とする
           MOVE    "WKSRYACT"          TO  HZN-WKSRY-TERMID
      *
           MOVE    HZN-WKSRYACT-REC    TO  PARA-DATA-REC
      *
           PERFORM 8002-PARA-DBINSERT-SEC
      *****WRITE                       PARA-REC
           .
       30011-WKSRYACT-INS-EXT.
           EXIT.
      *****************************************************************
      *    診察料の判定処理
      *****************************************************************
       3002-SRYKBN-CHK-SEC             SECTION.
      *
           IF      WKSRY-SRYKBN        =   "11"  OR  "12"
               MOVE    1               TO  FLG-CHK
           END-IF
      *H28.1
      *    加算料は対象
           IF      WKSRY-SRYSYUKBN     =   "114"
                                       OR  "124"
                                       OR  "133"
               MOVE    ZERO                TO  FLG-CHK
           END-IF
      *
      *    小児科外来診察料の判定
           PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF        >   5   )
               IF      WKSRY-SRYCD(IDXF)       =   "113003510"
                                               OR  "113003610"
                                               OR  "113003710"
                                               OR  "113003810"
                   MOVE    1               TO  FLG-CHK
               END-IF
      *H28.4
      *    小児かかりつけ
               IF      WKSRY-SRYCD(IDXF)       =   "113019710"
                                               OR  "113019810"
                                               OR  "113019910"
                                               OR  "113020010"
                   MOVE    1               TO  FLG-CHK
               END-IF
      *        同日初診は対象
               IF      WKSRY-SRYCD(IDXF)       =   "111011810"
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *H24.4
      *        再診料２科目は対象
               IF      WKSRY-SRYCD(IDXF)       =   "112015810"
                                               OR  "112015950"
                                               OR  "112016210"
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *H24.5
      *        同一日２科目（初診（紹介状なし）、外来診察料（紹介）
               IF      WKSRY-SRYCD(IDXF)       =   "111012610"
                                               OR  "112016410"
                   MOVE    ZERO            TO  FLG-CHK
               END-IF
      *
           END-PERFORM
           .
       3002-SRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      SPA-SRYYMD          =   WKSRY-SRYYMD
                   IF      WKSRY-MOD-FLG       =   1
                       MOVE    9                   TO  WKSRY-MOD-FLG
                  END-IF
               ELSE
                   MOVE    ZERO                TO  WKSRY-MOD-FLG
               END-IF
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "C95 WKSRYACT UPD ERR:" MCP-RC
                              ",KEY:" WKSRY-KEY
                   MOVE    "1999"          TO  SPA-ERRCD
                   MOVE    1               TO  FLG-WKSRYACT
               END-IF
      *
               IF      FLG-WKSRYACT        =   ZERO
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 940-WKSRYACT-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4900-WKSRYACT-CLEAR-EXT.
           EXIT.
      *ver.4.7
      *
      *****************************************************************
      *    API　患者初期情報取得処理
      *****************************************************************
       600-API-SPASET-SEC             SECTION.
      *
           INITIALIZE                  SPA-K02
      *
      *    患者決定時編集
           PERFORM 1101-PTINF-HENSYU-SEC
      *    受診履歴一覧編集
           MOVE    ZERO                TO  SPA-RRK-PAGE
           PERFORM 2002-JYURRK-SET-SEC
      *API00
      *    受付から保険組合せ設定
           IF      LNK-SC95-PG         =   SPACE
               MOVE    ZERO                TO  FLG-UKETUKE-ARI
               MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
               PERFORM 10014-UKETUKE-CHK-SEC
           END-IF
      *
      *    受診履歴より、診療科等を確定する
           PERFORM 10011-JYURRK-SEC
      *V3対応
      *    同日連番取得
           MOVE    ZERO                TO  LNK-SC95-LAST-RENNUM
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
      *        診療日より後の履歴は対象外
               IF     (SPA-SRYYMD      =   SPA-NAI-RRKYMD (IDX))
                 AND  (SPA-SRYKA       =   SPA-NAI-RRK-SRYKA(IDX))
                   IF      SPA-NAI-RENNUM (IDX)
                                           >   LNK-SC95-LAST-RENNUM
                       MOVE    SPA-NAI-RENNUM (IDX)
                                       TO  LNK-SC95-LAST-RENNUM
                   END-IF
               END-IF
           END-PERFORM
      *
      *    保険組合せ名称編集
           PERFORM 10013-HKNCOMBI-SEC
      *
      *    患者特記事項編集
           PERFORM 1102-PTTOKKI-SET-SEC
      *
      *    初診日、最終診察日
           MOVE    SPACE               TO  SPA-SYOYMD 
           MOVE    SPACE               TO  SPA-LSTYMD 
           PERFORM 1101-HKN-SYOYMD-SET-SEC
      *
      *H29.8 APIデフォルト取得
      *    最終来院日からの判定
           IF      LNK-SC95-PG         =   SPACE
               IF      SPA-LAST-KIKAN      NOT =   ZERO
                   PERFORM 2003-LAST-KIKAN-CHK-SEC
               END-IF
           END-IF
      *
      *    病名編集
           PERFORM 11012-BYOMEI-HENSYU-SEC
      *    特定疾患の有無検索
           PERFORM 11011-TOKUTEI-SYORI-SEC
      *
           .
       600-API-SPASET-EXT.
           EXIT.
      *ver.4.7
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル削除処理
      *****************************************************************
       8001-PARA-DELETE-SEC         SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       8001-PARA-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    パラメタ一時ファイル書き込み処理
      *****************************************************************
       8002-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    WRK-EDABAN          TO  PARA-EDANUM
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
               DISPLAY "K02 PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
               MOVE    "1999"          TO  SPA-ERRCD
           END-IF
      *
           .
       8002-PARA-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付マスタ読み込み処理
      *****************************************************************
       900-UKETUKE-READ-SEC         SECTION.
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  UKETUKE-REC
               MOVE    ZERO            TO  FLG-UKETUKE
           ELSE
               INITIALIZE                  UKETUKE-REC
               MOVE    1               TO  FLG-UKETUKE
           END-IF
      *
           .
       900-UKETUKE-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC          SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       940-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
                   MOVE    ZERO                TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       900-HKNCOMBI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       900-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者介護者認定情報データ読込処理
      *****************************************************************
       930-PTCARENINTEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTCARE-NINTEI
               MOVE    MCPDATA-REC         TO  PTCARE-NINTEI-REC
           ELSE
               MOVE    1                   TO  FLG-PTCARE-NINTEI
               INITIALIZE                 PTCARE-NINTEI-REC
           END-IF
      *
           .
       930-PTCARENINTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *    点数マスタ薬剤付加情報より編集
           IF     (TNS-SRYCD(1:1)      =   "6" )
               PERFORM 9102-TENSUPLUS2-READ-SEC
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ薬剤付加情報より編集処理
      *****************************************************************
       9102-TENSUPLUS2-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS2-REC
           INITIALIZE                      TENSUPLUS2-REC
           MOVE    TNS-SRYCD           TO  TNSP2-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS2-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS2
               ELSE
                   MOVE    1                   TO  FLG-TENSUPLUS2
                   INITIALIZE                      TENSUPLUS2-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS2
               INITIALIZE                      TENSUPLUS2-REC
           END-IF
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-TENSUPLUS2-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費み込み処理
      *****************************************************************
       970-PTKOHINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTKOHINF-REC
               MOVE    ZERO            TO  FLG-PTKOHINF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           .
       970-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスタ検索処理
      *****************************************************************
       900-HKNNUM-KEY-READ-SEC              SECTION.
      *
           MOVE    HKNNUM-REC          TO  MCPDATA-REC
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hknnum"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 921-HKNNUM-READ-SEC
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-HKNNUM-KEY-READ-EXT.
           EXIT.
      *****************************************************************
      *    公費名称読み込み
      *****************************************************************
       921-HKNNUM-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       921-HKNNUM-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       32012-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       32012-SRYACCTPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者特記事項読み込み
      *****************************************************************
       900-PTTOKKI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTTOKKI-REC
               MOVE    ZERO            TO  FLG-PTTOKKI
           ELSE
               INITIALIZE                  PTTOKKI-REC
               MOVE    1               TO  FLG-PTTOKKI
           END-IF
      *
           .
       900-PTTOKKI-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者特記事項読み込み
      *****************************************************************
       930-PTCONF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTCONF-REC
               MOVE    ZERO            TO  FLG-PTCONF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1               TO  FLG-PTCONF
           END-IF
      *
           .
       930-PTCONF-READ-EXT.
           EXIT.
      *****************************************************************
      *    同一患者識別マスター読込処理
      *****************************************************************
       900-PTSAME-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-PTSAME
               MOVE    MCPDATA-REC         TO  PTSAME-REC
           ELSE
               MOVE    1                   TO  FLG-PTSAME
           END-IF
      *
           .
       900-PTSAME-READ-EXT.
           EXIT.
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *990-PARA-NEXT-SEC           SECTION.
      *
      *    READ    PARA-FILE       NEXT
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *990-PARA-NEXT-EXT.
      *    EXIT.
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
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
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
